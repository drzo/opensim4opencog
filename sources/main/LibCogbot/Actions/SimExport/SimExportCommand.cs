using System;
using System.Collections.Generic;
using System.IO;
using System.Threading;
using System.Xml;
using OpenMetaverse;
using OpenMetaverse.Assets;
// taken initially from http://openmetaverse.org/svn/omf/libopenmetaverse/trunk/Programs/SimExport -r2392
namespace cogbot.Actions.SimExport
{
    public class SimExport
    {

        GridClient client;
        TexturePipeline texturePipeline;
        volatile bool running;
        ImportSettings importSettings = new ImportSettings();
        int totalPrims = -1;
        object totalPrimsLock = new object();
        DoubleDictionary<uint, UUID, Primitive> prims = new DoubleDictionary<uint, UUID, Primitive>();
        Dictionary<uint, uint> selectedPrims = new Dictionary<uint, uint>();
        Dictionary<UUID, UUID> texturesFinished = new Dictionary<UUID, UUID>();
        BlockingQueue<Primitive> primsAwaitingSelect = new BlockingQueue<Primitive>();
        string filename;
        string directoryname = "./oarfile/";

        public SimExport(string firstName, string lastName, string password, string loginServer, string regionName, string filename)
        {
            this.filename = filename;
            directoryname = Path.GetFileNameWithoutExtension(filename);

            try
            {
                if (!Directory.Exists(directoryname)) Directory.CreateDirectory(filename);
                if (!Directory.Exists(directoryname + "/assets")) Directory.CreateDirectory(directoryname + "/assets");
                if (!Directory.Exists(directoryname + "/objects")) Directory.CreateDirectory(directoryname + "/objects");
                if (!Directory.Exists(directoryname + "/terrains")) Directory.CreateDirectory(directoryname + "/terrains");

                CheckTextures();
            }
            catch (Exception ex) { Logger.Log(ex.Message, Helpers.LogLevel.Error); return; }

            running = true;

            client = new GridClient();
            texturePipeline = new TexturePipeline(client);
            //texturePipeline.OnDownloadFinished += new TexturePipeline.DownloadFinishedCallback(texturePipeline_OnDownloadFinished);

            //Settings.LOG_LEVEL = Helpers.LogLevel.Info;
            client.Settings.MULTIPLE_SIMS = false;
            client.Settings.PARCEL_TRACKING = true;
            client.Settings.ALWAYS_REQUEST_PARCEL_ACL = true;
            client.Settings.ALWAYS_REQUEST_PARCEL_DWELL = false;
            client.Settings.ALWAYS_REQUEST_OBJECTS = true;
            client.Settings.STORE_LAND_PATCHES = true;
            client.Settings.SEND_AGENT_UPDATES = true;
            client.Settings.DISABLE_AGENT_UPDATE_DUPLICATE_CHECK = true;
            /*
            //todo
            client.Network.OnCurrentSimChanged += Network_OnCurrentSimChanged;
            client.Objects.OnNewPrim += Objects_OnNewPrim;
            client.Objects.OnObjectKilled += Objects_OnObjectKilled;
            client.Objects.OnObjectProperties += Objects_OnObjectProperties;
            client.Objects.OnObjectUpdated += Objects_OnObjectUpdated;
            client.Parcels.OnSimParcelsDownloaded += new ParcelManager.SimParcelsDownloaded(Parcels_OnSimParcelsDownloaded);
            */
            LoginParams loginParams = client.Network.DefaultLoginParams(firstName, lastName, password, "SimExport", "0.0.1");
            loginParams.URI = loginServer;
            loginParams.Start = NetworkManager.StartLocation(regionName, 128, 128, 40);

            if (client.Network.Login(loginParams))
            {
                Run();
            }
            else
            {
                Logger.Log(String.Format("Login failed ({0}: {1}", client.Network.LoginErrorKey, client.Network.LoginMessage),
                    Helpers.LogLevel.Error);
            }
        }

        void CheckTextures()
        {
            lock (texturesFinished)
            {
                string[] files = Directory.GetFiles(directoryname + "/assets", "*.jp2");

                foreach (string file in files)
                {
                    // Parse the UUID out of the filename
                    UUID id;
                    if (UUID.TryParse(Path.GetFileNameWithoutExtension(file).Substring(0, 36), out id))
                        texturesFinished[id] = id;
                }
            }

            Logger.Log(String.Format("Found {0} previously downloaded texture assets", texturesFinished.Count),
                Helpers.LogLevel.Info);
        }
        /*
        void texturePipeline_OnDownloadFinished(UUID id, bool success)
        {
            if (success)
            {
                // Save this texture to the hard drive
                ImageDownload image = texturePipeline.GetTextureToRender(id);
                try
                {
                    File.WriteAllBytes(directoryname + "/assets/" + id.ToString() + "_texture.jp2", image.AssetData);
                    lock (texturesFinished) texturesFinished[id] = id;
                }
                catch (Exception ex)
                {
                    Logger.Log("Failed to save texture: " + ex.Message, Helpers.LogLevel.Error);
                }
            }
            else
            {
                Logger.Log("Texture failed to download: " + id.ToString(), Helpers.LogLevel.Warning);
            }
        }
        */
        void Run()
        {
            // Start the thread that monitors the queue of prims that need ObjectSelect packets sent
            Thread thread = new Thread(new ThreadStart(MonitorPrimsAwaitingSelect));
            thread.Start();

            while (running)
            {
                string command = Console.ReadLine();

                switch (command)
                {
                    case "queue":
                        Logger.Log(String.Format("Client Outbox contains {0} packets, ObjectSelect queue contains {1} prims",
                            client.Network.OutboxCount, primsAwaitingSelect.Count), Helpers.LogLevel.Info);
                        break;
                    case "prims":
                        Logger.Log(String.Format("Prims captured: {0}, Total: {1}", prims.Count, totalPrims), Helpers.LogLevel.Info);
                        break;
                    case "parcels":
                        if (!client.Network.CurrentSim.IsParcelMapFull())
                        {
                            Logger.Log("Downloading sim parcel information and prim totals", Helpers.LogLevel.Info);
                            client.Parcels.RequestAllSimParcels(client.Network.CurrentSim, false, 10);
                        }
                        else
                        {
                            Logger.Log("Sim parcel information has been retrieved", Helpers.LogLevel.Info);
                        }
                        break;
                    case "camera":
                        Thread cameraThread = new Thread(new ThreadStart(MoveCamera));
                        cameraThread.Start();
                        Logger.Log("Started random camera movement thread", Helpers.LogLevel.Info);
                        break;
                    case "movement":
                        Vector3 destination = RandomPosition();
                        Logger.Log("Teleporting to " + destination.ToString(), Helpers.LogLevel.Info);
                        client.Self.Teleport(client.Network.CurrentSim.Handle, destination, RandomPosition());
                        break;
                    case "textures":
                        Logger.Log(String.Format("Current texture requests: {0}, queued texture requests: {1}, completed textures: {2}",
                            texturePipeline.TransferCount, /*texturePipeline.QueuedCount*/ float.NaN, /*texturesFinished.Count*/ float.NaN), Helpers.LogLevel.Info);
                        break;
                    case "terrain":
                        TerrainPatch[] patches = client.Network.CurrentSim.SharedData.Terrain;
                        if (patches != null)
                        {
                            int count = 0;
                            for (int i = 0; i < patches.Length; i++)
                            {
                                if (patches[i] != null)
                                    ++count;
                            }

                            Logger.Log(count + " terrain patches have been received for the current simulator", Helpers.LogLevel.Info);
                        }
                        else
                        {
                            Logger.Log("No terrain information received for the current simulator", Helpers.LogLevel.Info);
                        }
                        break;
                    case "saveterrain":
                        patches = client.Network.CurrentSim.SharedData.Terrain;
                        if (patches != null)
                        {
                            try
                            {
                                using (FileStream stream = new FileStream(directoryname + "/terrains/heightmap.r32", FileMode.Create, FileAccess.Write))
                                {
                                    for (int y = 0; y < 256; y++)
                                    {
                                        for (int x = 0; x < 256; x++)
                                        {
                                            int xBlock = x / 16;
                                            int yBlock = y / 16;
                                            int xOff = x - (xBlock * 16);
                                            int yOff = y - (yBlock * 16);

                                            TerrainPatch patch = patches[yBlock * 16 + xBlock];
                                            float t = 0f;

                                            if (patch != null)
                                                t = patch.Data[yOff * 16 + xOff];
                                            else
                                                Logger.Log(String.Format("Skipping missing patch at {0},{1}", xBlock, yBlock),
                                                    Helpers.LogLevel.Warning);

                                            stream.Write(BitConverter.GetBytes(t), 0, 4);
                                        }
                                    }
                                }
                            }
                            catch (Exception ex)
                            {
                                Logger.Log("Failed saving terrain: " + ex.Message, Helpers.LogLevel.Error);
                            }
                        }
                        else
                        {
                            Logger.Log("No terrain information received for the current simulator", Helpers.LogLevel.Info);
                        }
                        break;
                    case "save":
                        Logger.Log(String.Format("Preparing to serialize {0} objects", prims.Count), Helpers.LogLevel.Info);
                        SavePrims(prims, directoryname + "/objects", importSettings);
                        Logger.Log("Saving " + directoryname, Helpers.LogLevel.Info);
                        OarFile.PackageArchive(directoryname, filename, true, false);
                        Logger.Log("Done", Helpers.LogLevel.Info);
                        break;
                    case "quit":
                        End();
                        break;
                }
            }
        }

        public static void SavePrims(DoubleDictionary<uint, UUID, Primitive> prims, string path, ImportSettings options)
        {
            // Delete all of the old linkset files
            try
            {
                Directory.Delete(path, true);
                Directory.CreateDirectory(path);
            }
            catch (Exception ex)
            {
                Logger.Log("Failed saving prims: " + ex.Message, Helpers.LogLevel.Error);
                return;
            }

            // Copy the double dictionary to a temporary list for iterating
            List<Primitive> primList = new List<Primitive>();
            prims.ForEach(delegate(Primitive prim)
            {
                primList.Add(prim);
            });

            foreach (Primitive p in primList)
            {
                if (p.ParentID == 0)
                {
                    Linkset linkset = new Linkset();
                    linkset.Parent = ImportCommand.Running.APrimToCreate(p);

                    prims.ForEach(delegate(Primitive q)
                    {
                        if (q.ParentID == p.LocalID)
                            linkset.Children.Add(ImportCommand.Running.APrimToCreate(q));
                    });

                    OarFile.SaveLinkset(linkset, path + "/Primitive_" + linkset.Parent.NewID.ToString() + ".xml", false,
                                        options);
                }
            }
        }


        Random random = new Random();

        Vector3 RandomPosition()
        {
            float x = (float)(random.NextDouble() * 256d);
            float y = (float)(random.NextDouble() * 128d);
            float z = (float)(random.NextDouble() * 256d);

            return new Vector3(x, y, z);
        }

        void MoveCamera()
        {
            while (running)
            {
                if (client.Network.Connected)
                {
                    // TWEAK: Randomize far distance to force an interest list recomputation
                    float far = (float)(random.NextDouble() * 252d + 4d);

                    // Random small movements
                    AgentManager.ControlFlags flags = AgentManager.ControlFlags.NONE;
                    if (far < 96f)
                        flags |= AgentManager.ControlFlags.AGENT_CONTROL_TURN_LEFT;
                    else if (far < 196f)
                        flags |= AgentManager.ControlFlags.AGENT_CONTROL_TURN_RIGHT;
                    else if (far < 212f)
                        flags |= AgentManager.ControlFlags.AGENT_CONTROL_UP_POS;
                    else
                        flags |= AgentManager.ControlFlags.AGENT_CONTROL_UP_NEG;

                    // Randomly change the camera position
                    Vector3 pos = RandomPosition();

                    client.Self.Movement.SendManualUpdate(
                        flags, pos, Vector3.UnitZ, Vector3.UnitX, Vector3.UnitY, Quaternion.Identity, Quaternion.Identity, far,
                        AgentFlags.None, AgentState.None, false);
                }

                Thread.Sleep(500);
            }
        }

        void End()
        {
            texturePipeline.Shutdown();

            if (client.Network.Connected)
            {
                if (Program.Verbosity > 0)
                    Logger.Log("Logging out", Helpers.LogLevel.Info);

                client.Network.Logout();
            }

            running = false;
        }

        void MonitorPrimsAwaitingSelect()
        {
            while (running)
            {
                try
                {
                    Primitive prim = primsAwaitingSelect.Dequeue(250);

                    if (!prims.ContainsKey(prim.LocalID) && prim != null)
                    {
                        client.Objects.SelectObject(client.Network.CurrentSim, prim.LocalID);
                        Thread.Sleep(20); // Hacky rate limiting
                    }
                }
                catch (InvalidOperationException)
                {
                }
            }
        }

        void Network_OnCurrentSimChanged(Simulator PreviousSimulator)
        {
            if (Program.Verbosity > 0)
                Logger.Log("Moved into simulator " + client.Network.CurrentSim.ToString(), Helpers.LogLevel.Info);
        }

        void Parcels_OnSimParcelsDownloaded(Simulator simulator, InternalDictionary<int, Parcel> simParcels, int[,] parcelMap)
        {
            lock (totalPrimsLock)
            {
                totalPrims = 0;
                simParcels.ForEach(
                    delegate(Parcel parcel) { totalPrims += parcel.TotalPrims; });

                if (Program.Verbosity > 0)
                    Logger.Log(String.Format("Counted {0} total prims in this simulator", totalPrims), Helpers.LogLevel.Info);
            }
        }

        void Objects_OnNewPrim(Simulator simulator, Primitive prim, ulong regionHandle, ushort timeDilation)
        {
            prims.Add(prim.LocalID, prim.ID, prim);
            primsAwaitingSelect.Enqueue(prim);
            UpdateTextureQueue(prim.Textures);
        }

        void UpdateTextureQueue(Primitive.TextureEntry te)
        {
            if (te != null)
            {
                for (int i = 0; i < te.FaceTextures.Length; i++)
                {
                    if (te.FaceTextures[i] != null && !texturesFinished.ContainsKey(te.FaceTextures[i].TextureID))
                    {
                        client.Assets.RequestImage(te.FaceTextures[i].TextureID, ImageType.Normal, 101300.0f, 0, 0,
                                                         OnComplete, false);
                    }
                }
            }
        }

        void OnComplete(TextureRequestState state, AssetTexture image)
        {
            if (state == TextureRequestState.Finished)
            {
                File.WriteAllBytes(directoryname + "/assets/" + image.AssetID.ToString() + "_texture.jp2", image.AssetData);
            }
        }

        void Objects_OnObjectUpdated(Simulator simulator, ObjectMovementUpdate update, ulong regionHandle, ushort timeDilation)
        {
            if (!update.Avatar)
            {
                Primitive prim;

                if (prims.TryGetValue(update.LocalID, out prim))
                {
                    lock (prim)
                    {
                        if (Program.Verbosity > 1)
                            Logger.Log("Updating state for " + prim.ID.ToString(), Helpers.LogLevel.Info);

                        prim.Acceleration = update.Acceleration;
                        prim.AngularVelocity = update.AngularVelocity;
                        prim.CollisionPlane = update.CollisionPlane;
                        prim.Position = update.Position;
                        prim.Rotation = update.Rotation;
                        prim.PrimData.State = update.State;
                        prim.Textures = update.Textures;
                        prim.Velocity = update.Velocity;
                    }

                    UpdateTextureQueue(prim.Textures);
                }
            }
        }

        void Objects_OnObjectProperties(Simulator simulator, Primitive.ObjectProperties props)
        {
            Primitive prim;

            if (prims.TryGetValue(props.ObjectID, out prim))
            {
                if (Program.Verbosity > 2)
                    Logger.Log("Received properties for " + props.ObjectID.ToString(), Helpers.LogLevel.Info);

                lock (prim)
                    prim.Properties = props;
            }
            else
            {
                Logger.Log("Received object properties for untracked object " + props.ObjectID.ToString(),
                    Helpers.LogLevel.Warning);
            }
        }

        void Objects_OnObjectKilled(Simulator simulator, uint objectID)
        {
            ;
        }
    }

    public class Program
    {
        public static int Verbosity = 0;

        static void MainSimExportExample(string[] args)
        {
            string loginServer = Settings.AGNI_LOGIN_SERVER;
            string filename = "simexport.tgz";
            string regionName = "OpenSim Test", firstName = "My", lastName = "Bot", password = "myBotPassword";
            bool showhelp = false;

            NDesk.Options.OptionSet argParser = new NDesk.Options.OptionSet()
                .Add("s|login-server=", "URL of the login server (default is '" + loginServer + "')", delegate(string v) { loginServer = v; })
                .Add("r|region-name=", "name of the region to export", delegate(string v) { regionName = v; })
                .Add("f|firstname=", "first name of the bot to log in", delegate(string v) { firstName = v; })
                .Add("l|lastname=", "last name of the bot to log in", delegate(string v) { lastName = v; })
                .Add("p|password=", "password of the bot to log in", delegate(string v) { password = v; })
                .Add("o|output=", "filename of the OAR to write (default is 'simexport.tgz')", delegate(string v) { filename = v; })
                .Add("h|?|help", delegate(string v) { showhelp = (v != null); })
                .Add("v|verbose", delegate(string v) { if (v != null) ++Verbosity; });
            argParser.Parse(args);

            if (!showhelp && !String.IsNullOrEmpty(regionName) &&
                !String.IsNullOrEmpty(firstName) && !String.IsNullOrEmpty(lastName) && !String.IsNullOrEmpty(password))
            {
                SimExport exporter = new SimExport(firstName, lastName, password, loginServer, regionName, filename);
            }
            else
            {
                WriteLine("Usage: SimExport.exe [OPTION]...");
                WriteLine("An interactive client for exporting assets");
                WriteLine("Options:");
                argParser.WriteOptionDescriptions(Console.Out);
            }
        }

        private static void WriteLine(string p)
        {
            throw new NotImplementedException();
        }
    }
}
