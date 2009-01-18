using System;
using System.Collections.Generic;
using System.IO;
using System.Threading;
using System.Xml;
using OpenMetaverse;
// taken initially from http://openmetaverse.org/svn/omf/libopenmetaverse/trunk/Programs/SimExport -r2392
namespace cogbot.Actions.SimExport
{
    public class SimExportCommand : cogbot.Actions.Command
    {
        public SimExportCommand(BotClient Client)
        {

            Name = "simexport";
            Description = "Exports parts of the Sim to TGZ file.";
            Category = CommandCategory.TestClient;
            helpString = "Exports parts of the Sim to TGZ file.";
            usageString = "simexport help";
           // return;
            //CurrentClient = Client.CurrentClient;
            return;
            Client.Network.OnLogin += new NetworkManager.LoginCallback(Network_OnLogin);
            SetUpSimExportCommand(Client);
        }

        void Network_OnLogin(LoginStatus login, string message)
        {
            WriteLine("SimExportCommand Logging In [" + login.ToString() + "] for MonitorPrimsAwaitingSelect");
            if (login == LoginStatus.Success)
            {
                try
                {
                    //throw new NotImplementedException();
                    Thread thread = new Thread(new ThreadStart(MonitorPrimsAwaitingSelect));
                    thread.Start();
                   // SetUpSimExportCommand(Client);
                }
                catch (Exception e)
                {
                }
            }
          
        }

        //GridClient CurrentClient;
        TexturePipeline texturePipeline;
        volatile bool running;

        int totalPrims = -1;
        object totalPrimsLock = new object();
        DoubleDictionary<uint, UUID, Primitive> prims = new DoubleDictionary<uint, UUID, Primitive>();
        Dictionary<uint, uint> selectedPrims = new Dictionary<uint, uint>();
        Dictionary<UUID, UUID> texturesFinished = new Dictionary<UUID, UUID>();
        BlockingQueue<Primitive> primsAwaitingSelect = new BlockingQueue<Primitive>();
        string filename = "simexport.tgz";
        string directoryname = "simexport";

        public void SetUpSimExportCommand(GridClient client)
            //string firstName, string lastName, string password, string loginServer, string regionName, string filename)
        {
            //CurrentClient = CurrentClient;
      
            running = true;

            //CurrentClient = new GridClient();
            texturePipeline = new TexturePipeline(client, 10);
            texturePipeline.OnDownloadFinished += new TexturePipeline.DownloadFinishedCallback(texturePipeline_OnDownloadFinished);
            
            //Settings.LOG_LEVEL = Helpers.LogLevel.Info;
            client.Settings.MULTIPLE_SIMS = false;
            client.Settings.PARCEL_TRACKING = true;
            client.Settings.ALWAYS_REQUEST_PARCEL_ACL = true;
            client.Settings.ALWAYS_REQUEST_PARCEL_DWELL = false;
            client.Settings.ALWAYS_REQUEST_OBJECTS = true;
            client.Settings.STORE_LAND_PATCHES = true;
            client.Settings.SEND_AGENT_UPDATES = true;
            client.Settings.OBJECT_TRACKING = true;
            client.Settings.DISABLE_AGENT_UPDATE_DUPLICATE_CHECK = true;

            client.Network.OnCurrentSimChanged += Network_OnCurrentSimChanged;
            client.Objects.OnNewPrim += Objects_OnNewPrim;
            client.Objects.OnObjectKilled += Objects_OnObjectKilled;
            client.Objects.OnObjectProperties += Objects_OnObjectProperties;
            client.Objects.OnObjectUpdated += Objects_OnObjectUpdated;
            client.Parcels.OnSimParcelsDownloaded += new ParcelManager.SimParcelsDownloaded(Parcels_OnSimParcelsDownloaded);
            client.Parcels.OnParcelProperties += new ParcelManager.ParcelPropertiesCallback( On_ParcelProperties);
              //CurrentClient.Parcels.OnParcelProperties 

            //LoginParams loginParams = CurrentClient.Network.DefaultLoginParams(firstName, lastName, password, "SimExport", "0.0.1");
            //loginParams.URI = loginServer;
            //loginParams.Start = NetworkManager.StartLocation(regionName, 128, 128, 40);

            //if (CurrentClient.Network.Login(loginParams))
           // {
                //Run();
           // }
            //else
            //{
              //  WriteLine(String.Format("Login failed ({0}: {1}", CurrentClient.Network.LoginErrorKey, CurrentClient.Network.LoginMessage),
                //    Helpers.LogLevel.Error);
            //}
        }

       public void On_ParcelProperties(Simulator simulator, Parcel parcel, ParcelResult result, int selectedPrims,
            int sequenceID, bool snapSelection) {
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

            WriteLine(String.Format("Found {0} previously downloaded texture assets", texturesFinished.Count),
                Helpers.LogLevel.Info);
        }

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
                    WriteLine("Failed to save texture: " + ex.Message, Helpers.LogLevel.Error);
                }
            }
            else
            {
                WriteLine("Texture failed to download: " + id.ToString(), Helpers.LogLevel.Warning);
            }
        }
        public override string Execute(string[] args, UUID fromAgentID)
        {
            if (args.Length > 1)
            {
                filename = args[1];
            }
            SetFilename(filename);
            return DoCommand(args[0]);
        }
        //void Run()
        public string DoCommand(string command)
        {
            // Start the thread that monitors the queue of prims that need ObjectSelect packets sent
//            Thread thread = new Thread(new ThreadStart(MonitorPrimsAwaitingSelect));
  //          thread.Start();
         //   while (running)
            {
                GridClient client = Client;
                switch (command)
                {
                    case "camera":
                        Thread cameraThread = new Thread(new ThreadStart(MoveCamera));
                        cameraThread.Start();
                        return ("Started random camera movement thread");
                    case "movement":
                    case "move":
                        Vector3 destination = RandomPosition();
                        WriteLine("Teleporting to " + destination.ToString());
                        client.Self.Teleport(Client.Network.CurrentSim.Handle, destination, RandomPosition());
                        return ("Done " + "Teleporting to " + destination.ToString());
                    case "info":
                        DoCommand("prims");
                        DoCommand("textures");
                        DoCommand("queue");
                        return DoCommand("terrain");
                    case "queue":
                        return (String.Format("Client Outbox contains {0} packets, ObjectSelect queue contains {1} prims",
                            client.Network.OutboxCount, primsAwaitingSelect.Count));
                    case "prims":
                        return (String.Format("Prims captured: {0}, Total: {1}", prims.Count, totalPrims));
                    case "textures":
                        return (String.Format("Current texture requests: {0}, queued texture requests: {1}, completed textures: {2}",
                            texturePipeline.CurrentCount, texturePipeline.QueuedCount, texturesFinished.Count));
                    case "parcels":
                        if (!client.Network.CurrentSim.IsParcelMapFull())
                        {
                            WriteLine("Downloading sim parcel information and prim totals");
                            client.Parcels.RequestAllSimParcels(client.Network.CurrentSim, false, 10);
                            return ("Done RequestAllSimParcels");
                        }
                        else
                        {
                            return ("Sim parcel information has been retrieved");
                        }
                    case "terrain":
                        TerrainPatch[] patches;
                        if (client.Terrain.SimPatches.TryGetValue(client.Network.CurrentSim.Handle, out patches))
                        {
                            int count = 0;
                            for (int i = 0; i < patches.Length; i++)
                            {
                                if (patches[i] != null)
                                    ++count;
                            }

                            return (count + " terrain patches have been received for the current simulator");
                        }
                        else
                        {
                            return ("No terrain information received for the current simulator");
                        }
                    case "saveterrain":
                        if (client.Terrain.SimPatches.TryGetValue(client.Network.CurrentSim.Handle, out patches))
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
                                                WriteLine(String.Format("Skipping missing patch at {0},{1}", xBlock, yBlock),
                                                    Helpers.LogLevel.Warning);

                                            stream.Write(BitConverter.GetBytes(t), 0, 4);
                                        }
                                    }
                                }
                            }
                            catch (Exception ex)
                            {
                                WriteLine("Failed saving terrain: " + ex.Message, Helpers.LogLevel.Error);
                            }
                        }
                        else
                        {
                            return ("No terrain information received for the current simulator");
                        }
                        return ("Done with saveterrain " + filename);
                    case "save":
                        WriteLine(String.Format("Preparing to serialize {0} objects", prims.Count));
                        OarFile.SavePrims(prims, directoryname + "/objects");
                        WriteLine("Saving " + directoryname);
                        OarFile.PackageArchive(directoryname, filename);
                        return ("Done with save " + filename);
                    case "all":
                        DoCommand("terrain");
                    //    DoCommand("parcels");
                        DoCommand("saveterrain");
                        return DoCommand("save");
                    case "help":
                    default:
                        break;
                 
//                    case "quit":
  //                      End();
    //                    break;
                }
            }
            WriteLine("simexport camera - starts random camera movement thread");
            WriteLine("simexport movement - random movement");
            WriteLine("simexport saveterrain [filename] - saves terrain info");
            WriteLine("simexport save [filename] - saves all info except for terrain");
            return "simexport [queue|prims|parcels|textures|terrain|info] - displays information about items";
        }

        private void SetFilename(string path)
        {
            if (!path.Contains("."))
            {
                path = path + ".tgz";
            }
            this.filename = path;
            directoryname = Path.GetFileNameWithoutExtension(filename);
            try
            {
                if (!Directory.Exists(directoryname)) Directory.CreateDirectory(directoryname);
                if (!Directory.Exists(directoryname + "/assets")) Directory.CreateDirectory(directoryname + "/assets");
                if (!Directory.Exists(directoryname + "/objects")) Directory.CreateDirectory(directoryname + "/objects");
                if (!Directory.Exists(directoryname + "/terrains")) Directory.CreateDirectory(directoryname + "/terrains");

                CheckTextures();
            }
            catch (Exception ex) { WriteLine(ex.Message, Helpers.LogLevel.Error); return; }
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
                GridClient client = Client;

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
                        AgentManager.AgentFlags.None, AgentManager.AgentState.None, false);
                }

                Thread.Sleep(500);
            }
        }

/*        void End()
        {
            texturePipeline.Shutdown();

            if (CurrentClient.Network.Connected)
            {
                if (Program.Verbosity > 0)
                    WriteLine("Logging out");

                CurrentClient.Network.Logout();
            }

            running = false;
        }
        */
        void MonitorPrimsAwaitingSelect()
        {
            while (running)
            {
                try
                {
                    if (primsAwaitingSelect.Count > 0)
                    {
                        Primitive prim = primsAwaitingSelect.Dequeue(250);

                        if (!prims.ContainsKey(prim.LocalID) && prim != null)
                        {
                            Client.Objects.SelectObject(Client.Network.CurrentSim, prim.LocalID);
                            Thread.Sleep(20); // Hacky rate limiting
                        }
                    }
                }
                catch (InvalidOperationException)
                {
                    Thread.Sleep(20); // Hacky rate limiting
                }
            }
        }

        void Network_OnCurrentSimChanged(Simulator PreviousSimulator)
        {
            if (Program.Verbosity > 0)
                WriteLine("Moved into simulator " + Client.Network.CurrentSim.ToString());
        }

        void Parcels_OnSimParcelsDownloaded(Simulator simulator, InternalDictionary<int, Parcel> simParcels, int[,] parcelMap)
        {
            lock (totalPrimsLock)
            {
                totalPrims = 0;
                simParcels.ForEach(
                    delegate(Parcel parcel) { totalPrims += parcel.TotalPrims; });

                if (Program.Verbosity > 0)
                    WriteLine(String.Format("Counted {0} total prims in this simulator", totalPrims));
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
                        texturePipeline.RequestTexture(te.FaceTextures[i].TextureID, ImageType.Normal);
                }
            }
        }

        void Objects_OnObjectUpdated(Simulator simulator, ObjectUpdate update, ulong regionHandle, ushort timeDilation)
        {
            if (!update.Avatar)
            {
                Primitive prim;

                if (prims.TryGetValue(update.LocalID, out prim))
                {
                    lock (prim)
                    {
                        if (Program.Verbosity > 1)
                            WriteLine("Updating state for " + prim.ID.ToString());

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
                    WriteLine("Received properties for " + props.ObjectID.ToString());

                lock (prim)
                    prim.Properties = props;
            }
            else
            {
                WriteLine("Received object properties for untracked object " + props.ObjectID.ToString(),
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
                //SimExport exporter = new SimExport(firstName, lastName, password, loginServer, regionName, filename);
            }
            else
            {
                Console.WriteLine("Usage: SimExport.exe [OPTION]...");
                Console.WriteLine("An interactive CurrentClient for exporting assets");
                Console.WriteLine("Options:");
                argParser.WriteOptionDescriptions(Console.Out);
            }
        }
    }
}
