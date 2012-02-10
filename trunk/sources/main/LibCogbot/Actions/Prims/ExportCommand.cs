using System;
using System.Collections;
using System.Collections.Generic;
using System.IO;
using System.Runtime.Serialization.Formatters.Binary;
using System.Threading;
using cogbot.Listeners;
using cogbot.TheOpenSims;
using MushDLR223.Utilities;
using OpenMetaverse;
using OpenMetaverse.StructuredData;
using OpenMetaverse.Assets;

using MushDLR223.ScriptEngines;

namespace cogbot.Actions.SimExport
{
    public class IHO
    {
        public InventoryBase I;
        public AssetManager.AssetReceivedCallback H;
        public SimObject O;
        public override string ToString()
        {
            return I.Name + "(" + I.GetType().Name.Substring("Inventory".Length) + ")@" + ExportCommand.named(O);
        }
    }
    public class SO
    {
        public string S;
        public string F;
        public SimObject O;
        public override string ToString()
        {
            return ExportCommand.named(O) + ":" + S;
        }
    }
    public class ExportCommand : Command, RegionMasterCommand
    {
        private static TaskQueueHandler slowlyExport = new TaskQueueHandler("slowlyExport", TimeSpan.FromMilliseconds(100),
                                                                            true);
        public static readonly List<UUID> ToDownloadAssets = new List<UUID>();
        public static readonly Dictionary<UUID, AssetType> AllRelatedAssets = new Dictionary<UUID, AssetType>();
        public static readonly List<UUID> PrimDepsAssets = new List<UUID>();
        public static readonly Dictionary<UUID, SO> PrimWaitingLinkset = new Dictionary<UUID, SO>();
        public static readonly Dictionary<InventoryBase, IHO> TaskAssetWaiting = new Dictionary<InventoryBase, IHO>();
        public static readonly Dictionary<InventoryBase, string> NotTaskAssetWaiting = new Dictionary<InventoryBase, string>();
        public static string dumpDir = "cog_export/objects/";
        public static string assetDumpDir = "cog_export/assets/";
        public static bool IsExporting = false;
        static private List<SimObject> exportedPrims = new List<SimObject>();
        static UUID inventoryHolderUUID = UUID.Zero;
        public static bool Incremental = true;
        static InventoryItem linkSpeaker = null;
        private int LocalFailures;
        static object fileWriterLock = new object();

        public ExportCommand(BotClient testClient)
        {
            // testClient.Objects.ObjectPropertiesFamily += new EventHandler<ObjectPropertiesFamilyEventArgs>(Objects_OnObjectPropertiesFamily);

            //testClient.Objects.ObjectProperties += new EventHandler<ObjectPropertiesEventArgs>(Objects_OnObjectProperties);
            //testClient.Avatars.ViewerEffectPointAt += new EventHandler<ViewerEffectPointAtEventArgs>(Avatars_ViewerEffectPointAt);
            testClient.Self.ChatFromSimulator += listen_forLinkset;
            testClient.Assets.XferReceived += Asset_Xfer;
            Name = "export";
            Description = "Exports an object to an xml file. Usage: export exportPrim-spec directory";
            Category = CommandCategory.Objects;
        }

        public override CmdResult Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
            IsExporting = true;
            EnsureTaskInvHolder();
            string[] nargs = { "$region" };
            List<string> arglist = new List<string>(args);
            if (args.Length > 1)
            {
                if (args[0] == "prims")
                {
                    nargs = Parser.SplitOff(args, 1);
                }
            }
            if (arglist.Contains("nonincr")) Incremental = false;
            if (arglist.Contains("incr")) Incremental = true;
            if (false) if (args.Length < 1) return ShowUsage();
            lock (fileWriterLock)
            {
                if (arglist.Contains("clear"))
                {
                    if (Directory.Exists(dumpDir)) Directory.Delete(dumpDir, true);
                    if (Directory.Exists(assetDumpDir)) Directory.Delete(assetDumpDir, true);
                    string sfile = Path.GetDirectoryName(SimAsset.CFileName(UUID.Zero, AssetType.Texture));
                    if (arglist.Contains("cache"))
                        if (Directory.Exists(sfile))
                        {
                            Directory.Delete(sfile, true);
                            Directory.CreateDirectory(sfile);
                        }
                    arglist.Add("reset");
                }

                if (!Directory.Exists(dumpDir)) Directory.CreateDirectory(dumpDir);
                if (!Directory.Exists(assetDumpDir)) Directory.CreateDirectory(assetDumpDir);
            }
            if (arglist.Contains("reset"))
            {
                slowlyExport.Clear();
                lock (ToDownloadAssets) ToDownloadAssets.Clear();
                lock (PrimWaitingLinkset) PrimWaitingLinkset.Clear();
                lock (AllRelatedAssets) AllRelatedAssets.Clear();
                lock (PrimDepsAssets) PrimDepsAssets.Clear();
                lock (TaskAssetWaiting) TaskAssetWaiting.Clear();
                lock (NotTaskAssetWaiting) NotTaskAssetWaiting.Clear();
            }
            //string file = args[args.Length - 1];
            int used;
            List<SimObject> PS = WorldSystem.GetPrimitives(nargs, out used);
            if (IsEmpty(PS)) return Failure("Cannot find objects from " + string.Join(" ", args));
            foreach (var P in PS)
            {
                if (P is SimAvatar) continue;
                // skip attachments
                if (P.Parent is SimAvatar) continue;
                if (!P.HasPrim)
                {
                    Failure("Missing Prim: " + P);
                    continue;
                }
                if (arglist.Contains("status")) continue;
                if (exportedPrims.Contains(P)) continue;
                LocalFailures = 0;
                PrimDepsAssets.Clear();
                ExportPrim(Client, P, LocalFailure);
                if (LocalFailures == 0)
                {
                    exportedPrims.Add(P);
                }
            }
            if (!arglist.Contains("nolinks"))
            {
                lock (PrimWaitingLinkset)
                {
                    InventoryItem found = GetLinkSpeaker(Client);
                    foreach (var pa in PrimWaitingLinkset)
                    {
                        var exportPrim = pa.Value.O;
                        Failure("Awaiting Linkset " + named(exportPrim));
                        if (arglist.Contains("links"))
                        {
                            PutLinkSpeaker(Client, exportPrim);
                        }
                    }
                }
            }

            if (!arglist.Contains("nostatus"))
            {
                lock (TaskAssetWaiting)
                {
                    foreach (var pa in TaskAssetWaiting)
                    {
                        Failure("Awaiting TaskAsset " + pa.Value);
                    }
                }
            }

            var res = ExportRelatedAssets();
            if (arglist.Contains("dl"))
            {

                foreach (var pa in LockInfo.CopyOf(ToDownloadAssets))
                {
                    AssetType type = assetTypeOf(pa);
                    Failure("Awaiting DL " + pa + " " + type);
                    byte[] b = Client.Assets.Cache.GetCachedAssetBytes(pa, type);
                    if (b != null)
                    {
                        string file = Path.GetFileName(Client.Assets.Cache.FileName(pa, type));
                        lock (fileWriterLock) File.WriteAllBytes(assetDumpDir + file, b);
                        lock (ToDownloadAssets) ToDownloadAssets.Remove(pa);
                    }
                }
            }
            Success("Awaiting Linkset of " + PrimWaitingLinkset.Count + " objects");
            Success("Awaiting TaskAsset of " + TaskAssetWaiting.Count + " assets");
            Success("Awaiting DL of " + ToDownloadAssets.Count + " assets");
            return res;
        }

        private void EnsureTaskInvHolder()
        {
            var rid = Client.Inventory.Store.RootFolder.UUID;
            if (inventoryHolderUUID != UUID.Zero) return;
            List<InventoryBase> cnt = null;
            while (cnt == null)
            {
                cnt = Client.Inventory.FolderContents(rid, Client.Self.AgentID, true, false, InventorySortOrder.ByDate,
                                                      10000);
            }

            foreach (var c in cnt)
            {
                if (c.Name == "TaskInvHolder")
                {
                    inventoryHolderUUID = c.UUID;
                    return;
                }
            }
            inventoryHolderUUID = Client.Inventory.CreateFolder(rid, "TaskInvHolder");
        }

        private void LocalFailure(string s, object[] args)
        {
            LocalFailures++;
            Failure(DLRConsole.SafeFormat(s, args));
        }

        public static void ExportPrim(BotClient Client, SimObject exportPrim, OutputDelegate Failure)
        {
            WorldObjects.EnsureSelected(exportPrim.LocalID, exportPrim.GetSimulator());
            string pathStem = Path.Combine(dumpDir, exportPrim.ID.ToString());

            string issues = exportPrim.Missing;
            if (!string.IsNullOrEmpty(issues))
            {
                Failure("Missing " + issues + " " + named(exportPrim));
                return;
            }
            ExportPrimReal(Client, pathStem, exportPrim, Failure);
            if (exportPrim.IsRoot && exportPrim.Children.Count > 1)
            {
                SaveLinksetInfo(Client, pathStem, exportPrim, Failure);
            }
            SaveTaskInv(Client, pathStem, exportPrim, Failure);
            AddRelatedTextures(exportPrim);
            SaveRelatedAssets(pathStem, exportPrim);
        }

        private static void SaveRelatedAssets(string pathStem, SimObject exportPrim)
        {
            string file = pathStem + ".deps";
            if (Incremental) lock (fileWriterLock) if (File.Exists(file)) return;
            if (PrimDepsAssets.Count == 0)
            {
                lock (fileWriterLock) File.WriteAllText(file, "");
                return;
            }
            string content = "";
            foreach (UUID type in PrimDepsAssets)
            {
                content += assetTypeOf(type) + "," + type + "\n";
            }
            lock (fileWriterLock) File.WriteAllText(file, content);
        }

        private static AssetType assetTypeOf(UUID uuid)
        {
            AssetType type;
            AllRelatedAssets.TryGetValue(uuid, out type);
            return type;
        }

        static void SaveLinksetInfo(BotClient Client, string pathStem, SimObject exportPrim, OutputDelegate Failure)
        {
            if (Incremental) lock (fileWriterLock) if (File.Exists(pathStem + ".link")) return;
            if (exportPrim.Children.Count < 2)
            {
                // so we dont do it again
                if (Incremental) lock (fileWriterLock) File.WriteAllText(pathStem + ".link", "");
                return;
            }
            lock (PrimWaitingLinkset)
            {
                if (PrimWaitingLinkset.ContainsKey(exportPrim.ID)) return;
                PrimWaitingLinkset.Add(exportPrim.ID, new SO { S = "", O = exportPrim, F = pathStem });
            }
            PutLinkSpeaker(Client, exportPrim);
        }

        private static void listen_forLinkset(object sender, ChatEventArgs e)
        {
            if (e.Type != ChatType.OwnerSay) return;
            UUID sourceId = e.SourceID;
            string eMessage = e.Message;
            lock (PrimWaitingLinkset)
            {
                SO so;
                if (!PrimWaitingLinkset.TryGetValue(sourceId, out so))
                {
                    return;
                }
                if (so.S == "")
                {
                    if (!eMessage.StartsWith("Y,") || eMessage.StartsWith("\u2127"))
                    {
                        lock (PrimWaitingLinkset)
                        {
                            PrimWaitingLinkset.Remove(sourceId);
                        }
                        throw new InvalidOperationException("wrong message came first " + so + " was " + eMessage);
                    }
                }
                else
                {
                    if (eMessage.StartsWith("Y,"))
                    {
                        lock (PrimWaitingLinkset)
                        {
                            PrimWaitingLinkset.Remove(sourceId);
                        }
                        throw new InvalidOperationException("new message came to " + so + " was " + eMessage);
                    }
                }
                so.S = so.S + eMessage;
                if (eMessage.EndsWith(",Z"))
                {
                    lock (PrimWaitingLinkset)
                    {
                        PrimWaitingLinkset.Remove(sourceId);
                    }
                    lock (fileWriterLock) File.WriteAllText(so.F + ".link", so.S);
                }
            }
        }

        private static void PutLinkSpeaker(BotClient Client, SimObject exportPrim)
        {
            InventoryItem found = GetLinkSpeaker(Client);
            Client.Inventory.CopyScriptToTask(exportPrim.LocalID, (InventoryItem)found, true);
            Client.Inventory.RequestSetScriptRunning(exportPrim.ID, found.AssetUUID, true);
        }

        public static InventoryItem GetLinkSpeaker(GridClient Client)
        {
            if (linkSpeaker != null) return linkSpeaker;
            Client.Inventory.FolderContents(Client.Inventory.FindFolderForType(AssetType.LSLText), Client.Self.AgentID,
                                            false, true, InventorySortOrder.ByName, 10000);
            foreach (var item in Client.Inventory.Store.GetContents(Client.Inventory.FindFolderForType(AssetType.LSLText)))
            {
                if (item.Name == "LinksetSpeaker")
                {
                    linkSpeaker = item as InventoryItem;
                    break;
                }
            }
            return linkSpeaker;
        }

        public static void ExportPrimReal(BotClient Client, string pathStem, SimObject exportPrim, OutputDelegate Failure)
        {
            if (exportPrim != null)
            {
                string llsdFile = pathStem + ".llsd";
                if (Incremental) lock (fileWriterLock) if (File.Exists(llsdFile)) return;

                var Properties = exportPrim.Properties;
                if (Properties == null)
                {
                    Client.Objects.RequestObjectPropertiesFamily(exportPrim.GetSimulator(), exportPrim.ID, true);
                    Failure("No props yet for " + named(exportPrim));
                    return;
                }
                // Check for export permission first
                //GotPermissions = false;
                //
                //if (!GotPermissions)
                // {
                //   Properties = exportPrim.Properties ?? new Primitive.ObjectProperties();
                //}
                //   GotPermissionsEvent.WaitOne(1000 * 10, false);
                if (Properties.OwnerID != Client.Self.AgentID &&
                    Properties.OwnerID != Client.MasterKey &&
                    Properties.GroupID != Client.Self.ActiveGroup)
                {
                    Failure("That object is owned by " + Properties.OwnerID + ", we don't have permission " +
                            "to export " + named(exportPrim));
                }

                List<SimObject> family = new List<SimObject>();
                family.Add(exportPrim);
                //family.AddRange(exportPrim.Children);

                /*bool complete = RequestObjectProperties(family, 250, exportPrim.GetSimulator());
                exportedPrims.AddRange(family);

                if (!complete)
                {
                    Logger.Log("Warning: Unable to retrieve full properties for:", Helpers.LogLevel.Warning, Client);
                    foreach (UUID uuid in PrimsWaiting.Keys)
                        Logger.Log(uuid.ToString(), Helpers.LogLevel.Warning, Client);
                }
                */
                OSD primOSD = exportPrim.Prim.GetOSD();
                string output = OSDParser.SerializeLLSDXmlString(primOSD);
                try
                {
                    lock (fileWriterLock) File.WriteAllText(llsdFile, output);
                }
                catch (Exception e)
                {
                    Failure("Writing file " + llsdFile);
                }
            }
        }

        public static string named(SimObject prim)
        {
            string s = ("" + prim);
            int fp = s.IndexOf(")");
            if (fp < 64) fp = 0;
            if (fp > 0) return s.Substring(0, fp + 1);
            if (s.Length < 100) return s;
            return s.Substring(0, 100);

        }

        static void SaveTaskInv(BotClient Client, string pathStem, SimObject exportPrim, OutputDelegate Failure)
        {
            string invFile = pathStem + ".task";
            if (Incremental) lock (fileWriterLock) if (File.Exists(invFile)) return;
            var ib = exportPrim.TaskInventory;
            if (ib == null)
            {
                Failure("NULL TaskInv for " + named(exportPrim));
                return;
            }
            if (ib.Count == 0)
            {
                if (!exportPrim.InventoryEmpty)
                {
                    ///  Failure("ZEROITEM TaskInv for " + exportPrim);
                    return;
                }
                if (Incremental)
                {
                    lock (fileWriterLock) File.WriteAllText(invFile, "");
                }
                return;
            }
            if (ib.Count == 1)
            {
                if (ib[0].Name == "Contents" && ib[0] is InventoryFolder)
                {
                    if (Incremental)
                    {
                        lock (fileWriterLock) File.WriteAllText(invFile, "");
                    }
                    return;
                }
            }
            string contents = "";
            bool foundObject = false;
            foreach (InventoryBase b in ib)
            {
                string was = SaveTaskItems(Client, exportPrim, b, Failure, ref foundObject);
                lock (NotTaskAssetWaiting) NotTaskAssetWaiting[b] = was;
                contents += was;
            }
            if (!foundObject)
            {
                lock (fileWriterLock) File.WriteAllText(invFile, contents);
            }
            else
            {
                Failure("Skipping writting contents unil Objects can be resolved:\n" + contents);
            }
        }

        static string SaveTaskItems(BotClient Client, SimObject exportPrim, InventoryBase b, OutputDelegate Failure, ref bool foundObject)
        {
            lock (NotTaskAssetWaiting)
            {
                string was;
                if (NotTaskAssetWaiting.TryGetValue(b, out was)) return was;
            }
            string primName = " from " + named(exportPrim);
            //primName = "";
            InventoryFolder fldr = b as InventoryFolder;
            if (fldr != null)
            {
                if (fldr.Name == "Contents")
                {
                    return "";
                }
                //  Success("Folder " + fldr.Name + primName);

                //                List<InventoryBase> currentContents = Client.Inventory.GetContents(fldr);
                //              fldr
                return "Folder," + UUID.Zero + "," + fldr.Name + "\n";
            }
            InventoryItem item = b as InventoryItem;
            if (item.InventoryType == InventoryType.Object)
            {
                Failure("Cant get taskinv object " + item.Name + primName);
                foundObject = true;
                return item.AssetType + "," + item.AssetUUID + "," + item.Name + "\n";
                //todo
                Client.Inventory.MoveTaskInventory(exportPrim.LocalID, item.UUID, inventoryHolderUUID, exportPrim.GetSimulator());

            }
            lock (TaskAssetWaiting)
            {
                if (TaskAssetWaiting.ContainsKey(b))
                {
                    return item.AssetType + "," + item.AssetUUID + "," + item.Name + "\n";
                }
            }
            AssetManager.AssetReceivedCallback rec = (trans, asset) =>
                                                         {
                                                             if (trans.AssetID != item.AssetUUID) return;
                                                             if (!trans.Success)
                                                             {
                                                                 lock (TaskAssetWaiting)
                                                                 {
                                                                     IHO iho;
                                                                     if (TaskAssetWaiting.TryGetValue(b, out iho))
                                                                     {
                                                                         SlowlyDo(() =>
                                                                                  Client.Assets.RequestInventoryAsset(
                                                                                      item.AssetUUID, item.UUID,
                                                                                      exportPrim.ID, item.OwnerID,
                                                                                      item.AssetType, true, iho.H));
                                                                         return;
                                                                     }
                                                                 }
                                                             }
                                                             Assets_OnReceived(trans, asset);
                                                             lock (TaskAssetWaiting)
                                                                 TaskAssetWaiting.Remove(b);
                                                         };
            AddRelated(item.AssetUUID, item.AssetType);
            lock (TaskAssetWaiting)
                TaskAssetWaiting.Add(b, new IHO { I = b, H = rec, O = exportPrim });
            SlowlyDo(() =>
                     Client.Assets.RequestInventoryAsset(item.AssetUUID, item.UUID, exportPrim.ID, item.OwnerID,
                                                         item.AssetType, true, rec));
            return item.AssetType + "," + item.AssetUUID + "," + item.Name + "\n";
            /*if (!are.WaitOne(10000))
            {
                Failure("Cant get taskinv " + item.InventoryType + " " + item.Name + primName);
            }  else
            {
                // Success("Received taskinv " + item.InventoryType + " " + item.Name + primName);
            }*/
        }

        private static void SlowlyDo(ThreadStart action)
        {
            slowlyExport.Enqueue(action);
        }


        /// <summary>
        /// Loads in inventory cache file into the inventory structure. Note only valid to call after login has been successful.
        /// </summary>
        /// <param name="filename">Name of the cache file to load</param>
        /// <returns>The number of inventory items sucessfully reconstructed into the inventory node tree</returns>
        public List<InventoryNode> RestoreFromDisk(string filename)
        {
            List<InventoryNode> nodes = new List<InventoryNode>();
            int item_count = 0;

            lock (fileWriterLock)
            {
                try
                {
                    if (!File.Exists(filename))
                        return null;

                    using (Stream stream = File.Open(filename, FileMode.Open))
                    {
                        BinaryFormatter bformatter = new BinaryFormatter();

                        while (stream.Position < stream.Length)
                        {
                            OpenMetaverse.InventoryNode node = (InventoryNode)bformatter.Deserialize(stream);
                            nodes.Add(node);
                            item_count++;
                        }
                    }
                }
                catch (Exception e)
                {
                    Logger.Log("Error accessing inventory cache file :" + e.Message, Helpers.LogLevel.Error);
                    return null;
                }
            }
            return nodes;
        }


        /// <summary>
        /// Saves the current inventory structure to a cache file
        /// </summary>
        /// <param name="filename">Name of the cache file to save to</param>
        static public void SaveToDisk(string filename, Object item)
        {
            lock (fileWriterLock)
            {
                try
                {
                    using (Stream stream = File.Open(filename, FileMode.Create))
                    {
                        BinaryFormatter bformatter = new BinaryFormatter();
                        lock (item)
                        {
                            bformatter.Serialize(stream, item);
                        }
                    }
                }
                catch (Exception e)
                {
                    Logger.Log("Error saving inventory cache to disk :" + e.Message, Helpers.LogLevel.Error);
                }
            }
        }

        public CmdResult ExportRelatedAssets()
        {


            // Create a list of all of the textures to download
            List<ImageRequest> textureRequests = new List<ImageRequest>();
            Dictionary<UUID, AssetType> otherRequests = new Dictionary<UUID, AssetType>();


            // Create a request list from all of the images
            lock (ToDownloadAssets)
            {
                foreach (var asset in ToDownloadAssets)
                {
                    if (assetTypeOf(asset) == AssetType.Texture)
                    {
                        textureRequests.Add(new ImageRequest(asset, ImageType.Normal, 1013000.0f, 0));
                    }
                    else
                    {
                        otherRequests.Add(asset, assetTypeOf(asset));
                    }
                }
            }

            // Download all of the textures in the export list
            foreach (ImageRequest request in textureRequests)
            {
                SlowlyDo(() => Client.Assets.RequestImage(request.ImageID, request.Type, Assets_OnImageReceived));
            }

            foreach (KeyValuePair<UUID, AssetType> asset in otherRequests)
            {
                if (asset.Value != AssetType.Texture)
                {
                    SlowlyDo(() => Client.Assets.RequestAsset(asset.Key, asset.Value, true, Assets_OnReceived));
                }
            }

            return Success("XML exported, began downloading " + ToDownloadAssets.Count + " textures for " + exportedPrims.Count);

        }

        static void AddRelatedTextures(SimObject simObject)
        {
            var exportPrim = simObject.Prim;
            lock (ToDownloadAssets)
            {
                AddRelated(exportPrim.Textures.DefaultTexture.TextureID, AssetType.Texture);

                for (int j = 0; j < exportPrim.Textures.FaceTextures.Length; j++)
                {
                    if (exportPrim.Textures.FaceTextures[j] != null)
                    {
                        AddRelated(exportPrim.Textures.FaceTextures[j].TextureID, AssetType.Texture);
                    }
                }

                if (exportPrim.Sculpt != null)
                {
                    AddRelated(exportPrim.Sculpt.SculptTexture, AssetType.Texture);
                }
                AddRelatedTexturesFromProps(simObject);
            }
        }

        static void AddRelatedTexturesFromProps(SimObject simObject)
        {
            UUID[] simObjectPropertiesTextureIDs = simObject.Properties.TextureIDs;
            if (simObjectPropertiesTextureIDs == null || simObjectPropertiesTextureIDs.Length == 0) return;
            foreach (var c in simObjectPropertiesTextureIDs)
            {
                AddRelated(c, AssetType.Texture);
            }
        }

        public static void AddRelated(UUID textureID, AssetType type)
        {
            if (textureID == null || textureID == UUID.Zero) return;
            lock (PrimDepsAssets)
                if (textureID != Primitive.TextureEntry.WHITE_TEXTURE && !PrimDepsAssets.Contains(textureID))
                {
                    PrimDepsAssets.Add(textureID);
                }
            lock (AllRelatedAssets)
                if (textureID != Primitive.TextureEntry.WHITE_TEXTURE && !AllRelatedAssets.ContainsKey(textureID))
                {
                    AllRelatedAssets.Add(textureID, type);
                    lock (ToDownloadAssets)
                        if (textureID != Primitive.TextureEntry.WHITE_TEXTURE && !ToDownloadAssets.Contains(textureID))
                        {
                            ToDownloadAssets.Add(textureID);
                        }
                }
        }

        /*

        private bool RequestObjectProperties(IList<SimObject> objects, int msPerRequest, Simulator sim)
        {
            // Create an array of the local IDs of all the prims we are requesting properties for
            uint[] localids = new uint[objects.Count];

            lock (PrimsWaiting)
            {
                PrimsWaiting.Clear();

                for (int i = 0; i < objects.Count; ++i)
                {
                    localids[i] = objects[i].LocalID;
                    PrimsWaiting.Add(objects[i].ID, objects[i].Prim);
                }
            }

            Client.Objects.SelectObjects(sim, localids);

            return AllPropertiesReceived.WaitOne(2000 + msPerRequest * objects.Count, false);
        }
        */
        private void Assets_OnImageReceived(TextureRequestState state, AssetTexture asset)
        {

            lock (ToDownloadAssets) if (state == TextureRequestState.Finished && ToDownloadAssets.Contains(asset.AssetID))
                {
                    lock (ToDownloadAssets)
                        ToDownloadAssets.Remove(asset.AssetID);



                    if (state == TextureRequestState.Finished)
                    {
                        string sfile = Path.GetFileName(SimAsset.CFileName(asset.AssetID, asset.AssetType));
                        try { lock (fileWriterLock) File.WriteAllBytes(assetDumpDir + sfile, asset.AssetData); }
                        catch (Exception ex) { Logger.Log(ex.Message, Helpers.LogLevel.Error, Client); }
                        return;

                        if (asset.Decode())
                        {
                            try { lock (fileWriterLock) File.WriteAllBytes(assetDumpDir + asset.AssetID + ".tga", asset.Image.ExportTGA()); }
                            catch (Exception ex) { Logger.Log(ex.Message, Helpers.LogLevel.Error, Client); }
                        }
                        else
                        {
                            try { lock (fileWriterLock) File.WriteAllBytes(assetDumpDir + sfile, asset.AssetData); }
                            catch (Exception ex) { Logger.Log(ex.Message, Helpers.LogLevel.Error, Client); }
                            return;
                        }

                        Logger.Log("Finished downloading image " + asset.AssetID, Helpers.LogLevel.Info, Client);
                    }
                    else
                    {
                        Logger.Log("Failed to download image " + asset.AssetID + ":" + state, Helpers.LogLevel.Warning, Client);
                    }
                }
        }

        private static void Asset_Xfer(object sender, XferReceivedEventArgs e)
        {
            var assetID = e.Xfer.AssetID;
            var assetType = e.Xfer.AssetType;
            lock (ToDownloadAssets) if (e.Xfer.Success && ToDownloadAssets.Contains(assetID))
                {
                    lock (ToDownloadAssets)
                        ToDownloadAssets.Remove(assetID);
                    string sfile = SimAsset.CFileName(assetID, assetType);
                    try
                    {
                        lock (fileWriterLock)
                            File.WriteAllBytes(assetDumpDir + Path.GetFileName(sfile), e.Xfer.AssetData);
                    }
                    catch (Exception ex)
                    {
                        Logger.Log(ex.Message, Helpers.LogLevel.Error);
                    }
                }
        }

        static void Assets_OnReceived(AssetDownload transfer, Asset asset)
        {
            lock (ToDownloadAssets) if (transfer.Success && ToDownloadAssets.Contains(asset.AssetID))
                {
                    lock (ToDownloadAssets)
                        ToDownloadAssets.Remove(asset.AssetID);
                    string sfile = SimAsset.CFileName(asset.AssetID, asset.AssetType);
                    try
                    {
                        lock (fileWriterLock)
                            File.WriteAllBytes(assetDumpDir + Path.GetFileName(sfile), asset.AssetData);
                    }
                    catch (Exception ex)
                    {
                        Logger.Log(ex.Message, Helpers.LogLevel.Error);
                    }
                }
        }

        /*
        void Objects_OnObjectPropertiesFamily(object sender, ObjectPropertiesFamilyEventArgs e)
        {            
            Properties.SetFamilyProperties(e.Properties);
            GotPermissions = true;
            GotPermissionsEvent.Set();
        }

        void Objects_OnObjectProperties(object sender, ObjectPropertiesEventArgs e)
        {
            lock (PrimsWaiting)
            {
                PrimsWaiting.Remove(e.Properties.ObjectID);

                if (PrimsWaiting.Count == 0)
                    AllPropertiesReceived.Set();
            }
        }*/
    }
}
