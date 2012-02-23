using System;
using System.Collections;
using System.Collections.Generic;
using System.Reflection;
using System.Threading;
using System.IO;
using cogbot.Actions.SimExport;
using cogbot.Listeners;
using cogbot.TheOpenSims;
using MushDLR223.Utilities;
using OpenMetaverse;
using OpenMetaverse.Assets;
using OpenMetaverse.StructuredData;

using MushDLR223.ScriptEngines;

namespace cogbot.Actions.SimExport
{
    public class ImportCommand : Command, RegionMasterCommand
    {
        private enum ImporterState
        {
            RezzingParent,
            RezzingChildren,
            Linking,
            Idle
        }
        public enum PrimImportState : uint
        {
            Unloaded,
            LoadedLLSD,
            RezRequested,
            NewPrimFound,
            PrimPropertiesSet,
            Linking,
            PostLinkProperiesSet,
            DependantTexturesConfirmed,
            DependantTaskAssetsUploaded,
            TaskInventoryCreated,
            TaskInventoryConfirmed,
            RepackagingComplete
        }
        private class Linkset
        {
            public Primitive RootPrim;
            public List<Primitive> Children = new List<Primitive>();

            public Linkset()
            {
                RootPrim = new Primitive();
            }

            public Linkset(Primitive rootPrim)
            {
                RootPrim = rootPrim;
            }
        }
        public class UUIDChange
        {
            public UUID NewID { get; set; }
            public UUID OldID { get; set; }

        }

        public class ItemToCreate : UUIDChange 
        {
            public override string ToString()
            {
                return assetType + " " + OldID + " -> " + NewID;
            }
            public override int GetHashCode()
            {
                return OldID.GetHashCode();
            }
            public override bool Equals(object obj)
            {
                var ptc = obj as ItemToCreate;
                return ptc != null && Item.AssetID == ptc.Item.AssetID;
            }
            public ItemToCreate(Asset item)
            {
                Item = item;
                OldID = item.AssetID;
                assetType = item.AssetType;
                LoadProgressFile();
            }
            public ItemToCreate(UUID oldID, AssetType type)
            {
                OldID = oldID;
                assetType = type;
                LoadProgressFile();

            }
            public Asset Item
            {
                get
                {                    
                    return _item;
                }
                set
                {
                    _item = value;
                }
            }

            public string LLSDFilename
            {
                get
                {
                    string sfile = SimAsset.CFileName(OldID, assetType);
                    _afiler = ExportCommand.assetDumpDir + Path.GetFileName(sfile);
                    return _afiler;
                }
                set
                {
                    _afiler = value;
                }
            }

            private SimAsset _rezed;
            private AssetType assetType;
            //public uint NewLocalID;
            public Asset NewItem
            {
                get
                {
                    var r = Rezed;
                    if (r == null) return null;
                    return r.ServerAsset;
                }
            }

            public bool RezRequested = false;
            private Asset _item;
            private string _progressFile;
            private string _afiler;

            public string ProgressFile
            {
                get
                {
                    if (_progressFile == null)
                    {
                        _progressFile = ExportCommand.assetDumpDir + OldID + ".simasset";
                    }
                    return _progressFile;
                }
            }
            public SimAsset Rezed
            {
                get
                {
                    if (_rezed == null)
                    {
                        LoadProgressFile();
                        _rezed = SimAssetStore.FindAsset(NewID);
                    }
                    return _rezed;
                }
                set
                {
                    if (value == null) return;
                    RezRequested = true;
                    _rezed = value;
                    NewID = value.ID;
                    //NewLocalID = value.LocalID;
                    File.WriteAllText(ProgressFile, "" + NewID);
                }
            }

            private void LoadProgressFile()
            {
                if (CogbotHelpers.IsNullOrZero(NewID))
                {
                    string importProgress = ProgressFile;
                    if (File.Exists(importProgress))
                    {
                        string data = File.ReadAllText(importProgress);
                        var sdata = data.Split(',');
                        NewID = UUIDFactory.GetUUID(sdata[0]);
                       // NewLocalID = uint.Parse(sdata[1]);
                        RezRequested = true;
                    }
                }
            }

            public void UploadAssetData()
            {
                if (CogbotHelpers.IsNullOrZero(NewID))
                {
                    string importProgress = LLSDFilename;                    
                    UUID newID;
                    Running.Client.Assets.RequestUpload(out newID, assetType, AssetData, false);
                    RezRequested = true; 
                    NewID = newID;
                }
            }

            public void WriteComplete()
            {
                File.WriteAllText(ProgressFile, NewID + "," + OldID + "," + assetType);
            }

            private byte[] assetData;
            public byte[] AssetData
            {
                get
                {
                    if (assetData == null) assetData = File.ReadAllBytes(LLSDFilename);
                    return assetData;
                }
            }

            public void OnDownloaded(AssetDownload transfer, Asset asset)
            {
                if (!transfer.Success)
                {
                    ImportCommand.Running.Error("bad transfer");
                }
            }

            public void ConfirmAsset(byte[] data)
            {
                Running.Client.Assets.RequestUploadKnown(NewID, assetType, data, false, UUID.Zero);
            }
        }

        public class PrimToCreate : UUIDChange
        {
            public override string ToString()
            {
                return Prim + " -> " + _rezed;
            }
            public override int GetHashCode()
            {
                return Prim.ID.GetHashCode();
            }
            public override bool Equals(object obj)
            {
                var ptc = obj as PrimToCreate;
                return ptc != null && Prim.ID == ptc.Prim.ID;
            }
            public PrimToCreate(Primitive prim)
            {
                Prim = prim;
                OldID = prim.ID;
                LoadProgressFile();
            }
            public PrimToCreate(UUID oldID)
            {
                OldID = oldID;
                LoadProgressFile();
            }
            public Primitive Prim
            {
                get
                {
                    if (_prim == null)
                    {
                        _prim = (Primitive)ExportCommand.FromFile(LLSDFilename);
                    }
                    return _prim;
                }
                set
                {
                    _prim = value;
                }
            }

            public string LLSDFilename
            {
                get
                {
                    return ExportCommand.dumpDir + OldID + ".llsd";
                }
            }

            private SimObject _rezed;
            private PrimImportState State = PrimImportState.Unloaded;
            public uint NewLocalID;
            public Primitive NewPrim
            {
                get
                {
                    var r = Rezed;
                    if (r == null) return null;
                    return r.Prim;
                }
            }

            public bool RezRequested = false;
            private Primitive _prim;
            private string _progressFile;
            public bool IsLinkParent;

            public string ProgressFile
            {
                get
                {
                    if (_progressFile == null)
                    {
                        _progressFile = ExportCommand.dumpDir + OldID + ".ptc";
                    }
                    return _progressFile;
                }
            }
            public SimObject Rezed
            {
                get
                {
                    if (_rezed == null)
                    {
                        LoadProgressFile();
                        _rezed = ExportCommand.GetSimObjectFromUUID(NewID);
                    }
                    return _rezed;
                }
                set
                {
                    if (value == null) return;
                    RezRequested = true;
                    _rezed = value;
                    NewID = value.ID;
                    NewLocalID = value.LocalID;
                    File.WriteAllText(ProgressFile, NewID + "," + NewLocalID + "," + value.RegionHandle);
                }
            }

            private void LoadProgressFile()
            {
                if (CogbotHelpers.IsNullOrZero(NewID))
                {
                    string importProgress = ProgressFile;
                    if (File.Exists(importProgress))
                    {
                        string data = File.ReadAllText(importProgress);
                        var sdata = data.Split(',');
                        NewID = UUIDFactory.GetUUID(sdata[0]);
                        NewLocalID = uint.Parse(sdata[1]);
                        RezRequested = true;
                    }

                }
            }

            public void ReplaceAll(Dictionary<UUID, UUID> dictionary)
            {
                ReplaceAllMembers(Prim, typeof (UUID), UUIDReplacer);
            }
        }
        static object UUIDReplacer(object arg)
        {
            UUID other;
            if (ChangeList.TryGetValue((UUID)arg, out other))
            {
                return other;
            }
            return arg;
        }

        Primitive currentPrim;
        Vector3 currentPosition;
        AutoResetEvent primDone = new AutoResetEvent(false);
        List<Primitive> primsCreated;
        List<uint> linkQueue;
        uint rootLocalID;
        private static readonly object WorkFlowLock = new object();
        ImporterState state = ImporterState.Idle;
        EventHandler<PrimEventArgs> callback;
        public static readonly Dictionary<UUID, UUIDChange> UUID2OBJECT = new Dictionary<UUID, UUIDChange>();
        public static readonly Dictionary<uint, PrimToCreate> UINT2OBJECT = new Dictionary<uint, PrimToCreate>();

        public static readonly Dictionary<UUID, UUIDChange> NewUUID2OBJECT = new Dictionary<UUID, UUIDChange>();
        public static readonly Dictionary<uint, PrimToCreate> NewUINT2OBJECT = new Dictionary<uint, PrimToCreate>();
        private List<PrimToCreate> parents;
        private List<PrimToCreate> childs;
        static readonly List<Primitive> diskPrims = new List<Primitive>();


        public ImportCommand(BotClient testClient)
        {
            Name = "simimport";
            Description = "Import prims from an exported xml file. Usage: import inputfile.xml [usegroup]";
            Category = CommandCategory.Objects;
            Client.Assets.AssetUploaded += new EventHandler<AssetUploadEventArgs>(Assets_AssetUploaded);
            ImportCommand.Running = this;
        }

        private void Assets_AssetUploaded(object sender, AssetUploadEventArgs e)
        {
            if (UUID2OBJECT!=null)
            {
                var u = e.Upload;
                if (u.Success)
                {
                    var itc = GetNew(u.AssetID) as ItemToCreate;
                    if (itc != null)
                    {
                        itc.WriteComplete();
                    }
                }
            }
        }


        public override CmdResult Execute(string[] args, UUID fromAgentID, OutputDelegate writeLine)
        {
            const string hlp = @"
            
            Toplevel Directives

            // todo  = shows what must be done for import to be complete (suggest adding verbose)
            // perms  = shows what perms are going to be a problem (suggest adding verbose)
            // clear - clear the export dir
            // reset - reset the exporter state
            // cache - blow away asset cache

            // prims [spec] - do only prims meeting spec (default is prims $region) 
            // incr - do only do what is 'todo'
            // nonincr - do things 'todo' but also 'redo' things already done

            // noperms = dont skip things when perms might be a problem
            // quietly = terser output
            // verbose = more verbose
            // request = will rerequest missing things like textures
            // force = will allow unequal LLSD files - this should only be used as last resort

            // users - load users files 
            // prims - load llsd files 
            // trees - load trees files 
            // terrain - load terrain files 
            // links - operate on linkset
            // deps - operate on dependant assets
            // dl - operate on dependant downloads
            // tasks - save task files
            // taskobj - task objects
            // all = llsd tasks deps links (dl and taskobj not included)           
            ";
            if (args == null || args.Length == 0) return Failure(hlp);
            UUID GroupID = (args.Length > 1) ? TheBotClient.GroupID : UUID.Zero;
            var CurSim = Client.Network.CurrentSim;
            this.arglist = new HashSet<string>(args);
            if (!arglist.Contains("noassets"))
            {
                UploadAllAssets(arglist.Contains("sameid"));                
            }
            ChangeList = GetChangeList();
            WriteLine("ChangeList Size is " + ChangeList.Count);

            parents = new List<PrimToCreate>();
            childs = new List<PrimToCreate>();
            var taskobjs = new List<PrimToCreate>();

            if (diskPrims.Count == 0)
            {
                foreach (var file in Directory.GetFiles(ExportCommand.dumpDir, "*.llsd"))
                {

                    Primitive prim = (Primitive)ExportCommand.FromFile(file);
                    diskPrims.Add(prim);
                    PrimToCreate ptc = APrimToCreate(prim);
                    if (prim.ParentID != 0)
                    {
                        childs.Add(ptc);
                        continue;
                    }
                    parents.Add(ptc);
                    CreatePrim(CurSim, ptc, GroupID);
                }
            }
            else
            {
                foreach (Primitive prim in diskPrims)
                {
                    PrimToCreate ptc = APrimToCreate(prim);
                    if (prim.ParentID != 0)
                    {
                        childs.Add(ptc);
                        CreatePrim(CurSim, ptc, GroupID);
                        continue;
                    }
                    parents.Add(ptc);
                }
            }
            WriteLine("Imported parents " + parents.Count);
            foreach (PrimToCreate ptc in parents)
            {
                CreatePrim(CurSim, ptc, GroupID);
            }
            WriteLine("Importing children " + childs.Count);
            foreach (PrimToCreate ptc in childs)
            {
                CreatePrim(CurSim, ptc, GroupID);
            }
            List<string> skipCompare = new List<string>() { "LocalID", "ID", "ParentID", "ObjectID", "Tag" };
            WriteLine("Linking imports"); 
            foreach (var file in Directory.GetFiles(ExportCommand.dumpDir, "*.link"))
            {
                var uuids = ExportCommand.GetUUIDs(File.ReadAllText(file));
                if (uuids.Length < 1) continue;
                UUID idp = uuids[0];
                var parent = GetOldPrim(idp);
                parent.IsLinkParent = true;
                var linkset = new List<uint>(uuids.Length) {parent.NewLocalID};
                for (int i = 1; i < uuids.Length; i++)
                {
                    UUID id = uuids[i];
                    PrimToCreate ptc = GetOldPrim(id);
                    if (ptc == null)
                    {
                        Failure("Relink cant find PTC=" + id);
                        continue;
                    }
                    uint newLocalID = ptc.NewLocalID;
                    if (newLocalID == 0)
                    {
                        Failure("Relink cant find linked prim ID=" + id);
                        continue;
                    }
                    linkset.Add(newLocalID);
                }
                //linkset.Add(UUID2OBJECT[uuids[0]].Rezed.LocalID);
                Client.Objects.LinkPrims(CurSim, linkset);
                linkset.RemoveAt(0);

                if (false)
                {
                    linkset.Reverse();
                    SetPrimsPostLink(CurSim, GroupID, parent, linkset, skipCompare);
                }
            }
            // hopefully unset some phantoms here
            foreach (PrimToCreate ptc in parents)
            {
                SetFlagsAndPhysics(CurSim, ptc.NewLocalID, ptc.Prim);
            }
            WriteLine("Imported P=" + parents.Count + " C=" + childs.Count);
            return SuccessOrFailure();
        }

        private void UploadAllAssets(bool sameIds)
        {
            int uploaded = 0;
            bool alwayReupload = arglist.Contains("reup");
            Success("Uploading assets... sameIds=" + sameIds);
            foreach (var file in Directory.GetFiles(ExportCommand.assetDumpDir, "*.*"))
            {
                if (file.EndsWith(".object") || file.EndsWith(".simasset"))
                {
                    continue;
                }
                string sid = file;
                AssetType assetType = AssetType.Unknown;
                foreach (var ate in ArchiveConstants.ASSET_TYPE_TO_EXTENSION)
                {
                    string ateValue = ate.Value;
                    if (file.EndsWith(ateValue))
                    {
                        assetType = ate.Key;
                        sid = Path.GetFileName(file.Substring(0, file.Length - ateValue.Length));
                        break;
                    }
                }
                if (assetType == AssetType.Unknown)
                {
                    if (file.EndsWith(".jp2"))
                    {
                        assetType = AssetType.Texture;
                        sid = Path.GetFileName(file.Substring(0, file.Length - ".jp2".Length));
                    }
                }
                if (assetType==AssetType.Unknown)
                {
                    Failure("Cant guess assetyype for " + file);
                    continue;
                }
                UUID oldID = UUID.Parse(sid);
                ItemToCreate itc = FindItemToCreate(oldID, assetType, sameIds);
                itc.LLSDFilename = file;
                if (!itc.RezRequested)
                {
                    itc.UploadAssetData();
                    NewUUID2OBJECT[itc.NewID] = itc;
                    uploaded++;
                }
                else if (alwayReupload)
                {
                    itc.ConfirmAsset(itc.AssetData);
                }
                if (!CogbotHelpers.IsNullOrZero(itc.NewID))
                {
                    NewUUID2OBJECT[itc.NewID] = itc;
                }
            }
            Success("Uploaded assets: " + uploaded);
        }

        private ItemToCreate FindItemToCreate(UUID uuid, AssetType assetType, bool sameIds)
        {
            var itc = GetOld(uuid) as ItemToCreate;
            if (itc == null)
            {
                itc = new ItemToCreate(uuid, assetType);
                UUID2OBJECT[uuid] = itc;
                if (!CogbotHelpers.IsNullOrZero(itc.NewID))
                {
                    NewUUID2OBJECT[itc.NewID] = itc;
                }
            }
            return itc;
        }

        private void SetPrimsPostLink(Simulator CurSim, UUID GroupID, PrimToCreate parent, IEnumerable<uint> linkset, ICollection<string> skipCompare)
        {
            foreach (uint u in linkset)
            {
                var ptc = GetNewPrim(u);
                if (ptc == null)
                {
                    Failure("Missing New LocalID " + u);
                    continue;
                }
                var prim = ptc.Prim;
                if (prim == null)
                {
                    Failure("Missing Old Prim " + u);
                    continue;
                }
                SetPrimInfo(CurSim, u, prim, GroupID, true, false, false);
                string diff = ExportCommand.MemberwiseCompare(prim, ptc.NewPrim, skipCompare);
                if (string.IsNullOrEmpty(diff)) Failure("after rez: " + diff);

                //Client.Objects.SetPosition(CurSim, u, prim.Position);
                //Client.Objects.SetRotation(CurSim, u, prim.Rotation);
            }
        }

        public PrimToCreate APrimToCreate(Primitive primitive)
        {
            lock (WorkFlowLock)
            {
                PrimToCreate ptc;
                if (UINT2OBJECT.TryGetValue(primitive.LocalID, out ptc)) return ptc;
                UUIDChange utc;
                if (UUID2OBJECT.TryGetValue(primitive.ID, out utc))
                {
                    return (PrimToCreate)utc;
                }
                if (ptc != null) return ptc;
                ptc = new PrimToCreate(primitive);
                ptc.ReplaceAll(ChangeList);
                UUID2OBJECT.Add(ptc.OldID, ptc);
                UINT2OBJECT.Add(primitive.LocalID, ptc);
                return ptc;
            }
        }
        private PrimToCreate GetOldPrim(uint localID)
        {
            lock (WorkFlowLock)
            {
                PrimToCreate ptc;
                if (UINT2OBJECT.TryGetValue(localID, out ptc))
                {
                    return ptc;
                }
                Failure("cant find LocalID=" + localID);
            }
            return null;
        }
        private PrimToCreate GetOldPrim(UUID id)
        {
            PrimToCreate getOld = (PrimToCreate)GetOld(id);
            if (getOld == null) Failure("cant find ID=" + id);
            return getOld;
        }
        private UUIDChange GetOld(UUID id)
        {
            lock (WorkFlowLock)
            {
                UUIDChange ptc;
                if (UUID2OBJECT.TryGetValue(id, out ptc))
                {
                    return (UUIDChange)ptc;
                }
            }
            return null;
        }
        private UUID GetChange(UUID id)
        {
            lock (WorkFlowLock)
            {
                UUIDChange ptc;
                if (UUID2OBJECT.TryGetValue(id, out ptc))
                {
                    return ptc.NewID;
                }
            }
            return null;
        }
        private Dictionary<UUID,UUID> GetChangeList()
        {
            Dictionary<UUID, UUID> cl = new Dictionary<UUID, UUID>();
            lock (WorkFlowLock)
            {
                foreach (KeyValuePair<UUID, UUIDChange> o in UUID2OBJECT)
                {
                    cl.Add(o.Key, o.Value.NewID);
                }
            }
            return cl;
        }
        private PrimToCreate GetNewPrim(uint localID)
        {
            lock (WorkFlowLock)
            {
                PrimToCreate ptc;
                if (NewUINT2OBJECT.TryGetValue(localID, out ptc))
                {
                    return ptc;
                }
                WriteLine("cant find LocalID=" + localID);
            }
            return null;
        }
        private UUIDChange GetNew(UUID id)
        {
            lock (WorkFlowLock)
            {
                UUIDChange ptc;
                if (NewUUID2OBJECT.TryGetValue(id, out ptc))
                {
                    return ptc;
                }
                WriteLine("cant find ID=" + id);
            }
            return null;
        }
        private void CreatePrim(Simulator CurSim, PrimToCreate ptc, UUID GroupID)
        {
            if (ptc.RezRequested) return;
            Primitive prim = ptc.Prim;
            Primitive newPrim = null;
            UUID found = UUID.Zero;
            if (prim.PrimData.PCode != PCode.Prim)
            {
                CreateTree(CurSim, ptc, GroupID);
                return;
            }
            InventoryItem invItem = ExportCommand.GetInvItem(Client, "BlankPrim", AssetType.Object);
            UUID queryID = UUID.Random();
            // Register a handler for the creation event
            AutoResetEvent creationEvent = new AutoResetEvent(false);
            EventHandler<ChatEventArgs> callback =
                (s, e) => //delegate(Simulator simulator0, Primitive prim, ulong regionHandle, ushort timeDilation)
                {
                    var regionHandle = e.Simulator.Handle;
                    if (regionHandle != CurSim.Handle) return;
                    if (e.Type != ChatType.OwnerSay) return;
                    UUID sourceId = e.SourceID;
                    string fromWho = e.FromName;
                    string eMessage = e.Message;
                    if (fromWho == "RegionSay4200")
                    {
                        int findC = eMessage.IndexOf(":");
                        string fu = eMessage.Substring(0, findC);
                        UUID.TryParse(fu, out sourceId);
                        eMessage = eMessage.Substring(findC + 1).TrimStart();
                    }
                    const string listenFor = "REZBLANK:";
                    if (!eMessage.StartsWith(listenFor))
                    {
                        return;
                    }
                    eMessage = eMessage.Substring(listenFor.Length).TrimStart();
                    UUID id;
                    if (!UUID.TryParse(eMessage, out id))
                    {
                        creationEvent.Set();
                        Error("cant get UUID from " + eMessage);
                        return;
                    }
                    found = id;
                    creationEvent.Set();
                };

            Vector3 pp = prim.Position;
            Quaternion pr = prim.Rotation;
            if (prim.ParentID != 0)
            {
                var parent = GetOldPrim(prim.ParentID);
                if (parent == null)
                {
                    pp += new Vector3(128, 128, Client.Self.SimPosition.Z + 20);
                    Failure("Cant GET parent of " + prim);
                }
                else
                {
                    pp = prim.Position*Matrix4.CreateFromQuaternion(parent.Prim.Rotation) + parent.Prim.Position;
                    pr = parent.Prim.Rotation*pr;
                }
            }

            Client.Self.ChatFromSimulator += callback;
            Client.Inventory.RequestRezFromInventory(CurSim, pr, pp, invItem, GroupID, queryID, true);
            ptc.RezRequested = true;
            //AddPrim(CurSim, prim.PrimData, GroupID, prim.Position, prim.Scale, prim.Rotation, PrimFlags.CreateSelected | PrimFlags.Phantom | PrimFlags.Temporary);
            if (!creationEvent.WaitOne(10000))
            {
                Client.Self.ChatFromSimulator -= callback;
                Debug("Error - no uuid of prim ");
                return;
            }
            Client.Self.ChatFromSimulator -= callback;
            ptc.NewID = found;
            var O = ExportCommand.GetSimObjectFromUUID(found);
            if (O == null)
            {
                Error("Error - no SimObject");
                return;
            }
            newPrim = O.Prim;
            if (newPrim == null)
            {
                Error("Error - no newPrim");
                return;
            }
            ptc.Rezed = O;
            SavePTC(ptc);
            uint localID = newPrim.LocalID;            
            SetPrimInfo(CurSim, localID, ptc.Prim, GroupID, false, false, true);
        }

        private PrimToCreate prev = null;
        public static ImportCommand Running;
        static private Dictionary<UUID, UUID> ChangeList;
        private HashSet<string> arglist;

        private void CreateTree(Simulator CurSim, PrimToCreate ptc, UUID GroupID)
        {
            AutoResetEvent foundObj = new AutoResetEvent(false);
            var prim = ptc.Prim;
            PCode pcode = prim.PrimData.PCode;
            Primitive newPrim = null;
            // need time for previous rezes to come thru?
            Thread.Sleep(100);
            EventHandler<PrimEventArgs> folliage = new EventHandler<PrimEventArgs>((s, e) =>
                                                                                       {
                                                                                           if (e.Prim.PrimData.PCode != pcode)
                                                                                               return;
                                                                                           //if (e.Prim.TreeSpecies == prim.TreeSpecies)                                                                                               return;
                                                                                           newPrim = e.Prim;
                                                                                           foundObj.Set();
                                                                                       }
                );
            Client.Objects.ObjectUpdate += folliage;
            if (pcode == PCode.Grass)
            {
                Client.Objects.AddGrass(CurSim, prim.Scale, prim.Rotation, prim.Position, (Grass)prim.TreeSpecies, GroupID);
            }
            else
            {
                Client.Objects.AddTree(CurSim, prim.Scale, prim.Rotation, prim.Position, prim.TreeSpecies, GroupID,
                                       pcode == PCode.NewTree);
            }
            if (!foundObj.WaitOne(5000))
            {
                Client.Objects.ObjectUpdate -= folliage;
                Failure(pcode + " not created! " + ptc + " " + prim.TreeSpecies);
                return;
            }
            Client.Objects.ObjectUpdate -= folliage;
            var O = ptc.Rezed = WorldSystem.GetSimObject(newPrim, CurSim);
            uint localID = O.LocalID;
            SavePTC(ptc);
            SetPrimInfo(CurSim, localID, ptc.Prim, GroupID, false, false, true);
            if (prev == null)
            {
                prev = ptc;
            }
            else
            {
                if (prev.Rezed.Prim.Scale.X > 30)
                {
                    Client.Inventory.RequestDeRezToInventory(prev.NewLocalID);
                    prev.RezRequested = false;
                    prev.NewLocalID = 0;
                    prev.NewID = UUID.Zero;
                    File.Delete(prev.ProgressFile);
                }
                prev = ptc;
            }
        }

        private void SavePTC(PrimToCreate ptc)
        {
            lock (WorkFlowLock)
            {
                NewUUID2OBJECT[ptc.NewID] = ptc;
                NewUINT2OBJECT[ptc.NewLocalID] = ptc;
            }
        }

        public static object ReplaceAllMembers(object from, Type ofType, Func<object,object > replacerFunc)
        {
            return ReplaceAllMembers(from, ofType, replacerFunc, new HashSet<object>());
        }
        public static object ReplaceAllMembers(object from, Type ofType, Func<object, object> replacerFunc, HashSet<object> exceptFor)
        {
            if (from == null) return from;
            var fromType = from.GetType();
            if (fromType == ofType)
            {
                var oo = replacerFunc(from);
                return oo;
            }
            if (fromType == typeof(string) || typeof(IConvertible).IsAssignableFrom(fromType)) return from;
            if (from is IList)
            {
                var ic = from as IList;
                for (int i = 0; i < ic.Count; i++)
                {
                    object o = ic[i];
                    var oo = ReplaceAllMembers(o, ofType, replacerFunc, exceptFor);
                    if (ReferenceEquals(oo, o)) continue;
                    ic[i] = oo;
                }
                return from;
            }
            if (exceptFor.Contains(from)) return from;
            exceptFor.Add(from);

            BindingFlags bf = BindingFlags.Instance | BindingFlags.NonPublic | BindingFlags.Public;
            foreach (var info in fromType.GetFields(bf))
            {
                object o = info.GetValue(from);
                var oo = ReplaceAllMembers(o, ofType, replacerFunc, exceptFor);
                if (ReferenceEquals(oo, o)) continue;
                info.SetValue(from, oo);
            }
            foreach (var info in fromType.GetProperties(bf))
            {
                object o = info.GetValue(from, null);
                var oo = ReplaceAllMembers(o, ofType, replacerFunc, exceptFor);
                if (ReferenceEquals(oo, o)) continue;
                info.SetValue(from, oo, null);
            }
            return from;
        }
        public void SetPrimInfo(Simulator CurSim, uint localID, Primitive prim, UUID GroupID, bool postLinked, bool nonPosOnly, bool setScale)
        {

            Vector3 pp = prim.Position;
            Quaternion pr = prim.Rotation;
            if (!postLinked && prim.ParentID != 0)
            {
                var parent = GetOldPrim(prim.ParentID);
                if (parent == null)
                {
                    pp += new Vector3(128, 128, Client.Self.SimPosition.Z + 20);
                    Failure("Cant GET parent of " + prim);
                }
                else
                {
                    pp = prim.Position * Matrix4.CreateFromQuaternion(parent.Prim.Rotation) + parent.Prim.Position;
                    pr = parent.Prim.Rotation * pr;
                }
            } else
            {
                //nonPosOnly = true;
            }
            

            Primitive.ObjectProperties props = prim.Properties;
            List<uint> prims = new List<uint> { localID };
            Client.Objects.SetName(CurSim, localID, props.Name);
            Client.Objects.SetDescription(CurSim, localID, props.Description);
            Client.Objects.SetShape(CurSim, localID, prim.PrimData);
            //Client.Objects.SetExtraParamOff(CurSim, localID, prim.);
            SetFlagsAndPhysics(CurSim, localID, prim);
            if (prim.Textures != null) Client.Objects.SetTextures(CurSim, localID, prim.Textures, prim.MediaURL);
            if (prim.Light != null) Client.Objects.SetLight(CurSim, localID, prim.Light);

            if (!CogbotHelpers.IsNullOrZero(prim.GroupID))
            {
                if (!CogbotHelpers.IsNullOrZero(GroupID)) Client.Objects.SetObjectsGroup(CurSim, prims, GroupID);
            }
            Client.Objects.SetPermissions(CurSim, new List<uint>() { localID },
                                          PermissionWho.Everyone | PermissionWho.Group | PermissionWho.NextOwner,
                                          PermissionMask.All, true);
            if (!nonPosOnly)
            {
                Client.Objects.SetPosition(CurSim, localID, pp, true);
                Client.Objects.SetRotation(CurSim, localID, pr, true);
            }
            Client.Objects.SetSaleInfo(CurSim, localID, props.SaleType, props.SalePrice);
            if (prim.Flexible != null) Client.Objects.SetFlexible(CurSim, localID, prim.Flexible);
            if (prim.Sculpt != null)
            {
                CheckTexture(prim.Sculpt.SculptTexture);                
                Client.Objects.SetSculpt(CurSim, localID, prim.Sculpt);
            }
            Client.Objects.SetMaterial(CurSim, localID, prim.PrimData.Material);
            if (setScale)
            {
                Client.Objects.SetScale(CurSim, localID, prim.Scale, true, false);
            }
        }

        private void CheckTexture(UUID texture)
        {
            var newt = GetNew(texture);
            var oldt = GetOld(texture);
            if (newt != null && oldt == null)
                return;
            return;
        }

        private void SetFlagsAndPhysics(Simulator CurSim, uint localID, Primitive prim)
        {
            var flags = prim.Flags;
            Primitive.PhysicsProperties physics = prim.PhysicsProps;
            if (physics != null)
            {
                Client.Objects.SetFlags(CurSim, localID, FlagSet(flags, PrimFlags.Physics),
                                        FlagSet(flags, PrimFlags.Temporary), FlagSet(flags, PrimFlags.Phantom),
                                        FlagSet(flags, PrimFlags.CastShadows), physics.PhysicsShapeType,
                                        physics.Density, physics.Friction, physics.Restitution,
                                        physics.GravityMultiplier);
            }
            else
            {
                Client.Objects.SetFlagsOnly(CurSim, localID, FlagSet(flags, PrimFlags.Physics),
                                            FlagSet(flags, PrimFlags.Temporary), FlagSet(flags, PrimFlags.Phantom),
                                            FlagSet(flags, PrimFlags.CastShadows));
            }
        }

        private static bool FlagSet(PrimFlags flags, PrimFlags set)
        {
            return (flags & set) == set;
        }

        private void Debug(string s, params object[] ps)
        {
            Client.DisplayNotificationInChat(DLRConsole.SafeFormat(s, ps));
        }
        private void Error(string s, params object[] ps)
        {
            string msg = DLRConsole.SafeFormat(s, ps);
            Client.DisplayNotificationInChat(msg);
            Failure(msg);
            return;
            throw new NotImplementedException(msg);
        }

        public CmdResult Execute2(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
            if (false) if (args.Length < 1)
                    return ShowUsage();// " import inputfile.xml [usegroup]";

            if (callback == null)
            {
                callback = new EventHandler<PrimEventArgs>(Objects_OnNewPrim);
                Client.Objects.ObjectUpdate += callback;
            }
            try
            {
                string filename = args[0];
                UUID GroupID = (args.Length > 1) ? TheBotClient.GroupID : UUID.Zero;
                string xml;
                List<Primitive> prims;

                try { xml = File.ReadAllText(filename); }
                catch (Exception e) { return Failure(e.Message); }

                try { prims = Helpers.OSDToPrimList(OSDParser.DeserializeLLSDXml(xml)); }
                catch (Exception e) { return Failure("failed to deserialize " + filename + ": " + e.Message); }

                // Build an organized structure from the imported prims
                Dictionary<uint, Linkset> linksets = new Dictionary<uint, Linkset>();
                for (int i = 0; i < prims.Count; i++)
                {
                    Primitive prim = prims[i];

                    if (prim.ParentID == 0)
                    {
                        if (linksets.ContainsKey(prim.LocalID))
                            linksets[prim.LocalID].RootPrim = prim;
                        else
                            linksets[prim.LocalID] = new Linkset(prim);
                    }
                    else
                    {
                        if (!linksets.ContainsKey(prim.ParentID))
                            linksets[prim.ParentID] = new Linkset();

                        linksets[prim.ParentID].Children.Add(prim);
                    }
                }

                primsCreated = new List<Primitive>();
                WriteLine("Importing " + linksets.Count + " structures.");

                foreach (Linkset linkset in linksets.Values)
                {
                    if (linkset.RootPrim.LocalID != 0)
                    {
                        Simulator CurSim = WorldSystem.GetSimulator(linkset.RootPrim);
                        state = ImporterState.RezzingParent;
                        currentPrim = linkset.RootPrim;
                        // HACK: Import the structure just above our head
                        // We need a more elaborate solution for importing with relative or absolute offsets
                        linkset.RootPrim.Position = GetSimPosition();
                        linkset.RootPrim.Position.Z += 3.0f;
                        currentPosition = linkset.RootPrim.Position;

                        // Rez the root prim with no rotation
                        Quaternion rootRotation = linkset.RootPrim.Rotation;
                        linkset.RootPrim.Rotation = Quaternion.Identity;

                        Client.Objects.AddPrim(CurSim, linkset.RootPrim.PrimData, GroupID,
                            linkset.RootPrim.Position, linkset.RootPrim.Scale, linkset.RootPrim.Rotation);

                        if (!primDone.WaitOne(10000, false))
                            return Failure("Rez failed, timed out while creating the root prim.");

                        Client.Objects.SetPosition(CurSim, primsCreated[primsCreated.Count - 1].LocalID, linkset.RootPrim.Position);

                        state = ImporterState.RezzingChildren;

                        // Rez the child prims
                        foreach (Primitive prim in linkset.Children)
                        {
                            currentPrim = prim;
                            currentPosition = prim.Position + linkset.RootPrim.Position;

                            Client.Objects.AddPrim(CurSim, prim.PrimData, GroupID, currentPosition,
                                prim.Scale, prim.Rotation);

                            if (!primDone.WaitOne(10000, false))
                                return Failure("Rez failed, timed out while creating child prim.");
                            Client.Objects.SetPosition(CurSim, primsCreated[primsCreated.Count - 1].LocalID, currentPosition);

                        }

                        // Create a list of the local IDs of the newly created prims
                        List<uint> primIDs = new List<uint>(primsCreated.Count);
                        primIDs.Add(rootLocalID); // Root prim is first in list.

                        if (linkset.Children.Count != 0)
                        {
                            // Add the rest of the prims to the list of local IDs
                            foreach (Primitive prim in primsCreated)
                            {
                                if (prim.LocalID != rootLocalID)
                                    primIDs.Add(prim.LocalID);
                            }
                            linkQueue = new List<uint>(primIDs.Count);
                            linkQueue.AddRange(primIDs);

                            // Link and set the permissions + rotation
                            state = ImporterState.Linking;
                            Client.Objects.LinkPrims(CurSim, linkQueue);

                            if (primDone.WaitOne(1000 * linkset.Children.Count, false))
                                Client.Objects.SetRotation(CurSim, rootLocalID, rootRotation);
                            else
                                WriteLine("Warning: Failed to link {0} prims", linkQueue.Count);

                        }
                        else
                        {
                            Client.Objects.SetRotation(CurSim, rootLocalID, rootRotation);
                        }

                        // Set permissions on newly created prims
                        Client.Objects.SetPermissions(CurSim, primIDs,
                            PermissionWho.Everyone | PermissionWho.Group | PermissionWho.NextOwner,
                            PermissionMask.All, true);

                        state = ImporterState.Idle;
                    }
                    else
                    {
                        // Skip linksets with a missing root prim
                        WriteLine("WARNING: Skipping a linkset with a missing root prim");
                    }

                    // Reset everything for the next linkset
                    primsCreated.Clear();
                }
                return Success("Import complete.");
            }
            finally
            {
                Client.Objects.ObjectUpdate -= callback;
                callback = null;
            }
        }

        void Objects_OnNewPrim(object s, PrimEventArgs e)
        {
            Simulator CurSim = e.Simulator;
            Primitive prim = e.Prim;
            if ((prim.Flags & PrimFlags.CreateSelected) == 0)
                return; // We received an update for an object we didn't create

            uint localID = prim.LocalID;
            switch (state)
            {
                case ImporterState.RezzingParent:
                    rootLocalID = localID;
                    goto case ImporterState.RezzingChildren;
                case ImporterState.RezzingChildren:
                    if (!primsCreated.Contains(prim))
                    {
                        WriteLine("Setting properties for " + localID);
                        // TODO: Is there a way to set all of this at once, and update more ObjectProperties stuff?
                        Client.Objects.SetPosition(CurSim, localID, currentPosition);
                        Client.Objects.SetTextures(CurSim, localID, currentPrim.Textures);

                        if (currentPrim.Light.Intensity > 0)
                        {
                            Client.Objects.SetLight(CurSim, localID, currentPrim.Light);
                        }

                        Client.Objects.SetFlexible(CurSim, localID, currentPrim.Flexible);

                        if (currentPrim.Sculpt.SculptTexture != UUID.Zero)
                        {
                            Client.Objects.SetSculpt(CurSim, localID, currentPrim.Sculpt);
                        }

                        if (!String.IsNullOrEmpty(currentPrim.Properties.Name))
                            Client.Objects.SetName(CurSim, localID, currentPrim.Properties.Name);
                        if (!String.IsNullOrEmpty(currentPrim.Properties.Description))
                            Client.Objects.SetDescription(CurSim, localID, currentPrim.Properties.Description);

                        primsCreated.Add(prim);
                        primDone.Set();
                    }
                    break;
                case ImporterState.Linking:
                    lock (linkQueue)
                    {
                        int index = linkQueue.IndexOf(localID);
                        if (index != -1)
                        {
                            linkQueue.RemoveAt(index);
                            if (linkQueue.Count == 0)
                                primDone.Set();
                        }
                    }
                    break;
            }
        }
    }
}
