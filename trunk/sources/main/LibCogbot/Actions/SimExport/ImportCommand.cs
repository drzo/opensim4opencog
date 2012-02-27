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
        public class UserOrGroupMapping : UUIDChange
        {
            public bool IsGroup = false;
            public string OldName;
            public string NewName;
            public UserOrGroupMapping(UUID id, String name, bool isGroup)
            {
                OldID = id;
                IsGroup = isGroup;
                OldName = name;
            }

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
            private byte[] assetData;
            public UUID NewItemID = UUID.Zero;
            public UUID OldItemID = UUID.Zero;
            public UUID NewTaskID = UUID.Zero;
            public UUID OldTaskID = UUID.Zero;


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
                    WriteProgress();
                }
            }

            private void LoadProgressFile()
            {
                if (CogbotHelpers.IsNullOrZero(NewID))
                {
                    string importProgress = ProgressFile;
                    var item = ExportCommand.Running.GetInvItem(Running.Client, "" + OldID, GetAssetUploadsFolder());
                    if (item != null)
                    {
                        NewID = item.AssetUUID;
                        RezRequested = true;
                        NewItemID = item.UUID;
                        NewTaskID = Running.Client.Self.AgentID;
                        lock (ExportCommand.fileWriterLock) if (File.Exists(importProgress)) return;
                        WriteProgress();
                        return;
                    }
                    if (File.Exists(importProgress))
                    {
                        string data = File.ReadAllText(importProgress);
                        var sdata = data.Split(',');
                        NewID = UUIDFactory.GetUUID(sdata[1]);
                        // NewLocalID = uint.Parse(sdata[1]);
                        RezRequested = true;
                    }
                }
            }

            public InventoryType inventoryType
            {
                get
                {
                    return AssetTypeToInventoryType(assetType);
                }
            }

            public static InventoryType AssetTypeToInventoryType(AssetType assetType)
            {
                var ret = Utils.StringToInventoryType(Utils.AssetTypeToString(assetType));
                if (ret != InventoryType.Unknown)
                {
                    return ret;
                }
                switch (assetType)
                {
                    case AssetType.Unknown:
                        break;
                    case AssetType.Sound:
                    case AssetType.SoundWAV:
                        return InventoryType.Sound;
                    case AssetType.ImageTGA:
                    case AssetType.ImageJPEG:
                    case AssetType.TextureTGA:
                    case AssetType.Texture:
                        return InventoryType.Texture;
                    case AssetType.Bodypart:
                    case AssetType.Clothing:
                        return InventoryType.Wearable;
                    case AssetType.TrashFolder:
                    case AssetType.SnapshotFolder:
                    case AssetType.LostAndFoundFolder:
                    case AssetType.Folder:
                    case AssetType.RootFolder:
                    case AssetType.FavoriteFolder:
                    case AssetType.CurrentOutfitFolder:
                    case AssetType.OutfitFolder:
                    case AssetType.MyOutfitsFolder:
                        return InventoryType.Folder;
                    case AssetType.Animation:
                        return InventoryType.Animation;
                    case AssetType.CallingCard:
                        return InventoryType.CallingCard;
                    case AssetType.Landmark:
                        return InventoryType.Landmark;
                    case AssetType.Object:
                        return InventoryType.Object;
                    case AssetType.Notecard:
                        return InventoryType.Notecard;
                    case AssetType.LSLText:
                        return InventoryType.LSL;
                    case AssetType.LSLBytecode:
                        return InventoryType.LSL;
                    case AssetType.Gesture:
                        return InventoryType.Gesture;
                    case AssetType.Mesh:
                        return InventoryType.Mesh;
                    case AssetType.Simstate:
                    case AssetType.Link:
                    case AssetType.LinkFolder:
                    case AssetType.EnsembleStart:
                    case AssetType.EnsembleEnd:
                    default:
                        throw new ArgumentOutOfRangeException("assetType");
                }
                return InventoryType.Texture;
            }

            public bool UploadAssetData(bool storeLocal)
            {
                if (CogbotHelpers.IsNullOrZero(NewID))
                {
                    if (ImportCommand.UseUploadKnown)
                    {
                        AssetUploaded.Reset();
                        Running.Client.Inventory.RequestCreateItemFromAsset(AssetData, "" + OldID, ProgressString,
                                                                            assetType, inventoryType, GetAssetUploadsFolder(),
                                                                            Permissions.FullPermissions, InvItemCreated);
                        if (!AssetUploaded.WaitOne(10000))
                        {
                            return false;
                        }
                        return true;
                    }
                    NewID = UUID.Combine(OldID, Running.Client.Self.SecureSessionID);
                    NewUUID2OBJECT[NewID] = this;
                    Running.Client.Assets.RequestUploadKnown(NewID, assetType, AssetData, storeLocal, OldID);
                    RezRequested = true;
                }
                return true;
            }

            private void InvItemCreated(bool success1, string status, UUID itemid, UUID assetid)
            {
                if (!success1)
                {
                    Running.Failure("!SUCCESS " + status + " ON " + this);
                    return;
                }
                NewID = assetid;
                this.NewItemID = itemid;
                this.NewTaskID = UUID.Zero;
                NewUUID2OBJECT[NewID] = this;
                ConfirmDLable();
            }

            public void WriteProgress()
            {
                lock (ExportCommand.fileWriterLock) File.WriteAllText(ProgressFile, ProgressString);
            }

            protected string ProgressString
            {
                get
                {
                    return "assetid," + uuidString(NewID) + "," + uuidString(OldID) +
                           "," + assetType + "," + uuidString(Running.Client.Self.SecureSessionID) +
                           ",itemid," + uuidString(NewItemID) + "," + uuidString(OldItemID) +
                           ",taskid," + uuidString(NewTaskID) + "," + uuidString(OldTaskID);
                }
            }

            public void WriteComplete()
            {
                ConfirmDLable();
            }

            private void ConfirmDLable()
            {
                Running.Client.Assets.RequestAsset(NewID, assetType, true, OnDownloaded);

            }


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
                if (transfer.AssetID != NewID) return;
                if (!transfer.Success)
                {
                    ImportCommand.Running.Error(ExportCommand.Running.LocalFailure, "bad transfer on " + this);
                }
                else
                {
                    WriteProgress();
                }
                Item = asset;
                ImportCommand.AssetUploaded.Set();
            }

            public void UpdateAsset(byte[] data)
            {
                Running.Client.Assets.RequestUploadKnown(NewID, assetType, data, false, OldID);
            }
        }

        public static string uuidString(UUID uuid)
        {
            if (CogbotHelpers.IsNullOrZero(uuid)) return "Zero";
            return uuid.ToString();
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
                        _prim = (Primitive)ExportCommand.FromFile(LLSDFilename, ExportCommand.UseBinarySerialization);
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
            public bool NeedsPositioning = true;
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
            public bool TaskInvComplete;

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
                        if (!CogbotHelpers.IsNullOrZero(NewID))
                        {
                            Rezed = ExportCommand.GetSimObjectFromUUID(NewID);
                        }
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
                    lock (ExportCommand.fileWriterLock) File.WriteAllText(ProgressFile, NewID + "," + NewLocalID + "," + value.RegionHandle);
                }
            }

            private void LoadProgressFile()
            {
                if (CogbotHelpers.IsNullOrZero(NewID))
                {
                    string importProgress = ProgressFile;
                    lock (ExportCommand.fileWriterLock) if (!File.Exists(importProgress)) return;
                    {
                        string data = File.ReadAllText(importProgress);
                        var sdata = data.Split(',');
                        NewID = UUIDFactory.GetUUID(sdata[0]);
                        NewLocalID = uint.Parse(sdata[1]);
                        RezRequested = true;
                        NeedsPositioning = false;
                    }

                }
            }

            public void ReplaceAll(Dictionary<UUID, UUID> dictionary)
            {
                ReplaceAllMembers(Prim, typeof(UUID), UUIDReplacer);
            }
        }
        static object UUIDReplacer(object arg)
        {
            UUID before = (UUID)arg;
            if (UnresolvedUUIDs.Contains(before)) return before;
            UUID other;
            if (ChangeList.TryGetValue(before, out other))
            {
                return other;
            }
            UnresolvedUUIDs.Add(before);
            return before;
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

        public static UUID GetAssetUploadsFolder()
        {
            UUID assetUploadsFolder = ExportCommand.Running.FolderCalled("AssetUploads");
            return assetUploadsFolder;
        }
        public ImportCommand(BotClient testClient)
        {
            Name = "simimport";
            Description = "Import prims from an exported xml file. Usage: import inputfile.xml [usegroup]";
            Category = CommandCategory.Objects;
            Client.Assets.AssetUploaded += new EventHandler<AssetUploadEventArgs>(Assets_AssetUploaded);
            Client.Network.EventQueueRunning += logged_in;
            ImportCommand.Running = this;
        }

        private void logged_in(object sender, EventQueueRunningEventArgs e)
        {
            Client.Network.EventQueueRunning -= logged_in;
            UUID id = GetAssetUploadsFolder();
            GleanUUIDsFrom(id);
        }

        private void GleanUUIDsFrom(UUID uuid)
        {
            if (CogbotHelpers.IsNullOrZero(uuid)) return;
            foreach (var item0 in Client.Inventory.Store.GetContents(uuid))
            {
                UUID oldID;
                if (UUID.TryParse(item0.Name, out oldID))
                {
                    var item = item0 as InventoryItem;
                    if (item == null) continue;
                    ItemToCreate itc = FindItemToCreate(oldID, item.AssetType, true);
                    if (itc.RezRequested)
                    {
                        
                    }
                }                
            }            
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
            // all = users llsd tasks deps links (dl and taskobj not included)           
            ";
            if (args == null || args.Length == 0) return Failure(hlp);
            UUID GroupID = (args.Length > 1) ? TheBotClient.GroupID : UUID.Zero;
            var CurSim = Client.Network.CurrentSim;
            arglist = new HashSet<string>();
            foreach (string s in args)
            {
                arglist.Add(s.TrimEnd(new[] { 's' }).ToLower().TrimStart(new[] { '-' }));
            }
            if (arglist.Contains("all"))
            {
                arglist.Add("terrain");
                arglist.Add("user");
                arglist.Add("group");
                arglist.Add("prim");
            }
            if (arglist.Contains("prim"))
            {
                arglist.Add("asset");
            }
            if (!arglist.Contains("noasset") && arglist.Contains("asset"))
            {
                UploadAllAssets(arglist.Contains("sameid"));                
            }
            GleanUUIDsFrom(GetAssetUploadsFolder());
            ScanForChangeList();
            if (arglist.Contains("terrain")) UploadTerrain();
            WriteLine("ChangeList Size is " + ChangeList.Count);

            parents = new List<PrimToCreate>();
            childs = new List<PrimToCreate>();
            var taskobjs = new List<PrimToCreate>();

            if (diskPrims.Count == 0)
            {
                foreach (var file in Directory.GetFiles(ExportCommand.dumpDir, "*.llsd"))
                {

                    Primitive prim = (Primitive) ExportCommand.FromFile(file, ExportCommand.UseBinarySerialization);
                    diskPrims.Add(prim);
                    //if (sculptOnly && (prim.Sculpt == null || CogbotHelpers.IsNullOrZero(prim.Sculpt.SculptTexture))) continue;
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
                    //if (sculptOnly && (prim.Sculpt == null || CogbotHelpers.IsNullOrZero(prim.Sculpt.SculptTexture))) continue;
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
            WriteLine("Imported parents " + parents.Count);
            foreach (PrimToCreate ptc in parents)
            {
                CreatePrim(CurSim, ptc, GroupID);
            }
            WriteLine("Importing children " + childs.Count);
            //if (sculptOnly) return Success("Sculpt Only");
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
            foreach (PrimToCreate ptc in childs)
            {
                SetFlagsAndPhysics(CurSim, ptc.NewLocalID, ptc.Prim, true);
            }
            foreach (PrimToCreate ptc in parents)
            {
                SetFlagsAndPhysics(CurSim, ptc.NewLocalID, ptc.Prim, true);
            }
            foreach (var file in Directory.GetFiles(ExportCommand.dumpDir, "*.task"))
            {
                string fileUUID = Path.GetFileNameWithoutExtension(Path.GetFileName(file));
                var ptc = GetOldPrim(UUID.Parse(fileUUID));                
                string taskData = File.ReadAllText(file);
                if (string.IsNullOrEmpty(taskData))
                {
                    ptc.TaskInvComplete = true;
                    continue;
                }
                int created = 0;
                int failed = 0;
                int skipped = 0;
                foreach (string item in taskData.Split('\n'))
                {
                    if (string.IsNullOrEmpty(item)) continue;
                    string[] fields = item.Split(',');
                    // From 
                    //  item.UUID + "," + item.AssetType + "," + item.AssetUUID + "," + item.OwnerID + "," + item.GroupID + "," + item.GroupOwned + "," + item.Permissions.ToHexString() + "," + item.Name + "\n";
                    AssetType assetType = (AssetType)Enum.Parse(typeof(AssetType), fields[1]);
                    UUID itemID = UUID.Parse(fields[0]);
                    UUID assetID = UUID.Parse(fields[2]);
                    Permissions perms = Permissions.FromHexString(fields[6]);
                    string itemName = fields[7];
                    if (CogbotHelpers.IsNullOrZero(assetID))
                    {
                        WriteLine("Cant create Item: " + item);
                        skipped++;
                        continue;
                    }
                    string sfile = ExportCommand.assetDumpDir + Path.GetFileName(SimAsset.CFileName(assetID, assetType));
                    if (!File.Exists(sfile))
                    {
                        WriteLine("Cant find asset: " + item);
                        skipped++;
                        continue;                        
                    }
                    if (assetType == AssetType.Object)
                    {
                        WriteLine("Cant create Object asset: " + item);
                        skipped++;
                        continue;
                    }
                    byte[] data = File.ReadAllBytes(sfile);
                    var tihh = ExportCommand.Running.FolderCalled("TaskInvHolder");
                    var tih = ExportCommand.Running.FolderCalled(ptc.OldID.ToString(), tihh);

                    ItemToCreate itc = FindItemToCreate(assetID, assetType, false);
                    UUID newAssetID;
                    UUID newItemID = null;
                    string ErrorMsg = "";
                    AutoResetEvent areItem = new AutoResetEvent(false);
                    InventoryManager.ItemCreatedFromAssetCallback CreatedAsset0 =
                        (suc, st, itemid, assetid) =>
                            {
                                if (!suc) ErrorMsg = st;
                                newItemID = itemid;
                                newAssetID = assetid;
                                areItem.Set();
                            };
                    /*                     
                    Client.Inventory.RequestCreateItemFromAsset(data, itemName, item, assetType,
                                                                ItemToCreate.AssetTypeToInventoryType(assetType),
                                                                tih, perms, CreatedAsset0);
                     */
                    InventoryBase copy = null;
                    InventoryManager.ItemCopiedCallback IBHUPdate = (newItem) =>
                                                                        {
                                                                            newItemID = newItem.UUID;
                                                                            copy = newItem;
                                                                            areItem.Set();
                                                                        };
                    Client.Inventory.RequestCopyItem(itc.NewItemID, tih, itemName, IBHUPdate);
                    //UUID itcNewItemID = itc.NewItemID;
                    if (!areItem.WaitOne(TimeSpan.FromSeconds(10)))
                    {
                        WriteLine("Cant copy inventory asset: " + item);
                        failed++;
                        continue;
                    }
                    var invItem = copy as InventoryItem;
                    if (invItem == null)
                    {
                        WriteLine("NULL inventory asset: " + item);
                        failed++;
                        continue;
                    }
                    //Client.Inventory.TaskItemReceived += tir;
                    if (invItem.InventoryType == InventoryType.LSL)
                    {
                        Client.Inventory.CopyScriptToTask(ptc.NewLocalID, (InventoryItem)invItem, true);
                        Client.Inventory.RequestSetScriptRunning(ptc.NewID, invItem.AssetUUID, true);
                    }
                    else
                    {
                        Client.Inventory.UpdateTaskInventory(ptc.NewLocalID, (InventoryItem)invItem);
                    }
                    created++;
                }
            }
            WriteLine("Imported P=" + parents.Count + " C=" + childs.Count);
            return SuccessOrFailure();
        }

        private void CreatedAsset(bool success1, string status, UUID itemid, UUID assetid)
        {
            throw new NotImplementedException();
        }

        private void UploadTerrain()
        {
            string fn = ExportCommand.terrainFileName;

            lock (ExportCommand.fileWriterLock)
            {
                if (!File.Exists(fn))
                {
                    // upload patches
                    var fn2 = ExportCommand.terrainDir + "terrain.patches";
                    lock (ExportCommand.fileWriterLock)
                    {
                        if (File.Exists(fn2))
                        {
                            TerrainPatch[] loaded = (TerrainPatch[]) ExportCommand.FromFile(fn2, true);
                            // TerrainCompressor.CreateLayerDataPacket(loaded, TerrainPatch.LayerType.Land);
                            float[,] hm = GetHeightMap(loaded);
                            hm = SmoothHM(hm);
                            byte[] raw = ToRaw32File(hm);
                            lock (ExportCommand.fileWriterLock) File.WriteAllBytes(fn, raw);
                        }
                    }
                }
            }
            if (File.Exists(fn))
            {
                Client.Estate.UploadTerrain(File.ReadAllBytes(fn), Path.GetFileName(fn));
                Success("Terrain file uploading");
            }
            else
            {
                Failure("unable to find any terrain files");
            }

        }

        private float[,] SmoothHM(float[,] doubles)
        {
            return doubles;
        }

        private byte[] ToRaw32File(float[,] doubles)
        {
            int o = 0;
            byte[] b = new byte[256*256];
            for (int y = 0; y < 256; y++)
            {
                for (int x = 0; x < 256; x++)
                {
                    b[o++] = (byte) doubles[x, y];
                }
            }
            return b;
        }

        static float[,] GetHeightMap(TerrainPatch[] Terrain)
        {
            float average = 23;
            float[,] height = new float[256,256];
            for (int x = 0; x < 256; x++)
            {
                for (int y = 0; y < 256; y++)
                {
                    int patchX = x/16;
                    int patchY = y/16;

                    TerrainPatch patch = Terrain[patchY*16 + patchX];
                    if (patch != null)
                    {
                        height[x, y] = average = patch.Data[(y % 16) * 16 + (x % 16)];
                    } else
                    {
                        height[x, y] = average;
                    }
                }
            }
            return height;
        }
    

        private void UploadAllAssets(bool sameIds)
        {
            int uploaded = 0;
            int reuploaded = 0;
            int seenAsset = 0;

            bool alwayReupload = arglist.Contains("reup");
            Success("Uploading assets... sameIds=" + sameIds);
            foreach (var file in Directory.GetFiles(ExportCommand.assetDumpDir, "*.*"))
            {
                if (file.EndsWith(".object") || file.EndsWith(".simasset") || file.EndsWith(".rzi"))
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
                    if (file.EndsWith(".ogg"))
                    {
                        assetType = AssetType.Sound;
                        sid = Path.GetFileName(file.Substring(0, file.Length - ".jp2".Length));
                    }
                }
                if (assetType == AssetType.Unknown)
                {
                    Failure("Cant guess assetyype for " + file);
                    continue;
                }
                seenAsset++;
                UUID oldID = UUID.Parse(sid);
                ItemToCreate itc = FindItemToCreate(oldID, assetType, sameIds);
                itc.LLSDFilename = file;
                if (!itc.RezRequested)
                {
                    itc.UploadAssetData(false);
                    if (!CogbotHelpers.IsNullOrZero(itc.NewID))
                    {
                        NewUUID2OBJECT[itc.NewID] = itc;
                    }
                    uploaded++;
                }
                else if (alwayReupload)
                {
                    itc.UpdateAsset(itc.AssetData);
                    reuploaded++;
                }
                if (!CogbotHelpers.IsNullOrZero(itc.NewID))
                {
                    NewUUID2OBJECT[itc.NewID] = itc;
                }
            }
            Success("Uploaded assets=" + uploaded + " seenAssets=" + seenAsset + " reuploaded=" + reuploaded);
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
                    return (PrimToCreate) utc;
                }
                if (ptc != null) return ptc;
                ptc = new PrimToCreate(primitive);
                if (primitive.Sculpt != null)
                {
                    var texture = primitive.Sculpt.SculptTexture;
                    if (texture != null)
                    {
                        var t2c = (ItemToCreate) (GetOld(texture) ?? GetNew(texture));
                        if (t2c != null)
                        {
                            t2c.UploadAssetData(true);
                        }
                    }
                }
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

        private void ScanForChangeList()
        {
            Dictionary<UUID, UUID> cl = new Dictionary<UUID, UUID>();
            lock (WorkFlowLock)
            {
                foreach (KeyValuePair<UUID, UUIDChange> o in UUID2OBJECT)
                {
                    ChangeList[o.Key] = o.Value.NewID;
                }
            }
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
           if (!CreatePrim0(CurSim, ptc, GroupID, ExportCommand.Running.LocalFailure))
           {
               Failure("Unable to create Prim " + ptc);
               //try again!
               CreatePrim0(CurSim, ptc, GroupID, ExportCommand.Running.LocalFailure);
           }
        }
        private bool CreatePrim0(Simulator CurSim, PrimToCreate ptc, UUID GroupID, OutputDelegate Failure)
        {
            if (ptc.RezRequested) return true;
            Primitive prim = ptc.Prim;
            Primitive newPrim = null;
            UUID found = UUID.Zero;
            if (prim.PrimData.PCode != PCode.Prim)
            {
                CreateTree(CurSim, ptc, GroupID);
                return true;
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
                        Error(Failure, "cant get UUID from " + eMessage);
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
                return false;
            }
            Client.Self.ChatFromSimulator -= callback;
            ptc.NewID = found;
            var O = ExportCommand.GetSimObjectFromUUID(found);
            if (O == null)
            {
                Error(Failure, "Error - no SimObject");
                return false;
            }
            newPrim = O.Prim;
            if (newPrim == null)
            {
                Error(Failure, "Error - no newPrim");
                return false;
            }
            ptc.Rezed = O;
            SavePTC(ptc);
            uint localID = newPrim.LocalID;            
            SetPrimInfo(CurSim, localID, ptc.Prim, GroupID, false, false, true);
            ptc.NeedsPositioning = false;
            return true;
        }

        private PrimToCreate prev = null;
        public static ImportCommand Running;
        static private readonly Dictionary<UUID, UUID> ChangeList = new Dictionary<UUID, UUID>();
        private static readonly HashSet<UUID> UnresolvedUUIDs = new HashSet<UUID>();
        private HashSet<string> arglist;
        //private bool sculptOnly = true;
        public static bool UseUploadKnown = true;
        public static ManualResetEvent AssetUploaded = new ManualResetEvent(false);

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
            ptc.NeedsPositioning = false;
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

            const BindingFlags bf = BindingFlags.Instance | BindingFlags.NonPublic | BindingFlags.Public;
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
            SetFlagsAndPhysics(CurSim, localID, prim, false);
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
                if (!CheckTexture(prim.Sculpt.SculptTexture))
                {
                    Failure("Missing Scupt texture on " + prim);
                }
                Client.Objects.SetSculpt(CurSim, localID, prim.Sculpt);
            }
            Client.Objects.SetMaterial(CurSim, localID, prim.PrimData.Material);
            if (setScale)
            {
                Client.Objects.SetScale(CurSim, localID, prim.Scale, true, false);
            }
        }

        private bool CheckTexture(UUID texture)
        {
            var newt = GetNew(texture);
            var oldt = GetOld(texture);
            if (newt != null && oldt == null)
                return true;
            return false;
        }

        private void SetFlagsAndPhysics(Simulator CurSim, uint localID, Primitive prim, bool canUsePhysics)
        {
            var flags = prim.Flags;
            Primitive.PhysicsProperties physics = prim.PhysicsProps;
            if (physics != null)
            {
                Client.Objects.SetFlags(CurSim, localID, FlagSet(flags, PrimFlags.Physics) && canUsePhysics,
                                        FlagSet(flags, PrimFlags.Temporary), FlagSet(flags, PrimFlags.Phantom) || !canUsePhysics,
                                        FlagSet(flags, PrimFlags.CastShadows), physics.PhysicsShapeType,
                                        physics.Density, physics.Friction, physics.Restitution,
                                        physics.GravityMultiplier);
            }
            else
            {
                Client.Objects.SetFlagsOnly(CurSim, localID, FlagSet(flags, PrimFlags.Physics) && canUsePhysics,
                                            FlagSet(flags, PrimFlags.Temporary), FlagSet(flags, PrimFlags.Phantom) || !canUsePhysics,
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
        private void Error(OutputDelegate Failure, string s, params object[] ps)
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
