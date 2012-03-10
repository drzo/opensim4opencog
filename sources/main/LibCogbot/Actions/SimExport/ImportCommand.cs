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
    public class ImportSettings
    {
        public UUID GroupID;
        public bool MakeEverythingGroupOwned;
        public HashSet<string> arglist = new HashSet<string>();
        public Simulator CurSim;

        public bool Contains(string task)
        {
            return arglist.Contains(task);
        }

        public void Add(string start)
        {
            arglist.Add(start);
        }

        public void Remove(string wait)
        {
            arglist.Remove(wait);
        }
    }


    public partial class ImportCommand : Command, RegionMasterCommand
    {
        public class LocalSimScene
        {
            public HashSet<ItemToCreate> Assets = new HashSet<ItemToCreate>();
            public HashSet<PrimToCreate> Objects = new HashSet<PrimToCreate>();
            public HashSet<UserOrGroupMapping> Users = new HashSet<UserOrGroupMapping>();
            public HashSet<UserOrGroupMapping> Groups = new HashSet<UserOrGroupMapping>();
            public LocalSimScene()
            {

            }
        }
        private static readonly object WorkFlowLock = new object();

        public static ImportCommand Running;
        public static bool IsLocalScene = true;
        public static LocalSimScene LocalScene = new LocalSimScene();
        private HashSet<string> arglist;

        private static readonly HashSet<UUID> UnresolvedUUIDs = new HashSet<UUID>();
        static private readonly Dictionary<UUID, UUID> ChangeList = new Dictionary<UUID, UUID>();

        public static readonly Dictionary<UUID, UUIDChange> UUID2OBJECT = new Dictionary<UUID, UUIDChange>();
        public static readonly Dictionary<uint, PrimToCreate> UINT2OBJECT = new Dictionary<uint, PrimToCreate>();

        public static readonly Dictionary<UUID, UUIDChange> NewUUID2OBJECT = new Dictionary<UUID, UUIDChange>();
        public static readonly Dictionary<uint, PrimToCreate> NewUINT2OBJECT = new Dictionary<uint, PrimToCreate>();

        public delegate object ObjectMemberReplacer(MemberInfo name, object before, HashSet<MissingItemInfo> missing);

        abstract public class UUIDChange
        {
            protected UUIDChange()
            {
                _newId = UUID.Zero;
                _oldId = UUID.Zero;
            }

            private UUID _newId;
            virtual public UUID NewID
            {
                get { return _newId; }
                set { _newId = value; }
            }

            private UUID _oldId;
            virtual public UUID OldID
            {
                get { return _oldId; }
                set { _oldId = value; }
            }

            public abstract void ReplaceAll();
        }

        public static string uuidString(UUID uuid)
        {
            if (CogbotHelpers.IsNullOrZero(uuid)) return "Zero";
            return uuid.ToString();
        }

        static object UUIDReplacer(MemberInfo memberName, object arg, HashSet<MissingItemInfo> missing)
        {
            if (typeof(Primitive) == memberName.DeclaringType)
            {
                string n = memberName.Name;
                if (n == "ID") return arg;
                if (n == "LocalID") return arg;
            }
            if (typeof(Primitive.ObjectProperties) == memberName.DeclaringType)
            {
                string n = memberName.Name;
                if (n == "ObjectID") return arg;
                if (n == "FolderID") return arg;
            }
            if (typeof(Asset) == memberName.DeclaringType)
            {
                string n = memberName.Name;
                //if (n == "AssetID") return arg;
            }
            {
                // skip identities such as FolderUUID AssetUUID Item.UUID
                string n = memberName.Name;
                if (n.Contains("UUID")) return arg;
            }
            UUID before = (UUID)arg;
            if (CogbotHelpers.IsNullOrZero(before)) return before;
            if (UnresolvedUUIDs.Contains(before))
            {
                return before;
            }
            UUID other;
            if (ChangeList.TryGetValue(before, out other))
            {
                return other;
            }
            UUIDChange utc;
            if (UUID2OBJECT.TryGetValue(before, out utc))
            {
                UUID utcNewID = utc.NewID;
                if (!CogbotHelpers.IsNullOrZero(utcNewID))
                {
                    ChangeList.Add(before, utcNewID);
                    return utcNewID;
                }
                return utcNewID ?? UUID.Zero;
            }
            MissingItemInfo mis = new MissingItemInfo(memberName, before);
            if (missing != null) missing.Add(mis);
            Running.Failure("Missing: " + mis);
            UnresolvedUUIDs.Add(before);
            return before;
        }

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
            Client.Objects.ObjectPropertiesFamily += OnObjectPropertiesFamily;
            Client.Network.EventQueueRunning += logged_in;
            ImportPTCFiles(new ImportSettings(), true, false);
            Running = this;
        }

        private void logged_in(object sender, EventQueueRunningEventArgs e)
        {
            Client.Network.EventQueueRunning -= logged_in;
            UUID id = GetAssetUploadsFolder();
            GleanUUIDsFrom(id);
        }


        public override CmdResult Execute(string[] args, UUID fromAgentID, OutputDelegate writeLine)
        {
            ExportCommand.IsExporting = false;
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
            // tasks - save task files
            // taskobjs - task objects
            // all = users llsd tasks deps links (dl and taskobj not included)           
            ";
            if (args == null || args.Length == 0) return Failure(hlp);
            ImportSettings importSettings = new ImportSettings();
            importSettings.GroupID = (args.Length > 1) ? TheBotClient.GroupID : UUID.Zero;
            importSettings.CurSim = Client.Network.CurrentSim;
            arglist = importSettings.arglist = new HashSet<string>();
            writeLine("Starting SimImport...");
            foreach (string s in args)
            {
                arglist.Add(s.TrimEnd(new[] { 's' }).ToLower().TrimStart(new[] { '-' }));
            }
            bool doRez = false;
            if (arglist.Contains("all"))
            {
                //  arglist.Add("terrain");
                arglist.Add("asset");
                arglist.Add("user");
                arglist.Add("group");
                arglist.Add("prim");
                arglist.Add("confirm");                
                arglist.Add("link");
                arglist.Add("task");
                arglist.Add("taskobj");
            }
            if (arglist.Contains("prim"))
            {
                doRez = true;
                arglist.Add("asset");
                arglist.Add("link");
            }
            if (arglist.Contains("nolink"))
            {
                arglist.Remove("link");
            }
            if (arglist.Contains("noasset"))
            {
                arglist.Remove("asset");
            }
            if (arglist.Contains("link"))
            {
                arglist.Add("confirm");
            }
            if (arglist.Contains("user") || arglist.Contains("group") || true)
            {
                LoadUsersAndGroups();
            }
            if (arglist.Contains("asset") || true)
            {
                UploadAllAssets(arglist.Contains("sameid"));
            }
            GleanUUIDsFrom(GetAssetUploadsFolder());
            ScanForChangeList();
            if (arglist.Contains("terrain")) UploadTerrain(importSettings);
            WriteLine("NewAsset ChangeList Size is " + ChangeList.Count);

            if (arglist.Contains("confirm")) ImportPTCFiles(importSettings, false, doRez);
            if (arglist.Contains("prim")) ImportPrims(importSettings, doRez);
            if (doRez) RezPrims(importSettings);
            if (arglist.Contains("confirm")) ConfirmLocalIDs(importSettings);
            if (arglist.Contains("link")) ImportLinks(importSettings);
            bool tasksObjs = arglist.Contains("taskobj") && !IsLocalScene;
            if (arglist.Contains("task") || tasksObjs) ImportTaskFiles(importSettings, tasksObjs);
            GleanUUIDsFrom(GetAssetUploadsFolder());
            SaveMissingIDs();
            if (arglist.Contains("request")) RequestMissingIDs();
            writeLine("Completed SimImport");
            return SuccessOrFailure();
        }

        private static void SaveMissingIDs()
        {
            FileStream saveTo = File.Open(ExportCommand.dumpDir + "../MissingFromExport.txt", FileMode.Create);
            var fw = new StreamWriter(saveTo);
            foreach (MissingItemInfo itemInfo in MissingFromExport)
            {
                fw.WriteLine(";; " + itemInfo.key + "\n" + itemInfo.MissingID);
            }
            fw.Close();
        }


        private void RequestMissingIDs()
        {
            ExportCommand.IsExporting = true;
            foreach (MissingItemInfo itemInfo in MissingFromExport)
            {
                var ex = ExportCommand.Running;
                if (itemInfo.AssetType == AssetType.Unknown)
                {
                    Failure("MISSING STILL: " + itemInfo);
                    continue;
                }
                Success("Requesting: " + itemInfo);
                if (itemInfo.AssetType == AssetType.Landmark)
                {
                    Client.Grid.RequestRegionHandle(itemInfo.MissingID);
                    continue;
                }
                if (itemInfo.AssetType == AssetType.EnsembleStart)
                {
                    Client.Groups.RequestGroupName(itemInfo.MissingID);
                    continue;
                }
                if (itemInfo.AssetType == AssetType.CallingCard)
                {
                    Client.Avatars.RequestAvatarName(itemInfo.MissingID);
                    continue;
                }
                ex.AddRelated(itemInfo.MissingID, itemInfo.AssetType);                
            }
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
            return;
            Dictionary<UUID, UUID> cl = new Dictionary<UUID, UUID>();
            lock (WorkFlowLock)
            {
                foreach (KeyValuePair<UUID, UUIDChange> o in UUID2OBJECT)
                {
                    UUID oValueNewID = o.Value.NewID;
                    if (!(o.Value is UserOrGroupMapping)) ChangeList[o.Key] = oValueNewID;
                }
            }
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

        public static object ReplaceAllMembers(object from, Type ofType, ObjectMemberReplacer replacerFunc, HashSet<MissingItemInfo> missing)
        {
            return ReplaceAllMembers(from, ofType, ofType, replacerFunc, new HashSet<object>(), missing);
        }
        public static object ReplaceAllMembers(object from, Type ofType, MemberInfo name, ObjectMemberReplacer replacerFunc, HashSet<object> exceptFor, HashSet<MissingItemInfo> missing)
        {
            if (from == null) return from;
            var fromType = from.GetType();
            if (fromType == ofType)
            {
                var oo = replacerFunc(name, from, missing);
                return oo;
            }
            Type fromType0 = fromType;
            while (fromType0.IsArray) fromType0 = fromType0.GetElementType();
            if (fromType0 == typeof(string) || fromType0 == typeof(byte[]) || typeof(IConvertible).IsAssignableFrom(fromType0))
                return from;
            if (from is IDictionary)
            {
                var ic = from as IDictionary;
                foreach (var k0 in LockInfo.CopyOf<object>(ic.Keys))
                {
                    var k = k0;
                    var ko = ReplaceAllMembers(k, ofType, k == null ? null : k.GetType(), replacerFunc, exceptFor, missing);                   
                    object o = ic[k];
                    var oo = ReplaceAllMembers(o, ofType, o == null ? null : o.GetType(), replacerFunc, exceptFor, missing);
                    bool keyChanged = false;
                    if (!ReferenceEquals(k, ko))
                    {
                        keyChanged = true;
                        ic.Remove(k);
                        k = ko;
                    }
                    if (ReferenceEquals(oo, o) && !keyChanged) continue;
                    ic[k] = oo;
                }
                return from;
            }
            if (from is IList)
            {
                var ic = from as IList;
                lock (ic) for (int i = 0; i < ic.Count; i++)
                {
                    object o = ic[i];
                    var oo = ReplaceAllMembers(o, ofType, name, replacerFunc, exceptFor, missing);
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
                var oo = ReplaceAllMembers(o, ofType, info, replacerFunc, exceptFor, missing);
                if (ReferenceEquals(oo, o)) continue;
                info.SetValue(from, oo);
            }
            foreach (var info in fromType.GetProperties(bf))
            {
                object o = info.GetValue(from, null);
                var oo = ReplaceAllMembers(o, ofType, info, replacerFunc, exceptFor, missing);
                if (ReferenceEquals(oo, o)) continue;
                info.SetValue(from, oo, null);
            }
            return from;
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

    }
}
