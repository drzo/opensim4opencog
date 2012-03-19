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
using PathSystem3D.Navigation;

namespace cogbot.Actions.SimExport
{
    public partial class ImportCommand 
    {
        private PrimToCreate prev = null;
        private readonly HashSet<PrimToCreate> parents = new HashSet<PrimToCreate>();
        private readonly HashSet<PrimToCreate> childs = new HashSet<PrimToCreate>();
        static readonly HashSet<PrimToCreate> diskPrims = new HashSet<PrimToCreate>();
        public String ResetPropertiesString = "";//"File.ReadAllText(ExportCommand.dumpDir + "..\\rezbankkill.txt");
        private HashSet<PrimToCreate> ORPHANS;
        private PrimToCreate CurrentOrphan = null;
        private HashSet<PrimToCreate> EXCUSED_ORPHANS = new HashSet<PrimToCreate>();

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

        public partial class PrimToCreate : UUIDChange
        {
            public override string ToString()
            {
                string rezString = "" + _rezed;
                if (string.IsNullOrEmpty(rezString))
                {
                    if (_prim != null && _prim.Properties != null)
                    {
                        rezString = "name='" + _prim.Properties.Name + " " + _prim.Properties.Description + "' ";
                    }
                }
                if (NewID != OldID && !IsLocalScene)
                {
                    rezString = rezString + " newID=" + NewID;
                }
                if (PackedInsideNow)
                {
                    rezString = "DEREZZED " + rezString;
                }
                if (!IsLocalScene) rezString = " -> " + rezString;
                if (_prim != null)
                {
                    return _prim + rezString + " @ " + SimPosition;
                }
                return OldID + rezString + " @ " + SimPosition;
            }

            public override int GetHashCode()
            {
                return OldID.GetHashCode();
            }
            public override bool Equals(object obj)
            {
                var ptc = obj as PrimToCreate;
                return ptc != null && OldID == ptc.OldID;
            }
            public PrimToCreate(Primitive prim)
            {
                if (CogbotHelpers.IsNullOrZero(prim.ID))
                {
                    throw new NullReferenceException("PrimToCreate!");
                }
                State = PrimImportState.LoadedLLSD;
                SetOldPrim(prim);
                LoadProgressFile();
                diskPrims.Add(this);
            }
            public PrimToCreate(UUID oldID)
            {
                if (CogbotHelpers.IsNullOrZero(oldID))
                {
                    throw new NullReferenceException("PrimToCreate!");
                }
                UUID2OBJECT[oldID] = this;
                State = PrimImportState.Unloaded;
                OldID = oldID;
                LoadProgressFile();
                diskPrims.Add(this);
            }
            public Primitive Prim
            {
                get
                {
                    if (_prim == null)
                    {
                        SetOldPrim(Primitive.FromTotalOSD(LoadOSD()));
                        State = PrimImportState.LoadedLLSD;
                        PrimOSD = null;
                    }
                    return _prim;
                }
            }

            private void SetOldPrim(Primitive value)
            {
                _prim = value;
                OldID = value.ID;
                _OldLocalID = value.LocalID;
                ReplaceAll();
                UUID2OBJECT[OldID] = this;
                UINT2OBJECT[_OldLocalID] = this;
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
            public PrimImportState State = PrimImportState.Unloaded;
            public uint NewLocalID
            {
                get
                {
                    if (IsLocalScene) return OldLocalID;
                    if (_rezed == null)
                    {
                        _rezed = WorldObjects.GetSimObjectFromUUID(NewID);
                        if (_rezed == null)
                        {
                            return 0;
                        }
                    }
                    uint rezedLocalID = _rezed.LocalID;
                    if (rezedLocalID == 0)
                    {
                        return 0;
                    }
                    return rezedLocalID;
                }
            }
            public bool PackedInsideNow;
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
            public Primitive _prim;
            private string _progressFile;
            public bool IsLinkParent
            {
               get
               {
                   return Childs != null;
               }
            }
            public uint _OldLocalID;
            public List<PrimToCreate> Childs;

            public uint OldLocalID
            {
                get
                {
                    var i = OldLocalID0;
                    if (i == 0)
                    {
                        i = Prim.LocalID;
                        if (i == 0)
                        {
                            Running.Failure("Failed to return LocalID");
                            i = OldLocalID0;                           
                        }
                    }
                    return i;
                }
            }
            public uint OldLocalID0
            {
                get
                {

                    if (_OldLocalID == 0)
                    {
                        if (_prim != null)
                        {
                            return _OldLocalID = _prim.LocalID;
                        }
                        if (!File.Exists(LLSDFilename))
                        {
                            // this migth grab to old localID
                            LoadProgressFile();
                        }
                        else
                        {
                            LoadOSD();
                            if (PrimOSD != null) return PrimOSD["LocalID"];
                            return Prim.LocalID;
                        }
                    }
                    return _OldLocalID;
                }
                set { _OldLocalID = value; }
            }

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
                  //  if (IsLocalScene) throw new NotSupportedException("rezed " + this);
                    if (_rezed == null)
                    {
                        LoadProgressFile();
                        if (!CogbotHelpers.IsNullOrZero(NewID))
                        {
                            _rezed = WorldObjects.GetSimObjectFromUUID(NewID);
                        }
                        if (!CogbotHelpers.IsNullOrZero(OldID))
                        {
                            _rezed = WorldObjects.GetSimObjectFromUUID(OldID);
                        }
                        if (_rezed == null)
                        {
                            var l = OldLocalID;
                            if (l != 0)
                            {
                                var s = CurSim;
                                Client.Objects.SelectObject(s, l, false);
                                Client.Objects.RequestObject(s, l);
                                Client.Objects.DeselectObject(s, l);
                            }
                        }
                    }
                    if (_rezed==null)
                    {
                        
                    }
                    return _rezed;
                }
                set
                {
                    if (value == null) return;
                    RezRequested = true;
                    _rezed = value;
                    NewID = value.ID;
                    // NewLocalID = value.LocalID;
                    SaveProgressFile();
                }
            }
            public void SaveProgressFile()
            {
                if (!HasLID)
                {
                    return;
                }
                lock (ExportCommand.fileWriterLock)
                    File.WriteAllText(ProgressFile,
                                      string.Format("{0},{1},{2},{3},{4},{5},{6}", NewID, NewLocalID, NewRegionHandle, OldLocalID, TaskInvComplete, PackedInsideNow, _rezed));
            }

            protected ulong NewRegionHandle
            {
                get
                {
                    if (_rezed != null) return _rezed.RegionHandle;
                    return Client.Network.CurrentSim.Handle;
                }
            }

            public bool HasLID
            {
                get { return _rezed != null && _rezed.LocalID > 0; }
            }

            private bool _target_confirmed = false;
            private bool _source_confirmed = false;
            private OSDMap PrimOSD;

            public bool IsConfirmed
            {
                get { return _target_confirmed;  }
                set
                {
                    if (HasLID)
                    {
                    }
                    RezRequested = value;
                    _target_confirmed = value;
                }
            }

            private UUID _ParentUUID;
            public UUID ParentUUID
            {
                get
                {
                    if (ParentID == 0) return null;
                    if (_ParentUUID != null) return _ParentUUID;
                    if (ParentPrim != null) return _ParentUUID = ParentPrim.OldID;
                    return _ParentUUID = LoadOSD()["ParentUUID"];
                }
                set
                {
                    _ParentUUID = value;
                }
            }
            public PrimToCreate ParentPrim;
            public Linkset Link;
            public int MissingChildern;

            public uint ParentID
            {
                get
                {
                    if (_prim == null)
                    {
                        OSDMap primOSD = LoadOSD();
                        return primOSD["ParentID"].AsUInteger();
                    }
                    return _prim.ParentID;
                }
            }

            public OSDMap LoadOSD()
            {
                if (PrimOSD == null)
                {
                    string llsdStr = File.ReadAllText(LLSDFilename);
                    this.PrimOSD = OSDParser.DeserializeLLSDXml(llsdStr) as OSDMap;
                }
                return PrimOSD;
            }

            private void LoadProgressFile()
            {
                if (File.Exists(ExportCommand.dumpDir + OldID + ".objectAsset"))
                {
                    IsAsset = true;
                }
                if (IsLocalScene) return;
                if (CogbotHelpers.IsNullOrZero(NewID))
                {
                    string importProgress = ProgressFile;
                    lock (ExportCommand.fileWriterLock) if (!File.Exists(importProgress)) return;
                    {
                        string data = File.ReadAllText(importProgress);
                        var sdata = data.Split(',');
                        NewID = UUIDFactory.GetUUID(sdata[0]);
                        //NewLocalID = uint.Parse(sdata[1]);
                        if (sdata.Length > 3)
                        {
                            _OldLocalID = uint.Parse(sdata[3]);
                            UINT2OBJECT[_OldLocalID] = this;
                            if (sdata.Length > 4)
                            {
                                _taskInvComplete = bool.Parse(sdata[4]);
                            }
                            if (sdata.Length > 5)
                            {
                                PackedInsideNow = bool.Parse(sdata[5]);
                            }
                        }
                        UUID2OBJECT[OldID] = this;
                        NewUUID2OBJECT[NewID] = this;
                        //NewUINT2OBJECT[NewLocalID] = this;
                    }
                }
            }

            public override void ReplaceAll()
            {
                ReplaceAllMembers(Prim, typeof(UUID), UUIDReplacer, MissingFromExport);
                Prim.ID = OldID;
                Prim.LocalID = OldLocalID;
            }

            public void RequestExists()
            {
                if (IsLocalScene) return;
                var CurSim = Client.Network.CurrentSim;
                //Client.Objects.RequestObject(CurSim, NewLocalID);
                Client.Objects.RequestObjectPropertiesFamily(CurSim, NewID, true);
            }

            public void LoadChildren()
            {
                if (Childs != null) return;
                string file = ExportCommand.dumpDir + OldID + ".link";
                if (!File.Exists(file))
                {
                    Childs = null;
                    return;
                }
                                
                var uuids = ExportCommand.GetUUIDs(File.ReadAllText(file));
                if (uuids.Length < 2)
                {
                    Childs = null;
                    return;
                }
                Childs = Childs ?? new List<PrimToCreate>();
                for (int i = 1; i < uuids.Length; i++)
                {
                    UUID id = uuids[i];
                        
                    if (MissingLLSD(id))
                    {
                        Running.Failure("Warning missing link child " + id + " on parent " + this.ToString());
                        ExportCommand.Exporting.AddMoveTo(SimPosition);
                        MissingChildern++;
                        continue;
                    }
                    PrimToCreate ptc = Running.APrimToCreate(id);
                    if (ptc == null)
                    {
                        //Failure("FAILED: Relink cant find PTC=" + id);
                        throw new NotImplementedException();
                        continue;
                    }
                    Childs.Add(ptc);
                }
                State = PrimImportState.Linking;
            }

            public Vector3 SimPosition
            {
                get
                {
                    if (_prim != null) 
                    {
                        bool fnd;
                        var V3 = PrimPos(_prim, out fnd);
                        if (fnd) return V3;
                    }
                    return LoadOSD()["RegionPosition"].AsVector3();
                }
            }

            public static Vector3 PrimPos(Primitive prim, out bool found)
            {
                found = true;
                Vector3 pp = prim.Position;
                if (prim.ParentID != 0)
                {
                    var parent = Running.GetOldPrim(prim.ParentID);
                    if (parent == null)
                    {
                        found = false;
                        return Vector3.Zero;
                       // throw new AbandonedMutexException("no parent for " + prim);
                    }
                    else
                    {
                        return prim.Position*Matrix4.CreateFromQuaternion(parent.Prim.Rotation) + parent.SimPosition;
                    }
                }
                return pp;
            }

            public bool WaitUntilFound(TimeSpan span)
            {
                if (IsLocalScene) return true;

                LoadProgressFile();
                RequestExists();
                var until = DateTime.Now.Add(span);
                while (DateTime.Now < until)
                {
                    if (_rezed != null)
                    {
                        IsConfirmed = true;
                        return true;
                    }
                    if (!CogbotHelpers.IsNullOrZero(NewID))
                    {
                        FastCheckTargetRezed();
                    }
                    if (IsConfirmed) return true;
                }
                return false;
            }

            public bool FastCheckTargetRezed()
            {
                if (IsLocalScene) return true;
                if (_target_confirmed) return true;
                if (this.PackedInsideNow) return true;
                if (_rezed != null) return true;
                var O = WorldObjects.GetSimObjectFromUUID(NewID);
                if (O != null)
                {
                    Rezed = O;
                    IsConfirmed = true;
                    return true;
                }
                return false;
            }

            public bool FastCheckSourceRezed()
            {
                if (_target_confirmed) return true;
                if (this.PackedInsideNow) return true;
                if (_rezed != null) return true;
                var O = WorldObjects.GetSimObjectFromUUID(NewID);
                if (O != null)
                {
                    Rezed = O;
                    IsConfirmed = true;
                    return true;
                }
                return false;
            }

            public void SetStoreLocal()
            {
                NewID = OldID;
                NewUINT2OBJECT[NewLocalID] = this;
                ReplaceAll();
                NeedsPositioning = false;
                RezRequested = true;
                _target_confirmed = true;
            }
        }

        static public bool MissingLLSD(UUID id)
        {
            return !File.Exists(ExportCommand.dumpDir + id + ".llsd");
        }

        static public bool MissingLINK(UUID id)
        {
            return !File.Exists(ExportCommand.dumpDir + id + ".link");
        }

        static public bool MissingTASK(UUID id)
        {
            return !File.Exists(ExportCommand.dumpDir + id + ".task");
        }

        private void ImportPTCFiles(ImportSettings arglist, bool lloadOnly, bool rezMissing)
        {
            if (IsLocalScene) return;
            WriteLine("Loading Prims from PTC Files...");
            int loaded = 0;
            int missing = 0;
            int corrected = 0;
            bool doBlankMissing = !string.IsNullOrEmpty(ResetPropertiesString);
            foreach (var file in Directory.GetFiles(ExportCommand.dumpDir, "*.ptc"))
            {
                string fileUUID = Path.GetFileNameWithoutExtension(Path.GetFileName(file));
                UUID oldId = UUID.Parse(fileUUID);
                //string linkFile = ExportCommand.dumpDir + fileUUID + ".link";
                if (++loaded % 25 == 0) WriteLine("PTCs loaded " + loaded + "...");
                PrimToCreate ptc = null;
                ptc = APrimToCreate(oldId);
                if (lloadOnly) continue;
                if (!ptc.PackedInsideNow)
                {
                    ptc.RequestExists();
                }
                if (arglist.arglist.Contains("ptc")) continue;
                if (!ptc.WaitUntilFound(TimeSpan.FromSeconds(10)))
                {
                    Failure("Missing Prim with PTC file! " + ptc);
                    if (++missing % 10 == 0) WriteLine("Missing " + missing + "...");
                    if (!rezMissing) continue;
                    var p = ptc.Prim;
                    CreatePrim(arglist, ptc);
                }
                if (doBlankMissing && ResetPropertiesString.Contains("REZBLANK:" + ptc.NewID))
                {
                    var newPrim = ptc.Rezed.Prim;
                    SavePTC(ptc);
                    uint localID = newPrim.LocalID;
                    SetPrimInfo(arglist.CurSim, localID, ptc.Prim, arglist.GroupID, false, true, false);                    
                    ptc.NeedsPositioning = false;
                    if (++corrected % 5 == 0) WriteLine("corrected " + corrected + "...");
                }

            }
            WriteLine("PTCs loaded " + loaded + "!");
            WriteLine("TOTAL missing " + missing + ".");
            WriteLine("TOTAL corrected " + corrected + ".");
        }

        private void ImportPrims(ImportSettings arglist, bool rezParents)
        {
            if (IsLocalScene) rezParents = false;            
            if (childs.Count == 0 || IsLocalScene)
            {
                bool ptcOnly = arglist.arglist.Contains("ptc") && !IsLocalScene;
                WriteLine("Loading Prims from LLSD Files");
                int loaded = 0;
                foreach (var file in Directory.GetFiles(ExportCommand.dumpDir, "*.llsd"))
                {
                    string fileUUID = Path.GetFileNameWithoutExtension(Path.GetFileName(file));
                    UUID oldId = UUID.Parse(fileUUID);
                    string ptcFile = ExportCommand.dumpDir + fileUUID + ".ptc";
                    if (++loaded % 25 == 0) WriteLine("Prims loaded " + loaded + "...");
                    PrimToCreate ptc = null;
                    if (File.Exists(ptcFile) && !IsLocalScene)
                    {
                        ptc = APrimToCreate(oldId);
                        if (!ptc.PackedInsideNow)
                        {
                            ptc.RequestExists();
                        }
                        if (!ptc.WaitUntilFound(TimeSpan.FromSeconds(10)))
                        {
                            Failure("Missing Prim witha  PTC file! " + ptc);
                            var p = ptc.Prim;
                            CreatePrim(arglist, ptc);
                            parents.Add(ptc);
                        } else
                        {
                            parents.Add(ptc);
                        }
                    }
                    if (ptc == null)
                    {
                        ptc = APrimToCreate(oldId);
                        if (ptc.ParentID != 0)
                        {
                            childs.Add(ptc);
                            continue;
                        }
                        parents.Add(ptc);
                    } else
                    {
                        if (ptc.Prim.RegionHandle != 1249045209445888)
                        {
                            KillID(ptc.OldID);
                            continue;
                        }
                    }
                    //diskPrims.Add(prim);
                    //if (sculptOnly && (prim.Sculpt == null || CogbotHelpers.IsNullOrZero(prim.Sculpt.SculptTexture))) continue;
                    //PrimToCreate ptc = APrimToCreate(prim);

                    if (ptcOnly) continue;
                    if (rezParents) CreatePrim(arglist, ptc);
                }
            }
            else
            {
                WriteLine("Loading Prims from Preloaded Files Count = " + parents.Count);
                int loaded = 0; 
                foreach (PrimToCreate ptc in diskPrims)
                {
                    Primitive prim = ptc.Prim; 
                    //if (sculptOnly && (prim.Sculpt == null || CogbotHelpers.IsNullOrZero(prim.Sculpt.SculptTexture))) continue;
                    if (++loaded % 25 == 0) WriteLine("Prims loaded " + loaded + "...");
                    if (prim.ParentID != 0)
                    {
                        childs.Add(ptc);
                        continue;
                    }
                    parents.Add(ptc);
                    if (rezParents) CreatePrim(arglist, ptc);
                }
            }
        }

        private void RezPrims(ImportSettings arglist)
        {
            WriteLine("Imported parents " + parents.Count);
            int created = 0; 
            foreach (PrimToCreate ptc in parents)
            {
                CreatePrim(arglist, ptc);
                if (++created % 25 == 0) WriteLine("Roots created " + created + "...");
                if (ptc.HasLID)
                {
                    SetPermissionsAll(arglist.CurSim, new List<uint>() { ptc.NewLocalID });
                }
            }
            WriteLine("Importing children " + childs.Count);
            //if (sculptOnly) return Success("Sculpt Only");
            foreach (PrimToCreate ptc in childs)
            {
                CreatePrim(arglist, ptc);
                if (++created % 25 == 0) WriteLine("Childs created " + created + "...");
            }
            List<string> skipCompare = new List<string>() { "LocalID", "ID", "ParentID", "ObjectID", "Tag" };
            WriteLine("COMPLETE: Imported P=" + parents.Count + " C=" + childs.Count);
        }

        private PrimToCreate GetFromWorld(ImportSettings arglist, UUID idp, SimObject O)
        {
            PrimToCreate parent = null;
            if (ExportCommand.IsSkipped(O, arglist))
            {
                Failure("Normally Skipped Parent=" + O);
            }
            arglist.Add("wait");
            arglist.Add("llsd");
            arglist.Add("task");
            arglist.Add("dep");
            arglist.Add("link");
            ExportCommand exp = ExportCommand.Exporting;

            exp.ExportPrim(Client, O, WriteLine, arglist);
            string subLLSD = ExportCommand.dumpDir + O.ID.ToString() + ".llsd";
            if (!File.Exists(subLLSD))
            {
                exp.ExportPrim(Client, O, WriteLine, arglist);
                Failure("No LLSD file Obj=" + O);
                return parent;
                // missing = true;
            }
            parent = APrimToCreate(idp);
            var p = parent.Prim;
            parent.SetStoreLocal();
            return parent;
        }
        public void KillID(PrimToCreate id)
        {
            RemoveFrom(ORPHANS, id);
            RemoveFrom(EXCUSED_ORPHANS, id);
            RemoveFrom(parents, id);
            RemoveFrom(childs, id);
            var uid = ExportCommand.dumpDir + id.OldID;
            File.Delete(uid + ".llsd");
            File.Delete(uid + ".task");
            File.Delete(uid + ".link");
            File.Delete(uid + ".ptc");
            RemoveFrom(UUID2OBJECT, id.OldID);
        }
        public void KillID(UUID id)
        {
            RemoveFrom(ORPHANS, id);
            RemoveFrom(EXCUSED_ORPHANS, id);
            RemoveFrom(parents, id);
            RemoveFrom(childs, id);
            var uid = ExportCommand.dumpDir + id;
            File.Delete(uid + ".llsd");
            File.Delete(uid + ".task");
            File.Delete(uid + ".link");
            File.Delete(uid + ".ptc");
            RemoveFrom(UUID2OBJECT, id);
        }

        private void RemoveFrom<T>(HashSet<T> set, T id)
        {
            if (set != null) lock (set) set.Remove(id);
        }
        private void RemoveFrom<T>(HashSet<T> set, UUID id)
        {
            if (set != null) lock (set)
            {
                foreach (var h in LockInfo.CopyOf(set))
                {
                    UUIDChange t = h as UUIDChange;
                    if (t.OldID == id)
                    {
                        set.Remove(h);
                        return;
                    }
                }
            }
        }
        private void RemoveFrom<T,Y>(Dictionary<T,Y> set, T id)
        {
            if (set != null) lock (set) set.Remove(id);
        }

        private void ImportLinks(ImportSettings arglist)
        {
            var CurSim = arglist.CurSim;
            WriteLine("Linking imports");
            List<string> skipCompare;
            foreach (var file in Directory.GetFiles(ExportCommand.dumpDir, "*.link"))
            {
                var uuids = ExportCommand.GetUUIDs(File.ReadAllText(file));
                if (uuids.Length < 1) continue;
                UUID idp = uuids[0];
                var parent = GetOldPrim(idp);
                if (parent == null)
                {
                    if (IsLocalScene)
                    {
                        SimObject O = WorldObjects.GetSimObjectFromUUID(idp);
                        if (O == null)
                        {
                            if (idp.ExternalData != null)
                            {
                                Failure("MISOBJ: ExternalData " + idp.ExternalData + " " + idp);
                            } else
                            {
                                Failure("MISOBJ: " + idp);
                            }
                            continue;
                        }
                        parent = GetFromWorld(arglist, idp, O);
                        if (parent == null) continue;
                        parents.Add(parent);
                    }
                    else
                    {
                        continue;
                    }
                }
                parent.LoadChildren();
                var linkset = new List<uint>(uuids.Length) { parent.NewLocalID };
                for (int i = 1; i < uuids.Length; i++)
                {
                    UUID id = uuids[i];
                    if (MissingLLSD(id)) continue;
                    PrimToCreate ptc = GetOldPrim(id);
                    if (ptc == null)
                    {
                        Failure("FAILED: Relink cant find PTC=" + id);
                        continue;
                    }
                    if (ptc.ParentPrim == null) ptc.ParentPrim = parent;
                    uint newLocalID = ptc.NewLocalID;
                    if (newLocalID == 0)
                    {
                        Failure("FAILED: Relink cant find linked prim ID=" + id);
                        continue;
                    }
                    linkset.Add(newLocalID);
                }
                if (IsLocalScene) continue;
                // set perms for linking!?
                SetPermissionsAll(CurSim, linkset);
                //linkset.Add(UUID2OBJECT[uuids[0]].Rezed.LocalID);
                Client.Objects.LinkPrims(CurSim, linkset);
                linkset.RemoveAt(0);

                if (false)
                {
                    linkset.Reverse();
                    SetPrimsPostLink(CurSim, arglist.GroupID, parent, linkset, skipCompare);
                }
            }
            if (IsLocalScene)
            {
                foreach (var child in childs)
                {
                    if (child.ParentPrim != null) continue;
                    var uui = child.ParentUUID;
                    var parent = GetOldPrim(uui);
                    if (parent != null) child.ParentPrim = parent;
                }
                ORPHANS = new HashSet<PrimToCreate>();
                foreach (var child in childs)
                {
                    if (child.ParentPrim != null) continue;                   
                    uint p = child.ParentID;
                    var parent = GetOldPrim(p) ?? GetNewPrim(p);
                    if (parent == null)
                    {
                        if (!EXCUSED_ORPHANS.Contains(child))
                        {
                            ORPHANS.Add(child);
                            if (CurrentOrphan == null) CurrentOrphan = child;
                        } 
                        Failure("ORPHAN: " + child);
                        continue;
                    }
                    child.ParentPrim = parent;
                    parent.Childs = parent.Childs ?? new List<PrimToCreate>();
                    if (parent.Childs.Contains(child)) continue;
                    parent.Childs.Add(child);
                }
                if (arglist.Contains("adopt"))
                {
                    if (CurrentOrphan != null)
                    {
                        EXCUSED_ORPHANS.Add(CurrentOrphan);
                        CurrentOrphan = null;
                    }
                    if (ORPHANS.Count > 0)
                    {
                        foreach (PrimToCreate toCreate in ORPHANS)
                        {
                            CurrentOrphan = toCreate;
                            break;
                        }
                    }
                }
                if (arglist.Contains("orphan"))
                {
                    if (CurrentOrphan != null)
                    {
                        Client.Self.Chat("" + CurrentOrphan.OldID, 4201, ChatType.Normal);
                    }
                }
                if (arglist.Contains("kill"))
                {
                    foreach (PrimToCreate toCreate in LockInfo.CopyOf(ORPHANS))
                    {
                        KillID(toCreate);
                    }
                    foreach (PrimToCreate toCreate in LockInfo.CopyOf(EXCUSED_ORPHANS))
                    {
                        KillID(toCreate);
                    }
                }
                Success("ORPHAN nonexcused=" + ORPHANS.Count + " excused=" + EXCUSED_ORPHANS.Count);
                return;
            }
            // hopefully unset some phantoms here
            foreach (PrimToCreate ptc in childs)
            {
                SetFlagsAndPhysics(CurSim, ptc.NewLocalID, ptc.Prim, true);
                ptc.State = PrimImportState.PostLinkProperiesSet;
            }
            foreach (PrimToCreate ptc in parents)
            {
                SetFlagsAndPhysics(CurSim, ptc.NewLocalID, ptc.Prim, true);
                ptc.State = PrimImportState.PostLinkProperiesSet;
            }

        }


        private void SetPermissionsAll(Simulator CurSim, List<uint> linkset)
        {
            Client.Objects.SetPermissions(CurSim, linkset, PermissionWho.All, PermissionMask.All, true);
            Client.Objects.SetPermissions(CurSim, linkset,
                                          PermissionWho.Group | PermissionWho.NextOwner | PermissionWho.Owner |
                                          PermissionWho.Base,
                                          PermissionMask.Move | PermissionMask.Copy | PermissionMask.Transfer |
                                          PermissionMask.Modify, true);
            Client.Objects.SetPermissions(CurSim, linkset,
                                          PermissionWho.Everyone, PermissionMask.Move | PermissionMask.Copy, true);
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
                    ptc = utc as PrimToCreate;
                }
                if (ptc != null) return ptc;
                ptc = new PrimToCreate(primitive);
                if (primitive.Sculpt != null)
                {
                    var texture = primitive.Sculpt.SculptTexture;
                    if (texture != null)
                    {
                        var t2c = (GetOld(texture) ?? GetNew(texture)) as ItemToCreate;
                        if (t2c != null)
                        {
                            t2c.UploadAssetData(true);
                        }
                    }
                }
                UUID2OBJECT[ptc.OldID] = ptc;
                UINT2OBJECT[ptc.OldLocalID] = ptc;
                return ptc;
            }
        }
        public PrimToCreate APrimToCreate(UUID oldObjectID)
        {
            lock (WorkFlowLock)
            {
                PrimToCreate ptc = null;
                UUIDChange utc;
                if (UUID2OBJECT.TryGetValue(oldObjectID, out utc))
                {
                    ptc = utc as PrimToCreate;
                }
                if (ptc != null) return ptc;
                ptc = new PrimToCreate(oldObjectID);
                UUID2OBJECT[ptc.OldID] = ptc;
                uint oldLocalId = ptc.OldLocalID;
                if (oldLocalId > 0)
                {
                    UINT2OBJECT[oldLocalId] = ptc;
                }
                else
                {
                    Failure("ERROR: APrimToCreate " + ptc);
                }
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
        public PrimToCreate GetOldPrim(UUID id)
        {
            PrimToCreate getOld = GetOld(id) as PrimToCreate;
            if (getOld == null)
            {
                Failure("cant find ID=" + id);
            }
            return getOld;
        }

        private PrimToCreate GetNewPrim(uint localID)
        {
            lock (WorkFlowLock)
            {
                PrimToCreate ptc;
                if (localID == 0)
                {
                    return null;
                }
                if (NewUINT2OBJECT.TryGetValue(localID, out ptc))
                {
                    return ptc;
                }
                WriteLine("cant find LocalID=" + localID);
            }
            return null;
        }

        private void CreatePrim(ImportSettings arglist, PrimToCreate ptc)
        {
            if (ptc.PackedInsideNow) return;
            if (IsLocalScene)
            {
                LocalScene.Objects.Add(ptc);
                ptc.SetStoreLocal();
                return;
            }
            if (!CreatePrim0(arglist.CurSim, ptc, arglist.GroupID, ExportCommand.Exporting.LocalFailure))
            {
                Failure("Unable to create Prim " + ptc);
                //try again!
                CreatePrim0(arglist.CurSim, ptc, arglist.GroupID, ExportCommand.Exporting.LocalFailure);
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
                    pp = prim.Position * Matrix4.CreateFromQuaternion(parent.Prim.Rotation) + parent.Prim.Position;
                    pr = parent.Prim.Rotation * pr;
                }
            }
            if (ptc.RezRequested || ptc.IsConfirmed || ptc.HasLID)
            {
                return true;
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
                    //prev.NewLocalID = 0;
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
            }
            else
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
            SetPermissionsAll(CurSim, new List<uint>() { localID });
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

        private void ConfirmLocalIDs(ImportSettings settings)
        {
            int nchecked = 0;
            int unconfirmed = 0;
            foreach (var i in UUID2OBJECT)
            {
                var o = i.Value as PrimToCreate;
                if (o == null) continue;
                if (++nchecked % 25 == 0) WriteLine("nchecked " + nchecked + "...");
                if (settings.arglist.Contains("ptc"))
                {
                    if (!o.FastCheckTargetRezed())
                    {
                        Failure("UNCONFIRMED YET: " + o);
                        if (++unconfirmed % 10 == 0) WriteLine("unconfirmed " + unconfirmed + "...");
                    }
                    continue;
                }
                uint parentID = o.Prim.ParentID;
                if (parentID != 0)
                {
                    lock (NewUINT2OBJECT)
                    {
                        PrimToCreate parent;
                        if (!NewUINT2OBJECT.TryGetValue(parentID, out parent))
                        {
                            Failure("UNCONFIRMED PARENT: " + o);  
                        }
                    }
                }
                if (IsLocalScene)
                {
                    continue;
                }
                var found = o.Rezed;
                if (found == null)
                {
                    Failure("UNCONFIRMED: " + o);
                    o.RequestExists();
                }
                else
                {
                    uint fid = found.LocalID;
                    if (o.NewLocalID == fid)
                    {
                        continue;
                    }
                    //o.NewLocalID = fid;
                    o.Rezed = found;
                }
            }
            WriteLine("TOTAL unconfirmed " + unconfirmed + ".");
            WriteLine("TOTAL nchecked " + nchecked + ".");
        }


        private void OnObjectPropertiesFamily(object sender, ObjectPropertiesFamilyEventArgs e)
        {
            var props = e.Properties;
            var id = props.ObjectID;
            UUIDChange utc;
            lock (UUID2OBJECT)
            {
                if (NewUUID2OBJECT.TryGetValue(id, out utc))
                {
                    PrimToCreate ptc = utc as PrimToCreate;
                    if (ptc != null)
                    {
                        ptc.IsConfirmed = true;
                    }
                }
                else
                {
                   // var ptc = APrimToCreate(id);
                   // ptc.IsConfirmed = true;
                }
            }
        }

        private void KillMissing(ImportSettings settings)
        {

            foreach (PrimToCreate parent in LockInfo.CopyOf(parents))
            {
                if (parent.MissingChildern > 0)
                {
                    KillID(parent);
                }
            }
        }

        private void LocatePrims(ImportSettings settings)
        {
            foreach (PrimToCreate toCreate in parents)
            {
                SimObject Rezed = toCreate.Rezed;
                if (Rezed == null)
                {
                    Exporting.AttemptMoveTo(toCreate.SimPosition);
                }
            }
        }
    }
}
