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
    public partial class ImportCommand 
    {
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

        private void ImportPrims(Simulator CurSim, UUID GroupID)
        {
            parents = new List<PrimToCreate>();
            childs = new List<PrimToCreate>();
            var taskobjs = new List<PrimToCreate>();

            if (diskPrims.Count == 0)
            {
                foreach (var file in Directory.GetFiles(ExportCommand.dumpDir, "*.llsd"))
                {

                    Primitive prim = (Primitive)ExportCommand.FromFile(file, ExportCommand.UseBinarySerialization);
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
                SetPermissionsAll(CurSim, new List<uint>() {ptc.NewLocalID});
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
                var linkset = new List<uint>(uuids.Length) { parent.NewLocalID };
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
                // set perms for linking!?
                SetPermissionsAll(CurSim, linkset);
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
            WriteLine("Imported P=" + parents.Count + " C=" + childs.Count);
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
                    return (PrimToCreate)utc;
                }
                if (ptc != null) return ptc;
                ptc = new PrimToCreate(primitive);
                if (primitive.Sculpt != null)
                {
                    var texture = primitive.Sculpt.SculptTexture;
                    if (texture != null)
                    {
                        var t2c = (ItemToCreate)(GetOld(texture) ?? GetNew(texture));
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
                    pp = prim.Position * Matrix4.CreateFromQuaternion(parent.Prim.Rotation) + parent.Prim.Position;
                    pr = parent.Prim.Rotation * pr;
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
            Client.Objects.SetPermissions(CurSim, new List<uint>() {localID},
                                          PermissionWho.Everyone | PermissionWho.Group | PermissionWho.NextOwner |
                                          PermissionWho.Owner | PermissionWho.Base,
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
    }
}
