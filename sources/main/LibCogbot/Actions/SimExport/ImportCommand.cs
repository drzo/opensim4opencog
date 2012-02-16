using System;
using System.Collections.Generic;
using System.Threading;
using System.IO;
using cogbot.Actions.SimExport;
using cogbot.Listeners;
using cogbot.TheOpenSims;
using OpenMetaverse;
using OpenMetaverse.StructuredData;

using MushDLR223.ScriptEngines;

namespace cogbot.Actions.Objects
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
        public class PrimToCreate
        {
            public PrimToCreate(Primitive prim)
            {
                Prim = prim;
            }
            public Primitive Prim;
            public SimObject Rezed;

        }

        Primitive currentPrim;
        Vector3 currentPosition;
        AutoResetEvent primDone = new AutoResetEvent(false);
        List<Primitive> primsCreated;
        List<uint> linkQueue;
        uint rootLocalID;
        ImporterState state = ImporterState.Idle;
        EventHandler<PrimEventArgs> callback;
        public static readonly Dictionary<UUID,PrimToCreate> UUID2OBJECT = new Dictionary<UUID, PrimToCreate>();

        public ImportCommand(BotClient testClient)
        {
            Name = "simimport";
            Description = "Import prims from an exported xml file. Usage: import inputfile.xml [usegroup]";
            Category = CommandCategory.Objects;
        }


        public override CmdResult Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
            UUID GroupID = (args.Length > 1) ? TheBotClient.GroupID : UUID.Zero;
            var CurSim = Client.Network.CurrentSim;
            var parents = new List<PrimToCreate>();
            var childs = new List<PrimToCreate>();
            var taskobjs = new List<PrimToCreate>();

            foreach (var file in Directory.GetFiles(ExportCommand.dumpDir, "*.llsd"))
            {

                Primitive prim = (Primitive) ExportCommand.FromFile(file);
                if (prim.ParentID != 0)
                {
                    childs.Add(APrimToCreate(prim));
                    continue;
                }
                parents.Add(APrimToCreate(prim));                
            }
            foreach (PrimToCreate ptc in parents)
            {
                CreatePrim(CurSim, ptc, GroupID);
            }
            foreach (PrimToCreate ptc in childs)
            {
                CreatePrim(CurSim, ptc, GroupID);
            }
            foreach (var file in Directory.GetFiles(ExportCommand.dumpDir, "*.link"))
            {
                var uuids = ExportCommand.GetUUIDs(File.ReadAllText(file));
                if (uuids.Length<1) continue;
                var linkset = new List<uint>(uuids.Length);
                for (int i = uuids.Length - 1; i >= 0; i--)
                {
                    linkset.Add(UUID2OBJECT[uuids[i]].Rezed.LocalID);
                }
                //linkset.Add(UUID2OBJECT[uuids[0]].Rezed.LocalID);
                Client.Objects.LinkPrims(CurSim, linkset);
            }
            return Success("Imported P=" + parents.Count + " C=" + childs.Count);
        }

        private static PrimToCreate APrimToCreate(Primitive primitive)
        {
            var ptc = new PrimToCreate(primitive);
            UUID2OBJECT.Add(primitive.ID, ptc);
            return ptc;
        }

        private void CreatePrim(Simulator CurSim, PrimToCreate ptc, UUID GroupID)
        {
            Primitive prim = ptc.Prim;
            Primitive newPrim = null;
            // Register a handler for the creation event
            AutoResetEvent creationEvent = new AutoResetEvent(false);
            var loc = prim.Position;
            Quaternion rot = Quaternion.Identity;
            EventHandler<PrimEventArgs> callback =
                (s, e) =>//delegate(Simulator simulator0, Primitive prim, ulong regionHandle, ushort timeDilation)
                    {
                        var regionHandle = e.Simulator.Handle;
                        var ePrim = e.Prim;
                        if (regionHandle != CurSim.Handle) return;
                        if ((loc - ePrim.Position).Length() > 3)
                        {
                            Debug("Not the prim " + (loc - ePrim.Position).Length());
                            return;
                        }
                        if (ePrim.PrimData.ProfileHole != prim.PrimData.ProfileHole)
                        {
                            Debug("Not the prim?  PrimData.ProfileHole: {0}!={1}", ePrim.PrimData.ProfileHole,
                                  prim.PrimData.ProfileHole);
                            // return;       //
                        }
                        if (prim.PrimData.Material != ePrim.PrimData.Material)
                        {
                            Debug("Not the prim? PrimData.Material: {0}!={1}", prim.PrimData.ProfileHole,
                                  ePrim.PrimData.Material);
                            // return;
                        }
                        if ((ePrim.Flags & PrimFlags.CreateSelected) == 0)
                        {
                            Debug("Not the prim? (prim.Flags & PrimFlags.CreateSelected) == 0) was {0}", ePrim.Flags);
                            // return;
                        }
                        if (prim.Type != ePrim.Type)
                        {
                            Debug("Not the prim? Material.Light != prim.PrimData.Material: {0}!={1}", ePrim.Type,
                                  prim.Type);
                            // return;
                        }
                        //if (prim.Scale != scale) return;
                        //     if (prim.Rotation != rot) return;

                        //  if (Material.Light != prim.PrimData.Material) return;
                        //if (CD != prim.PrimData) return;
                        newPrim = ePrim;
                        creationEvent.Set();
                    };

            Client.Objects.ObjectUpdate += callback;
            Client.Objects.AddPrim(CurSim, prim.PrimData, GroupID, prim.Position, prim.Scale, prim.Rotation, PrimFlags.CreateSelected | PrimFlags.Phantom | PrimFlags.Temporary);
            if (!creationEvent.WaitOne(10000) || newPrim == null)
            {
                Debug("Error - no prim");
                return;
            }
            ptc.Rezed = WorldSystem.GetSimObject(newPrim);
            Primitive.ObjectProperties props = prim.Properties;
            uint localID = newPrim.LocalID;
            List<uint> prims = new List<uint> { localID };
            Client.Objects.SetName(CurSim, localID, props.Name);
            Client.Objects.SetDescription(CurSim, localID, props.Description);
            //Client.Objects.SetExtraParamOff(CurSim, localID, prim.);
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
            if (prim.Textures != null) Client.Objects.SetTextures(CurSim, localID, prim.Textures);
            if (prim.Light != null) Client.Objects.SetLight(CurSim, localID, prim.Light);

            if (!CogbotHelpers.IsNullOrZero(prim.GroupID))
            {
                if (!CogbotHelpers.IsNullOrZero(GroupID)) Client.Objects.SetObjectsGroup(CurSim, prims, GroupID);
            }
            Client.Objects.SetPermissions(CurSim, new List<uint>() { ptc.Rezed.LocalID },
                                          PermissionWho.Everyone | PermissionWho.Group | PermissionWho.NextOwner,
                                          PermissionMask.All, true);
            Client.Objects.SetPosition(CurSim, localID, prim.Position);
            Client.Objects.SetRotation(CurSim, localID, prim.Rotation);
            Client.Objects.SetSaleInfo(CurSim, localID, props.SaleType, props.SalePrice);
            if (prim.Flexible != null) Client.Objects.SetFlexible(CurSim, localID, prim.Flexible);
            if (prim.Sculpt != null) Client.Objects.SetSculpt(CurSim, localID, prim.Sculpt);
            Client.Objects.SetMaterial(CurSim, localID, prim.PrimData.Material);
            Client.Objects.SetScale(CurSim, localID, prim.Scale, true, false);
        }

        static private bool FlagSet(PrimFlags flags, PrimFlags set)
        {
            return (flags & set) == set;
        }

        private void Debug(string s, params object[] ps)
        {
            //throw new NotImplementedException();
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
