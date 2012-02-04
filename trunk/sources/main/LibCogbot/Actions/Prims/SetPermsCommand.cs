using System;
using System.Collections.Generic;
using System.Threading;
using cogbot.TheOpenSims;
using MushDLR223.Utilities;
using OpenMetaverse;

using MushDLR223.ScriptEngines;

namespace cogbot.Actions.Objects
{
    public class SetPermsCommand : Command, RegionMasterCommand
    {
        //    AutoResetEvent GotPermissionsEvent = new AutoResetEvent(false);
        //   UUID SelectedObject = UUID.Zero;
        Dictionary<UUID, Primitive> Objects = new Dictionary<UUID, Primitive>();
        PermissionMask[] PermsAdd = new PermissionMask[1 + (int)PermissionWho.All];
        PermissionMask[] PermsSub = new PermissionMask[1 + (int)PermissionWho.All];
        bool PermsSent = false;
        int PermCount = 0;
        private bool skipPerms = false;
        private bool setGroup = false;

        //    ObjectManager.ObjectPropertiesCallback callback;
        public SetPermsCommand(BotClient testClient)
        {
            //callback = new ObjectManager.ObjectPropertiesCallback(Objects_OnObjectProperties);

            Name = "setperm";
            Description = "Recursively changes all of the permissions for child and task inventory objects.";
            Category = CommandCategory.Security;
            Usage = "Usage: setperms prim-spec [A][O][G][E][N] [+/-all] [+/-copy] [+/-mod] [+/-xfer] [task] [group [deed] group-spec] [incr]";
            // setperms $all A +all who E -all +copy deed group Logicmoo
            // 
        }

        public override CmdResult Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
            //opensim drew this line because of clients might be hardcoded to only support 255? or was this trying to copy linden?
            try
            {
                //Client.Objects.OnObjectProperties += callback;
                int argsUsed;
                Simulator CurSim = TryGetSim(args, out argsUsed) ?? Client.Network.CurrentSim;
                UUID groupID = UUID.Zero;
                Simulator CurrentSim = CurSim;
                Permissions AddPerms = new Permissions();
                Permissions SubPerms = new Permissions();
                bool doTaskInv = false;
                List<Primitive> TaskPrims = new List<Primitive>();
                List<uint> localIDs = new List<uint>();

                // Reset class-wide variables
                PermsSent = false;
                Objects.Clear();
                PermCount = 0;
                bool oneAtATime = false;

                if (args.Length < 3)
                    return ShowUsage();

                List<SimObject> PS = WorldSystem.GetPrimitives(args, out argsUsed);
                if (IsEmpty(PS)) return Failure("Cannot find objects from " + string.Join(" ", args));

                PermissionWho who = 0;

                bool deed = false;
                for (int i = argsUsed; i < args.Length; i++)
                {

                    bool add = true;
                    bool setPerms = false;
                    string arg = args[i];
                    int whoint = (int)who;
                    PermissionMask Perms = PermsAdd[whoint];
                    if (arg.StartsWith("+"))
                    {
                        arg = arg.Substring(1);
                    }
                    else if (arg.StartsWith("-"))
                    {
                        arg = arg.Substring(1);
                        add = false;
                        Perms = PermsSub[whoint];
                    }

                    switch (arg.ToLower())
                    {

                        // change owner referall
                        case "who":
                            who = 0;
                            break;

                        case "o":
                            who |= PermissionWho.Owner;
                            break;

                        case "g":
                            who |= PermissionWho.Group;
                            break;

                        case "e":
                            who |= PermissionWho.Everyone;
                            break;

                        case "n":
                            who |= PermissionWho.NextOwner;
                            break;

                        case "a":
                            who = PermissionWho.All;
                            break;

                        // change perms for owner
                        case "copy":
                            Perms |= PermissionMask.Copy;
                            setPerms = true;
                            break;
                        case "mod":
                            Perms |= PermissionMask.Modify;
                            setPerms = true;
                            break;
                        case "xfer":
                            Perms |= PermissionMask.Transfer;
                            setPerms = true;
                            break;
                        case "all":
                            Perms |= PermissionMask.All;
                            setPerms = true;
                            break;
                        case "dmg":
                            Perms |= PermissionMask.Damage;
                            setPerms = true;
                            break;
                        case "move":
                            Perms |= PermissionMask.Move;
                            setPerms = true;
                            break;
                        // dont change perms at all
                        case "noperms":
                            skipPerms = true;
                            break;
                        // deed (implies will use group)
                        case "deed":
                            deed = true;
                            break;
                        // set object group
                        case "group":
                            i++;
                            setGroup = true;
                            groupID = Client.GroupName2UUID(args[i]);
                            break;
                        case "task":
                            doTaskInv = true;
                            break;
                        case "incr":
                            oneAtATime = true;
                            break;
                        default:
                            return ShowUsage();
                    }
                    if (setPerms)
                    {
                        skipPerms = false;
                        if (add)
                        {
                            PermsAdd[whoint] = Perms;
                        }
                        else
                        {
                            PermsSub[whoint] = Perms;
                        }
                    }
                }
                ulong CurrentSimHandle = CurrentSim.Handle;
                foreach (SimObject o in PS)
                {
                    if (o is SimAvatar) continue;
                    if (o.RegionHandle != CurrentSimHandle) continue;
                    // Find the requested prim
                    Primitive rootPrim = o.Prim;
                    if (rootPrim == null) continue;
                    localIDs.Add(rootPrim.LocalID);
                    Objects[rootPrim.ID] = rootPrim;
                    if (doTaskInv)
                    {
                        TaskPrims.Add(rootPrim);
                    }

                    continue;
                    ;
                    UUID rootID = UUID.Zero;
                    if (rootPrim == null)
                        return Failure("Cannot find requested prim " + rootID.ToString());
                    else
                        WriteLine("Found requested prim " + rootPrim.ID.ToString(), Client);

                    if (rootPrim.ParentID != 0)
                    {
                        // This is not actually a root prim, find the root
                        if (!CurrentSim.ObjectsPrimitives.TryGetValue(rootPrim.ParentID, out rootPrim))
                            return Failure("Cannot find root prim for requested object");
                        else
                            WriteLine("Set root prim to " + rootPrim.ID.ToString(), Client);
                    }

                    List<Primitive> childPrims;
                    // Find all of the child objects linked to this root
                    childPrims = CurrentSim.ObjectsPrimitives.FindAll(delegate(Primitive prim) { return prim.ParentID == rootPrim.LocalID; });

                    // Build a dictionary of primitives for referencing later
                    // Objects[rootPrim.ID] = rootPrim;
                    for (int i = 0; i < childPrims.Count; i++)
                        Objects[childPrims[i].ID] = childPrims[i];

                    // Build a list of all the localIDs to set permissions for
                    localIDs.Add(rootPrim.LocalID);
                    for (int i = 0; i < childPrims.Count; i++)
                        localIDs.Add(childPrims[i].LocalID);

                    if (doTaskInv)
                    {
                        TaskPrims.AddRange(childPrims);
                        TaskPrims.Add(rootPrim);
                    }

                }

                WriteLine("Using PermissionMask: +" + PermsAdd.ToString() + " -" + PermsSub.ToString(), Client);



                // Go through each of the three main permissions and enable or disable them
                #region Set Linkset Permissions
                if (oneAtATime)
                {

                    List<uint> smallList = new List<uint>();
                    foreach (var o in Objects.Values)
                    {
                        if (o.OwnerID == Client.Self.AgentID)
                        //if (o.GroupID!=groupID)
                        {
                            if (doTaskInv)
                            {
                                TaskPrims.Add(o);
                            }
                            smallList.Clear();
                            smallList.Add(o.LocalID);
                            SetDeed(CurrentSim, smallList, groupID, deed);
                        }
                    }
                }
                else
                {
                    if (localIDs.Count < 50)
                        SetDeed(CurrentSim, localIDs, groupID, deed);
                    else
                    {
                        List<uint> smallList = new List<uint>();
                        while (localIDs.Count > 0)
                        {
                            if (localIDs.Count < 50)
                            {
                                SetDeed(CurrentSim, localIDs, groupID, deed);
                                break;
                            }
                            smallList.Clear();
                            smallList.AddRange(localIDs.GetRange(0, 50));
                            SetDeed(CurrentSim, smallList, groupID, deed);
                            localIDs.RemoveRange(0, 50);
                        }

                    }
                }

                #endregion Set Linkset Permissions

                // Check each prim for task inventory and set permissions on the task inventory
                int taskItems = 0;
                if (doTaskInv)
                    foreach (Primitive prim in TaskPrims)
                    {
                        if ((prim.Flags & PrimFlags.InventoryEmpty) == 0)
                        {
                            List<InventoryBase> items = Client.Inventory.GetTaskInventory(prim.ID, prim.LocalID, 1000 * 10);

                            if (items != null)
                            {
                                for (int i = 0; i < items.Count; i++)
                                {
                                    if (!(items[i] is InventoryFolder))
                                    {
                                        InventoryItem item = (InventoryItem)items[i];

                                        // prev and not (W or All)
                                        item.Permissions.GroupMask &= ~(PermsSub[(int)PermissionWho.Group] | PermsSub[(int)PermissionWho.All]);
                                        item.Permissions.OwnerMask &= ~(PermsSub[(int)PermissionWho.Owner] | PermsSub[(int)PermissionWho.All]);
                                        item.Permissions.NextOwnerMask &= ~(PermsSub[(int)PermissionWho.NextOwner] | PermsSub[(int)PermissionWho.All]);
                                        item.Permissions.EveryoneMask &= ~(PermsSub[(int)PermissionWho.Everyone] | PermsSub[(int)PermissionWho.All]);

                                        // prev and (W or All)
                                        item.Permissions.GroupMask |= PermsAdd[(int)PermissionWho.Group] | PermsAdd[(int)PermissionWho.All];
                                        item.Permissions.OwnerMask |= PermsAdd[(int)PermissionWho.Owner] | PermsAdd[(int)PermissionWho.All];
                                        item.Permissions.NextOwnerMask |= PermsAdd[(int)PermissionWho.NextOwner] | PermsAdd[(int)PermissionWho.All];
                                        item.Permissions.EveryoneMask |= PermsAdd[(int)PermissionWho.Everyone] | PermsAdd[(int)PermissionWho.All];

                                        Client.Inventory.UpdateTaskInventory(prim.LocalID, item);
                                        ++taskItems;
                                    }
                                }
                            }
                        }
                    }

                return Success("Using PermissionMask: +" + PermsAdd.ToString() + " -" + PermsSub.ToString() + " on " + Objects.Count + " objects and " + taskItems + " inventory items");
            }
            finally
            {
                // Client.Objects.OnObjectProperties -= callback;
            }
        }

        private void SetDeed(Simulator CurrentSim, List<uint> localIDs, UUID groupID, bool deed)
        {
            foreach (PermissionWho u in new[] { PermissionWho.All, PermissionWho.Everyone, PermissionWho.Owner, PermissionWho.NextOwner, PermissionWho.Group })
            {
                int whoint = (int)u;
                if (PermsSub[whoint] != PermissionMask.None)
                    Client.Objects.SetPermissions(CurrentSim, localIDs, u, PermsSub[whoint], false);
                if (PermsAdd[whoint] != PermissionMask.None)
                    Client.Objects.SetPermissions(CurrentSim, localIDs, u, PermsAdd[whoint], true);
            }
            if (setGroup) Client.Objects.SetObjectsGroup(CurrentSim, localIDs, groupID);
            if (deed) Client.Objects.DeedObjects(CurrentSim, localIDs, groupID);
        }

        private void SetPerms(Simulator CurrentSim, List<uint> localIDs, PermissionWho who, PermissionMask Perms, bool tf)
        {

            try
            {

                if (!skipPerms)
                {
                    PermCount = 0;

                    if ((Perms & PermissionMask.Modify) == PermissionMask.Modify)
                        Client.Objects.SetPermissions(CurrentSim, localIDs, who, PermissionMask.Modify,
                                                      tf);
                    PermsSent = true;

                    //  if (!GotPermissionsEvent.WaitOne(1000 * 30, false))
                    //     return Failure("failed to set the modify bit, permissions in an unknown state";

                    PermCount = 0;
                    if ((Perms & PermissionMask.Copy) == PermissionMask.Copy)
                        Client.Objects.SetPermissions(CurrentSim, localIDs, who, PermissionMask.Copy,
                                                      tf);

                    PermsSent = true;

                    //  if (!GotPermissionsEvent.WaitOne(1000 * 30, false))
                    //   return Failure("failed to set the copy bit, permissions in an unknown state";

                    PermCount = 0;
                    if ((Perms & PermissionMask.Transfer) == PermissionMask.Transfer)
                        Client.Objects.SetPermissions(CurrentSim, localIDs, who, PermissionMask.Transfer,
                                                      tf);

                    PermsSent = true;

                    PermCount = 0;
                    if ((Perms & PermissionMask.Move) == PermissionMask.Move)
                        Client.Objects.SetPermissions(CurrentSim, localIDs, who, PermissionMask.Transfer,
                                                      tf);

                    PermsSent = true;
                }
                /*
                else
                {
                    Client.Objects.SetPermissions(CurrentSim, localIDs, PermissionWho.Owner, PermissionMask.All, true);
                    Client.Objects.SetPermissions(CurrentSim, localIDs, PermissionWho.Group, PermissionMask.Modify, true);
                    Client.Objects.SetPermissions(CurrentSim, localIDs, PermissionWho.Group, PermissionMask.All, true);
                }
                //  if (!GotPermissionsEvent.WaitOne(1000 * 30, false))
                //    return Failure("failed to set the transfer bit, permissions in an unknown state";
                */
            }
            catch (Exception e)
            {
                DLRConsole.DebugWriteLine("ERROR {0}", e);
            }
        }

        void Objects_OnObjectProperties(Simulator simulator, Primitive.ObjectProperties properties)
        {
            if (PermsSent)
            {
                if (Objects.ContainsKey(properties.ObjectID))
                {
                    // FIXME: Confirm the current operation against properties.Permissions.GroupMask

                    ++PermCount;
                    //   if (PermCount >= Objects.Count)
                    //      GotPermissionsEvent.Set();
                }
            }
        }
    }
}
