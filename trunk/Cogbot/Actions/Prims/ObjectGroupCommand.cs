using System;
using System.Collections.Generic;
using System.Threading;
using MushDLR223.Utilities;
using OpenMetaverse;

using MushDLR223.ScriptEngines;

namespace cogbot.Actions.Objects
{
    public class ObjectGroupCommand : Command, RegionMasterCommand
    {
    //    AutoResetEvent GotPermissionsEvent = new AutoResetEvent(false);
     //   UUID SelectedObject = UUID.Zero;
        Dictionary<UUID, Primitive> Objects = new Dictionary<UUID, Primitive>();
        PermissionMask Perms = PermissionMask.None;
        bool PermsSent = false;
        int PermCount = 0;
        bool setPerms = false;

        //    ObjectManager.ObjectPropertiesCallback callback;
        public ObjectGroupCommand(BotClient testClient)
        {
            //callback = new ObjectManager.ObjectPropertiesCallback(Objects_OnObjectProperties);

            Name = "deed";
            Description = "Recursively changes all of the permissions for child and task inventory objects. Usage prim-uuid [copy] [mod] [xfer]";
            Category = CommandCategory.Security;
            Usage = "Usage: deed group-spec prim-uuid [copy] [mod] [xfer] [deed]";
        }

        public override CmdResult Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
            //opensim drew this line because of clients might be hardcoded to only support 255? or was this trying to copy linden?
            try
            {
                //Client.Objects.OnObjectProperties += callback;
                int argsUsed;
                Simulator CurSim = TryGetSim(args, out argsUsed) ?? Client.Network.CurrentSim;
                UUID rootID;
                UUID groupID;
                Simulator CurrentSim = CurSim;

                bool doTaskInv = false;
                List<Primitive> TaskPrims = new List<Primitive>();
                List<uint> localIDs = new List<uint>();

                // Reset class-wide variables
                PermsSent = false;
                Objects.Clear();
                Perms = PermissionMask.None;
                PermCount = 0;
                bool doIncr = false;

                if (args.Length < 3)
                    return ShowUsage();

                if (!UUIDTryParse(args, 0, out groupID, out argsUsed))
                    return ShowUsage();


                bool deed = false;
                for (int i = 2; i < args.Length; i++)
                {
                    switch (args[i].ToLower())
                    {
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
                        case "noperms":
                            setPerms = false;
                            break;
                        case "deed":
                            deed = true;
                            break;
                        case "task":
                            doTaskInv = true;
                            break;
                        case "incr":
                            doIncr = true;
                            break;
                        default:
                            return ShowUsage();
                    }
                }

                int argsUsed2;
                if (args[1]!="*" && UUIDTryParse(args, 1, out rootID, out argsUsed2))
                {
                    // Find the requested prim
                   Primitive rootPrim = CurrentSim.ObjectsPrimitives.Find(delegate(Primitive prim) { return prim.ID == rootID; });

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

                } else
                {
                    lock (CurrentSim.ObjectsPrimitives.Dictionary)
                    {
                        localIDs.AddRange(CurrentSim.ObjectsPrimitives.Dictionary.Keys);
                        foreach(var tt in  CurrentSim.ObjectsPrimitives.Dictionary)
                        {
                            Objects.Add(tt.Value.ID,tt.Value);
                        }
                    }
                }

                WriteLine("Using PermissionMask: " + Perms.ToString(), Client);

                 

                // Go through each of the three main permissions and enable or disable them
                #region Set Linkset Permissions
                if (doIncr)
                {

                    List<uint> smallList = new List<uint>();
                    foreach (var o in Objects.Values)
                    {
                        if (o.OwnerID==Client.Self.AgentID)
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
                                    item.Permissions.GroupMask = Perms;

                                    Client.Inventory.UpdateTaskInventory(prim.LocalID, item);
                                    ++taskItems;
                                }
                            }
                        }
                    }
                }

                return Success("Set permissions to " + Perms.ToString() + " on " + Objects.Count + " objects and " + taskItems + " inventory items");
            }
            finally
            {
               // Client.Objects.OnObjectProperties -= callback;
            }
        }

        private void SetDeed(Simulator CurrentSim, List<uint> localIDs, UUID groupID, bool deed)
        {
            try
            {

                if (setPerms)
                {
                    PermCount = 0;

                    if ((Perms & PermissionMask.Modify) == PermissionMask.Modify)
                        Client.Objects.SetPermissions(CurrentSim, localIDs, PermissionWho.Group, PermissionMask.Modify,
                                                      true);
                    else
                        Client.Objects.SetPermissions(CurrentSim, localIDs, PermissionWho.Group, PermissionMask.Modify,
                                                      false);
                    PermsSent = true;

                    //  if (!GotPermissionsEvent.WaitOne(1000 * 30, false))
                    //     return Failure("failed to set the modify bit, permissions in an unknown state";

                    PermCount = 0;
                    if ((Perms & PermissionMask.Copy) == PermissionMask.Copy)
                        Client.Objects.SetPermissions(CurrentSim, localIDs, PermissionWho.Group, PermissionMask.Copy,
                                                      true);
                    else
                        Client.Objects.SetPermissions(CurrentSim, localIDs, PermissionWho.Group, PermissionMask.Copy,
                                                      false);
                    PermsSent = true;

                    //  if (!GotPermissionsEvent.WaitOne(1000 * 30, false))
                    //   return Failure("failed to set the copy bit, permissions in an unknown state";

                    PermCount = 0;
                    if ((Perms & PermissionMask.Transfer) == PermissionMask.Transfer)
                        Client.Objects.SetPermissions(CurrentSim, localIDs, PermissionWho.Group, PermissionMask.Transfer,
                                                      true);
                    else
                        Client.Objects.SetPermissions(CurrentSim, localIDs, PermissionWho.Group, PermissionMask.Transfer,
                                                      false);
                    PermsSent = true;

                    PermCount = 0;
                    if ((Perms & PermissionMask.Transfer) == PermissionMask.Transfer)
                        Client.Objects.SetPermissions(CurrentSim, localIDs, PermissionWho.Group, PermissionMask.Transfer,
                                                      true);
                    else
                        Client.Objects.SetPermissions(CurrentSim, localIDs, PermissionWho.Group, PermissionMask.Transfer,
                                                      false);
                    PermsSent = true;
                } else
                {
                    Client.Objects.SetPermissions(CurrentSim, localIDs, PermissionWho.Owner, PermissionMask.All, true);
                    Client.Objects.SetPermissions(CurrentSim, localIDs, PermissionWho.Group, PermissionMask.Modify, true);
                    Client.Objects.SetPermissions(CurrentSim, localIDs, PermissionWho.Group, PermissionMask.All,true);
                }
                //  if (!GotPermissionsEvent.WaitOne(1000 * 30, false))
                //    return Failure("failed to set the transfer bit, permissions in an unknown state";

                Client.Objects.SetObjectsGroup(CurrentSim, localIDs, groupID);
                if (deed) Client.Objects.DeedObjects(CurrentSim, localIDs, groupID);

            }
            catch (Exception e)
            {
                DLRConsole.SystemWriteLine("" + e);
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
