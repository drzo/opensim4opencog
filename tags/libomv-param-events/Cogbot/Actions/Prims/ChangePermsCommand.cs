using System;
using System.Collections.Generic;
using System.Threading;
using cogbot.TheOpenSims;
using OpenMetaverse;

namespace cogbot.Actions
{
    public class ChangePermsCommand : Command, RegionMasterCommand
    {
        public ChangePermsCommand(BotClient testClient)
        {
            Name = "changeperms";
            Description = "Recursively changes all of the permissions for child and task inventory objects. Usage prim-uuid [copy] [mod] [xfer]";
            Category = CommandCategory.Objects;
        }

        public override CmdResult Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
            bool permsSent = false;
            int permCount = 0;
            AutoResetEvent GotPermissionsEvent = new AutoResetEvent(false);
            UUID selectedObject = UUID.Zero;
            Dictionary<UUID, Primitive> objects = new Dictionary<UUID, Primitive>();
            PermissionMask perms = PermissionMask.None;

            ObjectManager.ObjectPropertiesCallback callback = new ObjectManager.ObjectPropertiesCallback((simulator, properties) =>
                                                                                                             {
                                                                                                                 if (permsSent)
                                                                                                                 {
                                                                                                                     if (objects.ContainsKey(properties.ObjectID))
                                                                                                                     {
                                                                                                                         // FIXME: Confirm the current operation against properties.Permissions.NextOwnerMask

                                                                                                                         ++permCount;
                                                                                                                         if (permCount >= objects.Count)
                                                                                                                             GotPermissionsEvent.Set();
                                                                                                                     }
                                                                                                                 }
                                                                                                             });
            try
            {
                Client.Objects.OnObjectProperties += callback;

                // Reset class-wide variables
                permsSent = false;
                objects.Clear();
                perms = PermissionMask.None;
                permCount = 0;

                if (args.Length < 1 || args.Length > 4)
                    return ShowUsage(); //"Usage prim-uuid [copy] [mod] [xfer]";

                int argsUsed;
                List<SimObject> PS = WorldSystem.GetPrimitives(args, out argsUsed);
                if (IsEmpty(PS)) return Failure("Cannot find objects from " + string.Join(" ", args));

                for (int i = argsUsed; i < args.Length; i++)
                {
                    switch (args[i].ToLower())
                    {
                        case "copy":
                            perms |= PermissionMask.Copy;
                            break;
                        case "mod":
                            perms |= PermissionMask.Modify;
                            break;
                        case "xfer":
                            perms |= PermissionMask.Transfer;
                            break;
                        default:
                            return ShowUsage(); //"Usage prim-uuid [copy] [mod] [xfer]";
                    }
                }

                WriteLine("Using PermissionMask: " + perms.ToString());

                // Find the requested prim

                foreach (var rPrim in PS)
                {
                    Primitive rootPrim = rPrim.Prim;

                    Simulator curSim = WorldSystem.GetSimulator(rootPrim);

                    WriteLine("Found requested prim " + rootPrim.ID.ToString());

                    if (rootPrim.ParentID != 0)
                    {
                        // This is not actually a root prim, find the root
                        if (!curSim.ObjectsPrimitives.TryGetValue(rootPrim.ParentID, out rootPrim))
                            Failure("Cannot find root prim for requested object");
                        else
                            Logger.DebugLog("Set root prim to " + rootPrim.ID.ToString(), Client);
                    }

                    List<Primitive> childPrims;
                    // Find all of the child objects linked to this root
                    childPrims =
                        curSim.ObjectsPrimitives.FindAll(
                            delegate(Primitive prim) { return prim.ParentID == rootPrim.LocalID; });

                    // Build a dictionary of primitives for referencing later
                    objects[rootPrim.ID] = rootPrim;
                    for (int i = 0; i < childPrims.Count; i++)
                        objects[childPrims[i].ID] = childPrims[i];

                    List<uint> localIDs = new List<uint>();
                    // Build a list of all the localIDs to set permissions for
                    localIDs.Add(rootPrim.LocalID);
                    for (int i = 0; i < childPrims.Count; i++)
                        localIDs.Add(childPrims[i].LocalID);

                    // Go through each of the three main permissions and enable or disable them

                    #region Set Linkset Permissions

                    permCount = 0;
                    if ((perms & PermissionMask.Modify) == PermissionMask.Modify)
                        Client.Objects.SetPermissions(curSim, localIDs, PermissionWho.NextOwner,
                                                      PermissionMask.Modify, true);
                    else
                        Client.Objects.SetPermissions(curSim, localIDs, PermissionWho.NextOwner,
                                                      PermissionMask.Modify, false);
                    permsSent = true;

                    if (!GotPermissionsEvent.WaitOne(1000*30, false))
                        Failure("failed to set the modify bit, permissions in an unknown state");

                    permCount = 0;
                    if ((perms & PermissionMask.Copy) == PermissionMask.Copy)
                        Client.Objects.SetPermissions(curSim, localIDs, PermissionWho.NextOwner,
                                                      PermissionMask.Copy, true);
                    else
                        Client.Objects.SetPermissions(curSim, localIDs, PermissionWho.NextOwner,
                                                      PermissionMask.Copy, false);
                    permsSent = true;

                    if (!GotPermissionsEvent.WaitOne(1000*30, false))
                        Failure("failed to set the copy bit, permissions in an unknown state");

                    permCount = 0;
                    if ((perms & PermissionMask.Transfer) == PermissionMask.Transfer)
                        Client.Objects.SetPermissions(curSim, localIDs, PermissionWho.NextOwner,
                                                      PermissionMask.Transfer, true);
                    else
                        Client.Objects.SetPermissions(curSim, localIDs, PermissionWho.NextOwner,
                                                      PermissionMask.Transfer, false);
                    permsSent = true;

                    if (!GotPermissionsEvent.WaitOne(1000*30, false))
                        Failure("failed to set the transfer bit, permissions in an unknown state");

                    #endregion Set Linkset Permissions

                    // Check each prim for task inventory and set permissions on the task inventory
                    int taskItems = 0;
                    foreach (Primitive prim in objects.Values)
                    {
                        if ((prim.Flags & PrimFlags.InventoryEmpty) == 0)
                        {
                            List<InventoryBase> items = Client.Inventory.GetTaskInventory(prim.ID, prim.LocalID, 1000*30);

                            if (items != null)
                            {
                                for (int i = 0; i < items.Count; i++)
                                {
                                    if (!(items[i] is InventoryFolder))
                                    {
                                        InventoryItem item = (InventoryItem) items[i];
                                        item.Permissions.NextOwnerMask = perms;

                                        Client.Inventory.UpdateTaskInventory(prim.LocalID, item);
                                        ++taskItems;
                                    }
                                }
                            }
                        }
                    }

                    Success("Set permissions to " + perms.ToString() + " on " + localIDs.Count + " objects and " +
                                taskItems + " inventory items");
                }
            }
            finally
            {
                Client.Objects.OnObjectProperties -= callback;
            }
            return SuccessOrFailure();
        }
    }
}
