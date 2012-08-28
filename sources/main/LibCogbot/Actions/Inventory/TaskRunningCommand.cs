using System;
using System.Collections;
using System.Collections.Generic;
using System.Threading;
using Cogbot.World;
using OpenMetaverse;
using MushDLR223.ScriptEngines;

namespace Cogbot.Actions.Objects
{
    public class TaskRunningCommand : Command, RegionMasterCommand
    {
        public TaskRunningCommand(BotClient testClient)
        {
            Name = "taskrunning";
        }

        public override void MakeInfo()
        {
            Description =
                "Retrieves or set IsRunning flag on items inside an object (task inventory). Usage: taskrunning objectID [[scriptName] true|false]";
            Category = CommandCategory.Inventory;
            Parameters = CreateParams(Optional("--set", typeof (string), "The value to " + Name),
                                      Optional("--match", typeof (string), "The value to " + Name),
                                      "targets", typeof (PrimSpec), "The targets of " + Name);
        }

        public override CmdResult ExecuteRequest(CmdRequest args)
        {
            if (args.Length != 1)
                return ShowUsage(); // " taskrunning objectID [[scriptName] true|false]";


            List<SimObject> PS;
            if (!args.TryGetValue("targets", out PS) || IsEmpty(PS as ICollection))
            {
                PS = WorldSystem.GetAllSimObjects();
            }

            string matching = args.GetString("match");
            string tf = args.GetString("set").ToLower();
            foreach (var found in PS)
            {
                uint objectLocalID = found.LocalID;
                UUID objectID = found.ID;


                List<InventoryBase> items = Client.Inventory.GetTaskInventory(objectID, objectLocalID, 1000*30);

                //bool wantSet = false;
                bool setTaskTo = false;
                if (items != null)
                {
                    string result = String.Empty;
                    bool setAny = false;
                    if (tf == "true")
                    {
                        setAny = true;
                        setTaskTo = true;
                    }
                    else if (tf == "false")
                    {
                        setAny = true;
                        setTaskTo = false;
                    }
                    bool wasRunning = false;

                    EventHandler<ScriptRunningReplyEventArgs> callback;
                    using (AutoResetEvent OnScriptRunningReset = new AutoResetEvent(false))
                    {
                        callback = ((s, e) =>
                                        {
                                            if (e.ObjectID == objectID)
                                            {
                                                result += String.Format(" IsMono: {0} IsRunning: {1}", e.IsMono,
                                                                        e.IsRunning);
                                                wasRunning = e.IsRunning;
                                                OnScriptRunningReset.Set();
                                            }
                                        });

                        Client.Inventory.ScriptRunningReply += callback;

                        for (int i = 0; i < items.Count; i++)
                        {
                            if (items[i] is InventoryFolder)
                            {
                                // this shouldn't happen this year
                                result += String.Format("[Folder] Name: {0}", items[i].Name) + Environment.NewLine;
                            }
                            else
                            {
                                InventoryItem item = (InventoryItem) items[i];
                                AssetType assetType = item.AssetType;
                                result += String.Format("[Item] Name: {0} Desc: {1} Type: {2}", item.Name,
                                                        item.Description,
                                                        assetType);
                                if (assetType == AssetType.LSLBytecode || assetType == AssetType.LSLText)
                                {
                                    OnScriptRunningReset.Reset();
                                    Client.Inventory.RequestGetScriptRunning(objectID, item.UUID);
                                    if (!OnScriptRunningReset.WaitOne(10000, true))
                                    {
                                        result += " (no script info)";
                                    }
                                    if (setAny && item.Name.Contains(matching))
                                    {
                                        if (wasRunning != setTaskTo)
                                        {
                                            OnScriptRunningReset.Reset();
                                            result += " Setting " + setTaskTo + " => ";
                                            Client.Inventory.RequestSetScriptRunning(objectID, item.UUID, setTaskTo);
                                            if (!OnScriptRunningReset.WaitOne(10000, true))
                                            {
                                                result += " (was not set)";
                                            }
                                        }
                                    }
                                }

                                result += Environment.NewLine;
                            }
                        }
                    }
                    Client.Inventory.ScriptRunningReply -= callback;
                    AddSuccess(result);
                }
                else
                {
                    Failure("failed to download task inventory for " + objectLocalID);
                }
            }
            return SuccessOrFailure();
        }
    }
}