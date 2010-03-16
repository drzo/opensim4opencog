using System;
using System.Collections.Generic;
using System.Text;
using System.Threading;
using OpenMetaverse;
using OpenMetaverse.Packets;
using OpenMetaverse.Assets;

namespace cogbot.Actions.Inventory
{
    public class ViewNotecardCommand : Command, BotPersonalCommand
    {
        /// <summary>
        /// BotClient command to download and display a notecard asset
        /// </summary>
        /// <param name="testClient"></param>
        public ViewNotecardCommand(BotClient testClient)
        {
            Name = "viewnote";
            Description = "Downloads and displays a notecard asset";
			Usage = "viewnote [notecard asset uuid]";
            Category = CommandCategory.Inventory;
        }

        /// <summary>
        /// Exectute the command
        /// </summary>
        /// <param name="args"></param>
        /// <param name="fromAgentID"></param>
        /// <returns></returns>
        public override CmdResult Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {

            if (args.Length < 1)
            {
                return ShowUsage();// " viewnote [notecard asset uuid]";
            }
            UUID note;
            if (!UUID.TryParse(args[0], out note))
            {
                return Failure( "First argument expected UUID.");
            }

             AutoResetEvent waitEvent = new AutoResetEvent(false);

            StringBuilder result = new StringBuilder();

            // verify asset is loaded in store
            if (Client.Inventory.Store.Contains(note))
            {
                // retrieve asset from store
                InventoryItem ii = (InventoryItem)Client.Inventory.Store[note];

                // make request for asset
                Client.Assets.RequestInventoryAsset(ii, true,
                    delegate(AssetDownload transfer, Asset asset)
                    {
                        if (transfer.Success)
                        {
                            result.AppendFormat("Raw Notecard Data: " + Environment.NewLine + " {0}", Utils.BytesToString(asset.AssetData));
                            waitEvent.Set();
                        }
                    }
                );

                // wait for reply or timeout
                if (!waitEvent.WaitOne(10000, false))
                {
                    result.Append("Timeout waiting for notecard to download.");
                }
            }
            else
            {
                result.Append("Cannot find asset in inventory store, use 'i' to populate store");
            }

            // return results
            return Success(result.ToString());;
        }
    }
}
