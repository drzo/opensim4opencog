using System;
using System.Globalization;
using System.IO;
using System.Reflection;
using System.Threading;
using System.Windows.Forms;
using cogbot.Listeners;
using OpenMetaverse;
using OpenMetaverse.Assets;
using Radegast;

namespace cogbot.Actions.SimExport
{
    public class ShowImageCommand : Command, GridMasterCommand
    {

        public ShowImageCommand(BotClient testClient)
        {
            Name = "ShowImage";
            Description = "Shows the specified image. Usage: ShowImage [uuid]";
            Category = CommandCategory.Inventory;
        }

        public override CmdResult Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
            if (args.Length < 1) return ShowUsage();
            UUID AssetID;
            int argsUsed;

            if (!UUIDTryParse(args, 0, out AssetID, out argsUsed))
            {
                return Failure("Cant get asset from " + AssetID);
            }

            TheBotClient.InvokeGUI(() =>
                                    {
                                        var v = new SLImageHandler(TheBotClient.TheRadegastInstance, AssetID,
                                                                   AssetID.ToString()) {Dock = DockStyle.Fill};
                                        var f = new RadegastForm();
                                        f.Controls.Add(v);
                                        f.Show();
                                    });
            
            return Success("Done RequestAsset " + AssetID);
        }
    }
}
