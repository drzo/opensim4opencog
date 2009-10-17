using System;
using System.Collections.Generic;
using System.Text;
using System.Threading;
using OpenMetaverse;
using OpenMetaverse.Packets;

namespace cogbot.Actions
{
    public class SetMasterCommand : Command //,BotSystemCommand
    {
        public SetMasterCommand(BotClient testClient)
        {
            Name = "setmaster";
            Description = "Sets the user name of the master user. The master user can IM to run commands. Usage: setmaster [name]";
            Category = CommandCategory.TestClient;
        }

        public override CmdResult Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
            string masterName = String.Empty;
            for (int ct = 0; ct < args.Length; ct++)
                masterName = masterName + args[ct] + " ";
            masterName = masterName.TrimEnd();
            if (!string.IsNullOrEmpty(masterName)) TheBotClient.MasterName = masterName;
            return Success(string.Format("Master set to {0} ({1})", Client.MasterName, Client.MasterKey.ToString()));
        }
    }
}
