using System;
using System.Collections.Generic;
using System.Text;
using OpenMetaverse;
using OpenMetaverse.Packets;

using MushDLR223.ScriptEngines;

namespace cogbot.Actions.System
{
    public class SetMasterKeyCommand : Command, BotSystemCommand
    {

        public SetMasterKeyCommand(BotClient testClient)
        {
            Name = "setMasterKey";
            Description = "A bot can have a master - another account whose IM's are accepted as botcmds. " +
                "You can set the master in botconfig.xml or via this command.";
            Details = AddUsage("setmasterkey <uuid>", "Sets the master by UUID") +
                AddUsage("setmasterkey <name>", "Sets the master by user name");
            Parameters = CreateParams("master", typeof(AgentSpec), "name or UUID of master agent");

            Category = CommandCategory.Security;
        }

        public override CmdResult ExecuteRequest(CmdRequest args)
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
