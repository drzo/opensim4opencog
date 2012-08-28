using System;
using System.Collections.Generic;
using System.Text;
using Cogbot.World;
using OpenMetaverse;
using OpenMetaverse.Packets;
using MushDLR223.ScriptEngines;

namespace Cogbot.Actions.System
{
    public class SetMasterKeyCommand : Command, BotSystemCommand, SynchronousCommand
    {
        public SetMasterKeyCommand(BotClient testClient)
        {
            Name = "setMasterKey";
        }

        public override void MakeInfo()
        {
            Description = "A bot can have a master - another account whose IM's are accepted as botcmds. " +
                          "You can set the master in botconfig.xml or via this command.";
            Details = AddUsage("setmasterkey <uuid>", "Sets the master by UUID") +
                      AddUsage("setmasterkey <name>", "Sets the master by user name");
            Parameters = CreateParams(Rest("master", typeof (AgentSpec), "name or UUID of master agent"));

            Category = CommandCategory.Security;
        }

        public override CmdResult ExecuteRequest(CmdRequest args)
        {
            UUID masterUUID;
            if (!args.TryGetValue("master", out masterUUID))
            {
                TheBotClient.MasterName = args.GetString("master");
            }
            else
            {
                TheBotClient.MasterKey = masterUUID;
            }
            return Success(string.Format("Master set to {0} ({1})", Client.MasterName, Client.MasterKey.ToString()));
        }
    }
}