using System;
using System.Collections.Generic;
using System.Text;
using Cogbot.World;
using OpenMetaverse;
using OpenMetaverse.Packets;

using MushDLR223.ScriptEngines;

namespace Cogbot.Actions.Movement
{
    public class StandCommand : Command, BotPersonalCommand
    {
        public StandCommand(BotClient testClient)
        {
            Description = "Stand up.  OK to call it if already standing";
            Details = "stand";
            Parameters = CreateParams();
            ResultMap = CreateParams(
                "message", typeof (string), "if we could not stand up, why (shouldnt happen)",
                "success", typeof (bool), "true if we stood up");
            Category = CommandCategory.Movement;
        }

        public override CmdResult ExecuteRequest(CmdRequest args)
	    {
            SimActor sitter = WorldSystem.TheSimAvatar;
            if (!sitter.IsSitting)
            {
                Success("$bot is already standing.");
            }
            else
            {
                sitter.StandUp();
                Success("Standing up.");  ;
            }
            Client.Self.Stand();
            return SuccessOrFailure();
	    }
    }
}
