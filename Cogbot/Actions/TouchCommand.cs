using System;
using System.Collections.Generic;
using cogbot.TheOpenSims;
using OpenMetaverse;

using MushDLR223.ScriptEngines;

namespace cogbot.Actions
{
    public class TouchCommand: Command, BotPersonalCommand
    {
        public TouchCommand(BotClient testClient)
		{
			Name = "touch";
			Description = "Attempt to touch a prim with specified UUID";
            Category = CommandCategory.Objects;
            Parameters = new [] {  new NamedParam(typeof(SimObject), typeof(UUID)) };
		}
		
        public override CmdResult Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
		{
            if (args.Length < 1)
                return ShowUsage();// " touch UUID";

            int argsUsed;
            List<SimObject> PS = WorldSystem.GetPrimitives(args, out argsUsed);
            if (IsEmpty(PS)) return Failure("Cannot find objects from " + string.Join(" ", args));
            GridClient client = TheBotClient;
            foreach (var targetPrim in PS)
            {
                Success(Name + " on " + targetPrim);
                Client.Self.Touch(targetPrim.LocalID);
            }
            return SuccessOrFailure();
        }
    }
}
