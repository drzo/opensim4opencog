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
			Description = @"Touch a prim that meets a specified <a href='wiki/BotCommands#PrimSpec'>Prim Spec</a>.
Ignores the object's default action, always touches.";
            Usage = "touch [primspec]- see <a href='wiki/BotCommands#PrimSpec'>Prim Spec</a>"; 
            Category = CommandCategory.Objects;
            Parameters = NamedParam("object", typeof(SimObject),
                "The object to touch, as specified in <a href='wiki/BotCommands#PrimSpec'>Prim Spec</a>");
            ResultMap = NamedParam.CreateParams(
                 "message", typeof(string), "if success was false, the reason why",
                 "success", typeof(bool), "true if we touched the object");
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
