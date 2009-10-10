using System;
using cogbot.TheOpenSims;
using OpenMetaverse;

namespace cogbot.Actions
{
    public class TouchCommand: Command
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
            UUID target;

            if (args.Length < 1)
                return Failure(Usage);// " touch UUID";
            
            if (UUIDTryParse(args,0, out target))
            {
                Primitive targetPrim = WorldSystem.GetPrimitive(target, null);

                if (targetPrim != null)
                {
                    Client.Self.Touch(targetPrim.LocalID);
                    return Success("Touched prim " + targetPrim.LocalID);
                }
            }

            return Failure("Couldn't find a prim to touch with UUID " + args[0]);
		}
    }
}
