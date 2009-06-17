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
		}
		
        public override string Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
		{
            UUID target;

            if (args.Length < 1)
                return "Usage: touch UUID";
            
            if (UUIDTryParse(args,0, out target))
            {
                Primitive targetPrim = WorldSystem.GetPrimitive(target, null);

                if (targetPrim != null)
                {
                    Client.Self.Touch(targetPrim.LocalID);
                    return "Touched prim " + targetPrim.LocalID;
                }
            }

            return "Couldn't find a prim to touch with UUID " + args[0];
		}
    }
}
