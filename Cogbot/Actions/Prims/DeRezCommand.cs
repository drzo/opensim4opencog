using System;
using cogbot.TheOpenSims;
using OpenMetaverse;

namespace cogbot.Actions
{
    public class DeRezCommand : Command, RegionMasterCommand
    {
        public DeRezCommand(BotClient testClient)
        {
            Name = "derez";
            Description = "De-Rezes a specified prim. " + "Usage: derez [prim-uuid]";
            Category = CommandCategory.Objects;
        }

        public override string Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
            UUID primID;

            if (args.Length < 1)
            {
                return Description;
            }

            int argsUsed;
            Primitive target = WorldSystem.GetPrimitive(args, out argsUsed);
            if (target != null)
            {
                WorldSystem.DeletePrim(target);
                WriteLine("\n {0}", target);
                return "Done.";
            }
            return "Could not find prim " + String.Join(" ", args);
        }
    }
}
