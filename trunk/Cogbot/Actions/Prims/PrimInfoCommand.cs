using System;
using cogbot.TheOpenSims;
using OpenMetaverse;

namespace cogbot.Actions
{
    public class PrimInfoCommand : Command, RegionMasterCommand
    {
        public PrimInfoCommand(BotClient testClient)
        {
            Name = "priminfo";
            Description = "Dumps information about a specified prim. " + "Usage: priminfo [prim-uuid]";
            Category = CommandCategory.Objects;
            Parameters = new[] { typeof(Primitive), typeof(UUID) };
        }

        public override string Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
            UUID primID;

            if (args.Length < 1)
            {
                foreach (SimObject O in WorldSystem.TheSimAvatar.GetNearByObjects(10, true))
                {
                    WriteLine("\n {0}", WorldSystem.describePrim(O.Prim, false));
                }
                return "Done.";
            }

            int argsUsed;
            Primitive target = WorldSystem.GetPrimitive(args, out argsUsed);
            if (target != null)
            {
                WriteLine("\n {0}", WorldSystem.describePrim(target, true));
                return "Done.";
            }
            return "Could not find prim " + String.Join(" ", args);
        }
    }
}
