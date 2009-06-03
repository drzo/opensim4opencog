using System;
using cogbot.TheOpenSims;
using OpenMetaverse;

namespace cogbot.Actions
{
    public class PrimInfoCommand : Command
    {
        public PrimInfoCommand(BotClient testClient)
        {
            Name = "priminfo";
            Description = "Dumps information about a specified prim. " + "Usage: priminfo [prim-uuid]";
            Category = CommandCategory.Objects;
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

            if (UUIDTryParse(args, 0, out primID))
            {
                Primitive target = Client.Network.CurrentSim.ObjectsPrimitives.Find(
                    delegate(Primitive prim) { return prim.ID == primID; }
                );

                if (target==null) target = Client.Network.CurrentSim.ObjectsAvatars.Find(
                    delegate(Avatar prim) { return prim.ID == primID; }
                );

                if (target != null)
                {
                    WriteLine("\n {0}", WorldSystem.describePrim(target, true));
                    return "Done.";
                }
                else
                {
                    return "Could not find prim " + primID.ToString();
                }
            }
            else
            {
                return "Usage: priminfo [prim-uuid]";
            }
        }

    }
}
