using System;
using System.Collections.Generic;
using cogbot.TheOpenSims;
using OpenMetaverse;

namespace cogbot.Actions
{
    public class AttachmentsCommand : Command, RegionMasterCommand
    {
        public AttachmentsCommand(BotClient testClient)
        {
            TheBotClient = testClient;
            Name = "attachments";
            Description = "Prints a list of the currently known agent attachments. Usage: attachments [prim-uuid]";
            Category = CommandCategory.Appearance;
            Parameters = new [] { new NamedParam(typeof(SimObject), typeof(UUID)) };
        }

        public override string Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
            if (Client.Network.CurrentSim == null) return "not yet connected";
            int argsUsed;
            SimObject O = WorldSystem.GetSimObject(args, out argsUsed);
            if (O ==null)
            {
                O = TheSimAvatar;
            }
            WriteLine("Attachments for " + O);
            int count = O.Children.Count;
            foreach (var s in O.Children)
            {
                String point = "Unknown";
                Primitive prim = s.Prim;
                if (prim!=null)
                {
                    point = prim.PrimData.AttachmentPoint.ToString() + " Offset: " + prim.Position;
                }

                // TODO: Done? Fetch properties for the objects with missing property sets so we can show names
                WriteLine("[Attachment @ {0}] {1}", point, s);
            }
            return "Found " + count + " attachments";
        }
    }
}
