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
            Parameters = new[] { new NamedParam(typeof(SimObject), typeof(UUID)) };
        }

        public override CmdResult Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
            if (Client.Network.CurrentSim == null) return Failure("not yet connected");
            int argsUsed;
            List<SimObject> OS = WorldSystem.GetPrimitives(args, out argsUsed);
            if (OS.Count == 0)
            {
                OS.Add(TheSimAvatar);
            }
            foreach (var O in OS)
            {
                WriteLine("Attachments for " + O);
                int count = O.Children.Count;
                foreach (var s in O.Children)
                {
                    String point = "Unknown";
                    Primitive prim = s.Prim;
                    if (prim != null)
                    {
                        point = prim.PrimData.AttachmentPoint.ToString() + " Offset: " + prim.Position;
                    }

                    // TODO: Done? Fetch properties for the objects with missing property sets so we can show names
                    WriteLine("[Attachment @ {0}] {1}", point, s);
                }
                Success("Found " + count + " attachments");
            }
            return Success("Found " + OS.Count + " attachments");
        }
    }
}
