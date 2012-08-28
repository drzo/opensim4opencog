using System;
using System.Collections.Generic;
using Cogbot.World;
using OpenMetaverse;
using MushDLR223.ScriptEngines;

namespace Cogbot.Actions.Appearance
{
    public class AttachmentsCommand : Command, RegionMasterCommand, FFIComplete, AsynchronousCommand
    {
        public AttachmentsCommand(BotClient testClient)
        {
            TheBotClient = testClient;
            Name = "attachments";
        }

        public override void MakeInfo()
        {
            Description = "Prints a list of the currently known agent attachments or on another avatar";
            Details = AddUsage(Name + " [agent-spec]", "no prim-spec then use $self");
            Category = CommandCategory.Appearance;
            Parameters = CreateParams(Optional("targets", typeof (AgentSpec), "the agents you wish to see " + Name));
            ResultMap = CreateParams(
                "message", typeof (string), "if success was false, the reason why",
                "list", typeof (SimObject), "attachments found",
                "count", typeof (int), "attachments counts",
                "success", typeof (bool), "true if command was successful");
        }

        public override CmdResult ExecuteRequest(CmdRequest args)
        {
            if (Client.Network.CurrentSim == null) return Failure("not yet connected");
            int argsUsed;
            var args0 = args.GetProperty("targets");
            List<SimObject> OS = WorldSystem.GetPrimitives(args0, out argsUsed);
            bool writeInfo = !args.IsFFI;
            if (IsEmpty(OS))
            {
                OS = new List<SimObject> {TheSimAvatar};
            }
            int total = 0;
            foreach (var O in OS)
            {
                if (writeInfo) WriteLine("Attachments for " + O);
                int count = O.Children.Count;
                total++;
                foreach (var s in O.Children)
                {
                    AppendItem("list", O);
                    if (!writeInfo) continue;
                    String point = "Unknown";
                    Primitive prim = s.Prim;
                    if (prim != null)
                    {
                        point = prim.PrimData.AttachmentPoint.ToString() + " Offset: " + prim.Position;
                    }

                    // TODO: Done? Fetch properties for the objects with missing property sets so we can show names
                    WriteLine("[Attachment @ {0}] {1}", point, s);
                }
            }
            SetResult("count", total);
            return SuccessOrFailure();
        }
    }
}