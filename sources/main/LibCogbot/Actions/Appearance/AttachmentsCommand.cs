using System;
using System.Collections.Generic;
using Cogbot.World;
using OpenMetaverse;

using MushDLR223.ScriptEngines;

namespace Cogbot.Actions.Appearance
{
    public class AttachmentsCommand : Command, RegionMasterCommand
    {
        public AttachmentsCommand(BotClient testClient)
        {
            TheBotClient = testClient;
            Name = "attachments";
            Description = "Prints a list of the currently known agent attachments or on another avatar";
            Details = AddUsage(Name + " [agent-spec]", "no prim-spec then use $self");
            Category = CommandCategory.Appearance;
            Parameters = CreateParams(
                Optional("target", typeof(AgentSpec),
                                    "the agents you wish to see " + Name +
                                    " (see meets a specified <a href='wiki/BotCommands#PrimSpec'>Prim Spec</a>.)"));
            ResultMap = CreateParams(
                "message", typeof(string), "if success was false, the reason why",
                "success", typeof(bool), "true if command was successful");
        }

        public override CmdResult ExecuteRequest(CmdRequest args)
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
                return Success("Found " + count + " attachments");
            }
            return Success("Found " + OS.Count + " attachments");
        }
    }
}
