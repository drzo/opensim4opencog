using System;
using System.Collections.Generic;
using cogbot.TheOpenSims;
using OpenMetaverse;

using MushDLR223.ScriptEngines;

namespace cogbot.Actions.Objects
{
    public class AttachCommand : Command, BotPersonalCommand
    {
        public AttachCommand(BotClient testClient)
        {
            Name = "attach";
            Description = "attach a prim to specified (or default) attachment point from the world";
            Usage = "attach <prim> [attachmentPoint] [rotation] Example: /attach 98922187 RightHand";
            Category = CommandCategory.Objects;
            Parameters = new[] { new NamedParam(typeof(SimObject), typeof(UUID)) };
        }

        public override CmdResult Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
            if (args.Length < 1)
                return ShowUsage();

            int argsUsed;
            List<SimObject> PS = WorldSystem.GetPrimitives(args, out argsUsed);
            args = Parser.SplitOff(args, argsUsed);
            AttachmentPoint attachmentPoint = AttachmentPoint.Default;
            if (args.Length > 0)
            {
                object obj;
                if (TryEnumParse(typeof (AttachmentPoint), args, 0, out argsUsed, out obj))
                {
                    attachmentPoint = (AttachmentPoint) obj;
                }
            }
            args = Parser.SplitOff(args, argsUsed);
            Quaternion rotation = Quaternion.Identity;
            if (args.Length > 0)
            {
                Quaternion quat;
                if (Quaternion.TryParse(Parser.Rejoin(args, 0), out quat))
                {
                    rotation = quat;
                }
            }
            foreach (var found in PS)
            {
                Client.Objects.AttachObject(found.GetSimulator(), found.LocalID, attachmentPoint, rotation);
                Success("attaching " + found + " to " + attachmentPoint + " with rotation " + rotation);
            }
            return SuccessOrFailure();
        }
    }
}
