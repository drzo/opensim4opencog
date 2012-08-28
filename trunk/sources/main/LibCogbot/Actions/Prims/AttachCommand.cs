using System;
using System.Collections.Generic;
using Cogbot.World;
using OpenMetaverse;
using MushDLR223.ScriptEngines;

namespace Cogbot.Actions.Objects
{
    public class AttachCommand : Command, BotPersonalCommand
    {
        public AttachCommand(BotClient testClient)
        {
            Name = "attach";
        }

        public override void MakeInfo()
        {
            Description = "attach a prim to specified (or default) attachment point from the world";
            Details = "attach <prim> [attachmentPoint] [rotation] Example: /attach 98922187 RightHand";
            Category = CommandCategory.Objects;
            Parameters = CreateParams("targets", typeof (PrimSpec), "The targets of " + Name,
                                      Optional("attachpoint", typeof (AttachmentPoint), "where to attach to"),
                                      Optional("rotation", typeof (Quaternion), "rotation to attach to"));
        }

        public override CmdResult ExecuteRequest(CmdRequest args)
        {
            if (args.Length < 1)
                return ShowUsage();

            int argsUsed;
            List<SimObject> PS = WorldSystem.GetSingleArg(args, out argsUsed);
            args = args.AdvanceArgs(argsUsed);
            AttachmentPoint attachmentPoint = AttachmentPoint.Default;
            if (args.Length > 0)
            {
                object obj;
                if (TryEnumParse(typeof (AttachmentPoint), args, 0, out argsUsed, out obj))
                {
                    attachmentPoint = (AttachmentPoint) obj;
                }
            }
            args = args.AdvanceArgs(argsUsed);
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
                AddSuccess("attaching " + found + " to " + attachmentPoint + " with rotation " + rotation);
            }
            return SuccessOrFailure();
        }
    }
}