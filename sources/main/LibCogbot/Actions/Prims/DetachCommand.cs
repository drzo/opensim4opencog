using System;
using System.Collections.Generic;
using System.Text;
using Cogbot.World;
using OpenMetaverse;

using MushDLR223.ScriptEngines;
using OpenMetaverse.Messages.Linden;

namespace Cogbot.Actions.Objects
{
    public class DetachCommand : Command, BotPersonalCommand
    {
        public DetachCommand(BotClient testClient)
        {
            Name = "detach";
            Description = "detach prims or specified attachment point";
            Details = "detach <all|attachmentPoint|prim> Example: /detach prim98922187";
            Category = CommandCategory.Objects;
            Parameters = new[] { new NamedParam(typeof(SimObject), typeof(UUID)) };
        }

        public override CmdResult ExecuteRequest(CmdRequest args)
        {
            if (args.Length < 1)
                return ShowUsage();
            if (args[0].ToLower()=="all")
            {
                List<uint> ids = new List<uint>();
                foreach(SimObject o in TheSimAvatar.Children)
                {
                    Success("Detatching " + o);
                    ids.Add(o.LocalID);
                }
                Client.Objects.DetachObjects(TheSimAvatar.GetSimulator(), ids);
                return Success("detatched all " + ids.Count);
                
            }
            object obj;
            AttachmentPoint attachmentPoint = AttachmentPoint.Default;
            int argsUsed;
            if (TryEnumParse(typeof(AttachmentPoint), args, 0, out argsUsed, out obj))
            {
                attachmentPoint = (AttachmentPoint)obj;
                List<uint> ids = new List<uint>();
                foreach (SimObject o in TheSimAvatar.Children)
                {
                    if (o.AttachPoint != attachmentPoint) continue;
                    Success("Detatching " + o);
                    ids.Add(o.LocalID);
                }
                Client.Objects.DetachObjects(TheSimAvatar.GetSimulator(), ids);
                return Success("detatched  " + attachmentPoint + " " + ids.Count);
            }
            List<SimObject> PS = WorldSystem.GetPrimitives(args, out argsUsed);
            List<uint> idz = new List<uint>();
            foreach (var o in PS)
            {
                Success("Detatching " + o);
                idz.Add(o.LocalID);
            }
            Client.Objects.DetachObjects(TheSimAvatar.GetSimulator(), idz);
            return Success("detatched  " + args.str + " " + idz.Count);
        }
    }
}
