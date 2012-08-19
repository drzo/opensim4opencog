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
            TheBotClient = testClient;
		}

		override public void MakeInfo()
		{
			Description = "detach prims or specified attachment point";
            Details = "detach <all|attachmentPoint|prim> Example: /detach prim98922187";
            AddExample("detach [attachments parent $self attachpoint LeftHand]", "detach anything attached to left hand");
            Category = CommandCategory.Objects;
            Parameters = CreateParams("targets", typeof(PrimSpec), "The targets of " + Name);
        }

        public override CmdResult ExecuteRequest(CmdRequest args)
        {
            if (args.Length < 1)
                return ShowUsage();
            if (args.ContainsFlag("--all"))
            {
                List<uint> ids = new List<uint>();
                foreach (SimObject o in TheSimAvatar.Children)
                {
                    Success("Detatching " + o);
                    ids.Add(o.LocalID);
                }
                Client.Objects.DetachObjects(TheSimAvatar.GetSimulator(), ids);
                return Success("detatched all " + ids.Count);
            }

            int	 argsUsed;
            string[] keyargs = args.OnlyKey("targets");
            List<SimObject> PS = WorldSystem.GetPrimitives(keyargs, out argsUsed);
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
