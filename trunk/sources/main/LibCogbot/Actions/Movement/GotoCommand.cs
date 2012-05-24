using System;
using System.Collections.Generic;
using System.Text;
using cogbot.TheOpenSims;
using OpenMetaverse;
using OpenMetaverse.Packets;
using PathSystem3D.Navigation;

using MushDLR223.ScriptEngines;

namespace cogbot.Actions.Movement
{
    [Obsolete("Use Teleport Command instead")]
    public class GotoCommand : Command, BotPersonalCommand
    {
        public GotoCommand(BotClient testClient)
		{
			Name = "goto";
            Category = CommandCategory.Movement;
            Description = "goto to a location defined by an avatar, object, or position";
            Details = @"<p>goto  &lt;location&gt;</p>
<p>example: goto Zindra/112.3/114.4/23</p>
<p>example: goto Fluffybunny Resident</p>
<p>example: goto nth 3 Ship <i>teleports to 3rd nearest object named Ship</i></p>";
            Parameters = CreateParams("location", typeof(SimPosition),
                "Location to TP to. Can be an avatar, object, or position. See <a href='wiki/BotCommands#Location'>Locations</a>");
            ResultMap = CreateParams(
                 "message", typeof(string), "if we could not teleport, the reason why",
                 "success", typeof(bool), "true if the teleport succeeded");
		}

        public override CmdResult ExecuteRequest(CmdRequest args)
		{
			if (args.Length < 1)
                return ShowUsage();// " goto sim/x/y/z";

            int argsUsed;
            SimPosition position = WorldSystem.GetVector(args, out argsUsed);
            if (position == null) return Failure( "Teleport - Cannot resolve to a location: " + args.str);
            SimPathStore ps = position.PathStore;
            ulong handle = SimRegion.GetRegionHandle(ps);
            TheSimAvatar.StopMoving();
            if (Client.Self.Teleport(handle, position.SimPosition))
                return Success("Teleported to " + Client.Network.CurrentSim);
            else
                return Failure("Teleport failed: " + Client.Self.TeleportMessage);

		}
    }
}
