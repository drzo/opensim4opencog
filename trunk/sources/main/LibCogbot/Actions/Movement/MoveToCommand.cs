using System;
using System.Threading;
using Cogbot.Actions.Pathfinder;
using Cogbot.World;
using OpenMetaverse;
using PathSystem3D.Navigation;
using MushDLR223.ScriptEngines;

namespace Cogbot.Actions.Movement
{
    internal class MoveToCommand : Command, BotPersonalCommand
    {
        public MoveToCommand(BotClient client)
            : base(client)
        {
            Name = "move";
        }

        public override void MakeInfo()
        {
            Description =
                "Moves the avatar to the specified global position using robot turnto and walk, autopilot or other setting.";
            Category = CommandCategory.Movement;
            AddUsage(Name + " wp5", "move to wp6");
            Parameters = CreateParams(
                "to", typeof (SimPosition), "the location you wish to " + Name,
                Optional("dist", typeof (float), "the distance you wish to " + Name),
                Optional("sproc", typeof (MovementProceedure), "the salient " + Name),
                Optional("proc", typeof (MovementProceedure), "the simple " + Name));
        }

        public override CmdResult ExecuteRequest(CmdRequest args)
        {
            return ExecuteRequestProc(args, this);
        }

        public static CmdResult ExecuteRequestProc(CmdRequest args, Command cmd)
        {
            if (!args.ContainsKey("to"))
                args.SetValue("to", "verb");

            ;
            if (!cmd.Client.IsLoggedInAndReady)
            {
                return cmd.Failure("Not yet logged in!");
            }
            var TheSimAvatar = cmd.WorldSystem.TheSimAvatar;

            if (TheSimAvatar.IsSitting && !TheSimAvatar.IsDrivingVehical)
            {
                cmd.WriteLine("$bot is standing up before moving.");
                TheSimAvatar.StandUp();
                // WriteLine("$bot is sitting, Please stand up to move.");
            }
            SimPosition position;
            if (!args.TryGetValue("to", out position))
            {
                return cmd.Failure("I don't understand how to move " + args.str);
            }
            if (position == null)
            {
                return cmd.Failure("Coulnd not resolve location: " + args.str);
            }
            if (!position.IsRegionAttached)
            {
                return cmd.Failure("!IsRegionAttached: " + position);
            }
            if (position.SimPosition == Vector3.Zero)
            {
                return cmd.Failure("SimPosition.Zero: " + position);
            }
            Vector3d delta0 = position.GlobalPosition - TheSimAvatar.GlobalPosition;
            Vector3 delta = new Vector3((float) delta0.X, (float) delta0.Y, (float) delta0.Z);

            float fnd;
            if (args.TryGetValue("dist", out fnd))
            {
                delta.Normalize();
                delta = delta*fnd;
                position = new SimOffsetPosition(TheSimAvatar, delta);
            }

            MovementProceedure proc;
            bool salientProc = args.TryGetValue("sproc", out proc);
            if (salientProc)
            {
                TheSimAvatar.SalientMovementProceedure = proc;
            }

            if (args.TryGetValue("proc", out proc))
            {
                TheSimAvatar.SimpleMoveToMovementProceedure = proc;
            }

            Vector3d g = position.GlobalPosition;
            TheSimAvatar.SetClient(cmd.TheBotClient);
            if (salientProc)
            {
                return cmd.Result(string.Format("SalientGoto: {0},{1},{2}", position, g, position.SimPosition),
                                  TheSimAvatar.SalientGoto(position));
            }
            else
            {
                TheSimAvatar.SetMoveTarget(position, position.GetSizeDistance());
            }
            //Client.Self.AutoPilot(g.X, g.Y, g.Z);
            // MoveThread = new Thread(MoveProc);
            return cmd.Success(string.Format("SetMoveTarget: {0},{1},{2}", position, g, position.SimPosition));
        }
    }
}