using System;
using System.Threading;
using OpenMetaverse;
using PathSystem3D.Navigation;

using MushDLR223.ScriptEngines;

namespace cogbot.Actions.Movement
{
    class MovetoCommand : Command, BotPersonalCommand, BotStatefullCommand
    {
        public MovetoCommand(BotClient client)
        {
            Name = "moveto";
            Description = "Moves the avatar to the specified global position using robot turnto and walk. Usage: moveto x y z";
            Category = CommandCategory.Movement;
            Parameters = new[] { new NamedParam(typeof(SimPosition), typeof(SimPosition)) };

        }

        public override CmdResult Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
            int argsUsed;
            if (args.Length < 1)
                return ShowUsage();// " moveto x y z";
            SimPosition position = WorldSystem.GetVector(args, out argsUsed);
            if (position==null)
            {
                return Failure("Coulnd not resolve location: " + string.Join(" ", args));
            }
            Vector3d g = position.GlobalPosition;
            var TheSimAvatar = this.TheSimAvatar;
            TheSimAvatar.SetClient(TheBotClient);
            //TheSimAvatar.SetMoveTarget(position, position.GetSizeDistance());
            Client.Self.AutoPilot(g.X, g.Y, g.Z);
            Dispose();
           // MoveThread = new Thread(MoveProc);
            return Success(string.Format("SetMoveTarget: <{0},{1},{2}>", g.X, g.Y, g.Z));
        }

        private Vector3d target = Vector3d.Zero;
        private void MoveProc()
        {
            var TheSimAvatar = this.TheSimAvatar;
            while (true)
            {
                if (target == Vector3d.Zero) return;
                TheSimAvatar.StopMoving();
                TheSimAvatar.TurnToward(target);
            }
        }

        private Thread MoveThread;

        /// <summary>
        /// Performs application-defined tasks associated with freeing, releasing, or resetting unmanaged resources.
        /// </summary>
        /// <filterpriority>2</filterpriority>
        public void Dispose()
        {
            /*if (MoveThread == null) return;

            if (MoveThread.IsAlive)
            {
                MoveThread.Abort();
            }
            MoveThread = null;
        */
        }

    }
}
