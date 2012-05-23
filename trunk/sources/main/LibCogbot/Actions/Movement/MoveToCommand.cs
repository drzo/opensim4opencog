using System;
using System.Threading;
using cogbot.TheOpenSims;
using OpenMetaverse;
using PathSystem3D.Navigation;

using MushDLR223.ScriptEngines;

namespace cogbot.Actions.Movement
{
    class MovetoCommand : Command, BotPersonalCommand
    {
        public MovetoCommand(BotClient client)
            : base(client)
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
            if (!Client.IsLoggedInAndReady)
            {
                return Failure("Not yet logged in!");
            }
            SimPosition position = WorldSystem.GetVector(args, out argsUsed);
            if (position == null)
            {
                return Failure("Coulnd not resolve location: " + string.Join(" ", args));
            }
            if (!position.IsRegionAttached)
            {
                return Failure("!IsRegionAttached: " + position);
            }
            if (position.SimPosition==Vector3.Zero)
            {
                return Failure("SimPosition.Zero: " + position);
            }
            Dispose();
            Vector3d g = position.GlobalPosition;
            var TheSimAvatar = this.TheSimAvatar;
            TheSimAvatar.SetClient(TheBotClient);
            TheSimAvatar.SetMoveTarget(position, position.GetSizeDistance());
            //Client.Self.AutoPilot(g.X, g.Y, g.Z);
           // MoveThread = new Thread(MoveProc);
            return Success(string.Format("SetMoveTarget: {0},{1},{2}", position, g, position.SimPosition));
        }

        private Thread MoveThread;

        /// <summary>
        /// Performs application-defined tasks associated with freeing, releasing, or resetting unmanaged resources.
        /// </summary>
        /// <filterpriority>2</filterpriority>

        public void Dispose()
        {
            Client.Self.AutoPilotCancel();
            /*if (MoveThread == null) return;

            if (MoveThread.IsAlive)
            {
                MoveThread.Abort();
            }
            MoveThread = null;
        */
        }

        public CmdResult ExecuteOLD(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
            Client.Self.IM += OnIM;
            Results.Clear();
            int argsUsed;
            if (args.Length < 1)
                return ShowUsage();// " moveto x y z";
            SimPosition position = WorldSystem.GetVector(args, out argsUsed);
            if (position == null)
            {
                return Failure("Coulnd not resolve location: " + string.Join(" ", args));
            }
            Vector3d g = position.GlobalPosition;
            var TheSimAvatar = this.TheSimAvatar;
            TheSimAvatar.SetClient(TheBotClient);
            //TheSimAvatar.SetMoveTarget(position, position.GetSizeDistance());
            Dispose();
            Client.Self.AutoPilot(g.X, g.Y, g.Z);
            Thread.Sleep(TimeSpan.FromSeconds(1));
            Client.Self.Fly(false);
            Vector3 gg;
            SimRegion reg;
            SimRegion.GetRegionAndLocal(g, out reg, out gg);
            // MoveThread = new Thread(MoveProc);
            return Success(string.Format("SetMoveTarget: {0} <{1},{2},{3}>", reg.ToString(), gg.X, gg.Y, gg.Z));
        }

        private void OnIM(object sender, InstantMessageEventArgs e)
        {
            Dispose();
        }
        Vector3 myPos = new Vector3();
        Vector2 myPos0 = new Vector2();
        Vector3 target = new Vector3();
        Vector2 target0 = new Vector2();
        float diff, olddiff, saveolddiff;
        private DateTime startTime = DateTime.MinValue;
        int duration = 10000;
        EventHandler<TerseObjectUpdateEventArgs> callback;

        public CmdResult ExecuteNew(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
            EndFlyto();
            var Movement = Client.Self.Movement;
            if (args.Length < 1)
                return ShowUsage();// " FlyTo x y z [seconds]";
            int argsUsed;
            SimPosition position = WorldSystem.GetVector(args, out argsUsed);
            if (position == null)
            {
                return ShowUsage();// " FlyTo x y z [seconds]";
            }
            duration = 10000;
            target = position.SimPosition;
            target0.X = target.X;
            target0.Y = target.Y;


            Client.Objects.TerseObjectUpdate += callback;
            startTime = DateTime.Now;
            Movement.ResetControlFlags();
            Movement.TurnToward(target, true);
            Movement.AtPos = true;
            Movement.SendUpdate(true);
            XYMovement();
            return Success(string.Format(Name + " to {0} for {1} seconds started", target.ToString(), duration / 1000));
        }

        private void Objects_OnObjectUpdated(object s, TerseObjectUpdateEventArgs e)
        {
            if (e.Update.LocalID == Client.Self.LocalID)
            {
                var Movement = Client.Self.Movement;

                if (Movement.AtPos || Movement.AtNeg)
                {
                    Movement.TurnToward(target);
                    Debug("Flyxy ");
                }
                else if (Movement.UpPos || Movement.UpNeg)
                {
                    Movement.TurnToward(target);
                    //Movement.SendUpdate(false);
                    Debug("Fly z ");
                }
                else if (Vector3.Distance(target, GetSimPosition()) <= 1.0)
                {
                    EndFlyto();
                    Debug("At Target");
                }
            }
            if (DateTime.Now.Subtract(startTime).TotalMilliseconds > duration)
            {
                EndFlyto();
                Debug("End Flyto");
                return;
            }
            XYMovement();
            ZMovement();
        }

        private bool XYMovement()
        {
            bool res = false;
            var Movement = Client.Self.Movement;
            myPos = GetSimPosition();
            myPos0.X = myPos.X;
            myPos0.Y = myPos.Y;
            diff = Vector2.Distance(target0, myPos0);
            if (diff < 1)
            {
                EndFlyto();
                return true;
            }
            Vector2 vvel = new Vector2(Client.Self.Velocity.X, Client.Self.Velocity.Y);
            float vel = vvel.Length();
            if (diff >= 10.0)
            {
                Movement.ResetControlFlags();
                Movement.AtPos = true;
                Movement.SendUpdate(true);
                res = true;
            }
            else if (diff >= 3 && vel < 5)
            {
                Movement.ResetControlFlags();
                Movement.AtPos = true;
                Movement.SendUpdate(true);
            }
            else
            {
                Movement.ResetControlFlags();
                Movement.NudgeAtPos = true;
                Movement.SendUpdate(true);
            }
            saveolddiff = olddiff;
            olddiff = diff;
            return res;
        }

        private void ZMovement()
        {
            return;
            var Movement = Client.Self.Movement;
            Movement.UpPos = false;
            Movement.UpNeg = false;
            float diffz = (target.Z - GetSimPosition().Z);
            if (diffz >= 20.0)
                Movement.UpPos = true;
            else if (diffz <= -20.0)
                Movement.UpNeg = true;
            else if (diffz >= +5.0 && Client.Self.Velocity.Z < +4.0)
                Movement.UpPos = true;
            else if (diffz <= -5.0 && Client.Self.Velocity.Z > -4.0)
                Movement.UpNeg = true;
            else if (diffz >= +2.0 && Client.Self.Velocity.Z < +1.0)
                Movement.UpPos = true;
            else if (diffz <= -2.0 && Client.Self.Velocity.Z > -1.0)
                Movement.UpNeg = true;
        }

        private void EndFlyto()
        {
            startTime = DateTime.MinValue;
            var Movement = Client.Self.Movement;
            Movement.ResetControlFlags();
            Movement.SendUpdate(true);
            Client.Objects.TerseObjectUpdate -= callback;
        }

        private void Debug(string x)
        {
            return; /* remove for debugging */
            var Movement = Client.Self.Movement;
            WriteLine(
                x +
                " {0,3:##0} {1,3:##0} {2,3:##0} diff {3,5:##0.0} olddiff {4,5:##0.0}  At:{5,5} {6,5}  Up:{7,5} {8,5}  v: {9} w: {10}",
                myPos.X, myPos.Y, myPos.Z, diff, saveolddiff,
                Movement.AtPos, Movement.AtNeg, Movement.UpPos,
                Movement.UpNeg,
                Client.Self.Velocity.ToString(), Client.Self.AngularVelocity.ToString());
        }

    }
}
