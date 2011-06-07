using System;
using System.Threading;
using cogbot.TheOpenSims;
using OpenMetaverse;
using PathSystem3D.Navigation;

using MushDLR223.ScriptEngines;

namespace cogbot.Actions.Movement
{
    class FlyToCommand : Command, BotPersonalCommand
    {

        Vector3 myPos = new Vector3();
        Vector2 myPos0 = new Vector2();
        Vector3 target = new Vector3();
        Vector2 target0 = new Vector2();
        float diff, olddiff, saveolddiff;
        private DateTime startTime = DateTime.MinValue;
        int duration = 10000;
        EventHandler<TerseObjectUpdateEventArgs> callback; 

        public FlyToCommand(BotClient testClient)
        {
            TheBotClient = testClient;

            Name = "Fly To";
            Description = "Fly the avatar toward the specified position for a maximum of seconds. Usage: FlyTo x y z [seconds]";
            Category = CommandCategory.Movement;
            Parameters = new[] {  new NamedParam(typeof(SimPosition), typeof(SimPosition)) };
            callback = new EventHandler<TerseObjectUpdateEventArgs>(Objects_OnObjectUpdated);
        }

        public override CmdResult Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
            if (args.Length < 1)
                return ShowUsage();// " FlyTo x y z [seconds]";
            int argsUsed;
            SimPosition position = WorldSystem.GetVector(args, out argsUsed);
            if (position==null)
            {
                return ShowUsage();// " FlyTo x y z [seconds]";
            }
            duration = 30000;
            if (argsUsed < args.Length)
                if (Int32.TryParse(args[argsUsed], out duration))
                    duration *= 1000;

            if (true)
            {
                startTime = DateTime.Now;
                FlyToAction fta = new FlyToAction(TheSimAvatar, position);
                if (true)
                {
                    fta.InvokeReal();
                    fta.Abort();
                    return Success(string.Format("Start flying to {0}", target));
                }
                try
                {
                    bool KeepFollowing = true;
                    fta.RegCallback();
                    while (KeepFollowing)
                    {
                        if (DateTime.Now.Subtract(startTime).TotalMilliseconds > duration)
                        {
                            return Failure("Timer Complete");
                        }
                        if (!position.IsRegionAttached)
                        {
                            return Failure("Target detatched " + position);
                        }
                        KeepFollowing = fta.FlyToOnce();
                    }
                }
                finally
                {
                    fta.EndFlyto();
                }
                return Success(string.Format("Start flying to {0}", target));
            }
            // iterupt any motion
            //TheSimAvatar.CurrentAction = null;
            target = position.SimPosition;
            target0.X = target.X;
            target0.Y = target.Y;


            Client.Objects.TerseObjectUpdate += callback;
            startTime = DateTime.Now;
            Client.Self.Movement.Fly = true;
            Client.Self.Movement.AtPos = true;
            Client.Self.Movement.AtNeg = false;
            ZMovement();
            Client.Self.Movement.TurnToward(target);
            //System.Threading.Thread.Sleep(100);

            //XYMovement();
            //ZMovement();
            //Client.Self.Movement.SendUpdate(false);

            return Success(string.Format("flying to {0} in {1} seconds", target.ToString(), duration / 1000));
        }

        private void Objects_OnObjectUpdated(object s, TerseObjectUpdateEventArgs e)
        {
            if (startTime == DateTime.MinValue) return;
            if (e.Update.LocalID == Client.Self.LocalID)
            {
                XYMovement();
                ZMovement();
                if (Client.Self.Movement.AtPos || Client.Self.Movement.AtNeg)
                {
                    Client.Self.Movement.TurnToward(target);
                    Debug("Flyxy ");
                }
                else if (Client.Self.Movement.UpPos || Client.Self.Movement.UpNeg)
                {
                    Client.Self.Movement.TurnToward(target);
                    //Client.Self.Movement.SendUpdate(false);
                    Debug("Fly z ");
                }
                else if (Vector3.Distance(target, GetSimPosition()) <= 2.0)
                {
                    EndFlyto();
                    Debug("At Target");
                }
            }
            if (DateTime.Now.Subtract(startTime).TotalMilliseconds > duration)
            {
                EndFlyto();
                Debug("End Flyto");
            }
        }

        private bool XYMovement()
        {
            bool res = false;

            myPos = GetSimPosition();
            myPos0.X = myPos.X;
            myPos0.Y = myPos.Y;
            diff = Vector2.Distance(target0, myPos0);
            Vector2 vvel = new Vector2(Client.Self.Velocity.X, Client.Self.Velocity.Y);
            float vel = vvel.Length();
            if (diff >= 10.0)
            {
                Client.Self.Movement.AtPos = true;
                //  Client.Self.Movement.AtNeg = false;
                //if (Math.Abs(diff - olddiff) > 1.5) {
                //  Client.Self.Movement.AtPos = diff < olddiff;
                //  Client.Self.Movement.AtNeg = diff > olddiff;
                //} else if (!Client.Self.Movement.AtPos && !Client.Self.Movement.AtNeg) {
                //  Client.Self.Movement.AtPos = true;
                //  Client.Self.Movement.AtNeg = false;
                //}
                res = true;
            }
            else if (diff >= 2 && vel < 5)
            {
                Client.Self.Movement.AtPos = true;
            }
            else
            {
                Client.Self.Movement.AtPos = false;
                Client.Self.Movement.AtNeg = false;
            }
            saveolddiff = olddiff;
            olddiff = diff;
            return res;
        }

        private void ZMovement()
        {
            Client.Self.Movement.UpPos = false;
            Client.Self.Movement.UpNeg = false;
            float diffz = (target.Z - GetSimPosition().Z);
            if (diffz >= 20.0)
                Client.Self.Movement.UpPos = true;
            else if (diffz <= -20.0)
                Client.Self.Movement.UpNeg = true;
            else if (diffz >= +5.0 && Client.Self.Velocity.Z < +4.0)
                Client.Self.Movement.UpPos = true;
            else if (diffz <= -5.0 && Client.Self.Velocity.Z > -4.0)
                Client.Self.Movement.UpNeg = true;
            else if (diffz >= +2.0 && Client.Self.Velocity.Z < +1.0)
                Client.Self.Movement.UpPos = true;
            else if (diffz <= -2.0 && Client.Self.Velocity.Z > -1.0)
                Client.Self.Movement.UpNeg = true;
        }

        private void EndFlyto()
        {
            startTime = DateTime.MinValue;
            Client.Self.Movement.AtPos = false;
            Client.Self.Movement.AtNeg = false;
            Client.Self.Movement.UpPos = false;
            Client.Self.Movement.UpNeg = false;
            Client.Self.Movement.SendUpdate(false);
            Client.Objects.TerseObjectUpdate -= callback;
        }

        private void Debug(string x)
        {
           // return; /* remove for debugging */
            WriteLine(x + " {0,3:##0} {1,3:##0} {2,3:##0} diff {3,5:##0.0} olddiff {4,5:##0.0}  At:{5,5} {6,5}  Up:{7,5} {8,5}  v: {9} w: {10}",
        myPos.X, myPos.Y, myPos.Z, diff, saveolddiff,
        Client.Self.Movement.AtPos, Client.Self.Movement.AtNeg, Client.Self.Movement.UpPos, Client.Self.Movement.UpNeg,
        Client.Self.Velocity.ToString(), Client.Self.AngularVelocity.ToString());
        }
    }
}
