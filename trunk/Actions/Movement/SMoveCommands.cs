using System;
using System.Collections.Generic;
using System.Text;
using OpenMetaverse;
using cogbot.TheOpenSims.Navigation;
using cogbot.TheOpenSims;
using System.Windows.Forms;

namespace cogbot.Actions.Movement
{
    class SMoveCommands
    {
    }
    class gto : cogbot.Actions.Command
    {
        public gto(BotClient client)
        {
            Name = "gto";
            Description = "gto the avatar toward the specified position for a maximum of seconds. Usage: FlyTo x y z [seconds]";
            Category = cogbot.Actions.CommandCategory.Movement;
        }

        public override string Execute(string[] args, UUID fromAgentID)
        {
            Vector3 target = new Vector3();
            float distance = 2.0f;

            if (args.Length > 3 || args.Length == 0)
                return "Usage: gto [prim | [x y [dist]]";

            if (float.TryParse(args[0], out target.X) &&
                float.TryParse(args[1], out target.Y))
            {
                target.Z = Client.Self.SimPosition.Z;
                if (args.Length == 3) Single.TryParse(args[2], out distance);
            }
            else
            {
                string s = String.Join(" ", args);
                Primitive prim;


                if (WorldSystem.tryGetPrim(s, out prim))
                {

                    SimObject simObject = WorldSystem.GetSimObject(prim);
                    if (simObject.CanGetSimPosition())
                    {
                        target = simObject.GetSimPosition();
                        distance = 0.5f + simObject.GetSizeDistance();
                    }
                    else
                    {
                        return "Cannot get Sim Position of " + simObject;
                    }

                }
                else
                {
                    return "Cannot select " + s;
                }
            }

            Goto(target, distance);
            return string.Format("gto {0} {1}", target.ToString(), distance);
        }

        public void Goto(Vector3 target, float distance)
        {
            Approacher.AutoGoto1(Client, target, distance, 20000);
            Vector2 v2 = new Vector2(target.X, target.Y);
            float d = Approacher.DistanceTo(Client, v2);
            if (d < distance) return;
        }
        private void Goto1(Vector3 target, float p)
        {

            if (true)
            {
                uint x, y;
                Utils.LongToUInts(Client.Network.CurrentSim.Handle, out x, out y);
                Vector2 v2 = new Vector2(target.X, target.Y);
                Vector2 cp = new Vector2(Client.Self.SimPosition.X, Client.Self.SimPosition.Y);
                float d = Vector2.Distance(v2, cp);
                float dl = d;
                bool autoOff = true;
                while (d > p)
                {
                    if (autoOff)
                    {
                        Client.Self.Movement.TurnToward(target);
                        Client.Self.AutoPilot((ulong)(x + target.X), (ulong)(y + target.Y), Client.Self.SimPosition.Z);
                        autoOff = false;
                    }
                    cp = new Vector2(Client.Self.SimPosition.X, Client.Self.SimPosition.Y);
                    d = Vector2.Distance(v2, cp);
                    if (dl < d)
                    {
                        Client.Self.AutoPilotCancel();
                        autoOff = true;
                        Client.Self.Movement.TurnToward(target);
                        Client.Self.Movement.Stop = true;
                        Client.Self.Movement.AtPos = false;
                        Client.Self.Movement.NudgeAtPos = true;
                        Client.Self.Movement.SendUpdate(true);
                        Client.Self.Movement.NudgeAtPos = false;
                        Client.Self.Movement.SendUpdate(true);
                    }
                    //Thread.Sleep(10);
                    Application.DoEvents();
                    dl = d;
                }
                Client.Self.Movement.TurnToward(target);
                Client.Self.AutoPilotCancel();
                return;
            }
            if (true)
            {
                MovementToVector.MoveTo(Client, target, p);
                return;
            }
            GotoVector gvect = new GotoVector(Client, target, 10000, p);
            gvect.Goto();
        }
    }


}
