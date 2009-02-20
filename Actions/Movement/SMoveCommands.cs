using System;
using System.Collections.Generic;
using System.Text;
using OpenMetaverse;
using cogbot.TheOpenSims.Navigation;
using cogbot.TheOpenSims;
using System.Windows.Forms;
using cogbot.TheOpenSims.Navigation.Debug;
using System.Drawing;
using System.Net;
//using IdealistViewer;

namespace cogbot.Actions.Movement
{
    class SMoveCommands
    {
    }
    class ideal : cogbot.Actions.Command
    {
        public ideal(BotClient client)
        {
            Name = GetType().Name;
            Description = "Starts the waypoint debuger";
            Category = cogbot.Actions.CommandCategory.Movement;
        }

        public override string Execute(string[] args, UUID fromAgentID)
        {
          ///  BaseIdealistViewer.guithread.Start();//.Main(args);
            return "ran "+Name;
        }
    }
    class srdebug : cogbot.Actions.Command
    {
        public srdebug(BotClient client)
        {
            Name = GetType().Name;
            Description = "Starts the waypoint debuger";
            Category = cogbot.Actions.CommandCategory.Movement;
        }

        public override string Execute(string[] args, UUID fromAgentID)
        {
            GraphFormer gf = new GraphFormer(SimPathStore.Instance);
            gf.Reactivate();
            return "ran srdebug";
        }
    }
    class srmap : cogbot.Actions.Command
    {
        public srmap(BotClient client)
        {
            Name = GetType().Name;
            Description = "Reads the sim map for improving routes";
            Category = cogbot.Actions.CommandCategory.Movement;
        }

        public override string Execute(string[] args, UUID fromAgentID)
        {
            Image I = null;// WorldSystem.miniMap.Image;
            if (I == null)
            {

                String picUri = "http://71.197.210.170:9000/index.php?method=regionImaged63a88fe7db448c6b1a52b7628fe8d0d";
                // Create the requests.
                WebRequest requestPic = WebRequest.Create(picUri);

                WebResponse responsePic = requestPic.GetResponse();

                I = Image.FromStream(responsePic.GetResponseStream());

            }

            WorldSystem.SimPaths.UpdateFromImage(I);
            return "ran " + Name;
        }
    }
    class srprim : cogbot.Actions.Command
    {
        public srprim(BotClient client)
        {
            Name = GetType().Name;
            Description = "Reads the sim prims for improving routes";
            Category = cogbot.Actions.CommandCategory.Movement;
        }

        public override string Execute(string[] args, UUID fromAgentID)
        {
            IEnumerable<SimObject> objs = WorldSystem.GetAllSimObjects(String.Join(" ",args));
            SimPathStore pathStore = WorldSystem.SimPaths;
            foreach (SimObject o in objs)
            {
                if (o.IsPassable)
                {
                    pathStore.UpdateFromObject(o);
                }
            }
            foreach (SimObject o in objs)
            {
                if (!o.IsPassable)
                {
                    pathStore.UpdateFromObject(o);
                }
            }
            return "ran " + Name;
        }
    }
    class srpath : cogbot.Actions.Command
    {
        public srpath(BotClient client)
        {
            Name = GetType().Name;
            Description = "Show the route to the object";
            Category = cogbot.Actions.CommandCategory.Movement;
        }

        public override string Execute(string[] args, UUID fromAgentID)
        {
            int argsused;
            SimPosition v3 = WorldSystem.GetVector(args, out argsused);
            SimWaypoint wp = v3.GetWaypoint();
            bool IsFake;
            IList<SimRoute> route = WorldSystem.TheSimAvatar.GetRouteList(wp, out IsFake);
            String s = "v3=" + WorldSystem.TheSimAvatar.DistanceVectorString(v3) + " wp=" + wp.ToString();
            if (IsFake)
            {
                s += "\nIsFake: ";
            }
            else
            {
                s += "\nComputed ";
            }
            if (route!=null)
            for (int i = 0; i < route.Count; i++)
            {
                s += " \n" + i + ": " + route[i].ToInfoString();
            }
            return s;
        }
    }

    class srwp : cogbot.Actions.Command
    {
        public srwp(BotClient client)
        {
            Name = GetType().Name;
            Description = "Show the waypoint for object";
            Category = cogbot.Actions.CommandCategory.Movement;
        }

        public override string Execute(string[] args, UUID fromAgentID)
        {
            int argsused;
            SimPosition v3 = WorldSystem.GetVector(args, out argsused);
            SimWaypoint wp = v3.GetWaypoint();

            return "v3=" + WorldSystem.TheSimAvatar.DistanceVectorString(v3) + " wp=" + wp.ToString();
        }
    }

    class srm : cogbot.Actions.Command
    {
        public srm(BotClient client)
        {
            Name = GetType().Name;
            Description = "Move to a the specified point using MoveTo";
            Category = cogbot.Actions.CommandCategory.Movement;
        }

        public override string Execute(string[] args, UUID fromAgentID)
        {
            int argcount;
            SimPosition pos = WorldSystem.GetVector(args, out argcount);
            if (pos == null)
            {
                return "Cannot " + Name + " to " + String.Join(" ", args);
            }
            int maxSeconds = 6;
            float maxDistance = 1f;
            if (argcount < args.Length)
            {
            }
            string str = "MoveTo(" + pos.GetSimPosition() + ", " + maxDistance + ", " + maxSeconds + ")";
            WriteLine("Starting  " +str);
            bool MadIt = WorldSystem.TheSimAvatar.MoveTo(pos.GetSimPosition(), maxDistance, maxSeconds);
            if (MadIt)
            {
                return ("SUCCESS " + str);

            }
            else
            {
                return ("FAILED " + str);
            }
        }
    }

    class srg : cogbot.Actions.Command
    {
        public srg(BotClient client)
        {
            Name = GetType().Name;
            Description = "Use A* Pathfinding to get to object";
            Category = cogbot.Actions.CommandCategory.Movement;
        }

        public override string Execute(string[] args, UUID fromAgentID)
        {
            int argcount;
            SimPosition pos = WorldSystem.GetVector(args, out argcount);
            if (pos == null)
            {
                return "Cannot " + Name + " to " + String.Join(" ", args);
            }
            int maxSeconds = 6;
            float maxDistance = 1f;
            if (argcount < args.Length)
            {
            }
            String str = "GotoTarget(" + pos + ")";
            WriteLine(str);
            bool MadIt = WorldSystem.TheSimAvatar.GotoTarget(pos);
            if (MadIt)
            {
                return ("SUCCESS " + str);

            }
            else
            {
                return ("FAILED " + str);
            }
        }

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

            WorldSystem.TheSimAvatar.AutoGoto(target, distance, 20000);
            return string.Format("gto {0} {1}", target.ToString(), distance);
        }

        public void Goto(Vector3 target, float distance)
        {
            Approacher.AutoGoto(Client, target, distance, 20000);
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
                bool autoOff = false;
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
