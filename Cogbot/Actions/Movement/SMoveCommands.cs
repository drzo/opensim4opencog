using System;
using System.Collections.Generic;
using System.Text;
using OpenMetaverse;
using PathSystem3D.Navigation;
using cogbot.TheOpenSims;
using System.Windows.Forms;
using PathSystem3D.Navigation.Debug;
using System.Drawing;
using System.Net;
using cogbot.Listeners;
using System.Threading;
using PathSystem3D;
//using METAbolt;

namespace cogbot.Actions.Movement
{
    class SMoveCommands
    {
    }
    //class ideal : cogbot.Actions.Command
    //{
    //    public ideal(BotClient client)
    //    {
    //        Name = GetType().Name;
    //        Description = "Starts the GUI debugger";
    //        Category = cogbot.Actions.CommandCategory.Movement;
    //    }

    //    public override string Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
    //    {
    //        string[] tokens = args;
    //        if (tokens.Length > 0 && !String.IsNullOrEmpty(tokens[0]))
    //        {
    //            Client.BotLoginParams.FirstName = tokens[0];
    //        }
    //        if (tokens.Length > 1)
    //        {
    //            Client.BotLoginParams.LastName = tokens[1];
    //        }
    //        if (tokens.Length > 2)
    //        {
    //            Client.BotLoginParams.Password = tokens[2];
    //        }
    //        if (tokens.Length > 3)
    //        {
    //            Client.BotLoginParams.URI = tokens[3];
    //        }
    //        Thread th = new Thread(new ThreadStart(delegate()
    //        {

    //            try
    //            {
    //                tokens = new string[] { Client.BotLoginParams.FirstName, Client.BotLoginParams.LastName, Client.BotLoginParams.Password };
    //                METAboltInstance instance = new METAboltInstance(Client,true, tokens);              
    //                Application.Run(instance.MainForm);
    //            }
    //            catch (Exception e)
    //            {
    //                WriteLine("ideal error: " + e);
    //            }
    //        }));
    //        th.TrySetApartmentState(ApartmentState.STA);
    //        th.Start();
    //        ///  BaseIdealistViewer.guithread.Start();//.Main(args);
    //        return "ran " + Name;
    //    }
    //}

    class connections : cogbot.Actions.Command, SystemApplicationCommand
    {
        public connections(BotClient client)
        {
            Name = GetType().Name;
            Description = "Starts the waypoint debuger";
            Category = cogbot.Actions.CommandCategory.Movement;
        }

        public override string Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
            if (args.Length == 0)
            {
                foreach (SimRegion R in SimRegion.CurrentRegions)
                {
                    WriteLine(R.NetworkInfo());
                }
            }
            else
            {
                foreach (SimRegion R in SimRegion.CurrentRegions)
                {
                    if (R.RegionName.Contains(String.Join(" ", args)))
                        WriteLine(R.NetworkInfo());
                }
            }
            return "ran " + Name;
        }
    }

    class srdebug : cogbot.Actions.Command, SystemApplicationCommand
    {
        public srdebug(BotClient client)
        {
            Name = GetType().Name;
            Description = "Starts the waypoint debuger";
            Category = cogbot.Actions.CommandCategory.Movement;
        }

        public override string Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
            GraphFormer gf = new GraphFormer(SimGlobalRoutes.Instance);
            gf.Show();
            return "ran " + Name;
        }
    }
    class pfdebug : cogbot.Actions.Command, SystemApplicationCommand
    {
        public pfdebug(BotClient client)
        {
            Name = GetType().Name;
            Description = "Starts the pathfinder debuger";
            Category = cogbot.Actions.CommandCategory.Movement;
        }

        public override string Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
            if (args.Length == 0)
            {
                //foreach (SimRegion R in SimRegion.CurrentRegions)
                //{
                //    R.ShowDebugger();
                //}
                SimRegion.GetRegion(Client.Network.CurrentSim).ShowDebugger();
            }
            else
            {
                foreach (SimRegion R in SimRegion.CurrentRegions)
                {
                    if (R.RegionName.Contains(String.Join(" ", args)))
                        R.ShowDebugger();
                }
            }
            return "ran " + Name;
        }
    }

    class pfcatchup : cogbot.Actions.Command
    {
        public pfcatchup(BotClient client)
        {
            Name = GetType().Name;
            Description = "Catches up the pathfinder";
            Category = cogbot.Actions.CommandCategory.Movement;
        }

        public override string Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
            lock (Client.Network.Simulators)
            {
                foreach (Simulator S in Client.Network.Simulators)
                {
                    WorldSystem.CatchUp(S);
                }
            }
            return "ran " + Name;
        }
    }

    class pfg : cogbot.Actions.Command
    {
        public pfg(BotClient client)
        {
            Name = GetType().Name;
            Description = "pfg 180 5 will move backwards 5";
            Category = cogbot.Actions.CommandCategory.Movement;
        }

        public override string Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
            float Dist;
            if (args.Length > 1 && float.TryParse(args[1], out Dist))
            {
                Vector3d av = WorldSystem.TheSimAvatar.GetGlobalLeftPos(int.Parse(args[0]), Dist);
                WorldSystem.TheSimAvatar.MoveTo(av, 1f, 4);
            }
            else
            {
                Vector3d av = WorldSystem.TheSimAvatar.GetGlobalLeftPos(int.Parse(args[0]), 10);
                WorldSystem.TheSimAvatar.TurnToward(av);
            }
            return "ran " + Name;
        }
    }

    class meshinfo : cogbot.Actions.Command
    {
        public meshinfo(BotClient client)
        {
            Name = GetType().Name;
            Description = "Reads the sim prims for improving routes";
            Category = cogbot.Actions.CommandCategory.Movement;
        }

        public override string Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
            if (args.Length==0)
            {
                
            }
            IEnumerable<SimObject> objs = WorldSystem.GetAllSimObjects(String.Join(" ", args));
            foreach (SimObject o in objs)
            {
                WriteLine("MeshInfo: " + o);
                WriteLine(o.Mesh.DebugString());
            }
            return "ran " + Name;
        }
    }

    class simzinfo : cogbot.Actions.Command
    {
        public simzinfo(BotClient client)
        {
            Name = GetType().Name;
            Description = "Calculates the Z level of walking at point. Usage: simzinfo 120 123";
            Category = cogbot.Actions.CommandCategory.Movement;
        }

        public override string Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
            int argcount;
            SimPosition pos = WorldSystem.GetVector(args, out argcount);
            SimPathStore R = pos.GetPathStore();
            Vector3 v3 = pos.GetSimPosition();
            WriteLine("SimZInfo: " + pos + " " + R.GetGroundLevel(v3.X, v3.Y));
            SimWaypoint WP = R.GetWaypointOf(v3);
            WriteLine("WaypointInfo: {0}", WP.OccupiedString(R.GetCollisionPlane(v3.Z)));
            return "ran " + Name;
        }
    }

    class simhinfo : cogbot.Actions.Command
    {
        public simhinfo(BotClient client)
        {
            Name = GetType().Name;
            Description = "Calculates the Height (Z) level of walking at point. Usage: simzinfo 120 123 30";
            Category = cogbot.Actions.CommandCategory.Movement;
        }

        public override string Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
            int argcount;
            SimPosition pos = WorldSystem.GetVector(args, out argcount);
            SimPathStore R = pos.GetPathStore();
            Vector3 v3 = pos.GetSimPosition();
            WriteLine("SimZInfo: " + pos + " " + R.GetGroundLevel(v3.X, v3.Y));

#if COLLIDER_ODE  
            Vector3 landing = R.CreateAndDropPhysicalCube(v3);
            WriteLine("SimHInfo: {0}", landing);
#endif
            return "ran " + Name;
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

        public override string Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
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
            Description = "Reads the sim prims for improving routes then bakes the region";
            Category = cogbot.Actions.CommandCategory.Movement;
        }

        public override string Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
            IEnumerable<SimObject> objs = WorldSystem.GetAllSimObjects(String.Join(" ", args));
            foreach (SimObject o in objs)
            {
                o.UpdateOccupied();
            }
            SimRegion.BakeRegions();
            return "ran " + Name;
        }
    }

    //class srpath : cogbot.Actions.Command
    //{
    //    public srpath(BotClient client)
    //    {
    //        Name = GetType().Name;
    //        Description = "Show the route to the object";
    //        Category = cogbot.Actions.CommandCategory.Movement;
    //    }

    //    public override string Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
    //    {
    //        int argsused;
    //        SimPosition v3 = WorldSystem.GetVector(args, out argsused);
    //        CollisionIndex wp = v3.GetWaypoint();
    //        bool IsFake;
    //        IList<SimRoute> route = WorldSystem.TheSimAvatar.GetRouteList(wp, out IsFake);
    //        String s = "v3=" + WorldSystem.TheSimAvatar.DistanceVectorString(v3) + " wp=" + wp.ToString();
    //        if (IsFake)
    //        {
    //            s += "\nIsFake: ";
    //        }
    //        else
    //        {
    //            s += "\nComputed ";
    //        }
    //        if (route!=null)
    //        for (int i = 0; i < route.Count; i++)
    //        {
    //            s += " \n" + i + ": " + route[i].ToInfoString();
    //        }
    //        return s;
    //    }
    //}


    class srm : cogbot.Actions.Command
    {
        public srm(BotClient client)
        {
            Name = GetType().Name;
            Description = "Move to a the specified point using MoveTo";
            Category = cogbot.Actions.CommandCategory.Movement;
        }

        public override string Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
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
            WriteLine("Starting  " + str);
            bool MadIt = WorldSystem.TheSimAvatar.MoveTo(pos.GetWorldPosition(), maxDistance, maxSeconds);
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

        public override string Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
            int argcount;
            SimPosition pos = WorldSystem.GetVector(args, out argcount);
            if (pos == null)
            {
                return String.Format("Cannot {0} to {1}", Name, String.Join(" ", args));
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
                return String.Format("SUCCESS {0}", str);

            }
            else
            {
                return ("FAILED " + str);
            }
        }
    }

    class blocktw : cogbot.Actions.Command
    {
        public blocktw(BotClient client)
        {
            Name = GetType().Name;
            Description = "Puts one minute temp blocks toward objects";
            Category = cogbot.Actions.CommandCategory.Movement;
        }

        public override string Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
            int argcount;
            SimPosition pos = WorldSystem.GetVector(args, out argcount);
            if (pos == null)
            {
                return "Cannot " + Name + " to " + String.Join(" ", args);
            }

            Vector3d v3d = pos.GetWorldPosition();
            Vector3 v3 = pos.GetSimPosition();
            SimAbstractMover sam = SimCollisionPlaneMover.CreateSimPathMover(WorldSystem.TheSimAvatar, pos, pos.GetSizeDistance());
            sam.BlockTowardsVector(v3);
            return ("SUCCESS ");
        }
    }

    class gto : cogbot.Actions.Command
    {
        public gto(BotClient client)
        {
            Name = "gto";
            Description = "Go to the avatar toward the specified position for a maximum of seconds. gto [prim | [x y]] [dist]";
            Category = cogbot.Actions.CommandCategory.Movement;
        }

        public override string Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
            SimPosition simObject;
            float distance = 2.0f;

            if (args.Length > 3 || args.Length == 0)
                return "Usage: gto [prim | [x y]] [dist]";

            Vector3 local = new Vector3();
            if (float.TryParse(args[0], out local.X) &&
                float.TryParse(args[1], out local.Y))
            {
                local.Z = GetSimPosition().Z;
                Vector3d target = WorldSystem.TheSimAvatar.GetPathStore().LocalToGlobal(local);
                simObject = SimWaypointImpl.CreateGlobal(target);
                if (args.Length == 3) Single.TryParse(args[2], out distance);

            }
            else
            {
                string s = String.Join(" ", args);
                Primitive prim;


                if (WorldSystem.tryGetPrim(s, out prim))
                {

                    simObject = WorldSystem.GetSimObject(prim);
                    if (!simObject.IsRegionAttached())
                    {
                        return "Cannot get Sim Position of " + simObject;
                    }
                    distance = 0.5f + simObject.GetSizeDistance();
                }
                else
                {
                    return "Cannot select " + s;
                }
            }

            WriteLine("gto {0} {1}", simObject, distance);
            WorldSystem.TheSimAvatar.MoveTo(simObject.GetWorldPosition(), distance, 10);
            WorldSystem.TheSimAvatar.StopMoving();
            return WorldSystem.TheSimAvatar.DistanceVectorString(simObject);
        }

        private void Goto(Vector3 target, float p)
        {

            if (true)
            {
                uint x, y;
                Utils.LongToUInts(Client.Network.CurrentSim.Handle, out x, out y);
                Vector2 v2 = new Vector2(target.X, target.Y);
                Vector2 cp = new Vector2(GetSimPosition().X, GetSimPosition().Y);
                float d = Vector2.Distance(v2, cp);
                float dl = d;
                bool autoOff = false;
                while (d > p)
                {
                    if (autoOff)
                    {
                        Client.Self.Movement.TurnToward(target);
                        Client.Self.AutoPilot((ulong)(x + target.X), (ulong)(y + target.Y), GetSimPosition().Z);
                        autoOff = false;
                    }
                    cp = new Vector2(GetSimPosition().X, GetSimPosition().Y);
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
        }
    }

    class turnto : cogbot.Actions.Command
    {
        public turnto(BotClient client)
        {
            Name = "turnto";
            Description = "turn the avatar toward the specified position for a maximum of seconds. turnto [prim | [x y [z]]";
            Category = cogbot.Actions.CommandCategory.Movement;
        }

        public override string Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
            SimPosition simObject;

            if (args.Length > 3 || args.Length == 0)
                return "Usage: turnto [prim | [x y [z]]";

            Vector3 local = new Vector3();
            if (float.TryParse(args[0], out local.X) &&
                float.TryParse(args[1], out local.Y))
            {

                if (args.Length == 3)
                {
                    Single.TryParse(args[2], out local.Z);
                }
                else
                {
                    local.Z = GetSimPosition().Z;
                }
                Vector3d target = WorldSystem.TheSimAvatar.GetPathStore().LocalToGlobal(local);
                simObject = SimWaypointImpl.CreateGlobal(target);
            }
            else
            {
                string s = String.Join(" ", args);
                Primitive prim;


                if (WorldSystem.tryGetPrim(s, out prim))
                {

                    simObject = WorldSystem.GetSimObject(prim);
                    if (!simObject.IsRegionAttached())
                    {
                        return "Cannot get Sim Position of " + simObject;
                    }
                }
                else
                {
                    return "Cannot select " + s;
                }
            }

            WriteLine("ternto {0}", simObject);
            WorldSystem.TheSimAvatar.TurnToward(simObject);
            return WorldSystem.TheSimAvatar.DistanceVectorString(simObject);
        }
    }

    class selectobject : cogbot.Actions.Command
    {
        public selectobject(BotClient client)
        {
            Name = "selectobject";
            Description = "Re select object [prim]";
            Category = cogbot.Actions.CommandCategory.Movement;
        }

        public override string Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
            if (args.Length==0) {
                WorldObjects.ResetSelectedObjects();
                return "ResetSelectedObjects";
            }
            int used;
            Primitive P = WorldSystem.GetPrimitive(args, out used);
            WorldSystem.ReSelectObject(P);
            return "object selected " + P;
        }
    }

    class moveprim : cogbot.Actions.Command
    {
        public moveprim(BotClient client)
        {
            Name = "moveprim";
            Description = "move prim to the relative specified position. Usage: moveprim prim [x y [z]]";
            Category = cogbot.Actions.CommandCategory.Movement;
        }

        public override string Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {

            if (args.Length < 3)
                return "Usage: moveprim prim [x y [z]]";

            int used;
            Primitive P = WorldSystem.GetPrimitive(args, out used);
            SimObject O = WorldSystem.GetSimObject(P);

            used = 1;
            Vector3d prev = O.GetWorldPosition();
            Vector3d local = Vector3d.Zero;
            if (double.TryParse(args[used++], out local.X) &&
                double.TryParse(args[used++], out local.Y))
            {

                if (args.Length > used)
                {
                    double.TryParse(args[used++], out local.Z);
                }
                else
                {
                    local.Z = 0f;// O.GetSimPosition().Z;
                }
            }
            local += prev;
            O.MoveTo(local,1f,10);
            return WorldSystem.TheSimAvatar.DistanceVectorString(O);
        }
    }


    class mvprim : cogbot.Actions.Command
    {
        public mvprim(BotClient client)
        {
            Name = "mvprim";
            Description = "mv prim to the relative specified position. Usage: mvprim [rel] prim [[+/-]x y [z]]";
            Category = cogbot.Actions.CommandCategory.Movement;
        }

        public override string Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {

            if (args.Length < 3)
                return "Usage: mvprim [rel] prim [[+/-]x y [z]]";

            if (args[0] == "rel")
            {
            }
            int used;
            Primitive P = WorldSystem.GetPrimitive(args, out used);
            SimObject O = WorldSystem.GetSimObject(P);

            used = 1;
            Vector3 prev = O.GetSimPosition();
            Vector3 local = Vector3.Zero;
            if (float.TryParse(args[used++], out local.X) &&
                float.TryParse(args[used++], out local.Y))
            {

                if (args.Length > used)
                {
                    Single.TryParse(args[used++], out local.Z);
                }
                else
                {
                    local.Z = 0f;// O.GetSimPosition().Z;
                }
            }
            local += prev;
            O.SetObjectPosition(local);
            return WorldSystem.TheSimAvatar.DistanceVectorString(O);
        }
    }


}
