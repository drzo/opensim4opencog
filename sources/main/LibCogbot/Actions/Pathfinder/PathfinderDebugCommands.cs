using System;
using Cogbot.World;
using OpenMetaverse;
using PathSystem3D.Navigation;
using PathSystem3D.Navigation.Debug;
using MushDLR223.ScriptEngines;

namespace Cogbot.Actions.Pathfinder
{
    public class pfdebug : Cogbot.Actions.Command, SystemApplicationCommand
    {
        public pfdebug(BotClient client)
        {
            Name = GetType().Name;
        }

        public override void MakeInfo()
        {
            Description = "Starts the pathfinder debuger";
            Category = Cogbot.Actions.CommandCategory.Movement;
        }

        public override CmdResult ExecuteRequest(CmdRequest args)
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
            return Success("Ran " + Name);
        }
    }

    internal class srdebug : Cogbot.Actions.Command, SystemApplicationCommand
    {
        public srdebug(BotClient client)
        {
            Name = GetType().Name;
        }

        public override void MakeInfo()
        {
            Description = "Starts the waypoint debuger";
            Category = Cogbot.Actions.CommandCategory.Movement;
        }

        public override CmdResult ExecuteRequest(CmdRequest args)
        {
            GraphFormer gf = new GraphFormer(SimGlobalRoutes.Instance);
            gf.Show();
            return Success("Ran " + Name);
        }
    }

    //class ideal : Cogbot.Actions.Command
    //{
    //    public ideal(BotClient client)
    //    {
    //        Name = GetType().Name;
    //        } override public void MakeInfo() { Description = "Starts the GUI debugger";
    //        Category = Cogbot.Actions.CommandCategory.Movement;
    //    }

    //    public override CmdResult ExecuteRequest(CmdRequest args)
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
    //        return Success("Ran " + Name);
    //    }
    //}


    //class srpath : Cogbot.Actions.Command
    //{
    //    public srpath(BotClient client)
    //    {
    //        Name = GetType().Name;
    //        } override public void MakeInfo() { Description = "Show the route to the object";
    //        Category = Cogbot.Actions.CommandCategory.Movement;
    //    }

    //    public override CmdResult ExecuteRequest(CmdRequest args)
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
}