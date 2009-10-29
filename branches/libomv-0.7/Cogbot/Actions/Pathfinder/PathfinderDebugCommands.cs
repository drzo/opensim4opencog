using System;
using cogbot.TheOpenSims;
using OpenMetaverse;
using PathSystem3D.Navigation;
using PathSystem3D.Navigation.Debug;

namespace cogbot.Actions.Pathfinder
{
    public class pfdebug : cogbot.Actions.Command, SystemApplicationCommand
    {
        public pfdebug(BotClient client)
        {
            Name = GetType().Name;
            Description = "Starts the pathfinder debuger";
            Category = cogbot.Actions.CommandCategory.Movement;
        }

        public override CmdResult Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
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
    class srdebug : cogbot.Actions.Command, SystemApplicationCommand
    {
        public srdebug(BotClient client)
        {
            Name = GetType().Name;
            Description = "Starts the waypoint debuger";
            Category = cogbot.Actions.CommandCategory.Movement;
        }

        public override CmdResult Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
            GraphFormer gf = new GraphFormer(SimGlobalRoutes.Instance);
            gf.Show();
            return Success("Ran " + Name);
        }
    }
    //class ideal : cogbot.Actions.Command
    //{
    //    public ideal(BotClient client)
    //    {
    //        Name = GetType().Name;
    //        Description = "Starts the GUI debugger";
    //        Category = cogbot.Actions.CommandCategory.Movement;
    //    }

    //    public override CmdResult Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
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


    //class srpath : cogbot.Actions.Command
    //{
    //    public srpath(BotClient client)
    //    {
    //        Name = GetType().Name;
    //        Description = "Show the route to the object";
    //        Category = cogbot.Actions.CommandCategory.Movement;
    //    }

    //    public override CmdResult Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
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