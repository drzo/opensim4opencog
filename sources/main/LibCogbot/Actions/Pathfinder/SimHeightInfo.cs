using OpenMetaverse;
using PathSystem3D.Navigation;
using MushDLR223.ScriptEngines;

namespace Cogbot.Actions.Pathfinder
{
    public class simhinfo : Cogbot.Actions.Command, RegionMasterCommand, AsynchronousCommand
    {
        public simhinfo(BotClient client)
        {
            Name = GetType().Name;
        }

        public override void MakeInfo()
        {
            Description = "Calculates the Height (Z) level of walking at point. Usage: simzinfo 120 123 30";
            Category = Cogbot.Actions.CommandCategory.Movement;
            Parameters = CreateParams("pos", typeof (SimPosition), "postion for " + Name);
        }

        public override CmdResult ExecuteRequest(CmdRequest args)
        {
            SimPosition pos;
            if (!args.TryGetValue("pos", out pos)) pos = TheSimAvatar;
            SimPathStore R = pos.PathStore;
            Vector3 v3 = pos.SimPosition;
            WriteLine("SimZInfo: " + pos + " " + R.GetGroundLevel(v3.X, v3.Y));

#if COLLIDER_ODE  
            Vector3 landing = R.CreateAndDropPhysicalCube(v3);
            WriteLine("SimHInfo: {0}", landing);
#endif
            return Success("Ran " + Name);
        }
    }

    public class simzinfo : Cogbot.Actions.Command, RegionMasterCommand, AsynchronousCommand
    {
        public simzinfo(BotClient client)
        {
            Name = GetType().Name;
        }

        public override void MakeInfo()
        {
            Description = "Calculates the Z level of walking at point. Usage: simzinfo 120 123";
            Category = Cogbot.Actions.CommandCategory.Movement;
            Parameters = CreateParams("pos", typeof (SimPosition), "postion for " + Name);
        }

        public override CmdResult ExecuteRequest(CmdRequest args)
        {
            SimPosition pos;
            if (!args.TryGetValue("pos", out pos)) pos = TheSimAvatar;
            SimPathStore R = pos.PathStore;
            Vector3 v3 = pos.SimPosition;
            WriteLine("SimZInfo: " + pos + " " + R.GetGroundLevel(v3.X, v3.Y));
            SimWaypoint WP = R.GetWaypointOf(v3);
            WriteLine("WaypointInfo: {0}", WP.OccupiedString(R.GetCollisionPlane(v3.Z)));
            return Success("Ran " + Name);
        }
    }
}