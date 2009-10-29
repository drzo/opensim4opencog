using OpenMetaverse;
using PathSystem3D.Navigation;

namespace cogbot.Actions.Pathfinder
{
    public class simhinfo : cogbot.Actions.Command, RegionMasterCommand
    {
        public simhinfo(BotClient client)
        {
            Name = GetType().Name;
            Description = "Calculates the Height (Z) level of walking at point. Usage: simzinfo 120 123 30";
            Category = cogbot.Actions.CommandCategory.Movement;
            Parameters = new[] {  new NamedParam(typeof(SimPosition),typeof(SimPosition)) };
        }

        public override CmdResult Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
            int argcount;
            SimPosition pos = WorldSystem.GetVector(args, out argcount);
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

    public class simzinfo : cogbot.Actions.Command, RegionMasterCommand
    {
        public simzinfo(BotClient client)
        {
            Name = GetType().Name;
            Description = "Calculates the Z level of walking at point. Usage: simzinfo 120 123";
            Category = cogbot.Actions.CommandCategory.Movement;
            Parameters = new[] { new NamedParam(typeof(SimPosition), typeof(SimPosition)) };
        }

        public override CmdResult Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
            int argcount;
            SimPosition pos = WorldSystem.GetVector(args, out argcount);
            SimPathStore R = pos.PathStore;
            Vector3 v3 = pos.SimPosition;
            WriteLine("SimZInfo: " + pos + " " + R.GetGroundLevel(v3.X, v3.Y));
            SimWaypoint WP = R.GetWaypointOf(v3);
            WriteLine("WaypointInfo: {0}", WP.OccupiedString(R.GetCollisionPlane(v3.Z)));
            return Success("Ran " + Name);
        }
    }
}