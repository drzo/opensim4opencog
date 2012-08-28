using System;
using OpenMetaverse;
using PathSystem3D.Navigation;
using MushDLR223.ScriptEngines;
using Cogbot.World;

namespace Cogbot.Actions.Pathfinder
{
    public class AStarPath : Cogbot.Actions.Command, BotPersonalCommand
    {
        // http://logicmoo.dyndns.org:5580/?cmd=astarpath&args=%22Douglas%20Miles%22%20%22Nephrael%20Rae%22
        public AStarPath(BotClient client)
        {
            Name = GetType().Name;
        }

        public override void MakeInfo()
        {
            Description = "Return the path that would be used by A* Pathfinding to get to object";
            Category = Cogbot.Actions.CommandCategory.Movement;
            Parameters = CreateParams(
                OptionalFlag("--local", "use local " + Name),
                "start", typeof (SimPosition), "the start you wish to " + Name,
                "end", typeof (SimPosition), "the end you wish to " + Name);
        }

        public override CmdResult ExecuteRequest(CmdRequest args)
        {
            bool asLocal = args.IsTrue("--local");
            SimPosition pos, pos2;
            if (!args.TryGetValue("start", out pos)) pos = TheSimAvatar;
            if (!args.TryGetValue("end", out pos2)) pos = TheSimAvatar;
            Vector3d from, to;
            if (pos2 == null)
            {
                pos2 = pos;
                pos = TheSimAvatar;
            }
            from = pos.GlobalPosition;
            to = pos2.GlobalPosition;
            Vector3d offset = Vector3d.Zero;
            if (asLocal)
            {
                offset = pos.PathStore.GlobalStart;
            }
            bool onlyStart, faked;
            var path = SimPathStore.GetPath(null, from, to, 1, out onlyStart, out faked);
            WriteLine("onlyStart=" + onlyStart);
            WriteLine("faked=" + faked);
            WriteLine("start=" + VectorRoundString(from - offset));
            WriteLine("end=" + VectorRoundString(to - offset));
            WriteLine("pstart=" + pos);
            WriteLine("pend=" + pos2);
            WriteLine("len=" + path.Count);
            int step = 0;
            foreach (Vector3d vector3D in path)
            {
                WriteLine("p" + step + "=" + VectorRoundString(vector3D - offset));
                step++;
            }
            return Success("SUCCESS " + Name);
        }

        public static string VectorRoundString(Vector3d vector3D)
        {
            return string.Format("<{0},{1},{2}>", Math.Round(vector3D.X, 1), Math.Round(vector3D.Y, 1),
                                 Math.Round(vector3D.Z, 1));
        }
    }
}