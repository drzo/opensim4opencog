using System;
using OpenMetaverse;
using PathSystem3D.Navigation;

using MushDLR223.ScriptEngines;
using cogbot.TheOpenSims;

namespace cogbot.Actions.Pathfinder
{


    public class AStarPath : cogbot.Actions.Command, BotPersonalCommand
    {
        // http://logicmoo.dyndns.org:5580/?cmd=astarpath&args=%22Douglas%20Miles%22%20%22Nephrael%20Rae%22
        public AStarPath(BotClient client)
        {
            Name = GetType().Name;
            Description = "Use A* Pathfinding to get to object";
            Category = cogbot.Actions.CommandCategory.Movement;
            Parameters = new[] { new NamedParam( typeof(SimPosition), typeof(Vector3d)) };

        }

        public override CmdResult Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
            int argcount;
            bool asLocal = false;
            if (args.Length > 1 && args[0] == "local")
            {
                args = Parser.SplitOff(args, 1);
                asLocal = true;
            }
            SimPosition pos = WorldSystem.GetVector(args, out argcount);
            if (pos == null)
            {
                return Failure(String.Format("Cannot {0} to {1}", Name, String.Join(" ", args)));
            }
            args = Parser.SplitOff(args, argcount);
            Vector3d from, to;
            SimPosition pos2 = WorldSystem.GetVector(args, out argcount);
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

        public static  string VectorRoundString(Vector3d vector3D)
        {
            return string.Format("<{0},{1},{2}>", Math.Round(vector3D.X, 1), Math.Round(vector3D.Y, 1), Math.Round(vector3D.Z, 1));
        }
    }
}