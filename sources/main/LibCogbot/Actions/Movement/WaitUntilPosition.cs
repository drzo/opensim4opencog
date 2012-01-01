using System;
using System.Threading;
using OpenMetaverse;
using PathSystem3D.Navigation;

using MushDLR223.ScriptEngines;

namespace cogbot.Actions.Movement
{

    public class WaitUntilPosition : cogbot.Actions.Command, BotPersonalCommand
    {
        public WaitUntilPosition(BotClient client)
        {
            Name = "waitpos";
            Description = "Block until the robot gets to a certain position for a certain maxwait";
            Category = cogbot.Actions.CommandCategory.Movement;
            Usage = "waitpos seconds <x,y,z>";
            Parameters = new[]
                             {
                                 new NamedParam("seconds", typeof (TimeSpan), typeof (float)),
                                 new NamedParam("position", typeof (SimPosition), typeof (Vector3d))
                             };

        }

        public override CmdResult Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
            if (args.Length < 2)
                return ShowUsage();
            string str = Parser.Rejoin(args, 0);
            int argcount;
            float maxSeconds;
            if (!float.TryParse(args[0], out maxSeconds))
            {
                maxSeconds = -1;
            }
            else
            {
                args = Parser.SplitOff(args, 1);
            }
            SimPosition pos = WorldSystem.GetVector(args, out argcount);
            if (pos == null)
            {
                return Failure(String.Format("Cannot {0} to {1}", Name, String.Join(" ", args)));
            }

            DateTime waitUntil = DateTime.Now.Add(TimeSpan.FromSeconds(maxSeconds));
            double maxDistance = pos.GetSizeDistance();

            bool MadIt = false;
            while (waitUntil > DateTime.Now)
            {
                double cdist = Vector3d.Distance(pos.GlobalPosition, TheSimAvatar.GlobalPosition);
                if ( cdist <= maxDistance)
                {
                    MadIt = true;
                    break;
                }
                else
                {
                    Thread.Sleep(100);
                }
            }
            if (MadIt)
            {
                return Success(string.Format("SUCCESS {0}", str));

            }
            else
            {
                return Failure("FAILED " + str);
            }
        }
    }
}