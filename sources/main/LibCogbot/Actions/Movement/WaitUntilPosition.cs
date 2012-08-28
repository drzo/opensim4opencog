using System;
using System.Threading;
using OpenMetaverse;
using PathSystem3D.Navigation;
using MushDLR223.ScriptEngines;

namespace Cogbot.Actions.Movement
{
    public class WaitUntilPosition : Cogbot.Actions.Command, BotPersonalCommand
    {
        public WaitUntilPosition(BotClient client)
        {
            Name = "waitpos";
        }

        public override void MakeInfo()
        {
            Description = "Block until the robot gets to a certain position for a certain maxwait";
            Category = Cogbot.Actions.CommandCategory.Movement;
            Details = "waitpos seconds <x,y,z>";
            Parameters = CreateParams("seconds", typeof (TimeSpan), typeof (float),
                                      "position", typeof (SimPosition), typeof (Vector3d));
        }

        public override CmdResult ExecuteRequest(CmdRequest args)
        {
            if (args.Length < 2)
                return ShowUsage();
            string str = Parser.Rejoin(args, 0);
            int argcount;
            float maxSeconds;
            if (!args.TryGetValue("seconds", out maxSeconds))
            {
                maxSeconds = 60000;
            }
            SimPosition pos;
            if (!args.TryGetValue("position", out pos))
            {
                return Failure(String.Format("Cannot {0} to {1}", Name, String.Join(" ", args)));
            }

            DateTime waitUntil = DateTime.Now.Add(TimeSpan.FromSeconds(maxSeconds));
            double maxDistance = pos.GetSizeDistance();
            if (maxDistance < 1) maxDistance = 1;

            bool MadIt = false;
            while (waitUntil > DateTime.Now)
            {
                var gp1 = pos.GlobalPosition;
                var gp2 = TheSimAvatar.GlobalPosition;
                if (Math.Abs(gp1.Z - gp2.Z) < 2) gp1.Z = gp2.Z;
                // do it antyways
                gp1.Z = gp2.Z;
                double cdist = Vector3d.Distance(gp1, gp2);
                if (cdist <= maxDistance)
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