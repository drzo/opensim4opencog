using System.Collections.Generic;
using cogbot.Listeners;
using cogbot.TheOpenSims;
using cogbot.Utilities;
using OpenMetaverse;
using PathSystem3D.Navigation;

namespace cogbot.Actions
{
    public class SelectCommand : cogbot.Actions.Command
    {
        public SelectCommand(BotClient client)
        {
            Name = "select";
            Description = "selects one or more object in world. Usage: select +/-[prim0] +/-[prim1] +/-[prim2]";
            Category = cogbot.Actions.CommandCategory.Objects;
            Parameters = new[] { new NamedParam(typeof(SimObject), typeof(UUID)) };
        }

        public override CmdResult Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
            ListAsSet<SimPosition> objs = TheSimAvatar.GetSelectedObjects();

            if (args.Length == 0)
            {
                foreach (var o in objs)
                {
                    WriteLine(" " + o);
                }
            }
            if (args.Length == 1 && args[0].ToLower() == "none")
            {
                objs.Clear();
                bool was = TheSimAvatar.SelectedBeam;
                TheSimAvatar.SelectedBeam = !was;
                TheSimAvatar.SelectedBeam = was;
            }
            else
            {
                int used = 0;
                bool remove = false;
                while (used < args.Length)
                {
                    args = Parser.SplitOff(args, used);
                    string s = args[0];
                    if (s.StartsWith("-"))
                    {
                        remove = true;
                        s = s.Substring(1);
                    }
                    if (s.StartsWith("+"))
                    {
                        remove = false;
                        s = s.Substring(1);
                    }
                    if (s.Length < 0)
                    {
                        used = 1;
                        continue;
                    }
                    args[0] = s;
                    List<Primitive> PS = WorldSystem.GetPrimitives(args, out used);
                    foreach (var primitive in PS)
                    {
                        SimObject P = WorldSystem.GetSimObject(primitive);
                        if (P == null)
                        {
                            WriteLine("Cannot find " + s);
                            used = 1;
                            continue;
                        }
                        if (remove)
                        {
                            WriteLine("Removing " + P);
                            TheSimAvatar.SelectedRemove(P);
                        }
                        else
                        {
                            WriteLine("Adding " + P);
                            TheSimAvatar.SelectedAdd(P);
                        }
                    }
                    if (used == 0) break;
                }
            }
            return Success("selected objects count=" + objs.Count);
        }
    }
}