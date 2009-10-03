using System.Collections.Generic;
using cogbot.Listeners;
using cogbot.TheOpenSims;
using OpenMetaverse;
using Radegast;

namespace cogbot.Actions
{
    public class PointAtCommand : cogbot.Actions.Command, RegionMasterCommand
    {
        public PointAtCommand(BotClient client)
        {
            Name = "PointAt";
            Description = "PointAts from a prim. Usage: PointAt [prim]";
            Category = cogbot.Actions.CommandCategory.Objects;
            Parameters = new[] {  new NamedParam(typeof(SimObject), typeof(UUID)) };
        }

        private bool pointing;
        public override string Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
            RadegastInstance instance = TheBotClient.TheRadegastInstance;
            int used;
            SimObject o = WorldSystem.GetSimObject(args, out used);
            if (o == null) return string.Format("Cant find {0}", string.Join(" ", args));
            Primitive currentPrim = o.Prim;
            if (args.Length == 0)
            {
                instance.State.UnSetPointing();
            } else {
                instance.State.SetPointing(currentPrim, 3);
            }
            return Name + " on " + o;
        }
    }
}