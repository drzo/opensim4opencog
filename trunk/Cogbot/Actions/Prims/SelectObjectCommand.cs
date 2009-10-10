using cogbot.Listeners;
using cogbot.TheOpenSims;
using OpenMetaverse;

namespace cogbot.Actions
{
    public class SelectObjectCommand : cogbot.Actions.Command
    {
        public SelectObjectCommand(BotClient client)
        {
            Name = "selectobject";
            Description = "Re select object [prim]";
            Category = cogbot.Actions.CommandCategory.Objects;
            Parameters = new[] {  new NamedParam(typeof(SimObject), typeof(UUID)) };
        }

        public override CmdResult Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
            if (args.Length==0) {
                WorldObjects.ResetSelectedObjects();
                return Success("ResetSelectedObjects");
            }
            int used;
            Primitive P = WorldSystem.GetPrimitive(args, out used);
            WorldSystem.ReSelectObject(P);
            return Success("object selected " + P);
        }
    }
}