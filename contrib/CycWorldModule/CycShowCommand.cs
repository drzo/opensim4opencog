using cogbot;
using cogbot.Actions;
using cogbot.TheOpenSims;
using OpenMetaverse;
using PathSystem3D.Navigation;

namespace CycWorldModule
{
    public class CycShowCommand : Command
    {
        public CycShowCommand(BotClient Client)
            : base(Client)
        {
            Name = "CycShow";
            Parameters = new []{ new NamedParam(typeof(SimObject), typeof(UUID))};
        }
        public override CmdResult Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
            int argsUsed;
            SimPosition target = WorldSystem.GetVector(args, out argsUsed);
            CycWorldModule.CycModule.Show(target);
            return Success("CycShow " + target);
        }
    }
}