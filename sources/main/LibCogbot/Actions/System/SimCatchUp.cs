using MushDLR223.Utilities;
using OpenMetaverse;

using MushDLR223.ScriptEngines;

namespace cogbot.Actions.Land
{
    class SimCatchUp : cogbot.Actions.Command, SystemApplicationCommand
    {
        public SimCatchUp(BotClient client)
        {
            Name = GetType().Name;
            Description = "Catches up the pathfinder";
            Category = cogbot.Actions.CommandCategory.Simulator;
        }

        public override CmdResult Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
            {
                foreach (Simulator sim in LockInfo.CopyOf(Client.Network.Simulators))
                {
                    WorldSystem.CatchUp(sim);
                }
            }
            return Success("Ran " + Name);
        }
    }
}