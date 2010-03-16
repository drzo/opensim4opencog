using OpenMetaverse;

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
            lock (Client.Network.Simulators)
            {
                foreach (Simulator S in Client.Network.Simulators)
                {
                    WorldSystem.CatchUp(S);
                }
            }
            return Success("Ran " + Name);
        }
    }
}