using MushDLR223.Utilities;
using OpenMetaverse;
using MushDLR223.ScriptEngines;

namespace Cogbot.Actions.Land
{
    internal class SimCatchUp : Cogbot.Actions.Command, RegionMasterCommand, AsynchronousCommand
    {
        public SimCatchUp(BotClient client)
        {
            Name = GetType().Name;
        }

        public override void MakeInfo()
        {
            Description = "Force the pathfinder to update";
            AddVersion(CreateParams(), "Catches up the pathfinder. Forces the bot to synch it's model of the sim" +
                                       " used for pathfinding with the server. To have this happen constantly set" +
                                       " <a href='wiki/Sysvars#DoSimulatorsCatchUp'>DoSimulatorsCatchUp</a> to true");
            Category = Cogbot.Actions.CommandCategory.Simulator;
        }

        public override CmdResult ExecuteRequest(CmdRequest args)
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