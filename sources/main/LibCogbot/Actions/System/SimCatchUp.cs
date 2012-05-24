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
            Description = "Catches up the pathfinder. Forces the bot to synch it's model of the sim" +
                " used for pathfinding with the server. To have this happen constantly set" +
            " <a href='wiki/Sysvars#DoSimulatorsCatchUp'>DoSimulatorsCatchUp</a> to true";
            Details = AddUsage("simcatchup", "Force the pathfinder to update");
            Parameters = CreateParams();
            ResultMap = CreateParams(
                 "message", typeof(string), "if success was false, the reason why",
                 "success", typeof(bool), "true if we crouched");
            Category = cogbot.Actions.CommandCategory.Simulator;
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