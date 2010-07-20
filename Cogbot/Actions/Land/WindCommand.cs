using System;
using cogbot.TheOpenSims;
using OpenMetaverse;
using PathSystem3D.Navigation;

using MushDLR223.ScriptEngines;

namespace cogbot.Actions.Land
{
    public class WindCommand : Command, RegionMasterCommand
    {
        public WindCommand(BotClient testClient)
        {
            Name = "wind";
            Description = "Displays current wind data";
            Usage = "wind [position]";
            Category = CommandCategory.Simulator;
            Parameters = new[] { new NamedParam(typeof(SimPosition), typeof(SimPosition)) };
        }

        public override CmdResult Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
            // Get the agent's current "patch" position, where each patch of
            // wind data is a 16x16m square
            int argsUsed;
            SimPosition aPos = WorldSystem.GetVector(args, out argsUsed);
            Vector3 agentPos = aPos.SimPosition;
            int xPos = (int)Utils.Clamp(agentPos.X, 0.0f, 255.0f) / 16;
            int yPos = (int)Utils.Clamp(agentPos.Y, 0.0f, 255.0f) / 16;
            Simulator sim = SimRegion.GetRegion(aPos.GlobalPosition).TheSimulator;
            if (sim == null) return Failure("Unknown simulator for " + aPos);
            ulong handle = sim.Handle;
            if (!Client.Terrain.WindSpeeds.ContainsKey(handle))
            {
                return Failure("Unknown wind speed at sim: " + sim);
            }
            Vector2[] windSpeeds = Client.Terrain.WindSpeeds[handle];
            Vector2 windSpeed = windSpeeds[yPos * 16 + xPos];
            return Success("Wind speed at " + aPos + " is " + windSpeed);
        }
    }
}
