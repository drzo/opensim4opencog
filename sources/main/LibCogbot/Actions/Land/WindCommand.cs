using System;
using Cogbot.World;
using OpenMetaverse;
using PathSystem3D.Navigation;
using MushDLR223.ScriptEngines;

namespace Cogbot.Actions.Land
{
    public class WindCommand : Command, RegionMasterCommand, AsynchronousCommand
    {
        public WindCommand(BotClient testClient)
        {
            Name = "wind";
        }

        public override void MakeInfo()
        {
            Description = "Displays current wind data";
            Details = "wind [position]";
            Category = CommandCategory.Simulator;
            Parameters = CreateParams("position", typeof (SimPosition), "the location you wish to " + Name);
        }

        public override CmdResult ExecuteRequest(CmdRequest args)
        {
            // Get the agent's current "patch" position, where each patch of
            // wind data is a 16x16m square
            int argsUsed;
            SimPosition aPos = WorldSystem.GetVector(args, out argsUsed);
            Vector3 agentPos = aPos.SimPosition;
            int xPos = (int) Utils.Clamp(agentPos.X, 0.0f, 255.0f)/16;
            int yPos = (int) Utils.Clamp(agentPos.Y, 0.0f, 255.0f)/16;
            Simulator sim = SimRegion.GetRegion(aPos.GlobalPosition).TheSimulator;
            if (sim == null) return Failure("Unknown simulator for " + aPos);
            ulong handle = sim.Handle;
            if (sim.WindSpeeds == null)
            {
                return Failure("Unknown wind speed at sim: " + sim);
            }
            Vector2[] windSpeeds = sim.WindSpeeds;
            Vector2 windSpeed = windSpeeds[yPos*16 + xPos];
            return Success("Wind speed at " + aPos + " is " + windSpeed);
        }
    }
}