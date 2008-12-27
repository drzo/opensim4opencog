using System;
using OpenMetaverse;

namespace cogbot.Actions.Movement
{
    class RightCommand : Command
    {
        public RightCommand(cogbot.TextForm client)
        {
            Name = "right";
            Description = "Sends the move right command to the server for a single packet or a given number of seconds. Usage: right [seconds]";
            Category = CommandCategory.Movement;
        }

        public override string Execute(string[] args, UUID fromAgentID)
        {
            if (args.Length > 1)
                return "Usage: right [seconds]";

            if (args.Length == 0)
            {
                client.Self.Movement.SendManualUpdate(AgentManager.ControlFlags.AGENT_CONTROL_LEFT_NEG, client.Self.Movement.Camera.Position,
                    client.Self.Movement.Camera.AtAxis, client.Self.Movement.Camera.LeftAxis, client.Self.Movement.Camera.UpAxis,
                    client.Self.Movement.BodyRotation, client.Self.Movement.HeadRotation, client.Self.Movement.Camera.Far, AgentManager.AgentFlags.None,
                    AgentManager.AgentState.None, true);
            }
            else
            {
                // Parse the number of seconds
                int duration;
                if (!Int32.TryParse(args[0], out duration))
                    return "Usage: right [seconds]";
                // Convert to milliseconds
                duration *= 1000;

                int start = Environment.TickCount;

                client.Self.Movement.LeftNeg = true;

                while (Environment.TickCount - start < duration)
                {
                    // The movement timer will do this automatically, but we do it here as an example
                    // and to make sure updates are being sent out fast enough
                    client.Self.Movement.SendUpdate(false);
                    System.Threading.Thread.Sleep(100);
                }

                client.Self.Movement.LeftNeg = false;
            }

            return "Moved right";
        }
    }
}
