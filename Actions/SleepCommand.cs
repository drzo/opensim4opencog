using System;
using OpenMetaverse;
using OpenMetaverse.Packets;

namespace cogbot.Actions
{
    public class SleepCommand : Command
    {
        uint sleepSerialNum = 1;

        public SleepCommand(cogbot.TextForm testClient)
        {
            Name = "sleep";
            Description = "Uses AgentPause/AgentResume and sleeps for a given number of seconds. Usage: sleep [seconds]";
            Category = CommandCategory.TestClient;
        }

        public override string Execute(string[] args, UUID fromAgentID)
        {
            int seconds;
            if (args.Length != 1 || !Int32.TryParse(args[0], out seconds))
                return "Usage: sleep [seconds]";

            AgentPausePacket pause = new AgentPausePacket();
            pause.AgentData.AgentID = client.Self.AgentID;
            pause.AgentData.SessionID = client.Self.SessionID;
            pause.AgentData.SerialNum = sleepSerialNum++;

            client.Network.SendPacket(pause);

            // Sleep
            System.Threading.Thread.Sleep(seconds * 1000);

            AgentResumePacket resume = new AgentResumePacket();
            resume.AgentData.AgentID = client.Self.AgentID;
            resume.AgentData.SessionID = client.Self.SessionID;
            resume.AgentData.SerialNum = pause.AgentData.SerialNum;

            client.Network.SendPacket(resume);

            return "Paused, slept for " + seconds + " second(s), and resumed";
        }
    }
}
