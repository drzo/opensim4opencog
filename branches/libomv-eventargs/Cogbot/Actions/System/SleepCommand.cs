using System;
using OpenMetaverse;
using OpenMetaverse.Packets;

namespace cogbot.Actions
{
    public class SleepCommand : Command, BotSystemCommand
    {
        uint sleepSerialNum = 1;

        public SleepCommand(BotClient testClient)
        {
            Name = "sleep";
            Description = "Uses AgentPause/AgentResume and sleeps for a given number of seconds. Usage: sleep [seconds]";
            Category = CommandCategory.TestClient;
        }

        public override CmdResult Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
            int seconds;
            if (args.Length != 1 || !Int32.TryParse(args[0], out seconds))
                return ShowUsage();// " sleep [seconds]";

            AgentPausePacket pause = new AgentPausePacket();
            pause.AgentData.AgentID = Client.Self.AgentID;
            pause.AgentData.SessionID = Client.Self.SessionID;
            pause.AgentData.SerialNum = sleepSerialNum++;

            Client.Network.SendPacket(pause);

            // Sleep
            System.Threading.Thread.Sleep(seconds * 1000);

            AgentResumePacket resume = new AgentResumePacket();
            resume.AgentData.AgentID = Client.Self.AgentID;
            resume.AgentData.SessionID = Client.Self.SessionID;
            resume.AgentData.SerialNum = pause.AgentData.SerialNum;

            Client.Network.SendPacket(resume);

            return Success("Paused, slept for " + seconds + " second(s), and resumed");
        }
    }
}
