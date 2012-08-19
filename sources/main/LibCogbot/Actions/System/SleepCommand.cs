using System;
using OpenMetaverse;
using OpenMetaverse.Packets;
using System.Threading;

using MushDLR223.ScriptEngines;

namespace Cogbot.Actions.System
{
    public class SleepCommand : Command, BotSystemCommand, SynchronousCommand
    {
        uint sleepSerialNum = 1;

        public SleepCommand(BotClient testClient)
        {
            Name = "sleep";
            Description = "Uses AgentPause/AgentResume to sleep the avatar and tell the " +
                "simulator it won't need packets for a time period in seconds. A typical " + 
                "use would be to turn off a bot when not needed";
            Details = AddUsage("sleep &lt;seconds&gt;", "sleeps for nn seconds");
            Parameters = CreateParams("seconds", typeof(int), "seconds to sleep");
            ResultMap = CreateParams(
     "message", typeof(string), "if success was false, the reason why",
     "success", typeof(bool), "true if we slept");
            Category = CommandCategory.BotClient;
        }

        public override CmdResult ExecuteRequest(CmdRequest args)
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
            Thread.Sleep(seconds * 1000);

            AgentResumePacket resume = new AgentResumePacket();
            resume.AgentData.AgentID = Client.Self.AgentID;
            resume.AgentData.SessionID = Client.Self.SessionID;
            resume.AgentData.SerialNum = pause.AgentData.SerialNum;

            Client.Network.SendPacket(resume);

            return Success("Paused, slept for " + seconds + " second(s), and resumed");
        }
    }
}
