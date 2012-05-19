using System;
using System.Collections.Generic;
using cogbot.TheOpenSims;
using OpenMetaverse;

using MushDLR223.ScriptEngines;

namespace cogbot.Actions.System
{
    public class WaitEventCommand : Command, BotPersonalCommand
    {
        public WaitEventCommand(BotClient testClient)
        {
            Name = "waitevent";
            Description = "waits until a certain event takes place. see " +
                          Htmlize.Wiki("SimObjectEvents", "Object Events") + " for info about the format";
            Usage = Htmlize.Usage("waitevent timewaitms event-name [command] ", "wait for timewaitms for event to match evnet-name, if no event comes thru unblock and call command") +
                Htmlize.Example("waitevent 10000 On-Say say ten seconds are up an no one says a word", "waits ten seconds for someone to say something, if no one says anything it speaks the phrase");
            Parameters = NamedParam.CreateParams(
                "timewaitms", typeof(TimeSpan), "the amount of time to block waiting for the event",
                "eventName",typeof(string),Htmlize.Wiki("SimObjectEvents", "Sim Object Events") + " for info about the format",
                NamedParam.Optional("command", typeof(BotCommand),"The command to execute asynchronously"));
            ResultMap = NamedParam.CreateParams(
                 "event", typeof(SimObjectEvent), "The unblocking event that took place",
                 "message", typeof(string), "if success was false, the reason why",
                 "success", typeof(bool), "true if we got the events");
        }

        public override CmdResult Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
            if (args.Length < 1) return ShowUsage();
            string botcmd = String.Join(" ", args, 0, args.Length).Trim();
            Client.ExecuteCommand(botcmd, fromAgentID, WriteNothing);
            return Success(string.Empty);
        }

        static void WriteNothing(string str, params object[] args)
        {
        }
    }
}