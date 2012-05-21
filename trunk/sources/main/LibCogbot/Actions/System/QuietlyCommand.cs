using System;
using cogbot.TheOpenSims;
using OpenMetaverse;

using MushDLR223.ScriptEngines;

namespace cogbot.Actions.System
{
    public class QuietlyCommand : Command, BotSystemCommand
    {
        public QuietlyCommand(BotClient testClient)
        {
            Name = "quietly";
            Description = "Invoke a botcmd without printing anything.";
            Usage = Htmlize.Example("quietly priminfo", "run priminfo and discard results");
            Parameters = NamedParam.CreateParams(
    "command", typeof(BotCommand), "command to execute quietly");

            Category = CommandCategory.BotClient;
        }

        public override CmdResult Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
            if (args.Length < 1) return ShowUsage();
            string botcmd = String.Join(" ", args, 0, args.Length).Trim();
			try {
				Client.ExecuteCommand(botcmd, fromAgentID, WriteNothing);
			} catch (Exception e) {
				return Failure(string.Empty);
			}
			return Success(string.Empty);
		}

        static void WriteNothing(string str, params object[] args)
        {
        }
    }
}