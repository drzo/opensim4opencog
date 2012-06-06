using System;
using Cogbot.World;
using OpenMetaverse;

using MushDLR223.ScriptEngines;

namespace Cogbot.Actions.System
{
    public class QuietlyCommand : Command, BotSystemCommand
    {
        public QuietlyCommand(BotClient testClient)
        {
            Name = "quietly";
            Description = "Invoke a botcmd without printing anything.";
            Details = Example("quietly priminfo", "run priminfo and discard results");
            Parameters = CreateParams(
    "command", typeof(BotCommand), "command to execute quietly");

            Category = CommandCategory.BotClient;
        }

        public override CmdResult ExecuteRequest(CmdRequest args)
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