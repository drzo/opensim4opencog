using System;
using Cogbot.World;
using OpenMetaverse;

using MushDLR223.ScriptEngines;

namespace Cogbot.Actions.System
{
    public class QuietlyCommand : Command, BotSystemCommand, SynchronousCommand
    {
        public QuietlyCommand(BotClient testClient)
        {
            Name = "quietly";
            TheBotClient = testClient;
        }

        override public void MakeInfo()
        {
            Description = "Invoke a botcmd without printing anything.";
            AddExample("quietly priminfo", "run priminfo and discard results");
            AddVersion(CreateParams("command", typeof(BotCommand), "command to execute quietly"), Description);            
            Category = CommandCategory.BotClient;
        }

        public override CmdResult ExecuteRequest(CmdRequest args)
        {
            if (args.Length < 1) return ShowUsage();
            string botcmd = String.Join(" ", args, 0, args.Length).Trim();
            try
            {
                Client.ExecuteCommand(botcmd, args.CallerAgent, WriteNothing, args.CmdFlags);
            }
            catch (Exception e)
            {
                return Failure(string.Empty);
            }
            return Success(string.Empty);
        }

        static void WriteNothing(string str, params object[] args)
        {
        }
    }
}