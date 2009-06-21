using System;
using cogbot.TheOpenSims;
using OpenMetaverse;

namespace cogbot.Actions
{
    public class QuietlyCommand : Command
    {
        public QuietlyCommand(BotClient testClient)
        {
            Name = "quietly";
            Description = "Invoke a console command with no return results.  Usage: quietly priminfo";
            Category = CommandCategory.TestClient;
        }

        public override string Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
            if (args.Length < 1) return Description;
            string botcmd = String.Join(" ", args, 0, args.Length).Trim();
            Client.ExecuteCommand(botcmd, WriteNothing);
            return string.Empty;
        }

        static void WriteNothing(string str, object[] args)
        {
        }
    }
}