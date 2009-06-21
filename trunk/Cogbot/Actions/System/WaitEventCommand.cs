using System;
using cogbot.TheOpenSims;
using OpenMetaverse;

namespace cogbot.Actions
{
    public class WaitEventCommand : Command
    {
        public WaitEventCommand(BotClient testClient)
        {
            Name = "waitevent";
            Description = "wait until a certain event takes place.  Usage: waitevent 10000 EventPipeline say <_> ten seconds are up";
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