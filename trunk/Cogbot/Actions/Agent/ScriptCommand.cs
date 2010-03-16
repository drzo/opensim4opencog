using System;
using System.IO;
using OpenMetaverse;

namespace cogbot.Actions.Agent
{
    public class ScriptCommand : Command, BotPersonalCommand
    {
        public ScriptCommand(BotClient testClient)
        {
            Name = "script";
            Description = "Reads BotClient commands from a file. One command per line, arguments separated by spaces. Usage: script [filename]";
            Category = CommandCategory.BotClient;
        }

        public override CmdResult Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
            if (args.Length != 1)
                return ShowUsage();// " script [filename]";

            // Load the file
            string[] lines;
            try { lines = File.ReadAllLines(args[0]); }
            catch (Exception e) { return Failure(e.Message); }

            // Execute all of the commands
            for (int i = 0; i < lines.Length; i++)
            {
                string line = lines[i].Trim();

                if (line.Length > 0)
                    WriteLine(TheBotClient.ExecuteCommand(line, WriteLine).ToString());
            }

            return Success("Finished executing " + lines.Length + " commands");
        }
    }
}
