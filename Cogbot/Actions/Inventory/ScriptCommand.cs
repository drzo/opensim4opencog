using System;
using System.IO;
using OpenMetaverse;

namespace cogbot.Actions
{
    public class ScriptCommand : Command
    {
        public ScriptCommand(BotClient testClient)
        {
            Name = "script";
            Description = "Reads BotClient commands from a file. One command per line, arguments separated by spaces. Usage: script [filename]";
            Category = CommandCategory.TestClient;
        }

        public override string Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
            if (args.Length != 1)
                return "Usage: script [filename]";

            // Load the file
            string[] lines;
            try { lines = File.ReadAllLines(args[0]); }
            catch (Exception e) { return e.Message; }

            // Execute all of the commands
            for (int i = 0; i < lines.Length; i++)
            {
                string line = lines[i].Trim();

                if (line.Length > 0)
                    WriteLine(TheBotClient.ExecuteCommand(line, WriteLine));
            }

            return "Finished executing " + lines.Length + " commands";
        }
    }
}
