using System;
using System.Collections.Generic;
using System.Text;
using OpenMetaverse;

using MushDLR223.ScriptEngines;

namespace cogbot.Actions.System
{
    class Help : Command, SystemApplicationCommand
    {
        public Help(BotClient Client)
            : base(Client)
        {
            Description = "Print this help message.";
            Usage = Description;
            Parameters = new[] { new NamedParam(typeof(GridClient), null) };
            Category = CommandCategory.BotClient;
            Name = "help";
        }

        public override CmdResult acceptInput(string verb, Parser args, OutputDelegate WriteLine)
        {
            string mustHave = ": "; // everything
            if (args.Length > 0)
            {
                mustHave = args.str.ToLower();
            }
            int found = 0;
            SortedDictionary<string, Command> dictionary = new SortedDictionary<string, Command>(TheBotClient.ClientManager.groupActions);
            foreach (KeyValuePair<string, Command> action in TheBotClient.Commands)
            {
                if (dictionary.ContainsKey(action.Key))
                {
                    dictionary[action.Key] = action.Value;
                } else
                {
                    dictionary.Add(action.Key, action.Value);
                }
            }
            foreach (string action in dictionary.Keys)
            {
                string s = action + ": " + dictionary[action].Description;
                if (!s.ToLower().Contains(mustHave))
                {
                    continue;
                }
                found++;
                WriteLine(s);
            }
            dictionary = TheBotClient.Commands;            

            foreach (string tutorial in TheBotClient.tutorials.Keys)
            {
                string s = tutorial + ": " + TheBotClient.tutorials[tutorial].makeHelpString();
                {
                    continue;
                }
                found++;
                WriteLine(s);
            }
            if (found == 0) WriteLine("I don't know about the verb " + args.objectPhrase + ".");
            Client.describeNext = false;
            return Success("Help complete");
        }
    }
}
