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
            Description = "Print help on everything, or help on a topic string. The full help text is searched for the string.";
            Usage = Htmlize.Usage("help", "shows the overview of all commands") +
                    Htmlize.Usage("help moveto", "shows overvierw and usage/examples on the moveto command");
            Parameters = NamedParam.CreateParams(
                    NamedParam.Optional("topic", typeof(string), "Optional text to search for."));

            ResultMap = NamedParam.CreateParams(
                 "message", typeof(string), "if success was false, the reason why",
                 "success", typeof(bool), "true if outfit was worn");

            Category = CommandCategory.BotClient;
            Name = "help";
        }

        public override CmdResult acceptInput(string verb, Parser args, OutputDelegate WriteLine)
        {
            string mustHave = ": "; // everything
            bool detailed = false;
            if (args.Length > 0)
            {
                mustHave = args.str.ToLower().TrimEnd('s') + ": ";
                detailed = true;
            }
            int found = 0;
            var dictionary = new SortedDictionary<string, CommandInfo>(TheBotClient.ClientManager.groupActions);
            foreach (var action in TheBotClient.Commands)
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
                CommandInfo info = dictionary[action];
                string overview = action.TrimEnd('s') + ": " + info.Description;
                string s = overview + " " + info.usageString;
                if (!s.ToLower().Contains(mustHave))
                {
                    continue;
                }
                found++;
                WriteLine(overview);
                if (detailed)
                {
                    WriteLine(info.usageString);
                }
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
