using System;
using System.Collections.Generic;
using System.Text;
using OpenMetaverse;

using MushDLR223.ScriptEngines;

namespace cogbot.Actions.System
{
    class ParamHelp : Command, SystemApplicationCommand
    {
        public ParamHelp(BotClient Client)
            : base(Client)
        {
            Description = "Scans the documentation for cogbot commands using the 'Parameters' field.";
            Usage = Description;
            Parameters = new[] { new NamedParam(typeof(GridClient), null) };
            Category = CommandCategory.BotClient;
            Name = "paramhelp";
        }

        public override CmdResult acceptInput(string verb, Parser args, OutputDelegate WriteLine)
        {
            string mustHave = ": "; // everything
            if (args.Length > 0)
            {
                mustHave = args.str.ToLower();
            }
            int found = 0;
            foreach (string action in TheBotClient.Commands.Keys)
            {
                string s = action + ": " + TheBotClient.Commands[action].Description;
                if (!s.ToLower().Contains(mustHave))
                {
                    continue;
                }
                found++;
                WriteLine(s);
            }
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
