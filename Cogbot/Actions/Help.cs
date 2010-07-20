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
            // base.acceptInput(verb, args);

            BotClient Client = TheBotClient;
            
            if (args.objectPhrase.Length == 0)
            {
                foreach (string action in TheBotClient.Commands.Keys)
                {
                    WriteLine(action + ": " + TheBotClient.Commands[action].Description);
                }
                foreach (string tutorial in TheBotClient.tutorials.Keys)
                {
                    WriteLine(tutorial + ": " + TheBotClient.tutorials[tutorial].makeHelpString());
                }
            }
            else
            {
                if (Client.Commands.ContainsKey(args.objectPhrase))
                    WriteLine(Client.Commands[args.objectPhrase].makeUsageString());
                else if (Client.tutorials.ContainsKey(args.objectPhrase))
                    WriteLine(Client.tutorials[args.objectPhrase].makeHelpString());
                else
                    WriteLine("I don't know about the verb " + args.objectPhrase + ".");
            }

            Client.describeNext = false;
            return Success("Help complete");
        }
    }
}
