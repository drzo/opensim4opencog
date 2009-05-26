using System;
using System.Collections.Generic;
using System.Text;

namespace cogbot.Actions
{
    class Help : Action
    {
        public Help(BotClient Client)
            : base(Client)
        {
            helpString = "Print this help message.";
            usageString = helpString;
        }

        public override void acceptInput(string verb, Parser args)
        {
            // base.acceptInput(verb, args);

            BotClient Client = TheBotClient;

            if (args.objectPhrase.Length == 0)
            {
                foreach (string action in TheBotClient.Commands.Keys)
                {
                    WriteLine(action + ": " + TheBotClient.Commands[action].makeHelpString());
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
        }
    }
}
