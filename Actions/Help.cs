using System;
using System.Collections.Generic;
using System.Text;

namespace cogbot.Actions
{
    class Help : Action
    {
        public Help(TextForm parent)
            : base(parent)
        {
            helpString = "Print this help message.";
            usageString = helpString;
        }

        public override void acceptInput(string verb, Parser args)
        {
            // base.acceptInput(verb, args);

            if (args.objectPhrase.Length == 0)
            {
                foreach (string action in parent.actions.Keys)
                {
                    parent.output(action + ": " + parent.actions[action].makeHelpString());
                }
                foreach (string tutorial in parent.tutorials.Keys)
                {
                    parent.output(tutorial + ": " + parent.tutorials[tutorial].makeHelpString());
                }
            }
            else
            {
                if (parent.actions.ContainsKey(args.objectPhrase))
                    parent.output(parent.actions[args.objectPhrase].makeUsageString());
                else if (parent.tutorials.ContainsKey(args.objectPhrase))
                    parent.output(parent.tutorials[args.objectPhrase].makeHelpString());
                else
                    parent.output("I don't know about the verb " + args.objectPhrase + ".");
            }

            parent.describeNext = false;
        }
    }
}
