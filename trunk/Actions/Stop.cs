using System;
using System.Collections.Generic;
using System.Text;

namespace cogbot.Actions
{
    class Stop : Action
    {
        public Stop(TextForm parent)
            : base(parent)
        {
            helpString = "Cancels a particular action";
			usageString = "To cancel a particular action, type \"stop <action>\"";
        }

        public override void acceptInput(string verb, Parser args)
        {
            base.acceptInput(verb, args);

            if (args.objectPhrase.Length == 0)
            {
                foreach (string action in parent.actions.Keys)
                {
                    parent.output(action + ": " + parent.actions[action].makeHelpString());
                }
            }

            parent.describeNext = false;
        }
    }
}
