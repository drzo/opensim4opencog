using System;
using System.Collections.Generic;
using System.Text;
using System.Threading;

namespace cogbot.Actions
{
    class Stop : Action
    {
        public Stop(BotClient Client)
            : base(Client)
        {
            helpString = "Cancels a particular action";
			usageString = "To cancel a particular action, type \"stop <action>\"";
        }

        public override void acceptInput(string verb, Parser args)
        {
            //base.acceptInput(verb, args);

            BotClient Client = TheBotClient;
            if (args.objectPhrase.Length == 0)
            {
                foreach (string action in Client.Commands.Keys)
                {
                    //WriteLine(action + ": " + Client.Commands[action].makeHelpString());
                }
                lock (Client.botCommandThreads) foreach (Thread t in Client.botCommandThreads)
                    {

                        try
                        {
                            if (t.IsAlive)
                                t.Abort();
                            t.Join();

                        }
                        catch (Exception) { }

                    }
                lock (Client.botCommandThreads) Client.botCommandThreads.Clear();
            }
            WorldSystem.TheSimAvatar.StopMoving();
            Client.describeNext = false;
        }
    }
}
