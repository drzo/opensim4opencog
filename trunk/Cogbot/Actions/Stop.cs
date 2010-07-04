using System;
using System.Collections.Generic;
using System.Text;
using System.Threading;
using OpenMetaverse;

namespace cogbot.Actions
{
    class Stop : Command, BotSystemCommand
    {
        public Stop(BotClient Client)
            : base(Client)
        {
            Description = "Cancels a particular action";
			Usage = "To cancel a particular action, type \"stop <action>\"";
            Parameters = new[] { new NamedParam(typeof(GridClient), null) };
            Category = CommandCategory.BotClient;
        }

        public override CmdResult acceptInput(string verb, Parser args, OutputDelegate WriteLine)
        {
            //base.acceptInput(verb, args);

            BotClient Client = TheBotClient;
            int aborted = 0;
            if (args.objectPhrase.Length == 0)
            {
                foreach (string action in Client.Commands.Keys)
                {
                    //WriteLine(action + ": " + Client.Commands[action].makeHelpString());
                }

                var botCommandThreads = Client.GetBotCommandThreads();

                lock (botCommandThreads) foreach (Thread t in botCommandThreads)
                    {

                        try
                        {
                            if (t.IsAlive)
                            {
                                aborted++;
                                t.Abort();
                            }
                            t.Join();
                        }
                        catch (Exception) { }
                        Client.RemoveThread(t);

                    }
            }
            if (WorldSystem.TheSimAvatar.CurrentAction!=null)
            {
                WorldSystem.TheSimAvatar.CurrentAction = null;
                aborted++;
            }
            WorldSystem.TheSimAvatar.StopMoving();
            Client.describeNext = false;
            return Success("Stopped " + aborted);
        }
    }
}
