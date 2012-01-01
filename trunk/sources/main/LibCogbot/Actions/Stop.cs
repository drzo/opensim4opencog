using System;
using System.Collections.Generic;
using System.Text;
using System.Threading;
using OpenMetaverse;

using MushDLR223.ScriptEngines;

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
                int n = 0;
                var botCommandThreads = Client.GetBotCommandThreads();
                lock (botCommandThreads)
                {
                    int num = botCommandThreads.Count;
                    foreach (Thread t in botCommandThreads)
                    {
                        Client.RemoveThread(t);
                        n++;
                        num--;
                        //System.Threading.ThreadStateException: Thread is dead; state cannot be accessed.
                        //  at System.Threading.Thread.IsBackgroundNative()
                        if (!t.IsAlive)
                        {
                            WriteLine("Removing {0}: {1} IsAlive={2}", num, t.Name, t.IsAlive);
                        }
                        else
                        {
                            WriteLine("Killing/Removing {0}: {1} IsAlive={2}", num, t.Name, t.IsAlive);
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
                        }

                    }
                }
            }
            if (WorldSystem.TheSimAvatar.CurrentAction!=null)
            {
                WorldSystem.TheSimAvatar.CurrentAction = null;
                aborted++;
            }
            WorldSystem.TheSimAvatar.StopMoving();
            Client.ExecuteCommand("pointat", this, WriteLine);
            Client.describeNext = false;
            return Success("Stopped " + aborted);
        }
    }
}
