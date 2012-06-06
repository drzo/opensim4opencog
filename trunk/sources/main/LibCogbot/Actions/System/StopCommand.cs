using System;
using System.Collections.Generic;
using System.Text;
using System.Threading;
using MushDLR223.Utilities;
using OpenMetaverse;

using MushDLR223.ScriptEngines;
#if USE_SAFETHREADS
using Thread = MushDLR223.Utilities.SafeThread;
#endif

namespace Cogbot.Actions
{
    class StopCommand : Command, BotSystemCommand
    {
        public StopCommand(BotClient Client)
            : base(Client)
        {
            Description = @"If called with args, cancels the current modeless action. 
If called with no args, cancels all currently running actions.";
			Details = "<p>stop</p><p>example:</p><p>move 122/144/12</p><p>stop</p>";
            Parameters = CreateParams(
               Optional("action", typeof(BotCommand), 
               @"in theory the action to stop. In practice, if present the current action is stopped, 
if absent all actions are stopped."));
            ResultMap = CreateParams(
                 "message", typeof(string), "if success was false, the reason why",
                 "success", typeof(bool), "true if outfit was worn");
            Category = CommandCategory.BotClient;
        }

        public override CmdResult acceptInput(string verb, Parser args, OutputDelegate WriteLine)
        {
            //base.acceptInput(verb, args);

            BotClient Client = TheBotClient;
            int[] aborted = {0};
            if (args.objectPhrase.Length == 0)
            {
                foreach (string action in Client.Commands.Keys)
                {
                    //WriteLine(action + ": " + Client.Commands[action].makeHelpString());
                }
                int n = 0;
                var botCommandThreads = Client.GetBotCommandThreads();
                //lock (botCommandThreads)
                {
                    int num = botCommandThreads.Count;
                    foreach (Thread t in LockInfo.CopyOf(botCommandThreads))
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
                            Thread thread = t;
                            DLRConsole.ExecWithMaxTime(() =>
                                                           {
                                                               WriteLine("Killing/Removing {0}: {1} IsAlive={2}", num,
                                                                         thread.Name, thread.IsAlive);
                                                               try
                                                               {
                                                                   if (thread.IsAlive)
                                                                   {
                                                                       aborted[0]++;
                                                                       thread.Abort();
                                                                   }
                                                                   thread.Join();
                                                               }
                                                               catch (Exception)
                                                               {
                                                               }
                                                           }, 2000);
                        }
                    }
                }
            }
            if (WorldSystem.TheSimAvatar.CurrentAction!=null)
            {
                WorldSystem.TheSimAvatar.CurrentAction = null;
                aborted[0]++;
            }
            WorldSystem.TheSimAvatar.StopMoving();
            Client.ExecuteCommand("pointat", this, WriteLine);
            Client.describeNext = false;
            return Success("Stopped " + aborted[0]);
        }
    }
}
