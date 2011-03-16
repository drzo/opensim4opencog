using System;
using System.Collections.Generic;
using System.Runtime.InteropServices;
using System.Threading;
using OpenMetaverse;
using OpenMetaverse.Packets;

using MushDLR223.ScriptEngines;

namespace cogbot.Actions
{
    public class ThreadCommand : Command, BotSystemCommand
    {
        public ThreadCommand(BotClient testClient)
        {
            Name = "thread";
            Description = "executes a command in its own thread. Usage: thread anim 30 crouch";
            Category = CommandCategory.BotClient;
        }

        public override CmdResult Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
            //BotClient Client = TheBotClient;
            if (args.Length < 1)
            {
                return ShowUsage();// " ";
            }
            if (args.Length == 1 && args[0] == "list")
            {
                int n = 0;
                var botCommandThreads = Client.GetBotCommandThreads();
                List<string> list = new List<string>();
                lock (botCommandThreads)
                {
                    int num = botCommandThreads.Count;
                    foreach (Thread t in botCommandThreads)
                    {
                        n++;
                        num--;
                        //System.Threading.ThreadStateException: Thread is dead; state cannot be accessed.
                        //  at System.Threading.Thread.IsBackgroundNative()
                        if (!t.IsAlive)
                        {
                            list.Add(string.Format("{0}: {1} IsAlive={2}", num, t.Name, t.IsAlive));
                        }
                        else
                        {
                            list.Insert(0, string.Format("{0}: {1} IsAlive={2}", num, t.Name, t.IsAlive));
                        }
                    }
                }
                foreach (var s in list)
                {
                    WriteLine(s);
                }
                return Success("Total threads: " + n);
            }
            String cmd = String.Join(" ", args);
            ThreadStart thread = () =>
                                           {
                                               try
                                               {
                                                   try
                                                   {
                                                       var result = Client.ExecuteCommand(cmd, WriteLine);
                                                       if (result == null)
                                                       {
                                                           WriteLine("No command found! \"" + cmd + "\"");
                                                       }
                                                       else
                                                       {
                                                           WriteLine(result.ToString());
                                                       }
                                                   }
                                                   catch (Exception e)
                                                   {
                                                       WriteLine("Problem with " + cmd + " " + e);
                                                   }
                                               }
                                               finally
                                               {
                                                   try
                                                   {
                                                           TheBotClient.RemoveThread(Thread.CurrentThread);
                                                   }
                                                   catch (OutOfMemoryException){}catch (StackOverflowException){}catch (Exception){}
                                                   WriteLine("done with " + cmd);
                                               }
                                           };
            String threadName = "ThreadCommnand for " + cmd;
            TheBotClient.InvokeThread(threadName, thread);
            return Success(threadName);
        }
    }
}
