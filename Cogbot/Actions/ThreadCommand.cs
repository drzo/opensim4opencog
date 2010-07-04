using System;
using System.Collections.Generic;
using System.Runtime.InteropServices;
using System.Threading;
using OpenMetaverse;
using OpenMetaverse.Packets;

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
                lock (botCommandThreads)
                {
                    foreach (Thread t in botCommandThreads)
                    {
                        n++;
                        //System.Threading.ThreadStateException: Thread is dead; state cannot be accessed.
                        //  at System.Threading.Thread.IsBackgroundNative()
                        WriteLine("" + n + ": " + t.Name + " alive=" + t.IsAlive);
                    }
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
                                                       WriteLine(Client.ExecuteCommand(cmd, WriteLine).ToString());
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
            TheBotClient.Invoke(threadName,thread);
            return Success(threadName);
        }
    }
}
