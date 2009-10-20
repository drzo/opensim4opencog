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
            Category = CommandCategory.Other;
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
                lock (Client.botCommandThreads)
                {
                    foreach (Thread t in Client.botCommandThreads)
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
            Thread thread = new Thread(() =>
                                           {
                                               try
                                               {
                                                   try
                                                   {
                                                       WriteLine(Client.ExecuteCommand(cmd, WriteLine));
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
                                                       lock (Client.botCommandThreads)
                                                           TheBotClient.botCommandThreads.Remove(Thread.CurrentThread);
                                                   }
                                                   catch (OutOfMemoryException){}catch (StackOverflowException){}catch (Exception){}
                                                   WriteLine("done with " + cmd);
                                               }
                                           });
            thread.Name = "ThreadCommnand for " + cmd;
            lock (Client.botCommandThreads) Client.botCommandThreads.Add(thread);
            thread.Start();
            return Success(thread.Name);
        }
    }
}
