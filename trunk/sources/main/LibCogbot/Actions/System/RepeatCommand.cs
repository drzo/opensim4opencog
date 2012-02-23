using System;
using System.Collections.Generic;
using System.Runtime.InteropServices;
using System.Threading;
using OpenMetaverse;
using OpenMetaverse.Packets;

using MushDLR223.ScriptEngines;

namespace cogbot.Actions
{
    public class RepeatCommand : Command, BotSystemCommand
    {
        public RepeatCommand(BotClient testClient)
        {
            Name = "thread";
            Description = "Repeats a command in its own thread. Usage: thread 30 jump";
            Category = CommandCategory.BotClient;
        }

        public override CmdResult Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
            //BotClient Client = TheBotClient;
            if (args.Length < 2)
            {
                return ShowUsage();// " ";
            }
            int secondsOfSleep;
            if (!int.TryParse(args[0], out secondsOfSleep))
            {
                return ShowUsage();
            }
            // remove the time
            args = Parser.SplitOff(args, 1);
            String cmd = String.Join(" ", args);
            ThreadStart thread = () =>
                                           {
                                               try
                                               {
                                                   while (true)
                                                   {
                                                       try
                                                       {
                                                           var result = Client.ExecuteCommand(cmd, fromAgentID,
                                                                                              WriteLine);
                                                           if (result == null)
                                                           {
                                                               WriteLine("No command found! \"" + cmd + "\"");
                                                               return;
                                                           }
                                                           else
                                                           {
                                                               WriteLine(result.ToString());
                                                           }
                                                       }
                                                       catch (ThreadAbortException e)
                                                       {
                                                           WriteLine("Aborting " + cmd);
                                                       }
                                                       catch (Exception e)
                                                       {
                                                           WriteLine("Problem with " + cmd + " " + e);
                                                           return;
                                                       }
                                                       Thread.Sleep(secondsOfSleep*1000);
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
            String threadName = "Repeating " + cmd;
            TheBotClient.InvokeThread(threadName, thread);
            return Success(threadName);
        }
    }
}
