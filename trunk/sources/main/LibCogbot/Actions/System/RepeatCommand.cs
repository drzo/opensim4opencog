using System;
using System.Collections.Generic;
using System.Runtime.InteropServices;
using System.Threading;
using OpenMetaverse;
using OpenMetaverse.Packets;

using MushDLR223.ScriptEngines;

namespace Cogbot.Actions
{
    public class RepeatCommand : Command, BotSystemCommand
    {
        public RepeatCommand(BotClient testClient)
        {
            Name = "repeat";
            TheBotClient = testClient;            
        }

        override public void MakeInfo()
        {
            Description = "repeats a task every so many seconds";
            AddExample("repeat movement 30 jump", "jump every 30 seconds in the movement thread");
            AddVersion(CreateParams(
                           "taskid", typeof (string), "the task queue this thread will use or 'create' or 'list'",
                           Optional("--kill", typeof (bool), "whether to kill or append to previous taskid"),
                           "seconds", typeof (int), "number of seconds between repeats",
                           Rest("command", typeof (BotCommand), "command to repeat")), Description);

            Category = CommandCategory.BotClient;
        }

        public override CmdResult ExecuteRequest(CmdRequest args)
        {
            bool kill = args.IsTrue("--kill");
            bool createFresh;
            string id = GetTaskID(args, out createFresh);
            if (kill && createFresh)
            {
                return Failure("Cannot create and kill in the same operation");
            }
            int secondsOfSleep;
            if (!args.TryGetValue("seconds", out secondsOfSleep))
            {
                return ShowUsage();
            }            
            // remove the time
            string[] cmdS;
            args.TryGetValue("command", out cmdS);
            string cmd = Parser.Rejoin(cmdS, 0);
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
            string message = TheBotClient.CreateTask(id, thread, threadName, createFresh, kill, null, WriteLine);
            Results.Add("taskid", id);
            return Success(message);
        }
    }
}
