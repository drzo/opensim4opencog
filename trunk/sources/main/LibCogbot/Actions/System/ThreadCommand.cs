using System;
using System.Collections.Generic;
using System.Runtime.InteropServices;
using System.Threading;
using MushDLR223.Utilities;
using OpenMetaverse;
using OpenMetaverse.Packets;
#if USE_SAFETHREADS
using Thread = MushDLR223.Utilities.SafeThread;
#endif

using MushDLR223.ScriptEngines;

namespace Cogbot.Actions
{
    public class ThreadCommand : Command, BotSystemCommand
    {
        public ThreadCommand(BotClient testClient)
        {
            Name = "thread";
            TheBotClient = testClient;
        }

        override public void MakeInfo()
        {
            Description =
                "Manipulates and Shows the list of threads and task queue statuses. SL/Opensim is a streaming system." +
                " Many things happen asynchronously. Each asynch activity is represented by a 'taskid'. These tasks are" +
                " processed from task queues. This command displays the status of the queues. It is mostly useful for debugging" +
                " cogbot itself, but can also be useful for understanding bot performance.";

            AddExample("thread create anim 30 crouch",
                       "returns immediately, but the bot continues to crouch for 30 seconds (Executes a command in its own thread)");
            AddExample("thread movement moveto FluffyBunny Resident",
                       "returns immediately but enqueues the movement into the movement queue");
            AddExample("thread movement --kill moveto FluffyBunny Resident",
                       "detroys all previous 'movement' and adds the command the movement queue");
            AddVersion(CreateParams(
                           "taskid", typeof (string), "the task queue this thread will use or 'create' or 'list'",
                           Optional("--kill", typeof(bool), "whether to kill or append to previous taskid"),
                           "command", typeof(BotCommand), "The command to execute asynchronously or 'kill'",
                           Optional("--wait", typeof(TimeSpan), "blocks until the thread completes")),
                       Description);
            ResultMap = CreateParams(
                "message", typeof (string), "if the inner command failed, the reason why",
                "threadid", typeof (int), "returns a unique thread Id for killing later on",
                "success", typeof (bool), "true if the inner command succeeded");
            Category = CommandCategory.BotClient;
        }

        public override CmdResult ExecuteRequest(CmdRequest args)
        {
            bool kill = args.IsTrue("--kill");
            TimeSpan wait;
            ManualResetEvent mre = null;
            if (args.TryGetValue("--wait", out wait))
            {
                mre = new ManualResetEvent(false);
            }
            if (args[0] == "list")
            {

                int n = 0;
                var botCommandThreads = Client.GetBotCommandThreads();
                List<string> list = new List<string>();
                //lock (botCommandThreads)
                {
                    int num = botCommandThreads.Count;
                    foreach (Thread t in LockInfo.CopyOf(botCommandThreads))
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
                            if (kill)
                            {
                                t.Abort();
                            }
                        }
                        if (kill)
                        {
                            Client.RemoveThread(t);
                        }
                    }
                }
                foreach (var s in list)
                {
                    WriteLine(s);
                }
                int found = 0;
                lock (TaskQueueHandler.TaskQueueHandlers)
                {
                    var atq = TheBotClient != null
                                  ? TheBotClient.AllTaskQueues()
                                  : ClientManager.SingleInstance.AllTaskQueues();
                    foreach (var queueHandler in atq)
                    {
                        found++;
                        if (queueHandler.Busy)
                            WriteLine(queueHandler.ToDebugString(true));
                        else
                        {
                            list.Add(queueHandler.ToDebugString(true));
                        }
                    }
                }
                foreach (var s in list)
                {
                    WriteLine(s);
                }
                return Success("TaskQueueHandlers: " + found + ", threads: " + n);
            }

            bool createFresh;
            string id = GetTaskID(args, out createFresh);
            if (kill && createFresh)
            {
                return Failure("Cannot create and kill in the same operation");
            }
            String cmd;
            args.TryGetValue("command", out cmd);
            ThreadStart task = () =>
                                   {
                                       var result = Client.ExecuteCommand(cmd, fromAgentID, WriteLine);
                                       if (result == null)
                                       {
                                           WriteLine("No command found! \"" + cmd + "\"");
                                       }
                                       else
                                       {
                                           WriteLine(result.ToString());
                                       }
                                   };
            string message = TheBotClient.CreateTask(id, task, cmd, createFresh, kill, mre, WriteLine);
            Results.Add("taskid", id);
            if (mre != null)
            {
                if (!mre.WaitOne(wait)) return Failure("Timeout: " + message);
            }
            return Success(message);
        }

    }
}
