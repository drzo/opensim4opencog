using System;
using System.Collections.Generic;
using System.Threading;
using cogbot.Utilities;
using OpenMetaverse;

namespace cogbot.Actions.System
{

    public class TasksCommand : Command, BotSystemCommand
    {
        public TasksCommand(BotClient testClient)
        {
            Description = "Shows the list of task queue statuses.  Usage: tasks";
            Category = CommandCategory.BotClient;
        }

        public override CmdResult Execute(string[] args, UUID fromAgentID1, OutputDelegate WriteLine)
        {
            int n = 0;
            var botCommandThreads = Client.GetBotCommandThreads();
            List<string> list = new List<string>();
            lock (botCommandThreads)
                foreach (Thread t in botCommandThreads)
                {
                    n++;
                    //System.Threading.ThreadStateException: Thread is dead; state cannot be accessed.
                    //  at System.Threading.Thread.IsBackgroundNative()
                    if (!t.IsAlive)
                    {
                        list.Add("DEAD TASK!! " + t.Name);
                    }
                    else
                    {
                        list.Insert(0, string.Format("{0}: {1} IsAlive={2}", n, t.Name, t.IsAlive));
                    }
                }
            int found = 0;
            lock (TaskQueueHandler.TaskQueueHandlers)
            {
                foreach (var queueHandler in TaskQueueHandler.TaskQueueHandlers)
                {
                    found++;
                    if (queueHandler.Busy)
                        list.Insert(0, queueHandler.ToString());
                    else
                    {
                        list.Add(queueHandler.ToString());
                    }

                }
            }
            foreach (var s in list)
            {
                WriteLine(s);
            }
            return Success("TaskQueueHandlers: " + found + ", threads: " + n);
        }
    }
}
