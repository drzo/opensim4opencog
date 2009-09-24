using System;
using System.Threading;
using cogbot.Utilities;
using OpenMetaverse;

namespace cogbot.Actions
{

    public class TasksCommand : Command
    {
        public TasksCommand(BotClient testClient)
        {
            Description = "Shows the list of task queue statuses.  Usage: tasks";
            Category = CommandCategory.TestClient;
        }

        public override string Execute(string[] args, UUID fromAgentID1, OutputDelegate WriteLine)
        {
            int n = 0;
            lock (Client.botCommandThreads) foreach (Thread t in Client.botCommandThreads)
                {
                    n++;
                    //System.Threading.ThreadStateException: Thread is dead; state cannot be accessed.
                    //  at System.Threading.Thread.IsBackgroundNative()
                    WriteLine("{0}: {1} IsAlive={2}", n, t.Name, t.IsAlive);
                }
            int found = 0;
            lock (TaskQueueHandler.TaskQueueHandlers)
                foreach (var queueHandler in TaskQueueHandler.TaskQueueHandlers)
                {
                    found++;
                    WriteLine(queueHandler.ToString());
                }
            return "TaskQueueHandlers: " + found + ", threads: " + n;
        }
    }
}
