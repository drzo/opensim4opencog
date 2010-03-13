using System;
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
            Category = CommandCategory.TestClient;
        }

        public override CmdResult Execute(string[] args, UUID fromAgentID1, OutputDelegate WriteLine)
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
            return Success("TaskQueueHandlers: " + found + ", threads: " + n);
        }
    }
}
