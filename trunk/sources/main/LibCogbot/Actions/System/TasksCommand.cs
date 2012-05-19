using System.Collections.Generic;
using System.Threading;
using cogbot.Utilities;
using MushDLR223.Utilities;
using OpenMetaverse;

using MushDLR223.ScriptEngines;

namespace cogbot.Actions.System
{

    public class TasksCommand : Command, BotSystemCommand
    {
        public TasksCommand(BotClient testClient)
        {
            Description = "Shows the list of task queue statuses. SL/Opensim is a streaming system." +
" Many things happen asynchronously. Each asynch activity is represented by a 'task'. These tasks are" +
" processed from task queues. This command displays the status of the queues. It is mostly useful for debugging" +
" cogbot itself, but can also be useful for understanding bot performance.";
            Usage = Htmlize.Usage("tasks", "show the task queue statuses");
            Category = CommandCategory.BotClient;
        }

        public override CmdResult Execute(string[] args, UUID fromAgentID1, OutputDelegate WriteLine)
        {
            int n = 0;
            var botCommandThreads = Client.GetBotCommandThreads();
            List<string> list = new List<string>();
            bool changeDebug = false;
            bool newDebug = false;
            if (args.Length > 0)
            {
                changeDebug = true;
                if (args[0].ToLower().StartsWith("d")) newDebug = true;
                else if (args[0].ToLower().StartsWith("o")) newDebug = false;
            }
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
            int found = 0;
            lock (TaskQueueHandler.TaskQueueHandlers)
            {
                foreach (var queueHandler in TaskQueueHandler.TaskQueueHandlers)
                {
                    found++;
                    if (queueHandler.Busy)
                        WriteLine(queueHandler.ToDebugString(true));
                    else
                    {
                        list.Add(queueHandler.ToDebugString(true));
                    }
                    if (changeDebug)
                    {
                        TaskQueueHandler.TurnOffDebugMessages = false;
                        queueHandler.DebugQueue = newDebug;
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
