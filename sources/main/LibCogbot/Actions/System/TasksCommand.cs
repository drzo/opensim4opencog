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
            Description = "Shows the list of task queue statuses.  Usage: tasks";
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
