using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Diagnostics;

using System.IO;
using System.Threading;
using Iesi.Collections.Generic;
using LogicalParticleFilter1;
using MushDLR223.Utilities;
using ikvm.lang;

namespace AltAIMLbot
{
    //see http://mjhutchinson.com/journal/2010/02/01/iteratorbased_microthreading

    //tasks may move between lists but they may only be in one list at a time
    public class TaskItem
    {
        public readonly IEnumerator<RunStatus > Task;
        public TaskItem Next;
        public Scheduler Scheduler;
        public long Data;
        public string name;

        public TaskItem(IEnumerator<RunStatus> task, Scheduler scheduler)
        {
            this.Task = task;
            this.Scheduler = scheduler;
        }
        public TaskItem(IEnumerator<RunStatus> task, Scheduler scheduler,string myName)
        {
            this.Task = task;
            this.Scheduler = scheduler;
            this.name = myName;
        }

        public bool IsNamed(string n)
        {
            return KeyCase.DefaultFN.SameKey(name, n);
        }
    }

    class TaskList
    {
        public readonly Scheduler Scheduler;

        public TaskItem First { get; private set; }
        public TaskItem Last { get; private set; }

        public TaskList(Scheduler scheduler)
        {
            this.Scheduler = scheduler;
        }

        public void Append(TaskItem task)
        {
            Debug.Assert(task.Next == null);
            if (First == null)
            {
                Debug.Assert(Last == null);
                First = Last = task;
            }
            else
            {
                Debug.Assert(Last.Next == null);
                Last.Next = task;
                Last = task;
            }
        }
        public int Count
        {
            get
            {
                int count=0;
                var en = GetEnumerator();
                while (en.MoveNext())
                {
                    count++;
                }
                return count;
            }
        }

        public void Remove(TaskItem task, TaskItem previous)
        {
            if (previous == null)
            {
                Debug.Assert(task == First);
                First = task.Next;
            }
            else
            {
                Debug.Assert(previous.Next == task);
                previous.Next = task.Next;
            }

            if (task.Next == null)
            {
                Debug.Assert(Last == task);
                Last = previous;
            }
            task.Next = null;
        }

        public TaskEnumerator GetEnumerator()
        {
            return new TaskEnumerator(this);
        }

        public sealed class TaskEnumerator
        {
            TaskList list;
            TaskItem current, previous;

            public TaskEnumerator(TaskList list)
            {
                this.list = list;
                previous = current = null;
            }

            public TaskItem Current { get { return current; } }

            public bool MoveNext()
            {
                TaskItem next;
                if (current == null)
                {
                    if (previous == null)
                        next = list.First;
                    else
                        next = previous.Next;
                }
                else
                {
                    next = current.Next;
                }

                if (next != null)
                {
                    if (current != null)
                        previous = Current;
                    current = next;
                    return true;
                }
                return false;
            }

            public void MoveCurrentToList(TaskList otherList)
            {
                otherList.Append(RemoveCurrent());
            }

            public TaskItem RemoveCurrent()
            {
                Debug.Assert(current != null);
                TaskItem ret = current;
                list.Remove(current, previous);
                current = null;
                return ret;
            }
        }
    }

    public sealed class Scheduler
    {
        TaskList active, sleeping;
        Servitor servitor;
        public bool singular = true; // only one process
        BehaviorSet myBehaviors
        {
            get { return servitor.curBot.myBehaviors; }
        }

        public Scheduler(Servitor myServitor)
        {
            servitor = myServitor;
            active = new TaskList(this);
            sleeping = new TaskList(this);
        }

        internal void ActivateBehaviorTask(string name, bool waitUntilComplete)
        {
            if (!waitUntilComplete)
            {
                ActivateBehaviorTask(name);
                return;
            }
            TaskItem task = FindTask(name);
            string status = taskStatus(name);
                var are = new ManualResetEvent(false);
            Func<RunStatus> Unblock = () =>
                                          {
                                              are.Set();
                                              return RunStatus.Success;
                                          };
            if (task == null || status == null || status == "unknown")
            {
                string tempEvent = "waitUntil" + name + "Complete";
                myBehaviors.CreateEvent(tempEvent, Unblock);
                if (status == "sleeping")
                {
                    AwakenTask(name);
                    return;
                }
                // start up a new one
                if (!myBehaviors.definedBehavior(name))
                {
                    return;
                }
                IEnumerator<RunStatus> iterator = myBehaviors.getBehaviorEnumerator(name);
                iterator = new ListOfIters(new List<IEnumerator<RunStatus>>
                                               {
                                                   iterator,
                                                   new OneRunStatus()
                                                       {
                                                           Once = Unblock
                                                       }
                                               });


                if ((singular == false) || (active.Count == 0))
                {
                    active.Append(new TaskItem(iterator, this, name));
                }
                else
                {
                    //Put in background if we are single minded
                    sleeping.Append(new TaskItem(iterator, this, name));

                }

                are.WaitOne();
                return;
            }
        }

        public void ActivateBehaviorTask(string name)
        {
            // if its already running or sleeping 
            string status = taskStatus(name);
            if (status == "running") return;
            if (status == "active") return;
            if (status == "sleeping")
            {
                AwakenTask(name);
                return;
            }
            // start up a new one
            if (!myBehaviors.definedBehavior(name))
            {
                return;
            }
            IEnumerator<RunStatus> iterator = myBehaviors.getBehaviorEnumerator(name);
            
            if ((singular ==false) || (active.Count ==0))
            {
                active.Append(new TaskItem(iterator, this, name));
            }
            else
            {
                //Put in background if we are single minded
                sleeping.Append(new TaskItem(iterator, this, name));

            }

        }

        public void EnqueueEvent(string evnt)
        {
            string evntBehavior = myBehaviors.getEventHandler(evnt);
            if (string.IsNullOrEmpty(evntBehavior))
            {
                return;
            }
            ActivateBehaviorTask(evntBehavior);
        }

        public void RemoveBehaviorTask(string name)
        {
            var en = sleeping.GetEnumerator();
            while (en.MoveNext())
            {
                if (en.Current.IsNamed(name))
                {
                    en.RemoveCurrent();
                }
            }
            en = active.GetEnumerator();
            while (en.MoveNext())
            {
                if (en.Current.IsNamed(name))
                {
                    en.RemoveCurrent();
                }
            }
        }
        public void SleepBehaviorTask(string name)
        {
            var en = active.GetEnumerator();
            while (en.MoveNext())
            {
                if (en.Current.IsNamed(name))
                {
                    en.MoveCurrentToList(sleeping);
                }
            }
        }
        public void SleepBehaviorTask(string name, long msec)
        {
            long nowTicks = DateTime.Now.Ticks;
            long timeout = nowTicks + (msec * 10000);

            var en = active.GetEnumerator();
            while (en.MoveNext())
            {
                if (en.Current.IsNamed(name))
                {
                    en.Current.Data = timeout;
                    en.MoveCurrentToList(sleeping);
                }
            }
        }
        public void SleepAllTasks()
        {
            var en = active.GetEnumerator();
            while (en.MoveNext())
            {
                    en.MoveCurrentToList(sleeping);
            }
        }

        public void SleepAllTasks(long msec)
        {
            long nowTicks = DateTime.Now.Ticks;
            long timeout = nowTicks + (msec * 10000);

            var en = active.GetEnumerator();
            while (en.MoveNext())
            {
                en.Current.Data = timeout;
                en.MoveCurrentToList(sleeping);
            }
            

        }
        public void ReviveAllTasks()
        {
            var en = sleeping.GetEnumerator();
            while (en.MoveNext())
            {
                en.MoveCurrentToList(active);
            }
        }

        public string idStatus(string nodeID)
        {
            RunStatus status;
            var rs = myBehaviors.runState;
            lock (rs)
            {
                if (rs.TryGetValue(nodeID, out status))
                {
                    return status.ToString();
                }
            }
            return "non";
         }

        public string taskStatus(string nodeID)
        {
            string report = "unknown";
            var en = sleeping.GetEnumerator();
            while (en.MoveNext())
            {
                if (en.Current.IsNamed(nodeID))
                {
                    report = "sleeping";
                    return report;
                }
            }
            en = active.GetEnumerator();
            while (en.MoveNext())
            {
                if (en.Current.IsNamed(nodeID))
                {
                    report = "active";
                    return report;
                }
            }
            return report;
        }

        public void AddTask(IEnumerator<RunStatus> task)
        {
            active.Append(new TaskItem(task, this));
        }

        public void AwakenTask(string taskName)
        {
            var en = sleeping.GetEnumerator();
            while (en.MoveNext())
                if (en.Current.IsNamed(taskName))
                    en.MoveCurrentToList(active);
        }

        public TaskItem FindTask(string taskName)
        {
            TaskList.TaskEnumerator en;
            en = active.GetEnumerator();
            while (en.MoveNext())
                if (en.Current.IsNamed(taskName)) return en.Current;
            en = sleeping.GetEnumerator();
            while (en.MoveNext())
                if (en.Current.IsNamed(taskName)) return en.Current;
            return null;
        }

        public void Run()
        {
            //cache this, it's expensive to access DateTime.Now
            int sleepCount = sleeping.Count;
            int activeCount = active.Count;
            long nowTicks = DateTime.Now.Ticks;

            //move woken tasks back into the active list
            var en = sleeping.GetEnumerator();
            if ((singular == false) || (activeCount == 0))
            {
                while (en.MoveNext())
                    if (en.Current.Data < nowTicks)
                        en.MoveCurrentToList(active);
            }
            //run all the active tasks
            en = active.GetEnumerator();
            while (en.MoveNext())
            {
                //run each task's enumerator for one yield iteration
                IEnumerator<RunStatus>  t = en.Current.Task;
                if (!t.MoveNext())
                {
                    //it finished, so remove it
                    en.RemoveCurrent();
                    continue;
                }

                //check the current state
                object state = t.Current;
                if (state == null)
                {
                    //it's just cooperatively yielding, state unchanged
                    continue;
                }
                else if (state is RunStatus)
                {
                    if (t.Current == RunStatus.Running)
                    {
                        //it's just cooperatively yielding, state unchanged
                        continue;
                    }
                    if (t.Current == RunStatus.Failure )
                    {
                        //We're done, just not a positive outcome
                        en.RemoveCurrent();
                        continue;
                    }
                    if (t.Current == RunStatus.Success )
                    {
                        //We're done, and success!
                        en.RemoveCurrent();
                        continue;
                    }


                }
                else if (state is TimeSpan)
                {
                    //it wants to sleep, move to the sleeping list. we use the Data property for the wakeup time
                    en.Current.Data = nowTicks + ((TimeSpan)state).Ticks;
                    en.MoveCurrentToList(sleeping);
                }
                else if (state is IEnumerable<RunStatus>)
                {
                    throw new NotImplementedException("Nested tasks are not supported yet");
                }
                else if (state is Signal)
                {
                    TaskItem task = en.RemoveCurrent();
                    task.Data = 0;
                    ((Signal)state).Add(task);
                }
                else if (state is ICollection<Signal>)
                {
                    TaskItem task = en.RemoveCurrent();
                    task.Data = 0;
                    foreach (Signal s in ((ICollection<Signal>)state))
                        s.Add(task);
                }

                else
                {
                    throw new InvalidOperationException("Unknown task state returned:" + state.GetType().FullName);
                }
            }
        }

        internal void AddToActive(TaskItem task)
        {
            active.Append(task);
        }

        public void performAction(TextWriter writer, string action, string query, string behaviorName)
        {
            var multiBehaviorName = GatherTaskNames(behaviorName);
            if (multiBehaviorName != null)
            {
                if (multiBehaviorName.Count == 0)
                {
                    writer.WriteLine("Zero tasks or behaviors from :" + behaviorName);
                    return;
                }
                foreach (string behavorT in multiBehaviorName)
                {
                    performAction(writer, action, query, behavorT);
                }
                return;
            }
            if (action != null && action.Contains(","))
            {
                foreach (var a in action.Split(' ', ','))
                {
                    if (string.IsNullOrEmpty(a)) continue;
                    performAction(writer, a, query, behaviorName);
                }
            }
            string ids = "";
            string tsk = "";
            TaskList.TaskEnumerator  en = null;
            switch (action)
            {
                case "info":
                    ids = idStatus(behaviorName);
                    tsk = taskStatus(behaviorName);
                    writer.WriteLine("<status id=\"{0}\" idStatus=\"{1}\" taskStatus=\"{2}\" />", behaviorName, ids, tsk);
                    bool v01 = myBehaviors.visibleBehavior(behaviorName);
                    bool v03 = myBehaviors.definedBehavior(behaviorName);
                    string eh = myBehaviors.getEventHandler(behaviorName);
                    if (!string.IsNullOrEmpty(eh) && !KeyCase.DefaultFN.SameKey(eh, behaviorName))
                    {
                        writer.WriteLine("<eventHandler name=\"{0}\" value=\"{1}\">", behaviorName, eh);
                        performAction(writer, action, query, eh);
                        writer.WriteLine("</eventHandler>");
                    }
                    if (v03)
                    {
                        var treeByTreeName = myBehaviors.GetTreeByName(behaviorName);
                        writer.WriteLine(treeByTreeName.treeDoc.OuterXml);
                    }
                    writer.WriteLine("<visible name=\"{0}\" value=\"{1}\"/>", behaviorName, v01);
                    writer.WriteLine("<defined name=\"{0}\" value=\"{1}\"/>", behaviorName, v03);
                    break;

                case "source":
                    string behaviorFile = myBehaviors.behaviorDiskName(behaviorName);
                    string fileReadAllText = "";
                    lock (BehaviorTree.FileLock)
                    {
                        if (File.Exists(behaviorFile))
                        {
                            fileReadAllText = File.ReadAllText(behaviorFile);
                        }
                    }
                    writer.WriteLine("{0}", fileReadAllText);
                    break;

                case "activate":
                    ActivateBehaviorTask(behaviorName);
                     ids = idStatus(behaviorName);
                     tsk = taskStatus(behaviorName);
                     writer.WriteLine("<status id=\"{0}\" idStatus=\"{1}\" taskStatus=\"{2}\" />", behaviorName, ids, tsk);
                    break;
                case "deactivate":
                    RemoveBehaviorTask(behaviorName);
                     ids = idStatus(behaviorName);
                     tsk = taskStatus(behaviorName);
                     writer.WriteLine("<status id=\"{0}\" idStatus=\"{1}\" taskStatus=\"{2}\" />", behaviorName, ids, tsk);
                    break;
                case "sleep":
                    SleepBehaviorTask(behaviorName);
                    ids = idStatus(behaviorName);
                    tsk = taskStatus(behaviorName);
                    writer.WriteLine("<status id=\"{0}\" idStatus=\"{1}\" taskStatus=\"{2}\" />", behaviorName, ids, tsk);
                    break;
                case "sleepall":
                    SleepAllTasks();
                    ids = idStatus(behaviorName);
                    tsk = taskStatus(behaviorName);
                    writer.WriteLine("<status id=\"{0}\" idStatus=\"{1}\" taskStatus=\"{2}\" />", behaviorName, ids, tsk);
                    break;
                case "reviveall":
                    ReviveAllTasks();
                    ids = idStatus(behaviorName);
                    tsk = taskStatus(behaviorName);
                    writer.WriteLine("<status id=\"{0}\" idStatus=\"{1}\" taskStatus=\"{2}\" />", behaviorName, ids, tsk);
                    break;

                case "status":
                     ids = idStatus(behaviorName);
                     tsk = taskStatus(behaviorName);
                     writer.WriteLine("<status id=\"{0}\" idStatus=\"{1}\" taskStatus=\"{2}\" />", behaviorName, ids, tsk);
                    break;
                case "liststatus":
                    en = sleeping.GetEnumerator();
                    while (en.MoveNext())
                    {
                            writer.WriteLine("<status id=\"{0}\" taskStatus=\"{1}\" />", en.Current.name, "sleeping");
                    }
                    en = active.GetEnumerator();
                    while (en.MoveNext())
                    {
                        writer.WriteLine("<status id=\"{0}\" taskStatus=\"{1}\" />", en.Current.name, "active");
                    }
                    break;
                case "listidstatus":
                    var runState = LockInfo.CopyOf(myBehaviors.runState);
                    foreach (string key in runState.Keys)
                    {
                        string status= runState[key].ToString();
                        writer.WriteLine("<status id=\"{0}\" idStatus=\"{1}\" />", key, status);
                    }

                    break;
                case "stopall":
                    en = sleeping.GetEnumerator();
                    while (en.MoveNext())
                    {
                        writer.WriteLine("<status id=\"{0}\" taskStatus=\"{1}\" />", en.Current.name, "terminating");
                        en.RemoveCurrent();
                    }
                    en = active.GetEnumerator();
                    while (en.MoveNext())
                    {
                        writer.WriteLine("<status id=\"{0}\" taskStatus=\"{1}\" />", en.Current.name, "terminating");
                        en.RemoveCurrent();
                    }
                    break;

                case "block":
                    myBehaviors.makeInvisible(query);
                    myBehaviors.makeInvisible(behaviorName);
                    RemoveBehaviorTask(behaviorName);
                    ids = idStatus(behaviorName);
                    tsk = taskStatus(behaviorName);
                    writer.WriteLine("<status id=\"{0}\" idStatus=\"{1}\" taskStatus=\"{2}\" />", behaviorName, ids, tsk);
                    break;

                case "unblock":
                    myBehaviors.makeVisible(query);
                    ids = idStatus(behaviorName);
                    tsk = taskStatus(behaviorName);
                    writer.WriteLine("<status id=\"{0}\" idStatus=\"{1}\" taskStatus=\"{2}\" />", behaviorName, ids, tsk);
                    break;

                case "unblockall":
                    myBehaviors.invisiblePatterns .Clear();
                    ids = idStatus(behaviorName);
                    tsk = taskStatus(behaviorName);
                    writer.WriteLine("<status id=\"{0}\" idStatus=\"{1}\" taskStatus=\"{2}\" />", behaviorName, ids, tsk);
                    break;

                case "listblocks":
                    foreach (string p in myBehaviors.invisiblePatterns.Keys)
                    {
                        writer.WriteLine("<blockBehaviorPattern pattern=\"{0}\" />", p);
                    }
                    break;

                case "checkblock":
                    bool v1 = myBehaviors.visibleBehavior(behaviorName);
                    bool v2 = myBehaviors.visibleBehavior(query );
                    bool v3 = myBehaviors.definedBehavior(behaviorName);
                    bool v4 = myBehaviors.definedBehavior(query);
                    writer.WriteLine("<visible name=\"{0}\" value=\"{1}\"/>", behaviorName, v1);
                    writer.WriteLine("<visible name=\"{0}\" value=\"{1}\"/>", query, v2);
                    writer.WriteLine("<defined name=\"{0}\" value=\"{1}\"/>", behaviorName, v3);
                    writer.WriteLine("<defined name=\"{0}\" value=\"{1}\"/>", query, v4);
                    break;

                case "blockcron":
                    servitor.curBot.blockCron = true;
                    break;

                case "unblockcron":
                    servitor.curBot.blockCron = false;
                    break;


                 default  :
                    writer.WriteLine("<error action=\"{0}\" query=\"{1}\" behaviorName=\"{2}\" />", action, query, behaviorName);
 
                break;
            }
            writer.WriteLine("<fin/>");
            writer.Close();
        }

        public HashSet<string> GatherTaskNames(string behaviorName)
        {
            if (string.IsNullOrEmpty(behaviorName)) return null;
            var gatherNames = new HashSet<string>();
            if (behaviorName.Contains(","))
            {
                foreach (var name in behaviorName.Split(',', ' '))
                {
                    if (string.IsNullOrEmpty(name)) continue;
                    gatherNames.Add(name);
                }
                return gatherNames;
            }
            if(!behaviorName.Contains("*")) return null;
            TaskList.TaskEnumerator en;
            behaviorName = behaviorName.ToUpper();
            bool wasSpeced = false;
            if (behaviorName.Contains("*VISIBLE*"))
            {
                wasSpeced = true;
                foreach (var behaveT in myBehaviors.GetBTKeyNames())
                {
                    gatherNames.Add(behaveT);
                }
                foreach (var behaveT in myBehaviors.invisiblePatterns.Keys)
                {
                    gatherNames.Remove(behaveT);
                }
            }
            if (behaviorName.Contains("*DEFINED*"))
            {
                wasSpeced = true;
                foreach (var behaveT in myBehaviors.GetBTKeyNames())
                {
                    gatherNames.Add(behaveT);
                }
            }
            if (behaviorName.Contains("*ALL*"))
            {
                string[] fileList = Directory.GetFiles(myBehaviors.persistantDirectory);
                wasSpeced = true;
                foreach (string f in fileList)
                {
                    string behaveT = Path.GetFileNameWithoutExtension(Path.GetFileName(f));
                    gatherNames.Add(behaveT);
                }
            }
            if (behaviorName.Contains("*INVISIBLE*"))
            {
                wasSpeced = true;
                foreach (var behaveT in myBehaviors.invisiblePatterns.Keys)
                {
                    gatherNames.Add(behaveT);
                }
            }
            if (behaviorName.Contains("*TASKS*"))
            {
                wasSpeced = true;
                en = sleeping.GetEnumerator();
                while (en.MoveNext())
                {
                    gatherNames.Add(en.Current.name);
                }
                en = sleeping.GetEnumerator();
                while (en.MoveNext())
                {
                    gatherNames.Add(en.Current.name);
                }
            }
            if (behaviorName == "*ACTIVE*")
            {
                wasSpeced = true;
                en = active.GetEnumerator();
                while (en.MoveNext())
                {
                    gatherNames.Add(en.Current.name);
                }
            }
            if (behaviorName == "*ASLEEP*")
            {
                wasSpeced = true;
                en = sleeping.GetEnumerator();
                while (en.MoveNext())
                {
                    gatherNames.Add(en.Current.name);
                }
            }
            if (!wasSpeced) return null;
            return gatherNames;
        }
    }

    public class Signal
    {
        static int nextId = int.MinValue;

        int id = nextId++;
        List<TaskItem> tasks = new List<TaskItem>();
        bool isSet = true;

        public void Set()
        {
            if (isSet)
                return;
            isSet = true;
            //decrement the wait count of all tasks waiting for thsi signal
            foreach (TaskItem task in tasks)
                if (--task.Data == 0)
                    //if the wait count is zero, the task isn't waiting for any more signals, so re-schedule it
                    task.Scheduler.AddToActive(task);
            tasks.Clear();
        }

        internal void Add(TaskItem task)
        {
            //signal only becomes unset when it has tasks
            if (isSet)
                isSet = false;
            //the signal keeps a list of tasks that are waiting for it
            tasks.Add(task);
            //use the task's data for tracking the number of signals it's still waiting for
            task.Data++;
        }
    }



}
