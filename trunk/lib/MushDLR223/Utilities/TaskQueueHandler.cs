using System;
using System.Collections.Generic;
using System.Threading;
using MushDLR223.ScriptEngines;
using TASK = System.Threading.ThreadStart;

namespace MushDLR223.Utilities
{
    public class TaskQueueHandler : IDisposable
    {
        protected ThreadControl LocalThreadControl = new ThreadControl(new ManualResetEvent(false));

        List<ThreadStart> OnFinnaly = new List<ThreadStart>();
        // ping wiat time should be less than 4 seconds when going in
        public static readonly TimeSpan MAX_PING_WAIT = TimeSpan.FromSeconds(4);
        public static readonly TimeSpan PING_INTERVAL = TimeSpan.FromSeconds(10); // 30 seconds
        public static readonly HashSet<TaskQueueHandler> TaskQueueHandlers = new HashSet<TaskQueueHandler>();
        private readonly LinkedList<TASK> EventQueue = new LinkedList<TASK>();

        private readonly object EventQueueLock = new object();
        private readonly object OneTaskAtATimeLock = new object();
        private readonly object TaskThreadChangeLock = new object();
        private readonly object PingWaitingLock = new object();
        private readonly object DebugStringLock = new object();

        private Thread PingerThread;
        private Thread TaskThread;
        private Thread StackerThread;

        private readonly List<Thread> InteruptionList = new List<Thread>();

        //public delegate ThreadStart NameThreadStart(string named, ThreadStart action);

        private readonly string Name;
        private readonly TASK NOP = default(TASK);

        private readonly AutoResetEvent WaitingOn = new AutoResetEvent(false);
        private readonly AutoResetEvent IsCurrentTaskStarted = new AutoResetEvent(false);
        

        private bool _killTasksOverTimeLimit;
        private bool _noQueue;
        public bool Busy;
        public bool abortable;

        private DateTime BusyEnd;
        private DateTime BusyStart = DateTime.UtcNow;
        private DateTime PingStart;


        private TimeSpan OperationKillTimeout;
        private TimeSpan PauseBetweenOperations;
        private TimeSpan LastOperationTimespan;
        private TimeSpan LastPingLagTime;


        private ulong CompletedSinceLastPing = 0;
        public OutputDelegate debugOutput = TextFilter.DEVNULL;

        bool aborted = false;
        private bool debugRequested = false;
        private ulong GoodPings = 0;
        public bool IsDisposing = false;
        private ulong LatePings = 0;
        private bool problems = false;
        public bool SimplyLoopNoWait = false;
        private ulong StartedSinceLastPing = 0;
        private ulong todo;
        private ulong TotalComplete = 0;
        private ulong TotalFailures = 0;
        private ulong TotalStarted = 0;
        private bool WaitingOnPing = false;
        public string WaitingString = "";
        public string LastDebugMessage = "";
        const string INFO = "$INFO$";

        public TaskQueueHandler(string str, TimeSpan msWaitBetween)
            : this(str, msWaitBetween, true)
        {
        }

        public TaskQueueHandler(string str, int msWaitBetween)
            : this(str, TimeSpan.FromMilliseconds(msWaitBetween), true)
        {
        }

        public TaskQueueHandler(string str, TimeSpan msWaitBetween, bool autoStart)
            : this(str, msWaitBetween, msWaitBetween, autoStart)
        {
        }

        public TaskQueueHandler(string str, TimeSpan msWaitBetween, TimeSpan maxPerOperation, bool autoStart)
        {
            lock (TaskQueueHandlers)
            {
                BusyEnd = BusyStart;
                Name = str;
                TaskQueueHandlers.Add(this);


                if (maxPerOperation.TotalMilliseconds < 2) maxPerOperation = TimeSpan.FromMilliseconds(10);
                OperationKillTimeout = maxPerOperation;

                if (msWaitBetween.TotalMilliseconds < 10) msWaitBetween = TimeSpan.FromMilliseconds(10);
                // max ten minutes
                if (msWaitBetween.TotalMinutes > 10) msWaitBetween = TimeSpan.FromMinutes(10);
                PauseBetweenOperations = msWaitBetween;
                PingerThread = new Thread(EventQueue_Ping) { Name = str + " debug", Priority = ThreadPriority.Lowest };
            }
            KillTasksOverTimeLimit = false;
            if (autoStart) Start();
        }

        public bool IsRunning
        {
            get
            {
                if (IsDisposing) return false;
                return StackerThread != null && StackerThread.IsAlive;
            }
        }

        public bool KillTasksOverTimeLimit
        {
            get { lock (EventQueue) return _killTasksOverTimeLimit; }
            set
            {
                lock (EventQueue)
                {
                    if (_killTasksOverTimeLimit == value) return;
                    _noQueue = value;
                } // release waiters
                Set();
            }
        }

        public bool DebugQueue
        {
            get { return problems || debugRequested; }
            set { debugRequested = value; }
        }


        public bool NoQueue
        {
            get { lock (EventQueue) return _noQueue; }
            set
            {
                lock (EventQueue)
                {
                    if (_noQueue == value) return;
                    _noQueue = value;
                } // release waiters
                Set();
            }
        }

        #region IDisposable Members

        public void Dispose()
        {
            if (IsDisposing) return;
            IsDisposing = true;
            lock (TaskQueueHandlers)
                TaskQueueHandlers.Remove(this);
            if (PingerThread != null)
            {
                PingerThread.Abort();
                PingerThread = null;
            }
            if (StackerThread != null && StackerThread.IsAlive)
            {
                StackerThread.Abort();
                StackerThread = null;
            }
            try
            {
                WaitingOn.Set();
                WaitingOn.Close();
            }
            catch (ObjectDisposedException)
            {
            }
        }

        #endregion

        public void Start()
        {
            if (StackerThread == null) StackerThread = new Thread(EventQueue_Handler);
            if (!StackerThread.IsAlive)
            {
                StackerThread.Name = Name + " worker";
                StackerThread.Priority = ThreadPriority.Lowest;
                StackerThread.Start();
            }
            if (PingerThread != null && !PingerThread.IsAlive) PingerThread.Start();
            debugOutput = DLRConsole.DebugWriteLine;
        }

        public override string ToString()
        {
            return INFO;
        }


        public TASK NamedTask(string named, TASK action)
        {
            return () =>
                       {
                           lock (DebugStringLock)
                           {
                               WaitingString += "\n" + named;
                           }
                           action();
                       };
        }

        public string ToDebugString()
        {
            string WaitingS = "Not waiting";
            lock (DebugStringLock)
            {
                if (WaitingString.Length >= 0)
                {
                    WaitingS = "WaitingOn: " + WaitingString;
                }
            }

            string extraMesage =
                Name
                + string.Format(" TODO = {0} ", todo)
                + string.Format(" TOTALS = {0}/{1}/{2} ", TotalComplete, TotalStarted, GetTimeString(LastOperationTimespan))
                +
                string.Format(" PINGED = {0}/{1}/{2} ", CompletedSinceLastPing, StartedSinceLastPing,
                              GetTimeString(LastPingLagTime))
                ;


            if (IsDisposing) WaitingS += " IsDisposing";
            if (!IsRunning) WaitingS += " NOT RUNNING";
            string tdm = String.Format(
                "{0} {1} {2} {3} {4}",
                Busy ? "Busy" : "Idle",
                extraMesage,
                TotalFailures > 0 ? TotalFailures + " failures " : "",
                _noQueue ? "NoQueue" : "",
                WaitingS = " BEFORE ");
            return tdm;
        }

        private void EventQueue_Handler()
        {
            while (!(IsDisposing))
            {
                Busy = false;

                TASK evt;
                lock (EventQueueLock)
                {
                    if (EventQueue.Count > 0)
                    {
                        evt = EventQueue.First.Value;
                        EventQueue.RemoveFirst();
                    }
                    else
                    {
                        evt = NOP;
                    }
                }

                if (evt != null && evt != NOP)
                {
                    DoNow(evt);
                    if (PauseBetweenOperations.TotalMilliseconds > 1) Thread.Sleep(PauseBetweenOperations);
                }
                else
                {
                    lock (EventQueueLock)
                        if (EventQueue.Count > 0)
                        {
                            Busy = false;
                            continue;
                        }
                    //lock (EventQueueLock)
                    //{
                    // Reset();
                    //}
                    if (SimplyLoopNoWait)
                    {
                        Thread.Sleep(PauseBetweenOperations);
                        continue;
                    }
                    WaitOne();
                }
                Busy = false;
            }
        }

        private void WaitOne()
        {
            if (IsDisposing) return;
            WaitingOn.WaitOne(); // Thread.Sleep(100);
        }

        private void WaitOneMaybe()
        {
            bool waitOne = true;

            while (waitOne)
            {
                if (IsDisposing) return;
                while (EventQueue.Count == 0)
                {
                    if (!WaitingOn.WaitOne(10000))
                    {
                        waitOne = true;
                        continue;
                    }
                    else
                    {
                        if (EventQueue.Count > 0)
                        {
                            return;
                        }
                        else
                        {
                            continue;
                        }
                    }
                }
                bool wasBusy;
                lock (EventQueueLock)
                {
                    wasBusy = Busy;
                }
                waitOne = wasBusy;
                if (wasBusy)
                {
                    waitOne = true;
                    continue;
                }
            }
        }

        private void Reset()
        {
            if (IsDisposing) return;
            WaitingOn.Reset();
        }

        private void Set()
        {
            if (IsDisposing) return;
            WaitingOn.Set();
        }

        private void EventQueue_Ping()
        {
            while (!(IsDisposing))
            {
                ulong startedBeforeWait = TotalStarted;
                ulong completeBeforeWait = TotalComplete;
                Thread.Sleep(PING_INTERVAL);
                if (StackerThread != null)
                    if (!StackerThread.IsAlive)
                    {
                        problems = true;
                        WriteLine("ERROR EventQueueHandler DEAD! " + INFO);
                        continue;
                    }
                if (_noQueue) continue;
                lock (PingWaitingLock)
                {
                    todo = (ulong)EventQueue.Count;
                    if (Busy)
                    {
                        TimeSpan t = DateTime.UtcNow - BusyStart;
                        if (t > OperationKillTimeout)
                        {
                            problems = true;
                            WriteLine("BUSY LONGER THAN {0} time = {1} " + INFO,
                                      GetTimeString(OperationKillTimeout),
                                      GetTimeString(t));
                            if (KillTasksOverTimeLimit)
                            {
                                KillCurrentTask();
                            }
                            continue;
                        }
                    }
                    if (WaitingOnPing)
                    {
                        if (completeBeforeWait == TotalComplete)
                        {
                            if (startedBeforeWait == TotalStarted)
                            {
                                if (todo > 0)
                                {
                                    problems = true;
                                    TimeSpan t = DateTime.UtcNow - BusyEnd;
                                    WriteLine("NOTHING DONE IN time = {0} " + INFO, GetTimeString(t));
                                }
                            }
                        }

                        if (Busy /* && (StartedSinceLastPing == 0 && FinishedSinceLastPing == 0)*/)
                        {
                            // ReSharper disable ConditionIsAlwaysTrueOrFalse
                            TimeSpan tt = DateTime.UtcNow - PingStart;
                            if (tt > PING_INTERVAL)
                            {
                                if (DebugQueue)
                                    WriteLine("PINGER WAITING LONGER THAN {0} time = {1} " + INFO,
                                              GetTimeString(PING_INTERVAL),
                                              GetTimeString(tt));
                            }
                            // ReSharper restore ConditionIsAlwaysTrueOrFalse
                        }
                        continue;
                    }

                    if (WaitingOnPing) continue;
                    PingStart = DateTime.UtcNow;
                    WaitingOnPing = true;
                    Enqueue(EventQueue_Pong);
                }
            }
            // ReSharper disable FunctionNeverReturns
        }

        private void EventQueue_Pong()
        {
            lock (PingWaitingLock)
            {
                // todo can this ever happen?
                if (!WaitingOnPing) return;
            }
            LastPingLagTime = DateTime.UtcNow - PingStart;
            TotalStarted--;
            StartedSinceLastPing--;
            if (LastPingLagTime <= MAX_PING_WAIT)
            {
                GoodPings++;
                if (todo > 0)
                    if (DebugQueue)
                        WriteLine("PONG: {0} {1}", GetTimeString(LastPingLagTime), INFO);
                LatePings = 0;
                problems = false;
            }
            else
            {
                LatePings++;
                WriteLine("LATE PONG: {0} {1} {2}", LatePings, GetTimeString(LastPingLagTime), INFO);
                GoodPings = 0;
                problems = true;
            }
            lock (PingWaitingLock)
            {
                WaitingOnPing = false;
                StartedSinceLastPing = 0;
                CompletedSinceLastPing = 0;
            }
        }

        internal static string GetTimeString(TimeSpan lag)
        {
            string lagString;
            if (lag.TotalSeconds < 2)
            {
                lagString = string.Format("{0}ms", lag.Milliseconds);
            }
            else if (lag.TotalMinutes < 2)
            {
                lagString = string.Format("{0}secs", lag.TotalSeconds);
            }
            else if (lag.TotalHours < 2)
            {
                lagString = string.Format("{0}mins", lag.TotalMinutes);
            }
            else
            {
                lagString = "" + lag + "durration";
            }
            return lagString;
        }

        // ReSharper restore FunctionNeverReturns
        private void DoNow(TASK evt)
        {            
            if (IsDisposing) return;
            lock (TaskThreadChangeLock)
                abortable = false;
            Busy = true;            
            TotalStarted++;
            //var IsCurrentTaskComplete = new ManualResetEvent(false);
            try
            {
                todo--;
                BusyStart = DateTime.UtcNow;
                StartedSinceLastPing++;
                if (EventQueue_Pong == evt || (evt.Method != null && evt.Method.DeclaringType == GetType()))
                {
                    evt();
                }
                else
                {
                    Tick(evt);
                    lock (TaskThreadChangeLock)
                        abortable = false;
                    //() => RunWithTimeLimit(evt, "RunWithTimeLimit", OperationKillTimeout));
                }
                CompletedSinceLastPing++;
                TotalComplete++;
                Busy = false;
            }
            catch (ThreadAbortException e)
            {
                abortable = false;
                aborted = true;
                Thread.ResetAbort();
                WriteLine("ThreadAbortException 0");
            }
            catch (Exception e)
            {
                TotalFailures++;
                WriteLine("ERROR! {0} was {1} in {2}", INFO, e, Thread.CurrentThread);
            }
            finally
            {
                BusyEnd = DateTime.UtcNow;
                LastOperationTimespan = BusyEnd.Subtract(BusyStart);
                //TotalStarted++;
                Busy = false;
                lock (OnFinnaly)
                {
                    foreach (ThreadStart onFinal in OnFinnaly)
                    {
                        onFinal();
                    }
                    OnFinnaly.Clear();
                }
            }
        }

        private void Tick(TASK evt)
        {
            lock (OnFinnaly)
            {
                foreach (ThreadStart onFinal in OnFinnaly)
                {
                    onFinal();
                }
                OnFinnaly.Clear();
            }
            try
            {
                lock (TaskThreadChangeLock)
                {
                    lock (OnFinnaly)
                        // this is ur "finnaly" code
                        OnFinnaly.Add(() =>
                                          {

                                              lock (TaskThreadChangeLock)
                                              {
                                                  abortable = false;
                                                  if (aborted)
                                                  {
                                                      WriteLine("ThreadAbortException 2");
                                                      aborted = false;
                                                  }
                                              }
                                          });

                    aborted = false;
                    abortable = true;
                    IsCurrentTaskStarted.Set();
                }

                evt();
                abortable = false;
            }
            catch (ThreadAbortException e)
            {
                abortable = false;
                lock (TaskThreadChangeLock)
                {
                    abortable = false;
                    aborted = true;
                }
                Thread.ResetAbort();
            } finally
            {
                abortable = false;
            }
        }


        private void EnqueueWithTimeLimit(TASK evt, string name, TimeSpan maxTime)
        {
            EnqueueWithTimeLimitInteruptable00(evt, name, maxTime, LocalThreadControl);
        }

        public void EnqueueWithTimeLimit(TASK evt, string name, TimeSpan maxTime, ThreadControl control)
        {
            EnqueueWithTimeLimitInteruptable00(evt, name, maxTime, control);
        }

        private void EnqueueWithTimeLimitInteruptable00(ThreadStart evt, string name, TimeSpan maxTime, ThreadControl control)
        {
            Enqueue(NamedTask(name, () => RunWithTimeLimitInteruptable01(evt, maxTime, control)));
        }

        private void RunWithTimeLimitInteruptable01(TASK evt, TimeSpan maxTime, ThreadControl control)
        {
            bool wasKillTasksOverTimeLimit = KillTasksOverTimeLimit;
            TimeSpan prev = OperationKillTimeout;
            OperationKillTimeout = maxTime;
            BusyStart = DateTime.UtcNow;
            KillTasksOverTimeLimit = true;
            try
            {
                evt();
            }
            finally
            {
                OperationKillTimeout = prev;
                KillTasksOverTimeLimit = wasKillTasksOverTimeLimit;
            }
        }

        private void RunWithTimeLimitInteruptable11(TASK evt, string name, TimeSpan maxTime, ThreadControl control)
        {
            string before = null;
            try
            {
                // Add the worker to the Queue
                lock (DebugStringLock)
                {
                    before = WaitingString;
                    WaitingString = name + "\n" + before;
                }
                DoInteruptably(ref this.TaskThread, evt, maxTime, control);
            }
            finally
            {
                lock (DebugStringLock)
                {
                    WaitingString = before;
                }
            }
        }

        public void DoInteruptably(ref Thread threadVar, TASK evt, TimeSpan timeLimit, ThreadControl control)
        {

            var IsCurrentTaskComplete = control.TaskComplete;
            //
            try
            {

                Thread threadPlace = threadVar;
                IsCurrentTaskComplete = IsCurrentTaskComplete ?? new ManualResetEvent(false);
                //ManualResetEvent IsCurrentTaskComplete = new ManualResetEvent(false);      
                EventWaitHandle IsCurrentTaskEnded = control.TaskEnded = new AutoResetEvent(false);
                EventWaitHandle IsCurrentTaskStarted = control.TaskStart = new AutoResetEvent(false);
                control.ThreadForTask = threadPlace;
                threadPlace = new Thread(OneTask(evt, IsCurrentTaskStarted, IsCurrentTaskComplete, IsCurrentTaskEnded));

                control.ThreadForTask = threadPlace;
                //  lock (OneTaskAtATimeLock)
                {
                    try
                    {
                        lock (TaskThreadChangeLock)
                        {
                            threadVar = threadPlace;
                            threadPlace = threadVar;
                            // wait for started
                            threadPlace.Start();
                        }
#if !NEW_INERUPT

                        if (!threadPlace.Join(timeLimit))
                        {
                            threadPlace.Interrupt();
                        }
#else
                        IsCurrentTaskStarted.WaitOne();


                        //wait for complet4d
                        if (!IsCurrentTaskComplete.WaitOne(timeLimit))
                        {
                            // not completed on time
                            threadPlace.Interrupt();
                        }
                        // lets the interupt and cleanup happen
                        threadPlace.Join();
#endif
                    }
                    catch (ThreadInterruptedException interupted)
                    {
                        WriteLine("INTERPUTED {0} was {1} in {2}", INFO, interupted, threadPlace);
                        control.OnInterrupted(interupted);
                    }
                    catch (ThreadAbortException aborted)
                    {
                        WriteLine("ABORT {0} was {1} in {2}", INFO, aborted, threadPlace);
                        control.OnAborted(aborted);
                    }
                    catch (Exception ex)
                    {
                        WriteLine("ERROR {0} was {1} in {2}", INFO, ex, threadPlace);
                        control.OnException(ex);
                    }
                    finally
                    {
#if !NEW_INERUPT
                        NoExceptions<bool>(IsCurrentTaskStarted.Reset);
#endif
                    }
                }
            }
            catch (Exception interuption)
            {
                WriteLine("DoInteruptably {0} was {1} in {2}", INFO, interuption, Thread.CurrentThread);
            }
            finally
            {
                lock (TaskThreadChangeLock)
                {
                    threadVar = null;
                }
            }
        }

        private ThreadStart OneTask(TASK evt, EventWaitHandle taskStarted, EventWaitHandle taskCompleted,
                                    EventWaitHandle taskEnded)
        {
            return
                delegate
                {
                    try
                    {

                        SetEvent(taskStarted);
                        lock (OneTaskAtATimeLock)
                        {
                            evt();
                        }
                        SetEvent(taskCompleted);
                    }
                    catch (ThreadAbortException abortOrInterupt)
                    {
                        WriteLine("ThreadAbortException {0} was {1} in {2}",
                                  INFO, abortOrInterupt, Thread.CurrentThread);

                        Thread.ResetAbort();
                    }
                    finally
                    {
                        SetEvent(taskEnded);
                    }
                };
        }

        private void SetEvent(EventWaitHandle taskEvent)
        {
            lock (TaskThreadChangeLock) if (taskEvent != null) NoExceptions<bool>(taskEvent.Set);
        }

        public void KillCurrentTask()
        {
            if (Thread.CurrentThread == StackerThread)
            {
                WriteLine("NoSuicide! " + INFO);
                return;
            }
            if (StackerThread != null)
                if (StackerThread.IsAlive)
                {
                    WriteLine("KillCurrentTask! " + INFO);
                    IsCurrentTaskStarted.WaitOne(TimeSpan.FromSeconds(5));
                    lock (TaskThreadChangeLock)
                    {
                        if (abortable)
                        {
                            aborted = true;                                
                            TaskThread.Abort();
                        }
                    }
                    //TaskThread.Suspend();
                    problems = true;
                }
        }


        public void RestartCurrentTask()
        {
            StackerThread.Resume();
        }


        public void WriteLine(string s, params object[] parms)
        {
            s = DLRConsole.SafeFormat("\n[TASK " + Name + "] " + s, parms);
            var str = ToDebugString();
            LastDebugMessage = s;
            if (s.Contains(INFO)) s = s.Replace(INFO, str);

            if (debugOutput != DLRConsole.DebugWriteLine)
            {
                DLRConsole.DebugWriteLine(s);
            }
            if (debugOutput != null) debugOutput(s);
        }

        public void Enqueue(TASK evt)
        {
            if (IsDisposing) return;
            todo++;
            if (_noQueue)
            {
                DoNow(evt);
                return;
            }
            lock (EventQueueLock)
            {
                EventQueue.AddLast(evt);
            }
            Set();
        }

        public void AddFirst(TASK evt)
        {
            if (IsDisposing) return;
            todo++;
            if (NoQueue)
            {
                DoNow(evt);
                return;
            }
            lock (EventQueueLock)
            {
                EventQueue.AddFirst(evt);
            }
            Set();
        }

        public bool InvokeJoin(string s, int millisecondsTimeout)
        {
            lock (DebugStringLock)
            {
                if (WaitingString.Length > 0)
                {
                    WaitingString += "\n" + s;
                }
                else
                {
                    WaitingString = "\n" + s;
                }
            }
            ManualResetEvent are = new ManualResetEvent(false);
            Enqueue(() =>
                        {
                            try
                            {
                                lock (DebugStringLock)
                                {
                                    if (WaitingString.Contains("\n" + s))
                                    {
                                        WaitingString = WaitingString.Replace("\n" + s, "");
                                    }
                                }
                                lock (DebugStringLock)
                                {
                                    are.Set();
                                }
                            }
                            catch
                            {
                            }
                        });
            if (millisecondsTimeout == -1)
            {
                // three minutes
                millisecondsTimeout = 180000;
            }
            bool success = are.WaitOne(millisecondsTimeout);
            if (!success)
            {
                WriteLine("ERROR! TIMOUT " + s + " for " + ToString() + " in " + Thread.CurrentThread);
            }
            {
                lock (DebugStringLock)
                {
                    if (WaitingString.Contains("\n" + s))
                    {
                        WaitingString = WaitingString.Replace("\n" + s, "");
                    }
                }
            }
            return success;
        }

        public void RunTaskSyncronously(TASK action, string name, TimeSpan maxTime)
        {
            // Add the worker to the Queue
            Enqueue(() => MakeSyncronousTask(action, name, maxTime));
        }

        public void MakeSyncronousTask(TASK action, string name, TimeSpan maxTime)
        {
            EventWaitHandle IsComplete = new EventWaitHandle(false, EventResetMode.ManualReset);
            Thread t = CreateTask(action, name, IsComplete);
            Thread tr = new Thread(() =>
                                       {
                                           Thread threadKillThread = Thread.CurrentThread;
                                           try
                                           {
                                               t.Start();
                                               try
                                               {
                                                   // wait for maxTime .. then kill the thread
                                                   if (!IsComplete.WaitOne(maxTime))
                                                   {
                                                       lock (InteruptionList) if (t.IsAlive) t.Interrupt();
                                                   }
                                               }
                                               catch (Exception e)
                                               {
                                                   WriteLine("ERROR " + name + " " + e);
                                               }
                                           }
                                           finally
                                           {
                                               lock (InteruptionList)
                                               {
                                                   InteruptionList.Remove(t);
                                                   InteruptionList.Remove(threadKillThread);
                                               }
                                           }
                                       }) { Name = "Killer of " + name };
            tr.Start();
        }

        private Thread CreateTask(TASK action, string name, EventWaitHandle isComplete)
        {
            Thread tr = new Thread(() =>
                                       {
                                           Thread self = Thread.CurrentThread;
                                           try
                                           {
                                               lock (InteruptionList) InteruptionList.Add(self);
                                               try
                                               {
                                                   action();
                                               }
                                               catch (Exception e)
                                               {
                                                   WriteLine("ERROR " + name + " " + e);
                                               }
                                           }
                                           finally
                                           {
                                               lock (InteruptionList) InteruptionList.Remove(self);
                                               NoExceptions<bool>(isComplete.Set);
                                           }
                                       }) { Name = name };
            return tr;
        }

        private T NoExceptions<T>(Func<T> func)
        {
            try
            {
                return func();
            }
            catch (Exception e)
            {
                WriteLine("" + e);
                return default(T);
            }
        }

    }

    public class ThreadControl
    {
        public EventWaitHandle TaskStart;
        public EventWaitHandle TaskEnded;
        public EventWaitHandle TaskComplete;
        public Exception TaskException;
        public Thread ThreadForTask;
        public ThreadControl(EventWaitHandle onComplete)
        {
            TaskComplete = onComplete;
        }
        public bool OnInterrupted(ThreadInterruptedException exception)
        {
            TaskException = exception;
            throw exception;
        }
        public bool OnAborted(ThreadAbortException exception)
        {
            TaskException = exception;
            throw exception;
        }
        public bool OnException(Exception exception)
        {
            TaskException = exception;
            throw exception;
        }
    }
}