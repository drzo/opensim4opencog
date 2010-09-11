using System;
using System.Collections.Generic;
using System.IO;
using System.Text;
using System.Threading;
using MushDLR223.ScriptEngines;
using TASK = System.Threading.ThreadStart;

namespace MushDLR223.Utilities
{
    public class TaskQueueHandler : IDisposable
    {
        public static readonly OutputDelegate errOutput = DLRConsole.SYSTEM_ERR_WRITELINE;
        protected ThreadControl LocalThreadControl = new ThreadControl(new ManualResetEvent(false));
        readonly List<ThreadStart> OnFinnaly = new List<ThreadStart>();
        // ping wiat time should be less than 4 seconds when going in
        public static readonly TimeSpan MAX_PING_WAIT = TimeSpan.FromSeconds(4);
        public static readonly TimeSpan PING_INTERVAL = TimeSpan.FromSeconds(10); // 30 seconds
        // 10 minutes
        public static readonly TimeSpan TOO_LONG_INTERVAL = TimeSpan.FromMinutes(10);
        // 1ms
        public static readonly TimeSpan TOO_SHORT_INTERVAL = TimeSpan.FromMilliseconds(1);
        public static readonly HashSet<TaskQueueHandler> TaskQueueHandlers = new HashSet<TaskQueueHandler>();
        private readonly LinkedList<TASK> EventQueue = new LinkedList<TASK>();

        private readonly object EventQueueLock = new object();
        private readonly object PingNeverAbortLock = new object();
        private readonly object OneTaskAtATimeLock = new object();
        private readonly object TaskThreadChangeLock = new object();
        private readonly object PingWaitingLock = new object();
        private readonly object DebugStringLock = new object();
        private readonly object BusyTrackingLock = new object();
        

        private Thread PingerThread;
        private Thread TaskThread;
        private Thread StackerThread;

        public readonly List<Thread> InteruptableThreads = new List<Thread>();

        //public delegate ThreadStart NameThreadStart(string named, ThreadStart action);

        public string Name;
        private readonly TASK NOP;
        private readonly AutoResetEvent WaitingOn = new AutoResetEvent(false);
        private readonly AutoResetEvent IsCurrentTaskStarted = new AutoResetEvent(false);
        

        private bool _killTasksOverTimeLimit;
        private bool _noQueue;
        public bool Busy;
        public bool abortable;

        private DateTime BusyEnd;
        private DateTime BusyStart = DateTime.Now;
        private DateTime PingStart;


        private ulong CompletedSinceLastPing = 0;
        public OutputDelegate debugOutput = errOutput;

        bool AbortRequested = false;
        private bool debugRequested = false;
        private ulong GoodPings = 0;
        public bool IsDisposing = false;
        private ulong LatePings = 0;
        private bool problems = false;
        public bool SimplyLoopNoWait = false;
        private ulong StartedSinceLastPing = 0;
        private ulong ExpectedTodo;
        private ulong TotalComplete = 0;
        private ulong TotalFailures = 0;
        private ulong TotalStarted = 0;
        private bool WaitingOnPing = false;
        public string WaitingString = "NO_WAITING";
        public string LastDebugMessage = "NO_MESG";        
        const string INFO = "$INFO$";

        public TimeSpan PauseBetweenOperations = TOO_SHORT_INTERVAL;
        private TimeSpan LastOperationTimespan = TOO_LONG_INTERVAL;
        private TimeSpan LastPingLagTime = TOO_SHORT_INTERVAL;
        public TimeSpan LastRestTime = TOO_LONG_INTERVAL;
        private TimeSpan ThisMaxOperationTimespan = TOO_LONG_INTERVAL;

        private TimeSpan _operationKillTimeout = TOO_LONG_INTERVAL;
        /// <summary>
        /// When this is set the KillTasksOverTimeLimit should bne false
        /// </summary>
        private TimeSpan OperationKillTimeout
        {
            get { return _operationKillTimeout; }
            set
            {
                _operationKillTimeout = TOO_LONG_INTERVAL;
                if (value <= TOO_SHORT_INTERVAL)
                {
                    WriteLine("OperationKillTimeout at " + value + " bumping up to " +
                              TOO_LONG_INTERVAL);
                }
            }
        }

        public TaskQueueHandler(string str, TimeSpan msWaitBetween)
            : this(str, msWaitBetween, true)
        {
        }

        public TaskQueueHandler(string str, int msWaitBetween)
            : this(str, TimeSpan.FromMilliseconds(msWaitBetween), true)
        {
        }

        public TaskQueueHandler(string str, TimeSpan msWaitBetween, bool autoStart)
            : this(str, msWaitBetween, TimeSpan.MaxValue, autoStart)
        {
        }

        public TaskQueueHandler(string str, TimeSpan msWaitBetween, TimeSpan maxPerOperation, bool autoStart)
        {
            KillTasksOverTimeLimit = false;
            NOP = NOP ?? (() => { });
            lock (TaskQueueHandlers)
            {
                BusyEnd = BusyStart;
                Name = str;
                TaskQueueHandlers.Add(this);

                // 1ms min - ten minutes max
                OperationKillTimeout = TimeSpanBetween(maxPerOperation, TOO_SHORT_INTERVAL, TOO_LONG_INTERVAL);

                // zero secs - ten minutes max
                PauseBetweenOperations = TimeSpanBetween(msWaitBetween, TimeSpan.Zero, TOO_LONG_INTERVAL);

                PingerThread = new Thread(LoopNoAbort(EventQueue_Ping, PingNeverAbortLock))
                                   {Name = str + " pinger", Priority = ThreadPriority.Lowest};
            }
            if (autoStart) Start();
        }

        static TimeSpan TimeSpanBetween(TimeSpan orig, TimeSpan low, TimeSpan high)
        {
            if (orig < low) return low;
            if (orig > high) return high;
            return orig;
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
                    if (OperationKillTimeout <= TOO_SHORT_INTERVAL)
                    {
                        WriteLine("OperationKillTimeout at " + TOO_SHORT_INTERVAL + " bumping up to " +
                                  TOO_LONG_INTERVAL);
                        OperationKillTimeout = TOO_LONG_INTERVAL;
                    }
                    _killTasksOverTimeLimit = value;
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
            get { lock (EventQueueLock) return _noQueue; }
            set
            {
                lock (EventQueueLock)
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
                NoExceptions<bool>(WaitingOn.Set);
                NoExceptions(WaitingOn.Close);
            }
            catch (ObjectDisposedException)
            {
            }
        }

        #endregion

        public void Start()
        {
            lock (BusyTrackingLock)
            {
                Start0();
            }
        }
        private void Start0()
        {
            if (StackerThread == null) StackerThread = new Thread(LoopNoAbort(EventQueue_Handler, OneTaskAtATimeLock));
            if (!StackerThread.IsAlive)
            {
                StackerThread.Name = Name + " worker task queue";
                StackerThread.Priority = ThreadPriority.BelowNormal;
                StackerThread.Start();
            }
            if (PingerThread != null && !PingerThread.IsAlive) PingerThread.Start();
            debugOutput = debugOutput ?? errOutput;
        }

        public override string ToString()
        {
            return ToDebugString(false);
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

        public string ToDebugString(bool detailed)
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
                + string.Format(" TODO = {0} ", ExpectedTodo)
                +
                string.Format(" TOTALS = {0}/{1}/{2} ", TotalComplete, TotalStarted,
                              GetTimeString(LastOperationTimespan))
                +
                string.Format(" PINGED = {0}/{1}/{2} ", CompletedSinceLastPing, StartedSinceLastPing,
                              GetTimeString(LastPingLagTime));

            if (detailed)
            {
                WaitingS += ToThreadInfos();
            }


            if (IsDisposing) WaitingS += " IsDisposing";
            if (!IsRunning) WaitingS += " NOT RUNNING";
            string tdm = String.Format(
                "{0} {1} {2} {3} {4}",
                Busy ? "Busy" : "Idle",
                extraMesage,
                TotalFailures > 0 ? TotalFailures + " failures " : "",
                _noQueue ? "NoQueue" : "",
                WaitingS ?? " BEFORE ");
            return tdm;
        }

        private string ToThreadInfos()
        {
            var botCommandThreads = InteruptableThreads;
            List<string> status = new List<string>();
            StringBuilder stringBuilder = new StringBuilder("<begin>\nInteruptableThreads.Count=" + InteruptableThreads.Count,245);
            lock (botCommandThreads)
            {
                int n = 0;
                int dead = 0;
                int suspended = 0;
                int num = botCommandThreads.Count;
                foreach (Thread t in botCommandThreads)
                {
                    n++;
                    num--;
                    //System.Threading.ThreadStateException: Thread is dead; state cannot be accessed.
                    //  at System.Threading.Thread.IsBackgroundNative()
                    if (!t.IsAlive)
                    {
                        status.Add(string.Format("{0}: {1} IsAlive={2} {3}", num, t.Name, t.IsAlive,t.ThreadState));
                    }
                    else
                    {
                        stringBuilder.AppendLine(string.Format("{0}: {1} IsAlive={2} {3}", num, t.Name, t.IsAlive, t.ThreadState));
                    }
                }
            }
            foreach (var c in status)
            {
                stringBuilder.AppendLine(c);                
            }
            stringBuilder.AppendLine(" ..\n..<begin>");
            return stringBuilder.ToString();
        }

        public ThreadStart LoopNoAbort(Action innerloop, object oneTaskAtATimeLock)
        {

            return () =>
                       {
                           Thread starterThread = Thread.CurrentThread;
                           lock (InteruptableThreads)
                               if (!InteruptableThreads.Contains(starterThread))
                                   InteruptableThreads.Add(starterThread);
                           
                           Action doIt = () => InnerLoopNoAbortHelper(starterThread, innerloop, oneTaskAtATimeLock);

                           while (!(IsDisposing))
                               try
                               {
                                   while (!(IsDisposing))
                                       try
                                       {
                                           while (!(IsDisposing)) doIt();
                                       }
                                       catch (Exception)
                                       {
                                       }
                               }
                               catch (Exception)
                               {
                               }
                       };
        }

        private void InnerLoopNoAbortHelper(Thread except, Action action, object oneTaskAtATimeLock)
        {
            bool needsExit = false;
            try
            {
                needsExit = System.Threading.Monitor.TryEnter(oneTaskAtATimeLock);
                if (!needsExit)
                {
                    string str = "EventQueue_Handler is locked out!";
                    errOutput(str);
                    WriteLine(str);
                    throw new InternalBufferOverflowException(str);

                }
                else
                {
                    action();
                }
            }
            catch (ThreadAbortException abortException)
            {
                bool resumeable = !IsDisposing && Thread.CurrentThread == except;
                WriteLine("InnerLoopNoAbortHelper: " + abortException + "\n  reset=" + resumeable);
                if (!resumeable) return;
                NoExceptions(Thread.ResetAbort);
            }
            catch (ThreadInterruptedException abortException)
            {
                bool resumeable = !IsDisposing && Thread.CurrentThread == except;
                WriteLine("ThreadInterruptedException: " + abortException + "\n  reset=" +
                          resumeable);
                if (!resumeable) return;
                NoExceptions(except.Resume);
            }
            finally
            {
                if (needsExit) System.Threading.Monitor.Exit(oneTaskAtATimeLock);
            }
        }

        private void EventQueue_Handler()
        {
            while (!(IsDisposing))
            {
                Busy = false;

                TASK evt;
                int evtCount;
                lock (EventQueueLock)
                {
                    evtCount = EventQueue.Count;
                    if (evtCount > 0)
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
                    if (PauseBetweenOperations > TimeSpan.Zero) Thread.Sleep(PauseBetweenOperations);
                    // avoid reset/set semantics ?
                    if (evtCount > 1) continue;

                }
                else
                {
                    // avoid reset/set semantics ?
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
                        Thread.Sleep(TOO_SHORT_INTERVAL);
                        continue;
                    }
                    if (evtCount > 1) continue;
                    WaitOne();
                }
                Busy = false;
            }
        }

        private bool WaitOne()
        {
            return NoExceptions(() =>
                             {
                                 bool r = false;
                                 while (!IsDisposing)
                                 {
                                     r = WaitingOn.WaitOne(ThisMaxOperationTimespan);
                                     if (r) return true;
                                     errOutput(CreateMessage("WaitOne: TIMEOUT ERROR {0} was {1} ", INFO, GetTimeString(ThisMaxOperationTimespan)));
                                     problems = true;
                                     lock (BusyTrackingLock)
                                     {
                                         if (BusyEnd < BusyStart)
                                         {
                                             LastRestTime = BusyStart.Subtract(BusyEnd);
                                         }
                                         var len = DateTime.Now.Subtract(BusyStart);
                                         errOutput(CreateMessage("Moving on with TIMEOUT {0} was {1} ", INFO, GetTimeString(ThisMaxOperationTimespan)));
                                         if (ExpectedTodo > 0) return true;////r = true;
                                         return false;
                                     }
                                     if (SimplyLoopNoWait) return false;
                                     return r;
                                 }                                
                                 // ReSharper disable ConditionIsAlwaysTrueOrFalse
                                 return r;
                                 // ReSharper restore ConditionIsAlwaysTrueOrFalse
                             });
        }

        private void WaitOneMaybe()
        {
            bool waitOne = true;

            while (waitOne)
            {
                if (IsDisposing) return;
                while (EventQueue.Count == 0)
                {
                    if (!WaitingOn.WaitOne(TOO_LONG_INTERVAL))
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
// ReSharper disable RedundantAssignment
                    waitOne = true;
// ReSharper restore RedundantAssignment
                    continue;
                }
            }
        }

        private void Reset()
        {
            if (!IsDisposing) NoExceptions<bool>(WaitingOn.Reset);
        }

        public bool Set()
        {
            return (!IsDisposing) && NoExceptions<bool>(WaitingOn.Set);
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
                    lock (BusyTrackingLock)
                    {
                        ExpectedTodo = (ulong)EventQueue.Count;
                    }
                    if (Busy)
                    {
                        TimeSpan t;
                        lock (BusyTrackingLock)
                        {
                            t = DateTime.Now - BusyStart;
                        }
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
                                if (ExpectedTodo > 0)
                                {
                                    problems = true;
                                    TimeSpan t = DateTime.Now - BusyEnd;
                                    WriteLine("NOTHING DONE IN time = {0} " + INFO, GetTimeString(t));
                                }
                            }
                        }

                        if (Busy /* && (StartedSinceLastPing == 0 && FinishedSinceLastPing == 0)*/)
                        {
                            // ReSharper disable ConditionIsAlwaysTrueOrFalse
                            TimeSpan tt = DateTime.Now - PingStart;
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
                    PingStart = DateTime.Now;
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
            LastPingLagTime = DateTime.Now - PingStart;
            TotalStarted--;
            StartedSinceLastPing--;
            if (LastPingLagTime <= MAX_PING_WAIT)
            {
                GoodPings++;
                if (ExpectedTodo > 0)
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
            if (InternalEvent(evt))
            {
                NoExceptions(evt);
                return;
            }
            //var IsCurrentTaskComplete = new ManualResetEvent(false);
            try
            {
                lock (BusyTrackingLock)
                {
                    Busy = true;
                    TotalStarted++; ExpectedTodo--;
                    BusyStart = DateTime.Now;
                    LastRestTime = BusyStart.Subtract(BusyEnd);
                }
                StartedSinceLastPing++;
                {
                    Tick(evt);
                    lock (TaskThreadChangeLock)
                        abortable = false;
                    //() => RunWithTimeLimit(evt, "RunWithTimeLimit", OperationKillTimeout));
                }
                lock (BusyTrackingLock)
                {
                    CompletedSinceLastPing++;
                    TotalComplete++;
                    Busy = false;
                }
            }
            catch (ThreadAbortException e)
            {
                abortable = false;
                AbortRequested = true;
                Thread.ResetAbort();
                WriteLine("ThreadAbortException 0");
            }
            catch (Exception e)
            {
                lock (BusyTrackingLock)
                {
                    TotalFailures++;
                }
                WriteLine("ERROR! {0} was {1} in {2}", INFO, e, Thread.CurrentThread);
            }
            finally
            {
                lock (BusyTrackingLock)
                {
                    BusyEnd = DateTime.Now;
                    LastOperationTimespan = BusyEnd.Subtract(BusyStart);
                    if (LastOperationTimespan > TimeSpan.Zero)
                    {
                        var proposal = (LastOperationTimespan + LastOperationTimespan);
                        if (proposal.TotalMilliseconds > 500)
                        {
                            ThisMaxOperationTimespan = proposal;
                            WriteLine00("changeing " + ThisMaxOperationTimespan + " to ");
                        }
                    }
                    //TotalStarted++;
                    Busy = false;
                }
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

        private bool InternalEvent(TASK evt)
        {
            if (EventQueue_Pong == evt) return true;
            if (!(evt.Method != null && evt.Method.DeclaringType == GetType())) return false;
            return true;
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
                                                  if (AbortRequested)
                                                  {
                                                      WriteLine("ThreadAbortException 2");
                                                      AbortRequested = false;
                                                  }
                                              }
                                          });

                    AbortRequested = false;
                    abortable = true;
                    NoExceptions<bool>(IsCurrentTaskStarted.Set);
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
                    AbortRequested = true;
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
            control = control ?? new ThreadControl(null);
            var ctrl = control != null;
            lock (BusyTrackingLock)
            {
                BusyStart = DateTime.Now;
            }
            KillTasksOverTimeLimit = true;
            try
            {
                if (control.TaskStart != null) NoExceptions<bool>(control.TaskStart.Set);
                evt();
                if (control.TaskEnded != null) NoExceptions<bool>(control.TaskEnded.Set);
            }
            finally
            {
                if (control.TaskComplete != null) NoExceptions<bool>(control.TaskComplete.Set);
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
                EventWaitHandle isCurrentTaskStarted1 = control.TaskStart = new AutoResetEvent(false);
                control.ThreadForTask = threadPlace;
                threadPlace = new Thread(OneTask(evt, isCurrentTaskStarted1, IsCurrentTaskComplete, IsCurrentTaskEnded));

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
                        control.InvokeInterruptRaised(interupted);
                    }
                    catch (ThreadAbortException exception)
                    {
                        WriteLine("ABORT {0} was {1} in {2}", INFO, exception, threadPlace);
                        control.InvokeAbortRaised(exception);
                    }
                    catch (Exception ex)
                    {
                        WriteLine("ERROR {0} was {1} in {2}", INFO, ex, threadPlace);
                        control.InvokeExceptionRaised(ex);
                    }
                    finally
                    {
#if !NEW_INERUPT
                        NoExceptions<bool>(isCurrentTaskStarted1.Reset);
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
                        System.Threading.Monitor.Enter(OneTaskAtATimeLock);
                        try
                        {
                            evt();
                        }
                        finally
                        {
                            System.Threading.Monitor.Exit(OneTaskAtATimeLock);
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
                            AbortRequested = true;
                            lock (OnFinnaly)
                            {
                                TaskThread.Abort();
                            }
                        }
                    }
                    //TaskThread.Suspend();
                    problems = true;
                }
        }


        public void RestartCurrentTask()
        {
            NoExceptions(TaskThread.Resume);
            NoExceptions(StackerThread.Resume);
        }

        private bool inWriteline = false;
        public void WriteLine(string s, params object[] parms)
        {
            if (inWriteline)
            {
                DLRConsole.SYSTEM_ERR_WRITELINE("inWriteLine!!!!!!!!=" + new Exception().StackTrace);
                return;
            }
            try
            {
                inWriteline = true;
                WriteLine00(s, parms);
            }
            catch
            {
            }
            finally
            {
                inWriteline = false;
            }
        }

        public void WriteLine00(string s, params object[] parms)
        {
            debugOutput = debugOutput ?? errOutput;
            s = CreateMessage(s, parms);
            LastDebugMessage = s;
            if (debugOutput == errOutput)
            {
                errOutput(s);
                return;
            }
            if (debugOutput != DLRConsole.DebugWriteLine)
            {
                DLRConsole.DebugWriteLine(s);
            }
            if (debugOutput != null) debugOutput(s);
        }

        private string CreateMessage(string s, params object[] parms)
        {
            s = DLRConsole.SafeFormat("..\n[TASK " + Name + "] " + s, parms);
            if (s.Contains(INFO))
            {
                var str = ToDebugString(true);
                s = s.Replace(INFO, str + "\n...");
            }
            return s;
        }

        public void Enqueue(TASK evt)
        {
            lock (BusyTrackingLock)
            {
                if (IsDisposing) return;
                ExpectedTodo++;
            }
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
            lock(BusyTrackingLock)
            {
                ExpectedTodo++;
            }
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
                                    NoExceptions<bool>(are.Set);                                    
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
                                                       lock (InteruptableThreads) if (t.IsAlive) t.Interrupt();
                                                   }
                                               }
                                               catch (Exception e)
                                               {
                                                   WriteLine("ERROR " + name + " " + e);
                                               }
                                           }
                                           finally
                                           {
                                               lock (InteruptableThreads)
                                               {
                                                   InteruptableThreads.Remove(t);
                                                   InteruptableThreads.Remove(threadKillThread);
                                               }
                                           }
                                       }) { Name = "Killer of " + name };
            tr.Start();
        }

        public Thread CreateTask(TASK action, string name, EventWaitHandle isComplete)
        {
            Thread tr = new Thread(() =>
                                       {
                                           Thread self = Thread.CurrentThread;
                                           try
                                           {
                                               lock (InteruptableThreads) InteruptableThreads.Add(self);
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
                                               lock (InteruptableThreads) InteruptableThreads.Remove(self);
                                               NoExceptions<bool>(isComplete.Set);
                                           }
                                       }) { Name = name };
            return tr;
        }

        private void NoExceptions(TASK func)
        {
            try
            {
                if (IsDisposing)
                {
                    WriteLine("IsDisposing");
                    return;
                }
                
                func.Invoke();
                return;
            }
            catch (Exception e)
            {
                WriteLine("" + e);
                return;
            }
        }

        private T NoExceptions<T>(Func<T> func)
        {
            try
            {
                if (IsDisposing)
                {
                    WriteLine("IsDisposing");
                    return default(T);
                }
                return func.Invoke();
            }
            catch (Exception e)
            {
                WriteLine("" + e);
                return default(T);
            }
        }

        public void RemoveThread(Thread thread)
        {
        }
    }

    public class ThreadControl
    {
        public event Action<ThreadControl, ThreadAbortException> AbortRaised;
        public event Action<ThreadControl, ThreadInterruptedException> InterruptRaised;
        public event Action<ThreadControl, Exception> AbortOrInteruptedRaised;

        private void InvokeAbortOrInteruptedRaised(Exception arg2)
        {
            Action<ThreadControl, Exception> raised = AbortOrInteruptedRaised;
            if (raised != null) raised(this, arg2);
        }

        public event Action<ThreadControl, Exception> ExceptionRaised;

        public EventWaitHandle TaskStart;
        public EventWaitHandle TaskEnded;
        public EventWaitHandle TaskComplete;
        public Exception TaskException;
        public Thread ThreadForTask;

        public bool CancelAbort = false;
        public bool CancelInterp = false;
        public bool CancelException = false;

        public ThreadControl(EventWaitHandle onComplete)
        {
            TaskComplete = onComplete;
        }

        public void InvokeExceptionRaised(Exception exception)
        {
            InvokeAbortOrInteruptedRaised(exception);
            Action<ThreadControl, Exception> raised = ExceptionRaised;
            if (raised != null)
            {
                raised(this, exception);
            }
            TaskException = exception;
            throw exception;
        }

        public bool InvokeInterruptRaised(ThreadInterruptedException exception)
        {
            InvokeAbortOrInteruptedRaised(exception);
            Action<ThreadControl, ThreadInterruptedException> raised = InterruptRaised;
            if (raised != null)
            {
                raised(this, exception);
            }
            TaskException = exception;
            throw exception;
        }
        public bool InvokeAbortRaised(ThreadAbortException exception)
        {
            Action<ThreadControl, ThreadAbortException> raised = AbortRaised;
            if (raised != null)
            {
                raised(this, exception);
            }
            TaskException = exception;
            if (!CancelAbort)
            {
                throw exception;
            }
            return !CancelAbort;
        }

        public bool WaitUntilComplete()
        {
            if (TaskEnded!=null) return TaskEnded.WaitOne();
            else
            {
                if (TaskComplete!=null) return TaskComplete.WaitOne();
            }
            return !ThreadForTask.IsAlive;
        }
    }
}