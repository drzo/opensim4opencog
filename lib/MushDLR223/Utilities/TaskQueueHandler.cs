using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Text;
using System.Threading;
using MushDLR223.ScriptEngines;
using TASK = System.Threading.ThreadStart;
using ThreadState=System.Threading.ThreadState;

namespace MushDLR223.Utilities
{
    public class TaskQueueHandler : IDisposable
    {
        public static readonly OutputDelegate errOutput = DLRConsole.SYSTEM_ERR_WRITELINE;
        protected ThreadControl LocalThreadControl = new ThreadControl(new ManualResetEvent(false));
        readonly List<ThreadStart> OnFinnaly = new List<ThreadStart>();
        // ping wait time should be less than 4 seconds when going in
        public TimeSpan MAX_PING_WAIT = TimeSpan.FromSeconds(10);
        public TimeSpan PING_INTERVAL = TimeSpan.FromSeconds(30); // 30 seconds
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

        private bool WasStartCalled;
        private Thread PingerThread;
        private Thread TaskThreadCurrent;
        private Thread StackerThread;
        private TaskThreadHolder TaskHolder;
        //private Dictionary<Thread,TaskThreadKit> TaskThreadAbortKit = new Dictionary<Thread, TaskThreadKit>();

        public readonly List<Thread> InteruptableThreads = new List<Thread>();

        //public delegate ThreadStart NameThreadStart(string named, ThreadStart action);

        public string Name;
        private readonly TASK NOP;
        private readonly AutoResetEvent WaitingOn = new AutoResetEvent(false);
        //private readonly AutoResetEvent IsCurrentTaskStarted = new AutoResetEvent(false);
        

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
        public string WaitingString = "";
        private string LastOpString = "";
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

                MAX_PING_WAIT = TimeSpanBetween(MAX_PING_WAIT, maxPerOperation, OperationKillTimeout); 

                PING_INTERVAL = TimeSpanBetween(PING_INTERVAL, msWaitBetween, OperationKillTimeout);

                // zero secs - ten minutes max
                PauseBetweenOperations = TimeSpanBetween(msWaitBetween, TimeSpan.Zero, TOO_LONG_INTERVAL);

                PingerThread = new Thread(LoopNoAbort(EventQueue_Ping, PingNeverAbortLock))
                                   {Name = str + " pinger", Priority = ThreadPriority.Lowest};
            }
            if (autoStart) Start();
        }

        static TaskQueueHandler()
        {
            // If we don't have a high resolution timer then Stopwatch will fall back
            // to DateTime, which is much less reliable
            if (Stopwatch.IsHighResolution)
                DLRConsole.DebugWriteLine("We have a high resolution timer available");

            long frequency = Stopwatch.Frequency;
            DLRConsole.DebugWriteLine(" Timer frequency in ticks per second = {0}", frequency);
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
                if (!WasStartCalled) return false;
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
            try
            {
                if (IsDisposing) return;
                IsDisposing = true;
                lock (TaskQueueHandlers)
                    TaskQueueHandlers.Remove(this);
                AbortThread(PingerThread);
                PingerThread = null;
                if (!IsTaskThread)
                {
                    AbortThread(StackerThread);

                    StackerThread = null;
                    if (TaskHolder != null)
                    {
                        AbortThread(TaskHolder.ThreadForTask);
                    }
                    TaskHolder = null;
                }
                NoExceptions<bool>(WaitingOn.Set);
                NoExceptions(WaitingOn.Close);
            }
            catch (ObjectDisposedException)
            {
            }
        }

        private void AbortThread(Thread thread)
        {
            if (thread != null)
            {
                if (thread != Thread.CurrentThread && thread.IsAlive) NoExceptions(thread.Abort);
            }
        }

        #endregion

        public void Start()
        {
            if (WasStartCalled) return;
            //TestLock(BusyTrackingLock);
            //lock (BusyTrackingLock)
            {
                Start0();
            }
        }

        private void TestLock(object busyTrackingLock)
        {
            return;
            if (Monitor.TryEnter(busyTrackingLock, TimeSpan.FromSeconds(10)))
            {
                Monitor.Exit(busyTrackingLock);
                return;
            }
            DLRConsole.SYSTEM_ERR_WRITELINE_REAL("ERROR: RORROOR busyTrackingLock");
            VeryBad("Cant get into " + busyTrackingLock);
        }

        public static void TestLockHeld(object busyTrackingLock)
        {
            if (Monitor.TryEnter(busyTrackingLock, TimeSpan.FromSeconds(10)))
            {
                Monitor.Exit(busyTrackingLock);
                return;
            }
            DLRConsole.DebugWriteLine("Cant get into " + busyTrackingLock);
        }

        public static void WithLock(object busyTrackingLock, Action doit)
        {
            if (Monitor.TryEnter(busyTrackingLock, TimeSpan.FromSeconds(10)))
            {
                try
                {
                    doit();
                }
                catch (Exception e)
                {
                    throw e;
                }
                finally
                {
                    Monitor.Exit(busyTrackingLock);
                }
                return;
            }
            DLRConsole.DebugWriteLine("Cant get into " + busyTrackingLock);
            doit();
        }

        private void Start0()
        {
            WasStartCalled = true;
            if (StackerThread == null)
            {
                string tname = Name;
                StackerThread = new Thread(LoopNoAbort(EventQueue_Handler, OneTaskAtATimeLock))
                                    {
                                        Name = tname + " queueworker",
                                        Priority = ThreadPriority.BelowNormal,
                                    };
            }
            if (!StackerThread.IsAlive)
            {
                RestartCurrentTask(StackerThread);//.Start();
            }
            if (PingerThread != null && !PingerThread.IsAlive) RestartCurrentTask(PingerThread);//.Start();
            debugOutput = debugOutput ?? errOutput;
        }

        public override string ToString()
        {
            return ToDebugString(false);
        }


        public TASK NamedTask(string named, TASK action)
        {
            if (named == null) return action;
            return () =>
                       {
                           string s = named + "\n";
                           try
                           {
                               lock (DebugStringLock)
                               {
                                   TrySetThreadName(s);
                               }
                               action();
                           }
                           finally
                           {
                               lock (DebugStringLock)
                               {
                                   WaitingString = WaitingString.Replace(s, "").Trim();
                               }
                           }
                       };
        }

        public string ToDebugString(bool detailed)
        {
            string WaitingS = "NO_WAITING";
            lock (DebugStringLock)
            {
                if (WaitingString.Length >= 0)
                {
                    WaitingS = "WaitingOn: " + WaitingString.Replace("\n"," ").Trim();
                }
                if (LastOpString.Length >= 0)
                {
                    WaitingS += " op: " + LastOpString;
                }
            }
            string extraMesage =
                "\"" + Name + "\" "
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
            if (!IsRunning) extraMesage = " NOT_RUNNING " + extraMesage;
            string tdm = DLRConsole.SafeFormat(
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
            return "";
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
                        status.Add(DLRConsole.SafeFormat("{0}: {1} IsAlive={2} {3}", num, t.Name, t.IsAlive, t.ThreadState));
                    }
                    else
                    {
                        stringBuilder.AppendLine(DLRConsole.SafeFormat("{0}: {1} IsAlive={2} {3}", num, t.Name, t.IsAlive, t.ThreadState));
                    }
                }
            }
            foreach (var c in status)
            {
                stringBuilder.AppendLine(c);                
            }
            stringBuilder.AppendLine("\n</begin>");
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
                    VeryBad(str);
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
                    NoExceptions(() => DoNow(evt));
                    Busy = false;
                    if (evtCount < 2 && !WaitingOnPing)
                    {
                        Sleep(PauseBetweenOperations);
                    }
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
                        Busy = false;
                        Sleep(TOO_SHORT_INTERVAL);
                        continue;
                    }
                    if (evtCount > 1) continue;
                    WaitOne();
                }
                Busy = false;
            }
        }

        private void Sleep(TimeSpan pauseBetweenOperations)
        {
            if (PauseBetweenOperations >= TOO_SHORT_INTERVAL) 
            {
                if (PauseBetweenOperations < TimeSpan.Zero) Thread.Sleep(pauseBetweenOperations);
            }
        }

        private bool WaitOneIsBroken = false;
        private bool WaitOne()
        {
            if (WaitOneIsBroken)
            {
                return WaitOneAlt();
            }
            return NoExceptions(() =>
                             {
                                 bool r = false;
                                 while (!IsDisposing)
                                 {
                                     lock (EventQueueLock)
                                     {
                                         ExpectedTodo = (ulong)EventQueue.Count;
                                     }
                                     if (ExpectedTodo > 0)
                                     {
                                         // no need to wait
                                         return true;
                                     }
                                     r = WaitingOn.WaitOne(ThisMaxOperationTimespan);
                                     if (r) return true;
                                     lock (EventQueueLock)
                                     {
                                         ExpectedTodo = (ulong) EventQueue.Count;
                                     }
                                     if (ExpectedTodo == 0)
                                     {
                                         //wait longer
                                         continue;
                                     }
                                     errOutput(CreateMessage("WaitOne: TIMEOUT ERROR {0} was {1} ", INFO, GetTimeString(ThisMaxOperationTimespan)));
                                     problems = true;
                                     //TestLock(BusyTrackingLock);
                                     //lock (BusyTrackingLock)
                                     {
                                         if (BusyEnd < BusyStart)
                                         {
                                             LastRestTime = BusyStart.Subtract(BusyEnd);
                                         }
                                         //var len = DateTime.Now.Subtract(BusyStart);
                                     }
                                     errOutput(CreateMessage("Moving on with TIMEOUT {0} was {1} ", INFO, GetTimeString(ThisMaxOperationTimespan)));
                                     if (ExpectedTodo > 0) return true;////r = true;
                                     return false;
                                     if (SimplyLoopNoWait) return false;
                                     return r;
                                 }                                
                                 // ReSharper disable ConditionIsAlwaysTrueOrFalse
                                 return r;
                                 // ReSharper restore ConditionIsAlwaysTrueOrFalse
                             });
        }

        private bool WaitOneAlt()
        {
            if (DLRConsole.HasWinforms)
            {
                System.Windows.Forms.Application.DoEvents();
            }
            Sleep(TOO_SHORT_INTERVAL + PauseBetweenOperations);
            return true;
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

        ulong startedBeforeWait;
        ulong completeBeforeWait;
        ulong todoBeforeWait;
        ulong expectedTodoBeforeWait;
        private void EventQueue_Ping()
        {
            while (!(IsDisposing))
            {
                startedBeforeWait = TotalStarted;
                completeBeforeWait = TotalComplete;
#if TRACKING_LOCK
                TestLock(BusyTrackingLock);
                lock (BusyTrackingLock)
#endif
                {
                    expectedTodoBeforeWait = ExpectedTodo;
                    todoBeforeWait = (ulong) EventQueue.Count;
                    ExpectedTodo = todoBeforeWait;
                }
                Sleep(PING_INTERVAL);
                if (IsDisposing) break;
                if (!WasStartCalled) continue;

                bool wasBusy = false;
#if TRACKING_LOCK
                TestLock(BusyTrackingLock);
                lock (BusyTrackingLock)
#endif
                    wasBusy = Busy;

                if (wasBusy && CheckCurrentTaskOverTimeBudget())
                    WriteLine("OVER BUDGET");

                if (problems) LiveCheck();

                //              if (_noQueue) continue;
                bool wasWatingForPong = false;
                lock (PingWaitingLock)
                    wasWatingForPong = WaitingOnPing;

                if (!wasWatingForPong)
                {
                    lock (PingWaitingLock)
                    {
                        PingStart = DateTime.Now;
                        WaitingOnPing = true;
                    }
                    Enqueue0(EventQueue_Pong);
                    continue;
                }
                CheckTimedProgress();
                continue;
                CheckPingerTime();
            }
            // ReSharper disable FunctionNeverReturns
        }

        private void LiveCheck()
        {

            {
                if (StackerThread != null)
                    if (!StackerThread.IsAlive)
                    {
                        problems = true;
                        if (WasStartCalled)
                        {
                            WriteLine("ERROR EventQueueHandler DEAD! " + INFO);
                            RestartCurrentTask(StackerThread);
                            if (!StackerThread.IsAlive)
                            {
                                WriteLine("ERROR VERY DEAD! " + INFO);
                            }
                            else
                            {
                                WriteLine("RESTARTED " + INFO);
                            }
                        }
                        else
                        {
                            WriteLine("WARNING WARNING EventQueueHandler Not Started?! " + INFO);
                        }
                    }
            }
        }

        private bool CheckCurrentTaskOverTimeBudget()
        {
            {
                {
                    if (Busy)
                    {
                        TimeSpan t;
#if TRACKING_LOCK
                        lock (BusyTrackingLock)
#endif
                        {
                            t = DateTime.Now - BusyStart;
                        }
                        if (t > OperationKillTimeout)
                        {
                            problems = true;
                            VeryBad(CreateMessage("BUSY LONGER THAN {0} time = {1} " + INFO,
                                                  GetTimeString(OperationKillTimeout),
                                                  GetTimeString(t)));
                            if (KillTasksOverTimeLimit)
                            {
                                AbortCurrentOperation();
                                return true;
                            }
                        }
                    }
                }
            }
            return false;
        }

        private void CheckTimedProgress()
        {
            ulong todoBeforeWait = 0;
            ulong ExpectedTodo = 0;
            ulong todoAfterWait;
            lock (PingWaitingLock)
            {
                if (completeBeforeWait != TotalComplete || startedBeforeWait != TotalStarted) return;
                todoBeforeWait = this.todoBeforeWait;
                ExpectedTodo = this.ExpectedTodo;
            }
            if (ExpectedTodo > todoBeforeWait)
                                {
                                    var fromNow = BusyEnd;
                                    if (BusyStart > BusyEnd) fromNow = BusyStart;
                                    TimeSpan t = DateTime.Now - fromNow;
                                    if (t > TOO_LONG_INTERVAL)
                                    {
                                        VeryBad(CreateMessage("ERROR: NOTHING DONE IN time = {0} " + INFO,
                                                              GetTimeString(t)));
                                        problems = true;
                                        if (ExpectedTodo > 1) DebugQueue = true;
                                        AbortCurrentOperation();
                                    }
                                    string print = null;
                                    //lock (BusyTrackingLock)
                                    {
                                        if (todoBeforeWait + 1 < (ulong) EventQueue.Count && t > MAX_PING_WAIT)
                                        {
                                            problems = true;
                                            print = CreateMessage(
                                                "WARN: GROWING YET NOTHING DONE IN time = {0} " + INFO,
                                                GetTimeString(t));
                                        }
                                    }
                                    if (print != null)
                                    {
                                        WriteLine(print);
                                    }
     
            }
        }

        private void CheckPingerTime()
        {
            {
                {
                    {
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
                    }
                }
            }

        }

        private void EventQueue_Pong()
        {
            if (!WaitingOnPing) return;
            lock (PingWaitingLock)
            {
                // todo can this ever happen?
                if (!WaitingOnPing) return;
                WaitingOnPing = false;
                EventQueue_Pong0();
            }
        }

        private void EventQueue_Pong0()
        {
            LastPingLagTime = DateTime.Now - PingStart;
            if (LastPingLagTime <= MAX_PING_WAIT)
            {
                GoodPings++;
                if (ExpectedTodo > 0)
                    if (DebugQueue)
                        WriteLine("GOOD PONG: {0} {1}", GetTimeString(LastPingLagTime), INFO);
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
                lagString = string.Format("{0:0}ms", lag.Milliseconds);
            }
            else if (lag.TotalMinutes < 2)
            {
                lagString = string.Format("{0:0}secs", lag.TotalSeconds);
            }
            else if (lag.TotalHours < 2)
            {
                lagString = string.Format("{0:#.##}mins", lag.TotalMinutes);
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
                NonAbortedly(() => NoExceptions(evt));
                return;
            }
            //var IsCurrentTaskComplete = new ManualResetEvent(false);
            try
            {
#if TRACKING_LOCK
                lock (BusyTrackingLock)
#endif

                {
                    Busy = true;
                    TotalStarted++; ExpectedTodo--;
                    BusyStart = DateTime.Now;
                    LastRestTime = BusyStart.Subtract(BusyEnd);
                }
                StartedSinceLastPing++;
                lock (TaskThreadChangeLock)
                {
                    try
                    {
                        abortable = true;
                        Tick(evt);
                    }
                    finally
                    {
                        abortable = false;
                    }
                }
                //() => RunWithTimeLimit(evt, "RunWithTimeLimit", OperationKillTimeout));
#if TRACKING_LOCK
                lock (BusyTrackingLock)
#endif
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
#if TRACKING_LOCK
                lock (BusyTrackingLock)
#endif
                {
                    TotalFailures++;
                }
                WriteLine("ERROR! {0} was {1} in {2}", INFO, e, Thread.CurrentThread);
            }
            finally
            {
#if TRACKING_LOCK
                lock (BusyTrackingLock)
#endif
                {
                    BusyEnd = DateTime.Now;
                    Busy = false;
                    LastOperationTimespan = BusyEnd.Subtract(BusyStart);
                    if (LastOperationTimespan > TimeSpan.Zero)
                    {
                        TimeSpan proposal = LastOperationTimespan;
                        if ((LastOperationTimespan.TotalMilliseconds * 2) < ThisMaxOperationTimespan.TotalMilliseconds)
                        {
                            proposal =
                                TimeSpan.FromMilliseconds((LastOperationTimespan.TotalMilliseconds*2 +
                                                           ThisMaxOperationTimespan.TotalMilliseconds)/2);
                        } else
                        {
                            if (LastOperationTimespan.TotalMilliseconds>ThisMaxOperationTimespan.TotalMilliseconds)
                            {
                                proposal = TimeSpan.FromMilliseconds((LastOperationTimespan.TotalMilliseconds +
                                                                      ThisMaxOperationTimespan.TotalMilliseconds)/2);
                            }
                        }
                        if (proposal.TotalMilliseconds > 500)
                        {
                            if (proposal.TotalMilliseconds > 1500)
                            {
                                if (problems)
                                {
                                   // WriteLine00("MaxOperationTimespan: " + LastOperationTimespan + " to " + proposal);
                                }
                                ThisMaxOperationTimespan = proposal;
                            }
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
            if (abortable)
            {
                // already wrapped
                evt();
                return; 
            }
            TaskThreadHolder taskThreadAbortHolder = new TaskThreadHolder();
            Thread workerThread = new Thread(() => Tick0(evt, taskThreadAbortHolder)); ;
            taskThreadAbortHolder.ThreadForTask = workerThread;
                
            try
            {
                lock (TaskThreadChangeLock)
                {
                    TaskHolder = taskThreadAbortHolder;
                    TaskThreadCurrent = workerThread;
                    RestartCurrentTask(workerThread);//.Start();
                }
            }
            finally
            {
                workerThread.Join();
                lock (TaskThreadChangeLock)
                {
                    if (TaskThreadCurrent == workerThread)
                    {
                        TaskThreadCurrent = null;
                    }
                    if (TaskHolder == taskThreadAbortHolder)
                    {
                        TaskHolder = null;
                    }
                }
            }
        }

        private void Tick0(TASK evt, TaskThreadHolder taskThreadAbortHolder)
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
                    taskThreadAbortHolder.TaskAbortable = true;
                    abortable = true;
                    NoExceptions<bool>(taskThreadAbortHolder.TaskStart.Set);
                    if (TaskThreadCurrent != Thread.CurrentThread)
                    {
                        WriteLine("ERROR: Not on CurrentThread!!!!");
                    }
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
            control = control ?? new ThreadControl();
            var ctrl = control != null;
#if TRACKING_LOCK
            lock (BusyTrackingLock)
#endif
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
                }
                DoInteruptably(ref this.TaskThreadCurrent, NamedTask(name, evt), maxTime, control);
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
                            RestartCurrentTask(threadPlace);//.Start();
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

        public void AbortCurrentOperation()
        {
            if (TaskThreadCurrent==null)
            {
                if (TaskHolder==null)
                {
                    WriteLine("No Currents to Abort");
                    return;
                }
                TaskThreadCurrent = TaskHolder.ThreadForTask;
            }
            ToCurrentTaskThread("AbortCurrentOperation", TaskThreadChangeLock, TaskHolder, TaskThreadCurrent.Abort);
            Start0();
        }

        public void ToCurrentTaskThread(String named, object changeLock, TaskThreadHolder taskThreadHolder, ThreadStart taskThreadAbort)
        {
            var TaskThread = taskThreadHolder.ThreadForTask;
            if (Thread.CurrentThread == TaskThread)
            {
                VeryBad(named + ": NoSuicide! " + INFO);
                return;
            }
            
            if (null == TaskThread)
            {
                VeryBad(named + ": No actual TaskThread! " + INFO);
                return;
            }

            bool exitMonitor = true;
            if (!System.Threading.Monitor.TryEnter(changeLock, TimeSpan.FromSeconds(5)))
            {
                exitMonitor = false;
                VeryBad(named + ": cannot getTaskThreadChangeLock! " + INFO);
            }
            try
            {
                if (!TaskThread.IsAlive)
                {
                    VeryBad(named + ": TaskThread Not Alive ! " + INFO);
                    return;
                }
                try
                {
                    if (!taskThreadHolder.WaitUntilStarted())
                    {
                        VeryBad("!IsCurrentTaskStarted");
                    }
                    WriteLine(named + "! " + INFO);
                    lock (changeLock)
                    {
                        if (taskThreadHolder.TaskAbortable)
                        {
                            AbortRequested = true;
                            lock (OnFinnaly)
                            {
                                taskThreadAbort();
                            }
                        }
                    }
                    //TaskThread.Suspend();
                    problems = true;
                }
                catch (Exception e)
                {
                    VeryBad("ERROR: " + e);
                }
            }
            finally
            {
                if (exitMonitor) System.Threading.Monitor.Exit(changeLock);
            }
        }
        /*
        private TaskThreadKit GetTaskThreadKit(Thread taskThread)
        {
            TaskThreadKit taskThreadKit;
            if(!TaskThreadAbortKit.TryGetValue(taskThread,out taskThreadKit))
            {
                taskThreadKit = new TaskThreadKit();
                taskThreadKit.ThreadForTask = taskThread;
                taskThreadKit.TaskStart = new AutoResetEvent(false);
            }
            return taskThreadKit;
        }
        */
        public void RestartCurrentTask(Thread TaskThread)
        {
            NoExceptions(() => RestartCurrentTask0(TaskThread));            
        }
        public void RestartCurrentTask0(Thread TaskThread)
        {
            switch (TaskThread.ThreadState)
            {
                case ThreadState.Running:
                    return;
                    break;
                case ThreadState.StopRequested:
                    break;
                case ThreadState.SuspendRequested:
                    break;
                case ThreadState.Background:
                    return;
                    break;
                case ThreadState.Unstarted:
                    TaskThread.Start();
                    return;
                    break;
                case ThreadState.Stopped:
                    break;
                case ThreadState.WaitSleepJoin:
                    break;
                case ThreadState.Suspended:
                    TaskThread.Resume();
                    return;
                    break;
                case ThreadState.AbortRequested:
                    break;
                case ThreadState.Aborted:
                    if (TaskThread==Thread.CurrentThread)
                    {
                        Thread.ResetAbort();
                        return;
                    }

                    break;
                default:
                    throw new ArgumentOutOfRangeException();
            }
            VeryBad("RestartCurrentTask: " + new Exception().StackTrace + "\n " + TaskThread);
            TaskThread.Resume();
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

        private void VeryBad(String action)
        {
            action = CreateMessage(action);
            WriteLine00(action);
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
            if (!s.StartsWith("..\n[")) s = "..\n[TASK-" + Name.Replace(" ", "-") + "] " + s;
            
            s = DLRConsole.SafeFormat(s, parms);
            if (s.Contains(INFO))
            {
                var str = ToDebugString(true);
                s = s.Replace(INFO, str + "\n...");
            }
            return s;
        }

        public void Enqueue(TASK evt)
        {
            Enqueue(DLRConsole.FindCallerInStack(null, null, true), evt);
        }
        public void Enqueue0(TASK evt)
        {
            if (IsDisposing) return;
            //TestLock(BusyTrackingLock);
#if TRACKING_LOCK
            lock (BusyTrackingLock)
#endif
            {
                ExpectedTodo++;
            }
            if (_noQueue)
            {
                DoNow(evt);
                return;
            }
            TestLock(EventQueueLock);
            lock (EventQueueLock)
            {
                EventQueue.AddLast(evt);
            }
            Set();
        }

        public void Enqueue(String named, TASK evt)
        {
            Enqueue0(NamedTask(named, evt));
        }

        public void AddFirst(String named, TASK evt)
        {
            AddFirst0(NamedTask(named, evt));
        }

        public void AddFirst(TASK evt)
        {
            AddFirst(DLRConsole.FindCallerInStack(null, null, true), evt);
        }
        public void AddFirst0(TASK evt)
        {
            if (IsDisposing) return;
#if TRACKING_LOCK
            lock(BusyTrackingLock)
#endif
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


        private void TrySetThreadName(string s)
        {
            if (s == null)
            {
                return;
            }
            lock (DebugStringLock)
            {
                if (WaitingString.Length > 0)
                {
                    WaitingString = s.Trim() + "\n" + WaitingString;
                }
                else
                {
                    WaitingString = s;
                }
                if (!String.IsNullOrEmpty(s)) LastOpString = s;
            }
        }

        public bool InvokeJoin(string s, int millisecondsTimeout)
        {
            if (IsTaskThread)
            {
                return true;
            }
            return InvokeJoin(s, millisecondsTimeout, null, null);
        }
        public bool InvokeJoin(string s, int millisecondsTimeout, ThreadStart pretask, ThreadStart postask)
        {
            if (IsTaskThread)
            {
                CallNow(pretask, "pre" , s);
                CallNow(postask, "post" , s);
                return true;
            }
            bool success = false;
            WriteLine("InvokeJoin Cross-Thread: " + s);
            TrySetThreadName(s);
            AutoResetEvent are = new AutoResetEvent(false);
            ThreadStart TJ = (
                                 () =>
                                     {
                                         CallNow(pretask, "pre ", s);
                                         CallNow(() => NoExceptions<bool>(are.Set), "setter ", s);
                                     });
            if (!WasStartCalled)
            {
                VeryBad("ERROR in InvokeJoin " + s + " !WasStartCalled ");
                Start();
            }
            if (IsTaskThread)
            {
                TJ();
            }
            else
            {
                Enqueue(TJ);
            }
            if (millisecondsTimeout <= 0)
            {
                TimeSpan ticktock = TimeSpan.FromMinutes(1);
                while (!success)
                {
                    if (IsDisposing) return true;
                    try
                    {
                        success = are.WaitOne(ticktock);
                    }
                    catch (Exception e)
                    {
                        VeryBad("ERROR in InvokeJoin/While " + s + " " + e);
                    }
                    if (!success)
                    {
                        WriteLine("# ticktock " + GetTimeString(ticktock) + ": " + s + " for " + ToString() + " in " +
                                  Thread.CurrentThread);
                    }
                }
            }
            else
            {
                try
                {
                    success = are.WaitOne(millisecondsTimeout);
                }
                catch (Exception e)
                {
                    VeryBad("ERROR in InvokeJoin/WaitOne " + s + " " + e);
                }
            }
            if (!success)
            {
                 WriteLine("ERROR! TIMOUT " + GetTimeString(TimeSpan.FromMilliseconds(millisecondsTimeout)) + ": " + s + " for " + ToString() + " in " + Thread.CurrentThread);
            }
            CallNow(postask, "post", s);
            PopDebugString(s);
            return success;
        }

        protected bool IsTaskThread
        {
            get
            {
                Thread currentThread = Thread.CurrentThread;
                return currentThread == this.StackerThread || currentThread == TaskThreadCurrent;
            }
        }

        private void PopDebugString(string s)
        {
            lock (DebugStringLock)
            {
                if (WaitingString.Contains("\n" + s))
                {
                    WaitingString = WaitingString.Replace("\n" + s, "");
                }
            }
        }

        private void CallNow(ThreadStart task1, string n, string s)
        {
            if (task1 == null) return;
            var ss = n + " " + s;
          //  TrySetThreadName(ss);
            try
            {
                task1();
            }
            catch (Exception e)
            {
                VeryBad("ERROR in CallNow/" + ss + " " + e);
            }
            finally
            {
        //        PopDebugString(ss);
            }
        }

        public void RunTaskSyncronously(TASK action, string name, TimeSpan maxTime)
        {
            // Add the worker to the Queue
            Enqueue(() => MakeSyncronousTask(action, name, maxTime));
        }

        public void MakeSyncronousTask(TASK action, string name, TimeSpan maxTime)
        {
            EventWaitHandle IsComplete = new EventWaitHandle(false, EventResetMode.ManualReset);
            Thread t = new Thread(CreateTask(action, name, null, IsComplete));
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

        public void CreateTask(TaskType taskType, string name, TASK action, bool blockUntilComplete)
        {
            ThreadControl threadControl =  new ThreadControl();

            switch(taskType)
            {
                case TaskType.DoForground:
                    break;
                case TaskType.InteruptAndReplaceCurrentTaskAndAllQueued:
                    break;
                case TaskType.InteruptAndReplaceCurrentTask:
                    break;
                case TaskType.DoSoonAsCurrentTaskIsCompleteButBeforeAllOtherTasks:
                    break;
                case TaskType.DoAfterAllPreviousTasksComplete:
                    break;
                default:
                    throw new ArgumentOutOfRangeException("taskType");
            }
            if (blockUntilComplete)
            {
                threadControl.WaitUntilComplete();
            }
        }

        public void DestroyAllCurrentTasks(bool clearQueue)
        {
            throw new NotImplementedException();
        }

        public TASK CreateTask(TASK action, string name,EventWaitHandle isStarted, EventWaitHandle isComplete)
        {
            TaskQueueHandler handler = this;
            TASK tr = (() =>
                           {
                               Thread self = Thread.CurrentThread;
                               try
                               {
                                   lock (InteruptableThreads) InteruptableThreads.Add(self);
                                   try
                                   {
                                       handler.TrySetThreadName(name);
                                       if (isStarted != null) NoExceptions<bool>(isStarted.Set);
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
                           });// { Name = name };
            return tr;
        }

        private void NoExceptions(TASK func)
        {
            try
            {
                if (IsDisposing)
                {
                    WriteLine("IsDisposing NoExceptions? ");
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

        private void NonAbortedly(TASK func)
        {
            if (IsDisposing)
            {
                WriteLine("IsDisposing NonAbortedly?!");
            }
            Thread TaskThread = this.TaskThreadCurrent;

            if (TaskThread != Thread.CurrentThread)
            {
                func();
                return;
            }
            bool wasAbortable;

            lock (TaskThreadChangeLock)
            {
                wasAbortable = abortable;
                TaskThread = Thread.CurrentThread;
                abortable = false;
            }
            try
            {
                func();
            }
            finally
            {
                lock (TaskThreadChangeLock)
                {
                    if (TaskThread == null) TaskThread = Thread.CurrentThread;
                    abortable = wasAbortable;
                }
                this.TaskThreadCurrent = TaskThread;
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
                VeryBad("" + e);
                return default(T);
            }
        }

        private void NoException(ThreadStart func)
        {
            if (func == null) return;
            try
            {
                if (IsDisposing)
                {
                    WriteLine("IsDisposing");
                }
                func.Invoke();
            }
            catch (Exception e)
            {
                VeryBad("" + e);
            }
        }

        public void RemoveThread(Thread thread)
        {
        }


        public static TimeSpan TimeProcess(string named, Action action)
        {
            //long frequency = Stopwatch.Frequency;
            //Console.WriteLine(" Timer frequency in ticks per second = {0}", frequency);

            Stopwatch sw = Stopwatch.StartNew();
            TimeSpan used;
            try
            {
                try
                {
                    action();
                }
                catch (Exception ex)
                {
                    DLRConsole.DebugWriteLine("" + named + " threw " + ex);
                    throw;
                }
            }
            finally
            {
                sw.Stop();
                used = sw.Elapsed;
                if (named != null)
                {
                    named += " TIME: " + GetTimeString(used);
                    if (!Stopwatch.IsHighResolution)
                        named += " (non high resolution timer)";
                    DLRConsole.DebugWriteLine(named);
                }
            }
            return used;
        }
    }

    public class TaskThreadHolder
    {
        public TaskThreadHolder()
        {
            TaskStart = new AutoResetEvent(false);
        }
        public EventWaitHandle TaskStart;
        public bool TaskAbortable;
        public Thread ThreadForTask;

        public bool WaitUntilStarted()
        {
            if (TaskAbortable) return true;
            bool b = TaskStart.WaitOne(TimeSpan.FromSeconds(5));
            if (b) TaskAbortable = true;
            return b;
        }
    }

    public enum TaskType
    {
        DoForground,
        InteruptAndReplaceCurrentTaskAndAllQueued,
        InteruptAndReplaceCurrentTask,
        DoSoonAsCurrentTaskIsCompleteButBeforeAllOtherTasks,
        DoAfterAllPreviousTasksComplete,
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


        public ThreadControl()
        {
            ManualResetEvent newManualResetEvent = new ManualResetEvent(false);
            TaskComplete = newManualResetEvent;
        }
        public ThreadControl(EventWaitHandle onComplete)
        {
            TaskComplete = onComplete;
        }
        public ThreadControl(ThreadControl parent)
        {
            TaskStart = parent.TaskStart;
            TaskEnded = parent.TaskEnded;
            TaskComplete = parent.TaskComplete;
            parent.AbortRaised += (c, e) => InvokeAbortRaised(e);
            parent.InterruptRaised += (c, e) => InvokeInterruptRaised(e);
            parent.AbortOrInteruptedRaised += (c, e) => InvokeAbortOrInteruptedRaised(e);
            parent.ExceptionRaised += (c, e) => InvokeExceptionRaised(e);
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