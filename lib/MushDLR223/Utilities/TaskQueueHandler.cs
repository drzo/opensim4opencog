using System;
using System.Collections.Generic;
using System.Threading;
using MushDLR223.ScriptEngines;

namespace MushDLR223.Utilities
{
    public class TaskQueueHandler : IDisposable
    {
        static public HashSet<TaskQueueHandler> TaskQueueHandlers = new HashSet<TaskQueueHandler>();
        static public readonly TimeSpan PING_INTERVAL = new TimeSpan(0, 0, 0, 30); //30 seconds
        static public readonly TimeSpan MAX_PING_WAIT = new TimeSpan(0, 1, 1, 2);  // ping wiat time should be less than 2 seconds when going in
        private Thread EventQueueHandler;
        readonly private Thread EventQueuePing;
        readonly private string Name;
        readonly TimeSpan WAIT_AFTER;
        private ulong TotalComplete = 0;
        private ulong TotalStarted = 0;
        private ulong TotalFailures = 0;

        private ulong StartedSinceLastPing = 0;
        private ulong CompletedSinceLastPing = 0;
        public bool Busy;
        private ulong GoodPings = 0;
        private ulong LatePings = 0;
        public bool IsDisposing = false;
        public string WaitingString = "";
        private readonly object WaitingStringLock = new object();
        readonly object WaitingPingLock = new object();
        private OutputDelegate debugOutput = TextFilter.DEVNULL;
        bool WaitingOnPing = false;
        public bool IsRunning
        {
            get
            {
                if (IsDisposing) return false;
                return EventQueueHandler != null && EventQueueHandler.IsAlive;
            }
        }
        public bool SimplyLoopNoWait = false;
        readonly object EventQueueLock = new object();
        readonly AutoResetEvent WaitingOn = new AutoResetEvent(false);
        readonly LinkedList<ThreadStart> EventQueue = new LinkedList<ThreadStart>();
        public bool DebugQueue
        {
            get { return problems || debugRequested; }
            set { debugRequested = value; }
        }

        private bool debugRequested = false;
        private bool problems = false;

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
                MaxPerOperation = maxPerOperation;

                if (msWaitBetween.TotalMilliseconds < 10) msWaitBetween = TimeSpan.FromMilliseconds(10);
                // max ten minutes
                if (msWaitBetween.TotalMinutes > 10) msWaitBetween = TimeSpan.FromMinutes(10);
                WAIT_AFTER = msWaitBetween;
                EventQueuePing = new Thread(EventQueue_Ping) { Name = str + " debug", Priority = ThreadPriority.Lowest };
            }
            if (autoStart) Start();
        }


        public void Start()
        {
            if (EventQueueHandler==null) EventQueueHandler = new Thread(EventQueue_Handler);
            if (!EventQueueHandler.IsAlive)
            {
                EventQueueHandler.Name = Name + " worker";
                EventQueueHandler.Priority = ThreadPriority.BelowNormal;
                EventQueueHandler.Start();
            }
            if (EventQueuePing != null && !EventQueuePing.IsAlive) EventQueuePing.Start();
            debugOutput = DLRConsole.DebugWriteLine;
        }

        private bool _noQueue;
        public bool NoQueue
        {
            get
            {
                lock (EventQueue)
                {
                    return _noQueue;
                }
            }
            set
            {
                lock (EventQueue)
                {
                    if (_noQueue == value) return;
                    _noQueue = value;
                }
                // release waiters
                Set();
            }
        }
        public override string ToString()
        {
            return ToDebugString();
        }

        public string ToDebugString()
        {
            string WaitingS = "Not waiting";
            lock (WaitingStringLock)
            {
                if (WaitingString.Length >= 0)
                {
                    WaitingS = "WaitingOn: " + WaitingString;
                }
            }

            string extraMesage =
                Name
                + string.Format(" TODO = {0} ", todo)
                + string.Format(" TOTALS = {0}/{1}/{2} ", TotalComplete, TotalStarted, GetTimeString(OperationTime))
                + string.Format(" PINGED = {0}/{1}/{2} ", CompletedSinceLastPing, StartedSinceLastPing, GetTimeString(PingLag))
                ;

            

            if (IsDisposing) WaitingS += " IsDisposing";
            if (!IsRunning) WaitingS += " NOT RUNNING";
            return String.Format(
                "{0} {1} {2} {3} {4}",
                Busy ? "Busy" : "Idle",
                extraMesage,
                TotalFailures > 0 ? TotalFailures + " failures " : "",
                _noQueue ? "NoQueue" : "",
                WaitingS);
        }

        public void Dispose()
        {
            if (IsDisposing) return;
            IsDisposing = true;
            lock (TaskQueueHandlers)
                TaskQueueHandlers.Remove(this);
            if (EventQueuePing != null) EventQueuePing.Abort();
            if (EventQueueHandler != null && EventQueueHandler.IsAlive) EventQueueHandler.Abort();
            try
            {
                WaitingOn.Set();
                WaitingOn.Close();
            }
            catch (ObjectDisposedException) { }
        }

        readonly ThreadStart NOTHING = default(ThreadStart);
        private DateTime BusyStart = DateTime.UtcNow;
        private DateTime BusyEnd;
        private TimeSpan OperationTime;
        private DateTime PingStart;
        private TimeSpan PingLag;
        private ulong todo;
        private readonly TimeSpan MaxPerOperation;

        void EventQueue_Handler()
        {
            while (!(IsDisposing))
            {
                Busy = false;

                ThreadStart evt;
                lock (EventQueueLock)
                {
                    if (EventQueue.Count > 0)
                    {
                        evt = EventQueue.First.Value;
                        EventQueue.RemoveFirst();
                    }
                    else
                    {
                        evt = NOTHING;
                    }
                }

                if (evt != null && evt != NOTHING)
                {
                    DoNow(evt);
                    if (WAIT_AFTER.TotalMilliseconds > 1) Thread.Sleep(WAIT_AFTER);
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
                        Thread.Sleep(WAIT_AFTER);
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
            WaitingOn.WaitOne();// Thread.Sleep(100);
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

        void EventQueue_Ping()
        {

            while (!(IsDisposing))
            {
                ulong startedBeforeWait = TotalStarted;
                ulong completeBeforeWait = TotalComplete;
                Thread.Sleep(PING_INTERVAL);
                if (EventQueueHandler != null)
                    if (!EventQueueHandler.IsAlive)
                    {
                        problems = true;
                        WriteLine("ERROR EventQueueHandler DEAD! " + this.ToDebugString());
                        continue;
                    }
                if (_noQueue) continue;
                lock (WaitingPingLock)
                {
                    todo = (ulong) EventQueue.Count;
                    if (Busy)
                    {
                        TimeSpan t = DateTime.UtcNow - BusyStart;
                        if (t > MaxPerOperation)
                        {
                            problems = true;
                            WriteLine("BUSY LONGER THAN {0} time = {1} " + ToDebugString(),
                                      GetTimeString(MaxPerOperation),
                                      GetTimeString(t));
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
                                    WriteLine("NOTHING DONE IN time = {0} " + ToDebugString(), GetTimeString(t));
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
                                    WriteLine("PINGER WAITING LONGER THAN {0} time = {1} " + ToDebugString(),
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
            lock (WaitingPingLock)
            {
                // todo can this ever happen?
                if (!WaitingOnPing) return;
            }
            PingLag = DateTime.UtcNow - PingStart;
            TotalStarted--;
            StartedSinceLastPing--;
            if (PingLag <= MAX_PING_WAIT)
            {
                GoodPings++;
                if (todo > 0)
                    if (DebugQueue)
                        WriteLine("PONG: {0} {1}", GetTimeString(PingLag), ToDebugString());
                LatePings = 0;
                problems = false;
            }
            else
            {
                LatePings++;
                WriteLine("LATE PONG: {0} {1} {2}", LatePings, GetTimeString(PingLag), ToDebugString());
                GoodPings = 0;
                problems = true;
            }
            lock (WaitingPingLock)
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
        private void DoNow(ThreadStart evt)
        {
            if (IsDisposing) return;
            Busy = true;
            BusyStart = DateTime.UtcNow;
            TotalStarted++;
            try
            {
                todo--;
                StartedSinceLastPing++;
                evt();
                CompletedSinceLastPing++;
                TotalComplete++;
                Busy = false;
            }
            catch (Exception e)
            {
                TotalFailures++;
                WriteLine("ERROR! {0} was {1} in {2}", ToDebugString(), e, Thread.CurrentThread);
            }
            finally
            {
                BusyEnd = DateTime.UtcNow;
                OperationTime = BusyEnd.Subtract(BusyStart);
                //TotalStarted++;
                Busy = false;
            }
        }

        public void WriteLine(string s, params object[] parms)
        {
            s = "\n[TASK " + Name + "] " + s;            
            if (debugOutput != DLRConsole.DebugWriteLine)
            {
                DLRConsole.DebugWriteLine(s, parms);
            }
            if (debugOutput != null) debugOutput(s, parms);
        }

        public void Enqueue(ThreadStart evt)
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

        public void AddFirst(ThreadStart evt)
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
            lock(WaitingStringLock)
            {
                if (WaitingString.Length>0)
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
                                lock (WaitingStringLock)
                                {
                                    if (WaitingString.Contains("\n" + s))
                                    {
                                        WaitingString = WaitingString.Replace("\n" + s, "");
                                    }
                                }
                                lock (WaitingStringLock)
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
            bool success  = are.WaitOne(millisecondsTimeout);
            if (!success)
            {
                WriteLine("ERROR! TIMOUT " + s + " for " + ToString() + " in " + Thread.CurrentThread);
            }
            {
                lock (WaitingStringLock)
                {
                    if (WaitingString.Contains("\n" + s))
                    {
                        WaitingString = WaitingString.Replace("\n" + s, "");
                    }
                }
            }
            return success;
        }
    }
}