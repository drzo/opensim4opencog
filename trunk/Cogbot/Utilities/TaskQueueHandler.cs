using System;
using System.Collections.Generic;
using System.Threading;

namespace cogbot.Utilities
{
    public class TaskQueueHandler : IDisposable
    {
        static public HashSet<TaskQueueHandler> TaskQueueHandlers = new HashSet<TaskQueueHandler>();
        private Thread EventQueueHandler;
        readonly private Thread EventQueuePing;
        readonly private string Name;
        static private readonly TimeSpan PING_TIME = new TimeSpan(0, 0, 0, 30); //30 seconds
        private ulong processed = 0;
        ulong sequence = 1;
        ulong failures = 0;
        private ulong GoodPings = 0;
        public bool Busy;
        private ulong LastBusy = 0;
        readonly int WAIT_AFTER;
        public bool IsDisposing = false;
        private string WaitingString = "";
        private object WaitingStringLock = new object();
        public OutputDelegate debugOutput;

        public bool IsRunning
        {
            get
            {
                if (IsDisposing) return false;
                return EventQueueHandler != null && EventQueueHandler.IsAlive;
            }
        }
        readonly object EventQueueLock = new object();
        AutoResetEvent WaitingOn = new AutoResetEvent(false);
        readonly LinkedList<ThreadStart> EventQueue = new LinkedList<ThreadStart>();
        public static bool DebugQueue = true;
        public TaskQueueHandler(string str, int msWaitBetween) : this(str,msWaitBetween,true)
        {

        }

        public TaskQueueHandler(string str, int msWaitBetween, bool autoStart)
        {
            lock (TaskQueueHandlers) TaskQueueHandlers.Add(this);
            Name = str;
            if (msWaitBetween < 1) msWaitBetween = 1;
            WAIT_AFTER = msWaitBetween;
            EventQueuePing = new Thread(EventQueue_Ping) { Name = str + " debug", Priority = ThreadPriority.Lowest };
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
            string WaitingS = "Not waiting";
            lock (WaitingStringLock)
            {
                if (WaitingString.Length>=0)
                {
                    WaitingS = "WaitingOn: " + WaitingString;
                }
            }
            if (IsDisposing) WaitingS += " IsDisposing";
            if (!IsRunning) WaitingS += " NOT RUNNING";
            return String.Format(
                "{0} {1} Todo={2} Complete={3} {4} {5} {6}",
                Busy ? "Busy" : "Idle", 
                Name,
                EventQueue.Count, 
                processed,
                failures > 0 ? failures + " failures " : "",
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
        private DateTime BusyStart;

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
                    if (WAIT_AFTER > 1) Thread.Sleep(WAIT_AFTER);
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
            bool WaitingOnPing = false;
            while (!(IsDisposing))
            {
                if (EventQueueHandler != null)
                    if (!EventQueueHandler.IsAlive)
                    {
                        WriteLine("Dead " + this);
                        Thread.Sleep(PING_TIME);
                        continue;
                    }
                Thread.Sleep(PING_TIME);
                if (_noQueue) continue;
                if (Busy || WaitingOnPing)
                {
                    if (LastBusy == sequence)
                    {
                        TimeSpan t = DateTime.UtcNow - BusyStart;
                        if (DebugQueue) WriteLine("TOOK LONGER THAN {0} secs = {1} in Queue={2}",
                                           PING_TIME.TotalSeconds, t.TotalSeconds, EventQueue.Count);
                    }
                    LastBusy = sequence;
                    continue;
                }
                DateTime oldnow = DateTime.UtcNow;
                int count = EventQueue.Count;
                if (WaitingOnPing)
                {
                    continue;
                }
                WaitingOnPing = true;
                Enqueue(() =>
                {
                    WaitingOnPing = false;
                    if (sequence > 1) sequence--;
                    DateTime now = DateTime.UtcNow;
                    TimeSpan timeSpan = now - oldnow;
                    double secs = timeSpan.TotalSeconds;
                    if (secs < 2)
                    {
                        // WriteLine("PONG: " + Name + " " + timeSpan.TotalMilliseconds + " ms");
                        GoodPings++;
                    }
                    else
                    {
                        if (DebugQueue) WriteLine("{0} secs for {1} after {3} GoodPing(s)",
                                           timeSpan.TotalSeconds, count, GoodPings);
                        GoodPings = 0;
                    }
                }
                    );
            }
            // ReSharper disable FunctionNeverReturns
        }
        // ReSharper restore FunctionNeverReturns
        private void DoNow(ThreadStart evt)
        {
            if (IsDisposing) return;
            Busy = true;
            BusyStart = DateTime.UtcNow;
            sequence++;
            try
            {
                evt();
                processed++;
                Busy = false;
            }
            catch (Exception e)
            {
                failures++;
                WriteLine("ERROR! " + ToString() + " was " + e + " in " + Thread.CurrentThread);
            }
            finally
            {
                sequence++;
                Busy = false;
            }
        }

        public void WriteLine(string s, params object[] parms)
        {
            s = "\n[TASK " + Name + "] " + s;
            if (debugOutput != Console.WriteLine)
            {
                Console.WriteLine(s, parms);
            }
            if (debugOutput != null) debugOutput(s, parms);
        }

        public void Enqueue(ThreadStart evt)
        {
            if (IsDisposing) return;
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