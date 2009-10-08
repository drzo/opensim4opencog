using System;
using System.Collections.Generic;
using System.Threading;

namespace cogbot.Utilities
{
    public class TaskQueueHandler : IDisposable
    {
        static public HashSet<TaskQueueHandler> TaskQueueHandlers = new HashSet<TaskQueueHandler>();
        readonly private Thread EventQueueHandler;
        readonly private Thread EventQueuePing;
        readonly private string Name;
        static private readonly TimeSpan PING_TIME = new TimeSpan(0, 0, 0, 30); //30 seconds
        private ulong processed = 0;
        ulong sequence = 1;
        ulong failures = 0;
        private ulong GoodPings = 0;
        private bool Busy;
        private ulong LastBusy = 0;
        readonly int WAIT_AFTER;
        public bool IsDisposing = false;
        readonly object EventQueueLock = new object();
        AutoResetEvent WaitingOn = new AutoResetEvent(false);
        readonly LinkedList<ThreadStart> EventQueue = new LinkedList<ThreadStart>();
        public static bool DebugQueue = true;
        public TaskQueueHandler(string str, int msWaitBetween)
        {
            lock (TaskQueueHandlers) TaskQueueHandlers.Add(this);
            Name = str;
            if (msWaitBetween < 1) msWaitBetween = 1;
            WAIT_AFTER = msWaitBetween;
            EventQueueHandler = new Thread(EventQueue_Handler)
                                    {
                                        Name = str + " worker",
                                        Priority = ThreadPriority.BelowNormal
                                    };
            EventQueueHandler.Start();
            //if (DebugQueue)
            {
                EventQueuePing = new Thread(EventQueue_Ping) { Name = str + " debug" };
                EventQueueHandler.Priority = ThreadPriority.Lowest;
                EventQueuePing.Start();
            }
        }

        public override string ToString()
        {
            return String.Format(
                "{0} {1} Todo={2} Complete={3} {4} {5}",
                Busy ? "Busy" : "Idle", Name, EventQueue.Count, processed, failures>0? failures+ " failures ":"", NoQueue ? "NoQueue" : "");
        }

        public void Dispose()
        {
            if (IsDisposing) return;
            IsDisposing = true;
            lock (TaskQueueHandlers)
                TaskQueueHandlers.Remove(this);
            if (EventQueuePing!=null) EventQueuePing.Abort();
            EventQueueHandler.Abort();
            try
            {
                WaitingOn.Set();
                WaitingOn.Close();
            }
            catch (ObjectDisposedException) { }
        }

        readonly ThreadStart NOTHING = default(ThreadStart);
        private DateTime BusyStart;
        public bool NoQueue = false;


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
                Thread.Sleep(PING_TIME);
                if (NoQueue) continue;
                if (Busy || WaitingOnPing)
                {
                    if (LastBusy == sequence)
                    {
                        TimeSpan t = DateTime.UtcNow - BusyStart;
                        if (DebugQueue) Console.WriteLine("\n[TASK {0}] TOOK LONGER THAN {1} secs = {2} in Queue={3}",
                                          Name, PING_TIME.TotalSeconds, t.TotalSeconds, EventQueue.Count);
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
                        // Console.WriteLine("PONG: " + Name + " " + timeSpan.TotalMilliseconds + " ms");
                        GoodPings++;
                    }
                    else
                    {
                        if (DebugQueue) Console.WriteLine("[TASK {0}] {1} secs for {2} after {3} GoodPing(s)",
                                          Name, timeSpan.TotalSeconds, count, GoodPings);
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
                Console.WriteLine("" + e);
            }
            finally
            {
                sequence++;
                Busy = false;
            }
        }

        public void Enqueue(ThreadStart evt)
        {
            if (IsDisposing) return;
            if (NoQueue)
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
    }
}