using System;
using System.Collections.Generic;
using System.Threading;

namespace cogbot.Utilities
{
    public class TaskQueueHandler//<TValue>
    {
        readonly private Thread EventQueueHandler;
        readonly private Thread EventQueuePing;
        readonly private string Name;
        static private readonly TimeSpan PING_TIME = new TimeSpan(0,0,0,30); //30 seconds
        private ulong processed = 0;
        ulong sequence = 1;
        private ulong GoodPings = 0;
        private bool Busy;
        private ulong LastBusy = 0;
        readonly int WAIT_AFTER;
        AutoResetEvent WaitingOn = new AutoResetEvent(false);
        readonly LinkedList<ThreadStart> EventQueue = new LinkedList<ThreadStart>();
        public TaskQueueHandler(string str, int msWaitBetween)
        {
            Name = str;
            if (msWaitBetween < 1) msWaitBetween = 1;
            WAIT_AFTER = msWaitBetween;
            EventQueueHandler = new Thread(EventQueue_Handler) {Name = str + " worker"};
            EventQueueHandler.Start();
            EventQueuePing = new Thread(EventQueue_Ping) {Name = str + " timer"};
            EventQueuePing.Start();
        }

        readonly ThreadStart NOTHING = default(ThreadStart);
        private DateTime BusyStart;

        void EventQueue_Handler()
        {
            while (true)
            {
                Busy = false;

                ThreadStart evt;
                lock (EventQueue)
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
                        Console.WriteLine("" + e);
                    }
                    Busy = false;
                    if (WAIT_AFTER > 1) Thread.Sleep(WAIT_AFTER);
                }
                else
                {
                    lock (EventQueue)
                    {
                        if (EventQueue.Count > 0)
                        {
                            continue;
                        }
                        Reset();
                    }
                    WaitOne();
                }
                Busy = false;
            }
        }

        private void WaitOne()
        {
            WaitingOn.WaitOne();// Thread.Sleep(100);
        }

        private void Reset()
        {
            WaitingOn.Reset();
        }
        private void Set()
        {
            WaitingOn.Set();
        }

        public static bool DebugQueue = true;

        void EventQueue_Ping()
        {
            while (true)
            {
                Thread.Sleep(PING_TIME);
                if (Busy)
                {
                    if (LastBusy == sequence)
                    {
                        TimeSpan t = DateTime.UtcNow - BusyStart;
                        if (DebugQueue) Console.WriteLine("\n[TASK {0}] TOOK LONGER THAN {1} secs = {2} in Queue={3}",
                                          Name, PING_TIME.TotalSeconds, t.TotalSeconds, EventQueue.Count);
                    }
                    LastBusy = sequence;
                }
                DateTime oldnow = DateTime.UtcNow;
                int count = EventQueue.Count;
                Enqueue(() =>
                {
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

        public void Enqueue(ThreadStart evt)
        {
            lock (EventQueue)
            {
                EventQueue.AddLast(evt);
                Set();
            }
        }

        public void AddFirst(ThreadStart evt)
        {
            lock (EventQueue)
            {
                EventQueue.AddFirst(evt);
                Set();
            }
        }
    }
}