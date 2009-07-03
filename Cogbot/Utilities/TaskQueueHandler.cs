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
        ulong sequence = 0;
        private ulong GoodPings = 0;
        private bool Busy;
        private ulong LastBusy = 0;
        readonly int WAIT_AFTER;
        readonly LinkedList<ThreadStart> EventQueue = new LinkedList<ThreadStart>();
        public TaskQueueHandler(string str, int msWaitBetween)
        {
            Name = str;
            WAIT_AFTER = msWaitBetween;
            EventQueueHandler = new Thread(EventQueue_Handler) {Name = str + " worker"};
            EventQueueHandler.Start();
            EventQueuePing = new Thread(EventQueue_Ping) {Name = str + " timer"};
            EventQueuePing.Start();
        }

        readonly ThreadStart NOTHING = default(ThreadStart);

        void EventQueue_Handler()
        {
            while (true)
            {
                ThreadStart evt = NOTHING;
                // int eventQueueCountLast = 0;
                int eventQueueCount;
                lock (EventQueue)
                {
                    eventQueueCount = EventQueue.Count;
                    if (eventQueueCount > 0)
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
                    Thread.Sleep(WAIT_AFTER);
                }
                else
                {
                    Thread.Sleep(200);
                }
                Busy = false;
            }
        }

        void EventQueue_Ping()
        {
            while (true)
            {
                Thread.Sleep(PING_TIME);
                if (Busy)
                {
                    if (LastBusy == sequence)
                    {
                        Console.WriteLine("Task Longer than " + PING_TIME.TotalSeconds + " secs");
                    }
                    LastBusy = sequence;
                }
                DateTime oldnow = DateTime.UtcNow;
                AddLast(() =>
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
                        Console.WriteLine("[{0}] LAG {1} secs after {2} GoodPing(s)", Name, timeSpan.TotalSeconds, GoodPings);
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
            }
        }
        public void AddLast(ThreadStart evt)
        {
            lock (EventQueue)
            {
                EventQueue.AddLast(evt);
            }
        }
        public void AddFirst(ThreadStart evt)
        {
            lock (EventQueue)
            {
                EventQueue.AddFirst(evt);
            }
        }
    }
}