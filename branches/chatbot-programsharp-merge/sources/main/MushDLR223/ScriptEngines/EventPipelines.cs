using System;
using System.Collections.Generic;
using System.Text;
using MushDLR223.ScriptEngines;
using MushDLR223.Utilities;
#if (COGBOT_LIBOMV || USE_STHREADS)
using ThreadPoolUtil;
using Thread = ThreadPoolUtil.Thread;
using ThreadPool = ThreadPoolUtil.ThreadPool;
using Monitor = ThreadPoolUtil.Monitor;
#endif
using System.Threading;

namespace MushDLR223.ScriptEngines
{
    public class NamedPrefixThing
    {
        public string prefix;
        public Func<string> getNamed;
        public NamedPrefixThing(string named, Func<string> client)
        {
            prefix = named;
            getNamed = client;
        }
        public override string ToString()
        {
            return prefix + " " + getNamed();
        }
    }

    public class SimEventFilterSubscriber : SimEventSubscriber
    {
        private SimEventSubscriber Next;
        public bool DefaultUse = true;
        // If a fast shutoff
        public bool hideAllEvents = false;
        // Events to always send
        public ListAsSet<String> Always = new ListAsSet<string>();
        // Events to never send
        public ListAsSet<String> Never = new ListAsSet<string>();
        public ListAsSet<String> KnownSet = new ListAsSet<string>();

        public SimEventFilterSubscriber(SimEventSubscriber next)
        {
            Next = next;
            EventsEnabled = true;
        }

        public SimEventFilterSubscriber(SimEventSubscriber next, bool enabled)
        {
            Next = next;
            EventsEnabled = enabled;
        }

        #region Implementation of SimEventSubscriber

        public void OnEvent(CogbotEvent evt)
        {
            if (!EventsEnabled || Skipped(evt)) return;
            Next.OnEvent(evt);
        }

        private bool Skipped(CogbotEvent evt)
        {
            return (SkippedVerb(evt.Verb) || SkippedVerb(evt.EventType1.ToString()) || SkippedVerb(evt.EventName));
        }

        private bool SkippedVerb(string verb)
        {
            KnownSet.Add(verb);
            if (Always.Contains(verb)) return false;
            if (Never.Contains(verb)) return true;
            return !DefaultUse;
        }

        public void Dispose()
        {
            EventsEnabled = false;
            Next.Dispose();
            Next = null;
        }

        public bool EventsEnabled { get; set; }

        #endregion
    }


    public interface SimEventSubscriber
    {
        // fired when SendEvent is invoke and this subscriber is downstream in the pipeline
        void OnEvent(CogbotEvent evt);
        void Dispose();
        bool EventsEnabled { get; set; }
    }

    public interface SimEventPublisher
    {
        // this publisher will SendEvent to some SimEventPipeline after the Event params have been casted to the correct types
        CogbotEvent CreateEvent(SimEventType type, string eventName, params object[] args);
        // this object will propogate the event AS-IS 
        void SendEvent(CogbotEvent evt);
        void AddSubscriber(SimEventSubscriber sub);
        void RemoveSubscriber(SimEventSubscriber sub);
        void Dispose();
    }

    public class SimEventMulticastPipeline : SimEventSubscriber, SimEventPublisher
    {
        #region SimEventMulticastPipeline Members

        private object Publisher;
        readonly List<SimEventSubscriber> subscribers = new List<SimEventSubscriber>();
        readonly TaskQueueHandler taskQueue;
        public SimEventMulticastPipeline(object publisher)
        {
            Publisher = publisher;
            taskQueue = new TaskQueueHandler(publisher as ContextualSingleton, new NamedPrefixThing("SimEventMulticastPipeline ", publisher.ToString));
            EventsEnabled = true;
        }

        #endregion

        #region SimEventSubscriber Members

        public void Dispose()
        {
            EventsEnabled = false;
            taskQueue.Dispose();
            foreach (SimEventSubscriber subscriber in GetSubscribers())
            {
                subscriber.Dispose();
            }
        }

        private IEnumerable<SimEventSubscriber> GetSubscribers()
        {
            lock (subscribers)
            {
                return new List<SimEventSubscriber>(subscribers);
            }
        }

        #endregion

        #region SimEventPublisher Members

        public CogbotEvent CreateEvent(SimEventType type, string eventName, params object[] args)
        {
            return new ACogbotEvent(Publisher, type, eventName, args);
        }

        public static bool UseQueue = false;
        public static bool ExecSynchronous = true;
        [ThreadStatic]
        CogbotEvent LastEvent = null;
        CogbotEvent LastEventAW = null;
        public bool EventsEnabled { get; set; }

        // this pipelike will fire OnEvent to the subscriber list 
        public void SendEvent(CogbotEvent simObjectEvent)
        {
            if (!EventsEnabled) return;
            if (LastEvent != null && simObjectEvent.SameAs(LastEvent))
            {
                return;
            }
            if (LastEventAW != null && simObjectEvent.SameAs(LastEventAW))
            {
                return;
            }
            LastEvent = simObjectEvent;
            LastEventAW = simObjectEvent;

            ThreadStart start = () => SendNow(simObjectEvent);

            if (ExecSynchronous)
            {
                start();
                return;
            }
            if (!UseQueue)
            {
                new Thread(start).Start();
            }
            else taskQueue.Enqueue(start);
        }

        private void SendNow(CogbotEvent simObjectEvent)
        {
            foreach (SimEventSubscriber subscriber in GetSubscribers())
            {
                SimEventSubscriber sub = subscriber;
                try
                {
                    simObjectEvent.SendTo(sub);
                }
                catch (Exception e)
                {
                    DLRConsole.DebugWriteLine(e);
                }
            }
        }

        #endregion

        #region SimEventSubscriber Members

        public void OnEvent(CogbotEvent simObjectEvent)
        {
            if (!EventsEnabled) return;
            SendNow(simObjectEvent);
        }

        #endregion

        #region SimEventPublisher Members


        public void AddSubscriber(SimEventSubscriber sub)
        {
            if (sub == null) throw new NullReferenceException();
            lock (subscribers) if (!subscribers.Contains(sub))
                    subscribers.Add(sub);
        }

        public void RemoveSubscriber(SimEventSubscriber sub)
        {
            if (sub == null) throw new NullReferenceException();
            lock (subscribers) subscribers.Remove(sub);
        }

        #endregion
    }
}
