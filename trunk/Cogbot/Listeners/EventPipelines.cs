using System;
using System.Collections.Generic;
using System.Text;
using cogbot.TheOpenSims;
using cogbot.Utilities;
using MushDLR223.ScriptEngines;
using MushDLR223.Utilities;
using OpenMetaverse;
using System.Threading;

namespace cogbot.Listeners
{
    public class SimEventFilterSubscriber : SimEventSubscriber
    {
        private SimEventSubscriber Next;
        public bool DefaultUse = true;
        // Events to always send
        public List<String> Always = new List<string>();
        // Events to never send
        public List<String> Never = new List<string>();
        public List<String> KnownSet = new List<string>();

        public SimEventFilterSubscriber(SimEventSubscriber next)
        {
            Next = next;
        }
        #region Implementation of SimEventSubscriber

        public void OnEvent(SimObjectEvent evt)
        {
            if (Skipped(evt)) return;
            Next.OnEvent(evt);
        }

        private bool Skipped(SimObjectEvent evt)
        {
            return (SkippedVerb(evt.Verb) || SkippedVerb(evt.EventType.ToString()) || SkippedVerb(evt.EventName));
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
            Next.Dispose();
            Next = null;
        }

        #endregion
    }

    public class SimEventTextSubscriber : SimEventSubscriber
    {
        readonly BotClient From;
        readonly OutputDelegate textForm;
        public SimEventTextSubscriber(OutputDelegate _text, BotClient from)
        {
            From = from;
            textForm = _text;
        }

        #region SimEventSubscriber Members

        void SimEventSubscriber.OnEvent(SimObjectEvent evt)
        {
            if (evt.EventType == SimEventType.DATA_UPDATE) return;

            if (evt.EventType == SimEventType.EFFECT)
            {
                if (evt.Verb == "LookAtType-Idle") return;
                if (evt.Verb == "LookAtType-FreeLook") return;
            }
            String eventName = evt.GetVerb();
            object[] args = evt.GetArgs();

            String msg = "["+ From.GetName() + ": " + eventName.ToLower()+"]";
            int start = 0;
            if (args.Length > 1)
            {
                if (args[0] is Simulator)
                {
                   // start = 1;
                }
            }
            for (int i = start; i < args.Length; i++)
            {
                msg += " ";
                msg += From.argString(args[i]);
            }
            if (msg.Contains("Transfer failed with status code")) return;
            msg += "";
            
            textForm(msg);
        }

        void SimEventSubscriber.Dispose()
        {
            textForm("SimEventTextSubscriber shutdown for " + From);
        }

        #endregion
    }
    public interface SimEventSubscriber
    {
        // fired when SendEvent is invoke and this subscriber is downstream in the pipeline
        void OnEvent(SimObjectEvent evt);
        void Dispose();
    }

    public interface SimEventPublisher
    {
        // this publisher will SendEvent to some SimEventPipeline after the Event params have been casted to the correct types
        SimObjectEvent CreateEvent(SimEventType type,SimEventClass clazz, string eventName, params object[] args);
        // this object will propogate the event AS-IS 
        void SendEvent(SimObjectEvent evt);
        void AddSubscriber(SimEventSubscriber sub);
        void RemoveSubscriber(SimEventSubscriber sub);
        void Dispose();
    }

    public class SimEventMulticastPipeline : SimEventSubscriber, SimEventPublisher
    {
        #region SimEventMulticastPipeline Members

        List<SimEventSubscriber> subscribers = new List<SimEventSubscriber>();
        readonly TaskQueueHandler taskQueue;
        public SimEventMulticastPipeline(string name)
        {
            taskQueue = new TaskQueueHandler("SimEventMulticastPipeline " + name, TimeSpan.FromMilliseconds(1));
        }

        #endregion

        #region SimEventSubscriber Members

        public void Dispose()
        {
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

        public SimObjectEvent CreateEvent(SimEventType type, SimEventClass clazz, string eventName, params object[] args)
        {
            return new SimObjectEvent(type,clazz, eventName, args);
        }

        public static bool UseQueue = false;
        public static bool ExecSynchronous = false;
        SimObjectEvent LastEvent = null;
        // this pipelike will fire OnEvent to the subscriber list 
        public void SendEvent(SimObjectEvent simObjectEvent)
        {
            if (LastEvent != null && simObjectEvent.SameAs(LastEvent))
            {
                return;
            }
            LastEvent = simObjectEvent;
            foreach (SimEventSubscriber subscriber in GetSubscribers())
            {
                SimEventSubscriber sub = subscriber;
                ThreadStart start =()=>
                               {
                                   try
                                   {
                                       simObjectEvent.SendTo(sub);
                                   }
                                   catch (Exception e)
                                   {
                                       DLRConsole.DebugWriteLine(e);
                                   }
                               };
                if (ExecSynchronous)
                {
                    start();
                    return;
                }
                if (UseQueue)
                {
                    new Thread(start).Start();
                }
                else taskQueue.Enqueue(start);                    
                
                
                
            }
        }

        #endregion

        #region SimEventSubscriber Members

        public void OnEvent(SimObjectEvent simObjectEvent)
        {
            foreach (SimEventSubscriber subscriber in GetSubscribers())
            {
                simObjectEvent.SendTo(subscriber);
            }
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
