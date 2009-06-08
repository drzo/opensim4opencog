using System;
using System.Collections.Generic;
using System.Text;
using OpenMetaverse;

namespace cogbot.Listeners
{


    public class SimEvent
    {
        public SimEvent(string name, object[] paramz)
        {
            eventName = name;
            args = paramz;
        }
        string eventName;
        object[] args;
        List<SimEventSubscriber> receiversSent = new List<SimEventSubscriber>();
        SimEvent original = null;

        internal void SendTo(SimEventSubscriber subscriber)
        {
            if (!receiversSent.Contains(subscriber))
            {
                receiversSent.Add(subscriber);
                subscriber.OnEvent(this);
            }
        }

        public override string ToString()
        {
            return "SimEvent " + eventName + " " + ScriptEngines.ScriptEventListener.argsListString(args);
        }
        public string GetName()
        {
            return eventName;
        }
        public object[] GetArgs()
        {
            return args;
        }
    }


    public class SimEventTextSubscriber:SimEventSubscriber
    {
        BotClient From;
        TextForm textForm;
        public SimEventTextSubscriber(TextForm _text, BotClient from)
        {
            From = from;
            textForm = _text;
        }

        #region SimEventSubscriber Members

        void SimEventSubscriber.OnEvent(SimEvent evt)
        {
            String eventName = evt.GetName();
            object[] args = evt.GetArgs();

            String msg = From.Self.Name + ": [" + eventName.ToLower();
            int start = 0;
            if (args.Length > 1)
            {
                if (args[0] is Simulator)
                {
                    start = 1;
                }
            }
            for (int i = start; i < args.Length; i++)
            {
                msg += " ";
                msg += From.argString(args[i]);
            }

            msg += "]";

            textForm.WriteLine(msg);
        }

        void SimEventSubscriber.ShuttingDown()
        {
            textForm.WriteLine("SimEventTextSubscriber shuttdown for " + From);
        }

        #endregion
    }
    public interface SimEventSubscriber
    {
        // fired when SendEvent is invoke and this subscriber is downstream in the pipeline
        void OnEvent(SimEvent evt);
        void ShuttingDown();
    }

    public interface SimEventPublisher
    {
        // this publisher will SendEvent to some SimEventPipeline after the Event params have been casted to the correct types
        SimEvent CreateEvent(string eventName, params object[] args);
        // this object will propogate the event AS-IS 
        void SendEvent(SimEvent evt);
        void AddSubscriber(SimEventSubscriber sub);
    }

    public class SimEventMulticastPipeline : SimEventSubscriber, SimEventPublisher
    {
        #region SimEventMulticastPipeline Members

        List<SimEventSubscriber> subscribers = new List<SimEventSubscriber>();

        public SimEventMulticastPipeline()
        {
        }

        #endregion

        #region SimEventSubscriber Members

        public void ShuttingDown()
        {
            foreach (SimEventSubscriber subscriber in GetSubscribers())
            {
                subscriber.ShuttingDown();
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

        public SimEvent CreateEvent(string eventName, params object[] args)
        {
            return new SimEvent(eventName, args);
        }

        // this pipelike will fire OnEvent to the subscriber list 
        public void SendEvent(SimEvent simEvent)
        {
            foreach (SimEventSubscriber subscriber in GetSubscribers())
            {
                simEvent.SendTo(subscriber);
            }
        }

        #endregion

        #region SimEventSubscriber Members

        public void OnEvent(SimEvent simEvent)
        {
            foreach (SimEventSubscriber subscriber in GetSubscribers())
            {
                simEvent.SendTo(subscriber);
            }
        }

        #endregion

        #region SimEventPublisher Members


        public void AddSubscriber(SimEventSubscriber sub)
        {
            lock (subscribers) if (!subscribers.Contains(sub))
                subscribers.Add(sub);
        }

        #endregion
    }
}
