using System;
using System.Collections.Generic;
using cogbot.Listeners;
using OpenMetaverse;
using PathSystem3D.Navigation;

namespace cogbot.TheOpenSims
{
    public enum SimEventType
    {
        UNKNOWN,
        EFFECT,
        SOCIAL,
        SIT,
        ANIM,
        TOUCH,
        MOVEMENT,
        LISP
    }
    public enum SimEventStatus
    {
        _UNKNOWN,
        Start,
        Once,
        Stop
    }

    internal class SimEventDesc
    {
    }

    public class SimObjectEvent : BotMentalAspect
    {
        private static long serialCount = DateTime.UtcNow.Ticks;
        public readonly long serial = serialCount++;

        public readonly DateTime Time = DateTime.UtcNow;
        public SimObjectEvent(string name, object[] paramz)
        {
            EventType = SimEventType.LISP;
            EventStatus = SimEventStatus.Once;
            Verb = name;
            Parameters = paramz;
            ParameterNames();
        }
        // string eventName;
        // object[] args;
        readonly List<SimEventSubscriber> receiversSent = new List<SimEventSubscriber>();
        //SimObjectEvent original = null;

        internal void SendTo(SimEventSubscriber subscriber)
        {
            if (!receiversSent.Contains(subscriber))
            {
                receiversSent.Add(subscriber);
                subscriber.OnEvent(this);
            }
        }

        public string GetVerb()
        {
            return Verb;
        }
        public object[] GetArgs()
        {
            return Parameters;
        }

        public BotAction GetAction()
        {
            switch (EventType)
            {
                case SimEventType.SIT:
                    {

                        new BotObjectAction((SimActor)Parameters[0], GetSimObjectUsage());
                        break;
                    }
                case SimEventType.TOUCH:
                    {

                        new BotObjectAction((SimActor)Parameters[0], GetSimObjectUsage());
                        break;
                    }
                case SimEventType.ANIM:
                    {

                        new BotObjectAction((SimActor)Parameters[0], GetSimObjectUsage());
                        break;
                    }
                case SimEventType.SOCIAL:
                    {

                        new BotObjectAction((SimActor)Parameters[0], GetSimObjectUsage());
                        break;
                    }
                case SimEventType.EFFECT:
                    {
                        new BotObjectAction((SimActor)Parameters[0], GetHeading());
                        break;
                    }
                case SimEventType.MOVEMENT:
                    {

                        new BotObjectAction((SimActor)Parameters[0], GetHeading());
                        break;
                    }
                default:
                    {
                        break;
                    }
            }
            return null;
        }

        private SimObjectUsage GetHeading()
        {
            //            return (SimObjectUsage)GetTypeParam(typeof(SimHeading), 0);
            foreach (object o in Parameters)
                if (o is SimObjectUsage) return (SimObjectUsage)o;
            return null;
        }

        public object GetTypeParam(Type t, int after)
        {
            for (int i = after; i < Parameters.Length; i++)
            {
                object o = Parameters[i];
                if (t.IsInstanceOfType(o)) return o;
            }
            return null;
        }

        private SimObjectUsage GetSimObjectUsage()
        {
            return new SimObjectUsage(SimTypeSystem.FindObjectUse(Verb), (SimObject)Parameters[1]);
        }

        public string Verb;
        public object[] Parameters;
        public SimEventType EventType;
        public SimEventStatus EventStatus;
        public SimObjectEvent(string eventName, SimEventType type, SimEventStatus status, params object[] args)
        {
            Verb = eventName;
            Parameters = flattenArray(args);
            EventType = type;
            EventStatus = status;
            ParameterNames();
        }

        public string EventName
        {
            get { return Verb + "-" + EventStatus.ToString(); }
        }

        public string EventID
        {
            get { return EventName + "-" + serial; }
        }

        static object[] flattenArray(Array args)
        {

            if (args == null) return null;
            bool containsArray = false;
            foreach (var o in args)
            {
                if (o is Array)
                {
                    containsArray = true;
                    break;
                }
            }
            if (containsArray)
            {
                List<object> flat = new List<object>();

                foreach (var o in args)
                {
                    if (o is Array)
                    {
                        flat.AddRange(flattenArray((Array)o));
                    }
                    else
                    {
                        flat.Add(o);
                    }
                }
                return flat.ToArray();
            }
            return (object[])args;
        }

        public override string ToString()
        {
            return string.Format("{0}: {1}", EventID, ScriptEngines.ScriptEventListener.argsListString(Parameters));
        }

        public FirstOrderTerm GetTerm()
        {
            throw new NotImplementedException();
        }

        public int GetArity()
        {
            return Parameters.Length;
        }

        public object GetArg(int n)
        {
            if (n == 0) return Verb;
            object o = Parameters[n - 1];
            return o;
        }

        internal SimObjectEvent CombinesWith(SimObjectEvent SE)
        {
            if (this.Verb == SE.Verb)
            {
                if (this.EventStatus == SE.EventStatus)
                {
                    return new SimObjectEvent(Verb, this.EventType, this.EventStatus, this.Parameters, SE.Parameters);
                }
                if (this.EventStatus == SimEventStatus.Start && SE.EventStatus == SimEventStatus.Stop)
                {
                    return new SimObjectEvent(Verb, this.EventType, SimEventStatus.Once, this.Parameters, SE.Parameters);
                }
                if (this.EventStatus == SimEventStatus.Once && SE.EventStatus == SimEventStatus.Stop)
                {
                    return new SimObjectEvent(Verb, this.EventType, SimEventStatus.Once, this.Parameters, SE.Parameters);
                }
                if (this.EventStatus == SimEventStatus.Start && SE.EventStatus == SimEventStatus.Once)
                {
                    return new SimObjectEvent(Verb, this.EventType, SimEventStatus.Once, this.Parameters, SE.Parameters);
                }
            }
            return null;
        }


        internal bool SameAs(SimObjectEvent SE)
        {
            if (Verb != SE.Verb) return false;
            if (EventStatus != SE.EventStatus) return false;
            if (Parameters == null) return SE.Parameters == null;
            if (SE.Parameters == null) return Parameters == null;
            if (Parameters.Length != SE.Parameters.Length) return false;
            object[] other = SE.Parameters;
            for (int i = 0; i < other.Length; i++)
            {

                object otheri = other[i];
                if (otheri == null)
                {
                    if (Parameters[i] != null) return false;
                    continue;
                }
                if (NonComparable(otheri.GetType())) continue;
                if (!Equals(Parameters[i], otheri)) return false;
            }
            return true;
        }

        static bool NonComparable(Type type)
        {
            //     if (type == typeof (Vector3)) return true;
            return false;
        }

        private readonly static Dictionary<string, SimEventDesc> descs = new Dictionary<string, SimEventDesc>();

        public String[] ParameterNames()
        {
            string[] names = new string[Parameters.Length];
            for (int i = 0; i < Parameters.Length; i++)
            {
                var o = Parameters[i];
                if (o is Vector3)
                {
                    Console.WriteLine("Got v3 in " + this);
                }
                if (o == null)
                {
                    Console.WriteLine("Got null in " + this);
                }
                string s = o.GetType().Name;
                if (s.ToLower().StartsWith("sim"))
                {
                    if (!char.IsLower(s[3]))
                    {
                        s = s.Substring(3);
                    }
                }

                s = "sim" + s;
                names[i] = s + i;
            }
            return names;
        }
    }

}