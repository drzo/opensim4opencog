using System;
using System.Collections;
using System.Collections.Generic;
using cogbot.Listeners;
using cogbot.ScriptEngines;
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
        SCRIPT,
        NETWORK,
        DATA_UPDATE
    }
    public enum SimEventStatus
    {
        Once,
        Start,
        Stop
    }

    internal class SimEventDesc
    {
    }

    public class NullType
    {
        public NullType(Type type)
        {
            Type = type;
        }
        public Type Type { get; set; }
    }

    public class SimObjectEvent : BotMentalAspect
    {
        private static long serialCount = DateTime.UtcNow.Ticks;
        private long _serial = serialCount++;
        public long serial
        {
            get { return _serial; }
            set
            {
            	_serial = value;
                _EVETSTRING = null;
                ToEventString();
            }
        } 

        public readonly DateTime Time = DateTime.UtcNow;

        // string eventName;
        // object[] args;
        readonly List<SimEventSubscriber> receiversSent = new List<SimEventSubscriber>();
        //SimObjectEvent original = null;

        internal void SendTo(SimEventSubscriber subscriber)
        {
            lock (receiversSent)
            {
                if (receiversSent.Contains(subscriber)) return;
                receiversSent.Add(subscriber);
            }
            subscriber.OnEvent(this);
        }

        public string GetVerb()
        {
            return Verb;
        }
        public object[] GetArgs()
        {
            object[] os = new object[Parameters.Count];
            int a = 0;
            foreach (NamedParam pair in Parameters)
            {
                os[a++] = pair.Value;
            }
            return os;
        }

        public BotAction GetAction()
        {
            switch (EventType)
            {
                //case SimEventType.SIT:
                //    {

                //        new BotObjectAction((SimActor)Parameters[0], GetSimObjectUsage());
                //        break;
                //    }
                //case SimEventType.TOUCH:
                //    {

                //        new BotObjectAction((SimActor)Parameters[0], GetSimObjectUsage());
                //        break;
                //    }
                //case SimEventType.ANIM:
                //    {

                //        new BotObjectAction((SimActor)Parameters[0], GetSimObjectUsage());
                //        break;
                //    }
                //case SimEventType.SOCIAL:
                //    {

                //        new BotObjectAction((SimActor)Parameters[0], GetSimObjectUsage());
                //        break;
                //    }
                //case SimEventType.EFFECT:
                //    {
                //        new BotObjectAction((SimActor)Parameters[0], GetHeading());
                //        break;
                //    }
                //case SimEventType.MOVEMENT:
                //    {

                //        new BotObjectAction((SimActor)Parameters[0], GetHeading());
                //        break;
                //    }
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
            foreach (var o in Parameters)
                if (o.Value is SimObjectUsage) return (SimObjectUsage)o.Value;
            return null;
        }

        public object GetTypeParam(Type t, int after)
        {
            for (int i = after; i < Parameters.Count; i++)
            {
                var o = Parameters[i];
                if (t.IsInstanceOfType(o.Value)) return o.Value;
            }
            return null;
        }

        private SimObjectUsage GetSimObjectUsage()
        {
            return new SimObjectUsage(SimTypeSystem.FindObjectUse(Verb), (SimObject)Parameters[1].Value);
        }

        public string Verb;
        public List<NamedParam> Parameters;
        public SimEventType EventType;
        public SimEventStatus EventStatus;

        public SimObjectEvent(SimEventStatus status, string eventName, SimEventType type, IEnumerable<NamedParam> args)
        {
            Verb = eventName;
            Parameters = new List<NamedParam>(args);
            EventType = type;
            EventStatus = status;
            ParameterNames();
        }

        public SimObjectEvent(SimEventStatus status, string eventName, SimEventType type, params NamedParam[] args)
        {
            Verb = eventName;
            Parameters = new List<NamedParam>(args);
            EventType = type;
            EventStatus = status;
            ParameterNames();
        }

        public SimObjectEvent(SimEventType type, string name, IEnumerable paramz)
        {
            EventType = type;
            EventStatus = SimEventStatus.Once;
            Verb = name;
            Parameters = NamedParam.ObjectsToParams(paramz);
            ParameterNames();
        }

        public string EventName
        {
            get { return Verb + "-" + EventStatus.ToString(); }
        }

        public string EventID
        {
            get { return EventName; }
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
            return string.Format("{0}: {1} {2}",
                                 EventID,
                                 ScriptEngines.ScriptEventListener.argsListString(Parameters),
                                 serial);
        }

        public FirstOrderTerm GetTerm()
        {
            throw new NotImplementedException();
        }

        public int GetArity()
        {
            return Parameters.Count;
        }

        public object GetArg(int n)
        {
            if (n == 0) return Verb;
            object o = GetValue(Parameters[n - 1]);
            return o;
        }

        static object GetValue(object parameter)
        {
            if (parameter is NamedParam) parameter = ((NamedParam)parameter).Value;
            if (parameter is NullType || parameter == null)
            {
                return null;
            }
            return parameter;
        }

        //internal SimObjectEvent CombinesWith(SimObjectEvent SE)
        //{
        //    if (this.Verb == SE.Verb)
        //    {
        //        if (this.EventStatus == SE.EventStatus)
        //        {
        //            return new SimObjectEvent(Verb, this.EventType, this.EventStatus, this.Parameters, SE.Parameters);
        //        }
        //        if (this.EventStatus == SimEventStatus.Start && SE.EventStatus == SimEventStatus.Stop)
        //        {
        //            return new SimObjectEvent(Verb, this.EventType, SimEventStatus.Once, this.Parameters, SE.Parameters);
        //        }
        //        if (this.EventStatus == SimEventStatus.Once && SE.EventStatus == SimEventStatus.Stop)
        //        {
        //            return new SimObjectEvent(Verb, this.EventType, SimEventStatus.Once, this.Parameters, SE.Parameters);
        //        }
        //        if (this.EventStatus == SimEventStatus.Start && SE.EventStatus == SimEventStatus.Once)
        //        {
        //            return new SimObjectEvent(Verb, this.EventType, SimEventStatus.Once, this.Parameters, SE.Parameters);
        //        }
        //    }
        //    return null;
        //}


        internal bool SameAs(SimObjectEvent SE)
        {
            if (Verb != SE.Verb) return false;
            if (EventStatus != SE.EventStatus) return false;
            if (Parameters == null) return SE.Parameters == null;
            if (SE.Parameters == null) return Parameters == null;
            if (Parameters.Count != SE.Parameters.Count) return false;
            IList<NamedParam> other = SE.Parameters;
            for (int i = 0; i < other.Count; i++)
            {
                NamedParam otheri = other[i];
                if (otheri.Value == null)
                {
                    if (Parameters[i].Value != null) return false;
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

        static Type GetType(object o)
        {
            if (o is NamedParam) o = ((NamedParam)o).Value;
            if (o is NullType) return ((NullType)o).Type;
            if (o != null) return o.GetType();
            return typeof(NullType);
        }

        public String[] ParameterNames()
        {
            string[] names = new string[Parameters.Count];
            for (int i = 0; i < Parameters.Count; i++)
            {
                var o = Parameters[i];
                if (o.Value is Vector3)
                {
                    Console.WriteLine("Got v3 in {0}", this);
                }
                object key = o;

                while (key is NamedParam)
                {
                    key = ((NamedParam)key).Key;
                }
                object v = o.Value;

                if (v == null)
                {
                    Console.WriteLine("Got null in {0}", this);
                }

                // we already nave a good name
                if (key is String)
                {
                    names[i] = key.ToString();
                }
                else
                {
                    // otherwise we make one up
                    string s = GetType(o).Name + "" + i;

                    if (s.ToLower().StartsWith("sim"))
                    {
                        if (!char.IsLower(s[3]))
                        {
                            s = s.Substring(3);
                        }
                    }
                    names[i] = string.Format("sim{0}", s);
                }

            }
            ToEventString();
            return names;
        }

        private string _EVETSTRING;
        public string ToEventString()
        {
            if (_EVETSTRING != null) return _EVETSTRING;
            _EVETSTRING = string.Format("{0}: {1}", GetVerb(), serial);
            foreach (var c in Parameters)
            {
                object cValue = c.Value;
                if (cValue is SimHeading) continue;
                if (cValue is Vector3) continue;
                if (cValue is Vector3d) continue;
                if (cValue is ValueType) continue;
                _EVETSTRING += " " + ScriptEventListener.argString(cValue);
            }
            return _EVETSTRING;
        }

        public void AddParam(string name, object value)
        {
            Parameters.Add(new NamedParam(name, value));
        }
    }

    public struct NamedParam
    {
        public NamedParam(object k, object v)
        {
            Key = k;
            Value = v;
        }
        readonly public object Key;
        readonly public object Value;
        public override string ToString()
        {
            if (Key == null) return string.Format("{0}", (Value ?? "NULL"));
            return Key + "=" + Value;
        }

        public static bool operator ==(NamedParam p1,NamedParam p2)
        {
            return p1.Equals(p2);
        }

        public static bool operator !=(NamedParam p1, NamedParam p2)
        {
            return !(p1 == p2);
        }

        // override object.Equals
        public override bool Equals(object obj)
        {
            //       
            // See the full list of guidelines at
            //   http://go.microsoft.com/fwlink/?LinkID=85237  
            // and also the guidance for operator== at
            //   http://go.microsoft.com/fwlink/?LinkId=85238
            //

            if (obj == null || GetType() != obj.GetType())
            {
                return Equals(Value,obj);
            }
            return Equals(Value,((NamedParam) obj).Value);

        }

        // override object.GetHashCode
        public override int GetHashCode()
        {
            // TODO: write your implementation of GetHashCode() here
            return base.GetHashCode();
        }

        public static List<NamedParam> ObjectsToParams(IEnumerable paramz)
        {
            List<NamedParam> Parameters = new List<NamedParam>();
            foreach (var v in paramz)
            {
                if (v is NamedParam)
                {
                    Parameters.Add((NamedParam)v);
                }
                else
                {
                    Parameters.Add(new NamedParam(null, v));
                }
            }
            return Parameters;
        }

    }
}