using System;
using System.Collections;
using System.Collections.Generic;
using System.Reflection;
using System.Xml.Serialization;
using MushDLR223.Utilities;

namespace MushDLR223.ScriptEngines
{
    [Flags]
    public enum SimEventType
    {
        UNKNOWN = 1,
        EFFECT = 2,
        SOCIAL = 4,
        SIT = 8,
        ANIM = 16,
        TOUCH = 32,
        MOVEMENT = 64,
        SCRIPT = 128,
        NETWORK = 256,
        DATA_UPDATE = 512,
        VAR_UPDATE = 1024,
        COMMAND_CALL = 2048,
        COMMAND_RESULT = 4096,
        Once = 8192,
        Start = 2 ^ 14,
        Stop = 2 ^ 15,
        PERSONAL = 2 ^ 16,
        REGIONAL = 2 ^ 17,
    }

    internal class SimEventDesc
    {
    }

    public interface CogbotEvent
    {
        string Verb { get; }
        NamedParam[] Parameters { get; }
        SimEventType EventType1 { get; }
        SimEventType EventStatus { get; }
        bool IsEventType(SimEventType update);
        object IsPersonal { get; }
        string EventName { get; }
        long Serial { get; set; }
        DateTime Time { get; }
        void AddParam(string name, object value);
        object[] GetArgs();
        object GetArg(int i);
        object this[string target] { get; }
        bool SameAs(CogbotEvent lastEvent);
        string ToEventString();
        void SendTo(SimEventSubscriber subscriber);
        //string GetVerb();
        string[] ParameterNames();
        object Sender { get; set; }
    }
    [XmlType(TypeName = "simObjectEvt")]
    public class ACogbotEvent : EventArgs, CogbotEvent
    {
        public object IsPersonal
        {
            get
            {
                if (IsEventType(SimEventType.REGIONAL)) return null;
                return Sender;
            }
        }

        public ICollection<NamedParam> GetInfoMap()
        {
            return ScriptManager.GetMemberValues("", this, null);
        }

        private static long serialCount = DateTime.UtcNow.Ticks;
        private long _serial = serialCount++;
        [XmlArrayItem]
        public long Serial
        {
            get { return _serial; }
            set
            {
            	_serial = value;
                _EVETSTRING = null;
                ToEventString();
            }
        }

        [XmlArrayItem]
        public DateTime Time { get; private set; }

        // string eventName;
        // object[] args;
        readonly List<SimEventSubscriber> receiversSent = new List<SimEventSubscriber>();
        //SimObjectEvent original = null;

        public void SendTo(SimEventSubscriber subscriber)
        {
            lock (receiversSent)
            {
                if (receiversSent.Contains(subscriber)) return;
                receiversSent.Add(subscriber);
            }
            subscriber.OnEvent(this);
        }

        public object[] GetArgs()
        {
            object[] os = new object[Parameters.Length];
            int a = 0;
            foreach (NamedParam pair in Parameters)
            {
                os[a++] = pair.Value;
            }
            return os;
        }

        public object GetTypeParam(Type t, int after)
        {
            for (int i = after; i < Parameters.Length; i++)
            {
                var o = Parameters[i];
                if (t.IsInstanceOfType(o.Value)) return o.Value;
            }
            return null;
        }

        [XmlArrayItem]
        public string Verb { get; set; }
        [XmlArrayItem]
        public NamedParam[] Parameters { get; set; }
        [XmlArrayItem]
        public SimEventType EventType1 { get; set; }
        /*
        public ACogbotEvent(SimEventStatus status, string eventName, SimEventType type, SimEventClass clazz, IEnumerable<NamedParam> args)
        {
            SetVerb(eventName);
            Parameters = NamedParam.ToArray(args);
            EventType = type;
            EventStatus = status;
            EventClass = clazz;
            CheckStructure();
        }
        */

        public ACogbotEvent(object sender, SimEventType type, string name, params NamedParam[] paramz)
        {
            Sender = sender;
            SetVerb(name);
            EventType1 = type;
            Parameters = NamedParam.ToArray(paramz);
            CheckStructure();
        }
        public ACogbotEvent(object sender, SimEventType type, string name, IEnumerable paramz)
        {
            Sender = sender;
            SetVerb(name);
            EventType1 = type;
            Parameters = NamedParam.ObjectsToParams(paramz);
            CheckStructure();
        }

        private void SetVerb(string name)
        {
            name = string.Intern(name);
            Verb = name;
            Time = DateTime.UtcNow;
        }

        [XmlArrayItem]
        public string EventName
        {
            get { return Verb + "-" + EventType1.ToString(); }
        }

        [XmlArrayItem]
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
            if (IsEventType(SimEventType.EFFECT))
            {
                ToEventString();
            }
            string ParamString = "";
            foreach (NamedParam param in Parameters)
            {
                ParamString += param + " ";
            }
            return string.Format("{0}: {1} {2}",
                                 EventID,
                                 ParamString,
                                 Serial);
        }


        public int GetArity()
        {
            return Parameters.Length;
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


        public bool SameAs(CogbotEvent SE)
        {
            if (Verb != SE.Verb) return false;
            if (EventType1 != SE.EventType1) return false;
            if (Parameters == null) return SE.Parameters == null;
            if (SE.Parameters == null) return Parameters == null;
            if (Parameters.Length != SE.Parameters.Length) return false;
            NamedParam[] other = SE.Parameters;
            for (int i = 0; i < other.Length; i++)
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
            if (o is NullType) return ((NullType)o).Type.DeclaringType;
            if (o != null) return o.GetType();
            return typeof(NullType);
        }

        public void CheckStructure()
        {
            ParameterNames();
        }
        public String[] ParameterNames()
        {
            int len = Parameters.Length;
            string[] names = new string[len];
            for (int i = 0; i < len; i++)
            {
                var o = Parameters[i];
                object key = o;

                while (key is NamedParam)
                {
                    key = ((NamedParam)key).Key;
                }
                object v = o.Value;

                if (v == null)
                {
                    DLRConsole.DebugWriteLine("Got null in {0}", this);
                }

                // we already nave a good name
                if (key is String)
                {
                    names[i] = (String)key;//.ToString();
                }
                else
                {
                    // otherwise we make one up
                    string s = string.Intern(GetType(o).Name + "" + i);

                    if (s.ToLower().StartsWith("sim"))
                    {
                        if (!char.IsLower(s[3]))
                        {
                            s = s.Substring(3);
                        }
                    }
                    names[i] = string.Intern(string.Format("sim{0}", s));
                }
                if (o.Key == null)
                {
                    DLRConsole.DebugWriteLine("Got null in Key {0}", this);
                }
            }
            //ToEventString();
            return names;
        }

        public object Sender
        {
            get; set;
        }

        public SimEventType EventStatus
        {
            get
            {
                if (IsEventType(SimEventType.Start))
                {
                    if (IsEventType(SimEventType.Stop)) return SimEventType.Once;
                    return SimEventType.Start;
                } else
                {
                    if (IsEventType(SimEventType.Stop)) return SimEventType.Stop;
                }
                return SimEventType.Once;
            }
        }

        public bool IsEventType(SimEventType update)
        {
            if (update == EventType1) return true;
            return (update & EventType1) != 0;
        }

        private string _EVETSTRING;
        virtual public string ToEventString()
        {
            if (_EVETSTRING != null) return _EVETSTRING;
            _EVETSTRING = string.Format("{0}: {1}", Verb, Serial);
            foreach (var c in Parameters)
            {
                object cValue = c.Value;
                /*
                 
                if (cValue is SimHeading) continue;
                if (cValue is Vector3) continue;
                if (cValue is Vector3d) continue;
                if (cValue is ValueType) continue;
                _EVETSTRING += " " + ScriptEventListener.argString(cValue);

                 */
                _EVETSTRING += " " + c;
            }
            return _EVETSTRING;
        }

        public void AddParam(string name, object value)
        {
            var old = new List<NamedParam>(Parameters);
            old.Add(new NamedParam(name, value));
            Parameters = old.ToArray();
            CheckStructure();
        }

        public object this[string target]
        {
            get
            {
                foreach (NamedParam param in Parameters)
                {
                    if (param.Key == target) return param.ObjectValue;
                }
                foreach (NamedParam param in Parameters)
                {
                    if (param.Key.ToLower().Contains(target.ToLower())) return param.ObjectValue;
                }
                return null;
                //throw new ArgumentOutOfRangeException(target);
            }
        }

        public static CogbotEvent CreateEvent(object sender, SimEventType type, string eventName, IEnumerable pzs)
        {
            return new ACogbotEvent(sender, type, eventName, pzs);
        }
        public static CogbotEvent CreateEvent(object sender, string eventName, SimEventType type, params NamedParam[] pzs)
        {
            return new ACogbotEvent(sender, type, eventName, pzs);
        }
        public static CogbotEvent CreateEvent(object sender, SimEventType type, string eventName, SimEventType status, params NamedParam[] pzs)
        {
            return new ACogbotEvent(sender, type | status, eventName, pzs);
        }
    }
}