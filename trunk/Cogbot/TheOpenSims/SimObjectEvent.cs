using System;
using System.Collections.Generic;
using OpenMetaverse;

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
        MOVEMENT
    }
    public enum SimEventStatus
    {
        _UNKNOWN,
        Start,
        Once,
        Stop
    }

    public class SimObjectEvent: BotMentalAspect
    {

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
            throw new NotImplementedException();
        }

        private SimObjectUsage GetSimObjectUsage()
        {
            return new SimObjectUsage(SimTypeSystem.FindObjectUse(Verb),(SimObject)Parameters[1]);
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
        }

        public string EventName
        {
            get { return Verb + EventStatus.ToString(); }
        }

        static object[] flattenArray(Array args)
        {

            if (args==null) return null;
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
                List<object> flat= new List<object>();

                foreach (var o in args)
                {
                    if (o is Array)
                    {
                        flat.AddRange(flattenArray((Array) o));
                    }
                    else
                    {
                        flat.Add(o);
                    }
                }
                return flat.ToArray();
            }
            return (object[]) args;
        }

        public override string ToString()
        {
            return EventType + ":" + Verb + EventStatus.ToString() + " " +
                   ScriptEngines.ScriptEventListener.argsListString(Parameters);
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
                 if (this.EventStatus == SE.EventStatus )
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
    }
}