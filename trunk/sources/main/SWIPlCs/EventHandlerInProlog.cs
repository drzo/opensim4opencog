using System;
using System.Reflection;

namespace SbsSW.SwiPlCs
{
    public struct EventHandlerInPrologKey
    {
        public String Module;
        public String Name;
        public int Arity;
        public EventInfo Event;
        public Object Origin;
        public override string ToString()
        {
            return (Module ?? "user") + ":" + Name + "/" + Arity + " " + Event;
        }
    }

    public class EventHandlerInProlog : PrologGenericDelegate
    {
        public EventHandlerInPrologKey Key;

        public EventHandlerInProlog(EventHandlerInPrologKey key)
        {
            Key = key;
            var keyEvent = key.Event;
            var eht = keyEvent.EventHandlerType;
            SetInstanceOfDelegateType(eht);
            if (PrologArity != key.Arity)
            {
                throw new ArgumentException("Arity of needed info " + PrologArity + " does not match " + key.Arity + " for " + this);
            }
            SyncLock = Delegate;
        }

        public override string ToString()
        {
            return "EventHandlerInProlog: " + Key;
        }

        //#pragma unsafe
        public override object CallPrologFast(object[] paramz)
        {
            //lock (oneEvtHandlerAtATime)
            {
                try
                {
                    PrologEvents++;
                    return PrologClient.CallProlog(this, Key.Module ?? "user", Key.Name, PrologArity, Key.Origin, paramz,
                                                   ReturnType, true);
                }
                catch (AccessViolationException e)
                {
                    PrologClient.Warn("CallProlog: {0} ex: {1}", this, e);
                    return null;
                }
                catch (Exception e)
                {
                    PrologClient.Warn("CallProlog: {0} ex: {1}", this, e);

                    return null;
                }
            }
        }

        //static readonly Object oneEvtHandlerAtATime = new object();

        public static ulong PrologEvents;
    }
}
