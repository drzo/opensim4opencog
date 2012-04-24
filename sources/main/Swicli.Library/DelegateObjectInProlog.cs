using System;
using System.Reflection;
using System.Threading;

namespace SbsSW.SwiPlCs
{

    public struct DelegateObjectInPrologKey
    {
        public String Module;
        public String Name;
        public int Arity;
        public Type DelegateType;
        //public PlTerm Origin;
        public override string ToString()
        {
            return (Module ?? "user") + ":" + Name + "/" + Arity + " " + DelegateType;
        }
    }

    public class DelegateObjectInProlog : PrologGenericDelegate
    {
        public static bool UseCallN = false;

        DelegateObjectInPrologKey Key;

        public override string ToString()
        {
            return "DelegateObjectInProlog: " + Key;
        }

        //public PlTerm Origin;

        public DelegateObjectInProlog(DelegateObjectInPrologKey key)
        {
            Key = key;
            Type eht = key.DelegateType;
            SetInstanceOfDelegateType(eht);
            SyncLock = Delegate;
        }

        //#pragma unsafe
        public override object CallPrologFast(object[] paramz)
        {
            //lock (oneEvtHandlerAtATime)
            {
                try
                {
                    object arg1 =
                        //Key.Origin; //makes sense for UseCallN
                        this;
                    PrologEvents++;
                    if (UseCallN)
                    {
                        return PrologClient.CallProlog(this, Key.Module, "call", PrologArity, arg1, paramz, ReturnType,
                                                       false);
                    }
                    return PrologClient.CallProlog(this, Key.Module ?? "user", Key.Name, PrologArity, arg1, paramz,
                                                   ReturnType, false);
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
