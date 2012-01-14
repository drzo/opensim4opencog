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
       // public PlTerm Origin;
        public override string ToString()
        {
            return (Module ?? "user") + ":" + Name + "/" + Arity + " " + DelegateType;
        }
    }

    public class DelegateObjectInProlog
    {
        DelegateObjectInPrologKey Key;
        /*
        public String Module = "user";
        /// <summary>
        /// Prolog predicate name
        /// </summary>
        public String Name;
        /// <summary>
        /// prolog predicate arity (invokeMethod.IsStatic ? 0 : 1) + ParamArity + (IsVoid ? 0 : 1);
        /// </summary>       
        public int Arity;
        */
        public override string ToString()
        {
            return "" + Key;
        }

        //public PlTerm Origin;

        readonly public Delegate Delegate;
        readonly public Type[] ParamTypes;
        readonly public Type ReturnType;
        readonly bool IsVoid;
        private readonly int ParamArity;

        public DelegateObjectInProlog(DelegateObjectInPrologKey key)
        {            
            Key = key;           
            Type eht = key.DelegateType;
            MethodInfo invokeMethod = eht.GetMethod("Invoke");
            ReturnType = invokeMethod.ReturnType;
            ParameterInfo[] parms = invokeMethod.GetParameters();
            IsVoid = ReturnType == typeof(void);
            ParamArity = parms.Length;
            // For non static we like to send the first argument in from the Origin's value
            Key.Arity = (invokeMethod.IsStatic ? 0 : 1) + ParamArity + (IsVoid ? 0 : 1);
            if (Key.Arity != key.Arity)
            {
               // PrologClient.Warn("Arity of needed info " + Key.Arity + " does not match " + key.Arity + " for " + this);
                key.Arity = Key.Arity;
            }
            ParamTypes = new Type[ParamArity];
            for (int i = 0; i < ParamArity; i++)
            {
                ParamTypes[i] = parms[i].ParameterType;
            }
            Delegate = Delegate.CreateDelegate(eht, this, HandlerMethod);
            SyncLock = Delegate;
        }

        private MethodInfo _handlerMethod;
        public MethodInfo HandlerMethod
        {
            get
            {
                if (_handlerMethod != null) return _handlerMethod;
                Type c = GetType();
                if (IsVoid) return c.GetMethod("GenericFun" + ParamArity).MakeGenericMethod(ParamTypes);
                Type[] typesPlusReturn = new Type[ParamArity + 1];
                Array.Copy(ParamTypes, typesPlusReturn, ParamArity);
                typesPlusReturn[Key.Arity] = ReturnType;
                return c.GetMethod("GenericFunR" + ParamArity).MakeGenericMethod(typesPlusReturn);
            }
        }

        // non-void functions 0-6
        public R GenericFunR0<R>()
        {
            return (R)CallProlog();
        }
        public R GenericFunR1<A, R>(A a)
        {
            return (R)CallProlog(a);
        }
        public R GenericFunR2<A, B, R>(A a, B b)
        {
            return (R)CallProlog(a, b);
        }
        public R GenericFunR3<A, B, C, R>(A a, B b, C c)
        {
            return (R)CallProlog(a, b, c);
        }
        public R GenericFunR4<A, B, C, D, R>(A a, B b, C c, D d)
        {
            return (R)CallProlog(a, b, c, d);
        }
        public R GenericFunR5<A, B, C, D, E, R>(A a, B b, C c, D d, E e)
        {
            return (R)CallProlog(a, b, c, d, e);
        }
        public R GenericFunR6<A, B, C, D, E, F, R>(A a, B b, C c, D d, E e, F f)
        {
            return (R)CallProlog(a, b, c, d, e, f);
        }
        public R GenericFunR7<A, B, C, D, E, F, G, R>(A a, B b, C c, D d, E e, F f, G g)
        {
            return (R)CallProlog(a, b, c, d, e, f, g);
        }

        // void functions 0-6
        public void GenericFun0()
        {
            CallProlog();
        }
        public void GenericFun1<A>(A a)
        {
            CallProlog(a);
        }
        public void GenericFun2<A, B>(A a, B b)
        {
            CallProlog(a, b);
        }
        public void GenericFun3<A, B, C>(A a, B b, C c)
        {
            CallProlog(a, b, c);
        }
        public void GenericFun4<A, B, C, D>(A a, B b, C c, D d)
        {
            CallProlog(a, b, c, d);
        }
        public void GenericFun5<A, B, C, D, E>(A a, B b, C c, D d, E e)
        {
            CallProlog(a, b, c, d, e);
        }
        public void GenericFun6<A, B, C, D, E, F>(A a, B b, C c, D d, E e, F f)
        {
            CallProlog(a, b, c, d, e, f);
        }
        public void GenericFun7<A, B, C, D, E, F, G>(A a, B b, C c, D d, E e, F f, G g)
        {
            CallProlog(a, b, c, d, e, f, g);
        }

        object CallProlog(params object[] paramz)
        {
            if (!IsUsingGlobalQueue) return CallProlog0(paramz);
            string threadName = "CallProlog " + Thread.CurrentThread.Name;
#if USE_MUSHDLR
            PrologEventQueue.Enqueue(threadName, () => CallProlog0(paramz));
#endif
            return null;
        }

        object CallProlog0(object[] paramz)
        {
            if (IsSyncronous)
            {
                var syncLock = SyncLock;
                if (syncLock != null)
                {
                    lock (syncLock)
                    {
                        return CallProlog1(paramz);
                    }
                }
            }
            return CallProlog1(paramz);
        }

//#pragma unsafe
        object CallProlog1(object[] paramz)
        {
            //lock (oneEvtHandlerAtATime)
            {
                try
                {
                    PrologEvents++;
                    return PrologClient.CallProlog(this, Key.Module, Key.Name, Key.Arity, this/*Key.Origin*/, paramz, ReturnType);
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

        static readonly Object oneEvtHandlerAtATime = new object();
        public bool IsUsingGlobalQueue;
        public bool IsSyncronous = true;
        public object SyncLock;
        public static ulong PrologEvents;
    }
}
