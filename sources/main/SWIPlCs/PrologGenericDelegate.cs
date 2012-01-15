using System;
using System.Reflection;
using System.Threading;

namespace SbsSW.SwiPlCs
{
    public abstract class PrologGenericDelegate
    {
#if USE_MUSHDLR
        public static TaskQueueHandler PrologEventQueue = new TaskQueueHandler("PrologEventQueue");
#endif
        private Type[] ParamTypes;
        private bool IsVoid;
        private int ParamArity = -1;
        /// <summary>
        /// prolog predicate arity (invokeMethod.IsStatic ? 0 : 1) + ParamArity + (IsVoid ? 0 : 1);
        /// </summary>       
        public int PrologArity;
        public Delegate Delegate;
        public Type ReturnType;

        private MethodInfo _handlerMethod;
        public MethodInfo HandlerMethod
        {
            get
            {
                if (_handlerMethod != null) return _handlerMethod;
                if (ParamTypes == null) throw new InvalidOperationException("First set instance of DelegateType!");
                Type c = GetType();
                if (IsVoid) return c.GetMethod("GenericFun" + ParamArity).MakeGenericMethod(ParamTypes);
                Type[] typesPlusReturn = new Type[ParamArity + 1];
                Array.Copy(ParamTypes, typesPlusReturn, ParamArity);
                typesPlusReturn[PrologArity] = ReturnType;
                return _handlerMethod = c.GetMethod("GenericFunR" + ParamArity).MakeGenericMethod(typesPlusReturn);
            }
        }

        public void SetInstanceOfDelegateType(Type delegateType)
        {
            var invokeMethod = delegateType.GetMethod("Invoke");
            ReturnType = invokeMethod.ReturnType;
            ParameterInfo[] parms = invokeMethod.GetParameters();
            IsVoid = ReturnType == typeof(void);
            ParamArity = parms.Length;
            // For non static we like to send the first argument in from the Origin's value
            PrologArity = (invokeMethod.IsStatic ? 0 : 1) + ParamArity + (IsVoid ? 0 : 1);
            ParamTypes = new Type[ParamArity];
            for (int i = 0; i < ParamArity; i++)
            {
                ParamTypes[i] = parms[i].ParameterType;
            }
            Delegate = Delegate.CreateDelegate(delegateType, this, HandlerMethod);
            //SyncLock = SyncLock ?? Delegate;
        }

        //public abstract object CallProlog(params object[] args);

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
        public R GenericFunR8<A, B, C, D, E, F, G, H, R>(A a, B b, C c, D d, E e, F f, G g, H h)
        {
            return (R)CallProlog(a, b, c, d, e, f, g, h);
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
        public void GenericFun8<A, B, C, D, E, F, G, H>(A a, B b, C c, D d, E e, F f, G g, H h)
        {
            CallProlog(a, b, c, d, e, f, g, h);
        }


        public virtual object CallProlog(params object[] paramz)
        {
            if (!IsUsingGlobalQueue) return CallProlog0(paramz);
            string threadName = "CallProlog " + Thread.CurrentThread.Name;
#if USE_MUSHDLR
            PrologEventQueue.Enqueue(threadName, () => CallProlog0(paramz));
            return null;
#endif
            throw new MissingMemberException("No global queueing " + threadName);
        }

        private object CallProlog0(object[] paramz)
        {
            if (IsSyncronous)
            {
                var syncLock = SyncLock ?? Delegate;
                if (syncLock != null)
                {
                    lock (syncLock)
                    {
                        return CallPrologFast(paramz);
                    }
                }
            }
            return CallPrologFast(paramz);
        }

        public abstract object CallPrologFast(object[] paramz);

        readonly public bool IsUsingGlobalQueue = false;
        public bool IsSyncronous = true;
        public object SyncLock;

    }
}