using System;
using System.Collections;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Reflection.Emit;
using System.Runtime.InteropServices;
using System.Threading;
using ikvm.extensions;
using IKVM.Internal;
using ikvm.runtime;
using java.net;
using java.util;
using jpl;
using SbsSW.SwiPlCs.Callback;
using SbsSW.SwiPlCs.Exceptions;
using SbsSW.SwiPlCs.Streams;
using System.Windows.Forms;
using Hashtable = java.util.Hashtable;
using ClassLoader = java.lang.ClassLoader;
using Class = java.lang.Class;
using sun.reflect.misc;
using Util = ikvm.runtime.Util;

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

    public class EventHandlerInProlog
    {
        public String Module = "user";
        /// <summary>
        /// Prolog predicate name
        /// </summary>
        public String Name;
        /// <summary>
        /// prolog predicate arity (invokeMethod.IsStatic ? 0 : 1) + ParamArity + (IsVoid ? 0 : 1);
        /// </summary>       
        public int Arity;

        public override string ToString()
        {
            return (Module ?? "user") + ":" + Name + "/" + Arity + " " + Event;
        }

        public EventInfo Event;
        public Object Origin;

        readonly public Delegate Delegate;
        readonly public Type[] ParamTypes;
        readonly public Type ReturnType;
        readonly bool IsVoid;
        private readonly int ParamArity;

        public EventHandlerInProlog(EventHandlerInPrologKey key)
        {
            Name = key.Name;
            Origin = key.Origin;
            var keyEvent = Event = key.Event;
            var eht = keyEvent.EventHandlerType;
            var invokeMethod = eht.GetMethod("Invoke");
            ReturnType = invokeMethod.ReturnType;
            ParameterInfo[] parms = invokeMethod.GetParameters();
            IsVoid = ReturnType == typeof(void);
            ParamArity = parms.Length;
            // For non static we like to send the first argument in from the Origin's value
            Arity = (invokeMethod.IsStatic ? 0 : 1) + ParamArity + (IsVoid ? 0 : 1);
            if (Arity != key.Arity)
            {
                throw new ArgumentException("Arity of needed info " + Arity + " does not match " + key.Arity + " for " + this);
            }
            ParamTypes = new Type[ParamArity];
            for (int i = 0; i < ParamArity; i++)
            {
                ParamTypes[i] = parms[i].ParameterType;
            }
            Delegate = Delegate.CreateDelegate(eht, this, HandlerMethod);
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
                typesPlusReturn[Arity] = ReturnType;
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
            return (R) CallProlog(a, b, c, d, e, f, g);
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
#pragma unmanaged
        object CallProlog(params object[] paramz)
        {
            lock (oneEvtHandlerAtATime)
            {
                try
                {
                    return PrologClient.CallProlog(this, Module, Name, Arity, Origin, paramz, ReturnType);
                }
                catch (Exception e)
                {
                    PrologClient.Warn("CallProlog: " + this + " ex: " + e);

                    return null;
                }
            }
        }

        static readonly Object oneEvtHandlerAtATime = new object();
    }
}
