using System;
using System.Collections.Generic;
using SbsSW.SwiPlCs.Callback;
using SbsSW.SwiPlCs.Exceptions;

namespace SbsSW.SwiPlCs
{
    public interface SCCH
    {
        bool Setup(PlTermV a0);
        bool Call(PlTermV a0);
        bool Close(PlTermV a0);
        bool HasMore();
    }

    abstract public class AbstractNondetMethod : IDisposable, SCCH
    {
        readonly private DelegateParameterBacktrackVarArgs del;
        protected string Module;
        protected string Name;
        protected int Arity;

        protected AbstractNondetMethod()
        {
            del = BackrackImpl;
        }

        public virtual void Register()
        {
            libpl.PL_register_foreign_in_module(Module, Name, Arity, del,
                                                (int)(PlForeignSwitches.Nondeterministic | PlForeignSwitches.VarArgs));
        }

        public virtual void Dispose()
        {
            //   libpl.PL_register_foreign_in_module(Module, Name, Arity, del,
            //                                     (int)(PlForeignSwitches.Nondeterministic | PlForeignSwitches.VarArgs));
        }

        public int BackrackImpl(PlTerm a0, int arity, IntPtr control)
        {
            var handle = control;
            FRG fc = (FRG)(libpl.PL_foreign_control(control));
            NondetContextHandle v;
            switch (fc)
            {
                case FRG.PL_FIRST_CALL:
                    {
                        v = NondetContextHandle.ObtainHandle(control, Clone());
                        bool res = v.Setup(new PlTermV(a0, arity));
                        return Call0(v, new PlTermV(a0, arity));

                    } break;
                case FRG.PL_REDO:
                    {
                        v = NondetContextHandle.FindHandle(control);
                        return Call0(v, new PlTermV(a0, arity));

                    } break;
                case FRG.PL_CUTTED:
                    {
                        v = NondetContextHandle.FindHandle(control);
                        bool res = v.Close(new PlTermV(a0, arity));
                        NondetContextHandle.ReleaseHandle(v);
                        return res ? 1 : 0;
                    } break;
                default:
                    {
                        throw new PlException("no frg");
                        return libpl.PL_fail;
                    }
                    break;
            }
        }

        public virtual int Call0(NondetContextHandle handle, PlTermV termV)
        {
            bool res = handle.Call(termV);
            bool more = handle.HasMore();
            if (more)
            {
                libpl.PL_retry(handle.Handle);
                return res ? 3 : 0;
            }
            return res ? 1 : 0;
        }

        public abstract AbstractNondetMethod Clone();

        public abstract bool Setup(PlTermV a0);
        public abstract bool Call(PlTermV a0);
        public abstract bool Close(PlTermV a0);
        public abstract bool HasMore();
    }

    public class NondetContextHandle
    {
        static readonly LinkedList<NondetContextHandle> NonDetHandles = new LinkedList<NondetContextHandle>();

        static public void ReleaseHandle(NondetContextHandle hnd)
        {
            lock (NonDetHandles)
            {
                NondetContextHandle.ContextToObject.Remove(hnd.Context);
                hnd.Context = (IntPtr)0;
                NonDetHandles.AddLast(hnd);
            }
        }

        public delegate NondetContextHandle HandleMaker();
        static public NondetContextHandle ObtainHandle(IntPtr context, SCCH value)
        {
            lock (NonDetHandles)
            {
                NondetContextHandle hnd;
                if (NonDetHandles.Count == 0)
                {
                    hnd = new NondetContextHandle();
                }
                else
                {
                    hnd = NonDetHandles.First.Value;
                    NonDetHandles.RemoveFirst();
                }
                hnd.Context = context;
                hnd.NonDetMethods = value;
                lock (NondetContextHandle.HandleToObject)
                {
                    NondetContextHandle.ContextToObject[context] = hnd;
                }
                return hnd;
            }
        }

        static public NondetContextHandle ObtainHandle(IntPtr context)
        {
            return ObtainHandle(context, null);
        }
        //static NondetContextHandle lastHandle;
        public static NondetContextHandle FindHandle(IntPtr context)
        {
            //if (context == (IntPtr)0) return lastHandle;
            lock (NondetContextHandle.HandleToObject) return NondetContextHandle.ContextToObject[context];
        }

        public static Dictionary<int, NondetContextHandle> HandleToObject = new Dictionary<int, NondetContextHandle>();
        public static Dictionary<IntPtr, NondetContextHandle> ContextToObject = new Dictionary<IntPtr, NondetContextHandle>();
        public static int TotalHandles = 0;

        public SCCH NonDetMethods;
        public readonly int Handle;
        public IntPtr Context;

        public NondetContextHandle()
        {
            NonDetMethods = new ForNext(1, 20);
            lock (HandleToObject)
            {
                Handle = ++TotalHandles;
                HandleToObject[Handle] = this;
            }
        }

       #region Overrides of AbstractNondetMethod

        public bool Setup(PlTermV a0)
        {
            if (MissingImpl()) return false;
            return NonDetMethods.Setup(a0);
        }

        private bool MissingImpl()
        {
            if (NonDetMethods == null) throw new PlException("not impl");
            return NonDetMethods == null;
        }

        public bool Call(PlTermV a0)
        {
            if (MissingImpl()) return false;
            return NonDetMethods.Call(a0);
        }

        public bool Close(PlTermV a0)
        {
            if (MissingImpl()) return false;
            return NonDetMethods.Close(a0);
        }

        public bool HasMore()
        {
            if (NonDetMethods != null) return NonDetMethods.HasMore();
            return false;
        }

       #endregion
    }
}