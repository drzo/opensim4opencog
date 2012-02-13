using System;
using System.Collections;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Reflection.Emit;
using System.Runtime.InteropServices;
using System.Threading;
using System.Xml.Serialization;
using ikvm.extensions;
using IKVM.Internal;
using ikvm.runtime;
using java.net;
using java.util;
//using jpl;
using jpl;
using SbsSW.SwiPlCs.Callback;
using SbsSW.SwiPlCs.Exceptions;
using SbsSW.SwiPlCs.Streams;
using System.Windows.Forms;
using Hashtable = java.util.Hashtable;
using ClassLoader = java.lang.ClassLoader;
using Class = java.lang.Class;
using sun.reflect.misc;
using ArrayList=System.Collections.ArrayList;
using Util = ikvm.runtime.Util;
using CycFort = SbsSW.SwiPlCs.PlTerm;
using PrologCli = SbsSW.SwiPlCs.PrologClient;

namespace SbsSW.SwiPlCs
{
    /*
         
 5.6.1.1 Non-deterministic Foreign Predicates

By default foreign predicates are deterministic. Using the PL_FA_NONDETERMINISTIC attribute (see PL_register_foreign()) it is possible to register a predicate as a non-deterministic predicate. Writing non-deterministic foreign predicates is slightly more complicated as the foreign function needs context information for generating the next solution. Note that the same foreign function should be prepared to be simultaneously active in more than one goal. Suppose the natural_number_below_n/2 is a non-deterministic foreign predicate, backtracking over all natural numbers lower than the first argument. Now consider the following predicate:

quotient_below_n(Q, N) :- natural_number_below_n(N, N1), natural_number_below_n(N, N2), Q =:= N1 / N2, !.

In this predicate the function natural_number_below_n/2 simultaneously generates solutions for both its invocations.

Non-deterministic foreign functions should be prepared to handle three different calls from Prolog:

* Initial call (PL_FIRST_CALL)
Prolog has just created a frame for the foreign function and asks it to produce the first answer.
* Redo call (PL_REDO)
The previous invocation of the foreign function associated with the current goal indicated it was possible to backtrack. The foreign function should produce the next solution.
* Terminate call (PL_CUTTED)
The choice point left by the foreign function has been destroyed by a cut. The foreign function is given the opportunity to clean the environment. 

Both the context information and the type of call is provided by an argument of type control_t appended to the argument list for deterministic foreign functions. The macro PL_foreign_control() extracts the type of call from the control argument. The foreign function can pass a context handle using the PL_retry*() macros and extract the handle from the extra argument using the PL_foreign_context*() macro.

void PL_retry(long)
The foreign function succeeds while leaving a choice point. On backtracking over this goal the foreign function will be called again, but the control argument now indicates it is a `Redo' call and the macro PL_foreign_context() will return the handle passed via PL_retry(). This handle is a 30 bits signed value (two bits are used for status indication).

void PL_retry_address(void *)
As PL_retry(), but ensures an address as returned by malloc() is correctly recovered by PL_foreign_context_address().

int PL_foreign_control(control_t)
Extracts the type of call from the control argument. The return values are described above. Note that the function should be prepared to handle the PL_CUTTED case and should be aware that the other arguments are not valid in this case.

long PL_foreign_context(control_t)
Extracts the context from the context argument. In the call type is PL_FIRST_CALL the context value is 0L. Otherwise it is the value returned by the last PL_retry() associated with this goal (both if the call type is PL_REDO as PL_CUTTED).

void * PL_foreign_context_address(control_t)
Extracts an address as passed in by PL_retry_address(). 

Note: If a non-deterministic foreign function returns using PL_succeed or PL_fail, Prolog assumes the foreign function has cleaned its environment. No call with control argument PL_CUTTED will follow.

The code of figure 6 shows a skeleton for a non-deterministic foreign predicate definition.

typedef struct // define a context structure  { ... } context; 
 foreign_t my_function(term_t a0, term_t a1, foreign_t handle) { struct context * ctxt; switch( PL_foreign_control(handle) ) { case PL_FIRST_CALL: ctxt = malloc(sizeof(struct context)); ... PL_retry_address(ctxt); case PL_REDO: ctxt = PL_foreign_context_address(handle); ... PL_retry_address(ctxt); case PL_CUTTED: free(ctxt); PL_succeed; } } 
         
 */
    public delegate object AnyMethod(params object[] any);

    public partial class PrologClient
    {

        public static void InternMethod(string module, string pn, AnyMethod d)
        {
            if (!PlEngine.PinDelegate(module, pn.ToString(), -1, d)) return;
            InternMethod(module, pn, d.Method);
        }
        public static void InternMethod(string module, string pn, MethodInfo list)
        {
            InternMethod(module, pn, list, null);
        }
        public static void InternMethod(string module, string pn, MethodInfo list, object defaultInstanceWhenMissing)
        {
            if (list == null)
            {
                return;
            }
            Type type = list.DeclaringType;
            pn = pn ?? (type.Name + "." + list.Name);
            ParameterInfo[] ps = list.GetParameters();
            Type rt = list.ReturnType;
            int paramlen = ps.Length;
            bool nonvoid = rt != typeof(void);
            bool isbool = rt == typeof(bool);
            bool hasReturnValue = nonvoid && !isbool;
            bool isStatic = list.IsStatic;
            bool isVanilla = true;
            int maxOptionals = 0;
            foreach (ParameterInfo info in ps)
            {
                if (info.ParameterType != typeof(PlTerm))
                {
                    isVanilla = false;
                }
                if (IsOptionalParam(info))
                {
                    isVanilla = false;
                    maxOptionals++;
                }
            }
            if (isbool && isStatic)
            {
                if (isVanilla)
                {
                    Delegate d = null;
                    switch (paramlen)
                    {
                        case 0:
                            {
                                d = new DelegateParameter0(() => (bool)InvokeCaught(list, null, ZERO_OBJECTS));
                                PlEngine.RegisterForeign(module, pn, paramlen, d, PlForeignSwitches.None);
                                return;
                            }
                        case 1:
                            PlEngine.RegisterForeign(module, pn, paramlen,
                                                     new DelegateParameter1(
                                                         (p1) => (bool)InvokeCaught(list, null, new object[] { p1 })),
                                                     PlForeignSwitches.None);
                            return;
                        case 2:
                            PlEngine.RegisterForeign(module, pn, paramlen,
                                                     new DelegateParameter2(
                                                         (p1, p2) =>
                                                         (bool)InvokeCaught(list, null, new object[] { p1, p2 })),
                                                     PlForeignSwitches.None);
                            return;
                        case 3:
                            PlEngine.RegisterForeign(module, pn, paramlen,
                                                     new DelegateParameter3(
                                                         (p1, p2, p3) =>
                                                         (bool)InvokeCaught(list, null, new object[] { p1, p2, p3 })),
                                                     PlForeignSwitches.None);
                            return;
                        case 4:
                            PlEngine.RegisterForeign(module, pn, paramlen,
                                                     new DelegateParameter4(
                                                         (p1, p2, p3, p4) =>
                                                         (bool)InvokeCaught(list, null, new object[] { p1, p2, p3, p4 })),
                                                     PlForeignSwitches.None);
                            return;
                        case -5: // use the default please
                            PlEngine.RegisterForeign(module, pn, paramlen,
                                                     new DelegateParameter5(
                                                         (p1, p2, p3, p4, p5) =>
                                                         (bool)InvokeCaught(list, null, new object[] { p1, p2, p3, p4, p5 })),
                                                     PlForeignSwitches.None);
                            return;
                        default:
                            break;
                    }
                }
            }
            int plarity = paramlen + (hasReturnValue ? 1 : 0) + (isStatic ? 0 : 1);

            DelegateParameterVarArgs del = GetDelV(list, type, nonvoid, isbool, isStatic, plarity, defaultInstanceWhenMissing);
            PlEngine.RegisterForeign(module, pn, plarity, del, PlForeignSwitches.VarArgs);
            while (maxOptionals > 0)
            {
                del = GetDelV(list, type, nonvoid, isbool, isStatic, plarity - maxOptionals, defaultInstanceWhenMissing);
                PlEngine.RegisterForeign(module, pn, plarity - maxOptionals, del, PlForeignSwitches.VarArgs);
                maxOptionals--;
            }
        }

        private static DelegateParameterVarArgs GetDelV(MethodInfo list, Type type, bool nonvoid, bool isbool, bool isStatic, int plarity, object defaultInstanceWhenMissing)
        {
            DelegateParameterVarArgs d;
            d = (PlTermV termVector) =>
            {
                if (termVector.Size != plarity)
                {
                    //return false;
                    termVector.Resize(plarity);
                }
                object target = isStatic ? null : CastTerm(termVector[0], type) ?? defaultInstanceWhenMissing;
                Action postCallHook;
                int tvargnum = isStatic ? 0 : 1;
                object[] newVariable = PlListToCastedArray(tvargnum, termVector, list.GetParameters(),
                                                           out postCallHook);
                object result = InvokeCaught(list, target, newVariable, postCallHook);

                if (isbool)
                {
                    return (bool)result;
                }
                if (nonvoid)
                {
                    return termVector[plarity - 1].FromObject(result);
                }
                return true;

            };
            return d;
        }

        private static object InvokeCaught(MethodInfo info, object o, object[] os)
        {
            return InvokeCaught(info, o, os, Do_NOTHING);
        }
        private static object InvokeCaught(MethodInfo info, object o, object[] os, Action todo)
        {
            try
            {
                if (info.IsGenericMethodDefinition)
                {
                    Type[] paramTypes = GetObjectTypes(info.GetParameters());
                    Type[] t = GetObjectTypes(os, paramTypes);
                    info = info.MakeGenericMethod(t);
                }
                var ps = info.GetParameters();
                int psLengthM1 = ps.Length - 1;
                bool isVarArg = (info.CallingConvention & CallingConventions.VarArgs) != 0;
                if (isVarArg)
                {
                    int usedUp = 0;
                    object[] ao = new object[psLengthM1 + 1];
                    for (int i = 0; i < psLengthM1; i++)
                    {
                        ao[i] = RecastObject(ps[i].ParameterType, os[i], null);
                        usedUp++;
                    }
                    int slack = os.Length - usedUp;
                    object[] lastArray = new object[slack];
                    int fillAt = 0;
                    while (slack-- > 0)
                    {
                        lastArray[fillAt++] = os[usedUp++];
                    }
                    ao[psLengthM1] = lastArray;
                    os = ao;
                }
                if (ps.Length != os.Length)
                {
                    Warn("ArgCount mismatch " + info + ": call count=" + os.Length);
                }
                object to = o;
                if (!info.DeclaringType.IsInstanceOfType(o)) to = null;
                object ret = info.Invoke(to, os);
                if (todo != null) todo();
                if (ret == null)
                {
                    //return VoidOrNull(info);
                }
                return ret;
            }
            catch (Exception ex)
            {
                var pe = ToPlException(ex);
                string s = ex.ToString() + "\n" + ex.StackTrace;
                Warn("ex: {0}", s);
                //throw pe;
                return false;// pe;
            }
        }
        private static Type[] GetObjectTypes(ParameterInfo[] parameterInfos)
        {
            int parameterInfosLength = parameterInfos.Length;
            Type[] t = new Type[parameterInfosLength];
            for (int i = 0; i < parameterInfosLength; i++)
            {
                t[i] = parameterInfos[i].ParameterType;
            }
            return t;
        }
        private static Type[] GetObjectTypes(object[] objects, Type[] otherwise)
        {
            Type[] t = new Type[objects.Length];
            for (int i = 0; i < objects.Length; i++)
            {
                var obj = objects[i];
                if (obj != null) t[i] = objects[i].GetType();
                else t[i] = otherwise[i];
            }
            return t;
        }
        private static PlException ToPlException(Exception ex)
        {
            if (ex is PlException) return (PlException)ex;
            var ie = ex.InnerException;
            if (ie != null && ie != ex)
            {
                return ToPlException(ie);
            }
            return new PlException(ex.Message, ex);
        }

        private static Type GetArityType(int paramlen)
        {
            switch (paramlen)
            {
                case 0:
                    return typeof(DelegateParameter0);
                case 1:
                    return typeof(DelegateParameter1);
                case 2:
                    return typeof(DelegateParameter2);
                case 3:
                    return typeof(DelegateParameter3);
                case 4:
                    return typeof(DelegateParameter3);
                default:
                    return null;
            }
        }

        [PrologVisible(ModuleName = ExportModule)]
        private static bool testOut(int incoming, out int outbound)
        {
            outbound = incoming;
            return true;
        }
        [PrologVisible(ModuleName = ExportModule)]
        private static bool testOpt(int incoming, string optionalstr, out int outbound)
        {
            outbound = incoming;
            return true;
        }
        [PrologVisible(ModuleName = ExportModule)]
        private static bool testRef(int incoming, ref string optionalstr, out int outbound)
        {
            outbound = incoming;
            optionalstr = "" + incoming;
            return true;
        }
        [PrologVisible(ModuleName = ExportModule)]
        private static bool testVarArg(out int outbound, params int[] incoming)
        {
            outbound = 0;
            foreach (int i in incoming)
            {
                outbound += i;
            }
            return true;
        }

        private static int callNum = 0;


        // [StructLayout(LayoutKind.Sequential)]
        public struct NonDetTest
        {
            public int start;
            public int stop;
            public uint fid;
            //public NonDetDelegate Call;
            //public NonDetDelegate Cutted;
        }

        public class PinnedObject<T> : IDisposable where T : struct
        {
            public T managedObject;
            protected GCHandle handle;
            protected IntPtr ptr;
            protected bool disposed;

            public T ManangedObject
            {
                get
                {
                    return (T)handle.Target;
                }
                set
                {
                    managedObject = value;
                    Marshal.StructureToPtr(value, ptr, false);
                }
            }

            public IntPtr Pointer
            {
                get { return ptr; }
            }

            public PinnedObject()
            {
                handle = GCHandle.Alloc(managedObject, GCHandleType.Pinned);
                ptr = handle.AddrOfPinnedObject();
            }

            ~PinnedObject()
            {
                Dispose();
            }

            public void Dispose()
            {
                if (!disposed)
                {
                    handle.Free();
                    ptr = IntPtr.Zero;
                    disposed = true;
                }
            }

            public void Recopy()
            {
                handle = GCHandle.Alloc(managedObject, GCHandleType.Pinned);
                ptr = handle.AddrOfPinnedObject();
                Marshal.StructureToPtr(managedObject, ptr, false);
            }
        }


        static private PinnedObject<NonDetTest> ndtp;
        // foo(X,Y),writeq(f(X,Y)),nl,X=5.
        public static int Foo(PlTerm t0, PlTerm term2, IntPtr control)
        {
            callNum++;
            if (callNum > 10)
            {
                callNum = 0;
                //return libpl.PL_fail;
            }
            var handle = control;
            FRG fc = (FRG)(libpl.PL_foreign_control(control));

            switch (fc)
            {
                case FRG.PL_FIRST_CALL:
                    unsafe
                    {
                        ndtp = new PinnedObject<NonDetTest>();
                        ndtp.managedObject.start = 1;
                        ndtp.managedObject.stop = 3;
                        //ndtp.managedObject.fid = libpl.PL_open_foreign_frame();

                        ndtp.Recopy();
                        IntPtr ctxt = ndtp.Pointer;
                        goto redo;
                        int succeed = CountTo(t0, term2, ref ndtp.managedObject);
                        if (ndtp.managedObject.start <= ndtp.managedObject.stop)
                        {
                            libpl.PL_retry_address(ctxt);
                        }
                        if (succeed == 0) return 0;
                        return 3;
                    }
                    break;
                case FRG.PL_REDO:
                    unsafe
                    {
                        goto redo;
                        NonDetTest* o = (NonDetTest*)0;
                        IntPtr ctxt = libpl.PL_foreign_context_address(control);
                        if (!ctxt.ToString().Equals("0"))
                        {
                            o = (NonDetTest*)ctxt;
                        }
                        else
                        {
                            o = (NonDetTest*)ndtp.Pointer;
                        }
                        int succeed = CountTo(t0, term2, ref *o);
                        NonDetTest managedObject = *o;
                        if (managedObject.start <= managedObject.stop)
                        {
                            libpl.PL_retry_address(ctxt);
                            if (succeed == 0) return 0;
                            return 3;
                        }
                        if (managedObject.fid != 0)
                        {
                            libpl.PL_close_foreign_frame(managedObject.fid);
                            managedObject.fid = 0;
                        }
                        if (succeed == 0) return 0;
                        return 1;
                    }
                    break;
                case FRG.PL_CUTTED:
                    unsafe
                    {
                        NonDetTest* o = (NonDetTest*)0;
                        IntPtr ctxt = libpl.PL_foreign_context_address(control);
                        if (!ctxt.ToString().Equals("0"))
                        {
                            o = (NonDetTest*)ctxt;
                        }
                        else
                        {
                            o = (NonDetTest*)ndtp.Pointer;
                        }
                        NonDetTest managedObject = *o;
                        if (managedObject.fid != 0)
                        {
                            libpl.PL_close_foreign_frame(managedObject.fid);
                            managedObject.fid = 0;
                        }
                        return libpl.PL_succeed;

                    }
                    break;
                default:
                    {
                        throw new PlException("no frg");
                        return libpl.PL_fail;
                    }
                    break;
            }
        redo:
            unsafe
            {
                NonDetTest* o = (NonDetTest*)0;
                IntPtr ctxt = libpl.PL_foreign_context_address(control);
                var fc0 = libpl.PL_foreign_context(control);
                if (!ctxt.ToString().Equals("0"))
                {
                    o = (NonDetTest*)ctxt;
                }
                else
                {
                    o = (NonDetTest*)ndtp.Pointer;
                }
                int succeed = CountTo(t0, term2, ref *o);
                NonDetTest managedObject = *o;
                if (managedObject.start <= managedObject.stop)
                {
                    libpl.PL_retry_address(ctxt);
                    if (succeed == 0) return 0;
                    return 3;
                }
                if (managedObject.fid != 0)
                {
                    libpl.PL_close_foreign_frame(managedObject.fid);
                    managedObject.fid = 0;
                }
                if (succeed == 0) return 0;
                return 1;
            }
        }

        static public AbstractNondetMethod Fn015 = new ForNext(0, 15);

        // test with (foo2(X,Y)->writeln(p(X,Y));writeln(p(X,Y))),!.
        // test with (foo2(X,Y) *->writeln(p(X,Y));writeln(p(X,Y)),!).
        public static int FooTwo(PlTerm a0, PlTerm a1, IntPtr control)
        {
            var handle = control;
            FRG fc = (FRG)(libpl.PL_foreign_control(control));

            switch (fc)
            {
                case FRG.PL_FIRST_CALL:
                    {
                        var v = NondetContextHandle.ObtainHandle(control, new ForNext(1, a0.intValue()));
                        bool res = v.Setup(new PlTermV(a0, a1));
                        bool more = v.HasMore();
                        if (more)
                        {
                            libpl.PL_retry(v.Handle);
                            return res ? 3 : 0;
                        }
                        return res ? 1 : 0;
                    } break;
                case FRG.PL_REDO:
                    {
                        var v = NondetContextHandle.FindHandle(control);
                        bool res = v.Call(new PlTermV(a0, a1));
                        bool more = v.HasMore();
                        if (more)
                        {
                            libpl.PL_retry(v.Handle);
                            return res ? 3 : 0;
                        }
                        return res ? 1 : 0;
                    } break;
                case FRG.PL_CUTTED:
                    {
                        var v = NondetContextHandle.FindHandle(control);
                        bool res = v.Close(new PlTermV(a0, a1));
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

        public static int FooThree(PlTerm a0, int arity, IntPtr control)
        {
            var handle = control;
            FRG fc = (FRG)(libpl.PL_foreign_control(control));

            switch (fc)
            {
                case FRG.PL_FIRST_CALL:
                    {
                        var v = NondetContextHandle.ObtainHandle(control);
                        var tv = new PlTermV(a0, arity);
                        bool res = v.Setup(tv);
                        bool more = v.HasMore();
                        if (more)
                        {
                            libpl.PL_retry(v.Handle);
                            return res ? 3 : 0;
                        }
                        return res ? 1 : 0;
                    } break;
                case FRG.PL_REDO:
                    {
                        var v = NondetContextHandle.FindHandle(control);
                        bool res = v.Call(new PlTermV(a0, arity));
                        bool more = v.HasMore();
                        if (more)
                        {
                            libpl.PL_retry(v.Handle);
                            return res ? 3 : 0;
                        }
                        return res ? 1 : 0;
                    } break;
                case FRG.PL_CUTTED:
                    {
                        var v = NondetContextHandle.FindHandle(control);
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

        private static int CountTo(PlTerm term, PlTerm term2, ref NonDetTest o)
        {
            try
            {

                var c = o.start;
                bool succed = term.Unify("callnum" + c);
                if (!succed)
                {
                    succed = term2.Unify("callnum" + c);
                }

                if (succed)
                {
                    succed = term2.Unify(term);
                }
                if (succed)
                {
                    return libpl.PL_succeed;
                }
                return libpl.PL_fail;
            }
            finally
            {
                o.start++;
            }

        }

        public static T InvokeFromC<T>(Func<T> action, bool discard)
        {
            Thread threadCurrentThread = Thread.CurrentThread;
            RegisterThread(threadCurrentThread);
            uint fid = libpl.PL_open_foreign_frame();

            try
            {
                return action();
            }
            finally 
            {
                if (discard) libpl.PL_discard_foreign_frame(fid);
                DeregisterThread(threadCurrentThread);
            }
        }
    }
    public class ForNext : AbstractNondetMethod
    {
        private int start = 0;
        private int end = 0;
        public ForNext(int i, int ii)
        {
            start = i;
            end = ii;
        }

        public override AbstractNondetMethod Clone()
        {
            return new ForNext(start, end);
        }

        #region Overrides of AbstractNondetMethod

        public override bool Setup(PlTermV a0)
        {
            return Call(a0);
        }

        public override bool Call(PlTermV a0)
        {
            bool success = false;
            try
            {
                for (int i = 0; i < a0.Size; i++)
                {
                    if (a0[i].Unify(start))
                    {
                        success = true;
                    }
                }
            }
            finally
            {
                start++;
            }
            return success;

        }

        public override bool Close(PlTermV a0)
        {
            end = start + 1;
            return true;
        }

        public override bool HasMore()
        {
            return start <= end;
        }

        #endregion
    }

    public delegate int NonDetDelegate(PlTerm term, PlTerm term2);
}