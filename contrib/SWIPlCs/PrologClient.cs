using System;
using System.Collections;
using System.Collections.Generic;
using System.IO;
using System.Reflection;
using System.Runtime.InteropServices;
using System.Threading;
using java.util;
using jpl;
using SbsSW.SwiPlCs.Callback;
using SbsSW.SwiPlCs.Exceptions;
using SbsSW.SwiPlCs.Streams;
using System.Windows.Forms;
using Hashtable=java.util.Hashtable;

namespace SbsSW.SwiPlCs
{
    public class PrologClient
    {
        public delegate object AnyMethod(params object[] any);

        public static void InternMethod(string module, string pn, AnyMethod d)
        {
            InternMethod(module, pn, d.Method);
        }
        public static void InternMethod(string module, string pn, MethodInfo list)
        {
            Type type = list.DeclaringType;
            pn = pn ?? (type.Name + "." + list.Name);
            ParameterInfo[] ps = list.GetParameters();
            Type rt = list.ReturnType;
            int arity = ps.Length;
            bool nonvoid = rt != typeof(void);
            Delegate del
                = new DelegateParameterVarArgs((PlTermV termVector) =>
                                                   {
                                                       try
                                                       {
                                                           object[] newVariable = new object[arity];
                                                           int argnum = 0;
                                                           foreach (var o in newVariable)
                                                           {
                                                               newVariable[argnum] = ToVM(termVector[argnum], ps[argnum].ParameterType);
                                                               argnum++;
                                                           }
                                                           if (nonvoid)
                                                           {
                                                               return
                                                                   termVector[arity].Unify(
                                                                       ToProlog(list.Invoke(null, newVariable)));
                                                           }
                                                           else
                                                           {
                                                               list.Invoke(null, newVariable);
                                                           }
                                                           return true;
                                                       }
                                                       catch (Exception e)
                                                       {

                                                           throw new PlException(e.Message, e);
                                                       }
                                                   });
            libpl.PL_register_foreign_in_module(module, pn, arity + (nonvoid ? 1 : 0), del,
                                                (int)PlForeignSwitches.VarArgs);

        }

        private static PlTerm ToProlog(object o)
        {
            return new PlTerm("" + o);
        }

        private static Object ToVM(PlTerm o, Type pt)
        {
            if (pt == typeof(PlTerm)) return o;
            if (pt == typeof(string))
            {
                return (string)o;
            }
            switch (o.PlType)
            {
                case PlType.PlUnknown:
                    {
                        return (string)o;
                    }
                    break;
                case PlType.PlVariable:
                    {
                        return o;
                    }
                    break;
                case PlType.PlAtom:
                    {
                        return (string)o;
                    }
                    break;
                case PlType.PlInteger:
                    {
                        return (long)o;
                    }
                    break;
                case PlType.PlFloat:
                    {
                        return (double)o;
                    }
                    break;
                case PlType.PlString:
                    {
                        return (string)o;
                    }
                    break;
                case PlType.PlTerm:
                    {
                        return (string)o;
                    }
                    break;
                default:
                    throw new ArgumentOutOfRangeException();
            }
            return o.ToString();
        }


        ///<summary>
        ///</summary>
        ///<exception cref="NotImplementedException"></exception>
        public void Dispose()
        {

        }

        public object Eval(object obj)
        {
            throw new NotImplementedException();
        }

        public void Intern(string varname, object value)
        {
            throw new NotImplementedException();
        }

        public bool IsDefined(string name)
        {
            throw new NotImplementedException();
        }

        public object GetSymbol(string name)
        {
            throw new NotImplementedException();
        }

        public object Read(string line, TextWriter @delegate)
        {
            throw new NotImplementedException();
        }

        static public void load_swiplcs()
        {
            
        }

        private static void SetupProlog()
        {
            string plhome = Environment.GetEnvironmentVariable("SWI_HOME_DIR");
            if (string.IsNullOrEmpty(plhome))
            {
                plhome = "c:\\Program Files (x86)\\pl";
                Environment.SetEnvironmentVariable("SWI_HOME_DIR", plhome);
            }
            if (!File.Exists(plhome + "\\boot32.prc") && !File.Exists(plhome + "\\boot.prc") && !File.Exists(plhome + "\\boot64.prc"))
            {
                Console.WriteLine("RC file missing!");
            }
            String path = Environment.GetEnvironmentVariable("PATH");
            if (path != null)
                if (!path.ToLower().StartsWith(plhome.ToLower()))
                    Environment.SetEnvironmentVariable("PATH", plhome + "\\bin;" + path);
            string libpath = "";
            libpath += ".";
            //libpath += ";";
            //libpath += "c:\\Program Files (x86)\\pl\\bin";
            //libpath += ";";
            //libpath += "c:\\development\\opensim4opencog\\bin";
            java.lang.System.setProperty("java.library.path", libpath);

            try
            {
                jpl.fli.Prolog.initialise();
                if (!PlEngine.IsInitialized)
                {
                    String[] param = { "-q" }; // suppressing informational and banner messages
                    PlEngine.Initialize(param);
                }
                PlEngine.SetStreamFunctionRead(PlStreamType.Input, new DelegateStreamReadFunction(Sread));
                PlAssert("jpl:jvm_ready");
                PlAssert("module_transparent(jvm_ready)");
            }
            catch (Exception exception)
            {
                WriteException(exception);
                return;
            }
        }

        public static void Main(string[] args)
        {
            SetupProlog();
                
            DoQuery(new Query("asserta(fff(1))"));
            DoQuery(new Query("asserta(fff(9))"));
            DoQuery(new Query(new jpl.Atom("nl")));
            DoQuery(new Query(new jpl.Atom("flush")));

            

            PlAssert("father(martin, inka)");
            PlQuery.PlCall("assert(father(uwe, gloria))");
            PlQuery.PlCall("assert(father(uwe, melanie))");
            PlQuery.PlCall("assert(father(uwe, ayala))");
            using (PlQuery q = new PlQuery("father(P, C), atomic_list_concat([P,' is_father_of ',C], L)"))
            {
                foreach (PlTermV v in q.Solutions)
                    Console.WriteLine(ToCSString(v));

                foreach (PlQueryVariables v in q.SolutionVariables)
                    Console.WriteLine(v["L"].ToString());


                Console.WriteLine("all child's from uwe:");
                q.Variables["P"].Unify("uwe");
                foreach (PlQueryVariables v in q.SolutionVariables)
                    Console.WriteLine(v["C"].ToString());
            }
            //PlQuery.PlCall("ensure_loaded(library(thread_util))");
            //Warning: [Thread 2] Thread running "thread_run_interactor" died on exception: thread_util:attach_console/0: Undefined procedure: thread_util:win_open_console/5
            //PlQuery.PlCall("interactor");
            //Delegate Foo0 = foo0;
            RegisterPLCSForeigns();

            PlAssert("tc2:-foo2(X,Y),writeq(f(X,Y)),nl,X=5");
            PlAssert("tc3:-foo3(X,Y),writeq(f(X,Y)),nl,X=5");
            libpl.PL_toplevel();
            Console.WriteLine("press enter to exit");
            Console.ReadLine();
            PlEngine.PlCleanup();
            Console.WriteLine("finshed!");


        }

        private static void RegisterPLCSForeigns()
        {
            PlForeignSwitches Nondeterministic = PlForeignSwitches.Nondeterministic;
            PlEngine.RegisterForeign(null, "foo2", 2, new DelegateParameterBacktrack2(FooTwo), Nondeterministic);
            PlEngine.RegisterForeign(null, "foo3", 2, new DelegateParameterBacktrackVarArgs(FooThree), Nondeterministic | PlForeignSwitches.VarArgs);
            InternMethod(null, "cwl2", typeof(PrologClient).GetMethod("FooMethod"));
            InternMethod(null, "cwl", typeof(Console).GetMethod("WriteLine", new Type[] { typeof(string) }));
        }

        private static void DoQuery(Query query)
        {
            try
            {

                //if (!query.isOpen()) query.open();
                while (query.hasMoreSolutions())
                {
                    Hashtable ht = query.nextSolution();
                    foreach (var list in ToEnumer(ht.elements()))
                    {
                        string s = "" + list;
                        Console.WriteLine(s);
                    }
                }
            }
            catch (Exception exception)
            {
                WriteException(exception);
            }

        }

        private static void WriteException(Exception exception)
        {
            java.lang.Exception ex = exception as java.lang.Exception;
            if (ex != null)
            {
                ex.printStackTrace();
            }
            Console.WriteLine("SWIPL: " + exception);
        }

        private static IEnumerable ToEnumer(Enumeration enumeration)
        {
            List<object> list = new List<object>();
            while (enumeration.hasMoreElements())
            {
                list.Add(enumeration.nextElement());
            }
            return list;
        }
        private static IEnumerable ToEnumer(Iterator enumeration)
        {
            List<object> list = new List<object>();
            while (enumeration.hasNext())
            {
                list.Add(enumeration.next());
            }
            return list;
        }

        public static void FooMethod(String print)
        {
            //DoQuery(new Query("asserta(jpl:jvm_ready)."));
            //DoQuery(new Query("asserta(jpl:jpl_c_lib_version(3-3-3-3))."));

            //DoQuery(new Query("module(jpl)."));
            JplSafeNativeMethods.install();
            DoQuery(new Query("ensure_loaded(library(jpl))."));
            DoQuery(new Query("module(user)."));
            //DoQuery(new Query("load_foreign_library(foreign(jpl))."));
            // DoQuery(new Query(new jpl.Compound("member", new Term[] { new jpl.Integer(1), new jpl.Variable("H") })));
            //DoQuery(new Query(new jpl.Atom("interactor")));
            //DoQuery(new Query(new jpl.Compound("writeq", new Term[] { new jpl.Integer(1) })));

            Console.WriteLine(print);
        }

        static internal long Sread(IntPtr handle, System.IntPtr buffer, long buffersize)
        {
            int i = Console.Read();
            if (i == -1) return 0;
            string s = "" + (char)i;
            byte[] array = System.Text.Encoding.Unicode.GetBytes(s);
            System.Runtime.InteropServices.Marshal.Copy(array, 0, buffer, array.Length);
            return array.Length;
        }


        //[TestMethod]
        public void StreamRead()
        {
            DelegateStreamReadFunction rf = new DelegateStreamReadFunction(Sread);
            PlEngine.SetStreamFunctionRead(PlStreamType.Input, rf);
            // NOTE: read/1 needs a dot ('.') at the end
            PlQuery.PlCall("assert( (test_read(A) :- read(A)) )");
            PlTerm t = PlQuery.PlCallQuery("test_read(A)");
            //     Assert.AreEqual(ref_string_read, t.ToString() + ".");
        }
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
                        var v = NondetContextHandle.ObtainHandle(control, new ForNext(1, 3));
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
                        bool res = v.Setup(new PlTermV(a0, arity));
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


        private static string ToCSString(PlTermV termV)
        {
            int s = termV.Size;

            //var a0= termV.A0;
            PlTerm v0 = termV[0];
            PlTerm v1 = termV[1];
            PlQuery.PlCall("write", new PlTermV(v0));
            PlQuery.PlCall("nl");
            PlQuery.PlCall("writeq", new PlTermV(v1));
            PlQuery.PlCall("nl");
            return "";
        }

        private static void PlAssert(string s)
        {
            PlQuery.PlCall("assert((" + s + "))");
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

    }


    [System.Security.SuppressUnmanagedCodeSecurityAttribute]
    public static class JplSafeNativeMethods
    {
        //private const string DllFileName = @"D:\Lesta\swi-pl\pl\bin\LibPl.dll";
        private const string DllFileName = @"jpl.dll";//"libpl.dll" for 5.7.8; //was 

        public static string DllFileName1
        {
            get { return DllFileName; }
        }
        [DllImport(DllFileName)]
        public static extern void install();
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