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
using Util = ikvm.runtime.Util;
using CycFort = SbsSW.SwiPlCs.PlTerm;
using PrologCli = SbsSW.SwiPlCs.PrologClient;

namespace SbsSW.SwiPlCs
{
    public partial class PrologClient
    {

        public delegate object AnyMethod(params object[] any);

        /// <summary>
        /// the .Net process (Not OS)
        /// </summary>
        /// <returns></returns>
        internal static bool Is64BitRuntime()
        {
#if _PL_X64
            return true;
#endif
            int bits = IntPtr.Size * 8;
            return bits == 64;
        }

        public static List<Assembly> AssembliesLoaded = new List<Assembly>();
        public static List<Assembly> AssembliesLoading = new List<Assembly>();
        public static List<Type> TypesLoaded = new List<Type>();
        public static List<Type> TypesLoading = new List<Type>();

        public static Dictionary<Thread, IntPtr> SafeThreads = new Dictionary<Thread, IntPtr>();
        public static Dictionary<int, Thread> engineToThread = new Dictionary<int, Thread>();

        public static void RegisterMainThread()
        {
            lock (SafeThreads)
            {
                var t = Thread.CurrentThread;
                SafeThreads.Add(t, IntPtr.Zero);
                int self = libpl.PL_thread_self();
                engineToThread.Add(self, t);
            }
        }
        public static bool OneToOneEnginesPeThread = true;
        public static void RegisterThread(Thread thread)
        {
            lock (SafeThreads)
            {

                int self = libpl.PL_thread_self();
                IntPtr _iEngineNumber;
                IntPtr _oiEngineNumber;
                Thread otherThread;
                bool threadOnceHadEngine = SafeThreads.TryGetValue(thread, out _iEngineNumber);
                bool plthreadHasThread = engineToThread.TryGetValue(self, out otherThread);
                bool plThreadHasDifferntThread = false;
                if (plthreadHasThread)
                {
                    plThreadHasDifferntThread = otherThread != thread;
                }
                if (threadOnceHadEngine)
                {
                    if (self < 1)
                    {
                        Debug("self < 1: " + thread);
                        return; //maybe mnot fine                       
                    }
                    if (plThreadHasDifferntThread)
                    {
                        Debug("plThreadHasDifferntThread " + thread);
                        return; //maybe mnot fine       
                    }
                    return; // all was fine;
                  //  if (thread == CreatorThread || true) return;

                    int iRet = CheckEngine();
                   
                    return; // all was fine;
                }
                else
                {
                    // thread never had engine
                    int ret = libpl.PL_thread_attach_engine(IntPtr.Zero);
                    int self0 = libpl.PL_thread_self();
                    if (ret == self0)
                    {
                        SafeThreads.Add(thread, IntPtr.Zero);
                        engineToThread[self0] = thread;
                        RegisterThread(thread);
                        return;
                    }
                    _iEngineNumber = libpl.PL_create_engine(IntPtr.Zero);
                    SafeThreads.Add(thread, _iEngineNumber);
                    int self2 = libpl.PL_thread_self();
                    if (self2 == -1)
                    {
                        if (libpl.PL_is_initialised(IntPtr.Zero, IntPtr.Zero) == libpl.PL_fail)
                        {
                            try
                            {
                                ret = libpl.PL_thread_attach_engine(_iEngineNumber);
                                int self3 = libpl.PL_thread_self();
                                engineToThread.Add(self3, thread);
                                return;
                            }
                            catch (Exception ex)
                            {
                                throw (new PlException("PL_create_engine : " + ex.Message));
                            }
                        } else
                        {
                            //int ret = libpl.PL_thread_attach_engine(_iEngineNumber);
                            IntPtr pNullPointer = IntPtr.Zero;
                            int iRet = libpl.PL_set_engine(_iEngineNumber, ref pNullPointer);
                            switch (iRet)
                            {
                                case libpl.PL_ENGINE_SET:
                                    {
                                        int self4 = libpl.PL_thread_self();
                                        engineToThread.Add(self4, thread);
                                        return; // all is fine!
                                    }
                                case libpl.PL_ENGINE_INVAL: throw (new PlLibException("PlSetEngine returns Invalid")); //break;
                                case libpl.PL_ENGINE_INUSE: throw (new PlLibException("PlSetEngine returns it is used by an other thread")); //break;
                                default: throw (new PlLibException("Unknown return from PlSetEngine"));
                            }
                            int self3 = libpl.PL_thread_self();
                            engineToThread.Add(self3, thread);
                        }
                    }

                    engineToThread.Add(self2, thread);
                }
                /*
                //
                if (self != ret)
                {
                    engineToThread[ret] = thread;
                }

                if (engineToThread.TryGetValue(self, out otherThread))
                {
                    // All good!
                    if (otherThread == thread)
                        return;
                    bool othreadOnceHadEngine = SafeThreads.TryGetValue(otherThread, out _oiEngineNumber);
                    int ret = libpl.PL_thread_attach_engine(_iEngineNumber);
                    if (self != ret)
                    {
                        engineToThread[ret] = thread;
                        //what does this mean?
                        SafeThreads.TryGetValue(thread, out _iEngineNumber);
                    }
                }
                libpl.PL_set_engine(libpl.PL_ENGINE_CURRENT, ref oldEngine);
                if (!OneToOneEnginesPeThread)
                {
                }
                SafeThreads.Add(thread, _iEngineNumber);
                  */
            }
        }

        private static void Debug(string plthreadhasdifferntthread)
        {
           
        }

        private static int CheckEngine()
        {
            IntPtr _iEngineNumber;
            IntPtr pNullPointer = IntPtr.Zero;
            IntPtr PL_ENGINE_CURRENT_PTR = new IntPtr(libpl.PL_ENGINE_CURRENT); // ((PL_engine_t)0x2)
            int iRet = libpl.PL_set_engine(PL_ENGINE_CURRENT_PTR, ref pNullPointer);
            if (libpl.PL_ENGINE_SET == iRet) return iRet;
            switch (iRet)
            {
                case libpl.PL_ENGINE_SET:
                    {
                        break; // all is fine!
                    }
                case libpl.PL_ENGINE_INVAL:
                    throw (new PlLibException("PlSetEngine returns Invalid")); //break;
                case libpl.PL_ENGINE_INUSE:
                    throw (new PlLibException("PlSetEngine returns it is used by an other thread")); //break;
                default:
                    throw (new PlLibException("Unknown return from PlSetEngine"));
            }

            return iRet;
        }


        public static void DeregisterThread(Thread thread)
        {
            return;
            lock (SafeThreads)
            {
                int self = libpl.PL_thread_self();
                IntPtr _iEngineNumber;
                if (!SafeThreads.TryGetValue(thread, out _iEngineNumber))
                {
                    return;
                }
                SafeThreads.Remove(thread);
                if (libpl.PL_destroy_engine(_iEngineNumber) != 0)
                {
                    try
                    {
                        _iEngineNumber = libpl.PL_create_engine(IntPtr.Zero);
                    }
                    catch (Exception ex)
                    {
                        throw (new PlException("PL_create_engine : " + ex.Message));
                    }
                }
            }
        }

        /// <summary>
        /// The OS and not the .Net process
        ///  therefore "Program Files" are either for 64bit or 32bit apps
        /// </summary>
        /// <returns></returns>
        internal static bool Is64BitComputer()
        {
            return Is64BitRuntime() || !String.IsNullOrEmpty(Environment.GetEnvironmentVariable("ProgramFiles(x86)"));
        }

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
            if (isbool && isStatic)
            {
                bool isVanilla = true;
                foreach (ParameterInfo info in ps)
                {
                    if (info.ParameterType != typeof(PlTerm))
                    {
                        isVanilla = false;
                        break;
                    }
                }
                if (isVanilla)
                {
                    Delegate d = null;
                    switch (paramlen)
                    {
                        case 0:
                            {
                                d = new DelegateParameter0(() => (bool)InvokeCaught(list, null, new object[0]));
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
                        default:
                            break;
                    }
                }
            }
            int plarity = paramlen + (hasReturnValue ? 1 : 0) + (isStatic ? 0 : 1);

            Delegate del
                = new DelegateParameterVarArgs((PlTermV termVector) =>
                                                   {
                                                       if (termVector.Size != plarity)
                                                       {
                                                           return false;
                                                       }
                                                       object target = isStatic
                                                                           ? null
                                                                           : CastTerm(termVector[0], type) ??
                                                                             defaultInstanceWhenMissing;
                                                       object[] newVariable = new object[paramlen];

                                                       int tvargnum = isStatic ? 0 : 1;
                                                       for (int argnum = 0; argnum < paramlen; argnum++)
                                                       {
                                                           newVariable[argnum] = CastTerm(termVector[tvargnum],
                                                                                      ps[argnum].ParameterType);
                                                           tvargnum++;
                                                       }

                                                       object result = InvokeCaught(list, target, newVariable);

                                                       if (isbool)
                                                       {
                                                           return (bool)result;
                                                       }
                                                       if (nonvoid)
                                                       {
                                                           return termVector[plarity - 1].FromObject((result));
                                                       }
                                                       return true;

                                                   });

            PlEngine.RegisterForeign(module, pn, plarity, del, PlForeignSwitches.VarArgs);

        }

        private static object InvokeCaught(MethodInfo info, object o, object[] os)
        {
            try
            {
                return info.Invoke(o, os);
            }
            catch (Exception ex)
            {
                var pe = ToPlException(ex);
                string s = pe.ToString() + "\n" + pe.StackTrace;
                throw pe;
            }
        }

        private static PlException ToPlException(Exception ex)
        {
            if (ex is PlException) return (PlException)ex;
            var ie = ex.InnerException;
            if (ie != null)
            {
                if (ie is PlException) return (PlException)ie;
                ex = ie;
            }
            return new PlException(ex.Message, ex);
        }
        public static bool Warn(string text)
        {
            return libpl.PL_warning(text) == 0;
        }

        private static PlTerm ToPlList(PlTerm[] terms)
        {
            int termLen = terms.Length;
            if (termLen == 0) return ATOM_NIL;
            termLen--;
            PlTerm ret = listOfOne(terms[termLen]);
            while (--termLen >= 0)
            {
                ret = PlTerm.PlCompound(".", terms[termLen], ret);
            }
            return ret;
        }

        private static PlTermV ToPlTermV(PlTerm[] terms)
        {
            var tv = NewPlTermV(terms.Length);
            for (int i = 0; i < terms.Length; i++)
            {
                tv[i] = terms[i];
            }
            return tv;
        }

        private static PlTermV NewPlTermV(int length)
        {
            return new PlTermV(length);
        }

        private static PlTermV ToPlTermVParams(ParameterInfo[] terms)
        {
            var tv = NewPlTermV(terms.Length);
            for (int i = 0; i < terms.Length; i++)
            {
                tv[i] = typeToSpec(terms[i].ParameterType);
            }
            return tv;
        }
        private static PlTerm ToPlListParams(ParameterInfo[] terms)
        {
            PlTerm listOf = PlTerm.PlAtom("[]");
            for (int i = terms.Length - 1; i >= 0; i--)
            {
                PlTerm term = typeToSpec(terms[i].ParameterType);
                listOf = PlTerm.PlCompound(".", term, listOf);
            }
            return listOf;
        }
        private static PlTerm ToPlListTypes(Type[] terms)
        {
            PlTerm listOf = PlTerm.PlAtom("[]");
            for (int i = terms.Length - 1; i >= 0; i--)
            {
                PlTerm term = typeToSpec(terms[i]);
                listOf = PlTerm.PlCompound(".", term, listOf);
            }
            return listOf;
        }
        private static PlTermV ToPlTermVSpecs(Type[] terms)
        {
            var tv = NewPlTermV(terms.Length);
            for (int i = 0; i < terms.Length; i++)
            {
                tv[i] = typeToSpec(terms[i]);
            }
            return tv;
        }

        private static PlTerm paramInfoToSpecs(ParameterInfo[] terms)
        {
            int termLen = terms.Length;
            if (termLen == 0) return ATOM_NIL;
            if (termLen == 1)
            {
                return listOfOne(typeToSpec(terms[0].ParameterType));
            }
            termLen--;
            PlTerm ret = listOfLeastOne(typeToSpec(terms[0].ParameterType));
            PlTerm current = ret;
            for (int i = 1; i < termLen; i++)
            {
                ParameterInfo info = terms[i];
                var current2 = listOfLeastOne(typeToSpec(info.ParameterType));
                current[2].Unify(current2);
                current = current2;
            }
            current[2].Unify(listOfOne(typeToSpec(terms[termLen].ParameterType)));
            return ret;
        }

        private static PlTerm listOfOne(PlTerm term)
        {
            return PlTerm.PlCompound(".", term, ATOM_NIL);
        }
        private static PlTerm listOfLeastOne(PlTerm term)
        {
            return PlTerm.PlCompound(".", term, PlTerm.PlVar());
        }

        protected static PlTerm ATOM_NIL
        {
            get { return PlTerm.PlAtom("[]"); }
        }

#if plvar_pins
        public static Dictionary<Int64, PlRef> termToObjectPins = new Dictionary<Int64, PlRef>();
        public static Dictionary<object, PlRef> objectToPlRef = new Dictionary<object, PlRef>();
        public static Dictionary<string, PlRef> atomToPlRef = new Dictionary<string, PlRef>();
#endif
        public static PlTerm PLVOID { get { return PlTerm.PlCompound("@", PlTerm.PlAtom("null")); } }
        public static PlTerm PLNULL { get { return PlTerm.PlCompound("@", PlTerm.PlAtom("void")); } }
        public static PlTerm PLTRUE { get { return PlTerm.PlCompound("@", PlTerm.PlAtom("true")); } }
        public static PlTerm PLFALSE { get { return PlTerm.PlCompound("@", PlTerm.PlAtom("false")); } }


        readonly static private Dictionary<object, string> ObjToTag = new Dictionary<object, string>();
        readonly static private Dictionary<string, object> TagToObj = new Dictionary<string, object>();
        public static object tag_to_object(string s)
        {
            if (string.IsNullOrEmpty(s) || s == "void" || !s.StartsWith("C#"))
            {
                Warn("tag_to_object: " + s);
            }
            lock (ObjToTag)
            {
                object o;
                if (TagToObj.TryGetValue(s, out o))
                {
                    return o;
                }
                Warn("tag_to_object: " + s);
                return jpl.fli.Prolog.tag_to_object(s);
            }
        }
        public static string object_to_tag(object o)
        {
            if (o == null)
            {
                Warn("object_to_tag: NULL");
                return null;
            }

            Type t = o.GetType();
            if (IsStructRecomposable(t) || t.IsPrimitive)
            {
                Warn("object_to_tag:" + t + " from " + o);
            }

            lock (ObjToTag)
            {
                string s;
                if (ObjToTag.TryGetValue(o, out s))
                {
                    return s;
                }
                GCHandle gch = GCHandle.Alloc(o, GCHandleType.Normal);
                IntPtr iptr = (IntPtr)gch;
                s = "C#" + iptr.ToInt64();
                ObjToTag[o] = s;
                TagToObj[s] = o;
                if (ObjToTag.Count % 10000 == 0)
                {
                    Console.WriteLine("ObjToTag=" + ObjToTag);
                }

                return s;
            }
            //return jpl.fli.Prolog.object_to_tag(o);
        }

           internal static Term[] ToJPL(PlTermV args)
        {
            int UPPER = args.Size;
            Term[] target = new Term[UPPER];
            for (int i = 0; i < UPPER; i++)
            {
                target[i] = ToJPL(args[i]);
            }
            return target;
        }

        internal static jpl.fli.term_t ToFLI(PlTermV args)
        {
            return ToFLI(args.A0);
        }

        internal static jpl.fli.term_t ToFLI(PlTerm args)
        {
            return ToFLI(args.TermRef);
        }


        internal static PlTerm ToPLCS(Term args)
        {
            if (args is Atom) return new PlTerm(args.name());
            if (args is jpl.Variable) return new PlTerm((uint)GetField(args, "term_"));
            if (args is jpl.Float) return new PlTerm(args.doubleValue());
            if (args is jpl.Integer) return new PlTerm(args.longValue());
            if (args is jpl.Compound) return PlTerm.PlCompound(args.name(), ToPLCSV(args.args()));
            if (args is jpl.JRef)
            {
                var jref = (jpl.JRef)args;// return new PlTerm(args.doubleValue());
                var obj = jref.@ref();
                var t = new PlTerm();
                t.FromObject(obj);
                return t;
            }
            throw new ArgumentOutOfRangeException();
        }

        private static PlTermV ToPLCSV(Term[] terms)
        {
            int size = terms.Length;
            PlTermV target = NewPlTermV(size);
            for (int i = 0; i < size; i++)
            {
                target[i] = ToPLCS(terms[i]);
            }
            return target;
        }

        private static PlTermV ToPLCSV(PlTerm[] terms)
        {
            int size = terms.Length;
            PlTermV target = NewPlTermV(size);
            for (int i = 0; i < size; i++)
            {
                target[i] = terms[i];
            }
            return target;
        }

        private static PlTermV ToPLCSV1(PlTerm a0, PlTerm[] terms)
        {
            int size = terms.Length;
            PlTermV target = NewPlTermV(size + 1);
            int to = 1;
            target[0] = a0;
            for (int i = 0; i < size; i++)
            {
                target[to++] = terms[i];
            }
            return target;
        }

        private static object GetField(object term, string s)
        {
            return term.GetType().GetField(s, PrologCli.BindingFlagsALL).GetValue(term);
        }

        private static jpl.fli.term_t ToFLI(uint hndle)
        {
            jpl.fli.term_t t = new jpl.fli.term_t();
            t.value = hndle;
            return t;
        }

        internal static Term ToJPL(PlTerm o)
        {
            switch (o.PlType)
            {
                case PlType.PlAtom:
                    {
                        return new Atom((string)o);
                    }
                    break;
                case PlType.PlInteger:
                    {
                        return new jpl.Integer((long)o);
                    }
                    break;
                case PlType.PlFloat:
                    {
                        return new jpl.Float((double)o);
                    }
                    break;
                case PlType.PlString:
                    {
                        return new jpl.Atom((string)o);
                    }
                    break;
                case PlType.PlTerm:
                    {
                        var a = o.Arity;
                        var c = new jpl.Compound(o.Name, a);
                        for (int i = 1; i <= a; i++)
                        {
                            c.setArg(i, ToJPL(o[i]));
                        }
                        return c;
                    }
                    break;
                case PlType.PlVariable:
                    {
                        var v = new jpl.Variable();
                        SetField(v, "term_", o.TermRef);
                        return v;
                    }
                    break;
                case PlType.PlUnknown:
                    {
                        return jpl.Util.textToTerm((string)o);
                    }
                    break;
                default:
                    throw new ArgumentOutOfRangeException();
            }
        }

        public static void SetField(object target, string name, object value)
        {
            FieldInfo field = target.GetType().GetField(name);
            //if (!field.IsPublic) field..IsPublic = true;
            field.SetValue(field.IsStatic ? null : target, value);
        }

        public static jpl.Term InModule(string s, jpl.Term o)
        {
            if (s == null || s == "" || s == "user") return o;
            return new jpl.Compound(":", new Term[] { new jpl.Atom(s), o });
        }

        ///<summary>
        ///</summary>
        ///<exception cref="NotImplementedException"></exception>
        public void Dispose()
        {

        }

        protected PlTerm ThisClientTerm
        {
            get
            {
                return ToProlog(this);
            }
        }

        public static PlTerm ToProlog(object value)
        {
            PlTerm t = new PlTerm();
            t.FromObject(value);
            return t;
        }

        private bool ModuleCall0(string s, PlTermV termV)
        {
            return PlQuery.PlCall(ClientModule, ClientPrefix + s, termV);
        }

        private bool ModuleCall(string s, params PlTerm[] terms)
        {
            return PlQuery.PlCall(ClientModule, ClientPrefix + s, ToPLCSV1(ThisClientTerm, terms));
        }
        public static PlTerm PlNamed(string name)
        {
            return PlTerm.PlAtom(name);
        }

        public object Eval(object obj)
        {
            PlTerm termin = PlTerm.PlVar();
            termin.FromObject(obj);
            PlTerm termout = PlTerm.PlVar();
            if (!ModuleCall("Eval", termin, termout)) return null;
            return PrologCli.CastTerm(termout, typeof(System.Object));
        }

        public void Intern(string varname, object value)
        {
            PlTerm termin = PlTerm.PlVar();
            termin.FromObject(value);
            ModuleCall("Intern", PlNamed(varname), termin);
        }


        public bool IsDefined(string name)
        {
            return ModuleCall("IsDefined", PlNamed(name));
        }

        public object GetSymbol(string name)
        {
            PlTerm termout = PlTerm.PlVar();
            if (!ModuleCall("GetSymbol", PlNamed(name), termout)) return null;
            return PrologCli.CastTerm(termout, typeof(System.Object));
        }

        public object Read(string line, TextWriter @delegate)
        {
            return new Nullable<PlTerm>(PlTerm.PlCompound(line));
        }

        static public void load_swiplcs()
        {

        }

        private static readonly object PrologIsSetupLock = new object();
        private static bool PrologIsSetup;
        public static void SetupProlog()
        {
            lock (PrologIsSetupLock)
            {
                if (PrologIsSetup) return;
                PrologIsSetup = true;
                SetupProlog0();
                RegisterPLCSForeigns();
            }
        }
        public static void SetupProlog0()
        {
            SetupIKVM();

            if (!IsUseableSwiProlog(SwiHomeDir))
            {
                SwiHomeDir = Environment.GetEnvironmentVariable("SWI_HOME_DIR");
                if (!IsUseableSwiProlog(SwiHomeDir))
                {
                    SwiHomeDir = null;
                }
            }
            if (!IsUseableSwiProlog(SwiHomeDir))
            {

                SwiHomeDir = "c:\\Program Files\\pl";

                if (Is64BitComputer() && !Is64BitRuntime())
                {
                    SwiHomeDir = "c:\\Program Files (x86)\\pl";
                }
            }
            AltSwiHomeDir = AltSwiHomeDir ?? ".";
            bool copyPlatFormVersions = false;
            if (!IsUseableSwiProlog(SwiHomeDir))
            {
                SwiHomeDir = AltSwiHomeDir;
                copyPlatFormVersions = true;
            }
            SwiHomeDir = SwiHomeDir ?? AltSwiHomeDir;
            if (IsUseableSwiProlog(SwiHomeDir))
            {
                Environment.SetEnvironmentVariable("SWI_HOME_DIR", SwiHomeDir);
            }
            string platformSuffix = Is64BitRuntime() ? "-x64" : "-x86";
            if (copyPlatFormVersions)
            {
                string destination = Path.Combine(SwiHomeDir, "bin");
                CopyFiles(destination + platformSuffix, destination, true, "*.*", true);
                destination = Path.Combine(SwiHomeDir, "lib");
                CopyFiles(destination + platformSuffix, destination, true, "*.*", true);
            }
            if (IsUseableSwiProlog(SwiHomeDir))
            {
                Environment.SetEnvironmentVariable("SWI_HOME_DIR", SwiHomeDir);
            }
            String path = Environment.GetEnvironmentVariable("PATH");
            if (path != null)
                if (!path.ToLower().StartsWith(SwiHomeDir.ToLower()))
                {
                    Environment.SetEnvironmentVariable("PATH", SwiHomeDir + "\\bin;" + IKVMHome + ";" + path);
                }
            string libpath = "";


            string swiHomeBin = SwiHomeDir + "\\bin";
            libpath += swiHomeBin;
            if (swiHomeBin != IKVMHome)
            {
                libpath += ";";
                libpath += IKVMHome;
            }
            libpath += ";";
            libpath += ".";

            string LD_LIBRARY_PATH = Environment.GetEnvironmentVariable("LD_LIBRARY_PATH");
            if (String.IsNullOrEmpty(LD_LIBRARY_PATH))
            {
                LD_LIBRARY_PATH = libpath;
                Environment.SetEnvironmentVariable("LD_LIBRARY_PATH", LD_LIBRARY_PATH);
            }


            string CLASSPATH = java.lang.System.getProperty("java.class.path");
            string CLASSPATH0 = Environment.GetEnvironmentVariable("CLASSPATH");

            if (String.IsNullOrEmpty(CLASSPATH))
            {
                CLASSPATH = CLASSPATH0;
            }
            string jplcp = clasPathOf(new jpl.JPL());

            if (!JplDisabled)
                CLASSPATH = IKVMHome + "\\SWIJPL.dll" + ";" + IKVMHome + "\\SWIJPL.jar;" + CLASSPATH0;

            Environment.SetEnvironmentVariable("CLASSPATH", CLASSPATH);
            java.lang.System.setProperty("java.class.path", CLASSPATH);
            java.lang.System.setProperty("java.library.path", libpath);
            try
            {
                JPL.setNativeLibraryDir(SwiHomeDir + "\\bin");
                try
                {
                    JPL.loadNativeLibrary();
                }
                catch (Exception e)
                {
                    WriteException(e);
                    JplDisabled = true;
                }
                if (!JplDisabled)
                {
                    SafelyRun(() => jpl.fli.Prolog.initialise());
                }
                SafelyRun(TestClassLoader);

                //if (IsPLWin) return;
                try
                {
                    if (!PlEngine.IsInitialized)
                    {
                        String[] param = { "-q" }; // suppressing informational and banner messages
                        PlEngine.Initialize(param);
                    }
                    if (IsPLWin) return;
                    PlEngine.SetStreamFunctionRead(PlStreamType.Input, new DelegateStreamReadFunction(Sread));
                    PlQuery.PlCall("nl.");
                }
                catch (Exception e)
                {
                    WriteException(e);
                    PlCsDisabled = true;
                }
                //                PlAssert("jpl:jvm_ready");
                //                PlAssert("module_transparent(jvm_ready)");
            }
            catch (Exception exception)
            {
                WriteException(exception);
                return;
            }
        }

        private static bool IsUseableSwiProlog(string swiHomeDir)
        {
            if (string.IsNullOrEmpty(swiHomeDir)) return false;
            if (!Directory.Exists(swiHomeDir)) return false;
            if (File.Exists(swiHomeDir + "\\bin\\libpl.dll"))
            {
                Console.WriteLine("SWI too old: " + swiHomeDir + "\\bin\\libpl.dll");
                return false;
            }
            if (File.Exists(swiHomeDir + "\\bin\\swipl.dll")) return true;
            if (!File.Exists(swiHomeDir + "\\boot32.prc") &&
                !File.Exists(swiHomeDir + "\\boot.prc") &&
                !File.Exists(swiHomeDir + "\\boot64.prc"))
            {
                Console.WriteLine("RC file missing from " + swiHomeDir);
                return false;
            }
            return true;
        }

        //FileInfo & DirectoryInfo are in System.IO
        //This is something you should be able to tweak to your specific needs.
        static void CopyFiles(string source,
                              string destination,
                              bool overwrite,
                              string searchPattern, bool recurse)
        {
            if (Directory.Exists(source))
                CopyFiles(new DirectoryInfo(source), new DirectoryInfo(destination), overwrite, searchPattern, recurse);
        }

        static void CopyFiles(DirectoryInfo source,
                              DirectoryInfo destination,
                              bool overwrite,
                              string searchPattern, bool recurse)
        {
            FileInfo[] files = source.GetFiles(searchPattern);
            if (!destination.Exists)
            {
                destination.Create();
            }
            foreach (FileInfo file in files)
            {
                string destName = Path.Combine(destination.FullName, file.Name);
                try
                {
                    file.CopyTo(destName, overwrite);
                }
                catch (Exception e0)
                {
                    if (!overwrite)
                    {
                        System.Console.Error.WriteLine("file: " + file + " copy to " + destName + " " + e0);
                    }
                    else
                    {
                        try
                        {
                            if (File.Exists(destName))
                            {
                                if (File.Exists(destName + ".dead")) File.Delete(destName + ".dead");
                                File.Move(destName, destName + ".dead");
                                file.CopyTo(destName, false);
                            }
                        }
                        catch (Exception e)
                        {
                            System.Console.Error.WriteLine("file: " + file + " copy to " + destName + " " + e);
                        }
                    }
                }
            }
            if (recurse)
            {
                foreach (var info in source.GetDirectories())
                {
                    string destName = Path.Combine(destination.FullName, info.Name);
                    try
                    {
                        if (!Directory.Exists(destName)) Directory.CreateDirectory(destName);
                        CopyFiles(info, new DirectoryInfo(destName), overwrite, searchPattern, recurse);
                    }
                    catch (Exception e)
                    {
                        System.Console.Error.WriteLine("file: " + info + " copy to " + destName + " " + e);
                    }
                }
            }
        }

        private static void SetupIKVM()
        {
            if (String.IsNullOrEmpty(IKVMHome)) IKVMHome = Environment.GetEnvironmentVariable("IKVM_BINDIR");
            if (String.IsNullOrEmpty(IKVMHome)) IKVMHome = new FileInfo(typeof(ikvm.runtime.Util).Assembly.Location).DirectoryName;
            if (String.IsNullOrEmpty(IKVMHome)) IKVMHome = Environment.CurrentDirectory;
            Environment.SetEnvironmentVariable("IKVM_BINDIR", IKVMHome);
            DirectoryInfo destination = new DirectoryInfo(IKVMHome);
            DirectoryInfo source;
            if (Is64BitRuntime())
            {
                source = new DirectoryInfo(IKVMHome + "\\bin-x64\\");
            }
            else
            {
                source = new DirectoryInfo(IKVMHome + "\\bin-x86\\");
            }
            if (source.Exists) CopyFiles(source, destination, true, "*.*", false);
        }

        public class ScriptingClassLoader : URLClassLoader
        {
            readonly IList<ClassLoader> lc = new List<ClassLoader>();
            IList<AppDomainAssemblyClassLoader> AppDomainAssemblyClassLoaders = new List<AppDomainAssemblyClassLoader>();
            IList<AssemblyClassLoader> AssemblyClassLoaders = new List<AssemblyClassLoader>();
            IList<ClassPathAssemblyClassLoader> ClassPathAssemblyClassLoaders = new List<ClassPathAssemblyClassLoader>();
            IList<URLClassLoader> URLClassLoaders = new List<URLClassLoader>();
            IList<MethodUtil> MethodUtils = new List<MethodUtil>();

            public ScriptingClassLoader(ClassLoader cl)
                : base(new URL[0], cl)
            {
                AddLoader(cl);
            }

            public void AddLoader(ClassLoader cl)
            {
                lock (lc)
                {
                    if (cl != null)
                    {
                        if (lc.Contains(cl)) return;
                        lc.Add(cl);
                        bool added = false;
                        if (cl is AppDomainAssemblyClassLoader)
                        {
                            AppDomainAssemblyClassLoaders.Add((AppDomainAssemblyClassLoader)cl);
                            added = true;
                        }
                        if (cl is AssemblyClassLoader)
                        {
                            AssemblyClassLoaders.Add((AssemblyClassLoader)cl);
                            added = true;
                        }
                        if (cl is ClassPathAssemblyClassLoader)
                        {
                            ClassPathAssemblyClassLoaders.Add((ClassPathAssemblyClassLoader)cl);
                            added = true;
                        }
                        if (!added)
                        {
                            if (cl is MethodUtil)
                            {
                                MethodUtils.Add((MethodUtil)cl);
                                added = true;
                            }
                            else
                                if (cl is URLClassLoader)
                                {
                                    URLClassLoaders.Add((URLClassLoader)cl);
                                    added = true;
                                }
                        }
                        AddLoader(cl.getParent());
                    }
                }
            }

            public static void Check()
            {

            }

            public string FindLibrary(string libname)
            {
                return base.findLibrary(libname);
            }
            public Class LoadClass(string name, bool resolve)
            {
                return base.loadClass(name, resolve);
            }
            public java.lang.Class ResolveClass(java.lang.Class clz)
            {
                base.resolveClass(clz);
                return clz;
            }
        }

        private static void TestClassLoader()
        {
            //using java.lang;
            //IKVM.Internal.BootstrapClassLoader()
            ScriptingClassLoader cl = new ScriptingClassLoader(ClassLoader.getSystemClassLoader());

            string s = "jpl.fli.term_t";
            Class c;
            try
            {
                c = cl.loadClass(s);
            }
            catch (java.lang.ClassNotFoundException e)
            {
            }
            catch (java.security.PrivilegedActionException e)
            {

            }

            foreach (var s1 in new Type[] { 1.GetType(), true.GetType(), "".GetType(), typeof(void), 'a'.GetType(), typeof(Type[]), typeof(IComparable<Type>) })
            {
                c = ikvm.runtime.Util.getFriendlyClassFromType(s1);
                if (c != null)
                {
                    Console.WriteLine("class: " + c + " from type " + s1.FullName);
                    continue;
                }
                Console.WriteLine("cant get " + s1.FullName);
            }

            foreach (var s1 in new jpl.JPL().GetType().Assembly.GetTypes())
            {
                c = ikvm.runtime.Util.getFriendlyClassFromType(s1);
                if (c != null)
                {
                    //Console.WriteLine("" + c);
                    continue;
                }
                Console.WriteLine("cant get " + s1.FullName);
            }
            return;
        }

        private static string clasPathOf(jpl.JPL jpl1)
        {
            string s = null;
            var cl = jpl1.getClass().getClassLoader();
            if (cl != null)
            {
                var r = cl.getResource(".");
                if (r != null)
                {
                    s = r.getFile();
                }
                else
                {
                    var a = jpl1.GetType().Assembly;
                    if (a != null)
                    {
                        s = a.Location;
                    }
                }
            }
            return s;
        }

        //[MTAThread]
        public static void Main(string[] args0)
        {
            bool demo = false;
            SetupProlog();

            if (demo)
            {
                DoQuery("asserta(fff(1))");
                DoQuery("asserta(fff(9))");
                DoQuery("nl");
                DoQuery("flush");

                PlAssert("father(martin, inka)");
                if (!PlCsDisabled)
                {
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
                }

                PlAssert("tc2:-foo2(X,Y),writeq(f(X,Y)),nl,X=5");
                PlAssert("tc3:-foo3(X,Y,Z),Z,writeln(f(X,Y,Z)),X=5");
            }

            ClassFile.ThrowFormatErrors = false;
            libpl.NoToString = true;
            //SafelyRun((() => PlCall("jpl0")));            
            //SafelyRun((() => DoQuery(new Query(new jpl.Atom("jpl0")))));
            libpl.NoToString = false;
            ClassFile.ThrowFormatErrors = true;
            if (args0.Length > 0)
            {
                int i = 0;
                foreach (var s in args0)
                {
                    if (s == "-f")
                    {
                        string s1 = args0[i + 1];
                        args0[i + 1] = "['" + s1 + "']";
                        continue;
                    }
                    PlCall(s);
                    i++;
                }
            }
            if (!JplDisabled)
            {
                var run = new jpl.Atom("prolog");
                while (!IsHalted) SafelyRun(() => DoQuery(new jpl.Query(run)));

            }
            else
            {
                if (!PlCsDisabled)
                    // loops on exception
                    while (!SafelyRun((() => libpl.PL_toplevel()))) ;
            }



            Console.WriteLine("press enter to exit");
            Console.ReadLine();
            SafelyRun((() => PlEngine.PlCleanup()));

            Console.WriteLine("finshed!");


        }

        private static bool SafelyRun(MethodInvoker invoker)
        {
            try
            {
                invoker();
                return true;
            }
            catch (Exception e)
            {
                WriteException(e);
                return false;
            }
        }

        public static void RegisterPLCSForeigns()
        {
            CreatorThread = Thread.CurrentThread;
            RegisterMainThread();
            PlForeignSwitches Nondeterministic = PlForeignSwitches.Nondeterministic;
            Fn015.Register();
            PlEngine.RegisterForeign(null, "foo2", 2, new DelegateParameterBacktrack2(FooTwo), Nondeterministic);
            //PlEngine.RegisterForeign(null, "cliFindClass", 2, new DelegateParameter2(PrologCli.cliFindClass), PlForeignSwitches.None);
            PlEngine.RegisterForeign(null, "cliLoadAssembly", 1, new DelegateParameter1(PrologCli.cliLoadAssembly), PlForeignSwitches.None);
            PlEngine.RegisterForeign(null, "foo3", 3, new DelegateParameterBacktrackVarArgs(FooThree), Nondeterministic | PlForeignSwitches.VarArgs);

            InternMethod(null, "loadAssembly", typeof(PrologClient).GetMethod("LoadAssembly"));
            InternMethod(null, "cwl", typeof(Console).GetMethod("WriteLine", new Type[] { typeof(string) }));
            RegisterJPLForeigns();
            RegisterThread(CreatorThread);
            //PLNULL = PlTerm.PlCompound("@", PlTerm.PlAtom("null"));
            //PLVOID = PlTerm.PlCompound("@", PlTerm.PlAtom("void"));
            //PLTRUE = PlTerm.PlCompound("@", PlTerm.PlAtom("true"));
            //PLFALSE = PlTerm.PlCompound("@", PlTerm.PlAtom("false"));
        }

        private static Class ResolveClass(string name)
        {
            if (name == "@" || name == "$cli_object" || name == "array" || name == null) return null;
            Type t = ResolveClassAsType(name);
            Class c = ikvm.runtime.Util.getFriendlyClassFromType((Type)t);
            return c;
        }
        private static Type ResolveClassAsType(string name)
        {
            Type s1 = ResolveType0(name);
            if (s1 != null) return s1;
            if (name.EndsWith("[]"))
            {
                Type t1 = ResolveClassAsType(name.Substring(0, name.Length - 2));
                return t1.MakeArrayType();
            }
            var name2 = name.Replace("/", ".");
            if (name2 != name)
            {
                s1 = ResolveType0(name2);
                if (s1 != null) return s1;
            }
            name2 = name.Replace("cli.", "");
            if (name2 != name)
            {
                s1 = ResolveType0(name2);
                if (s1 != null) return s1;
            }
            return null;
        }

        private static Type ResolveType(string name)
        {
            if (name == "@" || name == "$cli_object" || name == "array" || name == null) return null;
            if (name.EndsWith("[]"))
            {
                Type t = ResolveType(name.Substring(0, name.Length - 2));
                return t.MakeArrayType();
            }
            var s1 = ResolveType0(name);
            if (s1 != null) return s1;
            var name2 = name.Replace("/", ".");
            if (name2 != name)
            {
                s1 = ResolveType0(name2);
                if (s1 != null) return s1;
            }
            name2 = name.Replace("cli.", "");
            if (name2 != name)
            {
                s1 = ResolveType0(name2);
                if (s1 != null) return s1;
            }
            return null;
        }

        public static Type ResolveType0(string typeName)
        {
            Type type;
            lock (ShortNameType)
            {
                if (ShortNameType.TryGetValue(typeName, out type))
                {
                    //return type;
                }
            }
            type = type ?? Type.GetType(typeName);
            if (type == null)
            {
                foreach (Assembly loaded in AssembliesLoaded)
                {
                    Type t = loaded.GetType(typeName, false);
                    if (t != null) return t;
                }
                Class obj = null;
                try
                {
                    obj = Class.forName(typeName);
                }
                catch (java.lang.ClassNotFoundException e)
                {
                }
                catch (Exception e)
                {
                }
                if (obj != null)
                {
                    type = ikvm.runtime.Util.getInstanceTypeFromClass((Class)obj);
                }
                if (type == null)
                {
                    type = getPrimitiveType(typeName);
                }
                if (type == null)
                {
                    type = Type.GetTypeFromProgID(typeName);
                }
            }
            return type;
        }

        public static Type getPrimitiveType(String name)
        {
            if (name.StartsWith("["))
            {
                Type t = ResolveType(name.Substring(1));
                return t.MakeArrayType();
            }
            switch (name)
            {
                case "byte":
                case "B":
                    return typeof(byte);
                case "int":
                case "I":
                    return typeof(int);
                case "long":
                case "J":
                    return typeof(long);
                case "short":
                case "S":
                    return typeof(short);
                case "sbyte":
                    return typeof(sbyte);
                case "uint":
                    return typeof(uint);
                case "ulong":
                    return typeof(ulong);
                case "ushort":
                    return typeof(ushort);
                case "decimal":
                    return typeof(decimal);
                case "double":
                    return typeof(double);
                case "float":
                    return typeof(float);
                case "object":
                    return typeof(object);
                case "string":
                    return typeof(string);
                case "void":
                case "V":
                    return typeof(void);
                case "char":
                case "C":
                    return typeof(char);
                case "bool":
                case "boolean":
                case "Z":
                    return typeof(bool);
                default:
                    return null;
            }
        }

        public static bool LoadAssembly(Assembly assembly)
        {
            lock (AssembliesLoaded)
            {
                if (AssembliesLoading.Contains(assembly))
                {
                    return true;
                }
                AssembliesLoading.Add(assembly);
                LoadReferencedAssemblies(assembly);
                // push to the front
                AssembliesLoaded.Remove(assembly);
                AssembliesLoaded.Insert(0, assembly);
            }
            LoadTypesFromAssembly(assembly);
            return true;
        }

        private static void LoadReferencedAssemblies(Assembly assembly)
        {
            foreach (var refed in assembly.GetReferencedAssemblies())
            {
                try
                {
                    LoadAssembly(Assembly.Load(refed));
                }
                catch (Exception e)
                {
                    // Warn(e.Message);
                }
            }
        }

        private static void LoadTypesFromAssembly(Assembly assembly)
        {            
            try
            {
                foreach (Type t in assembly.GetTypes())
                {
                    try
                    {
                        LoadType(t);
                    }
                    catch (Exception te)
                    {
                        // Warn(e.Message);
                    }
                }
            }
            catch (Exception e)
            {
                // get types problem
                // Warn(e.Message);
            }
        }

        [PrologVisible(Name = "cliLoadType", Arity = 1, TypeOf = null)]
        private static void LoadType(Type t)
        {
            lock (TypesLoaded)
            {
                if (TypesLoaded.Contains(t)) return;
                TypesLoading.Add(t);
                foreach (var m in t.GetMethods(BindingFlags.NonPublic | BindingFlags.Public | BindingFlags.Static))
                {
                    object[] f = m.GetCustomAttributes(typeof (PrologVisible), false);
                    if (f != null && f.Length > 0)
                    {
                        LoadMethod(m, (PrologVisible) f[0]);
                    }
                }
                TypesLoaded.Add(t);
            }
        }

        private static void LoadMethod(MethodInfo m, PrologVisible pm)
        {
            if (pm.Name == null)
            {
                if (char.IsLower(m.Name[0]))
                {
                    pm.Name = m.Name;
                }
            }
            InternMethod(pm.ModuleName, pm.Name, m);
        }

        private static void RegisterJPLForeigns()
        {
            // backup old jpl.pl and copy over it
            if (!JplDisabled)
                SafelyRun(() =>
                              {
                                  if (File.Exists(IKVMHome + "\\jpl_for_ikvm.phps"))
                                  {
                                      if (!File.Exists(SwiHomeDir + "\\library\\jpl.pl.old"))
                                      {
                                          File.Copy(SwiHomeDir + "\\library\\jpl.pl",
                                                    SwiHomeDir + "\\library\\jpl.pl.old",
                                                    true);
                                      }
                                      File.Copy(IKVMHome + "\\jpl_for_ikvm.phps", SwiHomeDir + "\\library\\jpl.pl", true);
                                  }
                              });

            PlEngine.RegisterForeign(null, "link_swiplcs", 1, new DelegateParameter1(link_swiplcs),
                                     PlForeignSwitches.None);
            //JplSafeNativeMethods.install();
            JplSafeNativeMethodsCalled = true;
            //DoQuery(new Query("ensure_loaded(library(jpl))."));
            /*
                             
             
jpl_jlist_demo :-
	jpl_new( 'javax.swing.JFrame', ['modules'], F),
	jpl_new( 'javax.swing.DefaultListModel', [], DLM),
	jpl_new( 'javax.swing.JList', [DLM], L),
	jpl_call( F, getContentPane, [], CP),
	jpl_call( CP, add, [L], _),
	(	current_module( M),
		jpl_call( DLM, addElement, [M], _),
		fail
	;	true
	),
	jpl_call( F, pack, [], _),
	jpl_call( F, getHeight, [], H),
	jpl_call( F, setSize, [150,H], _),
	jpl_call( F, setVisible, [@(true)], _).


% this directive runs the above demo

:- jpl_jlist_demo.

             */
            PlCall("use_module(library(jpl)).");
            PlAssert("jpl0 :- jpl_new( 'java.lang.String', ['hi'], DLM),writeln(DLM)");
            PlAssert("jpl1 :- jpl_new( 'javax.swing.DefaultListModel', [], DLM),writeln(DLM)");
        }

        public static bool PlCall(string s)
        {
            try
            {
                if (!JplDisabled)
                {
                    return DoQuery(s);
                }
                if (PlCsDisabled)
                {
                    WriteDebug("Disabled PlCall " + s);
                    return false;
                }
                return PlQuery.PlCall(s);
            }
            catch (Exception e)
            {
                WriteException(e);
                throw e;
            }
        }

        public static bool PlCall(string m, string f, PlTermV args)
        {
            try
            {
                if (!JplDisabled)
                {
                    return DoQuery(m, f, args);
                }
                if (PlCsDisabled)
                {
                    WriteDebug("Disabled PlCall " + f);
                    return false;
                }
                return PlQuery.PlCall(m, f, args);
            }
            catch (Exception e)
            {
                WriteException(e);
                throw e;
            }
        }

        private static bool link_swiplcs(PlTerm term)
        {
            try
            {
                return true;
                if (JplSafeNativeMethodsCalled)
                {
                    bool enabled = !JplSafeNativeMethodsDisabled;
                    SafelyRun(
                        () => Console.WriteLine("JplSafeNativeMethods called again from " + term + " result=" + enabled));
                    return enabled;
                }
                JplSafeNativeMethodsCalled = true;
                SafelyRun(() => Console.WriteLine("JplSafeNativeMethods call first time from " + term));
                JplSafeNativeMethods.install();
                //var v = new PlTerm("_1");
                //JplSafeNativeMethods.jpl_c_lib_version_1_plc(v.TermRef);
                return true;
            }
            catch (Exception e)
            {
                JplSafeNativeMethodsDisabled = true;
                WriteException(e);
                return false;
            }
        }

        private static bool DoQuery(string query)
        {
            if (JplDisabled) return PlCall(query);
            Query q;
            try
            {
                q = new Query(query);
            }
            catch (Exception e)
            {
                WriteException(e);
                return false;
            }
            return DoQuery(q);
        }

        public static bool DoQuery(string m, string f, PlTermV args)
        {
            if (JplDisabled) return PlCall(m, f, args);
            Query q;
            try
            {
                q = new Query(InModule(m, new Compound(f, ToJPL(args))));
            }
            catch (Exception e)
            {
                WriteException(e);
                return false;
            }
            return DoQuery(q);
        }

        private static bool DoQuery(Query query)
        {
            try
            {
                bool any = false;
                //if (!query.isOpen()) query.open();
                while (query.hasMoreSolutions())
                {
                    any = true;
                    Hashtable ht = query.nextSolution();
                    foreach (var list in ToEnumer(ht.elements()))
                    {
                        string s = "" + list;
                        Console.WriteLine(s);
                    }
                }
                return any;
            }
            catch (Exception exception)
            {
                WriteException(exception);
                return false;
            }

        }

        private static void WriteException(Exception exception)
        {
            java.lang.Exception ex = exception as java.lang.Exception;
            if (ex != null)
            {
                ex.printStackTrace();
            }
            else
            {
                Exception inner = exception.InnerException;
                if (inner != null && inner != exception)
                {
                    WriteException(inner);
                }
                Console.WriteLine("ST: " + exception.StackTrace);
            }

            Console.WriteLine("PrologClient: " + exception);
        }

        private static IEnumerable ToEnumer(java.util.Enumeration enumeration)
        {
            List<object> list = new List<object>();
            while (enumeration.hasMoreElements())
            {
                list.Add(enumeration.nextElement());
            }
            return list;
        }
        private static IEnumerable ToEnumer(java.util.Iterator enumeration)
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
            //JplSafeNativeMethods.install();
            //DoQuery("ensure_loaded(library(jpl)).");
            //DoQuery("module(user).");
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
            if (PlCsDisabled)
            {
                WriteDebug("Disabled PlAssert " + s);
                return;
            }
            PlQuery.PlCall("assert((" + s + "))");
        }

        private static void WriteDebug(string s)
        {
            Console.WriteLine(s);
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
        public static bool JplDisabled = false;
        public static bool PlCsDisabled = false;
        private static string _ikvmHome;
        public static string IKVMHome
        {
            get { return _ikvmHome; }
            set { _ikvmHome = RemoveTrailingPathSeps(value); }
        }

        private static string _swiHomeDir;// = Path.Combine(".", "swiprolog");
        public static string SwiHomeDir
        {
            get { return _swiHomeDir; }
            set
            {
                _swiHomeDir = RemoveTrailingPathSeps(value); ;
            }
        }

        private static string RemoveTrailingPathSeps(string value)
        {
            if (value != null)
            {
                value = value.TrimEnd("/\\".ToCharArray());
            }
            return value;
        }


        public static string AltSwiHomeDir = "C:\\development\\opensim4opencog";// Path.Combine(".", "swiprolog");
        public static bool JplSafeNativeMethodsDisabled = false;
        public static bool JplSafeNativeMethodsCalled = false;
        public static bool IsHalted = false;
        private static Int64 TopOHandle = 6660000;
        private static readonly Dictionary<string, object> SavedDelegates = new Dictionary<string, object>();
        public static bool FailOnMissingInsteadOfError = true;
        public static Thread CreatorThread;
        public static bool IsPLWin;
        public static bool RedirectStreams = true;
        public static int VMStringsAsAtoms = libpl.CVT_STRING;
        
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

        /* static public void WriteLine(string s, params object[] args)
         {
             Console.DebugWriteLine(s, args);
         }
         */

        public bool Consult(string filename)
        {
            // atomic quote the filename
            string replace = "'" + filename.Replace("\\", "\\\\").Replace("'", "\\'") + "'";
            return PlCall("[" + replace + "]");
        }

        public void InitFromUser()
        {
            ConsultIfExists("clipl.pl");
        }

        public void ConsultIfExists(string file)
        {
            if (File.Exists(file)) Consult(file);
        }

        public static object CallProlog(object target, string module, string name, int arity, object origin, object[] paramz, Type returnType)
        {
            Thread threadCurrentThread = Thread.CurrentThread;
            RegisterThread(threadCurrentThread);
            PlTermV args = NewPlTermV(arity);
            int fillAt = 0;
            if (origin != null)
            {
                args[fillAt++].FromObject((origin));
            }
            for (int i = 0; i < paramz.Length; i++)
            {
                args[fillAt++].FromObject((paramz[i]));
            }
            bool IsVoid = returnType == typeof(void);
            if (!IsVoid)
            {
                //args[fillAt] = PlTerm.PlVar();
            }
            if (!PlQuery.PlCall(module, name, args))
            {
                if (!IsVoid) PrologClient.Warn("Failed Event Handler " + target + " failed");
            }
            if (IsVoid) return null;
            object ret = PrologClient.CastTerm(args[fillAt], returnType);
            DeregisterThread(threadCurrentThread);
            return ret;
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

        //[DllImport(DllFileName)]
        //public static extern java.lang.Thread jni_env();

        //[DllImport(DllFileName)]
        //public static extern int jpl_c_lib_version_1_plc(uint term_t);
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