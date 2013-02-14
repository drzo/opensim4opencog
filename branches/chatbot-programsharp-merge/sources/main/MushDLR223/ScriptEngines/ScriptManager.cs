using System;
using System.Collections;
using System.Collections.Generic;
using System.IO;
using System.Reflection;
using System.Reflection.Emit;
using MushDLR223.Utilities;
using Exception=System.Exception;
#if (COGBOT_LIBOMV || USE_STHREADS)
using ThreadPoolUtil;
using Thread = ThreadPoolUtil.Thread;
using ThreadPool = ThreadPoolUtil.ThreadPool;
using Monitor = ThreadPoolUtil.Monitor;
#endif
using System.Threading;

using DotLisp;
namespace MushDLR223.ScriptEngines
{
    public partial class ScriptManager
    {
        public static Exception InnerMostException(Exception ex)
        {
            if (ex is ReflectionTypeLoadException)
            {
                var ile = ((ReflectionTypeLoadException)ex).LoaderExceptions;
                if (ile.Length == 1) return InnerMostException(ile[0]);
            }
            var ie = ex.InnerException;
            if (ie != null && ie != ex)
            {
                return InnerMostException(ie);
            }
            return ex;
        }

        static private System.Diagnostics.TraceListener tl;
        static ScriptManager()
        {
          //  tl = new 
        }
        static public List<ScriptInterpreter> Interpreters = new List<ScriptInterpreter>();
        static public Dictionary<ScriptInterpreter,int> InterpreterTypeCount = new Dictionary<ScriptInterpreter, int>();
        static public HashSet<Type> LoadedTypes = new HashSet<Type>();
        static public HashSet<Type> ScannedTypes = new HashSet<Type>();
        static public HashSet<Type> ScannedInterpTypes = new HashSet<Type>();
        static public HashSet<Type> LoadedInterpTypes = new HashSet<Type>();
        static public HashSet<Assembly> ScannedAssemblies = new HashSet<Assembly>();
        static public HashSet<Type> MissingTypes = new HashSet<Type>();
        static public HashSet<Type> VerifiedTypes = new HashSet<Type>();
        static public HashSet<object> VerifiedMembers = new HashSet<object>();
        static public object InterpLock = new object();
        static public object TypeLock = new object();
        public static Dictionary<Assembly, HashSet<Type>> AssemblyTypes = new Dictionary<Assembly, HashSet<Type>>();
        public static bool NewTypes = false;
        public static bool NewInterpTypes = false;
        public static bool FullSearch = false;

        static public bool AddType(Type type)
        {            
            {
                if (!ScanInfo(type)) return true;
                bool changed = false;
                lock (TypeLock) if (!LoadedTypes.Add(type)) return false;
                {
                    NewTypes = false;
                    changed = !ScannedTypes.Remove(type);
                    List<ScriptInterpreter> scriptInterpreters = new List<ScriptInterpreter>();

                    lock (InterpLock)
                    {
                        scriptInterpreters.AddRange(ScriptManager.Interpreters);
                    }
                    foreach (var set in scriptInterpreters)
                    {
                        set.InternType(type);
                    }
                    try
                    {
                        //lock (VerifyLock)
                        {
                            if (!ScanInfo(type))
                            {
                                WriteLine("-------------Verified BAD " + type);
                            }
                        }
                    }
                    catch (Exception e)
                    {
                        WriteLine("-------" + e + "------Verified BAD " + type);
                    }
                }
                return changed;
            }
        }

        private static bool ScanType(Type type)
        {
            if (ReferenceEquals(type, null)) return false;
            lock (TypeLock)
            {
                bool changed = false;
                if (typeof (ScriptInterpreter).IsAssignableFrom(type))
                {
                    if (!LoadedInterpTypes.Contains(type))
                    {
                        changed = ScannedInterpTypes.Add(type);
                        if (FullSearch) VerifyType(type);
                        NewInterpTypes = true;
                    }
                }
                if (!LoadedTypes.Contains(type))
                {
                    if (ScannedTypes.Add(type))
                    {
                        NewTypes = true;
                        if (FullSearch) VerifyType(type);
                    }
                    changed = true;
                }
                return changed;
            }
        }

        readonly static object VerifyLock = new object();
        public const BindingFlags ALL = BindingFlags.NonPublic | BindingFlags.Public | BindingFlags.Instance | BindingFlags.Static;
        public const BindingFlags ALL_Public = BindingFlags.Public | BindingFlags.Instance | BindingFlags.Static;
        public const BindingFlags ALL_NonPublic = BindingFlags.NonPublic | BindingFlags.Instance | BindingFlags.Static;
        private static bool VerifyType(Type type)
        {
            if (type.IsArray)
            {
                if (type.IsAbstract)
                {
                    WriteLine("Abstract array " + type);
                }
            }
            try
            {
                var a = type.Assembly;
                if (FullSearch) VerifyAssemblyLoadable(a);

                int ins = ScanInfo(type.GetInterfaces());
                int dm = ScanInfo(type.GetDefaultMembers());
                int nts = ScanInfo(type.GetNestedTypes());
                int nts0 = ScanInfo(type.GetNestedTypes(ALL));
                int pubm =
                    ScanInfo(type.GetMethods()) + ScanInfo(type.GetFields()) +
                    ScanInfo(type.GetProperties()) + ScanInfo(type.GetEvents()) +
                    ScanInfo(type.GetConstructors());
                int membs =
                    ScanInfo(type.GetMethods(ALL)) +ScanInfo(type.GetFields(ALL)) +
                    ScanInfo(type.GetProperties(ALL)) +ScanInfo(type.GetEvents(ALL)) +
                    ScanInfo(type.GetConstructors(ALL));
                int allnp =
                    ScanInfo(type.GetMethods(ALL_NonPublic)) +ScanInfo(type.GetFields(ALL_NonPublic)) +
                    ScanInfo(type.GetProperties(ALL_NonPublic)) +ScanInfo(type.GetEvents(ALL_NonPublic)) +
                    ScanInfo(type.GetConstructors(ALL_NonPublic));
                int pub0 = ScanInfo(type.GetMembers());
                int pub = ScanInfo(type.GetMembers(ALL_Public));
                MemberInfo[] allmembs = type.GetMembers(ALL);
                int all = ScanInfo(allmembs);
                int declonly = ScanInfo(type.GetMembers(ALL | BindingFlags.DeclaredOnly));
                int nonpub = ScanInfo(type.GetMembers(ALL_NonPublic));

                if ((((nonpub + pub) != all)) || ((all != (nts0 + membs))) || ((pub0 != pub)))
                {
                    WriteLine("{0} found {1} of {2}", type, pubm, membs, allnp);
                }


                if (type.IsGenericType)
                    try
                    {
                        var t = type.GetGenericTypeDefinition();
                        if (!ReferenceEquals(t, type))
                        {
                            ScanInfo(t);
                        }
                    }
                    catch (Exception e)
                    {
                        string s = "verify " + type + " was " + e;
                        WriteLine(s);
                    }

                ScanInfo(type.GetElementType());
                ScanInfo(type.BaseType);
                ScanInfo(type.UnderlyingSystemType);
                ScanInfo(type.BaseType);
                ScanInfo(type.DeclaringType);
                ScanInfo(type.ReflectedType);
                ScanInfo(type.TypeInitializer);
                ScanInfo(a);

                if (!VerifiedTypes.Add(type))
                {
                    WriteLine("Re---Verified " + type);
                    Debug();
                }
                return true;
            }
            catch (Exception e)
            {
                string s = "verify " + type + " was " + e;
                if (!(e is VerifyEx))
                {
                    Debug();
                    throw new VerifyEx(s, e);
                }
                WriteLine(s);
                if (!MissingTypes.Add(type))
                {

                    WriteLine("Re---Verified BAD " + type);
                    Debug();
                    return false;
                }
                return false;
            }

        }

        private static HashSet<Type> VerifyAssemblyLoadable(Assembly assembly)
        {
            if (ReferenceEquals(assembly, null)) return null;
            try
            {
                HashSet<Type> types;
                if (!AssemblyTypes.TryGetValue(assembly, out types))
                {
                    types = AssemblyTypes[assembly] = new HashSet<Type>(assembly.GetTypes());
                }
                return types;
            }
            catch (Exception e)
            {
                AssemblyTypes[assembly] = new HashSet<Type>();
                WriteLine("VerifyAssemblyLoadable: " + assembly);
                throw;
            }
        }

        private static void Debug()
        {
             WriteLine("!!debug!!");// throw new NotImplementedException();
        }

        private static void ScanInfo(Module[] modules)
        {
            foreach (var module in modules)
            {
                ScanInfo(module);
            }
        }

        private static void ScanInfo(Module type)
        {
            if (!FullSearch) return;
            if (ReferenceEquals(type, null)) return;
            if (!VerifiedMembers.Add(type)) return;
            ScanInfo(type.Assembly);
            ScanInfo(type.GetTypes());
            ScanInfo(type.GetMethods());
            ScanInfo(type.GetFields());
        }

        private static int ScanInfo(IEnumerable<MemberInfo> type)
        {
            if (type == null) return -1;
            int c = 0;
            foreach (var enumerable in type)
            {
                c++;
                ScanInfo(enumerable);
            }
            return c;
        }

        public static bool ScanInfo(MemberInfo set)
        {
            if (ReferenceEquals(set, null)) return true;
            if (!VerifiedMembers.Add(set)) return true;
            ScanInfo(set.DeclaringType);
            ScanInfo(set.ReflectedType);
            if (set is Type)
            {
                try
                {
                    ScanType((Type)set);
                }
                catch (Exception)
                {                    
                    throw;
                }
                return true;
            }
             
            if (set is MethodInfo)
                ScanMember0((MethodInfo) set);
            else if (set is FieldInfo)
                ScanMember0((FieldInfo) set);
            else if (set is EventInfo)
                ScanMember0((EventInfo) set);
            else if (set is PropertyInfo)
                ScanMember0((PropertyInfo) set);
            else if (set is ConstructorInfo)
                ScanMember0((ConstructorInfo)set);
            //else if (set is RuntimeType)
              //  ScanMember0((RuntimeType)set);
            else
            {
                WriteLine("unknown member type " + set.GetType());
            }
            return true;
        }


        private static void ScanMember0(FieldInfo set)
        {
            ScanInfo(set.FieldType);
            ScanInfo(set.GetOptionalCustomModifiers());
            ScanInfo(set.GetRequiredCustomModifiers());
        }
        private static void ScanMember0(EventInfo set)
        {
            ScanInfo(set.GetRaiseMethod());
            ScanInfo(set.GetRemoveMethod());
            ScanInfo(set.GetOtherMethods());
            ScanInfo(set.GetAddMethod());
            ScanInfo(set.EventHandlerType);
        }

        private static void ScanMember0(MethodInfo set)
        {
            ScanInfo(set.ReturnType);
            if (set.IsGenericMethod)
            {
                ScanInfo(set.GetGenericArguments());
            }
            ScanInfo(set.GetBaseDefinition());
            if (set.IsGenericMethod) ScanInfo(set.GetGenericMethodDefinition());
            foreach (var parameter in set.GetParameters())
            {
                ScanInfo(parameter);
            }
        }
        private static void ScanMember0(ConstructorInfo set)
        {
            ScanInfo(set.DeclaringType);
            if (set.IsGenericMethod) ScanInfo(set.GetGenericArguments());
            foreach (var parameter in set.GetParameters())
            {
                ScanInfo(parameter);
            }
        }

        private static void ScanInfo(ParameterInfo parameter)
        {
            if (object.ReferenceEquals(parameter, null)) return;
            ScanInfo(parameter.ParameterType);
        }

        private static void ScanMember0(PropertyInfo set)
        {
            ScanInfo(set.PropertyType);
            ScanInfo(set.GetAccessors());
            ScanInfo(set.GetGetMethod());
            ScanInfo(set.GetSetMethod());
            ScanInfo(set.GetRequiredCustomModifiers());
            ScanInfo(set.GetOptionalCustomModifiers());
            foreach (var parameter in set.GetIndexParameters())
            {
                ScanInfo(parameter.ParameterType);
            }
        }


        public static ScriptInterpreter LoadScriptInterpreter(string type, object self, ScriptInterpreter parent)
        {

            try
            {
                //lock (Lock)
                {
                    ScriptInterpreter si = UsedCSharpScriptDefinedType(self, type);
                    if (si != null)
                    {
                        si.Self = self;
                        return si;
                    } 
                    si = LoadScriptInterpreter0(type, self, parent);
                    StartScanningAppDomain();
                    return si;
                }
            }
            catch (Exception e)
            {
                WriteLine("LoadScriptInterpreter: " + e);
                throw e;
            }
        }

        static object ScanAppDomainLock = new object();
        private static bool ScanAppDomainInProgress = false;
        private static Thread ScanAppDomainThread;

        private static Dictionary<string, ScriptInterpreterFactory> TypeToInterpFactory =
            new Dictionary<string, ScriptInterpreterFactory>();

        public static void StartScanningAppDomain()
        {
            lock (ScanAppDomainLock)
            {
                if (ScanAppDomainInProgress) return;
                ScanAppDomainInProgress = true;
                ScanAppDomainThread = new Thread(DoScanningAppDomain);
                ScanAppDomainThread.Name = "DoScanningAppDomain";
                ScanAppDomainThread.Start();
            }

        }
        private static void DoScanningAppDomain()
        {
            int ScannedAssembliesCount = ScannedAssemblies.Count;
            if (ScanAppDomain())
            {
                int found = ScannedAssemblies.Count - ScannedAssembliesCount;
                if (found > 0)
                {
                    WriteLine("Found new Assemblies: {0}", found);
                    foreach (var s in CopyOf(ScannedTypes))
                    {
                        AddType(s);
                    }
                }
            }
            lock (ScanAppDomainLock)
            {
                ScanAppDomainInProgress = false;
            }
        }

        public static bool SafelyDo(string msg, Action self)
        {
            try
            {
                self();
                return true;
            }
            catch (Exception e)
            {
                WriteLine("LoadScriptInterpreter: " + msg + " - " + e);
                return false;
            }
        }

        /// <summary>
        /// 
        /// </summary>
        /// <param name="type"></param>
        /// <returns></returns>
        public static ScriptInterpreter LoadScriptInterpreter0(string type, object self, ScriptInterpreter parent)
        {
            var Interpreters = ScriptManager.Interpreters;
            lock (Interpreters)
            {
                var ret = UsedCSharpScriptDefinedType(self, type);
                if (ret != null) return ret;
                ScriptInterpreter typed = null;
                foreach (ScriptInterpreter set in Interpreters)
                {
                    if (set.LoadsFileType(type))
                    {
                        typed = set;
                        if (self != null) if (typed.IsSelf(self)) return set;
                    }
                }
                if (typed != null)
                {
                    return typed.newInterpreter(self);
                }
                ScanPredfinedAssemblies();
                InstanceNewInterpTypes(self);
                return FindOrCreate(type, self, parent);
                //default
                var dl = new DotLispInterpreter();
                dl.Self = self;
                return dl;
            } // method: LoadScriptInterpreter
        }

        /// <summary>
        /// </summary>
        /// <param name="lang"></param>
        /// <param name="source"></param>
        /// <param name="extendEnv"></param>
        /// <param name="scopeOrCurrentResolver"></param>
        /// <param name="reusableIdentityOrSelf"></param>
        /// <param name="parentCtx"></param>
        /// <returns></returns>
        public static object EvalFromReader(string lang, TextReader source, bool extendEnv,
            ICollectionProvider scopeOrCurrentResolver, object reusableIdentityOrSelf, ScriptInterpreter parentCtx)
        {
            if (parentCtx == null)
            {
                parentCtx = FindOrCreate(lang, reusableIdentityOrSelf, null);
            }
            var si = parentCtx.newInterpreter(reusableIdentityOrSelf);
            if (extendEnv) si = si.newInterpreter(scopeOrCurrentResolver);
            return si.Eval(source);
        }


        public static ScriptInterpreter FindOrCreate(string type, object self, ScriptInterpreter parent)
        {
            ScriptInterpreterFactory maker;
            if (parent == null)
            {
                maker = GetInterpreterFactory(type);
            }
            else
            {
                maker = parent.GetLoaderOfFiletype(type) ?? GetInterpreterFactory(type);
            }
            if (maker == null) return null;
            ScriptInterpreter si = maker.GetLoaderOfFiletype(type);
            if (si == null) return null;
            return si.newInterpreter(self);
        }

        public static ScriptInterpreterFactory GetInterpreterFactory(string type)
        {
            lock (TypeToInterpFactory)
            {
                ScriptInterpreterFactory function;
                if (TypeToInterpFactory.TryGetValue(type, out function)) return function;
                lock (Interpreters)
                {
                    foreach (ScriptInterpreterFactory interpreter in Interpreters)
                    {
                        var nonnull = interpreter.GetLoaderOfFiletype(type);
                        if (nonnull != null) return nonnull;
                    }
                    ScanPredfinedAssemblies();
                    StartScanningAppDomain();
                    return null;// return new ScriptManager();
                }
            }
        }

        private static ScriptInterpreter UsedCSharpScriptDefinedType(object self, string type)
        {
            if (type != null)
            {
                type = type.ToLower();
                if (type.StartsWith("dotlisp"))
                {
                    var dl = new DotLispInterpreter();
                    dl.Self = self;
                    return dl;
                }
            }
            return null;
        }

#if COGOBOT
        private static void LoadPredfinedInterpretors(object self)
        {
            if (false)
            {
                //if (self is ClientManager) self = ((ClientManager)self).LastBotClient ?? self;
                BotScriptInterpreter newBotScriptInterpreter = new BotScriptInterpreter(self);
                newBotScriptInterpreter = (BotScriptInterpreter)newBotScriptInterpreter.newInterpreter(self);
                AddInterpreter(newBotScriptInterpreter);
                DotLispInterpreter dotLispInterpreter = new DotLispInterpreter(self);
                dotLispInterpreter = (DotLispInterpreter)dotLispInterpreter.newInterpreter(self);
                AddInterpreter(dotLispInterpreter);
            }
        }
#endif

        static public bool ScanPredfinedAssemblies()
        {
            return ScanAppDomain() || (FullSearch && ScanPath(".", false));
        }

        private static bool ScanAppDomain()
        {
            bool changed = false;
            foreach (var a in AppDomain.CurrentDomain.GetAssemblies())
            {
                Assembly assembly = a;
                SafelyDo("scan assemly " + a, () =>
                                                  {
                                                      if (ScanInfo(assembly)) changed = true;
                                                  });
            }
            return changed;
        }

        private static bool ScanPath(string path, bool recurse)
        {
            bool changed = false;
            if (File.Exists(path))
            {
                changed = ScanFile(path);
            }
            else if (Directory.Exists(path))
            {
                foreach (var s in Directory.GetFiles(path))
                {
                    if (ScanFile(path)) changed = true;
                }
                if (recurse) foreach (var s in Directory.GetDirectories(path))
                    {
                        if (ScanPath(path, recurse)) changed = true;
                    }
            }
            return changed;
        }

        private static bool ScanFile(string path)
        {
            if (!File.Exists(path))
            {
                return false;
            }
            string lower = path.ToLower();
            if (path.EndsWith(".exe") || path.EndsWith(".dll"))
            {
                return ScanInfo(path);
            }
            return false;
        }

        private static bool ScanInfo(string path)
        {
            if (!File.Exists(path))
            {
                return false;
            }
            bool changed = false;
            SafelyDo("ScanInfo: " + path, () =>
                                                         {
                                                             var a = Assembly.LoadFile(path);
                                                             if (ScanInfo(a)) changed = true;
                                                         });
            return changed;
        }

        private static bool ScanInfo(Assembly a)
        {
            lock (TypeLock)
            {
                if (ScannedAssemblies.Add(a))
                {
                    WriteLine("ScanInfo " + a);
                    bool changed = false;
                    SafelyDo("ScanInfo: " + a, () =>
                                                   {
                                                       if (FullSearch) ScanInfo(a.GetModules());
                                                       if (FullSearch) ScanInfo(a.GetLoadedModules());
                                                       if (FullSearch) ScanInfo(a.GetExportedTypes());
                                                       if (FullSearch) ScanInfo(a.ManifestModule);
                                                       foreach (var list in VerifyAssemblyLoadable(a))
                                                           if (ScanInfo(list)) changed = true;
                                                   });
                    return changed;
                }
                return false;
            }
        }

        private static bool IsInterpType(Type type)
        {
            if (type == null || type.IsAbstract) return false;
            if (typeof(ScriptInterpreter).IsAssignableFrom(type)) return true;
            if (LoadedInterpTypes.Contains(type))
            {
                return true;
            }
            return false;
        }


        public static bool ScanAssemblyForScriptInterpretors(Assembly a, object self)
        {
            bool changed = ScanInfo(a);
            if (changed)
            {
                changed = InstanceNewInterpTypes(self);
            }
            return changed;
        }

        private static bool InstanceNewInterpTypes(object self)
        {
            Type bt = typeof (object);
            if (!ReferenceEquals(self, null))
            {
                bt = self.GetType();
            }
            bool changed = false;
            foreach (var list in CopyOf(ScannedInterpTypes))
            {
                if (IsInterpType(list))
                {
                    if (LoadedInterpTypes.Contains(list)) continue;
                    Type type = list;
                    SafelyDo("load interptype " + list, () =>
                                                            {
                                                                var mi = type.GetConstructor(new Type[] {bt});
                                                                if (mi == null)
                                                                    mi =
                                                                        type.GetConstructor(new Type[] {typeof (object)});
                                                                if (mi != null)
                                                                {
                                                                    ScriptInterpreter si =
                                                                        (ScriptInterpreter)
                                                                        mi.Invoke(new object[] {self});
                                                                    AddInterpreter(si);
                                                                    changed = true;
                                                                }
                                                                else
                                                                {
                                                                    mi = type.GetConstructor(new Type[0]);
                                                                    ScriptInterpreter si =
                                                                        (ScriptInterpreter) mi.Invoke(new object[0]);
                                                                    si.Self = self;
                                                                    AddInterpreter(si);
                                                                    changed = true;
                                                                }
                                                            });
                }
            }
            return changed;
        }

        private static IEnumerable<T> CopyOf<T>(ICollection<T> types)
        {
            if (types == null) return null;
            lock (types)
            {
                List<T> copy = new List<T>();
                copy.Capacity = types.Count;
                copy.AddRange(types);
                return copy;                
            }
        }
        private static IEnumerable<T> CopyOf<T>(List<T> types)
        {
            if (types == null) return null;
            lock (types)
            {
                return types.ToArray();
            }
        }


        public static bool AddInterpreter(ScriptInterpreter interpreter)
        {
            lock (InterpLock)
            {
                bool changed = false;
                if (!Interpreters.Contains(interpreter))
                {
                    NewInterpTypes = true;
                    changed = true;
                    Interpreters.Add(interpreter);
                    Type interpreterGetType = interpreter.GetType();
                    LoadedInterpTypes.Add(interpreterGetType);
                    ScannedInterpTypes.Remove(interpreterGetType);
                    var types = CopyOf(LoadedTypes);
                    foreach (var type in types)
                    {
                        try
                        {
                            interpreter.InternType(type);
                        }
                        catch (Exception e)
                        {
                            WriteLine("LoadedTypes InternType: {0} {1} {2} ", interpreter.GetType().Name, type, e);
                        }
                    }
                    var types2 = CopyOf(ScannedTypes);
                    foreach (var type in types2)
                    {
                        try
                        {
                            interpreter.InternType(type);
                        }
                        catch (Exception e)
                        {
                            WriteLine("ScannedTypes InternType: {0} {1} {2} ", interpreter.GetType().Name, type, e);
                        }
                    }
                    NewInterpTypes = false;
                }
                return changed;
            }
        }

        public static bool RemoveInterpreter(ScriptInterpreter interpreter)
        {
            lock (InterpLock)
            {
                if (!Interpreters.Contains(interpreter))
                {
                    Interpreters.Remove(interpreter);
                    return true;
                }
                return false;
            }
        }

        public static void WriteLine(string msg, params object[] args)
        {
            try
            {
                string f = DLRConsole.SafeFormat(msg, args);
                DLRConsole.DebugWriteLine("{0}", f);
            }
            catch (Exception e)
            {
                DLRConsole.DebugWriteLine("WriteLine: {0} {1}", e, msg);
            }
        }

        public static List<object> ResolveToObject(object any, string membername)
        {
            List<object> objs = null;           
            if (any == null)
            {
                foreach (var s in CopyOf(Interpreters))
                {
                    if (s.IsSubscriberOf(membername))
                    {
                        objs = objs ?? new List<object>();
                        objs.Add(s.GetSymbol(membername));
                    }                    
                }
            }
            return objs;
        }

        public static object EvalScriptInterpreter(string src, string lang, object seff, ScriptInterpreter parent, OutputDelegate wl)
        {
            var si = LoadScriptInterpreter(lang, seff, parent);
            object so = si.Read("EvalScriptInterpreter read: " + src, new StringReader(src.ToString()), wl);
            if (so is CmdResult) return (CmdResult) so;
            if (so == null) return ACmdResult.Complete(lang + " " + src, "void", true);
            if (si.Eof(so)) return ACmdResult.Complete(lang + " " + src, "EOF " + so, true);
            object o = si.Eval(so);
            return o;
        }
        
        public static Type[] GetTypeArray(object[] argarray)
        {
             return CLSMember.GetTypeArray(argarray);
        }


    }

    public static class OKAssemblyResolve
    {
        public static bool ResolverEnabled { get; set; }

        static OKAssemblyResolve()
        {
            //The AssemblyResolve event is called when the common language runtime tries to bind to the assembly and fails.
            AppDomain currentDomain = AppDomain.CurrentDomain;
            currentDomain.AssemblyResolve += new ResolveEventHandler(CurrentDomain_AssemblyResolve);
        }

        public static bool GetResolverOnOff()
        {
            return ResolverEnabled;
        }
        public static void SetResolverOnOff(bool onOff)
        {
            ResolverEnabled = onOff;
        }
        static Dictionary<string,Assembly> ResolvedAssemblies = new Dictionary<string, Assembly>(); 
        private static Assembly CurrentDomain_AssemblyResolve(object sender, ResolveEventArgs args)
        {
            if (!ResolverEnabled)
            {
                return null;
            }
            try
            {
                string s = args.Name;
                lock (ResolvedAssemblies)
                {
                    Assembly fnd;
                    if (ResolvedAssemblies.TryGetValue(s, out fnd))
                    {
                        if (fnd == null)
                        {
                            return null;
                        }
                        return fnd;
                    }
                    ResolvedAssemblies[s] = null;
                    Assembly try1 = CurrentDomain_AssemblyResolveType1(sender as AppDomain, s);
                    if (try1 != null)
                    {
                        ResolvedAssemblies[s] = try1;
                        return try1;
                    }
                }
            }
            catch (Exception e)
            {
                DLRConsole.DebugWriteLine("AssemblyResolver: " + sender + " lookingfor=<" + args.Name + "> cause: " + e);
            }
            return null;
        }


        private static bool AssemblyMatches(Assembly assembly, string assemblyName)
        {
            if (assembly.FullName == assemblyName)
                return true;
            if (assembly.ManifestModule.Name == assemblyName)
                return true;
            if (assembly.CodeBase == assemblyName)
                return true;
            return false;
        }
        private static bool AssemblyMatches(AssemblyName assembly, string assemblyName)
        {
            if (assembly.FullName == assemblyName)
                return true;
            if (assembly.Name == assemblyName)
                return true;
            if (assembly.CodeBase == assemblyName)
                return true;
            return false;
        }


        private static Assembly CurrentDomain_AssemblyResolveType1(AppDomain domain, string assemblyName)
        {
            if (domain != null)
            {
                foreach (var assembly in LockInfo.CopyOf(domain.GetAssemblies()))
                {
                    if (AssemblyMatches(assembly, assemblyName)) return assembly;
                }
            }
            foreach (Assembly assembly in LockInfo.CopyOf(AssembliesLoaded))
            {
                if (AssemblyMatches(assembly, assemblyName)) return assembly;
            }

            var objExecutingAssemblies = Assembly.GetExecutingAssembly();
            if (objExecutingAssemblies != null)
            {
                AssemblyName[] arrReferencedAssmbNames = objExecutingAssemblies.GetReferencedAssemblies();
                foreach (var assemblyN in arrReferencedAssmbNames)
                {
                    if (AssemblyMatches(assemblyN, assemblyName))
                    {
                         Assembly load =  Assembly.Load(assemblyN);
                    }
                }
            }

            int comma = assemblyName.IndexOf(",");
            if (comma > 0)
            {
                assemblyName = assemblyName.Substring(0, comma);
            }
            return LoadAssemblyByFile(assemblyName);
        }

        private static Assembly LoadAssemblyByFile(string assemblyName)
        {
            if (File.Exists(assemblyName))
            {
                try
                {
                    var fi = new FileInfo(assemblyName);
                    if (fi.Exists) return Assembly.LoadFile(fi.FullName);
                }
                catch (Exception)
                {
                    throw;
                }
            }
            IList<string> sp = LockInfo.CopyOf((IEnumerable<string>)AssemblySearchPaths);
            foreach (
                var dir in
                    new[]
                        {
                            AppDomain.CurrentDomain.BaseDirectory, new DirectoryInfo(".").FullName,
                            Path.GetDirectoryName(typeof (ScriptInterpreter).Assembly.CodeBase), Environment.CurrentDirectory
                        })
            {
                if (!sp.Contains(dir)) sp.Add(dir);
            }
            string lastTested = "";
            foreach (var s in LockInfo.CopyOf(AppDomain.CurrentDomain.GetAssemblies()))
            {
                try
                {
                    if (s is AssemblyBuilder) continue;
                    string dir = Path.GetDirectoryName(s.CodeBase);
                    if (dir.StartsWith("file:\\"))
                    {
                        dir = dir.Substring(6);
                    }
                    if (dir == lastTested) continue;
                    lastTested = dir;
                    if (!sp.Contains(dir)) sp.Add(dir);
                }
                catch (NotSupportedException)
                {
                    // Reflected Assemblies do this
                }
            }
            foreach (string pathname in sp)
            {
                var assemj = FindAssemblyByPath(assemblyName, pathname);
                if (assemj != null) return assemj;
            }

            return null;
        }

        private static string NormalizePath(string dirname1)
        {
            string dirname = dirname1;
            if (dirname.StartsWith("file:\\"))
            {
                dirname = dirname.Substring(6);
            }
            if (dirname.StartsWith("file://"))
            {
                dirname = dirname.Substring(7);
            }
            dirname = new FileInfo(dirname).FullName;
            if (dirname != dirname1)
            {
                return dirname;
            }
            return dirname1;
        }


        private static readonly List<string>
            LoaderExtensionStrings = new List<string>
                                         {
                                             "dll",
                                             "exe",
                                             "jar",
                                             "lib",
                                             "dynlib",
                                             "class",
                                             "so"
                                         };

        public static Assembly FindAssemblyByPath(string assemblyName, string dirname)
        {

            dirname = NormalizePath(dirname);
            string filename = Path.Combine(dirname, assemblyName);
            string loadfilename = filename;
            bool tryexts = !File.Exists(loadfilename);
            string filenameLower = filename.ToLower();
            List<string> LoaderExtensions = new List<string>();
            lock (LoaderExtensionStrings)
            {
                LoaderExtensions.AddRange(LoaderExtensionStrings);
            }
            foreach (string extension in LoaderExtensions)
            {
                if (filenameLower.EndsWith("." + extension))
                {
                    tryexts = false;
                    break;
                }
            }

            if (tryexts)
            {
                foreach (var s in LoaderExtensions)
                {
                    string testfile = loadfilename + "." + s;
                    if (File.Exists(testfile))
                    {
                        loadfilename = testfile;
                        break;
                    }

                }
            }
            if (File.Exists(loadfilename))
            {
                try
                {
                    return Assembly.LoadFile(new FileInfo(loadfilename).FullName);
                }
                catch (Exception)
                {
                    throw;
                }
            }
            return null;
        }

        public static Assembly AssemblyLoad(string assemblyName)
        {
            Assembly assembly = null;
            try
            {
                assembly = Assembly.Load(assemblyName);
            }
            catch (FileNotFoundException fnf)
            {
                if (fnf.FileName != assemblyName) throw fnf;
            }
            catch (Exception)
            {
                throw;
            }
            if (assembly != null) return assembly;
            assembly = LoadAssemblyByFile(assemblyName);
            if (assembly != null) return assembly;
            return Assembly.LoadFrom(assemblyName);

        }

        public static List<string> AssemblySearchPaths = new List<string>();
        public static List<Assembly> AssembliesLoaded = new List<Assembly>();
        public static List<string> AssembliesLoading = new List<string>();
        public static List<Type> TypesLoaded = new List<Type>();
        public static List<Type> TypesLoading = new List<Type>();

        public static bool ResolveAssembly(Assembly assembly)
        {
            string assemblyName = assembly.FullName;
            lock (AssembliesLoaded)
            {
                if (AssembliesLoading.Contains(assemblyName))
                {
                    return true;
                }
                AssembliesLoading.Add(assemblyName);
                LoadReferencedAssemblies(assembly);
                // push to the front
                AssembliesLoaded.Remove(assembly);
                AssembliesLoaded.Insert(0, assembly);
            }
            return true;
        }

        private static void LoadReferencedAssemblies(Assembly assembly)
        {
            foreach (var refed in assembly.GetReferencedAssemblies())
            {
                string assemblyName = refed.FullName;
                try
                {
                    Assembly assemblyLoad = Assembly.Load(refed);
                    ResolveAssembly(assemblyLoad);
                    continue;
                }
                catch (Exception e)
                {
                    var top = Path.Combine(Path.GetDirectoryName(assembly.Location), refed.Name);

                    foreach (var ext in LoaderExtensionStrings)
                    {
                        string filename = top + "." + ext;
                        try
                        {
                            if (!File.Exists(filename)) continue;
                            Assembly assemblyLoad = Assembly.LoadFrom(filename);
                            ResolveAssembly(assemblyLoad);
                            continue;
                        }
                        catch (Exception)
                        {
                        }
                    }
                    DLRConsole.DebugWriteLine("LoadReferencedAssemblies:{0} caused {1}", assemblyName, e);
                }
            }
        }


        static Assembly currentDomain_AssemblyResolveType2(object sender, ResolveEventArgs args)
        {
            //This handler is called only when the common language runtime tries to bind to the assembly and fails.

            //Retrieve the list of referenced assemblies in an array of AssemblyName.
            Assembly MyAssembly, objExecutingAssemblies;
            string strTempAssmbPath = "";

            objExecutingAssemblies = Assembly.GetExecutingAssembly();
            AssemblyName[] arrReferencedAssmbNames = objExecutingAssemblies.GetReferencedAssemblies();

            //Loop through the array of referenced assembly names.
            foreach (AssemblyName strAssmbName in arrReferencedAssmbNames)
            {
                //Check for the assembly names that have raised the "AssemblyResolve" event.
                if (strAssmbName.FullName.Substring(0, strAssmbName.FullName.IndexOf(",")) ==
                    args.Name.Substring(0, args.Name.IndexOf(",")))
                {
                    //Build the path of the assembly from where it has to be loaded.
                    //The following line is probably the only line of code in this method you may need to modify:
                    strTempAssmbPath = ".";// txtAssemblyDir.Text;
                    if (strTempAssmbPath.EndsWith("\\")) strTempAssmbPath += "\\";
                    strTempAssmbPath += args.Name.Substring(0, args.Name.IndexOf(",")) + ".dll";
                    break;
                }

            }
            //Load the assembly from the specified path.
            MyAssembly = Assembly.LoadFrom(strTempAssmbPath);

            //Return the loaded assembly.
            return MyAssembly;
        }
    }

    internal class VerifyEx : Exception
    {
        public VerifyEx(string s)
            : base(s)
        {
        }
        public VerifyEx(string s, Exception e)
            : base(s ,e)
        {
        }
    }
}
