using System;
using System.Collections;
using System.Collections.Generic;
using System.IO;
using System.Reflection;
using System.Runtime.InteropServices;
using java.lang;
using MushDLR223.Utilities;
using Exception=System.Exception;
using Thread = System.Threading.Thread;
using DotLisp;
namespace MushDLR223.ScriptEngines
{
    public class ScriptManager
    {
        static private System.Diagnostics.TraceListener tl;
        static ScriptManager()
        {
          //  tl = new 
        }
        static public List<ScriptInterpreter> Interpreters = new List<ScriptInterpreter>();
        static public HashSet<Type> LoadedTypes = new HashSet<Type>();
        static public HashSet<Type> ScannedTypes = new HashSet<Type>();
        static public HashSet<Type> ScannedInterpTypes = new HashSet<Type>();
        static public HashSet<Type> LoadedInterpTypes = new HashSet<Type>();
        static public HashSet<Assembly> ScannedAssemblies = new HashSet<Assembly>();
        static public HashSet<Type> MissingTypes = new HashSet<Type>();
        static public HashSet<Type> VerifiedTypes = new HashSet<Type>();
        static public HashSet<object> VerifiedMembers = new HashSet<object>();
        static public object Lock = new object();
        static public object TypeLock = new object();
        public static Dictionary<Assembly, HashSet<Type>> AssemblyTypes = new Dictionary<Assembly, HashSet<Type>>();
        public static bool NewTypes = false;
        public static bool NewInterpTypes = false;
        public static bool FullSearch = false;

        static public bool AddType(Type type)
        {
            lock (TypeLock)
            {
                if (!ScanInfo(type)) return true;
                bool changed = false;
                if (LoadedTypes.Add(type))
                {
                    NewTypes = false;
                    changed = !ScannedTypes.Remove(type);
                    foreach (var set in Interpreters)
                    {
                        set.InternType(type);
                    }
                    try
                    {
                        lock (VerifyLock)
                        {
                            if (!ScanInfo(type))
                            {
                                WriteLine("-------------Verified BAD " + type);
                            }
                        }
                    }
                    catch (Exception e)
                    {
                        WriteLine("-------"+e+"------Verified BAD " + type);
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


        public static ScriptInterpreter LoadScriptInterpreter(string type, object self)
        {

            try
            {
                lock (Lock)
                {
                    ScriptInterpreter si = UsedCSharpScriptDefinedType(self, type);
                    if (si != null) return si; 
                    si = LoadScriptInterpreter0(type, self);
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
        public static void StartScanningAppDomain()
        {
            lock (ScanAppDomainLock)
            {
                if (ScanAppDomainInProgress) return;
                ScanAppDomainInProgress = true;
                ScanAppDomainThread = new Thread(DoScanningAppDomain);
                ScanAppDomainThread.Start();
            }

        }
        private static void DoScanningAppDomain()
        {
            int ScannedAssembliesCount = ScannedAssemblies.Count;
            if (ScanAppDomain())
            {
                WriteLine("Found new Assemblies: {0}", ScannedAssemblies.Count - ScannedAssembliesCount);
                foreach (var s in CopyOf(ScannedTypes))
                {
                    AddType(s);
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
        public static ScriptInterpreter LoadScriptInterpreter0(string type, object self)
        {
            var Interpreters = ScriptManager.Interpreters;
            lock (Interpreters)
            {
                if (Interpreters.Count == 0)
                {
                    var ret = UsedCSharpScriptDefinedType(self, type);
                    if (ret != null) return ret;
                    ScanPredfinedAssemblies();
                    InstanceNewInterpTypes(self);
                }
                foreach (var set in Interpreters)
                {
                    if (set.LoadsFileType(type, self))
                    {
#if COGBOT
                        if (set is BotScriptInterpreter)
                        {
                            if (self is BotClient)
                            {
                                ((BotScriptInterpreter)set).BotClient = self as BotClient;
                            }
                        }
#endif
                        return set;
                    }
                }
                return UsedCSharpScriptDefinedType(self, type);
                //default
                return new DotLispInterpreter(self);
            } // method: LoadScriptInterpreter
        }

        private static ScriptInterpreter UsedCSharpScriptDefinedType(object self, string type)
        {
            if (type != null)
            {
                type = type.ToLower();
                if (type.StartsWith("dotlisp"))
                {
                    return new DotLispInterpreter(self);
                }
                if (type.StartsWith("cyc"))
                {
                    return (ScriptInterpreter)new CycInterpreter(self);
                }
                if (type.StartsWith("abcl"))
                {
                    return (ScriptInterpreter)new ABCLInterpreter(self);
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
            bool changed = true;
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
                List<T> copy = new List<T>(types.Count);
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
            lock (Lock)
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
                    foreach (var set in CopyOf(LoadedTypes))
                    {
                        try
                        {
                            interpreter.InternType(set);
                        }
                        catch (Exception e)
                        {
                            WriteLine("ai: {0} {1} ", set, e);
                        }
                    }
                    foreach (var set in CopyOf(ScannedTypes))
                    {
                        try
                        {
                            interpreter.InternType(set);
                        }
                        catch (Exception e)
                        {
                            WriteLine("ai: {0} {1} ", set, e);
                        }
                    }
                    NewInterpTypes = false;
                }
                return changed;
            }
        }

        public static bool RemoveInterpreter(ScriptInterpreter interpreter)
        {
            lock (Lock)
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

        public static object EvalScriptInterpreter(string src, string lang, object seff, OutputDelegate wl)
        {
            var si = LoadScriptInterpreter(lang, seff);
            object so = si.Read("EvalScriptInterpreter read: " + src, new StringReader(src.ToString()), wl);
            if (so is CmdResult) return (CmdResult) so;
            if (so == null) return new CmdResult("void", true);
            if (si.Eof(so)) return new CmdResult("EOF " + so, true);
            object o = si.Eval(so);
            return o;
        }
        
        public static Type[] GetTypeArray(object[] argarray)
        {
             return CLSMember.GetTypeArray(argarray);
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
