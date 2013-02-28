using System;
using System.Collections;
using System.Collections.Generic;
using System.ComponentModel;
using System.Diagnostics;
using System.IO;
using System.Reflection;
using System.Runtime.CompilerServices;
using System.Text;
using System.Threading;
using System.Windows.Forms;
using System.Xml;
using MushDLR223.ScriptEngines;

namespace MushDLR223.Utilities
{
    public class StructToStringAttribute : Attribute
    {
        public bool IsDisabled;

        public StructToStringAttribute(bool enable)
        {
            IsDisabled = !enable;
        }
    }

    public class StructToStringDisableAttribute : StructToStringAttribute
    {
        public StructToStringDisableAttribute()
            : base(false)
        {
        }
    }

    public static class ToStringExtensionMethods
    {
        public static bool InStructToString
        {
            get
            {
                return tl_StructToStringDepth > 0 || tl_StructToStringDisable;
            }
        }
        [ThreadStatic] public static int tl_StructToStringDepth;
        [ThreadStatic] public static bool tl_StructToStringDisable;
        [ThreadStatic] public static List<object> tl_StructToString_LoopingOn;
        [StructToString(false)]
        public static int MaxStructToStringDepth = 3;

        static ToStringExtensionMethods()
        {

            int i = 1;
            Type c = i.GetTypeSafely();
            String s = "";
            s = null;
            Type ts = s.GetTypeSafely();
            Type ts2 = s.GetTypeSafely<String>();

        }

        private static HashSet<Type> DoCallStructToStringTypes = new HashSet<Type>();
        private static HashSet<Type> DontCallStructToStringTypes = new HashSet<Type>();
        public static string StructToString<T>(this T obj)
        {
            return StructToStringReal(obj, obj.GetTypeSafely().BetterType(typeof (T)));
        }

        public static string StructToStringReal(this object t, Type structType)
        {
            int before = tl_StructToStringDepth;
            try
            {
                return StructToStringSetup(t, structType);
            }
            finally
            {
                tl_StructToStringDepth = before;
            }
        }

        private static bool HasElements(ICollection props)
        {
            return props != null && props.Count > 0;
        }

        private const BindingFlags fpub = BindingFlags.Public | BindingFlags.Instance;
        private const BindingFlags fpriv = BindingFlags.NonPublic | BindingFlags.Instance;
        private const BindingFlags finst = BindingFlags.NonPublic | BindingFlags.Instance | BindingFlags.Public;

        public static unsafe string StructToStringSetup(object t, Type structType)
        {
            if (ReferenceEquals(t, null))
            {
                if (structType != null)
                {
                    Interesting();
                    return "<<" + ShortTypeName(structType) + "@NULL>>";
                }
                return "<<NULL>>";
            }
            bool nullOutS2S = false;
            if (tl_StructToString_LoopingOn == null)
            {
                tl_StructToString_LoopingOn = new List<object>();
                nullOutS2S = true;
            }
            try
            {
                bool addedI;
                var s = StructToStringC(t, structType, false, true, true);
                return s;
            }
            finally
            {
                if (nullOutS2S) tl_StructToString_LoopingOn = null;
            }
        }

        private static string StructToStringC(object t, Type structType, bool loopCheck, bool mayCallToString, bool mayDescendMembers)
        {
            if (ReferenceEquals(t, null)) return "NULL";
            if (DontCallStructToString(structType))
            {
                return CallToStringNoLoop(t);
            }
            bool vt = structType.IsValueType;

            int idx = -1;

            if (!vt)
            {
                idx = tl_StructToString_LoopingOn.LastIndexOf(t);
            }
            tl_StructToStringDepth++;
            if (tl_StructToStringDepth > MaxStructToStringDepth || tl_StructToStringDisable || idx != -1)
            {
                return TypeIdx(structType, t, idx, mayCallToString);
            }
            return StructToStringS(t, structType, idx, loopCheck, mayCallToString, mayDescendMembers);
        }

        public static string StructToStringE(IEnumerable t, Type structType, int idx, bool loopCheck, bool mayCallToString, bool mayDescendMembers)
        {
            if (ReferenceEquals(t, null)) return "NULL";
            if (t is IEnumerable)
            {
                StringBuilder result = new StringBuilder();
                IEnumerable ic = t as IEnumerable;
                int max = 100;
                int fnd = 0;
                bool printSomething = true;

                foreach (var i in ic)
                {
                    if (printSomething)
                    {
                        result.Append(fnd + ": " + StructToString1(t, i, mayCallToString, mayDescendMembers) + " ");
                    }
                    fnd++;
                    max--;
                    if (max < 1)
                    {
                        if (printSomething) result.Append("...");
                        printSomething = false;
                    }
                }
                if (fnd == 0)
                {
                    return "{COL=" + ShortTypeName(structType) + " Count=0]";
                }
                return "{COL=" + ShortTypeName(structType) + " Count=" + fnd + " [" + result.ToString().TrimEnd() + "]}";
            }
            return null;
        }

        public static string StructToStringS(object t, Type structType, int idx, bool loopCheck, bool mayCallToString, bool mayDescendMembers)
        {
            if (!DontCallStructToString(structType))
            {
                return CallToStringNoLoop(t);
            }
            MethodInfo toString = GetToStringIfDeclared(structType, 1);
            if (toString != null && mayCallToString && idx < 0)
            {
                return CallToStringNoLoop(t);
            }
            if (!mayDescendMembers)
            {
                return CallToStringNoLoop(t);
            }
            if (t is IEnumerable)
            {
                return StructToStringE((IEnumerable)t, structType, idx, loopCheck, mayCallToString, mayDescendMembers);
            }
            FieldInfo[] fields = structType.GetFields(fpub);
            PropertyInfo[] props = structType.GetProperties(fpub);
            bool hasProps = HasElements(props);
            if (!HasElements(fields) && !hasProps)
            {
                fields = structType.GetFields(fpriv);
            }
            if (!HasElements(props))
            {
                props = structType.GetProperties(fpriv);
            }
            bool needSimpleToString = true;

            StringBuilder result = new StringBuilder();
            HashSet<string> unneeded = new HashSet<string>();
            //if (HasElements(fields))
            {
                foreach (PropertyInfo prop in props)
                {
                    if (prop.GetIndexParameters().Length != 0) continue;
                    if (!prop.CanRead) continue;
                    if (prop.GetCustomAttributes(typeof(System.Runtime.CompilerServices.CompilerGeneratedAttribute), true).Length > 0) continue;
                    needSimpleToString = false;
                    string propname = prop.Name;
                    if (propname == "AToString") continue;
                    unneeded.Add(NormalPropName(propname));
                    result.Append("{" + propname + ": " + StructToString1(t, prop.GetValue(t, null), mayCallToString, mayDescendMembers) + "}");
                }
            }
            //if (needSimpleToString)
            {
                foreach (FieldInfo prop in fields)
                {
                    string propname = prop.Name;
                    if (prop.GetCustomAttributes(typeof(System.Runtime.CompilerServices.CompilerGeneratedAttribute), true).Length > 0) continue;
                    if (unneeded.Contains(NormalPropName(propname))) continue;
                    needSimpleToString = false;
                    result.Append("{" + propname + ": " + StructToString1(t, prop.GetValue(t), mayCallToString, mayDescendMembers) + "}");
                }
            }

            if (needSimpleToString)
            {
                return CallToStringNoLoop(t);
            }

            return result.ToString().TrimEnd();
        }

        private static string NormalPropName(string propname)
        {
            return propname.TrimStart('m', '_').TrimEnd('_', '0').ToUpper();
        }


        private static MethodInfo GetToStringIfDeclared(Type structType, int maxDepth)
        {
            MethodInfo toString = structType.GetMethod("ToString",
                                                       BindingFlags.DeclaredOnly | BindingFlags.Instance |
                                                       BindingFlags.Public, null, new Type[0], new ParameterModifier[0]);
            if (toString == null && maxDepth > 0)
            {
                var bt = structType.BaseType;
                if (bt != null && bt != typeof(object))
                {
                    return GetToStringIfDeclared(structType, maxDepth - 1);
                }
            }
            return toString;
        }

        private static string CallToStringNoLoop(object t)
        {
            if (t == null) return "<<NULL>>";
            if (t is String)
                return "\"" + t.ToString().Replace("\\", "\\\\").Replace("\"", "\\\"") + "\"";
            Type structType = t.GetType();
            if (structType == typeof(string)) return t.ToString();
            if (tl_StructToStringDisable)
            {
                // must be in it?
                return ObjHashCode(t, structType);
            }
            bool before = tl_StructToStringDisable;
            tl_StructToStringDisable = true;
            try
            {
                if (t is IComparable<string>)
                    return "\"" + t.ToString().Replace("\\", "\\\\").Replace("\"", "\\\"") + "\"";

                if (DontCallStructToString(structType))
                {
                    return "" + t;
                }
                try
                {
                    tl_StructToString_LoopingOn.Insert(0, t);
                    return "" + t;
                }
                finally
                {
                    tl_StructToString_LoopingOn.Remove(t);
                }
            }
            finally
            {
                tl_StructToStringDisable = before;
            }
        }

        private static string ObjHashCode(object o, Type structType)
        {
            return ShortTypeName(structType).Replace(".", "") + "@" + RuntimeHelpers.GetHashCode(o);
        }

        private static bool DontCallStructToString(Type structType)
        {
            lock (DontCallStructToStringTypes) if (DontCallStructToStringTypes.Contains(structType)) return true;
            lock (DoCallStructToStringTypes) if (DoCallStructToStringTypes.Contains(structType)) return false;
            bool dont = ComputeDontCallStructToString(structType);
            HashSet<Type> which = dont ? DontCallStructToStringTypes : DoCallStructToStringTypes;
            lock (which) which.Add(structType);
            return dont;
        }

        private static bool ComputeDontCallStructToString(Type t)
        {
            if (typeof(IConvertible).IsAssignableFrom(t) ||
                typeof(IComparable<string>).IsAssignableFrom(t) ||
                    typeof(IFormattable).IsAssignableFrom(t) ||
                        typeof(Uri).IsAssignableFrom(t) ||
                        // typeof(IDisposable).IsAssignableFrom(t) ||
            typeof(MarshalByRefObject).IsAssignableFrom(t)) return true;
            var attribs = t.GetCustomAttributes(true);
            if (attribs.Length > 0)
            {
                foreach (var attrib in attribs)
                {
                    if (attrib is TypeConverterAttribute)
                    {
                        return true;
                    }
                    if (attrib is StructToStringAttribute)
                    {
                        return ((StructToStringAttribute) attrib).IsDisabled;
                    }
                }
            }
            foreach (var fi in t.GetFields(BindingFlags.DeclaredOnly))
            {
                // someone marked the class
                if (fi.GetCustomAttributes(typeof (StructToStringAttribute), true).Length > 0) return false;
            }
            return false;
        }

        private static string StructToString1(object outer, object t, bool mayCallToString, bool mayDescendMembers)
        {
            try
            {
                tl_StructToString_LoopingOn.Insert(0, outer);
                return StructToStringC(t, t.GetTypeSafely(), true, mayCallToString, mayDescendMembers);
            }
            finally
            {
                tl_StructToString_LoopingOn.Remove(outer);
            }
        }

        static Type GetTypeSafely(this object o)
        {
            if (o == null) return null;
            return o.GetType();
        }

        static Type NullableWhat(object o)
        {
            if (o == null) return typeof (object);
            return o.GetType();
        }
        static Type GetTypeSafely<T>(this T o)
        {
            if (o == null) return typeof(T);
            return o.GetType().BetterType(typeof (T));
        }

        static Type BetterType(this Type t1, Type t2)
        {
            if (t1 == t2) return t1;
            if (t1 == null) return t2;
            if (t2 == null) return t1;
            int t1Rate = RateType(t1);
            int t2Rate = RateType(t2);
            if (t2Rate > t1Rate) return t2;
            return t1;
        }

        private static int RateType(Type t1)
        {
            if (t1 == null) return -1;
            if (t1 == typeof (object)) return 0;
            float val = 100;
            val += arrayLen(t1.GetFields(finst), -1)*3;
            val += arrayLen(t1.GetProperties(finst), -1)*2;
            if (t1.IsAbstract) val = val*0.8f;
            if (t1.IsInterface) val = val*0.7f;
            return (int) val;
        }

        private static int arrayLen<T>(this T[] getFields, int nullLen)
        {
            if (getFields == null) return -nullLen;
            return getFields.Length;
        }

        private static string TypeIdx(Type structType, object t, int idx, bool mayCallToString)
        {
            if (idx == -1)
            {
                if (mayCallToString)
                {
                    return CallToStringNoLoop(t);
                }
            }
            return ("<" + ObjHashCode(t, structType) + "=" + idx + ">");
        }

        public static string ShortTypeName(this Type[] ts)
        {
            if (ts == null || ts.Length == 0) return "";
            if (ts.Length == 1) return ts[0].ShortTypeName();
            var stn = new StringWriter();
            bool needcomma = false;
            foreach (var t in ts)
            {
                if (needcomma) stn.Write(",");
                stn.Write((string) t.ShortTypeName());
                needcomma = true;
            }
            return stn.ToString();
        }

        public static string ShortTypeName(this Type type)
        {
            if (type == null) return "TYPENULL";
            if (type.IsArray && type.HasElementType)
            {
                return ShortTypeName(type.GetElementType()) + "[]";
            }
            if (type.IsByRef)
            {
                return ShortTypeName(type) + "&";
            }
            if (type.IsByRef)
            {
                return ShortTypeName(type.GetElementType()) + "&";
            }
            if (type.IsPointer)
            {
                return ShortTypeName(type.GetElementType()) + "*";
            }
            if (type.IsGenericParameter)
            {
                Type[] gt = type.GetGenericParameterConstraints();
                return "<" + (type.FullName ?? type.Name) + ":" + gt.ShortTypeName() + ">";
            }
            string stn = type.Name;
            if (String.IsNullOrEmpty(stn)) return type.FullName;
            if (type.IsGenericType)
            {
                return stn.Split('`')[0] + "<" + type.GetGenericArguments().ShortTypeName() + ">";
            }
            return stn;
        }

        public static Type GetParameterType(ParameterInfo paramInfo)
        {
            Type paramType = paramInfo.ParameterType;
            return paramType.IsByRef ? paramType.GetElementType() : paramType;
        }

        public static bool IsByRef(ParameterInfo paramInfo)
        {
            Type paramType = paramInfo.ParameterType;
            return paramType.IsByRef;
        }

        public static bool IsOptionalParam(ParameterInfo info)
        {
            if ((info.Attributes & ParameterAttributes.Optional) != 0)
            {
                return true;
            }
            if ((info.Attributes & ParameterAttributes.HasDefault) != 0)
            {
                return true;
            }
            return info.IsOptional || (info.Name != null && info.Name.ToLower().StartsWith("optional"));
        }

        public static string ToStringSafe(this object obj)
        {
            if (obj == null) return "";
            try
            {
                return obj.ToString();
            }
            catch (Exception e)
            {
                return "<<<ex:" + e + ">>>";
            }
        }

        public static string ToStringSafeWithType<T>(this T obj)
        {
            Type toUse = typeof (T);
            bool maybeUnitited = Object.Equals((T) obj, default(T));
            if (maybeUnitited && !toUse.IsValueType) return "";
            var tobj = obj.GetType();
            if (toUse != tobj)
            {
                Interesting();
            }
            if (toUse == typeof (object) || toUse == null)
            {
                toUse = tobj;
            }
            try
            {
                string s1 = obj.ToString();
                string s2 = ShortTypeName(toUse);
                if (!s1.Contains(s2)) s1 = s2 + ":";
                if (maybeUnitited)
                {
                    Interesting();
                }
                return s1;
            }
            catch (Exception e)
            {
                Interesting();
                return "<<<ex:" + e + ">>>";
            }
        }

        private static void Interesting()
        {
           if (DLRConsole.Trace("Interesting"))
           {
               
           }
        }

        public static string ToCollectionString(this IEnumerable col, string sep)
        {
            if (col == null) return null;
            bool needsSep = false;
            string ret = "";
            foreach (object t in col)
            {
                if (needsSep)
                {
                    ret += sep;
                }
                else
                {
                    needsSep = true;
                }
                ret += t;
            }
            return ret;
        }

        public static string ToCollectionString<T>(this IEnumerable<T> col, string sep)
        {
            if (col == null) return null;
            bool needsSep = false;
            string ret = "";
            foreach (T t in col)
            {
                if (needsSep)
                {
                    ret += sep;
                }
                else
                {
                    needsSep = true;
                }
                ret += t.ToStringSafe();
            }
            return ret;
        }
    }
    public struct Boxed<T>
    {
        public FirstUse<T> io;
        public static implicit operator T(Boxed<T> value)
        {
            return value.io.Value;
        }
        public static implicit operator Boxed<T>(Func<T> value)
        {
            return (Boxed<T>)(FirstUse<T>)value;
        }

    }
    public static class InitOnceExtensionsTest
    {
        static void Main()
        {
            var m = new Func<int>(() => 1);
            var vg = m.ToFirstUse();
            var v = vg.Value;
            int i = vg;

            int? foo = 1;
            var f3 = foo + 2;
            FirstUse<int> bi = (Func<int>)(() => 1);
            int f4 = 1 + bi;
            int f5 = (int)new Nullable<int>(f4).ToFirstUseN();
            int f6 = (int)((int?)f4).ToFirstUseN();           
        }
    }
    public static class InitOnceExtensions
    {
        /// <summary>
        /// Makes an Initializer to be called only on first use
        /// </summary>
        /// <typeparam name="T"></typeparam>
        /// <param name="func"></param>
        /// <returns></returns>
        public static FirstUse<T> ToFirstUse<T>(this Func<T> func)
        {
            return func;
        }
        /// <summary>
        /// Makes an Initializer to be called only on first use
        /// </summary>
        /// <typeparam name="T"></typeparam>
        /// <param name="func"></param>
        /// <returns></returns>
        public static FirstUse<T> ToFirstUse<T>(this T? func) where T : struct
        {
            return (Func<T>)(() => func.Value);
        }
        public static T? ToFirstUseN<T>(this T? func) where T : struct
        {
            return new Nullable<T>(new FirstUse<T>(() => func.Value));
        }
        /// <summary>
        /// Makes an Initializer to be called only on first use
        /// </summary>
        /// <typeparam name="T"></typeparam>
        /// <param name="func"></param>
        /// <returns></returns>
        public static FirstUse<T> ToFirstUse<T>(this Boxed<T> func)
        {
            return func.io;
        }
        /// <summary>
        /// Makes an Initializer to be called only on first use
        /// </summary>
        /// <typeparam name="T"></typeparam>
        /// <param name="func"></param>
        /// <returns></returns>
        public static FirstUse<T> ToFirstUse<T>(this FirstUse<T> func)
        {
            return func;
        }
        /// <summary>
        /// Makes an Initializer to be called only on first use
        /// </summary>
        /// <typeparam name="T"></typeparam>
        /// <param name="func"></param>
        /// <returns></returns>
        public static FirstUse<T> ToFirstUse<T>(this object func)
        {
            return (Delegate)func;
        }
        /// <summary>
        /// Makes an Initializer to be called only on first use
        /// </summary>
        /// <typeparam name="T"></typeparam>
        /// <param name="func"></param>
        /// <returns></returns>
        public static FirstUse<T> ToFirstUse<T>(this Delegate func)
        {
            return (Func<T>)func;
        }
        /// <summary>
        /// Makes an Initializer to be called only on first use
        /// </summary>
        /// <typeparam name="T"></typeparam>
        /// <param name="func"></param>
        /// <returns></returns>
        public static FirstUse<T> ToFirstUse<T>(AnyFunc func)
        {
            return (Func<T>)(() => (T)func());
        }
        /// <summary>
        /// Makes an Initializer to be called only on first use
        /// </summary>
        /// <typeparam name="T"></typeparam>
        /// <param name="func"></param>
        /// <returns></returns>
        public static FirstUse<T> ToFirstUse<T>(AnyFunc<T> func)
        {
            return (Func<T>) (() => func());
           
        }
    }
    
    public delegate object AnyFunc(params object[] args);
    public delegate T AnyFunc<T>(params object[] args);

    public class FirstUse<T> // : Nullable<T>
    {
        public FirstUse()
        {
            
        }
        public static implicit operator T(FirstUse<T> value)
        {
            return value.Value;
        } 
        public static implicit operator Boxed<T>(FirstUse<T> value)
        {
            return new Boxed<T>() { io = value };
        }
        public static implicit operator Func<T>(FirstUse<T> value)
        {
            return () => value.Value;
        }
        public static implicit operator FirstUse<T>(T value)
        {
            return new FirstUse<T>()
            {
                m_value = value,
                m_isValid = true,
                m_valueFactory = (() => value)
            };

        }
        public static implicit operator FirstUse<T>(Func<T> value)
        {
            return new FirstUse<T>()
            {
                m_valueFactory = value
            };
        }
        public static implicit operator FirstUse<T>(Delegate value)
        {
            return new FirstUse<T>()
            {
                m_valueFactory = (Func<T>)value
            };
        }

        private T m_value;
        private bool m_isValid;
        // a delegate that returns the created value, if null the created value will be default(T)
        private Func<T> m_valueFactory;

        public FirstUse(Func<T> func)
        {
            m_valueFactory = func;
            m_value = default(T);
            m_isValid = false;
        }
        public bool HasValue
        {
            get
            {
                // once we have m_isValid true.. the value is valid!
                if (m_isValid) return true;
                // if m_isValid is false we might need to be waiting on a m_valueFactory invokation on another thread
                lock (m_valueFactory.GetType()) return m_isValid;
            }
        }
        public T Value
        {
            get
            {
                // once we have m_isValid true.. the value is valid!
                //if (m_isValid) return m_value;
                // if m_isValid is false we might need to be waiting on a m_valueFactory invokation on another thread
                lock (m_valueFactory.GetType())
                {
                    if (!m_isValid)
                    {
                        m_value = m_valueFactory();
                        m_isValid = true;
                    }
                    return m_value;
                }
            }
        }

        public static FirstUse<T> F(Func<T> func)
        {
            return new FirstUse<T>(func);
        }
    }

    public class LockInfo
    {

        public static object Watch(object o, params string[] named)
        {
            return o;
            //return Swicli.Library.LockInfo.Watch(o, named);
        }
    

        public static IList<T> CopyOf<T>(List<T> list)
        {
            if (list == null) return new List<T>();
            lock (list)
            {
                return list.ToArray();
            }
        }
        public static IEnumerable<object> CopyOf<T>(System.Collections.ICollection list)
        {
            var copy = new List<object>();
            if (list == null) return copy;
            lock (list)
            {
                foreach (var o in copy)
                {
                    copy.Add(o);
                }
            }
            return copy;
        }

        public static IList<T> CopyOf<T>(IEnumerable<T> list)
        {
            var copy = new List<T>();
            if (list == null) return copy;
            lock (list)
            {
                copy.AddRange(list);
            }
            return copy;
        }

        public static IDictionary<K, V> CopyOf<K, V>(IDictionary<K, V> list)
        {
            var copy = new Dictionary<K, V>();
            if (list == null) return copy;
            lock (list)
            {
                foreach (var kv in list)
                {
                    copy.Add(kv.Key, kv.Value);
                }
            }
            return copy;
        }

        public static bool DontRealyLock = true;
        public static R WeaklyLock<R>(object lockObject, TimeSpan maxWaitTryEnter, Func<R> action, Func<string> operationType, OutputDelegate output)
        {
            if (DontRealyLock)
            {
                return action();
            }
            Action needsExit = MonitorTryEnter(operationType(), lockObject, maxWaitTryEnter);
            try
            {
                return action();
            }
            finally
            {
                needsExit();
            }
        }
        public static void WeaklyLock(object lockObject, TimeSpan maxWaitTryEnter, Action action, Func<string> operationType, OutputDelegate output)
        {
            if (DontRealyLock)
            {
                action(); ;
                return;
            }
            Action needsExit = MonitorTryEnter(operationType(), lockObject, maxWaitTryEnter);
            try
            {
                action();
            }
            finally
            {
                needsExit();
            }
        }

        public static Action MonitorTryEnter(string lockType, object codeLock, TimeSpan maxWaitTryEnter)
        {
            //lock (LockInfos)
            {
                Thread currentThread = Thread.CurrentThread;
                bool needsExit = Monitor.TryEnter(codeLock, maxWaitTryEnter);

                if (!needsExit)
                {
                    lock (LockInfos)
                    {
                        LockInfo made = CantEnterUserThread(lockType, currentThread, codeLock);
                        return () => { };
                    }
                }
                else
                {
                    lock (LockInfos)
                    {
                        LockInfo made = LockInfo.EnterUserThread(lockType, currentThread, codeLock);
                        return () =>
                                   {
                                       lock (LockInfos)
                                       {
                                           try
                                           {
                                               LockInfo.ExitUserThread(lockType, made, codeLock);
                                           }
                                           finally
                                           {
                                               if (codeLock != null)
                                               {
                                                   Monitor.Exit(codeLock);
                                               }
                                           }
                                       }
                                   };
                    }
                }
            }
        }

        private static LockInfo CantEnterUserThread(string lockType, Thread currentThread, object codeLock)
        {
            LockInfo info = LockInfo.FindLockInfo(codeLock);
            string infostring = "Cannot get lock " + lockType;
            string newVariable = infostring + "in " + (info.StartTime - DateTime.Now) + " on " + info;
            writeDebugLine(newVariable);
            info.MoreInfoWST(infostring);
            return info;
        }

        public static LockInfo FindLockInfo(object codeLock)
        {
            LockInfo info = null;
            lock (LockInfos)
            {
                if (LockInfos.TryGetValue(codeLock, out info))
                {
                }
            }
            return info;
        }
        
        public static LockInfo EnterUserThread(string lockType, Thread currentThread, object codeLock)
        {
            lock (LockInfos)
            {
                LockInfo info = FindLockInfo(codeLock);
                if (info != null)
                {
                    if (info.FirstThread == Thread.CurrentThread)
                    {
                        info.MoreInfo("entering " + lockType);
                        info.needsUnlock++;
                        return info;
                    }
                    {
                        if (!DLRConsole.SkipStackTraces) info.MoreInfoWST("side-entering " + lockType);
                        /*
                        string here = LockInfo.GetStackTraceString();
                        string there = info.StartStack.ToString();

                        string newVariable = "FoundLock ??! " + lockType + " " + info;
                        writeDebugLine(newVariable);
                        writeDebugLine("here: " + here);
                        writeDebugLine("there: " + there);
                        writeDebugLine(newVariable);                                  
                        info.MoreInfo("Weird Entry " + lockType);
                         */
                        info.needsUnlock++;
                        return info;
                    }
                }
                info = CreateLockInfo(lockType, codeLock);
                info.needsUnlock++;
                return info;
            }
        }

        internal static void writeDebugLine(string s)
        {
            DLRConsole.DebugWriteLine(s);
        }

        public static LockInfo ExitUserThread(string lockType, LockInfo lockInfo, object codeLock)
        {
            lock (LockInfos)
            {
                LockInfo info = FindLockInfo(codeLock);

                if (info != null)
                {
                    if (info.IsLockerCurrentThread)
                    {
                        if (codeLock != null)
                        {
                            info.needsUnlock--;
                            //if (info.needsUnlock==0)
                            {
                                info.wasUnlocked++;
                            } 
                            
                            if (info.needsUnlock == 0)
                            {
                                info.MoreInfo("Exiting " + lockType);
                                LockInfos.Remove(info);
                            } else
                            {
                                info.MoreInfo("departing " + lockType);
                            }
                        }
                    }
                }
                else
                {
                    writeDebugLine("Cannot exit lock " + lockType + " " + lockInfo);
                }
                return info;
            }
        }
        private static readonly Dictionary<object, LockInfo> LockInfos = new Dictionary<object, LockInfo>();
        public static LockInfo CreateLockInfo(string lockType, object codeLock)
        {
            lock (LockInfos)
            {
                LockInfo lockinfo;
                if (!LockInfos.TryGetValue(codeLock, out lockinfo))
                {
                    return LockInfos[codeLock] = new LockInfo(lockType);
                }
                return lockinfo;
            }
        }
        public LockInfo(string named)
        {
            Name = named;
            StartTime = DateTime.Now;
            FirstThread = Thread.CurrentThread;
            if (DLRConsole.SkipStackTraces) return;
            StartStack = new StackTrace(true);
            MoreInfoWST("Created" + this);
        }
        public bool IsLockerCurrentThread
        {
            get
            {
                return Thread.CurrentThread == FirstThread;
            }
        }
        public override string ToString()
        {
            string s = "LockInfo " + Name + "\n" + (DateTime.Now - StartTime);
            //s += GetExtraInfo();
            return s;
        }

        public static bool TestLock(string named, object busyTrackingLock, TimeSpan timeSpan)
        {
            if (DontRealyLock) return true;
            // return;
            if (Monitor.TryEnter(busyTrackingLock, timeSpan))
            {
                Monitor.Exit(busyTrackingLock);
                return true;
            }
            DLRConsole.DebugWriteLine("ERROR: Cant get into " + named + " " + busyTrackingLock);
            return false;
        }

        public string GetExtraInfo()
        {
            StringBuilder sb = new StringBuilder();
            lock(Waiters)
            {
                foreach (string infostring in Waiters)
                {
                    sb.AppendLine(infostring);
                }
            }
            return sb.ToString();
        }

        public readonly Thread FirstThread;
        public readonly DateTime StartTime;
        public StackTrace StartStack;
        public int wasUnlocked = 0;
        public int needsUnlock = 0;
        public readonly string Name;
        public readonly List<string > Waiters = new List<string>();

        public void MoreInfoWST(string s)
        {
            string toString1 = GetStackTraceString();
            MoreInfo(s + "\n" + toString1);
        }

        public static string GetStackTraceString()
        {
            return (new StackTrace(true)).ToString();
        }

        public void MoreInfo(string p)
        {
            lock (Waiters)
                Waiters.Add(p);
        }

        public static void EnsureLocked(object lockObj, Action<string> bad)
        {
            string s = CheckLocked(lockObj);
            if (s == null) return;
            if (bad != null) bad(s);
        }
        static Dictionary<object, Thread> myPool = new Dictionary<object, Thread>();
        static public string CheckLocked(object lockObj)
        {
            if (DontRealyLock) return null;
            bool[] o = { false };
            Thread n = new Thread(() => isUnlocked(o, lockObj));
            n.Start();
            if (!n.Join(2000))
            {
                return "Could not join";
            }
            bool lockingIt = o[0];
            if (!lockingIt)
            {
                return "Everyone forgot to lock";
            }
            if (!Monitor.TryEnter(lockObj))
            {
                return "We forgot to lock";
            }
            else
            {
                Monitor.Exit(lockObj);
                return null;
            }
        }
        static private void isUnlocked(bool[] o, object lockObj)
        {
            if (Monitor.TryEnter(lockObj))
            {
                Monitor.Exit(lockObj);
                o[0] = false;
            }
            else
            {
                o[0] = true;
            }
        }

        public static T WithLock<T>(object lockObj, Func<T> func)
        {
            lock (lockObj) return func();
        }

        public static string CreationTrace()
        {
            var rs = new StringWriter();
            var fs = new System.Diagnostics.StackTrace(true).GetFrames();
            if (fs != null) foreach (StackFrame frame in fs)
                {
                    rs.WriteLine("" + frame);
                }
            return rs.ToString();
        }
    }

}