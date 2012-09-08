using System;
using System.Collections;
using System.Collections.Generic;
using System.Reflection;
using MushDLR223.ScriptEngines;

namespace MushDLR223.Utilities
{
    public class ConfigSettingAttribute : Attribute, IKeyValuePair<string,object>
    {
        private bool HasSingleton
        {
            get
            {
                EnsureSingleton();
                return _singleton != null;
            }
        }

        private void EnsureSingleton()
        {
            if (!UseSingleton) return;
            if (_singleton == null)
            {
                var fnd = Member.DeclaringType.GetMember("SingleInstance");
                if (fnd != null && fnd.Length > 0)
                {
                    var fnd0 = fnd[0];
                    _singleton = FindValue(fnd0, null);
                }
            }
            if (_singleton == null)
            {
                if (sysvarCtx != null)
                {
                    _singleton = FindValueOfType(sysvarCtx, Member.DeclaringType, 2);
                }
            }
            if (_singleton == null)
            {
                return;
            }
        }

        public static object FindValueOfType(object ctx, Type type, int depth)
        {
            if (type.IsInstanceOfType(ctx)) return ctx;
            if (depth < 0) return null;
            object obj;
            bool mustBeTagged = ctx is ExactMemberTree;
            if (ctx is HasInstancesOfType)
            {
                HasInstancesOfType hit = (HasInstancesOfType) ctx;
                if (hit.TryGetInstance(type, depth - 1, out obj))
                {
                    if (!type.IsInstanceOfType(obj))
                    {
                        // trace this
                        return null;
                    }
                    return obj;
                }
                if (mustBeTagged)
                {
                    // trace this
                    return null;
                }
                return null;
            }
            foreach (var s in ctx.GetType().GetProperties(BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic))
            {
                if (mustBeTagged && !HasAttribute(s, typeof(MemberTree))) continue;
                if (HasAttribute(s, typeof(SkipMemberTree))) continue;
                if (type.IsAssignableFrom(s.PropertyType))
                {
                    obj = s.GetValue(ctx, null);
                    if (type.IsInstanceOfType(obj)) return obj;
                }
                else
                {
                    if (depth > 0)
                    {
                        obj = s.GetValue(ctx, null);
                        if (type.IsInstanceOfType(obj))
                        {
                            // trace this
                            return obj;
                        }
                        obj = FindValueOfType(obj, type, depth - 1);
                        if (type.IsInstanceOfType(obj)) return obj;
                    }
                }
            }
            foreach (var s in ctx.GetType().GetFields(BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic))
            {
                if (mustBeTagged && !HasAttribute(s, typeof(MemberTree))) continue;
                if (HasAttribute(s, typeof(SkipMemberTree))) continue;
                obj = s.GetValue(ctx);
                if (type.IsInstanceOfType(obj)) return obj;
                if (depth > 0)
                {
                    obj =  FindValueOfType(obj, type, depth - 1);
                    if (type.IsInstanceOfType(obj)) return obj;
                }
            }
            if (ctx is IEnumerable)
            {
                IEnumerable hit = (IEnumerable)ctx;
                foreach (var e in hit)
                {
                    obj = e;
                    if (type.IsInstanceOfType(obj)) return obj;
                    if (depth > 0)
                    {
                        obj = FindValueOfType(obj, type, depth - 1);
                        if (type.IsInstanceOfType(obj)) return obj;
                    }
                }
            }
            return null;
        }

        public static bool AtLeastOne(object[] attributes)
        {
            return attributes != null && attributes.Length > 0;
        }
        public static bool HasAttribute(MemberInfo info, Type type)
        {
            return info.IsDefined(type, true);
        }
        public static bool IsSingletonClass(Type type)
        {
            lock (SingletonClasses) if (SingletonClasses.Contains(type)) return true;
            if (typeof(NotContextualSingleton).IsAssignableFrom(type) && !typeof(ContextualSingleton).IsAssignableFrom(type)) return false;
            bool singleton = typeof(ContextualSingleton).IsAssignableFrom(type);
            if (!singleton)
            {
                singleton = AtLeastOne(type.GetMember("SingleInstance"));
            }
            if (!singleton)
            {
                foreach (var set in type.GetMembers(BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Static))
                {
                    if (set is MethodBase) continue;
                    if (set is EventInfo) continue;
                    if (set is Type) continue;
                    if (MemberValueType(set) == set.DeclaringType)
                    {
                        //singleton = true;
                        break;
                    }    
                }
            }
            if (singleton)
            {
                lock (SingletonClasses) SingletonClasses.Add(type);
                return true;
            }
            return false;
        }

        public static HashSet<Type> SingletonClasses = new HashSet<Type>();
        public static void AddSingletonClass(Type type)
        {
            lock (SingletonClasses)
            {
                // already seen
                if (!SingletonClasses.Add(type)) return;
            }
            ScriptManager.LoadSysVars(type);
        }

        private MemberInfo member;
        private object initialValue;
        private object saveValue;
        [NonSerialized]
        [ThreadStatic]
        private object _singleton;
        [NonSerialized]
        [ThreadStatic]
        public static object sysvarCtx;
        public bool UseSingleton;
        public MemberInfo Member
        {
            get
            {
                return member;
            }
        }
            
        public object Singleton
        {
            get
            {
                EnsureSingleton();
                return _singleton;
            }
            set
            {
                _singleton = value;
            }
        }
        public bool IsReadOnly { get; set; }
        public bool SkipSaveOnExit
        {
            set { VSkipSaveOnExit = value; }
            get
            {
                if (!VSkipSaveOnExit.HasValue)
                {
                    SkipSaveOnExit = SkipOnExit(Member);
                }
                return VSkipSaveOnExit.Value;
            }
        }

        static bool SkipOnExit(MemberInfo s)
        {
            ConfigSettingAttribute cs0 = FindConfigSetting(s, false);
            if (cs0.VSkipSaveOnExit.HasValue)
            {
                return cs0.VSkipSaveOnExit.Value;
            }
            ConfigSettingAttribute cs1 = FindConfigSetting(s.DeclaringType, false);
            if (cs1 != null && cs1.VSkipSaveOnExit.HasValue)
            {
                return cs1.VSkipSaveOnExit.Value;
            }
            return false;
        }

        public static Dictionary<MemberInfo, ConfigSettingAttribute> M2C = new Dictionary<MemberInfo, ConfigSettingAttribute>();

        public static ConfigSettingAttribute FindConfigSetting(MemberInfo s, bool forceCreate)
        {
            const bool createIfDeclared = true;
            ConfigSettingAttribute cs0;
            lock (M2C)
            {
                if (!M2C.TryGetValue(s, out cs0))
                {
                    if (createIfDeclared)
                    {
                        if (s.IsDefined(typeof (ConfigSettingAttribute), true))
                        {
                            var cs = s.GetCustomAttributes(typeof (ConfigSettingAttribute), true);
                            if (cs != null && cs.Length > 0)
                            {
                                cs0 = (ConfigSettingAttribute) cs[0];
                                cs0.SetMember(s);
                            }
                            else
                            {
                                cs0 = null;
                            }
                        }
                        M2C[s] = cs0;
                    }
                }
                if (cs0 == null && forceCreate)
                {
                    cs0 = new ConfigSettingAttribute();
                    cs0.SetMember(s);
                    M2C[s] = cs0;
                }
            }
            return cs0;
        }

        public Nullable<bool> VSkipSaveOnExit { get; set; }

        public void Save()
        {
            if (SkipSaveOnExit) return;
            if (saveValue == null) return;
            Value = saveValue;
        }

        private string _name;
        public string Key
        {
            get
            {
                if (_name == null)
                {
                    if (!IsStatic)
                    {
                        return "" + Member.DeclaringType.Name + ".this." + Member.Name;
                    }
                    return "" + Member.DeclaringType.Name + ":" + Member.Name;
                }
                return _name;
            }
            set { _name = value; }
        }

        private string _description;
        public bool IsStatic;
        public bool IsNonValue;
        public Type ReturnType;

        public string Description
        {
            get
            {
                if (_description == null)
                {
                    return "Member setter " + Member + " for " + Member.DeclaringType.FullName;
                }
                return _description;
            }
            set { _description = value; }
        }
        public override bool Equals(object obj)
        {
            var other = obj as ConfigSettingAttribute;
            return other != null && other.Member == Member;
        }
        public override int GetHashCode()
        {
            if (Member == null) return -1;
            return Member.GetHashCode();
        }

        /// <summary>
        /// Performs application-defined tasks associated with freeing, releasing, or resetting unmanaged resources.
        /// </summary>
        /// <filterpriority>2</filterpriority>
        public void Dispose()
        {
            member = null;
            _singleton = null;
            initialValue = null;
        }

        public void SetMember(MemberInfo member0)
        {
            lock(M2C)
            {
                M2C[member0] = this;
            }
            if (member0 is Type)
            {
                IsNonValue = true;
                return;
            }
            member = member0;
            if (IsReadOnlyMember(member0))
            {
                // cant restore?
                // SkipSaveOnExit = true;
                IsReadOnly = true;
            }
            IsStatic = TestIsStatic(member0);
            UseSingleton = !IsStatic;
            ReturnType = MemberValueType(member0);
            if (ReturnType.GetGenericTypeDefinition() == (typeof(ListAsSet<>)))
            {
                return;
            }
        }

        public string DebugInfo
        {
            get
            {
                string comments = Comments;

                if (comments.Length != 0)
                {
                    return Key + " = " + Value + " //" + comments;
                }
                return Key + " = " + Value;
            }
        }

        public string Comments
        {
            get
            {
                string comments = "";
                if (SkipSaveOnExit) comments += " SkipSavedOnExit";
                if (IsReadOnly) comments += " ReadOnly";
                if (!IsStatic) comments += " Virtual";
                if (!string.IsNullOrEmpty(_description)) comments += " " + _description;
                return comments;
            }
        }

        public override string ToString()
        {
            return DebugInfo;
        }
        public object Value
        {
            get
            {
                var m = this.Member;
                object target = null;
                if (UseSingleton && !IsStatic)
                {
                    target = Singleton;
                    if (target == null)
                        return "(unknown singleton)";
                }
                try
                {
                    var v = FindValue(m, target);
                    if (v != null) return v;
                }
                catch (Exception e)
                {
                    var ie = ScriptManager.InnerMostException(e);
                    if (ie != null) e = ie;
                    return "(excpt " + e.Message + ")";
                }
                return "(unknown)";
            }
            set {
                if (IsReadOnly)
                {
                    if (Value == value) return;
                    throw new InvalidOperationException("ReadOnly cannot set " + ToString() + " to " + value);
                    return;
                }
                saveValue = value;
                try
                {
                   if(SetValue(value)) return;
                   if (Value == value) return;
                }
                catch (Exception e)
                {
                    throw new InvalidOperationException("Error setting " + ToString() + " to " + value, e);
                }
                throw new InvalidOperationException("cannot find how to set " + ToString() + " to " + value);
            }
        }

        private bool SetValue(object value)
        {
            var m = Member;
            object target = null;
            if (UseSingleton && !IsStatic) target = Singleton;
            if (m is FieldInfo)
            {
                var inf = m as FieldInfo;
                if (inf.IsStatic || HasSingleton)
                {
                    object tvalue = ChangeType(value, inf.FieldType);
                    inf.SetValue(target, tvalue);
                    return true;
                }
            }
            if (m is PropertyInfo)
            {
                var inf = m as PropertyInfo;
                m = inf.GetSetMethod();
                if (m == null)
                {
                    // @todo trace
                    object tvalue = ChangeType(value, inf.PropertyType);
                    inf.SetValue(target, value, null);
                    return true;
                }
            }
            if (m is MethodInfo)
            {
                var inf = m as MethodInfo;
                if (inf.IsStatic || HasSingleton)
                {
                    object tvalue = ChangeType(value, inf.GetParameters()[0].ParameterType);
                    inf.Invoke(target, new object[] { tvalue });
                    return true;
                }
            }
            return false;            
        }

        public static object ChangeType(object value, Type type)
        {
            return ScriptManager.ChangeType(value,type);
        }

        static object FindValue(MemberInfo m, object target)
        {
            if (m is FieldInfo)
            {
                var inf = m as FieldInfo;
                return inf.GetValue(target);
            }
            if (m is PropertyInfo)
            {
                var inf = m as PropertyInfo;
                m = inf.GetGetMethod();
            }
            if (m is MethodInfo)
            {
                var inf = m as MethodInfo;
                return inf.Invoke(target, null);
            }
            return null;
        }

        public static bool IsPossibleConfig(MemberInfo info)
        {
            if (HasAttribute(info, typeof(ConfigSettingAttribute))) return true;
            if (HasAttribute(info, typeof(NotConfigurable))) return false;

            if (info.DeclaringType.IsEnum) return false;
            {
                var inf = info as FieldInfo;
                if (inf != null && (inf.IsStatic || IsSingletonClass(inf.DeclaringType)) && 
                    !inf.IsInitOnly && !inf.IsLiteral)
                {
                    if (IsSettableType(inf.FieldType))
                    {
                        if (inf.DeclaringType.IsValueType)
                        {
                            return false;
                        }
                        return true;
                    }
                }
            }
            {
                var inf = info as PropertyInfo;
                if (inf != null)
                {
                    if (inf.GetSetMethod() == null) return false;
                    info = inf.GetGetMethod();
                    var inf0 = info as MethodInfo;
                    if (inf0 != null && (inf0.IsStatic || IsSingletonClass(inf0.DeclaringType))
                         && inf.CanRead && inf.CanWrite)
                    {
                        if (IsSettableType(inf.PropertyType))
                        {
                            if (inf0.DeclaringType.IsValueType)
                            {
                                return false;
                            }
                            return true;
                        }
                        ;
                    }
                }
            }
            return false;       
        }
        public static bool IsGoodForConfig(MemberInfo info, bool notDeclaredOK, bool notPublicOK, bool notWriteableOK, bool notFromStringOK)
        {
            if (HasAttribute(info, typeof(ConfigSettingAttribute))) return true;
            if (HasAttribute(info, typeof(NotConfigurable))) return false;
            if (!notDeclaredOK) return false;

            if (info.DeclaringType.IsEnum) return false;
            {
                var inf = info as FieldInfo;
                if (inf != null)
                {
                    bool readOnly = inf.IsInitOnly || inf.IsLiteral;
                    if ((inf.IsStatic || IsSingletonClass(inf.DeclaringType)) && (notPublicOK || inf.IsPublic) &&
                        (notWriteableOK || !readOnly))
                    {
                        if (!notFromStringOK)
                        {
                            if (inf.FieldType.IsArray)
                            {
                                return false;
                            }
                        }
                        //if (readOnly) notFromStringOK = true;
                        if (notFromStringOK || IsSettableType(inf.FieldType))
                        {
                            if (inf.DeclaringType.IsValueType)
                            {
                                if (readOnly) return false;
                                return false;
                            }
                            return true;
                        }
                    }
                }
            }
            {
                var inf = info as PropertyInfo;
                if (inf != null)
                {
                    MethodInfo m = inf.GetGetMethod(true) ?? inf.GetSetMethod(true);
                    if ((m.IsStatic || IsSingletonClass(inf.DeclaringType))
                        && (notPublicOK || m.IsPublic) && inf.CanRead && (notWriteableOK || inf.CanWrite))
                    {
                        bool readOnly = !inf.CanWrite;
                        //if (readOnly) notFromStringOK = true;
                        if (notFromStringOK || IsSettableType(inf.PropertyType))
                        {
                            if (inf.DeclaringType.IsValueType)
                            {
                                if (readOnly) return false;
                                return false;
                            }
                            return true;
                        }
                        ;
                    }
                }
            }
            return false;
        }
        public static Type MemberValueType(MemberInfo info)
        {
            if (info.DeclaringType.IsEnum) return info.DeclaringType;
            {
                var inf = info as FieldInfo;
                if (inf != null) return inf.FieldType;
            }
            {
                var inf = info as PropertyInfo;
                if (inf != null) return inf.PropertyType;
            }
            {
                var inf = info as MethodInfo;
                if (inf != null) return inf.ReturnType;
            }
            return info.DeclaringType;
        }
        public static bool TestIsStatic(MemberInfo info)
        {
            if (info.DeclaringType.IsEnum) return true;
            {
                var inf = info as FieldInfo;
                if (inf != null && inf.IsStatic)
                {
                    return true;
                }
            }
            {
                var inf = info as PropertyInfo;
                if (inf != null)
                {
                    info = inf.GetSetMethod() ?? inf.GetSetMethod();
                    var inf0 = info as MethodInfo;
                    if (inf0 != null && inf0.IsStatic)
                    {
                        return true;
                    }
                }
            }
            return false;
        }

        private static bool IsSettableType(Type type)
        {
            if (type == null) return false;
            if (type == typeof(string)) return true;
            if (type.IsArray) return false;
            if (type.IsVisible && type.IsValueType) return true;
            if (type == typeof(TimeSpan) || type == typeof(DateTime))
            {
                return true;
            }
            return false;
        }

        static public bool IsReadOnlyMember(MemberInfo member)
        {
            if (member != null)
            {
                var fi = member as FieldInfo;
                if (fi != null) return fi.IsLiteral || fi.IsInitOnly;
                var pi = member as PropertyInfo;
                if (pi != null) return !pi.CanWrite;
            }
            return false;
        }

    }

    [ConfigSetting]
    public interface ContextualSingleton
    {
    }
    public interface NotContextualSingleton
    {
    }
    public class NotConfigurable : Attribute
    {
    }
    public class MemberTree : Attribute
    {
        public string ChildName;
    }
    public class ExactMemberTree : Attribute
    {
    }
    public class SkipMemberTree : Attribute
    {
    }
    public interface HasInstancesOfType: ContextualSingleton
    {
        bool TryGetInstance(Type type, int depth, out object fnd);
    }
    public class TypeConversionAttribute : Attribute
    {
    }
    public interface IKeyValuePair<K, V> : IDisposable
    {
        V Value { get; set; }
        K Key { get; }
        K Comments { get; }
        string DebugInfo { get; }
        bool IsReadOnly { get; }
    }
}
