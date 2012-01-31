using System;
using System.Reflection;

namespace MushDLR223.Utilities
{
    public class ConfigSettingAttribute : Attribute
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
                var fnd = member.DeclaringType.GetMember("SingleInstance");
                if (fnd != null && fnd.Length > 0)
                {
                    var fnd0 = fnd[0];
                    _singleton = FindValue(fnd0, null);
                }
            }
        }

        public static bool IsSingletonClass(Type type)
        {
            var fnd = type.GetMember("SingleInstance");
            return (fnd != null && fnd.Length > 0);
        }

        private MemberInfo member;
        private object initialValue;
        private object saveValue;
        private object _singleton;
        public bool UseSingleton;
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
        public bool ReadOnly { get; set; }
        public bool SkipSaveOnExit { get; set; }

        public void Save()
        {
            if (SkipSaveOnExit) return;
            if (saveValue == null) return;
            Value = saveValue;
        }

        private string _name;
        public string Name
        {
            get
            {
                if (_name == null)
                {
                    return "" + member.DeclaringType.Name + ":" + member.Name;
                }
                return _name;
            }
            set { _name = value; }
        }

        private string _description;
        public bool IsStatic;

        public string Description
        {
            get
            {
                if (_description == null)
                {
                    return "Member setter " + member + " for " + member.DeclaringType.FullName;
                }
                return _description;
            }
            set { _description = value; }
        }
        public override bool Equals(object obj)
        {
            var other = obj as ConfigSettingAttribute;
            return other != null && other.member == member;
        }
        public override int GetHashCode()
        {
            if (member == null) return -1;
            return member.GetHashCode();
        }
        public void SetMember(MemberInfo member0)
        {
            member = member0;
            IsStatic = TestIsStatic(member0);
            UseSingleton = !IsStatic;
        }

        public string DebugInfo
        {
            get
            {
                string comments = "";
                if (SkipSaveOnExit) comments += " SkipSavedOnExit";
                if (ReadOnly) comments += " ReadOnly";
                if (!string.IsNullOrEmpty(_description)) comments += " " + _description;

                if (comments.Length != 0)
                {
                    return Name + " = " + Value + " //" + comments;
                }
                return Name + " = " + Value;
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
                var m = this.member;
                object target = null;
                if (UseSingleton && !IsStatic) target = Singleton;
                try
                {
                    var v = FindValue(m, target);
                    if (v != null) return v;
                }
                catch (Exception e)
                {
                    var ie = e.InnerException;
                    if (ie != null) e = ie;
                    return e.Message;
                }
                return "(unknown)";
            }
            set
            {
                if (ReadOnly)
                {
                    throw new InvalidOperationException("ReadOnly cannot set " + ToString() + " to " + value);
                    return;
                }
                var m = this.member;
                saveValue = value;
                object target = null;
                if (UseSingleton && !IsStatic) target = Singleton;
                if (m is FieldInfo)
                {
                    var inf = m as FieldInfo;
                    if (inf.IsStatic || HasSingleton)
                    {
                        object tvalue = Convert.ChangeType(value, inf.FieldType);
                        inf.SetValue(target, tvalue);
                        return;
                    }
                }
                if (m is PropertyInfo)
                {
                    var inf = m as PropertyInfo;
                    m = inf.GetSetMethod();
                }
                if (m is MethodInfo)
                {
                    var inf = m as MethodInfo;
                    if (inf.IsStatic || HasSingleton)
                    {
                        object tvalue = Convert.ChangeType(value, inf.GetParameters()[0].ParameterType);
                        inf.Invoke(target, new object[] { tvalue });
                        return;
                    }
                }
                if (Value == value) return;
                throw new InvalidOperationException("cannot find how to set " + ToString() + " to " + value);
            }
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

        public bool Finds(string find)
        {
            return string.IsNullOrEmpty(find) || DebugInfo.ToLower().Contains(find);
        }

        public static ConfigSettingAttribute CreateSetting(MemberInfo s)
        {
            ConfigSettingAttribute attr = new ConfigSettingAttribute();
            attr.SetMember(s);
            return attr;
        }

        public static bool IsGoodForConfig(MemberInfo info)
        {
            if (info.DeclaringType.IsEnum) return false;
            {
                var inf = info as FieldInfo;
                if (inf != null && (inf.IsStatic || IsSingletonClass(inf.DeclaringType)) && inf.IsPublic &&
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
                        && inf0.IsPublic && inf.CanRead && inf.CanWrite)
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
                    info = inf.GetSetMethod();
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
            if (type == typeof (string)) return true;
            if (type.IsArray) return false;
            if (type.IsVisible && type.IsValueType) return true;
            return false;
        }
    }
}
