using System;
using System.Collections;
using System.Collections.Generic;
using System.Reflection;
using System.Xml.Serialization;
using MushDLR223.Utilities;
using KeyType = System.String;


namespace MushDLR223.Utilities
{
    public interface YesAutoLoad
    {
    }

    /// <summary>
    /// Ensure the reflection API is not used to load this command
    /// The command is loaded likely byu the plugin 
    /// </summary>
    public interface NotAutoLoaded
    {
    }
}

namespace MushDLR223.ScriptEngines
{
    public struct NullType
    {
        public readonly Object Inst;
        public MemberInfo Type; // { get; set; }

        public NullType(Object inst, MemberInfo type)
        {
            Inst = inst;
            Type = type;
        }

        public NullType(Type type)
        {
            Inst = null;
            Type = type;
        }

        public override int GetHashCode()
        {
            return Type.GetHashCode() ^ Inst.GetHashCode();
        }

        public static NullType GetNullType(Type dataType)
        {
            return new NullType(dataType);
        }

        public override string ToString()
        {
            return "(" + Type.Name + ")null";
        }
    }

    [XmlType]
    public struct NamedParam
    {
        private readonly Type _Type;
        public readonly object[] Choices;
        private String _key;
        private object _value;
        public string Comment;
        public MemberInfo info;
        public bool IsFlag;
        public bool IsOneOf;
        public bool IsOptional;
        public bool IsRequired
        {
            get { return !IsOptional; }
            set { IsOptional = !value; }
        }
        public bool IsRest;
        public bool IsSequence;
        public object memberTarget;

        /// <summary>
        /// For building simevent info
        /// </summary>
        /// <param name="k"></param>
        /// <param name="v"></param>
        public NamedParam(String k, object v)
            : this()
        {
            _key = ToKey(k);
            _value = v;
            if (v != null)
            {
                if (v is Type)
                {
                    _Type = v as Type;
                }
                else
                {
                    _Type = v.GetType();
                }
            }
            checkKey(k);
        }

        public NamedParam(string k, Type v)
            : this(k, (object) null)
        {
            _Type = v;
        }

        /// <summary>
        /// Used for describing commands
        /// </summary>
        /// <param name="k"></param>
        /// <param name="type"></param>
        /// <param name="v"></param>
        public NamedParam(String k, Type type, Type v)
            : this(k, (object) v)
        {
            _Type = type;
        }

        /// <summary>
        /// Used for making InfoMaps
        /// </summary>
        /// <param name="target"></param>
        /// <param name="inf"></param>
        /// <param name="k"></param>
        /// <param name="type"></param>
        /// <param name="v"></param>
        public NamedParam(object target, MemberInfo inf, String k, Type type, object v)
            : this(k, v)
        {
            memberTarget = target;
            info = inf;
            _Type = type ?? FieldType(inf);
            checkKey(k);
        }

        /// <summary>
        /// Used for describing commands
        /// </summary>
        /// <param name="k"></param>
        /// <param name="type"></param>
        /// <param name="v"></param>
        /// <param name="choices"></param>
        public NamedParam(String k, Type type, object v, params object[] choices)
            : this(k, v)
        {
            _Type = type;
            Choices = choices;
            info = null;
            checkKey(k);
        }


        /// <summary>
        /// Used for constructing Cyc-like functions
        /// </summary>
        /// <param name="param"></param>
        /// <param name="o"></param>
        public NamedParam(NamedParam param, object o)
            : this()
        {
            memberTarget = param.memberTarget;
            _Type = param._Type;
            _value = o;
            _key = param._key;
            Choices = param.Choices;
            info = param.info;
        }

        /// <summary>
        /// Used for describing commands 
        /// </summary>
        /// <param name="type"></param>
        /// <param name="DataType"></param>
        public NamedParam(Type type, Type DataType)
            : this()
        {
            _Type = type;
            _value = NullType.GetNullType(DataType);
            _key = _Type.Name;
        }

        public KeyValuePair<string, object> Pair
        {
            get { return new KeyValuePair<string, object>(_key, _value); }
        }

        public Type Type
        {
            get
            {
                if (_Type != null) return _Type;
                if (_value is NullType) return ((NullType) _value).Type.DeclaringType;
                if (_value != null) return _value.GetType();
                if (info == null) return null;
                return info.DeclaringType;
            }
        }

        public Type SourceType
        {
            get
            {
                if (info != null) return info.DeclaringType;
                return Type;
            }
        }

        public Type DestinationType
        {
            get { return Type; }
        }

        [XmlArrayItem]
        public object Value
        {
            get { return _value; }
            set { SetValue(value); }
        }

        public object ObjectValue
        {
            get
            {
                object val = Value;
                if (val is NullType) return null;
                return val;
            }
            set { SetValue(value); }
        }

        [XmlArrayItem]
        public string Key
        {
            get { return _key; }
            set { _key = string.Intern(value); }
        }

        private static void checkKey(object o)
        {
            return;
            if (o != null)
                if (o.ToString().Contains("offsetU"))
                {
                }
        }

        private static Type FieldType(MemberInfo field)
        {
            if (field is FieldInfo)
            {
                return ((FieldInfo) field).FieldType;
            }
            if (field is PropertyInfo)
            {
                return ((PropertyInfo) field).PropertyType;
            }
            if (field is MethodInfo)
            {
                var mi = (MethodInfo) field;
                return mi.ReturnType;
            }
            if (field is ConstructorInfo)
            {
                return (field).DeclaringType;
            }
            throw new IndexOutOfRangeException("" + field);
        }

        private static string ToKey(string s)
        {
            if (s == null) return null;
            return Intern(s);
        }

        public void SetValue(object value)
        {
            _value = value;
            if (info == null || memberTarget == null)
            {
                DLRConsole.DebugWriteLine("No way to set value on NamedParam " + this);
                return;
            }
            SetMemberValue(info, memberTarget, value);
        }

        public override string ToString()
        {
            if (_key == null) return string.Format("{0}", (_value ?? "NULL"));
            return string.Format("{0}='{1}'", (_key ?? "NULL"), (_value ?? "NULL"));
        }

        public static bool operator ==(NamedParam p1, NamedParam p2)
        {
            return p1.Equals(p2);
        }

        public static bool operator !=(NamedParam p1, NamedParam p2)
        {
            return !(p1 == p2);
        }

        // override object.Equals
        public override bool Equals(object obj)
        {
            //       
            // See the full list of guidelines at
            //   http://go.microsoft.com/fwlink/?LinkID=85237  
            // and also the guidance for operator== at
            //   http://go.microsoft.com/fwlink/?LinkId=85238
            //

            if (obj == null || GetType() != obj.GetType())
            {
                return Equals(_value, obj);
            }
            return Equals(_value, ((NamedParam) obj)._value);
        }

        // override object.GetHashCode
        public override int GetHashCode()
        {
            // TODO: write your implementation of GetHashCode() here
            return base.GetHashCode();
        }

        public static T[] ToArray<T>(IEnumerable<T> args)
        {
            if (args is T[]) return (T[]) args;
            if (args is List<T>) return ((List<T>) args).ToArray();
            return new List<T>(args).ToArray();
        }

        public static NamedParam[] ObjectsToParams(IEnumerable paramz)
        {
            var Parameters = new List<NamedParam>();
            int pn = 0;
            foreach (object v in paramz)
            {
                if (v is NamedParam)
                {
                    Parameters.Add((NamedParam) v);
                }
                else
                {
                    Parameters.Add(new NamedParam("p" + pn, v));
                }
                pn++;
            }
            return Parameters.ToArray();
        }

        public static void SetMemberValue(MemberInfo field, object o, object value)
        {
            if (field is FieldInfo)
            {
                ((FieldInfo) field).SetValue(o, value);
                return;
            }
            if (field is PropertyInfo)
            {
                MethodInfo setterMethod = ((PropertyInfo) field).GetSetMethod(true);
                if (setterMethod == null)
                {
                    DLRConsole.DebugWriteLine("No setter method on " + field);
                    return;
                }
                setterMethod.Invoke(o, new[] {value});
                return;
            }
            if (field is MethodInfo)
            {
                var mi = (MethodInfo) field;
                if (mi.IsStatic)
                {
                    mi.Invoke(null, new[] {o, value});
                    return;
                }
                ((MethodInfo) field).Invoke(o, new[] {value});
                return;
            }
            throw new IndexOutOfRangeException("" + field);
            if (field is ConstructorInfo)
            {
                ((ConstructorInfo) field).Invoke(new[] {o, value});
                return;
            }
        }

        public static NamedParam[] CreateParams(params object[] paramz)
        {
            var paramsz = new List<NamedParam>();
            int argNum = 1;
            for (int i = 0; i < paramz.Length;)
            {
                object o = paramz[i++];
                if (o is NamedParam)
                {
                    paramsz.Add((NamedParam) o);
                    continue;
                }
                if (o is string)
                {
                    var k = (string) o;
                    var t = paramz[i++] as Type;
                    string comment = "" + paramz[i++];
                    var namedParam = new NamedParam(k, t);
                    namedParam.Comment = comment;
                    paramsz.Add(namedParam);
                }
            }
            return paramsz.ToArray();
        }

        public static NamedParam[][] CreateParamVersions(params NamedParam[][] paramz)
        {
            return paramz;
        }

        public static NamedParam Optional(string name, Type type, string description)
        {
            var namedParam = new NamedParam(name, type);
            namedParam.Comment = Intern(description);
            namedParam.IsOptional = true;
            return namedParam;
        }

        private static string Intern(string s)
        {
            if (s == null) return null;
            return string.Intern(s);
        }

        public static NamedParam Rest(string name, Type type, string description)
        {
            var namedParam = new NamedParam(name, type);
            namedParam.Comment = description;
            namedParam.IsOptional = true;
            return namedParam;
        }

        public static string Usage(String cmdname, NamedParam[] parameters, string description)
        {
            throw new NotImplementedException();
        }
    }
}