using System;
using System.Collections;
using System.Collections.Generic;
using System.Reflection;
using System.Runtime.InteropServices;
using System.Xml.Serialization;
using cogbot.TheOpenSims;
using MushDLR223.Utilities;
using KeyType = System.String;
namespace cogbot
{
    [XmlType]
    public struct NamedParam
    {
        /// <summary>
        /// For building simevent info
        /// </summary>
        /// <param name="k"></param>
        /// <param name="v"></param>
        internal NamedParam(KeyType k, object v)
        {
            memberTarget = null;
            _key = ToKey(k);
            _value = v;
            _Type = null;
            Choices = null;
            info = null;
            checkKey(k);
        }
        
        private NamedParam(string k, Type v)
        {
            memberTarget = null;
            _key = ToKey(k);
            _value = v;
            _Type = null;
            Choices = null;
            info = null;
            checkKey(k);
        }

        /// <summary>
        /// Used for describing commands
        /// </summary>
        /// <param name="k"></param>
        /// <param name="type"></param>
        /// <param name="v"></param>
        public NamedParam(KeyType k, Type type, Type v)
        {
            memberTarget = null;
            _key = ToKey(k);
            _value = v;
            _Type = type;
            Choices = null;
            info = null;
            checkKey(k);
        }

        private static void checkKey(object o)
        {
            return;
            if (o!=null) if (o.ToString().Contains("offsetU"))
            {
                
            }
        }

        /// <summary>
        /// Used for making InfoMaps
        /// </summary>
        /// <param name="target"></param>
        /// <param name="inf"></param>
        /// <param name="k"></param>
        /// <param name="type"></param>
        /// <param name="v"></param>
        internal NamedParam(object target, MemberInfo inf, KeyType k, Type type, object v)
        {
            memberTarget = target;
            info = inf;
            _key = ToKey(k);
            _value = v;
            _Type = type ?? FieldType(inf);
            Choices = null;
            checkKey(k);
        }

        /// <summary>
        /// Used for describing commands
        /// </summary>
        /// <param name="k"></param>
        /// <param name="type"></param>
        /// <param name="v"></param>
        /// <param name="choices"></param>
        internal NamedParam(KeyType k, Type type, object v, params object[] choices)
        {
            memberTarget = null;
            _key = ToKey(k);
            _value = v;
            _Type = type;
            Choices = choices;
            info = null;
            checkKey(k);
        }

        private static Type FieldType(MemberInfo field)
        {
            if (field is FieldInfo)
            {
                return ((FieldInfo)field).FieldType;
            }
            if (field is PropertyInfo)
            {
                return ((PropertyInfo)field).PropertyType;
            }
            if (field is MethodInfo)
            {
                MethodInfo mi = (MethodInfo)field;
                return mi.ReturnType;
            }
            if (field is ConstructorInfo)
            {
                return ((ConstructorInfo)field).DeclaringType;
            }
            throw new IndexOutOfRangeException("" + field);
        }

        private static string ToKey(string s)
        {
            if (s == null) return null;
            return string.Intern(s);
        }

        public KeyValuePair<string, object> Pair
        {
            get { return new KeyValuePair<string, object>(_key, _value); }
        }


        private KeyType _key;
        private object _value;
        readonly public object[] Choices;
        public MemberInfo info;
        private Type _Type;
        public object memberTarget;

        /// <summary>
        /// Used for constructing Cyc-like functions
        /// </summary>
        /// <param name="param"></param>
        /// <param name="o"></param>
        internal NamedParam(NamedParam param, object o)
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
        internal NamedParam(Type type, Type DataType)
        {
            memberTarget = null;
            _Type = type;
            _value = NullType.GetNullType(DataType);
            _key = _Type.Name;
            Choices = null;
            info = null;
        }

        public Type Type
        {
            get
            {
                if (_Type != null) return _Type;
                if (_value is NullType) return ((NullType) _value).Type.DeclaringType;
                if (_value != null) return _value.GetType();
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
            get
            {                
                return Type;
            }
        }

        [XmlArrayItem]
        public object Value
        {
            get { return _value; }
            set { SetValue(value); }
        }

        internal void SetValue(object value)
        {
            _value = value;
            if (info == null || memberTarget == null)
            {
                DLRConsole.DebugWriteLine("No way to set value on NamedParam " + this);
                return;
            }
            SetMemberValue(info, memberTarget, value);
        }

        [XmlArrayItem]
        public string Key
        {
            get { return _key; }
            set { _key = value; }
        }

        public override string ToString()
        {
            if (_key == null) return string.Format("{0}", (_value ?? "NULL"));
            return string.Format("{0}='{1}'", (_key ?? "NULL"), (_value ?? "NULL"));
        }

        public static bool operator ==(NamedParam p1,NamedParam p2)
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
                return Equals(_value,obj);
            }
            return Equals(_value,((NamedParam) obj)._value);

        }

        // override object.GetHashCode
        public override int GetHashCode()
        {
            // TODO: write your implementation of GetHashCode() here
            return base.GetHashCode();
        }

        public static List<NamedParam> ObjectsToParams(IEnumerable paramz)
        {
            List<NamedParam> Parameters = new List<NamedParam>();
            foreach (var v in paramz)
            {
                if (v is NamedParam)
                {
                    Parameters.Add((NamedParam)v);
                }
                else
                {
                    Parameters.Add(new NamedParam(null, v));
                }
            }
            return Parameters;
        }

        public static void SetMemberValue(MemberInfo field, object o, object value)
        {
            if (field is FieldInfo)
            {
                ((FieldInfo)field).SetValue(o, value);
                return;
            }
            if (field is PropertyInfo)
            {
                MethodInfo setterMethod = ((PropertyInfo)field).GetSetMethod(true);
                if (setterMethod == null)
                {
                    DLRConsole.DebugWriteLine("No setter method on " + field);
                    return;
                }
                setterMethod.Invoke(o, new object[] { value });
                return;
            }
            if (field is MethodInfo)
            {
                MethodInfo mi = (MethodInfo)field;
                if (mi.IsStatic)
                {
                    mi.Invoke(null, new object[] { o, value });
                    return;
                }
                ((MethodInfo)field).Invoke(o, new object[] { value });
                return;
            }
            throw new IndexOutOfRangeException("" + field);
            if (field is ConstructorInfo)
            {
                ((ConstructorInfo)field).Invoke(new object[] { o, value });
                return;
            }
        }
    }
}