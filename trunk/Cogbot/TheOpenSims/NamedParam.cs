using System;
using System.Collections;
using System.Collections.Generic;
using System.Reflection;
using cogbot.TheOpenSims;
using KeyType = System.String;
namespace cogbot
{
    public struct NamedParam
    {
        public NamedParam(KeyType k, object v)
        {
            _key = ToKey(k);
            _value = v;
            _Type = null;
            Choices = null;
            info = null;
            checkKey(k);
        }
        public NamedParam(string k, Type v)
        {
            _key = ToKey(k);
            _value = v;
            _Type = null;
            Choices = null;
            info = null;
            checkKey(k);
        }
        public NamedParam(KeyType k, Type type, object v)
        {
            _key = ToKey(k);
            _value = v;
            _Type = type;
            Choices = null;
            info = null;
            checkKey(k);
        }

        private void checkKey(object o)
        {
            return;
            if (o!=null) if (o.ToString().Contains("offsetU"))
            {
                
            }
        }

        public NamedParam(MemberInfo inf, KeyType k, Type type, object v)
        {
            info = inf;
            _key = ToKey(k);
            _value = v;
            _Type = type;
            Choices = null;
            checkKey(k);
        }
        public NamedParam(KeyType k, Type type, object v, params object[] choices)
        {
            _key = ToKey(k);
            _value = v;
            _Type = type;
            Choices = choices;
            info = null;
            checkKey(k);
        }

        private static string ToKey(string s)
        {
            return string.Intern(s);
        }

        private readonly KeyType _key;
        private readonly object _value;
        readonly public object[] Choices;
        public readonly MemberInfo info;
        private readonly Type _Type;

        public NamedParam(NamedParam param, object o)
        {
            _Type = param._Type;
            _value = o;
            _key = param._key;
            Choices = param.Choices;
            info = param.info;
        }

        public NamedParam(Type type, Type DataType)
        {
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

        public object Value
        {
            get { return _value; }
        }

        public string Key
        {
            get { return _key; }
        }

        public override string ToString()
        {
            if (_key == null) return string.Format("{0}", (_value ?? "NULL"));
            return _key + "=" + _value;
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

    }
}