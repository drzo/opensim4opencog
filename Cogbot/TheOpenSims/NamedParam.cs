using System;
using System.Collections;
using System.Collections.Generic;
using cogbot.TheOpenSims;

namespace cogbot
{
    public struct NamedParam
    {
        public NamedParam(object k, object v)
        {
            Key = k;
            Value = v;
            _Type = null;
            Choices = null;
        }
        public NamedParam(object k, Type type, object v)
        {
            Key = k;
            Value = v;
            _Type = type;
            Choices = null;
        }
        public NamedParam(object k, Type type, object v, params object[] choices)
        {
            Key = k;
            Value = v;
            _Type = type;
            Choices = choices;
        }

        readonly public object Key;
        readonly public object Value;
        readonly public object[] Choices;
        public Type _Type;

        public Type Type
        {
            get
            {
                if (_Type != null) return _Type;
                if (Value is NullType) return ((NullType) Value).Type;
                return Value.GetType();
            }
        }

        public override string ToString()
        {
            if (Key == null) return string.Format("{0}", (Value ?? "NULL"));
            return Key + "=" + Value;
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
                return Equals(Value,obj);
            }
            return Equals(Value,((NamedParam) obj).Value);

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