using System;
using System.Globalization;
using System.Xml;

namespace RTParser
{
    public class Unifiable
    {
        public static Unifiable Empty
        {
            get { return ""; }
        }

        public static implicit operator string(Unifiable value)
        {
            if (Object.ReferenceEquals(value, null))
            {
                return null;
            }
            return value.AsString();
        }

        public static implicit operator Unifiable(string value)
        {
            if (value == null) return null;
            return new Unifiable(value);
        }

        internal static Unifiable Join(string p, Unifiable[] fsp, int p_3, int p_4)
        {
            return string.Join(p, FromArrayOf(fsp), p_3, p_4);
        }

        private static Unifiable[] arrayOf(string[] strs)
        {
            Unifiable[] it = new Unifiable[strs.Length];
            for (int i = 0; i < it.Length; i++)
            {
                it[i] = strs[i];
            }
            return it;
        }

        private static string[] FromArrayOf(Unifiable[] tokens)
        {
            string[] it = new string[tokens.Length];
            for (int i = 0; i < it.Length; i++)
            {
                it[i] = tokens[i];
            }
            return it;
        }

        public static Unifiable Format(string s, params object[] args)
        {
            return string.Format(s, args);
        }

        static public bool operator ==(Unifiable t, string s)
        {
            return t.AsString() == s;
        }


        static public bool operator ==(Unifiable t, Unifiable s)
        {
            if (IsNull(t))
            {
                return IsNull(s);
            }
            if (IsNull(s))
            {
                return false;
            }

            return t.AsString().ToLower() == s.AsString().ToLower();
        }

        public static bool operator !=(Unifiable t, Unifiable s)
        {
            return !(t == s);
        }


        public static bool operator !=(Unifiable t, string s)
        {
            return !(t == s);
        }


        static public bool operator ==(string s, Unifiable t)
        {
            return t.AsString() == s;
        }

        public static bool operator !=(string s, Unifiable t)
        {
            return !(s == t);
        }


        private readonly string str;

        public Unifiable(string value)
        {
            str = value;
        }

        public int Length
        {
            get { return str.Length; }
        }


        public Unifiable Replace(object marker, object param1)
        {
            return str.Replace(astr(marker), astr(param1));
        }

        private static string astr(object param1)
        {
            return "" + param1;
        }


        public Unifiable Trim()
        {
            return str.Trim();
        }

        public string AsString()
        {
            return str;
        }

        internal Unifiable ToLower()
        {
            return str.ToLower();
        }

        public Unifiable ToUpper()
        {
            return str.ToUpper();
        }

        public Unifiable Substring(int i, int ii)
        {
            return str.Substring(i, ii);
        }

        internal char[] ToCharArray()
        {
            return str.ToCharArray();
        }

        public bool EndsWith(string s)
        {
            return str.EndsWith(s);
        }

        public bool StartsWith(string s)
        {
            return str.StartsWith(s);
        }

        public Unifiable[] Split(char[] c, StringSplitOptions options)
        {
            return arrayOf(str.Split(c, options));
        }


        public override bool Equals(object obj)
        {
            return str == obj.ToString();
        }

        public override string ToString()
        {
            return str.ToString();
        }

        public override int GetHashCode()
        {
            return str.GetHashCode();
        }

        internal Unifiable Substring(int p)
        {
            return str.Substring(p);
        }

        internal int IndexOf(string p)
        {
            return str.IndexOf(p);
        }


        internal Unifiable[] Split(Unifiable[] tokens, StringSplitOptions stringSplitOptions)
        {
            return arrayOf(str.Split(FromArrayOf(tokens), stringSplitOptions));
        }


        internal Unifiable[] Split(char[] p)
        {
            return arrayOf(str.Split(p));
        }

        public bool Contains(string p)
        {
            return str.Contains(p);
        }

        internal string ToLower(CultureInfo cultureInfo)
        {
            return str.ToLower(cultureInfo);
        }

        internal string ToUpper(CultureInfo cultureInfo)
        {
            return str.ToUpper(cultureInfo);
        }

        internal Unifiable TrimEnd()
        {
            return str.TrimEnd();
        }

        public Unifiable TrimStart()
        {
            return str.TrimStart();
        }

        static internal bool IsTrue(Unifiable v)
        {
            return !IsFalse(v);
        }

        internal bool IsWildCard()
        {
            return (str == "*" || str == "_");
        }

        public Unifiable[] Split()
        {
            return arrayOf(str.Split(" \r\n\t".ToCharArray()));
        }

        public bool IsTag(string that)
        {
            return str == that;
        }

        internal static bool IsFalse(Unifiable tf)
        {
            if (Object.ReferenceEquals(tf, null)) return true;
            string found = tf.AsString();
            return  found.Trim() == "" || found.Trim() == "NIL";
        }

        internal static bool IsNull(Unifiable name)
        {
            return Object.ReferenceEquals(name, null) || name.str == null;
        }
    }
}