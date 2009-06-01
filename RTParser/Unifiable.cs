using System;
using System.Globalization;
using System.Xml;
using RTParser.AIMLTagHandlers;

namespace RTParser
{

    public class Unifiable
    {
        public static Unifiable Empty = new Unifiable("")
                                 //{
                                 //    public override void Append(Unifiable p)
                                 //    {
                                 //        if (str == "")
                                 //            str = p.AsString();
                                 //        else
                                 //        {
                                 //            str += " ";
                                 //            str += p.AsString();
                                 //        }
                                 //    }
                                 //} 
                                 ;

                                            
        public static Unifiable STAR = new Unifiable("*");
        public static Unifiable UNIV_STAR
        {
            get
            {
                return new Unifiable("*");
            }
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

        public static Unifiable Join(string p, Unifiable[] fsp, int p_3, int p_4)
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

        protected string str;

        public Unifiable()
        {
            str = "";
        }

        public Unifiable(string value)
        {
            str = value;
        }

        public int Length
        {
            get
            {
                if (str == null)
                {
                    return 0;
                }
                return str.Length;
            }
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
            string str2 = str.Trim();
            if (str2==str) return this;
            return str.Trim();
        }

        public string AsString()
        {
            return str;
        }

        public virtual Unifiable ToLower()
        {
            return str.ToLower();
        }

        public Unifiable ToUpper()
        {
            return Create(str.ToUpper());
        }

        public Unifiable Substring(int i, int ii)
        {
            return str.Substring(i, ii);
        }

        public virtual char[] ToCharArray()
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

        //public Unifiable[] Split(char[] c, StringSplitOptions options)
        //{
        //    return arrayOf(str.Split(c, options));
        //}


        public override bool Equals(object obj)
        {
            if (obj is Unifiable) return ((Unifiable)obj) == this;
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

        public virtual Unifiable Substring(int p)
        {
            return str.Substring(p);
        }

        public virtual int IndexOf(string p)
        {
            return str.IndexOf(p);
        }


        public virtual Unifiable[] Split(Unifiable[] tokens, StringSplitOptions stringSplitOptions)
        {
            return arrayOf(str.Split(FromArrayOf(tokens), stringSplitOptions));
        }


        //public virtual Unifiable[] Split(char[] p)
        //{
        //    return arrayOf(str.Split(p));
        //}

        public bool Contains(string p)
        {
            return str.Contains(p);
        }

        public virtual string ToLower(CultureInfo cultureInfo)
        {
            return str.ToLower(cultureInfo);
        }

        public virtual string ToUpper(CultureInfo cultureInfo)
        {
            return str.ToUpper(cultureInfo);
        }

        public virtual Unifiable TrimEnd()
        {
            return str.TrimEnd();
        }

        public Unifiable TrimStart()
        {
            return str.TrimStart();
        }

        static public bool IsTrue(Unifiable v)
        {
            return !IsFalse(v);
        }

        public virtual bool IsWildCard()
        {
            return (str.Contains("*") || str.Contains("_") || str.Contains("<"));
        }



        public Unifiable[] Split()
        {
            return arrayOf(str.Trim().Split(BRKCHARS, StringSplitOptions.RemoveEmptyEntries));
        }

        public bool IsTag(string that)
        {
            return str == that;
        }

        public static bool IsFalse(Unifiable tf)
        {
            if (Object.ReferenceEquals(tf, null)) return true;
            string found = tf.AsString();
            return found.Trim() == "" || found.Trim() == "NIL";
        }

        public static bool IsNull(Object name)
        {
            if (Object.ReferenceEquals(name, null)) return true;
            return (name is Unifiable && ((Unifiable) name).str == null);
        }

        public virtual string GetSettingName()
        {
            return str;
        }

        public static Unifiable operator +(Unifiable u, string more)
        {
            return u.str + more;
        }
        public static Unifiable operator +(Unifiable u, Unifiable more)
        {
            return u.str + more.AsString();
        }

        public static Unifiable Create(string p)
        {
            return new Unifiable(p);
        }
        public static Unifiable Create(Unifiable p)
        {
            return p;
        }

        internal static Unifiable CreateFromObject(XmlNode pattern)
        {
           // TODO
            return new Unifiable(pattern.InnerXml);
        }

        public virtual void Append(Unifiable p)
        {
            if (str == "")
                str = p.AsString();
            else
            {
                str += " ";
                str += p.AsString();
            }
        }

        public virtual void Remove(int p, int c)
        {
            str = str.Remove(p, c);
        }

        public virtual Unifiable Frozen()
        {
            return Create(str);
        }


        internal Unifiable ToPropper()
        {
            int len = str.Length;

            if (len == 0) return this;
            string newWord = str.Substring(0, 1).ToUpper();
            if (len == 1)
            {
                if (newWord == str) return this;
            }
            newWord += str.Substring(1);
            return newWord;
        }

        internal Unifiable Rest()
        {
            if (String.IsNullOrEmpty(this.str)) return Unifiable.Empty;
            int i = str.IndexOfAny(BRKCHARS);
            if (i == -1) return Empty;
            string rest = str.Substring(i + 1);
            return Create(rest.Trim());
        }

        readonly static char[] BRKCHARS = " \r\n\t".ToCharArray();

        internal Unifiable First()
        {
            if (String.IsNullOrEmpty(str)) return Unifiable.Empty;
            int i = str.IndexOfAny(BRKCHARS);
            if (i == -1) return Create(str);
            string rest = str.Substring(0, i - 1);
            return Create(rest.Trim());
        }
    }
}

