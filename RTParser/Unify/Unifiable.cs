using System;
using System.Collections.Generic;
using System.Globalization;
using System.Xml;
using RTParser.Utils;
using UPath = RTParser.Unifiable;

namespace RTParser
{
    abstract public class Unifiable
    {

        public const float UNIFY_TRUE = 0;
        public const float UNIFY_FALSE = 1;


        /// <summary>
        /// This should be overridden!
        /// </summary>
        /// <param name="obj"></param>
        /// <returns></returns>
        public override bool Equals(object obj)
        {
            throw new NotImplementedException();
        }

        /// <summary>
        /// This should be overridden!
        /// </summary>
        /// <returns></returns>
        public override int GetHashCode()
        {
            throw new NotImplementedException();
        }

        public static string InnerXmlText(XmlNode templateNode)
        {
            if (templateNode.NodeType == XmlNodeType.Text)
            {
                if (templateNode.InnerXml.Length>0)
                {
                    return templateNode.InnerText + templateNode.InnerXml;                   
                }
                if (templateNode.InnerText.Length > 0)
                {
                    return templateNode.InnerText + templateNode.InnerXml;
                }
                return templateNode.InnerText;
            }
            return templateNode.InnerXml;
        }

                
        public static implicit operator string(Unifiable value)
        {
            if (Object.ReferenceEquals(value, null))
            {
                return null;
            }
            return value.AsString();
        }

        static Dictionary<string,Unifiable> internedUnifiables = new Dictionary<string,Unifiable>(20000);
        public static implicit operator Unifiable(string value)
        {
            return MakeStringUnfiable(value);
        }

        private static Unifiable MakeStringUnfiable(string value)
        {
            if (value == null) return null;
            Unifiable u;
            if (true)
                lock (internedUnifiables)
                {
                    if (!internedUnifiables.TryGetValue(value, out u))
                    {
                        u = internedUnifiables[value] = new StringUnifiable(value);
                    }
                    return u;
                }
            u = new StringUnifiable(value);
            return u;
        }

        public static Unifiable Empty = new EmptyUnifiable();


        public static Unifiable STAR
        {
            get
            {
                return MakeStringUnfiable("*");
            }
        }

        static public bool IsTrue(Unifiable v)
        {
            return !IsFalse(v);
        }

        static public bool IsLogicTF(Unifiable v)
        {
            if (IsFalse(v)) return false;
            String value = v.ToValue().ToLower();
            if (value.Length == 0) return false;
            char c = value[0];
            if (c == 'n' || c == 'f') return false;
            return true;           
        }


        public static Unifiable Join(string sep, Unifiable[] values, int startIndex, int count)
        {
            return string.Join(sep, FromArrayOf(values), startIndex, count);
        }

        public static Unifiable[] arrayOf(string[] strs)
        {
            Unifiable[] it = new Unifiable[strs.Length];
            for (int i = 0; i < it.Length; i++)
            {
                it[i] = Create(strs[i].Trim());
            }
            return it;
        }

        public static string[] FromArrayOf(Unifiable[] tokens)
        {
            string[] it = new string[tokens.Length];
            for (int i = 0; i < it.Length; i++)
            {
                it[i] = tokens[i].AsString().Trim();
            }
            return it;
        }

        //public static Unifiable Format(string s, params object[] args)
        //{
        //    return string.Format(s, args);
        //}

        //static public bool operator ==(Unifiable t, string s)
        //{
        //    return t.AsString().ToLower() == s.ToLower();
        //}


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

            if (t.AsString().ToLower() == s.AsString().ToLower()) return true;
            if (t.ToValue().ToLower() == s.ToValue().ToLower())
            {
                return true;
            }

            return false;
        }

        public static bool operator !=(Unifiable t, Unifiable s)
        {
            return !(t == s);
        }

        public static bool IsFalse(Unifiable tf)
        {
            if (Object.ReferenceEquals(tf, null)) return true;
            if (Object.ReferenceEquals(tf.Raw, null)) return true;
            return tf.IsFalse();
        }

        public static bool IsNullOrEmpty(Object name)
        {
            if (Object.ReferenceEquals(name, null)) return true;
            if (name is String)
            {
                return String.IsNullOrEmpty((String)name);
            }
            return (name is Unifiable && ((Unifiable)name).Raw == null);
        }
        public static bool IsNull(Object name)
        {
            if (Object.ReferenceEquals(name, null)) return true;
            return (name is Unifiable && ((Unifiable)name).Raw == null);
        }

        public static Unifiable operator +(Unifiable u, string more)
        {
            return u.AsString() + more;
        }
        public static Unifiable operator +(Unifiable u, Unifiable more)
        {
            return u.AsString() + more.AsString();
        }

        public static Unifiable Create(object p)
        {
            if (p is string)
            {
                return MakeStringUnfiable((string)p);
            }
            if (p is Unifiable) return (Unifiable) p;
            if (p is XmlNode) return MakeStringUnfiable(InnerXmlText((XmlNode)p));
            // TODO
            if (p == null)
            {
                return null;
            }
            return MakeStringUnfiable(p.ToString());
        }

        internal static StringAppendableUnifiable CreateAppendable()
        {
            var u = new StringAppendableUnifiable();
            return u;
        }

        public static Unifiable ThatTag = Create("TAG-THAT");
        public static Unifiable TopicTag = Create("TAG-TOPIC");
        public static Unifiable FlagTag = Create("TAG-FLAG");

        public abstract object Raw { get; }
        public virtual bool IsEmpty
        {
            get
            {
                return string.IsNullOrEmpty(ToValue());
            }
        }

        public Unifiable LegacyPath
        {
            get { return this; }
        }

        protected virtual bool IsFalse()
        {
            return IsEmpty;            
        }
        public abstract bool IsTag(string s);
        public abstract bool IsMatch(Unifiable unifiable);
        public virtual bool IsWildCard()
        {
            return true;
        }
        public abstract bool IsLazyStar();
        public abstract bool IsLongWildCard();
        public abstract bool IsShortWildCard();

        static public SubQuery subquery;
        public abstract bool IsLazy();

        public abstract float UnifyLazy(Unifiable other);

        public virtual float Unify(Unifiable other, SubQuery query)
        {
            if (IsShortWildCard()) if (other.AsString().Contains(" ")) return UNIFY_FALSE;
            subquery = query;
            if (IsWildCard())
            {
                if (IsLazy())
                {
                    try
                    {
                        return UnifyLazy(other);
                    }
                    catch (Exception e)
                    {
                        //RTPBot.writeDebugLine(""+e);
                        return UNIFY_FALSE;
                    }
                }
                return 0;
            }
            if (false && other.IsWildCard())
            {
                // return unifiable.Unify(this, query);
            }
            if (other.AsString().ToUpper() == AsString().ToUpper()) return UNIFY_TRUE;
            if (IsMatch(other))
            {
                //return UNIFY_FALSE;
                return UNIFY_TRUE;
            }
            return UNIFY_FALSE;
        }

        public virtual Unifiable ToCaseInsenitive()
        {
            return this;
        }
        public virtual Unifiable Frozen()
        {
            return ToValue();
        }
        public abstract string ToValue();
        public abstract string AsString();
        public virtual Unifiable ToPropper()
        {
            return this;
        }
        public virtual Unifiable Trim()
        {
            return this;
        }

        //public abstract Unifiable[] Split(Unifiable[] unifiables, StringSplitOptions options);
        //public abstract Unifiable[] Split();


        // join functions
        public abstract void Append(Unifiable part);
        public abstract void Clear();

        public abstract Unifiable First();

        public abstract Unifiable Rest();

        public abstract bool IsShort();

        public abstract object AsNodeXML();

        public static UPath MakePath(Unifiable unifiable)
        {
            return unifiable;
        }

        public abstract Unifiable[] ToArray();

        virtual public bool StoreWildCard()
        {
            return true;
        }
    }
}

