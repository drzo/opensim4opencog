using System;
using System.Collections.Generic;
using System.Globalization;
using System.Text.RegularExpressions;
using System.Xml;
using RTParser.Database;
using RTParser.Utils;
using UPath = RTParser.Unifiable;

namespace RTParser
{
    abstract public class Unifiable
    {

        public UFlags Flags = UFlags.NO_FLAGS;
        public bool IsFlag(UFlags f)
        {
            return (f & Flags) != 0;
        }

        [Flags]
        public enum UFlags : uint 
        {
            NO_FLAGS = 0,
            IS_TRUE = 1,
            IS_FALSE = 2,
            IS_NULL = 4,
            IS_EMPTY = 8,
            IS_EXACT = 16,

            BINDS_STARS = 32,
            LONG_WILDCARD = 64,
            SHORT_WILDCARD = 128,
            LAZY_XML = 256,
            REG_CLASS = 512,
            ONLY_ONE = 1024,
            ONE_OR_TWO = 2048,
            MORE_THAN_ONE =4096,
            IS_TAG = 8192,
            IS_PUNCT = 16384,
            APPENDABLE = 32768,
            NO_BINDS_STARS = 65536,
            ZERO_OR_MORE = 131072,
        }

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
            return InnerXmlText0(templateNode).Trim();
        }

        static string InnerXmlText0(XmlNode templateNode)
        {
            if (templateNode == null) return "-NULLXML-";
            switch (templateNode.NodeType)
            {
                case XmlNodeType.None:
                    break;
                case XmlNodeType.Element:
                    if (false && templateNode.InnerText != templateNode.InnerXml)
                    {
                        string ss = "";
                        foreach (XmlNode childNode in templateNode.ChildNodes)
                        {
                            ss = ss + " " + Unifiable.InnerXmlText(childNode);
                        }
                      //  return ss;
                    }
                    return templateNode.InnerXml;
                    break;
                case XmlNodeType.Attribute:
                    break;
                case XmlNodeType.Text:
                    if (templateNode.InnerXml.Length > 0)
                    {
                        return templateNode.InnerText + templateNode.InnerXml;
                    }
                    return templateNode.InnerText;

                case XmlNodeType.CDATA:
                    break;
                case XmlNodeType.EntityReference:
                    break;
                case XmlNodeType.Entity:
                    {
                        string ss = "";
                        foreach (XmlNode childNode in templateNode.ChildNodes)
                        {
                            ss = ss + " " + Unifiable.InnerXmlText(childNode);
                        }
                        return ss;
                    }
                    break;
                case XmlNodeType.ProcessingInstruction:
                    break;
                case XmlNodeType.Comment:
                    break;
                case XmlNodeType.Document:
                    break;
                case XmlNodeType.DocumentType:
                    break;
                case XmlNodeType.DocumentFragment:
                    break;
                case XmlNodeType.Notation:
                    break;
                case XmlNodeType.Whitespace:
                    break;
                case XmlNodeType.SignificantWhitespace:
                    break;
                case XmlNodeType.EndElement:
                    break;
                case XmlNodeType.EndEntity:
                    break;
                case XmlNodeType.XmlDeclaration:
                    break;
                default:
                    throw new ArgumentOutOfRangeException();
            }
            string s = String.Format("Unsurported Node Type {0} in {1}", templateNode.NodeType, templateNode.OuterXml);
            writeToLog(s);
            throw new ArgumentOutOfRangeException(s);
        }

                
        public static implicit operator string(Unifiable value)
        {
            if (ReferenceEquals(value, null))
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

        private static StringUnifiable MakeStringUnfiable(string value)
        {
            if (value == null) return null;
            Unifiable u;
            if (true)
                lock (internedUnifiables)
                {
                    if (internedUnifiables.TryGetValue(value, out u))
                    {
                        return (StringUnifiable)u;
                    }
                }

            string key = AIMLLoader.CleanWhitepaces(value);
            if (value != key)
            {
             //   writeToLog("Triming? '" + value + "'");
                value = key;
                //return new StringUnifiable(value);
            }
            if (true)
                lock (internedUnifiables)
                {
                    if (!internedUnifiables.TryGetValue(key, out u))
                    {
                        u = internedUnifiables[key] = new StringUnifiable(value, true);
                    }
                    return (StringUnifiable)u;
                }
            u = new StringUnifiable(value);
            return (StringUnifiable)u;
        }

        public static Unifiable Empty = new EmptyUnifiable();


        public static Unifiable STAR
        {
            get
            {
                return MakeStringUnfiable("*");
            }
        }


        public string ToMatchPattern()
        {
            return ToUpper();
        }

        static public bool IsTrue(Unifiable v)
        {
            if (!IsFalse(v))
            {
                if (!IsNullOrEmpty(v)) return true;
                return false;
            }
            return true;
        }

        static public bool IsLogicTF(Unifiable v, SubQuery subquery)
        {
            if (IsFalse(v)) return false;
            String value = v.ToValue(subquery).ToLower();
            if (value.Length == 0) return false;
            char c = value[0];
            if (c == 'n' || c == 'f') return false;
            return true;           
        }


        public static Unifiable Join(string sep, Unifiable[] values, int startIndex, int count)
        {
            if (count == 1)
            {
                return values[startIndex];
            }
            return String.Join(sep, FromArrayOf(values), startIndex, count);
        }

        public static Unifiable Join(string sep, string[] values, int startIndex, int count)
        {
            if (count == 1)
            {
                return values[startIndex];
            }
            return String.Join(sep, values, startIndex, count);
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

            if (t.ToUpper() == s.ToUpper()) return true;
            return false;
            if (t.ToValue(null).ToLower() == s.ToValue(null).ToLower())
            {
                writeToLog("==({0},{1})", t.AsString(), s.AsString());
                return false;
            }

            return false;
        }

        public static bool operator !=(Unifiable t, Unifiable s)
        {
            return !(t == s);
        }

        public static bool IsFalse(Unifiable tf)
        {
            if (ReferenceEquals(tf, null)) return true;
            if (ReferenceEquals(tf.Raw, null)) return true;
            return tf.IsFalse();
        }

        public static bool IsNullOrEmpty(Object name)
        {
            if (name is String)
            {
                return ((String) name).Trim().Length == 0;
            }
            if (IsNull(name)) return true;
            return (name is Unifiable && ((Unifiable)name).IsEmpty);
        }
        public static bool IsNull(Object name)
        {
            if (ReferenceEquals(name, null)) return true;
            return (name is Unifiable && ((Unifiable)name).Raw == null);
        }

        public static bool IsEMPTY(Object name)
        {
            if (name is String)
            {
                return ((String)name).Trim().Length == 0;
            }
            return (name is Unifiable && ((Unifiable)name).Raw == null);
        }

        public static Unifiable operator +(string u, Unifiable more)
        {
            if (u.Length == 0) return more;
            if (more == null) return u + " -NULL-";
            string moreAsString = more.AsString();
            if (moreAsString.Length == 0) return u;
            return MakeStringUnfiable(u + more.AsString());
        }
        public static Unifiable operator +(Unifiable u, string more)
        {
            return MakeStringUnfiable("" + u.AsString() + more);
        }
        public static Unifiable operator +(Unifiable u, Unifiable more)
        {
            string moreAsString = more.AsString();
            if (moreAsString.Length == 0) return u;
            return MakeStringUnfiable(u.AsString() + " " + moreAsString);
        }

        public static Unifiable Create(object p)
        {
            if (p is string)
            {
                return MakeStringUnfiable((string)p);
            }
            if (p is Unifiable) return (Unifiable) p;
            if (p is XmlNode)
            {
                var n = (XmlNode) p;
                string inner = InnerXmlText(n);
                writeToLog("MAking XML Node " + n.OuterXml + " -> " + inner);
                StringUnifiable unifiable = MakeStringUnfiable(inner);
                //unifiable.node = (XmlNode)p;
            }
            // TODO
            if (p == null)
            {
                return null;
            }
            return MakeStringUnfiable(p.ToString());
        }

        public override string ToString()
        {
            writeToLog("ToSTring");
            return  GetType().Name + "={" + Raw + "}";//Raw.ToString();}
        }

        internal static StringAppendableUnifiable CreateAppendable()
        {
            var u = new StringAppendableUnifiable();
            return u;
        }

        public static Unifiable ThatTag = Create("TAG-THAT");
        public static Unifiable TopicTag = Create("TAG-TOPIC");
        public static Unifiable FlagTag = Create("TAG-FLAG");
        public static Unifiable InputTag = Create("TAG-INPUT");
        public static Unifiable TagStartText = Create("TAG-START");
        public static Unifiable TagEndText = Create("TAG-END");

        public abstract object Raw { get; }
        public virtual bool IsEmpty
        {
            get
            {
                string s = AsString();
                if (String.IsNullOrEmpty(s)) return true;
                s = s.Trim();
                if (s.Length != 0)
                {
                   // writeToLog("was IsEmpty");
                    return false;
                }
                return true;
            }
        }

        public Unifiable LegacyPath
        {
            get { return this; }
        }

        public virtual bool IsUnitMatcher
        {
            get { return IsShort(); }
        }

        public virtual bool IsStarMatcher
        {
            get { return !IsUnitMatcher && IsWildCard(); }
        }

        public bool CanMatchZero
        {
            get { return Raw != null && ToUpper().Length == 0; }
        }

        protected virtual bool IsFalse()
        {
            return IsEmpty;            
        }
        public abstract bool IsTag(string s);
        public virtual bool IsWildCard()
        {
            return true;
        }
        public abstract bool IsLazyStar();
        public abstract bool IsLongWildCard();
        public abstract bool IsFiniteWildCard();

        public abstract bool IsLazy();
        public abstract bool IsLitteral();
        public abstract bool IsLitteralText();

        public static void writeToLog(string message, params object[] args)
        {
            try
            {
                RTPBot.writeDebugLine("UNIFYABLETRACE: " + message, args);
            }
            catch
            {
            }
        }

        public abstract bool ConsumePath(int at, string[] tokens,
                                         out string fw, out Unifiable right,
                                         out int newAt, SubQuery query);

        public bool WillUnify(Unifiable other, SubQuery query)
        {
            string su = ToUpper();
            if (su == "*") return !other.IsEmpty;
            if (su == "_") return other.IsShort();
            return Unify(other, query) == UNIFY_TRUE;
        }

        public bool CanUnify(Unifiable other, SubQuery query)
        {
            string su = ToUpper();
            if (su == "*") return !other.IsEmpty;
            if (su == "_") return other.IsShort();
            return Unify(other, query) == UNIFY_TRUE;
        }

        public abstract float Unify(Unifiable unifiable, SubQuery query);

        public static bool IsStringMatch(string s, string u)
        {
            if (s == u) return true;
            if (s.ToUpper().Trim() == u.ToUpper().Trim())
            {
                return true;
            }
            if (MustBeFast) return false;
            return IsMatch2(s, u);
        }

        static public bool IsMatch2(string st, string actualValue)
        {
            if (ReferenceEquals(st, actualValue)) return true;
            string that = " " + actualValue + " ";
            string thiz = " " + st + " ";
            if (thiz == that)
            {
                return true;
            }
            if (thiz.ToLower() == that.ToLower())
            {
                return true;
            }
            if (TwoMatch0(that, thiz))
            {
                return true;
            }
            string a1 = st;
            string a2 = actualValue;
            thiz = " " + a1 + " ";
            that = " " + a2 + " ";
            if (TwoMatch0(that, thiz))
            {
                return true;
            }
            if (TwoSemMatch(a1, a2))
            {
                return true;
            }
            if (st.StartsWith("~"))
            {
                string type = st.Substring(1);
                var b = NatLangDb.IsWordClass(actualValue, type);
                if (b)
                {
                    return true;
                }
                return b;
            }
            return false;
        }


        static bool TwoSemMatch(string that, string thiz)
        {
            return false;
            if (NatLangDb.IsWordClassCont(that, " determ") && NatLangDb.IsWordClassCont(thiz, " determ"))
            {
                return true;
            }
            return false;
        }

        public static bool MustBeFast = true;
        static bool TwoMatch0(string s1, string s2)
        {
            if (s1 == s2) return true;
            if (MustBeFast) return false;
            Regex matcher = new Regex(s1.Replace(" ", "\\s").Replace("*", "[\\sA-Z0-9]+"), RegexOptions.IgnoreCase);
            bool b = matcher.IsMatch(s2);
            if (b)
            {
                return true;
            }
            return b;
        }


        public static Unifiable NULL = new StringUnifiable(null);

        public static Unifiable FAIL_NIL = new StringUnifiable("NIL");

        public virtual Unifiable ToCaseInsenitive()
        {
            return this;
        }
        public virtual Unifiable Frozen(SubQuery subquery)
        {
            return ToValue(subquery);
        }
        public abstract string ToValue(SubQuery subquery);
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
        public abstract void Append(string part);
        public abstract void Clear();

        public abstract Unifiable First();

        public abstract Unifiable Rest();

        public abstract bool IsShort();

        public abstract object AsNodeXML();

        public static Unifiable MakePath(Unifiable unifiable)
        {
            return unifiable;
        }

        public abstract Unifiable[] ToArray();

        virtual public bool StoreWildCard()
        {
            return true;
        }

        public static string ToVMString(object unifiable)
        {
            if (unifiable is Unifiable)
            {
                return ((Unifiable) unifiable).AsString();
            }
            if (ReferenceEquals(null,unifiable))
            {
                return "-NULL-";
            }
            return "" + unifiable;
        }

        public abstract bool IsAnySingleUnit();

        public abstract string ToUpper();

        public static bool IsUnknown(object unifiable)
        {
            if (IsNullOrEmpty(unifiable)) return true;
            string ss = AIMLLoader.CleanWhitepacesLower(unifiable.ToString());
            string s = " " + ss.Replace("_", " ").Replace("-", " ") + " ";
            bool b = s.Contains("unknown") || s.Contains("unrec") || s.Contains("unnam")
                     || s.Contains("unseen") || s.Contains("default")
                     || s.Contains(" some") || s.Contains("*") || s.Contains(" _ ")
                     || s.Contains(" nothing ") || s.Contains("undefined");
            if (b) return true;
            if (unifiable is Unifiable)
            {
                if (!((Unifiable) unifiable).IsWildCard()) return false;
                return true;
            }
            return false;
            //switch (s)
            //{
            //    case "":
            //        return true;
            //    case "unknown":
            //        return true;
            //    case "nothing":
            //        return true;
            //    case "*":
            //        return true;
            //    case "_":
            //        return true;
            //    case "undefined":
            //        return true;
            //    default:
            //        return false;
            //}
        }

        public bool IsAnyWord()
        {
            return IsUnitMatcher;
        }
    }
}

