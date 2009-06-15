using System;
using System.Collections.Generic;
using System.Globalization;
using System.Text.RegularExpressions;
using System.Xml;
using RTParser.AIMLTagHandlers;
using RTParser.Utils;

namespace RTParser
{
    abstract public class Unifiable
    {

        /// <summary>
        /// This should be overridden!
        /// </summary>
        /// <param name="obj"></param>
        /// <returns></returns>
        public new abstract bool Equals(object obj);
        //{
        //    throw new NotImplementedException();
        //}

        /// <summary>
        /// This should be overridden!
        /// </summary>
        /// <returns></returns>
        public new abstract int GetHashCode();

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

        public static implicit operator Unifiable(string value)
        {
            if (value == null) return null;
            Unifiable u = new StringUnifiable(value);
            if (u.IsWildCard())
            {
                
            }
            return u;
        }
        public static Unifiable Empty = new EmptyUnifiable();


        public static Unifiable STAR
        {
            get
            {
                return new StringUnifiable("*");
            }
        }

        static public bool IsTrue(Unifiable v)
        {
            return !IsFalse(v);
        }


        public static Unifiable Join(string p, Unifiable[] fsp, int p_3, int p_4)
        {
            return string.Join(p, FromArrayOf(fsp), p_3, p_4);
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

            return t.AsString().ToLower() == s.AsString().ToLower() || t.ToValue().ToLower() == s.ToValue().ToLower();
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
            if (p is Unifiable) return (Unifiable) p;
            if (p is string) return new StringUnifiable((string) p);
            // TODO
            if (p is XmlNode) return new StringUnifiable(InnerXmlText((XmlNode) p));
            return new StringUnifiable(p.ToString());
        }

        internal static Unifiable CreateAppendable()
        {
            return new StringUnifiable();
        }

        public static Unifiable ThatTag = Create("TAG-THAT");
        public static Unifiable TopicTag = Create("TAG-TOPIC");

        protected abstract object Raw { get; }
        public virtual bool IsEmpty
        {
            get
            {
                return string.IsNullOrEmpty(ToValue());
            }
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
        public abstract bool Unify(Unifiable unifiable, SubQuery query);

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

    }

    public class StringUnifiable : Unifiable
    {

        public string _str;
        protected string str
        {
            get
            {
                return _str;
            }
             
            set
            {
                _str = value;
            }
        }

        public StringUnifiable()
        {
            str = "";
        }

        public StringUnifiable(string value)
        {
            str = value;
        }

        //public int Length
        //{
        //    get
        //    {
        //        if (str == null)
        //        {
        //            return 0;
        //        }
        //        return str.Length;
        //    }
        //}




        //public Unifiable Replace(object marker, object param1)
        //{
        //    return str.Replace(astr(marker), astr(param1));
        //}

        public static string astr(object param1)
        {
            return "" + param1;
        }



        public override Unifiable Trim()
        {
            string str2 = str.Trim().Replace("  "," ").Replace("  "," ");
            if (str2==str) return this;
            return str.Trim();
        }

        public override string AsString()
        {
            return str;
        }

        public override Unifiable ToCaseInsenitive()
        {
            return Create(str.ToUpper());
        }

        public virtual char[] ToCharArray()
        {
            return str.ToCharArray();
        }

        public override bool Equals(object obj)
        {
            if (obj is Unifiable) return ((Unifiable)obj) == this;
            return str == astr(obj);
        }

        public override string ToString()
        {
            return str.ToString();
        }

        public override int GetHashCode()
        {
            if (IsWildCard()) return -1;
            return str.GetHashCode();
        }

        //public override Unifiable[] Split(Unifiable[] tokens, StringSplitOptions stringSplitOptions)
        //{
        //    return arrayOf(str.Split(FromArrayOf(tokens), stringSplitOptions));
        //}

        protected override object Raw
        {
            get { return str; }
        }

        protected override bool IsFalse()
        {
            if (String.IsNullOrEmpty(str)) return true;
            string found = str.Trim().ToUpper();
            return found == "" || found == "NIL" || found == "()" || found == "FALSE";
        }

        public override bool IsWildCard()
        {
            if (IsMarkerTag()) return false;
            return (str.Contains("*") || str.Contains("_") || str.Contains("<"));
        }

        public Unifiable[] Split()
        {
            return Splitter(str); 
        }

        static Unifiable[] Splitter(string str)
        {
            string strTrim = str.Trim().Replace("  "," ").Replace("  "," ");
            if (!strTrim.Contains("<"))
                return arrayOf(strTrim.Split(BRKCHARS, StringSplitOptions.RemoveEmptyEntries));
            XmlDocument doc = new XmlDocument();
            List<Unifiable> list = new List<Unifiable>();

            try
            {
                doc.LoadXml("<node>" + strTrim + "</node>");
                foreach (XmlNode node in doc.FirstChild.ChildNodes)
                {
                    if (node.NodeType == XmlNodeType.Whitespace) continue;
                    if (node.NodeType == XmlNodeType.Text)
                    {
                        string splitMe = node.OuterXml.Trim();
                        list.AddRange(Splitter(splitMe));
                    }
                    else
                    {
                        string splitMe = node.OuterXml.Trim();
                        list.Add(splitMe);
                    }
                }
                return list.ToArray();
            }
            catch (Exception e)
            {
                Console.WriteLine("" + e.Message + ": " +strTrim);
            }
            return arrayOf(strTrim.Split(BRKCHARS, StringSplitOptions.RemoveEmptyEntries));
        }

        public override bool IsTag(string that)
        {
            return str == "TAG-" + that;
        }

        public override void Append(Unifiable p)
        {
            if (p==null) return;
            if (str == "")
                str = p.AsString();
            else
            {
                str += " ";
                str += p.AsString();
            }
        }

        public override Unifiable Frozen()
        {
            return Create(str);
        }

        public override Unifiable ToPropper()
        {
            int len = str.Length;

            if (len == 0) return this;
            string newWord = str.Substring(0, 1).ToUpper();
            if (len == 1)
            {
                if (newWord == str) return this;
            }
            newWord += str.Substring(1).ToLower();
            return newWord;
        }

        public override Unifiable Rest()
        {
            Unifiable[] splitted = Splitter(str);
            return Join(" ", splitted, 1, splitted.Length - 1);
            if (String.IsNullOrEmpty(this.str)) return Unifiable.Empty;
            int i = str.IndexOfAny(BRKCHARS);
            if (i == -1) return Empty;
            string rest = str.Substring(i + 1);
            return Create(rest.Trim());
        }

        readonly static char[] BRKCHARS = " \r\n\t".ToCharArray();

        public override Unifiable First()
        {
            if (String.IsNullOrEmpty(str)) return Unifiable.Empty;
            //int i = str.IndexOfAny(BRKCHARS);
            //if (i == -1) return Create(str);
            return Split()[0];
            //string rest = str.Substring(0, i - 1);
            //return Create(rest.Trim());
        }

        public override bool IsShortWildCard()
        {
            if (str == "_") return true;
            // if (this.IsMarkerTag()) return false; // tested by the next line
            if (IsLazyStar()) return false;
            if (IsLazy()) return true;
            return false;
        }

        public override bool IsLongWildCard()
        {
            if (str == ("*")) return true;
            if (this.IsMarkerTag()) return false;
            if (IsLazyStar()) return true;
            return false;
        }

        public virtual bool IsLazy()
        {
            if (this.IsMarkerTag()) return false;
            return str.StartsWith("<");
        }

        public virtual bool IsMarkerTag()
        {
            string test = str.Trim().ToUpper();
            return test.StartsWith("TAG-");
        }

        static public SubQuery subquery;
        public override bool Unify(Unifiable unifiable, SubQuery query)
        {
            if (IsShortWildCard()) if (unifiable.AsString().Contains(" ")) return false;
            subquery = query;
            if (IsWildCard())
            {
                if (IsLazy())
                {
                    try
                    {
                        return UnifyLazy(unifiable);
                    } catch(Exception e)
                    {
                        //Console.WriteLine(""+e);
                        return false;
                    }
                }
                return true;
            }
            if (unifiable.IsWildCard())
            {
                // return unifiable.Unify(this, query);
            }
            return unifiable.AsString().ToUpper() == str.ToUpper();
        }

        public virtual bool UnifyLazy(Unifiable unifiable)
        {
            AIMLTagHandler tagHandler = GetTagHandler();
            if (tagHandler.CanUnify(unifiable)) return true;
            Unifiable outputSentence = tagHandler.CompleteProcess();///bot.GetTagHandler(templateNode, subquery, request, result, request.user);
            string value = outputSentence.AsString();
            string mustBe = unifiable.ToValue();
            return mustBe.ToUpper() == value.ToUpper();
        }

        public virtual AIMLTagHandler GetTagHandler()
        {
            XmlNode node = GetNode();
            // if (node.ChildNodes.Count == 0) ;
            Result result = subquery.Result;
            Request request = result.request;
            User user = result.user;
            RTPBot bot = request.Proccessor;
            return bot.GetTagHandler(user, subquery, request, result, node);
        }

        public virtual XmlNode GetNode()
        {
            return AIMLTagHandler.getNode(str);
        }

        public override bool IsEmpty
        {
            get { return string.IsNullOrEmpty(str); }
        }

        public override void Clear()
        {
            str = "";
        }

        public override string ToValue()
        {
            if (IsLazy())
            {
                //todo 
                Console.WriteLine("TODO " + ToString());
            }
            return AsString();
        }

        public override bool IsMatch(Unifiable actualValue)
        {
            return TwoMatch(actualValue.AsString(), this.AsString()) || TwoMatch(actualValue.ToValue(), this.ToValue());
        }

        static bool TwoMatch(string s1, string s2)
        {
            Regex matcher = new Regex(s1.Replace(" ", "\\s").Replace("*", "[\\sA-Z0-9]+"), RegexOptions.IgnoreCase);
            return matcher.IsMatch(s2);
        }

        public override bool IsLazyStar()
        {
            if (!IsLazy()) return false;
            return GetTagHandler() is star;
        }
    }
    
    internal class EmptyUnifiable : StringUnifiable
    {
        public EmptyUnifiable()
            : base()
        {

        }

        public override bool IsEmpty
        {
            get
            {
                return true;
            }
        }

        public override void Append(Unifiable p)
        {
            throw new InvalidCastException("Empty Unifiable");
            if (str == "")
                str = p.AsString();
            else
            {
                str += " ";
                str += p.AsString();
            }
        }
    }
}

