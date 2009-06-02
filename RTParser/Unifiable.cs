using System;
using System.Collections.Generic;
using System.Globalization;
using System.Text.RegularExpressions;
using System.Xml;
using RTParser.AIMLTagHandlers;
using RTParser.Utils;

namespace RTParser
{

    public class Unifiable
    {
        public static Unifiable InnerXmlText(XmlNode templateNode)
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

                                            
        public static Unifiable STAR
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
            Unifiable u = new Unifiable(value);
            if (u.IsWildCard())
            {
                
            }
            return u;
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


        //public static bool operator !=(Unifiable t, string s)
        //{
        //    return !(t == s);
        //}


        //static public bool operator ==(string s, Unifiable t)
        //{
        //    return t.AsString().ToUpper() == s.ToUpper();
        //}

        //public static bool operator !=(string s, Unifiable t)
        //{
        //    return !(s == t);
        //}

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

        public Unifiable()
        {
            str = "";
        }

        private Unifiable(string value)
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

        public static Unifiable ThatTag = Create("TAG-THAT");
        public static Unifiable TopicTag = Create("TAG-TOPIC");


        //public Unifiable Replace(object marker, object param1)
        //{
        //    return str.Replace(astr(marker), astr(param1));
        //}

        public static string astr(object param1)
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

        //public virtual Unifiable ToLower()
        //{
        //    return str.ToLower();
        //}

        public Unifiable ToUpper()
        {
            return Create(str.ToUpper());
        }

        //public Unifiable Substring(int i, int ii)
        //{
        //    return str.Substring(i, ii);
        //}

        public virtual char[] ToCharArray()
        {
            return str.ToCharArray();
        }

        //public bool EndsWith(string s)
        //{
        //    return str.EndsWith(s);
        //}

        //public bool StartsWith(string s)
        //{
        //    return str.StartsWith(s);
        //}

        //public Unifiable[] Split(char[] c, StringSplitOptions options)
        //{
        //    return arrayOf(str.Split(c, options));
        //}


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

        //public virtual Unifiable Substring(int p)
        //{
        //    return str.Substring(p);
        //}

        //public virtual int IndexOf(string p)
        //{
        //    return str.IndexOf(p);
        //}


        public virtual Unifiable[] Split(Unifiable[] tokens, StringSplitOptions stringSplitOptions)
        {
            return arrayOf(str.Split(FromArrayOf(tokens), stringSplitOptions));
        }


        //public virtual Unifiable[] Split(char[] p)
        //{
        //    return arrayOf(str.Split(p));
        //}

        //public bool Contains(string p)
        //{
        //    return str.Contains(p);
        //}

        //public virtual string ToLower(CultureInfo cultureInfo)
        //{
        //    return str.ToLower(cultureInfo);
        //}

        //public virtual string ToUpper(CultureInfo cultureInfo)
        //{
        //    return str.ToUpper(cultureInfo);
        //}

        //public virtual Unifiable TrimEnd()
        //{
        //    return str.TrimEnd();
        //}

        //public Unifiable TrimStart()
        //{
        //    return str.TrimStart();
        //}

        static public bool IsTrue(Unifiable v)
        {
            return !IsFalse(v);
        }

        public virtual bool IsWildCard()
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
            string strTrim = str.Trim();
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
                Console.WriteLine("" + e);
            }
            return arrayOf(strTrim.Split(BRKCHARS, StringSplitOptions.RemoveEmptyEntries));
        }

        public bool IsTag(string that)
        {
            return str == that;
        }

        public static bool IsFalse(Unifiable tf)
        {
            if (Object.ReferenceEquals(tf, null)) return true;
            if (String.IsNullOrEmpty(tf.str)) return true;
            string found = tf.AsString().Trim().ToUpper();
            return found == "" || found == "NIL" || found == "()" || found == "FALSE";
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

        public static Unifiable CreateFromObject(XmlNode pattern)
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

        //public virtual void Remove(int p, int c)
        //{
        //    str = str.Remove(p, c);
        //}

        public virtual Unifiable Frozen()
        {
            return Create(str);
        }


        public virtual Unifiable ToPropper()
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

        //public virtual Unifiable Rest()
        //{
        //    Unifiable[] splitted = Splitter(str);
        //    return Join(" ", splitted, 1, splitted.Length - 1);
        //    if (String.IsNullOrEmpty(this.str)) return Unifiable.Empty;
        //    int i = str.IndexOfAny(BRKCHARS);
        //    if (i == -1) return Empty;
        //    string rest = str.Substring(i + 1);
        //    return Create(rest.Trim());
        //}

        readonly static char[] BRKCHARS = " \r\n\t".ToCharArray();

        //public virtual Unifiable First()
        //{
        //    if (String.IsNullOrEmpty(str)) return Unifiable.Empty;
        //    //int i = str.IndexOfAny(BRKCHARS);
        //    //if (i == -1) return Create(str);
        //    return Split()[0];
        //    //string rest = str.Substring(0, i - 1);
        //    //return Create(rest.Trim());
        //}

        public virtual bool IsShortWildCard()
        {
            if (str == "_") return true;
            // if (this.IsMarkerTag()) return false; // tested by the next line
            if (IsLazy()) return true;
            return false;
        }

        public virtual bool IsLongWildCard()
        {
            if (str == ("*")) return true;
            if (this.IsMarkerTag()) return false;
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
        public virtual bool Unify(Unifiable unifiable, SubQuery query)
        {
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
                        Console.WriteLine(""+e);
                        return false;
                    }
                }
                return true;
            }
            if (unifiable.IsWildCard())
            {
                return unifiable.Unify(this, query);
            }
            return unifiable.str.ToUpper() == str.ToUpper();
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

        public virtual bool IsEmpty
        {
            get { return string.IsNullOrEmpty(str); }
        }

        public virtual void Clear()
        {
            str = "";
        }

        public string ToValue()
        {
            if (IsLazy())
            {
                
            }
            return AsString();
        }

        public virtual bool IsMatch(Unifiable actualValue)
        {
            return TwoMatch(actualValue.AsString(), this.AsString()) || TwoMatch(actualValue.ToValue(), this.ToValue());
        }

        static bool TwoMatch(string s1, string s2)
        {
            Regex matcher = new Regex(s1.Replace(" ", "\\s").Replace("*", "[\\sA-Z0-9]+"), RegexOptions.IgnoreCase);
            return matcher.IsMatch(s2);
        }

        public bool IsStar()
        {
            if (str == "_") return true;
            if (str == "*") return true;
            if (IsLazyStar())
            {
                return true;
            }
            //if (GetTagHandler() is star) return true;
            return false;
        }

        public virtual bool IsLazyStar()
        {
            if (!IsLazy()) return false;
            return GetTagHandler() is star;
        }
    }
}

