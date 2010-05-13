using System;
using System.Collections.Generic;
using System.Text.RegularExpressions;
using System.Xml;
using java.io;
using RTParser.AIMLTagHandlers;
using RTParser.Utils;
using Console=System.Console;

namespace RTParser
{
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

        public override object AsNodeXML()
        {
            return str;
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

        public override object Raw
        {
            get { return str; }
        }

        protected override bool IsFalse()
        {
            if (String.IsNullOrEmpty(str)) return true;
            string found = str.Trim().ToUpper();
            return found == "" || found == "NIL" || found == "()" || found == "FALSE" || found == "NO" || found == "OFF";
        }

        public override bool IsWildCard()
        {
            if (IsMarkerTag()) return false;
            return (str.Contains("*") || str.Contains("_") || str.Contains("<"));
        }

        public Unifiable[] ToArray()
        {
            return Splitter(str); 
        }

        static Unifiable[] Splitter(string str)
        {
            string strTrim = str.Trim().Replace("  "," ").Replace("  "," ");
            if (!strTrim.Contains("<"))
                return arrayOf(strTrim.Split(BRKCHARS, StringSplitOptions.RemoveEmptyEntries));
            XmlDocument doc = new XmlDocument();
            List<Unifiable> u = new List<Unifiable>();

            try
            {
                doc.LoadXml("<node>" + strTrim + "</node>");
                foreach (XmlNode node in doc.FirstChild.ChildNodes)
                {
                    if (node.NodeType == XmlNodeType.Comment) continue;
                    if (node.NodeType == XmlNodeType.Whitespace) continue;
                    if (node.NodeType == XmlNodeType.Text)
                    {
                        string splitMe = node.Value.Trim();
                        u.AddRange(Splitter(splitMe));
                    }
                    else if (node.NodeType == XmlNodeType.Element)
                    {
                        string splitMe = node.OuterXml.Trim();
                        u.Add(splitMe);
                    }
                    else
                    {
                        string splitMe = node.OuterXml.Trim();
                        u.Add(splitMe);
                    }
                }
                return u.ToArray();
            }
            catch (Exception e)
            {
                Console.WriteLine("" + e.Message + ": " +strTrim);
            }
            return arrayOf(strTrim.Split(BRKCHARS, StringSplitOptions.RemoveEmptyEntries));
        }

        public override bool IsTag(string that)
        {
            return str == "TAG-" + that || str.StartsWith("<" + that.ToLower());
        }

        public override void Append(Unifiable p)
        {
            throw new InvalidObjectException("this " + AsString() + " cannot be appended with " + p);

            if (!IsAppendable)
            {
                throw new InvalidObjectException("this " + AsString() + " cannot be appended with " + p);
            }
            if (p==null) return;
            if (str == "")
                str = p.AsString().Trim();
            else
            {
                str += " ";
                str += p.AsString().Trim();
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
            return ToArray()[0];
            //string rest = str.Substring(0, i - 1);
            //return Create(rest.Trim());
        }

        public override bool IsShort()
        {
            if (str == "_") return true;
            // if (this.IsMarkerTag()) return false; // tested by the next line
            if (IsLazyStar()) return false;
            if (IsLazy()) return true;
            return false;
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

        public override bool IsLazy()
        {
            if (this.IsMarkerTag()) return false;
            return str.StartsWith("<");
        }

        public virtual bool IsMarkerTag()
        {
            string test = str.Trim().ToUpper();
            return test.StartsWith("TAG-");
        }



        public override float UnifyLazy(Unifiable unifiable)
        {
            AIMLTagHandler tagHandler = GetTagHandler();
            if (tagHandler.CanUnify(unifiable) == UNIFY_TRUE) return UNIFY_TRUE;
            Unifiable outputSentence = tagHandler.CompleteProcess();///bot.GetTagHandler(templateNode, subquery, request, result, request.user);
            string value = outputSentence.AsString();
            string mustBe = unifiable.ToValue();
            return mustBe.ToUpper() == value.ToUpper() ? UNIFY_TRUE : UNIFY_FALSE;
        }

        public virtual AIMLTagHandler GetTagHandler()
        {
            XmlNode node = GetNode();
            // if (node.ChildNodes.Count == 0) ;
            Result result = subquery.Result;
            Request request = result.request;
            User user = result.user;
            RTPBot bot = request.Proccessor;
            return bot.GetTagHandler(user, subquery, request, result, node, null);
        }

        public virtual XmlNode GetNode()
        {
            return AIMLTagHandler.getNode(str);
        }

        public override bool IsEmpty
        {
            get { return string.IsNullOrEmpty(str); }
        }

        private bool ppendable;

        public bool IsAppendable
        {
            get { return ppendable; }
            set { ppendable=value; }
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
                Console.WriteLine("TODO " + str);
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
}