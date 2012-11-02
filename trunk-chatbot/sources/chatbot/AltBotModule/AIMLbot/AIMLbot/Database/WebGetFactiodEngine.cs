using System;
using System.Collections;
using System.Collections.Generic;
using System.Xml;
using AltAIMLParser;
using MushDLR223.ScriptEngines;
using RTParser.Utils;
using MushDLR223.Utilities;

namespace RTParser.Database
{
    abstract public class WebGetFactiodEngine : IEnglishFactiodEngine, IDocSearch
    {
        public readonly static ICollection<ISearchResult> NO_RESULTS = new ISearchResult[0];

        /// <summary>
        /// ASK.COM was 
        ///   return "q_result,query_result,result,q_answer";
        /// </summary>
        /// <returns></returns>
        public abstract string GetResultTags();

        /// <summary>
        ///  return @"http://www.ask.com/web?q=" + searchTerm1;
        /// </summary>
        /// <param name="searchTerm1"></param>
        /// <returns></returns>
        protected abstract string MakeSearchString(string searchTerm1);

        public virtual object AskTextString(string textstr, Request request)
        {
            string said;
            string tolang;
            if (!TextPatternUtils.SplitOff(textstr, "-", out tolang, out said))
            {
                tolang = "";
                said = textstr;
            }
            string res = tolang;
            var allResults = new List<ISearchResult>();
            foreach (ISearchResult re in Search(said, null))
            {
                res += " " + re.ToString();
                allResults.Add(re);
            }
            if (allResults.Count == 0) return tolang;// +"\n";
            return res.Trim();
        }

        private readonly AltBot TheBot;
        private readonly IEnglishFactiodEngine assertTo;

        public WebGetFactiodEngine(IEnglishFactiodEngine fallback, AltBot AltBot)
        {
            TheBot = AltBot;
            assertTo = fallback;
// ReSharper disable DoNotCallOverridableMethodsInConstructor
            string named = GetServiceName();
// ReSharper restore DoNotCallOverridableMethodsInConstructor
            
            TheBot.AddExcuteHandler(named, AskTextString);
        }

        public virtual string GetServiceName()
        {
            string named = GetType().Name;
            named = named.ToLower();
            named = named.Replace("engine", "");
            named = named.Replace("factiod", "");
            named = named.Substring(0, 5).ToLower();
            return named;
        }

        public string AskQuery(string searchTerm1, OutputDelegate dbgLog, Func<Unifiable> OnFalure, XmlNode templateNode, float threshold, bool expandWithWordNet, bool expandOnNoHits, out float reliablity)
        {
            reliablity = 1.0f;
            return GetTextResult(searchTerm1);
        }

        public int InsertFactiod(string myText, XmlNode templateNode, WordExpander WordNetExpand)
        {
            return assertTo.InsertFactiod(myText, templateNode, WordNetExpand);
        }

        public int UpdateFactoid(string searchQuery, string myText, XmlNode templateNode)
        {
            return assertTo.UpdateFactoid(searchQuery, myText, templateNode);
        }

        public string MayPush(string text, XmlNode templateNodeOrNull)
        {
            // readonly engine
            return null;
        }

        public string MayAsk(string text, XmlNode templateNodeOrNull)
        {
            if (IsNullOrEmpty(text)) return null;
            return text;
        }

        public int DeleteTopScoring(string myText, XmlNode templateNode, bool mustContainExact)
        {
            return assertTo.DeleteTopScoring(myText, templateNode, mustContainExact);
        }

        public long LoadDocuments(string file, XmlNode templateNode)
        {
            return assertTo.LoadDocuments(file, templateNode);
        }

        public bool IsDbPresent
        {
            get { return MayAsk("What is 1 plus 1?", null) != null; }
        }

        public string FixPronouns(string myText, Func<string ,string> templateNode)
        {
            return assertTo.FixPronouns(myText, templateNode);
        }

        public virtual ICollection<ISearchResult> Search(string searchTerm1, WordExpander wordNetExpanderOnNoHits)
        {
            string ret = GetTextResult(searchTerm1);
            if (IsNullOrEmpty(ret)) return NO_RESULTS;
            float reliablity = DefaultReliablity(searchTerm1, ret);
            if (reliablity <= 0.0) return NO_RESULTS;
            return new[] { new OneSearchResult(GetType().Name + ": " + searchTerm1, ret, reliablity) };
        }

        virtual protected float DefaultReliablity(string searchTerm1, string ret)
        {
            if (IsNullOrEmpty(searchTerm1) || IsNullOrEmpty(ret)) return 0.0f;
            return 1.0F;
        }

        protected bool IsNullOrEmpty(string searchTerm1)
        {
            if (string.IsNullOrEmpty(searchTerm1)) return true;
            return searchTerm1.Trim().Length == 0;
        }

        public virtual string GetTextResult(string searchTerm1)
        {
            string ret;
            string res =
                HttpUtil.GetUrlData(
                    MakeSearchString(System.Web.HttpUtility.UrlEncode(System.Web.HttpUtility.UrlDecode(searchTerm1))));
            res = HttpUtil.GetWellFormedHTML(res, null);
            try
            {
                return ParseXmlResult(res,GetResultTags());
            }
            catch (Exception)
            {
               return res;
            }
        }

        private readonly RenderOptions PASS1 =
            new RenderOptions(
                new List<string>()
            //  {"li", "a", "div", "span", "ul", "p", "b", "font", "img"}
                ,
                new List<string>() { "script", "#comment", "like" , "style"});

        private readonly RenderOptions PASS2 =
            new RenderOptions(
                new List<string>()
                    {
                        "li",
                        "a",
                        "div",
                        "span",
                        "ul",
                        "p",
                        "b",
                        "i",
                        "node",
                        "font",
                        "img",
                        "tr",
                        "td",
                        "table",
                        "KW",
                        "notrim"
                    }
                ,
                new List<string>() {"script", "#comment", "like"});
    
        public virtual string ParseXmlResult(string res, string resFind)
        {
            int idxOf = res.IndexOf("?>");
            if (idxOf > 5)
            {
                if (idxOf < 50)
                {
                    string prefix = ""; // res.Substring(0, idxOf + 2);
                    res = res.Substring(idxOf + 2);
                    res = prefix + "<node>" + res + "</node>";
                }
            }
            var nodes = StaticXMLUtils.getNode(res);
            var body = StaticXMLUtils.FindNode("body", nodes, null, 10);
            if (body != null) nodes = body;
            var text_result = StaticXMLUtils.FindNode(resFind, nodes, null, 10);
            if (text_result == null) text_result = body;
            if (text_result != null)
            {
                var res2 = StaticXMLUtils.VisibleRendering(text_result.ChildNodes, PASS1);
                string s = res2.Split(new string[] {"<div id=\"ga_mainDebug"}, StringSplitOptions.RemoveEmptyEntries)[0];
                // StaticXMLUtils.InnerXmlText(text_result);
                //if (s.Contains("<"))
                {
                    try
                    {
                        var s2 =
                            StaticXMLUtils.VisibleRendering(
                                StaticXMLUtils.getNode("<node>" + HttpUtil.GetWellFormedHTML("<node>" + s + "</node>", null) + "</node>").ChildNodes, PASS2);
                        if (!IsNullOrEmpty(s2)) s = s2;
                    }
                    catch
                    {
                    }
                }
                return s;
            }
            TheBot.writeToLog(GetServiceName() + ": unused " + res);
            return null;
        }
    }
}