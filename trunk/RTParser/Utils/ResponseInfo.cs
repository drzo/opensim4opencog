using System;
using System.Collections.Generic;
using System.Xml;

namespace RTParser.Utils
{
    [Serializable]
    public class ResponseInfo : OutputInfo
    {
        public List<CategoryInfo> CategoryInfos = new List<CategoryInfo>();
        public Node GraphmasterNode;
        public Unifiable Response;

        public ResponseInfo(XmlNode pattern, Unifiable unifiable) : base(pattern)
        {
            Response = unifiable;
        }

        public void AddCategory(CategoryInfo template)
        {
            CategoryInfos.Add(template);
        }

        public static ResponseInfo GetResponse(LoaderOptions loaderOptions, XmlNode pattern, Unifiable unifiable)
        {
            if (NoInfo) return null;
            return loaderOptions.CtxGraph.FindResponse(pattern, unifiable);
        }

        internal bool LoopsFrom(string innerXml)
        {
            string p = StaticAIMLUtils.MakeAimlMatchable(Response.AsString().Replace("_", "*"));
            p = "<srai>" + p + "</srai>";

            string t = StaticAIMLUtils.MakeAimlMatchable(innerXml);

            if (t.Contains(p))
            {
                return true;
            }
            return false;
        }

        internal bool DivergesFrom(TemplateInfo newTemplateInfo, out Unifiable from, out Unifiable to)
        {
            if (true)
            {
                from = "";
                to = "";
                return false;
            }
            string p = StaticAIMLUtils.MakeAimlMatchable(Response.AsString().Replace("_", "*"));
            p = "<srai>" + p + "</srai>";
            string t = StaticAIMLUtils.MakeAimlMatchable(newTemplateInfo.InnerXml);

            int firstTP = FirstMismatch(t, p);
            int lastTP = LastMismatch(t, p);
            int firstPT = FirstMismatch(p, t);
            int lastPT = LastMismatch(p, t);
            from = "";
            to = "";
            return false;
        }

        private int FirstMismatch(string s1, string s2)
        {
            int i = 0;
            for (; i < s1.Length; i++)
            {
                if (s1[i] == s2[i]) continue;
                return i - 1;
            }
            return i - 1;
        }

        private int LastMismatch(string s1, string s2)
        {
            int i = s1.Length - 1;
            for (; i >= 0; i--)
            {
                if (s1[i] == s2[i]) continue;
                return i - 1;
            }
            return i - 1;
        }
    }
}