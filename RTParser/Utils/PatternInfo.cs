using System;
using System.Collections.Generic;
using System.Xml;

namespace RTParser.Utils
{
    [Serializable]
    public class PatternInfo : GraphLinkInfo
    {

        public List<CategoryInfo> CategoryInfos = null;

        public void AddCategory(CategoryInfo template)
        {
            CategoryInfos = CategoryInfos ?? new List<CategoryInfo>();
            CategoryInfos.Add(template);
        }       

        internal bool LoopsFrom(string innerXml)
        {
            string p = StaticAIMLUtils.MakeAimlMatchable(FullPath.AsString().Replace("_", "*"));
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
            string p = StaticAIMLUtils.MakeAimlMatchable(FullPath.AsString().Replace("_", "*"));
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

        public PatternInfo(XmlNode pattern, Unifiable unifiable)
            : base(pattern)
        {
            FullPath = unifiable;
        }

        public bool IsCatchAll
        {
            get { return FullPath.IsWildCard(); }
        }

        public XmlNode PatternNode
        {
            get { return srcNode; }
        }

        public string GetKey()
        {
            return FullPath.AsString();
        }

        public override string ToString()
        {
            if (FullPath != null) return FullPath.AsString();
            return PatternNode.OuterXml;
        }
    }
}