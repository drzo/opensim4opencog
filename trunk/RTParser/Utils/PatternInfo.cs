using System;
using System.Collections.Generic;
using System.Xml;

namespace RTParser.Utils
{
    [Serializable]
    public class PatternInfo : MatchInfo
    {
        public Node GraphmasterNode;
        public List<CategoryInfo> CategoryInfos = new List<CategoryInfo>();

        public PatternInfo(XmlNode pattern, Unifiable unifiable):base(pattern,unifiable)
        {
            FullPath = unifiable;
        }

        public void AddCategory(CategoryInfo template)
        {
            CategoryInfos.Add(template);
        }

        public static PatternInfo GetPattern(LoaderOptions loaderOptions, XmlNode pattern, Unifiable unifiable)
        {
            if (NoInfo) return null;
            return loaderOptions.Graph.FindPattern(pattern, unifiable);
        }

        internal bool LoopsFrom(TemplateInfo newTemplateInfo)
        {
            var p = FullPath.AsString().ToLower();
            p = p.Replace("_", "*");
            p = AIMLLoader.CleanWhitepacesLower(p);
            p = "<srai>" + p + "</srai>";

            var t = newTemplateInfo.InnerXml.ToLower();
            t = AIMLLoader.CleanWhitepacesLower(t);
            t = t.Replace("<star index=\"1\"/>", "*");
            t = t.Replace("<sr/>", "<srai>*</srai>");

            if (t.Contains(p))
            {
                return true;
            }
            return false;
        }
        internal bool DivergesFrom(TemplateInfo newTemplateInfo,out Unifiable from,out Unifiable to)
        {
            if (true)
            {
                from = "";
                to = "";
                return false;
            }
            var p = FullPath.AsString().ToLower();
            p = p.Replace("_", "*");
            p = AIMLLoader.CleanWhitepacesLower(p);
            p = "<srai>" + p + "</srai>";
            var t = newTemplateInfo.InnerXml.ToLower();
            t = AIMLLoader.CleanWhitepacesLower(t);
            t = t.Replace("<star/>", "*");
            t = t.Replace("<sr/>", "<srai>*</srai>");

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
            for(;i<s1.Length;i++)
            {
                if (s1[i]==s2[i]) continue;
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