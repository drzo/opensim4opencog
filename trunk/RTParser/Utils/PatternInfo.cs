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
            p = AIMLLoader.CleanWhitepaces(p);
            p = "<srai>" + p + "</srai>";
            var t = newTemplateInfo.InnerXml.ToLower();
            t = AIMLLoader.CleanWhitepaces(t);
            t = t.Replace("<star/>", "*");
            t = t.Replace("<sr/>", "<srai>*</srai>");
            if (t.Contains(p))
            {
                return true;
            }
            return false;
        }
    }
}