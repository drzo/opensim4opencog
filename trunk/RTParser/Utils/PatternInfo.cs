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
            return loaderOptions.Graph.FindPattern(pattern, unifiable);
        }
    }
}