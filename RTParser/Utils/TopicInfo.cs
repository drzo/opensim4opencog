using System;
using System.Collections.Generic;
using System.Xml;

namespace RTParser.Utils
{
    [Serializable]
    public class TopicInfo : MatchInfo
    {
        public Node GraphmasterNode;
        public List<CategoryInfo> CategoryInfos = new List<CategoryInfo>();

        public TopicInfo(XmlNode pattern, Unifiable unifiable)
            : base(pattern, unifiable)
        {
            FullPath = unifiable;
        }

        public void AddCategory(CategoryInfo template)
        {
            CategoryInfos.Add(template);
        }

        public static TopicInfo FindTopic(LoaderOptions loaderOptions, Unifiable unifiable)
        {
            if (NoInfo) return null;
            return loaderOptions.CtxGraph.FindTopic(unifiable);
        }
    }
}