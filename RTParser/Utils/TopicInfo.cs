using System;
using System.Collections.Generic;
using System.Xml;
//using CategoryInfo = RTParser.Utils.TemplateInfo;

namespace RTParser.Utils
{
    [Serializable]
    public class TopicInfo : MatchInfo
    {
        public List<CategoryInfo> CategoryInfos = new List<CategoryInfo>();
        public Node GraphmasterNode;

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