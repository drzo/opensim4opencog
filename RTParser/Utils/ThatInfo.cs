using System;
using System.Collections.Generic;
using System.Xml;
//using CategoryInfo = RTParser.Utils.TemplateInfo;

namespace RTParser.Utils
{
    [Serializable]
    public class ThatInfo : MatchInfo
    {
        public List<CategoryInfo> CategoryInfos = new List<CategoryInfo>();
        public Node GraphmasterNode;

        public ThatInfo(XmlNode pattern, Unifiable unifiable)
            : base(pattern, unifiable)
        {
            FullPath = unifiable;
        }

        public void AddCategory(CategoryInfo template)
        {
            CategoryInfos.Add(template);
        }

        public static ThatInfo GetPattern(LoaderOptions loaderOptions, Unifiable unifiable)
        {
            return loaderOptions.CtxGraph.FindThat(unifiable);
        }
    }
}