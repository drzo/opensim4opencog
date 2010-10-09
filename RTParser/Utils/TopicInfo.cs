using System;
using System.Collections.Generic;
using System.Xml;
//using CategoryInfo = RTParser.Utils.TemplateInfo;

namespace RTParser.Utils
{
    [Serializable]
    public class TopicInfo : PatternInfo
    {
        public TopicInfo(XmlNode pattern, Unifiable unifiable)
            : base(pattern, unifiable)
        {
            FullPath = unifiable;
        }
    }
}