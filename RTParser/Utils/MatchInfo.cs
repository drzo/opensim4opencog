using System.Xml;

namespace RTParser.Utils
{
    public abstract class MatchInfo : GraphLinkInfo
    {

        public MatchInfo(XmlNode pattern, Unifiable unifiable)
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