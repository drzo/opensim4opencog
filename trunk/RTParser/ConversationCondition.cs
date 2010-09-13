using System.Text;
using System.Xml;
using MushDLR223.Utilities;

namespace RTParser.Utils
{
    public class ConversationCondition
    {
        public bool IsPrecond;
        public string TagName;
        public string Pattern;
        public string IndexVal;
        public bool IndexPosition;
        public XmlNode SourceNode;
        public override string ToString()
        {
            StringBuilder sb = new StringBuilder();
            if (IsPrecond) sb.Append("precond");
            else sb.Append("condition");
            if (TagName != null) sb.Append("=\"" + TagName.ToUpper() + "\"");
            if (IndexVal != null) sb.Append(" pos=" + IndexVal );
            if (IndexPosition) sb.Append(" idx=" + IndexPosition);
            if (Pattern != null) sb.Append(" match=\"" + Pattern + "\" ");
            if (SourceNode != null)
            {
                sb.AppendLine(StaticXMLUtils.LocationInfo(SourceNode) + SourceNode.OuterXml);
            }
            return sb.ToString();

        }

        public ConversationCondition(bool isPrecond, string tagName, string pattern, string indexVal, bool position1, XmlNode proof)
        {
            IsPrecond = isPrecond;
            TagName = tagName;
            Pattern = pattern;
            IndexVal = indexVal;
            IndexPosition = position1;
            SourceNode = proof;
        }

        public ConversationCondition(XmlNode node)
        {
            SetValue(node);
        }

        public void SetValue(XmlNode node)
        {
            SourceNode = node;
            bool isRequired;
            bool indexPosition;
            string indexVal;
            TagName = node.LocalName;
            Pattern = AIMLLoader.TryGetPrecondionThat(TagName, node,
                                                      out isRequired, out indexVal, out indexPosition);
            IsPrecond = isRequired;
            IndexPosition = indexPosition;
            IndexVal = indexVal;
        }

        /// <summary>
        /// Name/Value requirment
        /// </summary>
        /// <param name="name"></param>
        /// <param name="value"></param>
        /// <param name="proof"></param>
        public ConversationCondition(string name, Unifiable value, XmlNode proof)
            : this(true, name, value, "1", true, proof)
        {
        }
    }
}