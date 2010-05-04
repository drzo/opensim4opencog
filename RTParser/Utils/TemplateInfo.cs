using System;
using System.Xml;

namespace RTParser.Utils
{
    [Serializable]
    public class TemplateInfo : OutputInfo
    {
        public GuardInfo Guard;
        public CategoryInfo CategoryInfo;
        public Node GraphmasterNode;

        public TemplateInfo(XmlNode template, GuardInfo guard, Node patternNode, CategoryInfo categoryInfo):base(template)
        {
            if (template.Name != "template")
            {
                throw new UnauthorizedAccessException();
            }
            Guard = guard;
            GraphmasterNode = patternNode;
            CategoryInfo = categoryInfo;
        }
        public override string ToString()
        {
            string s = base.ToString();
            if (Guard!=null)
            {
                s = s + Guard.ToString();
            }
            return s;
        }
    }
}