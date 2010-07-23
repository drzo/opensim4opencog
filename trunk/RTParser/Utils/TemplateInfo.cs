using System;
using System.Xml;

namespace RTParser.Utils
{
    [Serializable]
    public class TemplateInfo : OutputInfo, IAIMLInfo
    {
        public GuardInfo Guard;
        public ThatInfo That;
        public CategoryInfo CategoryInfo;
        public Node GraphmasterNode;
        public double Rating = 1.0;
        public SubQuery Query;
        public Unifiable TextSaved;

        public bool IsSilent
        {
            get
            {
                string s = InnerXml;
                if (s.StartsWith("<think") && s.EndsWith("k>"))
                {
                    return true;
                }
                return false;
            }
        }
        public TemplateInfo(XmlNode template, GuardInfo guard, ThatInfo that, Node patternNode, CategoryInfo categoryInfo)
            : base(template)
        {
            if (template.Name != "template")
            {
                throw new UnauthorizedAccessException();
            }
            Guard = guard;
            That = that;
            GraphmasterNode = patternNode;
            CategoryInfo = categoryInfo;
            try
            {
                Rating = double.Parse(RTPBot.GetAttribValue(template, "score", "1.0"));
            }
            catch
            {
            }
            if (Rating!=1.0)
            {
                RTPBot.writeDebugLine("!! SCORE =" + Rating + " for " + OuterXml + " in " + categoryInfo);
            }
        }

        public override string ToString()
        {
            XmlNode tryit = base.Output.ParentNode;
            if (tryit!=null)
            {
                return "" + AIMLLoader.CleanWhitepaces(tryit.OuterXml) +
                       AIMLLoader.LocationEscapedInfo(tryit);                
            }
            string s = base.ToString();
            if (Guard!=null)
            {
                s = s + Guard.ToString();
            }
            if (That != null)
            {
                s = s +  That.OuterXml;
            }
            return s;
        }

        public static TemplateInfo GetTemplateInfo(XmlNode template, GuardInfo guard, ThatInfo thatInfo, Node node, CategoryInfo category)
        {
            bool prev = NoInfo;
            try
            {
                NoInfo = false;
                return new TemplateInfo(template, guard, thatInfo, node, category);
            }
            finally 
            {
                NoInfo = prev;               
            }
        }

         public string ToFileString()
        {
            if (CategoryInfo != null) return CategoryInfo.ToFileString();
            return ToString();
        }
    }

    public interface IAIMLInfo
    {
        string ToFileString();
    }
}