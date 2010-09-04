using System;
using System.Collections.Generic;
using System.Xml;

namespace RTParser.Utils
{
    [Serializable]
    public class TemplateInfo : OutputInfo, IAIMLInfo
    {
        public CategoryInfo CategoryInfo;
        public Node GraphmasterNode;
        public GuardInfo Guard;
        public SubQuery Query;
        public double Rating = 1.0;
        public Unifiable TextSaved;

        public TemplateInfo(XmlNode template, GuardInfo guard, Node patternNode, CategoryInfo categoryInfo)
            : base(template)
        {
            if (template.Name != "template")
            {
                throw new UnauthorizedAccessException();
            }
            Guard = guard;
            //That = that;
            GraphmasterNode = patternNode;
            CategoryInfo = categoryInfo;
            try
            {
                Rating = double.Parse(StaticXMLUtil.GetAttribValue(template, "score", "1.0"));
            }
            catch
            {
            }
            if (Rating != 1.0)
            {
                RTPBot.writeDebugLine("!! SCORE =" + Rating + " for " + OuterXml + " in " + categoryInfo);
            }
        }

        public ThatInfo That
        {
            get { return CategoryInfo.That; }
        }

        public List<XmlNode> Preconds
        {
            get { return CategoryInfo.Preconds; }
        }

        // override object.Equals

        public bool IsDisabled
        {
            get { return CategoryInfo.IsDisabled; }
            set
            {
                if (value != CategoryInfo.IsDisabled)
                {
                    CategoryInfo.IsDisabled = value;
                    Node node = GraphmasterNode;
                    if (value)
                    {
                        node.TemplateInfos.Remove(this);
                        node.TemplateInfosDisabled = node.TemplateInfosDisabled ?? new List<TemplateInfo>();
                        node.TemplateInfosDisabled.Add(this);
                    }
                    else
                    {
                        //node.TemplateInfosDisabled = node.TemplateInfosDisabled ?? new List<TemplateInfo>();                        
                        node.TemplateInfosDisabled.Remove(this);

                        node.TemplateInfos = node.TemplateInfos ?? new List<TemplateInfo>();
                        node.TemplateInfos.Add(this);
                    }
                }
            }
        }

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

        public XmlNode ClonedOutput
        {
            get { return StaticXMLUtil.CopyNode(Output, true); }
        }

        #region IAIMLInfo Members

        public GraphMaster Graph
        {
            get { return GraphmasterNode.Graph; }
        }

        public string ToFileString(PrintOptions printOptions)
        {
            if (CategoryInfo != null) return CategoryInfo.ToFileString(printOptions);
            return ToString();
        }

        public string SourceInfo()
        {
            return StaticXMLUtil.LocationInfo(srcNode);
        }

        #endregion

        public override bool Equals(object obj)
        {
            //       
            // See the full list of guidelines at
            //   http://go.microsoft.com/fwlink/?LinkID=85237  
            // and also the guidance for operator== at
            //   http://go.microsoft.com/fwlink/?LinkId=85238
            //

            if (obj == null || GetType() != obj.GetType())
            {
                return false;
            }

            // TODO: write your implementation of Equals() here
            //throw new NotImplementedException();
            return base.Equals(obj);
        }

// override object.GetHashCode
        public override int GetHashCode()
        {
            // TODO: write your implementation of GetHashCode() here
            //throw new NotImplementedException();
            return base.GetHashCode();
        }

        public override string ToString()
        {
            XmlNode tryit = base.Output.ParentNode;
            if (tryit != null)
            {
                return "" + TextPatternUtils.CleanWhitepaces(tryit.OuterXml) +
                       StaticXMLUtil.LocationEscapedInfo(tryit);
            }
            string s = base.ToString();
            /*            if (Guard != null)
                        {
                            s = s + Guard.ToString();
                        }
                        if (That != null)
                        {
                            s = s + That.OuterXml;
                        }*/
            return s;
        }

        public static TemplateInfo GetTemplateInfo(XmlNode template, GuardInfo guard, ThatInfo thatInfo, Node node,
                                                   CategoryInfo category)
        {
            bool prev = NoInfo;
            try
            {
                NoInfo = false;
                return new TemplateInfo(template, guard, node, category);
                if (thatInfo != null) category.That = thatInfo;
            }
            finally
            {
                NoInfo = prev;
            }
        }
    }

    public interface IAIMLInfo
    {
        GraphMaster Graph { get; }
        string ToFileString(PrintOptions printOptions);
        string SourceInfo();
    }
}