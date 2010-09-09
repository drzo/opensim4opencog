using System;
using System.Collections.Generic;
using System.Xml;
using MushDLR223.Utilities;
using RTParser.AIMLTagHandlers;

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
        string _templateKey;

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
                Rating = double.Parse(StaticXMLUtils.GetAttribValue(template, "score", "1.0"));
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
            get { return StaticXMLUtils.CopyNode(Output, true); }
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
            return StaticXMLUtils.LocationInfo(srcNode);
        }

        #endregion

        // override object.GetHashCode
        public override int GetHashCode()
        {
            return TemplateKey.GetHashCode();
        }

        public override string ToString()
        {
            XmlNode tryit = base.Output.ParentNode;
            if (tryit != null)
            {
                return "" + TextPatternUtils.CleanWhitepaces(tryit.OuterXml) +
                       StaticXMLUtils.LocationEscapedInfo(tryit);
            }
            //string s = base.ToString();
            /*            if (Guard != null)
                        {
                            s = s + Guard.ToString();
                        }
                        if (That != null)
                        {
                            s = s + That.OuterXml;
                        }*/
            return "TemplateInfoKey:" + TemplateKey;
            //            return s;
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

        static public string MakeKey(XmlNode templateNode, XmlNode guard, XmlNode thatInfo)
        {
            return MakeKey(makeStar(templateNode), makeStar(guard), true ? null : makeStar(thatInfo));
        }

        private static string makeStar(XmlNode templateNode)
        {
            return IsStarLikeNode(templateNode) ? "*" : templateNode.InnerXml;
        }

        private static bool IsStarLikeNode(XmlNode thatInfo)
        {
            if (thatInfo == null ||  thatInfo.ChildNodes.Count == 0) return true;
            XmlNode staticAIMLUtilsXmlStar = StaticAIMLUtils.XmlStar;
            if (thatInfo == staticAIMLUtilsXmlStar) return true;
            XmlNode thatInfoLastChild = thatInfo.LastChild;
            return thatInfoLastChild == staticAIMLUtilsXmlStar && thatInfo.FirstChild == thatInfoLastChild;
        }

        static public string MakeKey(string newStr, string newGuard, string newThat)
        {
            var f = AsStar(newStr);
            string gs = AsStar(newGuard);
            if (gs == "*") return StaticAIMLUtils.MakeAimlMatchable(f);
            return StaticAIMLUtils.MakeAimlMatchable(f + " guardbom " + gs);
        }

        internal static string AsStar(string that)
        {
            if (that == null) return "*";
            string thatTrim = that.Trim();
            return thatTrim.Length == 0 ? "*" : thatTrim;
        }

        public bool AimlSameKey(string newStr, string newGuard, string newThat)
        {
            if (_templateKey!=null) return _templateKey == MakeKey(newStr, newGuard, newThat);
            if(!StaticAIMLUtils.AimlSame(makeStar(Guard.Output), AsStar(newGuard))) return false;
            if(!StaticAIMLUtils.AimlSame(makeStar(Output), AsStar(newStr))) return false;
            return true;
            /*

            return MakeKey(oldStr, oldGuard, oldThat);
            return StaticAIMLUtils.AimlSame(newStr, Output.OuterXml)
                   && StaticAIMLUtils.AimlSame(newGuard, oldGuard)
                   && StaticAIMLUtils.AimlSame(newThat, oldThat);
             */
        }

        public bool AimlSameKey(string s)
        {
            if (TemplateKey != s) return false;
            return true;
        }

        public string TemplateKey
        {
            get
            {
                if (_templateKey == null)
                {
                    return MakeKey(Output, Guard != null ? Guard.Output : null, That.PatternNode);
                  //  _templateKey = MakeKey(Output, Guard != null ? Guard.Output : null, That.PatternNode);
                }
                return _templateKey;
            }
            set
            {
                //_templateKey = value;
            }
        }

        public void AppendTemplate(XmlNode node, CategoryInfo category, List<XmlNode> nodes)
        {
            throw new NotImplementedException();
        }
    }

    public interface IAIMLInfo
    {
        GraphMaster Graph { get; }
        string ToFileString(PrintOptions printOptions);
        string SourceInfo();
    }
}