using System;
using System.Collections.Generic;
using System.Xml;
using MushDLR223.Utilities;
using RTParser.AIMLTagHandlers;
using PatternInfo = RTParser.Unifiable;
using ThatInfo = RTParser.Unifiable;
using TopicInfo = RTParser.Unifiable;
using GuardInfo = RTParser.Unifiable;
using ResponseInfo = RTParser.Unifiable;



namespace RTParser.Utils
{
    [Serializable]
    public sealed class TemplateInfo : CategoryInfoImpl1, CategoryInfo
    {
        //public CategoryInfo ParentCategoryInfo;
        public CategoryInfo CategoryInfo { get { return this; } }
        //public Node GraphmasterNode;
        public GuardInfo Guard;
        public ResponseInfo Response;
        public SubQuery Query;
        public double Rating = 1.0;
        public Unifiable TextSaved;
        public bool NeckCut = true;

        public event Func<SubQuery, Request, bool> OutputsCreateOnSuccees;
        public event Func<SubQuery, Request, bool> TemplateSucceededCallback;
        public event Func<SubQuery, Request, bool> TemplateFailedCallback;

        public void OnOutputsCreated(SubQuery query, Request request)
        {
            if (OutputsCreateOnSuccees != null) OutputsCreateOnSuccees(query, request);
        }

        public void OnTemplatesSucceeded(SubQuery query, Request request)
        {
            if (TemplateSucceededCallback != null) TemplateSucceededCallback(query, request);
        }
        public void OnTemplatesFailed(SubQuery query, Request request)
        {
            if (NeckCut)
            {
                query.Result.WhyResultComplete = "cut from " + ToString();
                if (request.SuspendSearchLimits)
                {
                    request.SuspendSearchLimits = false;
                }
            }
            if (TemplateFailedCallback != null) TemplateFailedCallback(query, request);
        }

        string _templateKey;

        public override int CompareTo(TemplateInfo other)
        {
            return CompareTemplates(this, other);
        }
        public static int CompareTemplates(TemplateInfo thiz, TemplateInfo other)
        {
            return StaticAIMLUtils.CollectionCompare<XmlNode>(thiz.TemplateXml.ChildNodes, other.TemplateXml.ChildNodes, StaticAIMLUtils.CompareXmlNodes);
        }

        public XmlNode TemplateXml
        {
            get
            {
                if (Response != null && Response.PatternNode != null) return Response.PatternNode;
                return base.TemplateXmlNode;
            }
           // set { Response.srcNode = value; }
        }


        public TemplateInfo(PatternInfo pattern, XmlNode cateNode, LoaderOptions options, ResponseInfo template,
            GuardInfo guard, Node patternNode, ThatInfo thatInfo)
            : base(pattern, cateNode, options)
        {
            if (template != null && template.FullPath == Unifiable.Empty)
            {
                throw new UnauthorizedAccessException();
            }
            Guard = guard;
            That = thatInfo;
            Response = template;
            GraphmasterNode = patternNode;
            //ParentCategoryInfo = categoryInfo;
            try
            {
                if (CategoryXml.Attributes != null)
                {
                    bool doCut;
                    if (StaticXMLUtils.TryParseBool(CategoryXml, "cut", out doCut) || StaticXMLUtils.TryParseBool(TemplateXml, "cut", out doCut))
                    {
                        NeckCut = doCut;
                    }
                }
                string scoreString = StaticXMLUtils.GetAttribValue(template.PatternNode, "score", null);
                scoreString = scoreString ?? StaticXMLUtils.GetAttribValue(cateNode, "score", null);
                Rating = double.Parse(scoreString ?? "1.0");
            }
            catch
            {
            }
            if (Rating != 1.0)
            {
                RTPBot.writeDebugLine("!! SCORE =" + Rating + " for " + OuterXml + " in " + CategoryInfo);
            }
        }

        /*
        public ThatInfo That
        {
            get { return CategoryInfo.That; }
            set { throw new NotImplementedException(); }
        }
        

        public List<ConversationCondition> Preconds
        {
            get { return CategoryInfo.Preconds; }
        }

        // override object.Equals
        public bool IsTraced
        {
            get { return CategoryInfo.IsTraced; }
            set { CategoryInfo.IsTraced = value; }
        }*/

        public bool IsDisabled
        {
            get { return base.IsDisabled; }
            set
            {
                if (value != base.IsDisabled)
                {
                    base.IsDisabled = value;
                    Node node = GraphmasterNode;
                    if (value)
                    {
                        if (node.TemplateInfos != null) node.TemplateInfos.Remove(this);
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
                string s = TemplateXml.InnerXml;
                if (s.StartsWith("<think") && s.EndsWith("k>"))
                {
                    return true;
                }
                return false;
            }
        }

        public XmlNode ClonedOutput
        {
            get { return StaticXMLUtils.CopyNode(TemplateXml, true); }
        }

        #region IAIMLInfo Members

        public GraphMaster Graph
        {
            get { return GraphmasterNode.Graph; }
        }

        public string ToFileString(PrintOptions printOptions)
        {
            if (CategoryInfo != null) return base.ToFileString(printOptions);
            return ToString();
        }

        public string SourceInfo()
        {
            return StaticXMLUtils.LocationInfo(srcNode);
        }
        /*
        public TemplateInfo Response
        {
            get { return this; }
        }
        */
        #endregion

        // override object.GetHashCode
        public override int GetHashCode()
        {
            return TemplateKey.GetHashCode();
        }

        internal override XmlNode srcNode { get; set; }

        public override string ToString()
        {
            XmlNode tryit = TemplateXml.ParentNode;
            if (tryit != null)
            {
                string rules = GetRuleStrings();
                if (rules != "")
                {
                    rules = "\n" + rules;
                }
                return "" + TextPatternUtils.CleanWhitepaces(tryit.OuterXml) +
                       StaticXMLUtils.LocationEscapedInfo(tryit) + rules;
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

        public override Unifiable FullPath{ get; set; }

        public static TemplateInfo GetTemplateInfo(XmlNode template, GuardInfo guard, ThatInfo thatInfo, Node node,
                                                   CategoryInfo category, GraphMaster graphMaster)
        {
            bool prev = NoInfo;
            try
            {
                NoInfo = false;
                //return new TemplateInfo(template, guard, node, category);
                ResponseInfo responseInfo = graphMaster.FindResponse(template, template.OuterXml);
                category.GraphmasterNode = node;
                //category.Graph = node;
                if (thatInfo != null) category.That = thatInfo;
                TemplateInfo info = category as TemplateInfo;
                info.Guard = guard;
                info.Response = responseInfo;
                return info;
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
            if (thatInfo == null || thatInfo.ChildNodes.Count == 0) return true;
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
            if (_templateKey != null) return _templateKey == MakeKey(newStr, newGuard, newThat);
            if (!StaticAIMLUtils.AimlSame(makeStar(Guard.PatternNode), AsStar(newGuard))) return false;
            if (!StaticAIMLUtils.AimlSame(makeStar(TemplateXml), AsStar(newStr))) return false;
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
                    return MakeKey(Pattern.PatternNode, Guard != null ? Guard.PatternNode : null, That.PatternNode);
                    //  _templateKey = MakeKey(Output, Guard != null ? Guard.Output : null, That.PatternNode);
                }
                return _templateKey;
            }
            set
            {
                //_templateKey = value;
            }
        }

        public void AppendTemplate(XmlNode node, XmlNode  category, List<ConversationCondition> nodes)
        {
            throw new NotImplementedException();
        }

        public void AddRules(List<ConversationCondition> rules)
        {
            foreach (var r in rules) base.AddPrecondition(r);
        }


        public bool IsSatisfied(SubQuery subQuery)
        {
            var rules = this.Preconds;
            if (rules != null && rules.Count > 0)
            {
                foreach (var s in rules)
                {
                    if (!s.IsConditionTrue(subQuery))
                    {
                        return false;
                    }
                }
            }
            return true;
        }

        protected override string GetRuleStrings()
        {
            string s = "";
            var templateInfo = this.Template;
            var addRules = templateInfo.Preconds;
            if (addRules != null)
            {
                int c = 1;
                foreach (ConversationCondition rule in addRules)
                {
                    s += "<!-- Rule:  " + c + " " + rule.ToString().
                                                        Replace("<!--", "<#--").Replace("-->", "--#>") + " -->\n";
                    c++;
                }
            }
            return s;
        }
    }

    public interface IAIMLInfo
    {
        GraphMaster Graph { get; }
        string ToFileString(PrintOptions printOptions);
        string SourceInfo();
        TemplateInfo Template { get; }
    }
}