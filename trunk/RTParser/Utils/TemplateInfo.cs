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
    public sealed class TemplateInfoImpl : CategoryInfoImpl1, TemplateInfo
    {
        //public CategoryInfo ParentCategoryInfo;
        public CategoryInfo CategoryInfo { get { return this; } }
        //public Node GraphmasterNode;
        public override GuardInfo Guard { get; set; }
        public override ResponseInfo Response { get; set; }
        public SubQuery Query { get; set; }
        public double Rating { get; set; }
        public Unifiable TextSaved { get; set; }
        public bool NeckCut = false;        

//        public event Func<SubQuery, Request, bool> OutputsCreateOnSuccees;
//        public event Func<SubQuery, Request, bool> TemplateSucceededCallback;
//        public event Func<SubQuery, Request, bool> TemplateFailedCallback;

        public void OnOutputsCreated(SubQuery query, Request request)
        {
            if (NeckCut)
            {
                query.Result.WhyResultComplete = "OnOutputsCreated cut from " + ToString();
                if (request.SuspendSearchLimits)
                {
                    request.SuspendSearchLimits = false;
                }
            }
            //if (OutputsCreateOnSuccees != null) OutputsCreateOnSuccees(query, request);
        }

        public void OnTemplatesSucceeded(SubQuery query, Request request)
        {
            //if (TemplateSucceededCallback != null) TemplateSucceededCallback(query, request);
        }
        public void OnTemplatesFailed(SubQuery query, Request request)
        {
            if (NeckCut)
            {
                query.Result.WhyResultComplete = "OnTemplatesFailed cut from " + ToString();
                if (request.SuspendSearchLimits)
                {
                    request.SuspendSearchLimits = false;
                }
            }
            //if (TemplateFailedCallback != null) TemplateFailedCallback(query, request);
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

        public override XmlNode TemplateXml
        {
            get
            {
                var tn = StaticXMLUtils.FindNode("template", srcNode, null);
                if (tn != null) return tn;
                var response = Response;
                if (response != null && response.PatternNode != null) return response.PatternNode;
                return TemplateXml;
            }
            // set { Response.srcNode = value; }
        }
        public bool IsHighlyUsefull { get; set; }

        public TemplateInfoImpl(PatternInfo pattern, XmlNode cateNode, XmlNode templateNode,
            LoaderOptions options, ResponseInfo responseInfo,
            GuardInfo guard, TopicInfo topicInfo, Node patternNode, ThatInfo thatInfo)
            : base(pattern, cateNode, options)
        {
            if (templateNode == null || responseInfo.FullPath == Unifiable.Empty)
            {
                throw new UnauthorizedAccessException();
            }
            srcNode = templateNode;
            Rating = 1.0;
            Guard = guard;
            if ((thatInfo != null && !thatInfo.IsUnrestrictedLongWildCard) || (guard != null && !guard.IsUnrestrictedLongWildCard)
                || (topicInfo != null && !topicInfo.IsUnrestrictedLongWildCard))
            {
                IsHighlyUsefull = true;
            }
            //CategoryXml = cateNode;
            That = thatInfo;
            Response = responseInfo;
            Topic = topicInfo;
            Pattern = pattern;
            GraphmasterNode = patternNode;
            PreconditionKey = patternNode.GetPath();
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
                string scoreString = StaticXMLUtils.GetAttribValue(TemplateXml, "score", null);
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
        override public bool IsTraced { get; set; }
        
        override public bool IsDisabledOutput { get; set; }
        override public bool IsSearchDisabled { get; set; }

        public override bool IsDisabled
        {
            get { return IsDisabledOutput || IsSearchDisabled || base.IsDisabled; }
            set
            {
                if (value != base.IsDisabled)
                {
                    base.IsDisabled = value;
                    SetDisabledInNode(value);
                }                
                IsSearchDisabled = value;
                IsDisabledOutput = value;
            }
        }

        public void SetDisabledInNode(bool value)
        {
            Node node = GraphmasterNode;
            if (node!=null) node.SetDisabled(this, value);
            if (InGraph != null) InGraph.SetDisabled(this, value);
        }

        public bool IsSilent
        {
            get
            {
                if (IsSilentTag(TemplateXml))
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

        public override GraphMaster Graph
        {
            get
            {
                Node gmNode = Template.GraphmasterNode;
                var g = gmNode != null ? gmNode.Graph : base.Graph;
                return g;
            }
        }

        override public string ToFileString(PrintOptions printOptions)
        {
            if (CategoryInfo != null) return base.ToFileString(printOptions);
            return base.ToFileString(printOptions);
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
            var tryit = TemplateXml;
            tryit = tryit != null ? TemplateXml.ParentNode : CategoryXml;
            if (tryit != null)
            {
                string rules = GetRuleStrings;
                if (rules != "")
                {
                    rules = "\n" + rules;
                }
                string disables = WhyDisabled ?? "";
                return "" + disables + " " + TextPatternUtils.CleanWhitepaces(tryit.OuterXml) + " " +
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

        //public override Unifiable FullPath { get; set; }

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
        internal static string MakeKey(XmlNode templateNode, XmlNode guard, List<ConversationCondition> additionalRules)
        {
            if (additionalRules == null || additionalRules.Count==0)
            {
                return MakeKey(makeStar(templateNode), makeStar(guard), null);
            }
            return MakeKey(makeStar(templateNode), makeStar(guard), true ? null : MakeRuleStrings(additionalRules));
        }

        static public string MakeKey(XmlNode templateNode, XmlNode guard/*, XmlNode thatInfo*/)
        {
            return MakeKey(makeStar(templateNode), makeStar(guard), null);//true ? null : makeStar(thatInfo));
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
            var f = AsStar(newStr) + " " + AsStar(newThat);
            string gs = AsStar(newGuard);
            if (gs != "*") return StaticAIMLUtils.MakeAimlMatchable(f);
             return StaticAIMLUtils.MakeAimlMatchable(f + " guardbom " + gs);
        }

        internal static string AsStar(string that)
        {
            if (that == null) return "*";
            string thatTrim = TextPatternUtils.Trim(that);
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
                    _templateKey = MakeKey(Response, Guard != null ? Guard.FullPath : null, GetRuleStrings);
                    //  _templateKey = MakeKey(Output, Guard != null ? Guard.Output : null, That.PatternNode);
                }
                return _templateKey;
            }
            set
            {
                _templateKey = value;
            }
        }

        public TemplateInfo AppendTemplate(XmlNode templateNode, XmlNode category, List<ConversationCondition> nodes)
        {
            throw new NotImplementedException();
        }

        public void AddRules(IEnumerable<ConversationCondition> rules)
        {
            if (rules != null)
            {
                foreach (var r in rules) base.AddPrecondition(r);
            }
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

        public override string GetRuleStrings
        {
            get
            {
                string s = "";
                var templateInfo = this.Template;
                var addRules = templateInfo.Preconds;
                return MakeRuleStrings(addRules);
            }
        }

        static public string MakeRuleStrings(IEnumerable<ConversationCondition> addRules )
        {
            {
                string s = "";
                if (addRules != null)
                {
                    int c = 1;
                    foreach (ConversationCondition rule in addRules)
                    {
                        s += string.Format("<!-- Rule {0}: {1} -->\n", c, StaticXMLUtils.MakeXmlCommentSafe(rule.ToString()));
                        c++;
                    }
                }
                return s;
            }
        }

        public bool BuildIndexes()
        {
           return BuildIndex(GetIndexObjects());
        }
        public bool RemoveIndexes()
        {
           return RemoveIndex(GetIndexObjects());
        }

        private IEnumerable<Unifiable> GetIndexObjects()
        {
            var v = new[]
                       {
                         //  makeStarNamed(Filename, "Filename"),
                           makeStarNamed(Pattern, "Pattern"),
                          // makeStarNamed(Guard, null),
                           makeStarNamed(Response, "Response"), 
                           makeStarNamed(That, null),
                           makeStarNamed(Topic, null)
                       };
            return v;
        }

        private Unifiable makeStarNamed(Unifiable phrase, string ifNullOrStar)
        {
            if (phrase == null)
            {
                if (ifNullOrStar == null) return ifNullOrStar;
                return "No" + ifNullOrStar;
            }
            if (phrase == "*")
            {
                if (ifNullOrStar == null) return ifNullOrStar;
                return "Star" + ifNullOrStar;
            }
            if (phrase == "_")
            {
                if (ifNullOrStar == null) return ifNullOrStar;
                return "One" + ifNullOrStar;
            }
            return phrase;
        }

        private bool BuildIndex(IEnumerable<Unifiable> names)
        {
            if (names == null) return false;
            bool didSomething = false;
            foreach (var name in names)
            {
                if (name != null)
                {
                    didSomething |= name.AddCategory(this);                   
                }
            }
            return didSomething;
        }

        private bool RemoveIndex(IEnumerable<Unifiable> names)
        {
            if (names == null) return false;
            bool didSomething = false;
            foreach (var name in names)
            {
                if (name != null)
                {
                    didSomething |= name.RemoveCategory(this);
                }
            }
            return didSomething;
        }
    }

    public interface TemplateResult
    {
        Unifiable TextSaved { get; set; }
        double Rating { get; set; }
    }
    public interface TemplateInfo : CategoryInfo, TemplateResult
    {
        XmlNode TemplateXml { get; }
        bool IsSilent { get; }
        XmlNode ClonedOutput { get; }
        CategoryInfo CategoryInfo { get; }
        GuardInfo Guard { get; set; }
        SubQuery Query { get; set; }
        string TemplateKey { get; set; }
        GraphMaster InGraph { get; set; }
        bool IsHighlyUsefull { get; set; }
        //double Rating { get; }
        bool IsSatisfied(SubQuery query);
        void AddRules(IEnumerable<ConversationCondition> additionalRules);
        TemplateInfo AppendTemplate(XmlNode templateNode, XmlNode cateNode, List<ConversationCondition> additionalRules);
        bool AimlSameKey(string templateKey);
        void OnTemplatesFailed(SubQuery query, Request request);
        void OnTemplatesSucceeded(SubQuery query, Request request);
        void OnOutputsCreated(SubQuery query, Request request);
        bool BuildIndexes();
        bool RemoveIndexes();
    }

    public interface IAIMLInfo
    {
        GraphMaster Graph { get; }
        string ToFileString(PrintOptions printOptions);
        string SourceInfo();
        TemplateInfo Template { get; }
        bool IsDisabledOutput { get; set; }
        bool IsSearchDisabled { get; set; }
    }
}