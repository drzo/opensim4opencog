using System;
using System.Collections.Generic;
using System.Xml;
using AltAIMLbot.Utils;
using AltAIMLParser;
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
        public long RunLowMemHooks()
        {
            long total = 0;
            if (_srcNode != null)
            {
                _srcNode = null;
                total++;
            }
            if (_srcNode != null)
            {
                _srcNodeString = null;
                total++;
            }
            if (_subQuery != null)
            {
                _subQuery = null;
                total++;
            }
            if (_textSaved != null)
            {
                _textSaved = null;
                total++;
            }
            if (_templateKey != null)
            {
                _templateKey = null;
                total++;
            }
            if (_preconditionKey != null)
            {
                _preconditionKey = null;
                total++;
            }
            if (Preconds != null) foreach (ConversationCondition list in Preconds)
            {
                total += list.RunLowMemHooks();
            }
            return total;
        }

        //public CategoryInfo ParentCategoryInfo;
        public CategoryInfo CategoryInfo { get { return this; } }
        //public Node GraphmasterNode;
        private Unifiable _guard;
        public override GuardInfo Guard
        {
            get { return _guard; }
            set { _guard = value; }
        }
        
        private Unifiable _response;
        public override ResponseInfo Response
        {
            get { return _response; }
            set { _response = value; }
        }

        public SubQuery Query
        {
            get { return _subQuery; }
            set { _subQuery = value; }
        }

        [NonSerialized]
        private SubQuery _subQuery;
        
        private double _rating;
        public double TemplateRating
        {
            get { return _rating; }
            set { _rating = value; }
        }

        [NonSerialized]
        private Unifiable _textSaved;
        public Unifiable TextSaved
        {
            get { return _textSaved; }
            set { _textSaved = value; }
        }

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

        [NonSerialized]
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
                return base.TemplateXml;
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
                throw new NotImplementedException();                
            }       
            TemplateRating = 1.0;
            Guard = guard;
            if ((thatInfo != null && !thatInfo.IsCatchAll) || (guard != null && !guard.IsCatchAll)
                || (topicInfo != null && !topicInfo.IsCatchAll))
            {
                IsHighlyUsefull = true;
            }
            //CategoryXml = cateNode;
            That = thatInfo;
            Response = responseInfo;
            Topic = topicInfo;
            Pattern = pattern;
            GraphmasterNode = patternNode;
            srcNode = templateNode;
            //ParentCategoryInfo = categoryInfo;
            try
            {                
                {
                    bool doCut;
                    if (StaticXMLUtils.TryParseBool(templateNode, "cut", out doCut) || StaticXMLUtils.TryParseBool(cateNode, "cut", out doCut))
                    {
                        NeckCut = doCut;
                    }
                }
                string scoreString = StaticXMLUtils.GetAttribValue(templateNode, "score", null);
                scoreString = scoreString ?? StaticXMLUtils.GetAttribValue(cateNode, "score", null);
                TemplateRating = scoreString != null ? double.Parse(scoreString) : 1.0;
            }
            catch
            {
            }
            if (TemplateRating != 1.0)
            {
                AltBot.writeDebugLine("!! SCORE =" + TemplateRating + " for " + this);
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
                if (StaticAIMLUtils.IsSilentTag(TemplateXml))
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
            return Filename + ":" + StaticXMLUtils.GetLineNumberOfXmlNode(this);
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
        
        public override string ToString()
        {
            {
                string rules = GetRuleStrings;
                if (rules != "")
                {
                    rules = "\n" + rules;
                }
                if (Guard != null)
                {
                    rules += "\n Guard:" + Guard.ToString();
                }

                string disables = WhyDisabled ?? "";
                string getSourceWithTopic;
                return string.Format("TemplateInfo: {0} {1} {2} {3}", disables, GetSourceWithTopic(PrintOptions.PROOF_OUTPUT), SourceInfo(), rules);
            }
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
            XmlNode staticAIMLUtilsXmlStar = StaticAIMLUtils.XmlStar.Value;
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
        double TemplateRating { get; set;  }
    }
    public interface TemplateInfo : CategoryInfo, TemplateResult, IXmlLineInfo
    {
        string ToString();
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
        long RunLowMemHooks();
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