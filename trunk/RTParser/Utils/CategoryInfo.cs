using System;
using System.Collections.Generic;
using System.Text.RegularExpressions;
using System.Xml;
using MushDLR223.Utilities;
using PatternInfo = RTParser.Unifiable;
using ThatInfo = RTParser.Unifiable;
using TopicInfo = RTParser.Unifiable;
using GuardInfo = RTParser.Unifiable;
using ResponseInfo = RTParser.Unifiable;

namespace RTParser.Utils
{
    [Serializable]
    abstract public class CategoryInfoImpl1 : GraphLinkInfo, IAIMLInfo, IComparable<TemplateInfo>
    {
        public string Filename { get; set; }
        public PatternInfo Pattern { get; set; }
        public List<ConversationCondition> Preconds;
        public TemplateInfo Template { get { return (TemplateInfo)this; } }
        public ThatInfo That { get; set; }
        public TopicInfo Topic { get; set; }

        static XmlNode CheckXml(XmlNode xmlNode)
        {
            if (xmlNode != null) return xmlNode;
            return xmlNode;
        }

        //private TemplateInfo ParentCategory;

        public bool IsTraced { get; set; }

        protected CategoryInfoImpl1(PatternInfo pattern, XmlNode cateNode, LoaderOptions options)
            : base(cateNode)
        {
            Pattern = pattern;
            Filename = options.CurrentFilename;
        }

        public bool IsDisabledOutput { get; set; }
        public virtual bool IsDisabled { get; set; }
        public string WhyDisabled
        {
            get
            {
                if (Template.IsDisabledOutput)
                {
                    return "IsDisabledOutput";
                }
                else if (IsDisabled)
                {
                    return "IsDisabled";
                }
                return null;
            }
        }

        public XmlNode CategoryXml { get { return CheckXml(StaticXMLUtils.FindNode("category", CheckXml(srcNode), null)); } }

        public XmlNode TemplateXmlNode
        {
            get { return CheckXml(StaticXMLUtils.FindNode("template", CategoryXml, null)); }
        }

        public XmlNode TopicXml
        {
            get { return CheckXml(StaticXMLUtils.FindNode("topic", CategoryXml, null)); }
        }

        public XmlNode ThatXml
        {
            get { return CheckXml(StaticXMLUtils.FindNode("that", CategoryXml, null)); }
        }

        #region IAIMLInfo Members

        string IAIMLInfo.SourceInfo()
        {
            return StaticXMLUtils.LocationInfo(CategoryXml);
        }

        public GraphMaster Graph
        {
            get { return Template.GraphmasterNode.Graph; }
        }

        //protected abstract XmlNode TemplateXml { get; set; }
        public virtual Node GraphmasterNode { get; set; }
        public abstract XmlNode TemplateXml { get; }

        public string ToFileString(PrintOptions printOptions)
        {
            //if (XmlDocumentLineInfo.SkipXmlns && this.srcNode.Attributes != null) this.srcNode.Attributes.RemoveNamedItem("xmlns");
            string s = "";
            if (IsDisabled)
            {
                if (!printOptions.WriteDisabledItems) return s;
            }
            string graphName = ((IAIMLInfo) this).Graph.ScriptingName;
            if (printOptions.IncludeGraphName)
            {
                if (graphName != printOptions.CurrentGraphName)
                {
                    if (printOptions.InsideAiml)
                    {
                        s += "\n</aiml>\n";
                        s += string.Format("\n<aiml graph=\"{0}\">\n", graphName);
                        printOptions.CurrentGraphName = graphName;
                    }
                    else
                    {
                        printOptions.InsideAiml = true;
                        s += string.Format("\n<aiml graph=\"{0}\">\n", graphName);
                        printOptions.CurrentGraphName = graphName;
                    }
                }
            }

            s += GetSourceWithTopic(printOptions);

            string escapedStuff = "";

            if (!printOptions.IncludeLineInfoExternal)
            {
                if (!printOptions.GroupFileElements)
                {
                    if (printOptions.IncludeFileNamePerNode)
                    {
                        string cfile = Filename ?? StaticXMLUtils.FileNameOfXmlNode(srcNode);
                        if (cfile != printOptions.InsideFilename)
                        {
                            escapedStuff += cfile;
                            printOptions.InsideFilename = cfile;
                        }
                    }
                }
                if (printOptions.IncludeLinenoPerNode)
                {
                    escapedStuff += ":" + LineNumberInfo;
                }
            }

            escapedStuff = StaticXMLUtils.MakeXmlCommentSafe(escapedStuff);
            if (IsDisabled)
            {
                s = DLRConsole.SafeFormat("<!-- {0} {1} {2} -->", MakeXmlCommentSafe(WhyDisabled), MakeXmlCommentSafe(s), escapedStuff);
            } else
            {
                s = DLRConsole.SafeFormat("{0} <!-- {1}  -->", s, escapedStuff);
            }
            return s;
        }

        public string GetSourceWithTopic(PrintOptions printOptions)
        {
            string s = "";
            XmlNode topic1 = this.TopicXml;
            string insideTopic = printOptions.InsideTopic;
            string thisTopic = Topic ?? insideTopic;
            thisTopic = StaticXMLUtils.GetAttribValue(topic1, "name,topic", () => thisTopic, null);
            bool hasTopic = (thisTopic != insideTopic);
            if (hasTopic)
            {
                s += "<topic name=\"";                                    
                s += thisTopic;
                s += "\">";
            }
            else if (Topic != null && !Topic.IsCatchAll)
            {
                hasTopic = true;
                s += "<topic name=\"";
                string n = (string)Topic.FullPath;
                s += n;
                s += "\">";
            }

            s += printOptions.FormatXML(srcNode);
            s += GetRuleStrings;

            if (hasTopic) s += "</topic>";
            printOptions.InsideTopic = insideTopic;
            return s;
        }

        public string LineNumberInfo
        {
            get
            {
                return StaticXMLUtils.GetLineNumberOfXmlNode(srcNode);
            }
        }

        public abstract string GetRuleStrings { get; }

        #endregion

        public abstract int CompareTo(TemplateInfo other);

        public override string ToString()
        {
            return CategoryXml.OuterXml + " " + StaticXMLUtils.LocationEscapedInfo(CategoryXml);
        }

        public string XMLInfo()
        {
            return CategoryXml.OuterXml;
        }

        public void AddTemplate(TemplateInfo templateInfo)
        {
            //if (Template != null && Template != templateInfo) throw new InvalidCastException("non null " + Template);
            //Template = templateInfo;
            //  TemplateInfos.Add(templateInfo);
        }

        public static CategoryInfo GetCategoryInfo(PatternInfo info, XmlNode node, LoaderOptions filename, XmlNode templateNode,
            ResponseInfo template, GuardInfo guard, TopicInfo topicInfo, Node patternNode, ThatInfo thatInfo, IEnumerable<ConversationCondition> conds)
        {
            return filename.CtxGraph.FindCategoryInfo(info, node, filename, templateNode, template, guard, topicInfo,
                                                      patternNode, thatInfo, conds);
        }

        public static CategoryInfo MakeCategoryInfo(PatternInfo info, XmlNode cateNode, LoaderOptions filename,
            XmlNode templateNode, ResponseInfo template, GuardInfo guard, TopicInfo topicInfo, Node patternNode, ThatInfo thatInfo, IEnumerable<ConversationCondition> conds)
        {
            if (NoInfo) return null;
            var vv = new TemplateInfo(info, cateNode, templateNode, filename, template, guard, thatInfo, patternNode,
                                      thatInfo);
            vv.AddRules(conds);
            return vv;
        }

        internal void Check()
        {
            throw new NotImplementedException();
        }

        public virtual bool Matches(string pattern)
        {
            if (pattern == null || pattern == "*" || pattern == "") return true;
            string s = ToFileString(PrintOptions.VERBOSE_FOR_MATCHING);
            if (pattern.Contains(s)) return true;
            return Regex.Matches(s, pattern).Count > 0;
        }

        public void AddPrecondition(ConversationCondition info)
        {
            if (Preconds == null) Preconds = new List<ConversationCondition>();
            if (Preconds.Contains(info)) return;
            Preconds.Add(info);
        }

        public void AddPrecondition(ThatInfo info)
        {
            AddPrecondition(new ConversationCondition(info.PatternNode));
        }

        public void SetCategoryTag(Unifiable generatedPath, PatternInfo patternInfo, CategoryInfo category,
                                   XmlNode outerNode, XmlNode templateNode, GuardInfo guard, ThatInfo thatInfo)
        {
#if false
            var node = this.RootNode;
            if (SilentTagsInPutParent && AIMLLoader.IsSilentTag(templateNode))
            {
                GraphMaster parent1 = makeParent();
                this.Parents.Add(parent1);
                parent1.Size++;
                node = parent1.RootNode;
                writeToLog("Adding to Parent " + category);
            }
            Node created = Node.addCategoryTag(node, generatedPath, patternInfo,
                                category, outerNode, templateNode, guard, thatInfo, this);

            this.Size++;
#endif
            // keep count of the number of categories that have been processed
        }
    }

    public interface CategoryInfo
    {
        ThatInfo That { get; set; }
        bool IsDisabled { get; set; }
        PatternInfo Pattern { get; }
        TemplateInfo Template { get; }
        bool IsTraced { get; set; }
        XmlNode CategoryXml { get; }
        Node GraphmasterNode { get; set; }
        string Filename { get; }
        
        void SetCategoryTag(Unifiable categoryPath, PatternInfo patternInfo, CategoryInfo categoryInfo, XmlNode outerNode, XmlNode templateNode, GuardInfo guard, ThatInfo thatInfo);
        bool Matches(string match);
        void AddPrecondition(ThatInfo node);
        void AddPrecondition(ConversationCondition node);
        void AddTemplate(TemplateInfo newTemplateInfo);
    }
}