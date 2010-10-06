using System;
using System.Collections.Generic;
using System.Text.RegularExpressions;
using System.Xml;
using MushDLR223.Utilities;
//using CategoryInfo = RTParser.Utils.TemplateInfo;

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
        public TopicInfo Topic;
        
        private TemplateInfo ParentCategory;

        public bool IsTraced { get; set; }

        protected CategoryInfoImpl1(PatternInfo pattern, XmlNode cateNode, LoaderOptions options)
            : base(cateNode)
        {
            Pattern = pattern;
            Filename = options.CurrentFilename;
        }

        public bool IsDisabled { get; set; }

        public XmlNode Category
        {
            get { return srcNode; }
        }

        public virtual XmlNode TemplateXml
        {
            get { return StaticXMLUtils.FindNode("template", Category, null); }
            set { throw new NotImplementedException(); }
        }

        public XmlNode TopicXml
        {
            get { return StaticXMLUtils.FindNodeOrHigher("topic", Category, null); }
        }

        public XmlNode ThatXml
        {
            get { return StaticXMLUtils.FindNodeOrHigher("that", Category, null); }
        }

        #region IAIMLInfo Members

        string IAIMLInfo.SourceInfo()
        {
            return StaticXMLUtils.LocationInfo(Category);
        }

        public GraphMaster Graph
        {
            get { return Pattern.GraphmasterNode.Graph; }
        }

        //protected abstract XmlNode TemplateXml { get; set; }
        public virtual Node GraphmasterNode { get; set; }

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
            XmlNode topic1 = this.TopicXml;
            bool hasTopic = topic1 != null;
            if (hasTopic)
            {
                s += "<topic name=\"";
                Unifiable n = StaticXMLUtils.GetAttribValue(topic1, "name", () => (string) null, null);
                s += n;
                s += "\">";
            }
            else if (Topic != null && !Topic.IsCatchAll)
            {
                hasTopic = true;
                s += "<topic name=\"";
                string n = (string) Topic.FullPath;
                s += n;
                s += "\">";
            }
            XmlWriterSettings settings = printOptions.XMLWriterSettings;
            s += printOptions.FormatXML(srcNode);

            if (hasTopic) s += "</topic>";
            if (IsDisabled)
            {
                s += "<!-- IsDisabled  " + s.Replace("<!--", "<#--").Replace("-->", "--#>") + " -->";
            }
            s += GetRuleStrings();
            return s;
        }

        protected abstract string GetRuleStrings();

        #endregion

        public abstract int CompareTo(TemplateInfo other);

        public override string ToString()
        {
            return Category.OuterXml + " " + StaticXMLUtils.LocationEscapedInfo(Category);
        }

        public string XMLInfo()
        {
            return Category.OuterXml;
        }

        public void AddTemplate(TemplateInfo templateInfo)
        {
            //if (Template != null && Template != templateInfo) throw new InvalidCastException("non null " + Template);
            //Template = templateInfo;
            //  TemplateInfos.Add(templateInfo);
        }

        public static CategoryInfo GetCategoryInfo(PatternInfo info, XmlNode node, LoaderOptions filename, ResponseInfo template, GuardInfo guard, Node patternNode, CategoryInfo categoryInfo)
        {
            return filename.CtxGraph.FindCategoryInfo(info, node, filename, template, guard, patternNode, categoryInfo);
        }

        public static CategoryInfo MakeCategoryInfo(PatternInfo info, XmlNode node, LoaderOptions filename, ResponseInfo template, GuardInfo guard, Node patternNode, CategoryInfo categoryInfo)
        {
            if (NoInfo) return null;
            return new TemplateInfo(info, node, filename, template, guard, patternNode, categoryInfo);
        }

        internal void Check()
        {
            throw new NotImplementedException();
        }

        public bool Matches(string pattern)
        {
            if (pattern == null || pattern == "*" || pattern == "") return true;
            string s = ToFileString(PrintOptions.VERBOSE_FOR_MATCHING);
            if (pattern.Contains(s)) return true;
            return Regex.Matches(s, pattern).Count > 0;
        }

        public void AddPrecondition(ConversationCondition info)
        {
            if (Preconds == null) Preconds = new List<ConversationCondition>();
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
        XmlNode Category { get; }
        Node GraphmasterNode { get; set; }
        string Filename { get; }
        
        void SetCategoryTag(Unifiable categoryPath, PatternInfo patternInfo, CategoryInfo categoryInfo, XmlNode outerNode, XmlNode templateNode, GuardInfo guard, ThatInfo thatInfo);
        bool Matches(string match);
        void AddPrecondition(ThatInfo node);
        void AddPrecondition(ConversationCondition node);
        void AddTemplate(TemplateInfo newTemplateInfo);
    }
}