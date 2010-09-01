using System;
using System.Collections.Generic;
using System.Text.RegularExpressions;
using System.Xml;

namespace RTParser.Utils
{
    [Serializable]
    public class CategoryInfo : GraphLinkInfo, IAIMLInfo
    {

        public bool IsDisabled { get; set; }

        public XmlNode Category
        {
            get { return srcNode; }
        }

        public TemplateInfo Template;

        public XmlNode TemplateXml
        {
            get { return AIMLLoader.FindNode("template", Category, null); }
        }

        public TopicInfo Topic;

        public XmlNode TopicXml
        {
            get { return AIMLLoader.FindNodeOrHigher("topic", Category, null); }
        }

        public ThatInfo That;

        public XmlNode ThatXml
        {
            get { return AIMLLoader.FindNodeOrHigher("that", Category, null); }
        }

        public PatternInfo Pattern;
        // public GuardInfo Guard;
        public string Filename;
        public List<XmlNode> Preconds;
        //public List<TemplateInfo> TemplateInfos = new List<TemplateInfo>();
        //private object node;

        public CategoryInfo(PatternInfo pattern, XmlNode cateNode, LoaderOptions options)
            : base(cateNode)
        {
            Pattern = pattern;
            Filename = options.CurrentFilename;
        }

        public override string ToString()
        {
            return Category.OuterXml + " " + AIMLLoader.LocationEscapedInfo(Category);
        }

        public string XMLInfo()
        {
            return Category.OuterXml;
        }

        public void AddTemplate(TemplateInfo templateInfo)
        {
            if (Template != null && Template != templateInfo) throw new InvalidCastException("non null " + Template);
            Template = templateInfo;
          //  TemplateInfos.Add(templateInfo);
        }

        public static CategoryInfo GetCategoryInfo(PatternInfo info, XmlNode node, LoaderOptions filename)
        {
            return filename.CtxGraph.FindCategoryInfo(info, node, filename);
        }

        public static CategoryInfo MakeCategoryInfo(PatternInfo info, XmlNode node, LoaderOptions filename)
        {
            if (NoInfo) return null;
            return new CategoryInfo(info, node, filename);
        }

        internal void Check()
        {
            throw new NotImplementedException();
        }

        #region IAIMLInfo Members

        string IAIMLInfo.SourceInfo()
        {
            return AIMLLoader.LocationInfo(Category);
        }

        public GraphMaster Graph
        {
            get { return Pattern.GraphmasterNode.Graph; }
        }

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
                if (graphName!=printOptions.CurrentGraphName)
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
            var topic1 = this.TopicXml;
            bool hasTopic = topic1 != null;
            if (hasTopic)
            {
                s += "<topic name=\"";
                var n = AIMLTagHandler.GetAttribValue(topic1, "name", () => (string)null, null);
                s += n;
                s += "\">";
            }
            else if (Topic != null && !Topic.IsCatchAll)
            {
                hasTopic = true;
                s += "<topic name=\"";
                var n = (string) Topic.FullPath;
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
            return s;
        }
        #endregion
        public bool Matches(string pattern)
        {
            if (pattern == null || pattern == "*" || pattern == "") return true;
            string s = ToFileString(PrintOptions.VERBOSE_FOR_MATCHING);
            if (pattern.Contains(s)) return true;
            return Regex.Matches(s, pattern).Count > 0;
        }

        public void AddPrecondition(XmlNode info)
        {
            if (Preconds==null) Preconds=new List<XmlNode>();
            Preconds.Add(info);
        }
        public void AddPrecondition(ThatInfo info)
        {
            AddPrecondition(info.PatternNode);
        }

        public void SetCategoryTag(Unifiable generatedPath, PatternInfo patternInfo, CategoryInfo category, XmlNode outerNode, XmlNode templateNode, GuardInfo guard, ThatInfo thatInfo)
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
}