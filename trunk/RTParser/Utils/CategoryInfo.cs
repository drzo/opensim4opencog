using System;
using System.Collections.Generic;
using System.Text.RegularExpressions;
using System.Xml;

namespace RTParser.Utils
{
    [Serializable]
    public class CategoryInfo : GraphLinkInfo, IAIMLInfo
    {
        public XmlNode Category
        {
            get { return srcNode; }
        }

        public XmlNode Template
        {
            get { return AIMLLoader.FindNode("template", Category, null); }
        }

        public XmlNode Topic
        {
            get { return AIMLLoader.FindNodeOrHigher("topic", Category, null); }
        }

        public XmlNode That
        {
            get { return AIMLLoader.FindNodeOrHigher("that", Category, null); }
        }

        public PatternInfo Pattern;
        // public GuardInfo Guard;
        public string Filename;
        public List<TemplateInfo> TemplateInfos = new List<TemplateInfo>();
        private object node;

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
            TemplateInfos.Add(templateInfo);
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

        public string SourceInfo()
        {
            return AIMLLoader.LocationInfo(Category);
        }
        public string ToFileString()
        {
            if (XmlDocumentLineInfo.SkipXmlns && this.srcNode.Attributes != null) this.srcNode.Attributes.RemoveNamedItem("xmlns");
            string s = "";
            var topic1 = this.Topic;
            bool hasTopic = topic1 != null;
            if (hasTopic)
            {
                s += "<topic name=\"";
                var n = AIMLTagHandler.GetAttribValue(topic1, "name", () => (string)null, null);
                s += n;
                s += "\">";
            }
            s += srcNode.OuterXml;
            if (hasTopic) s += "</topic>";
            return s;
        }

        public bool Matches(string pattern)
        {
            if (pattern == null || pattern == "*" || pattern == "") return true;
            string s = ToFileString();
            if (s.Contains(s)) return true;
            return Regex.Matches(s, pattern).Count > 0;
        }

        public void AddPrecondition(ThatInfo info)
        {

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