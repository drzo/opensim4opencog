using System;
using System.Collections.Generic;
using System.Xml;

namespace RTParser.Utils
{
    [Serializable]
    public class CategoryInfo : GraphLinkInfo
    {
        public XmlNode Category
        {
            get { return srcNode; }
        }

        public XmlNode Template
        {
            get { return AIMLLoader.FindNode("template", Category); }
        }

        public XmlNode Topic
        {
            get { return AIMLLoader.FindNode("topic", Category); }
        }

        public XmlNode What
        {
            get { return AIMLLoader.FindNode("what", Category); }
        }

        public PatternInfo Pattern;
       // public GuardInfo Guard;
        public LoaderOptions Filename;
        public List<TemplateInfo> TemplateInfos = new List<TemplateInfo>();
        public CategoryInfo(PatternInfo pattern, XmlNode node, LoaderOptions filename)
            : base(node)
        {
            Pattern = pattern;
            Filename = filename;
        }

        public override string ToString()
        {
            return XMLInfo() + " " + Filename;
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
            return filename.Graph.FindCategoryInfo(info, node, filename);
        }
    }
}