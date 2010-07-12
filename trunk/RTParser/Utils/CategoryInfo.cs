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
            get { return AIMLLoader.FindNode("template", Category, null); }
        }

        public XmlNode Topic
        {
            get { return AIMLLoader.FindNode("topic", Category, null); }
        }

        public XmlNode That
        {
            get { return AIMLLoader.FindNode("that", Category, null); }
        }

        public PatternInfo Pattern;
       // public GuardInfo Guard;
        public LoaderOptions Filename;
        public List<TemplateInfo> TemplateInfos = new List<TemplateInfo>();
        public CategoryInfo(PatternInfo pattern, XmlNode cateNode, LoaderOptions filename)
            : base(cateNode)
        {
            Pattern = pattern;
            Filename = filename;
        }

        public override string ToString()
        {
            return Category.OuterXml + " " + AIMLLoader.LineNumberInfo(Category);
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

        public static CategoryInfo MakeCategoryInfo(PatternInfo info, XmlNode node, LoaderOptions filename)
        {
            if (NoInfo) return null;
            return new CategoryInfo(info, node, filename);
        }

        internal void Check()
        {
            throw new NotImplementedException();
        }
    }
}