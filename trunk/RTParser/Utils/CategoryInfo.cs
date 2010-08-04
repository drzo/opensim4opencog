using System;
using System.Collections.Generic;
using System.IO;
using System.Text;
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
            var topic1 = this.Topic;
            bool hasTopic = topic1 != null;
            if (hasTopic)
            {
                s += "<topic name=\"";
                var n = AIMLTagHandler.GetAttribValue(topic1, "name", () => (string) null, null);
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

    public class PrintOptions
    {
        public PrintOptions()
        {
            //tw = new StringWriter(sw);
        }   
        public XmlWriterSettings XMLWriterSettings = new XmlWriterSettings();

        public string CurrentGraphName;

        public bool InsideAiml = false;

        public XmlWriter GetXMLTextWriter(TextWriter w)
        {
            lock (this)
            {
                if (w != lastW)
                {
                    lastW = w;
                    currentWriter = XmlWriter.Create(w, XMLWriterSettings);
                }
            }
            return currentWriter;
        }

        public bool RemoveDuplicates = true;       
        public bool CleanWhitepaces = true;
        public bool IncludeLineno = true;
        public bool CategoryPerLine = false;
        public bool IncludeGraphName = true;
        public List<object> dontPrint = new List<object>(); 
        public string InsideThat = null;
        public string InsideTopic = null;
        public Request RequestImplicits;
        readonly public StringBuilder sw = new StringBuilder();
        //readonly public StringWriter tw;

        public static PrintOptions CONSOLE_LISTING = new PrintOptions();
        public static PrintOptions VERBOSE_FOR_MATCHING = new PrintOptions();

        public static PrintOptions SAVE_TO_FILE = new PrintOptions()
                                                      {
                                                          CategoryPerLine = false,
                                                      };

        private XmlWriter currentWriter;
        private TextWriter lastW;
        public bool WriteDisabledItems = true;
        public bool WriteStatistics = true;

        internal string FormatXML(XmlNode srcNode)
        {
            lock (sw)
            {
                sw.Remove(0, sw.Length);
                //var tw = new StringWriter(sw);
                XMLWriterSettings.Encoding = Encoding.ASCII;
                XMLWriterSettings.CloseOutput = false;
                XMLWriterSettings.OmitXmlDeclaration = true;
                XMLWriterSettings.IndentChars = " ";// = true;
                XMLWriterSettings.Indent = true;

                var v = XmlWriter.Create(sw, XMLWriterSettings);
                try
                {
                    srcNode.WriteTo(v);
                    v.Flush();
                    var s = sw.ToString();
                    return s;
                }
                catch (Exception exception)
                {                    
                    throw exception;
                }
            }
        }

        private string hide = "";
        public bool DontPrint(object cws)
        {
            if (hide.Contains("" + cws)) return true;
            return false;
        }

        public void Writting(object cws)
        {
            if (hide.Length > 30000)
                hide = "" + cws;
            else
                hide += "" + cws;
        }

        public void ClearHistory()
        {
            InsideAiml = false;
            InsideTopic = null;
            InsideThat = null;
            dontPrint.Clear();
            sw.Length = 0;
            sw.Capacity = 100000;
            hide = "";
        }
    }
}