using System;
using System.Text;
using System.Xml;
using AltAIMLbot.AIMLTagHandlers;
using AltAIMLParser;
using MushDLR223.Utilities;
using RTParser;
using RTParser.Variables;
using TextPatternUtils = RTParser.Utils.TextPatternUtils;

namespace AltAIMLbot.Utils 
{
    /// <summary>
    /// The template for all classes that handle the AIML tags found within template nodes of a
    /// category.
    /// </summary>
    abstract public class AIMLTagHandler : TextTransformer
    { 
        /// <summary>
        /// Ctor
        /// </summary>
        /// <param name="bot">The bot involved in this request</param>
        /// <param name="user">The user making the request</param>
        /// <param name="query">The query that originated this node</param>
        /// <param name="request">The request itself</param>
        /// <param name="result">The result to be passed back to the user</param>
        /// <param name="templateNode">The node to be processed</param>
        public AIMLTagHandler   (   AltBot bot, 
                                    User user, 
                                    AltAIMLbot.Utils.SubQuery query,
                                    Request request, 
                                    AltAIMLbot.Result result, 
                                    XmlNode templateNode) :base(bot,templateNode.OuterXml)
        {
            this.user = user;
            this.query = query;
            this.request = request;
            this.result = result;
            this.templateNode = templateNode;
            if (templateNode is XmlElement) TemplateNodeAttributes.RemoveNamedItem("xmlns");
        }

        /// <summary>
        /// Default ctor to use when late binding
        /// </summary>
        public AIMLTagHandler()
        {
        }

        /// <summary>
        /// A flag to denote if inner tags are to be processed recursively before processing this tag
        /// </summary>
        public bool isRecursive = true;

        /// <summary>
        /// A flag to denote if inner tags are to be processed recursively before processing this tag
        /// </summary>
        public bool IsStarAtomically = false;

        public bool IsStillStarAtomically
        {
            get
            {
                if (!IsStarAtomically) return false;
                if (!templateNode.InnerXml.Contains("<"))
                {
                    return false;
                }
                return true;
            }
        }
        /// <summary>
        /// A flag to denote if just the innertext should be used instead of full recrusive processing
        /// </summary>
        public bool isBoring = false;
        
        /// <summary>
        /// A representation of the user who made the request
        /// </summary>
        public User user { get; set; }

        /// <summary>
        /// The query that produced this node containing the wildcard matches
        /// </summary>
        public AltAIMLbot.Utils.SubQuery query;

        /// <summary>
        /// A representation of the input into the bot made by the user
        /// </summary>
        public Request request { get; set; }

        /// <summary>
        /// A representation of the result to be returned to the user
        /// </summary>
        public AltAIMLbot.Result result { get; set; }

        /// <summary>
        /// The template node to be processed by the class
        /// </summary>
        public XmlNode templateNode;

        #region Helper methods

        /// <summary>
        /// Helper method that turns the passed string into an XML node
        /// </summary>
        /// <param name="outerXML">the string to XMLize</param>
        /// <returns>The XML node</returns>
        public static XmlNode getNode(string outerXML)
        {
            XmlDocument temp = new XmlDocument/*LineInfo*/();
            temp.PreserveWhitespace = true;
            temp.LoadXml(outerXML);
            return temp.FirstChild;
        }

        public string GetAttribValue(string attributeName, string otherwise)
        {
            return GetAttribValue<string>(attributeName, otherwise);
        }
        public T GetAttribValue<T>(string attributeName, T otherwise) where T : IConvertible
        {
            return GetAttribValue(templateNode, attributeName, otherwise);
        }
        public T GetAttribValue<T>(string attribName, Func<T> defaultIfEmpty) where T : IConvertible
        {
            return StaticXMLUtils.GetAttribValue(templateNode, attribName, defaultIfEmpty, null);
        }

        public static T GetAttribValue<T>(XmlNode templateNode, string attribName, T defaultIfEmpty) where T : IConvertible
        {
            return StaticXMLUtils.GetAttribValue(templateNode, attribName, () => (defaultIfEmpty), null);
        }
        protected string TemplateNodeName
        {
            get { return templateNode.Name.ToLower(); }
        }
        protected XmlAttributeCollection TemplateNodeAttributes
        {
            get { return templateNode.Attributes; }
        }
        
        protected bool TemplateNodeHasText
        {
            get { return TemplateNodeInnerText.Length > 0; }
        }

        protected string RecurseStar()
        {
            this.TemplateNodeInnerText = GetStarContent();
            if (this.TemplateNodeHasText)
            {
                return this.Transform();
            }
            else
            {
                return string.Empty;
            }
        }

        public string GetStarContent()
        {
            XmlNode starNode = Utils.AIMLTagHandler.getNode("<star/>");
            star recursiveStar = new star(this.bot, this.user, this.query, this.request, this.result, starNode);
            return recursiveStar.Transform();
        }

        protected string TemplateNodeInnerXml
        {
            get { return templateNode.InnerXml; }
        }

        protected string TemplateNodeOuterXml
        {
            get { return templateNode.OuterXml; }
        }
        protected string TemplateNodeInnerText
        {
            get
            {
                return templateNode.InnerText;
            }
            set
            {
                templateNode.InnerText = value;
            }
        }

        public virtual bool isVerbatum
        {
            get { return false; }
        }
        public override bool isFormatter
        {
            get { return false; }
        }

        #endregion
        internal static string GetNameOfDict(SubQuery query, string dictName, XmlNode templateNode, out ISettingsDictionary dict)
        {
            string type = TextPatternUtils.ToLower(dictName);
            //ISettingsDictionary udict = query.GetDictionary(type, templateNode, dict);
            while (templateNode != null)
            {
                string type0 = TextPatternUtils.GetAttribValue(templateNode, "type,dict", null);
                if (type0 != null)
                {
                    type = type0;
                    break;
                }
                string uname = TextPatternUtils.GetAttribValue(templateNode, "user", null);
                if (uname != null)
                {
                    type0 = TextPatternUtils.GetNamedType("user", uname);

                    if (type0 != null)
                    {
                        type = type0;
                        break;
                    }
                }
                string bname = TextPatternUtils.GetAttribValue(templateNode, "bot", null);
                if (bname != null)
                {
                    type0 = TextPatternUtils.GetNamedType("bot", bname);
                    if (type0 != null)
                    {
                        type = type0;
                        break;
                    }
                }
                dict = query.Request.GetDictionary(templateNode.LocalName);
                if (dict != null)
                {
                    type = dict.NameSpace;
                    break;
                }
                templateNode = templateNode.ParentNode;
            }
            if (type == null) type = dictName;
            bool preferBotOverUser = (type == "bot");

            if (preferBotOverUser)
            {
                dict = query.Request.Responder.Predicates;
            }
            else
            {
                dict = query.Request.GetDictionary(type);
            }
            if (dict == null) dict = query.Request.TargetSettings;
            return type ?? dict.NameSpace;
        }

        protected void writeToLogError(string s)
        {
            writeToLog("Error! " + s);
        }
        protected void writeToLog(string s)
        {
            this.bot.writeToLog("" + s + " in " + templateNode.OuterXml);
        }
        protected bool CheckNode(string s)
        {
            return s.Contains(TemplateNodeName);
        }
        public override string ToString()
        {
            if (templateNode.OuterXml != initialString)
            {
                return base.ToString() + " => " + templateNode.OuterXml;
            }
            return GetType().Name + ": " + templateNode.OuterXml;
        }

        protected override Unifiable ProcessChangeU()
        {
            return TransformU();
        }
    }
}
