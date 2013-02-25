using System;
using System.Text;
using System.Xml;
using AltAIMLbot.AIMLTagHandlers;
using AltAIMLParser;
using MushDLR223.Utilities;
using AltAIMLbot;
using AltAIMLbot.Variables;
using TextPatternUtils = AltAIMLbot.Utils.TextPatternUtils;

namespace AltAIMLbot.Utils 
{
    public abstract partial class AIMLTagHandlerB : TextTransformer
    {
        public AIMLTagHandlerB(AltBot bot, string instr, Unifiable inu)
        {
            this.bot = bot;
            inputStringU = inu ?? instr;
            inputString = initialString = instr ?? inputStringU.AsString();
        }

        protected AIMLTagHandlerB()
            : base()
        {

        }

        public abstract SubQuery query { get; set; }
        public abstract User user { get; set; }
        public abstract Request request { get; set; }
        public abstract Result result { get; set; }
        public abstract bool IsStillStarAtomically { get; }
    }
    /// <summary>
    /// The template for all classes that handle the AIML tags found within template nodes of a
    /// category.
    /// </summary>
    abstract public partial class AIMLTagHandler : AIMLTagHandlerB
    {
        public bool IsTraced { get; set; }

        /// <summary>
        /// A flag to denote if inner tags are to be processed recursively before processing this tag
        /// </summary>
        public bool isRecursive = true;

        /// <summary>
        /// A flag to denote if inner tags are to be processed recursively before processing this tag
        /// </summary>
        public bool IsStarAtomically = false;

        sealed override public bool IsStillStarAtomically
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
        override public User user { get; set; }

        /// <summary>
        /// The query that produced this node containing the wildcard matches
        /// </summary>
        override public AltAIMLbot.Utils.SubQuery query { get; set; }

        /// <summary>
        /// A representation of the input into the bot made by the user
        /// </summary>
        override public Request request { get; set; }

        /// <summary>
        /// A representation of the result to be returned to the user
        /// </summary>
        override public AltAIMLbot.Result result { get; set; }

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
        public static XmlNode getNode_S(string outerXML)
        {
            //XmlDocument temp = new XmlDocumentLineInfo();
            var temp = new XmlDocument();
            temp.PreserveWhitespace = true;
            temp.LoadXml(outerXML);
            return temp.FirstChild;
        }

        public string GetAttribValue_S(string attributeName, string otherwise)
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

        public static T GetAttribValue_S<T>(XmlNode templateNode, string attribName, T defaultIfEmpty) where T : IConvertible
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
            get { return ((string) Recurse()).Length > 0; }
        }

        protected string RecurseStar()
        {
            innerResult.Value = GetStarContent();
            if (this.TemplateNodeHasText)
            {
                return this.Transform();
            }
            else
            {
                return string.Empty;
            }
        }

        public Unifiable GetStarContent_S()
        {
            XmlNode starNode = getNode("<star/>");
            star recursiveStar = new star(this.bot, this.user, this.query, this.request, this.result, starNode);
            return ((TextTransformer) recursiveStar).Transform();
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
            get { return SelfProcessing || IsFormatter; }
        }
        public override bool IsFormatter
        {
            get { return false; }
        }

        #endregion
        internal static string GetNameOfDict_S(SubQuery query, string dictName, XmlNode templateNode, out ISettingsDictionary dict)
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
        protected bool CheckNode_S(string s)
        {
            return s.Contains(TemplateNodeName);
        }
        public  string ToString_S()
        {
            if (templateNode.OuterXml != initialString)
            {
                return base.ToString() + " => " + templateNode.OuterXml;
            }
            return GetType().Name + ": " + templateNode.OuterXml;
        }

        public bool SelfProcessing { get; set; }
        //public abstract Unifiable Recurse();
    }
}
