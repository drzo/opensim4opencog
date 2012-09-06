using System;
using System.Text;
using System.Xml;
using AltAIMLbot.AIMLTagHandlers;
using MushDLR223.Utilities;

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
        public AIMLTagHandler   (   AltAIMLbot.AltBot bot, 
                                    AltAIMLbot.User user, 
                                    AltAIMLbot.Utils.SubQuery query,
                                    AltAIMLbot.Request request, 
                                    AltAIMLbot.Result result, 
                                    XmlNode templateNode) :base(bot,templateNode.OuterXml)
        {
            this.user = user;
            this.query = query;
            this.request = request;
            this.result = result;
            this.templateNode = templateNode;
            TemplateNodeAttributes.RemoveNamedItem("xmlns");
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
        public bool isNonAtomic = true;

        /// <summary>
        /// A representation of the user who made the request
        /// </summary>
        public AltAIMLbot.User user;

        /// <summary>
        /// The query that produced this node containing the wildcard matches
        /// </summary>
        public AltAIMLbot.Utils.SubQuery query;

        /// <summary>
        /// A representation of the input into the bot made by the user
        /// </summary>
        public AltAIMLbot.Request request;

        /// <summary>
        /// A representation of the result to be returned to the user
        /// </summary>
        public AltAIMLbot.Result result;

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
            XmlDocument temp = new XmlDocument();
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
            XmlNode starNode = Utils.AIMLTagHandler.getNode("<star/>");
            star recursiveStar = new star(this.bot, this.user, this.query, this.request, this.result, starNode);
            this.TemplateNodeInnerText0 = recursiveStar.Transform();
            if (this.TemplateNodeHasText)
            {
                return this.ProcessChange();
            }
            else
            {
                return string.Empty;
            }
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
                var ret = TemplateNodeInnerText0;
                if (TemplateNodeInnerXml.Contains("<") && innerTextOverride == null && !TemplateNodeInnerXml.Contains("star>"))
                {
                   // throw new NotSupportedException("Template Node XML");
                }
                return ret;
            }
        }
        protected string TemplateNodeInnerText0
        {
            get
            {
                if (innerTextOverride != null) return innerTextOverride;
                return templateNode.InnerText;
            }
            set
            {
                innerTextOverride = value;
                templateNode.InnerText = value;
            }
        }

        private string innerTextOverride = null;
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
    }
}
