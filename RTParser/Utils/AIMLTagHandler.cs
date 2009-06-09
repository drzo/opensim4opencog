using System;
using System.Text;
using System.Xml;

namespace RTParser.Utils
{
    /// <summary>
    /// The template for all classes that handle the AIML tags found within template nodes of a
    /// category.
    /// </summary>
    abstract public class AIMLTagHandler : TextTransformer
    {

        protected Unifiable templateNodeInnerText
        {
            get { return templateNode.InnerText.Trim(); }
            set { templateNode.InnerText = value.Trim(); }
        }

        /// <summary>
        /// Ctor
        /// </summary>
        /// <param name="bot">The bot involved in this request</param>
        /// <param name="user">The user making the request</param>
        /// <param name="query">The query that originated this node</param>
        /// <param name="request">The request itself</param>
        /// <param name="result">The result to be passed back to the user</param>
        /// <param name="templateNode">The node to be processed</param>
        public AIMLTagHandler(RTParser.RTPBot bot,
                                    RTParser.User user,
                                    RTParser.Utils.SubQuery query,
                                    RTParser.Request request,
                                    RTParser.Result result,
                                    XmlNode templateNode)
            : base(bot, templateNode.OuterXml)
        {
            this.user = user;
            this.query = query;
            this.request = request;
            this.result = result;
            this.templateNode = templateNode;
            this.templateNode.Attributes.RemoveNamedItem("xmlns");
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
        /// A representation of the user who made the request
        /// </summary>
        public RTParser.User user;

        /// <summary>
        /// The query that produced this node containing the wildcard matches
        /// </summary>
        public RTParser.Utils.SubQuery query;

        /// <summary>
        /// A representation of the input into the Proc made by the user
        /// </summary>
        public RTParser.Request request;

        /// <summary>
        /// A representation of the result to be returned to the user
        /// </summary>
        public RTParser.Result result;

        /// <summary>
        /// The template node to be processed by the class
        /// </summary>
        public XmlNode templateNode;
        protected Unifiable Recurse()
        {
            Unifiable templateResult = Unifiable.CreateAppendable();
            if (this.templateNode.HasChildNodes)
            {
                // recursively check
                foreach (XmlNode childNode in this.templateNode.ChildNodes)
                {
                    if (childNode.NodeType == XmlNodeType.Text)
                    {
                        templateResult.Append(childNode.InnerText);
                    }
                    else
                    {
                        Unifiable found = Proc.processNode(childNode, query, request, result, user);
                        if (Unifiable.IsFalse(found))
                        {
                        //    return Unifiable.Empty;
                        }
                        templateResult.Append(found);
                    }
                }
                templateNodeInnerText = templateResult;//.ToString();
                return templateResult;
            }
            else
            {
                Unifiable before = Unifiable.InnerXmlText(this.templateNode);//.InnerXml;               
                return before;                
            }

        }


        #region Helper methods

        /// <summary>
        /// Helper method that turns the passed Unifiable into an XML node
        /// </summary>
        /// <param name="outerXML">the Unifiable to XMLize</param>
        /// <returns>The XML node</returns>
        public static XmlNode getNode(string outerXML)
        {
            XmlDocument temp = new XmlDocument();
            temp.LoadXml(outerXML);
            return temp.FirstChild;
        }



        /// <summary>
        /// Helper method that turns the passed Unifiable into an XML node
        /// </summary>
        /// <param name="outerXML">the Unifiable to XMLize</param>
        /// <returns>The XML node</returns>
        public virtual bool CanUnify(Unifiable with)
        {
            string w = with.ToValue();
            Unifiable t1 = ProcessChange();
            if (t1.Unify(with,query)) return true;
            Unifiable t2 = CompleteProcess();
            if (t2.Unify(with,query)) return true;
            return false;
        }

        #endregion

        protected Unifiable GetAttribValue(string attribName,Unifiable defaultIfEmpty)
        {
            attribName = attribName.ToLower();
            foreach (XmlAttribute attrib in this.templateNode.Attributes)
            {
                if (attrib.Name.ToLower() == attribName) return attrib.Value;
            }
            return defaultIfEmpty;
        }


        public virtual Unifiable CompleteProcess()
        {
            AIMLTagHandler tagHandler = this;
            XmlNode node = templateNode;
            if (tagHandler.isRecursive)
            {
                if (node.HasChildNodes)
                {
                    // recursively check
                    foreach (XmlNode childNode in node.ChildNodes)
                    {
                        if (childNode.NodeType != XmlNodeType.Text)
                        {
                            childNode.InnerText = Proc.processNode(childNode, query, request, result, user);
                        }
                    }
                }
                return tagHandler.Transform();
            }
            else
            {
                Unifiable resultNodeInnerXML = tagHandler.Transform();
                XmlNode resultNode = getNode(String.Format("<node>{0}</node>", resultNodeInnerXML));
                if (resultNode.HasChildNodes)
                {
                    Unifiable recursiveResult = Unifiable.CreateAppendable();
                    // recursively check
                    foreach (XmlNode childNode in resultNode.ChildNodes)
                    {
                        recursiveResult.Append(Proc.processNode(childNode, query, request, result, user));
                    }
                    return recursiveResult;//.ToString();
                }
                else
                {
                    return resultNode.InnerXml;
                }
            }
        }
    }
}
