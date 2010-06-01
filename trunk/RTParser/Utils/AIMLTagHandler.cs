using System;
using System.IO;
using System.Text;
using System.Text.RegularExpressions;
using System.Xml;

namespace RTParser.Utils
{
    /// <summary>
    /// The template for all classes that handle the AIML tags found within template nodes of a
    /// category.
    /// </summary>
    abstract public class AIMLTagHandler : TextTransformer
    {
        public string ReduceStar(string name)
        {
            try
            {
                if (name.StartsWith("star_"))
                {
                    int i = Int32.Parse(name.Substring(5)) - 1;
                    name = query.InputStar[i];
                }
                if (name.StartsWith("that_"))
                {
                    int i = Int32.Parse(name.Substring(5)) - 1;
                    name = query.ThatStar[i + 1];
                }
                if (name.StartsWith("topic_"))
                {
                    int i = Int32.Parse(name.Substring(6)) - 1;
                    name = query.TopicStar[i];
                }
                if (name.StartsWith("guard_"))
                {
                    int i = Int32.Parse(name.Substring(6)) - 1;
                    name = query.GuardStar[i];
                }
            }
            catch (Exception e)
            {
                Console.WriteLine("" + e);
            }
            return name;
        }


        public static bool IsPredMatch(Unifiable value, Unifiable actualValue)
        {
            value = value.Trim();
            actualValue = actualValue.Trim();
            if (actualValue.IsMatch(value)) return true;
            Regex matcher = new Regex(value.AsString().Replace(" ", "\\s").Replace("*", "[\\sA-Z0-9]+"),
                                      RegexOptions.IgnoreCase);
            if (matcher.IsMatch(actualValue)) return true;
            if (MeansUnknown(value) && (MeansUnknown(actualValue))) return true;
            return false;
        }

        private static bool MeansUnknown(Unifiable unifiable)
        {
            if (Unifiable.IsNullOrEmpty(unifiable)) return true;
            string s = unifiable.AsString().ToLower();
            switch (s)
            {
                case "":
                    return true;
                case "unknown":
                    return true;
                case "nothing":
                    return true;
                case "*":
                    return true;
                case "_":
                    return true;
                case "undefined":
                    return true;
                default:
                    return false;
            }
        }

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
            if (this.templateNode.Attributes != null) this.templateNode.Attributes.RemoveNamedItem("xmlns");
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

        public AIMLTagHandler Parent;
        public void SetParent(AIMLTagHandler handler)
        {
            if (handler==this)
            {
                throw new InvalidOperationException("same");
            }
            Parent = handler;
        }

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
                        Unifiable found = Proc.processNode(childNode, query, request, result, user, this);
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
        public static LineInfoElement getNode(string outerXML)
        {
            var sr = new StringReader( outerXML);
            try
            {
                XmlDocumentLineInfo doc = new XmlDocumentLineInfo("From " + outerXML);
                doc.Load(sr);
                if (doc.ChildNodes.Count == 0)
                {
                    Console.WriteLine("NULL outerXML=" + outerXML);
                    return null;
                }
                if (doc.ChildNodes.Count != 1)
                {
                    Console.WriteLine("1 != outerXML=" + outerXML);
                }
                var temp = doc.FirstChild;
                if (temp is LineInfoElement)
                {
                    LineInfoElement li = (LineInfoElement)temp;
                    return (LineInfoElement)temp; //.FirstChild;}
                }
                return (LineInfoElement)temp; //.FirstChild;}
            }
            catch (Exception exception)            
            {
                Console.WriteLine("outerXML=" + outerXML);
                throw exception;
            }
        }
        public static LineInfoElement getNode(string outerXML, XmlNode templateNode)
        {
            var sr = new StringReader(outerXML);
            try
            {
                XmlDocumentLineInfo doc =
                    new XmlDocumentLineInfo("From '" + templateNode.OwnerDocument ?? " NODOC " + "' " + templateNode.OuterXml);
                doc.Load(sr);
                if (doc.ChildNodes.Count == 0)
                {
                    Console.WriteLine("NULL outerXML=" + outerXML);
                    return null;
                }
                if (doc.ChildNodes.Count != 1)
                {
                    Console.WriteLine("1 != outerXML=" + outerXML);
                }
                var temp = doc.FirstChild;
                if (temp is LineInfoElement)
                {
                    LineInfoElement li = (LineInfoElement)temp;
                    li.SetParentFromNode(templateNode);
                    return (LineInfoElement)temp; //.FirstChild;}
                }
                return (LineInfoElement)temp; //.FirstChild;}
            }
            catch (Exception exception)
            {
                Console.WriteLine("outerXML=" + outerXML);
                throw exception;
            }
        }
        public override string ToString()
        {
            return LineNumberTextInfo();
        }
        public string LineNumberTextInfo()
        {
            return LineTextInfo() + " " + LineNumberInfo();
        }
        public string LineTextInfo()
        {
            string s = templateNode.OuterXml.Trim();
            if (String.IsNullOrEmpty(s))
            {

                LineInfoElement li = (LineInfoElement) templateNode;
                s = s + " " + li.OwnerDocument.ToString();
                if (Parent != null && Parent != this)
                {
                    s = s + " " + Parent.LineTextInfo();
                }
                else
                {
                    return s;
                }

            }
            return s;
        }

        public string LineNumberInfo()
        {
            string s = "";
            if (templateNode is LineInfoElement)
            {
                LineInfoElement li = (LineInfoElement) templateNode;
                if (li.lineNumber == 0)
                {
                    s = s + " " + li.OwnerDocument.ToString();
                    if (Parent != null && Parent != this)
                    {
                        s = s + " " + Parent.LineNumberInfo();
                    }
                    else
                    {
                        s = s + " (" + li.lineNumber + ":" + li.linePosition + ")";
                    }
                }
                else
                {
                    s = s + " (" + li.OwnerDocument.ToString() + ":line " + li.lineNumber + "," + li.linePosition + ") ";
                }
            }
            else
            {
                return s;
            }
            return s;
        }


        /// <summary>
        /// Helper method that turns the passed Unifiable into an XML node
        /// </summary>
        /// <param name="outerXML">the Unifiable to XMLize</param>
        /// <returns>The XML node</returns>
        public virtual float CanUnify(Unifiable with)
        {
            string w = with.ToValue();
            Unifiable t1 = ProcessChange();
            float score1 = t1.Unify(with,query);
            if (score1 == 0) return score1;
            Unifiable t2 = CompleteProcess();
            float score2 = t2.Unify(with, query);
            if (score2 == 0) return score2;
            return (score1 < score2) ? score1 : score2;
        }

        #endregion

        protected string GetAttribValue(string attribName,string defaultIfEmpty)
        {
            return GetAttribValue(templateNode, attribName, defaultIfEmpty);
        }

        public string GetAttribValue(XmlNode node, string attribName, string defaultIfEmpty)
        {
            attribName = attribName.ToLower();
            foreach (XmlAttribute attrib in node.Attributes)
            {
                if (attrib.Name.ToLower() == attribName) return ReduceStar(attrib.Value);
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
                            childNode.InnerText = Proc.processNode(childNode, query, request, result, user, this);
                        }
                    }
                }
                return tagHandler.Transform();
            }
            else
            {
                Unifiable resultNodeInnerXML = tagHandler.Transform();
                XmlNode resultNode = getNode(String.Format("<node>{0}</node>", resultNodeInnerXML), templateNode);
                if (resultNode.HasChildNodes)
                {
                    Unifiable recursiveResult = Unifiable.CreateAppendable();
                    // recursively check
                    foreach (XmlNode childNode in resultNode.ChildNodes)
                    {
                        recursiveResult.Append(Proc.processNode(childNode, query, request, result, user, this));
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
