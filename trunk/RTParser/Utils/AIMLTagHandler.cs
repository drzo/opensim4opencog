using System;
using System.Collections.Generic;
using System.IO;
using System.Text;
using System.Text.RegularExpressions;
using System.Xml;
using RTParser.AIMLTagHandlers;

namespace RTParser.Utils
{
    /// <summary>
    /// The template for all classes that handle the AIML tags found within template nodes of a
    /// category.
    /// </summary>
    abstract public class AIMLTagHandler : TextTransformer, IXmlLineInfo
    {
        protected void Succeed()
        {
            if (query != null && query.CurrentTemplate != null)
            {
                string type = GetType().Name;
                double defualtReward = query.GetSucceedReward(type);
                double score = GetAttribValue(templateNode, "score", defualtReward , query);
                writeToLog("TSCORE {3} {0}*{1}->{2} ",
                           score, query.CurrentTemplate.Rating,
                           query.CurrentTemplate.Rating *= score, score);
            }
        }

        public bool WhenTrue(Unifiable unifiable)
        {
            if (!Unifiable.IsNullOrEmpty(unifiable))
            {
                if (Unifiable.IsFalse(unifiable)) return false;
                if (!Unifiable.IsTrue(unifiable))
                {
                    writeToLog("DEBUG: !WhenTrue " + unifiable);
                }
                templateNodeInnerText = unifiable;
                return true;
            }
            return false;
        }


        internal TemplateInfo templateInfo;
        public TemplateInfo GetTemplateInfo()
        {
            if (templateInfo == null)
            {
                if (query != null)
                {
                    templateInfo = query.CurrentTemplate;
                }
                if (templateInfo != null) return templateInfo;
                if (Parent != null)
                {
                    templateInfo = Parent.GetTemplateInfo();
                    if (templateInfo != null) return templateInfo;
                }
            }
            return templateInfo;
        }

        static public Unifiable ReduceStar(string name, SubQuery query)
        {
            try
            {
                if (name.StartsWith("star_"))
                {
                    int i = Int32.Parse(name.Substring(5));
                    name = GetDictData(query.InputStar, i);
                }
                if (name.StartsWith("that_"))
                {
                    int i = Int32.Parse(name.Substring(5)) + 1;
                    name = GetDictData(query.ThatStar, i);
                }
                if (name.StartsWith("thatstar_"))
                {
                    int i = Int32.Parse(name.Substring(9)) + 1;
                    name = GetDictData(query.ThatStar, i);
                }
                if (name.StartsWith("topicstar_"))
                {
                    int i = Int32.Parse(name.Substring(10));
                    name = GetDictData(query.TopicStar, i);
                }
                if (name.StartsWith("topic_"))
                {
                    int i = Int32.Parse(name.Substring(6));
                    name = GetDictData(query.TopicStar, i);
                }
                if (name.StartsWith("guard_"))
                {
                    int i = Int32.Parse(name.Substring(6));
                    name = GetDictData(query.GuardStar, i);
                }
            }
            catch (Exception e)
            {
                RTPBot.writeDebugLine("" + e);
            }
            return name;
        }

        private static string GetDictData(List<Unifiable> unifiables, int i)
        {
            int ii = i - 1;
            int uc = unifiables.Count;
            if (uc == 0)
            {
                RTPBot.writeDebugLine(" !ERROR -star underflow! " + i + "- ");
                return String.Empty;
            }
            if (ii > uc)
            {
                return unifiables[ii];
            }
            if (ii < 0)
            {
                ii = 0;
            }
            return unifiables[ii];
        }


        public static bool IsPredMatch(Unifiable value, Unifiable actualValue, SubQuery subquery)
        {
            value = value.Trim();
            actualValue = actualValue.Trim();
            if (actualValue.WillUnify(value, subquery)) return true;
            Regex matcher = new Regex(value.AsString().Replace(" ", "\\s").Replace("*", "[\\sA-Z0-9]+"),
                                      RegexOptions.IgnoreCase);
            if (matcher.IsMatch(actualValue)) return true;
            if (MeansUnknown(value) && (MeansUnknown(actualValue))) return true;
            return false;
        }

        public static bool MeansUnknown(Unifiable unifiable)
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
            get
            {
                string s2;
                if (!Unifiable.IsNull(RecurseResult))
                {
                    s2 = RecurseResult;
                }
                else
                {
                    s2 = Recurse();                    
                }
                string s0 = templateNode.InnerXml.Trim();
                string s1 = Unifiable.InnerXmlText(templateNode);
                string sr = s1;
                return CheckValue(sr);
            }

            set
            {
                if (AIMLLoader.ContainsAiml(value))
                {
                    writeToLogWarn("ContainsAiml = " + value);
                }
                if (Unifiable.IsNullOrEmpty(value))
                {
                    writeToLog("ERROR ?!?! templateNodeInnerText = " + value);
                }
                templateNode.InnerText = CheckValue(value);
            }
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
            this._request = request;
            this.result = result;
            this.templateNode = templateNode;
            inputString = templateNode.OuterXml;
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
        public RTParser.Request _request;
        public RTParser.Request request
        {
            get
            {
                if (query!=null)
                {
                    if (_request==query.Request)
                    {
                        return _request;
                    }
                    return query.Request;
                }
                return _request;
            }
            set
            {
                _request = value; 
            }
        }

        /// <summary>
        /// A representation of the result to be returned to the user
        /// </summary>
        private RTParser.Result _result0;
        public RTParser.Result result
        {
            get
            {
                if (query != null)
                {
                    if (_result0 == query.Result)
                    {
                        return _result0;
                    } else
                    {
                        return _result0;
                    }
                    return query.Result;
                }
                return _result0;
            }
            set
            {
                _result0 = value;
            }
        }

        /// <summary>
        /// The template node to be processed by the class
        /// </summary>
        public XmlNode templateNode;

        public AIMLTagHandler Parent;
        public void SetParent(AIMLTagHandler handler)
        {
            if (handler == this)
            {
                throw new InvalidOperationException("same");
            }
            else if (handler == null)
            {
                //throw new InvalidOperationException("no ParentResult handler");                
            }
            Parent = handler;
        }

        protected Unifiable RecurseResult = Unifiable.NULL;
        protected Unifiable Recurse()
        {
            Unifiable templateNodeInnerText;//= this.templateNodeInnerText;
            Unifiable templateResult = Unifiable.CreateAppendable();
            if (this.templateNode.HasChildNodes)
            {
                // recursively check
                foreach (XmlNode childNode in this.templateNode.ChildNodes)
                {
                    if (childNode.NodeType == XmlNodeType.Text)
                    {
                        templateResult.Append(childNode.InnerText.Trim());
                    }
                    else
                    {
                        Unifiable found = Proc.processNode(childNode, query, request, result, user, this);
                        if (Unifiable.IsFalse(found))
                        {
                            // until it beomes true once ?   return Unifiable.Empty;
                        }
                        templateResult.Append(found);
                    }
                }
                //templateNodeInnerText = templateResult;//.ToString();
                if (!templateResult.IsEmpty)
                {
                    templateResult = CheckValue(templateResult);
                    RecurseResult = templateResult;
                }
                return templateResult;
            }
            else
            {
                Unifiable before = Unifiable.InnerXmlText(this.templateNode);//.InnerXml;        
                return CheckValue(before);
            }

        }


        #region Helper methods


        public Unifiable CheckValue(Unifiable value)
        {
            if (Object.ReferenceEquals(value, Unifiable.Empty)) return value;
            if (value == null)
            {
                RTPBot.writeDebugLine("ERROR " + value + " NULL");
                return null;
            }
            else
            {
                if (Unifiable.IsNullOrEmpty(value))
                {
                    return Unifiable.Empty;
                    writeToLog("ERROR ?!?! templateNodeInnerText = " + value);
                }
                string v = value.AsString();
                if (v.Contains("&"))
                {
                    RTPBot.writeDebugLine("!@ERROR BAD INPUT? " + value);
                }
                return value;
            }
        }

        /// <summary>
        /// Helper method that turns the passed Unifiable into an XML node
        /// </summary>
        /// <param name="outerXML">the Unifiable to XMLize</param>
        /// <returns>The XML node</returns>
        public static LineInfoElement getNode(string outerXML)
        {
            var sr = new StringReader(outerXML);
            try
            {
                XmlDocumentLineInfo doc = new XmlDocumentLineInfo("From " + outerXML);
                doc.Load(sr);
                if (doc.ChildNodes.Count == 0)
                {
                    RTPBot.writeDebugLine("NULL outerXML=" + outerXML);
                    return null;
                }
                if (doc.ChildNodes.Count != 1)
                {
                    RTPBot.writeDebugLine("1 != outerXML=" + outerXML);
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
                RTPBot.writeDebugLine("outerXML=" + outerXML);
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
                    RTPBot.writeDebugLine("NULL outerXML=" + outerXML);
                    return null;
                }
                if (doc.ChildNodes.Count != 1)
                {
                    RTPBot.writeDebugLine("1 != outerXML=" + outerXML);
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
                RTPBot.writeDebugLine("outerXML=" + outerXML);
                throw exception;
            }
        }
        public override string ToString()
        {
            return LineNumberTextInfo();
        }
        public string LineNumberTextInfo()
        {
            string s = LineTextInfo() + " " + LineNumberInfo();
            if (!s.Contains(initialString))
            {
                return "-WAS- '" + inputString + "' => " + s;
            }
            return s;
        }
        public string LineTextInfo()
        {
            string s = templateNode.OuterXml.Trim();
            if (String.IsNullOrEmpty(s))
            {
                LineInfoElement li = (LineInfoElement)templateNode;
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
            string s = "<!--";
            if (templateNode is LineInfoElement)
            {
                LineInfoElement li = (LineInfoElement)templateNode;
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
            return s + "-->";
        }


        /// <summary>
        /// Helper method that turns the passed Unifiable into an XML node
        /// </summary>
        /// <param name="outerXML">the Unifiable to XMLize</param>
        /// <returns>The XML node</returns>
        public virtual float CanUnify(Unifiable with)
        {
            string w = with.ToValue(query);
            Unifiable t1 = ProcessChange();
            float score1 = t1.Unify(with, query);
            if (score1 == 0) return score1;
            Unifiable t2 = CompleteProcess();
            float score2 = t2.Unify(with, query);
            if (score2 == 0) return score2;
            return (score1 < score2) ? score1 : score2;
        }

        #endregion

        protected Unifiable GetAttribValue(string attribName, Unifiable defaultIfEmpty)
        {
            return GetAttribValue(templateNode, attribName, defaultIfEmpty, query);
        }

        static public Unifiable GetAttribValue(XmlNode node, string attribName, Unifiable defaultIfEmpty, SubQuery sq)
        {
            attribName = attribName.ToLower();
            foreach (XmlAttribute attrib in node.Attributes)
            {
                if (attrib.Name.ToLower() == attribName) return ReduceStar(attrib.Value, sq);
            }
            return defaultIfEmpty;
        }

        static public double GetAttribValue(XmlNode node, string attribName, double defaultIfEmpty, SubQuery sq)
        {
            attribName = attribName.ToLower();
            foreach (XmlAttribute attrib in node.Attributes)
            {
                if (attrib.Name.ToLower() == attribName)
                {
                    Unifiable reduceStar = ReduceStar(attrib.Value, sq);
                    try
                    {
                        return double.Parse(reduceStar);
                    }
                    catch (Exception exception)
                    {

                         RTPBot.writeDebugLine("AIMLTRACE: DECIMAL " + reduceStar + " " + exception);
                    }
                }
            }
            return defaultIfEmpty;
        }

        public virtual Unifiable CompleteProcess()
        {
            if (!Unifiable.IsNull(RecurseResult))
            {
                return RecurseResult;
            }
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
                            SaveResultOnChild(childNode, Proc.processNode(childNode, query, request, result, user, this));
                        }
                    }
                }
                var v = tagHandler.Transform();
                return CheckValue(v);
            }
            else
            {
                Unifiable resultNodeInnerXML = tagHandler.Transform();
                XmlNode resultNode = getNode(String.Format("<node>{0}</node>", Unifiable.ToVMString(resultNodeInnerXML)), templateNode);
                if (resultNode.HasChildNodes)
                {
                    Unifiable recursiveResult = Unifiable.CreateAppendable();
                    // recursively check
                    foreach (XmlNode childNode in resultNode.ChildNodes)
                    {
                        recursiveResult.Append(Proc.processNode(childNode, query, request, result, user, this));
                    }
                    if (!recursiveResult.IsEmpty)
                    {
                        recursiveResult = CheckValue(recursiveResult);
                        RecurseResult = recursiveResult;
                    }
                    return recursiveResult;//.ToString();
                }
                else
                {
                    Unifiable before = Unifiable.InnerXmlText(resultNode);//.InnerXml;        
                    return CheckValue(before);
                }
            }
        }

        public void SaveResultOnChild(XmlNode node, string value)
        {
            if (value == null) return;
            if (value == "") return;
            RTPBot.writeDebugLine("-!SaveResultOnChild AIMLTRACE " + value + " -> " + node.OuterXml);
            node.InnerXml = CheckValue(value);
        }

        protected void writeToLogWarn(string unifiable, params object[] objs)
        {
            writeToLog("WARNING: " + unifiable, objs);
        }

        public virtual void writeToLog(string unifiable, params object[] objs)
        {
            if (unifiable.ToUpper().StartsWith("ERROR"))
            {
                writeToLogWarn("BAD " + unifiable, objs);
                return;
            }
            this.Proc.writeToLog("AIMLTRACE: " + unifiable + " in " + GetType().Name + "  " + LineNumberTextInfo(), objs);
        }

        #region Implementation of IXmlLineInfo

        /// <summary>
        /// Gets a value indicating whether the class can return line information.
        /// </summary>
        /// <returns>
        /// true if <see cref="P:System.Xml.IXmlLineInfo.LineNumber"/> and <see cref="P:System.Xml.IXmlLineInfo.LinePosition"/> can be provided; otherwise, false.
        /// </returns>
        public bool HasLineInfo()
        {
            if (templateNode is IXmlLineInfo) return true;
            if (Parent != null && Parent != this) return Parent.HasLineInfo();
            return false;
        }

        /// <summary>
        /// Gets the current line number.
        /// </summary>
        /// <returns>
        /// The current line number or 0 if no line information is available (for example, <see cref="M:System.Xml.IXmlLineInfo.HasLineInfo"/> returns false).
        /// </returns>
        public int LineNumber
        {
            get
            {
                IXmlLineInfo ix = (templateNode as IXmlLineInfo) ?? Parent ?? this;
                if (ix != this) return ix.LineNumber;
                //throw new NotImplementedException();
                return 0;
            }
        }

        /// <summary>
        /// Gets the current line position.
        /// </summary>
        /// <returns>
        /// The current line position or 0 if no line information is available (for example, <see cref="M:System.Xml.IXmlLineInfo.HasLineInfo"/> returns false).
        /// </returns>
        public int LinePosition
        {
            get
            {
                IXmlLineInfo ix = (templateNode as IXmlLineInfo) ?? Parent ?? this;
                if (ix != this) return ix.LinePosition;
                return 0;
            }
        }
        #endregion

        protected bool CheckNode(string name)
        {
            if (this.templateNode.Name.ToLower() == name) return true;
            writeToLog("CheckNode change " + name + " -> " + templateNode.Name);
            return true;
        }    
    }
}
