using System;
using System.Collections.Generic;
using System.IO;
using System.Text.RegularExpressions;
using System.Xml;
using RTParser.AIMLTagHandlers;
using RTParser.Variables;

namespace RTParser.Utils
{
    abstract public partial class AIMLTagHandler
    {
        public RTParser.Utils.SubQuery TheQuery
        {
            get
            {
                SubQuery ret = this.query 
                    //?? request.CurrentQuery ?? this.query ?? result.CurrentQuery
                    ;
                return ret;
            }
        }

        public Unifiable GetStarContent()
        {
            XmlNode starNode = Utils.AIMLTagHandler.getNode("<star/>", templateNode);
            LineInfoElement.unsetReadonly(starNode);
            star recursiveStar = new star(this.Proc, this.user, this.query, this.request, this.result, starNode);
            return recursiveStar.Transform();
        }

        public AIMLTagHandler Parent;
        public void SetParent(AIMLTagHandler handler)
        {
            if (handler == this)
            {
                throw new InvalidOperationException("same");
            }
            else if (handler == null)
            {
                //throw new InvalidOperationException("no parent handler");                
            }
            Parent = handler;
        }

        // This was from the original source of the AIML Tag
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


        /// <summary>
        /// The method that does the recursion of the text
        /// </summary>
        /// <returns>The resulting processed text</returns>
        protected Unifiable TransformAtomically(Func<Unifiable, Unifiable>
            afterEachOrNull, bool saveResultsOnChildren)
        {
            afterEachOrNull = afterEachOrNull ?? ((passthru) => passthru);

            // pre textualized ?
            if (!templateNodeInnerText.IsEmpty)
            {
                // non atomic version of the node
                return afterEachOrNull(templateNodeInnerText);
            }
            else
            {
                if (!templateNode.HasChildNodes)
                {
                    // atomic version of the node
                    Unifiable templateResult = GetStarContent();

                    var a = afterEachOrNull(templateResult);
                    if (saveResultsOnChildren)
                    {
                        templateNodeInnerText = a;
                        return a;
                    }
                    return a;
                }
                else
                {
                    // needs some recursion
                    StringAppendableUnifiable templateResult = Unifiable.CreateAppendable();
                    foreach (XmlNode childNode in templateNode.ChildNodes)
                    {
                        try
                        {
                            bool protectChildren = ReadOnly;

                            Unifiable part = ProcessChildNode(childNode, protectChildren, false);
                            
                            part = afterEachOrNull(part);
                            if (saveResultsOnChildren)
                            {
                                SaveResultOnChild(childNode, part);
                            }
                            templateResult.Append(part);
                        }
                        catch (Exception e)
                        {
                            RTPBot.writeDebugLine("ERROR: " + e);
                        }

                    }
                    return templateResult;
                }
            }
            return Unifiable.Empty;
        }

        public static int AttributesCount(XmlNode node, string s)
        {
            if (node.Attributes == null) return 0;
            int i = 0;
            string ss = "," + s + ",";
            foreach (XmlAttribute cn in node.Attributes)
            {
                if (ss.Contains("," + cn.Name + ","))
                {
                    i++;
                }
            }
            return i;
        }

        public string GetAttribValue(string attribName, string defaultIfEmpty)
        {
            return GetAttribValue(templateNode, attribName, () => defaultIfEmpty, query);
        }

        static public Unifiable GetAttribValue(XmlNode node, string attribName, Func<string> defaultIfEmpty, SubQuery sq)
        {
            bool found = false;
            Unifiable u = Unifiable.NULL;
            if (node.Attributes == null) return defaultIfEmpty();
            foreach (var nameS in attribName.Split(new[] { ',' }, StringSplitOptions.RemoveEmptyEntries))
            {
                String attribnym = nameS.ToLower();
                foreach (XmlAttribute attrib in node.Attributes)
                {
                    if (attrib.Name.ToLower() == attribnym)
                    {
                        found = true;
                        var r = ReduceStar(attrib.Value, sq, sq);
                        if (!Unifiable.IsNullOrEmpty(r)) return r;
                        if (Unifiable.IsNull(r)) continue;
                        u = r;
                    }
                }
            }
            if (found) return u;
            return defaultIfEmpty();
        }

        static public double GetAttribValue(XmlNode node, string attribName, double defaultIfEmpty, SubQuery sq)
        {
            attribName = attribName.ToLower();
            foreach (XmlAttribute attrib in node.Attributes)
            {
                if (attrib.Name.ToLower() == attribName)
                {
                    string reduceStar = "" + ReduceStar(attrib.Value, sq, sq);
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

        protected string GetAttribValue(IEnumerable<string> attribNames, Func<string> defaultIfEmpty)
        {
            string ifMissing = "MISSING";
            foreach (var name in attribNames)
            {
                string vv = GetAttribValue(templateNode, name, () => ifMissing, query);
                if (vv != ifMissing) return vv;
            }
            return defaultIfEmpty();
        }

        static public Unifiable ReduceStar(string name, SubQuery query, ISettingsDictionary dict)
        {
            try
            {
                if (name.StartsWith("star_"))
                {
                    int i = Int32.Parse(name.Substring(5));
                    name = GetDictData(query.InputStar, i);
                }
                else if (name.StartsWith("that_"))
                {
                    int i = Int32.Parse(name.Substring(5)) + 1;
                    name = GetDictData(query.ThatStar, i);
                }
                else if (name.StartsWith("thatstar_"))
                {
                    int i = Int32.Parse(name.Substring(9)) + 1;
                    name = GetDictData(query.ThatStar, i);
                }
                else if (name.StartsWith("topicstar_"))
                {
                    int i = Int32.Parse(name.Substring(10));
                    name = GetDictData(query.TopicStar, i);
                }
                else if (name.StartsWith("topic_"))
                {
                    int i = Int32.Parse(name.Substring(6));
                    name = GetDictData(query.TopicStar, i);
                }
                else if (name.StartsWith("guard_"))
                {
                    int i = Int32.Parse(name.Substring(6));
                    name = GetDictData(query.GuardStar, i);
                }
                else if (name.StartsWith("@"))
                {
                    Unifiable value = query.Request.TargetBot.SystemExecute(name, null, query.Request);
                    if (!Unifiable.IsNullOrEmpty(value)) return value;
                }
                else if (name.StartsWith("%dictvar_"))
                {
                    Unifiable value = dict.grabSetting(name.Substring(8));
                    if (!Unifiable.IsNullOrEmpty(value)) return value;
                }
            }
            catch (Exception e)
            {
                RTPBot.writeDebugLine("" + e);
            }
            return name;
        }

        private static Unifiable GetDictData(List<Unifiable> unifiables, int i)
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
                if (false)
                {
                    var sa = Unifiable.CreateAppendable();
                    foreach (var u in unifiables)
                    {
                        sa.Append(u);
                    }
                    return sa;
                }
                ii = 0;
            }
            return unifiables[ii];
        }


        public static bool IsPredMatch(Unifiable required, Unifiable actualValue, SubQuery subquery)
        {
            if (Unifiable.IsNull(required))
            {
                return Unifiable.IsNullOrEmpty(actualValue);
            }
            if (Unifiable.IsNull(actualValue))
            {
                return Unifiable.IsNullOrEmpty(required);
            }
            required = required.Trim();
            if (required.IsAnySingleUnit())
            {
                return !Unifiable.IsNullOrEmpty(actualValue);
            }

            actualValue = actualValue.Trim();
            if (actualValue.WillUnify(required, subquery))
            {
                return true;
            }
            string requiredAsStringReplaceReplace = required.AsString().Replace(" ", "\\s").Replace("*", "[\\sA-Z0-9]+");
            Regex matcher = new Regex("^"+requiredAsStringReplaceReplace+"$",
                                      RegexOptions.IgnoreCase);
            if (matcher.IsMatch(actualValue))
            {
                return true;
            }
            if (required.ToUpper() == "UNKNOWN" && (Unifiable.IsUnknown(actualValue))) return true;
            return false;
        }

        public virtual Unifiable CheckValue(Unifiable value)
        {
            if (Object.ReferenceEquals(value, Unifiable.Empty)) return value;
            if (value == null)
            {
                writeToLogWarn("ChackValue NULL");
                return null;
            }
            else
            {
                return value;
                if (Unifiable.IsNullOrEmpty(value))
                {
                    writeToLogWarn("CheckValue EMPTY = '" + value + "'");
                    return value;
                }
                string v = value.AsString();
                if (!value.AsString().Contains("<a href"))
                {
                    if (v.Contains("<"))
                    {
                        writeToLogWarn("CheckValue XML = '" + value + "'");
                    }
                    else if (v.Contains("&"))
                    {
                        writeToLogWarn("CheckValue HTML = '" + value + "'");
                    }
                }
                return value;
            }
        }

        /// <summary>
        /// Helper method that turns the passed Unifiable into an XML node
        /// </summary>
        /// <param name="outerXML">the Unifiable to XMLize</param>
        /// <returns>The XML node</returns>
        public static LineInfoElement getNode00(string outerXML)
        {
            var sr = new StringReader(outerXML);
            try
            {
                XmlDocumentLineInfo doc = new XmlDocumentLineInfo("From " + outerXML, false);
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
                    li.ReadOnly = false;
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
        public static LineInfoElement getNode00(string outerXML, XmlNode templateNode)
        {
            var sr = new StringReader(outerXML);
            try
            {
                string named = "From " + outerXML;
                if (templateNode != null)
                {
                    named = "" + templateNode.OwnerDocument;
                    if (string.IsNullOrEmpty(named)) named = "from: " + templateNode.OuterXml;
                }
                XmlDocumentLineInfo doc =
                    new XmlDocumentLineInfo(named, true);
                doc.Load(sr);
                var de = doc.DocumentElement;
                if (doc.ChildNodes.Count == 0)
                {
                    RTPBot.writeDebugLine("NULL outerXML=" + outerXML);
                    //  return null;
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
                    li.ReadOnly = false;
                    return (LineInfoElement)temp; //.FirstChild;}
                }
                return (LineInfoElement)temp; //.FirstChild;}
            }
            catch (Exception exception)
            {
                RTPBot.writeDebugLine("ERROR outerXML='" + outerXML + "'\n" + exception + "\n" + AIMLLoader.LocationInfo(templateNode));
                throw exception;
            }
        }

        #region Helper methods

        public override string ToString()
        {
            return LineNumberTextInfo();
        }

        public string LineNumberTextInfo()
        {
            string s = LineTextInfo() + " " + LineNumberInfo();
            if (!s.Contains(initialString))
            {
                return "-WAS- '" + initialString + "' -NOW- " + s;
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

        public string DocumentInfo()
        {
            string s = null;
            if (templateNode is LineInfoElement)
            {
                LineInfoElement li = (LineInfoElement)templateNode;
                s = "" + li.OwnerDocument;
                if (!string.IsNullOrEmpty(s)) return s;
                if (Parent != null && Parent != this)
                {
                    s = DocumentInfo();
                    if (!string.IsNullOrEmpty(s)) return s;
                }
            }
            return s;
        }

        #endregion

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

        protected AIMLTagHandler GetChildTagHandler(XmlNode childNode)
        {
            AIMLTagHandler part = Proc.GetTagHandler(user, query, request, result, childNode, this);
            return part;
        }

        protected Unifiable ProcessChildNode(XmlNode childNode, bool protectChildren, bool saveOnInnerXML)
        {
            try
            {
                if (childNode.NodeType == XmlNodeType.Text)
                {
                    Unifiable value = childNode.InnerText.Trim();
                    if (saveOnInnerXML) childNode.InnerXml = value;
                    return value;
                }
                else if (childNode.NodeType == XmlNodeType.Comment)
                {
                    if (saveOnInnerXML) childNode.InnerXml = "";
                    return childNode.OuterXml;
                }
                else
                {
                    if (childNode.InnerXml.StartsWith("+"))
                    {
                        string s = childNode.InnerXml.Substring(1);
                        while (s.StartsWith("+")) s = s.Substring(1);
                        return s;
                    }
                    bool copyParent, copyChild;
                    copyParent = copyChild = protectChildren;

                    AIMLTagHandler tagHandlerChild;
                    var value = Proc.processNode(childNode, query,
                                                 request, result, user,
                                                 this, copyChild, copyParent,
                                                 out tagHandlerChild);
                    if (Unifiable.IsNull(value))
                    {
                        writeToLogWarn("ERROR NULL AIMLTRACE " + value + " -> " + childNode.OuterXml + "!");                        
                    } 
                    if (saveOnInnerXML)
                    {
                        SaveResultOnChild(childNode, value);
                    }
                    return value;
                }
            }

            catch (Exception e)
            {
                var value = "ERROR: " + e;
                writeToLog(value);
                return value;
            }
        }

        public bool IsInnerTextOnly(XmlNode node)
        {
            string s = node.InnerXml.Trim();
            if (s == node.InnerText.Trim())
            {
                if (s.Length > 0) return true;
                return true;
            }
            return false;
        }

        protected void DebugCheck(int i)
        {
            if (!request.IsTraced) return;
            if (request.DebugLevel < i)
            {
                RTPBot.Breakpoint("Level " + request.DebugLevel + "<" + i);
            }
        }
    }
}