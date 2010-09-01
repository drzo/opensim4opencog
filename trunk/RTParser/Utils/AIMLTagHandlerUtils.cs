using System;
using System.Collections.Generic;
using System.IO;
using System.Text.RegularExpressions;
using System.Xml;
using RTParser.AIMLTagHandlers;
using RTParser.Variables;
using StringAppendableUnifiable = RTParser.StringAppendableUnifiableImpl;
//using StringAppendableUnifiable = System.Text.StringBuilder;

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

        internal string GetDictName(string tryFirst)
        {
            string type = GetAttribValue(tryFirst, null);
            if (type == null)
            {
                string uname = GetAttribValue("user", null);
                if (uname != null) type = GetNamedType("user", uname);
                string bname = GetAttribValue("bot", null);
                if (bname != null) type = GetNamedType("bot", bname);
            }
            return type;
        }

        internal static string GetNamedType(string name, string value)
        {
            if (value != null)
            {
                value = value.ToLower();
                if (value == "true" || value == "t" || value == "yes" || value == "y")
                {
                    return name;
                }
                if (value == "false" || value == "f" || value == "no" || value == "n")
                {
                    return null;
                }
                else
                {
                    return value;
                }
            }
            return value;
        }

        public Unifiable GetStarContent()
        {
            XmlNode starNode = Utils.AIMLTagHandler.getNode("<star/>", templateNode);
            LineInfoElementImpl.unsetReadonly(starNode);
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
            if (node == null) return defaultIfEmpty();
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
            string[] nameSplit = name.Split(new[] { ',' }, StringSplitOptions.RemoveEmptyEntries);
            foreach (var nameS in nameSplit)
            {
                var r = AltStar(nameS, query, dict);
                if (!Unifiable.IsNullOrEmpty(r))
                {
                    return r;
                }                
            }
            return name;
        }
        static public Unifiable AltStar(string name, SubQuery query, ISettingsDictionary dict)
        {
            try
            {
                if (name.StartsWith("star_"))
                {
                    return GetDictData(query.InputStar, name, 5);
                }
                else if (name.StartsWith("inputstar_"))
                {
                    return GetDictData(query.InputStar, name, 10);
                }
                else if (name.StartsWith("input_"))
                {
                    return GetDictData(query.InputStar, name, 6);
                }
                else if (name.StartsWith("thatstar_"))
                {
                    return GetDictData(query.ThatStar, name, 9);
                }
                else if (name.StartsWith("that_"))
                {
                    return GetDictData(query.ThatStar, name, 5);
                }
                else if (name.StartsWith("topicstar_"))
                {
                    return GetDictData(query.TopicStar, name, 10);
                }
                else if (name.StartsWith("topic_"))
                {
                    return GetDictData(query.TopicStar, name, 6);
                }
                else if (name.StartsWith("guardstar_"))
                {
                    return GetDictData(query.GuardStar, name, 10);
                }
                else if (name.StartsWith("guard_"))
                {
                    return GetDictData(query.GuardStar, name, 6);
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
                else if (name.StartsWith("%"))
                {
                    Unifiable value = null;
                    string str = name.Substring(1);
                    if (str.StartsWith("bot."))
                    {
                        ISettingsDictionary dict2 = query.Request.TargetBot.GlobalSettings;
                        str = str.Substring(4);
                        value = dict2.grabSetting(str);
                        if (!Unifiable.IsNullOrEmpty(value)) return value;
                    }
                    else if (str.StartsWith("user."))
                    {
                        ISettingsDictionary dict2 = query.Request.user;
                        str = str.Substring(5);
                        value = dict2.grabSetting(str);
                        if (!Unifiable.IsNullOrEmpty(value)) return value;                        
                    }
                    if (dict != null)
                    {
                        value = dict.grabSetting(str);
                        if (!Unifiable.IsNullOrEmpty(value)) return value;
                    }
                }
            }
            catch (Exception e)
            {
                RTPBot.writeDebugLine("" + e);
            }
            return null;
        }

        private static Unifiable GetDictData(List<Unifiable> unifiables, string name, int startChars)
        {
            string s = name.Substring(startChars);

            if (s == "*" || s == "ALL" || s == "0")
            {
                var result = Unifiable.CreateAppendable();
                foreach (Unifiable u in unifiables)
                {
                    result.Append(u);
                }
                return result;
            }

            int uc = unifiables.Count;

            bool fromend = false;
            if (s.StartsWith("-"))
            {
                fromend = true;
                s = s.Substring(1);
            }

            int i = Int32.Parse(s);

            if (i==0)
            {
                if (uc == 0) return "";
            }
            int ii = i - 1;
            if (fromend) ii = uc - i;
            if (uc == 0)
            {
                RTPBot.writeDebugLine(" !ERROR -star underflow! " + i + " in " + name);
                return String.Empty;
            }
            if (ii >= uc || ii < 0)
            {

                RTPBot.writeDebugLine(" !ERROR -star badindexed 0 < " + i + " < " + uc + " in " + name);
                return unifiables[ii];
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
            if (required.ToUpper() == "UNKNOWN" && (Unifiable.IsUnknown(actualValue)))
            {
                return true;
            }
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
                var li = templateNode;
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
            if (templateNode is IXmlLineInfo)
            {
                IXmlLineInfo li = (IXmlLineInfo)templateNode;
                if (li.LineNumber == 0)
                {
                    s = s + " " + templateNode.OwnerDocument.ToString();
                    if (Parent != null && Parent != this)
                    {
                        s = s + " " + Parent.LineNumberInfo();
                    }
                    else
                    {
                        s = s + " (" + li.LineNumber + ":" + li.LinePosition + ")";
                    }
                }
                else
                {
                    s = s + " (" + templateNode.OwnerDocument.ToString() + ":line " + li.LineNumber + "," + li.LinePosition + ") ";
                }
            }
            return s + "-->";
        }

        public string DocumentInfo()
        {
            string s = null;
            if (templateNode != null)
            {
                var li = templateNode;
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
                if (childNode.InnerXml.StartsWith("+"))
                {
                    string s = childNode.InnerXml.Substring(1);
                    s = s.TrimStart("+ ".ToCharArray());
                    return s;
                }
                if (childNode.NodeType == XmlNodeType.Text)
                {
                    string value = childNode.InnerText.Trim();
                    if (value.StartsWith("+"))
                    {
                        return value.Substring(1);
                    }
                    if (saveOnInnerXML)
                    {
                        childNode.InnerText = "+" + value;
                    }
                    return value;
                }
                else if (childNode.NodeType == XmlNodeType.Comment)
                {
                    if (saveOnInnerXML) childNode.InnerXml = "";
                    return String.Empty;
                }
                else
                {
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

        protected string ToVisiable(XmlNode node)
        {
            string oxml = node.OuterXml;
            if (node.NodeType == XmlNodeType.Attribute)
            {
                return node.Name + "=\"" + node.Value + "\"";
            }
            if (oxml.Trim().Length > 0) return oxml;
            oxml = node.InnerText;
            if (oxml.Trim().Length > 0) return oxml;
            if (node.NodeType == XmlNodeType.Element)
            {
                return node.OuterXml;
            }
            return node.OuterXml;
        }
    }
}