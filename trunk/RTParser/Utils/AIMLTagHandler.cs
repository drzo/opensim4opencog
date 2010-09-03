using System;
using System.Collections;
using System.Collections.Generic;
using System.IO;
using System.Text;
using System.Text.RegularExpressions;
using System.Threading;
using System.Xml;
using RTParser.AIMLTagHandlers;
using RTParser.Database;
using RTParser.Variables;
//using LineInfoElement = System.Xml.XmlNode;
using LineInfoElement = RTParser.Utils.LineInfoElementImpl;

namespace RTParser.Utils
{
    /// <summary>
    /// The template for all classes that handle the AIML tags found within template nodes of a
    /// category.
    /// </summary>
    abstract public partial class AIMLTagHandler : TextTransformer, IXmlLineInfo
    {

        /// <summary>
        /// Default ctor to use when late binding
        /// </summary>
        public AIMLTagHandler()
        {
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
            initialString = inputString;
            if (this.templateNode.Attributes != null) this.templateNode.Attributes.RemoveNamedItem("xmlns");
        }
        /// <summary>
        /// The template node to be processed by the class
        /// </summary>
        public XmlNode templateNode;
        protected Unifiable RecurseResult = Unifiable.NULL;
        public bool IsStarAtomically = true;

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

        /// <summary>
        /// A representation of the input into the Proc made by the user
        /// </summary>
        public RTParser.Request _request;
        public RTParser.Request request
        {
            get
            {
                if (query != null)
                {
                    if (_request == query.Request)
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
        public RTParser.Result  result
        {
            get
            {
                if (query != null)
                {
                    if (_result0 == query.Result)
                    {
                        return _result0;
                    }
                    else
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

        protected bool ReadOnly
        {
            get { return false; }
            set { throw new NotImplementedException(); }
        }

        protected bool ResultReady(out string result0)
        {
            result0 = RecurseResult;
            if (templateNode.InnerXml.StartsWith("+"))
            {
                string s = templateNode.InnerXml.Substring(1);
                s = s.TrimStart("+ ".ToCharArray());
                result0 = s;
                return true;
            }
            // if (!Unifiable.IsNullOrEmpty(result0)) return true;
            return false;
        }

        public bool IsStarted = false;
        protected RTPBot TargetBot
        {
            get
            {
                if (query != null) return query.TargetBot;
                if (result != null) return result.bot;
                if (request != null) return request.TargetBot;
                if (user != null) return user.bot;
                return Proc;
            }
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
        /// By calling this and not just ProcessChange() 
        /// You've ensure we have a proper calling context
        /// </summary>
        /// <returns></returns>
        public override Unifiable ProcessAimlChange()
        {
            ThreadStart OnExit = EnterTag(request, templateNode, query);
            try
            {
                if (!Unifiable.IsNull(RecurseResult))
                {
                    return RecurseResult;
                }
                IsStarted = true;
                RecurseResult = ProcessChange();
                return RecurseResult;
            }
            finally
            {
                if (OnExit != null) OnExit();
            }
        }

        /// <summary>
        /// By calling this and not just CompleteProcess() 
        /// You've ensure we have a proper calling context
        /// </summary>
        public Unifiable CompleteAimlProcess()
        {
            ThreadStart OnExit = EnterTag(request, templateNode, query);
            try
            {
                var test = CompleteProcess();
                if (Unifiable.IsNull(test))
                {
                    writeToLogWarn("NULL " + test);
                }
                if (Unifiable.IsNull(RecurseResult))
                {
                    //writeToLog("NULL RecurseResult");
                }
                if (test == RecurseResult) return test;
                return test;
            }
            finally
            {
                if (OnExit != null) OnExit();
            }
        }

        /// <summary>
        /// Helper method that turns the passed Unifiable into an XML node
        /// </summary>
        /// <param name="outerXML">the Unifiable to XMLize</param>
        /// <returns>The XML node</returns>
        public virtual float CanUnify(Unifiable with)
        {
            string w = with.ToValue(query);
            Unifiable t1 = ProcessAimlChange();
            float score1 = t1.Unify(with, query);
            if (score1 == 0) return score1;
            Unifiable t2 = CompleteAimlProcess();
            float score2 = t2.Unify(with, query);
            if (score2 == 0) return score2;
            return (score1 < score2) ? score1 : score2;
        }

        sealed public override float CallCanUnify(Unifiable with)
        {
            bool prev = NamedValuesFromSettings.UseLuceneForGet;
            try
            {
                NamedValuesFromSettings.UseLuceneForGet = false;
                return CanUnify(with);
            }
            finally
            {
                NamedValuesFromSettings.UseLuceneForGet = prev;
            }
        }

        protected Unifiable Failure(string p)
        {
            writeToLog("<!-- FAILURE: " + p.Replace("<!--", "<#-").Replace("-->", "-#>") + "-->");
            return Unifiable.Empty;
        }

        protected Unifiable Succeed(string p)
        {
            return "<!-- SUCCEED: " + p.Replace("<!--", "<#-").Replace("-->", "-#>") + "-->";
        }

        protected Unifiable Succeed()
        {
            if (query != null && query.CurrentTemplate != null)
            {
                string type = GetType().Name;
                double defualtReward = query.GetSucceedReward(type);
                double score = GetAttribValue(templateNode, "score", defualtReward, query);
                writeToLog("TSCORE {3} {0}*{1}->{2} ",
                           score, query.CurrentTemplate.Rating,
                           query.CurrentTemplate.Rating *= score, score);
            }
            string s = Unifiable.InnerXmlText(templateNode);
            if (string.IsNullOrEmpty(s))
            {
                if (!Unifiable.IsNullOrEmpty(RecurseResult))
                    return RecurseResult;
                return Succeed(ToVisiable(templateNode));
            }
            return s;
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

        protected Unifiable templateNodeInnerText
        {
            get
            {
                string s2;
                if (!Unifiable.IsNull(RecurseResult))
                {
                    return RecurseResult;
                }
                else
                {
                    s2 = Recurse();
                    if (!Unifiable.IsNull(RecurseResult))
                    {
                        return RecurseResult;
                    }
                }
                string sr = Unifiable.InnerXmlText(templateNode);
                return CheckValue(sr);
            }

            set
            {
                if (Unifiable.IsNullOrEmpty(value))
                {
                    writeToLog("ERROR ?!?! templateNodeInnerText = " + value);
                }
                else if (!value.AsString().Contains("<a href"))
                    if (AIMLLoader.ContainsAiml(value))
                    {
                        writeToLogWarn("ContainsAiml = " + value);
                    }
                templateNode.InnerText = CheckValue(value);
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

        public Unifiable GetStarContent()
        {
            XmlNode starNode = getNode("<star/>", templateNode);
            LineInfoElementImpl.unsetReadonly(starNode);
            star recursiveStar = new star(this.Proc, this.user, this.query, this.request, this.result, starNode);
            return recursiveStar.Transform();
        }

        protected Unifiable callSRAI(Unifiable starContent)
        {
            XmlNode sraiNode = getNode(String.Format("<srai>{0}</srai>", starContent), templateNode);
            LineInfoElementImpl.unsetReadonly(sraiNode);
            srai sraiHandler = new srai(this.Proc, this.user, this.query, this.request, this.result, sraiNode);
            return sraiHandler.Transform();
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
                    var templateResult = Unifiable.CreateAppendable();
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
                            RTPBot.writeDebugLine("ERROR: {0}", e);
                        }

                    }
                    return templateResult;
                }
            }
            return Unifiable.Empty;
        }

        public string GetAttribValue(string attribName, string defaultIfEmpty)
        {
            return GetAttribValue(templateNode, attribName, () => defaultIfEmpty, query);
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

        protected Unifiable Recurse()
        {
            if (!Unifiable.IsNull(RecurseResult))
            {
                writeToLog("USING CACHED RECURSE " + RecurseResult);
                // use cached recurse value
                return RecurseResult;
            }
            //Unifiable templateNodeInnerText;//= this.templateNodeInnerText;
            Unifiable templateResult = Unifiable.CreateAppendable();
            if (this.templateNode.HasChildNodes)
            {
                // recursively check
                foreach (XmlNode childNode in this.templateNode.ChildNodes)
                {
                    Unifiable found = ProcessChildNode(childNode, ReadOnly, false);
                    templateResult.Append(found);
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
                if (IsStarAtomically)
                {
                    try
                    {
                        // atomic version of the node
                        Unifiable starContent = GetStarContent();
                        if (!Unifiable.IsNullOrEmpty(starContent))
                            return starContent;
                    }
                    catch (Exception e) 
                    {
                        writeToLogWarn("ERROR {0}", e);
                    }
                }
                Unifiable before = Unifiable.InnerXmlText(this.templateNode); //.InnerXml;        
                return CheckValue(before);
            }

        }

        /// <summary>
        /// Do a transformation on the Unifiable found in the InputString attribute
        /// </summary>
        /// <returns>The resulting transformed Unifiable</returns>
        public override string Transform()
        {
            if (!this.inputString.IsEmpty)
            {
                if (!Unifiable.IsNull(RecurseResult))
                {
                    return RecurseResult;
                }
                RecurseResult = this.ProcessAimlChange();
                return RecurseResult;
            }
            else
            {
                return Unifiable.Empty;
            }
        }

        public override Unifiable CompleteProcess()
        {
//#if false
            return RecurseProcess();
        }

        public virtual Unifiable RecurseProcess()
        {
//#endif
            if (!Unifiable.IsNullOrEmpty(RecurseResult))
            {
                return RecurseResult;
            }
            AIMLTagHandler tagHandler = this;
            XmlNode node = templateNode;
            if (tagHandler.isRecursive)
            {
                var recursiveResult = Unifiable.CreateAppendable();
                if (node.HasChildNodes)
                {
                    // recursively check
                    foreach (XmlNode childNode in node.ChildNodes)
                    {
                        
                        Unifiable processChildNode = ProcessChildNode(childNode, ReadOnly, false);
                        SaveResultOnChild(childNode, processChildNode);
                        recursiveResult.Append(processChildNode);
                    }
                }
                var v = tagHandler.Transform();
                if (v == recursiveResult)
                {
                    return v;
                }
                /*
                if (Unifiable.IsNullOrEmpty(v))
                {
                    if (!Unifiable.IsNullOrEmpty(recursiveResult))
                        ;// v = recursiveResult;
                }
                 */
                return CheckValue(v);
            }
            else
            {
                string resultNodeInnerXML = tagHandler.Transform();
                var resultNode = getNode("<node>" + resultNodeInnerXML + "</node>", templateNode);
                LineInfoElementImpl.unsetReadonly(resultNode);
                if (resultNode.HasChildNodes)
                {
                    var recursiveResult = Unifiable.CreateAppendable();
                    // recursively check
                    foreach (XmlNode childNode in resultNode.ChildNodes)
                    {
                        recursiveResult.Append(ProcessChildNode(childNode, ReadOnly, false));
                    }
                    if (!recursiveResult.IsEmpty)
                    {
                        RecurseResult = recursiveResult;
                    }
                    return recursiveResult;
                }
                else
                {
                    return Unifiable.InnerXmlText(resultNode);
                }
            }
        }

        public virtual void SaveResultOnChild(XmlNode node, string value)
        {
            //if (value == null) return;
            //if (value == "") return;
            value = CheckValue(value);
            if (value==null || value.Trim()=="")
            {
                writeToLog("-!SaveResultOnChild AIMLTRACE " + value + " -> " + node.OuterXml);                
            }
            if (node.NodeType == XmlNodeType.Comment) return;

            if (node is XmlText) node.InnerText = value;
            else
                node.InnerXml = "+" + value;
        }

        protected bool CheckNode(string name)
        {
            if (this.templateNode.Name.ToLower() == name) return true;
            if (name.Contains(","))
            {
                foreach (var s in name.Split(",".ToCharArray(), StringSplitOptions.RemoveEmptyEntries))
                {
                    if (CheckNode(s)) return true;
                }
            }
            //writeToLog("CheckNode change " + name + " -> " + templateNode.Name);
            return true;
        }

        protected Unifiable GetActualValue(string name, bool preferBotOverUser, out bool succeed)
        {
            ISettingsDictionary dict = query;
            Unifiable defaultVal = GetAttribValue("default,defaultValue", Unifiable.Empty);
            string dictName = GetDictName("type,dict");
            if (dictName == null)
            {
                dictName = preferBotOverUser ? "bot" : "user";
            }
            if (preferBotOverUser)
            {
                dict = request.TargetBot.GlobalSettings;
            }
            Unifiable gName = GetAttribValue("global_name", name);
            string realName;
            Unifiable v = NamedValuesFromSettings.GetSettingForType(
                dictName, query, dict, name, out realName,
                gName, defaultVal, out succeed, templateNode);
            return v;
        }
    }
}
