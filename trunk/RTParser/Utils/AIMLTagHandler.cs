using System;
using System.Threading;
using System.Xml;
using MushDLR223.Utilities;
using RTParser.AIMLTagHandlers;
using RTParser.Database;
using RTParser.Variables;
using LineInfoElement = MushDLR223.Utilities.LineInfoElementImpl;

namespace RTParser.Utils
{
    /// <summary>
    /// The template for all classes that handle the AIML tags found within template nodes of a
    /// category.
    /// </summary>
    public abstract partial class AIMLTagHandler : TextTransformer, IXmlLineInfo
    {
        /// <summary>
        /// A representation of the input into the Proc made by the user
        /// </summary>
        public Request _request;

        /// <summary>
        /// A representation of the result to be returned to the user
        /// </summary>
        private Result _result0;

        /// <summary>
        /// A flag to denote if inner tags are to be processed recursively before processing this tag
        /// </summary>
        public bool isRecursive = true;

        public bool IsStarAtomically = false; // true break it right now
        public bool IsStarted;
        public bool IsOverBudget;
        public AIMLTagHandler Parent;

        /// <summary>
        /// The query that produced this node containing the wildcard matches
        /// </summary>
        public SubQuery query;

        internal TemplateInfo templateInfo;

        /// <summary>
        /// The template node to be processed by the class
        /// </summary>
        public XmlNode templateNode;

        /// <summary>
        /// A representation of the user who made the request
        /// </summary>
        public User user;

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
        public AIMLTagHandler(RTPBot bot,
                              User user,
                              SubQuery query,
                              Request request,
                              Result result,
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

        public SubQuery TheQuery
        {
            get
            {
                SubQuery ret = this.query
                    //?? request.CurrentQuery ?? this.query ?? result.CurrentQuery
                    ;
                return ret;
            }
        }

        public Request request
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
            set { _request = value; }
        }

        public Result result
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
            set { _result0 = value; }
        }

        protected bool ReadOnly
        {
            get { return false; }
            set { throw new NotImplementedException(); }
        }

        protected RTPBot TargetBot
        {
            get
            {
                if (query != null) return query.TargetBot;
                if (result != null) return result.TargetBot;
                if (request != null) return request.TargetBot;
                if (user != null) return user.bot;
                return Proc;
            }
        }

        public string GetTemplateNodeInnerText()
        {
            return templateNodeInnerText;
        }

        protected Unifiable templateNodeInnerText
        {
            get
            {
                string s2 = null;
                if (!innerResult.IsValid)
                {
                    s2 = Recurse();
                }
                if (innerResult.IsValid)
                {
                    s2 = finalResult.Value;
                }
                else if (finalResult.IsValid) return finalResult.Value;
                string sr = s2 ?? InnerXmlText(templateNode);
                sr = ValueText(sr);
                return CheckValue(sr);
            }

            set
            {
                if (Unifiable.IsNullOrEmpty(value))
                {
                    writeToLog("ERROR ?!?! templateNodeInnerText = " + value);
                }
                string valueAsString = value.AsString();

                if (valueAsString.StartsWith(isValueSetStart))
                {
                    RecurseResult = ValueText(valueAsString);
                }
                else
                {
                    innerResult.Value = value;
                }

                if (!valueAsString.Contains("<a href"))
                    if (ContainsAiml(valueAsString))
                    {
                        writeToLogWarn("ContainsAiml = " + valueAsString);
                    }
                templateNode.InnerText = CheckValue(value);
            }
        }

        public virtual bool QueryHasFailed
        {
            get { return query.HasFailed > 0; }
            set
            {
                if (InUnify)
                {
                    writeToLog("InUnify QueryHasFailed=" + value);
                    return;
                } 
                query.HasFailed += (value ? 1 : 0);
            }
        }
        public virtual bool QueryHasSuceeded
        {
            get { return query.HasSuceeded > 0; }
            set
            {
                if (InUnify)
                {
                    writeToLog("InUnify QueryHasFailed=" + value);
                    return;
                } 
                query.HasSuceeded += (value ? 1 : 0);
            }
        }

        public virtual int QueryHasFailedN
        {
            get { return query.HasFailed; }
            set { query.HasFailed += value; }
        }
        public virtual int QueryHasSuceededN        
        {
            get { return query.HasSuceeded; }
            set { query.HasSuceeded += value; }
        }
        protected bool InUnify;

        protected ResultCache finalResult = new ResultCache();
        protected ResultCache innerResult = new ResultCache();

        public bool RecurseResultValid
        {
            get
            {
                if (templateNode.InnerXml.StartsWith(isValueSetStart)) return true;
                return finalResult.IsValid;

            }
            set
            {
                if (value)
                {
                    if (IsNull(RecurseResult))
                    {
                        writeToLogWarn("Set _RecurseResult to String.Empty at least!");
                    }
                }
                finalResult.IsValid = value;
            }
        }
       
        /// <summary>
        /// Final Result (not "innerResult!")
        /// </summary>
        public Unifiable RecurseResult
        {
            get
            {
                if (!finalResult.IsValid) return InnerXml(templateNode);
                return finalResult.Value;
            }
            set
            {                
                finalResult.Value = value;
                templateNode.InnerXml = XmlValueSettable(value);
            }
        }

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
            ResetValues(true);
        }

        public void ResetValues(bool childsTo)
        {
            QueryHasFailed = false;
            IsStarted = false;
            finalResult.Reset();
            innerResult.Reset(); // Unifiable.NULL;
        }

        // This was from the original source of the AIML Tag

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
            if (finalResult.IsValid) return finalResult.Value;
            ThreadStart OnExit = EnterTag(request, templateNode, query);
            try
            {
                if (RecurseResultValid) return RecurseResult;
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
            if (finalResult.IsValid) return finalResult.Value;
            ThreadStart OnExit = EnterTag(request, templateNode, query);
            try
            {
                Unifiable test = CompleteProcess();
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
            if (ReferenceEquals(t1, t2)) return score1;            
            float score2 = t2.Unify(with, query);
            if (score2 == 0) return score2;
            return (score1 < score2) ? score1 : score2;
        }


        public ThreadStart EnterUnify()
        {
            bool prev = NamedValuesFromSettings.UseLuceneForGet;
            bool prevUnify = InUnify;
            NamedValuesFromSettings.UseLuceneForGet = false;
            InUnify = true;
            return () =>
                       {
                           InUnify = prevUnify;
                           NamedValuesFromSettings.UseLuceneForGet = prev;
                       };
        }

        public override sealed float CallCanUnify(Unifiable with)
        {
            var exitUnify = EnterUnify();
            try
            {
                return CanUnify(with);
            }
            finally
            {
                exitUnify();
            }
        }

        protected Unifiable Failure(string p)
        {
            writeToLog("<!-- FAILURE: " + p.Replace("<!--", "<#-").Replace("-->", "-#>") + "-->");
            return Unifiable.Empty;
            this.QueryHasFailedN++;
        }

        protected Unifiable Succeed(string p)
        {
            this.QueryHasSuceededN++;
            return "<!-- SUCCEED: " + p.Replace("<!--", "<#-").Replace("-->", "-#>") + "-->";
        }

        protected Unifiable Succeed()
        {
            if (query != null && query.CurrentTemplate != null)
            {
                string type = GetType().Name;
                double defualtReward = query.GetSucceedReward(type);
                double score = GetAttribValue<double>(templateNode, "score", () => defualtReward,
                                                      ReduceStarAttribute<double>);
                writeToLog("TSCORE {3} {0}*{1}->{2} ",
                           score, query.CurrentTemplate.Rating,
                           query.CurrentTemplate.Rating *= score, score);
            }
            string s = Unifiable.InnerXmlText(templateNode);
            if (string.IsNullOrEmpty(s))
            {
                if (RecurseResultValid) return RecurseResult;
                return Succeed(templateNode.OuterXml);
            }
            return s;
        }

        protected T ReduceStarAttribute<T>(IConvertible arg) where T : IConvertible
        {
            return ReduceStar<T>(arg, query, query);
        }
        protected Unifiable ReduceStarAttribute(IConvertible arg)
        {
            return ReduceStar<Unifiable>(arg, query, query);
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
                //templateNodeInnerText = isValueSetStart + unifiable;                
                RecurseResult = unifiable;
                return true;
            }
            return false;
        }

        public Unifiable GetStarContent()
        {
            XmlNode starNode = getNode("<star/>", templateNode);
            LineInfoElement.unsetReadonly(starNode);
            star recursiveStar = new star(this.Proc, this.user, this.query, this.request, this.result, starNode);
            return recursiveStar.Transform();
        }

        protected Unifiable callSRAI(string starContent)
        {
            XmlNode sraiNode = getNode(String.Format("<srai>{0}</srai>", starContent), templateNode);
            LineInfoElement.unsetReadonly(sraiNode);
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
            bool templateNodeHasChildNodes = templateNode.HasChildNodes;
            // pre textualized ?
            var innerText = this.templateNodeInnerText;
            if (!innerText.IsEmpty)
            {
                if (!templateNodeHasChildNodes)
                {
                    writeToLog("!templateNodeHasChildNodes ?!");
                }
                // non atomic version of the node
                return afterEachOrNull(innerText);
            }
            else
            {
                if (!templateNodeHasChildNodes)
                {
                    // atomic version of the node
                    Unifiable templateResult = GetStarContent();

                    Unifiable a = afterEachOrNull(templateResult);
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
                    StringAppendableUnifiableImpl templateResult = Unifiable.CreateAppendable();
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
                        catch (ChatSignal ex)
                        {
                            throw;
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
            return GetAttribValue(templateNode, attribName, () => defaultIfEmpty, query.ReduceStarAttribute<string>);
        }

        public virtual Unifiable CheckValue(Unifiable value)
        {
            if (ReferenceEquals(value, Unifiable.Empty)) return value;
            if (value == null)
            {
                writeToLogWarn("ChackValue NULL");
                return null;
            }
            else
            {
                if (Unifiable.IsNullOrEmpty(value))
                {
                    writeToLogWarn("CheckValue EMPTY = '" + value + "'");
                    return value;
                }
                string v = value.AsString();
                if (!v .Contains("<a href"))
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
            bool success;
            var vv = ProcessChildNode(childNode, protectChildren, saveOnInnerXML, out success);
            if (!success)
            {
                QueryHasFailedN++;
            }

            return vv;
        }

        protected Unifiable ProcessChildNode(XmlNode childNode, bool protectChildren, bool saveOnInnerXML, out bool success)
        {
            var vv = ProcessChildNode00(childNode, protectChildren, saveOnInnerXML, out success);
            if (!success)
            {
                QueryHasFailedN++;
            }
            if (success && ContainsAiml(vv))
            {                
                var vv2 = ProcessChildNode00(getNode("<template>" + vv + "</template>"), false, saveOnInnerXML,
                                             out success);
                writeToLog("Continie Processing of " + vv + " -> " + vv2);
                vv = vv2;
            }
            return vv;
        }

        protected Unifiable ProcessChildNode00(XmlNode childNode, bool protectChildren, bool saveOnInnerXML, out bool success)
        {
            if (saveOnInnerXML) throw new InvalidOperationException("saveOnInnerXML!" + this);
            try
            {
                string childNodeInnerXml = childNode.InnerXml;
                if (childNodeInnerXml.StartsWith(isValueSetStart))
                {
                    string s = ValueText(childNodeInnerXml);
                    success = true;
                    return s;
                }
                if (childNode.NodeType == XmlNodeType.Text)
                {
                    string value = childNode.InnerText.Trim();
                   
                    if (value.StartsWith(isValueSetStart))
                    {
                        success = true;
                        return ValueText(value);
                    }
                    if (saveOnInnerXML)
                    {
                        childNode.InnerText = XmlValueSettable(value);
                    }
                    success = true;
                    return value;
                }
                else if (childNode.NodeType == XmlNodeType.Comment)
                {
                    success = true;
                    if (saveOnInnerXML) childNode.InnerXml = XmlValueSettable("");
                    return String.Empty;
                }
                else
                {
                    bool copyParent, copyChild;
                    copyParent = copyChild = protectChildren;

                    AIMLTagHandler tagHandlerChild;
                    string value = Proc.processNode(childNode, query,
                                                    request, result, user,
                                                    this, copyChild, copyParent,
                                                    out tagHandlerChild);
                    success = true;
                    if (IsNull(value))
                    {
                        if (tagHandlerChild != null && tagHandlerChild.QueryHasSuceeded)
                            success = true;
                        writeToLogWarn("ERROR NULL AIMLTRACE " + value + " -> " + childNode.OuterXml + "!");

                        if (tagHandlerChild != null)
                        {
                            IsOverBudget = tagHandlerChild.IsOverBudget;
                        }
                        value = Proc.processNodeDebug(childNode, query,
                                                 request, result, user,
                                                 this, copyChild, copyParent,
                                                 out tagHandlerChild);

                        success = false;
                    }
                    else
                    {
                        if (tagHandlerChild == null)
                        {
                            writeToLogWarn("ERROR tagHandlerChild AIMLTRACE " + value + " -> " + childNode.OuterXml + "!");
                            request.TimesOutAt = DateTime.Now + TimeSpan.FromMinutes(2); // for debugging
                            value = Proc.processNodeDebug(childNode, query,
                                                    request, result, user,
                                                    this, copyChild, copyParent,
                                                    out tagHandlerChild);
                        }
                        else
                        {
                            if (tagHandlerChild.QueryHasFailed) success = false;
                            if (tagHandlerChild.QueryHasSuceeded)
                                success = true;
                        }
                    }
                    if (saveOnInnerXML)
                    {
                        SaveResultOnChild(childNode, value);
                    }
                    return value;
                }
            }
            catch (ChatSignal ex)
            {
                throw;
            }
            catch (Exception e)
            {
                string value = "ERROR: " + e;
                writeToLog(value);
                success = false;
                return value;
            }
        }

        protected Unifiable Recurse()
        {
            bool _wasRecurseResultValid = innerResult.IsValid;
            try
            {
                if (innerResult.IsValid)
                {
                    writeToLog("USING CACHED RECURSE " + RecurseResult);
                    return CheckValue(innerResult.Value);
                    // use cached recurse value
                    return innerResult.Value;
                }
                Unifiable real = RecurseReal(templateNode, false);
                if (IsNullOrEmpty(real))
                {
                    if (IsStarAtomically)
                    {
                        if (IsNull(real)) return null;
                    }
                    if (IsNull(real)) return null;
                    if (IsEMPTY(real)) return Unifiable.Empty;
                }
                if (real.AsString().Contains("<"))
                {
                    return real;
                }
                return real;
            }
            finally
            {
                // ReSharper disable ConditionIsAlwaysTrueOrFalse
                if (false && innerResult.IsValid != _wasRecurseResultValid)
                // ReSharper restore ConditionIsAlwaysTrueOrFalse
                {
                    writeToLogWarn("_RecurseResultValid chged to " + innerResult.IsValid);
                }
                innerResult.IsValid = _wasRecurseResultValid;
            }            
        }

        protected Unifiable RecurseReal(XmlNode node, bool saveOnChildren)
        {
            //Unifiable templateNodeInnerText;//= this.templateNodeInnerText;
            Unifiable templateResult = Unifiable.CreateAppendable();
            if (node.HasChildNodes)
            {
                var ReadOnly = node.IsReadOnly;
                int goods = 0;
                // recursively check
                foreach (XmlNode childNode in node.ChildNodes)
                {
                    if (childNode.NodeType == XmlNodeType.Comment) continue;
                    bool success;
                    Unifiable found = ProcessChildNode(childNode, ReadOnly, false, out success);
                    if (saveOnChildren) SaveResultOnChild(childNode, found);
                    templateResult.Append(found);
                    if (found == "" && StaticAIMLUtils.IsSilentTag(childNode))
                    {
                        goods++;
                        continue;
                    }
                    if (success)
                    {
                        QueryHasSuceeded = true;
                        continue;
                    }
                    if (found == "" && !IsSilentTag(childNode))
                    {
                        if (goods == 0) QueryHasFailed = true;
                    }
                    if (goods == 0) QueryHasFailed = true;
                }
                //templateNodeInnerText = templateResult;//.ToString();
                if (!templateResult.IsEmpty)
                {
                    templateResult = CheckValue(templateResult);
                    innerResult.Value = templateResult;
                    return templateResult;
                }
                if (QueryHasSuceeded) return templateResult;
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
                        innerResult.Value = starContent;
                        if (!Unifiable.IsNullOrEmpty(starContent))
                            return starContent;
                    }
                    catch (ChatSignal ex)
                    {
                        throw;
                    }
                    catch (Exception e)
                    {
                        writeToLogWarn("ERROR {0}", e);
                    }
                }
                Unifiable before = Unifiable.InnerXmlText(node); //.InnerXml;        
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
                if (RecurseResultValid)
                {
                    return RecurseResult;
                }
                innerResult.Value = this.ProcessAimlChange();
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
            if (RecurseResultValid)
            {
                return RecurseResult;
            }
            AIMLTagHandler tagHandler = this;
            Unifiable recursiveResult = null;
            if (tagHandler.isRecursive)
            {
                if (templateNode.HasChildNodes)
                {
                    recursiveResult = RecurseReal(templateNode,true);
                } else
                {
                    recursiveResult = InnerXmlText(templateNode);
                }
                string v = tagHandler.Transform();
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
                XmlNode resultNode = getNode("<template>" + resultNodeInnerXML + "</template>", templateNode);
                LineInfoElementImpl.unsetReadonly(resultNode);
                if (resultNode.HasChildNodes)
                {
                    recursiveResult = RecurseReal(resultNode, false);
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
            value = ValueText(value);
            if (InUnify)
            {
                return;
            }
            //if (value == null) return;
            //if (value == "") return;
            value = CheckValue(value);
            if (value == null || value.Trim() == "")
            {
                writeToLog("-!SaveResultOnChild AIMLTRACE " + value + " -> " + node.OuterXml);
            }
            if (node.NodeType == XmlNodeType.Comment) return;
            if (node is XmlText)
            {
                node.InnerText = XmlValueSettable(value);
            }
            else
            {
                node.InnerXml = XmlValueSettable(value);
            }
        }

        protected bool CheckNode(string name)
        {
            if (this.templateNode.Name.ToLower() == name) return true;
            if (name.Contains(","))
            {
                foreach (string s in name.Split(",".ToCharArray(), StringSplitOptions.RemoveEmptyEntries))
                {
                    if (CheckNode(s)) return true;
                }
            }
            //writeToLog("CheckNode change " + name + " -> " + templateNode.Name);
            return true;
        }

        protected Unifiable GetActualValue(string name, string dictName, out bool succeed)
        {
            //ISettingsDictionary dict = query;
            return GetActualValue(templateNode, name, dictName, out succeed, query);
        }

        static protected Unifiable GetActualValue(XmlNode templateNode, string name, string dictNameIn, out bool succeed, SubQuery query)
        {
            ISettingsDictionary dict;// = query;
            Unifiable defaultVal = GetAttribValue(templateNode, "default,defaultValue", null);
            string dictName = GetNameOfDict(query, dictNameIn, templateNode, out dict);
            Unifiable gName = GetAttribValue(templateNode, "global_name", name);
            string realName;

            var prev = NamedValuesFromSettings.UseLuceneForSet;
            NamedValuesFromSettings.UseLuceneForSet = query.UseLuceneForSet;
            try
            {
                Unifiable v = NamedValuesFromSettings.GetSettingForType(
                    dictName, query, dict, name, out realName,
                    gName, defaultVal, out succeed, templateNode);
                return v;
            }
            finally
            {
                NamedValuesFromSettings.UseLuceneForSet = prev;
            }
        }

        protected virtual void AddSideEffect(string namedEffect, ThreadStart func)
        {
            if (InUnify)
            {
                writeToLog("InUnify " + namedEffect);
                return;
            } 
            if (QueryHasFailed)
            {
                writeToLog("SKIPPING " + namedEffect);
                return;
            }
            AIMLTagHandler tagHandler = FirstTagHandlerOrOuterMost("template");
            if (Parent == null || tagHandler == this || tagHandler == null)
            {
                query.AddSideEffect(namedEffect, func);
            }
            else
            {
                tagHandler.AddSideEffect(namedEffect, func);
            }
        }

        protected actMSM botActionMSM
        {
            get { return user.botActionMSM; }
        }
        /// <summary>
        /// Machine SideEffect - this denoates that he state of machine will change when processing the taghandler
        /// </summary>
        /// <param name="func"></param>
        protected virtual void MachineSideEffect(ThreadStart func)
        {
            LocalSideEffect(LineNumberTextInfo(), () =>
            {
                botActionMSM.PushSave();
                func();
            },
                            () => botActionMSM.PopLoad());
        }

        protected virtual void LocalSideEffect(string namedEffect, ThreadStart enter, ThreadStart exit)
        {
            if (QueryHasFailed)
            {
                writeToLog("SKIPPING LocalSideEffect " + namedEffect);
                return;
            }
            AIMLTagHandler tagHandler = FirstTagHandlerOrOuterMost("template");
            if (Parent == null || tagHandler == this || tagHandler == null)
            {
                query.LocalSideEffect(namedEffect, enter, exit);
            }
            else
            {
                tagHandler.LocalSideEffect(namedEffect, enter, exit);
            }
        }

        protected AIMLTagHandler FirstTagHandlerOrOuterMost(string type)
        {
            var current = this;
            while (current != null)
            {
                if (current.IsNode(type))
                {
                    return current;
                }
                if (current.Parent != null)
                    current = current.Parent;
            }
            return current;
        }

        private bool IsNode(string type)
        {
            foreach (var c in StaticXMLUtils.NamesStrings(type))
            {
                if (SearchStringMatches(c, templateNode.Name))
                {
                    return true;
                }
            }
            return false;
        }

        protected string InnerXml(XmlNode xmlNode)
        {
            return ValueText(InnerXmlText(xmlNode));
        }
    }
}