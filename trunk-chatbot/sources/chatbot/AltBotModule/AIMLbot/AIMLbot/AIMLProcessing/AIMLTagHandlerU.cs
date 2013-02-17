using System;
using System.Collections;
using System.Collections.Generic;
using System.Diagnostics;
using System.Threading;
using System.Xml;
using AltAIMLbot;
using AltAIMLbot.AIMLTagHandlers;
using AltAIMLbot.Database;
using AltAIMLbot.Utils;
using AltAIMLParser;
using AltAIMLbot.Variables;
using MushDLR223.Utilities;
using LineInfoElement = MushDLR223.Utilities.LineInfoElementImpl;
using MushDLR223.ScriptEngines;

namespace AltAIMLbot.Utils
{
    /// <summary>
    /// The template for all classes that handle the AIML tags found within template nodes of a
    /// category.
    /// </summary>
    public abstract partial class AIMLTagHandlerU : AIMLTagHandler, IAIMLTransaction, IXmlLineInfo, IDisposable
    {
        sealed protected override string ProcessChange()
        {
            var u = CompleteAimlProcess();
            return u;
        }
       
        public bool IsStarted;
        public bool IsOverBudget;
        public bool IsDisposing;
        internal static bool throwOnSave = true;
        public bool SuspendLimits;
        protected virtual bool ExpandingSearchWillYieldNoExtras { get { return false; } }
        public bool IsDeterministic = true;
        public AIMLTagHandlerU Parent;
        /// <summary>
        /// The query that produced this node containing the wildcard matches
        /// </summary>
        //public SubQuery query;

        internal TemplateInfo templateInfo;

#if false
        /// <summary>
        /// A representation of the user who made the request
        /// </summary>
        public User user
        {
            get { return request.Requester; }
            set
            {
                if (value != null)
                {
                    if (value != request.Requester)
                    {
                        return;                        
                    }
                }
            }
        }
#endif

        /// <summary>
        /// Default ctor to use when late binding
        /// </summary>
        public AIMLTagHandlerU()
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
        public AIMLTagHandlerU(AltBot bot,
                              User user,
                              SubQuery query,
                              Request request,
                              Result result,
                              XmlNode templateNode)
            : base(bot,user,query,request,result, templateNode)
        {
            this.query = query;
           // this._request = request;
            this.result = result;
            this.templateNode = templateNode;
            InputStringU = templateNode.OuterXml;
            initialString = InputString;
            this.user = user;
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

        public SubQuery CurrentQuery
        {
            get { return query; }
        }

#if false
        /// <summary>
        /// A representation of the input into the Proc made by the user
        /// </summary>
        public Request _request;

        /// <summary>
        /// A representation of the result to be returned to the user
        /// </summary>
        private Result _result0;
        
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
#endif
        protected bool ReadOnly;

        public AltBot TargetBot
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
            if (RecurseResultValid)
            {
                return RecurseResult;
            }
            //return null;
            return templateNodeInnerText;
        }

        protected virtual Unifiable templateNodeInnerText
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
                    s2 = innerResult.Value;
                    return s2;
                }
                //else if (finalResult.IsValid) return finalResult.Value;
                // either the fully evaluated innerXML non-failing result or else the failing innerXML
                string innerXML = s2 ?? InnerXmlText(templateNode);
                innerXML = ValueText(innerXML);
                return innerXML;
            }

            set
            {
                if (Unifiable.IsNullOrEmpty(value))
                {
                    writeToLogWarn("ERROR ?!?! templateNodeInnerText = " + value);
                }
                if (InUnify)
                {
                    writeToLogWarn("InUnify ?!?! templateNodeInnerText = " + value);
                    return;
                }
                string valueAsString = value.AsString();

                bool isOuter = IsValueSetter(valueAsString);
                var stringVal = ValueText(value);
                innerResult.Value = stringVal;
                if (isOuter)
                {
                    RecurseResult = ValueText(valueAsString);
                    return;
                    //innerResult.Value = value;
                }
                else
                {
                    return;
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
                if (QueryHasFailed == value) return;
                if (InUnify)
                {
                    //writeToLog("InUnify QueryHasFailed=" + value);
                    return;
                }
                if (value)
                {
                    writeToLogWarn("!InUnify AND QueryHasFailed=" + value);
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
                    writeToLog("InUnify QueryHasSuceeded=" + value);
                    return;
                }
                query.HasSuceeded += (value ? 1 : 0);
            }
        }

        public virtual int QueryHasFailedN
        {
            get { return query.HasFailed; }
            set { query.HasFailed = value; }
        }
        public virtual int QueryHasSuceededN
        {
            get { return query.HasSuceeded; }
            set { query.HasSuceeded = value; }
        }
        protected bool InUnify;

        protected ResultCache finalResult = new ResultCache();
        protected ResultCache innerResult = new ResultCache();
        private List<AIMLTagHandlerU> TagHandlerChilds;
        public static bool SaveAIMLChange = true;
        public static bool SaveAIMLComplete = false;
        public static bool EnforceStartedBool = false;
        private bool IsNeedingInnerXMLSet = true;

        public Unifiable FAIL
        {
            get
            {
                if (!QueryHasFailed) if (!Debugger.IsAttached)
                    {
                        QueryHasFailed = true;
                    }
                return null;
            }
        }

        public virtual bool RecurseResultValid
        {
            get
            {
                string templateNodeInnerXml = templateNode.InnerXml;
                if (templateNodeInnerXml.StartsWith(isValueSetStart))
                {
                    if (templateNodeInnerXml.Length > 3)
                    {
                        var vv = ToXMLValueNotOuter(templateNode);
                        if (vv == null)
                        {
                            return false;
                        }
                        return true;
                    }
                    if (!finalResult.IsValid)
                    {
                        return false;
                    }
                }
                if (!finalResult.IsValid)
                {
                    return false;
                }
                var recurseResult = this.RecurseResult;
                {
                    if (IsNull(recurseResult))
                    {
                        writeToLogWarn("IsNull _RecurseResult to String.Empty at least!");
                    }
                    else if (IsIncomplete(recurseResult))
                    {
                        writeToLogWarn("IsMissing _RecurseResult to String.Empty at least!");
                    }
                }
                return finalResult.IsValid;
            }
            set
            {
                var recurseResult = this.RecurseResult;
                if (value)
                {
                    if (IsNull(recurseResult))
                    {
                        writeToLogWarn("IsNull _RecurseResult to String.Empty at least!");
                    }
                    else
                        if (IsIncomplete(recurseResult))
                        {
                            writeToLogWarn("IsMissing _RecurseResult to String.Empty at least!");
                        }
                }
                finalResult.IsValid = value;
            }
        }

        /// <summary>
        /// Final Result (not "innerResult!")
        /// </summary>
        public virtual Unifiable RecurseResult
        {
            get
            {
                if (!finalResult.IsValid)
                {
                    var vv = ToXMLValueNotOuter(templateNode);
                    if (vv != null)
                    {
                        return CheckValue(vv);
                    }
                    return null;
                }
                return finalResult.Value;
            }
            set
            {
                if (!IsNullOrEmpty(value))
                {
                    if (IsNeedingInnerXMLSet)
                    {
                        IsNeedingInnerXMLSet = false;
                        string xmlValueSettable = XmlValueSettable(value);
                        templateNode.InnerXml = xmlValueSettable;
                    }
                    var vv = CheckValue(value);
                    finalResult.Value = vv;
                    if (InUnify)
                    {
                        return;
                    }
                }
                if (IsNull(value))
                {
                    writeToLog("WARN: Null = " + Unifiable.DescribeUnifiable(value));
                    return;
                }
                if (IsEMPTY(value))
                {
                    writeToLog("WARN: EXPTY" + Unifiable.DescribeUnifiable(value));
                }
            }
        }

        public void SetParent(AIMLTagHandlerU handlerU)
        {
            if (handlerU == this)
            {
                Proc.RaiseError(new InvalidOperationException("SetParent: same: " + this));
            }
            else if (handlerU == null)
            {
                //throw new InvalidOperationException("no parent handler");                
            }
            else
            {
                if (handlerU.InUnify)
                {
                    InUnify = handlerU.InUnify;
                }
                else
                {
                    InUnify = handlerU.InUnify;
                }
            }
            Parent = handlerU;
            ResetValues(true);
        }

        public void ResetValues(bool childsTo)
        {
            if (Parent != null && Parent.SuspendLimits)
            {
                SuspendLimits = false;
            }
            QueryHasFailed = false;
            QueryHasSuceededN = 0;
            QueryHasFailedN = 0;
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
            if (QueryHasFailed)
            {
                return null;
            }
            ThreadStart OnExit = EnterTag(request, templateNode, query);
            try
            {
                if (RecurseResultValid) return RecurseResult;
                IsStarted = true;
                string recurseResult = ProcessChangeU();
                var recurseResult0 = recurseResult;
                if (CompleteEvaluatution(recurseResult0, this, out recurseResult))
                {
                    RecurseResult = recurseResult;
                    return recurseResult;
                }
                
                if (!AltBot.BE_COMPLETE_NOT_FAST) return recurseResult0;

                var recurseResult1 = RecurseProcess();
                if (CompleteEvaluatution(recurseResult1, this, out recurseResult))
                {
                    RecurseResult = recurseResult;
                    return recurseResult;
                }
                var recurseResult2 = templateNodeInnerText;
                if (CompleteEvaluatution(recurseResult2, this, out recurseResult))
                {
                    writeToLogWarn("ProcessAimlChange -> templateNodeInnerText=" + recurseResult2 + "->" + recurseResult);
                    RecurseResult = recurseResult;
                    return recurseResult;
                }
                if (RecurseResultValid) return RecurseResult;
                return recurseResult0;
            }
            finally
            {
                if (OnExit != null) OnExit();
            }
        }

        public static bool CompleteEvaluatution(string vv, AIMLTagHandlerU childHandlerU, out Unifiable output)
        {
            string soutput;
            bool res = CompleteEvaluatution(vv, childHandlerU, out soutput);
            if (soutput == null)
            {
                output = null;
            }
            else
            {
                output = soutput;
            }
            return res;
        }

        public static bool CompleteEvaluatution(String vv, AIMLTagHandlerU childHandlerU, out string output)
        {
            const string NULL = default(string);
            output = NULL;
            bool success = true;
            if (vv == null) return false;
            if (IsNullOrEmpty(vv)) return false;
            //if (DLRConsole.IsTooDeep()) ;
            //    throw new ChatSignalOverBudget(childHandler.request, "CompleteEvaluatution: too Deep " + vv);
            if (!IsUnevaluated(vv))
            {
                output = vv;
                return true;
            }
            success = false;
            var request = childHandlerU.request.ParentMostRequest;
            Request CurrentSettings = request;
            int breakoutInWas = CurrentSettings.MaxCanEvalResult;
            if (breakoutInWas == 0)
            {
                // looper
                output = vv;
                return false;
            }
            int breakoutIn = breakoutInWas;
            var vv1 = vv;
            while (breakoutIn-- > 0 && IsUnevaluated(vv))
            {
                CurrentSettings.MaxCanEvalResult = 0;
                Unifiable vv2 = ProcessAiml(vv, out success, childHandlerU);
                if (!success)
                {
                    output = NULL;
                    CurrentSettings.MaxCanEvalResult = breakoutInWas;
                    return true;
                }
                if (IsNullOrEmpty(vv2)) break;
                vv = vv2;
                continue;
            }
            
            if (!IsNullOrEmpty(vv) && success)
            {
                if (Unifiable.ToStringLValue(vv) == Unifiable.ToStringLValue(vv1))
                {
                    // looper
                    output = vv;
                    return false;
                }
                // the best case
                output = vv;
                return true;
            }
            if (IsNull(vv)) return false;
            output = vv;
            if (IsEMPTY(vv))
            {
                success = IsSilentTag(childHandlerU.templateNode);
                return success;
            }
            output = vv;
            return success;
        }

        /// <summary>
        /// By calling this and not just CompleteProcess() 
        /// You've ensure we have a proper calling context
        /// </summary>
        public virtual Unifiable CompleteAimlProcess()
        {
            if (finalResult.IsValid) return finalResult.Value;
            if (RecurseResultValid) return RecurseResult;

            ThreadStart OnExit = EnterTag(request, templateNode, query);
            try
            {
                if (RecurseResultValid) return RecurseResult;
                string src;
                if (query.IsSourceRequest(this, out src))
                {
                    return src ?? OuterSource();
                }
                var test = CompleteProcessU();
                if (Unifiable.IsNull(test))
                {
                    if (QueryHasFailed)
                    {
                        return FAIL;
                    }
                    test = GetTemplateNodeInnerText();
                    if (test == null) writeToLogWarn("NULL response in " + templateNode.OuterXml + " for " + query);
                    string value2;
                    if (CompleteEvaluatution(test, this, out value2))
                    {
                        test = value2;
                    }
                }
                if (RecurseResultValid)
                {
                    if (test == RecurseResult)
                    {
                        return test;
                    }
                }
                if (test == null)
                {
                    writeToLogWarn("STILL NULL response in " + templateNode.OuterXml + " for " + query);
                }
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
            if (IsNull(t2)) return 1.0f;
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
            var prevPassFail = QueryResultsCurrent();
            InUnify = true;
            return () =>
                       {
                           InUnify = prevUnify;
                           NamedValuesFromSettings.UseLuceneForGet = prev;
                           QueryResultsRestore(prevPassFail);
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

        public Unifiable GET_FAIL()
        {
            this.QueryHasFailedN++;
            return FAIL;
        }
        public Unifiable Failure(string p)
        {
            writeToLog("<!-- FAILURE: " + (p + ToString()).Replace("<!--", "<#-").Replace("-->", "-#>") + "-->");
            return GET_FAIL();
        }

        public Unifiable Succeed(object p0)
        {
            Succeed();
            if (true) return think.THINKYTAG;
            string p = p0.ToString();
            return "<!-- SUCCEED: " + p.Replace("<!--", "<#-").Replace("-->", "-#>") + "-->";
        }

        public void Succeed()
        {
            if (query != null && query.CurrentTemplate != null)
            {
                string type = GetType().Name;
                double defualtReward = query.GetSucceedReward(type);
                double templateScore = GetAttribValue<double>(templateNode, "score", () => defualtReward,
                                                      ReduceStarAttribute<double>);
                templateScore *= CurrentQuery.CurrentTemplate.TemplateRating;
                double beforerating = request.TopLevelScore;
                double newrating = beforerating*templateScore;
                request.TopLevelScore = newrating;
                if (false)
                {
                    string str = SafeFormat("TSCORE {1}<-{2}*{3}", newrating, beforerating, templateScore);
                    writeToLog(str);
                }
            }
            this.QueryHasSuceededN++;
        }
        protected virtual Unifiable ProcessSucceed()
        {
            string s = InnerSource();
            if (string.IsNullOrEmpty(s))
            {
                s = OuterSource();
            }
            Unifiable res = Succeed(s);
            if (RecurseResultValid) return RecurseResult;
            return res;
        }

        protected T ReduceStarAttribute<T>(IConvertible arg) where T : IConvertible
        {
            bool found;
            return ReduceStar<T>(arg, query, query, out found);
        }

        protected Unifiable ReduceStarAttribute(IConvertible arg)
        {
            bool found;
            return ReduceStar<Unifiable>(arg, query, query, out found);
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
            // becasue we are gettign "star" content we must not be deterministic
            // ?? IsDeterministic = false;
            XmlNode starNode = getNodeAndSetSiblingNode("<star />", templateNode);
            LineInfoElement.unsetReadonly(starNode);
            star recursiveStar = new star(this.Proc, this.user, this.query, this.request, this.result, starNode);
//          recursiveStar.SetParent(this);
            var vv = recursiveStar.ProcessAimlChange();
            //var vv2 = recursiveStar.CompleteAimlProcess(););
            if (!IsNullOrEmpty(vv)) return vv;
            writeToLogWarn("GetStarContent ERROR: NULL <- " + vv);
            return vv;
        }

        protected Unifiable callSRAI(string starContent)
        {
            if (RecurseResultValid)
            {
                return RecurseResult;
            }
            if (InUnify)
            {
                return null;
            }
            if (!request.CanProcess(starContent)) return null;
            XmlNode sraiNode = getNodeAndSetSiblingNode(String.Format("<srai>{0}</srai>", starContent), templateNode);
            LineInfoElement.unsetReadonly(sraiNode);
            srai sraiHandler = new srai(this.Proc, this.user, this.query, this.request, this.result, sraiNode);
            sraiHandler.KnowsCanProcess = true;
            var vv = sraiHandler.CompleteAimlProcess();// Transform();
            if (Unifiable.IsNull(vv))
            {
                writeToLogWarn("CALLSRAI ERROR: NULL <- " + starContent);
                return vv;
            }
            if (Unifiable.IsEMPTY(vv))
            {
                writeToLogWarn("CALLSRAI EMPTY: <- " + starContent);
                sraiNode = getNodeAndSetSiblingNode(String.Format("<srai>{0}</srai>", starContent), templateNode);
                LineInfoElement.unsetReadonly(sraiNode);
                sraiHandler = new srai(this.Proc, this.user, this.query, this.request, this.result, sraiNode);                
                vv = sraiHandler.CompleteAimlProcess();// Transform();
                return vv;
            }
            RecurseResult = vv;
            return vv;
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
            if (!templateNodeHasChildNodes && IsStillStarAtomically)
            {
                // atomic version of the node
                Unifiable templateResult = GetStarContent();
                bool starFailed = IsNull(templateResult);
                if (starFailed)
                {
                    if (saveResultsOnChildren && throwOnSave)
                    {
                        Proc.RaiseError(new InvalidOperationException("save NULLL ResultsOnChildren! " + this));
                    }
                    QueryHasFailedN++;
                    templateResult = UnifiableEmpty;
                }
                else
                {
                    QueryHasSuceededN++;
                }
                Unifiable a = afterEachOrNull(templateResult);
                if (saveResultsOnChildren)
                {
                    SaveResultOnChild(templateNode, a);
                }
                if (!IsNullOrEmpty(a))
                {
                    return a;
                }
                if (a == null)
                {
                    return UnifiableEmpty;
                }
                return a;
            }
            // needs some recursion
            var vv = RecurseReal(afterEachOrNull, templateNode, saveResultsOnChildren);
            if (!IsNullOrEmpty(vv))
            {
                return vv;
            }
            if (vv == null)
            {
                return UnifiableEmpty;
            }
            return vv;
        }

        protected Unifiable UnifiableEmpty
        {
            get { return Unifiable.Empty;  }
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
                if (Unifiable.IsNull(value))
                {
                    writeToLogWarn("CheckValue NULL = '" + Unifiable.DescribeUnifiable(value) + "'");
                    return value;
                }
                if (Unifiable.IsEMPTY(value))
                {
                    //writeToLogWarn("CheckValue EMPTY = '" + value + "'");
                    return value;
                }
                if (!IsUnevaluated(value))
                {
                    return value;
                }
                string valueStr;
                if (CompleteEvaluatution(value, this, out valueStr))
                {
                    //RecurseResult = vv;
                    return valueStr;
                }
                // writeToLogWarn("CheckValue XML = '" + (string)value + "'");
                return value;
            }
        }

        public static bool IsUnevaluated(object value)
        {
            string v = Unifiable.ToStringLValue(value);
            if (value == null) return false;
            if ((!v.Contains("<a href") && !v.Contains("<!--")))
            {
                if (v.Contains("<"))
                {
                    return true;
                }
                else if (v.Contains("&"))
                {
                    return true;
                }
            }
            return false;
        }

        protected AIMLTagHandlerU GetChildTagHandler(XmlNode childNode)
        {
            var Proc = this.Proc.TagHandling;
            User user = request.Requester ?? this.user;
            AIMLTagHandlerU part = Proc.GetTagHandler(user, query, request, result, childNode, this);
            //AddChild(part);
            return part;
        }

        static public Unifiable ProcessAiml(string vv, out bool success, AIMLTagHandlerU tagHandlerU)
        {
            AIMLTagHandlerU parent = tagHandlerU;
            if (DLRConsole.IsTooDeep())
            {
                success = false;
                return tagHandlerU.Failure("TOO DEEP DOING '" + vv + "'");
            }
            var vv1 = vv;
            var tn = StaticAIMLUtils.getTemplateNode(vv1);

            var passFails = parent.QueryResultsCurrent();

            Unifiable vv2 = null;
            try
            {
                vv2 = ProcessTagHandlerNode(tn, false, false, out success, tagHandlerU);
            }
            finally
            {
                parent.QueryResultsRestore(passFails);
            }

            parent.writeToLog("Continue sub-Processing of '{0}' -> '{1}'", vv, vv2);
            if (vv == vv2)
            {
                Unifiable vv3 = null;
                parent.Proc.TraceTest(" ", () =>
                {
                    bool successM;
                    vv3 = ProcessTagHandlerNode(tn, false, false, out successM, tagHandlerU);
                });                
                success = false;
                return null;
            }
            if (!success)
            {
                return null;
            }
            if (CompleteEvaluatution(vv2, tagHandlerU, out vv))
            {
                return vv;
            }
            vv = vv2;
            if (!IsNullOrEmpty(vv))
            {
                return vv;
            }
            return vv;
        }

        protected Unifiable ProcessChildNode(XmlNode childNode)
        {
            bool success;
            var chosenXML = Unifiable.InnerXmlText(childNode);
            AIMLTagHandlerU tagHandlerUChild = GetChildTagHandler(childNode);
            var vv = ProcessChildNode(childNode, ReadOnly, false, out success, tagHandlerUChild);
            if (!success)
            {
                Proc.TraceTest(TextPatternUtils.SafeFormat("RE-EVALING CHILD '{0}' '{1}'", Unifiable.DescribeUnifiable(vv), childNode),
                               () => ProcessChildNode(childNode, ReadOnly, false, out success, tagHandlerUChild));
                //return null;
                QueryHasFailedN++;
            }
            if (!IsNullOrEmpty(vv))
            {
                if (success) QueryHasSuceededN++;
                return vv;
            }
            return vv;
        }

        protected Unifiable ProcessChildNode(XmlNode childNode, bool protectChildren, bool saveOnInnerXML, out bool success, AIMLTagHandlerU tagHandlerUChild)
        {
            AIMLTagHandlerU outerParent = this;
            var passFails = outerParent.QueryResultsCurrent();
            Unifiable vv = null;
            bool childSuccess;
            try
            {
                vv = ProcessTagHandlerNode(childNode, protectChildren, saveOnInnerXML, out childSuccess,
                                        tagHandlerUChild);
                if (!tagHandlerUChild.IsDeterministic && IsDeterministic)
                {
                    IsDeterministic = false;
                }
            }
            finally
            {
                outerParent.QueryResultsRestore(passFails);
            }
            if (!childSuccess)
            {
                Proc.TraceTest("!childSuccess in " + tagHandlerUChild, () =>
                {
                    vv = ProcessTagHandlerNode(childNode, protectChildren, saveOnInnerXML, out childSuccess,
                                            tagHandlerUChild);
                });
                if (IsNull(vv))
                {
                    success = false;
                    return vv;
                }
                success = true;
                return vv;
            }
            var vv1 = vv;
            if (IsNull(vv))
            {
                success = false;
                return vv;
            }
            if (IsUnevaluated(vv))
            {
                success = childSuccess;
                Unifiable output;
                if (CompleteEvaluatution(vv, tagHandlerUChild, out output))
                {
                    if (output == vv1)
                    {
                        writeToLogWarn("CompleteEvaluatution: trying to return the same items for " + vv1);
                        return null;
                    }
                    return output;
                }
                if (output == vv1)
                {
                    writeToLogWarn("IsUnevaluated: trying to return the same items for " + vv1);
                    return null;
                }
                return vv1;
            }
            if (!IsNullOrEmpty(vv) || IsSilentTag(childNode))
            {
                success = true;
                return vv;
            }
            if (IsValue(vv))
            {
                success = true;
                return vv;
            }
            success = childSuccess;
            return vv;
        }

        static public Unifiable ProcessNonElement(bool saveOnInnerXML, XmlNode childNode, out bool success)
        {
            if (saveOnInnerXML && throwOnSave) AltBot.RaiseErrorStatic(new InvalidOperationException("saveOnInnerXML! " + childNode)); 
            
            {
                string childNodeInnerXml = childNode.InnerXml;
                if (IsValueSetter(childNodeInnerXml))
                {
                    string s = ValueText(childNodeInnerXml);
                    success = true;
                    return s;
                }
                if (childNode.NodeType == XmlNodeType.Text)
                {
                    string value = Trim(TextNodeValue(childNode));

                    if (IsValueSetter(value))
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
                    if (saveOnInnerXML)
                    {
                        childNode.InnerXml = XmlValueSettable("");
                    }
                    return String.Empty;
                }
                else if (childNode.NodeType == XmlNodeType.Element)
                {
                    success = false;
                    return childNode.OuterXml;
                }
                else if (childNode.NodeType == XmlNodeType.Whitespace)
                {
                    success = true;
                    return childNode.OuterXml;
                }
                else
                {
                    throw new Exception("NonElement Child?!" + childNode.NodeType + " : " + ToXmlValue(childNode));
                }
            }
        }

        static protected Unifiable ProcessTagHandlerNode(XmlNode childNode, bool protectChildren, bool saveOnInnerXML, out bool success, AIMLTagHandlerU tagHandlerUChild)
        {            
            OutputDelegate writeToLogWarn = tagHandlerUChild.writeToLogWarn;
            OutputDelegate writeToLog = tagHandlerUChild.writeToLog;

            AIMLTagHandlerU parentHandlerU =  tagHandlerUChild.Parent;

            if (saveOnInnerXML && throwOnSave)
            {
                throw tagHandlerUChild.Proc.RaiseError(new InvalidOperationException("saveOnInnerXML! " + tagHandlerUChild));
            }
            try
            {
                bool wasNonElement;
                Unifiable vv1 = ProcessNonElement(saveOnInnerXML, childNode, out wasNonElement);
                if (wasNonElement)
                {
                    success = true;
                    return vv1;
                }
                else
                {
                    var request = tagHandlerUChild.request;
                    var result = tagHandlerUChild.result;
                    var user = tagHandlerUChild.user;
                    var query = tagHandlerUChild.query;

                    bool suspendingLimits = request.IsToplevelRequest || request.SuspendSearchLimits
                                            || tagHandlerUChild.SuspendLimits || (parentHandlerU != null && parentHandlerU.SuspendLimits);
                    
                    

                    bool copyParent, copyChild;
                    copyParent = copyChild = protectChildren;

                    var Proc = tagHandlerUChild.Proc.TagHandling;
                    //if (tagHandlerChild == null) tagHandlerChild = Proc.GetTagHandler(user, query, request, result, childNode, parent);

                    string value = Proc.processNode(childNode, query,
                                                    request, result, user,
                                                    parentHandlerU, copyChild, copyParent,
                                                    tagHandlerUChild, suspendingLimits, out success);
                    var vv2 = value;
                    //success = !IsIncomplete(value);
                    if (tagHandlerUChild.QueryHasFailed) success = false;
                    if (tagHandlerUChild.QueryHasSuceeded) success = true;

                    if (!AltBot.BE_COMPLETE_NOT_FAST)
                    {
                        return value;
                    }

                    success = true;                   
                    if (IsNull(value))
                    {
                        value = tagHandlerUChild.TryProcessingNodeAgain(parentHandlerU, childNode, tagHandlerUChild, Proc, copyChild, copyParent, ref suspendingLimits, ref value, writeToLogWarn, ref success);
                    }
                    if (IsEMPTY(value))
                    {
                        if (IsSilentTag(childNode) && success)
                        {
                            if (saveOnInnerXML)
                            {
                                if (parentHandlerU != null) parentHandlerU.SaveResultOnChild(childNode, "+++ ");
                            }
                            return value;
                        }
                        value = tagHandlerUChild.TryProcessingNodeAgain(parentHandlerU, childNode, tagHandlerUChild, Proc, copyChild, copyParent, ref suspendingLimits, ref value, writeToLogWarn, ref success);
                    }
                    if (vv2 != value)
                    {
                        success = !IsNull(value);
                    }
                    if (!IsNullOrEmpty(value))
                    {
                        if (tagHandlerUChild.QueryHasFailed) success = false;
                        if (tagHandlerUChild.QueryHasSuceeded)
                            success = true;
                    }

                    if (saveOnInnerXML)
                    {
                        if (!success) return null;
                        if (parentHandlerU != null) parentHandlerU.SaveResultOnChild(childNode, value);
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

        Unifiable TryProcessingNodeAgain(AIMLTagHandlerU parent, XmlNode childNode, AIMLTagHandlerU tagHandlerUChild, 
            TagHandlerProcessor Proc, bool copyChild, bool copyParent, 
            ref bool suspendingLimits, ref string value, OutputDelegate writeToLogWarn, ref bool success)
        {
            success = false;
            if (tagHandlerUChild.QueryHasSuceeded)
            {
                success = true;
                if (tagHandlerUChild.QueryHasFailed) success = false;
                writeToLogWarn = tagHandlerUChild.writeToLogWarn;
                if (parent != null) parent.IsOverBudget = tagHandlerUChild.IsOverBudget;
            }
            if (tagHandlerUChild.QueryHasFailed && success)
            {
                writeToLogWarn("ERROR QueryHasFailed + success?! AIMLTRACE " + value + " -> " + childNode.OuterXml + "!");
            }
            // cant do much bette than the first call
            if (suspendingLimits || tagHandlerUChild.ExpandingSearchWillYieldNoExtras || InUnify || !IsDeterministic)
            {
                writeToLog("ERROR GIVINGUP ON AIMLTRACE " + value + " -> " + childNode.OuterXml + "!");
                return value;
            }
            string whyComplete = request.WhyComplete;
            bool isTopLevel = request.IsToplevelRequest;
            if (!isTopLevel && request.SraiDepth.IsOverMax)
            {
                writeToLogWarn("ERROR GIVINGUP ON AIMLTRACE " + whyComplete + " -> " + childNode.OuterXml + "!");
                return value;
            }
            int sraiDepth = request.SraiDepth.Current;
            suspendingLimits = true;
        
            var oldValue = value;
            if (this != tagHandlerUChild)
            {
                writeToLog("AIMLTRACE: TAG HANDLERCHILD NOT THIS " + childNode.OuterXml + "!");
            }
            if (sraiDepth > 3)
            {
                success = false;
                return value;
            }
            bool childSuccess;
            value = Proc.processNode(childNode, query,
                                     request, result, user,
                                     parent, copyChild, copyParent,
                                     tagHandlerUChild, suspendingLimits, out childSuccess);

            success = !IsNull(value) || childSuccess;
            if (success)
            {
                if (tagHandlerUChild.QueryHasFailed) success = false;
                if (tagHandlerUChild.QueryHasSuceeded) success = true;
                writeToLogWarn("RECOVERED AIMLTRACE " + value + " -> " + childNode.OuterXml + "!");
                return value;
            }
            return value;
        }

        virtual protected Unifiable Recurse()
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
                    if (IsStillStarAtomically)
                    {
                        if (IsNull(real)) return null;
                    }
                    if (IsIncomplete(real)) return null;
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
            return RecurseReal(null, node, saveOnChildren);
        }
        protected Unifiable RecurseReal(Func<Unifiable, Unifiable> afterEachOrNull, XmlNode node, bool saveOnChildren)
        {
            //Unifiable templateNodeInnerText;//= this.templateNodeInnerText;
            Unifiable templateResult = Unifiable.CreateAppendable();
            if (node.HasChildNodes)
            {
                ReadOnly = node.IsReadOnly;
                int goods = 0;
                // recursively check
                foreach (XmlNode childNode in node.ChildNodes)
                {
                    if (childNode.NodeType == XmlNodeType.Comment) continue;

                    bool success;
                    AIMLTagHandlerU tagHandlerUChild = GetChildTagHandler(childNode);
                    Unifiable found = ProcessChildNode(childNode, ReadOnly, false, out success, tagHandlerUChild);
                    var found1 = found;
                    if (success)
                    {
                        if (afterEachOrNull != null)
                        {
                            if (found == null) found = UnifiableEmpty;
                            found = afterEachOrNull(found);
                        }
                        if (saveOnChildren)
                        {
                            if (!IsEMPTY(found) && found1 != null)
                            {
                                SaveResultOnChild(childNode, found);
                            }
                            else
                            {
                                if (IsNull(found))
                                {
                                    success = false;
                                }
                            }
                        }
                    }
                    else
                    {
                        if (!tagHandlerUChild.IsDeterministic && IsDeterministic)
                        {
                            IsDeterministic = false;
                        }
                        if (saveOnChildren) writeToLogWarn("!success and writeToChild!");
                        QueryHasFailedN++;
                        return null;
                    }

                    if (IsNullOrEmpty(found) && !StaticAIMLUtils.IsSilentTag(childNode))
                    {
                        if (!IsNull(found) && !QueryHasFailed)
                        {
                            success = true;
                        }
                        if (success)
                        {
                            return "";
                        }
                        writeToLogWarn("IsNullOrEmpty: " + ToXmlValue(childNode));
                        found = ProcessChildNode(childNode, ReadOnly, false, out success, tagHandlerUChild);
                        QueryHasFailedN++;
                        return null;
                    }
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
                        if (goods == 0) QueryHasFailedN++;
                    }
                    if (goods == 0) QueryHasFailedN++;
                }
                //templateNodeInnerText = templateResult;//.ToString();
                if (!IsNullOrEmpty(templateResult))
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
                if (IsStillStarAtomically)
                {
                    try
                    {
                        // atomic version of the node
                        Unifiable starContent = GetStarContent();
                        bool starFailed = IsNull(starContent);
                        if (starFailed)
                        {
                            QueryHasFailed = true;
                            return UnifiableEmpty;
                        }
                        innerResult.Value = starContent;
                        if (!Unifiable.IsNullOrEmpty(starContent))
                        {
                            return starContent;
                        }
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
                Unifiable before = InnerSource();
                return CheckValue(before);
            }
        }

        protected virtual string InnerSource()
        {
            Unifiable before = InnerXmlText(templateNode);
            return before;
        }

        protected virtual string OuterSource()
        {
            string src;
            if (query.IsSourceRequest(this, out src))
            {
                if (src != null) return src;
            }
            bool wasNonElement;
            string itext = ProcessNonElement(false, templateNode, out wasNonElement);
            return itext;
        }

        /// <summary>
        /// Do a transformation on the Unifiable found in the InputString attribute
        /// </summary>
        /// <returns>The resulting transformed Unifiable</returns>
        public override Unifiable TransformU()
        {
            if (!IsNullOrEmpty(this.InputStringU))
            {
                if (RecurseResultValid)
                {

                    return RecurseResult;
                }
                var finalResultValue = this.CompleteAimlProcess();// this.ProcessAimlChange();
                if (!Unifiable.IsNullOrEmpty(finalResultValue))
                {
                    RecurseResult = finalResultValue;
                    return finalResultValue;
                }
                if (!Unifiable.IsNull(finalResultValue))
                {
                    RecurseResult = finalResultValue;
                }
                return finalResultValue;
            }
            else
            {
                return Unifiable.Empty;
            }
        }

        public override Unifiable CompleteProcessU()
        {
            if (RecurseResultValid) return RecurseResult;
            var vv1 = ProcessAChange();
            Unifiable vv2;
            if (CompleteEvaluatution(vv1, this, out vv2))
            {
                RecurseResult = vv2;
                return vv2;
            }
            // RecurseResult = vv;
            if (!Unifiable.IsNullOrEmpty(vv1))
            {
                return vv1;
            }
            if (!Unifiable.IsNull(vv1))
            {
                return vv1;
            }
            return vv1;
        }

        protected string ProcessAChange()
        {
            return ProcessChangeU();
        }

        /*
        public override Unifiable CompleteProcess()
        {
            if (RecurseResultValid)
            {
                return RecurseResult;
            }
            if (!IsStarted)
            {
                IsStarted = true;
                ProcessChange();
            }
            DLRConsole.DepthCheck();
            var vv = ProcessAimlChange();
            return vv;
        }*/
        /* public override Unifiable CompleteProcess()
         {
             //#if false
             return RecurseProcess();
         }*/
        private bool CheckReturnValue(Unifiable resultValue)
        {
            if (IsNull(resultValue))
            {
                ResetValues(true);
                var problem = "CompleteProcess() == NULL " + LineNumberTextInfo();
                writeToLogWarn(problem);
                Proc.TraceTest(problem, () => ProcessAChange());
                return false;
            }
            if (resultValue.IsWildCard)
            {
                return false;
            }
            return true;
        }

        protected Unifiable OutputFromTagHandlers(IEnumerable<AIMLTagHandlerU> aimlTagHandlers0)
        {
            IEnumerable<AIMLTagHandlerU> aimlTagHandlers = null;
            Unifiable appendable = Unifiable.CreateAppendable();
            lock (aimlTagHandlers0)
            {
                var LoopOver = new List<AIMLTagHandlerU>(aimlTagHandlers0);
                aimlTagHandlers = LoopOver;
            }
            Request request = this.request;
            foreach (var childTagHandler in aimlTagHandlers)
            {
                var vv = childTagHandler.CompleteAimlProcess();

                if (childTagHandler.QueryHasSuceeded)
                {
                    Unifiable checkValue = childTagHandler.CheckValue(vv);
                    appendable.Append(checkValue);
                    this.QueryHasSuceededN++;
                }
                else if (childTagHandler.QueryHasFailed)
                {
                    this.QueryHasFailedN++;
                }
                else
                {
                    if (vv == null)
                    {
                        bool wasSuspended = request.SuspendSearchLimits;
                        request.SuspendSearchLimits = true;
                        childTagHandler.ResetValues(true);
                        childTagHandler.SuspendLimits = true;
                        try
                        {
                            vv = childTagHandler.ProcessAimlChange();
                        }
                        finally
                        {
                            request.SuspendSearchLimits = wasSuspended;
                        }
                    }
                    if (vv == null)
                    {
                        continue;
                    }
                    Unifiable checkValue = childTagHandler.CheckValue(vv);
                    appendable.Append(checkValue);
                }

            }
            if (!IsEMPTY(appendable))
            {
                if (QueryHasSuceededN > 0 && QueryHasFailedN == 0)
                {
                    innerResult.Value = appendable;
                }
                return appendable;
            }
            return appendable;
        }

        /*
        protected IEnumerable<AIMLTagHandler> CreateChildTagHandlers(IEnumerable<XmlNode> xmlNodes)
        {
            List<AIMLTagHandler> aimlTagHandlers = new List<AIMLTagHandler>();
            foreach (var xmlNode in xmlNodes)
            {
                var childTagHandler = GetChildTagHandler(xmlNode);
                aimlTagHandlers.Add(childTagHandler);
            }
            return aimlTagHandlers;
        }
        */

        public virtual Unifiable RecurseProcess()
        {
            //#endif
            if (RecurseResultValid)
            {
                return RecurseResult;
            }
            AIMLTagHandlerU tagHandlerU = this;
            Unifiable recursiveResult = null;
            if (tagHandlerU.isRecursive)
            {
                if (templateNode.HasChildNodes)
                {
                    recursiveResult = RecurseReal(templateNode, true);
                }
                else
                {
                    recursiveResult = InnerXmlText(templateNode);
                }
                string v = tagHandlerU.ProcessAChange();
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
                return NonRecusiveResult();
            }
        }

        private Unifiable NonRecusiveResult()
        {
            Unifiable recursiveResult = RecurseReal(templateNode, false);
            if (!IsNull(recursiveResult)) return recursiveResult;
            string resultNodeInnerXML = templateNode.OuterXml;//.ProcessChange();
            XmlNode resultNode = getNodeAndSetSiblingNode("<template>" + resultNodeInnerXML + "</template>", templateNode);
            LineInfoElementImpl.unsetReadonly(resultNode);
            if (resultNode.HasChildNodes)
            {
                recursiveResult = ProcessChildNode(resultNode);
                if (!IsNullOrEmpty(recursiveResult))
                {
                    RecurseResult = recursiveResult;
                    return recursiveResult;
                }
                recursiveResult = RecurseReal(resultNode, false);
                if (!IsNullOrEmpty(recursiveResult))
                {
                    RecurseResult = recursiveResult;
                    return recursiveResult;
                }
                return recursiveResult;
            }
            else
            {
                return Unifiable.InnerXmlText(resultNode);
            }
        }

        public virtual void SaveResultOnChild(XmlNode node, string value)
        {
            Unifiable value2 = value;
            bool emptyIsOK = IsSilentTag(node);
            if (value != null) value = ValueText(value);
            if (IsUnevaluated(value))
            {
                writeToLogWarn("XML onto child " + value);
                if (CompleteEvaluatution(value, this, out value2))
                {
                    value = value2;
                }
            }
            if (value != null) value = ValueText(value);
            if (value == null || (IsEMPTY(value) && !emptyIsOK))
            {
                Unifiable errmsg = "-!SaveResultOnChild AIMLTRACE " + value + " was " + value2 + " -> " + node.OuterXml;
                writeToLog(errmsg);
                if (throwOnSave)
                {
                    throw Proc.RaiseError(new InvalidOperationException("save NULL ResultsOnChildren! " + this + " " + errmsg));
                }
            }
            if (InUnify)
            {
                return;
            }
            //if (value == null) return;
            //if (value == "") return;
            if (!emptyIsOK)
                value = CheckValue(value);

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
            string templateNodeName = this.templateNode.Name;
            if (templateNodeName.ToLower() == name) return true;
            if (name.Contains(","))
            {
                string[] nameSplit = name.Split(",".ToCharArray(), StringSplitOptions.RemoveEmptyEntries);
                foreach (string s in nameSplit)
                {
                    if (templateNodeName == s) return true;
                }
                foreach (string s in nameSplit)
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
                if (succeed)
                {
                    return v;
                }
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
                writeToLogWarn("InUnify " + namedEffect);
                return;
            }
            if (QueryHasFailed)
            {
                writeToLog("SKIPPING " + namedEffect);
                return;
            }
            AIMLTagHandlerU tagHandlerU = FirstTagHandlerOrOuterMost("template");
            if (Parent == null || tagHandlerU == this || tagHandlerU == null)
            {
                query.AddSideEffect(namedEffect, func);
            }
            else
            {
                tagHandlerU.AddSideEffect(namedEffect, func);
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
            AIMLTagHandlerU tagHandlerU = FirstTagHandlerOrOuterMost("template");
            if (Parent == null || tagHandlerU == this || tagHandlerU == null)
            {
                query.LocalSideEffect(namedEffect, enter, exit);
            }
            else
            {
                tagHandlerU.LocalSideEffect(namedEffect, enter, exit);
            }
        }

        protected AIMLTagHandlerU FirstTagHandlerOrOuterMost(string type)
        {
            var current = this;
            while (true)
            {
                if (current.IsNode(type))
                {
                    return current;
                }
                if (null == current.Parent) return current;
                current = current.Parent;
            }
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


        public void AddChild(AIMLTagHandlerU part)
        {
            TagHandlerChilds = TagHandlerChilds ?? new List<AIMLTagHandlerU>();
            TagHandlerChilds.Add(part);
        }
        public virtual void Dispose()
        {
            lock (this)
            {
                if (IsDisposing) return;
                IsDisposing = true;
            }
            if (Parent != null) Parent.Dispose();
            if (TagHandlerChilds != null)
            {
                foreach (AIMLTagHandlerU child in TagHandlerChilds)
                {
                    child.Dispose();
                }
                TagHandlerChilds.Clear();
                TagHandlerChilds = null;
            }
            return;
        }

        protected virtual ICollection<XmlNode> SelectNodes(XmlNodeList candidates)
        {
            List<XmlNode> matchedNodes = new List<XmlNode>();
            Unifiable outerName = GetAttribValue("name,var", null);
            Unifiable outerValue = GetAttribValue("value", null);

            bool outerNamePresent = !IsIncomplete(outerName);
            bool outerValuePresent = !IsIncomplete(outerValue);

            if (candidates == null || candidates.Count == 0)
            {
                return matchedNodes;
            }

            foreach (XmlNode childLINode in candidates)
            {
                string name = GetAttribValue(childLINode, "name,var", NullStringFunct, ReduceStarAttribute<string>);

                bool namePresent = !string.IsNullOrEmpty(name);
                if (!namePresent) name = outerName;


                Unifiable value = GetAttribValue<Unifiable>(childLINode, "value", () => null,
                    ReduceStarAttribute<Unifiable>);


                bool valuePresent = !IsIncomplete(value);
                if (!valuePresent) value = outerValue;

                //skip comments
                if (childLINode.LocalName.ToLower() == "#comment")
                {
                    //matchedNodes.Add(childLINode);
                    continue;
                }

                // non-<li> elements are freebies
                if (childLINode.LocalName.ToLower() != "li")
                {
                    matchedNodes.Add(childLINode);
                    continue;
                }

                if ((outerNamePresent || namePresent) && (valuePresent || outerValuePresent))
                {
                    // 1 name and 1 value (outer or inner + outer or inner)
                    bool succeed;
                    Unifiable actualValue = GetActualValue(childLINode, name, childLINode.Name, out succeed,
                                                           query);
                    if (IsPredMatch(value, actualValue, query))
                    {
                        matchedNodes.Add(childLINode);
                        continue;
                    }
                    continue;
                }
                // 0 names and 0 values default freebie
                if (!namePresent && !valuePresent)
                {
                    matchedNodes.Add(childLINode);
                    continue;
                }
                // name and value must be present
                if (outerNamePresent && !namePresent && !valuePresent && outerValuePresent)
                {
                    // 1 name and 1 value (outer + outer)
                    bool succeed;
                    Unifiable actualValue = GetActualValue(childLINode, name, childLINode.Name, out succeed,
                                   query);
                    if (IsPredMatch(value, actualValue, query))
                    {
                        matchedNodes.Add(childLINode);
                    }
                    continue;
                }
                if (!outerNamePresent && namePresent && !valuePresent && outerValuePresent)
                {
                    // 1 name and 1 value (inner + outer)
                    bool succeed;
                    Unifiable actualValue = GetActualValue(childLINode, name, childLINode.Name, out succeed,
                                   query);
                    if (IsPredMatch(value, actualValue, query))
                    {
                        matchedNodes.Add(childLINode);
                    }
                    continue;
                }
                if (!namePresent && !outerNamePresent)
                {
                    // 0 names and 0 values default freebie
                    if (!outerValuePresent && !valuePresent)
                    {
                        matchedNodes.Add(childLINode);
                        continue;
                    }
                    // 0 names and 2 values
                    if (outerValuePresent && valuePresent)
                    {
                        if (IsPredMatch(value, outerValue, query))
                        {
                            matchedNodes.Add(childLINode);
                        }
                        continue;
                    }
                }
                // 1 name and 1 value present
                if (!outerNamePresent && namePresent && valuePresent && !outerValuePresent)
                {
                    bool succeed;
                    Unifiable actualValue = GetActualValue(childLINode, name, childLINode.Name, out succeed,
                                                           query);
                    if (IsPredMatch(value, actualValue, query))
                    {
                        matchedNodes.Add(childLINode);
                    }
                    continue;
                }

                UnknownCondition();
            }
            if (matchedNodes.Count == 0)
            {
                writeToLogWarn("ERROR: no matched nodes " + LineNumberTextInfo());
            }
            return matchedNodes;
        }

        public void UnknownCondition()
        {
            writeToLogWarn("ERROR Unknown conditions " + LineNumberTextInfo());
        }

        protected Unifiable OutputFromNodes(ICollection<XmlNode> nodes, Predicate<XmlNode> predicate)
        {
            List<AIMLTagHandlerU> aimlTagHandlers = GetAIMLTagHandlers(nodes, predicate);

            Unifiable appendable = OutputFromTagHandlers(aimlTagHandlers);
            Unifiable appendable1;
            if (CompleteEvaluatution(appendable, this, out appendable1))
            {
                RecurseResult = appendable1;
                return appendable1;
            }
            return CheckValue(appendable);
        }


        protected List<AIMLTagHandlerU> GetAIMLTagHandlers(ICollection<XmlNode> nodes, Predicate<XmlNode> predicate)
        {
            TagHandlerChilds = TagHandlerChilds ?? new List<AIMLTagHandlerU>();
            lock (TagHandlerChilds)
            {
                List<AIMLTagHandlerU> aimlTagHandlers = new List<AIMLTagHandlerU>();
                foreach (var xmlNode in nodes)
                {
                    if (predicate(xmlNode))
                    {
                        var childTagHandler = GetChildTagHandler(xmlNode);
                        aimlTagHandlers.Add(childTagHandler);
                    }
                }
                var aimlTagHandlerz = aimlTagHandlers;

                lock (aimlTagHandlerz)
                {
                    if (aimlTagHandlers.Count == this.TagHandlerChilds.Count)
                    {
                        aimlTagHandlers = TagHandlerChilds;
                    }
                    else
                    {
                        TagHandlerChilds = aimlTagHandlers;
                    }
                    return aimlTagHandlers;
                }
            }
        }

        protected void QueryResultsRestore(int[] passFail)
        {
            QueryHasSuceededN = passFail[0];
            QueryHasFailedN = passFail[1];
        }

        protected int[] QueryResultsCurrent()
        {
            return new int[] { QueryHasSuceededN, QueryHasFailedN };
        }

    }

    public interface IAIMLTransaction
    {
        AltBot TargetBot { get; }
        SubQuery CurrentQuery { get; }
        User user { get; }
        Request request { get; }
        Result result { get; }
    }
}