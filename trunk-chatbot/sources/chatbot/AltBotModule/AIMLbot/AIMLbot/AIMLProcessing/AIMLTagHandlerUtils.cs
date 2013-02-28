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
    public interface EmptyIsNotFailure
    {
    }
    public interface NoReturnResult : EmptyIsNotFailure
    {
    }
    public interface CanReturnFailure
    {
    }

    public abstract partial class AIMLTagHandler : IAIMLTransaction, IXmlLineInfo, IDisposable
    {

        public bool IsStarted;
        public bool IsOverBudget;
        public bool IsDisposing;
        internal static bool throwOnSave = true;
        public bool SuspendLimits;

        protected virtual bool ExpandingSearchWillYieldNoExtras
        {
            get { return false; }
        }

        public bool IsDeterministic = true;
        public AIMLTagHandler Parent;

        /// <summary>
        /// The query that produced this node containing the wildcard matches
        /// </summary>
        //public SubQuery query;

        public TemplateInfo templateInfo;

        /// <summary>
        /// Default ctor to use when late binding
        /// </summary>
        public AIMLTagHandler()
        {
            ReadOnly = true;
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
        public AIMLTagHandler(AltBot bot,
                              User user,
                              SubQuery query,
                              Request request,
                              Result result,
                              XmlNode templateNode)
            : base(bot, templateNode.OuterXml, templateNode.OuterXml)
        {
            this.query = query;
            this.request = request;
            this.result = result;
            this.templateNode = templateNode;
            InputStringU = templateNode.OuterXml;
            initialString = InputString;
            this.user = user;
            ReadOnly = true;
            if (this.templateNode.Attributes != null) this.templateNode.Attributes.RemoveNamedItem("xmlns");
        }

        protected bool ReadOnly { get; set; }

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
                    FinalResult = ValueText(valueAsString);
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
            get
            {
                if (!ChatOptions.AIML_MAY_USE_FAILURE) return false;
                return query != null && query.HasFailed > 0;
            }
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
            get
            {
                if (!ChatOptions.AIML_MAY_USE_FAILURE) return true;
                return query != null && query.HasSuceeded > 0;
            }
            set
            {
                if (InUnify)
                {
                    writeToLog("InUnify QueryHasSuceeded=" + value);
                    return;
                }
                if (query != null) query.HasSuceeded += (value ? 1 : 0);
            }
        }

        public virtual int QueryHasFailedN
        {
            get
            {
                if (!ChatOptions.AIML_MAY_USE_FAILURE) return 0;
                if (query == null) return 0;
                return query.HasFailed;
            }
            set
            {
                if (value == 0 && query == null) return;
                query.HasFailed = value;
            }
        }

        public virtual int QueryHasSuceededN
        {
            get
            {
                if (query == null) return 1;
                return query.HasSuceeded;
            }
            set
            {
                if (query == null) return;
                query.HasSuceeded = value;
            }
        }

        protected bool InUnify;

        protected ResultCache finalResult = new ResultCache();
        protected ResultCache innerResult = new ResultCache();
        private List<AIMLTagHandler> TagHandlerChilds;
        public static bool SaveAIMLChange = true;
        public static bool SaveAIMLComplete = false;
        public static bool EnforceStartedBool = false;
        private bool IsNeedingInnerXMLSet = false;

        public Unifiable FAIL
        {
            get
            {
                if (!ChatOptions.AIML_MAY_USE_FAILURE)
                {
                    writeToLogError("AIML_WITHOUT_FAILURE!");
                }
                if (!QueryHasFailed)
                    if (!Debugger.IsAttached)
                    {
                        QueryHasFailed = true;
                    }
                return ChatOptions.AIML_FAILURE_INDICATOR;
            }
        }

        public virtual bool FinalResultValid
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
                var finalResultValue = this.FinalResult;
                {
                    if (IsNull(finalResultValue))
                    {
                        writeToLogWarn("IsNull _FinalResult to String.Empty at least!");
                    }
                    else if (IsIncomplete(finalResultValue))
                    {
                        writeToLogWarn("IsMissing _FinalResult to String.Empty at least!");
                    }
                }
                return finalResult.IsValid;
            }
            set
            {
                var finalResultValue = this.FinalResult;
                if (value)
                {
                    if (IsNull(finalResultValue))
                    {
                        writeToLogWarn("IsNull _FinalResult to String.Empty at least!");
                    }
                    else if (IsIncomplete(finalResultValue))
                    {
                        writeToLogWarn("IsMissing _FinalResult to String.Empty at least!");
                    }
                }
                finalResult.IsValid = value;
            }
        }

        /// <summary>
        /// Final Result (not "innerResult!")
        /// </summary>
        public virtual Unifiable FinalResult
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
                    if (!innerResult.IsValid)
                    {
                        return false;
                    }
                }
                if (!innerResult.IsValid)
                {
                    return false;
                }
                var recurseResultValue = this.RecurseResult;
                {
                    if (IsNull(recurseResultValue))
                    {
                        writeToLogWarn("IsNull _RecurseResult to String.Empty at least!");
                    }
                    else if (IsIncomplete(recurseResultValue))
                    {
                        writeToLogWarn("IsMissing _RecurseResult to String.Empty at least!");
                    }
                }
                return innerResult.IsValid;
            }
            set
            {
                var recurseResultValue = this.RecurseResult;
                if (value)
                {
                    if (IsNull(recurseResultValue))
                    {
                        writeToLogWarn("IsNull _RecurseResult to String.Empty at least!");
                    }
                    else if (IsIncomplete(recurseResultValue))
                    {
                        writeToLogWarn("IsMissing _RecurseResult to String.Empty at least!");
                    }
                }
                innerResult.IsValid = value;
            }
        }

        /// <summary>
        /// Recurse Result (not "Final Result!")
        /// </summary>
        public virtual Unifiable RecurseResult
        {
            get
            {
                if (!innerResult.IsValid)
                {
                    var vv = ToXMLValueNotOuter(templateNode);
                    if (vv != null)
                    {
                        return CheckValue(vv);
                    }
                    return null;
                }
                return innerResult.Value;
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
                    innerResult.Value = vv;
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
                  //  writeToLog("WARN: EXPTY" + Unifiable.DescribeUnifiable(value));
                }
            }
        }


        public void SetParent(AIMLTagHandler handlerU)
        {
            if (handlerU == this)
            {
                Proc.RaiseError("SetParent: same: " + this);
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
        public override sealed Unifiable Transform()
        {
            if (finalResult.IsValid) return finalResult.Value;
            if (ChatOptions.AIML_MAY_USE_FAILURE && QueryHasFailed)
            {
                // this will call the debugger (wont happen unless AIML_MAY_USE_FAILURE == true)
                return FAIL;
            }
            var OnExit = EnterTag(request, templateNode, query);
            try
            {
                IsStarted = true;
                var recurseResult00 = Recurse();
                var recurseResult0 = ProcessChangeU();
                var recurseResultS = (string)recurseResult0;
                Unifiable recurseResult;
                var wasRealyNull = Object.ReferenceEquals(recurseResult0, null);
                if (this is EmptyIsNotFailure && !wasRealyNull && recurseResult0.AsString() == String.Empty)
                {
                    string ret = String.Empty;
                    finalResult.Value = ret;
                    return ret;
                }
                if (this is NoReturnResult)
                {
                    if (!string.IsNullOrEmpty(recurseResultS))
                    {
                        recurseResultS = recurseResultS.Trim();
                        if (!recurseResultS.StartsWith("<!") && recurseResultS.Length > 1)
                        {
                            writeToLogWarn("Something is returning text and shouldnt be!");
                        }
                    }
                    string ret = ChatOptions.THINK_RETURN;
                    finalResult.Value = ret;
                    return ret;
                }
                if (FinalResultValid)
                {
                    return FinalResult;
                }
                if (CompleteEvaluation(recurseResult0, this, out recurseResult))
                {
                    FinalResult = recurseResult;
                    return recurseResult;
                }

                if (!AltBot.BE_COMPLETE_NOT_FAST) return recurseResult0;

                var recurseResult1 = FinalResult;
                if (CompleteEvaluation(recurseResult1, this, out recurseResult))
                {
                    FinalResult = recurseResult;
                    return recurseResult;
                }
                var recurseResult2 = templateNodeInnerText;
                if (CompleteEvaluation(recurseResult2, this, out recurseResult))
                {
                    writeToLogWarn("ProcessAimlChange -> templateNodeInnerText=" + recurseResult2 + "->" + recurseResult);
                    FinalResult = recurseResult;
                    return recurseResult;
                }
                if (FinalResultValid) return FinalResult;
                return recurseResult0;
            }
            finally
            {
                if (OnExit != null) OnExit();
            }
        }

        protected static bool CompleteEvaluation(string vv, AIMLTagHandler childHandlerU, out Unifiable output)
        {
            string soutput;
            bool res = CompleteEvaluation(vv, childHandlerU, out soutput);
            if (soutput == null)
            {
                output = null;
            }
            else
            {
                output = soutput;
            }
            if (output == null)
            {
                if (DLRConsole.Trace("CompleteEvaluation returned " + res + " for " + vv))
                {
                    res = CompleteEvaluation(vv, childHandlerU, out soutput);
                    if (soutput == null)
                    {
                        output = null;
                    }
                    else
                    {
                        output = soutput;
                    }
                }
            }
            return res;
        }

        private static bool CompleteEvaluation(String vv, AIMLTagHandler childHandlerU, out string output)
        {
            if (!ChatOptions.AIML_TEMPLATE_REEVAL)
            {
                output = vv;
                return true;
            }
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
            request.CurrentResult = childHandlerU.result;
            childHandlerU.result.request = childHandlerU.request;
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
        public virtual Unifiable CompleteOuterProcessAiml()
        {
            if (request != null && result != null)
            {
                request.CurrentResult = result;
            }
            if (finalResult.IsValid) return finalResult.Value;
            if (FinalResultValid) return FinalResult;

            var OnExit = EnterTag(request, templateNode, query);
            try
            {
                if (FinalResultValid) return FinalResult;
                string src;
                if (query.IsSourceRequest(this, out src))
                {
                    return src ?? OuterSource();
                }
                var test = TransformInner();
                if (Unifiable.IsNull(test))
                {
                    if (QueryHasFailed)
                    {
                        return FAIL;
                    }
                    test = GetTemplateNodeInnerText();
                    if (test == null) writeToLogWarn("NULL response in " + templateNode.OuterXml + " for " + query);
                    string value2;
                    if (CompleteEvaluation(test, this, out value2))
                    {
                        test = value2;
                    }
                }
                if (FinalResultValid)
                {
                    if (test == FinalResult)
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
            Unifiable t1 = TransformInner();
            float score1 = t1.Unify(with, query);
            if (score1 == 0) return score1;
            Unifiable t2 = Transform();
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
            var thinkReturn = ChatOptions.THINK_RETURN;
            if (thinkReturn != null) return thinkReturn;
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
                templateScore *= query.CurrentTemplate.TemplateRating;
                double beforerating = request.TopLevelScore;
                double newrating = beforerating*templateScore;
                request.TopLevelScore = newrating;
                if (this.IsTraced && Math.Abs(newrating - beforerating) > 0.01)
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
            if (FinalResultValid) return FinalResult;
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
                FinalResult = unifiable;
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
            var vv = recursiveStar.Transform();
            //var vv2 = recursiveStar.CompleteAimlProcess(););
            if (!IsNullOrEmpty(vv)) return vv;
            writeToLogWarn("GetStarContent ERROR: NULL <- " + vv);
            return vv;
        }

        protected XmlNode ExpandedTemplateNode
        {
            get { return templateNode; }
        }

        protected Unifiable callSRAI(string starContent)
        {
            if (FinalResultValid)
            {
                return FinalResult;
            }
            if (InUnify)
            {
                return null;
            }
            if (!request.CanProcess(starContent)) return null;
            XmlNode sraiNode = getNodeAndSetSiblingNode(String.Format("<srai>{0}</srai>", starContent), templateNode);
            LineInfoElement.unsetReadonly(sraiNode);
            srai_odd sraiHandler = new srai_odd(this.Proc, this.user, this.query, this.request, this.result, sraiNode);
            sraiHandler.KnowsCanProcess = true;
            var vv = sraiHandler.Transform(); // Transform();
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
                sraiHandler = new srai_odd(this.Proc, this.user, this.query, this.request, this.result, sraiNode);
                vv = sraiHandler.Transform(); // Transform();
                return vv;
            }
            FinalResult = vv;
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
                        Proc.RaiseError("save NULLL ResultsOnChildren! " + this);
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
            get { return Unifiable.Empty; }
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
            //if (ReferenceEquals(value.AsString(), string.Empty)) return value;
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
                if (isVerbatum) return value;
                string valueStr;
                if (CompleteEvaluation(value, this, out valueStr))
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

        public AIMLTagHandler GetChildTagHandler(XmlNode childNode)
        {
            var Proc = this.Proc.TagHandling;
            User user = this.user;
            if (request != null)
            {
                var uu = request.Requester;
                if (uu != null) user = uu;
            }
            AIMLTagHandler part = Proc.GetTagHandler(user, query, request, result, childNode, this);
            //AddChild(part);
            return part;
        }

        public static Unifiable ProcessAiml(string vv, out bool success, AIMLTagHandler tagHandlerU)
        {
            AIMLTagHandler parent = tagHandlerU;
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
                                                   vv3 = ProcessTagHandlerNode(tn, false, false, out successM,
                                                                               tagHandlerU);
                                               });
                success = false;
                return null;
            }
            if (!success)
            {
                return null;
            }
            if (CompleteEvaluation(vv2, tagHandlerU, out vv))
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

        public Unifiable ProcessChildNode(XmlNode childNode)
        {
            bool success;
            var chosenXML = Unifiable.InnerXmlText(childNode);
            AIMLTagHandler tagHandlerUChild = GetChildTagHandler(childNode);
            var vv = ProcessChildNode(childNode, ReadOnly, false, out success, tagHandlerUChild);
            if (!success)
            {
                Proc.TraceTest(
                    TextPatternUtils.SafeFormat("RE-EVALING CHILD '{0}' '{1}'", Unifiable.DescribeUnifiable(vv),
                                                childNode),
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

        public Unifiable ProcessChildNode(XmlNode childNode, bool protectChildren, bool saveOnInnerXML,
                                             out bool success, AIMLTagHandler tagHandlerUChild)
        {
            if (saveOnInnerXML)
            {
                writeToLogWarn("Saving child data on XML");
            }
            AIMLTagHandler outerParent = this;
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
                                                                               vv = ProcessTagHandlerNode(childNode,
                                                                                                          protectChildren,
                                                                                                          saveOnInnerXML,
                                                                                                          out
                                                                                                              childSuccess,
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
                if (CompleteEvaluation(vv, tagHandlerUChild, out output))
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

        public static Unifiable ProcessNonElement(bool saveOnInnerXML, XmlNode childNode, out bool success)
        {
            if (saveOnInnerXML && throwOnSave)
                AltBot.RaiseErrorStatic(new InvalidOperationException("saveOnInnerXML! " + childNode));

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

        public static Unifiable ProcessTagHandlerNode(XmlNode childNode, bool protectChildren, bool saveOnInnerXML,
                                                         out bool success, AIMLTagHandler tagHandlerUChild)
        {
            OutputDelegate writeToLogWarn = tagHandlerUChild.writeToLogWarn;
            OutputDelegate writeToLog = tagHandlerUChild.writeToLog;

            AIMLTagHandler parentHandlerU = tagHandlerUChild.Parent;

            if (saveOnInnerXML && throwOnSave)
            {
                 tagHandlerUChild.Proc.RaiseError("saveOnInnerXML! " + tagHandlerUChild);
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
                    var request = tagHandlerUChild.request ?? parentHandlerU.request;
                    var result = tagHandlerUChild.result ?? parentHandlerU.result;
                    var user = tagHandlerUChild.user ?? parentHandlerU.user;
                    var query = tagHandlerUChild.query ?? parentHandlerU.query;

                    bool suspendingLimits = (request != null &&
                                             (request.IsToplevelRequest || request.SuspendSearchLimits))
                                            || tagHandlerUChild.SuspendLimits ||
                                            (parentHandlerU != null && parentHandlerU.SuspendLimits);



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
                        value = tagHandlerUChild.TryProcessingNodeAgain(parentHandlerU, childNode, tagHandlerUChild,
                                                                        Proc, copyChild, copyParent,
                                                                        ref suspendingLimits, ref value, writeToLogWarn,
                                                                        ref success);
                    }
                    if (IsEMPTY(value))
                    {
                        if (IsSilentTag(childNode) && success)
                        {
                            if (saveOnInnerXML)
                            {
                                if (parentHandlerU != null)
                                {

                                    parentHandlerU.SaveResultOnChild(childNode, "+++ ");
                                }
                            }
                            return value;
                        }
                        value = tagHandlerUChild.TryProcessingNodeAgain(parentHandlerU, childNode, tagHandlerUChild,
                                                                        Proc, copyChild, copyParent,
                                                                        ref suspendingLimits, ref value, writeToLogWarn,
                                                                        ref success);
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

        private Unifiable TryProcessingNodeAgain(AIMLTagHandler parent, XmlNode childNode,
                                                 AIMLTagHandler tagHandlerUChild,
                                                 TagHandlerProcessor Proc, bool copyChild, bool copyParent,
                                                 ref bool suspendingLimits, ref string value,
                                                 OutputDelegate writeToLogWarn, ref bool success)
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

        private AIMLTagHandler RecurseVistor = null;
        private AIMLTagHandler RecurseVistor2 = null;

        public Unifiable Recurse()
        {
            if (RecurseResultValid)
            {
                return RecurseResult;
            }
            if (RecurseVistor == this)
            {
                if (RecurseVistor2 == this)
                {
                    writeToLogWarn("In Recurse() loop");
                }
                RecurseVistor2 = this;
                return RecurseChildrenReal();
            }
            RecurseVistor = this;
            try
            {
                return RecurseChildren();
            }
            finally
            {
                RecurseVistor = null;
                RecurseVistor2 = null;
            }

        }

        public virtual Unifiable RecurseChildren()
        {
            if (RecurseResultValid)
            {
                return RecurseResult;
            }
            if (!isRecursive)
            {
                return InnerSource();
            }
            return RecurseChildrenReal();
        }

        public Unifiable RecurseChildrenReal()
        {
            bool _wasRecurseResultValid = innerResult.IsValid;
            try
            {
                if (RecurseResultValid)
                {
                    var rr = RecurseResult;
                    writeToLog("USING CACHED RECURSE " + rr);
                    return CheckValue(rr);
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

        public Unifiable RecurseReal(Func<Unifiable, Unifiable> afterEachOrNull, XmlNode node, bool saveOnChildren)
        {
            if (isRecursive && !node.HasChildNodes)
            {
                return think.THINKYTAG;
            }

            saveOnChildren = false;
            //Unifiable templateNodeInnerText;//= this.templateNodeInnerText;
            Unifiable templateResult = Unifiable.CreateAppendable();
            if (node.HasChildNodes && isRecursive)
            {
                ReadOnly = node.IsReadOnly || true;
                int goods = 0;
                // recursively check
                foreach (XmlNode childNode in node.ChildNodes)
                {
                    if (childNode.NodeType == XmlNodeType.Comment) continue;

                    bool success;
                    AIMLTagHandler tagHandlerUChild = GetChildTagHandler(childNode);
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


        protected Unifiable OutputFromTagHandlers(IEnumerable<AIMLTagHandler> aimlTagHandlers0)
        {
            IEnumerable<AIMLTagHandler> aimlTagHandlers = null;
            Unifiable appendable = Unifiable.CreateAppendable();
            lock (aimlTagHandlers0)
            {
                var LoopOver = new List<AIMLTagHandler>(aimlTagHandlers0);
                aimlTagHandlers = LoopOver;
            }
            Request request = this.request;
            foreach (var childTagHandler in aimlTagHandlers)
            {
                var vv = childTagHandler.Transform();

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
                            vv = childTagHandler.Transform();
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

        public virtual Unifiable TransformInner()
        {
            return Recurse();
        }

        private Unifiable NonRecusiveResult()
        {
            Unifiable recursiveResult = RecurseReal(templateNode, false);
            if (!IsNull(recursiveResult)) return recursiveResult;
            string resultNodeInnerXML = templateNode.OuterXml; //.ProcessChange();
            XmlNode resultNode = getNodeAndSetSiblingNode("<template>" + resultNodeInnerXML + "</template>",
                                                          templateNode);
            LineInfoElementImpl.unsetReadonly(resultNode);
            if (resultNode.HasChildNodes)
            {
                recursiveResult = ProcessChildNode(resultNode);
                if (!IsNullOrEmpty(recursiveResult))
                {
                    FinalResult = recursiveResult;
                    return recursiveResult;
                }
                recursiveResult = RecurseReal(resultNode, false);
                if (!IsNullOrEmpty(recursiveResult))
                {
                    FinalResult = recursiveResult;
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
                if (CompleteEvaluation(value, this, out value2))
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
                    Proc.RaiseError("save NULL ResultsOnChildren! " + this + " " + errmsg);
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
            string templateNodeName = this.templateNode.LocalName;
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
            writeToLogWarn("WARN CheckNode change " + name + " -> " + templateNode.Name);
            return true;
        }

        protected Unifiable GetActualValue(string name, string dictName, out bool succeed)
        {
            //ISettingsDictionary dict = query;
            return GetActualValue(templateNode, name, dictName, out succeed, query);
        }

        protected static Unifiable GetActualValue(XmlNode templateNode, string name, string dictNameIn, out bool succeed,
                                                  SubQuery query)
        {
            ISettingsDictionary dict; // = query;
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
            AIMLTagHandler tagHandlerU = FirstTagHandlerOrOuterMost("template");
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
            AIMLTagHandler tagHandlerU = FirstTagHandlerOrOuterMost("template");
            if (Parent == null || tagHandlerU == this || tagHandlerU == null)
            {
                query.LocalSideEffect(namedEffect, enter, exit);
            }
            else
            {
                tagHandlerU.LocalSideEffect(namedEffect, enter, exit);
            }
        }

        protected AIMLTagHandler FirstTagHandlerOrOuterMost(string type)
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


        public void AddChild(AIMLTagHandler part)
        {
            TagHandlerChilds = TagHandlerChilds ?? new List<AIMLTagHandler>();
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
                foreach (AIMLTagHandler child in TagHandlerChilds)
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
            List<AIMLTagHandler> aimlTagHandlers = GetAIMLTagHandlers(nodes, predicate);

            Unifiable appendable = OutputFromTagHandlers(aimlTagHandlers);
            Unifiable appendable1;
            if (CompleteEvaluation(appendable, this, out appendable1))
            {
                FinalResult = appendable1;
                return appendable1;
            }
            return CheckValue(appendable);
        }


        protected List<AIMLTagHandler> GetAIMLTagHandlers(ICollection<XmlNode> nodes, Predicate<XmlNode> predicate)
        {
            TagHandlerChilds = TagHandlerChilds ?? new List<AIMLTagHandler>();
            lock (TagHandlerChilds)
            {
                List<AIMLTagHandler> aimlTagHandlers = new List<AIMLTagHandler>();
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
            if (query == null) return new[] {1, 0};
            return new int[] {QueryHasSuceededN, QueryHasFailedN};
        }

        protected void DebugCheck(int i)
        {
            QuerySettingsReadOnly request = (QuerySettingsReadOnly) this.request;
            if (!request.IsTraced) return;
            if (request.DebugLevel < i)
            {
                AltBot.Breakpoint("Level " + request.DebugLevel + "<" + i);
            }
        }

        public override string ToString()
        {
            return LineNumberTextInfo();
        }

        public string LineNumberTextInfo()
        {
            string s = LineTextInfo() + " " + LineNumberInfo();
            string ss = s;
            if (ss.StartsWith(isValueSetStart))
            {
                ss = s + " -FROM- " + templateNode.OuterXml;
            }
            if (!s.Replace(" /", "/").Contains(initialString.Replace(" /", "/")))
            {
                return "-WAS- '" + initialString + "' -NOW- " + ss;
            }
            return ss;
        }

        public string LineTextInfo()
        {
            if (templateNode == null) return "noXML";
            string s = Trim(templateNode.OuterXml);
            if (String.IsNullOrEmpty(s))
            {
                XmlNode li = templateNode;
                s = s + " " + li.OwnerDocument;
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
                IXmlLineInfo li = (IXmlLineInfo) templateNode;
                if (li.LineNumber == 0)
                {
                    s = s + " " + templateNode.OwnerDocument;
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
                    s = s + " (" + templateNode.OwnerDocument + ":line " + li.LineNumber + "," + li.LinePosition + ") ";
                }
            }
            return s + "-->";
        }

        public string DocumentInfo()
        {
            string s = null;
            if (templateNode != null)
            {
                XmlNode li = templateNode;
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

        protected void writeToLogWarn(string f, params object[] a)
        {
            writeToLog("WARNING: " + f, a);
        }

        public virtual void writeToLog(string f, params object[] a)
        {
            if (f.ToUpper().StartsWith("ERROR"))
            {
                writeToLogWarn("BAD " + f, a);
                return;
            }
            this.mbot.writeToLog(
                "AIMLTRACE: " + f +
                DLRConsole.NoFormatDirectives(" in " + GetType().Name + "  " + LineNumberTextInfo()), a);
        }

        protected virtual AltBot mbot
        {
            get { return bot; }
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

        internal static string GetNameOfDict(SubQuery query, string dictName, XmlNode templateNode,
                                             out ISettingsDictionary dict)
        {
            string type = ToLower(dictName);
            //ISettingsDictionary udict = query.GetDictionary(type, templateNode, dict);
            while (templateNode != null)
            {
                string type0 = GetAttribValue(templateNode, "type,dict", null);
                if (type0 != null)
                {
                    type = type0;
                    break;
                }
                string uname = GetAttribValue(templateNode, "user", null);
                if (uname != null)
                {
                    type0 = GetNamedType("user", uname);

                    if (type0 != null)
                    {
                        type = type0;
                        break;
                    }
                }
                string bname = GetAttribValue(templateNode, "bot", null);
                if (bname != null)
                {
                    type0 = GetNamedType("bot", bname);
                    if (type0 != null)
                    {
                        type = type0;
                        break;
                    }
                }
                dict = query.GetDictionary(templateNode.LocalName);
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
                dict = query.ResponderPredicates;
            }
            else
            {
                dict = query.GetDictionary(type);
            }
            if (dict == null) dict = query;
            return type ?? dict.NameSpace;
        }

        #region IAIMLTransaction Members


        public SubQuery CurrentQuery
        {
            get { return query; }
        }

        #endregion

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