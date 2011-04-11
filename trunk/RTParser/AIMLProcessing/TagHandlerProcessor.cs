using System;
using System.Collections.Generic;
using System.Web;
using System.Xml;
using MushDLR223.ScriptEngines;
using MushDLR223.Utilities;
using RTParser.AIMLTagHandlers;
using RTParser.Utils;
using PatternInfo = RTParser.Unifiable;
using ThatInfo = RTParser.Unifiable;
using TopicInfo = RTParser.Unifiable;
using GuardInfo = RTParser.Unifiable;
using ResponseInfo = RTParser.Unifiable;
using AIMLbot;

namespace RTParser
{
    /// <summary>
    /// Encapsulates an AIML Tag Proccessor.
    /// </summary>
    public partial class TagHandlerProcessor : StaticAIMLUtils
    {

        internal AIMLTagHandler GetTagHandler(User user, SubQuery query, Request request, Result result, XmlNode node,
                                              AIMLTagHandler parentTagHandler)
        {
            AIMLTagHandler tag = GetTagHandler00(user, query, request, result, node, true);
            if (query != null) query.CurrentTagHandler = tag;
            if (query != null) query.CurrentNode = node;
            if (tag == null)
            {
                writeToLog("NULL TAG " + node.OuterXml);
            }
            if (tag != null)
            {
                tag.SetParent(parentTagHandler);
                if (query != null) query.LastTagHandler = parentTagHandler ?? tag;
                TemplateInfo ti = tag.templateInfo;
                if (ti == null && query != null)
                {
                    ti = query.CurrentTemplate;
                }
                if (ti == null && parentTagHandler != null)
                {
                    ti = parentTagHandler.GetTemplateInfo();
                }
                if (ti != null)
                {
                    tag.templateInfo = ti;
                }
                else
                {
                    //    writeToLog("DEBUG9 Missing templateInfo " + node.OuterXml);
                }
            }
            return tag;
        }

        internal AIMLTagHandler GetTagHandler00(User user, SubQuery query, Request request, Result result, XmlNode node, bool liText)
        {
            AIMLTagHandler tagHandler = getBespokeTags(user, query, request, result, node);
            string nodeNameLower = ToLower(node.LocalName);
            RTPBot targetBot = query.TargetBot;
            if (Equals(null, tagHandler))
            {
                switch (nodeNameLower)
                {
                    case "template":
                    case "answer": //CML
                        tagHandler = new template(targetBot, user, query, request, result, node);
                        break;
                    case "aiml":
                    case "cml": //CML
                        tagHandler = new aiml(targetBot, user, query, request, result, node);
                        break;
                    case "aimlexec":
                    case "eval":
                        tagHandler = new aimlexec(targetBot, user, query, request, result, node);
                        break;
                    case "vars":
                    case "root":
                    case "predicates": //CML
                        tagHandler = new root(targetBot, user, query, request, result, node, (() => query.TargetSettings));
                        break;
                    case "properties":
                    case "bots":
                        tagHandler = new root(targetBot, user, query, request, result, node,
                                              (() => request.TargetBot.GlobalSettings));
                        break;
                    case "substitutions":
                        tagHandler = new root(targetBot, user, query, request, result, node,
                                              (() => request.TargetBot.InputSubstitutions));
                        break;
                    case "topic":
                    case "conversation": //CML
                        tagHandler = new topic(targetBot, user, query, request, result, node);
                        break;
                    case "category":
                    case "conv": //CML
                        tagHandler = new category(targetBot, user, query, request, result, node);
                        break;
                    case "and":
                        tagHandler = new and(targetBot, user, query, request, result, node);
                        break;
                    case "or":
                        tagHandler = new or(targetBot, user, query, request, result, node);
                        break;
                    case "optional":
                        tagHandler = new optional(targetBot, user, query, request, result, node);
                        break;
                    case "isa":
                        tagHandler = new isa(targetBot, user, query, request, result, node);
                        break;
                    case "bot":
                        tagHandler = new bot(targetBot, user, query, request, result, node);
                        break;
                    case "condition":
                    case "options": //cml
                        tagHandler = new condition(targetBot, user, query, request, result, node);
                        break;
                    case "li":
                        if (liText)
                            tagHandler = new liif(targetBot, user, query, request, result, node);
                        break;
                    case "if":
                        tagHandler = new liif(targetBot, user, query, request, result, node);
                        break;
                    case "personf":
                        tagHandler = new format(targetBot, user, query, request, result, node,
                                                new Func<string, string>(HttpUtility.UrlEncode),
                                                null);
                        break;
                    case "date":
                        tagHandler = new date(targetBot, user, query, request, result, node);
                        break;
                    case "formal":
                        tagHandler = new formal(targetBot, user, query, request, result, node);
                        break;
                    case "gender":
                        tagHandler = new gender(targetBot, user, query, request, result, node);
                        break;
                    case "get":
                        tagHandler = new get(targetBot, user, query, request, result, node);
                        break;
                    case "gossip":
                        tagHandler = new gossip(targetBot, user, query, request, result, node);
                        break;
                    case "get_ip":
                    case "id":
                        tagHandler = new id(targetBot, user, query, request, result, node);
                        break;
                    case "request":
                        tagHandler = new input(targetBot, user, query, request, result, node, 1);
                        break;
                    case "input":
                        tagHandler = new input(targetBot, user, query, request, result, node, 1);
                        break;
                    case "justthat": // <input index="2"/> 
                        tagHandler = new input(targetBot, user, query, request, result, node, 2);
                        break;
                    case "beforethat": // <input index="3"/> 
                        tagHandler = new input(targetBot, user, query, request, result, node, 3);
                        break;
                    case "javascript":
                        tagHandler = new javascript(targetBot, user, query, request, result, node);
                        break;
                    case "learn":
                    case "load":
                    case "noload": // the commented version of <load>
                        tagHandler = new learn(targetBot, user, query, request, result, node);
                        break;
                    case "lowercase":
                        tagHandler = new lowercase(targetBot, user, query, request, result, node);
                        break;
                    case "person":
                        tagHandler = new person(targetBot, user, query, request, result, node);
                        break;
                    case "person2":
                        tagHandler = new person2(targetBot, user, query, request, result, node);
                        break;
                    case "random":
                        tagHandler = new random(targetBot, user, query, request, result, node);
                        break;
                    case "sentence":
                        tagHandler = new sentence(targetBot, user, query, request, result, node);
                        break;
                    case "set":
                        tagHandler = new set(targetBot, user, query, request, result, node);
                        break;
                    case "size":
                    case "getsize":
                        tagHandler = new size(targetBot, user, query, request, result, node);
                        break;
                    case "sr":
                        tagHandler = new sr(targetBot, user, query, request, result, node);
                        break;
                    case "srai":
                        tagHandler = new srai(targetBot, user, query, request, result, node);
                        break;
                    case "star":
                        tagHandler = new star(targetBot, user, query, request, result, node);
                        break;
                    case "system":
                        tagHandler = new system(targetBot, user, query, request, result, node);
                        break;
                    case "that": //default <that index="1,1"/>
                        tagHandler = new that(targetBot, user, query, request, result, node, 1);
                        break;
                    case "justbeforethat": //treated as <that index="2,1"/>
                        tagHandler = new that(targetBot, user, query, request, result, node, 2);
                        break;
                    case "response": //treated as <that index="1,1"/>
                        tagHandler = new that(targetBot, user, query, request, result, node, 2);
                        break;
                    case "thatstar":
                        tagHandler = new thatstar(targetBot, user, query, request, result, node);
                        break;
                    case "think":
                        tagHandler = new think(targetBot, user, query, request, result, node);
                        break;
                    case "topicstar":
                        tagHandler = new topicstar(targetBot, user, query, request, result, node);
                        break;
                    case "uppercase":
                        tagHandler = new uppercase(targetBot, user, query, request, result, node);
                        break;
                    case "version":
                    case "getversion":
                        tagHandler = new version(targetBot, user, query, request, result, node);
                        break;
                    case "cycsystem":
                        tagHandler = new cycsystem(targetBot, user, query, request, result, node);
                        break;
                    case "cycretract":
                        tagHandler = new cycretract(targetBot, user, query, request, result, node);
                        break;
                    case "cycassert":
                        tagHandler = new cycassert(targetBot, user, query, request, result, node);
                        break;
                    case "cycterm":
                        tagHandler = new cycterm(targetBot, user, query, request, result, node);
                        break;
                    case "cycquery":
                        tagHandler = new cycquery(targetBot, user, query, request, result, node);
                        break;
                    case "cyccondition":
                        tagHandler = new cyccondition(targetBot, user, query, request, result, node);
                        break;
                    case "cycphrase":
                        tagHandler = new cycphrase(targetBot, user, query, request, result, node);
                        break;
                    case "cycparaphrase":
                        tagHandler = new cycphrase(targetBot, user, query, request, result, node);
                        break;
                    case "guard":
                        tagHandler = new guard(targetBot, user, query, request, result, node);
                        break;
                    case "guardstar":
                        tagHandler = new guardstar(targetBot, user, query, request, result, node);
                        break;
                    case "cycrandom":
                        tagHandler = new cycrandom(targetBot, user, query, request, result, node);
                        break;
                    case "space":
                        tagHandler = new space(targetBot, user, query, request, result, node);
                        break;
                    case "markov":
                        tagHandler = new markov(targetBot, user, query, request, result, node);
                        break;
                    case "soundcode":
                        tagHandler = new soundcode(targetBot, user, query, request, result, node);
                        break;

                    // MSM
                    case "msm":
                        tagHandler = new msm(targetBot, user, query, request, result, node);
                        break;
                    case "processmsm":
                        tagHandler = new process_msm(targetBot, user, query, request, result, node);
                        break;
                    case "setstate":
                        tagHandler = new setstate(targetBot, user, query, request, result, node);
                        break;
                    case "state":
                        tagHandler = new state(targetBot, user, query, request, result, node);
                        break;
                    case "transition":
                        tagHandler = new transition(targetBot, user, query, request, result, node);
                        break;
                    case "setevidence":
                        tagHandler = new setevidence(targetBot, user, query, request, result, node);
                        break;
                    case "evidenceassoc":
                        tagHandler = new evidence_assoc(targetBot, user, query, request, result, node);
                        break;
                    case "evidencepattern":
                        tagHandler = new evidence_pattern(targetBot, user, query, request, result, node);
                        break;
                    case "evidencestate":
                        tagHandler = new evidencestate(targetBot, user, query, request, result, node);
                        break;
                    case "dependentmachine":
                        tagHandler = new dependentmachine(targetBot, user, query, request, result, node);
                        break;
                    case "responsetopic":
                        tagHandler = new response_topic(targetBot, user, query, request, result, node);
                        break;

                    case "push":
                        tagHandler = new push(targetBot, user, query, request, result, node);
                        break;
                    case "pop":
                        tagHandler = new pop(targetBot, user, query, request, result, node);
                        break;
                    case "peekstack":
                        tagHandler = new peekstack(targetBot, user, query, request, result, node);
                        break;

                    case "lex":
                        tagHandler = new lex(targetBot, user, query, request, result, node);
                        break;
                    case "lexset":
                        tagHandler = new lexset(targetBot, user, query, request, result, node);
                        break;
                    case "lexis":
                        tagHandler = new lexis(targetBot, user, query, request, result, node);
                        break;

                    case "dbpush":
                        tagHandler = new dbpush(targetBot, user, query, request, result, node);
                        break;
                    case "dbquery":
                        tagHandler = new dbquery(targetBot, user, query, request, result, node);
                        break;
                    case "dbupdate":
                        tagHandler = new dbupdate(targetBot, user, query, request, result, node);
                        break;
                    case "dbdelete":
                        tagHandler = new dbdelete(targetBot, user, query, request, result, node);
                        break;
                    case "dbload":
                        tagHandler = new dbload(targetBot, user, query, request, result, node);
                        break;


                    case "regex":
                        tagHandler = new regex(targetBot, user, query, request, result, node);
                        break;

                    case "bind": // <bind>#$isa</bind>
                        tagHandler = new bind(targetBot, user, query, request, result, node);
                        break;

                    case "#text":
                        if (!liText) return null;
                        return new verbatum(node.InnerText, targetBot, user, query, request, result, node);
                    case "#comment":
                        return new verbatum(node.OuterXml, targetBot, user, query, request, result, node);
                    case "br":
                        return new verbatum("\n", targetBot, user, query, request, result, node);
                    case "pre":
                        return new verbatum(StaticXMLUtils.InnerXmlText(node), targetBot, user, query, request, result, node);
                    case "p":
                        return new verbatum("\n\n", targetBot, user, query, request, result, node);
                    case "meta":
                        return new verbatum(node.OuterXml, targetBot, user, query, request, result, node);
                    default:
                        break;
                }
            }
            if (tagHandler != null) return tagHandler;
            if (IsHtmlTag(node.Name))
            {
                return new recursiveVerbatum(node, targetBot, user, query, request, result, node, true);
            }
            if (tagHandler == null)
            {
                // "bot", "favorite", "fav" 
                foreach (KeyValuePair<string, string> prefix in new[]
                                                                    {
                                                                        new KeyValuePair<string, string>("get_", "get"),
                                                                        new KeyValuePair<string, string>("set_", "set"),
                                                                        new KeyValuePair<string, string>("bot_", "bot"),
                                                                        new KeyValuePair<string, string>("favorite_", "bot"), 
                                                                        new KeyValuePair<string, string>("favorite", "bot"),
                                                                        new KeyValuePair<string, string>("fav_", "bot"),
                                                                        new KeyValuePair<string, string>("fav", "bot"),
                                                                                                                       
                                                                        new KeyValuePair<string, string>("get", "get"),
                                                                        new KeyValuePair<string, string>("set", "set"),
                                                                        new KeyValuePair<string, string>("bot", "bot"),
                                                                    })
                {
                    if (nodeNameLower.StartsWith(prefix.Key) && node.Name.Length > prefix.Key.Length)
                    {
                        string name = node.Name.Substring(prefix.Key.Length);
                        XmlNode pn = node.ParentNode;
                        LineInfoElementImpl newnode = CopyNode(prefix.Value, node, false);
                        XmlAttributeLineInfo atr = (XmlAttributeLineInfo)newnode.OwnerDocument.CreateAttribute("name");
                        atr.ReadOnly = false;
                        atr.Value = name;
                        newnode.Attributes.Append(atr);
                        if (node.Name.ToLower() != newnode.Name.ToLower())
                        {
                            writeToLog("AIMLLOADER: converted " + node.OuterXml + " -> " + newnode.OuterXml);
                            return GetTagHandler00(user, query, request, result, newnode, liText);
                        }
                        writeToLog("AIMLLOADER: ! convert " + node.OuterXml + " -> " + newnode.OuterXml);
                    }
                }
            }
            if (tagHandler != null) return tagHandler;
            if (nodeNameLower == "name")
            {
                return new bot(targetBot, user, query, request, result, node);
            }
            tagHandler = new lazyClosure(targetBot, user, query, request, result, node);
            writeToLog("AIMLLOADER:  lazyClosure: " + node.OuterXml);
            return tagHandler;
        }

        static public void writeToLog(string message, params object[] args)
        {
            RTPBot.writeDebugLine(message, args);
        }


        /// <summary>
        /// Should return the child tagHandler
        /// </summary>
        /// <param name="query"></param>
        /// <param name="request"></param>
        /// <param name="result"></param>
        /// <param name="templateNode"></param>
        /// <param name="sGuard"></param>
        /// <param name="createdOutput"></param>
        /// <param name="templateSucceeded"></param>
        /// <param name="parentHandler"></param>
        /// <param name="templateInfo"></param>
        /// <param name="copyChild"></param>
        /// <param name="copyParent"></param>
        /// <returns></returns>
        public AIMLTagHandler proccessResponse(SubQuery query,
                                     Request request, Result result,
                                     XmlNode templateNode, GuardInfo sGuard,
                                     out bool createdOutput, out bool templateSucceeded,
                                     AIMLTagHandler parentHandler, TemplateInfo templateInfo,
                                     bool copyChild, bool copyParent)
        {
            //request.CurrentResult = result;
            query = query ?? request.CurrentQuery;
            if (request.CurrentQuery==null)
            {
                request.CurrentQuery = query;
            }
            if (templateInfo == null)
            {
              //  writeToLog("templateInfo is null " + templateNode);
            }
            templateInfo = templateInfo ?? query.CurrentTemplate;
            //request.CurrentQuery = query;
            if (!request.CanUseResultTemplate(templateInfo, result))
            {
                templateSucceeded = false;
                createdOutput = false;
                return null;
            }
            // now cant use it again
            if (templateInfo != null) result.ResultTemplates.Add(templateInfo);
            UndoStack undoStack = UndoStack.GetStackFor(query);
            try
            {
                var childTagHandler = proccessTemplate(query, request, result, templateNode, sGuard,
                                           out createdOutput, out templateSucceeded,
                                           parentHandler, templateInfo, copyChild, copyParent);
                return childTagHandler;
            }
            finally
            {
                undoStack.UndoAll();
            }
        }

        private AIMLTagHandler proccessTemplate(SubQuery query, Request request, Result result,
                                                XmlNode templateNode, GuardInfo sGuard,
                                                out bool createdOutput, out bool templateSucceeded,
                                                AIMLTagHandler parentHandler, TemplateInfo templateInfo,
                                                bool copyChild, bool copyParent)
        {
            ChatLabel label = request.PushScope;
            var prevTraced = request.IsTraced;
            var untraced = request.Graph.UnTraced;
            var superTrace = templateInfo != null && templateInfo.IsTraced;
            try
            {
                if (superTrace)
                {
                    request.IsTraced = true;
                    request.Graph.UnTraced = false;
                }

                var th = proccessResponse000(query, request, result, templateNode, sGuard,
                                           out createdOutput, out templateSucceeded,
                                           parentHandler, templateInfo, copyChild, copyParent);

                if (superTrace)
                {
                    writeDebugLine("SuperTrace=" + templateSucceeded + ": " + templateInfo);
                }
                return th;
            }
            catch (ChatSignalOverBudget ex)
            {
                throw;
            }
            catch (ChatSignal ex)
            {
                if (label.IsSignal(ex))
                {
                    // if (ex.SubQuery != query) throw;
                    if (ex.NeedsAdding)
                    {
                        request.AddOutputSentences(templateInfo, ex.TemplateOutput, result);
                    }
                    templateSucceeded = ex.TemplateSucceeded;
                    createdOutput = ex.CreatedOutput;
                    return ex.TagHandler;
                }
                throw;
            }
            catch (Exception ex)
            {
                throw;
            }
            finally
            {
                request.IsTraced = prevTraced;
                request.Graph.UnTraced = untraced;
                label.PopScope();
            }
        }

        public AIMLTagHandler proccessResponse000(SubQuery query, Request request, Result result,
                                                XmlNode sOutput, GuardInfo sGuard,
                                                out bool createdOutput, out bool templateSucceeded,
                                                AIMLTagHandler parentHandler, TemplateInfo templateInfo,
                                                bool copyChild, bool copyParent)
        {
            RTPBot Proc = query.TargetBot;

            //query.LastTagHandler = handler;
            bool isTraced = request.IsTraced || result.IsTraced || !request.GraphsAcceptingUserInput ||
                            (templateInfo != null && templateInfo.IsTraced);
            //XmlNode guardNode = AIMLTagHandler.getNode(s.Guard.InnerXml);
            bool usedGuard = sGuard != null && sGuard.PatternNode != null;
            sOutput = sOutput ?? templateInfo.ClonedOutput;
            string output = sOutput.OuterXml;
            XmlNode templateNode = sOutput;
            bool childOriginal = true;
            result.Started = true;
            if (usedGuard)
            {
                output = "<template>" + sGuard.PatternNode.OuterXml + " GUARDBOM " + output +
                                  "</template>";
                templateNode = getNodeAndSetSibling(output, false, false, sOutput);
                
                childOriginal = false;
            }

            bool protectChild = copyChild || childOriginal;
            bool suspendingLimits = request.IsToplevelRequest || request.SuspendSearchLimits;
            AIMLTagHandler tagHandler = GetTagHandler(request.Requester, query, request, result,
                                                                  templateNode, parentHandler);
            string outputSentenceOut = processNode(templateNode, query,
                                                               request, result, request.Requester, parentHandler,
                                                               protectChild, copyParent, tagHandler, suspendingLimits, out templateSucceeded);

            templateSucceeded = !IsFalse(outputSentenceOut);

            if (outputSentenceOut == null)
            {
                if (tagHandler == null)
                {
                    writeToLog("tagHandler = null " + output);
                }
                else
                {
                    bool success = false;
                    if (tagHandler.QueryHasSuceeded) success = true;
                    if (tagHandler.QueryHasFailed) success = false;
                    if (success)
                    {
                        writeToLog("Very BaD " + output);
                    }
                    else
                    {
                    }
                    templateSucceeded = false;
                    createdOutput = false;
                    return tagHandler;
                }
            }
            outputSentenceOut = outputSentenceOut ?? "";
            string left, outputSentence;
            if (!SplitOff(outputSentenceOut, "GUARDBOM", out left, out outputSentence))
            {
                left = null;
                outputSentence = outputSentenceOut;
            }
            if (sGuard == null || sGuard.PatternNode != null)
            {
                string o = Proc.ToEnglish(outputSentence);
                if (outputSentence.Trim().Length > 0)
                {
                    templateSucceeded = true;
                }
                if (Proc.IsOutputSentence(o, outputSentence))
                {
                    if (isTraced)
                    {
                        string aIMLLoaderParentTextAndSourceInfo = ParentTextAndSourceInfo(templateNode);
                        if (aIMLLoaderParentTextAndSourceInfo.Length > 300)
                        {
                            aIMLLoaderParentTextAndSourceInfo = TextFilter.ClipString(
                                aIMLLoaderParentTextAndSourceInfo, 300);
                        }
                        writeToLog("AIMLTRACE '{0}' IsOutputSentence={1}", o, aIMLLoaderParentTextAndSourceInfo);
                    }
                    createdOutput = true;
                    request.AddOutputSentences(templateInfo, o, result);
                }
                else
                {
                    createdOutput = false;
                }
                // @TODO @HACK @BUG isTraced is a HORRID flag here!               
                if (false &&!createdOutput && isTraced && request.GraphsAcceptingUserInput)
                {
                    if (templateInfo != null)
                    {
                        string fromStr = " from " + templateInfo.Graph;
                        if (!StaticAIMLUtils.IsSilentTag(templateNode))
                        {
                            writeToLog("SILENT '{0}' TEMPLATE={1}", o, ParentTextAndSourceInfo(templateNode) + fromStr);
                        }
                        request.DisableTemplateUntilFinished(templateInfo);
                    }
                    else
                    {
                        writeToLog("UNUSED '{0}' TEMPLATE={1}", o, ParentTextAndSourceInfo(templateNode));
                    }

                }

                return tagHandler;
            }
            try
            {
                templateSucceeded = !IsFalse(left);
                if (!templateSucceeded)
                {
                    createdOutput = false;
                    return tagHandler;
                }
                string lang = GetAttribValue(sGuard.PatternNode, "lang", "cycl").ToLower();

                try
                {
                    Unifiable ss = Proc.SystemExecute(left, lang, request);
                    if (IsFalse(ss) || IsNullOrEmpty(ss))
                    {
                        if (isTraced)
                            writeToLog("GUARD FALSE '{0}' TEMPLATE={1}", request,
                                       ParentTextAndSourceInfo(templateNode));
                        templateSucceeded = false;
                        createdOutput = false;
                        return tagHandler;
                    }
                    else
                    {
                        templateSucceeded = true;
                    }
                }
                catch (ChatSignal e)
                {
                    throw;
                }
                catch (Exception e)
                {
                    Proc.writeToLog(e);
                    templateSucceeded = false;
                    createdOutput = false;
                    return tagHandler;
                }

                //part the BOM
                string o = Proc.ToEnglish(outputSentence);
                if (Proc.IsOutputSentence(o, outputSentence))
                {
                    if (isTraced)
                        writeToLog(query.Graph + ": GUARD SUCCESS '{0}' TEMPLATE={1}", o,
                                   ParentTextAndSourceInfo(templateNode));
                    templateSucceeded = true;
                    createdOutput = true;
                    request.AddOutputSentences(templateInfo, o, result);
                    return tagHandler;
                }
                else
                {
                    writeToLog("GUARD SKIP '{0}' TEMPLATE={1}", outputSentence,
                               ParentTextAndSourceInfo(templateNode));
                }
                templateSucceeded = false;
                createdOutput = false;
                return tagHandler;
            }
            catch (ChatSignal e)
            {
                throw;
            }
            catch (Exception ex)
            {
                Proc.writeToLog(ex);
                templateSucceeded = false;
                createdOutput = false;
                return tagHandler;
            }
        }


        /// <summary>
        /// Recursively evaluates the template nodes returned from the Proccessor
        /// </summary>
        /// <param name="node">the node to evaluate</param>
        /// <param name="query">the query that produced targetBot node</param>
        /// <param name="request">the request from the user</param>
        /// <param name="result">the result to be sent to the user</param>
        /// <param name="user">the user who originated the request</param>
        /// <returns>the output Unifiable</returns>
        public string processNode(XmlNode node, SubQuery query,
                                  Request request, Result result, User user,
                                  AIMLTagHandler parentHandler, bool protectChild, bool copyParent,
                                  AIMLTagHandler nodeHandler, bool suspendLimits, out bool templateSucceeded)
        {
            Request originalSalientRequest = MasterRequest.GetOriginalSalientRequest(request);
            var wasSuspendRestrati = request.SuspendSearchLimits;
            templateSucceeded = true;
            request.SuspendSearchLimits = suspendLimits;
            var sraiMark = originalSalientRequest.CreateSRAIMark();
            try
            {
                bool childSuccess;
                string outputSentence = processNodeVV(node, query,
                                                      request, result, user, parentHandler,
                                                      protectChild, copyParent, nodeHandler, out childSuccess);
                if (!childSuccess)
                {
                    templateSucceeded = false;
                }
                if (Unifiable.IsNull(outputSentence))
                {
                    //outputSentence = tagHandler.GetTemplateNodeInnerText();
                    templateSucceeded = false;
                    return outputSentence;
                }
                if (!Unifiable.IsNullOrEmpty(outputSentence))
                {
                    return outputSentence;
                }
                if (IsSilentTag(node) && !Unifiable.IsEMPTY(outputSentence))
                {
                    return "";
                }
                if (nodeHandler.RecurseResultValid) return nodeHandler.RecurseResult;
                return outputSentence;
            }
            finally
            {
                originalSalientRequest.ResetSRAIResults(sraiMark);
                request.SuspendSearchLimits = wasSuspendRestrati;
            }
        }

        /// <summary>
        /// Recursively evaluates the template nodes returned from the Proccessor
        /// </summary>
        /// <param name="node">the node to evaluate</param>
        /// <param name="query">the query that produced targetBot node</param>
        /// <param name="request">the request from the user</param>
        /// <param name="result">the result to be sent to the user</param>
        /// <param name="user">the user who originated the request</param>
        /// <returns>the output Unifiable</returns>
        public string processNodeVV(XmlNode node, SubQuery query,
                                  Request request, Result result, User user,
                                  AIMLTagHandler parentHandler, bool protectChild, bool copyParent,
                                  AIMLTagHandler tagHandler, out bool childSuccess)
        {
            RTPBot TargetBot = request.TargetBot;
            childSuccess = true;
            if (node == null)
            {
                string whyError = "ERROR null NODE " + tagHandler;
                writeToLog(whyError);
                throw new ChatSignalOverBudget(request, whyError);
            }
            if (node.NodeType == XmlNodeType.Text)
            {
                childSuccess = true;
                if (tagHandler != null)
                {
                    tagHandler.QueryHasSuceeded = true;
                }
                string s = Trim(TextNodeValue(node));
                if (!String.IsNullOrEmpty(s))
                {
                    return ValueText(s);
                }
                //return s;
            }
            if (tagHandler == null)
            {
                string whyError = "ERROR null THND " + node;
                childSuccess = false;
                writeToLog(whyError);
            }
            bool isTraced = request.IsTraced || result.IsTraced || !request.GraphsAcceptingUserInput ||
                 (query != null && query.IsTraced);

            // check for timeout (to avoid infinite loops)
            bool overBudget = false;
            if (request.IsComplete(result))
            {
                object gn = request.Graph;
                if (query != null) gn = query.Graph;
                string s = SafeFormat("WARNING! Request " + request.WhyComplete +
                                         ". User: {0} raw input: {3} \"{1}\" processing {2} templates: \"{4}\"",
                                         request.Requester.UserID, Unifiable.DescribeUnifiable(request.rawInput),
                                         (query == null ? "-NOQUERY-" : query.Templates.Count.ToString()), gn, node);

                if (isTraced)
                    request.writeToLog(s);
                overBudget = true;
                if (!request.IsToplevelRequest)
                {
                    throw new ChatSignalOverBudget(request, s);
                }
            }


            // process the node
            if (ReferenceEquals(null, tagHandler))
            {
                childSuccess = true;
                if (node.NodeType == XmlNodeType.Comment)
                {
                    return Unifiable.Empty;
                }
                if (node.NodeType == XmlNodeType.Text)
                {
                    string s = Trim(TextNodeValue(node));
                    if (String.IsNullOrEmpty(s))
                    {
                        return Unifiable.Empty;
                    }
                    return s;
                }
                // ReSharper disable ConditionIsAlwaysTrueOrFalse
                OutputDelegate del = (request != null) ? request.writeToLog : writeToLog;
                // ReSharper restore ConditionIsAlwaysTrueOrFalse
                if (overBudget)
                {
                    return Unifiable.Empty;
                }
                string nodeInner = node.InnerXml;
                TargetBot.EvalAiml(node, request, del ?? DEVNULL);
                return node.InnerXml;
            }

            XmlNode oldNode = node;
            bool wasReadonlyNode = oldNode.IsReadOnly;
            // copy the node!?!
            if (protectChild)
            {
                copyParent = true;
                LineInfoElementImpl newnode = CopyNode(node, copyParent);
                newnode.ReadOnly = false;
                node = newnode;
            }

            if (overBudget)
            {
                tagHandler.Dispose();
                tagHandler = null;
                childSuccess = true;
                return Unifiable.Empty;
            }

            tagHandler.SetParent(parentHandler);
            //if (parent!=null) parent.AddChild(tagHandler);

            Unifiable cp = tagHandler.CompleteAimlProcess();
            if (Unifiable.IsNullOrEmpty(cp) && (!tagHandler.QueryHasSuceeded || tagHandler.QueryHasFailed))
            {
                bool needsOneMoreTry = !request.SuspendSearchLimits &&
                                       (request.IsToplevelRequest /*|| result.ParentRequest.IsToplevelRequest*/);
                if (isTraced || needsOneMoreTry)
                {
                    //writeDebugLine("ERROR: Try Again since NULL " + tagHandler);
                    bool wsl = request.SuspendSearchLimits;
                    try
                    {
                        request.SuspendSearchLimits = true;
                        cp = tagHandler.CompleteAimlProcess();
                        if (Unifiable.IsNull(cp))
                        {
                            childSuccess = false;
                            return tagHandler.GetTemplateNodeInnerText();
                        }
                        if (false && Unifiable.IsNullOrEmpty(cp))
                        {
                            // trace the next line to see why
                            AIMLTagHandler handler = tagHandler;
                            TargetBot.TraceTest("ERROR: Try Again since NULL " + handler,
                                () => { cp = handler.CompleteAimlProcess(); });
                        }
                    }
                    finally
                    {
                        request.SuspendSearchLimits = wsl;

                    }
                }
            }
            if (tagHandler.QueryHasFailed)
            {
                childSuccess = false;
                return tagHandler.FAIL;
            }
            childSuccess = !tagHandler.QueryHasFailed;
            if (!childSuccess)
            {

            }
            var st = IsSilentTag(node);
            var ine = Unifiable.IsNullOrEmpty(cp);
            if (!ine || st)
            {
                childSuccess = true;
                return cp;
            }
            if (Unifiable.IsNull(cp))
            {
                cp = tagHandler.GetTemplateNodeInnerText();
                if (tagHandler.QueryHasSuceeded)
                {
                    childSuccess = true;
                    return cp;
                }
                return cp;
            }
            return cp;
        }

    }
}