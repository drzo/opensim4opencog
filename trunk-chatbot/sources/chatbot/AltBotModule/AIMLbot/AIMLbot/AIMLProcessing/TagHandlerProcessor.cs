using System;
using System.Collections.Generic;
using System.Web;
using System.Xml;
using AltAIMLbot;
using AltAIMLbot.Utils;
using AltAIMLParser;
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

        internal AIMLTagHandlerU GetTagHandler(User user, SubQuery query, Request request, Result result, XmlNode node,
                                              AIMLTagHandlerU parentTagHandlerU)
        {
            AIMLTagHandlerU tag = GetTagHandlerU(user, query, request, result, node, true);
            if (query != null) query.CurrentTagHandlerU = tag;
            if (query != null) query.CurrentNode = node;
            if (tag == null)
            {
                writeToLog("NULL TAG " + node.OuterXml);
            }
            if (tag != null)
            {
                tag.SetParent(parentTagHandlerU);
                if (query != null) query.LastTagHandlerU = parentTagHandlerU ?? tag;
                TemplateInfo ti = tag.templateInfo;
                if (ti == null && query != null)
                {
                    ti = query.CurrentTemplate;
                }
                if (ti == null && parentTagHandlerU != null)
                {
                    ti = parentTagHandlerU.GetTemplateInfo();
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

        internal AIMLTagHandlerU GetTagHandlerU(User user, SubQuery query, Request request, Result result, XmlNode node, bool liText)
        {
            AIMLTagHandlerU tagHandlerU = getBespokeTags(user, query, request, result, node);
            string nodeNameLower = ToLower(node.LocalName);
            AltBot targetBot = query.TargetBot;
            if (Equals(null, tagHandlerU))
            {
                switch (nodeNameLower)
                {
                    case "template":
                    case "answer": //CML
                        tagHandlerU = new template(targetBot, user, query, request, result, node);
                        break;
                    case "aiml":
                    case "cml": //CML
                        tagHandlerU = new aiml(targetBot, user, query, request, result, node);
                        break;
                    case "aimlexec":
                    case "eval":
                        tagHandlerU = new aimlexec(targetBot, user, query, request, result, node);
                        break;
                    case "vars":
                    case "root":
                    case "predicates": //CML
                        tagHandlerU = new root(targetBot, user, query, request, result, node, (() => query.TargetSettings));
                        break;
                    case "properties":
                    case "bots":
                        tagHandlerU = new root(targetBot, user, query, request, result, node,
                                              (() => request.TargetBot.GlobalSettings));
                        break;
                    case "substitutions":
                        tagHandlerU = new root(targetBot, user, query, request, result, node,
                                              (() => request.TargetBot.InputSubstitutions));
                        break;
                    case "topic":
                    case "conversation": //CML
                        tagHandlerU = new topic(targetBot, user, query, request, result, node);
                        break;
                    case "category":
                    case "conv": //CML
                        tagHandlerU = new category(targetBot, user, query, request, result, node);
                        break;
                    case "and":
                        tagHandlerU = new and(targetBot, user, query, request, result, node);
                        break;
                    case "or":
                        tagHandlerU = new or(targetBot, user, query, request, result, node);
                        break;
                    case "optional":
                        tagHandlerU = new optional(targetBot, user, query, request, result, node);
                        break;
                    case "isa":
                        tagHandlerU = new isa(targetBot, user, query, request, result, node);
                        break;
                    case "bot":
                        tagHandlerU = new bot(targetBot, user, query, request, result, node);
                        break;
                    case "condition":
                    case "options": //cml
                        tagHandlerU = new condition(targetBot, user, query, request, result, node);
                        break;
                    case "li":
                        if (liText)
                            tagHandlerU = new liif(targetBot, user, query, request, result, node);
                        break;
                    case "if":
                        tagHandlerU = new liif(targetBot, user, query, request, result, node);
                        break;
                    case "personf":
                        tagHandlerU = new format(targetBot, user, query, request, result, node,
                                                new Func<string, string>(HttpUtility.UrlEncode),
                                                null);
                        break;
                    case "date":
                        tagHandlerU = new date(targetBot, user, query, request, result, node);
                        break;
                    case "formal":
                        tagHandlerU = new formal(targetBot, user, query, request, result, node);
                        break;
                    case "gender":
                        tagHandlerU = new gender(targetBot, user, query, request, result, node);
                        break;
                    case "get":
                        tagHandlerU = new get(targetBot, user, query, request, result, node);
                        break;
                    case "gossip":
                        tagHandlerU = new gossip(targetBot, user, query, request, result, node);
                        break;
                    case "get_ip":
                    case "id":
                        tagHandlerU = new id(targetBot, user, query, request, result, node);
                        break;
                    case "inputreq":
                        tagHandlerU = new inputreq(targetBot, user, query, request, result, node);
                        break;
                    case "request":
                        tagHandlerU = new input(targetBot, user, query, request, result, node, 1);
                        break;
                    case "input":
                        tagHandlerU = new input(targetBot, user, query, request, result, node, 1);
                        break;
                    case "justthat": // <input index="2"/> 
                        tagHandlerU = new input(targetBot, user, query, request, result, node, 2);
                        break;
                    case "beforethat": // <input index="3"/> 
                        tagHandlerU = new input(targetBot, user, query, request, result, node, 3);
                        break;
#if !(__MonoCS__)
                    case "javascript":
                        tagHandlerU = new javascript(targetBot, user, query, request, result, node);
                        break;
#endif
                    case "learn":
                    case "load":
                    case "noload": // the commented version of <load>
                        tagHandlerU = new learn(targetBot, user, query, request, result, node);
                        break;
                    case "lowercase":
                        tagHandlerU = new lowercase(targetBot, user, query, request, result, node);
                        break;
                    case "person":
                        tagHandlerU = new substitute(targetBot, user, query, request, result, node);
                        break;
                    case "person2":
                        tagHandlerU = new substitute(targetBot, user, query, request, result, node);
                        break;
                    case "random":
                        tagHandlerU = new random(targetBot, user, query, request, result, node);
                        break;
                    case "sentence":
                        tagHandlerU = new sentence(targetBot, user, query, request, result, node);
                        break;
                    case "set":
                        tagHandlerU = new set(targetBot, user, query, request, result, node);
                        break;
                    case "size":
                    case "getsize":
                        tagHandlerU = new size(targetBot, user, query, request, result, node);
                        break;
                    case "sr":
                        tagHandlerU = new sr(targetBot, user, query, request, result, node);
                        break;
                    case "srai":
                        tagHandlerU = new srai(targetBot, user, query, request, result, node);
                        break;
                    case "star":
                        tagHandlerU = new star(targetBot, user, query, request, result, node);
                        break;
                    case "system":
                        tagHandlerU = new system(targetBot, user, query, request, result, node);
                        break;
                    case "that": //default <that index="1,1"/>
                        tagHandlerU = new that(targetBot, user, query, request, result, node, 1);
                        break;
                    case "justbeforethat": //treated as <that index="2,1"/>
                        tagHandlerU = new that(targetBot, user, query, request, result, node, 2);
                        break;
                    case "response": //treated as <that index="1,1"/>
                        tagHandlerU = new that(targetBot, user, query, request, result, node, 2);
                        break;
                    case "thatstar":
                        tagHandlerU = new thatstar(targetBot, user, query, request, result, node);
                        break;
                    case "think":
                        tagHandlerU = new think(targetBot, user, query, request, result, node);
                        break;
                    case "topicstar":
                        tagHandlerU = new topicstar(targetBot, user, query, request, result, node);
                        break;
                    case "uppercase":
                        tagHandlerU = new uppercase(targetBot, user, query, request, result, node);
                        break;
                    case "version":
                    case "getversion":
                        tagHandlerU = new version(targetBot, user, query, request, result, node);
                        break;
                    case "cycsystem":
                        tagHandlerU = new cycsystem(targetBot, user, query, request, result, node);
                        break;
                    case "cycretract":
                        tagHandlerU = new cycretract(targetBot, user, query, request, result, node);
                        break;
                    case "cycassert":
                        tagHandlerU = new cycassert(targetBot, user, query, request, result, node);
                        break;
                    case "cycterm":
                        tagHandlerU = new cycterm(targetBot, user, query, request, result, node);
                        break;
                    case "cycquery":
                        tagHandlerU = new cycquery(targetBot, user, query, request, result, node);
                        break;
                    case "cyccondition":
                        tagHandlerU = new cyccondition(targetBot, user, query, request, result, node);
                        break;
                    case "cycphrase":
                        tagHandlerU = new cycphrase(targetBot, user, query, request, result, node);
                        break;
                    case "cycparaphrase":
                        tagHandlerU = new cycphrase(targetBot, user, query, request, result, node);
                        break;
                    case "guard":
                        tagHandlerU = new guard(targetBot, user, query, request, result, node);
                        break;
                    case "guardstar":
                        tagHandlerU = new guardstar(targetBot, user, query, request, result, node);
                        break;
                    case "cycrandom":
                        tagHandlerU = new cycrandom(targetBot, user, query, request, result, node);
                        break;
                    case "tscore":
                        tagHandlerU = new tscore(targetBot, user, query, request, result, node);
                        break;
                    case "space":
                        tagHandlerU = new space(targetBot, user, query, request, result, node);
                        break;
                    case "markov":
                        tagHandlerU = new markov(targetBot, user, query, request, result, node);
                        break;
                    case "soundcode":
                        tagHandlerU = new soundcode(targetBot, user, query, request, result, node);
                        break;

                    // MSM
                    case "msm":
                        tagHandlerU = new msm(targetBot, user, query, request, result, node);
                        break;
                    case "processmsm":
                        tagHandlerU = new process_msm(targetBot, user, query, request, result, node);
                        break;
                    case "setstate":
                        tagHandlerU = new setstate(targetBot, user, query, request, result, node);
                        break;
                    case "state":
                        tagHandlerU = new state(targetBot, user, query, request, result, node);
                        break;
                    case "transition":
                        tagHandlerU = new transition(targetBot, user, query, request, result, node);
                        break;
                    case "setevidence":
                        tagHandlerU = new setevidence(targetBot, user, query, request, result, node);
                        break;
                    case "evidenceassoc":
                        tagHandlerU = new evidence_assoc(targetBot, user, query, request, result, node);
                        break;
                    case "evidencepattern":
                        tagHandlerU = new evidence_pattern(targetBot, user, query, request, result, node);
                        break;
                    case "evidencestate":
                        tagHandlerU = new evidencestate(targetBot, user, query, request, result, node);
                        break;
                    case "dependentmachine":
                        tagHandlerU = new dependentmachine(targetBot, user, query, request, result, node);
                        break;
                    case "responsetopic":
                        tagHandlerU = new response_topic(targetBot, user, query, request, result, node);
                        break;

                    case "push":
                        tagHandlerU = new push(targetBot, user, query, request, result, node);
                        break;
                    case "pop":
                        tagHandlerU = new pop(targetBot, user, query, request, result, node);
                        break;
                    case "peekstack":
                        tagHandlerU = new peekstack(targetBot, user, query, request, result, node);
                        break;

                    case "lex":
                        tagHandlerU = new lex(targetBot, user, query, request, result, node);
                        break;
                    case "lexset":
                        tagHandlerU = new lexset(targetBot, user, query, request, result, node);
                        break;
                    case "lexis":
                        tagHandlerU = new lexis(targetBot, user, query, request, result, node);
                        break;

                    case "dbpush":
                        tagHandlerU = new dbpush(targetBot, user, query, request, result, node);
                        break;
                    case "dbquery":
                        tagHandlerU = new dbquery(targetBot, user, query, request, result, node);
                        break;
                    case "dbupdate":
                        tagHandlerU = new dbupdate(targetBot, user, query, request, result, node);
                        break;
                    case "dbdelete":
                        tagHandlerU = new dbdelete(targetBot, user, query, request, result, node);
                        break;
                    case "dbload":
                        tagHandlerU = new dbload(targetBot, user, query, request, result, node);
                        break;


                    case "regex":
                        tagHandlerU = new regex(targetBot, user, query, request, result, node);
                        break;

                    case "bind": // <bind>#$isa</bind>
                        tagHandlerU = new bind(targetBot, user, query, request, result, node);
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
            if (tagHandlerU != null) return tagHandlerU;
            if (IsHtmlTag(node.Name))
            {
                return new recursiveVerbatum(node, targetBot, user, query, request, result, node, true);
            }
            if (tagHandlerU == null)
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
                        LineInfoElementImpl newnode = StaticXMLUtils.CopyNode(prefix.Value, node, false);
                        XmlAttributeLineInfo atr = (XmlAttributeLineInfo)newnode.OwnerDocument.CreateAttribute("name");
                        atr.ReadOnly = false;
                        atr.Value = name;
                        newnode.Attributes.Append(atr);
                        if (node.Name.ToLower() != newnode.Name.ToLower())
                        {
                            writeToLog("AIMLLOADER: converted " + node.OuterXml + " -> " + newnode.OuterXml);
                            return GetTagHandlerU(user, query, request, result, newnode, liText);
                        }
                        writeToLog("AIMLLOADER: ! convert " + node.OuterXml + " -> " + newnode.OuterXml);
                    }
                }
            }
            if (tagHandlerU != null) return tagHandlerU;
            if (nodeNameLower == "name")
            {
                return new bot(targetBot, user, query, request, result, node);
            }

            tagHandlerU = new lazyClosure(targetBot, user, query, request, result, node);
            writeToLog("AIMLLOADER:  lazyClosure: " + node.OuterXml);
            return tagHandlerU;
        }

        static public void writeToLog(string message, params object[] args)
        {
            AltBot.writeDebugLine(message, args);
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
        /// <param name="parentHandlerU"></param>
        /// <param name="templateInfo"></param>
        /// <param name="copyChild"></param>
        /// <param name="copyParent"></param>
        /// <returns></returns>
        public AIMLTagHandlerU proccessResponse(SubQuery query,
                                     Request request, Result result,
                                     XmlNode templateNode, GuardInfo sGuard,
                                     out bool createdOutput, out bool templateSucceeded,
                                     AIMLTagHandlerU parentHandlerU, TemplateInfo templateInfo,
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
                                           parentHandlerU, templateInfo, copyChild, copyParent);
                return childTagHandler;
            }
            finally
            {
                undoStack.UndoAll();
            }
        }

        private AIMLTagHandlerU proccessTemplate(SubQuery query, Request request, Result result,
                                                XmlNode templateNode, GuardInfo sGuard,
                                                out bool createdOutput, out bool templateSucceeded,
                                                AIMLTagHandlerU parentHandlerU, TemplateInfo templateInfo,
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
                                           parentHandlerU, templateInfo, copyChild, copyParent);

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
                        request.AddOutputSentences(templateInfo, ex.TemplateOutput, result, request.TopLevelScore);
                    }
                    templateSucceeded = ex.TemplateSucceeded;
                    createdOutput = ex.CreatedOutput;
                    return ex.TagHandlerU;
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

        public AIMLTagHandlerU proccessResponse000(SubQuery query, Request request, Result result,
                                                XmlNode sOutput, GuardInfo sGuard,
                                                out bool createdOutput, out bool templateSucceeded,
                                                AIMLTagHandlerU parentHandlerU, TemplateInfo templateInfo,
                                                bool copyChild, bool copyParent)
        {
            AltBot Proc = query.TargetBot;

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
            AIMLTagHandlerU tagHandlerU = GetTagHandler(request.Requester, query, request, result,
                                                                  templateNode, parentHandlerU);
            string outputSentenceOut = processNode(templateNode, query,
                                                               request, result, request.Requester, parentHandlerU,
                                                               protectChild, copyParent, tagHandlerU, suspendingLimits, out templateSucceeded);

            templateSucceeded = !IsFalse(outputSentenceOut);

            if (outputSentenceOut == null)
            {
                if (tagHandlerU == null)
                {
                    writeToLog("tagHandler = null " + output);
                }
                else
                {
                    bool success = false;
                    if (tagHandlerU.QueryHasSuceeded) success = true;
                    if (tagHandlerU.QueryHasFailed) success = false;
                    if (success)
                    {
                        writeToLog("Very BaD " + output);
                    }
                    else
                    {
                    }
                    templateSucceeded = false;
                    createdOutput = false;
                    return tagHandlerU;
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
                    request.AddOutputSentences(templateInfo, o, result, request.TopLevelScore);
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

                return tagHandlerU;
            }
            try
            {
                templateSucceeded = !IsFalse(left);
                if (!templateSucceeded)
                {
                    createdOutput = false;
                    return tagHandlerU;
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
                        return tagHandlerU;
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
                    return tagHandlerU;
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
                    request.AddOutputSentences(templateInfo, o, result, request.TopLevelScore);
                    return tagHandlerU;
                }
                else
                {
                    writeToLog("GUARD SKIP '{0}' TEMPLATE={1}", outputSentence,
                               ParentTextAndSourceInfo(templateNode));
                }
                templateSucceeded = false;
                createdOutput = false;
                return tagHandlerU;
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
                return tagHandlerU;
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
                                  AIMLTagHandlerU parentHandlerU, bool protectChild, bool copyParent,
                                  AIMLTagHandlerU nodeHandlerU, bool suspendLimits, out bool templateSucceeded)
        {
            Request originalSalientRequest = Request.GetOriginalSalientRequest(request);
            var wasSuspendRestrati = request.SuspendSearchLimits;
            templateSucceeded = true;
            request.SuspendSearchLimits = suspendLimits;
            Dictionary<Unifiable, Unifiable> sraiMark = null;
            if (srai.UseSraiLimiters)
            {
                sraiMark = originalSalientRequest.CreateSRAIMark();
            }
            try
            {
                bool childSuccess;
                string outputSentence = processNodeVV(node, query,
                                                      request, result, user, parentHandlerU,
                                                      protectChild, copyParent, nodeHandlerU, out childSuccess);
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
                if (nodeHandlerU.RecurseResultValid) return nodeHandlerU.RecurseResult;
                return outputSentence;
            }
            finally
            {
                if (srai.UseSraiLimiters) originalSalientRequest.ResetSRAIResults(sraiMark);
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
                                  AIMLTagHandlerU parentHandlerU, bool protectChild, bool copyParent,
                                  AIMLTagHandlerU tagHandlerU, out bool childSuccess)
        {
            AltBot TargetBot = request.TargetBot;
            childSuccess = true;
            if (node == null)
            {
                string whyError = "ERROR null NODE " + tagHandlerU;
                writeToLog(whyError);
                throw new ChatSignalOverBudget(request, whyError);
            }
            if (node.NodeType == XmlNodeType.Text)
            {
                childSuccess = true;
                if (tagHandlerU != null)
                {
                    tagHandlerU.QueryHasSuceeded = true;
                }
                string s = Trim(TextNodeValue(node));
                if (!String.IsNullOrEmpty(s))
                {
                    return ValueText(s);
                }
                //return s;
            }
            if (tagHandlerU == null)
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
            if (ReferenceEquals(null, tagHandlerU))
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
                tagHandlerU.Dispose();
                tagHandlerU = null;
                childSuccess = true;
                return Unifiable.Empty;
            }

            tagHandlerU.SetParent(parentHandlerU);
            //if (parent!=null) parent.AddChild(tagHandler);

            Unifiable cp = tagHandlerU.CompleteAimlProcess();
            if (Unifiable.IsNullOrEmpty(cp) && (!tagHandlerU.QueryHasSuceeded || tagHandlerU.QueryHasFailed))
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
                        cp = tagHandlerU.CompleteAimlProcess();
                        if (Unifiable.IsNull(cp))
                        {
                            childSuccess = false;
                            return tagHandlerU.GetTemplateNodeInnerText();
                        }
                        if (false && Unifiable.IsNullOrEmpty(cp))
                        {
                            // trace the next line to see why
                            AIMLTagHandlerU handlerU = tagHandlerU;
                            TargetBot.TraceTest("ERROR: Try Again since NULL " + handlerU,
                                () => { cp = handlerU.CompleteAimlProcess(); });
                        }
                    }
                    finally
                    {
                        request.SuspendSearchLimits = wsl;

                    }
                }
            }
            if (tagHandlerU.QueryHasFailed)
            {
                childSuccess = false;
                return tagHandlerU.FAIL;
            }
            childSuccess = !tagHandlerU.QueryHasFailed;
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
                cp = tagHandlerU.GetTemplateNodeInnerText();
                if (tagHandlerU.QueryHasSuceeded)
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