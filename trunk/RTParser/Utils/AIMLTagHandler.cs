using System;
using System.Collections;
using System.Collections.Generic;
using System.IO;
using System.Text;
using System.Text.RegularExpressions;
using System.Threading;
using System.Xml;
using RTParser.AIMLTagHandlers;
using RTParser.Variables;

namespace RTParser.Utils
{
    /// <summary>
    /// The template for all classes that handle the AIML tags found within template nodes of a
    /// category.
    /// </summary>
    abstract public partial class AIMLTagHandler : TextTransformer, IXmlLineInfo
    {

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

        /// <summary>
        /// Attributes that we use from AIML not intended to be stacked nto user dictionary
        /// </summary>
        public static ICollection<string> ReservedAttributes = new HashSet<string> { };
        public bool IsStarted = false;

        /// <summary>
        /// By calling this and not just ProcessChange() 
        /// You've ensure we have a proper calling context
        /// </summary>
        /// <returns></returns>
        public override Unifiable ProcessAimlChange()
        {
            ThreadStart OnExit = EnterTag(request,templateNode,query);
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


        static public ThreadStart EnterTag(Request request, XmlNode templateNode, SubQuery query)
        {
            bool needsUnwind = false;
            object thiz = (object)query ?? request;
            ISettingsDictionary dict = query ?? request.TargetSettings;
            XmlAttributeCollection collection = templateNode.Attributes;
            if (collection!=null && collection.Count > 0)
            {
                // graphmaster
                GraphMaster oldGraph = request.Graph;
                GraphMaster newGraph = null;
                // topic
                Unifiable oldTopic = request.Topic;
                Unifiable newTopic = null;
                
                // that
                Unifiable oldThat = request.That;
                Unifiable newThat = null;

                UndoStack savedValues = null;

                foreach (XmlAttribute node in collection)
                {
                    switch (node.Name.ToLower())
                    {
                        case "graph":
                            {
                                string graphName = ReduceStar(node.Value, query, dict);
                                if (graphName != null)
                                {
                                    GraphMaster innerGraph = request.TargetBot.GetGraph(graphName, oldGraph);
                                    needsUnwind = true;
                                    if (innerGraph != null)
                                    {
                                        if (innerGraph != oldGraph)
                                        {
                                            request.Graph = innerGraph;
                                            newGraph = innerGraph;
                                            request.writeToLog("ENTERING: {0} as {1} from {2}",
                                                               graphName, innerGraph, oldGraph);
                                        }
                                        else
                                        {
                                            newGraph = innerGraph;
                                        }
                                    }
                                    else
                                    {
                                        oldGraph = null; //?
                                    }
                                }
                            }
                            break;
                        case "topic":
                            {
                                newTopic = ReduceStar(node.Value, query, dict);
                                if (newTopic != null)
                                {
                                    if (newTopic.IsEmpty) newTopic = "Nothing";
                                    needsUnwind = true;
                                    request.Topic = newTopic;
                                }

                            }
                            break;
                        case "that":
                            {
                                newThat = ReduceStar(node.Value, query, dict);
                                if (newThat != null)
                                {
                                    if (newThat.IsEmpty) newThat = "Nothing";
                                    needsUnwind = true;
                                    request.That = newThat;
                                }

                            }
                            break;
                        case "name":
                            continue;
                        case "index":
                            continue;
                        case "default":
                            continue;
                        case "match":
                            continue;
                        case "value":
                            continue;

                        default:
                            {

                                string n = node.Name;
                                if (ReservedAttributes.Contains(n))
                                {
                                    continue;
                                }
                                Unifiable v = (Unifiable)ReduceStar(node.Value, query, dict);
                                UndoStack.FindUndoAll(thiz);
                                savedValues = savedValues ?? UndoStack.GetStackFor(thiz);
                                //savedValues = savedValues ?? query.GetFreshUndoStack();
                                savedValues.pushValues(dict, n, v);
                                needsUnwind = true;
                            }
                            break;
                    }
                }

                // unwind
                if (needsUnwind)
                {
                    return () =>
                               {
                                   try
                                   {

                                       if (savedValues != null)
                                       {
                                           savedValues.UndoAll();
                                       }
                                       if (newGraph != null)
                                       {
                                           var cg = request.Graph;
                                           if (cg==newGraph)
                                           {
                                               request.writeToLog("LEAVING: {0}  back to {1}", request.Graph, oldGraph);
                                               request.Graph = oldGraph;                                               
                                           } else
                                           {
                                               request.writeToLog(
                                                   "WARNING: UNWIND GRAPH UNEXPECTED CHANGE {0} FROM {1} SETTING TO {2}",
                                                   cg, newGraph, oldGraph);
                                               request.Graph = oldGraph;
                                           }
                                       }
                                       if (newTopic != null)
                                       {
                                           var ct = request.Topic;
                                           if (newTopic == ct)
                                           {
                                               request.Topic = oldTopic;
                                           }
                                           else
                                           {
                                               request.writeToLog(
                                                   "WARNING: UNWIND TOPIC UNEXPECTED CHANGE {0} FROM {1} SETTING TO {2}",
                                                   ct, newTopic, oldTopic);
                                               request.Topic = oldTopic;
                                           }
                                       }
                                       if (newThat != null)
                                       {
                                           var ct = request.That;
                                           if (newThat == ct)
                                           {
                                               request.That = oldThat;
                                           }
                                           else
                                           {
                                               request.writeToLog(
                                                   "WARNING: UNWIND THAT UNEXPECTED CHANGE {0} FROM {1} SETTING TO {2}",
                                                   ct, newThat, oldThat);
                                               request.That = oldThat;
                                           }
                                       }
                                   }
                                   catch (Exception ex)
                                   {
                                       request.writeToLog("ERROR " + ex);
                                   }
                               };
                }
            }
            return () => { };
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
                return Succeed(templateNode.OuterXml);
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
        public RTParser.Result result
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

        /// <summary>
        /// The template node to be processed by the class
        /// </summary>
        public XmlNode templateNode;

        protected bool ReadOnly
        {
            get { return false; }
            set { throw new NotImplementedException(); }
        }

        protected Unifiable RecurseResult = Unifiable.NULL;
        protected static Func<string> EmptyFunct = (() => String.Empty);
        protected static Func<string> NullStringFunct = (() => null);


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
                Unifiable before = Unifiable.InnerXmlText(this.templateNode); //.InnerXml;        
                return CheckValue(before);
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
                    return (LineInfoElement)temp; //.FirstChild;}
                }
                return getNode("<node>" + outerXML + "</node>");
                //return (LineInfoElement)temp; //.FirstChild;}
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
                string named = "From " + outerXML;
                if (templateNode != null)
                {
                    named = "" + templateNode.OwnerDocument;
                    if (string.IsNullOrEmpty(named)) named = "from: " + templateNode.OuterXml;
                }
                XmlDocumentLineInfo doc = null;// templateNode.OwnerDocument as XmlDocumentLineInfo;
                if (doc == null)
                {
                    doc = new XmlDocumentLineInfo(named, true);
                }
                doc.Load(sr);
                var de = doc.DocumentElement;
                //doc.IsReadOnly = false;
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
                if (Unifiable.IsNullOrEmpty(v))
                {
                    if (!Unifiable.IsNullOrEmpty(recursiveResult))
                        ;// v = recursiveResult;
                }
                return CheckValue(v);
            }
            else
            {
                string resultNodeInnerXML = tagHandler.Transform();
                LineInfoElement resultNode = AIMLTagHandler.getNode("<node>" + resultNodeInnerXML + "</node>", templateNode);
                resultNode.ReadOnly = false;
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
    }
}
