using System;
using System.Collections.Generic;
using System.Collections;
//using System.Linq;
using System.Text;
using System.Text.RegularExpressions;
using System.Runtime.Serialization.Formatters.Binary;
using System.Xml;
using System.Threading;
using System.IO;
using AltAIMLbot.Utils;
using Aima.Core.Logic.Propositional.Algorithms;
using Aima.Core.Logic.Propositional.Parsing;
using Aima.Core.Logic.Propositional.Parsing.AST;
using Aima.Core.Logic.Propositional.Visitors;
using MiniSatCS;
using System.Reflection;
/******************************************************************************************
AltAIMLBot -- Copyright (c) 2011-2012,Kino Coursey, Daxtron Labs

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and
associated documentation files (the "Software"), to deal in the Software without restriction,
including without limitation the rights to use, copy, modify, merge, publish, distribute,
sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or
substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT
NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT
OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
**************************************************************************************************/

namespace AltAIMLbot
{
    [Serializable]
    public class BehaviorTree
    {
        public string curState;
        public string initialState;
        public Int32 transitionTime;
        public Random rgen = new Random();
        public string name;
        public int tickRate = 1000;
        public long satCount = 0;
        public long satMod = 400;
        public Dictionary<string, int> restorePoint;
        private string serialDoc = null;

        public string monitorBehavior = null; // Sub-behavior for every tick
        public Stack<string> monitorStack = new Stack<string>();

        public AltBot bot
        {
            get { return _bot; }
            set
            {
                _bot = value;

            }
        }

        [NonSerialized]
        private AltBot _bot;
        [NonSerialized]
        public XmlDocument treeDoc;

        // Kinda based on the idea at ...
        // http://www.garagegames.com/community/blogs/view/21143
        public BehaviorTree()
        {
            treeDoc = new XmlDocument();
            restorePoint = new Dictionary<string, int>();

        }


        public void preSerial()
        {
            serialDoc = treeDoc.OuterXml;
        }
        public void postSerial(AltBot bot)
        {
            _bot = bot;
            if (treeDoc == null) treeDoc = new XmlDocument();
            treeDoc.LoadXml(serialDoc);
        }

        public void defineBehavior(string mname, string behaviorDef)
        {
            try
            {
                name = mname;
                behaviorDef = behaviorDef.Replace("&gt;", ">");
                behaviorDef = behaviorDef.Replace("&lt;", "<");

                treeDoc.LoadXml(behaviorDef);
                //initialState = rulesDoc.FirstChild.Attributes["initialstate"].Value;
                //transitionTime = Environment.TickCount;
                //curState = initialState;
                //XmlNodeList rules = rulesDoc.SelectNodes(String.Format(ruleMatch, initialState));
            }
            catch (Exception e)
            {
                Console.WriteLine("ERR: SourceTree =", mname);
                Console.WriteLine("ERR:" + e.Message);
                Console.WriteLine("ERR:" + e.StackTrace);
                Console.WriteLine("ERR XML:" + behaviorDef);
            }
        }

        public IEnumerable<RunStatus> runBehaviorTree(AltBot theBot)
        {
            bot = theBot;
            // Execute All Children
            foreach (XmlNode childNode in treeDoc.ChildNodes)
            {
                RunStatus childResult = RunStatus.Failure;
                foreach (RunStatus myChildResult in processNode(childNode))
                {
                    childResult = myChildResult;
                    if (childResult != RunStatus.Running) break;
                    yield return RunStatus.Running;
                }

                // Except for Asserts
                if (isAnAssert(childNode.Name))
                {
                    if (childResult == RunStatus.Failure)
                    {
                        yield return RunStatus.Failure;
                    }
                }
                else
                {
                    // Normal processing (We dont care)
                    if (childResult == RunStatus.Success)
                    {
                        // return RunStatus.Success;
                    }
                }
            }
            yield return RunStatus.Success;
            yield break;
        }

        /// <summary>
        /// Executes a subBehavior in the same context
        /// </summary>
        /// <param name="subDoc"></param>
        /// <returns></returns>
        public IEnumerable<RunStatus> runSubTree(XmlDocument subDoc)
        {
            // Execute All Children
            foreach (XmlNode childNode in subDoc.ChildNodes)
            {
                RunStatus childResult = RunStatus.Failure;
                foreach (RunStatus myChildResult in processNode(childNode))
                {
                    childResult = myChildResult;
                    if (childResult != RunStatus.Running) break;
                    yield return RunStatus.Running;
                }

                // Except for Asserts
                if (isAnAssert(childNode.Name))
                {
                    if (childResult == RunStatus.Failure)
                    {
                        yield return RunStatus.Failure;
                        yield break;
                    }
                }
                else
                {
                    // Normal processing (We dont care)
                    if (childResult == RunStatus.Success)
                    {
                        // return RunStatus.Success;
                    }
                }
            }
            yield return RunStatus.Success;
            yield break;
        }

        public double Ema(double newVal, double oldVal, int N)
        {
            double K = (double)2 / (double)(1 + N);
            return (double)(K * (newVal - oldVal)) + oldVal;
        }

        public bool isAnAssert(string nodeName)
        {
            return (
                       (nodeName.ToLower() == "assert")
                    || (nodeName.ToLower() == "asserttimer")
                    || (nodeName.ToLower() == "assertguest")
                    );
        }
        public bool isBreaker(string nodeName)
        {
            return ((nodeName.ToLower() == "breaker"));
        }

        // in case we want the xpath of a node
        // see http://stackoverflow.com/questions/241238/how-to-get-xpath-from-an-xmlnode-instance-c-sharp
        //     http://stackoverflow.com/questions/451950/get-the-xpath-to-an-xelement
        // includes names for readibility. maybe useful for behavior stack
        public string getIndent(XmlNode node)
        {
            if (node.ParentNode == null)
            {
                // the only node with no parent is the root node, which has no path 
                return "";
            }
            return "  " + getIndent(node.ParentNode);
        }

        public string GetXPathToNode(XmlNode node)
        {
            if (node.NodeType == XmlNodeType.Attribute)
            {
                // attributes have an OwnerElement, not a ParentNode; also they have              
                // to be matched by name, not found by position              
                return String.Format("{0}/@{1}", GetXPathToNode(((XmlAttribute)node).OwnerElement), node.Name);
            }
            if (node.ParentNode == null)
            {
                // the only node with no parent is the root node, which has no path 
                return "";
            }

            //get the index 
            int iIndex = 1;
            XmlNode xnIndex = node;
            while (xnIndex.PreviousSibling != null && xnIndex.PreviousSibling.Name == xnIndex.Name)
            {
                iIndex++;
                xnIndex = xnIndex.PreviousSibling;
            }

            // the path to a node is the path to its parent, plus "/node()[n]", where 
            // n is its position among its siblings.          
            return String.Format("{0}/{1}[{2}]", GetXPathToNode(node.ParentNode), node.Name, iIndex);
        }

        public RunStatus tickMonitor()
        {
            RunStatus result = RunStatus.Success;
            if (monitorBehavior == null) return RunStatus.Success;
            if (monitorBehavior.Length == 0) return RunStatus.Success;
            if (bot.myBehaviors.behaveTrees.ContainsKey(monitorBehavior))
            {
                result = bot.myBehaviors.runBotBehavior(monitorBehavior, bot);
            }
            return result;
        }

        public IEnumerable<RunStatus> atomicSuccess()
        {
            yield return RunStatus.Success;
        }
        public IEnumerable<RunStatus> atomicFailure()
        {
            yield return RunStatus.Failure ;
        }
        public IEnumerable<RunStatus> atomicRunning()
        {
            yield return RunStatus.Running ;
        }

        public void logNode(string msg, XmlNode myNode)
        {
            lock (bot.loglock)
            {
                string miniLog = String.Format(@"./aiml/BTTrace.txt");
                string astr = "";
                string indent = getIndent(myNode);
                if (myNode.Attributes != null)
                {
                    foreach (XmlAttribute anode in myNode.Attributes)
                    {
                        string evnt = anode.Name.ToLower();
                        string val = anode.Value;
                        astr += " " + evnt + "='" + val + "'";
                    }
                }
                try
                {
                    string tag = String.Format("{1} {0}<{2} {3} >\n", msg, indent, myNode.Name.ToLower(), astr);
                    System.IO.File.AppendAllText(miniLog, tag);
                }
                catch
                { }
            }

        }
        public void logText(string msg)
        {
            lock (bot.loglock)
            {
                try
                {
                    string miniLog = String.Format(@"./aiml/BTTrace.txt");
                    System.IO.File.AppendAllText(miniLog, msg + "\n");
                }
                catch
                { }
            }
        }
        public IEnumerable<RunStatus> processNode(XmlNode myNode)
        {
            if (myNode == null)
            {
                yield return RunStatus.Failure;
                yield break;
            }
            Thread.Sleep(50);
            RunStatus myResult = RunStatus.Failure;
            string nodeID = "null";
            try
            {
                if ((myNode.Attributes != null) && (myNode.Attributes.Count > 0))
                {
                    if (myNode.Attributes["id"] != null)
                    {
                        nodeID = myNode.Attributes["id"].Value;
                    }
                }
            }
            catch (Exception e)
            {
                nodeID = "null";
            }

            // Initiate our status
            if (!bot.myBehaviors.runState.ContainsKey(nodeID))
            {
                bot.myBehaviors.runState.Add(nodeID,  RunStatus.Running);

            }
            bot.myBehaviors.runState[nodeID] = RunStatus.Running;

            bool origCritical = bot.inCritical;
            logNode("BEGIN", myNode);
            ProcessNodeAddEvents(myNode);
            // Console.WriteLine("Process BNode {1} {0}", nodeID, myNode.Name.ToLower());
            // Start a winner
            bot.myBehaviors.keepTime(nodeID, RunStatus.Success);
            //check watchdog if any
            tickMonitor();
                switch (myNode.Name.ToLower())
                {
                    case "assert":
                        foreach (RunStatus result in ProcessAssert(myNode))
                        {
                            myResult = result;
                            bot.myBehaviors.runState[nodeID]=myResult;
                            if (myResult != RunStatus.Running) break;
                            yield return result;
                        }
                        break;
                    case "asserttimer":
                         foreach (RunStatus result in ProcessAssertTimer(myNode))
                         {
                             myResult = result;
                             bot.myBehaviors.runState[nodeID] = myResult;
                             if (myResult != RunStatus.Running) break;
                             yield return result;
                         }
                         break;
                    case "sequence":
                        foreach (RunStatus result in ProcessSequence(myNode))
                        {
                            myResult = result;
                            bot.myBehaviors.runState[nodeID] = myResult;
                            if (myResult != RunStatus.Running) break;
                            yield return result;
                        }
                        break;
                    case "selector":
                        foreach (RunStatus result in ProcessSelector(myNode))
                        {
                            myResult = result;
                            bot.myBehaviors.runState[nodeID] = myResult;
                            if (myResult != RunStatus.Running) break;
                            yield return result;
                        }
                        break;
                    case "random":
                        foreach (RunStatus result in ProcessRandomSelector(myNode))
                        {
                            myResult = result;
                            bot.myBehaviors.runState[nodeID] = myResult;
                            if (myResult != RunStatus.Running) break;
                            yield return result;
                        }
                        break;
                    case "weighted":
                        foreach (RunStatus result in ProcessWeightedSelector(myNode))
                        {
                            myResult = result;
                            bot.myBehaviors.runState[nodeID] = myResult;
                            if (myResult != RunStatus.Running) break;
                            yield return result;
                        }
                        break;
                    case "parallel":
                        foreach (RunStatus result in  ProcessParallel(myNode))
                        {
                            myResult = result;
                            bot.myBehaviors.runState[nodeID] = myResult;
                            if (myResult != RunStatus.Running) break;
                            yield return result;
                        }
                        break;
                    case "loop":
                        foreach (RunStatus result in  ProcessLoop(myNode))
                        {
                            myResult = result;
                            bot.myBehaviors.runState[nodeID] = myResult;
                            if (myResult != RunStatus.Running) break;
                            yield return result;
                        }
                        break;
                    case "loopuntil":
                        foreach (RunStatus result in  ProcessLoopUntil(myNode))
                        {
                            myResult = result;
                            bot.myBehaviors.runState[nodeID] = myResult;
                            if (myResult != RunStatus.Running) break;
                            yield return result;
                        }
                        break;
                    case "task":
                        foreach (RunStatus result in ProcessTask(myNode))
                         { 
                            myResult = result;
                            bot.myBehaviors.runState[nodeID] = myResult;
                            if (myResult != RunStatus.Running) break;
                            yield return result;
                         }
                        break; 
                    case "subaiml":
                        foreach (RunStatus result in ProcessStateAiml(myNode))
                        {
                            myResult = result;
                            bot.myBehaviors.runState[nodeID] = myResult;
                            if (myResult != RunStatus.Running) break;
                            yield return result;
                        }
                        break;
                    case "drive":
                        foreach (RunStatus result in ProcessDrive(myNode))
                        //Thread.Sleep(2000);
                        {
                            myResult = result;
                            bot.myBehaviors.runState[nodeID] = myResult;
                            if (myResult != RunStatus.Running) break;
                            yield return result;
                        }
                        break;
                    case "motive":
                        foreach (RunStatus result in ProcessMotive(myNode))
                        //Thread.Sleep(2000);
                        {
                            myResult = result;
                            bot.myBehaviors.runState[nodeID] = myResult;
                            if (myResult != RunStatus.Running) break;
                            yield return result;
                        }
                        break;

                    case "inhibit":
                        this.bot.myBehaviors.VSoup.setRefVal(nodeID, 0);
                        bot.myBehaviors.runState[nodeID] = RunStatus.Success;
                        { yield return RunStatus.Success; }
                        break;

                    case "release":
                        this.bot.myBehaviors.VSoup.adjust(nodeID, 1.0);
                        bot.myBehaviors.runState[nodeID] = RunStatus.Success;
                        { yield return RunStatus.Success; }
                        break;

                    case "behavior":
                        foreach (RunStatus result in ProcessBehavior(myNode))
                        {
                            myResult = result;
                            bot.myBehaviors.runState[nodeID] = myResult;
                            if (myResult != RunStatus.Running) break;
                            yield return result;
                        }
                        break;
                    case "rbehavior":
                        foreach (RunStatus result in ProcessRBehavior(myNode))
                        {
                            myResult = result;
                            bot.myBehaviors.runState[nodeID] = myResult;
                            if (myResult != RunStatus.Running) break;
                            yield return result;
                        }
                        break;
                    case "subbehavior":
                        foreach (RunStatus result in ProcessSubBehavior(myNode))
                        {
                            myResult = result;
                            bot.myBehaviors.runState[nodeID] = myResult;
                            if (myResult != RunStatus.Running) break;
                            yield return result;
                        }
                        break;
                    case "scheduler":
                        foreach (RunStatus result in ProcessScheduler(myNode))
                        {
                            myResult = result;
                            bot.myBehaviors.runState[nodeID] = myResult;
                            if (myResult != RunStatus.Running) break;
                            yield return result;
                        }
                        break;
                    case "starttimer":
                        // start a stopwatch
                        bot.myBehaviors.keepTime(nodeID, RunStatus.Success);
                        bot.myBehaviors.activationTime(nodeID, RunStatus.Success);
                        bot.myBehaviors.runEventHandler("onsuccess");
                        ProcessNodeDeleteEvents(myNode);
                        bot.inCritical = origCritical;
                        bot.myBehaviors.runState[nodeID] = RunStatus.Success;
                        yield return RunStatus.Success;
                        yield break;
                        break;
                    case "stoptimer":
                        // stop the stopwatch
                        bot.myBehaviors.keepTime(nodeID, RunStatus.Failure);
                        if (bot.myBehaviors.entryTime.ContainsKey(nodeID))
                        {
                            bot.myBehaviors.entryTime.Remove(nodeID);
                        }
                        bot.myBehaviors.runEventHandler("onsuccess");
                        ProcessNodeDeleteEvents(myNode);
                        bot.inCritical = origCritical;
                        bot.myBehaviors.runState[nodeID] = RunStatus.Success;
                        yield return RunStatus.Success;
                        yield  break;
                        break;

                    case "tellkb":
                        foreach (RunStatus result in ProcessTellKB(myNode))
                        {
                            myResult = result;
                            bot.myBehaviors.runState[nodeID] = myResult;
                            if (myResult != RunStatus.Running) break;
                            yield return result;
                        }
                        break;
                    case "tellbasekb":
                        foreach (RunStatus result in ProcessTellBaseKB(myNode))
                        {
                            myResult = result;
                            bot.myBehaviors.runState[nodeID] = myResult;
                            if (myResult != RunStatus.Running) break;
                            yield return result;
                        }
                        break;
                    case "tellkbocc":
                        foreach (RunStatus result in ProcessTellKBOCC(myNode))
                        {
                            myResult = result;
                            bot.myBehaviors.runState[nodeID] = myResult;
                            if (myResult != RunStatus.Running) break;
                            yield return result;
                        }
                        break;
                    case "clearkb":
                        foreach (RunStatus result in ProcessClearKB(myNode))
                        {
                            myResult = result;
                            bot.myBehaviors.runState[nodeID] = myResult;
                            if (myResult != RunStatus.Running) break;
                            yield return result;
                        }
                        break;
                    case "clearbasekb":
                        foreach (RunStatus result in  ProcessClearBaseKB(myNode))
                        {
                            myResult = result;
                            bot.myBehaviors.runState[nodeID] = myResult;
                            if (myResult != RunStatus.Running) break;
                            yield return result;
                        }
                        break;
                    case "processkb":
                        foreach (RunStatus result in ProcessKBModel(myNode))
                        {
                            myResult = result;
                            bot.myBehaviors.runState[nodeID] = myResult;
                            if (myResult != RunStatus.Running) break;
                            yield return result;
                        }
                        break;

                    case "chat":
                        foreach (RunStatus result in  ProcessChat(myNode))
                        {
                            myResult = result;
                            bot.myBehaviors.runState[nodeID] = myResult;
                            if (myResult != RunStatus.Running) break;
                            yield return result;
                        }
                        break;

                    case "assertguest":
                        foreach (RunStatus result in ProcessAssertGuest(myNode))
                        {
                            myResult = result;
                            bot.myBehaviors.runState[nodeID] = myResult;
                            if (myResult != RunStatus.Running) break;
                            yield return result;
                        }
                        break;
                    case "taskguest":
                        foreach (RunStatus result in  ProcessTaskGuest(myNode))
                        {
                            myResult = result;
                            bot.myBehaviors.runState[nodeID] = myResult;
                            if (myResult != RunStatus.Running) break;
                            yield return result;
                        }
                        break;
                    case "enqueue":
                        foreach (RunStatus result in ProcessEnqueue(myNode))
                        {
                            myResult = result;
                            bot.myBehaviors.runState[nodeID] = myResult;
                            if (myResult != RunStatus.Running) break;
                            yield return result;
                        }
                        break;
                    case "flushqueue":
                        foreach (RunStatus result in ProcessFlushQueue(myNode))
                        {
                            myResult = result;
                            bot.myBehaviors.runState[nodeID] = myResult;
                            if (myResult != RunStatus.Running) break;
                            yield return result;
                        }
                        break;
                    case "breaker":
                        foreach (RunStatus result in  ProcessBreaker(myNode))
                        {
                            myResult = result;
                            bot.myBehaviors.runState[nodeID] = myResult;
                            if (myResult != RunStatus.Running) break;
                            yield return result;
                        }
                        break;

                    default:
                        // Ignore the Nops
                        bot.myBehaviors.runState[nodeID] = RunStatus.Success;
                        yield return RunStatus.Success;
                        yield  break;
                        break;
                }
             try
            {
           }
            catch (Exception e)
            {
                bot.inCritical = origCritical;
                myResult = RunStatus .Failure ;
                Console.WriteLine("### BNODE ERR:{0} {1} {2}", myNode.Name.ToLower(), nodeID, myResult);
                Console.WriteLine("### BNODE ERR:" + e.Message);
                Console.WriteLine("### BNODE ERR:" + e.StackTrace);
                Console.WriteLine("### BNODE ERR NODE XML:" + myNode.OuterXml);
                if (myNode.ParentNode != null)
                {
                    Console.WriteLine("### BNODE ERR PARENT XML:" + myNode.ParentNode.OuterXml);
                }
            }
            // update on exit
            //bot.inCritical = origCritical;
             bot.myBehaviors.runState[nodeID] = myResult;
             bot.myBehaviors.keepTime(nodeID, myResult);
             bot.myBehaviors.activationTime(nodeID, myResult);

             if ((myResult == RunStatus.Success) && (nodeID != "null"))
            {
                // Console.WriteLine("Result BNode {0} {1} {2}", myNode.Name.ToLower(), nodeID, result);
            }
             if (myResult == RunStatus.Success) bot.myBehaviors.runEventHandler("onsuccess");
             if (myResult == RunStatus.Failure) bot.myBehaviors.runEventHandler("onfail");
            ProcessNodeDeleteEvents(myNode);
            logNode("END(" + myResult.ToString() + ") ", myNode);
            bot.inCritical = origCritical;
            yield return myResult;
            yield break;
        }
        /// <summary>
        /// ProcessNodeAddEvents: saves previous "onX" hanlders and adds defines owns
        /// </summary>
        /// <param name="myNode"></param>
        public void ProcessNodeAddEvents(XmlNode myNode)
        {
            this.bot.myBehaviors.pushHandlers();
            foreach (XmlAttribute anode in myNode.Attributes)
            {
                string evnt = anode.Name.ToLower();
                string val = anode.Value;
                if (evnt.StartsWith("on"))
                {
                    this.bot.myBehaviors.addEventHandler(evnt, val);
                }
                if (evnt == "incritical")
                {
                    this.bot.inCritical = (val == "true");
                }
                if (evnt == "onmonitor")
                {
                    monitorStack.Push(monitorBehavior);
                    monitorBehavior = val;
                }
            }
        }

        /// <summary>
        /// ProcessNodeDeleteEvents: deletes current"onX" hanlders and restores previously defined ones
        /// </summary>
        /// <param name="myNode"></param>

        public void ProcessNodeDeleteEvents(XmlNode myNode)
        {
            foreach (XmlAttribute anode in myNode.Attributes)
            {
                string evnt = anode.Name.ToLower();
                string val = anode.Value;
                if (evnt.StartsWith("on"))
                {
                    this.bot.myBehaviors.deleteEventHandler(evnt, val);
                }
                if (evnt == "onmonitor")
                {
                    if (monitorStack.Count > 0)
                    {
                        monitorBehavior = monitorStack.Pop();
                    }
                }
            }
            this.bot.myBehaviors.popHandlers();
        }


        #region TagProcessors

        public IEnumerable<RunStatus> ProcessPushBehavior(XmlNode myNode)
        {
            string behavior = myNode.InnerText;
            bot.myBehaviors.pushUniqueToStack(behavior);
            yield return RunStatus.Success;
        }
        public IEnumerable<RunStatus> ProcessPopBehavior(XmlNode myNode)
        {
            bot.myBehaviors.processOneEventStack();
            yield return RunStatus.Success;
        }
        public IEnumerable<RunStatus> ProcessPopRandomBehavior(XmlNode myNode)
        {
            bot.myBehaviors.processRandomEventStack();
            yield return RunStatus.Success;
        }

        public IEnumerable<RunStatus> ProcessBehavior(XmlNode myNode)
        {
            // A behavior is implicitly a <parallel> node
            //    RunStatus result = RunStatus.Failure;
            //    result = ProcessParallel(myNode);
            //    return result;

            // behavior accepts attributes of
            //restore
            //pace
            //onchat
            //onrestore
            //onabort
            //onsuccess
            //onfail

            bool restorable = false;
            bool restoring = false;
            int pace = 0;
            bool continueflag = true;
            try
            {
                if (myNode.Attributes["restore"] != null)
                {
                    restorable |= (myNode.Attributes["restore"].Value.ToLower() == "true");
                }
                if (myNode.Attributes["resumes"] != null)
                {
                    restorable |= (myNode.Attributes["resumes"].Value.ToLower() == "true");
                }
                if (myNode.Attributes["pace"] != null)
                {
                    pace = 5000;
                    pace = Int32.Parse(myNode.Attributes["pace"].Value);
                }
            }
            catch (Exception e)
            {
                Console.WriteLine("ERROR: processing <Behavior > attributes");
            }
            // just use <parallel> if its not restorable
            if (restorable == false)
            {
                RunStatus result = RunStatus.Failure;
                //result = ProcessParallel(myNode);
                foreach (RunStatus myChildResult in ProcessParallel(myNode))
                {
                    result = myChildResult;
                    if (result != RunStatus.Running) break;
                    yield return RunStatus.Running;
                }

                yield return result;
                yield break;
            }
            Console.WriteLine("\n\n****** COMPLEX BEHAVIOR BEGIN\n\n");
            string nodeID = "null";
            try
            {
                if ((myNode.Attributes != null) && (myNode.Attributes.Count > 0))
                {
                    if (myNode.Attributes["id"] != null)
                    {
                        nodeID = myNode.Attributes["id"].Value;
                    }
                }
            }
            catch (Exception e)
            {
                nodeID = "null";
            }
            // If it is not restorable then set index to zero
            if (!restorePoint.ContainsKey(nodeID))
                restorePoint.Add(nodeID, 0);

            if (restorable == false)
            {
                restorePoint[nodeID] = 0;
            }

            int restP = restorePoint[nodeID];
            restoring = (restP > 0) && (restP < myNode.ChildNodes.Count - 1); ;

            // if restorable then check all asserts up to the restore point
            int childIndex = 0;
            for (childIndex = 0; childIndex < restP; childIndex++)
            {
                XmlNode childNode = myNode.ChildNodes[childIndex];
                if (isAnAssert(childNode.Name))
                {
                    //RunStatus childResult = processNode(childNode);
                    RunStatus childResult = RunStatus.Failure;
                    foreach (RunStatus myChildResult in processNode(childNode))
                    {
                        childResult = myChildResult;
                        if (childResult != RunStatus.Running) break;
                        yield return RunStatus.Running;
                    }
                    if (childResult == RunStatus.Failure)
                    {
                        yield return RunStatus.Failure;
                        yield break;
                    }
                }

            }
            // Execute All Children
            if (restoring)
            {
                bot.myBehaviors.queueEvent("onrestore");
                continueflag = tickEventQueues(continueflag);
            }
            //foreach (XmlNode childNode in myNode.ChildNodes)
            for (childIndex = restP; childIndex < myNode.ChildNodes.Count; childIndex++)
            {
                XmlNode childNode = myNode.ChildNodes[childIndex];
                //RunStatus childResult = processNode(childNode);
                //RunStatus childResult = processNode(childNode);
                RunStatus childResult = RunStatus.Failure;
                foreach (RunStatus myChildResult in processNode(childNode))
                {
                    childResult = myChildResult;
                    if (childResult != RunStatus.Running) break;
                    yield return RunStatus.Running;
                }
                // Except for Asserts
                if (isAnAssert(childNode.Name))
                {
                    if (childResult == RunStatus.Failure)
                    {
                        yield return RunStatus.Failure;
                        yield break;
                    }
                }
                else
                {
                    // Normal processing (We dont care)
                    if (childResult == RunStatus.Success)
                    {
                        // return RunStatus.Success;
                    }
                }
                restorePoint[nodeID] = childIndex;
                // if child is a breaker node 
                //   you should return to the next sibling not the breaker
                //   which will just break again 
                // do we have an interrupt ?
                if (isBreaker(childNode.Name))
                {
                    restorePoint[nodeID] = childIndex + 1;
                }

                continueflag = tickEventQueues(continueflag);
                if (continueflag == false)
                {
                    restorePoint[nodeID] = childIndex;
                    if (isBreaker(childNode.Name))
                    {
                        restorePoint[nodeID] = childIndex + 1;
                    }
                    Console.WriteLine("\n\n****** COMPLEX BEHAVIOR EXIT FAILURE\n\n");
                    // we put this behavior in the running for resumption
                    bot.myBehaviors.pushUniqueToStack(nodeID);
                    yield return RunStatus.Failure;
                    yield break;
                }
                if (pace > 0)
                {
                    long timeout = Environment.TickCount + pace;
                    while (Environment.TickCount < timeout)
                    {
                        yield return RunStatus.Running;
                        //Thread.Sleep(pace);
                    }
                }

            }
            // if we make it to the end then the reset the restore point
            restorePoint[nodeID] = 0;
            bot.myBehaviors.removeFromStack(nodeID);
            Console.WriteLine("\n\n****** COMPLEX BEHAVIOR EXIT SUCCESS\n\n");
            yield return RunStatus.Success;
            yield break;
        }

        public IEnumerable<RunStatus> ProcessBreaker(XmlNode myNode)
        {

            RunStatus result = RunStatus.Success;
            try
            {
                bot.myBehaviors.queueEvent("break");
            }
            catch (Exception e)
            {
                Console.WriteLine("ERR: ProcessBraker");
                Console.WriteLine("ERR:" + e.Message);
                Console.WriteLine("ERR:" + e.StackTrace);
                Console.WriteLine("ERR XML:" + myNode.OuterXml);
            }
            yield return result;
        }

        public IEnumerable<RunStatus> ProcessRBehavior(XmlNode myNode)
        {
            // A RBehavior is implicitly a <random> node
            //    RunStatus result = RunStatus.Failure;
            //    result = ProcessRandom(myNode);
            //    return result;

            // This node should pick some child at random
            // and remember and return to this child until it returns success
            // this is a top level intermediate node.
            // The real recovery should be in the child called, but it needs
            // to remember in case the child is interrupted, you want to 
            // go back to the same child
            // Could be thought of as a random routing behavior 

            // behavior accepts attributes of
            //restore
            //pace
            //onchat
            //onrestore
            //onabort
            //onsuccess
            //onfail

            bool restorable = false;
            bool restoring = false;
            int pace = 0;
            bool continueflag = true;
            try
            {
                if (myNode.Attributes["restore"] != null)
                {
                    restorable |= (myNode.Attributes["restore"].Value.ToLower() == "true");
                }
                if (myNode.Attributes["resumes"] != null)
                {
                    restorable |= (myNode.Attributes["resumes"].Value.ToLower() == "true");
                }
                if (myNode.Attributes["pace"] != null)
                {
                    pace = 5000;
                    pace = Int32.Parse(myNode.Attributes["pace"].Value);
                }
            }
            catch (Exception e)
            {
                Console.WriteLine("ERROR: processing <RBehavior > attributes");
            }
            // just use <random> if its not restorable
            if (restorable == false)
            {
                //RunStatus result = RunStatus.Failure;
                //result = ProcessRandomSelector(myNode);

                RunStatus result = RunStatus.Failure;
                foreach (RunStatus myChildResult in ProcessRandomSelector(myNode))
                {
                    result = myChildResult;
                    if (result != RunStatus.Running) break;
                    yield return RunStatus.Running;
                }

                yield return result;
                yield break;
            }
            Console.WriteLine("\n\n****** COMPLEX RBEHAVIOR BEGIN\n\n");
            string nodeID = "null";
            try
            {
                if ((myNode.Attributes != null) && (myNode.Attributes.Count > 0))
                {
                    if (myNode.Attributes["id"] != null)
                    {
                        nodeID = myNode.Attributes["id"].Value;
                    }
                }
            }
            catch (Exception e)
            {
                nodeID = "null";
            }
            // If it is not restorable then set index to zero
            if (!restorePoint.ContainsKey(nodeID))
                restorePoint.Add(nodeID, -1);

            if (restorable == false)
            {
                restorePoint[nodeID] = -1;
            }

            int restP = restorePoint[nodeID];
            restoring = (restP >= 0) && (restP < myNode.ChildNodes.Count - 1); // zero node matters in an RBehavior

            // if restorable then check all asserts up to the restore point
            int childIndex = 0;
            for (childIndex = 0; childIndex < restP; childIndex++)
            {
                XmlNode childNode = myNode.ChildNodes[childIndex];
                if (isAnAssert(childNode.Name))
                {
                    //RunStatus childResult = processNode(childNode);
                    RunStatus childResult = RunStatus.Failure;
                    foreach (RunStatus myChildResult in processNode(childNode))
                    {
                        childResult = myChildResult;
                        if (childResult != RunStatus.Running) break;
                        yield return RunStatus.Running;
                    }
                    if (childResult == RunStatus.Failure)
                    {
                        yield return RunStatus.Failure;
                        yield break;
                    }
                }

            }
            // Execute All Children
            if (restoring)
            {
                bot.myBehaviors.queueEvent("onrestore");
                continueflag = tickEventQueues(continueflag);
            }
            else
            {
                int randomChild = this.bot.myRandMem.selectOneXMLIndex(myNode);

                restorePoint[nodeID] = randomChild;
                restP = randomChild;
            }

            //foreach (XmlNode childNode in myNode.ChildNodes)
            //for (childIndex = restP; childIndex < myNode.ChildNodes.Count; childIndex++)
            childIndex = restP;
            {
                XmlNode childNode = myNode.ChildNodes[childIndex];
                //RunStatus childResult = processNode(childNode);
                //RunStatus childResult = processNode(childNode);

                RunStatus childResult = RunStatus.Failure;
                foreach (RunStatus myChildResult in processNode(childNode))
                {
                    childResult = myChildResult;
                    if (childResult != RunStatus.Running) break;
                    yield return RunStatus.Running;
                }

                // Except for Asserts
                if (isAnAssert(childNode.Name))
                {
                    if (childResult == RunStatus.Failure)
                    {
                        yield return RunStatus.Failure;
                        yield break;
                    }
                }
                else
                {
                    // Normal processing (We dont care)
                    if (childResult == RunStatus.Success)
                    {
                        // return RunStatus.Success;
                        // For a RBehavior child their karma is complete
                        restorePoint[nodeID] = -1;
                    }
                }
                //restorePoint[nodeID] = childIndex;
                // do we have an interrupt ?

                continueflag = tickEventQueues(continueflag);
                if ((continueflag == false) || (childResult == RunStatus.Failure))
                {
                    restorePoint[nodeID] = childIndex;
                    Console.WriteLine("\n\n****** COMPLEX RBEHAVIOR EXIT FAILURE\n\n");

                    yield return RunStatus.Failure;
                    yield break;

                }
                if (pace > 0)
                {
                    long timeout = Environment.TickCount + pace;
                    while (Environment.TickCount < timeout)
                    {
                        yield return RunStatus.Running;
                        //Thread.Sleep(pace);
                    }
                }

            }
            // if we make it to the end then the reset the restore point
            restorePoint[nodeID] = -1;
            bot.myBehaviors.removeFromStack(nodeID);
            Console.WriteLine("\n\n****** COMPLEX RBEHAVIOR EXIT SUCCESS\n\n");
            yield return RunStatus.Success;
            yield break;
        }

        public bool tickEventQueues(bool continueflag)
        {
            while ((bot.outputQueue.Count > 0) || (bot.myBehaviors.eventQueue.Count > 0))
            {
                if (bot.myBehaviors.eventQueue.Count > 0)
                {
                    string peekstr = bot.myBehaviors.eventQueue.Peek();
                    if (peekstr == "abort")
                    {

                        continueflag = false;
                        bot.myBehaviors.queueEvent("onabort");
                        bot.flushOutputQueue();
                    }
                    if (peekstr == "break")
                    {

                        continueflag = false;
                        bot.myBehaviors.queueEvent("onbreak");
                        bot.flushOutputQueue();
                    }
                    bot.myBehaviors.processOneEventQueue();
                }
                bot.processOutputQueue();
                processSATEvents();
            }
            return continueflag;
        }

        public void processSATEvents()
        {
            // Scan to see if any defined events are listed
            // in the positive SATModel table
            // this means events can be SAT inferable
            // so you can have onpanic, onhappy, onfear
            // One problem could be multiple triggers, but that
            // could be handled by the called behaviors
            // like onmousemove versus onclick
            if (bot.myPositiveSATModleString == null) return;
            if (bot.myBehaviors.eventTable.Count == 0) return;
            foreach (string evnt in bot.myBehaviors.eventTable.Keys)
            {
                if (bot.myPositiveSATModleString.Contains(evnt))
                {
                    bot.myBehaviors.queueEvent(evnt);
                }
            }
        }

        public RunStatus ProcessSubBehavior0(XmlNode myNode)
        {

            RunStatus result = RunStatus.Failure;
            string behaviorName = "root";
            try
            {
                behaviorName = myNode.Attributes["id"].Value;
                result = bot.myBehaviors.runBotBehavior(behaviorName, bot);
            }
            catch (Exception e)
            {
                Console.WriteLine("ERR: ProcessSubBehavior");
                Console.WriteLine("ERR:" + e.Message);
                Console.WriteLine("ERR:" + e.StackTrace);
                Console.WriteLine("ERR XML:" + myNode.OuterXml);
            }
            return result;
        }
        public IEnumerable<RunStatus> ProcessSubBehavior(XmlNode myNode)
        {

            RunStatus result = RunStatus.Failure;
            string behaviorName = "root";
                behaviorName = myNode.Attributes["id"].Value;
                //result = bot.myBehaviors.runBotBehavior(behaviorName, bot);
                if (bot.myBehaviors.definedBehavior(behaviorName))
                {
                    logText("ProcessSubBehavior found:" + behaviorName);
                    BehaviorTree curTree = (BehaviorTree)bot.myBehaviors.behaveTrees[behaviorName];

                    //result = runSubTree(curTree.treeDoc);
                    foreach (RunStatus myChildResult in runSubTree(curTree.treeDoc))
                    {
                        result = myChildResult;
                        if (result != RunStatus.Running) break;
                        yield return RunStatus.Running;
                    }
                }
                else
                {
                    // We don't have one
                    logText("ProcessSubBehavior did >>>> NOT <<<< find:" + behaviorName);
                    result = RunStatus.Failure;
                }
            try
            {
            }
            catch (Exception e)
            {
                Console.WriteLine("ERR: ProcessSubBehavior");
                Console.WriteLine("ERR:" + e.Message);
                Console.WriteLine("ERR:" + e.StackTrace);
                Console.WriteLine("ERR XML:" + myNode.OuterXml);
            }
            yield return result;
            yield break;
        }

        public IEnumerable<RunStatus> ProcessScheduler(XmlNode myNode)
        {
            // talk to the scheduler directly
            RunStatus result = RunStatus.Success;
            string behaviorName = "root";
            string action = "";
            string query = "";
            try
            {
                //behaviorName = myNode.Attributes["id"].Value;
                if (myNode.Attributes["action"] != null) action = myNode.Attributes["action"].Value;
                if (myNode.Attributes["query"] != null) query = myNode.Attributes["query"].Value;
                if (myNode.Attributes["a"] != null) action = myNode.Attributes["a"].Value;
                if (myNode.Attributes["q"] != null) query = myNode.Attributes["q"].Value;
                if (myNode.Attributes["id"] != null) behaviorName = myNode.Attributes["id"].Value;
                StreamWriter sw = new StreamWriter("procschedtag.txt");
                bot.myServitor.myScheduler.performAction(sw, action, query, behaviorName);

            }
            catch (Exception e)
            {
                Console.WriteLine("ERR: ProcessSubBehavior");
                Console.WriteLine("ERR:" + e.Message);
                Console.WriteLine("ERR:" + e.StackTrace);
                Console.WriteLine("ERR XML:" + myNode.OuterXml);
            }
            yield return result;
            yield break;
        }

        public IEnumerable<RunStatus> ProcessEnqueue(XmlNode myNode)
        {

            RunStatus result = RunStatus.Success;
            string beventNames = "root";
            try
            {
                beventNames = myNode.InnerText;
                string[] behaviorList = beventNames.Split(' ');
                foreach (string behavior in behaviorList)
                {
                    bot.myBehaviors.queueEvent(behavior);
                }
                logText(String.Format("EVENTQUEUE: {0}", bot.myBehaviors.eventQueue.ToString()));
            }
            catch (Exception e)
            {
                Console.WriteLine("ERR: ProcessEnqueue");
                Console.WriteLine("ERR:" + e.Message);
                Console.WriteLine("ERR:" + e.StackTrace);
                Console.WriteLine("ERR XML:" + myNode.OuterXml);
            }
            yield return result;
            yield break;
        }
        public IEnumerable<RunStatus> ProcessFlushQueue(XmlNode myNode)
        {

            RunStatus result = RunStatus.Success;
            try
            {
                bot.myBehaviors.eventQueue.Clear();
                bot.myBehaviors.behaviorStack.Clear();
                logText(String.Format("EVENTQUEUE: {0}", bot.myBehaviors.eventQueue.ToString()));
                logText(String.Format("behaviorStack: {0}", bot.myBehaviors.behaviorStack.ToString()));

            }
            catch (Exception e)
            {
                Console.WriteLine("ERR: ProcessFlushQueue");
                Console.WriteLine("ERR:" + e.Message);
                Console.WriteLine("ERR:" + e.StackTrace);
                Console.WriteLine("ERR XML:" + myNode.OuterXml);
            }
            yield return result;
            yield break;
        }
        public IEnumerable<RunStatus> ProcessDrive(XmlNode myNode)
        {
            // <drive name="needFood" halflife="3600" threshold="0.2">
            // <motive name="motiveFear" halflife="3600" threshold="0.2">
            //  halflife in seconds
            //  once it returns success it will reset the timer and
            //   will not try until the estimated timer decay is below threshold
            // Motive = Threshold.Positive
            // Drive = Threshold.Negative

            RunStatus result = RunStatus.Failure;
            string driveName = "root";
            double halflife = 60000;
            double threshold = 0.1;
            double lastRun = int.MaxValue;
            double curV = 0;
            try
            {
                driveName = myNode.Attributes["id"].Value;
                halflife = 1000 * double.Parse(myNode.Attributes["halflife"].Value);
                threshold = double.Parse(myNode.Attributes["threshold"].Value);

                bot.myBehaviors.VSoup.setHalflife(driveName, halflife);
                curV = bot.myBehaviors.VSoup.CurVal(driveName);

                //lastRun = bot.myBehaviors.lastRunning(driveName);
                //curV = Math.Pow( 0.5,(lastRun / halflife));

                //Console.WriteLine("Drive check '{0}' = {1} ={2}, hfl={3}, thrs = {4}",driveName,lastRun ,curV,halflife,threshold );
                // Drive does not need satisfying
            }
            catch (Exception e)
            {
                Console.WriteLine("*** DRIVE EXCEPTION : {0} {1}", e.Message, e.StackTrace);
            }

                if (curV > threshold)
                {
                    yield return RunStatus.Failure;
                    yield break;
                }

                Console.WriteLine("  *** Execute Drive Procedure:{0} ***", driveName);
                // Run the children just like a selector
                //  if they any are true then you have success
                //result = ProcessSelector(myNode);
                RunStatus childResult = RunStatus.Failure;
                foreach (RunStatus myChildResult in ProcessSelector(myNode))
                {
                    result = myChildResult;
                    if (result != RunStatus.Running) break;
                    yield return RunStatus.Running;
                }

            try
            {
                // Reset the last run timer
                if (result == RunStatus.Success)
                {
                    //bot.myBehaviors.activationTime(driveName, result);
                    bot.myBehaviors.VSoup.adjust(driveName, 1.0);
                    Console.WriteLine("  *** Drive Procedure Success:{0} ***", driveName);
                }
                else
                {
                    Console.WriteLine("  *** Drive Procedure Failure:{0} ***", driveName);
                }

            }
            catch (Exception e)
            {
                Console.WriteLine("*** DRIVE EXCEPTION : {0} {1}", e.Message, e.StackTrace);
            }
            yield return result;
            yield break;
        }

        public IEnumerable<RunStatus> ProcessMotive(XmlNode myNode)
        {
            // <drive name="needFood" halflife="3600" threshold="0.2">
            // <motive name="motiveFear" halflife="3600" threshold="0.2">
            //  halflife in seconds
            //  once it returns success it will reset the timer and
            //   will not try until the estimated timer decay is below threshold
            // Motive = Threshold.Positive
            // Drive = Threshold.Negative

            RunStatus result = RunStatus.Failure;
            string driveName = "root";
            double halflife = 60000;
            double threshold = 0.1;
            double lastRun = int.MaxValue;
            double curV = 0;
            try
            {
                driveName = myNode.Attributes["id"].Value;
                halflife = 1000 * double.Parse(myNode.Attributes["halflife"].Value);
                threshold = double.Parse(myNode.Attributes["threshold"].Value);

                bot.myBehaviors.VSoup.setHalflife(driveName, halflife);
                curV = bot.myBehaviors.VSoup.CurVal(driveName);

                //lastRun = bot.myBehaviors.lastRunning(driveName);
                //curV = Math.Pow( 0.5,(lastRun / halflife));

                Console.WriteLine("Motive check '{0}' = {1} ={2}, hfl={3}, thrs = {4}", driveName, lastRun, curV, halflife, threshold);
                // Drive does not need satisfying
            }
            catch (Exception e)
            {
                Console.WriteLine("*** Motive EXCEPTION : {0} {1}", e.Message, e.StackTrace);
            }

                if (curV < threshold)
                {
                    yield return RunStatus.Failure;
                    yield break;
                }

                Console.WriteLine("  *** Motive Drive Procedure:{0} ***", driveName);
                // Run the children just like a selector
                //  if they any are true then you have success
                //result = ProcessSelector(myNode);
                //RunStatus result = RunStatus.Failure;
                foreach (RunStatus myChildResult in processNode(myNode))
                {
                    result = myChildResult;
                    if (result != RunStatus.Running) break;
                    yield return RunStatus.Running;
                }

            try
            {
                // Reset the last run timer
                if (result == RunStatus.Success)
                {
                    //bot.myBehaviors.activationTime(driveName, result);
                    //bot.myBehaviors.VSoup.setRefVal(driveName, 0);
                    Console.WriteLine("  *** Motive Procedure Success:{0} ***", driveName);
                }
                else
                {
                    Console.WriteLine("  *** Motive Procedure Failure:{0} ***", driveName);
                }

            }
            catch (Exception e)
            {
                Console.WriteLine("*** Motive EXCEPTION : {0} {1}", e.Message, e.StackTrace);
            }
            yield return result;
            yield break;

        }
        public IEnumerable<RunStatus> ProcessAssert(XmlNode myNode)
        {
            string condData = myNode.Attributes["cond"] == null ? null : myNode.Attributes["cond"].Value;
            string[] parms = condData.Trim().Split(' ');
            string varName = parms[0];
            string rel = "";
            string val = "";
            double dval = 0;
            Int32 elapsedTime = Environment.TickCount - transitionTime;

            //Console.WriteLine("  CondData='{0}' '{1}' '{2}' '{3}'", condData, varName,rel,val);
            try { rel = parms[1].ToLower(); }
            catch { }
            try { val = parms[2]; }
            catch { }
            try
            {
                dval = double.Parse(val);
            }
            catch (Exception e)
            {
                dval = 0;
            }
            // Fetch conditional test
            // Checking blackboard/cache but should include 
            // bot predicates,cache, chemistry
            string sv = "";
            double bbVal = 0;
            try
            {
                //sv = myChemistry.m_cBus.getHash("mdollhearduuid");
                sv = bot.myChemistry.m_cBus.getHash(varName);
                bbVal = double.Parse(sv);
            }
            catch (Exception e) { }

            // Special variables?
            if (varName == "timeout") bbVal = elapsedTime;
            if (varName == "behaviorstackcount") bbVal = bot.myBehaviors.behaviorStack.Count;
            if (varName == "behaviorqueuecount") bbVal = bot.myBehaviors.eventQueue.Count;
            if (varName == "prob") bbVal = rgen.NextDouble();
            if (varName.Contains(".runtime"))
            {
                string tName = varName.Replace(".runtime", "");
                bbVal = bot.myBehaviors.timeRunning(tName);
            }
            if (varName.Contains(".lastrun"))
            {
                string tName = varName.Replace(".lastrun", "");
                bbVal = bot.myBehaviors.lastRunning(tName);
            }
            if (varName.Contains(".drive"))
            {
                string dName = varName.Replace(".drive", "");
                double halflife = 1000 * double.Parse(myNode.Attributes["halflife"].Value);
                int lastRun = bot.myBehaviors.lastRunning(dName);
                bbVal = Math.Pow(0.5, (lastRun / halflife));
            }
            //Console.WriteLine("  CondTest=('{0}' '{1}') '{2}' '{3}'", sv, bbVal, rel, val);

            // Check Condition
            bool valid = false;
            Match match = null;
            switch (rel)
            {
                case "==":
                    valid = (bbVal == dval);
                    break;
                case "<":
                    valid = (bbVal < dval);
                    break;
                case ">":
                    valid = (bbVal > dval);
                    break;
                case "lt":
                    valid = (bbVal < dval);
                    break;
                case "gt":
                    valid = (bbVal > dval);
                    break;
                case "<=":
                    valid = (bbVal <= dval);
                    break;
                case ">=":
                    valid = (bbVal >= dval);
                    break;
                case "matches":
                    match = (new Regex(val)).Match(sv);
                    valid = match.Success;
                    break;
                case "!matches":
                    match = (new Regex(val)).Match(sv);
                    valid = !match.Success;
                    break;
                case "notmatches":
                    match = (new Regex(val)).Match(sv);
                    valid = !match.Success;
                    break;
                case "=~":
                    match = (new Regex(val)).Match(sv);
                    valid = match.Success;
                    break;
                case "!~":
                    match = (new Regex(val)).Match(sv);
                    valid = !match.Success;
                    break;
            }

            // General check of the last valid propositional model
            // <assert cond="activemodel (NOT selfWantsTouch)"/>
            if (varName == "activemodel")
            {
                string query = condData;
                query = query.Replace("activemodel", "");
                query = query.Replace("implies", "=>");
                query = query.Replace(" imp ", "=> ");
                query = query.Replace("equiv", "<=>");
                query = query.Trim();

                Sentence sen = (Sentence)new PEParser().Parse(query);
                try
                {
                    valid = bot.myActiveModel.IsTrue(sen);
                }
                catch
                {
                    valid = false;
                }
            }

            if (varName == "TRUE") valid = true;
            // The return
            if (valid)
            {
                yield return RunStatus.Success;
                yield break;
            }
            else
            {
                yield return RunStatus.Failure;
                yield break;
            }
        }

        public IEnumerable<RunStatus> ProcessAssertTimer(XmlNode myNode)
        {
            //if it doesn't exist then create an entry and return Success
            string nodeID = myNode.Attributes["id"].Value;
            if (!bot.myBehaviors.entryTime.ContainsKey(nodeID))
            {
                bot.myBehaviors.keepTime(nodeID, RunStatus.Success);
                bot.myBehaviors.activationTime(nodeID, RunStatus.Success);
                yield return RunStatus.Success;
                yield break;
            }

            //otherwise treat as a normal assert
            // reset after assert returns true
            RunStatus result = RunStatus.Failure;
            foreach (RunStatus myChildResult in ProcessAssert(myNode))
            {
                result = myChildResult;
                if (result != RunStatus.Running) break;
                yield return RunStatus.Running;
            }
            if (result == RunStatus.Success)
            {
                try
                {
                    bot.myBehaviors.entryTime.Remove(nodeID);
                    bot.myBehaviors.execTime.Remove(nodeID);
                }
                catch
                {
                }
                //Reset
                bot.myBehaviors.keepTime(nodeID, RunStatus.Success);
                bot.myBehaviors.activationTime(nodeID, RunStatus.Success);

            }
            yield return result;
            yield break;
        }

        // Assert based on guest object Eval
        // "cond" will be invoked on bot.guestEvalObject which should return a bool
        // which will determine the assert success or failure
        // fails if guest does not exist
        // the inner text defines the call parameters
        // One idiom would be to check if the guest object exists as a guard to
        //  a guest specific behavior subtree
        // One can also use the check for <selector>'s that depend on a particular
        //  guest object type
        public IEnumerable<RunStatus> ProcessAssertGuest(XmlNode myNode)
        {
            string condition = myNode.Attributes["cond"].Value;
            string parameters = myNode.InnerText;
            //if it doesn't exist then return failure
            if (bot.guestEvalObject == null)
            {
                yield return RunStatus.Failure;
                yield break;
            }
            RunStatus r = RunStatus.Failure;
            try
            {
                MethodInfo info = bot.guestEvalObject.GetType().GetMethod(condition);
                if (info != null)
                {
                    bool result = (bool)info.Invoke(bot.guestEvalObject, new object[] { parameters });
                    if (result) r = RunStatus.Success;
                }
            }
            catch (Exception e)
            {
                Console.WriteLine("Error ProcessAssertGuest '{0}':{1}", condition, e.Message);
                r = RunStatus.Failure;
            }

            yield return r;
            yield break;
        }

        public IEnumerable<RunStatus> ProcessTaskGuest(XmlNode myNode)
        {
            string condition = myNode.Attributes["call"].Value;
            string parameters = myNode.InnerText;
            //if it doesn't exist then return  failure(success would also make sense)
            if (bot.guestEvalObject == null)
            {
                yield return RunStatus.Failure;
                yield break;
            }
            RunStatus r = RunStatus.Failure;
            try
            {
                MethodInfo info = bot.guestEvalObject.GetType().GetMethod(condition);
                if (info != null)
                {
                    bool result = (bool)info.Invoke(bot.guestEvalObject, new object[] { parameters });
                    if (result) r = RunStatus.Success;
                }
            }
            catch (Exception e)
            {
                Console.WriteLine("Error ProcessAssertGuest '{0}':{1}", condition, e.Message);
                r= RunStatus.Failure;
            }

            yield return r;
            yield break;
        }


        //CHAT: chat controlled by the behavior system
        public IEnumerable<RunStatus> ProcessChat(XmlNode myNode)
        {
            string sentStr = myNode.InnerXml;
            string graphName = "*";
            try
            {
                if (myNode.Attributes["graph"] != null)
                {
                    graphName = myNode.Attributes["graph"].Value;
                }
            }
            catch (Exception e)
            {
                graphName = "*";
            }


            bot.lastBehaviorChatInput = "";
            bot.lastBehaviorChatOutput = "";
            if (bot.chatInputQueue.Count == 0)
            {
                yield return RunStatus.Success;
                yield break;
            }
            RunStatus rs = RunStatus.Failure;
            try
            {
                if (bot.chatInputQueue.Count > 0)
                {
                    bot.lastBehaviorChatInput = bot.chatInputQueue.Dequeue();
                    sentStr += bot.lastBehaviorChatInput;
                }
                Request r = new Request(sentStr, bot.lastBehaviorUser, bot);
                Result res = bot.Chat(r, graphName);
                //bot.lastBehaviorChatOutput=res.Output;
                bot.lastBehaviorChatOutput = "";
                bot.postOutput(res.Output);
                if (res.isValidOutput)
                {
                    rs = RunStatus.Success;
                }
                else
                {
                    rs = RunStatus.Failure;
                }
            }
            catch (Exception e)
            {
                Console.WriteLine("Error ProcessChat '{0}' '{1}':{2}", sentStr, bot.lastBehaviorChatInput, e.Message);
                rs = RunStatus.Failure;
            }
            yield return rs;
            yield break;
        }

        // Propositional Reasoner interface
        // Assert to KB
        public IEnumerable<RunStatus> ProcessTellKB(XmlNode myNode)
        {
            RunStatus rs = RunStatus.Failure;
            string sentStr = myNode.InnerXml;
            try
            {
                sentStr = sentStr.Replace("implies", "=>");
                sentStr = sentStr.Replace(" imp ", "=> ");
                sentStr = sentStr.Replace("equiv", "<=>");

                bot.myKB.Tell(sentStr);
                rs = RunStatus.Success;
            }
            catch (Exception e)
            {
                Console.WriteLine("Error ProcessTellKB '{0}':{1}", sentStr, e.Message);
                rs = RunStatus.Failure;
            }
            yield return rs;
            yield break;
        }


        public IEnumerable<RunStatus> ProcessTellBaseKB(XmlNode myNode)
        {
            RunStatus rs = RunStatus.Failure;
            string sentStr = myNode.InnerXml;
            try
            {
                sentStr = sentStr.Replace("implies", "=>");
                sentStr = sentStr.Replace(" imp ", "=> ");
                sentStr = sentStr.Replace("equiv", "<=>");

                bot.myBaseKB.Tell(sentStr);
                rs = RunStatus.Success;
            }
            catch (Exception e)
            {
                Console.WriteLine("Error ProcessTellBaseKB '{0}':{1}", sentStr, e.Message);
                rs = RunStatus.Failure;
            }
            yield return rs;
            yield break;
        }
        public IEnumerable<RunStatus> ProcessTellKBOCC(XmlNode myNode)
        {
            RunStatus rs = RunStatus.Failure;
            try
            {
                addOCCLogicForInteraction(bot.myBaseKB, myNode.InnerXml);
                rs = RunStatus.Success;
            }
            catch (Exception e)
            {
                Console.WriteLine("Error ProcessTellKBOCC '{0}':{1}", myNode.InnerXml, e.Message);
                rs = RunStatus.Failure;
            }
            yield return rs;
            yield break;
        }
        // 
        public IEnumerable<RunStatus> ProcessClearKB(XmlNode myNode)
        {
            RunStatus rs = RunStatus.Failure;
            try
            {
                bot.myKB = new KnowledgeBase();
                rs = RunStatus.Success;
            }
            catch (Exception e)
            {
                Console.WriteLine("Error ProcessClearKB '{0}':{1}", myNode.InnerXml, e.Message);
                rs = RunStatus.Failure;
            }
            yield return rs;
            yield break;
        }
        public IEnumerable<RunStatus> ProcessClearBaseKB(XmlNode myNode)
        {
            RunStatus rs = RunStatus.Failure;
            try
            {
                bot.myBaseKB = new KnowledgeBase();
                rs = RunStatus.Success;
            }
            catch (Exception e)
            {
                Console.WriteLine("Error ProcessClearBaseKB '{0}':{1}", myNode.InnerXml, e.Message);
                rs = RunStatus.Failure;
            }
            yield return rs;
            yield break;
        }

        // Process KB
        public double trialsMemory = 2048;

        public IEnumerable<RunStatus> ProcessKBModel(XmlNode myNode)
        {
            RunStatus rs = RunStatus.Failure;
            Console.WriteLine("*** PROCESSING KB ***");
            Model MiniModel = new Model();
            Model walkModel = null;
            MiniSatCSSolver myMiniSolver = new MiniSatCSSolver();
            string myReport = "";
            string miniPostives = "";
            string walkPostives = "";
            try
            {
                // Afraid that if you just put an AND between the two kb sentences
                // then it will only have two mega clauses, and no read gradient info

                KnowledgeBase totalKB = new KnowledgeBase();
                foreach (Sentence s in bot.myBaseKB.Sentences)
                {
                    totalKB.Tell(s.ToString());
                }
                foreach (Sentence s in bot.myKB.Sentences)
                {
                    totalKB.Tell(s.ToString());
                }
                string totalKBs = totalKB.AsSentence().ToString();
                Console.WriteLine("*** Built KB ***");

                // Lets try minisat ...
                var sset = (Sentence)new PEParser().Parse(totalKBs);
                var transformer = new CNFTransformer();
                var clauseGatherer = new CNFClauseGatherer();
                IList<Sentence> clauses = clauseGatherer.GetClausesFrom(transformer.Transform(sset)).ToList();
                string AIMACNF = "";
                foreach (Sentence clause in clauses)
                {
                    AIMACNF += clause.ToString() + "\n";
                }
                //Model MiniModel = new Model();
                //MiniSatCSSolver myMiniSolver = new MiniSatCSSolver();

                Console.WriteLine("*** MiniSatCSSolver ***");

                string miniLog = String.Format(@"./MiniTotalKB.txt");
                System.IO.File.WriteAllText(miniLog, totalKBs);
                miniLog = String.Format(@"./AIMACNF.txt");
                System.IO.File.WriteAllText(miniLog, AIMACNF);
                string tempDIMACSproblem = myMiniSolver.translator.AIMAToDIMACS(AIMACNF);
                miniLog = String.Format(@"./tempDIMACSproblem.txt");
                System.IO.File.WriteAllText(miniLog, tempDIMACSproblem);

                string answer = myMiniSolver.solveIt(AIMACNF, out MiniModel);
                Console.WriteLine("*** MiniSatCSSolver ***");
                //Console.WriteLine("AIMACNF:{0}\n",AIMACNF);
                //Console.WriteLine("MiniANS:{0}\n", answer);

                miniLog = String.Format(@"./Minianswer.txt");
                System.IO.File.WriteAllText(miniLog, answer);
                miniLog = String.Format(@"./Minireport.txt");
                System.IO.File.WriteAllText(miniLog, myMiniSolver.solverReport);
                if ((myMiniSolver.wasSAT) && (MiniModel != null))
                {
                    bot.myModel = MiniModel;
                    bot.myActiveModel = MiniModel;
                    miniPostives = MiniModel.strPositives();
                    myReport = answer;
                    bot.myChemistry.m_cBus.setHash("foundSATModel", "True");
                }
                else
                {
                    bot.myChemistry.m_cBus.setHash("foundSATModel", "False");
                }

                //
                if (trialsMemory < 512) trialsMemory = 512;
                if (trialsMemory > 10240) trialsMemory = 10240;

                 walkModel = bot.myWalkSAT.FindModelFor(totalKBs, (int)(trialsMemory * 1.5), 0.5, bot.myActiveModel);
                // bot.myModel = bot.myWalkSAT.FindModelFor(totalKBs, (int)(trialsMemory * 1.5), 0.5, MiniModel);
                trialsMemory = Ema(bot.myWalkSAT.trials, trialsMemory, 5);

                myReport += bot.myWalkSAT.ExamineClauseStatistics(0.05);
            }
            catch (Exception e)
            {
                Console.WriteLine("Error ProcessKBModel '{0}':{1}\n{2}", myNode.InnerXml, e.Message, e.StackTrace);
                rs = RunStatus.Failure;
            }


                if ((walkModel == null) && (myMiniSolver.wasSAT == false))
                {
                    // We fail if we are unsat
                    Console.WriteLine("NO SAT MODEL FOUND");
                    if (bot.myActiveModel == null)
                    {
                        bot.myChemistry.m_cBus.setHash("activeModel", "(NOT sat)");
                    }
                    try
                    {
                        string logFileName = String.Format(@"./unsatStats.txt");
                        System.IO.File.WriteAllText(logFileName, myReport);
                    }
                    catch
                    {
                        Console.WriteLine("ERR: CANNOT WRITE UNSAT TRACE TO FILE");
                    }

                    yield return RunStatus.Failure;
                    yield break;
                }
            try
            {
                if (walkModel != null)
                {
                    bot.myModel = walkModel;
                    walkPostives = walkModel.strPositives();
                }

                // update active model
                bot.myActiveModel = bot.myModel;
                string totalModel = bot.myModel.AsSentenceString();
                string postPositives = bot.myModel.strPositives();
                bot.myPositiveSATModleString = postPositives;
                //bot.myChemistry.m_cBus.setHash("activeModel", totalModel);
                bot.myChemistry.m_cBus.setHash("activeModel", postPositives);
                Console.WriteLine("SAT MODEL FOUND:{0}", postPositives);
                try
                {
                    satCount++;
                    satCount = satCount % satMod;
                    string logFileName = String.Format(@"./lastSatModel{0}.txt", satCount);
                    System.IO.File.WriteAllText(logFileName, postPositives + "\n" + totalModel + "\n" + myReport);
                }
                catch
                {
                    Console.WriteLine("ERR: CANNOT WRITE SAT MODEL TO FILE");
                }

                rs = RunStatus.Success;
            }
            catch (Exception e)
            {
                Console.WriteLine("Error ProcessKBModel '{0}':{1}\n{2}", myNode.InnerXml, e.Message, e.StackTrace);
                rs = RunStatus.Failure;
            }
            yield return rs;
            yield break;

        }

        public IEnumerable<RunStatus> ProcessSequence(XmlNode myNode)
        {
            // Execute children in order until one returns false
            foreach (XmlNode childNode in myNode.ChildNodes)
            {
                RunStatus childResult = RunStatus.Failure;
                foreach (RunStatus myChildResult in processNode(childNode))
                {
                    childResult = myChildResult;
                    if (childResult != RunStatus.Running) break;
                    yield return RunStatus.Running;
                }

                if (isAnAssert(childNode.Name))
                {
                    // Except for Asserts
                    if (childResult == RunStatus.Failure)
                    {
                        yield return RunStatus.Failure;
                        yield break;
                    }
                }
                else
                {
                    // Normal processing
                    if (childResult == RunStatus.Failure)
                    {
                        yield return RunStatus.Failure;
                        yield break;
                    }
                }
            }
           yield return RunStatus.Success;
           yield break;
        }

        public IEnumerable<RunStatus> ProcessSelector(XmlNode myNode)
        {
            // Execute children until one returns true
            foreach (XmlNode childNode in myNode.ChildNodes)
            {
                RunStatus childResult = RunStatus.Failure;
                foreach (RunStatus myChildResult in processNode(childNode))
                {
                    childResult = myChildResult;
                    if (childResult != RunStatus.Running) break;
                    yield return RunStatus.Running;
                }
                // Except for Asserts
                if (isAnAssert(childNode.Name))
                {
                    if (childResult == RunStatus.Failure)
                    {
                        yield return RunStatus.Failure;
                        yield break;
                    }
                }
                else
                {
                    // Normal processing
                    if (childResult == RunStatus.Success)
                    {
                        yield return RunStatus.Success;
                        yield break;
                    }
                }
            }
            yield return RunStatus.Failure;
            yield break;

        }

        public IEnumerable<RunStatus> ProcessRandomSelector(XmlNode myNode)
        {
            // Pick a random child with equal weighting
            //int selected = rgen.Next(myNode.ChildNodes.Count);
            //XmlNode childNode = myNode.ChildNodes[selected];
            XmlNode childNode = this.bot.myRandMem.selectOneXML(myNode);
            foreach (RunStatus childResult in processNode(childNode))
            {
            yield return childResult;
            }
        }

        public IEnumerable<RunStatus> ProcessWeightedSelector(XmlNode myNode)
        {
            double totalWeight = 0;
            // Does a basic roulette wheel selection of the child nodes
            //  each of which can have a "weight" attribute
            //  otherwise they are evenly weighted

            // Another variant would could use the node ID to look up the weight
            // so the results could be dynamic
            foreach (XmlNode childNode in myNode.ChildNodes)
            {
                try
                {
                    totalWeight += double.Parse(childNode.Attributes["weight"].Value);
                }
                catch
                {
                    totalWeight += 1 / (1 + myNode.ChildNodes.Count);
                }
            }

            // Pick a random child with equal weighting
            double selectedV = rgen.NextDouble() * totalWeight;
            double accum = 0;
            // Execute children in order until one returns false
            foreach (XmlNode childNode in myNode.ChildNodes)
            {
                try
                {
                    accum += double.Parse(childNode.Attributes["weight"].Value);
                }
                catch
                {
                    accum += 1 / (1 + myNode.ChildNodes.Count);
                }
                if (accum >= selectedV)
                {
                    RunStatus childResult = RunStatus.Failure;
                    foreach (RunStatus myChildResult in processNode(childNode))
                    {
                        childResult = myChildResult;
                        if (childResult != RunStatus.Running) break;
                        yield return RunStatus.Running;
                    }
                    yield return childResult;
                    yield break;
                }
            }

            yield return RunStatus.Failure;
            yield break;
        }

        public IEnumerable<RunStatus> ProcessParallel(XmlNode myNode)
        {
            // Execute All Children
            foreach (XmlNode childNode in myNode.ChildNodes)
            {
                RunStatus childResult = RunStatus.Failure;
                foreach (RunStatus myChildResult in processNode(childNode))
                {
                    childResult = myChildResult;
                    if (childResult != RunStatus.Running) break;
                    yield return RunStatus.Running;
                }
                // Except for Asserts
                if (isAnAssert(childNode.Name))
                {
                    if (childResult == RunStatus.Failure)
                    {
                        yield return RunStatus.Failure;
                        yield break;
                    }
                }
                else
                {
                    // Normal processing (We dont care)
                    if (childResult == RunStatus.Success)
                    {
                        // return RunStatus.Success;
                    }
                }
            }
            yield return RunStatus.Success;
            yield break;
        }

        public IEnumerable<RunStatus> ProcessLoop(XmlNode myNode)
        {
            // you can specify the maximum number of times to loop
            // after which it will return SUCCESS
            //  since we tried N times and none of the asserts failed
            //  the maxloop is the final success condition 
            //   (we executed the loop N times without FAIL so it is a SUCCESS)
            // Allows pervention of infinite loops

            Int32 maxloop = 0;
            Int32 loopCount = 0;
            try
            {
                maxloop = Int32.Parse(myNode.Attributes["maxloop"].Value);
            }
            catch
            {
            }
            // We just keep looping until either an assert fails 
            //  or maxloops occur if specified
            while (true)
            {
                    if ((maxloop > 0) && (loopCount >= maxloop))
                    {
                        yield return RunStatus.Success;
                        yield break;
                    }
                    // Execute All Children
                    foreach (XmlNode childNode in myNode.ChildNodes)
                    {
                        RunStatus childResult = RunStatus.Failure;
                        foreach (RunStatus myChildResult in processNode(childNode))
                        {
                            childResult = myChildResult;
                            if (childResult != RunStatus.Running) break;
                            yield return RunStatus.Running;
                        }
                        // Except for Asserts
                        if (isAnAssert(childNode.Name))
                        {
                            if (childResult == RunStatus.Failure)
                            {
                                yield return RunStatus.Failure;
                                yield break;
                            }
                        }
                        else
                        {
                            // Normal processing (We dont care)
                            if (childResult == RunStatus.Success)
                            {
                                // return RunStatus.Success;
                            }
                        }
                    }
                    long timeout = Environment.TickCount + tickRate;
                    while (Environment.TickCount < timeout)
                    {
                        yield return RunStatus.Running;
                        //Thread.Sleep(tickRate);
                    }

                try
                {
                }
                catch
                { 
                    //yield return RunStatus.Failure;
                    yield break;
                }
                loopCount++;
            }
            //return RunStatus.Failure;
        }

        public IEnumerable<RunStatus> ProcessLoopUntil(XmlNode myNode)
        {
            // you can specify the maximum number of times to loop
            // after which it will return FAILURE
            //   since we would have left if any child was successful
            //   after N trials nothing worked so we should return FAIL
            // Allows pervention of infinite loops

            Int32 maxloop = 0;
            Int32 loopCount = 0;
            RunStatus finalResult = RunStatus.Non;

            try
            {
                maxloop = Int32.Parse(myNode.Attributes["maxloop"].Value);
            }
            catch
            {
            }
            // We go through all chil
            while (true)
            {
                if ((maxloop > 0) && (loopCount >= maxloop))
                {
                    yield return RunStatus.Failure;
                    yield break;
                }
                    // Execute All Children
                    foreach (XmlNode childNode in myNode.ChildNodes)
                    {
                        RunStatus childResult = RunStatus.Failure;
                        foreach (RunStatus myChildResult in processNode(childNode))
                        {
                            childResult = myChildResult;
                            if (childResult != RunStatus.Running) break;
                            yield return RunStatus.Running;
                        }
                        // Except for Asserts
                        if (isAnAssert(childNode.Name))
                        {
                            if (childResult == RunStatus.Failure)
                            {
                                finalResult= RunStatus.Failure;
                            }
                        }
                        else
                        {
                            // Normal processing Return on any normal child success
                            if (childResult == RunStatus.Success)
                            {
                                finalResult = RunStatus.Success;
                            }
                        }
                        if (finalResult != RunStatus.Non) break;
                    }
                    long timeout = Environment.TickCount + tickRate;
                    while (Environment.TickCount < timeout)
                    {
                        yield return RunStatus.Running;
                        //Thread.Sleep(tickRate);
                    }
                try
                {
                }
                catch
                {
                    finalResult = RunStatus.Failure;
                }
                if (finalResult != RunStatus.Non)
                {
                    yield return finalResult;
                    yield break;
                }
                loopCount++;
            }
            //return RunStatus.Failure;
        }
        public IEnumerable<RunStatus> ProcessTask(XmlNode myNode)
        {
            RunStatus result = RunStatus.Success;
            if (myNode == null)
            {
                yield return result;
                yield break;
            }

            try
            {
                if ((myNode.Attributes != null) && (myNode.Attributes.Count > 0))
                {
                    string returnV = myNode.Attributes["return"].Value;
                    if (returnV.ToLower() == "failure") { result = RunStatus.Failure; }
                    if (returnV.ToLower() == "success") { result = RunStatus.Success; }
                }
            }
            catch (Exception e)
            {
                result = RunStatus.Success;

            }
            yield return RunStatus.Running;
            try
            {
                XmlNode resultTemplateNode = AIMLTagHandler.getNode("<template>" + myNode.InnerXml + "</template>");
                bot.evalTemplateNode(resultTemplateNode);
                //bot.evalTemplateNode(templateNode);
            }
            catch (Exception e)
            {
                Console.WriteLine("ERR: ProcessTask");
                Console.WriteLine("ERR:" + e.Message);
                Console.WriteLine("ERR:" + e.StackTrace);
                Console.WriteLine("ERR XML:" + myNode.OuterXml);
                //result = RunStatus.Failure;
            }
            yield return result;
            yield break;

            // Works like OnEntry/OnExit in SCXML
            foreach (XmlNode templateNode in myNode.ChildNodes)
            {
                try
                {
                    XmlNode resultTemplateNode = AIMLTagHandler.getNode("<template>" + templateNode.InnerXml + "</template>");
                    bot.evalTemplateNode(resultTemplateNode);
                    //bot.evalTemplateNode(templateNode);
                }
                catch (Exception e)
                {
                    Console.WriteLine("ERR: ProcessTask");
                    Console.WriteLine("ERR:" + e.Message);
                    Console.WriteLine("ERR:" + e.StackTrace);
                    Console.WriteLine("ERR XML:" + templateNode.OuterXml);
                    //result = RunStatus.Failure;
                }
            }

            yield return result;
            yield break;
        }


        public IEnumerable<RunStatus> ProcessStateAiml(XmlNode myNode)
        {
            // Takes the inner text as AIML category definitions, to create new responses
            // So the system can change its verbal response behavior based on
            // its behavior tree
            RunStatus result = RunStatus.Success;

            try
            {
                if (myNode.Attributes["return"] != null)
                {

                    string returnV = myNode.Attributes["return"].Value;
                    if (returnV.ToLower() == "failure") { result = RunStatus.Failure; }
                    if (returnV.ToLower() == "success") { result = RunStatus.Success; }
                }
            }
            catch
            {
                result = RunStatus.Success;

            }
            string graphName = "*";
            try
            {
                if (myNode.Attributes["graph"] != null)
                {
                    graphName = myNode.Attributes["graph"].Value;
                }
            }
            catch (Exception e)
            {
                graphName = "*";
            }
            try
            {

                //XmlNode resultTemplateNode = AIMLTagHandler.getNode("<template>" + myNode.InnerXml + "</template>");
                XmlDocument resultAIMLDoc = new XmlDocument();
                resultAIMLDoc.LoadXml("<aiml graph='" + graphName + "'>" + myNode.InnerXml + "</aiml>");
                bot.loadAIMLFromXML(resultAIMLDoc, "dynamic_code");
                //bot.evalTemplateNode(templateNode);
            }
            catch (Exception e)
            {
                Console.WriteLine("ERR: ProcessStateAiml");
                Console.WriteLine("ERR:" + e.Message);
                Console.WriteLine("ERR:" + e.StackTrace);
                Console.WriteLine("ERR XML:" + myNode.OuterXml);
                //result = RunStatus.Failure;
            }
            yield return result;
            yield break;

            // Works like OnEntry/OnExit in SCXML
            foreach (XmlNode templateNode in myNode.ChildNodes)
            {
                try
                {
                    XmlNode resultTemplateNode = AIMLTagHandler.getNode("<template>" + templateNode.InnerXml + "</template>");
                    bot.evalTemplateNode(resultTemplateNode);
                    //bot.evalTemplateNode(templateNode);
                }
                catch (Exception e)
                {
                    Console.WriteLine("ERR: ProcessTask");
                    Console.WriteLine("ERR:" + e.Message);
                    Console.WriteLine("ERR:" + e.StackTrace);
                    Console.WriteLine("ERR XML:" + templateNode.OuterXml);
                    //result = RunStatus.Failure;
                }
            }

            yield return result;
            yield break;
        }
        #endregion

        public void addOCCLogicForInteraction(KnowledgeBase kb, string target)
        {

            kb.Tell(string.Format("((NOT selfLikeUser) => (NOT selfWant{0}))", target));
            kb.Tell(string.Format("((NOT selfLikeUser) => (badUserBehavior{0}))", target));
            kb.Tell(string.Format("((NOT selfWant{0}) <=> undesirable{0})", target));
            kb.Tell(string.Format("(selfWant{0} => desirable{0})", target));
            kb.Tell(string.Format("(userWants{0} => expect{0})", target));
            kb.Tell(string.Format("(badUserBehavior{0} <=> (NOT goodUserBehavior{0}))", target));
            kb.Tell(string.Format("(badSelfBehavior{0} <=> (NOT goodSelfBehavior{0}))", target));
            kb.Tell(string.Format("(undesirable{0} <=> (NOT desirable{0}))", target));
            kb.Tell(string.Format("((userWants{0} AND (selfLikeUser AND event{0})) <=> happyForUserFor{0})", target));
            kb.Tell(string.Format("((userWants{0} AND (selfLikeUser AND (NOT event{0}))) <=> sorryForUserForNot{0})", target));
            kb.Tell(string.Format("( (userWants{0} AND ((NOT selfLikeUser) AND event{0})) <=> resentmentForUserFor{0})", target));
            kb.Tell(string.Format("( (userWants{0} AND ((NOT selfLikeUser) AND (NOT event{0})) ) <=> gloatingForUserForNot{0})", target));

            kb.Tell(string.Format("((goodSelfBehavior{0} AND selfPerforms{0}) <=> prideSelfIn{0})", target));
            kb.Tell(string.Format("((badSelfBehavior{0} AND selfPerforms{0} ) <=> shameSelfIn{0})", target));
            kb.Tell(string.Format("((goodUserBehavior{0} AND userPerforms{0} ) <=> admireUserFor{0})", target));
            kb.Tell(string.Format("((badUserBehavior{0} AND userPerforms{0})  <=> reproachUserFor{0})", target));

            kb.Tell(string.Format("((prideSelfIn{0} AND selfFeelJoyIn{0} ) <=> gradificationFor{0})", target));
            kb.Tell(string.Format("((shameSelfIn{0} AND selfFeelDistressFor{0})  <=> remorseFor{0})", target));
            kb.Tell(string.Format("((admireUserFor{0} AND selfFeelJoyIn{0})  <=> gratitudeFor{0})", target));
            kb.Tell(string.Format("((reproachUserFor{0} AND selfFeelDistressFor{0})  <=> angerFor{0})", target));

            kb.Tell(string.Format("((event{0} AND desirable{0}) <=> selfFeelJoyIn{0})", target));
            kb.Tell(string.Format("((event{0} AND undesirable{0}) <=> selfFeelDistressFor{0})", target));
            kb.Tell(string.Format("((expect{0} AND desirable{0}) <=> selfFeelHopeFor{0})", target));
            kb.Tell(string.Format("((expect{0} AND undesirable{0}) <=> selfFeelFearOf{0})", target));

            kb.Tell(string.Format("( ((NOT event{0}) AND selfFeelHopeFor{0}) <=> selfDisappointmentForNot{0})", target));
            kb.Tell(string.Format("( ((NOT event{0}) AND selfFeelFearOf{0}) <=> selfFeelReliefForNot{0})", target));


            kb.Tell(string.Format("((event{0} AND selfFeelHopeFor{0}) <=> selfFeelSatisfactionFor{0})", target));
            kb.Tell(string.Format("((event{0} AND selfFeelFearOf{0}) <=> selfFeelFearConfirmedOf{0})", target));

            kb.Tell(string.Format("((selfWant{0} AND badSelfBehavior{0}) <=> selfFeelNaughtyAbout{0})", target));

            // Having a specific emotion about something implies having the generic emotion
            kb.Tell(string.Format("((happyForUserFor{0}) => selfFeelHappyForUser)", target));
            kb.Tell(string.Format("((sorryForUserForNot{0}) => selfFeelSorryForUser)", target));
            kb.Tell(string.Format("((resentmentForUserFor{0}) => selfFeelResentmentForUser)", target));
            kb.Tell(string.Format("((gloatingForUserForNot{0}) => selfFeelGloatingForUser)", target));
            kb.Tell(string.Format("((prideSelfIn{0}) => selfFeelPride)", target));
            kb.Tell(string.Format("((shameSelfIn{0}) => selfFeelShame)", target));
            kb.Tell(string.Format("((admireUserFor{0}) => selfAdmireUser)", target));
            kb.Tell(string.Format("((reproachUserFor{0}) => selfReproachUser)", target));

            kb.Tell(string.Format("((gradificationFor{0}) => selfFeelGradification)", target));
            kb.Tell(string.Format("((remorseFor{0}) => selfFeelRemorse)", target));
            kb.Tell(string.Format("((gratitudeFor{0}) => selfFeelGratitude)", target));
            kb.Tell(string.Format("((angerFor{0}) => selfFeelAngry)", target));

            kb.Tell(string.Format("((selfFeelJoyIn{0}) => selfFeelHappy)", target));
            kb.Tell(string.Format("((selfFeelDistressFor{0}) => selfFeelDistress)", target));
            kb.Tell(string.Format("((selfFeelHopeFor{0}) => selfFeelHope)", target));
            kb.Tell(string.Format("((selfFeelFearOf{0}) => selfFeelFear)", target));
            kb.Tell(string.Format("((selfFeelFearConfirmedOf{0}) => selfFeelFear)", target));

            kb.Tell(string.Format("((selfDisappointmentForNot{0}) => selfFeelDisappointed)", target));
            kb.Tell(string.Format("((selfFeelReliefForNot{0}) => selfFeelRelief)", target));

            kb.Tell(string.Format("((selfFeelSatisfactionFor{0}) => selfFeelSatisfaction)", target));
            kb.Tell(string.Format("((selfFeelFearConfirmedOf{0}) => selfFeelFearConfirmed)", target));

            kb.Tell(string.Format("((selfFeelNaughtyAbout{0}) => selfFeelNaughty)", target));
        }
    }
}
