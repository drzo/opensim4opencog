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
using AltAIMLParser;
using MiniSatCS;
using System.Reflection;
using RTParser;
using VDS.RDF.Parsing;
using LogicalParticleFilter1;
using LAIR.ResourceAPIs.WordNet;
using LAIR.Collections.Generic;
using CAMeRAVUEmotion;
using MushDLR223.Utilities;

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
        public SymbolicParticleFilter ourFilter = new SymbolicParticleFilter();


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
        public IEnumerable<RunStatus> evalBehaviorXml(string behaviorDef)
        {
            XmlDocument evalDoc = new XmlDocument ();
            try
            {

                behaviorDef = behaviorDef.Replace("&gt;", ">");
                behaviorDef = behaviorDef.Replace("&lt;", "<");
                evalDoc.LoadXml(behaviorDef);
            }
            catch (Exception e)
            {
                Console.WriteLine("ERR:" + e.Message);
                Console.WriteLine("ERR:" + e.StackTrace);
                Console.WriteLine("ERR XML:" + behaviorDef);
            }
            foreach (RunStatus myChildResult in runSubTree(evalDoc))
            {
                RunStatus childResult = RunStatus.Failure;
                childResult = myChildResult;
                if (childResult != RunStatus.Running) break;
                yield return RunStatus.Running;
            }
            yield return RunStatus.Success;
            yield break;
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
                //Sequence semantics
                if (childResult == RunStatus.Failure)
                {
                    yield return RunStatus.Failure;
                    yield break;
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
                    || (nodeName.ToLower() == "assertmenu")
                    || (nodeName.ToLower() == "assertprolog")
                    || (nodeName.ToLower().StartsWith ("assert"))
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
            if (StaticXMLUtils.IsBlank(myNode))
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
                            //Console.WriteLine("  yield {0} {1} = {2}", myNode.Name.ToLower(), nodeID, result.ToString());
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
//Siprolog interface
                    case "clearprologmt":
                        foreach (RunStatus result in ProcessClearPrologMt(myNode))
                        {
                            myResult = result;
                            bot.myBehaviors.runState[nodeID] = myResult;
                            if (myResult != RunStatus.Running) break;
                            yield return result;
                        }
                        break;
                    case "connectmt":
                        foreach (RunStatus result in ProcessConnectMt(myNode))
                        {
                            myResult = result;
                            bot.myBehaviors.runState[nodeID] = myResult;
                            if (myResult != RunStatus.Running) break;
                            yield return result;
                        }
                        break;
                    case "tellprologtmt":
                        foreach (RunStatus result in ProcessTellPrologMt(myNode))
                        {
                            myResult = result;
                            bot.myBehaviors.runState[nodeID] = myResult;
                            if (myResult != RunStatus.Running) break;
                            yield return result;
                        }
                        break;
                    case "loadke":
                        foreach (RunStatus result in ProcessLoadKEKB(myNode))
                        {
                            myResult = result;
                            bot.myBehaviors.runState[nodeID] = myResult;
                            if (myResult != RunStatus.Running) break;
                            yield return result;
                        }
                        break;
                    case "insertmt":
                        foreach (RunStatus result in ProcessInsertMt(myNode))
                        {
                            myResult = result;
                            bot.myBehaviors.runState[nodeID] = myResult;
                            if (myResult != RunStatus.Running) break;
                            yield return result;
                        }
                        break;
                    case "appendmt":
                        foreach (RunStatus result in ProcessAppendMt(myNode))
                        {
                            myResult = result;
                            bot.myBehaviors.runState[nodeID] = myResult;
                            if (myResult != RunStatus.Running) break;
                            yield return result;
                        }
                        break;
                    case "assertprolog":
                        foreach (RunStatus result in ProcessAssertProlog(myNode))
                        {
                            myResult = result;
                            bot.myBehaviors.runState[nodeID] = myResult;
                            if (myResult != RunStatus.Running) break;
                            yield return result;
                        }
                        break;

                    case "evalprologmacro":
                        foreach (RunStatus result in ProcessEvalPrologMacro(myNode))
                        {
                            myResult = result;
                            bot.myBehaviors.runState[nodeID] = myResult;
                            if (myResult != RunStatus.Running) break;
                            yield return result;
                        }
                        break;
                    //Particle filter interface
                    case "definestate":
                        foreach (RunStatus result in ProcessDefineState(myNode))
                        {
                            myResult = result;
                            bot.myBehaviors.runState[nodeID] = myResult;
                            if (myResult != RunStatus.Running) break;
                            yield return result;
                        }
                        break;
                    case "stateconstraintset":
                        foreach (RunStatus result in ProcessConstraintSet(myNode))
                        {
                            myResult = result;
                            bot.myBehaviors.runState[nodeID] = myResult;
                            if (myResult != RunStatus.Running) break;
                            yield return result;
                        }
                        break;
                    case "statesense":
                        foreach (RunStatus result in ProcessStateSense(myNode))
                        {
                            myResult = result;
                            bot.myBehaviors.runState[nodeID] = myResult;
                            if (myResult != RunStatus.Running) break;
                            yield return result;
                        }
                        break;
                    case "stateacttransition":
                        foreach (RunStatus result in ProcessStateActTransition(myNode))
                        {
                            myResult = result;
                            bot.myBehaviors.runState[nodeID] = myResult;
                            if (myResult != RunStatus.Running) break;
                            yield return result;
                        }
                        break;
                    case "quickfilter":
                        foreach (RunStatus result in ProcessQuickFilter(myNode))
                        {
                            myResult = result;
                            bot.myBehaviors.runState[nodeID] = myResult;
                            if (myResult != RunStatus.Running) break;
                            yield return result;
                        }
                        break;
                    case "meanparticle":
                        foreach (RunStatus result in ProcessMeanParticle(myNode))
                        {
                            myResult = result;
                            bot.myBehaviors.runState[nodeID] = myResult;
                            if (myResult != RunStatus.Running) break;
                            yield return result;
                        }
                        break;
                    case "genfilterfrommt":
                        foreach (RunStatus result in ProcessGenFilterFromMt(myNode))
                        {
                            myResult = result;
                            bot.myBehaviors.runState[nodeID] = myResult;
                            if (myResult != RunStatus.Running) break;
                            yield return result;
                        }
                        break;
// Chatmapper
                    case "loadchatmapper":
                        foreach (RunStatus result in ProcessLoadChatMapper(myNode))
                        {
                            myResult = result;
                            bot.myBehaviors.runState[nodeID] = myResult;
                            if (myResult != RunStatus.Running) break;
                            yield return result;
                        }
                        break;
                    case "waitforchatinput":
                        foreach (RunStatus result in ProcessWaitForChatInput(myNode))
                        {
                            myResult = result;
                            bot.myBehaviors.runState[nodeID] = myResult;
                            if (myResult != RunStatus.Running) break;
                            yield return result;
                        }
                        break;
                    case "assertmenu":
                        foreach (RunStatus result in ProcessAssertMenu(myNode))
                        {
                            myResult = result;
                            bot.myBehaviors.runState[nodeID] = myResult;
                            if (myResult != RunStatus.Running) break;
                            yield return result;
                        }
                        break;

// Coppelia

                    case "coppeliaaction":
                        foreach (RunStatus result in ProcessCoppeliaAction(myNode))
                        {
                            myResult = result;
                            bot.myBehaviors.runState[nodeID] = myResult;
                            if (myResult != RunStatus.Running) break;
                            yield return result;
                        }
                        break;
                    case "coppeliaactionstate":
                        foreach (RunStatus result in ProcessCoppeliaActionState(myNode))
                        {
                            myResult = result;
                            bot.myBehaviors.runState[nodeID] = myResult;
                            if (myResult != RunStatus.Running) break;
                            yield return result;
                        }
                        break;
                    case "coppeliaagentfeature":
                        foreach (RunStatus result in ProcessCoppeliaAgentFeature(myNode))
                        {
                            myResult = result;
                            bot.myBehaviors.runState[nodeID] = myResult;
                            if (myResult != RunStatus.Running) break;
                            yield return result;
                        }
                        break;
                    case "coppeliaagentresponsiblebelief":
                        foreach (RunStatus result in ProcessCoppeliaAgentResponsibleBelief(myNode))
                        {
                            myResult = result;
                            bot.myBehaviors.runState[nodeID] = myResult;
                            if (myResult != RunStatus.Running) break;
                            yield return result;
                        }
                        break;
                    case "coppeliaanger":
                        foreach (RunStatus result in ProcessCoppeliaAnger(myNode))
                        {
                            myResult = result;
                            bot.myBehaviors.runState[nodeID] = myResult;
                            if (myResult != RunStatus.Running) break;
                            yield return result;
                        }
                        break;
                    case "coppeliaambition":
                        foreach (RunStatus result in ProcessCoppeliaAmbition(myNode))
                        {
                            myResult = result;
                            bot.myBehaviors.runState[nodeID] = myResult;
                            if (myResult != RunStatus.Running) break;
                            yield return result;
                        }
                        break;
                    case "coppeliadesired":
                        foreach (RunStatus result in ProcessCoppeliaDesired(myNode))
                        {
                            myResult = result;
                            bot.myBehaviors.runState[nodeID] = myResult;
                            if (myResult != RunStatus.Running) break;
                            yield return result;
                        }
                        break;
                    case "coppeliaactiontendancy":
                        foreach (RunStatus result in ProcessCoppeliaActionTendancy(myNode))
                        {
                            myResult = result;
                            bot.myBehaviors.runState[nodeID] = myResult;
                            if (myResult != RunStatus.Running) break;
                            yield return result;
                        }
                        break;

                    case "coppeliaemotion":
                        foreach (RunStatus result in ProcessCoppeliaEmotion(myNode))
                        {
                            myResult = result;
                            bot.myBehaviors.runState[nodeID] = myResult;
                            if (myResult != RunStatus.Running) break;
                            yield return result;
                        }
                        break;
                    case "coppeliaexpectedsatisfaction":
                        foreach (RunStatus result in ProcessCoppeliaExpectedSatisfaction(myNode))
                        {
                            myResult = result;
                            bot.myBehaviors.runState[nodeID] = myResult;
                            if (myResult != RunStatus.Running) break;
                            yield return result;
                        }
                        break;
                    case "coppeliafeatureemotionbelief":
                        foreach (RunStatus result in ProcessCoppeliaFeatureEmotionBelief(myNode))
                        {
                            myResult = result;
                            bot.myBehaviors.runState[nodeID] = myResult;
                            if (myResult != RunStatus.Running) break;
                            yield return result;
                        }
                        break;
                    case "coppeliafeaturebelief":
                        foreach (RunStatus result in ProcessCoppeliaFeatureBelief(myNode))
                        {
                            myResult = result;
                            bot.myBehaviors.runState[nodeID] = myResult;
                            if (myResult != RunStatus.Running) break;
                            yield return result;
                        }
                        break;
                    case "coppeliaperform":
                        foreach (RunStatus result in ProcessCoppeliaPerform(myNode))
                        {
                            myResult = result;
                            bot.myBehaviors.runState[nodeID] = myResult;
                            if (myResult != RunStatus.Running) break;
                            yield return result;
                        }
                        break;
                    case "coppeliapraiseworthy":
                        foreach (RunStatus result in ProcessCoppeliaPraiseworthy(myNode))
                        {
                            myResult = result;
                            bot.myBehaviors.runState[nodeID] = myResult;
                            if (myResult != RunStatus.Running) break;
                            yield return result;
                        }
                        break;
                    case "coppeliarelation":
                        foreach (RunStatus result in ProcessCoppeliaRelation(myNode))
                        {
                            myResult = result;
                            bot.myBehaviors.runState[nodeID] = myResult;
                            if (myResult != RunStatus.Running) break;
                            yield return result;
                        }
                        break;
                    case "assertcoppeila":
                        foreach (RunStatus result in ProcessAssertCoppelia(myNode))
                        {
                            myResult = result;
                            bot.myBehaviors.runState[nodeID] = myResult;
                            if (myResult != RunStatus.Running) break;
                            yield return result;
                        }
                        break;
                    case "coppeliaresponse":
                        foreach (RunStatus result in ProcessCoppeliaResponse(myNode))
                        {
                            myResult = result;
                            bot.myBehaviors.runState[nodeID] = myResult;
                            if (myResult != RunStatus.Running) break;
                            yield return result;
                        }
                        break;
                    case "coppeliastatelikelihood":
                        foreach (RunStatus result in ProcessCoppeliaStateLikelihood(myNode))
                        {
                            myResult = result;
                            bot.myBehaviors.runState[nodeID] = myResult;
                            if (myResult != RunStatus.Running) break;
                            yield return result;
                        }
                        break;
                    case "coppeliastate":
                        foreach (RunStatus result in ProcessCoppeliaState(myNode))
                        {
                            myResult = result;
                            bot.myBehaviors.runState[nodeID] = myResult;
                            if (myResult != RunStatus.Running) break;
                            yield return result;
                        }
                        break;
                    case "coppeliastatestate":
                        foreach (RunStatus result in ProcessCoppeliaStateState(myNode))
                        {
                            myResult = result;
                            bot.myBehaviors.runState[nodeID] = myResult;
                            if (myResult != RunStatus.Running) break;
                            yield return result;
                        }
                        break;

                    case "coppeliamoral":
                        foreach (RunStatus result in ProcessCoppeliaMoral(myNode))
                        {
                            myResult = result;
                            bot.myBehaviors.runState[nodeID] = myResult;
                            if (myResult != RunStatus.Running) break;
                            yield return result;
                        }
                        break;
                    case "coppeliamoralambitionl":
                        foreach (RunStatus result in ProcessCoppeliaMoralAmbition(myNode))
                        {
                            myResult = result;
                            bot.myBehaviors.runState[nodeID] = myResult;
                            if (myResult != RunStatus.Running) break;
                            yield return result;
                        }
                        break;
                    case "coppeliaactionmoralbelief":
                        foreach (RunStatus result in ProcessCoppeliaActionMoralBelief(myNode))
                        {
                            myResult = result;
                            bot.myBehaviors.runState[nodeID] = myResult;
                            if (myResult != RunStatus.Running) break;
                            yield return result;
                        }
                        break;
                    case "gencoppeliafrommt":
                        foreach (RunStatus result in ProcessGenCoppeliaFromMt(myNode))
                        {
                            myResult = result;
                            bot.myBehaviors.runState[nodeID] = myResult;
                            if (myResult != RunStatus.Running) break;
                            yield return result;
                        }
                        break;

//DEFAULT
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
            string processAs = "parallel";
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
                if (myNode.Attributes["processas"] != null)
                {
                    processAs = myNode.Attributes["processas"].Value.ToLower();
                }
                if (myNode.Attributes["pace"] != null)
                {
                    pace = 5000;
                    pace = Int32.Parse(myNode.Attributes["pace"].Value);
                }
                foreach (XmlAttribute att in myNode.Attributes)
                {
                    // attribute stuff
                    Console.WriteLine(" behavior attribute: {0} = {1}", att.Name, att.Value);
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
                switch (processAs.ToLower())
                {
                    case "parallel":
                            //result = ProcessParallel(myNode);
                            foreach (RunStatus myChildResult in ProcessParallel(myNode))
                            {
                                result = myChildResult;
                                if (result != RunStatus.Running) break;
                                yield return RunStatus.Running;
                            }

                            yield return result;
                            yield break;

                    case "selector":
                            //result = ProcessParallel(myNode);
                            foreach (RunStatus myChildResult in ProcessSelector(myNode))
                            {
                                result = myChildResult;
                                if (result != RunStatus.Running) break;
                                yield return RunStatus.Running;
                            }

                            yield return result;
                            yield break;

                    case "sequence":
                            //result = ProcessParallel(myNode);
                            foreach (RunStatus myChildResult in ProcessSequence(myNode))
                            {
                                result = myChildResult;
                                if (result != RunStatus.Running) break;
                                yield return RunStatus.Running;
                            }

                            yield return result;
                            yield break;


                }
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
        //WaitForChatInput
        public IEnumerable<RunStatus> ProcessWaitForChatInput(XmlNode myNode)
        {
            string sentStr = myNode.InnerXml;
             RunStatus rs = RunStatus.Failure;
             string wait = "10000";
             try
             {
                 if (myNode.Attributes["wait"] != null)
                 {
                     wait = myNode.Attributes["wait"].Value;
                 }
             }
             catch (Exception e)
             {
                 wait = "10000";
             }
             int waitV = Int32.Parse(wait);
             int starttime = Environment.TickCount;
             int triggertime = starttime +waitV;
             bot.myBehaviors.waitingForChat = true;
             while (Environment.TickCount < triggertime)
             {
                 if (bot.chatInputQueue.Count > 0)
                 {
                     rs = RunStatus.Success;
                     bot.myBehaviors.waitingForChat = false;

                     yield return rs;
                     yield break;
                     
                 }
                 yield return RunStatus.Running;

             }
             bot.myBehaviors.waitingForChat = false;
             yield return rs;
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
            Aima.Core.Logic.Propositional.Algorithms.Model MiniModel = new Aima.Core.Logic.Propositional.Algorithms.Model();
            Aima.Core.Logic.Propositional.Algorithms.Model walkModel = null;
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
        #region Siprolog
        // Siprolog clauses
        // =<clearKB mt="name>
        // =<tellKB mt="name"> si_text
        // =<assertwrt mt="name">si_query
        // <random_select mt="name" var="var"> si_query
        // <sequence_query mt="name" var="var"> si_query
        // <selector_query mt="name" var="var"> si_query
        // <query_macro mt="name" var="var" cmd="selector|sequence|random|..."> si_query
        // =<connectMT child="name" parent="name">>
        // =<loadKEKB file="path">
        // =<appendKB mt="name">si_text
        // =<insertKB mt="name">si_text
        public IEnumerable<RunStatus> ProcessClearPrologMt(XmlNode myNode)
        {
            // Clear a specified KB
            RunStatus rs = RunStatus.Failure;
            string mtName = "root";
            string innerStr = myNode.InnerXml.Trim();

            try
            {
                if (myNode.Attributes["mt"] != null) mtName = myNode.Attributes["mt"].Value;


                bot.myServitor.prologEngine.insertKB("", mtName);
                rs = RunStatus.Success;
            }
            catch (Exception e)
            {
                Console.WriteLine("Error ProcessClearProKB '{0}':{1}", mtName, e.Message);
                rs = RunStatus.Failure;
            }
            yield return rs;
            yield break;
        }

        public IEnumerable<RunStatus> ProcessConnectMt(XmlNode myNode)
        {
            //Connect two MT's
            RunStatus rs = RunStatus.Failure;
            string childMtName = "root";
            string parentMtName = "root";
            string innerStr = myNode.InnerXml.Trim();

            try
            {
                if (myNode.Attributes["child"] != null) childMtName = myNode.Attributes["child"].Value;
                if (myNode.Attributes["parent"] != null) parentMtName = myNode.Attributes["parent"].Value;


                bot.myServitor.prologEngine.connectMT(childMtName, parentMtName);
                rs = RunStatus.Success;
            }
            catch (Exception e)
            {
                Console.WriteLine("Error ProcessConnectMt '{0}', '{1}':{2}", childMtName, parentMtName, e.Message);
                rs = RunStatus.Failure;
            }
            yield return rs;
            yield break;
        }

        public IEnumerable<RunStatus> ProcessTellPrologMt(XmlNode myNode)
        {
            // Assert with overwrite some si_text
            RunStatus rs = RunStatus.Failure;
            string mtName = "root";
            string innerStr = myNode.InnerXml.Trim();

            try
            {
                if (myNode.Attributes["mt"] != null) mtName = myNode.Attributes["mt"].Value;


                bot.myServitor.prologEngine.insertKB(innerStr, mtName);
                rs = RunStatus.Success;
            }
            catch (Exception e)
            {
                Console.WriteLine("Error ProcessTellProKB '{0}':{1}", mtName, e.Message);
                rs = RunStatus.Failure;
            }
            yield return rs;
            yield break;
        }
        public IEnumerable<RunStatus> ProcessLoadKEKB(XmlNode myNode)
        {
            // load some KE (which will have MT definitions)
            RunStatus rs = RunStatus.Failure;
            string path = "default.ke";
            string innerStr = myNode.InnerXml.Trim();

            try
            {
                if (myNode.Attributes["path"] != null) path = myNode.Attributes["path"].Value;


                bot.myServitor.prologEngine.loadKEKB(path);
                rs = RunStatus.Success;
            }
            catch (Exception e)
            {
                Console.WriteLine("Error ProcessLoadKEKB '{0}':{1}", path, e.Message);
                rs = RunStatus.Failure;
            }
            yield return rs;
            yield break;
        }
        public IEnumerable<RunStatus> ProcessInsertMt(XmlNode myNode)
        {
            // insert some si_text (overwrite)
            RunStatus rs = RunStatus.Failure;
            string mtName = "root";
            string innerStr = myNode.InnerXml.Trim();

            try
            {
                if (myNode.Attributes["mt"] != null) mtName = myNode.Attributes["mt"].Value;


                bot.myServitor.prologEngine.insertKB(innerStr, mtName);
                rs = RunStatus.Success;
            }
            catch (Exception e)
            {
                Console.WriteLine("Error ProcessInsertKB '{0}':{1}", mtName, e.Message);
                rs = RunStatus.Failure;
            }
            yield return rs;
            yield break;
        }

        public IEnumerable<RunStatus> ProcessAppendMt(XmlNode myNode)
        {
            // Append some si_text 
            RunStatus rs = RunStatus.Failure;
            string mtName = "root";
            string innerStr = myNode.InnerXml.Trim();

            try
            {
                if (myNode.Attributes["mt"] != null) mtName = myNode.Attributes["mt"].Value;


                bot.myServitor.prologEngine.appendKB(innerStr, mtName);
                rs = RunStatus.Success;
            }
            catch (Exception e)
            {
                Console.WriteLine("Error ProcessAppendKB '{0}':{1}", mtName, e.Message);
                rs = RunStatus.Failure;
            }
            yield return rs;
            yield break;
        }


        public IEnumerable<RunStatus> ProcessAssertProlog(XmlNode myNode)
        {
            // Conduct a test if the si_query is true
            string condition = myNode.Attributes["cond"].Value;
            string mtName = "root";
            string innerStr = myNode.InnerXml.Trim();
            //if it doesn't exist then return failure
            if (bot.myServitor.prologEngine == null)
            {
                yield return RunStatus.Failure;
                yield break;
            }
            RunStatus r = RunStatus.Failure;
            try
            {
                bool result = bot.myServitor.prologEngine.isTrueIn(innerStr, mtName);
                if (result) r = RunStatus.Success;
            }
            catch (Exception e)
            {
                Console.WriteLine("Error ProcessAssertGuest '{0}':{1}", condition, e.Message);
                r = RunStatus.Failure;
            }

            yield return r;
            yield break;
        }

        public IEnumerable<RunStatus> ProcessEvalPrologMacro(XmlNode myNode)
        {
            // given a prolog query, will take the bindings of var as a list of subbehavior id's (or other)
            // then place them inside a <cmd> tag and execute
            //examples:
            // <query_macro outtercmd='select' innercmd='subbehavior' mt='decide' var="ACTION" >should_act(ACTION)</query_macro>
            // <query_macro outtercmd='random' innercmd='task,say' mt='tellerSpindle' var="MESSAGE" >couldSay(MESSAGE)</query_macro>
            //
            string filler = "";

            string var = "X";
            string outerCmd ="random";
            string innerCmd = "task,say";
            string mtName = "root";
            string outerCode = "";
            string innerCode = "";
            try
            {
                if (myNode.Attributes["filler"] != null) filler = myNode.Attributes["filler"].Value;
                if (myNode.Attributes["var"] != null) var = myNode.Attributes["var"].Value;
                if (myNode.Attributes["outercmd"] != null) outerCmd = myNode.Attributes["outercmd"].Value;
                if (myNode.Attributes["innercmd"] != null) innerCmd = myNode.Attributes["innercmd"].Value;
                if (myNode.Attributes["mt"] != null) mtName = myNode.Attributes["mt"].Value;
            }
            catch(Exception e)
            {
            }
            string innerStr = myNode.InnerXml.Trim();
            //if it doesn't exist then return failure
            if (bot.myServitor.prologEngine == null)
            {
                yield return RunStatus.Failure;
                yield break;
            }
            RunStatus r = RunStatus.Failure;
            try
            {
                string[] splitOuter = outerCmd.Split(',');
                string[] splitInner = innerCmd.Split(',');
                List<Dictionary<string, string>> bingingsList = new List<Dictionary<string, string>>();
                // Dictionary<string, string> bindings = new Dictionary<string,string> ();
                bot.myServitor.prologEngine.askQuery(innerStr, mtName, out bingingsList);
                foreach (Dictionary<string, string> bindings in bingingsList)
                {
                    foreach (string k in bindings.Keys)
                    {
                        //Console.WriteLine("BINDING {0} = {1}", k, bindings[k]);
                        if (k == var)
                        {
                            if (innerCmd == "subbehavior")
                            {
                                innerCode += String.Format("<{0} id='{1}'/>\n", innerCmd, bindings[k]);
                            }
                            else
                            {
                                if (splitInner.Length == 1)
                                {
                                    innerCode += String.Format("<{0}>{1}{2}</{0}>\n", innerCmd,filler, bindings[k]);
                                }
                                else
                                {
                                    string frag = bindings[k];
                                    if (filler.Length > 0) { frag = filler + " " + frag; }
                                    for (int i = splitInner.Length-1; i >=0 ;i-- )
                                    {
                                        string tag = splitInner[i];
                                        frag = String.Format("<{0}>{1}</{0}>\n", tag, frag);

                                    }
                                    innerCode += frag + "\n";
                                }
                            }
                        }
                    }
                }

                string outerFrag = innerCode;

                for (int i = splitOuter.Length-1; i >=0 ;i-- )
                {
                    string tag = splitOuter[i];
                    outerFrag = String.Format("<{0}>\n{1}</{0}>\n", tag, outerFrag);
                }
                outerCode += outerFrag + "\n";
                
            }
            catch (Exception e)
            {
                Console.WriteLine("Error ProcessPrologBehaveMacro '{0}','{1}','{2}','{3}','{4}':'{5}'", var, innerCmd, outerCmd, innerStr, mtName, e.Message);
                r = RunStatus.Failure;
            }

             RunStatus result = RunStatus.Failure;
            //result = ProcessParallel(myNode);
            foreach (RunStatus myChildResult in evalBehaviorXml(outerCode))
            {
                result = myChildResult;
                if (result != RunStatus.Running) break;
                yield return RunStatus.Running;
            }

            yield return result;
            yield break;

        }

 
        #endregion


        #region particleFilter

        public SymbolicParticleFilter findOrCreatePF(string name)
        {
            SymbolicParticleFilter resultFilter = null;
            if (!bot.servitor.partFilterDict.ContainsKey(name))
            {
                bot.servitor.partFilterDict.Add(name, new SymbolicParticleFilter());
            }
            resultFilter = bot.servitor.partFilterDict[name];
            return resultFilter;
        }

        // <definestate prob=""> state_list|...</>
        //bot.myServitor.partFilter.prototype.variables.Add("in(r0)", 0.8);
        public IEnumerable<RunStatus> ProcessDefineState(XmlNode myNode)
        {
            // Append some si_text 
            RunStatus rs = RunStatus.Failure;
            string probStr = "0.5";
            string innerStr = myNode.InnerXml.Trim();
            string filter = "basic";

            try
            {
                if (myNode.Attributes["prob"] != null) probStr = myNode.Attributes["prob"].Value;
                if (myNode.Attributes["filter"] != null) filter = myNode.Attributes["filter"].Value;
                SymbolicParticleFilter myFilter = findOrCreatePF(filter);
                double prob = Double.Parse(probStr);
                string[] state_list = innerStr.Split('|');
                foreach (string state in state_list)
                {
                    if (!myFilter.prototype.variables.ContainsKey(state))
                    {
                        myFilter.prototype.variables.Add(state, prob);
                    }
                    myFilter.prototype.variables[state] = prob;
                }
                rs = RunStatus.Success;
            }
            catch (Exception e)
            {
                Console.WriteLine("Error ProcessDefineState '{0}','{1}':{2}", probStr, innerStr, e.Message);
                rs = RunStatus.Failure;
            }
            yield return rs;
            yield break;
        }
            
        // <stateConstraintSet> state_list|...</>
        //    bot.myServitor.partFilter.constraintSet.Add("in(r0)|in(r1)|in(r2)|in(r3)|in(r4)");
        public IEnumerable<RunStatus> ProcessConstraintSet(XmlNode myNode)
        {
            // Append some si_text 
            RunStatus rs = RunStatus.Failure;
            string innerStr = myNode.InnerXml.Trim();
            string filter = "basic";

            try
            {
                if (myNode.Attributes["filter"] != null) filter = myNode.Attributes["filter"].Value;
                SymbolicParticleFilter myFilter = findOrCreatePF(filter);
                myFilter.constraintSet.Add(innerStr);
                rs = RunStatus.Success;
            }
            catch (Exception e)
            {
                Console.WriteLine("Error ProcessConstraintSet '{0}':{1}", innerStr, e.Message);
                rs = RunStatus.Failure;
            }
            yield return rs;
            yield break;
        }

        // <stateSense state="" prob=""> senseList|...</>
        //    bot.myServitor.partFilter.addStateSenseProb("in(r0)|sense(even)=0.95");
        public IEnumerable<RunStatus> ProcessStateSense(XmlNode myNode)
        {
            // Append some si_text 
            RunStatus rs = RunStatus.Failure;
            string probStr = "0.5";
            string state = "in(0)";
            string innerStr = myNode.InnerXml.Trim();
            string filter = "basic";
            try
            {
                if (myNode.Attributes["prob"] != null) probStr = myNode.Attributes["prob"].Value;
                if (myNode.Attributes["state"] != null) state = myNode.Attributes["state"].Value;
                if (myNode.Attributes["filter"] != null) filter = myNode.Attributes["filter"].Value;
                SymbolicParticleFilter myFilter = findOrCreatePF(filter);
                double prob = Double.Parse(probStr);
                string[] sense_list = innerStr.Split('|');
                foreach (string sense in sense_list)
                {
                    string frag = String.Format("{0}|{1}={2}", state, sense, prob);
                    myFilter.addStateSenseProb(frag);
                }
                rs = RunStatus.Success;
            }
            catch (Exception e)
            {
                Console.WriteLine("Error ProcessStateSense '{0}','{1}':{2}", probStr, innerStr, e.Message);
                rs = RunStatus.Failure;
            }
            yield return rs;
            yield break;
        }

         //   <stateActTransition state="" act="" prob="">nextstatelist|...</>
         //   bot.myServitor.partFilter.addStateActTransition("in(r0)|act(forward)=0.5:in(r0)");
        public IEnumerable<RunStatus> ProcessStateActTransition(XmlNode myNode)
        {
            // Append some si_text 
            RunStatus rs = RunStatus.Failure;
            string probStr = "0.5";
            string state = "in(0)";
            string act = "act(0)";
            string innerStr = myNode.InnerXml.Trim();
            string filter = "basic";

            try
            {
                if (myNode.Attributes["prob"] != null) probStr = myNode.Attributes["prob"].Value;
                if (myNode.Attributes["state"] != null) state = myNode.Attributes["state"].Value;
                if (myNode.Attributes["action"] != null) act = myNode.Attributes["action"].Value;
                if (myNode.Attributes["filter"] != null) filter = myNode.Attributes["filter"].Value;
                SymbolicParticleFilter myFilter = findOrCreatePF(filter);
                double prob = Double.Parse(probStr);
                string[] state_list = innerStr.Split('|');

                string frag = String.Format("{0}|{1}={2}:{3}", state, act, prob, innerStr);
                myFilter.addStateActTransition(frag);

                rs = RunStatus.Success;
            }
            catch (Exception e)
            {
                Console.WriteLine("Error ProcessStateActTransition '{0}','{1}':{2}", probStr, innerStr, e.Message);
                rs = RunStatus.Failure;
            }
            yield return rs;
            yield break;
        }

        // use particleActMt, particleSenseMt, particleStateMt
        //    bot.myServitor.partFilter.quickFilter("act(forward)", "sense(odd)");
        public IEnumerable<RunStatus> ProcessQuickFilter(XmlNode myNode)
        {
            // collects true acts in senses in pointed to mt's
            // then updates filter
            RunStatus rs = RunStatus.Failure;
            string senseMt = "particleSenseMt";
            string actMt = "particleActMt";
            string innerStr = myNode.InnerXml;
            string filter = "basic";

            string actset = "";
            string senseset = "";
            try
            {
                if (myNode.Attributes["sensemt"] != null) senseMt = myNode.Attributes["sensemt"].Value;
                if (myNode.Attributes["actmt"] != null) actMt = myNode.Attributes["actmt"].Value;
                if (myNode.Attributes["filter"] != null) filter = myNode.Attributes["filter"].Value;
                SymbolicParticleFilter myFilter = findOrCreatePF(filter);

                foreach (string q in myFilter.actList)
                {
                    if (bot.servitor.prologEngine.isTrueIn(q, actMt))
                    {
                        if (actset.Length > 0) actset += "|";
                        actset += q;
                    }
                }
                foreach (string q in myFilter.senseList)
                {
                    if (bot.servitor.prologEngine.isTrueIn(q, senseMt))
                    {
                        if (senseset.Length > 0) senseset += "|";
                        senseset += q;
                    }
                }

                myFilter.quickFilter(actset, senseset);

                rs = RunStatus.Success;
            }
            catch (Exception e)
            {
                Console.WriteLine("Error ProcessQuickFilter '{0}','{1}':{2}", senseMt, actMt, e.Message);
                rs = RunStatus.Failure;
            }
            yield return rs;
            yield break;
        }       
 
       //     bot.myServitor.partFilter.defMeanParticle();
      //      //bot.myServitor.partFilter.dump();
      //      Console.WriteLine("meanP raw:{0}", bot.myServitor.partFilter.meanParticle.ToString());
      //      bot.myServitor.partFilter.meanParticle.normalize(bot.myServitor.partFilter.constraintSet);
      //      Console.WriteLine("meanP norm:{0}", bot.myServitor.partFilter.meanParticle.ToString());
       // <meanparticle threshold="0.5" mt="meanParticleMt"/>
        public IEnumerable<RunStatus> ProcessMeanParticle(XmlNode myNode)
        {
            // Append some si_text 
            RunStatus rs = RunStatus.Failure;
            string probStr = "0.5";
            string mtName = "meanParticleMt";
            string innerStr = myNode.InnerXml;
            double threshold = 0.5;
            string filter = "basic";

            try
            {
                if (myNode.Attributes["threshold"] != null) probStr = myNode.Attributes["threshold"].Value;
                if (myNode.Attributes["mt"] != null) mtName = myNode.Attributes["mt"].Value;
                if (myNode.Attributes["filter"] != null) filter = myNode.Attributes["filter"].Value;
                SymbolicParticleFilter myFilter = findOrCreatePF(filter);
                threshold = Double.Parse(probStr);

                myFilter.defMeanParticle();
                myFilter.meanParticle.normalize(myFilter.constraintSet);
                string meanDMT = myFilter.meanParticle.asDataMt(threshold);
                bot.servitor.prologEngine.insertKB(meanDMT, mtName);

                rs = RunStatus.Success;
            }
            catch (Exception e)
            {
                Console.WriteLine("Error ProcessMeanParticle '{0}','{1}':{2}", threshold, mtName, e.Message);
                rs = RunStatus.Failure;
            }
            yield return rs;
            yield break;
        }

        // <genFilterFromMt mt="defParticleMt" threshold="0.0001" filter="basic"/>
        // will skip probabilities below threshold
        // will stateProb(STATE,PROB) , stateSenseProb(STATE,SENSE,PROB) and
        // stateTransitionProb(STATE,ACT,PROB,NEXT)

        public IEnumerable<RunStatus> ProcessGenFilterFromMt(XmlNode myNode)
        {
            // <genFilterFromMt mt="defParticleMt" threhold="0.001" filter="basic"/>

            RunStatus rs = RunStatus.Failure;
            string probStr = "0.001";
            string mtName = "defParticleMt";
            string innerStr = myNode.InnerXml;
            double threshold = 0.001;
            string filter = "basic";

            try
            {
                if (myNode.Attributes["threshold"] != null) probStr = myNode.Attributes["threshold"].Value;
                if (myNode.Attributes["mt"] != null) mtName = myNode.Attributes["mt"].Value;
                if (myNode.Attributes["filter"] != null) filter = myNode.Attributes["filter"].Value;
                SymbolicParticleFilter myFilter = findOrCreatePF(filter);
                threshold = Double.Parse(probStr);

                List<Dictionary<string, string>> bingingsList = new List<Dictionary<string, string>>();

                //State-Apriori
                bot.servitor.prologEngine.askQuery("stateProb(STATE,PROB)", mtName, out bingingsList);
                foreach (Dictionary<string, string> bindings in bingingsList)
                {
                    double prob = Double.Parse(bindings["PROB"]);
                    string state = bindings["STATE"];
                    if (prob >= threshold)
                    {
                        if (!myFilter.prototype.variables.ContainsKey(state))
                        {
                            myFilter.prototype.variables.Add(state, prob);
                        }
                        myFilter.prototype.variables[state] = prob;
                    }
                }

                //State-Sense-Prob
                bot.servitor.prologEngine.askQuery("stateSenseProb(STATE,SENSE,PROB)", mtName, out bingingsList);
                foreach (Dictionary<string, string> bindings in bingingsList)
                {
                    string state = bindings["STATE"];
                    string sense = bindings["SENSE"];
                    double prob = Double.Parse(bindings["PROB"]);
                    if (prob >= threshold)
                    {
                        string frag = String.Format("{0}|{1}={2}", state, sense, prob);
                        myFilter.addStateSenseProb(frag);
                    }
                }

                //State-Act-NextAct
                bot.servitor.prologEngine.askQuery("stateTransitionProb(STATE,ACT,PROB,NEXT)", mtName, out bingingsList);
                foreach (Dictionary<string, string> bindings in bingingsList)
                {
                    string state = bindings["STATE"];
                    string act = bindings["ACT"];
                    string next = bindings["NEXT"];
                    double prob = Double.Parse(bindings["PROB"]);
                    if (prob >= threshold)
                    {
                        string frag = String.Format("{0}|{1}={2}:{3}", state, act, prob, next);
                        myFilter.addStateActTransition(frag);
                    }
                }


                rs = RunStatus.Success;
            }
            catch (Exception e)
            {
                Console.WriteLine("Error ProcessGenFilterFromMt '{0}','{1}':{2}", threshold, mtName,filter, e.Message);
                rs = RunStatus.Failure;
            }
            yield return rs;
            yield break;
        }
        #endregion

        #region Coppelia
        // <UpdateCoppeliaFromMt mt="defCoppeliaMt" threshold="0.0001" filter="basic"/>
        // will update Coppelia states by querying state(cState) in Mt

        public IEnumerable<RunStatus> ProcessUpdateCoppeliaFromMt(XmlNode myNode)
        {
            // Updates the Coppelia Current state model from a given Mt

            RunStatus rs = RunStatus.Failure;
            string probStr = "0.001";
            string mtName = "defCoppeliaMt";
            string innerStr = myNode.InnerXml;
            double threshold = 0.001;
            string filter = "basic";

            try
            {
                if (myNode.Attributes["threshold"] != null) probStr = myNode.Attributes["threshold"].Value;
                if (myNode.Attributes["mt"] != null) mtName = myNode.Attributes["mt"].Value;
                if (myNode.Attributes["filter"] != null) filter = myNode.Attributes["filter"].Value;
                SymbolicParticleFilter myFilter = findOrCreatePF(filter);
                threshold = Double.Parse(probStr);

                List<Dictionary<string, string>> bingingsList = new List<Dictionary<string, string>>();
                foreach (string state in bot.servitor.CoppeliaStateDictionary.Keys)
                {
                    string query = String.Format("state({0})", state);
                    bool isTrue = bot.servitor.prologEngine.isTrueIn(query, mtName);
                    int stateIndex = bot.servitor.CoppeliaStateDictionary[state];
                    Global.SetState(stateIndex, isTrue);
                }
                Console.WriteLine("coppelia ProcessUpdateCoppeliaFromMt :{0}", myNode.OuterXml);
                rs = RunStatus.Success;
            }
            catch (Exception e)
            {
                Console.WriteLine("Error ProcessUpdateCoppeliaFromMt '{0}','{1}':{2}", threshold, mtName, filter, e.Message);
                rs = RunStatus.Failure;
            }
            yield return rs;
            yield break;
        }

        // <GenCoppeliaFromMt mt="defCoppeliaMt" threshold="0.0001"/>
        // agentActions(Action, positive, negative)
        // actionResponse(Action,Response)
        // defState(State,Initial)
        // stateAmbition(Actor,State,value)
        // actionStateBelief(Actor,Action,State,value)

        // moralPrinciple(Moral,Initial)
        // moralAmbition(Actor,Moral,value)
        // actionMoralBelief(Actor,Action,Moral,value)

        public IEnumerable<RunStatus> ProcessGenCoppeliaFromMt0(XmlNode myNode)
        {
            // Append some si_text 
            RunStatus rs = RunStatus.Failure;
            string probStr = "0.001";
            string mtName = "defCoppeliaMt";
            string innerStr = myNode.InnerXml;
            double threshold = 0.001;
            string filter = "basic";

            try
            {
                if (myNode.Attributes["threshold"] != null) probStr = myNode.Attributes["threshold"].Value;
                if (myNode.Attributes["mt"] != null) mtName = myNode.Attributes["mt"].Value;
                if (myNode.Attributes["filter"] != null) filter = myNode.Attributes["filter"].Value;
                SymbolicParticleFilter myFilter = findOrCreatePF(filter);
                threshold = Double.Parse(probStr);

                List<Dictionary<string, string>> bingingsList = new List<Dictionary<string, string>>();

                // agentActions(Action, positive, negative)
                bot.servitor.prologEngine.askQuery("agentActions(ACTION, POS, NEG)", mtName, out bingingsList);
                foreach (Dictionary<string, string> bindings in bingingsList)
                {
                    float pos = float.Parse(bindings["POS"]);
                    float neg = float.Parse(bindings["NEG"]);
                    string cAction = bindings["ACTION"];
                    if (bot.servitor.CoppeliaActionDictionary.ContainsKey(cAction))
                    {
                        bot.servitor.CoppeliaActionDictionary[cAction].SetValence(pos, neg);
                    }
                    else
                    {
                        AgentAction newAction = new AgentAction(cAction, pos, neg);
                        bot.servitor.CoppeliaActionDictionary[cAction] = newAction;
                    }
                }

                // actionResponse(Action,Response)
                bot.servitor.prologEngine.askQuery("actionResponse(ACTION,RESPONSE)", mtName, out bingingsList);
                foreach (Dictionary<string, string> bindings in bingingsList)
                {
                    string cAction = bindings["ACTION"];
                    string cResponse = bindings["RESPONSE"];
                    if (bot.servitor.CoppeliaActionDictionary.ContainsKey(cAction))
                    {
                        if (bot.servitor.CoppeliaActionDictionary.ContainsKey(cResponse))
                        {
                            AgentAction a1 = bot.servitor.CoppeliaActionDictionary[cAction];
                            AgentAction a2 = bot.servitor.CoppeliaActionDictionary[cResponse];
                            a1.AddResponse(a2.GlobalIndex);
                        }
                    }
                }

                // defState(State,Initial)
                bot.servitor.prologEngine.askQuery("defState(STATE,INITIAL)", mtName, out bingingsList);
                foreach (Dictionary<string, string> bindings in bingingsList)
                {
                    string cState = bindings["STATE"];
                    string cInitState = bindings["INITIAL"];
                    bool bState = false;
                    bState = bool.Parse(cInitState);
                    if (!bot.servitor.CoppeliaStateDictionary.ContainsKey(cState))
                    {
                        int newState = Global.AddState(bState);
                        bot.servitor.CoppeliaStateDictionary[cState] = newState;
                    }
                }
                // ambition(Actor,State,value)
                bot.servitor.prologEngine.askQuery("stateAmbition(ACTOR,STATE,VALUE)", mtName, out bingingsList);
                foreach (Dictionary<string, string> bindings in bingingsList)
                {
                    string cActor = bindings["ACTOR"];
                    string cState = bindings["STATE"];
                    string cValue = bindings["VALUE"];
                    float fValue = 0;
                    fValue = float.Parse(cValue);
                    if (bot.servitor.CoppeliaStateDictionary.ContainsKey(cState))
                    {
                        if (bot.servitor.CoppeliaAgentDictionary.ContainsKey(cActor))
                        {
                            Agent a1 = bot.servitor.CoppeliaAgentDictionary[cActor];
                            int state = bot.servitor.CoppeliaStateDictionary[cState];
                            a1.AddAmbition(state, fValue);
                        }
                    }
                }
                // actionStateBelief(Actor,Action,State,value)
                bot.servitor.prologEngine.askQuery("actionStateBelief(ACTOR,ACTION,STATE,VALUE)", mtName, out bingingsList);
                foreach (Dictionary<string, string> bindings in bingingsList)
                {
                    string cActor = bindings["ACTOR"];
                    string cAction = bindings["ACTION"];
                    string cState = bindings["STATE"];
                    string cValue = bindings["VALUE"];
                    float fValue = 0;
                    fValue = float.Parse(cValue);

                    if (bot.servitor.CoppeliaAgentDictionary.ContainsKey(cActor))
                    {
                        if (bot.servitor.CoppeliaActionDictionary.ContainsKey(cAction))
                        {
                            if (bot.servitor.CoppeliaStateDictionary.ContainsKey(cState))
                            {
                                AgentAction act = bot.servitor.CoppeliaActionDictionary[cAction];
                                Agent a1 = bot.servitor.CoppeliaAgentDictionary[cActor];
                                int state = bot.servitor.CoppeliaStateDictionary[cState];
                                a1.SetActionStateBelief(act.GlobalIndex, state, fValue);
                            }
                        }
                    }
                }
                //Morals
                // moralPrinciple(Moral,Initial)
                bot.servitor.prologEngine.askQuery("moralPrinciple(MORAL,INITIAL)", mtName, out bingingsList);
                foreach (Dictionary<string, string> bindings in bingingsList)
                {
                    string cMoral = bindings["MORAL"];
                    string cInitState = bindings["INITIAL"];
                    bool bState = false;
                    bState = bool.Parse(cInitState);
                    if (!bot.servitor.CoppeliaMoralsDictionary.ContainsKey(cMoral))
                    {
                        int newState = Global.AddState(bState);
                        bot.servitor.CoppeliaMoralsDictionary[cMoral] = newState;
                    }
                }
                // moralAmbition(Actor,Moral,value)
                bot.servitor.prologEngine.askQuery("stateAmbition(ACTOR,MORAL,VALUE)", mtName, out bingingsList);
                foreach (Dictionary<string, string> bindings in bingingsList)
                {
                    string cActor = bindings["ACTOR"];
                    string cMoral = bindings["MORAL"];
                    string cValue = bindings["VALUE"];
                    float fValue = 0;
                    fValue = float.Parse(cValue);
                    if (bot.servitor.CoppeliaMoralsDictionary.ContainsKey(cMoral))
                    {
                        if (bot.servitor.CoppeliaAgentDictionary.ContainsKey(cActor))
                        {
                            Agent a1 = bot.servitor.CoppeliaAgentDictionary[cActor];
                            int moral = bot.servitor.CoppeliaMoralsDictionary[cMoral];
                            a1.AddMoralAmbition(moral, fValue);
                        }
                    }
                }
                // actionMoralBelief(Actor,Action,Moral,value)
                bot.servitor.prologEngine.askQuery("actionStateBelief(ACTOR,ACTION,MORAL,VALUE)", mtName, out bingingsList);
                foreach (Dictionary<string, string> bindings in bingingsList)
                {
                    string cActor = bindings["ACTOR"];
                    string cAction = bindings["ACTION"];
                    string cMoral = bindings["MORAL"];
                    string cValue = bindings["VALUE"];
                    float fValue = 0;
                    fValue = float.Parse(cValue);

                    if (bot.servitor.CoppeliaAgentDictionary.ContainsKey(cActor))
                    {
                        if (bot.servitor.CoppeliaActionDictionary.ContainsKey(cAction))
                        {
                            if (bot.servitor.CoppeliaMoralsDictionary.ContainsKey(cMoral))
                            {
                                AgentAction act = bot.servitor.CoppeliaActionDictionary[cAction];
                                Agent a1 = bot.servitor.CoppeliaAgentDictionary[cActor];
                                int moral = bot.servitor.CoppeliaMoralsDictionary[cMoral];
                                a1.SetActionMoralPrincipleBelief(act.GlobalIndex, moral, fValue);
                            }
                        }
                    }
                }



                rs = RunStatus.Success;
            }
            catch (Exception e)
            {
                Console.WriteLine("Error ProcessGenCoppeliaFromMt '{0}','{1}':{2}", threshold, mtName, filter, e.Message);
                rs = RunStatus.Failure;
            }
            yield return rs;
            yield break;
        }

        public IEnumerable<RunStatus> ProcessGenCoppeliaFromMt(XmlNode myNode)
        {
            //Queries MT for Coppelia related info to create a BTXML fragment
            // that is then interperted

            RunStatus rs = RunStatus.Failure;
            string mtName = "defCoppeliaMt";
            string innerStr = myNode.InnerXml;

            try
            {
                if (myNode.Attributes["mt"] != null) mtName = myNode.Attributes["mt"].Value;
                string xcode = "";
                xcode += kbToBTXML("coppeliaState(STATE,INITIAL)", mtName);
                xcode += kbToBTXML("coppeliaAction(ACTION,POSITIVITY,NEGATIVITY)", mtName);
                xcode += kbToBTXML("coppeliaFeature(AGENT,FEATURE,VALUE)", mtName);
                xcode += kbToBTXML("coppeliaFeatuerBelief(AGENT,FEATURE,TARGET,VALUE)", mtName);
                xcode += kbToBTXML("coppeliaAgentResponsibleBelief(AGENT,STATE,TARGET,VALUE)", mtName);
                xcode += kbToBTXML("coppeliaExpectedSatisfaction(AGENT,ACTION,TARGET,VALUE)", mtName);
                xcode += kbToBTXML("coppeliaAnger(AGENT,TARGET,VALUE)", mtName);
                xcode += kbToBTXML("coppeliaPraiseworthy(AGENT,TARGET,VALUE)", mtName);
                xcode += kbToBTXML("coppeliaEmotion(AGENT,EMOTION,VALUE)", mtName);
                xcode += kbToBTXML("coppeliaDesired(AGENT,EMOTION,VALUE)", mtName);
                xcode += kbToBTXML("coppeliaActionTendancy(AGENT,ACTION,TARGET,VALUE)", mtName);
                xcode += kbToBTXML("coppeliaFeatureEmotionBelief(AGENT,FEATURE,EMOTION,VALUE)", mtName);
                xcode += kbToBTXML("coppeliaStateLikelihood(AGENT,STATE,LIKELIHOOD)", mtName);

                xcode += kbToBTXML("coppeliaResponse(ACTION,RESPONSE)", mtName);
                xcode += kbToBTXML("coppeliaAmbition(AGENT,STATE,VALUE)", mtName);

                xcode += kbToBTXML("coppeliaMoral(MORAL,INITIAL)", mtName);
                xcode += kbToBTXML("coppeliaMoralAmbition(AGENT,MORAL,VALUE)", mtName);
                xcode += kbToBTXML("coppeliaActionMoralBelief(AGENT,ACTION,MORAL,VALUE)", mtName);

                xcode += kbToBTXML("coppeliaActionState(AGENT,ACTION,STATE,VALUE)", mtName);
                xcode += kbToBTXML("coppeliaStateState(AGENT,STATESRC,STATEDEST,VALUE)", mtName);
                xcode += kbToBTXML("coppeliaRelation(AGENT,RELATION,RECIPIENT)", mtName);
                xcode += kbToBTXML("coppeliaPerform(AGENT,ACTION,RECIPIENT)", mtName);

                string btxmlCode="";
                btxmlCode += "<aiml version=\"1.0\">\n";
                btxmlCode += " <state name=\"*\">\n";
                btxmlCode += "  <btxml>\n";
                btxmlCode += xcode;
                btxmlCode += "  </btxml>\n";
                btxmlCode += " </state>\n";
                btxmlCode += "</aiml>\n";
                XmlDocument coppeliaDoc = new XmlDocument ();
                coppeliaDoc.LoadXml (btxmlCode);
                Console.WriteLine("-------------------------------------");
                Console.WriteLine("------ GenCoppeliaFromMt:{0} --------", mtName);
                Console.WriteLine(btxmlCode);
                Console.WriteLine("-------------------------------------");
                bot.loadAIMLFromXML(coppeliaDoc, "mt:"+mtName +DateTime.Now.ToString());
            
                rs = RunStatus.Success;
            }
            catch (Exception e)
            {
                Console.WriteLine("Error ProcessGenCoppeliaFromMt '{0}',{1}", mtName, e.Message);
                rs = RunStatus.Failure;
            }
            yield return rs;
            yield break;
        }

        public string kbToBTXML(string query, string mtName)
        {
            // A query like coppeliaAnger(AGENT,TARGET,VALUE) might return 
            // <coppeliaAnger agent="self" target="other" value="1" />
            string code = "";
            try
            {
                List<Dictionary<string, string>> bingingsList = new List<Dictionary<string, string>>();
                string[] part = query.Split('(');
                string head = part[0].Trim();
                bot.servitor.prologEngine.askQuery(query, mtName, out bingingsList);
                foreach (Dictionary<string, string> bindings in bingingsList)
                {
                    string frag = "<" + head;
                    foreach (string key in bindings.Keys)
                    {
                        frag += String.Format(" {0}=\"{1}\"", key.ToLower(), bindings[key].Trim());
                    }
                    frag += " />\n";
                    code += frag;
                }
            }
            catch (Exception e)
            {
                Console.WriteLine("Error kbToBTXML '{0}':{1}",  mtName,  e.Message);
            }
            return code;
        }


        public IEnumerable<RunStatus> ProcessCoppeliaAgentFeature(XmlNode myNode)
        {
            // <coppeliaFeature agent="self" feature="good" value="1" />
            // Agent has feature to the degree of value

            RunStatus rs = RunStatus.Success;
            string innerStr = myNode.InnerXml.Trim();
            string cFeature = "good";
            string cAgent = "self";
            string cValue = "0";
            float fValue = 0;
            try
            {
                if (myNode.Attributes["feature"] != null) cFeature = myNode.Attributes["feature"].Value;
                if (myNode.Attributes["agent"] != null) cAgent = myNode.Attributes["agent"].Value;
                if (myNode.Attributes["value"] != null) cValue = myNode.Attributes["value"].Value;
                fValue = float.Parse(cValue);
                int featureID = AgentFeatures.Parse(cFeature);
                if (featureID >=0 )
                {
                    if (bot.servitor.CoppeliaAgentDictionary.ContainsKey(cAgent))
                    {
                        Agent a1 = bot.servitor.CoppeliaAgentDictionary[cAgent];
                        a1.SetFeature(featureID, fValue);
                        Console.WriteLine("coppelia Processed :{0}", myNode.OuterXml);
                    }
                }
            }
            catch
            {
            }
            //int newState = Global.AddState(bState);
            //bot.servitor.CoppeliaStateDictionary[cState] = newState;
            yield return rs;
            yield break;
        }

        public IEnumerable<RunStatus> ProcessCoppeliaFeatureBelief(XmlNode myNode)
        {
            // <coppeliaFeatuerBelief agent="self" feature="good" target="other" value="1" />
            // agent believes that targets feature will facilitate state

            RunStatus rs = RunStatus.Success;
            string innerStr = myNode.InnerXml.Trim();
            string cFeature = "good";
            string cAgent = "self";
            string cTarget = "other";
            string cState = "state";
            string cValue = "0";
            float fValue = 0;
            try
            {
                if (myNode.Attributes["feature"] != null) cFeature = myNode.Attributes["feature"].Value;
                if (myNode.Attributes["agent"] != null) cAgent = myNode.Attributes["agent"].Value;
                if (myNode.Attributes["target"] != null) cTarget = myNode.Attributes["target"].Value;
                if (myNode.Attributes["value"] != null) cValue = myNode.Attributes["value"].Value;
                if (myNode.Attributes["state"] != null) cState = myNode.Attributes["state"].Value;
                fValue = float.Parse(cValue);
                int featureID = AgentFeatures.Parse(cFeature);

                if (featureID >= 0)
                {
                    if (bot.servitor.CoppeliaAgentDictionary.ContainsKey(cAgent))
                    {
                        if (bot.servitor.CoppeliaAgentDictionary.ContainsKey(cTarget))
                        {
                            if (bot.servitor.CoppeliaStateDictionary.ContainsKey(cState))
                            {
                                Agent a1 = bot.servitor.CoppeliaAgentDictionary[cAgent];
                                Agent target = bot.servitor.CoppeliaAgentDictionary[cTarget];
                                int state = bot.servitor.CoppeliaStateDictionary[cState];
                                a1.SetFeatureBelief(featureID, state, target.AgentID, fValue);
                                Console.WriteLine("coppelia Processed :{0}", myNode.OuterXml);
                            }
                        }
                    }
                }
            }
            catch
            {
            }
            //int newState = Global.AddState(bState);
            //bot.servitor.CoppeliaStateDictionary[cState] = newState;
            yield return rs;
            yield break;
        }
        public IEnumerable<RunStatus> ProcessCoppeliaAgentResponsibleBelief(XmlNode myNode)
        {
            // <coppeliaAgentResponsibleBelief agent="self" state="state" target="other" value="1" />
            // agent believes that targets is responsible for state

            RunStatus rs = RunStatus.Success;
            string innerStr = myNode.InnerXml.Trim();
            string cFeature = "good";
            string cAgent = "self";
            string cTarget = "other";
            string cState = "state";
            string cValue = "0";
            float fValue = 0;
            try
            {
                if (myNode.Attributes["agent"] != null) cAgent = myNode.Attributes["agent"].Value;
                if (myNode.Attributes["target"] != null) cTarget = myNode.Attributes["target"].Value;
                if (myNode.Attributes["value"] != null) cValue = myNode.Attributes["value"].Value;
                if (myNode.Attributes["state"] != null) cState = myNode.Attributes["state"].Value;
                fValue = float.Parse(cValue);
                    if (bot.servitor.CoppeliaAgentDictionary.ContainsKey(cAgent))
                    {
                        if (bot.servitor.CoppeliaAgentDictionary.ContainsKey(cTarget))
                        {
                            if (bot.servitor.CoppeliaStateDictionary.ContainsKey(cState))
                            {
                                Agent a1 = bot.servitor.CoppeliaAgentDictionary[cAgent];
                                Agent target = bot.servitor.CoppeliaAgentDictionary[cTarget];
                                int state = bot.servitor.CoppeliaStateDictionary[cState];
                                a1.SetAgentResponsibleBelief(target.AgentID, state,  fValue);
                                Console.WriteLine("coppelia Processed :{0}", myNode.OuterXml);
                            }
                        }
                    }
            }
            catch
            {
            }
            //int newState = Global.AddState(bState);
            //bot.servitor.CoppeliaStateDictionary[cState] = newState;
            yield return rs;
            yield break;
        }
        public IEnumerable<RunStatus> ProcessCoppeliaExpectedSatisfaction(XmlNode myNode)
        {
            // <coppeliaExpectedSatisfaction agent="self" action="act" target="other" value="1" />
            // Agent believes that performing Action to Target will have an expected satisfaction

            RunStatus rs = RunStatus.Success;
            string innerStr = myNode.InnerXml.Trim();
            string cFeature = "good";
            string cAgent = "self";
            string cAction = "action";
            string cTarget = "other";
            string cValue = "0";
            float fValue = 0;
            try
            {
                if (myNode.Attributes["agent"] != null) cAgent = myNode.Attributes["agent"].Value;
                if (myNode.Attributes["value"] != null) cValue = myNode.Attributes["value"].Value;
                if (myNode.Attributes["action"] != null) cAction = myNode.Attributes["state"].Value;
                if (myNode.Attributes["target"] != null) cTarget = myNode.Attributes["target"].Value;
                fValue = float.Parse(cValue);
                if (bot.servitor.CoppeliaAgentDictionary.ContainsKey(cAgent))
                {
                        if (bot.servitor.CoppeliaActionDictionary.ContainsKey(cAction))
                        {
                            Agent a1 = bot.servitor.CoppeliaAgentDictionary[cAgent];
                            Agent target = bot.servitor.CoppeliaAgentDictionary[cTarget];
                            AgentAction action= bot.servitor.CoppeliaActionDictionary[cAction];
                            a1.SetExpectedSatisfaction(target.AgentID, action.GlobalIndex , fValue);
                            Console.WriteLine("coppelia Processed :{0}", myNode.OuterXml);
                        }
                }
            }
            catch
            {
            }
            //int newState = Global.AddState(bState);
            //bot.servitor.CoppeliaStateDictionary[cState] = newState;
            yield return rs;
            yield break;
        }

        public IEnumerable<RunStatus> ProcessCoppeliaAnger(XmlNode myNode)
        {
            // <coppeliaAnger agent="self" target="other" value="1" />
            // agent believes that targets is  Anger worthy

            RunStatus rs = RunStatus.Success;
            string innerStr = myNode.InnerXml.Trim();
            string cAgent = "self";
            string cTarget = "other";
            string cValue = "0";
            float fValue = 0;
            try
            {
                if (myNode.Attributes["agent"] != null) cAgent = myNode.Attributes["agent"].Value;
                if (myNode.Attributes["value"] != null) cValue = myNode.Attributes["value"].Value;
                if (myNode.Attributes["target"] != null) cTarget = myNode.Attributes["target"].Value;
                fValue = float.Parse(cValue);
                if (bot.servitor.CoppeliaAgentDictionary.ContainsKey(cAgent))
                {
                    if (bot.servitor.CoppeliaAgentDictionary.ContainsKey(cTarget))
                    {
                        Agent a1 = bot.servitor.CoppeliaAgentDictionary[cAgent];
                        Agent target = bot.servitor.CoppeliaAgentDictionary[cTarget];
                        a1.SetAnger(target.AgentID, fValue);
                        Console.WriteLine("coppelia Processed :{0}", myNode.OuterXml);
                    }
                }
            }
            catch
            {
            }
            //int newState = Global.AddState(bState);
            //bot.servitor.CoppeliaStateDictionary[cState] = newState;
            yield return rs;
            yield break;
        }
        public IEnumerable<RunStatus> ProcessCoppeliaPraiseworthy(XmlNode myNode)
        {
            // <coppeliaPraiseworthy agent="self" target="other" value="1" />
            // agent believes that targets is Praiseworthy

            RunStatus rs = RunStatus.Success;
            string innerStr = myNode.InnerXml.Trim();
            string cAgent = "self";
            string cTarget = "other";
            string cValue = "0";
            float fValue = 0;
            try
            {
                if (myNode.Attributes["agent"] != null) cAgent = myNode.Attributes["agent"].Value;
                if (myNode.Attributes["value"] != null) cValue = myNode.Attributes["value"].Value;
                if (myNode.Attributes["target"] != null) cTarget = myNode.Attributes["target"].Value;
                fValue = float.Parse(cValue);
                if (bot.servitor.CoppeliaAgentDictionary.ContainsKey(cAgent))
                {
                    if (bot.servitor.CoppeliaAgentDictionary.ContainsKey(cTarget))
                    {
                        Agent a1 = bot.servitor.CoppeliaAgentDictionary[cAgent];
                        Agent target = bot.servitor.CoppeliaAgentDictionary[cTarget];
                        a1.SetPraiseworthy(target.AgentID, fValue);
                        Console.WriteLine("coppelia Processed :{0}", myNode.OuterXml);
                    }
                }
            }
            catch
            {
            }
            //int newState = Global.AddState(bState);
            //bot.servitor.CoppeliaStateDictionary[cState] = newState;
            yield return rs;
            yield break;
        }
        public IEnumerable<RunStatus> ProcessCoppeliaEmotion(XmlNode myNode)
        {
            // <coppeliaEmotion agent="self" emotion="joy" value="1" />
            // agent feels Emotion to degree value

            RunStatus rs = RunStatus.Success;
            string innerStr = myNode.InnerXml.Trim();
            string cAgent = "self";
            string cEmotion = "joy";
            string cValue = "0";
            float fValue = 0;
            try
            {
                if (myNode.Attributes["agent"] != null) cAgent = myNode.Attributes["agent"].Value;
                if (myNode.Attributes["value"] != null) cValue = myNode.Attributes["value"].Value;
                if (myNode.Attributes["emotion"] != null) cEmotion = myNode.Attributes["emotion"].Value;
                fValue = float.Parse(cValue);
                int iEmotion = AgentEmotions.Parse(cEmotion);
                if (bot.servitor.CoppeliaAgentDictionary.ContainsKey(cAgent))
                {
                    if (iEmotion >=0 )
                    {
                        Agent a1 = bot.servitor.CoppeliaAgentDictionary[cAgent];
                        a1.SetEmotion(iEmotion, fValue);
                        Console.WriteLine("coppelia Processed :{0}", myNode.OuterXml);
                    }
                }
            }
            catch
            {
            }
            //int newState = Global.AddState(bState);
            //bot.servitor.CoppeliaStateDictionary[cState] = newState;
            yield return rs;
            yield break;
        }

        public IEnumerable<RunStatus> ProcessCoppeliaDesired(XmlNode myNode)
        {
            // <coppeliaDesired agent="self" emotion="joy" value="1" />
            // agent WANTS to feel Emotion to degree value

            RunStatus rs = RunStatus.Success;
            string innerStr = myNode.InnerXml.Trim();
            string cAgent = "self";
            string cEmotion = "joy";
            string cValue = "0";
            float fValue = 0;
            try
            {
                if (myNode.Attributes["agent"] != null) cAgent = myNode.Attributes["agent"].Value;
                if (myNode.Attributes["value"] != null) cValue = myNode.Attributes["value"].Value;
                if (myNode.Attributes["emotion"] != null) cEmotion = myNode.Attributes["emotion"].Value;
                fValue = float.Parse(cValue);
                int iEmotion = AgentEmotions.Parse(cEmotion);
                if (bot.servitor.CoppeliaAgentDictionary.ContainsKey(cAgent))
                {
                    if (iEmotion >= 0)
                    {
                        Agent a1 = bot.servitor.CoppeliaAgentDictionary[cAgent];
                        a1.SetDesired(iEmotion, fValue);
                        Console.WriteLine("coppelia Processed :{0}", myNode.OuterXml);
                    }
                }
            }
            catch
            {
            }
            //int newState = Global.AddState(bState);
            //bot.servitor.CoppeliaStateDictionary[cState] = newState;
            yield return rs;
            yield break;
        }
        public IEnumerable<RunStatus> ProcessCoppeliaActionTendancy(XmlNode myNode)
        {
            // <coppeliaActionTendancy agent="self" action="act" target="other" value="1" />
            // Agent has a tendency to degree value to perform Action to Target

            RunStatus rs = RunStatus.Success;
            string innerStr = myNode.InnerXml.Trim();
            string cAgent = "self";
            string cTarget = "other";
            string cAction = "act";
            string cValue = "0";
            float fValue = 0;
            try
            {
                if (myNode.Attributes["agent"] != null) cAgent = myNode.Attributes["agent"].Value;
                if (myNode.Attributes["target"] != null) cTarget = myNode.Attributes["target"].Value;
                if (myNode.Attributes["action"] != null) cAction = myNode.Attributes["action"].Value;
                if (myNode.Attributes["value"] != null) cValue = myNode.Attributes["value"].Value;
                fValue = float.Parse(cValue);
                
                if (bot.servitor.CoppeliaAgentDictionary.ContainsKey(cAgent))
                {
                    if (bot.servitor.CoppeliaAgentDictionary.ContainsKey(cTarget))
                    {
                        if (bot.servitor.CoppeliaActionDictionary.ContainsKey(cAction))
                        {
                            Agent a1 = bot.servitor.CoppeliaAgentDictionary[cAgent];
                            Agent a2 = bot.servitor.CoppeliaAgentDictionary[cTarget];
                            AgentAction act = bot.servitor.CoppeliaActionDictionary[cAction];
                            a1.SetAT(a2.AgentID, act.GlobalIndex, fValue);
                            Console.WriteLine("coppelia Processed :{0}", myNode.OuterXml);
                        }
                    }
                }
            }
            catch
            {
            }
            //int newState = Global.AddState(bState);
            //bot.servitor.CoppeliaStateDictionary[cState] = newState;
            yield return rs;
            yield break;
        }
  
        public IEnumerable<RunStatus> ProcessCoppeliaFeatureEmotionBelief(XmlNode myNode)
        {
            // <coppeliaFeatureEmotionBelief agent="self" feature="good" emotion="joy" value="1" />
            // agent believes that having feature will facilitate Emotion

            RunStatus rs = RunStatus.Success;
            string innerStr = myNode.InnerXml.Trim();
            string cFeature = "good";
            string cEmotion = "joy";
            string cAgent = "self";
            string cValue = "0";
            float fValue = 0;
            try
            {
                if (myNode.Attributes["agent"] != null) cAgent = myNode.Attributes["agent"].Value;
                if (myNode.Attributes["feature"] != null) cFeature = myNode.Attributes["feature"].Value;
                if (myNode.Attributes["emotion"] != null) cEmotion = myNode.Attributes["emotion"].Value;

                fValue = float.Parse(cValue);
                int iEmotion = AgentEmotions.Parse(cEmotion);
                int iFeature = AgentFeatures.Parse(cFeature);

                if (bot.servitor.CoppeliaAgentDictionary.ContainsKey(cAgent))
                {
                    if (iEmotion >=0)
                    {
                        if (iFeature>=0)
                        {
                            Agent a1 = bot.servitor.CoppeliaAgentDictionary[cAgent];
                            a1.SetFeatureEmotionBelief(iFeature,iEmotion, fValue);
                            Console.WriteLine("coppelia Processed :{0}", myNode.OuterXml);
                        }
                    }
                }
            }
            catch
            {
            }
            //int newState = Global.AddState(bState);
            //bot.servitor.CoppeliaStateDictionary[cState] = newState;
            yield return rs;
            yield break;
        }

        public IEnumerable<RunStatus> ProcessCoppeliaAction(XmlNode myNode)
        {
            // <coppeliaAction state="act" positivity="0" negativity="0" />
            // defines an Action along with its positive and negative valance

            RunStatus rs = RunStatus.Success;
            string innerStr = myNode.InnerXml.Trim();
            string cAction = "actionUnknown";
            string cPositivity = "0";
            string cNegativity = "0";
            float fPositivity = 0;
            float fNegativity = 0;

            try
            {
                if (myNode.Attributes["action"] != null) cAction = myNode.Attributes["action"].Value;
                if (myNode.Attributes["positivity"] != null) cPositivity = myNode.Attributes["positivity"].Value;
                if (myNode.Attributes["negativity"] != null) cNegativity = myNode.Attributes["negativity"].Value;
                fPositivity = float.Parse(cPositivity);
                fNegativity = float.Parse(cNegativity);
                if (!bot.servitor.CoppeliaActionDictionary.ContainsKey(cAction))
                {
                    AgentAction newAction = new AgentAction(cAction, fPositivity, fNegativity);
                    bot.servitor.CoppeliaActionDictionary[cAction] = newAction;
                }
                else
                {
                    bot.servitor.CoppeliaActionDictionary[cAction].SetValence(fPositivity, fNegativity);
                }
                Console.WriteLine("coppelia Processed :{0}", myNode.OuterXml);
            }
            catch
            {
            }
            yield return rs;
            yield break;
        }

        public IEnumerable<RunStatus> ProcessCoppeliaState(XmlNode myNode)
        {
            // <coppeliaState state="act" initial="true/false" />
            // defines a state and its initial truth value

            RunStatus rs = RunStatus.Success;
            string innerStr = myNode.InnerXml.Trim();
            string cState = "stateUnknown";
            string cInitState = "false";
            bool bState = false;

            try
            {
                if (myNode.Attributes["state"] != null) cState = myNode.Attributes["state"].Value;
                if (myNode.Attributes["initial"] != null) cInitState = myNode.Attributes["initial"].Value;
                bState = bool.Parse(cInitState);
                if (!bot.servitor.CoppeliaStateDictionary.ContainsKey(cState))
                {
                    int newState = Global.AddState(bState);
                    bot.servitor.CoppeliaStateDictionary[cState] = newState;
                    Console.WriteLine("coppelia Processed :{0}", myNode.OuterXml);
                }
            }
            catch
            {
            }
            yield return rs;
            yield break;
        }



        public IEnumerable<RunStatus> ProcessCoppeliaStateLikelihood(XmlNode myNode)
        {
            // <coppeliaStateLikelihood agent="self" state="state1" likelihood="0.5" />
            // sets a states likelihood

            RunStatus rs = RunStatus.Success;
            string innerStr = myNode.InnerXml.Trim();
            string cAgent = "self";
            string cState = "stateUnknown";
            string cStateProb = "0.5";
            float pStateProb = (float)0.5;

            try
            {
                if (myNode.Attributes["agent"] != null) cAgent = myNode.Attributes["agent"].Value;

                if (myNode.Attributes["state"] != null) cState = myNode.Attributes["state"].Value;
                if (myNode.Attributes["likelihood"] != null) cStateProb = myNode.Attributes["likelihood"].Value;
                pStateProb = float.Parse(cStateProb);
                if (bot.servitor.CoppeliaStateDictionary.ContainsKey(cState))
                {
                    if (bot.servitor.CoppeliaAgentDictionary.ContainsKey(cAgent))
                    {
                        Agent a1 = bot.servitor.CoppeliaAgentDictionary[cAgent];
                        int state = bot.servitor.CoppeliaStateDictionary[cState];
                        a1.SetStateLikelihood(state, pStateProb);
                        Console.WriteLine("coppelia Processed :{0}", myNode.OuterXml);
                    }
                }
            }
            catch
            {
            }
            yield return rs;
            yield break;
        }

        public IEnumerable<RunStatus> ProcessCoppeliaResponse(XmlNode myNode)
        {
            // <coppeliaResponse action="act" response="reaction" />
            // reaction can follow an agent receiving act

            RunStatus rs = RunStatus.Success;
            string innerStr = myNode.InnerXml.Trim();
            string cAct = "actAction";
            string cResponse = "actReaction";

            try
            {
                if (myNode.Attributes["action"] != null) cAct = myNode.Attributes["action"].Value;
                if (myNode.Attributes["response"] != null) cResponse = myNode.Attributes["response"].Value;

                if (bot.servitor.CoppeliaActionDictionary.ContainsKey(cAct))
                {
                    if (bot.servitor.CoppeliaActionDictionary.ContainsKey(cResponse))
                    {
                        AgentAction a1 = bot.servitor.CoppeliaActionDictionary[cAct];
                        AgentAction a2 = bot.servitor.CoppeliaActionDictionary[cResponse];
                        a1.AddResponse(a2.GlobalIndex);
                        Console.WriteLine("coppelia Processed :{0}", myNode.OuterXml);
                    }
                }
            }
            catch
            {
            }
            //int newState = Global.AddState(bState);
            //bot.servitor.CoppeliaStateDictionary[cState] = newState;
            yield return rs;
            yield break;
        }

        public IEnumerable<RunStatus> ProcessCoppeliaAmbition(XmlNode myNode)
        {
            // <coppeliaAmbition agent="self" state="targetstate" value="1" />
            // Agent wants state to occur or not occur

            RunStatus rs = RunStatus.Success;
            string innerStr = myNode.InnerXml.Trim();
            string cState = "state";
            string cAgent = "self";
            string cValue = "0";
            float fValue = 0;
            try
            {
                if (myNode.Attributes["state"] != null) cState = myNode.Attributes["state"].Value;
                if (myNode.Attributes["agent"] != null) cAgent = myNode.Attributes["agent"].Value;
                if (myNode.Attributes["value"] != null) cValue = myNode.Attributes["value"].Value;
                fValue = float.Parse(cValue);

                if (bot.servitor.CoppeliaStateDictionary.ContainsKey(cState))
                {
                    if (bot.servitor.CoppeliaAgentDictionary.ContainsKey(cAgent))
                    {
                        Agent a1 = bot.servitor.CoppeliaAgentDictionary[cAgent];
                        int state = bot.servitor.CoppeliaStateDictionary[cState];
                        a1.AddAmbition(state, fValue);
                        Console.WriteLine("coppelia Processed :{0}", myNode.OuterXml);
                    }
                }
            }
            catch
            {
            }
            //int newState = Global.AddState(bState);
            //bot.servitor.CoppeliaStateDictionary[cState] = newState;
            yield return rs;
            yield break;
        }
// Coppelia Morals
        public IEnumerable<RunStatus> ProcessCoppeliaMoral(XmlNode myNode)
        {
            // <coppeliaMoral moral="act" initial="true/false" />
            // defines a moral value and its initial state value

            RunStatus rs = RunStatus.Success;
            string innerStr = myNode.InnerXml.Trim();
            string cMoral = "stateUnknown";
            string cInitState = "false";
            bool bState = false;

            try
            {
                if (myNode.Attributes["moral"] != null) cMoral = myNode.Attributes["moral"].Value;
                if (myNode.Attributes["initial"] != null) cInitState = myNode.Attributes["initial"].Value;
                bState = bool.Parse(cInitState);
                if (!bot.servitor.CoppeliaMoralsDictionary.ContainsKey(cMoral))
                {
                    int newMoral = Global.AddMoralPrinciple(bState);
                    bot.servitor.CoppeliaMoralsDictionary[cMoral] = newMoral;
                    Console.WriteLine("coppelia Processed :{0}", myNode.OuterXml);
                }
            }
            catch
            {
            }
            yield return rs;
            yield break;
        }

        public IEnumerable<RunStatus> ProcessCoppeliaMoralAmbition(XmlNode myNode)
        {
            // <coppeliaMoralAmbition agent="self" moral="moral" value="1" />
            // Agent wants Moral to have Value

            RunStatus rs = RunStatus.Success;
            string innerStr = myNode.InnerXml.Trim();
            string cMoral = "moral";
            string cAgent = "self";
            string cValue = "0";
            float fValue = 0;
            try
            {
                if (myNode.Attributes["moral"] != null) cMoral = myNode.Attributes["moral"].Value;
                if (myNode.Attributes["agent"] != null) cAgent = myNode.Attributes["agent"].Value;
                if (myNode.Attributes["value"] != null) cValue = myNode.Attributes["value"].Value;
                fValue = float.Parse(cValue);

                if (bot.servitor.CoppeliaMoralsDictionary.ContainsKey(cMoral))
                {
                    if (bot.servitor.CoppeliaAgentDictionary.ContainsKey(cAgent))
                    {
                        Agent a1 = bot.servitor.CoppeliaAgentDictionary[cAgent];
                        int moral = bot.servitor.CoppeliaStateDictionary[cMoral];
                        a1.AddMoralAmbition(moral, fValue);
                        Console.WriteLine("coppelia Processed :{0}", myNode.OuterXml);
                    }
                }
            }
            catch
            {
            }
            //int newState = Global.AddState(bState);
            //bot.servitor.CoppeliaStateDictionary[cState] = newState;
            yield return rs;
            yield break;
        }
        public IEnumerable<RunStatus> ProcessCoppeliaActionMoralBelief(XmlNode myNode)
        {
            // <coppeliaActionMoralBelief agent="self" act="act1" moral="s2"  value="1" />
            // Agent believes Act will cause Moral to have Value

            RunStatus rs = RunStatus.Success;
            string innerStr = myNode.InnerXml.Trim();
            string cAct = "act";
            string cMoral = "moral";
            string cAgent = "self";
            string cValue = "0";
            float fValue = 0;
            try
            {
                if (myNode.Attributes["action"] != null) cAct = myNode.Attributes["action"].Value;
                if (myNode.Attributes["moral"] != null) cMoral = myNode.Attributes["moral"].Value;
                if (myNode.Attributes["agent"] != null) cAgent = myNode.Attributes["agent"].Value;
                if (myNode.Attributes["value"] != null) cValue = myNode.Attributes["value"].Value;
                fValue = float.Parse(cValue);

                if (bot.servitor.CoppeliaMoralsDictionary.ContainsKey(cMoral))
                {
                    if (bot.servitor.CoppeliaAgentDictionary.ContainsKey(cAgent))
                    {
                        if (bot.servitor.CoppeliaActionDictionary.ContainsKey(cAct))
                        {
                            AgentAction act = bot.servitor.CoppeliaActionDictionary[cAct];
                            Agent a1 = bot.servitor.CoppeliaAgentDictionary[cAgent];
                            int moral = bot.servitor.CoppeliaMoralsDictionary[cMoral];
                            a1.SetActionMoralPrincipleBelief(act.GlobalIndex, moral, fValue);
                            Console.WriteLine("coppelia Processed :{0}", myNode.OuterXml);
                        }
                    }
                }
            }
            catch
            {
            }
            //int newState = Global.AddState(bState);
            //bot.servitor.CoppeliaStateDictionary[cState] = newState;
            yield return rs;
            yield break;
        }
        public IEnumerable<RunStatus> ProcessCoppeliaActionState(XmlNode myNode)
        {
            // <coppeliaActionState agent="self" action="act1" state="s2"  value="1" />
            // Agent believes Act will cause state with prob v

            RunStatus rs = RunStatus.Success;
            string innerStr = myNode.InnerXml.Trim();
            string cAct = "act";
            string cState = "state";
            string cAgent = "self";
            string cValue = "0";
            float fValue = 0;
            try
            {
                if (myNode.Attributes["action"] != null) cAct = myNode.Attributes["action"].Value;
                if (myNode.Attributes["state"] != null) cState = myNode.Attributes["state"].Value;
                if (myNode.Attributes["agent"] != null) cAgent = myNode.Attributes["agent"].Value;
                if (myNode.Attributes["value"] != null) cValue = myNode.Attributes["value"].Value;
                fValue = float.Parse(cValue);

                if (bot.servitor.CoppeliaStateDictionary.ContainsKey(cState))
                {
                    if (bot.servitor.CoppeliaAgentDictionary.ContainsKey(cAgent))
                    {
                        if (bot.servitor.CoppeliaActionDictionary.ContainsKey(cAct))
                        {
                            AgentAction act = bot.servitor.CoppeliaActionDictionary[cAct];
                            Agent a1 = bot.servitor.CoppeliaAgentDictionary[cAgent];
                            int state = bot.servitor.CoppeliaStateDictionary[cState];
                            a1.SetActionStateBelief(act.GlobalIndex ,state, fValue);
                            Console.WriteLine("coppelia Processed :{0}", myNode.OuterXml);
                        }
                    }
                }
            }
            catch
            {
            }
            //int newState = Global.AddState(bState);
            //bot.servitor.CoppeliaStateDictionary[cState] = newState;
            yield return rs;
            yield break;
        }

        public IEnumerable<RunStatus> ProcessCoppeliaStateState(XmlNode myNode)
        {
            // <coppeliaStateState agent="self" statesrc="s1" statedest="s2"  value="1" />
            // Agent believes StateSRC will facilitate StateDEST with probability Value

            RunStatus rs = RunStatus.Success;
            string innerStr = myNode.InnerXml.Trim();
            string cStateSrc = "state1";
            string cStateDest = "state2";
            string cAgent = "self";
            string cValue = "0";
            float fValue = 0;
            try
            {
                if (myNode.Attributes["statesrc"] != null) cStateSrc = myNode.Attributes["state"].Value;
                if (myNode.Attributes["statedest"] != null) cStateDest = myNode.Attributes["state"].Value;
                if (myNode.Attributes["agent"] != null) cAgent = myNode.Attributes["agent"].Value;
                if (myNode.Attributes["value"] != null) cValue = myNode.Attributes["value"].Value;
                fValue = float.Parse(cValue);

                if (bot.servitor.CoppeliaStateDictionary.ContainsKey(cStateSrc))
                {
                    if (bot.servitor.CoppeliaAgentDictionary.ContainsKey(cAgent))
                    {
                        if (bot.servitor.CoppeliaStateDictionary.ContainsKey(cStateDest))
                        {
                            Agent a1 = bot.servitor.CoppeliaAgentDictionary[cAgent];
                            int stateSrc = bot.servitor.CoppeliaStateDictionary[cStateSrc];
                            int stateDest = bot.servitor.CoppeliaStateDictionary[cStateDest];
                            a1.SetStateFacStateBelief(stateSrc, stateDest, fValue);
                            Console.WriteLine("coppelia Processed :{0}", myNode.OuterXml);
                        }
                    }
                }
            }
            catch
            {
            }
            //int newState = Global.AddState(bState);
            //bot.servitor.CoppeliaStateDictionary[cState] = newState;
            yield return rs;
            yield break;
        }

        public IEnumerable<RunStatus> ProcessCoppeliaPerform(XmlNode myNode)
        {
            // <coppeliaPerform agent="self" action="a1" recipient="other" />
            // Agent just performed act to recipient

            RunStatus rs = RunStatus.Success;
            string innerStr = myNode.InnerXml.Trim();
            string cAct = "actAction";
            string cAgent = "agent";
            string cRecipient = "reciptient";

            try
            {
                if (myNode.Attributes["action"] != null) cAct = myNode.Attributes["action"].Value;
                if (myNode.Attributes["agent"] != null) cAgent = myNode.Attributes["agent"].Value;
                if (myNode.Attributes["reciptient"] != null) cRecipient = myNode.Attributes["reciptient"].Value;

                if (bot.servitor.CoppeliaActionDictionary.ContainsKey(cAct))
                {
                    if (bot.servitor.CoppeliaAgentDictionary.ContainsKey(cAgent))
                    {
                        if (bot.servitor.CoppeliaAgentDictionary.ContainsKey(cRecipient))
                        {
                            AgentAction act = bot.servitor.CoppeliaActionDictionary[cAct];
                            Agent agent = bot.servitor.CoppeliaAgentDictionary[cAgent];
                            Agent recipent = bot.servitor.CoppeliaAgentDictionary[cRecipient];
                            agent.ManualPerform(act, recipent);
                            Console.WriteLine("coppelia Processed :{0}", myNode.OuterXml);
                        }
                        else
                        {
                            Console.WriteLine("!bot.servitor.CoppeliaAgentDictionary.ContainsKey(cRecipient)");
                        }
                    }
                    else
                    {
                        Console.WriteLine("!bot.servitor.CoppeliaAgentDictionary.ContainsKey(cAgent)");
                    }
                }
                else
                {
                    Console.WriteLine("!bot.servitor.CoppeliaActionDictionary.ContainsKey(cAct)");
                }

            }
            catch
            {
            }
            //int newState = Global.AddState(bState);
            //bot.servitor.CoppeliaStateDictionary[cState] = newState;
            yield return rs;
            yield break;
        }


        public IEnumerable<RunStatus> ProcessCoppeliaRelation(XmlNode myNode)
        {
            // <coppeliaRelation agent="self" relation="r1" recipient="other" />
            // Agent has Relation with Recipient

            RunStatus rs = RunStatus.Success;
            string innerStr = myNode.InnerXml.Trim();
            string cRelation = "relation";
            string cAgent = "agent";
            string cRecipient = "reciptient";
            string cValue = "0";
            float fValue = 0;

            try
            {
                if (myNode.Attributes["relation"] != null) cRelation = myNode.Attributes["relation"].Value;
                if (myNode.Attributes["agent"] != null) cAgent = myNode.Attributes["agent"].Value;
                if (myNode.Attributes["reciptient"] != null) cRecipient = myNode.Attributes["reciptient"].Value;
                if (myNode.Attributes["value"] != null) cValue = myNode.Attributes["value"].Value;
                fValue = float.Parse(cValue);
                int relationID = AgentRelations.Parse(cRelation);

                if (relationID >=0 )
                {
                    if (bot.servitor.CoppeliaAgentDictionary.ContainsKey(cAgent))
                    {
                        if (bot.servitor.CoppeliaAgentDictionary.ContainsKey(cRecipient))
                        {
                            Agent agent = bot.servitor.CoppeliaAgentDictionary[cAgent];
                            Agent recipent = bot.servitor.CoppeliaAgentDictionary[cRecipient];
                            agent.SetRelation(recipent.AgentID,relationID,fValue);
                            Console.WriteLine("coppelia Processed :{0}", myNode.OuterXml);

                        }
                    }
                }

            }
            catch
            {
            }
            //int newState = Global.AddState(bState);
            //bot.servitor.CoppeliaStateDictionary[cState] = newState;
            yield return rs;
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
        public IEnumerable<RunStatus> ProcessAssertCoppelia(XmlNode myNode)
        {
            string condition = myNode.Attributes["cond"].Value;
            string parameters = myNode.InnerText;
            string condData = myNode.Attributes["cond"] == null ? null : myNode.Attributes["cond"].Value;
            string[] parms = condData.Trim().Split(' ');
            string varName = parms[0];
            string rel = "";
            string val = "";
            double dval = 0;
            Int32 elapsedTime = Environment.TickCount - transitionTime;

            string cAgent = "agent";
            string cRelation = "null";
            string cRecipient = "agent";
            string cAct = "act";
            string cState = "state";
            string cFeature = "good";
            string cEmotion = "joy";

            if (myNode.Attributes["agent"] != null) cAgent = myNode.Attributes["agent"].Value;
            if (myNode.Attributes["relation"] != null) cRelation = myNode.Attributes["relation"].Value;
            if (myNode.Attributes["reciptient"] != null) cRecipient = myNode.Attributes["reciptient"].Value;
            if (myNode.Attributes["action"] != null) cAct = myNode.Attributes["action"].Value;
            if (myNode.Attributes["state"] != null) cState = myNode.Attributes["state"].Value;
            if (myNode.Attributes["feature"] != null) cFeature = myNode.Attributes["feature"].Value;
            if (myNode.Attributes["emotion"] != null) cEmotion = myNode.Attributes["emotion"].Value;
            Agent a1 = null;
            Agent a2 = null;
            int iRelation = AgentRelations.Parse(cRelation);
            int iFeature = AgentFeatures.Parse(cFeature);
            int iEmotions = AgentRelations.Parse(cEmotion);

            if (bot.servitor.CoppeliaAgentDictionary.ContainsKey(cAgent))
            {
               a1 =bot.servitor.CoppeliaAgentDictionary[cAgent];
            }
            if (bot.servitor.CoppeliaAgentDictionary.ContainsKey(cRecipient))
            {
                a2 = bot.servitor.CoppeliaAgentDictionary[cRecipient];
            }

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

            if (varName == "angerat") bbVal = a1.GetAnger(a2.AgentID);
            if (varName == "emotion") bbVal = a1.GetEmotion(iEmotions);


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
            Console.WriteLine("coppelia Processed :{0}", myNode.OuterXml);

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

        #endregion

        // <loadchatmaper path="chatmapper\example.xml"/>
        public IEnumerable<RunStatus> ProcessLoadChatMapper(XmlNode myNode)
        {
            // Append some si_text 
            RunStatus rs = RunStatus.Failure;
            string innerStr = myNode.InnerXml.Trim();
            string srcFile = @"chatmapper\Example.xml";

            try
            {
                if (myNode.Attributes["path"] != null) srcFile = myNode.Attributes["path"].Value;
                ChatToBTXML myTranslator = new ChatToBTXML();
                myTranslator.defineChatTreeFile(srcFile);
                string myCodes = myTranslator.btxmlCode;
                XmlDocument chatDoc = new XmlDocument ();
                string destFile = srcFile.Replace(".xml", ".btxml");
                System.IO.File.WriteAllText(destFile, myCodes);
                chatDoc.LoadXml (myCodes);
                bot.loadAIMLFromXML(chatDoc, srcFile +DateTime.Now.ToString());

                rs = RunStatus.Success;
            }
            catch (Exception e)
            {
                Console.WriteLine("Error ProcessDefineState '{0}','{1}':{2}", srcFile, innerStr, e.Message);
                rs = RunStatus.Failure;
            }
            yield return rs;
            yield break;
        }


        public IEnumerable<RunStatus> ProcessAssertMenu(XmlNode myNode)
        {
            string condition = myNode.Attributes["cond"].Value;
            string parameters = myNode.InnerText;
            //if no input exists doesn't exist then return failure
            if (bot.chatInputQueue.Count==0)
              {
                yield return RunStatus.Failure;
                yield break;
              }

            string userInput = bot.chatInputQueue.Peek();
            string[] userWords = userInput.Split(' ');
            string[] condWords = condition.Split(' ');

            RunStatus r = RunStatus.Success;
            try
            {
                // for each condition sense, scan user words to see if any match
                //  if you can find a match for all the condition senses then it matches
                //  otherwise the assert fails
                foreach (string condWord in condWords)
                {
                    bool goodMatch = false;
                    foreach (string userWord in userWords)
                    {
                        if (matchesWildSense (condWord,userWord)){ goodMatch =true; break;}
                    }
                    if (goodMatch == false)
                    {
                         r = RunStatus.Failure ;
                         break;
                    }
                }

            }
            catch (Exception e)
            {
                Console.WriteLine("Error ProcessAssertGuest '{0}':{1}", condition, e.Message);
                r = RunStatus.Failure;
            }
            if (r == RunStatus.Success)
            {
                // if we match then consume the input
                bot.chatInputQueue.Dequeue();
            }
            yield return r;
            yield break;
        }

         bool matchesWildSense(string sense, string queryWord)//, Request request)
        {
            // always clip off the first "*";
            if (sense.StartsWith("*")) sense = sense.Substring(1);
            if (sense.Length == 0) return false;
            //AltBot contextBot = request.bot;
            AltBot contextBot = bot;

            // ported from unifying AIML bot's <lexis> tag
            // form is "*<word>:<relation>:<part-of-speech>:<+/~>"
            // <word> can be a regular expression 
            // Should support "*canine", "*canine:hypo","*canine:hypo:n"
            // and "*can::v" 
            // Also POS can be "bot" or "user" for reference to the predicates
            // so  "*location::bot" or "*girlfriend::user"
            // can also have negation, 
            //   for "*girfriend::user:~" or or "*disease::n:~"
            // default is POS=noun, Relations = InstanceHypernym +Hypernym
            string[] senseArgs = sense.ToLower().Split(':');
            string wnWord = "";
            string wnRelation = "";
            string wnPos = "";
            bool negation = false;

            wnWord = senseArgs[0];
            if (senseArgs.Length >= 2) wnRelation = senseArgs[1];
            if (senseArgs.Length >= 3) wnPos = senseArgs[2];
            if (senseArgs.Length >= 4) negation = senseArgs[3].ToLower().Contains("~");

            Console.WriteLine("MWS:{0},{1},{2},{3} [{4}]", wnWord, wnRelation, wnPos, negation, queryWord);

            // Can you find a match inside (using regex while we're here)?
            var matcher = new Regex(wnWord);
            if (matcher.IsMatch(queryWord))
            {
                Console.WriteLine("MWS:Regex Match");
                return (true ^ negation);
            }

            // bot settings check

            if (wnPos == "bot")
            {
                string val = contextBot.GlobalSettings.grabSetting(wnWord);
                if (val == null) return (false ^ negation);
                if (queryWord.ToLower().Contains(val.ToLower()))
                {
                    Console.WriteLine("MWS:bot pred Match");
                    return (true ^ negation);
                }
                return (false ^ negation);
            }

            if (wnPos == "user")
            {
                //string val = request.user.Predicates.grabSetting(wnWord);
                string val = bot.lastBehaviorUser.Predicates.grabSetting(wnWord); ; //request.user.Predicates.grabSetting(wnWord);
                if (val == null) return (false ^ negation);
                if (queryWord.ToLower().Contains(val.ToLower()))
                {
                    Console.WriteLine("MWS:user pred Match");
                    return (true ^ negation);
                }
                return (false ^ negation);
            }

            // Ok, lets try WordNet
            //if ((contextBot == null) || (contextBot.wordNetEngine == null)) return (false ^ negation);

            // NO ENGINE == JUST PLAIN FALSE (unknowable == false)
            if ((contextBot == null) || (contextBot.wordNetEngine == null)) return (false);

            WordNetEngine ourWordNetEngine = contextBot.wordNetEngine;
            Set<SynSet> synPatternSet = null;
            // find our POS domain if possible
            WordNetEngine.POS ourPOS = WordNetEngine.POS.Noun;
            if (wnPos.Length > 0)
            {
                // populate POS list
                foreach (WordNetEngine.POS p in Enum.GetValues(typeof(WordNetEngine.POS)))
                    if (p != WordNetEngine.POS.None)
                    {
                        if (p.ToString().ToLower().Equals(wnPos) || p.ToString().ToLower().StartsWith(wnPos))
                        {
                            ourPOS = p;
                        }
                    }
            }
            try { synPatternSet = ourWordNetEngine.GetSynSets(wnWord, ourPOS); }
            catch (Exception)
            {
                return (false ^ negation);
            }
            if (synPatternSet.Count == 0)
            {
                try { synPatternSet = ourWordNetEngine.GetSynSets(wnWord.ToLower(), ourPOS); }
                catch (Exception)
                {
                    return (false ^ negation);
                }

            }

            Set<SynSet> synInputSet = null;
            try { synInputSet = ourWordNetEngine.GetSynSets(queryWord, ourPOS); }
            catch (Exception)
            {
                return (false ^ negation);
            }
            if (synInputSet.Count == 0)
            {
                try { synInputSet = ourWordNetEngine.GetSynSets(queryWord.ToLower(), ourPOS); }
                catch (Exception)
                {
                    return (false ^ negation);
                }

            }


            List<WordNetEngine.SynSetRelation> vlist = new List<WordNetEngine.SynSetRelation>(); //[2];
            //vlist[0] = WordNetEngine.SynSetRelation.Hyponym;
            //vlist[1] = WordNetEngine.SynSetRelation.InstanceHyponym;
            //vlist[0] = WordNetEngine.SynSetRelation.Hypernym ;
            //vlist[1] = WordNetEngine.SynSetRelation.InstanceHypernym;
            if (wnRelation.Length == 0)
            {
                vlist.Add(WordNetEngine.SynSetRelation.Hypernym);
                vlist.Add(WordNetEngine.SynSetRelation.InstanceHypernym);
            }
            else
            {
                // populate Relation list
                foreach (WordNetEngine.SynSetRelation r in Enum.GetValues(typeof(WordNetEngine.SynSetRelation)))
                    if (r != WordNetEngine.SynSetRelation.None)
                    {
                        if (r.ToString().ToLower().Contains(wnRelation))
                        {
                            vlist.Add(r);
                        }
                    }

            }

            if ((synInputSet.Count > 0) && (synPatternSet.Count > 0))
            {
                foreach (SynSet synDstSet in synInputSet)
                {
                    foreach (SynSet synSrcSet in synPatternSet)
                    {
                        //synSets.Items.Add(synSet);
                        List<SynSet> linkageList = null;

                        linkageList = synDstSet.GetShortestPathTo(synSrcSet, vlist);
                        if ((linkageList != null) && (linkageList.Count > 0))
                        {
                            Console.WriteLine("MWS:WordNetMatch Match");
                            foreach (SynSet link in linkageList)
                            {
                                Console.WriteLine("MWS: link({0})", link.ToString());
                            }
                            return (true ^ negation);
                        }
                    }
                }
                return (false ^ negation);
            }


            return (false ^ negation);
        }


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
    #region chatmapper
    /******************************************************************************************
        ChatMapper XML to BTXML importer -- Copyright (c) 2012,Kino Coursey, Daxtron Labs

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

    class ChatToBTXML
    {
        public XmlDocument treeDoc;
        public string btxmlCode = "";

        public string chatXML = "";
        bool inDialog = false;
        string precode = "";
        string midcode = "";
        string postcode = "";
        string inputwait = "";
        string choicetag = "selector";
        string processas = "sequence";

        public ChatToBTXML()
        {
            treeDoc = new XmlDocument();

        }

        public void defineChatTreeFile(string filename)
        {
            string chatXML = System.IO.File.ReadAllText(filename);
            btxmlCode = "";
            //btxmlCode += "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n";
            btxmlCode += "<aiml version=\"1.0\">\n";
            btxmlCode += " <state name=\"*\">\n";

            treeDoc.LoadXml(chatXML);
            foreach (XmlNode childNode in treeDoc.ChildNodes)
            {
                processNode(childNode);
            }
            btxmlCode += " </state>\n";
            btxmlCode += "</aiml>\n";

        }

        public void processNode(XmlNode myNode)
        {
            switch (myNode.Name.ToLower())
            {
                case "dialogentry":
                    precode = "";
                    midcode = "";
                    postcode = "";
                    choicetag = "selector";
                    inputwait = "";
                    processDialogEntry(myNode);

                    break;
                case "outgoinglinks":
                    postcode = "";
                    if (myNode.ChildNodes.Count > 0)
                    {
                        if (myNode.ChildNodes.Count > 1)
                        {
                            //postcode += "   <task>\n";
                            //postcode += "     <get_user_input/>\n";
                            //postcode += "   <task>\n";
                            if (inputwait.Length > 0)
                            {
                                processas="sequence";
                                postcode += "   <waitForChatInput wait='" + inputwait + "'/>\n";
                            }
                        }
                        postcode += "   <" + choicetag + ">\n";
                        for (int childIndex = 0; childIndex < myNode.ChildNodes.Count; childIndex++)
                        {
                            XmlNode childNode = myNode.ChildNodes[childIndex];
                            processNode(childNode);
                        }
                        postcode += "   </" + choicetag + ">\n";
                    }
                    break;
                case "link":
                    processLink(myNode);
                    break;
                case "field":
                    processField(myNode);
                    break;
                case "actors":
                    break;
                case "items":
                    break;
                case "locations":
                    break;
                case "uservariables":
                    break;
                case "conditionsstring":
                    if (myNode.InnerText.Length > 0)
                    {
                        precode += "   <comment> ConditionsString - " + myNode.InnerText + "</comment>\n";
                    }
                    break;

                case "userscript":
                    if (myNode.InnerText.Length > 0)
                    {
                        precode += "   <comment> UserScript - " + myNode.InnerText + "</comment>\n";
                    }
                    break;

                default:
                    for (int childIndex = 0; childIndex < myNode.ChildNodes.Count; childIndex++)
                    {
                        XmlNode childNode = myNode.ChildNodes[childIndex];
                        processNode(childNode);
                    }
                    break;
            }
        }

        public void processField(XmlNode myNode)
        {
            if (!inDialog) return;

            string type = "";
            string hint = "";
            string title = "";
            string value = "";
            try
            {
               if (myNode.Attributes["type"]!=null) type = myNode.Attributes["type"].Value;
               if (myNode.Attributes["hint"]!=null)  hint = myNode.Attributes["hint"].Value;
            }
            catch (Exception e)
            {
                Console.WriteLine("ERROR: processField attributes");
            }

            for (int childIndex = 0; childIndex < myNode.ChildNodes.Count; childIndex++)
            {
                XmlNode childNode = myNode.ChildNodes[childIndex];
                switch (childNode.Name.ToLower())
                {
                    case "title":
                        title = childNode.InnerText;
                        break;
                    case "value":
                        value = childNode.InnerText;
                        break;
                    default:
                        break;

                }
            }

            if (value.Length > 0)
            {
                value = value.Replace("\r\n", " ");
                value = value.Replace('\n', ' ');
                value = value.Replace('\r', ' ');
                switch (title.ToLower())
                {
                    case "menu text":

                        midcode += "   <assertmenu cond='" + value + "' />\n";
                        break;
                    case "dialogue text":
                        midcode += "   <task>\n";
                        midcode += "      <say>" + value + "</say>\n";
                        midcode += "   </task>\n";
                        break;
                    case "description":
                        precode += "   <comment> desc - " + value + "</comment>\n";
                        break;
                    case "title":
                        precode += "   <comment> title - " + value + "</comment>\n";
                        break;
                    case "animation files":
                        precode += "   <comment> animation - " + value + "</comment>\n";
                        break;
                    case "audio files":
                        precode += "   <comment> audio - " + value + "</comment>\n";
                        break;
                    case "conditionsstring":
                        precode += "   <comment> ConditionsString - " + value + "</comment>\n";
                        break;
                    case "assertcondition":
                        precode += "   <assert cond='" + value + "/>\n";
                        break;
                    case "choicetag":
                        choicetag = value;
                        break;
                    case "inputwait":
                        inputwait = value;
                        break;
                    case "behaviorcode":
                        midcode += value + "\n";
                        break;

                    default:
                        break;
                }
            }

        }

        public void processLink(XmlNode myNode)
        {
            string OriginConvoID = "";
            string OriginDialogID = "";
            string DestinationConvoID = "";
            string DestinationDialogID = "";
            string IsConnector = "";

            try
            {
                OriginConvoID = myNode.Attributes["OriginConvoID"].Value;
                OriginDialogID = myNode.Attributes["OriginDialogID"].Value;
                DestinationConvoID = myNode.Attributes["DestinationConvoID"].Value;
                DestinationDialogID = myNode.Attributes["DestinationDialogID"].Value;
                IsConnector = myNode.Attributes["IsConnector"].Value;
            }
            catch (Exception e)
            {
                Console.WriteLine("ERROR: processLink attributes");
            }
            string srckey = OriginConvoID + "_" + OriginDialogID;
            string destkey = DestinationConvoID + "_" + DestinationDialogID;

            postcode += "      <subbehavior id='" + destkey + "' />\n";

        }

        public void processDialogEntry(XmlNode myNode)
        {
            string ID = "";
            string ConversationID = "";
            string IsRoot = "";
            string IsGroup = "";
            string DelaySimStatus = "";
            string FalseCondtionAction = "";
            string ConditionPriority = "";
            bool prevInDialog = inDialog;
            inDialog = true;
            processas = "sequence";

            try
            {
                ID = myNode.Attributes["ID"].Value;
                ConversationID = myNode.Attributes["ConversationID"].Value;
                IsRoot = myNode.Attributes["IsRoot"].Value;
                IsGroup = myNode.Attributes["IsGroup"].Value;
                DelaySimStatus = myNode.Attributes["DelaySimStatus"].Value;
                FalseCondtionAction = myNode.Attributes["FalseCondtionAction"].Value;
                ConditionPriority = myNode.Attributes["ConditionPriority"].Value;

            }
            catch (Exception e)
            {
                Console.WriteLine("ERROR: processDialogEntry attributes");
            }
            string bkey = ConversationID + "_" + ID;

            for (int childIndex = 0; childIndex < myNode.ChildNodes.Count; childIndex++)
            {
                XmlNode childNode = myNode.ChildNodes[childIndex];
                processNode(childNode);
            }
            btxmlCode += "\n\n  <behavior processas='"+processas+"' id='" + bkey + "'  >\n";
            //btxmlCode += "   <sequence>\n";
            btxmlCode += precode;
            btxmlCode += midcode;
            btxmlCode += postcode;

            //btxmlCode += "   </sequence>\n";
            btxmlCode += "  </behavior>\n";

            inDialog = prevInDialog;
        }

    }
    #endregion

}
