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
using MushDLR223.Utilities;
using RTParser;
using NotImplementedException=sun.reflect.generics.reflectiveObjects.NotImplementedException;

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
    /// <summary>
    /// Values that can be returned from composites and the like.
    /// </summary> 
    public enum RunStatus
    {
        Non,
        Success,
        Failure,
        Running,
    }
    public enum Threshold
    {
        Positive,
        Negative,
    }

    /// <summary>
    /// Valence: A value that exhibits exponential decay defined by a halflife over time
    /// </summary>

    [Serializable]

    public class Valence
    {
        /// <summary>
        /// ID: the variable name
        /// </summary>
        public String ID = null;
        /// <summary>
        /// The value of the variable at LastUpdate
        /// </summary>
        public double RefValue = 0;
        /// <summary>
        /// The tickCount of the change
        /// </summary>
        public Int32 LastUpdate=0;

        /// <summary>
        /// The halflife of the variable in milliseconds (since it is based on Environment.TickCount)
        /// </summary>
        public double Halflife =1;

        public Valence(string myID)
        {
            ID = myID;
            LastUpdate = 0;
        }

        public double CurVal()
        {
            double timeInterval;
            timeInterval = Environment.TickCount - LastUpdate;
            double CV = Math.Pow(0.5, (timeInterval / Halflife));
            return CV;
        }
        public void adjust (double increment)
        {
            RefValue = CurVal() + increment;
            LastUpdate = Environment.TickCount;
        }
        public void setHalflife(double halfLife)
        {
            Halflife = halfLife;
        }
        public void setInitialValue(double initV)
        {
            RefValue = initV;
            LastUpdate = Environment.TickCount;
        }
    }


    /// <summary>
    /// ValenceSet: Dictionary of Valences
    /// </summary>

    [Serializable]

    public class ValenceSet
    {       

        public CIDictionary<string, Valence> Valences = new CIDictionary<string, Valence>();

        public ValenceSet()
        {
            Valences = new CIDictionary<string, Valence>();
        }

        public void Add(string ID,double halflife)
        {
            Valence substance = new Valence(ID);
            substance.Halflife = halflife;
            substance.adjust(1.0);
            Valences[ID] = substance;
            
        }
        public void adjust(string ID, double amount)
        {
            if (!Valences.ContainsKey(ID))
            {
                Add(ID, 1);
            }
            Valence deStuff = (Valence)Valences[ID];
            deStuff.adjust(amount);
        }

        public double CurVal(string ID)
        {
            if (!Valences.ContainsKey(ID))
            {
                return 0;
            }
            else
            {
                Valence deStuff = (Valence)Valences[ID];
                return deStuff.CurVal();
            }
        }

        public void setHalflife(string ID, double halfLife)
        {
            if (!Valences.ContainsKey(ID))
            {
                Add(ID,halfLife);
            }
            else
            {
                Valence deStuff = (Valence)Valences[ID];
               deStuff.setHalflife( halfLife);
            }
        }
        public void setRefVal(string ID, Int32 refTick)
        {
            if (!Valences.ContainsKey(ID))
            {
                Add(ID, 1);
            }
            else
            {
                Valence deStuff = (Valence)Valences[ID];
                deStuff.RefValue = refTick;
            }
        }


    }

    /// <summary>
    /// SemiStringStackQueue: A Stack and a Queue at the same time
    /// </summary>
    public class SemiStringStackQueue
    {
        private readonly List<string> items = new List<string>();

        public void Push(string item)
        {
            lock (loc) items.Add(item);
        }
        public string Pop()
        {
            lock (loc) if (items.Count > 0)
                {
                    string temp = items[items.Count - 1];
                    items.RemoveAt(items.Count - 1);
                    return temp;
                }
                else
                    return default(string);
        }
        public string Dequeue()
        {
            return Pop();
        }
        public string Peek()
        {
            lock (loc) if (items.Count > 0)
                {
                    string temp = items[items.Count - 1];
                    return temp;
                }
                else
                    return default(string);
        }
        public string PopRandom()
        {
            lock (loc) if (items.Count > 0)
                {
                    Random rng = new Random();
                    int pick = rng.Next(items.Count - 1);
                    string temp = items[pick];
                    items.RemoveAt(pick);
                    return temp;
                }
                else
                    return default(string);
        }

        public int Count
        {
            get { lock (loc) return items.Count; }
        }
        public void Clear()
        {
            lock (loc) items.Clear();
        }

        private object loc
        {
            get { return items; }
        }

        public bool Contains(string s)
        {
            return GetIndex(s) > -1;
        }
        public void Remove(int itemAtPosition)
        {
            lock (loc) items.RemoveAt(itemAtPosition);
        }
        public void RemoveAny(string item)
        {
            lock (loc)
            {
                int index = GetIndex(item);
                if (index >= 0)
                    items.RemoveAt(index);
            }
        }

        private int GetIndex(string item)
        {
            lock (loc) return items.FindLastIndex(delegate(string s) { return KeyCase.SameKey(s, item); });
        }

        public void Enqueue(string item)
        {
            lock (loc) items.Insert(0, item);
        }
        override public string ToString()
        {
            string result = "[";
            lock (loc) foreach (string x in items)
            {
                result += x + "|";
            }
            result += "]";
            return result;
        }
    }

    /// <summary>
    /// BehaviorSet: Dictionary of Behaviors with begin/finish times and event handlers
    /// </summary>

    [Serializable ]
    public class BehaviorSet
    {
        //public Hashtable behaveTrees;
        public CIDictionary<string, BehaviorTree> behaveTrees;
        public CIDictionary<string, RunStatus> runState;
        public CIDictionary<string, Int32> entryTime = new CIDictionary<string, Int32>();
        public CIDictionary<string, Int32> execTime = new CIDictionary<string, Int32>();
        public CIDictionary<string, string> eventTable = new CIDictionary<string, string>();
        public ValenceSet VSoup;
        public Stack handlerStack;
        public string persistantDirectory=null;
        public CIDictionary<string, bool> invisiblePatterns;
        public bool waitingForChat = false;


        public AltBot bot {
                get { return _bot; }
                set {
                        _bot = value;

                        List<string> btList = new List<string>();
                        lock (behaveTrees)
                        {
                            foreach (String treeName in behaveTrees.Keys)
                            {
                                btList.Add(treeName);
                            }
                        }
                        foreach (string k in btList)
                        {
                            behaveTrees[k].bot = _bot;
                        }
                    } 
          }
        [NonSerialized ]
        private AltBot _bot;

        
        public BehaviorSet(AltBot bot)
        {
            _bot = bot;
            //behaveTrees = new Hashtable();
            behaveTrees = new CIDictionary<string, BehaviorTree>();
            runState = new CIDictionary<string, RunStatus>();
            VSoup = new ValenceSet();
            handlerStack = new Stack();
            invisiblePatterns = new CIDictionary<string, bool>();
        }
       
        
        public void preSerial()
        {
            foreach (string k in behaveTrees.Keys)
            {
                behaveTrees[k].preSerial();
            }

        }

        public void postSerial(AltBot deBot)
        {
            bot = deBot;
            foreach (string k in behaveTrees.Keys)
            {
                behaveTrees[k].postSerial(deBot);
            }
        }

        public void persistAllToFiles()
        {
            foreach (string k in behaveTrees.Keys)
            {
                persistToFile(k);
            }
        }

        public string behaviorDiskName(string behaviorName)
        {
            return String.Format("{0}{1}{2}.BTX", persistantDirectory,Path.DirectorySeparatorChar , behaviorName);
        }
        public void persistToFile(string treeName)
        {
            if (persistantDirectory == null) return;
            if (!Directory.Exists(persistantDirectory)) return;

            string diskName = String.Format("{0}{1}{2}.BTX", persistantDirectory, Path.DirectorySeparatorChar, behaveTrees[treeName.ToLower()].name);
            //if (File.Exists(diskName)) return;
            StreamWriter outfile = new StreamWriter(diskName);
            string docText = behaveTrees[treeName.ToLower()].treeDoc.OuterXml;
            outfile.Write(docText);
            outfile.Flush();
            outfile.Close();
        }

        public List<string> behaviorXmlList()
        {
            List<string> XList = new List<string>();
            foreach (String treeName in behaveTrees.Keys)
            {
                string docText = behaveTrees[treeName].treeDoc.OuterXml;
                XList.Add(docText);
            }
            return XList;
        }

        public void loadFromFiles(string ID)
        {
            if (behaveTrees.ContainsKey(ID.ToLower()))
            {
                if (behaveTrees[ID].treeDoc != null)
                {
                    return;
                }
            }
            string diskName = String.Format("{0}{1}{2}.BTX", persistantDirectory, Path.DirectorySeparatorChar, behaveTrees[ID.ToLower()].name);
            if (!File.Exists(diskName))
            {
                //defineBehavior(ID, "");
            }
            else
            {
                string readText = File.ReadAllText(diskName);

                defineBehavior(ID, readText);
            }
        }

        public void defineBehavior(string treeName, string behaviorDef)
        {
            try
            {
                BehaviorTree newTree = new BehaviorTree(bot);
                newTree.defineBehavior(treeName, behaviorDef);
                behaveTrees[treeName.ToLower()] = newTree;
                if (_bot != null) behaveTrees[treeName.ToLower()].bot = _bot;
                persistToFile(treeName);
            }
            catch (Exception e)
            {
                Console.WriteLine("ERR:" + e.Message);
                Console.WriteLine("ERR:" + e.StackTrace);
            }
        }

        public void keepTime(string nodeID, RunStatus R)
        {
            if (bot == null) return;
            try
            {
                    // Update on first entry
                if ((R==null) || (R == RunStatus.Success))
                {
                    if (!bot.myBehaviors.entryTime.ContainsKey(nodeID))
                    {
                        bot.myBehaviors.entryTime[nodeID] = Environment.TickCount;
                    }
                }
                else
                {
                    // Remove the ID on any failure
                    bot.myBehaviors.entryTime.Remove(nodeID);
                }
              }
            catch (Exception e)
            {
                Console.WriteLine("ERR:" + e.Message);
                Console.WriteLine("ERR:" + e.StackTrace);
            }
       }

        public Int32 timeRunning(string nodeID)
        {
            if (bot.myBehaviors.entryTime.ContainsKey(nodeID))
            {
                return Environment.TickCount - bot.myBehaviors.entryTime[nodeID];
            }
            return 0;
        }

        public void activationTime(string nodeID, RunStatus R)
        {
            if (bot == null) return;
            try
            {
                if ((R == null) || (R == RunStatus.Success))
                {
                    if ((bot.myBehaviors.execTime.Count==0)||
                        (!bot.myBehaviors.execTime.ContainsKey(nodeID)))
                    {
                        bot.myBehaviors.execTime.Add(nodeID,Environment.TickCount);
                    }
                    bot.myBehaviors.execTime[nodeID] = Environment.TickCount;
                }

            }
            catch (Exception e)
            {
                Console.WriteLine("ERR:" + e.Message);
                Console.WriteLine("ERR:" + e.StackTrace);
            }
    }

        public Int32 lastRunning(string nodeID)
        {
            if (bot.myBehaviors.execTime.ContainsKey(nodeID))
            {
                return Environment.TickCount - bot.myBehaviors.execTime[nodeID];
            }
            // Doesn't exist means never run before
            return Int32.MaxValue;
        }

        public void satisfyDrive0(string driveName)
        {
            activationTime(driveName, RunStatus.Success); 
        }

        public void satisfyDrive(string driveName)
        {
            VSoup.adjust(driveName, 1.0); 
        }

        public RunStatus runBTXML(string BTXML)
        {
            RunStatus result = RunStatus.Failure;
            BehaviorTree newTree = new BehaviorTree(bot);
            string treeName = "temptree123" +newTree.rgen.Next();
            newTree.defineBehavior(treeName, BTXML);
            //result = newTree.runBehaviorTree(this.bot);
            foreach (RunStatus myChildResult in newTree.runBehaviorTree(this.bot))
            {
                result = myChildResult;
                if (result != RunStatus.Running) break;
                //yield return RunStatus.Running;
                Thread.Sleep(100);
            }
           
            return result;
        }

        public RunStatus runBTXML(XmlNode BTXML)
        {
            RunStatus result = RunStatus.Failure;
            BehaviorTree newTree = new BehaviorTree(bot);
            newTree.bot = bot;
            //result = newTree.processNode(BTXML);

            // Execute All Children
            foreach (XmlNode childNode in BTXML.ChildNodes)
            {
                //RunStatus childResult = newTree.processNode(childNode);
                RunStatus childResult = RunStatus .Failure ;
                foreach (RunStatus myChildResult in newTree.processNode(childNode))
                {
                    childResult = myChildResult;
                    if (childResult != RunStatus.Running) break;
                    //yield return RunStatus.Running;
                    Thread.Sleep(100);
                }
                // Except for Asserts
                if (newTree.isAnAssert(childNode.Name))
                {
                    if (childResult == RunStatus.Failure)
                    {
                        return RunStatus.Failure;
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
            return RunStatus.Success;           
            return result;
        }

        // tests for if we can see a behavior or not
        public void makeInvisible(string pattern)
        {
            if (pattern.Length == 0) return;
            invisiblePatterns.Add(pattern, true);
        }
        public void makeVisible(string pattern)
        {
            if (invisiblePatterns.ContainsKey(pattern))
            {
                invisiblePatterns.Remove(pattern);
            }
        }
        public bool visibleBehavior(string behaviorName)
        {
            
            foreach (string pattern in invisiblePatterns.Keys)
            {
                if (Regex.IsMatch(behaviorName,pattern )) return false;
            }
            return true;
        }


        public bool definedBehavior(string behaviorName)
        {
            if (!visibleBehavior(behaviorName)) return false;
            if (behaveTrees.ContainsKey(behaviorName)) return true;
            string diskName = String.Format("{0}{1}{2}.BTX", persistantDirectory,Path.DirectorySeparatorChar, behaviorName);
            //Console.WriteLine("definedBehavior( {0} -> {1})",behaviorName, diskName);
            if (!File.Exists(diskName))
            {
                return false;
            }
            string readText = File.ReadAllText(diskName);
            defineBehavior(behaviorName, readText);
            return behaveTrees.ContainsKey(behaviorName);
        }

        public void defineSubAIML(XmlNode xnode)
        {
            BehaviorTree newTree = new BehaviorTree(bot);
            newTree.bot = bot;
            newTree.ProcessStateAiml(xnode);
        }

        #region EventHandlers
        //public Queue<string> eventQueue = new Queue<string>();
        public SemiStringStackQueue eventQueue = new SemiStringStackQueue();

        public void addEventHandler(string evnt, string val)
        {
            if (eventTable.ContainsKey(evnt))
                eventTable[evnt] = val;
            else
                eventTable.Add(evnt, val);

        }
        public void deleteEventHandler(string evnt, string val)
        {
            if (eventTable.ContainsKey(evnt))
                eventTable.Remove(evnt);
        }
        public  bool hasEventHandler(string evnt)
        {
            if (eventTable.ContainsKey(evnt))
            {
                Console.WriteLine("   hasEventHandler({0}) ={1}", evnt, true);
            }
            if (definedBehavior(evnt))
            {
                Console.WriteLine("   definedBehavior({0}) ={1}", evnt, true);
            }
            return eventTable.ContainsKey(evnt);
        }

        public void runEventHandler0(string evnt)
        {
            if (hasEventHandler(evnt))
            {
                runBotBehavior(eventTable[evnt], bot);
            }
            else
            {
                if (definedBehavior(evnt)) runBotBehavior(evnt, bot);
            }

        }
        public void runEventHandler(string evnt)
        {
            if ((bot != null) && (bot.myServitor != null) && (bot.myServitor.myScheduler != null))
            {
                if (hasEventHandler(evnt))
                {
                    bot.myServitor.myScheduler.EnqueueEvent(eventTable[evnt]);
                }
                else
                {
                    if (definedBehavior(evnt)) 
                    bot.myServitor.myScheduler.EnqueueEvent(evnt);
                }
            }
            else
            {
                if (hasEventHandler(evnt))
                {
                    runBotBehavior(eventTable[evnt], bot);
                }
                else
                {
                    if (definedBehavior(evnt)) runBotBehavior(evnt, bot);
                }
            }
        }

        public string getEventHandler(string evnt)
        {
            if (hasEventHandler(evnt))
            {
                return eventTable[evnt];
            }
            else
            {
                if (definedBehavior(evnt)) 
                    return evnt;
            }
            return "";
        }

        public void queueEvent(string evnt)
        {
            Console.WriteLine("ENQUE EVENT : {0}", evnt);
            logText(String.Format("ENQUE EVENT : {0}", evnt));
            eventQueue.Enqueue(evnt);
            logText(String.Format("EVENTQUEUE: {0}", eventQueue.ToString()));
        }

        public void pushHandlers()
        {
            // Remember and keep a clone
            handlerStack.Push(eventTable);
            eventTable = new CIDictionary<string, string>(eventTable);
        }

        public void popHandlers()
        {
            // Restore the last one
            if (handlerStack.Count > 0)
            {
                eventTable = (CIDictionary<string, string>)handlerStack.Pop();
            }
        }

        public void processEventQueue()
        {
            string ourEvent = "";
            while (eventQueue.Count > 0)
            {
                try
                {
                    ourEvent = eventQueue.Dequeue();
                    Console.WriteLine(" *** processEventQueue : {0}", ourEvent);
                    logText(String.Format("EVENTQUEUE: {0}", eventQueue.ToString()));
                    logText(String.Format(" *** processEventQueue : {0}", ourEvent));
                    runEventHandler(ourEvent);

                }
                catch (Exception e)
                {
                    Console.WriteLine(" ERR: processEventQueue '{0}' exception induced failure! ", ourEvent);
                    Console.WriteLine("ERR:" + e.Message);
                    Console.WriteLine("ERR:" + e.StackTrace);
                }
            }
        }
        public void processOneEventQueue()
        {
            string ourEvent = "";
            if (eventQueue.Count > 0)
            {
                try
                {
                    ourEvent = eventQueue.Dequeue();
                    Console.WriteLine(" *** processOneEventQueue : {0}", ourEvent);
                    logText(String.Format("EVENTQUEUE: {0}", eventQueue.ToString ()));
                    logText(String.Format(" *** processOneEventQueue : {0}", ourEvent));
                    runEventHandler(ourEvent);
                }
                catch (Exception e)
                {
                    Console.WriteLine(" ERR: processOneEventQueue '{0}' exception induced failure! ", ourEvent);
                    Console.WriteLine("ERR:" + e.Message);
                    Console.WriteLine("ERR:" + e.StackTrace);
                }
            }

 
        }
        public void logText(string msg)
        {
            if (bot == null) return;
            if (SkipLog) return;
            lock (bot.loglock)
            {
                try
                {
                    string miniLog = String.Format(@"./aiml/BTTrace.txt");
                    System.IO.File.AppendAllText(miniLog, msg + "\n");
                }
                catch
                {
                }
            }
        }
        public void logNode(string msg, XmlNode myNode)
        {
            if (bot == null) return;
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
        public string getIndent(XmlNode node)
        {
            if (node.ParentNode == null)
            {
                // the only node with no parent is the root node, which has no path 
                return "";
            }
            return "  " + getIndent(node.ParentNode);
        }
        #endregion
        #region bStack
        /// <summary>
        /// A general stack to remember things to activate later
        /// </summary>
        public SemiStringStackQueue behaviorStack = new SemiStringStackQueue();

        public bool SkipLog;

        public void pushUniqueToStack(string evnt)
        {
            // only one instance of an entry can be on the stack at a time
            if (behaviorStack.Contains(evnt)) return;
            behaviorStack.Push(evnt);
        }
        public void removeFromStack(string evnt)
        {
            // only one instance of an entry can be on the stack at a time
            if (!behaviorStack.Contains(evnt)) return;
            behaviorStack.RemoveAny(evnt);
        }
        public bool behaviorStackActive()
        {
            return (behaviorStack.Count>0);
        }

        public void processRandomEventStack()
        {
            string ourEvent = "";
            if (behaviorStack.Count > 0)
            {
                try
                {
                    ourEvent = behaviorStack.PopRandom();
                    Console.WriteLine(" *** processOneEventStack : {0}", ourEvent);
                    runEventHandler(ourEvent);
                }
                catch (Exception e)
                {
                    Console.WriteLine(" ERR: processOneEventStack '{0}' exception induced failure! ", ourEvent);
                    Console.WriteLine("ERR:" + e.Message);
                    Console.WriteLine("ERR:" + e.StackTrace);
                }
            }
        }
        public void processOneEventStack()
        {
            string ourEvent = "";
            if (behaviorStack.Count > 0)
            {
                try
                {
                    ourEvent = behaviorStack.Pop();
                    Console.WriteLine(" *** processOneEventStack : {0}", ourEvent);
                    runEventHandler(ourEvent);
                }
                catch (Exception e)
                {
                    Console.WriteLine(" ERR: processOneEventStack '{0}' exception induced failure! ", ourEvent);
                    Console.WriteLine("ERR:" + e.Message);
                    Console.WriteLine("ERR:" + e.StackTrace);
                }
            }
        } 
        #endregion


        public RunStatus runBotBehavior(string behaviorName, AltBot deBot)
        {
            bot = deBot;
            
            if (hasEventHandler(behaviorName))
            {
                return runBotBehavior(eventTable[behaviorName], deBot);
            }
            bool known= definedBehavior(behaviorName);
            if (known || (behaveTrees.ContainsKey(behaviorName)))
            {
                try
                {
                    BehaviorTree curTree = (BehaviorTree)behaveTrees[behaviorName];
                    //RunStatus result = curTree.runBehaviorTree(deBot);
                    RunStatus result = RunStatus.Running;
                    foreach (RunStatus myChildResult in curTree.runBehaviorTree(deBot))
                    {
                        result = myChildResult;
                        if (result != RunStatus.Running) break;
                        //yield return RunStatus.Running;
                        Thread.Sleep(100);
                    }
                    return result;
                }
                catch (Exception e)
                {
                    Console.WriteLine(" ERR: runBotBehavior '{0}' exception induced failure! ", behaviorName);
                    Console.WriteLine("ERR:" + e.Message);
                    Thread.Sleep(4000);
                    Console.WriteLine("ERR:" + e.StackTrace);
                }
                return RunStatus.Failure;
            }
            else
            { 
                
                Console.WriteLine(" ERR: runBotBehavior did not find Tree '{0}' ", behaviorName);
                return RunStatus.Failure;
            }
        }
        public IEnumerator<RunStatus> getBehaviorEnumerator(string name)
        {

            BehaviorTree curTree = (BehaviorTree)behaveTrees[name.ToLower()];
            if (curTree == null)
            {
                Console.WriteLine("WARN: Tree '{0}' is null", name);
                return null;
            }
            return curTree.runBehaviorTree(bot).GetEnumerator();

        }
        public void runBotBehaviors(AltBot deBot)
        {
            if (bot != deBot) bot = deBot;

            processEventQueue();
            // if there is a root defined then run it
            // otherwise run them all
            if (behaveTrees.ContainsKey("root"))
            {
                    try
                    {
                        BehaviorTree curTree = (BehaviorTree)behaveTrees["root"];
                        if (curTree == null)
                        {
                            Console.WriteLine("WARN: Tree '{0}' is null", "null");

                            return;
                        }
                        curTree.runBehaviorTree(deBot);
                    }
                    catch (Exception e)
                    {
                        Console.WriteLine("ERR: SourceTree = root");
                        Console.WriteLine("ERR:" + e.Message);
                        Console.WriteLine("ERR:" + e.StackTrace);
                    }

            }
            else
            {
                try
                {
                    List <string> bkeys = new List<string> ();
                    foreach (string treeName in behaveTrees.Keys)
                    {
                        bkeys.Add(treeName);
                    }

                    foreach (string treeName in bkeys)
                    {
                        try
                        {
                            BehaviorTree curTree = (BehaviorTree)behaveTrees[treeName];
                            if (curTree == null)
                            {
                                Console.WriteLine("WARN: Tree '{0}' is null", treeName);
                                continue;
                            }
                            curTree.runBehaviorTree(deBot);
                        }
                        catch (Exception e)
                        {
                            Console.WriteLine("ERR: SourceTree =", treeName);
                            Console.WriteLine("ERR:" + e.Message);
                            Console.WriteLine("ERR:" + e.StackTrace);
                        }
                    }
                }
                catch (Exception e)
                {
                    Console.WriteLine("ERR: runBotBehaviors");
                Console.WriteLine("ERR:" + e.Message);
                Console.WriteLine("ERR:" + e.StackTrace);
                }

            }

        }
    }
    [Serializable]
    public class BehaviorTree0 
    {
        public string curState;
        public string initialState;
        public Int32 transitionTime;
        public Random rgen = new Random();
        public string name;
        public int tickRate=1000;
        public long satCount = 0;
        public long satMod = 400;
        public Dictionary<string, int> restorePoint;
        private string serialDoc=null;

        public string monitorBehavior=null; // Sub-behavior for every tick
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
        public BehaviorTree0()
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

        public RunStatus runBehaviorTree(AltBot theBot)
        {
            bot = theBot;
            // Execute All Children
            foreach (XmlNode childNode in treeDoc.ChildNodes)
            {
                RunStatus childResult = processNode(childNode);
                // Except for Asserts
                if (isAnAssert(childNode.Name))
                {
                    if (childResult == RunStatus.Failure)
                    {
                        return RunStatus.Failure;
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
            return RunStatus.Success;           
        }

        /// <summary>
        /// Executes a subBehavior in the same context
        /// </summary>
        /// <param name="subDoc"></param>
        /// <returns></returns>
        public RunStatus runSubTree(XmlDocument subDoc)
        {
            // Execute All Children
            foreach (XmlNode childNode in subDoc.ChildNodes)
            {
                RunStatus childResult = processNode(childNode);
                // Except for Asserts
                if (isAnAssert(childNode.Name))
                {
                    if (childResult == RunStatus.Failure)
                    {
                        return RunStatus.Failure;
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
            return RunStatus.Success;
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
            if (monitorBehavior.Length ==0) return RunStatus.Success;
            if (bot.myBehaviors.behaveTrees.ContainsKey(monitorBehavior))
            {
                result = bot.myBehaviors.runBotBehavior(monitorBehavior, bot);
            }
            return result;
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
        public RunStatus processNode(XmlNode myNode)
        {
            if (myNode == null) return RunStatus.Failure;
            if (StaticXMLUtils.IsBlank(myNode))
            {
                return RunStatus.Success;
            }
            Thread.Sleep(50);
            RunStatus result = RunStatus .Failure ;
            string nodeID = "null";
            try
            {
                if((myNode.Attributes != null)&&(myNode .Attributes .Count >0))
                {
                    if (myNode.Attributes["id"] != null)
                    {
                        nodeID = myNode.Attributes["id"].Value;
                    }
                }
            }
            catch(Exception e)
            {
                nodeID = "null";
            }
            
            bool origCritical = bot.inCritical;
            logNode("BEGIN",myNode);
            ProcessNodeAddEvents(myNode);
            // Console.WriteLine("Process BNode {1} {0}", nodeID, myNode.Name.ToLower());
            // Start a winner
            bot.myBehaviors.keepTime(nodeID, RunStatus.Success);
            //check watchdog if any
            tickMonitor();
            try
            {
                switch (myNode.Name.ToLower())
                {
                    case "assert":
                        result = ProcessAssert(myNode);
                        break;
                    case "asserttimer":
                        result = ProcessAssertTimer(myNode);
                        break;
                    case "sequence":
                        result = ProcessSequence(myNode);
                        break;
                    case "selector":
                        result = ProcessSelector(myNode);
                        break;
                    case "random":
                        result = ProcessRandomSelector(myNode);
                        break;
                    case "weighted":
                        result = ProcessWeightedSelector(myNode);
                        break;
                    case "parallel":
                        result = ProcessParallel(myNode);
                        break;
                    case "loop":
                        result = ProcessLoop(myNode);
                        break;
                    case "loopuntil":
                        result = ProcessLoopUntil(myNode);
                        break;
                    case "task":
                        result = ProcessTask(myNode);
                        break;
                    case "subaiml":
                        result = ProcessStateAiml(myNode);
                        break;
                    case "drive":
                        result = ProcessDrive(myNode);
                        //Thread.Sleep(2000);
                        break;
                    case "motive":
                        result = ProcessMotive(myNode);
                        //Thread.Sleep(2000);
                        break;

                    case "inhibit":
                        this.bot.myBehaviors.VSoup.setRefVal(nodeID, 0);
                        break;

                    case "release":
                        this.bot.myBehaviors.VSoup.adjust(nodeID, 1.0);
                        break;

                    case "behavior":
                        result = ProcessBehavior(myNode);
                        break;
                    case "rbehavior":
                        result = ProcessRBehavior(myNode);
                        break;
                    case "subbehavior":
                        result = ProcessSubBehavior(myNode);
                        break;
                    case "scheduler":
                        result = ProcessScheduler(myNode);
                        break;
                        
                    case "starttimer":
                        // start a stopwatch
                        bot.myBehaviors.keepTime(nodeID, RunStatus.Success);
                        bot.myBehaviors.activationTime(nodeID, RunStatus.Success);
                        bot.myBehaviors.runEventHandler("onsuccess");
                        ProcessNodeDeleteEvents(myNode);
                        bot.inCritical = origCritical;
                        return RunStatus.Success;
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
                        return RunStatus.Success;
                        break;

                    case "tellkb":
                        result = ProcessTellKB(myNode);
                        break;
                    case "tellbasekb":
                        result = ProcessTellBaseKB(myNode);
                        break;
                    case "tellkbocc":
                        result = ProcessTellKBOCC(myNode);
                        break;
                    case "clearkb":
                        result = ProcessClearKB(myNode);
                        break;
                    case "clearbasekb":
                        result = ProcessClearBaseKB(myNode);
                        break;
                    case "processkb":
                        result = ProcessKBModel(myNode);
                        break;

                    case "chat":
                        result = ProcessChat(myNode);
                        break;

                    case "assertguest":
                        result = ProcessAssertGuest(myNode);
                        break;
                    case "taskguest":
                        result = ProcessTaskGuest(myNode);
                        break;
                    case "enqueue":
                        result = ProcessEnqueue(myNode);
                        break;
                    case "flushqueue":
                        result = ProcessFlushQueue(myNode);
                        break;
                    case "breaker":
                        result = ProcessBreaker(myNode);
                        break;

                    default:
                        // Ignore the Nops
                        result = RunStatus.Success;
                        break;
                }
            }
            catch (Exception e)
            {
                bot.inCritical = origCritical;
                result = RunStatus.Failure;
                Console.WriteLine("### BNODE ERR:{0} {1} {2}", myNode.Name.ToLower(), nodeID, result);
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
            bot.myBehaviors.keepTime(nodeID, result);
            bot.myBehaviors.activationTime(nodeID, result);

            if ((result == RunStatus.Success) && (nodeID != "null"))
            {
               // Console.WriteLine("Result BNode {0} {1} {2}", myNode.Name.ToLower(), nodeID, result);
            }
            if (result == RunStatus.Success) bot.myBehaviors.runEventHandler("onsuccess");
            if (result == RunStatus.Failure) bot.myBehaviors.runEventHandler("onfail");
            ProcessNodeDeleteEvents(myNode);
            logNode("END("+result .ToString ()+") ", myNode);
            bot.inCritical = origCritical;
            return result;

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
                if (evnt.StartsWith ("on"))
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
                if (evnt.StartsWith ("on"))
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

        public RunStatus ProcessPushBehavior(XmlNode myNode)
        {
            string behavior = myNode.InnerText;
            bot.myBehaviors.pushUniqueToStack(behavior);
            return RunStatus.Success;
        }
        public RunStatus ProcessPopBehavior(XmlNode myNode)
        {
            bot.myBehaviors.processOneEventStack();
            return RunStatus.Success;
        }
        public RunStatus ProcessPopRandomBehavior(XmlNode myNode)
        {
            bot.myBehaviors.processRandomEventStack();
            return RunStatus.Success;
        }

        public RunStatus ProcessBehavior(XmlNode myNode)
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
                result = ProcessParallel(myNode);
                return result;
            }
            Console.WriteLine("\n\n****** COMPLEX BEHAVIOR BEGIN\n\n");
            string nodeID = "null";
            try
            {
                if((myNode.Attributes != null)&&(myNode .Attributes .Count >0))
                {
                    if (myNode.Attributes["id"] != null)
                    {
                        nodeID = myNode.Attributes["id"].Value;
                    }
                }
            }
            catch(Exception e)
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

            int restP = restorePoint [nodeID];
            restoring = (restP > 0) && (restP < myNode.ChildNodes.Count - 1); ;

            // if restorable then check all asserts up to the restore point
            int childIndex = 0;
            for (childIndex = 0; childIndex < restP; childIndex++)
            {
                XmlNode childNode = myNode.ChildNodes[childIndex];
                if (isAnAssert(childNode.Name))
                {
                    RunStatus childResult = processNode(childNode);
                    if (childResult == RunStatus.Failure)
                    {
                        return RunStatus.Failure;
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
                RunStatus childResult = processNode(childNode);
                // Except for Asserts
                if (isAnAssert(childNode.Name))
                {
                    if (childResult == RunStatus.Failure)
                    {
                        return RunStatus.Failure;
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
                    return RunStatus.Failure;

                }
                if (pace > 0)
                {
                    Thread.Sleep(pace);
                }

            }
            // if we make it to the end then the reset the restore point
            restorePoint[nodeID] = 0;
            bot.myBehaviors.removeFromStack(nodeID);
            Console.WriteLine("\n\n****** COMPLEX BEHAVIOR EXIT SUCCESS\n\n");
            return RunStatus.Success;

        }

        public RunStatus ProcessBreaker(XmlNode myNode)
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
            return result;
        }

        public RunStatus ProcessRBehavior(XmlNode myNode)
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
                RunStatus result = RunStatus.Failure;
                result = ProcessRandomSelector (myNode);
                return result;
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
            restoring = (restP >= 0) && (restP <myNode.ChildNodes.Count-1); // zero node matters in an RBehavior

            // if restorable then check all asserts up to the restore point
            int childIndex = 0;
            for (childIndex = 0; childIndex < restP; childIndex++)
            {
                XmlNode childNode = myNode.ChildNodes[childIndex];
                if (isAnAssert(childNode.Name))
                {
                    RunStatus childResult = processNode(childNode);
                    if (childResult == RunStatus.Failure)
                    {
                        return RunStatus.Failure;
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
                RunStatus childResult = processNode(childNode);
                // Except for Asserts
                if (isAnAssert(childNode.Name))
                {
                    if (childResult == RunStatus.Failure)
                    {
                        return RunStatus.Failure;
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
                if ((continueflag == false)||(childResult == RunStatus.Failure))
                {
                    restorePoint[nodeID] = childIndex;
                    Console.WriteLine("\n\n****** COMPLEX RBEHAVIOR EXIT FAILURE\n\n");

                    return RunStatus.Failure;

                }
                if (pace > 0)
                {
                    Thread.Sleep(pace);
                }

            }
            // if we make it to the end then the reset the restore point
            restorePoint[nodeID] = -1;
            bot.myBehaviors.removeFromStack(nodeID);
            Console.WriteLine("\n\n****** COMPLEX RBEHAVIOR EXIT SUCCESS\n\n");
            return RunStatus.Success;

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
                result =bot.myBehaviors.runBotBehavior(behaviorName, bot);
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

        public RunStatus ProcessScheduler(XmlNode myNode)
        {
            // talk to the scheduler directly
            RunStatus result = RunStatus.Running;
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
            return result;
        }

        public RunStatus ProcessSubBehavior(XmlNode myNode)
        {

            RunStatus result = RunStatus.Failure;
            string behaviorName = "root";
            try
            {
                behaviorName = myNode.Attributes["id"].Value;
                //result = bot.myBehaviors.runBotBehavior(behaviorName, bot);
                if (bot.myBehaviors.definedBehavior(behaviorName))
                {
                    logText("ProcessSubBehavior found:" + behaviorName);
                    BehaviorTree curTree = (BehaviorTree)bot.myBehaviors.behaveTrees[behaviorName];

                    result = runSubTree(curTree.treeDoc);
                }
                else
                {
                    // We don't have one
                    logText("ProcessSubBehavior did >>>> NOT <<<< find:" + behaviorName);
                    result = RunStatus.Failure;
                }
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
        public RunStatus ProcessEnqueue(XmlNode myNode)
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
            return result;
        }
        public RunStatus ProcessFlushQueue(XmlNode myNode)
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
            return result;
        }
        public RunStatus ProcessDrive(XmlNode myNode)
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

                    if (curV > threshold) return RunStatus.Failure;


                Console.WriteLine("  *** Execute Drive Procedure:{0} ***", driveName);
                // Run the children just like a selector
                //  if they any are true then you have success
                result = ProcessSelector(myNode);

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
            catch(Exception e)
            {
                Console.WriteLine("*** DRIVE EXCEPTION : {0} {1}", e.Message, e.StackTrace);
            }
            return result;
 
        }

        public RunStatus ProcessMotive(XmlNode myNode)
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

                if (curV < threshold) return RunStatus.Failure;


                Console.WriteLine("  *** Motive Drive Procedure:{0} ***", driveName);
                // Run the children just like a selector
                //  if they any are true then you have success
                result = ProcessSelector(myNode);

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
            return result;

        }
        public RunStatus ProcessAssert(XmlNode myNode)
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
                sv = bot.getBBHash(varName) ?? "0.0";
                if (!string.IsNullOrEmpty(sv)) bbVal = double.Parse(sv);
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
            if (valid) return RunStatus.Success;
            return RunStatus.Failure;
        }

        public RunStatus ProcessAssertTimer(XmlNode myNode)
        {
            //if it doesn't exist then create an entry and return Success
            string nodeID = myNode.Attributes["id"].Value;
            if (!bot.myBehaviors.entryTime.ContainsKey(nodeID))
            {
                bot.myBehaviors.keepTime(nodeID, RunStatus.Success);
                bot.myBehaviors.activationTime(nodeID, RunStatus.Success);
                return RunStatus.Success;
            }

            //otherwise treat as a normal assert
            // reset after assert returns true
            RunStatus r = ProcessAssert(myNode);
            if (r == RunStatus.Success)
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
            return r;
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
        public RunStatus ProcessAssertGuest(XmlNode myNode)
        {
            string condition = myNode.Attributes["cond"].Value;
            string parameters = myNode.InnerText;
            //if it doesn't exist then return failure
            if (bot.guestEvalObject == null)
            {
                return RunStatus.Failure;
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
            catch(Exception e)
            {
                Console.WriteLine("Error ProcessAssertGuest '{0}':{1}", condition, e.Message);
                return RunStatus.Failure;
            }

            return r;
        }

        public RunStatus ProcessTaskGuest(XmlNode myNode)
        {
            string condition = myNode.Attributes["call"].Value;
            string parameters = myNode.InnerText;
            //if it doesn't exist then return  failure(success would also make sense)
            if (bot.guestEvalObject == null)
            {
                return RunStatus.Failure;
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
                return RunStatus.Failure;
            }

            return r;
        }


        //CHAT: chat controlled by the behavior system
        public RunStatus ProcessChat(XmlNode myNode)
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

            try
            {

                bot.lastBehaviorChatInput = "";
                bot.lastBehaviorChatOutput = "";
                if (bot.chatInputQueue.Count == 0)
                {
                    return RunStatus.Success;
                }
               
                if (bot.chatInputQueue.Count > 0)
                {
                    bot.lastBehaviorChatInput = bot.chatInputQueue.Dequeue();
                    sentStr += bot.lastBehaviorChatInput;
                }
                Request r = new Request(sentStr, bot.lastBehaviorUser, bot);
                Result res= bot.Chat(r, graphName);
                //bot.lastBehaviorChatOutput=res.Output;
                bot.lastBehaviorChatOutput = "";
                bot.postOutput(res.Output);
                if (res.isValidOutput)
                {
                    return RunStatus.Success;
                }
                else
                {
                    return RunStatus.Failure;
                }
            }
            catch (Exception e)
            {
                Console.WriteLine("Error ProcessChat '{0}' '{1}':{2}", sentStr, bot.lastBehaviorChatInput, e.Message);
                return RunStatus.Failure;
            }
        }

        // Propositional Reasoner interface
        // Assert to KB
        public RunStatus ProcessTellKB(XmlNode myNode)
        {
            string sentStr = myNode.InnerXml;
            try
            {
            sentStr = sentStr.Replace("implies", "=>");
            sentStr = sentStr.Replace(" imp ", "=> ");
            sentStr = sentStr.Replace("equiv", "<=>");

            bot.myKB.Tell(sentStr);
            return RunStatus.Success;
            }
            catch (Exception e)
            {
                Console.WriteLine("Error ProcessTellKB '{0}':{1}", sentStr, e.Message);
                return RunStatus.Failure;
            }
        }


        public RunStatus ProcessTellBaseKB(XmlNode myNode)
        {
            string sentStr = myNode.InnerXml;
            try
            {
                sentStr = sentStr.Replace("implies", "=>");
                sentStr = sentStr.Replace(" imp ", "=> ");
                sentStr = sentStr.Replace("equiv", "<=>");

                bot.myBaseKB.Tell(sentStr);
                return RunStatus.Success;
            }
            catch(Exception e)
            {
                Console.WriteLine("Error ProcessTellBaseKB '{0}':{1}",sentStr,e.Message);
                return RunStatus .Failure ;
            }
        }
        public RunStatus ProcessTellKBOCC(XmlNode myNode)
        {
            try
            {
            addOCCLogicForInteraction(bot.myBaseKB, myNode.InnerXml);
            return RunStatus.Success;
            }
            catch (Exception e)
            {
                Console.WriteLine("Error ProcessTellKBOCC '{0}':{1}", myNode.InnerXml, e.Message);
                return RunStatus.Failure;
            }
        }
        // 
        public RunStatus ProcessClearKB(XmlNode myNode)
        {
            try
            {
            bot.myKB = new KnowledgeBase();
            return RunStatus.Success;
            }
            catch (Exception e)
            {
                Console.WriteLine("Error ProcessClearKB '{0}':{1}", myNode.InnerXml, e.Message);
                return RunStatus.Failure;
            }
        }
        public RunStatus ProcessClearBaseKB(XmlNode myNode)
        {
            try
            {
            bot.myBaseKB = new KnowledgeBase();
            return RunStatus.Success;
            }
            catch (Exception e)
            {
                Console.WriteLine("Error ProcessClearBaseKB '{0}':{1}", myNode.InnerXml, e.Message);
                return RunStatus.Failure;
            }
        }

        // Process KB
        public double trialsMemory = 2048;

        public RunStatus ProcessKBModel(XmlNode myNode)
        {
            Console.WriteLine("*** PROCESSING KB ***");
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
            Model MiniModel = new Model();
            MiniSatCSSolver myMiniSolver = new MiniSatCSSolver();
  
            Console.WriteLine("*** MiniSatCSSolver ***");

            string miniLog = String.Format(@"./MiniTotalKB.txt");
            System.IO.File.WriteAllText(miniLog, totalKBs );
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
            string myReport = "";
            string miniPostives = "";
            string walkPostives = "";
            if ((myMiniSolver.wasSAT) && (MiniModel != null))
            {
                bot.myModel = MiniModel;
                bot.myActiveModel = MiniModel;
                miniPostives = MiniModel.strPositives();
                myReport = answer;
                bot.setBBHash("foundSATModel", "True");
            }
            else
            {
                bot.setBBHash("foundSATModel", "False");
            }

                //
                if (trialsMemory < 512) trialsMemory = 512;
                if (trialsMemory > 10240) trialsMemory = 10240;

                Model walkModel = bot.myWalkSAT.FindModelFor(totalKBs, (int)(trialsMemory * 1.5), 0.5, bot.myActiveModel);
               // bot.myModel = bot.myWalkSAT.FindModelFor(totalKBs, (int)(trialsMemory * 1.5), 0.5, MiniModel);
                trialsMemory = Ema(bot.myWalkSAT.trials, trialsMemory, 5);

                myReport += bot.myWalkSAT.ExamineClauseStatistics(0.05);
                if ((walkModel == null) && (myMiniSolver.wasSAT==false))
                {
                    // We fail if we are unsat
                    Console.WriteLine("NO SAT MODEL FOUND");
                    if (bot.myActiveModel == null)
                    {
                        bot.setBBHash("activeModel", "(NOT sat)");
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

                    return RunStatus.Failure;
                }

               if (walkModel !=null)
                {
                    bot.myModel = walkModel;
                    walkPostives = walkModel.strPositives();
                }

            // update active model
            bot.myActiveModel = bot.myModel;
            string totalModel = bot.myModel.AsSentenceString();
            string postPositives = bot.myModel.strPositives();
            bot.myPositiveSATModleString = postPositives;
            //bot.bbSetHash("activeModel", totalModel);
            bot.setBBHash("activeModel", postPositives);
            Console.WriteLine("SAT MODEL FOUND:{0}", postPositives);
            try
            {
                satCount++;
                satCount = satCount % satMod;
                string logFileName = String.Format(@"./lastSatModel{0}.txt", satCount);
                System.IO.File.WriteAllText(logFileName,postPositives+"\n"+ totalModel + "\n" + myReport);
            }
            catch
            {
                Console.WriteLine("ERR: CANNOT WRITE SAT MODEL TO FILE");
            }

            return RunStatus.Success;
            }
            catch (Exception e)
            {
                Console.WriteLine("Error ProcessKBModel '{0}':{1}\n{2}", myNode.InnerXml, e.Message,e.StackTrace);
                return RunStatus.Failure;
            }
        }

        public RunStatus ProcessSequence(XmlNode myNode)
        {
            // Execute children in order until one returns false
            foreach (XmlNode childNode in myNode.ChildNodes)
            {
                RunStatus childResult= processNode(childNode);
                if (isAnAssert(childNode.Name))
                {
                    // Except for Asserts
                    if (childResult == RunStatus.Failure)
                    {
                        return RunStatus.Failure;
                    }
                }
                else
                {
                    // Normal processing
                    if (childResult == RunStatus.Failure)
                    {
                        return RunStatus.Failure;
                    }
                }
            }
            return RunStatus.Success;

        }

        public RunStatus ProcessSelector(XmlNode myNode)
        {
            // Execute children until one returns true
            foreach (XmlNode childNode in myNode.ChildNodes)
            {
                RunStatus childResult = processNode(childNode);
                // Except for Asserts
                if (isAnAssert(childNode.Name))
                {
                    if (childResult == RunStatus.Failure)
                    {
                        return RunStatus.Failure;
                    }
                }
                else
                {
                    // Normal processing
                    if (childResult == RunStatus.Success)
                    {
                        return RunStatus.Success;
                    }
                }
            }
            return RunStatus.Failure;
        }

        public RunStatus ProcessRandomSelector(XmlNode myNode)
        {
            // Pick a random child with equal weighting
            //int selected = rgen.Next(myNode.ChildNodes.Count);
            //XmlNode childNode = myNode.ChildNodes[selected];
            XmlNode childNode = this.bot.myRandMem.selectOneXML(myNode);
            RunStatus childResult = processNode(childNode);
            return childResult;
        }

        public RunStatus ProcessWeightedSelector(XmlNode myNode)
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
                    totalWeight += 1 / (1+myNode.ChildNodes.Count);
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
                    RunStatus childResult = processNode(childNode);
                    return childResult;
                }
            }

            return RunStatus.Failure;

        }

        public RunStatus ProcessParallel(XmlNode myNode)
        {
            // Execute All Children
            foreach (XmlNode childNode in myNode.ChildNodes)
            {
                RunStatus childResult = processNode(childNode);
                // Except for Asserts
                if (isAnAssert(childNode.Name))
                {
                    if (childResult == RunStatus.Failure)
                    {
                        return RunStatus.Failure;
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
            return RunStatus.Success ;
        }

        public RunStatus ProcessLoop(XmlNode myNode)
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
                try
                {
                    if ((maxloop > 0) && (loopCount >= maxloop))
                    {
                        return RunStatus.Success;
                    }
                    // Execute All Children
                    foreach (XmlNode childNode in myNode.ChildNodes)
                    {
                        RunStatus childResult = processNode(childNode);
                        // Except for Asserts
                        if (isAnAssert(childNode.Name))
                        {
                            if (childResult == RunStatus.Failure)
                            {
                                return RunStatus.Failure;
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
                    Thread.Sleep(tickRate);
                }
                catch
                { return RunStatus.Failure;  }
                loopCount++;
            }
            //return RunStatus.Failure;
        }

        public RunStatus ProcessLoopUntil(XmlNode myNode)
        {
            // you can specify the maximum number of times to loop
            // after which it will return FAILURE
            //   since we would have left if any child was successful
            //   after N trials nothing worked so we should return FAIL
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
            // We go through all chil
            while (true)
            {
                try
                {
                    if ((maxloop > 0) && (loopCount >= maxloop))
                    {
                        return RunStatus.Failure;
                    }
                    // Execute All Children
                    foreach (XmlNode childNode in myNode.ChildNodes)
                    {
                        RunStatus childResult = processNode(childNode);
                        // Except for Asserts
                        if (isAnAssert(childNode.Name))
                        {
                            if (childResult == RunStatus.Failure)
                            {
                                return RunStatus.Failure;
                            }
                        }
                        else
                        {
                            // Normal processing Return on any normal child success
                            if (childResult == RunStatus.Success)
                            {
                                return RunStatus.Success;
                            }
                        }
                    }
                    Thread.Sleep(tickRate);
                }
                catch
                { return RunStatus.Failure; }
                loopCount++;
            }
            //return RunStatus.Failure;
        }
        public RunStatus ProcessTask(XmlNode myNode)
        {
            RunStatus result = RunStatus.Success;
            if (myNode == null) return result;

            try
            {
                if ((myNode.Attributes != null)&&(myNode .Attributes .Count >0))
                {
                    string returnV = myNode.Attributes["return"].Value;
                    if (returnV.ToLower() == "failure") { result = RunStatus.Failure; }
                    if (returnV.ToLower() == "success") { result = RunStatus.Success; }
                }
            }
            catch(Exception e)
            {
                result = RunStatus.Success;

            }
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
            return result;

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

            return result;
        }


        public RunStatus ProcessStateAiml(XmlNode myNode)
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
                resultAIMLDoc.LoadXml( "<aiml graph='"+graphName+"'>" + myNode.InnerXml + "</aiml>");
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
            return result;

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

            return result;
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

    public class KeyCase : IEqualityComparer<string>
    {
        public static KeyCase Default = new KeyCase();
        #region Implementation of IEqualityComparer<string>

        /// <summary>
        /// Determines whether the specified objects are equal.
        /// </summary>
        /// <returns>
        /// true if the specified objects are equal; otherwise, false.
        /// </returns>
        /// <param name="x">The first object of type <paramref name="T"/> to compare.
        ///                 </param><param name="y">The second object of type <paramref name="T"/> to compare.
        ///                 </param>
        public bool Equals(string x, string y)
        {
            return SameKey(x, y);
        }

        /// <summary>
        /// Returns a hash code for the specified object.
        /// </summary>
        /// <returns>
        /// A hash code for the specified object.
        /// </returns>
        /// <param name="obj">The <see cref="T:System.Object"/> for which a hash code is to be returned.
        ///                 </param><exception cref="T:System.ArgumentNullException">The type of <paramref name="obj"/> is a reference type and <paramref name="obj"/> is null.
        ///                 </exception>
        public int GetHashCode(string obj)
        {
            return NormalizeKey(obj).GetHashCode();
        }

        public static string NormalizeKey(object s)
        {
            return s.ToString().Trim().ToLower().Replace(" ", "_");
        }

        #endregion

        public static bool SameKey(object u1, object key)
        {
            if (Equals(u1, key)) return true;
            if (u1.GetType().IsValueType)
            {
                throw new InvalidOperationException("lcase " + u1.GetType());
            }
            if (NormalizeKey(u1) != NormalizeKey(key)) return false;
            return true;
        }
    }

    public class CIDictionary<K, V> : Dictionary<K, V>
    {
        static public IEqualityComparer<K> comp
        {
            get
            {
                return (IEqualityComparer<K>)KeyCase.Default;
            }
        }
        public CIDictionary()
            : base((IEqualityComparer<K>)comp)
        {

        }
        public CIDictionary(IDictionary<K, V> dict)
            : base(dict, (IEqualityComparer<K>)comp)
        {

        }
    }
    public class CIDictionary2<U1, U2> : IDictionary<U1, U2>
    {

        private KeyValuePair<U1, U2> GetItem(KeyValuePair<U1, U2> item)
        {
            KeyValuePair<U1, U2>? realKeyValue = GetKV(item.Key);
            if (realKeyValue != null)
            {
                item = new KeyValuePair<U1, U2>(realKeyValue.Value.Key, item.Value);
            }
            return item;
        }
        private KeyValuePair<U1, U2>? GetKV(U1 key)
        {
            foreach (var u1 in backing)
            {
                if (KeyCase.SameKey(u1.Key, key)) return u1;
            }
            return null;
        }

        readonly IDictionary<U1, U2> backing = new Dictionary<U1, U2>();

        public CIDictionary2()
        {

        }

        public CIDictionary2(IDictionary<U1, U2> table)
        {
            foreach (KeyValuePair<U1, U2> kv in table)
            {
                this[kv.Key] = kv.Value;
            }
        }

        public IEnumerator<KeyValuePair<U1, U2>> GetEnumerator()
        {
            return backing.GetEnumerator();
        }

        IEnumerator IEnumerable.GetEnumerator()
        {
            return backing.GetEnumerator();
        }

        public void Add(KeyValuePair<U1, U2> item)
        {
            var item2 = GetItem(item);
            backing.Add(item2);
        }

        public void Clear()
        {
            backing.Clear();
        }

        public bool Contains(KeyValuePair<U1, U2> item)
        {
            item = GetItem(item);
            return backing.Contains(item);
        }

        public void CopyTo(KeyValuePair<U1, U2>[] array, int arrayIndex)
        {
            backing.CopyTo(array, arrayIndex);
        }

        public bool Remove(KeyValuePair<U1, U2> item)
        {
            item = GetItem(item);
            return backing.Remove(item);
        }

        public int Count
        {
            get { return backing.Count; }
        }

        public bool IsReadOnly
        {
            get { return backing.IsReadOnly; }
        }

        public bool ContainsKey(U1 key)
        {
            KeyValuePair<U1, U2>? realKeyValue = GetKV(key);
            return (realKeyValue != null);
        }

        public void Add(U1 key, U2 value)
        {
            KeyValuePair<U1, U2>? realKeyValue = GetKV(key);
            if (realKeyValue != null)
            {
                key = realKeyValue.Value.Key;
            }
            backing.Add(key, value);
        }

        public bool Remove(U1 key)
        {
            KeyValuePair<U1, U2>? realKeyValue = GetKV(key);
            if (realKeyValue == null) return false;
            return backing.Remove(realKeyValue.Value.Key);
        }

        public bool TryGetValue(U1 key, out U2 value)
        {
            value = default(U2);
            KeyValuePair<U1, U2>? realKeyValue = GetKV(key);
            if (realKeyValue == null) return false;
            value = realKeyValue.Value.Value;
            return true;
        }

        public U2 this[U1 key]
        {
            get
            {
                U2 v;
                if (TryGetValue(key, out v))
                {
                    return v;
                }
                return backing[key];
            }

            set { backing[key] = value; }
        }

        public ICollection<U1> Keys
        {
            get { return backing.Keys; }
        }

        public ICollection<U2> Values
        {
            get { return backing.Values; }
        }
    }

}
