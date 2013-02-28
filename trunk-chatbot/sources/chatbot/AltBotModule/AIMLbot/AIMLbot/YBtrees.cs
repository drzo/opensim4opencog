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
using AltAIMLbot.Variables;
using Aima.Core.Logic.Propositional.Algorithms;
using Aima.Core.Logic.Propositional.Parsing;
using Aima.Core.Logic.Propositional.Parsing.AST;
using Aima.Core.Logic.Propositional.Visitors;
using AltAIMLParser;
using LAIR.Collections.Generic;
using MiniSatCS;
using System.Reflection;
using MushDLR223.ScriptEngines;
using MushDLR223.Virtualization;
using AltAIMLbot;
using VDS.RDF.Parsing;
using LogicalParticleFilter1;
using LAIR.ResourceAPIs.WordNet;
using CAMeRAVUEmotion;
using MushDLR223.Utilities;
using BTXmlNode = System.Xml.XmlNode;
#if (COGBOT_LIBOMV || USE_STHREADS || true)
using ThreadPoolUtil;
using Thread = ThreadPoolUtil.Thread;
using ThreadPool = ThreadPoolUtil.ThreadPool;
using Monitor = ThreadPoolUtil.Monitor;
using NativeThread = System.Threading.Thread;
#endif

using BCTX = AltAIMLbot.AltBot;

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
    public class ListOfIters : IEnumerator<RunStatus>, IEnumerable<RunStatus>
    {
        public List<Func<IEnumerator<RunStatus>>> Iters;
        private IEnumerator<RunStatus> curIter = null;

        public bool pastEnd
        {
            get
            {
                if (LastException != null) return true;
                return listIndex >= 0 && curIter == null;
            }
        }

        private RunStatus result;
        [ThreadStatic] 
        static bool inToString = false;
        public override string ToString()
        {
            bool b4 = inToString;
            try
            {
                inToString = true;
                return "ListOfIters=" + (listIndex) + "/" + Iters.Count + ".Current=" + Current + " hasNxt=" + HasNext + " " +
                       LastException;
            }
            finally
            {
                inToString = b4;
            }
        }
        public ListOfIters(List<Func<IEnumerator<RunStatus>>> list)
        {
            this.Iters = list;
            this.listIndex = -1;
        }

        #region IEnumerator<RunStatus> Members

        /// <summary>
        /// Gets the element in the collection at the current position of the enumerator.
        /// </summary>
        /// <returns>
        /// The element in the collection at the current position of the enumerator.
        /// </returns>
        public RunStatus Current
        {
            get
            {
                result = Currently;
                if (result == RunStatus.Running)
                {
                    return result;
                }
                return result;
            }
        }

        public RunStatus Currently
        {
            get
            {
                if (!beenCalled)
                {
                    RaiseException("must first call MoveNExt()");
                    return RunStatus.Running;
                }
                if (pastEnd)
                {
                    RaiseException("must call Reset()");
                    if (result == RunStatus.Running)
                    {
                        result = RunStatus.Success;
                    }
                    return result;
                }
                if (curIter == null)
                {
                    RaiseException("must call Reset2()");
                    return result;
                    return RunStatus.Success;
                }
                result = curIter.Current;
                if (result == RunStatus.Failure) return result;
                if (result == RunStatus.Success)
                {
                    if (HasNext)
                    {
                        result = RunStatus.Running;
                    }
                    return result;
                }
                return RunStatus.Running;
            }
        }

        public bool HasNext
        {
            get
            {
                var curIter = this.curIter;
                int count = Iters.Count;
                if (!beenCalled) return count > 0;
                if (pastEnd) return false;
                if (listIndex < (count - 1)) return true;
                if (curIter == null) return false;
                if (listIndex < count) return true;
                return false;
            }
        }

        public static void RaiseException(string why)
        {
            if (inToString) return;
           // throw new NotImplementedException(why);
        }

        #endregion

        #region IDisposable Members

        public void Dispose()
        {

        }

        #endregion

        #region IEnumerator Members

        object IEnumerator.Current
        {
            get { return Current; }
        }

        public bool MoveNext()
        {
            if (curIter == null)
            {
                listIndex++;
                if (listIndex >= Iters.Count)
                {
                    return false;
                }
                var f = Iters[listIndex];
                if (f == null)
                {
                    return MoveNext();
                }
                curIter = f();
                if (curIter == null)
                {
                    return MoveNext();
                }
            }
            if (!curIterMoveNext())
            {
                curIter = null;
                return MoveNext();
            }
            return true;
        }

        private bool curIterMoveNext()
        {
            result = RunStatus.Running;
            try
            {
                return curIter.MoveNext();
            }
            catch (Exception e)
            {
                LastException = e;
                if (result == RunStatus.Running)
                {
                    result = RunStatus.Failure;
                }
                Console.WriteLine("" + e);
                return true;
            }
        }

        protected bool beenCalled
        {
            get { return listIndex != -1; }
        }

        public void Reset()
        {
            curIter = null;
            LastException = null;
            listIndex = -1;
        }

        #endregion

        public IEnumerator<RunStatus> GetEnumerator()
        {
            return this;
        }

        IEnumerator IEnumerable.GetEnumerator()
        {
            return this;
        }

        //public List<Func<IEnumerator<RunStatus>>>.Enumerator iiter { get; set; }

        public int listIndex { get; set; }

        public Exception LastException { get; set; }
    }

    public class EnumeratorToEnumerable : IEnumerator<RunStatus>, IEnumerable<RunStatus>
    {
        private IEnumerator<RunStatus> curIter;
        private IEnumerable<RunStatus> curAble;
        public EnumeratorToEnumerable(IEnumerator<RunStatus> rso )
        {
            curIter = rso;
        }
        public EnumeratorToEnumerable(IEnumerable<RunStatus> rse )
        {
            curAble = rse;
        }
        #region IEnumerable<RunStatus> Members

        IEnumerator<RunStatus> IEnumerable<RunStatus>.GetEnumerator()
        {
            if (curAble != null)
            {
                return new EnumeratorToEnumerable(curAble.GetEnumerator());
            }
            return this;
        }

        #endregion

        #region IEnumerable Members

        IEnumerator IEnumerable.GetEnumerator()
        {
            return ((IEnumerable<RunStatus>)this).GetEnumerator();
        }

        #endregion

        #region IEnumerator<RunStatus> Members

        RunStatus IEnumerator<RunStatus>.Current
        {
            get { return curIter.Current; }
        }

        #endregion

        #region IDisposable Members

        void IDisposable.Dispose()
        {
            curIter.Dispose();
        }

        #endregion

        #region IEnumerator Members

        object IEnumerator.Current
        {
            get { return curIter.Current; }
        }

        bool IEnumerator.MoveNext()
        {
            return curIter.MoveNext();
        }

        void IEnumerator.Reset()
        {
            curIter.Reset();
        }

        #endregion
    }

    public class OneRunStatus : IEnumerator<RunStatus>, IEnumerable<RunStatus>
    {
        public static Func<IEnumerator<RunStatus>> oneAct(Action act)
        {
            return new OneRunStatus(act).GetEnumerator;
        }
        public static Func<IEnumerator<RunStatus>> oneAct(Func<RunStatus> act)
        {
            return new OneRunStatus(act).GetEnumerator;
        }

        public OneRunStatus()
        {
        }
        public OneRunStatus(Action any)
        {
            Once = () =>
            {
                any();
                return RunStatus.Success;
            };
        }
        public OneRunStatus(Action any, RunStatus result0)
        {
            Once = () =>
            {
                any();
                return result0;
            };
        }
        public OneRunStatus(Func<RunStatus> any)
        {
            Once = any;
        }

        public Func<RunStatus> Once;
        private bool beenCalled = false;
        public void Dispose()
        {
           // throw new NotImplementedException();
        }

        public bool MoveNext()
        {
            if (beenCalled)
            {
                if (result == RunStatus.Running)
                {
                    result = RunStatus.Success;
                }
                pastEnd = true;
                return false;
            }
            result = RunStatus.Running;
            beenCalled = true;
            try
            {
                result = Once();
            }
            catch (Exception e)
            {
                if (result == RunStatus.Running)
                {
                    result = RunStatus.Failure;
                }
                Console.WriteLine("" + e);
            }
            finally
            {
                if (result == RunStatus.Running)
                {
                    result = RunStatus.Success;
                }
            }
            return true;

        }

        public void Reset()
        {
            beenCalled = false;
            pastEnd = false;
        }

        public RunStatus Current
        {
            get
            {
                if (!beenCalled)
                {
                    ListOfIters.RaiseException("must first call MoveNExt()");
                    return RunStatus.Running;
                }
                if (pastEnd) 
                {
                    ListOfIters.RaiseException("must call Reset()");
                    return RunStatus.Success;
                }
                return result;
            }
        }

        object IEnumerator.Current
        {
            get { return Current; }
        }

        public RunStatus result { get; set; }

        #region IEnumerable<RunStatus> Members

        public IEnumerator<RunStatus> GetEnumerator()
        {
            return this;
        }

        #endregion

        #region IEnumerable Members

        IEnumerator IEnumerable.GetEnumerator()
        {
            return ((IEnumerable<RunStatus>)this).GetEnumerator();
        }

        #endregion

        public bool pastEnd { get; set; }
    }

    public class BTXmlDocument : XmlDocument
    {
        public override XmlElement CreateElement(string prefix, string localname, string nsURI)
        {
            var elem = new BTXmlNodeImpl(prefix, localname, nsURI, this);
            return elem;
        }
    }

    /// <summary>
    /// Allows any BTXML node operation to work on any XML node
    /// </summary>
    public static class BTXmlNodeExtensions
    {
        static public XmlNode CloneNode(this XmlNode baze, bool deep)
        {
            return baze.Clone();
        }
        static public XmlNode CloneNodeV(this XmlNode baze, bool deep)
        {
            return baze.Clone();
        }

        static public string InnerTextV(this XmlNode baze)
        {
            var it = baze.InnerText;
            if (baze.InnerXml != it)
            {
                return baze.InnerXml;
            }
            return it;
        }

        static public string AttributeValueOfDefault(this XmlNode baze, string attrib, string def)
        {
            if (baze.Attributes != null)
            {
                if (baze.Attributes[attrib] != null)
                    return baze.Attributes[attrib].Value;
            }
            return def;
        }

        static public string AttributesV(this XmlNode baze, string attrib)
        {
            if (baze.Attributes != null)
            {
                if (baze.Attributes[attrib] != null)
                    return baze.Attributes[attrib].Value;
            }
            foreach (XmlNode childNode in baze.ChildNodes)
            {
                if (childNode.NodeType == XmlNodeType.Element)
                {
                    if (childNode.Name == attrib)
                        return childNode.InnerTextV();
                }

            }
            return string.Empty;
        }
        static public XmlNode OwnerElement(this XmlNode baze)
        {
            // KHC: probably wrong but closest
            {
                if (baze.NodeType== XmlNodeType.Attribute)
                    return baze.ParentNode;
                else
                    return null;
            }
        }

        static internal Dictionary_usingWeakKey<object, XmlNode> CloneOfDict = new Dictionary_usingWeakKey<object, XmlNode>();
        static public XmlNode get_CloneOf(this XmlNode baze)
        {
            lock (CloneOfDict)
            {
                XmlNode clone;
                if (CloneOfDict.TryGetValue(baze, out clone))
                {
                    return clone;
                }
                return null;
            }
        }
        static public void set_CloneOf(this XmlNode baze, XmlNode cloned)
        {
            lock (CloneOfDict)
            {
                CloneOfDict[baze] = cloned;
            }
        }
    }

    public class BTXmlNodeImpl : XmlElement
    {
        public string ADebugString
        {
            get { return base.OuterXml; }
        }

        public override string ToString()
        {
            return base.OuterXml;
        }
        public BTXmlNodeImpl(string prefix, string localName, string namespaceURI, XmlDocument doc)
            : base(prefix, localName, namespaceURI, doc)
        {
        }

        public string AttributesV(string attrib)
        {           
            if (base.Attributes != null)
            {
                if (base.Attributes[attrib] != null)
                    return base.Attributes[attrib].Value;
            }
            foreach (XmlNode childNode in base.ChildNodes)
            {
                if (childNode.NodeType == XmlNodeType.Element)
                {
                    if (childNode.Name == attrib)
                        return childNode.InnerText;
                }

            }
            return string.Empty;
        }

        public override void WriteContentTo(XmlWriter w)
        {
            base.WriteContentTo(w);
        }

        public override void WriteTo(XmlWriter w)
        {
            base.WriteTo(w);
        }

        public XmlNode CloneOf { get; set; }

        public override XmlNode CloneNode(bool deep)
        {
            return base.CloneNode(deep);
            return base.Clone();
        }

        public override XmlNodeType NodeType
        {
            get { return base.NodeType; }
        }

        public override string LocalName
        {
            get { return base.LocalName; }
        }

        public override string Name
        {
            get { return base.Name; }
        }

        public BTXmlNode OwnerElement
        {
            // KHC: probably wrong but closest
            get
            {
                if (this.NodeType == XmlNodeType.Attribute)
                    return (BTXmlNode)base.ParentNode;
                else
                    return null;
            }
        }

    }

    #region BTXML

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

        public BCTX contextBot
        {
            get { return _bot; }
            set
            {
                if (_bot == value) return;
                _bot = value;
            }
        }

        private BehaviorSet myLocalBehaviors
        {
            get { return contextBot.myBehaviors; }
        }

        [NonSerialized] public SymbolicParticleFilter ourFilter = new SymbolicParticleFilter();


        [NonSerialized] internal AltBot _bot;
        [NonSerialized] public BTXmlDocument treeDoc;
        [NonSerialized] public Thread OnlyThisThread = null;

        // Kinda based on the idea at ...
        // http://www.garagegames.com/community/blogs/view/21143
        public BehaviorTree(AltBot bot)
        {
            _bot = bot;
            treeDoc = new BTXmlDocument();
            restorePoint = new Dictionary<string, int>();

        }


        public void preSerial()
        {
            serialDoc = treeDoc.OuterXml;
        }

        public void postSerial(BCTX bot)
        {
            contextBot = bot;
            if (treeDoc == null) treeDoc = new BTXmlDocument();
            treeDoc.LoadXml(serialDoc);
        }

        public IEnumerable<RunStatus> evalBehaviorXml(string behaviorDef)
        {
            BTXmlDocument evalDoc = new BTXmlDocument();
            try
            {

                behaviorDef = FixXmlEnitites(behaviorDef);
                evalDoc.LoadXml(behaviorDef);
            }
            catch (Exception e)
            {
                LogException(e, "evalBehaviorXml = {0}", behaviorDef);
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


        public static string FixXmlEnitites(string stateDef)
        {
            stateDef =
                stateDef.Replace("&gt;", ">").Replace("&lt;", "<").Replace("&quot;", "\"").Replace("&amp;", "(Zamp)").
                    Replace
                    ("&apos", "'").Replace("&0a", "\n");
            if (stateDef.Contains("&"))
            {
                throw new NotImplementedException("Fogot to clean some entity? " + stateDef);
            }
            return stateDef.Replace("(Zamp)", "&");
        }

        public void defineBehavior(string mname, string behaviorDef)
        {
            try
            {
                name = mname;
                behaviorDef = FixXmlEnitites(behaviorDef);

                treeDoc.LoadXml(behaviorDef);
                //initialState = rulesDoc.FirstChild.AttributesV("initialstate");
                //transitionTime = Environment.TickCount;
                //curState = initialState;
                //XmlNodeList rules = rulesDoc.SelectNodes(String.Format(ruleMatch, initialState));
            }
            catch (Exception e)
            {
                LogException(e, "defineBehavior {0}={1}", mname, behaviorDef);
            }
        }

        private void LogException(Exception exception, string f, params object[] a)
        {
            try
            {
                LogException0(null, exception, f, a, BehaviorSet.LogToConsole ?? Console.WriteLine);
            }
            catch (Exception e)
            {
                Console.WriteLine("ERROR " + f + " " + e);
            }
        }

        private void LogException(XmlNode node, Exception exception, string f, params object[] a)
        {
            try
            {
                LogException0(node, exception, f, a, BehaviorSet.LogToConsole ?? Console.WriteLine);
            }
            catch (Exception e)
            {
                Console.WriteLine("ERROR " + f + " " + e);
            }
        }

        private void LogException0(XmlNode node, Exception exception, string f, object[] a, OutputDelegate cw)
        {
            if (a != null && a.Length > 1)
            {
                //header
                cw("ERROR:" + f);
            }
            string stuff1 = DLRConsole.SafeFormat(f, a);
            cw("ERR:" + stuff1);
            if (exception != null)
            {
                string stuff2 = EMsg(exception);
                if (!stuff1.Contains(stuff2))
                {
                    cw("ERR:" + stuff1);
                }
            }
            if (node != null)
            {
                cw("ERROR XML: " + node.OuterXml);
            }
        }

        public object TreeNameLock = new object();

        // will be used to restore some use states
        public IEnumerable<RunStatus> monitorNode(BCTX bot, Func<IEnumerable<RunStatus>> processNode0, BTXmlNode myNode)
        {
            Func<IEnumerable<RunStatus>> processNode1;
            var o = bot.PreNodeProcess(this, processNode0, myNode, out processNode1);
            if (o == null)
            {
                return processNode1();
            }
            try
            {
                return processNode1();
            }
            finally
            {
                bot.PostNodeProcess(this, myNode, o);
            }
        }

        public IEnumerable<RunStatus> runBehaviorTree(BCTX theBot)
        {
            return monitorNode(theBot, () => runBehaviorTree1(theBot), treeDoc);
        }

        public IEnumerable<RunStatus> runBehaviorTree1(BCTX theBot)
        {
            _bot = theBot;
            // Execute All Children
            foreach (XmlNode childNode in treeDoc.ChildNodes)
            {
                RunStatus childResult = RunStatus.Failure;
                foreach (RunStatus myChildResult in processNode((BTXmlNode) ((XmlElement) childNode)))
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
        public IEnumerable<RunStatus> runSubTree(BTXmlDocument subDoc)
        {
            // Execute All Children
            foreach (BTXmlNode childNode in subDoc.ChildNodes)
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
            double K = (double) 2/(double) (1 + N);
            return (double) (K*(newVal - oldVal)) + oldVal;
        }

        public bool isAnAssert(string nodeName)
        {
            return (
                       (nodeName.ToLower() == "assert")
                       || (nodeName.ToLower() == "asserttimer")
                       || (nodeName.ToLower() == "assertguest")
                       || (nodeName.ToLower() == "assertmenu")
                       || (nodeName.ToLower() == "assertprolog")
                       || (nodeName.ToLower().StartsWith("assert"))
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
        public string getIndent(BTXmlNode node)
        {
            if (node == null || node.ParentNode == null)
            {
                // the only node with no parent is the root node, which has no path 
                return "";
            }
            if (node.ParentNode.NodeType == XmlNodeType.Document)

                return "  " + getIndent(node.ParentNode as BTXmlDocument);
            else
                return "  " + getIndent((BTXmlNode) (node.ParentNode));
        }

        public string getIndent(BTXmlDocument node)
        {
            if (node == null || node.ParentNode == null)
            {
                // the only node with no parent is the root node, which has no path 
                return "";
            }
            if (node.ParentNode.NodeType == XmlNodeType.Document)

                return "  " + getIndent(node.ParentNode as BTXmlDocument);
            else
                return "  " + getIndent((BTXmlNode) (node.ParentNode));
        }

        public string GetXPathToNode(BTXmlNode node)
        {
            if (node.NodeType == XmlNodeType.Attribute)
            {
                // attributes have an OwnerElement, not a ParentNode; also they have              
                // to be matched by name, not found by position              
                return String.Format("{0}/@{1}", GetXPathToNode(node.OwnerElement()), node.Name);
            }
            if (node.ParentNode == null)
            {
                // the only node with no parent is the root node, which has no path 
                return "";
            }

            //get the index 
            int iIndex = 1;
            BTXmlNode xnIndex = node;
            while (xnIndex.PreviousSibling != null && xnIndex.PreviousSibling.Name == xnIndex.Name)
            {
                iIndex++;
                xnIndex = (BTXmlNode) xnIndex.PreviousSibling;
            }

            // the path to a node is the path to its parent, plus "/node()[n]", where 
            // n is its position among its siblings.          
            return String.Format("{0}/{1}[{2}]", GetXPathToNode((BTXmlNode) node.ParentNode), node.Name, iIndex);
        }

        public RunStatus tickMonitor()
        {
            RunStatus result = RunStatus.Success;
            if (monitorBehavior == null) return RunStatus.Success;
            if (monitorBehavior.Length == 0) return RunStatus.Success;
            if (myLocalBehaviors.behaveTrees.ContainsKey(monitorBehavior))
            {
                result = myLocalBehaviors.runBotBehavior(monitorBehavior, contextBot);
            }
            return result;
        }

        public IEnumerable<RunStatus> atomicSuccess()
        {
            yield return RunStatus.Success;
        }

        public IEnumerable<RunStatus> atomicFailure()
        {
            yield return RunStatus.Failure;
        }

        public IEnumerable<RunStatus> atomicRunning()
        {
            yield return RunStatus.Running;
        }

        public void logText(string msg)
        {
            BehaviorSet.logTextToBTTrace(msg);
        }

        public void logNode(string msg, XmlNode myNode)
        {
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
            BehaviorSet.logTextToBTTrace(String.Format("{1} {0}<{2} {3} >\n", msg, indent, myNode.Name.ToLower(), astr));
        }

        public IEnumerable<RunStatus> processNode(BTXmlNode myNode)
        {
            return monitorNode(this.contextBot, () => processNode0(myNode), myNode);
        }

        public IEnumerable<RunStatus> processNode0(BTXmlNode myNode)
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
                if (!string.IsNullOrEmpty(myNode.AttributesV("id")))
                {
                    nodeID = myNode.AttributesV("id");
                }
            }
            catch (Exception e)
            {
                nodeID = "null";
            }

            var rs = myLocalBehaviors.runState;
            Action<RunStatus> SetCurNodeIdStatus = v => { lock (rs) rs[nodeID] = v; };
            // Initiate our status
            SetCurNodeIdStatus(RunStatus.Running);

            bool origCritical = contextBot.inCritical;
            logNode("BEGIN", myNode);
            ProcessNodeAddEvents(myNode);
            // Console.WriteLine("Process BNode {1} {0}", nodeID, myNode.Name.ToLower());
            // Start a winner
            myLocalBehaviors.keepTime(nodeID, RunStatus.Success);
            //check watchdog if any
            tickMonitor();

#if false

            RunStatus childResult = RunStatus.Failure;
            foreach (var myChildResult in processNodeSw(myNode, SetCurNodeIdStatus, nodeID, origCritical))
            {
                childResult = myChildResult;
                if (childResult != RunStatus.Running) break;
                yield return RunStatus.Running;
            }
        }


        public IEnumerable<RunStatus> processNodeSw(BTXmlNode myNode, Action<RunStatus> SetCurNodeIdStatus, string nodeID, bool origCritical)
        {
            RunStatus myResult = RunStatus.Failure;
#endif
            {

                switch (myNode.Name.ToLower())
                {
                    case "assert":
                        foreach (RunStatus result in ProcessAssert(myNode))
                        {
                            myResult = result;
                            SetCurNodeIdStatus(myResult);
                            if (myResult != RunStatus.Running) break;
                            yield return result;
                        }
                        break;
                    case "asserttimer":
                        foreach (RunStatus result in ProcessAssertTimer(myNode))
                        {
                            myResult = result;
                            SetCurNodeIdStatus(myResult);
                            if (myResult != RunStatus.Running) break;
                            yield return result;
                        }
                        break;
                    case "sequence":
                        foreach (RunStatus result in ProcessSequence(myNode))
                        {
                            myResult = result;
                            SetCurNodeIdStatus(myResult);
                            if (myResult != RunStatus.Running) break;
                            yield return result;
                        }
                        break;
                    case "selector":
                        foreach (RunStatus result in ProcessSelector(myNode))
                        {
                            myResult = result;
                            SetCurNodeIdStatus(myResult);
                            if (myResult != RunStatus.Running) break;
                            yield return result;
                        }
                        break;
                    case "random":
                        foreach (RunStatus result in ProcessRandomSelector(myNode))
                        {
                            myResult = result;
                            SetCurNodeIdStatus(myResult);
                            if (myResult != RunStatus.Running) break;
                            yield return result;
                        }
                        break;
                    case "weighted":
                        foreach (RunStatus result in ProcessWeightedSelector(myNode))
                        {
                            myResult = result;
                            SetCurNodeIdStatus(myResult);
                            if (myResult != RunStatus.Running) break;
                            yield return result;
                        }
                        break;
                    case "parallel":
                        foreach (RunStatus result in ProcessParallel(myNode))
                        {
                            myResult = result;
                            SetCurNodeIdStatus(myResult);
                            if (myResult != RunStatus.Running) break;
                            yield return result;
                        }
                        break;
                    case "loop":
                        foreach (RunStatus result in ProcessLoop(myNode))
                        {
                            myResult = result;
                            SetCurNodeIdStatus(myResult);
                            if (myResult != RunStatus.Running) break;
                            yield return result;
                        }
                        break;
                    case "loopuntil":
                        foreach (RunStatus result in ProcessLoopUntil(myNode))
                        {
                            myResult = result;
                            SetCurNodeIdStatus(myResult);
                            if (myResult != RunStatus.Running) break;
                            yield return result;
                        }
                        break;
                    case "task":
                        foreach (RunStatus result in ProcessTask(myNode))
                        {
                            myResult = result;
                            SetCurNodeIdStatus(myResult);
                            if (myResult != RunStatus.Running) break;
                            yield return result;
                        }
                        break;
                    case "subaiml":
                        foreach (RunStatus result in ProcessStateAiml(myNode))
                        {
                            myResult = result;
                            SetCurNodeIdStatus(myResult);
                            if (myResult != RunStatus.Running) break;
                            yield return result;
                        }
                        break;
                    case "drive":
                        foreach (RunStatus result in ProcessDrive(myNode))
                            //Thread.Sleep(2000);
                        {
                            myResult = result;
                            SetCurNodeIdStatus(myResult);
                            if (myResult != RunStatus.Running) break;
                            yield return result;
                        }
                        break;
                    case "motive":
                        foreach (RunStatus result in ProcessMotive(myNode))
                            //Thread.Sleep(2000);
                        {
                            myResult = result;
                            SetCurNodeIdStatus(myResult);
                            if (myResult != RunStatus.Running) break;
                            yield return result;
                        }
                        break;

                    case "inhibit":
                        this.myLocalBehaviors.VSoup.setRefVal(nodeID, 0);
                        SetCurNodeIdStatus(RunStatus.Success);
                        {
                            yield return RunStatus.Success;
                        }
                        break;

                    case "release":
                        this.myLocalBehaviors.VSoup.adjust(nodeID, 1.0);
                        SetCurNodeIdStatus(RunStatus.Success);
                        {
                            yield return RunStatus.Success;
                        }
                        break;
                    case "genchemsimfrommt":
                        foreach (RunStatus result in ProcessGenChemsysFromMt(myNode))
                        {
                            myResult = result;
                            SetCurNodeIdStatus(myResult);
                            if (myResult != RunStatus.Running) break;
                            yield return result;
                        }
                        break;

                    case "behavior":
                        foreach (RunStatus result in ProcessBehavior(myNode))
                        {
                            myResult = result;
                            //Console.WriteLine("  yield {0} {1} = {2}", myNode.Name.ToLower(), nodeID, result.ToString());
                            SetCurNodeIdStatus(myResult);
                            if (myResult != RunStatus.Running) break;
                            yield return result;
                        }
                        break;
                    case "rbehavior":
                        foreach (RunStatus result in ProcessRBehavior(myNode))
                        {
                            myResult = result;
                            SetCurNodeIdStatus(myResult);
                            if (myResult != RunStatus.Running) break;
                            yield return result;
                        }
                        break;
                    case "subbehavior":
                        foreach (RunStatus result in ProcessSubBehavior(myNode))
                        {
                            myResult = result;
                            SetCurNodeIdStatus(myResult);
                            if (myResult != RunStatus.Running) break;
                            yield return result;
                        }
                        break;
                    case "scheduler":
                        foreach (RunStatus result in ProcessScheduler(myNode))
                        {
                            myResult = result;
                            SetCurNodeIdStatus(myResult);
                            if (myResult != RunStatus.Running) break;
                            yield return result;
                        }
                        break;
                    case "starttimer":
                        // start a stopwatch
                        myLocalBehaviors.keepTime(nodeID, RunStatus.Success);
                        myLocalBehaviors.activationTime(nodeID, RunStatus.Success);
                        myLocalBehaviors.runEventHandler("onsuccess", contextBot);
                        ProcessNodeDeleteEvents(myNode);
                        contextBot.inCritical = origCritical;
                        SetCurNodeIdStatus(RunStatus.Success);
                        yield return RunStatus.Success;
                        yield break;
                        break;
                    case "stoptimer":
                        // stop the stopwatch
                        myLocalBehaviors.keepTime(nodeID, RunStatus.Failure);
                        if (myLocalBehaviors.entryTime.ContainsKey(nodeID))
                        {
                            myLocalBehaviors.entryTime.Remove(nodeID);
                        }
                        myLocalBehaviors.runEventHandler("onsuccess", contextBot);
                        ProcessNodeDeleteEvents(myNode);
                        contextBot.inCritical = origCritical;
                        SetCurNodeIdStatus(RunStatus.Success);
                        yield return RunStatus.Success;
                        yield break;
                        break;

                    case "tellkb":
                        foreach (RunStatus result in ProcessTellKB(myNode))
                        {
                            myResult = result;
                            SetCurNodeIdStatus(myResult);
                            if (myResult != RunStatus.Running) break;
                            yield return result;
                        }
                        break;
                    case "tellbasekb":
                        foreach (RunStatus result in ProcessTellBaseKB(myNode))
                        {
                            myResult = result;
                            SetCurNodeIdStatus(myResult);
                            if (myResult != RunStatus.Running) break;
                            yield return result;
                        }
                        break;
                    case "tellkbocc":
                        foreach (RunStatus result in ProcessTellKBOCC(myNode))
                        {
                            myResult = result;
                            SetCurNodeIdStatus(myResult);
                            if (myResult != RunStatus.Running) break;
                            yield return result;
                        }
                        break;
                    case "clearkb":
                        foreach (RunStatus result in ProcessClearKB(myNode))
                        {
                            myResult = result;
                            SetCurNodeIdStatus(myResult);
                            if (myResult != RunStatus.Running) break;
                            yield return result;
                        }
                        break;
                    case "clearbasekb":
                        foreach (RunStatus result in ProcessClearBaseKB(myNode))
                        {
                            myResult = result;
                            SetCurNodeIdStatus(myResult);
                            if (myResult != RunStatus.Running) break;
                            yield return result;
                        }
                        break;
                    case "processkb":
                        foreach (RunStatus result in ProcessKBModel(myNode))
                        {
                            myResult = result;
                            SetCurNodeIdStatus(myResult);
                            if (myResult != RunStatus.Running) break;
                            yield return result;
                        }
                        break;

                    case "chat":
                        foreach (RunStatus result in ProcessChat(myNode))
                        {
                            myResult = result;
                            SetCurNodeIdStatus(myResult);
                            if (myResult != RunStatus.Running) break;
                            yield return result;
                        }
                        break;

                    case "setaimlvar":
                        foreach (RunStatus result in ProcessSetAimlVar(myNode))
                        {
                            myResult = result;
                            SetCurNodeIdStatus(myResult);
                            if (myResult != RunStatus.Running) break;
                            yield return result;
                        }
                        break;

                    case "assertguest":
                        foreach (RunStatus result in ProcessAssertGuest(myNode))
                        {
                            myResult = result;
                            SetCurNodeIdStatus(myResult);
                            if (myResult != RunStatus.Running) break;
                            yield return result;
                        }
                        break;
                    case "taskguest":
                        foreach (RunStatus result in ProcessTaskGuest(myNode))
                        {
                            myResult = result;
                            SetCurNodeIdStatus(myResult);
                            if (myResult != RunStatus.Running) break;
                            yield return result;
                        }
                        break;
                    case "enqueue":
                        foreach (RunStatus result in ProcessEnqueue(myNode))
                        {
                            myResult = result;
                            SetCurNodeIdStatus(myResult);
                            if (myResult != RunStatus.Running) break;
                            yield return result;
                        }
                        break;
                    case "flushqueue":
                        foreach (RunStatus result in ProcessFlushQueue(myNode))
                        {
                            myResult = result;
                            SetCurNodeIdStatus(myResult);
                            if (myResult != RunStatus.Running) break;
                            yield return result;
                        }
                        break;
                    case "breaker":
                        foreach (RunStatus result in ProcessBreaker(myNode))
                        {
                            myResult = result;
                            SetCurNodeIdStatus(myResult);
                            if (myResult != RunStatus.Running) break;
                            yield return result;
                        }
                        break;
                        //Siprolog interface
                    case "clearprologmt":
                        foreach (RunStatus result in ProcessClearPrologMt(myNode))
                        {
                            myResult = result;
                            SetCurNodeIdStatus(myResult);
                            if (myResult != RunStatus.Running) break;
                            yield return result;
                        }
                        break;
                    case "connectmt":
                        foreach (RunStatus result in ProcessConnectMt(myNode))
                        {
                            myResult = result;
                            SetCurNodeIdStatus(myResult);
                            if (myResult != RunStatus.Running) break;
                            yield return result;
                        }
                        break;
                    case "tellprologtmt":
                        foreach (RunStatus result in ProcessTellPrologMt(myNode))
                        {
                            myResult = result;
                            SetCurNodeIdStatus(myResult);
                            if (myResult != RunStatus.Running) break;
                            yield return result;
                        }
                        break;
                    case "loadke":
                        foreach (RunStatus result in ProcessLoadKEKB(myNode))
                        {
                            myResult = result;
                            SetCurNodeIdStatus(myResult);
                            if (myResult != RunStatus.Running) break;
                            yield return result;
                        }
                        break;
                    case "insertmt":
                        foreach (RunStatus result in ProcessInsertMt(myNode))
                        {
                            myResult = result;
                            SetCurNodeIdStatus(myResult);
                            if (myResult != RunStatus.Running) break;
                            yield return result;
                        }
                        break;
                    case "appendmt":
                        foreach (RunStatus result in ProcessAppendMt(myNode))
                        {
                            myResult = result;
                            SetCurNodeIdStatus(myResult);
                            if (myResult != RunStatus.Running) break;
                            yield return result;
                        }
                        break;
                    case "assertprolog":
                        foreach (RunStatus result in ProcessAssertProlog(myNode))
                        {
                            myResult = result;
                            SetCurNodeIdStatus(myResult);
                            if (myResult != RunStatus.Running) break;
                            yield return result;
                        }
                        break;

                    case "evalprologmacro":
                        foreach (RunStatus result in ProcessEvalPrologMacro(myNode))
                        {
                            myResult = result;
                            SetCurNodeIdStatus(myResult);
                            if (myResult != RunStatus.Running) break;
                            yield return result;
                        }
                        break;

                    case "inducefrommt":
                        foreach (RunStatus result in ProcessInduceFromMt(myNode))
                        {
                            myResult = result;
                            SetCurNodeIdStatus(myResult);
                            if (myResult != RunStatus.Running) break;
                            yield return result;
                        }
                        break;

                    case "inventfrommt":
                        foreach (RunStatus result in ProcessInventFromMt(myNode))
                        {
                            myResult = result;
                            SetCurNodeIdStatus(myResult);
                            if (myResult != RunStatus.Running) break;
                            yield return result;
                        }
                        break;
                    case "planfrommt":
                        foreach (RunStatus result in ProcessPlanFromMt(myNode))
                        {
                            myResult = result;
                            SetCurNodeIdStatus(myResult);
                            if (myResult != RunStatus.Running) break;
                            yield return result;
                        }
                        break;

                        //Particle filter interface
                    case "definestate":
                        foreach (RunStatus result in ProcessDefineState(myNode))
                        {
                            myResult = result;
                            SetCurNodeIdStatus(myResult);
                            if (myResult != RunStatus.Running) break;
                            yield return result;
                        }
                        break;
                    case "stateconstraintset":
                        foreach (RunStatus result in ProcessConstraintSet(myNode))
                        {
                            myResult = result;
                            SetCurNodeIdStatus(myResult);
                            if (myResult != RunStatus.Running) break;
                            yield return result;
                        }
                        break;
                    case "statesense":
                        foreach (RunStatus result in ProcessStateSense(myNode))
                        {
                            myResult = result;
                            SetCurNodeIdStatus(myResult);
                            if (myResult != RunStatus.Running) break;
                            yield return result;
                        }
                        break;
                    case "stateacttransition":
                        foreach (RunStatus result in ProcessStateActTransition(myNode))
                        {
                            myResult = result;
                            SetCurNodeIdStatus(myResult);
                            if (myResult != RunStatus.Running) break;
                            yield return result;
                        }
                        break;
                    case "quickfilter":
                        foreach (RunStatus result in ProcessQuickFilter(myNode))
                        {
                            myResult = result;
                            SetCurNodeIdStatus(myResult);
                            if (myResult != RunStatus.Running) break;
                            yield return result;
                        }
                        break;
                    case "meanparticle":
                        foreach (RunStatus result in ProcessMeanParticle(myNode))
                        {
                            myResult = result;
                            SetCurNodeIdStatus(myResult);
                            if (myResult != RunStatus.Running) break;
                            yield return result;
                        }
                        break;
                    case "genfilterfrommt":
                        foreach (RunStatus result in ProcessGenFilterFromMt(myNode))
                        {
                            myResult = result;
                            SetCurNodeIdStatus(myResult);
                            if (myResult != RunStatus.Running) break;
                            yield return result;
                        }
                        break;
                        // Kinect via Mt
                    case "updatepersona":
                        foreach (RunStatus result in ProcessUpdatePersona(myNode))
                        {
                            myResult = result;
                            SetCurNodeIdStatus(myResult);
                            if (myResult != RunStatus.Running) break;
                            yield return result;
                        }
                        break;
                        // Chatmapper
                    case "loadchatmapper":
                        foreach (RunStatus result in ProcessLoadChatMapper(myNode))
                        {
                            myResult = result;
                            SetCurNodeIdStatus(myResult);
                            if (myResult != RunStatus.Running) break;
                            yield return result;
                        }
                        break;
                    case "waitforchatinput":
                        foreach (RunStatus result in ProcessWaitForChatInput(myNode))
                        {
                            myResult = result;
                            SetCurNodeIdStatus(myResult);
                            if (myResult != RunStatus.Running) break;
                            yield return result;
                        }
                        break;
                    case "assertmenu":
                        foreach (RunStatus result in ProcessAssertMenu(myNode))
                        {
                            myResult = result;
                            SetCurNodeIdStatus(myResult);
                            if (myResult != RunStatus.Running) break;
                            yield return result;
                        }
                        break;

// Coppelia

                    case "coppeliaaction":
                        foreach (RunStatus result in ProcessCoppeliaAction(myNode))
                        {
                            myResult = result;
                            SetCurNodeIdStatus(myResult);
                            if (myResult != RunStatus.Running) break;
                            yield return result;
                        }
                        break;
                    case "coppeliaactionstate":
                        foreach (RunStatus result in ProcessCoppeliaActionState(myNode))
                        {
                            myResult = result;
                            SetCurNodeIdStatus(myResult);
                            if (myResult != RunStatus.Running) break;
                            yield return result;
                        }
                        break;
                    case "coppeliaagentfeature":
                        foreach (RunStatus result in ProcessCoppeliaAgentFeature(myNode))
                        {
                            myResult = result;
                            SetCurNodeIdStatus(myResult);
                            if (myResult != RunStatus.Running) break;
                            yield return result;
                        }
                        break;
                    case "coppeliaagentresponsiblebelief":
                        foreach (RunStatus result in ProcessCoppeliaAgentResponsibleBelief(myNode))
                        {
                            myResult = result;
                            SetCurNodeIdStatus(myResult);
                            if (myResult != RunStatus.Running) break;
                            yield return result;
                        }
                        break;
                    case "coppeliaanger":
                        foreach (RunStatus result in ProcessCoppeliaAnger(myNode))
                        {
                            myResult = result;
                            SetCurNodeIdStatus(myResult);
                            if (myResult != RunStatus.Running) break;
                            yield return result;
                        }
                        break;
                    case "coppeliaambition":
                        foreach (RunStatus result in ProcessCoppeliaAmbition(myNode))
                        {
                            myResult = result;
                            SetCurNodeIdStatus(myResult);
                            if (myResult != RunStatus.Running) break;
                            yield return result;
                        }
                        break;
                    case "coppeliadesired":
                        foreach (RunStatus result in ProcessCoppeliaDesired(myNode))
                        {
                            myResult = result;
                            SetCurNodeIdStatus(myResult);
                            if (myResult != RunStatus.Running) break;
                            yield return result;
                        }
                        break;
                    case "coppeliaactiontendancy":
                        foreach (RunStatus result in ProcessCoppeliaActionTendancy(myNode))
                        {
                            myResult = result;
                            SetCurNodeIdStatus(myResult);
                            if (myResult != RunStatus.Running) break;
                            yield return result;
                        }
                        break;

                    case "coppeliaemotion":
                        foreach (RunStatus result in ProcessCoppeliaEmotion(myNode))
                        {
                            myResult = result;
                            SetCurNodeIdStatus(myResult);
                            if (myResult != RunStatus.Running) break;
                            yield return result;
                        }
                        break;
                    case "coppeliaexpectedsatisfaction":
                        foreach (RunStatus result in ProcessCoppeliaExpectedSatisfaction(myNode))
                        {
                            myResult = result;
                            SetCurNodeIdStatus(myResult);
                            if (myResult != RunStatus.Running) break;
                            yield return result;
                        }
                        break;
                    case "coppeliafeatureemotionbelief":
                        foreach (RunStatus result in ProcessCoppeliaFeatureEmotionBelief(myNode))
                        {
                            myResult = result;
                            SetCurNodeIdStatus(myResult);
                            if (myResult != RunStatus.Running) break;
                            yield return result;
                        }
                        break;
                    case "coppeliafeaturebelief":
                        foreach (RunStatus result in ProcessCoppeliaFeatureBelief(myNode))
                        {
                            myResult = result;
                            SetCurNodeIdStatus(myResult);
                            if (myResult != RunStatus.Running) break;
                            yield return result;
                        }
                        break;
                    case "coppeliaperform":
                        foreach (RunStatus result in ProcessCoppeliaPerform(myNode))
                        {
                            myResult = result;
                            SetCurNodeIdStatus(myResult);
                            if (myResult != RunStatus.Running) break;
                            yield return result;
                        }
                        break;
                    case "coppeliapraiseworthy":
                        foreach (RunStatus result in ProcessCoppeliaPraiseworthy(myNode))
                        {
                            myResult = result;
                            SetCurNodeIdStatus(myResult);
                            if (myResult != RunStatus.Running) break;
                            yield return result;
                        }
                        break;
                    case "coppeliarelation":
                        foreach (RunStatus result in ProcessCoppeliaRelation(myNode))
                        {
                            myResult = result;
                            SetCurNodeIdStatus(myResult);
                            if (myResult != RunStatus.Running) break;
                            yield return result;
                        }
                        break;
                    case "assertcoppeila":
                        foreach (RunStatus result in ProcessAssertCoppelia(myNode))
                        {
                            myResult = result;
                            SetCurNodeIdStatus(myResult);
                            if (myResult != RunStatus.Running) break;
                            yield return result;
                        }
                        break;
                    case "coppeliaresponse":
                        foreach (RunStatus result in ProcessCoppeliaResponse(myNode))
                        {
                            myResult = result;
                            SetCurNodeIdStatus(myResult);
                            if (myResult != RunStatus.Running) break;
                            yield return result;
                        }
                        break;
                    case "coppeliastatelikelihood":
                        foreach (RunStatus result in ProcessCoppeliaStateLikelihood(myNode))
                        {
                            myResult = result;
                            SetCurNodeIdStatus(myResult);
                            if (myResult != RunStatus.Running) break;
                            yield return result;
                        }
                        break;
                    case "coppeliastate":
                        foreach (RunStatus result in ProcessCoppeliaState(myNode))
                        {
                            myResult = result;
                            SetCurNodeIdStatus(myResult);
                            if (myResult != RunStatus.Running) break;
                            yield return result;
                        }
                        break;
                    case "coppeliastatestate":
                        foreach (RunStatus result in ProcessCoppeliaStateState(myNode))
                        {
                            myResult = result;
                            SetCurNodeIdStatus(myResult);
                            if (myResult != RunStatus.Running) break;
                            yield return result;
                        }
                        break;

                    case "coppeliamoral":
                        foreach (RunStatus result in ProcessCoppeliaMoral(myNode))
                        {
                            myResult = result;
                            SetCurNodeIdStatus(myResult);
                            if (myResult != RunStatus.Running) break;
                            yield return result;
                        }
                        break;
                    case "coppeliamoralambitionl":
                        foreach (RunStatus result in ProcessCoppeliaMoralAmbition(myNode))
                        {
                            myResult = result;
                            SetCurNodeIdStatus(myResult);
                            if (myResult != RunStatus.Running) break;
                            yield return result;
                        }
                        break;
                    case "coppeliaactionmoralbelief":
                        foreach (RunStatus result in ProcessCoppeliaActionMoralBelief(myNode))
                        {
                            myResult = result;
                            SetCurNodeIdStatus(myResult);
                            if (myResult != RunStatus.Running) break;
                            yield return result;
                        }
                        break;
                    case "gencoppeliafrommt":
                        foreach (RunStatus result in ProcessGenCoppeliaFromMt(myNode))
                        {
                            myResult = result;
                            SetCurNodeIdStatus(myResult);
                            if (myResult != RunStatus.Running) break;
                            yield return result;
                        }
                        break;



                        //DEFAULT
                    default:
                        // Ignore the Nops
                        SetCurNodeIdStatus(RunStatus.Success);
                        yield return RunStatus.Success;
                        yield break;
                        break;
                }
            }
            try
            {
            }
            catch (Exception e)
            {
                LogException(myNode, e, "evalBehaviorXml = {0}", myNode.OuterXml);

                contextBot.inCritical = origCritical;
                myResult = RunStatus.Failure;
                Console.WriteLine("### BNODE ERR:{0} {1} {2}", myNode.Name.ToLower(), nodeID, myResult);
                Console.WriteLine("### BNODE ERR:" + EMsg(e));
                Console.WriteLine("### BNODE ERR:" + e.StackTrace);
                Console.WriteLine("### BNODE ERR NODE XML:" + myNode.OuterXml);
                if (myNode.ParentNode != null)
                {
                    Console.WriteLine("### BNODE ERR PARENT XML:" + myNode.ParentNode.OuterXml);
                }
            }
            // update on exit
            //bot.inCritical = origCritical;
            SetCurNodeIdStatus(myResult);
            myLocalBehaviors.keepTime(nodeID, myResult);
            myLocalBehaviors.activationTime(nodeID, myResult);

            if ((myResult == RunStatus.Success) && (nodeID != "null"))
            {
                // Console.WriteLine("Result BNode {0} {1} {2}", myNode.Name.ToLower(), (myResult == RunStatus.Success), result);
            }
            if (myResult == RunStatus.Success) myLocalBehaviors.runEventHandler("onsuccess", contextBot);
            if (myResult == RunStatus.Failure) myLocalBehaviors.runEventHandler("onfail", contextBot);
            ProcessNodeDeleteEvents(myNode);
            logNode("END(" + myResult.ToString() + ") ", myNode);
            if (myResult != RunStatus.Success)
            {
                if (BehaviorSet.LogToConsole == null)
                {
                    Console.WriteLine("WARN NonSuccess " + nodeID + "END(" + myResult.ToString() + ") " +
                                      StaticXMLUtils.NodeString(myNode));
                }
            }
            contextBot.inCritical = origCritical;
            yield return myResult;
            yield break;
        }
        /// <summary>
        /// ProcessNodeAddEvents: saves previous "onX" hanlders and adds defines owns
        /// </summary>
        /// <param name="myNode"></param>
        public void ProcessNodeAddEvents(BTXmlNode myNode)
        {
            this.myLocalBehaviors.pushHandlers();
            foreach (XmlAttribute anode in myNode.Attributes)
            {
                string evnt = anode.Name.ToLower();
                string val = anode.Value;
                if (evnt.StartsWith("on"))
                {
                    this.myLocalBehaviors.addEventHandler(evnt, val);
                }
                if (evnt == "incritical")
                {
                    this.contextBot.inCritical = (val == "true");
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

        public void ProcessNodeDeleteEvents(BTXmlNode myNode)
        {
            foreach (XmlAttribute anode in myNode.Attributes)
            {
                string evnt = anode.Name.ToLower();
                string val = anode.Value;
                if (evnt.StartsWith("on"))
                {
                    this.myLocalBehaviors.deleteEventHandler(evnt, val);
                }
                if (evnt == "onmonitor")
                {
                    if (monitorStack.Count > 0)
                    {
                        monitorBehavior = monitorStack.Pop();
                    }
                }
            }
            this.myLocalBehaviors.popHandlers();
        }


        #region BTXMLTags

        public IEnumerable<RunStatus> ProcessPushBehavior(BTXmlNode myNode)
        {
            string behavior = myNode.InnerText;
            myLocalBehaviors.pushUniqueToStack(behavior);
            yield return RunStatus.Success;
        }
        public IEnumerable<RunStatus> ProcessPopBehavior(BTXmlNode myNode)
        {
            myLocalBehaviors.processOneEventStack(contextBot);
            yield return RunStatus.Success;
        }
        public IEnumerable<RunStatus> ProcessPopRandomBehavior(BTXmlNode myNode)
        {
            myLocalBehaviors.processRandomEventStack(contextBot);
            yield return RunStatus.Success;
        }

        public IEnumerable<RunStatus> ProcessBehavior(BTXmlNode myNode)
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
                if (!string.IsNullOrEmpty(myNode.AttributesV("restore")))
                {
                    restorable |= (myNode.AttributesV("restore").ToLower() == "true");
                }
                if (!string.IsNullOrEmpty(myNode.AttributesV("resumes")))
                {
                    restorable |= (myNode.AttributesV("resumes").ToLower() == "true");
                }
                if (!string.IsNullOrEmpty(myNode.AttributesV("processas")))
                {
                    processAs = myNode.AttributesV("processas").ToLower();
                }
                if (!string.IsNullOrEmpty(myNode.AttributesV("pace")))
                {
                    pace = 5000;
                    pace = Int32.Parse(myNode.AttributesV("pace"));
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
                if (!string.IsNullOrEmpty(myNode.AttributesV("id")))
                {
                    nodeID = myNode.AttributesV("id");
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
            restoring = (restP > 0) && (restP < myNode.ChildNodes.Count - 1);            

            // if restorable then check all asserts up to the restore point
            int childIndex = 0;
            for (childIndex = 0; childIndex < restP; childIndex++)
            {
                BTXmlNode childNode = (BTXmlNode) myNode.ChildNodes[childIndex];
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
                myLocalBehaviors.queueEvent("onrestore");
                continueflag = tickEventQueues(continueflag);
            }
            //foreach (BTXmlNode childNode in myNode.ChildNodes)
            for (childIndex = restP; childIndex < myNode.ChildNodes.Count; childIndex++)
            {
                BTXmlNode childNode = (BTXmlNode) myNode.ChildNodes[childIndex];
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
                    myLocalBehaviors.pushUniqueToStack(nodeID);
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
            myLocalBehaviors.removeFromStack(nodeID);
            Console.WriteLine("\n\n****** COMPLEX BEHAVIOR EXIT SUCCESS\n\n");
            yield return RunStatus.Success;
            yield break;
        }

        public IEnumerable<RunStatus> ProcessBreaker(BTXmlNode myNode)
        {

            RunStatus result = RunStatus.Success;
            try
            {
                myLocalBehaviors.queueEvent("break");
            }
            catch (Exception e)
            {
                Console.WriteLine("ERR: ProcessBraker");
                Console.WriteLine("ERR:" + EMsg(e));
                Console.WriteLine("ERR:" + e.StackTrace);
                Console.WriteLine("ERR XML:" + myNode.OuterXml);
            }
            yield return result;
        }

        public IEnumerable<RunStatus> ProcessRBehavior(BTXmlNode myNode)
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
                if (!string.IsNullOrEmpty(myNode.AttributesV("restore")))
                {
                    restorable |= (myNode.AttributesV("restore").ToLower() == "true");
                }
                if (!string.IsNullOrEmpty(myNode.AttributesV("resumes")))
                {
                    restorable |= (myNode.AttributesV("resumes").ToLower() == "true");
                }
                if (!string.IsNullOrEmpty(myNode.AttributesV("pace")))
                {
                    pace = 5000;
                    pace = Int32.Parse(myNode.AttributesV("pace"));
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
                if (!string.IsNullOrEmpty(myNode.AttributesV("id")))
                {
                    nodeID = myNode.AttributesV("id");
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
                BTXmlNode childNode = (BTXmlNode) myNode.ChildNodes[childIndex];
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
                myLocalBehaviors.queueEvent("onrestore");
                continueflag = tickEventQueues(continueflag);
            }
            else
            {
                int randomChild = _bot.myRandMem.selectOneXMLIndex(myNode);

                restorePoint[nodeID] = randomChild;
                restP = randomChild;
            }

            //foreach (BTXmlNode childNode in myNode.ChildNodes)
            //for (childIndex = restP; childIndex < myNode.ChildNodes.Count; childIndex++)
            childIndex = restP;
            {
                BTXmlNode childNode = (BTXmlNode) myNode.ChildNodes[childIndex];
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
            myLocalBehaviors.removeFromStack(nodeID);
            Console.WriteLine("\n\n****** COMPLEX RBEHAVIOR EXIT SUCCESS\n\n");
            yield return RunStatus.Success;
            yield break;
        }

        public bool tickEventQueues(bool continueflag)
        {
            var bot = this.contextBot.BotBehaving;
            while ((bot._outputQueue.Count > 0) || (myLocalBehaviors.eventQueue.Count > 0))
            {
                if (myLocalBehaviors.eventQueue.Count > 0)
                {
                    string peekstr = myLocalBehaviors.eventQueue.Peek();
                    if (peekstr == "abort")
                    {

                        continueflag = false;
                        myLocalBehaviors.queueEvent("onabort");
                        bot.flushOutputQueue();
                    }
                    if (peekstr == "break")
                    {

                        continueflag = false;
                        myLocalBehaviors.queueEvent("onbreak");
                        bot.flushOutputQueue();
                    }
                    myLocalBehaviors.processOneEventQueue(bot);
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
            if (contextBot.myPositiveSATModleString == null) return;
            IDictionary<string, string> eventTable = LockInfo.CopyOf(myLocalBehaviors.eventTable);
            if (eventTable.Count == 0) return;
            foreach (string evnt in eventTable.Keys)
            {
                if (contextBot.myPositiveSATModleString.Contains(evnt))
                {
                    myLocalBehaviors.queueEvent(evnt);
                }
            }
        }

        public RunStatus ProcessSubBehavior0(BTXmlNode myNode)
        {

            RunStatus result = RunStatus.Failure;
            string behaviorName = "root";
            try
            {
                behaviorName = myNode.AttributesV("id");
                result = myLocalBehaviors.runBotBehavior(behaviorName, contextBot);
            }
            catch (Exception e)
            {
                Console.WriteLine("ERR: ProcessSubBehavior");
                Console.WriteLine("ERR:" + EMsg(e));
                Console.WriteLine("ERR:" + e.StackTrace);
                Console.WriteLine("ERR XML:" + myNode.OuterXml);
            }
            return result;
        }
        public IEnumerable<RunStatus> ProcessSubBehavior(BTXmlNode myNode)
        {

            RunStatus result = RunStatus.Failure;
            string behaviorName = "root";
            behaviorName = myNode.AttributesV("id");
            //result = myLocalBehaviors.runBotBehavior(behaviorName, bot);
            if (myLocalBehaviors.definedBehavior(behaviorName))
            {
                logText("ProcessSubBehavior found:" + behaviorName);
                BehaviorTree curTree = myLocalBehaviors.GetTreeByName(behaviorName);

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
                Console.WriteLine("ERR:" + EMsg(e));
                Console.WriteLine("ERR:" + e.StackTrace);
                Console.WriteLine("ERR XML:" + myNode.OuterXml);
            }
            yield return result;
            yield break;
        }

        public IEnumerable<RunStatus> ProcessScheduler(BTXmlNode myNode)
        {
            // talk to the scheduler directly
            RunStatus result = RunStatus.Success;
            string behaviorName = "root";
            string action = "";
            string query = "";
            try
            {
                //behaviorName = myNode.AttributesV("id");
                if (!string.IsNullOrEmpty(myNode.AttributesV("action"))) action = myNode.AttributesV("action");
                if (!string.IsNullOrEmpty(myNode.AttributesV("query"))) query = myNode.AttributesV("query");
                if (!string.IsNullOrEmpty(myNode.AttributesV("a"))) action = myNode.AttributesV("a");
                if (!string.IsNullOrEmpty(myNode.AttributesV("q"))) query = myNode.AttributesV("q");
                if (!string.IsNullOrEmpty(myNode.AttributesV("id"))) behaviorName = myNode.AttributesV("id");
                StreamWriter sw = new StreamWriter("procschedtag.txt");
                contextBot.myServitor.myScheduler.performAction(sw, action, query, behaviorName, contextBot.BotBehaving);

            }
            catch (Exception e)
            {
                Console.WriteLine("ERR: ProcessSubBehavior");
                Console.WriteLine("ERR:" + EMsg(e));
                Console.WriteLine("ERR:" + e.StackTrace);
                Console.WriteLine("ERR XML:" + myNode.OuterXml);
            }
            yield return result;
            yield break;
        }

        public IEnumerable<RunStatus> ProcessEnqueue(BTXmlNode myNode)
        {

            RunStatus result = RunStatus.Success;
            string beventNames = "root";
            try
            {
                beventNames = myNode.InnerText;
                string[] behaviorList = beventNames.Split(' ');
                foreach (string behavior in behaviorList)
                {
                    myLocalBehaviors.queueEvent(behavior);
                }
                logText(String.Format("EVENTQUEUE: {0}", myLocalBehaviors.eventQueue.ToString()));
            }
            catch (Exception e)
            {
                Console.WriteLine("ERR: ProcessEnqueue");
                Console.WriteLine("ERR:" + EMsg(e));
                Console.WriteLine("ERR:" + e.StackTrace);
                Console.WriteLine("ERR XML:" + myNode.OuterXml);
            }
            yield return result;
            yield break;
        }
        public IEnumerable<RunStatus> ProcessFlushQueue(BTXmlNode myNode)
        {

            RunStatus result = RunStatus.Success;
            try
            {
                myLocalBehaviors.eventQueue.Clear();
                myLocalBehaviors.behaviorStack.Clear();
                logText(String.Format("EVENTQUEUE: {0}", myLocalBehaviors.eventQueue.ToString()));
                logText(String.Format("behaviorStack: {0}", myLocalBehaviors.behaviorStack.ToString()));

            }
            catch (Exception e)
            {
                Console.WriteLine("ERR: ProcessFlushQueue");
                Console.WriteLine("ERR:" + EMsg(e));
                Console.WriteLine("ERR:" + e.StackTrace);
                Console.WriteLine("ERR XML:" + myNode.OuterXml);
            }
            yield return result;
            yield break;
        }
        public IEnumerable<RunStatus> ProcessDrive(BTXmlNode myNode)
        {
            // <drive name="needFood" halflife="3600" threshold="0.2">
            // <motive name="motiveFear" halflife="3600" threshold="0.2">
            //  halflife in seconds
            //  once it returns success it will reset the timer and
            //   will not try until the estimated timer decay is below threshold
            // Motive = Threshold.Positive (trigger while above threshold)
            // Drive = Threshold.Negative (trigger when below threshold)

            RunStatus result = RunStatus.Failure;
            string driveName = "root";
            double halflife = 60000;
            double threshold = 0.1;
            double lastRun = int.MaxValue;
            double curV = 0;
            try
            {
                driveName = myNode.AttributesV("id");
                halflife = 1000 * double.Parse(myNode.AttributesV("halflife"));
                threshold = double.Parse(myNode.AttributesV("threshold"));

                myLocalBehaviors.VSoup.setHalflife(driveName, halflife);
                curV = myLocalBehaviors.VSoup.CurVal(driveName);

                //lastRun = myLocalBehaviors.lastRunning(driveName);
                //curV = Math.Pow( 0.5,(lastRun / halflife));

                //Console.WriteLine("Drive check '{0}' = {1} ={2}, hfl={3}, thrs = {4}",driveName,lastRun ,curV,halflife,threshold );
                // Drive does not need satisfying
            }
            catch (Exception e)
            {
                Console.WriteLine("*** DRIVE EXCEPTION : {0} {1}", EMsg(e), e.StackTrace);
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
                    //myLocalBehaviors.activationTime(driveName, result);
                    myLocalBehaviors.VSoup.adjust(driveName, 1.0);
                    Console.WriteLine("  *** Drive Procedure Success:{0} ***", driveName);
                }
                else
                {
                    Console.WriteLine("  *** Drive Procedure Failure:{0} ***", driveName);
                }

            }
            catch (Exception e)
            {
                Console.WriteLine("*** DRIVE EXCEPTION : {0} {1}", EMsg(e), e.StackTrace);
            }
            yield return result;
            yield break;
        }

        public IEnumerable<RunStatus> ProcessMotive(BTXmlNode myNode)
        {
            // <drive name="needFood" halflife="3600" threshold="0.2">
            // <motive name="motiveFear" halflife="3600" threshold="0.2">
            //  halflife in seconds
            //  once it returns success it will reset the timer and
            //   will not try until the estimated timer decay is below threshold
            // Motive = Threshold.Positive (trigger while above threshold)
            // Drive = Threshold.Negative (trigger when below threshold)

            RunStatus result = RunStatus.Failure;
            string driveName = "root";
            double halflife = 60000;
            double threshold = 0.1;
            double lastRun = int.MaxValue;
            double curV = 0;
            try
            {
                driveName = myNode.AttributesV("id");
                halflife = 1000 * double.Parse(myNode.AttributesV("halflife"));
                threshold = double.Parse(myNode.AttributesV("threshold"));

                myLocalBehaviors.VSoup.setHalflife(driveName, halflife);
                curV = myLocalBehaviors.VSoup.CurVal(driveName);

                //lastRun = myLocalBehaviors.lastRunning(driveName);
                //curV = Math.Pow( 0.5,(lastRun / halflife));

                Console.WriteLine("Motive check '{0}' = {1} ={2}, hfl={3}, thrs = {4}", driveName, lastRun, curV, halflife, threshold);
                // Drive does not need satisfying
            }
            catch (Exception e)
            {
                Console.WriteLine("*** Motive EXCEPTION : {0} {1}", EMsg(e), e.StackTrace);
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
                    //myLocalBehaviors.activationTime(driveName, result);
                    //myLocalBehaviors.VSoup.setRefVal(driveName, 0);
                    Console.WriteLine("  *** Motive Procedure Success:{0} ***", driveName);
                }
                else
                {
                    Console.WriteLine("  *** Motive Procedure Failure:{0} ***", driveName);
                }

            }
            catch (Exception e)
            {
                Console.WriteLine("*** Motive EXCEPTION : {0} {1}", EMsg(e), e.StackTrace);
            }
            yield return result;
            yield break;

        }

        public string kbToTagged(string query, string mtName, string tag, string innerText)
        {
            // A query like coppeliaAnger(AGENT,TARGET,VALUE) might return 
            // <coppeliaAnger agent="self" target="other" value="1" />
            string code = "";
            try
            {
                List<Dictionary<string, string>> bingingsList = new List<Dictionary<string, string>>();
                string[] part = query.Split('(');
                string head = part[0].Trim();
                lock (contextBot.myServitor.prologEngine)
                {
                    contextBot.prologEngine.askQuery(query, mtName, out bingingsList);
                }
                foreach (Dictionary<string, string> bindings in bingingsList)
                {
                    string frag = "<" + tag + ">";
                    string innerFrag = innerText;
                    foreach (string key in bindings.Keys)
                    {
                        //frag += String.Format(" {0}=\"{1}\"", key.ToLower(), bindings[key].Trim());
                        innerFrag = innerFrag.Replace(key.Trim(), bindings[key].Trim());
                    }
                    frag += "</" + tag + ">\n";
                    code += frag;
                }
            }
            catch (Exception e)
            {
                LogException(e, "kbToTagged '{0}','{1}','{2}','{3}':{4}", mtName, query, tag, innerText, EMsg(e));
            }
            return code;
        }
        public IEnumerable<RunStatus> ProcessGenChemsysFromMt(BTXmlNode myNode)
        {
            //Queries MT for chemsym related info to create a BTXML fragment
            // that is then interperted
            // <genChemSysFromMt query="drive(CHEM)" mt="defChemSimMt" tag="chemsys">normaldrive,CHEM</genChemSysFromMt>

            RunStatus rs = RunStatus.Failure;
            string mtName = "defChemSimMt";
            string tag = "chemsyem";
            string query = "chemsys(CODE)";
            string innerStr = myNode.InnerXml;

            try
            {
                if (!string.IsNullOrEmpty(myNode.AttributesV("mt"))) mtName = myNode.AttributesV("mt");
                if (!string.IsNullOrEmpty(myNode.AttributesV("tag"))) tag = myNode.AttributesV("tag");
                if (!string.IsNullOrEmpty(myNode.AttributesV("query"))) query = myNode.AttributesV("query");
                string xcode = "";
                xcode += kbToTagged(query, mtName, tag, innerStr);
                string btxmlCode = "";
                btxmlCode += "<aiml version=\"1.0\">\n";
                btxmlCode += " <state name=\"*\">\n";
                btxmlCode += "  <btxml>\n";
                btxmlCode += "   <task>\n";
                btxmlCode += xcode;
                btxmlCode += "   </task>\n";
                btxmlCode += "  </btxml>\n";
                btxmlCode += " </state>\n";
                btxmlCode += "</aiml>\n";
                BTXmlDocument chemsymDoc = new BTXmlDocument();
                chemsymDoc.LoadXml(btxmlCode);
                Console.WriteLine("-------------------------------------");
                Console.WriteLine("------ GenChemsysFromMt:{0} --------", mtName);
                Console.WriteLine(btxmlCode);
                Console.WriteLine("-------------------------------------");
                contextBot.loadAIMLFromXML(chemsymDoc, "mt:" + mtName + DateTime.Now.ToString());

                rs = RunStatus.Success;
            }
            catch (Exception e)
            {
                LogException(myNode, e, "ProcessGenChemsysFromMt '{0}',{1}", mtName, EMsg(e));
                rs = RunStatus.Failure;
            }
            yield return rs;
            yield break;
        }

        public IEnumerable<RunStatus> ProcessAssert(BTXmlNode myNode)
        {
            string condData = myNode.AttributesV("cond") == null ? null : myNode.AttributesV("cond");
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
            catch (FormatException e)
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
                sv = contextBot.BotBehaving.getBBHash(varName) ?? "0.0";
                if (!string.IsNullOrEmpty(sv)) bbVal = double.Parse(sv);
            }
            catch (FormatException) { }

            // Special variables?
            if (varName == "timeout") bbVal = elapsedTime;
            if (varName == "behaviorstackcount") bbVal = myLocalBehaviors.behaviorStack.Count;
            if (varName == "behaviorqueuecount") bbVal = myLocalBehaviors.eventQueue.Count;
            if (varName == "prob") bbVal = rgen.NextDouble();
            if (varName.Contains(".runtime"))
            {
                string tName = varName.Replace(".runtime", "");
                bbVal = myLocalBehaviors.timeRunning(tName);
            }
            if (varName.Contains(".lastrun"))
            {
                string tName = varName.Replace(".lastrun", "");
                bbVal = myLocalBehaviors.lastRunning(tName);
            }
            if (varName.Contains(".drive"))
            {
                string dName = varName.Replace(".drive", "");
                double halflife = 1000 * double.Parse(myNode.AttributesV("halflife"));
                int lastRun = myLocalBehaviors.lastRunning(dName);
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
                    valid = contextBot.myActiveModel.IsTrue(sen);
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

        public IEnumerable<RunStatus> ProcessAssertTimer(BTXmlNode myNode)
        {
            //if it doesn't exist then create an entry and return Success
            string nodeID = myNode.AttributesV("id");
            if (!myLocalBehaviors.entryTime.ContainsKey(nodeID))
            {
                myLocalBehaviors.keepTime(nodeID, RunStatus.Success);
                myLocalBehaviors.activationTime(nodeID, RunStatus.Success);
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
                    myLocalBehaviors.entryTime.Remove(nodeID);
                    myLocalBehaviors.execTime.Remove(nodeID);
                }
                catch(Exception e)
                {
                    LogException(myNode, e, "ProcessAssertTImner");
                }
                //Reset
                myLocalBehaviors.keepTime(nodeID, RunStatus.Success);
                myLocalBehaviors.activationTime(nodeID, RunStatus.Success);

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
        public IEnumerable<RunStatus> ProcessAssertGuest(BTXmlNode myNode)
        {
            string condition = myNode.AttributesV("cond");
            string parameters = myNode.InnerText;
            //if it doesn't exist then return failure
            if (contextBot.guestEvalObject == null)
            {
                yield return RunStatus.Failure;
                yield break;
            }
            RunStatus r = RunStatus.Failure;
            try
            {
                MethodInfo info = contextBot.guestEvalObject.GetType().GetMethod(condition);
                if (info != null)
                {
                    bool result = (bool)info.Invoke(contextBot.guestEvalObject, new object[] { parameters });
                    if (result) r = RunStatus.Success;
                }
            }
            catch (Exception e)
            {
                LogException(myNode, e, "ProcessAssertGuest '{0}':{1}", condition, EMsg(e));
                r = RunStatus.Failure;
            }

            yield return r;
            yield break;
        }

        public IEnumerable<RunStatus> ProcessTaskGuest(BTXmlNode myNode)
        {
            string condition = myNode.AttributesV("call");
            string parameters = myNode.InnerText;
            //if it doesn't exist then return  failure(success would also make sense)
            var guestEvalObject = _bot.guestEvalObject;
            if (guestEvalObject == null)
            {
                yield return RunStatus.Failure;
                yield break;
            }
            RunStatus r = RunStatus.Failure;
            try
            {
                MethodInfo info = guestEvalObject.GetType().GetMethod(condition);
                if (info != null)
                {
                    bool result = (bool)info.Invoke(guestEvalObject, new object[] { parameters });
                    if (result) r = RunStatus.Success;
                }
            }
            catch (Exception e)
            {
                LogException(myNode, e, "ProcessAssertGuest '{0}':{1}", condition, EMsg(e));
                r = RunStatus.Failure;
            }

            yield return r;
            yield break;
        }
        //WaitForChatInput
        public IEnumerable<RunStatus> ProcessWaitForChatInput(BTXmlNode myNode)
        {
            string sentStr = myNode.InnerXml;
             RunStatus rs = RunStatus.Failure;
             string wait = "10000";
             try
             {
                 if (!string.IsNullOrEmpty(myNode.AttributesV("wait")))
                 {
                     wait = myNode.AttributesV("wait");
                 }
             }
             catch (Exception e)
             {
                 wait = "10000";
             }
             int waitV = Int32.Parse(wait);
             int starttime = Environment.TickCount;
             int triggertime = starttime +waitV;
             myLocalBehaviors.waitingForChat = true;
             while (Environment.TickCount < triggertime)
             {
                 if (contextBot.BotBehaving.chatInputQueue.Count > 0)
                 {
                     rs = RunStatus.Success;
                     myLocalBehaviors.waitingForChat = false;

                     yield return rs;
                     yield break;
                     
                 }
                 yield return RunStatus.Running;

             }
             myLocalBehaviors.waitingForChat = false;
             yield return rs;
             yield break;
       }

        //CHAT: chat controlled by the behavior system (currently calls AIML)
        public IEnumerable<RunStatus> ProcessChat(BTXmlNode myNode)
        {
            string sentStr;
            string graphName = "*";
            try
            {
                if (!string.IsNullOrEmpty(myNode.AttributesV("graph")))
                {
                    graphName = myNode.AttributesV("graph");
                }
            }
            catch (Exception e)
            {
                graphName = "*";
            }

            BehaviorContext behaviorContext = contextBot.BotBehaving;
            User user;

            // codes goal:  If there is no input Queued to return RunStatus.Success;
            /// if there was input queude append it to the inner xml
            /// (this is what it seemed to be doing before?)
            lock (behaviorContext.ChatQueueLock)
            {
                if (behaviorContext.chatInputQueue.Count == 0)
                {
                    yield return RunStatus.Success;
                    yield break;
                }
                behaviorContext.ClearLastInputOutput(true);
                behaviorContext.lastBehaviorChatInput = behaviorContext.chatInputQueue.Peek();
                sentStr = myNode.InnerXml + " " + behaviorContext.lastBehaviorChatInput;
                user = behaviorContext.lastBehaviorUser;
            }

            // ChatQueueLock is now unlocked.. this willl let other threads call postOutput 
            //    and potentually use the input since we havent removed it yet

// ReSharper disable RedundantAssignment
            RunStatus rs = RunStatus.Failure;
// ReSharper restore RedundantAssignment
            Request r = null;
            try
            {
                r = new Request(sentStr, user, user.That, contextBot, true, RequestKind.BehaviourChat);
                Result res = contextBot.Chat(r, graphName);
                bool hasOuput = res.isValidOutput;
                lock (behaviorContext.ChatQueueLock)
                {
                    // DMILES: currently we are not calling postOutput here.. 
                    // DMILES:  because proccessChat isnt meant to assume we want to "say it"?
                    if (hasOuput)
                    {
                        // bot.postOutput(res.Output.AsString());
                        behaviorContext.lastBehaviorChatOutput = res.Output.AsString();
                        rs = RunStatus.Success;
                        // eat input on success
                        behaviorContext.chatInputQueue.Dequeue();
                    }
                    else
                    {
                        behaviorContext.lastBehaviorChatOutput = "";
                        rs = RunStatus.Failure;
                    }
                }
            }
            catch (Exception e)
            {
                LogException(myNode, e, "ProcessChat '{0}' '{1}':{2}", sentStr, r, EMsg(e));
                rs = RunStatus.Failure;
            }
            yield return rs;
            yield break;
        }

        // Propositional Reasoner interface
        // Assert to KB
        public IEnumerable<RunStatus> ProcessTellKB(BTXmlNode myNode)
        {
            RunStatus rs = RunStatus.Failure;
            string sentStr = myNode.InnerXml;
            try
            {
                sentStr = sentStr.Replace("implies", "=>");
                sentStr = sentStr.Replace(" imp ", "=> ");
                sentStr = sentStr.Replace("equiv", "<=>");

                contextBot.myKB.Tell(sentStr);
                rs = RunStatus.Success;
            }
            catch (Exception e)
            {
                LogException(myNode, e, "ProcessTellKB '{0}':{1}", sentStr, EMsg(e));
                rs = RunStatus.Failure;
            }
            yield return rs;
            yield break;
        }

        private string EMsg(Exception exception)
        {
            return ""
                   + exception;
        }


        public IEnumerable<RunStatus> ProcessTellBaseKB(BTXmlNode myNode)
        {
            RunStatus rs = RunStatus.Failure;
            string sentStr = myNode.InnerXml;
            try
            {
                sentStr = sentStr.Replace("implies", "=>");
                sentStr = sentStr.Replace(" imp ", "=> ");
                sentStr = sentStr.Replace("equiv", "<=>");

                contextBot.myBaseKB.Tell(sentStr);
                rs = RunStatus.Success;
            }
            catch (Exception e)
            {
                LogException(myNode, e, "ProcessTellBaseKB '{0}':{1}", sentStr, EMsg(e));
                rs = RunStatus.Failure;
            }
            yield return rs;
            yield break;
        }
        public IEnumerable<RunStatus> ProcessTellKBOCC(BTXmlNode myNode)
        {
            RunStatus rs = RunStatus.Failure;
            try
            {
                addOCCLogicForInteraction(contextBot.myBaseKB, myNode.InnerXml);
                rs = RunStatus.Success;
            }
            catch (Exception e)
            {
                LogException(myNode, e, "ProcessTellKBOCC '{0}':{1}", myNode.InnerXml, EMsg(e));
                rs = RunStatus.Failure;
            }
            yield return rs;
            yield break;
        }
        // 
        public IEnumerable<RunStatus> ProcessClearKB(BTXmlNode myNode)
        {
            RunStatus rs = RunStatus.Failure;
            try
            {
                contextBot.myKB = new KnowledgeBase();
                rs = RunStatus.Success;
            }
            catch (Exception e)
            {
                LogException(myNode, e, "ProcessClearKB '{0}':{1}", myNode.InnerXml, EMsg(e));
                rs = RunStatus.Failure;
            }
            yield return rs;
            yield break;
        }
        public IEnumerable<RunStatus> ProcessClearBaseKB(BTXmlNode myNode)
        {
            RunStatus rs = RunStatus.Failure;
            try
            {
                contextBot.myBaseKB = new KnowledgeBase();
                rs = RunStatus.Success;
            }
            catch (Exception e)
            {
                LogException(myNode, e, "ProcessClearBaseKB '{0}':{1}", myNode.InnerXml, EMsg(e));
                rs = RunStatus.Failure;
            }
            yield return rs;
            yield break;
        }

        // Process KB
        public double trialsMemory = 2048;
        public static object FileLock = new object();

        public IEnumerable<RunStatus> ProcessKBModel(BTXmlNode myNode)
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
                foreach (Sentence s in contextBot.myBaseKB.Sentences)
                {
                    totalKB.Tell(s.ToString());
                }
                foreach (Sentence s in contextBot.myKB.Sentences)
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
                    _bot.myModel = MiniModel;
                    _bot.myActiveModel = MiniModel;
                    miniPostives = MiniModel.strPositives();
                    myReport = answer;
                    contextBot.BotBehaving.setBBHash("foundSATModel", "True");
                }
                else
                {
                    contextBot.BotBehaving.setBBHash("foundSATModel", "False");
                }

                //
                if (trialsMemory < 512) trialsMemory = 512;
                if (trialsMemory > 10240) trialsMemory = 10240;

                walkModel = _bot.myWalkSAT.FindModelFor(totalKBs, (int)(trialsMemory * 1.5), 0.5, _bot.myActiveModel);
                // bot.myModel = bot.myWalkSAT.FindModelFor(totalKBs, (int)(trialsMemory * 1.5), 0.5, MiniModel);
                trialsMemory = Ema(_bot.myWalkSAT.trials, trialsMemory, 5);

                myReport += _bot.myWalkSAT.ExamineClauseStatistics(0.05);
            }
            catch (Exception e)
            {
                LogException(myNode, e, "ProcessKBModel '{0}':{1}\n{2}", myNode.InnerXml, EMsg(e), e.StackTrace);
                rs = RunStatus.Failure;
            }


            if ((walkModel == null) && (myMiniSolver.wasSAT == false))
            {
                // We fail if we are unsat
                Console.WriteLine("NO SAT MODEL FOUND");
                if (contextBot.myActiveModel == null)
                {
                    contextBot.BotBehaving.setBBHash("activeModel", "(NOT sat)");
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
                    contextBot.myModel = walkModel;
                    walkPostives = walkModel.strPositives();
                }

                // update active model
                contextBot.myActiveModel = contextBot.myModel;
                string totalModel = contextBot.myModel.AsSentenceString();
                string postPositives = contextBot.myModel.strPositives();
                contextBot.myPositiveSATModleString = postPositives;
                //bot.bbSetHash("activeModel", totalModel);
                contextBot.BotBehaving.setBBHash("activeModel", postPositives);
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
                LogException(myNode, e, "ProcessKBModel '{0}':{1}\n{2}", myNode.InnerXml, EMsg(e), e.StackTrace);
                rs = RunStatus.Failure;
            }
            yield return rs;
            yield break;

        }

        public IEnumerable<RunStatus> ProcessSequence(BTXmlNode myNode)
        {
            // Execute children in order until one returns false
            foreach (BTXmlNode childNode in myNode.ChildNodes)
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

        public IEnumerable<RunStatus> ProcessSelector(BTXmlNode myNode)
        {
            // Execute children until one returns true
            foreach (BTXmlNode childNode in myNode.ChildNodes)
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

        public IEnumerable<RunStatus> ProcessRandomSelector(BTXmlNode myNode)
        {
            // Pick a random child with equal weighting
            //int selected = rgen.Next(myNode.ChildNodes.Count);
            //BTXmlNode childNode = myNode.ChildNodes[selected];
            BTXmlNode childNode = (BTXmlNode)_bot.myRandMem.selectOneXML(myNode);
            foreach (RunStatus childResult in processNode(childNode))
            {
                yield return childResult;
            }
        }

        public IEnumerable<RunStatus> ProcessWeightedSelector(BTXmlNode myNode)
        {
            double totalWeight = 0;
            // Does a basic roulette wheel selection of the child nodes
            //  each of which can have a "weight" attribute
            //  otherwise they are evenly weighted

            // Another variant would could use the node ID to look up the weight
            // so the results could be dynamic
            foreach (BTXmlNode childNode in myNode.ChildNodes)
            {
                try
                {
                    totalWeight += double.Parse(childNode.AttributesV("weight"));
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
            foreach (BTXmlNode childNode in myNode.ChildNodes)
            {
                try
                {
                    accum += double.Parse(childNode.AttributesV("weight"));
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

        public IEnumerable<RunStatus> ProcessParallel(BTXmlNode myNode)
        {
            // Execute All Children
            foreach (BTXmlNode childNode in myNode.ChildNodes)
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

        public IEnumerable<RunStatus> ProcessLoop(BTXmlNode myNode)
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
                maxloop = Int32.Parse(myNode.AttributesV("maxloop"));
            }
            catch (FormatException)
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
                foreach (BTXmlNode childNode in myNode.ChildNodes)
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

        public IEnumerable<RunStatus> ProcessLoopUntil(BTXmlNode myNode)
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
                maxloop = Int32.Parse(myNode.AttributesV("maxloop"));
            }
            catch(FormatException)
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
                foreach (BTXmlNode childNode in myNode.ChildNodes)
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
                            finalResult = RunStatus.Failure;
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
        public IEnumerable<RunStatus> ProcessTask(BTXmlNode myNode)
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
                    string returnV = myNode.AttributesV("return");
                    if (returnV.ToLower() == "failure") { result = RunStatus.Failure; }
                    if (returnV.ToLower() == "success") { result = RunStatus.Success; }
                }
            }
            catch (Exception e)
            {
                result = RunStatus.Success;

            }
            // yield return RunStatus.Running;
            try
            {
                contextBot.evalTemplateNodeInnerXml(myNode, RequestKind.BehaviourProcess);
                //bot.evalTemplateNode(templateNode);
            }
            catch (Exception e)
            {
                LogException(myNode, e, "ProcessTask");
                //result = RunStatus.Failure;
            }
            yield return result;
            yield break;

            // Works like OnEntry/OnExit in SCXML
            foreach (BTXmlNode templateNode in myNode.ChildNodes)
            {
                try
                {
                    contextBot.evalTemplateNodeInnerXml(templateNode, RequestKind.BehaviourProcess);
                    //bot.evalTemplateNode(templateNode);
                }
                catch (Exception e)
                {
                    Console.WriteLine("ERR: ProcessTask");
                    Console.WriteLine("ERR:" + EMsg(e));
                    Console.WriteLine("ERR:" + e.StackTrace);
                    Console.WriteLine("ERR XML:" + templateNode.OuterXml);
                    //result = RunStatus.Failure;
                }
            }

            yield return result;
            yield break;
        }

        public IEnumerable<RunStatus> ProcessSetAimlVar(BTXmlNode myNode)
        {
            // Takes the inner text as AIML category definitions, to create new responses
            // So the system can change its verbal response behavior based on
            // its behavior tree
            RunStatus result = RunStatus.Success;

            try
            {
                if (!string.IsNullOrEmpty(myNode.AttributesV("return")))
                {

                    string returnV = myNode.AttributesV("return");
                    if (returnV.ToLower() == "failure") { result = RunStatus.Failure; }
                    if (returnV.ToLower() == "success") { result = RunStatus.Success; }
                }
            }
            catch
            {
                result = RunStatus.Success;

            }
            string varname = "nulvar";
            string settingValue = myNode.InnerText;
            User user = contextBot.BotBehaving.lastBehaviorUser;

            try
            {
                if (!string.IsNullOrEmpty(myNode.AttributesV("var")))
                {
                    varname = myNode.AttributesV("var");
                }
            }
            catch (Exception e)
            {
                varname = "nulvar";
            }
            if (!string.IsNullOrEmpty(myNode.AttributesV("value")))
            {
                settingValue = myNode.AttributesV("value");
            }

            try
            {
                ISettingsDictionary dict;
                if (contextBot.BotBehaving.lastBehaviorUser != null) dict = user;
                else dict = contextBot.GetDictionary("user");

                dict.addSetting(varname, settingValue);
            }
            catch (Exception e)
            {
                LogException(myNode, e, "ProcessSetAimlVar = {0}", myNode.OuterXml);
                //result = RunStatus.Failure;
            }
            yield return result;
            yield break;
        }

        public IEnumerable<RunStatus> ProcessStateAiml(BTXmlNode myNode)
        {
            // Takes the inner text as AIML category definitions, to create new responses
            // So the system can change its verbal response behavior based on
            // its behavior tree
            RunStatus result = RunStatus.Success;

            try
            {
                if (!string.IsNullOrEmpty(myNode.AttributesV("return")))
                {

                    string returnV = myNode.AttributesV("return");
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
                if (!string.IsNullOrEmpty(myNode.AttributesV("graph")))
                {
                    graphName = myNode.AttributesV("graph");
                }
            }
            catch (Exception e)
            {
                graphName = "*";
            }
            try
            {

                //BTXmlNode resultTemplateNode = AIMLTagHandler.getNode("<template>" + myNode.InnerXml + "</template>");
                BTXmlDocument resultAIMLDoc = new BTXmlDocument();
                resultAIMLDoc.LoadXml("<aiml graph='" + graphName + "'>" + myNode.InnerXml + "</aiml>");
                contextBot.loadAIMLFromXML(resultAIMLDoc, "dynamic_code_from_btx");
                //bot.evalTemplateNode(templateNode);
            }
            catch (Exception e)
            {
                LogException(myNode, e, "ProcessStateAiml = {0}", myNode.OuterXml);
                //result = RunStatus.Failure;
            }
            yield return result;
            yield break;

            // Works like OnEntry/OnExit in SCXML
            foreach (BTXmlNode templateNode in myNode.ChildNodes)
            {
                try
                {
                    contextBot.evalTemplateNodeInnerXml(templateNode, RequestKind.BehaviourProcess);
                    //bot.evalTemplateNode(templateNode);
                }
                catch (Exception e)
                {
                    Console.WriteLine("ERR: ProcessTask");
                    Console.WriteLine("ERR:" + EMsg(e));
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
        public IEnumerable<RunStatus> ProcessClearPrologMt(BTXmlNode myNode)
        {
            // Clear a specified KB
            RunStatus rs = RunStatus.Failure;
            string mtName = "root";
            string innerStr = myNode.InnerXml.Trim();

            try
            {
                if (!string.IsNullOrEmpty(myNode.AttributesV("mt"))) mtName = myNode.AttributesV("mt");

                lock (contextBot.myServitor.prologEngine)
                {
                    contextBot.myServitor.prologEngine.insertKB("", mtName);
                }
                rs = RunStatus.Success;
            }
            catch (Exception e)
            {
                LogException(myNode, e, "ProcessClearProKB '{0}':{1}", mtName, EMsg(e));
                rs = RunStatus.Failure;
            }
            yield return rs;
            yield break;
        }

        public IEnumerable<RunStatus> ProcessConnectMt(BTXmlNode myNode)
        {
            //Connect two MT's
            RunStatus rs = RunStatus.Failure;
            string childMtName = "root";
            string parentMtName = "root";
            string innerStr = myNode.InnerXml.Trim();

            try
            {
                if (!string.IsNullOrEmpty(myNode.AttributesV("child"))) childMtName = myNode.AttributesV("child");
                if (!string.IsNullOrEmpty(myNode.AttributesV("parent"))) parentMtName = myNode.AttributesV("parent");

                lock (contextBot.myServitor.prologEngine)
                {
                    contextBot.myServitor.prologEngine.connectMT(childMtName, parentMtName);
                }
                rs = RunStatus.Success;
            }
            catch (Exception e)
            {
                LogException(myNode, e, "ProcessConnectMt '{0}', '{1}':{2}", childMtName, parentMtName, EMsg(e));
                rs = RunStatus.Failure;
            }
            yield return rs;
            yield break;
        }

        public IEnumerable<RunStatus> ProcessTellPrologMt(BTXmlNode myNode)
        {
            // Assert with overwrite some si_text
            RunStatus rs = RunStatus.Failure;
            string mtName = "root";
            string innerStr = myNode.InnerXml.Trim();

            try
            {
                if (!string.IsNullOrEmpty(myNode.AttributesV("mt"))) mtName = myNode.AttributesV("mt");

                lock (contextBot.myServitor.prologEngine)
                {
                    contextBot.myServitor.prologEngine.insertKB(innerStr, mtName);
                }
                rs = RunStatus.Success;
            }
            catch (Exception e)
            {
                LogException(myNode, e, "ProcessTellProKB '{0}':{1}", mtName, EMsg(e));
                rs = RunStatus.Failure;
            }
            yield return rs;
            yield break;
        }
        public IEnumerable<RunStatus> ProcessLoadKEKB(BTXmlNode myNode)
        {
            // load some KE (which will have MT definitions)
            RunStatus rs = RunStatus.Failure;
            string path = "default.ke";
            string innerStr = myNode.InnerXml.Trim();

            try
            {
                if (!string.IsNullOrEmpty(myNode.AttributesV("path"))) path = myNode.AttributesV("path");

                lock (contextBot.myServitor.prologEngine)
                {
                    contextBot.myServitor.prologEngine.loadKEFile(path);
                }
                rs = RunStatus.Success;
            }
            catch (Exception e)
            {
                LogException(myNode, e, "ProcessLoadKEKB '{0}':{1}", path, EMsg(e));
                rs = RunStatus.Failure;
            }
            yield return rs;
            yield break;
        }

        public IEnumerable<RunStatus> ProcessInduceFromMt(BTXmlNode myNode)
        {
            // load some KE (which will have MT definitions)
            RunStatus rs = RunStatus.Failure;
            string sourceMt = "sourceMt";
            string resultMt = "resultMt";
            string innerStr = myNode.InnerXml.Trim();

            try
            {
                if (!string.IsNullOrEmpty(myNode.AttributesV("source"))) sourceMt = myNode.AttributesV("source");
                if (!string.IsNullOrEmpty(myNode.AttributesV("result"))) resultMt = myNode.AttributesV("result");
                DecisionTreeImplementation DTI = new DecisionTreeImplementation();
                lock (contextBot.myServitor.prologEngine)
                {
                    DTI.GenRulesFromMt(contextBot.myServitor.prologEngine, sourceMt, resultMt);
                }
                rs = RunStatus.Success;
            }
            catch (Exception e)
            {
                LogException(myNode, e, "ProcessInduceFromMt '{0}':'{1}':{2}", sourceMt, resultMt, EMsg(e));
                rs = RunStatus.Failure;
            }
            yield return rs;
            yield break;
        }

        public IEnumerable<RunStatus> ProcessInventFromMt(BTXmlNode myNode)
        {
            // load some KE (which will have MT definitions)
            RunStatus rs = RunStatus.Failure;
            string problemMt = "problemMt";
            string moduleMt = "moduleMt";
            string solutionMt = "solutionMt";
            string innerStr = myNode.InnerXml.Trim();
            string behaviorID = "";
            string behaviorTag = "";

            try
            {
                if (!string.IsNullOrEmpty(myNode.AttributesV("problem"))) problemMt = myNode.AttributesV("problem");
                if (!string.IsNullOrEmpty(myNode.AttributesV("modules"))) moduleMt = myNode.AttributesV("modules");
                if (!string.IsNullOrEmpty(myNode.AttributesV("solution"))) solutionMt = myNode.AttributesV("solution");
                if (!string.IsNullOrEmpty(myNode.AttributesV("behavior"))) behaviorID = myNode.AttributesV("behavior");
                if (!string.IsNullOrEmpty(myNode.AttributesV("behaviortag"))) behaviorTag = myNode.AttributesV("behaviortag");
                lock (contextBot.myServitor.prologEngine)
                {
                    CemaSolver Inventor = new CemaSolver(contextBot.myServitor.prologEngine);

                    if (!string.IsNullOrEmpty(myNode.AttributesV("admissible")))
                    {
                        if (myNode.AttributesV("admissible").ToLower().Contains("t"))
                        {
                            Inventor.worstWeighting = true;
                        }
                    }
                    if (!string.IsNullOrEmpty(myNode.AttributesV("nondeterministic")))
                    {
                        if (myNode.AttributesV("nondeterministic").ToLower().Contains("t"))
                        {
                            Inventor.nondeterministic = true;
                        }
                        else
                        {
                            Inventor.nondeterministic = false;
                        }
                    }

                    if (!string.IsNullOrEmpty(myNode.AttributesV("trials")))
                    {
                        Inventor.limitTrials = int.Parse(myNode.AttributesV("trials"));
                    }
                    if (!string.IsNullOrEmpty(myNode.AttributesV("budget")))
                    {
                        Inventor.limitCost = double.Parse(myNode.AttributesV("budget"));
                    }

                    bool outcome = Inventor.constructSolution(problemMt, moduleMt, solutionMt);
                    if (outcome)
                        rs = RunStatus.Success;
                    else
                        rs = RunStatus.Failure;

                    if ((outcome) && (behaviorID.Length > 0))
                    {
                        string bcode = Inventor.getBTXMLBehaviorCode(behaviorID, behaviorTag);
                        defineBehavior(behaviorID, bcode);
                    }
                }
            }
            catch (Exception e)
            {
                LogException(myNode, e, "ProcessInventFromMt '{0}':'{1}':'{2}':{3}", problemMt, moduleMt, solutionMt, EMsg(e));
                rs = RunStatus.Failure;
            }
            yield return rs;
            yield break;
        }
        public IEnumerable<RunStatus> ProcessPlanFromMt(BTXmlNode myNode)
        {
            // load some KE (which will have MT definitions)
            RunStatus rs = RunStatus.Failure;
            string goalMt = "goalMt";
            string moduleMt = "moduleMt";
            string solutionMt = "solutionMt";
            string nowMt = "nowMt";
            string backgroundMt = "backgroundMt";
            string behaviorID = "";
            string behaviorTag = "";

            string innerStr = myNode.InnerXml.Trim();

            try
            {
                if (!string.IsNullOrEmpty(myNode.AttributesV("goal"))) goalMt = myNode.AttributesV("goal");
                if (!string.IsNullOrEmpty(myNode.AttributesV("now"))) nowMt = myNode.AttributesV("now");
                if (!string.IsNullOrEmpty(myNode.AttributesV("background"))) backgroundMt = myNode.AttributesV("background");
                if (!string.IsNullOrEmpty(myNode.AttributesV("modules"))) moduleMt = myNode.AttributesV("modules");
                if (!string.IsNullOrEmpty(myNode.AttributesV("solution"))) solutionMt = myNode.AttributesV("solution");
                if (!string.IsNullOrEmpty(myNode.AttributesV("behavior"))) behaviorID = myNode.AttributesV("behavior");
                if (!string.IsNullOrEmpty(myNode.AttributesV("behaviortag"))) behaviorTag = myNode.AttributesV("behaviortag");
                lock (contextBot.myServitor.prologEngine)
                {
                    GOAPSolver Planner = new GOAPSolver(contextBot.myServitor.prologEngine);

                    if (!string.IsNullOrEmpty(myNode.AttributesV("admissible")))
                    {
                        if (myNode.AttributesV("admissible").ToLower().Contains("t"))
                        {
                            Planner.worstWeighting = true;
                        }

                        if (!string.IsNullOrEmpty(myNode.AttributesV("nondeterministic")))
                        {
                            if (myNode.AttributesV("nondeterministic").ToLower().Contains("t"))
                            {
                                Planner.nondeterministic = true;
                            }
                            else
                            {
                                Planner.nondeterministic = false;
                            }
                        }

                        if (!string.IsNullOrEmpty(myNode.AttributesV("trials")))
                        {
                            Planner.limitTrials = int.Parse(myNode.AttributesV("trials"));
                        }
                        if (!string.IsNullOrEmpty(myNode.AttributesV("budget")))
                        {
                            Planner.limitCost = double.Parse(myNode.AttributesV("budget"));
                        }

                        bool outcome = Planner.constructPlan(goalMt, nowMt, moduleMt, backgroundMt, solutionMt);
                        if (outcome)
                            rs = RunStatus.Success;
                        else
                            rs = RunStatus.Failure;

                        if ((outcome) && (behaviorID.Length > 0))
                        {
                            string bcode = Planner.getBTXMLBehaviorCode(behaviorID, behaviorTag);
                            defineBehavior(behaviorID, bcode);
                        }
                    }
                }
            }
            catch (Exception e)
            {
                LogException(myNode, e, "ProcessPlanFromMt '{0}':'{1}':'{2}':'{3}':'{4}':{5}", goalMt, nowMt, moduleMt, backgroundMt, solutionMt, EMsg(e));
                rs = RunStatus.Failure;
            }
            yield return rs;
            yield break;
        }
        public IEnumerable<RunStatus> ProcessInsertMt(BTXmlNode myNode)
        {
            // insert some si_text (overwrite)
            RunStatus rs = RunStatus.Failure;
            string mtName = "root";
            string innerStr = myNode.InnerXml.Trim();

            try
            {
                if (!string.IsNullOrEmpty(myNode.AttributesV("mt"))) mtName = myNode.AttributesV("mt");

                lock (contextBot.myServitor.prologEngine)
                {
                    contextBot.myServitor.prologEngine.insertKB(innerStr, mtName);
                }
                rs = RunStatus.Success;
            }
            catch (Exception e)
            {
                LogException(myNode, e, "ProcessInsertKB '{0}':{1}", mtName, EMsg(e));
                rs = RunStatus.Failure;
            }
            yield return rs;
            yield break;
        }

        public IEnumerable<RunStatus> ProcessAppendMt(BTXmlNode myNode)
        {
            // Append some si_text 
            RunStatus rs = RunStatus.Failure;
            string mtName = "root";
            string innerStr = myNode.InnerXml.Trim();

            try
            {
                if (!string.IsNullOrEmpty(myNode.AttributesV("mt"))) mtName = myNode.AttributesV("mt");

                lock (contextBot.myServitor.prologEngine)
                {
                    contextBot.myServitor.prologEngine.appendKB(innerStr, mtName);
                }
                rs = RunStatus.Success;
            }
            catch (Exception e)
            {
                LogException(myNode, e, "ProcessAppendKB '{0}':{1}", mtName, EMsg(e));
                rs = RunStatus.Failure;
            }
            yield return rs;
            yield break;
        }


        public IEnumerable<RunStatus> ProcessAssertProlog(BTXmlNode myNode)
        {
            // Conduct a test if the si_query is true
            string condition = myNode.AttributesV("cond");
            string mtName = "root";
            string innerStr = myNode.InnerXml.Trim();
            //if it doesn't exist then return failure
            if (contextBot.myServitor.prologEngine == null)
            {
                yield return RunStatus.Failure;
                yield break;
            }
            RunStatus r = RunStatus.Failure;
            try
            {
                lock (contextBot.myServitor.prologEngine)
                {
                    bool result = contextBot.myServitor.prologEngine.isTrueIn(innerStr, mtName);
                    if (result) r = RunStatus.Success;
                }
            }
            catch (Exception e)
            {
                LogException(myNode, e, "ProcessAssertGuest '{0}':{1}", condition, EMsg(e));
                r = RunStatus.Failure;
            }

            yield return r;
            yield break;
        }

        public IEnumerable<RunStatus> ProcessEvalPrologMacro(BTXmlNode myNode)
        {
            // given a prolog query, will take the bindings of var as a list of subbehavior id's (or other)
            // then place them inside a <cmd> tag and execute
            //examples:
            // <evalprologmacro outtercmd='select' innercmd='subbehavior' mt='decide' var="ACTION" >should_act(ACTION)</query_macro>
            // <evalprologmacro outtercmd='random' innercmd='task,say' mt='tellerSpindle' var="MESSAGE" >couldSay(MESSAGE)</query_macro>
            //
            string filler = "";

            string var = "X";
            string outerCmd = "random";
            string innerCmd = "task,say";
            string mtName = "root";
            string outerCode = "";
            string innerCode = "";
            try
            {
                if (!string.IsNullOrEmpty(myNode.AttributesV("filler"))) filler = myNode.AttributesV("filler");
                if (!string.IsNullOrEmpty(myNode.AttributesV("var"))) var = myNode.AttributesV("var");
                if (!string.IsNullOrEmpty(myNode.AttributesV("outercmd"))) outerCmd = myNode.AttributesV("outercmd");
                if (!string.IsNullOrEmpty(myNode.AttributesV("innercmd"))) innerCmd = myNode.AttributesV("innercmd");
                if (!string.IsNullOrEmpty(myNode.AttributesV("mt"))) mtName = myNode.AttributesV("mt");
            }
            catch (Exception e)
            {
                LogException(myNode, e, "PrologMacro");
            }
            string innerStr = myNode.InnerXml.Trim();
            //if it doesn't exist then return failure
            if (contextBot.myServitor.prologEngine == null)
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
                lock (contextBot.myServitor.prologEngine)
                {
                    contextBot.myServitor.prologEngine.askQuery(innerStr, mtName, out bingingsList);
                }

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
                                    innerCode += String.Format("<{0}>{1}{2}</{0}>\n", innerCmd, filler, bindings[k]);
                                }
                                else
                                {
                                    string frag = bindings[k];
                                    if (filler.Length > 0) { frag = filler + " " + frag; }
                                    for (int i = splitInner.Length - 1; i >= 0; i--)
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

                for (int i = splitOuter.Length - 1; i >= 0; i--)
                {
                    string tag = splitOuter[i];
                    outerFrag = String.Format("<{0}>\n{1}</{0}>\n", tag, outerFrag);
                }
                outerCode += outerFrag + "\n";

            }
            catch (Exception e)
            {
                LogException(myNode, e, "ProcessPrologBehaveMacro '{0}','{1}','{2}','{3}','{4}':'{5}'", var, innerCmd, outerCmd, innerStr, mtName, EMsg(e));
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
            if (!contextBot.servitor.partFilterDict.ContainsKey(name))
            {
                contextBot.servitor.partFilterDict.Add(name, new SymbolicParticleFilter());
            }
            resultFilter = contextBot.servitor.partFilterDict[name];
            return resultFilter;
        }

        // <definestate prob=""> state_list|...</>
        //bot.myServitor.partFilter.prototype.variables.Add("in(r0)", 0.8);
        public IEnumerable<RunStatus> ProcessDefineState(BTXmlNode myNode)
        {
            // Append some si_text 
            RunStatus rs = RunStatus.Failure;
            string probStr = "0.5";
            string innerStr = myNode.InnerXml.Trim();
            string filter = "basic";

            try
            {
                if (!string.IsNullOrEmpty(myNode.AttributesV("prob"))) probStr = myNode.AttributesV("prob");
                if (!string.IsNullOrEmpty(myNode.AttributesV("filter"))) filter = myNode.AttributesV("filter");
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
                LogException(myNode, e, "ProcessDefineState '{0}','{1}':{2}", probStr, innerStr, EMsg(e));
                rs = RunStatus.Failure;
            }
            yield return rs;
            yield break;
        }
            
        // <stateConstraintSet> state_list|...</>
        //    bot.myServitor.partFilter.constraintSet.Add("in(r0)|in(r1)|in(r2)|in(r3)|in(r4)");
        public IEnumerable<RunStatus> ProcessConstraintSet(BTXmlNode myNode)
        {
            // Append some si_text 
            RunStatus rs = RunStatus.Failure;
            string innerStr = myNode.InnerXml.Trim();
            string filter = "basic";

            try
            {
                if (!string.IsNullOrEmpty(myNode.AttributesV("filter"))) filter = myNode.AttributesV("filter");
                SymbolicParticleFilter myFilter = findOrCreatePF(filter);
                if (!myFilter.constraintSet.Contains(innerStr))
                {
                    myFilter.constraintSet.Add(innerStr);
                }
                rs = RunStatus.Success;
            }
            catch (Exception e)
            {
                LogException(myNode, e, "ProcessConstraintSet '{0}':{1}", innerStr, EMsg(e));
                rs = RunStatus.Failure;
            }
            yield return rs;
            yield break;
        }

        // <stateSense state="" prob=""> senseList|...</>
        //    bot.myServitor.partFilter.addStateSenseProb("in(r0)|sense(even)=0.95");
        public IEnumerable<RunStatus> ProcessStateSense(BTXmlNode myNode)
        {
            // Append some si_text 
            RunStatus rs = RunStatus.Failure;
            string probStr = "0.5";
            string state = "in(0)";
            string innerStr = myNode.InnerXml.Trim();
            string filter = "basic";
            try
            {
                if (!string.IsNullOrEmpty(myNode.AttributesV("prob"))) probStr = myNode.AttributesV("prob");
                if (!string.IsNullOrEmpty(myNode.AttributesV("state"))) state = myNode.AttributesV("state");
                if (!string.IsNullOrEmpty(myNode.AttributesV("filter"))) filter = myNode.AttributesV("filter");
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
                LogException(myNode, e, "ProcessStateSense '{0}','{1}':{2}", probStr, innerStr, EMsg(e));
                rs = RunStatus.Failure;
            }
            yield return rs;
            yield break;
        }

        //   <stateActTransition state="" act="" prob="">nextstatelist|...</>
        //   bot.myServitor.partFilter.addStateActTransition("in(r0)|act(forward)=0.5:in(r0)");
        public IEnumerable<RunStatus> ProcessStateActTransition(BTXmlNode myNode)
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
                if (!string.IsNullOrEmpty(myNode.AttributesV("prob"))) probStr = myNode.AttributesV("prob");
                if (!string.IsNullOrEmpty(myNode.AttributesV("state"))) state = myNode.AttributesV("state");
                if (!string.IsNullOrEmpty(myNode.AttributesV("action"))) act = myNode.AttributesV("action");
                if (!string.IsNullOrEmpty(myNode.AttributesV("filter"))) filter = myNode.AttributesV("filter");
                SymbolicParticleFilter myFilter = findOrCreatePF(filter);
                double prob = Double.Parse(probStr);
                string[] state_list = innerStr.Split('|');

                string frag = String.Format("{0}|{1}={2}:{3}", state, act, prob, innerStr);
                myFilter.addStateActTransition(frag);

                rs = RunStatus.Success;
            }
            catch (Exception e)
            {
                LogException(myNode, e, "ProcessStateActTransition '{0}','{1}':{2}", probStr, innerStr, EMsg(e));
                rs = RunStatus.Failure;
            }
            yield return rs;
            yield break;
        }

        // use particleActMt, particleSenseMt, particleStateMt
        //    bot.myServitor.partFilter.quickFilter("act(forward)", "sense(odd)");
        public IEnumerable<RunStatus> ProcessQuickFilter(BTXmlNode myNode)
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
                if (!string.IsNullOrEmpty(myNode.AttributesV("sensemt"))) senseMt = myNode.AttributesV("sensemt");
                if (!string.IsNullOrEmpty(myNode.AttributesV("actmt"))) actMt = myNode.AttributesV("actmt");
                if (!string.IsNullOrEmpty(myNode.AttributesV("filter"))) filter = myNode.AttributesV("filter");
                SymbolicParticleFilter myFilter = findOrCreatePF(filter);
                lock (contextBot.prologEngine)
                {
                    foreach (string q in myFilter.actList)
                    {
                        if (contextBot.prologEngine.isTrueIn(q, actMt))
                        {
                            if (actset.Length > 0) actset += "|";
                            actset += q;
                        }
                    }
                    foreach (string q in myFilter.senseList)
                    {
                        if (contextBot.prologEngine.isTrueIn(q, senseMt))
                        {
                            if (senseset.Length > 0) senseset += "|";
                            senseset += q;
                        }
                    }
                }
                myFilter.quickFilter(actset, senseset);

                rs = RunStatus.Success;
            }
            catch (Exception e)
            {
                LogException(myNode, e, "ProcessQuickFilter '{0}','{1}':{2}", senseMt, actMt, EMsg(e));
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
        public IEnumerable<RunStatus> ProcessMeanParticle(BTXmlNode myNode)
        {
            // Append some si_text 
            RunStatus rs = RunStatus.Failure;
            string probStr = "0.5";
            string mtName = "meanParticleMt";
            string innerStr = myNode.InnerXml;
            double threshold = 0.5;
            string filter = "basic";
            string spindle = "";
            string common = "";
            try
            {
                if (!string.IsNullOrEmpty(myNode.AttributesV("threshold"))) probStr = myNode.AttributesV("threshold");
                if (!string.IsNullOrEmpty(myNode.AttributesV("mt"))) mtName = myNode.AttributesV("mt");
                if (!string.IsNullOrEmpty(myNode.AttributesV("filter"))) filter = myNode.AttributesV("filter");
                if (!string.IsNullOrEmpty(myNode.AttributesV("spindle"))) spindle = myNode.AttributesV("spindle");
                if (!string.IsNullOrEmpty(myNode.AttributesV("common"))) common = myNode.AttributesV("common");
                
                SymbolicParticleFilter myFilter = findOrCreatePF(filter);
                threshold = Double.Parse(probStr);

                myFilter.defMeanParticle();
                myFilter.meanParticle.normalize(myFilter.constraintSet);
                string meanDMT = myFilter.meanParticle.asDataMt(threshold);
                lock (contextBot.prologEngine)
                {
                    contextBot.prologEngine.insertKB(meanDMT, mtName);
                    if ((spindle != "") || (common != ""))
                    {
                        int plen = myFilter.particles.Length;
                        for (int i = 0; i < plen; i++)
                        {
                            string partDMT = myFilter.particles[i].asDataMt(threshold);
                            string particleMt = String.Format("{0}_particle_{1}", filter, i);
                            contextBot.prologEngine.insertKB(partDMT, particleMt);
                            contextBot.prologEngine.setMtProbability(particleMt, myFilter.particles[i].prob);
                            if (spindle != "") { contextBot.prologEngine.connectMT(spindle, particleMt); }
                            if (common != "") { contextBot.prologEngine.connectMT(particleMt, common); }
                        }
                    }
                }
                rs = RunStatus.Success;
            }
            catch (Exception e)
            {
                LogException(myNode, e, "ProcessMeanParticle '{0}','{1}':{2}", threshold, mtName, EMsg(e));
                rs = RunStatus.Failure;
            }
            yield return rs;
            yield break;
        }

        // <genFilterFromMt mt="defParticleMt" threshold="0.0001" filter="basic"/>
        // will skip probabilities below threshold
        // will stateProb(STATE,PROB) , stateSenseProb(STATE,SENSE,PROB) and
        // stateTransitionProb(STATE,ACT,PROB,NEXT)

        public IEnumerable<RunStatus> ProcessGenFilterFromMt(BTXmlNode myNode)
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
                if (!string.IsNullOrEmpty(myNode.AttributesV("threshold"))) probStr = myNode.AttributesV("threshold");
                if (!string.IsNullOrEmpty(myNode.AttributesV("mt"))) mtName = myNode.AttributesV("mt");
                if (!string.IsNullOrEmpty(myNode.AttributesV("filter"))) filter = myNode.AttributesV("filter");
                SymbolicParticleFilter myFilter = findOrCreatePF(filter);
                threshold = Double.Parse(probStr);

                List<Dictionary<string, string>> bingingsList = new List<Dictionary<string, string>>();
                lock (contextBot.prologEngine)
                {
                    //State-Apriori
                    contextBot.prologEngine.askQuery("stateProb(STATE,PROB)", mtName, out bingingsList);
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
                    contextBot.prologEngine.askQuery("stateSenseProb(STATE,SENSE,PROB)", mtName, out bingingsList);
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
                    contextBot.prologEngine.askQuery("stateTransitionProb(STATE,ACT,PROB,NEXT)", mtName, out bingingsList);
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
                }

                rs = RunStatus.Success;
            }
            catch (Exception e)
            {
                LogException(myNode, e, "ProcessGenFilterFromMt '{0}','{1}':{2}", threshold, mtName, filter, EMsg(e));
                rs = RunStatus.Failure;
            }
            yield return rs;
            yield break;
        }
        #endregion

        #region Coppelia
        // <UpdateCoppeliaFromMt mt="defCoppeliaMt" threshold="0.0001" filter="basic"/>
        // will update Coppelia states by querying state(cState) in Mt

        public IEnumerable<RunStatus> ProcessUpdateCoppeliaFromMt(BTXmlNode myNode)
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
                if (!string.IsNullOrEmpty(myNode.AttributesV("threshold"))) probStr = myNode.AttributesV("threshold");
                if (!string.IsNullOrEmpty(myNode.AttributesV("mt"))) mtName = myNode.AttributesV("mt");
                if (!string.IsNullOrEmpty(myNode.AttributesV("filter"))) filter = myNode.AttributesV("filter");
                SymbolicParticleFilter myFilter = findOrCreatePF(filter);
                threshold = Double.Parse(probStr);

                List<Dictionary<string, string>> bingingsList = new List<Dictionary<string, string>>();
                foreach (string state in contextBot.servitor.CoppeliaStateDictionary.Keys)
                {
                    string query = String.Format("state({0})", state);
                    bool isTrue = contextBot.prologEngine.isTrueIn(query, mtName);
                    int stateIndex = contextBot.servitor.CoppeliaStateDictionary[state];
                    Global.SetState(stateIndex, isTrue);
                }
                Console.WriteLine("coppelia ProcessUpdateCoppeliaFromMt :{0}", myNode.OuterXml);
                rs = RunStatus.Success;
            }
            catch (Exception e)
            {
                LogException(myNode, e, "ProcessUpdateCoppeliaFromMt '{0}','{1}':{2}", threshold, mtName, filter, EMsg(e));
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

        public IEnumerable<RunStatus> ProcessGenCoppeliaFromMt0(BTXmlNode myNode)
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
                if (!string.IsNullOrEmpty(myNode.AttributesV("threshold"))) probStr = myNode.AttributesV("threshold");
                if (!string.IsNullOrEmpty(myNode.AttributesV("mt"))) mtName = myNode.AttributesV("mt");
                if (!string.IsNullOrEmpty(myNode.AttributesV("filter"))) filter = myNode.AttributesV("filter");
                SymbolicParticleFilter myFilter = findOrCreatePF(filter);
                threshold = Double.Parse(probStr);

                List<Dictionary<string, string>> bingingsList = new List<Dictionary<string, string>>();
                lock (contextBot.myServitor.prologEngine)
                {
                    // agentActions(Action, positive, negative)
                    contextBot.prologEngine.askQuery("agentActions(ACTION, POS, NEG)", mtName, out bingingsList);
                    foreach (Dictionary<string, string> bindings in bingingsList)
                    {
                        float pos = float.Parse(bindings["POS"]);
                        float neg = float.Parse(bindings["NEG"]);
                        string cAction = bindings["ACTION"];
                        if (contextBot.servitor.CoppeliaActionDictionary.ContainsKey(cAction))
                        {
                            contextBot.servitor.CoppeliaActionDictionary[cAction].SetValence(pos, neg);
                        }
                        else
                        {
                            AgentAction newAction = new AgentAction(cAction, pos, neg);
                            contextBot.servitor.CoppeliaActionDictionary[cAction] = newAction;
                        }
                    }

                    // actionResponse(Action,Response)
                    contextBot.prologEngine.askQuery("actionResponse(ACTION,RESPONSE)", mtName, out bingingsList);
                    foreach (Dictionary<string, string> bindings in bingingsList)
                    {
                        string cAction = bindings["ACTION"];
                        string cResponse = bindings["RESPONSE"];
                        if (contextBot.servitor.CoppeliaActionDictionary.ContainsKey(cAction))
                        {
                            if (contextBot.servitor.CoppeliaActionDictionary.ContainsKey(cResponse))
                            {
                                AgentAction a1 = contextBot.servitor.CoppeliaActionDictionary[cAction];
                                AgentAction a2 = contextBot.servitor.CoppeliaActionDictionary[cResponse];
                                a1.AddResponse(a2.GlobalIndex);
                            }
                        }
                    }

                    // defState(State,Initial)
                    contextBot.prologEngine.askQuery("defState(STATE,INITIAL)", mtName, out bingingsList);
                    foreach (Dictionary<string, string> bindings in bingingsList)
                    {
                        string cState = bindings["STATE"];
                        string cInitState = bindings["INITIAL"];
                        bool bState = false;
                        bState = bool.Parse(cInitState);
                        if (!contextBot.servitor.CoppeliaStateDictionary.ContainsKey(cState))
                        {
                            int newState = Global.AddState(bState);
                            contextBot.servitor.CoppeliaStateDictionary[cState] = newState;
                        }
                    }
                    // ambition(Actor,State,value)
                    contextBot.prologEngine.askQuery("stateAmbition(ACTOR,STATE,VALUE)", mtName, out bingingsList);
                    foreach (Dictionary<string, string> bindings in bingingsList)
                    {
                        string cActor = bindings["ACTOR"];
                        string cState = bindings["STATE"];
                        string cValue = bindings["VALUE"];
                        float fValue = 0;
                        fValue = float.Parse(cValue);
                        if (contextBot.servitor.CoppeliaStateDictionary.ContainsKey(cState))
                        {
                            if (contextBot.servitor.CoppeliaAgentDictionary.ContainsKey(cActor))
                            {
                                Agent a1 = contextBot.servitor.CoppeliaAgentDictionary[cActor];
                                int state = contextBot.servitor.CoppeliaStateDictionary[cState];
                                a1.AddAmbition(state, fValue);
                            }
                        }
                    }
                    // actionStateBelief(Actor,Action,State,value)
                    contextBot.prologEngine.askQuery("actionStateBelief(ACTOR,ACTION,STATE,VALUE)", mtName, out bingingsList);
                    foreach (Dictionary<string, string> bindings in bingingsList)
                    {
                        string cActor = bindings["ACTOR"];
                        string cAction = bindings["ACTION"];
                        string cState = bindings["STATE"];
                        string cValue = bindings["VALUE"];
                        float fValue = 0;
                        fValue = float.Parse(cValue);

                        if (contextBot.servitor.CoppeliaAgentDictionary.ContainsKey(cActor))
                        {
                            if (contextBot.servitor.CoppeliaActionDictionary.ContainsKey(cAction))
                            {
                                if (contextBot.servitor.CoppeliaStateDictionary.ContainsKey(cState))
                                {
                                    AgentAction act = contextBot.servitor.CoppeliaActionDictionary[cAction];
                                    Agent a1 = contextBot.servitor.CoppeliaAgentDictionary[cActor];
                                    int state = contextBot.servitor.CoppeliaStateDictionary[cState];
                                    a1.SetActionStateBelief(act.GlobalIndex, state, fValue);
                                }
                            }
                        }
                    }
                    //Morals
                    // moralPrinciple(Moral,Initial)
                    contextBot.prologEngine.askQuery("moralPrinciple(MORAL,INITIAL)", mtName, out bingingsList);
                    foreach (Dictionary<string, string> bindings in bingingsList)
                    {
                        string cMoral = bindings["MORAL"];
                        string cInitState = bindings["INITIAL"];
                        bool bState = false;
                        bState = bool.Parse(cInitState);
                        if (!contextBot.servitor.CoppeliaMoralsDictionary.ContainsKey(cMoral))
                        {
                            int newState = Global.AddState(bState);
                            contextBot.servitor.CoppeliaMoralsDictionary[cMoral] = newState;
                        }
                    }
                    // moralAmbition(Actor,Moral,value)
                    contextBot.prologEngine.askQuery("stateAmbition(ACTOR,MORAL,VALUE)", mtName, out bingingsList);
                    foreach (Dictionary<string, string> bindings in bingingsList)
                    {
                        string cActor = bindings["ACTOR"];
                        string cMoral = bindings["MORAL"];
                        string cValue = bindings["VALUE"];
                        float fValue = 0;
                        fValue = float.Parse(cValue);
                        if (contextBot.servitor.CoppeliaMoralsDictionary.ContainsKey(cMoral))
                        {
                            if (contextBot.servitor.CoppeliaAgentDictionary.ContainsKey(cActor))
                            {
                                Agent a1 = contextBot.servitor.CoppeliaAgentDictionary[cActor];
                                int moral = contextBot.servitor.CoppeliaMoralsDictionary[cMoral];
                                a1.AddMoralAmbition(moral, fValue);
                            }
                        }
                    }
                    // actionMoralBelief(Actor,Action,Moral,value)
                    contextBot.prologEngine.askQuery("actionStateBelief(ACTOR,ACTION,MORAL,VALUE)", mtName, out bingingsList);
                    foreach (Dictionary<string, string> bindings in bingingsList)
                    {
                        string cActor = bindings["ACTOR"];
                        string cAction = bindings["ACTION"];
                        string cMoral = bindings["MORAL"];
                        string cValue = bindings["VALUE"];
                        float fValue = 0;
                        fValue = float.Parse(cValue);

                        if (contextBot.servitor.CoppeliaAgentDictionary.ContainsKey(cActor))
                        {
                            if (contextBot.servitor.CoppeliaActionDictionary.ContainsKey(cAction))
                            {
                                if (contextBot.servitor.CoppeliaMoralsDictionary.ContainsKey(cMoral))
                                {
                                    AgentAction act = contextBot.servitor.CoppeliaActionDictionary[cAction];
                                    Agent a1 = contextBot.servitor.CoppeliaAgentDictionary[cActor];
                                    int moral = contextBot.servitor.CoppeliaMoralsDictionary[cMoral];
                                    a1.SetActionMoralPrincipleBelief(act.GlobalIndex, moral, fValue);
                                }
                            }
                        }
                    }
                }


                rs = RunStatus.Success;
            }
            catch (Exception e)
            {
                LogException(myNode, e, "ProcessGenCoppeliaFromMt '{0}','{1}':{2}", threshold, mtName, filter, EMsg(e));
                rs = RunStatus.Failure;
            }
            yield return rs;
            yield break;
        }

        public IEnumerable<RunStatus> ProcessGenCoppeliaFromMt(BTXmlNode myNode)
        {
            //Queries MT for Coppelia related info to create a BTXML fragment
            // that is then interperted

            RunStatus rs = RunStatus.Failure;
            string mtName = "defCoppeliaMt";
            string innerStr = myNode.InnerXml;

            try
            {
                if (!string.IsNullOrEmpty(myNode.AttributesV("mt"))) mtName = myNode.AttributesV("mt");
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

                string btxmlCode = "";
                btxmlCode += "<aiml version=\"1.0\">\n";
                btxmlCode += " <state name=\"*\">\n";
                btxmlCode += "  <btxml>\n";
                btxmlCode += xcode;
                btxmlCode += "  </btxml>\n";
                btxmlCode += " </state>\n";
                btxmlCode += "</aiml>\n";
                BTXmlDocument coppeliaDoc = new BTXmlDocument();
                coppeliaDoc.LoadXml(btxmlCode);
                Console.WriteLine("-------------------------------------");
                Console.WriteLine("------ GenCoppeliaFromMt:{0} --------", mtName);
                Console.WriteLine(btxmlCode);
                Console.WriteLine("-------------------------------------");
                var bot = this._bot;
                bot.loadAIMLFromXML(coppeliaDoc, "mt:" + mtName + DateTime.Now.ToString());

                rs = RunStatus.Success;
            }
            catch (Exception e)
            {
                LogException(myNode, e, "ProcessGenCoppeliaFromMt '{0}',{1}", mtName, EMsg(e));
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
                contextBot.prologEngine.askQuery(query, mtName, out bingingsList);
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
                LogException(e, "kbToBTXML '{0}':{1}", mtName, query);
            }
            return code;
        }

 



        public IEnumerable<RunStatus> ProcessCoppeliaAgentFeature(BTXmlNode myNode)
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
                if (!string.IsNullOrEmpty(myNode.AttributesV("feature"))) cFeature = myNode.AttributesV("feature");
                if (!string.IsNullOrEmpty(myNode.AttributesV("agent"))) cAgent = myNode.AttributesV("agent");
                if (!string.IsNullOrEmpty(myNode.AttributesV("value"))) cValue = myNode.AttributesV("value");
                fValue = float.Parse(cValue);
                int featureID = AgentFeatures.Parse(cFeature);
                if (featureID >= 0)
                {
                    if (contextBot.servitor.CoppeliaAgentDictionary.ContainsKey(cAgent))
                    {
                        Agent a1 = contextBot.servitor.CoppeliaAgentDictionary[cAgent];
                        a1.SetFeature(featureID, fValue);
                        //Console.WriteLine("coppelia Processed :{0}", myNode.OuterXml);
                    }
                }
            }
            catch(Exception e)
            {
                LogException(myNode, e, "ProcessCoppeliaAgentFeature");
            }
            //int newState = Global.AddState(bState);
            //bot.servitor.CoppeliaStateDictionary[cState] = newState;
            yield return rs;
            yield break;
        }

        public IEnumerable<RunStatus> ProcessCoppeliaFeatureBelief(BTXmlNode myNode)
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
                if (!string.IsNullOrEmpty(myNode.AttributesV("feature"))) cFeature = myNode.AttributesV("feature");
                if (!string.IsNullOrEmpty(myNode.AttributesV("agent"))) cAgent = myNode.AttributesV("agent");
                if (!string.IsNullOrEmpty(myNode.AttributesV("target"))) cTarget = myNode.AttributesV("target");
                if (!string.IsNullOrEmpty(myNode.AttributesV("value"))) cValue = myNode.AttributesV("value");
                if (!string.IsNullOrEmpty(myNode.AttributesV("state"))) cState = myNode.AttributesV("state");
                fValue = float.Parse(cValue);
                int featureID = AgentFeatures.Parse(cFeature);

                if (featureID >= 0)
                {
                    if (contextBot.servitor.CoppeliaAgentDictionary.ContainsKey(cAgent))
                    {
                        if (contextBot.servitor.CoppeliaAgentDictionary.ContainsKey(cTarget))
                        {
                            if (contextBot.servitor.CoppeliaStateDictionary.ContainsKey(cState))
                            {
                                Agent a1 = contextBot.servitor.CoppeliaAgentDictionary[cAgent];
                                Agent target = contextBot.servitor.CoppeliaAgentDictionary[cTarget];
                                int state = contextBot.servitor.CoppeliaStateDictionary[cState];
                                a1.SetFeatureBelief(featureID, state, target.AgentID, fValue);
                                //Console.WriteLine("coppelia Processed :{0}", myNode.OuterXml);
                            }
                        }
                    }
                }
            }
            catch (Exception e)
            {
                LogException(myNode, e, "ProcessCoppeliaFeatureBelief");
            }
            //int newState = Global.AddState(bState);
            //bot.servitor.CoppeliaStateDictionary[cState] = newState;
            yield return rs;
            yield break;
        }
        public IEnumerable<RunStatus> ProcessCoppeliaAgentResponsibleBelief(BTXmlNode myNode)
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
                if (!string.IsNullOrEmpty(myNode.AttributesV("agent"))) cAgent = myNode.AttributesV("agent");
                if (!string.IsNullOrEmpty(myNode.AttributesV("target"))) cTarget = myNode.AttributesV("target");
                if (!string.IsNullOrEmpty(myNode.AttributesV("value"))) cValue = myNode.AttributesV("value");
                if (!string.IsNullOrEmpty(myNode.AttributesV("state"))) cState = myNode.AttributesV("state");
                fValue = float.Parse(cValue);
                if (contextBot.servitor.CoppeliaAgentDictionary.ContainsKey(cAgent))
                {
                    if (contextBot.servitor.CoppeliaAgentDictionary.ContainsKey(cTarget))
                    {
                        if (contextBot.servitor.CoppeliaStateDictionary.ContainsKey(cState))
                        {
                            Agent a1 = contextBot.servitor.CoppeliaAgentDictionary[cAgent];
                            Agent target = contextBot.servitor.CoppeliaAgentDictionary[cTarget];
                            int state = contextBot.servitor.CoppeliaStateDictionary[cState];
                            a1.SetAgentResponsibleBelief(target.AgentID, state, fValue);
                            //Console.WriteLine("coppelia Processed :{0}", myNode.OuterXml);
                        }
                    }
                }
            }
            catch (Exception e)
            {
                LogException(myNode, e, "ProcessCoppelia");
            }
            //int newState = Global.AddState(bState);
            //bot.servitor.CoppeliaStateDictionary[cState] = newState;
            yield return rs;
            yield break;
        }
        public IEnumerable<RunStatus> ProcessCoppeliaExpectedSatisfaction(BTXmlNode myNode)
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
                if (!string.IsNullOrEmpty(myNode.AttributesV("agent"))) cAgent = myNode.AttributesV("agent");
                if (!string.IsNullOrEmpty(myNode.AttributesV("value"))) cValue = myNode.AttributesV("value");
                if (!string.IsNullOrEmpty(myNode.AttributesV("action"))) cAction = myNode.AttributesV("state");
                if (!string.IsNullOrEmpty(myNode.AttributesV("target"))) cTarget = myNode.AttributesV("target");
                fValue = float.Parse(cValue);
                if (contextBot.servitor.CoppeliaAgentDictionary.ContainsKey(cAgent))
                {
                    if (contextBot.servitor.CoppeliaActionDictionary.ContainsKey(cAction))
                    {
                        Agent a1 = contextBot.servitor.CoppeliaAgentDictionary[cAgent];
                        Agent target = contextBot.servitor.CoppeliaAgentDictionary[cTarget];
                        AgentAction action = contextBot.servitor.CoppeliaActionDictionary[cAction];
                        a1.SetExpectedSatisfaction(target.AgentID, action.GlobalIndex, fValue);
                        //Console.WriteLine("coppelia Processed :{0}", myNode.OuterXml);
                    }
                }
            }
            catch (Exception e)
            {
                LogException(myNode, e, "ProcessCoppelia");
            }
            //int newState = Global.AddState(bState);
            //bot.servitor.CoppeliaStateDictionary[cState] = newState;
            yield return rs;
            yield break;
        }

        public IEnumerable<RunStatus> ProcessCoppeliaAnger(BTXmlNode myNode)
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
                if (!string.IsNullOrEmpty(myNode.AttributesV("agent"))) cAgent = myNode.AttributesV("agent");
                if (!string.IsNullOrEmpty(myNode.AttributesV("value"))) cValue = myNode.AttributesV("value");
                if (!string.IsNullOrEmpty(myNode.AttributesV("target"))) cTarget = myNode.AttributesV("target");
                fValue = float.Parse(cValue);
                if (contextBot.servitor.CoppeliaAgentDictionary.ContainsKey(cAgent))
                {
                    if (contextBot.servitor.CoppeliaAgentDictionary.ContainsKey(cTarget))
                    {
                        Agent a1 = contextBot.servitor.CoppeliaAgentDictionary[cAgent];
                        Agent target = contextBot.servitor.CoppeliaAgentDictionary[cTarget];
                        a1.SetAnger(target.AgentID, fValue);
                        //Console.WriteLine("coppelia Processed :{0}", myNode.OuterXml);
                    }
                }
            }
            catch (Exception e)
            {
                LogException(myNode, e, "ProcessCoppelia");
            }
            //int newState = Global.AddState(bState);
            //bot.servitor.CoppeliaStateDictionary[cState] = newState;
            yield return rs;
            yield break;
        }
        public IEnumerable<RunStatus> ProcessCoppeliaPraiseworthy(BTXmlNode myNode)
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
                if (!string.IsNullOrEmpty(myNode.AttributesV("agent"))) cAgent = myNode.AttributesV("agent");
                if (!string.IsNullOrEmpty(myNode.AttributesV("value"))) cValue = myNode.AttributesV("value");
                if (!string.IsNullOrEmpty(myNode.AttributesV("target"))) cTarget = myNode.AttributesV("target");
                fValue = float.Parse(cValue);
                if (contextBot.servitor.CoppeliaAgentDictionary.ContainsKey(cAgent))
                {
                    if (contextBot.servitor.CoppeliaAgentDictionary.ContainsKey(cTarget))
                    {
                        Agent a1 = contextBot.servitor.CoppeliaAgentDictionary[cAgent];
                        Agent target = contextBot.servitor.CoppeliaAgentDictionary[cTarget];
                        a1.SetPraiseworthy(target.AgentID, fValue);
                        //Console.WriteLine("coppelia Processed :{0}", myNode.OuterXml);
                    }
                }
            }
            catch (Exception e)
            {
                LogException(myNode, e, "ProcessCoppelia");
            }
            //int newState = Global.AddState(bState);
            //bot.servitor.CoppeliaStateDictionary[cState] = newState;
            yield return rs;
            yield break;
        }
        public IEnumerable<RunStatus> ProcessCoppeliaEmotion(BTXmlNode myNode)
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
                if (!string.IsNullOrEmpty(myNode.AttributesV("agent"))) cAgent = myNode.AttributesV("agent");
                if (!string.IsNullOrEmpty(myNode.AttributesV("value"))) cValue = myNode.AttributesV("value");
                if (!string.IsNullOrEmpty(myNode.AttributesV("emotion"))) cEmotion = myNode.AttributesV("emotion");
                fValue = float.Parse(cValue);
                int iEmotion = AgentEmotions.Parse(cEmotion);
                if (contextBot.servitor.CoppeliaAgentDictionary.ContainsKey(cAgent))
                {
                    if (iEmotion >= 0)
                    {
                        Agent a1 = contextBot.servitor.CoppeliaAgentDictionary[cAgent];
                        a1.SetEmotion(iEmotion, fValue);
                        //Console.WriteLine("coppelia Processed :{0}", myNode.OuterXml);
                    }
                }
            }
            catch (Exception e)
            {
                LogException(myNode, e, "ProcessCoppelia");
            }
            //int newState = Global.AddState(bState);
            //bot.servitor.CoppeliaStateDictionary[cState] = newState;
            yield return rs;
            yield break;
        }

        public IEnumerable<RunStatus> ProcessCoppeliaDesired(BTXmlNode myNode)
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
                if (!string.IsNullOrEmpty(myNode.AttributesV("agent"))) cAgent = myNode.AttributesV("agent");
                if (!string.IsNullOrEmpty(myNode.AttributesV("value"))) cValue = myNode.AttributesV("value");
                if (!string.IsNullOrEmpty(myNode.AttributesV("emotion"))) cEmotion = myNode.AttributesV("emotion");
                fValue = float.Parse(cValue);
                int iEmotion = AgentEmotions.Parse(cEmotion);
                if (contextBot.servitor.CoppeliaAgentDictionary.ContainsKey(cAgent))
                {
                    if (iEmotion >= 0)
                    {
                        Agent a1 = contextBot.servitor.CoppeliaAgentDictionary[cAgent];
                        a1.SetDesired(iEmotion, fValue);
                        //Console.WriteLine("coppelia Processed :{0}", myNode.OuterXml);
                    }
                }
            }
            catch (Exception e)
            {
                LogException(myNode, e, "ProcessCoppelia");
            }
            //int newState = Global.AddState(bState);
            //bot.servitor.CoppeliaStateDictionary[cState] = newState;
            yield return rs;
            yield break;
        }
        public IEnumerable<RunStatus> ProcessCoppeliaActionTendancy(BTXmlNode myNode)
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
                if (!string.IsNullOrEmpty(myNode.AttributesV("agent"))) cAgent = myNode.AttributesV("agent");
                if (!string.IsNullOrEmpty(myNode.AttributesV("target"))) cTarget = myNode.AttributesV("target");
                if (!string.IsNullOrEmpty(myNode.AttributesV("action"))) cAction = myNode.AttributesV("action");
                if (!string.IsNullOrEmpty(myNode.AttributesV("value"))) cValue = myNode.AttributesV("value");
                fValue = float.Parse(cValue);

                if (contextBot.servitor.CoppeliaAgentDictionary.ContainsKey(cAgent))
                {
                    if (contextBot.servitor.CoppeliaAgentDictionary.ContainsKey(cTarget))
                    {
                        if (contextBot.servitor.CoppeliaActionDictionary.ContainsKey(cAction))
                        {
                            Agent a1 = contextBot.servitor.CoppeliaAgentDictionary[cAgent];
                            Agent a2 = contextBot.servitor.CoppeliaAgentDictionary[cTarget];
                            AgentAction act = contextBot.servitor.CoppeliaActionDictionary[cAction];
                            a1.SetAT(a2.AgentID, act.GlobalIndex, fValue);
                            //Console.WriteLine("coppelia Processed :{0}", myNode.OuterXml);
                        }
                    }
                }
            }
            catch (Exception e)
            {
                LogException(myNode, e, "ProcessCoppelia");
            }
            //int newState = Global.AddState(bState);
            //bot.servitor.CoppeliaStateDictionary[cState] = newState;
            yield return rs;
            yield break;
        }

        public IEnumerable<RunStatus> ProcessCoppeliaFeatureEmotionBelief(BTXmlNode myNode)
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
                if (!string.IsNullOrEmpty(myNode.AttributesV("agent"))) cAgent = myNode.AttributesV("agent");
                if (!string.IsNullOrEmpty(myNode.AttributesV("feature"))) cFeature = myNode.AttributesV("feature");
                if (!string.IsNullOrEmpty(myNode.AttributesV("emotion"))) cEmotion = myNode.AttributesV("emotion");

                fValue = float.Parse(cValue);
                int iEmotion = AgentEmotions.Parse(cEmotion);
                int iFeature = AgentFeatures.Parse(cFeature);

                if (contextBot.servitor.CoppeliaAgentDictionary.ContainsKey(cAgent))
                {
                    if (iEmotion >= 0)
                    {
                        if (iFeature >= 0)
                        {
                            Agent a1 = contextBot.servitor.CoppeliaAgentDictionary[cAgent];
                            a1.SetFeatureEmotionBelief(iFeature, iEmotion, fValue);
                            //Console.WriteLine("coppelia Processed :{0}", myNode.OuterXml);
                        }
                    }
                }
            }
            catch (Exception e)
            {
                LogException(myNode, e, "ProcessCoppelia");
            }
            //int newState = Global.AddState(bState);
            //bot.servitor.CoppeliaStateDictionary[cState] = newState;
            yield return rs;
            yield break;
        }

        public IEnumerable<RunStatus> ProcessCoppeliaAction(BTXmlNode myNode)
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
                if (!string.IsNullOrEmpty(myNode.AttributesV("action"))) cAction = myNode.AttributesV("action");
                if (!string.IsNullOrEmpty(myNode.AttributesV("positivity"))) cPositivity = myNode.AttributesV("positivity");
                if (!string.IsNullOrEmpty(myNode.AttributesV("negativity"))) cNegativity = myNode.AttributesV("negativity");
                fPositivity = float.Parse(cPositivity);
                fNegativity = float.Parse(cNegativity);
                if (!contextBot.servitor.CoppeliaActionDictionary.ContainsKey(cAction))
                {
                    AgentAction newAction = new AgentAction(cAction, fPositivity, fNegativity);
                    contextBot.servitor.CoppeliaActionDictionary[cAction] = newAction;
                }
                else
                {
                    contextBot.servitor.CoppeliaActionDictionary[cAction].SetValence(fPositivity, fNegativity);
                }
                //Console.WriteLine("coppelia Processed :{0}", myNode.OuterXml);
            }
            catch (Exception e)
            {
                LogException(myNode, e, "ProcessCoppelia");
            }
            yield return rs;
            yield break;
        }

        public IEnumerable<RunStatus> ProcessCoppeliaState(BTXmlNode myNode)
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
                if (!string.IsNullOrEmpty(myNode.AttributesV("state"))) cState = myNode.AttributesV("state");
                if (!string.IsNullOrEmpty(myNode.AttributesV("initial"))) cInitState = myNode.AttributesV("initial");
                bState = bool.Parse(cInitState);
                if (!contextBot.servitor.CoppeliaStateDictionary.ContainsKey(cState))
                {
                    int newState = Global.AddState(bState);
                    contextBot.servitor.CoppeliaStateDictionary[cState] = newState;
                    //Console.WriteLine("coppelia Processed :{0}", myNode.OuterXml);
                }
            }
            catch (Exception e)
            {
                LogException(myNode, e, "ProcessCoppelia");
            }
            yield return rs;
            yield break;
        }



        public IEnumerable<RunStatus> ProcessCoppeliaStateLikelihood(BTXmlNode myNode)
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
                if (!string.IsNullOrEmpty(myNode.AttributesV("agent"))) cAgent = myNode.AttributesV("agent");

                if (!string.IsNullOrEmpty(myNode.AttributesV("state"))) cState = myNode.AttributesV("state");
                if (!string.IsNullOrEmpty(myNode.AttributesV("likelihood"))) cStateProb = myNode.AttributesV("likelihood");
                pStateProb = float.Parse(cStateProb);
                if (contextBot.servitor.CoppeliaStateDictionary.ContainsKey(cState))
                {
                    if (contextBot.servitor.CoppeliaAgentDictionary.ContainsKey(cAgent))
                    {
                        Agent a1 = contextBot.servitor.CoppeliaAgentDictionary[cAgent];
                        int state = contextBot.servitor.CoppeliaStateDictionary[cState];
                        a1.SetStateLikelihood(state, pStateProb);
                        //Console.WriteLine("coppelia Processed :{0}", myNode.OuterXml);
                    }
                }
            }
            catch (Exception e)
            {
                LogException(myNode, e, "ProcessCoppelia");
            }
            yield return rs;
            yield break;
        }

        public IEnumerable<RunStatus> ProcessCoppeliaResponse(BTXmlNode myNode)
        {
            // <coppeliaResponse action="act" response="reaction" />
            // reaction can follow an agent receiving act

            RunStatus rs = RunStatus.Success;
            string innerStr = myNode.InnerXml.Trim();
            string cAct = "actAction";
            string cResponse = "actReaction";

            try
            {
                if (!string.IsNullOrEmpty(myNode.AttributesV("action"))) cAct = myNode.AttributesV("action");
                if (!string.IsNullOrEmpty(myNode.AttributesV("response"))) cResponse = myNode.AttributesV("response");

                if (contextBot.servitor.CoppeliaActionDictionary.ContainsKey(cAct))
                {
                    if (contextBot.servitor.CoppeliaActionDictionary.ContainsKey(cResponse))
                    {
                        AgentAction a1 = contextBot.servitor.CoppeliaActionDictionary[cAct];
                        AgentAction a2 = contextBot.servitor.CoppeliaActionDictionary[cResponse];
                        a1.AddResponse(a2.GlobalIndex);
                        //Console.WriteLine("coppelia Processed :{0}", myNode.OuterXml);
                    }
                }
            }
            catch (Exception e)
            {
                LogException(myNode, e, "ProcessCoppelia");
            }
            //int newState = Global.AddState(bState);
            //bot.servitor.CoppeliaStateDictionary[cState] = newState;
            yield return rs;
            yield break;
        }

        public IEnumerable<RunStatus> ProcessCoppeliaAmbition(BTXmlNode myNode)
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
                if (!string.IsNullOrEmpty(myNode.AttributesV("state"))) cState = myNode.AttributesV("state");
                if (!string.IsNullOrEmpty(myNode.AttributesV("agent"))) cAgent = myNode.AttributesV("agent");
                if (!string.IsNullOrEmpty(myNode.AttributesV("value"))) cValue = myNode.AttributesV("value");
                fValue = float.Parse(cValue);

                if (contextBot.servitor.CoppeliaStateDictionary.ContainsKey(cState))
                {
                    if (contextBot.servitor.CoppeliaAgentDictionary.ContainsKey(cAgent))
                    {
                        Agent a1 = contextBot.servitor.CoppeliaAgentDictionary[cAgent];
                        int state = contextBot.servitor.CoppeliaStateDictionary[cState];
                        a1.AddAmbition(state, fValue);
                        //Console.WriteLine("coppelia Processed :{0}", myNode.OuterXml);
                    }
                }
            }
            catch (Exception e)
            {
                LogException(myNode, e, "ProcessCoppelia");
            }
            //int newState = Global.AddState(bState);
            //bot.servitor.CoppeliaStateDictionary[cState] = newState;
            yield return rs;
            yield break;
        }
        // Coppelia Morals
        public IEnumerable<RunStatus> ProcessCoppeliaMoral(BTXmlNode myNode)
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
                if (!string.IsNullOrEmpty(myNode.AttributesV("moral"))) cMoral = myNode.AttributesV("moral");
                if (!string.IsNullOrEmpty(myNode.AttributesV("initial"))) cInitState = myNode.AttributesV("initial");
                bState = bool.Parse(cInitState);
                if (!contextBot.servitor.CoppeliaMoralsDictionary.ContainsKey(cMoral))
                {
                    int newMoral = Global.AddMoralPrinciple(bState);
                    contextBot.servitor.CoppeliaMoralsDictionary[cMoral] = newMoral;
                    //Console.WriteLine("coppelia Processed :{0}", myNode.OuterXml);
                }
            }
            catch (Exception e)
            {
                LogException(myNode, e, "ProcessCoppelia");
            }
            yield return rs;
            yield break;
        }

        public IEnumerable<RunStatus> ProcessCoppeliaMoralAmbition(BTXmlNode myNode)
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
                if (!string.IsNullOrEmpty(myNode.AttributesV("moral"))) cMoral = myNode.AttributesV("moral");
                if (!string.IsNullOrEmpty(myNode.AttributesV("agent"))) cAgent = myNode.AttributesV("agent");
                if (!string.IsNullOrEmpty(myNode.AttributesV("value"))) cValue = myNode.AttributesV("value");
                fValue = float.Parse(cValue);

                if (contextBot.servitor.CoppeliaMoralsDictionary.ContainsKey(cMoral))
                {
                    if (contextBot.servitor.CoppeliaAgentDictionary.ContainsKey(cAgent))
                    {
                        Agent a1 = contextBot.servitor.CoppeliaAgentDictionary[cAgent];
                        int moral = contextBot.servitor.CoppeliaStateDictionary[cMoral];
                        a1.AddMoralAmbition(moral, fValue);
                        //Console.WriteLine("coppelia Processed :{0}", myNode.OuterXml);
                    }
                }
            }
            catch (Exception e)
            {
                LogException(myNode, e, "ProcessCoppelia");
            }
            //int newState = Global.AddState(bState);
            //bot.servitor.CoppeliaStateDictionary[cState] = newState;
            yield return rs;
            yield break;
        }
        public IEnumerable<RunStatus> ProcessCoppeliaActionMoralBelief(BTXmlNode myNode)
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
                if (!string.IsNullOrEmpty(myNode.AttributesV("action"))) cAct = myNode.AttributesV("action");
                if (!string.IsNullOrEmpty(myNode.AttributesV("moral"))) cMoral = myNode.AttributesV("moral");
                if (!string.IsNullOrEmpty(myNode.AttributesV("agent"))) cAgent = myNode.AttributesV("agent");
                if (!string.IsNullOrEmpty(myNode.AttributesV("value"))) cValue = myNode.AttributesV("value");
                fValue = float.Parse(cValue);

                if (contextBot.servitor.CoppeliaMoralsDictionary.ContainsKey(cMoral))
                {
                    if (contextBot.servitor.CoppeliaAgentDictionary.ContainsKey(cAgent))
                    {
                        if (contextBot.servitor.CoppeliaActionDictionary.ContainsKey(cAct))
                        {
                            AgentAction act = contextBot.servitor.CoppeliaActionDictionary[cAct];
                            Agent a1 = contextBot.servitor.CoppeliaAgentDictionary[cAgent];
                            int moral = contextBot.servitor.CoppeliaMoralsDictionary[cMoral];
                            a1.SetActionMoralPrincipleBelief(act.GlobalIndex, moral, fValue);
                            //Console.WriteLine("coppelia Processed :{0}", myNode.OuterXml);
                        }
                    }
                }
            }
            catch (Exception e)
            {
                LogException(myNode, e, "ProcessCoppelia");
            }
            //int newState = Global.AddState(bState);
            //bot.servitor.CoppeliaStateDictionary[cState] = newState;
            yield return rs;
            yield break;
        }
        public IEnumerable<RunStatus> ProcessCoppeliaActionState(BTXmlNode myNode)
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
                if (!string.IsNullOrEmpty(myNode.AttributesV("action"))) cAct = myNode.AttributesV("action");
                if (!string.IsNullOrEmpty(myNode.AttributesV("state"))) cState = myNode.AttributesV("state");
                if (!string.IsNullOrEmpty(myNode.AttributesV("agent"))) cAgent = myNode.AttributesV("agent");
                if (!string.IsNullOrEmpty(myNode.AttributesV("value"))) cValue = myNode.AttributesV("value");
                fValue = float.Parse(cValue);

                if (contextBot.servitor.CoppeliaStateDictionary.ContainsKey(cState))
                {
                    if (contextBot.servitor.CoppeliaAgentDictionary.ContainsKey(cAgent))
                    {
                        if (contextBot.servitor.CoppeliaActionDictionary.ContainsKey(cAct))
                        {
                            AgentAction act = contextBot.servitor.CoppeliaActionDictionary[cAct];
                            Agent a1 = contextBot.servitor.CoppeliaAgentDictionary[cAgent];
                            int state = contextBot.servitor.CoppeliaStateDictionary[cState];
                            a1.SetActionStateBelief(act.GlobalIndex, state, fValue);
                            //Console.WriteLine("coppelia Processed :{0}", myNode.OuterXml);
                        }
                    }
                }
            }
            catch (Exception e)
            {
                LogException(myNode, e, "ProcessCoppelia");
            }            //int newState = Global.AddState(bState);
            //bot.servitor.CoppeliaStateDictionary[cState] = newState;
            yield return rs;
            yield break;
        }

        public IEnumerable<RunStatus> ProcessCoppeliaStateState(BTXmlNode myNode)
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
                if (!string.IsNullOrEmpty(myNode.AttributesV("statesrc"))) cStateSrc = myNode.AttributesV("state");
                if (!string.IsNullOrEmpty(myNode.AttributesV("statedest"))) cStateDest = myNode.AttributesV("state");
                if (!string.IsNullOrEmpty(myNode.AttributesV("agent"))) cAgent = myNode.AttributesV("agent");
                if (!string.IsNullOrEmpty(myNode.AttributesV("value"))) cValue = myNode.AttributesV("value");
                fValue = float.Parse(cValue);

                if (contextBot.servitor.CoppeliaStateDictionary.ContainsKey(cStateSrc))
                {
                    if (contextBot.servitor.CoppeliaAgentDictionary.ContainsKey(cAgent))
                    {
                        if (contextBot.servitor.CoppeliaStateDictionary.ContainsKey(cStateDest))
                        {
                            Agent a1 = contextBot.servitor.CoppeliaAgentDictionary[cAgent];
                            int stateSrc = contextBot.servitor.CoppeliaStateDictionary[cStateSrc];
                            int stateDest = contextBot.servitor.CoppeliaStateDictionary[cStateDest];
                            a1.SetStateFacStateBelief(stateSrc, stateDest, fValue);
                            //Console.WriteLine("coppelia Processed :{0}", myNode.OuterXml);
                        }
                    }
                }
            }
            catch (Exception e)
            {
                LogException(myNode, e, "ProcessCoppelia");
            }
            //int newState = Global.AddState(bState);
            //bot.servitor.CoppeliaStateDictionary[cState] = newState;
            yield return rs;
            yield break;
        }

        public IEnumerable<RunStatus> ProcessCoppeliaPerform(BTXmlNode myNode)
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
                if (!string.IsNullOrEmpty(myNode.AttributesV("action"))) cAct = myNode.AttributesV("action");
                if (!string.IsNullOrEmpty(myNode.AttributesV("agent"))) cAgent = myNode.AttributesV("agent");
                if (!string.IsNullOrEmpty(myNode.AttributesV("reciptient"))) cRecipient = myNode.AttributesV("reciptient");

                if (contextBot.servitor.CoppeliaActionDictionary.ContainsKey(cAct))
                {
                    if (contextBot.servitor.CoppeliaAgentDictionary.ContainsKey(cAgent))
                    {
                        if (contextBot.servitor.CoppeliaAgentDictionary.ContainsKey(cRecipient))
                        {
                            AgentAction act = contextBot.servitor.CoppeliaActionDictionary[cAct];
                            Agent agent = contextBot.servitor.CoppeliaAgentDictionary[cAgent];
                            Agent recipent = contextBot.servitor.CoppeliaAgentDictionary[cRecipient];
                            agent.ManualPerform(act, recipent);
                            //Console.WriteLine("coppelia Processed :{0}", myNode.OuterXml);
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
            catch (Exception e)
            {
                LogException(myNode, e, "ProcessCoppelia");
            }
            //int newState = Global.AddState(bState);
            //bot.servitor.CoppeliaStateDictionary[cState] = newState;
            yield return rs;
            yield break;
        }


        public IEnumerable<RunStatus> ProcessCoppeliaRelation(BTXmlNode myNode)
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
                if (!string.IsNullOrEmpty(myNode.AttributesV("relation"))) cRelation = myNode.AttributesV("relation");
                if (!string.IsNullOrEmpty(myNode.AttributesV("agent"))) cAgent = myNode.AttributesV("agent");
                if (!string.IsNullOrEmpty(myNode.AttributesV("reciptient"))) cRecipient = myNode.AttributesV("reciptient");
                if (!string.IsNullOrEmpty(myNode.AttributesV("value"))) cValue = myNode.AttributesV("value");
                fValue = float.Parse(cValue);
                int relationID = AgentRelations.Parse(cRelation);

                if (relationID >= 0)
                {
                    if (contextBot.servitor.CoppeliaAgentDictionary.ContainsKey(cAgent))
                    {
                        if (contextBot.servitor.CoppeliaAgentDictionary.ContainsKey(cRecipient))
                        {
                            Agent agent = contextBot.servitor.CoppeliaAgentDictionary[cAgent];
                            Agent recipent = contextBot.servitor.CoppeliaAgentDictionary[cRecipient];
                            agent.SetRelation(recipent.AgentID, relationID, fValue);
                            //Console.WriteLine("coppelia Processed :{0}", myNode.OuterXml);

                        }
                    }
                }

            }
            catch (Exception e)
            {
                LogException(myNode, e, "ProcessCoppelia");
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
        public IEnumerable<RunStatus> ProcessAssertCoppelia(BTXmlNode myNode)
        {
            string condition = myNode.AttributesV("cond");
            string parameters = myNode.InnerText;
            string condData = myNode.AttributesV("cond") == null ? null : myNode.AttributesV("cond");
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
            
            if (!string.IsNullOrEmpty(myNode.AttributesV("agent"))) cAgent = myNode.AttributesV("agent");
            if (!string.IsNullOrEmpty(myNode.AttributesV("relation"))) cRelation = myNode.AttributesV("relation");
            if (!string.IsNullOrEmpty(myNode.AttributesV("reciptient"))) cRecipient = myNode.AttributesV("reciptient");
            if (!string.IsNullOrEmpty(myNode.AttributesV("action"))) cAct = myNode.AttributesV("action");
            if (!string.IsNullOrEmpty(myNode.AttributesV("state"))) cState = myNode.AttributesV("state");
            if (!string.IsNullOrEmpty(myNode.AttributesV("feature"))) cFeature = myNode.AttributesV("feature");
            if (!string.IsNullOrEmpty(myNode.AttributesV("emotion"))) cEmotion = myNode.AttributesV("emotion");
            Agent a1 = null;
            Agent a2 = null;
            int iRelation = AgentRelations.Parse(cRelation);
            int iFeature = AgentFeatures.Parse(cFeature);
            int iEmotions = AgentRelations.Parse(cEmotion);

            if (contextBot.servitor.CoppeliaAgentDictionary.ContainsKey(cAgent))
            {
                a1 = contextBot.servitor.CoppeliaAgentDictionary[cAgent];
            }
            if (contextBot.servitor.CoppeliaAgentDictionary.ContainsKey(cRecipient))
            {
                a2 = contextBot.servitor.CoppeliaAgentDictionary[cRecipient];
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
            catch (FormatException e)
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
                sv = contextBot.BotBehaving.getBBHash(varName) ?? "0.0";
                if (!string.IsNullOrEmpty(sv)) bbVal = double.Parse(sv);
            }
            catch (FormatException e) { }

            if (varName == "angerat") bbVal = a1.GetAnger(a2.AgentID);
            if (varName == "emotion") bbVal = a1.GetEmotion(iEmotions);


            // Special variables?
            if (varName == "timeout") bbVal = elapsedTime;
            if (varName == "behaviorstackcount") bbVal = myLocalBehaviors.behaviorStack.Count;
            if (varName == "behaviorqueuecount") bbVal = myLocalBehaviors.eventQueue.Count;
            if (varName == "prob") bbVal = rgen.NextDouble();
            if (varName.Contains(".runtime"))
            {
                string tName = varName.Replace(".runtime", "");
                bbVal = myLocalBehaviors.timeRunning(tName);
            }
            if (varName.Contains(".lastrun"))
            {
                string tName = varName.Replace(".lastrun", "");
                bbVal = myLocalBehaviors.lastRunning(tName);
            }
            if (varName.Contains(".drive"))
            {
                string dName = varName.Replace(".drive", "");
                double halflife = 1000 * double.Parse(myNode.AttributesV("halflife"));
                int lastRun = myLocalBehaviors.lastRunning(dName);
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
                    valid = _bot.myActiveModel.IsTrue(sen);
                }
                catch
                {
                    valid = false;
                }
            }
            //Console.WriteLine("coppelia Processed :{0}", myNode.OuterXml);

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
        public IEnumerable<RunStatus> ProcessUpdatePersona(BTXmlNode myNode)
        {
            // Append some si_text 
            RunStatus rs = RunStatus.Success;
            List<Dictionary<string, string>> bingingsList = new List<Dictionary<string, string>>();
            string innerStr = myNode.InnerXml.Trim();
            string mtName = "audioSourceMt";
            string angleQuery = "audioSourceAngle(ANG)";
            string faceQuery = "faceRect(avgx,LOC)";
            string faceMt = "faceTrackerMt";
            string angStr = "0.0";
            string locStr = "0.0";
            float angRot;
            // Try face first then last noise
            lock (contextBot.prologEngine)
            {
                contextBot.prologEngine.FindOrCreateKB(mtName);
                contextBot.prologEngine.FindOrCreateKB(faceMt);

                contextBot.prologEngine.askQuery(faceQuery, faceMt, out bingingsList);
                foreach (Dictionary<string, string> bindings in bingingsList)
                {
                    foreach (string key in bindings.Keys)
                    {
                        if (key == "LOC") locStr = bindings[key];
                    }
                }
                angRot = float.Parse(locStr);
                angRot = (angRot - 0.5f) * 60f;
                while (angRot < 0) angRot = angRot + 360;
                // Try last noise
                if ((locStr == "0.0") || (locStr == "0") || (rgen.NextDouble() < 0.5))
                {
                    contextBot.prologEngine.askQuery(angleQuery, mtName, out bingingsList);
                    foreach (Dictionary<string, string> bindings in bingingsList)
                    {
                        foreach (string key in bindings.Keys)
                        {
                            if (key == "ANG") angStr = bindings[key];
                        }
                    }
                    angRot = float.Parse(angStr) * 1.0f;
                    while (angRot < 0) angRot = angRot + 360;
                }
            }

            if (contextBot.personaProcessor != null)
            {
                string rotCommand = String.Format("face {0}", angRot);
                contextBot.personaProcessor(rotCommand);
                Console.WriteLine("UpdatePersona: LOC:{0} AUD:{1} CMD:{2}",locStr,angStr ,rotCommand);
            }
            yield return rs;
            yield break;
        }


        #region ChatMapperImport
        // <loadchatmaper path="chatmapper\example.xml"/>
        public IEnumerable<RunStatus> ProcessLoadChatMapper(BTXmlNode myNode)
        {
            // Append some si_text 
            RunStatus rs = RunStatus.Failure;
            string innerStr = myNode.InnerXml.Trim();
            string srcFile = @"chatmapper\Example.xml";

            try
            {
                if (!string.IsNullOrEmpty(myNode.AttributesV("path"))) srcFile = myNode.AttributesV("path");
                ChatToBTXML myTranslator = new ChatToBTXML();
                myTranslator.defineChatTreeFile(srcFile);
                string myCodes = myTranslator.btxmlCode;
                BTXmlDocument chatDoc = new BTXmlDocument();
                string destFile = srcFile.Replace(".xml", ".btxml");
                destFile = HostSystem.FileSystemPath(destFile);
                System.IO.File.WriteAllText(destFile, myCodes);
                chatDoc.LoadXml(myCodes);
                contextBot.loadAIMLFromXML(chatDoc, "vf:" + srcFile + DateTime.Now.ToString());

                rs = RunStatus.Success;
            }
            catch (Exception e)
            {
                LogException(myNode, e, "ProcessLoadChatMapper '{0}','{1}':{2}", srcFile, innerStr, EMsg(e));
                rs = RunStatus.Failure;
            }
            yield return rs;
            yield break;
        }


        public IEnumerable<RunStatus> ProcessAssertMenu(BTXmlNode myNode)
        {
            string condition = myNode.AttributesV("cond");
            string parameters = myNode.InnerText;
            //if no input exists doesn't exist then return failure
            if (contextBot.BotBehaving.chatInputQueue.Count == 0)
              {
                yield return RunStatus.Failure;
                yield break;
              }

            string userInput = contextBot.BotBehaving.chatInputQueue.Peek();
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
                LogException(myNode, e, "ProcessAssertGuest '{0}':{1}", condition, EMsg(e));
                r = RunStatus.Failure;
            }
            if (r == RunStatus.Success)
            {
                // if we match then consume the input
                contextBot.BotBehaving.chatInputQueue.Dequeue();
            }
            yield return r;
            yield break;
        }

        bool matchesWildSense(string sense, string queryWord)//, Request request)
        {
            // always clip off the first "*";
            if (sense.StartsWith("*")) sense = sense.Substring(1);
            if (sense.Length == 0) return false;
            //BCTX contextBot = request.bot;
            BCTX contextBot = this.contextBot;

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
                string val = this.contextBot.BotBehaving.lastBehaviorUser.Predicates.grabSetting(wnWord); ; //request.user.Predicates.grabSetting(wnWord);
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

        public bool AcceptsThread(NativeThread currentThread)
        {
            return true;
            if (OnlyThisThread != null) return currentThread == OnlyThisThread;
            if (contextBot.myServitor.IsServitorThread(currentThread))
            {
                return true;
            }
            Console.WriteLine("Thread " + currentThread + " taking over this " + name);
            OnlyThisThread = currentThread;
            return true;
        }
    }
    #endregion 
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
        public BTXmlDocument treeDoc;
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
            treeDoc = new BTXmlDocument();

        }

        public void defineChatTreeFile(string filename)
        {
            filename = HostSystem.FileSystemPath(filename);
            string chatXML = System.IO.File.ReadAllText(filename);
            btxmlCode = "";
            //btxmlCode += "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n";
            btxmlCode += "<aiml version=\"1.0\">\n";
            btxmlCode += " <state name=\"*\">\n";

            treeDoc.LoadXml(chatXML);

            foreach (XmlNode childNode in treeDoc.ChildNodes)
            {
                if (childNode.NodeType == XmlNodeType.Element)
                {
                    processNode((BTXmlNode)childNode);
                }
            }
            btxmlCode += " </state>\n";
            btxmlCode += "</aiml>\n";

        }

        public void processNode(BTXmlNode myNode)
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
                                processas = "sequence";
                                postcode += "   <waitForChatInput wait='" + inputwait + "'/>\n";
                            }
                        }
                        postcode += "   <" + choicetag + ">\n";
                        for (int childIndex = 0; childIndex < myNode.ChildNodes.Count; childIndex++)
                        {
                            XmlNode childNode = (XmlNode)myNode.ChildNodes[childIndex];
                            if (childNode.NodeType == XmlNodeType.Element)
                            {
                                processNode((BTXmlNode)childNode);
                            }
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
                        XmlNode childNode = (XmlNode)myNode.ChildNodes[childIndex];
                        if (childNode.NodeType == XmlNodeType.Element)
                        {
                            processNode((BTXmlNode)childNode);
                        }
                    }
                    break;
            }
        }

        public void processField(BTXmlNode myNode)
        {
            if (!inDialog) return;

            string type = "";
            string hint = "";
            string title = "";
            string value = "";
            try
            {
                if (!string.IsNullOrEmpty(myNode.AttributesV("type"))) type = myNode.AttributesV("type");
                if (!string.IsNullOrEmpty(myNode.AttributesV("hint"))) hint = myNode.AttributesV("hint");
            }
            catch (Exception e)
            {
                Console.WriteLine("ERROR: processField attributes");
            }

            for (int childIndex = 0; childIndex < myNode.ChildNodes.Count; childIndex++)
            {
                BTXmlNode childNode = (BTXmlNode)myNode.ChildNodes[childIndex];
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

        public void processLink(BTXmlNode myNode)
        {
            string OriginConvoID = "";
            string OriginDialogID = "";
            string DestinationConvoID = "";
            string DestinationDialogID = "";
            string IsConnector = "";

            try
            {
                OriginConvoID = myNode.AttributesV("OriginConvoID");
                OriginDialogID = myNode.AttributesV("OriginDialogID");
                DestinationConvoID = myNode.AttributesV("DestinationConvoID");
                DestinationDialogID = myNode.AttributesV("DestinationDialogID");
                IsConnector = myNode.AttributesV("IsConnector");
            }
            catch (Exception e)
            {
                Console.WriteLine("ERROR: processLink attributes");
            }
            string srckey = OriginConvoID + "_" + OriginDialogID;
            string destkey = DestinationConvoID + "_" + DestinationDialogID;

            postcode += "      <subbehavior id='" + destkey + "' />\n";

        }

        public void processDialogEntry(BTXmlNode myNode)
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
                ID = myNode.AttributesV("ID");
                ConversationID = myNode.AttributesV("ConversationID");
                IsRoot = myNode.AttributesV("IsRoot");
                IsGroup = myNode.AttributesV("IsGroup");
                DelaySimStatus = myNode.AttributesV("DelaySimStatus");
                FalseCondtionAction = myNode.AttributesV("FalseCondtionAction");
                ConditionPriority = myNode.AttributesV("ConditionPriority");

            }
            catch (Exception e)
            {
                Console.WriteLine("ERROR: processDialogEntry attributes");
            }
            string bkey = ConversationID + "_" + ID;

            for (int childIndex = 0; childIndex < myNode.ChildNodes.Count; childIndex++)
            {
                BTXmlNode childNode = (BTXmlNode)myNode.ChildNodes[childIndex];
                processNode(childNode);
            }
            btxmlCode += "\n\n  <behavior processas='" + processas + "' id='" + bkey + "'  >\n";
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
