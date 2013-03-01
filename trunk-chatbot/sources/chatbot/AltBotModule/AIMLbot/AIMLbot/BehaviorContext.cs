#region

using System;
using System.Collections;
using System.Collections.Generic;
using System.Globalization;
using System.Text.RegularExpressions;
using System.Xml;
using AIMLbot;
using Aima.Core.Logic.Propositional.Algorithms;
using AltAIMLbot.AIMLTagHandlers;
using AltAIMLbot.Database;
using AltAIMLbot.Utils;
using AltAIMLbot.Variables;
using LAIR.ResourceAPIs.WordNet;
using LogicalParticleFilter1;
using MushDLR223.ScriptEngines;
using MushDLR223.Utilities;

#endregion

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
    sealed class BehaviorContext : AIMLTagHandler
    {
        public object RequestLock = new object();
        public TaskItem RunningItem;
        public List<TaskItem> RunningItems = new List<TaskItem>();
        public List<TaskItem> RanItems = new List<TaskItem>();
        private Queue<string> _chatInputQueue = new Queue<string>();
        private string _lastBehaviorChatInput = "";

        private string _lastBehaviorChatOutput = "";
        public Queue<string> _outputQueue = new Queue<string>();

        #region Conversation methods

        private readonly Regex SentRegex = new Regex(@"(\S.+?[.!?,\)])(?=\s+|$)");

        protected override AltBot mbot
        {
            get
            {
                //lock (SyncQueue)
                {
                    return base.bot;
                }
            }
        }

        internal TagHandlerProcessor TagHandling
        {
            get { return mbot.TagHandling; }
        }

        public MBrain STM_Brain
        {
            get { return mbot.STM_Brain; }
        }

        public MBrain MBrain
        {
            get { return mbot.MBrain; }
        }

        public BehaviorContext BotBehaving
        {
            get { return this; }
        }

        internal bool blockCron
        {
            get { return mbot._blockCron; }
            set { mbot._blockCron = value; }
        }

        internal bool saySapi
        {
            get { return mbot.saySapi; }
            set { mbot.saySapi = value; }
        }

        internal bool useMemcache
        {
            get { return mbot.useMemcache; }
            set { mbot.useMemcache = value; }
        }

        internal SIProlog prologEngine
        {
            get { return mbot.prologEngine; }
        }

        internal string OutputQueueString
        {
            get
            {
                lock (ChatQueueLock)
                {
                    Queue<string> outputQueue = outputQueue0;
                    if (outputQueue.Count == 0) return "<!--emtpy outputQueue-->";
                    string outmsg = "";
                    foreach (string msg in outputQueue)
                    {
                        outmsg += msg + "\r\n";
                    }
                    return outmsg.Trim();
                }
            }
        }

        public override string ToString()
        {
            return "BehaviorContext:" + ((object) _user ?? "NULL");
        }

        protected override Unifiable ProcessChangeU()
        {
            return OuterSource();
        }

        internal void processOutputQueue()
        {
            lock (ChatQueueLock)
            {
                Queue<string> outputQueue = outputQueue0;
                if (!isPerformingOutput)
                {
                    if (outputQueue.Count > 0)
                    {
                        writeToLogWarn("WARN: BOT OUTPUT ABOUT TO GO MISSING!:" + OutputQueueString);
                    }
                    return;
                }

                while (outputQueue.Count > 0)
                {
                    string msg = outputQueue.Dequeue();
                    SayMessageOutsideQueue(msg);
                }
            }
        }

        public void SayMessageOutsideQueue(string msg)
        {
            lock (ChatQueueLock)
            {
                if (sayProcessor != null)
                {
                    sayProcessor(msg);
                }
                else
                {
                    writeToLogWarn("Missing sayProcessor! BOT OUTPUT:{0}", msg);
                }
                myBehaviors.logText("BOT OUTPUT:" + msg);
            }
        }

        internal void flushOutputQueue()
        {
            lock (ChatQueueLock)
            {
                int fc = 0;
                Queue<string> outputQueue = outputQueue0;
                if (outputQueue.Count > 0)
                {
                    while (outputQueue.Count > 0)
                    {
                        fc++;
                        string msg = outputQueue.Dequeue();
                        msg = string.Format("flushOutputQueue BOT OUTPUT: {0}", msg);
                        Console.WriteLine(msg);
                        myBehaviors.logText("BOT OUTPUT: " + msg);
                    }
                }

                if (outputQueue.Count > 0)
                {
                    writeToLogWarn("The queue was supposed to be empty here!");
                    outputQueue.Clear();
                }

                myBehaviors.logText("BOT flushedOutputQueue Count=" + fc);
                string flushsignal = mbot.GlobalSettings.grabSetting("flushsignal", false);
                if ((flushsignal != null) && (flushsignal.Length > 2))
                {
                    postOutput(flushsignal);
                }
            }
        }

        internal void postOutput(string msg)
        {
            lock (ChatQueueLock)
            {
                Queue<string> outputQueue = outputQueue0;
                if (string.IsNullOrEmpty(msg)) return;
                Action<string> postOutput0 = ((s) => { lock (ChatQueueLock) outputQueue.Enqueue(s); });
                if (msg.Length < 256)
                {
                    // just post output
                    postOutput0(msg);
                    myBehaviors.logText("BOT postOutput:" + msg);
                }
                else
                {
                    //http://stackoverflow.com/questions/1936388/what-is-a-regular-expression-for-parsing-out-individual-sentences
                    //  a quick splitter better than just using '.'
                    //Regex Sentrx = new Regex(@"(\S.+?[.!?])(?=\s+|$)");
                    foreach (Match match in SentRegex.Matches(msg))
                    {
                        int i = match.Index;
                        string s = match.Value;
                        postOutput0(msg);
                        myBehaviors.logText("BOT postOutput:" + s);
                    }
                }
            }
        }

        internal void sendOutput(string msg)
        {
            lock (ChatQueueLock)
            {
                Queue<string> outputQueue = outputQueue0;
                // posts and processes
                if (msg.Length < 1024)
                {
                    // just post output
                    outputQueue.Enqueue(msg);
                    myBehaviors.logText("BOT sendOutput:" + msg);
                }
                else
                {
                    string[] sents = msg.Split('.');
                    foreach (string s in sents)
                    {
                        outputQueue.Enqueue(s);
                        myBehaviors.logText("BOT sendOutput:" + s);
                    }
                }
                processOutputQueue();
            }
        }

        internal string getPendingOutput()
        {
            lock (ChatQueueLock)
            {
                Queue<string> outputQueue = outputQueue0;
                string outmsg = "";
                while (outputQueue.Count > 0)
                {
                    string msg = outputQueue.Dequeue();
                    outmsg += msg + "\r\n";
                }
                return outmsg;
            }
        }

        #endregion

        internal BehaviorContext(AltBot mbot, User user)
        {
            base.bot = mbot;
            ReadOnly = true;
            _user = user;
            if (_user != null)
            {
                _user.BehaviorContext = this;
            }
        }

        internal User _user
        {
            get { return user; }
            set
            {
                user = value;
                IsTraced = true;
            }
        }

        public override SubQuery query
        {
            get
            {
                SubQuery q = base.query;
                if (q != null) return q;
                if (request != null) return request.CurrentQuery;
                if (result != null) return result.CurrentQuery;
                return null;
            }
            set { base.query = value; }
        }

        public override Request request
        {
            get
            {
                Request r = base.request;
                if (r != null) return r;
                r = user.LastRequest;
                if (r != null) return r;
                return null;
            }
            set { base.request = value; }
        }

        public string lastBehaviorChatInput
        {
            get { return _lastBehaviorChatInput; }
            set
            {
                if (value == _lastBehaviorChatInput)
                {
                    RedundantSet();
                    return;
                }
                if (string.IsNullOrEmpty(value))
                {
                    ClearLastInput(false);
                    return;
                }
                if (!string.IsNullOrEmpty(_lastBehaviorChatInput))
                {
                    ClearLastInput(false);
                }
                _lastBehaviorChatInput = value;
            }
        }

        private void RedundantSet()
        {
           
        }

        internal BehaviorSet myBehaviors
        {
            get { return mbot.myBehaviors; }
        }

        internal BehaviorSet myLocalBehaviors
        {
            get { return mbot.myBehaviors; }
        }

        public sayProcessorDelegate sayProcessor
        {
            get { return mbot._sayProcessor; }
            set { mbot._sayProcessor = value; }
        }

        internal User lastBehaviorUser
        {
            get { return mbot.LastUser; }
            set { mbot.LastUser = value; }
        }


        internal GraphMaster Graphmaster
        {
            get { return mbot.Graphmaster; }
        }

        internal MasterUser LastUser
        {
            get { return (MasterUser) lastBehaviorUser; }
            set { lastBehaviorUser = value; }
        }

        public systemPersonaDelegate personaProcessor
        {
            get { return mbot._personaProcessor; }
            set { mbot._personaProcessor = value; }
        }


        /// <summary>
        ///   Flag to show if the mbot is producing output
        /// </summary>
        public bool isPerformingOutput
        {
            get { lock (ChatQueueLock) return mbot.isPerformingOutput; }
            set
            {
                lock (ChatQueueLock)
                {
                    Queue<string> outputQueue = outputQueue0;
                    if (value == false && outputQueue.Count > 0)
                    {
                        if (mbot.isPerformingOutput)
                        {
                            AltBot.writeToLogWarn("ERROR Was preformingOUTUT! ");
                        }
                        processOutputQueue();
                    }
                    mbot.isPerformingOutput = value;
                }
            }
        }

        //public AltBot mbot { get; internal set; }

        internal Servitor servitor
        {
            get { return mbot.servitor; }
        }

        internal User BotAsUser
        {
            get { return mbot.BotAsUser; }
        }

        internal Servitor myServitor
        {
            get { return mbot.myServitor; }
            set { mbot.myServitor = value; }
        }

        internal bool inCritical
        {
            get { return mbot.inCritical; }
            set { mbot.inCritical = value; }
        }

        internal SettingsDictionaryReal GlobalSettings
        {
            get { return mbot.GlobalSettings; }
        }

        internal bool isAcceptingUserInput_nr
        {
            get { return mbot.isAcceptingUserInput; }
            set { mbot.isAcceptingUserInput = value; }
        }

        internal EasyLogger Logger
        {
            get { return mbot.Logger; }
        }

        internal bool bbSafe
        {
            get { return mbot.bbSafe; }
            set { mbot.bbSafe = value; }
        }

        internal OutputDelegate outputDelegate
        {
            get { return mbot.outputDelegate; }
            set { mbot.outputDelegate = value; }
        }

        /*internal bool IsTraced
        {
            get { return mbot.inCritical; }
            set { mbot.inCritical = value; }
        }*/

        internal Model myActiveModel
        {
            get { return mbot._myActiveModel; }
            set { mbot._myActiveModel = value; }
        }

        internal Model myModel
        {
            get { return mbot.myModel; }
            set { mbot.myModel = value; }
        }

        internal string myPositiveSATModleString
        {
            get { return mbot.myPositiveSATModleString; }
            set { mbot.myPositiveSATModleString = value; }
        }

        internal object myChemistry
        {
            get { return mbot.myChemistry; }
        }

        public CultureInfo Locale
        {
            get { return mbot.Locale; }
        }

        public Qchem realChem
        {
            get { return mbot.realChem; }
        }

        public Cron myCron
        {
            get { return mbot.myCron; }
        }

        public Stack<string> conversationStack
        {
            get { return mbot.conversationStack; }
        }

        public PhoneticHmm pHMM
        {
            get { return mbot.pHMM; }
        }

        public CycDatabase TheCyc
        {
            get { return mbot.TheCyc; }
        }

        public Hashtable wordAttributeHash
        {
            get { return mbot.wordAttributeHash; }
        }

        public bool chatTrace
        {
            get { return mbot.chatTrace; }
            set { throw new NotImplementedException(); }
        }

        public WordNetEngine wordNetEngine
        {
            get { return mbot.wordNetEngine; }
        }

        public actMSM pMSM
        {
            get { return mbot.pMSM; }
        }

        public string lastBehaviorChatOutput
        {
            get { lock (ChatQueueLock) return _lastBehaviorChatOutput; }
            set
            {
                if (_lastBehaviorChatOutput == value)
                {
                    RedundantSet();
                    return;
                }
                if (string.IsNullOrEmpty(value))
                {
                    ClearLastOutput(false);
                    return;
                }
                if (!string.IsNullOrEmpty(_lastBehaviorChatOutput))
                {
                    ClearLastOutput(false);
                }
                lock (ChatQueueLock) _lastBehaviorChatOutput = value;
            }
        }

        public Queue<string> chatInputQueue
        {
            get { lock (ChatQueueLock) return _chatInputQueue; }
            set { lock (ChatQueueLock) _chatInputQueue = value; }
        }

        private Queue<string> outputQueue0
        {
            get { lock (ChatQueueLock) return _outputQueue; }
            set { lock (ChatQueueLock) _outputQueue = value; }
        }

        public int outputQueueCount
        {
            get
            {
                lock (ChatQueueLock)
                {
                    return outputQueue0.Count;
                }
            }
        }

        public decimal chatInputQueueCount
        {
            get
            {
                lock (ChatQueueLock)
                {
                    return chatInputQueue.Count;
                }
            }
        }

        internal object ChatQueueLock
        {
            get { return RequestLock; }
        }

        public AltBot curBot
        {
            get { return mbot; }
        }


        public static implicit operator AltBot(BehaviorContext bhu)
        {
            return bhu.mbot;
        }

        public static explicit operator BehaviorContext(AltBot b)
        {
            return b.BotBehaving;
        }

        internal void evalTemplateNodeInnerXml(XmlNode myNode, RequestKind requestKind)
        {
            mbot.evalTemplateNodeInnerXml(myNode, requestKind, this);
        }

        internal Result Chat(Request request)
        {
            result = mbot.Chat(request);
            return result;
        }

        internal Result Chat(Request request, string graphName)
        {
            result = mbot.Chat(request, graphName);
            return result;
        }

        internal string getBBHash(string varName)
        {
            return mbot.getBBHash(varName, this);
        }

        internal ISettingsDictionary GetDictionary(string p)
        {
            return mbot.GetDictionary(p);
        }

        internal void updateRTP2Sevitor(User curUser)
        {
            mbot.updateRTP2Sevitor(curUser);
            mbot.LastUser = curUser;
        }

        internal void performBehaviors()
        {
            mbot.performBehaviors();
        }

        internal void setBBHash(string key, string data)
        {
            mbot.setBBHash(key, data, this);
        }

        internal void setBBHash0(string key, string p1)
        {
            mbot.setBBHash0(key, p1, this);
        }

        internal User FindOrCreateUser(string userId)
        {
            return mbot.FindOrCreateUser(userId);
        }

        internal void loadAIMLFromXML(XmlNode newAiml, string toString)
        {
            mbot.loadAIMLFromXML(newAiml, toString);
        }

        internal void loadAIMLFromFiles(string s)
        {
            mbot.loadAIMLFromFiles(s);
        }

        internal void importBBBot()
        {
            mbot.WithBBBot(mbot.importBBBotSettings);
        }

        internal void exportBBBot()
        {
            mbot.WithBBBot(mbot.exportBBBotSettings);
        }

        internal void importBBUser(MasterUser curUser)
        {
            mbot.WithBBUser(curUser, mbot.importBBUserSettings);
        }

        internal void exportBBUser(MasterUser curUser)
        {
            mbot.WithBBUser(curUser, mbot.exportBBUserSettings);
        }

        internal bool IsLastKnownUser(string value)
        {
            return mbot.IsLastKnownUser(value);
        }

        internal void defineBehavior(string myName, string templateNodeTotalValue)
        {
            mbot.defineBehavior(myName, templateNodeTotalValue);
        }

        internal void defineFSM(string myName, string templateNodeTotalValue)
        {
            mbot.defineFSM(myName, templateNodeTotalValue);
        }

        internal void Release()
        {
            throw new NotImplementedException();
        }

        internal void updateServitor2RTP(User myUser)
        {
            mbot.LastUser = myUser;
        }

        internal string getBBHash0(string bbKey)
        {
            return mbot.getBBHash0(bbKey, this);
        }

        public object evalTemplateNode(XmlNode templateNode0, RequestKind stateMachineProcess)
        {
            return mbot.evalTemplateNode(templateNode0, stateMachineProcess, this);
        }

        public void logText(string m)
        {
            mbot.logText(m);
        }

        public GraphMaster GetGraph(string graphName, GraphMaster current)
        {
            return mbot.GetGraph(graphName, current);
        }

        internal void writeToLog(Exception e)
        {
            mbot.writeToLog(e);
        }

        public void TraceTest(string s, Action func)
        {
            mbot.TraceTest(s, func);
        }

        public Exception RaiseError(Exception e)
        {
            Exception err = mbot.RaiseError(e);
            if (ChatOptions.WarningsAsErrors && ChatOptions.AllowRuntimeErrors)
            {
                throw err;
            }
            return e;
        }

        public string GetUserMt(User user1, SubQuery subQuery)
        {
            return mbot.GetUserMt(user1, subQuery);
        }

        internal IEnumerable<XmlNode> EvalAiml(XmlNode templateNode, Request req, OutputDelegate outputDelegate)
        {
            return mbot.EvalAiml(templateNode, req, outputDelegate);
        }

        public Unifiable SystemExecute(Unifiable cmd, string lang, Request req)
        {
            return mbot.SystemExecute(cmd, lang, req);
        }

        public Result ChatWithRequest(Request subRequest)
        {
            return mbot.ChatWR(subRequest);
        }

        internal void AddAiml(GraphMaster myGraph, string evidenceCode)
        {
            mbot.AddAiml(myGraph, evidenceCode);
        }

        internal void RaiseError(string f, params object[] a)
        {
            mbot.RaiseError(f, a);
        }


        public void writeToUserLog(string f, params object[] a)
        {
            mbot.writeToUserLog(f, a);
        }

        internal void SetInputClearingOutput(string eventName, string input, User curUser)
        {
            lock (ChatQueueLock)
            {
                BehaviorContext curBot = this;
                //curBot.lastBehaviorChatInput = input;
                curBot.isPerformingOutput = false;
                curBot.myBehaviors.logText(eventName + " USER INPUT:" + input);
                curBot.chatInputQueue.Clear();
                curBot.chatInputQueue.Enqueue(input);
                curBot.lastBehaviorUser = curUser;
                //curBot.myBehaviors.runBotBehavior(eventName, curBot);
                curBot.flushOutputQueue();

                //curBot.myBehaviors.queueEvent(eventName);
                //curBot.processOutputQueue();

                curBot.lastBehaviorChatOutput = "";
            }
        }

        public string GetLastBehaviorChatOutput()
        {
            lock (ChatQueueLock)
            {
                string lo = lastBehaviorChatOutput;
                if (!string.IsNullOrEmpty(lo))
                {
                    return lo;
                }
                writeToLogWarn("returning empty lastBehaviorChatOutput");
                return "";
            }
        }

        public string chatInputQueuePeek()
        {
            lock (ChatQueueLock)
            {
                return chatInputQueue.Peek();
            }
        }

        internal void chatInputQueueDequeue(string expected)
        {
            lock (ChatQueueLock)
            {
                bool didIt = false;
                if (chatInputQueueCount > 0)
                {
                    if (chatInputQueue.Peek() == expected)
                    {
                        didIt = true;
                        chatInputQueue.Dequeue();
                    }
                }
                if (lastBehaviorChatInput == expected)
                {
                    didIt = true;
                    lastBehaviorChatInput = "";
                }
                if (!didIt)
                {
                    writeToLogWarn("cant dequeue: " + expected);
                }
            }
        }

        public string lastBehaviorChatInputPeek()
        {
            lock (ChatQueueLock)
            {
                if (chatInputQueueCount > 0)
                {
                    return chatInputQueuePeek();
                }
                else
                {
                    return lastBehaviorChatInput;
                }
            }
        }

        public void ClearLastOutput(bool vetted)
        {
            lock (ChatQueueLock)
            {
                string lo = lastBehaviorChatOutput;
                if (!string.IsNullOrEmpty(lo))
                {
                    if (!vetted) writeToLogWarn("Loosing lastBehaviorChatOutput=" + lo);
                    _lastBehaviorChatOutput = "";
                }
            }
        }

        public void ClearLastInput(bool vetted)
        {
            lock (ChatQueueLock)
            {
                string lo = lastBehaviorChatInput;
                if (!string.IsNullOrEmpty(lo))
                {
                    if (!vetted) writeToLogWarn("Loosing lastBehaviorChatInput=" + lo);
                    _lastBehaviorChatInput = "";
                }
            }
        }

        public void ClearLastInputOutput(bool vetted)
        {
            lock (ChatQueueLock)
            {
                ClearLastInput(true);
                ClearLastOutput(vetted);
            }
        }

        internal string lastBehaviorChatInputDeque()
        {
            lock (ChatQueueLock)
            {
                if (chatInputQueueCount > 0)
                {
                    string was = chatInputQueuePeek();
                    if (!string.IsNullOrEmpty(lastBehaviorChatInput) && was != lastBehaviorChatInput)
                    {
                        writeToLogWarn("not seeing lastBehaviorChatInput=" + lastBehaviorChatInput + " instead seeing " +
                                       was);
                        lastBehaviorChatInput = was;
                        return was;
                    }
                }
                if (!string.IsNullOrEmpty(lastBehaviorChatInput))
                {
                    writeToLogWarn("not seeing lastBehaviorChatInput=" + lastBehaviorChatInput +
                                   " instead returning Empty");
                    //return lastBehaviorChatInput;
                }
                return "";
            }
        }

        internal void SetCurrentTask(TaskItem ti, TaskList list, bool wasActiveTaskList)
        {
            if (!ChatOptions.ServitortUserSwitchingLock) return;
            bool match = (RunningItem != ti);
            if (ChatOptions.DebugMicrothreader > 1) Console.WriteLine("CT-RUN: " + ti + " MATCH=" + match + " active=" + wasActiveTaskList);
            RanItems.Remove(ti);
            RunningItems.Remove(ti);
            RunningItems.Insert(0, ti);
            RunningItem = ti;
        }

        internal void AddCurrentTask(TaskItem ti, TaskList list, bool wasActiveTaskList)
        {
            if (!ChatOptions.ServitortUserSwitchingLock) return;
            bool match = (RunningItem != ti);
            if (ChatOptions.DebugMicrothreader > 3) Console.WriteLine("CT-ADD: " + ti + " MATCH=" + match + " active=" + wasActiveTaskList);
            RanItems.Remove(ti);
            RunningItems.Remove(ti);
            RunningItems.Insert(0, ti);
            if (!match && RunningItem == null) RunningItem = ti;
        }

        public void RemoveCurrentTask(TaskItem ti, TaskList list, bool wasActiveTaskList)
        {
            if (!ChatOptions.ServitortUserSwitchingLock) return;
            bool match = (RunningItem != ti);
            if (ChatOptions.DebugMicrothreader > 2) Console.WriteLine("CT-REMOVE: " + ti + " MATCH=" + match + " active=" + wasActiveTaskList);
            RunningItems.Remove(ti);
            RanItems.Remove(ti);
            RanItems.Insert(0, ti);
            if (match) RunningItem = null;
        }
    }
}