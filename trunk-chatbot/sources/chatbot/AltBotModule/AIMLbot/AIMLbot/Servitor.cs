using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.IO;
using AIMLbot;
using AltAIMLParser;
using DcBus;
using System.Runtime.Serialization.Formatters.Binary;

using System.Threading;
using Aima.Core.Logic.Propositional.Algorithms;
using Aima.Core.Logic.Propositional.Parsing;
using Aima.Core.Logic.Propositional.Parsing.AST;
using LAIR.ResourceAPIs.WordNet;
using RTParser;
using VDS.RDF.Parsing;
using LogicalParticleFilter1;
using CAMeRAVUEmotion;

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
    public class Servitor
    {
        // Connects a AltBot with some world, real or virtual
        // Creates multiple threads so it can have an independent existance

        // To be used with AltBot
        // see 
        //   - Bot.cs AltBot(),
        //   - BotConsole.cs Prepare(), 
        //   - WorldObjectsForAimLBot.cs StartupListener00()

        public  AltBot curBot;

        public bool NeedsLoad = true;
        public User curUser
        {
            get { return curBot.LastUser; }
        }
        public  Thread tmTalkThread = null;
        public bool tmTalkEnabled = true;
        public  Thread tmFSMThread = null;
        public bool tmFSMEnabled = true;
        public  Thread tmBehaveThread = null;
        public bool tmBehaveEnabled = true;

        public  Thread myCronThread = null;
        public  string lastAIMLInstance = "";
        public bool traceServitor = true;
        public bool skiploading = false;
        public void DontSkiploading(System.Action act)
        {
            var sl = skiploading;
            skiploading = false;
            try
            {
                act();
            }
            finally
            {
                skiploading = sl;
            }
        }

        public bool savedServitor = false;
        public bool skipPersonalityCheck = false;
        public bool initialCritical = false;
        public Scheduler myScheduler = null;
        public InvertedIndex myIndex = null;

        public string _rapStoreDirectory;
        public int _rapStoreSlices;
        public int _rapStoreTrunkLevel;
        public static Servitor LastServitor;

        [NonSerialized]
        public SIProlog prologEngine = new SIProlog();
        [NonSerialized]
        public Dictionary<string, SymbolicParticleFilter> partFilterDict = new Dictionary<string, SymbolicParticleFilter>();
        //[NonSerialized]
        //public SymbolicParticleFilter partFilter = new SymbolicParticleFilter();

        [NonSerialized]
        HumanAgent a2;
        [NonSerialized]
        Agent a1;

        [NonSerialized]
        public Dictionary<string, Agent> CoppeliaAgentDictionary = new Dictionary<string, Agent>();
        public Dictionary<string, AgentAction> CoppeliaActionDictionary = new Dictionary<string, AgentAction>();
        public Dictionary<string, int> CoppeliaStateDictionary = new Dictionary<string, int>();
        public Dictionary<string, int> CoppeliaMoralsDictionary = new Dictionary<string, int>();

        public string rapStoreDirectory
        {
            get
            {
                if (curBot != null) return curBot.rapStoreDirectory;                
                return _rapStoreDirectory;
            }
            set
            {
                if (curBot != null)
                {
                    curBot.rapStoreDirectory = value;
                }
                _rapStoreDirectory = value;
            }
        }
        public int rapStoreSlices
        {
            get { return _rapStoreSlices; }
            set { _rapStoreSlices = value; }
        }
        public int rapStoreTrunkLevel
        {
            get { return _rapStoreTrunkLevel; }
            set { _rapStoreTrunkLevel = value; }
        }

        public Servitor(AltBot aimlBot, sayProcessorDelegate outputDelegate)
        {
            curBot = aimlBot;
            curBot.myServitor = this;
            Start( outputDelegate);
        }
        public Servitor(AltBot aimlBot, sayProcessorDelegate outputDelegate, bool skipLoading, bool skippersonalitycheck, bool initialcritical)
        {
            curBot = aimlBot;
            curBot.myServitor = this;
            skiploading = skipLoading;
            skipPersonalityCheck = skippersonalitycheck;
            initialCritical = initialcritical;
            Start(outputDelegate);
        }

        public string GetCoppeliaAgentNameByID(int queryID)
        {
            foreach (string name in CoppeliaAgentDictionary.Keys)
            {
                if (CoppeliaAgentDictionary[name].AgentID == queryID)
                {
                    return name;
                }
            }
            return "unknown";
        }
        public void InitCoppelia()
        {
            //Create new agents
            a1 = new Agent(0.5f, 0.5f, 0.5f, 0.5f, 0.5f, 0.5f, 0.5f, 0.5f);
            a2 = new HumanAgent();
            CoppeliaAgentDictionary["self"] = a1;
            CoppeliaAgentDictionary["other"] = a2;
            CoppeliaAgentDictionary["human"] = a2;

            //Register the RequestingInput function (see below) as a function that needs to be called when Agent a2 (which is the HumanAgent) needs to respond to input
            //a2.input += new HumanAgent.InputRequest(RequestingInput);
            a2.input += new HumanAgent.InputRequest(RequestingInputFromMt);

            //Register the GlobalActionReceived function (see below) as a function that needs to be called whenever any action is performed by any agent.
            Global.actionBroadcast += new Global.ActionListener(GlobalActionReceived);
            //Register both agents with the model, so they will be updated
            //You'll most likely always need to do this for all agents
            CAMeRAVUEmotion.Model.RegisterAgent(a1);
            CAMeRAVUEmotion.Model.RegisterAgent(a2);

            //Register the agents with each other, so they know they exist
            //Agents will not target other agents with actions unless they know they exist
            a1.AddAgent(a2.AgentID);
            a2.AddAgent(a1.AgentID);

            // Need to 
            // - Creat actions with positivity/negativity +
            // - define the responses between actions +
            // - define the states +
            // - define ambition for each actor and state+
            // - defeine Action->State facilitation for each actor +
            // - define State->State facilitation for each actor
            // - define Actor features
            // - define manual actions


            //Start running the model
            //This will run the Model in a separate thread, until it is stopped
            //Stopping the model will in essense pause the simulation, so you can do this whenever necessary
            //The Model will pause when a HumanAgent needs to respond to input
            CAMeRAVUEmotion.Model.Start();
        }

        /// <summary>
        /// This function is called when the HumanAgent a1's input member is fired (which happens when it receives an action from another agent)
        /// </summary>
        /// <returns></returns>
        int RequestingInput()
        {
            //Some display so users know what's going on

            Console.WriteLine("Select Response");

            //display possible responses
            for (int i = 0; i < a2.PossibleResponses.Count; ++i)
            {
                Console.WriteLine("" + i + ": " + Global.GetActionByID(a2.PossibleResponses[i]).Name);
            }

            //Request input from users
            int num = -1;
            bool failedParse = false;
            do
            {
                failedParse = false;
                string input = Console.In.ReadLine();
                if (!int.TryParse(input, out num))
                {
                    failedParse = true;
                }
            } while (!failedParse && num < 0 || num >= a2.PossibleResponses.Count);

            int responseID = -1;

            //Check if the input of the user is valid
            if (num >= 0 && num < a2.PossibleResponses.Count)
                responseID = a2.PossibleResponses[num];

            //Return the selected response
            //-1 is an invalid ActionID and will constitute "no action"
            return responseID;
        }

        int RequestingInputFromMt()
        {
            int responseID = -1;
            for (int i = 0; i < a2.PossibleResponses.Count; ++i)
            {
                string query = String.Format("performed({0})", Global.GetActionByID(a2.PossibleResponses[i]).Name);
                if (this.prologEngine.isTrueIn(query, "coppeliaInputMt"))
                {
                    responseID = a2.PossibleResponses[i];
                }
                //Console.WriteLine("" + i + ": " + Global.GetActionByID(a2.PossibleResponses[i]).Name);
            }

            //Return the selected response
            //-1 is an invalid ActionID and will constitute "no action"
            if (responseID == -1)
            {
                // since we're looking an an MT we may want to give some time back
                // so someone else can post to it
                Thread.Sleep(1000);
            }
            else
            {
                Thread.Sleep(1000);
            }
            return responseID;
        }

        /// <summary>
        /// This function is called when any agent performs any action.
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="action"></param>
        /// <param name="target"></param>
        void GlobalActionReceived(int sender, int action, int target)
        {
            Console.WriteLine("Caught Action: Agent " + sender + " performed action " + Global.GetActionByID(action).Name + " on Agent " + target);

            //if the OK action is performed, update STATE_LOST_THE_GAME to true
           // if (action == OK.GlobalIndex)
           // {
           //     Console.WriteLine("Setting state STATE_LOST_THE_GAME to true");
           //     Global.SetState(STATE_LOST_THE_GAME, true);
           // }
            string mt = "coppeliaOutputMt";
            string actionReport = "";
            if (GetCoppeliaAgentNameByID(sender) == "self")
            {
                this.prologEngine.insertKB("", mt);
                this.prologEngine.insertKB("", "coppeliaInputMt");
                actionReport = String.Format("selfAct({0},{1}).", Global.GetActionByID(action).Name, GetCoppeliaAgentNameByID(target));
                this.prologEngine.appendKB(actionReport, mt);
            }
            actionReport = String.Format("performedAction({0},{1},{2}).", GetCoppeliaAgentNameByID(sender), Global.GetActionByID(action).Name, GetCoppeliaAgentNameByID(target));
            this.prologEngine.appendKB(actionReport, mt);

        }

        public void Start(sayProcessorDelegate outputDelegate)
        {
            Servitor.LastServitor = this;
            Console.WriteLine("RealBot operating in :" + Environment.CurrentDirectory);
            Console.WriteLine("       ProcessorCount:" + Environment.ProcessorCount);
            Console.WriteLine("             UserName:" + Environment.UserName);
            Console.WriteLine("            TickCount:" + Environment.TickCount);
            Console.WriteLine("            UserID:" + curUser.UserID);
            curBot.myServitor = this;
            myScheduler = myScheduler ?? new Scheduler(this);
            myIndex = myIndex ?? new InvertedIndex();
            rapStoreDirectory = ".//rapstore//";
            curBot.bbSafe = true;
            outputDelegate = outputDelegate ?? curBot.sayProcessor;
            if (outputDelegate == null)
            {
                curBot.sayProcessor = new sayProcessorDelegate(sayResponse);
                Console.WriteLine(" using default sayProcessorDelegate");
            }
            else
            {
                curBot.sayProcessor = outputDelegate;
                Console.WriteLine(" using external sayProcessorDelegate");
            }
            //Console.WriteLine("Servitor loadSettings");

            //curBot.loadSettings();

            //Console.WriteLine("Servitor User");
            //var myUser = new MasterUser(UserID, myBot);

            //curUser = myUser;
            //myBot.isAcceptingUserInput = false;
            curBot.inCritical = initialCritical;

            Console.WriteLine("Servitor startMtalkWatcher");
            startMtalkWatcher();
            Thread.Sleep(600);
            Console.WriteLine("Servitor checkNewPersonality");

            bool personDefined=false;
            if (!skipPersonalityCheck) personDefined= checkNewPersonality();

            lock (curBot)
            {
                if ((personDefined == false) && (lastAIMLInstance.Length == 0))
                {
                    //myBot.loadAIMLFromFiles();
                }
            }

            curBot.isAcceptingUserInput = true;

            Console.WriteLine("Servitor startFSMEngine");
            startFSMEngine();
            Console.WriteLine("Servitor startBehaviorEngine");
            startBehaviorEngine();
            Console.WriteLine("Servitor startCronEngine");
            startCronEngine();
            curBot.myBehaviors.keepTime("activation", RunStatus.Success);
            curBot.myBehaviors.activationTime("activation", RunStatus.Success);

            Console.WriteLine(" Servitor beginning Coppelia");
            InitCoppelia();
            Console.WriteLine(" Servitor startup complete");
        }
        public void loadComplete()
        {
            string servRoot = curBot.GlobalSettings.grabSetting("serverRoot", false);
            if ((servRoot != null) && (servRoot.Length > 7))
            {
                WebServitor.serverRoot = servRoot;
            }
            string servPort = curBot.GlobalSettings.grabSetting("serverPort", false);
            if (servPort != null)
            {
                try
                {
                    WebServitor.serverPort = int.Parse(servPort);
                }
                catch { }
            }

            Console.WriteLine("Servitor WebServitor.beginService");
            WebServitor.beginService(this);
            if ((myScheduler != null) && curBot.myBehaviors.definedBehavior("startup"))
            {
                myScheduler.ActivateBehaviorTask("startup");
            }
            curBot.isAcceptingUserInput = true;

        }
        public void initWordNet(string wordNetPath)
        {
            Console.WriteLine("*** Servitor init WN-Load ***");
            curBot.wordNetEngine = curBot.wordNetEngine ?? new WordNetEngine(wordNetPath, true);
        }
        public bool setGuestEvalObject(object guestObj)
        {
            if (curBot == null) return false;
            curBot.guestEvalObject = guestObj;
            return true;
        }

        public string respondToChat(string input)
        {
            return respondToChat(input, tmBehaveEnabled);
        }
        public string respondToChat(string input, bool doHaviours)
        {
            if (string.IsNullOrEmpty(input)) return input;
            input = input.TrimStart();
            if (input.StartsWith("@"))
            {
                curBot.AcceptInput(Console.WriteLine, input, curUser);
                return "@@";
            }
            if (input.StartsWith("<"))
            {
                curBot.myBehaviors.runBTXML(input);
                return "@<>";
            }
            curBot.isPerformingOutput = true;
            if (curBot.myBehaviors.waitingForChat)
            {
                Console.WriteLine(" ************ FOUND waitingForChat ************");
                curUser.Predicates.updateSetting("lastinput", input);
                prologEngine.postListPredToMt("lastinput", input, "lastinputMt");
                //curBot.lastBehaviorChatInput = input;
                curBot.myBehaviors.logText("waitingForChat USER INPUT:" + input);
                curBot.chatInputQueue.Clear();
                curBot.chatInputQueue.Enqueue(input);
                curBot.lastBehaviorUser = curUser;
                //curBot.myBehaviors.runEventHandler("onchat");
                curBot.flushOutputQueue();

                //curBot.myBehaviors.queueEvent("onchat");
                //curBot.processOutputQueue();

                curBot.lastBehaviorChatOutput = "";
                myScheduler.SleepAllTasks(30000);
                curBot.isPerformingOutput = true;
                return "";
            }
            // Try the event first
            if (doHaviours && curBot.myBehaviors.hasEventHandler("onchat"))
            {
                Console.WriteLine(" ************ FOUND ONCHAT ************");
                curUser.Predicates.updateSetting("lastinput", input);
                prologEngine.postListPredToMt("lastinput", input, "lastinputMt");
                //curBot.lastBehaviorChatInput = input;
                curBot.isPerformingOutput = false;
                curBot.myBehaviors.logText("ONCHAT USER INPUT:" + input);
                curBot.chatInputQueue.Clear ();
                curBot.chatInputQueue.Enqueue(input);
                curBot.lastBehaviorUser = curUser;
                //curBot.myBehaviors.runEventHandler("onchat");
                curBot.flushOutputQueue();

                //curBot.myBehaviors.queueEvent("onchat");
                //curBot.processOutputQueue();

                curBot.lastBehaviorChatOutput = "";
                myScheduler.SleepAllTasks(30000);
                myScheduler.EnqueueEvent("onchat");

                curBot.isPerformingOutput = true;
                curBot.myBehaviors.logText("ONCHAT IMMED RETURN:" + curBot.lastBehaviorChatOutput);
                prologEngine.postListPredToMt("lastoutput", curBot.lastBehaviorChatOutput, "lastoutputMt");
                return curBot.lastBehaviorChatOutput;
            }
            // else try the named behavior
            if (doHaviours && curBot.myBehaviors.definedBehavior("chatRoot"))
            {
                curUser.Predicates.updateSetting("lastinput", input);
                prologEngine.postListPredToMt("lastinput", input, "lastinputMt");
                //curBot.lastBehaviorChatInput = input;
                curBot.isPerformingOutput = false;
                curBot.myBehaviors.logText("CHATROOT USER INPUT:" + curBot.lastBehaviorChatOutput);
                curBot.chatInputQueue.Clear();
                curBot.chatInputQueue.Enqueue(input);
                curBot.lastBehaviorUser = curUser;
                //curBot.myBehaviors.runBotBehavior("chatRoot", curBot);
                curBot.flushOutputQueue();
                //curBot.myBehaviors.queueEvent("chatRoot");
                //curBot.processOutputQueue();
                curBot.lastBehaviorChatOutput = "";
                myScheduler.SleepAllTasks(30000);
                myScheduler.ActivateBehaviorTask("chatRoot");

                curBot.isPerformingOutput = true;
                curBot.myBehaviors.logText("CHATROOT IMMED RETURN:" + curBot.lastBehaviorChatOutput);
                prologEngine.postListPredToMt("lastoutput", curBot.lastBehaviorChatOutput, "lastoutputMt");
                return curBot.lastBehaviorChatOutput;
            }
            // else just do it (no other behavior is defined)
            try
            {
                    Request r = new Request(input, curUser, curBot);
                    Result res = curBot.Chat(r);
                    if (traceServitor)
                    {
                        Console.WriteLine("SERVITOR: respondToChat({0})={1}", input, res.Output);
                    }
                    curBot.isPerformingOutput = true;
                    return res.Output;
            }
            catch
            {
                curBot.isPerformingOutput = true;
                return "..."; 
            }

        }
        /*
        public string respondToChat(string input,string UserID)
        {
            try
            {
                prologEngine.postListPredToMt("lastinput", input, "lastinputMt");
                var u = new MasterUser(UserID, curBot);
                Request r = new Request(input, u, curBot);
                Result res = curBot.Chat(r);
                if (traceServitor)
                {
                    Console.WriteLine("SERVITOR: respondToChat({0},{2})={1}", input, res.Output, UserID);
                }
                prologEngine.postListPredToMt("lastoutput", res.Output, "lastoutputMt");
                return res.Output;
            }
            catch
            { return "..."; }

        }

        public void reactToChat(string input)
        {
            try
            {
                prologEngine.postListPredToMt("lastinput", input, "lastinputMt");
                Request r = new Request(input, curUser, curBot);
                Result res = curBot.Chat(r);
                if (traceServitor)
                {
                    Console.WriteLine("SERVITOR: reactToChat({0})={1}", input, res.Output);
                }
                sayResponse(res.Output);
                prologEngine.postListPredToMt("lastoutput", res.Output, "lastoutputMt");
                // Mark the output time
                curBot.myBehaviors.keepTime("lastchatoutput", RunStatus.Success);
                curBot.myBehaviors.activationTime("lastchatoutput", RunStatus.Success);
            }
            catch
            { }

        }
        public void reactToChat(string input, string UserID)
        {
            try
            {
                prologEngine.postListPredToMt("lastinput", input, "lastinputMt");
                var u = new MasterUser(UserID, curBot);
                Request r = new Request(input, u, curBot);
                Result res = curBot.Chat(r);
                if (traceServitor)
                {
                    Console.WriteLine("SERVITOR: reactToChat({0},{2})={1}", input, res.Output, UserID);
                }
                prologEngine.postListPredToMt("lastoutput", res.Output, "lastoutputMt");
                sayResponse(res.Output);
                // Mark the output time
                curBot.myBehaviors.keepTime("lastchatoutput", RunStatus.Success);
                curBot.myBehaviors.activationTime("lastchatoutput", RunStatus.Success);

            }
            catch
            { }

        }
        */
        public void Main(string[] args)
        {
            Start(new sayProcessorDelegate(sayResponse));

            while (true)
            {
                try
                {
                    Console.Write("You: ");
                    string input = Console.ReadLine();
                    if (input.ToLower() == "quit")
                    {
                        break;
                    }
                    else
                    {
                        prologEngine.postListPredToMt("lastinput", input, "lastinputMt");
                        string answer = respondToChat(input);
                        Console.WriteLine("Bot: " + answer);
                        sayResponse(answer);
                        prologEngine.postListPredToMt("lastoutput", answer, "lastoutputMt");
                    }
                }
                catch
                { }

            }
        }


        public  void startCronEngine()
        {
            try
            {
                if (IsRunning(myCronThread)) return; 
                if (myCronThread == null)
                {
                    myCronThread = new Thread(curBot.myCron.start);
                }
                myCronThread.Name = "cron";
                myCronThread.IsBackground = true;
                myCronThread.Start();
            }
            catch (Exception e)
            {
                Console.WriteLine("{0}\n{1}", e.Message, e.StackTrace);
            }
        }
        #region FSM
        public  void startFSMEngine()
        {
            if (IsRunning(tmFSMThread)) return;
            //Start our own chem thread
            try
            {
                if (tmFSMThread == null)
                {
                    tmFSMThread = new Thread(memFSMThread);
                }
                tmFSMThread.IsBackground = true;
                tmFSMThread.Start();
            }
            catch (Exception e)
            {
                Console.WriteLine("{0}\n{1}", e.Message, e.StackTrace);
            }

        }

        public void memFSMThread()
        {
            int interval = 1000;
            int tickrate = interval;
            while (true)
            {
                Thread.Sleep(interval);
                if (!tmFSMEnabled) continue;
                try
                {
                    if ((curBot.myFSMS != null) && (curBot.isAcceptingUserInput))
                    {
                        curBot.myFSMS.runBotMachines(curBot);
                    }
                    string tickrateStr = getBBHash("tickrate");
                    tickrate = interval;

                    tickrate = int.Parse(tickrateStr);
                }
                catch
                {
                    tickrate = interval;
                }
                interval = tickrate;
            }

        }
        #endregion
       #region behavior
        public  void startBehaviorEngine()
        {
            //Start our own chem thread
            if (IsRunning(tmBehaveThread)) return;
            try
            {
                if (tmBehaveThread == null)
                {
                    tmBehaveThread = new Thread(memBehaviorThread);
                }
                tmBehaveThread.IsBackground = true;
                tmBehaveThread.Start();
            }
            catch (Exception e)
            {
                Console.WriteLine("{0}\n{1}", e.Message, e.StackTrace);
            }

        }

        public  void memBehaviorThread()
        {
            int interval = 1000;
            int tickrate = interval;
            while (true)
            {
                Thread.Sleep(interval);
                if (!tmBehaveEnabled) continue;
                try
                {
                    
                    if ((curBot.myBehaviors != null) && (curBot.isAcceptingUserInput))
                    {
                        try
                        {
                            //curBot.myBehaviors.runBotBehaviors(curBot);
                            curBot.performBehaviors();
                        }
                        catch (Exception e)
                        {
                            Console.WriteLine("ERR: {0}\n{1}", e.Message, e.StackTrace);
                        }
                    }
                    string tickrateStr = getBBHash("tickrate");
                    tickrate = interval;

                    tickrate = int.Parse(tickrateStr);
                }
                catch
                {
                    tickrate = interval;
                }
                interval = tickrate;
            }

        }
        #endregion




        public  void startMtalkWatcher()
        {
            if (curBot.myChemistry == null)
            {
                curBot.realChem = new Qchem(myConst.MEMHOST);
                curBot.myChemistry = new RChem(myConst.MEMHOST, true);
            }
            //Start our own chem thread
            try
            {
                if (IsRunning(tmTalkThread)) return;
                if (tmTalkThread == null)
                {
                    tmTalkThread = new Thread(memTalkThread);
                }
                tmTalkThread.IsBackground = true;
                tmTalkThread.Start();
            }
            catch (Exception e)
            {
                Console.WriteLine("{0}\n{1}", e.Message, e.StackTrace);
            }
        }

        private static bool IsRunning(Thread thread)
        {
            if (thread == null) return false;
            return thread.IsAlive;
        }

        public  bool checkNewPersonality()
        {
            bool loadedCore = false;

            lock (curBot)
            {
                if (safeBB())
                {
                    try
                    {
                        string curAIMLClass = getBBHash("aimlclassdir");
                        string curAIMLInstance = getBBHash("aimlinstancedir");
                        if (!(lastAIMLInstance.Contains(curAIMLInstance)))
                        {
                            Console.WriteLine("loadAIMLFromFiles: " + curAIMLClass);
                            Console.WriteLine("loadAIMLFromFiles: " + curAIMLInstance);
                            //curBot.isAcceptingUserInput = false;
                            //myCronThread.Abort(); //.Suspend();
                            //tmFSMThread.Abort(); //.Suspend();
                            //tmBehaveThread.Abort(); //.Suspend();

                            lastAIMLInstance = curAIMLInstance;
                            if (curBot.myCron != null) curBot.myCron.clear();
                            curBot.SizeC = 0;
                            curBot.loadAIMLFromFiles();
                            loadedCore = true;
                            if (Directory.Exists(curAIMLClass)) curBot.loadAIMLFromFiles(curAIMLClass);
                            if (Directory.Exists(curAIMLInstance)) curBot.loadAIMLFromFiles(curAIMLInstance);

                            Console.WriteLine("Load Complete.");

                            curBot.isAcceptingUserInput = true;
                            //startMtalkWatcher();
                            //startFSMEngine();
                            //startBehaviorEngine();
                            //startCronEngine();

                            //myCronThread.Resume();
                            //tmFSMThread.Resume();
                            //tmBehaveThread.Resume();

                            return loadedCore;
                        }
                    }
                    catch (Exception e)
                    {
                        curBot.isAcceptingUserInput = true;
                        Console.WriteLine("Warning:*** Load Incomplete. *** \n {0}\n{1}", e.Message, e.StackTrace);
                    }

                }
                return loadedCore;
            }
        }

        public  void memTalkThread()
        {
            int interval = 200;
            int lastuutid = 0;
            int uutid = 0;
            //string utterance = "";

            string lastUtterance = "";
            Console.WriteLine("");
            Console.WriteLine("******* MTALK ACTIVE *******");
            Console.WriteLine("");

            while (true)
            {
                Thread.Sleep(interval);
                updateTime();
                // Tick the microThreader
                if (myScheduler != null && tmTalkEnabled)
                {
                    myScheduler.Run();
                }

                if ((curBot != null) && (curBot.outputQueue.Count > 0))
                {
                    curBot.processOutputQueue();
                }

                if (safeBB())
                {
                   bool newPerson= checkNewPersonality();
                    

                    string sv = null;
                    try
                    {
                        //sv = myChemistry.m_cBus.getHash("mdollhearduuid");
                        sv = getBBHash("uttid");
                        uutid = int.Parse(sv);
                    }
                    catch (Exception e) { }
                    if (uutid == lastuutid) { continue; }
                    if (!curBot.isAcceptingUserInput) { continue; }
                    try
                    {
                        lastuutid = uutid;
                        //string myInput = (myChemistry.m_cBus.getHash("mdollheard"));
                        string myInput = (getBBHash("speechhyp"));

                        //Get lastTTS output as <that>
                            // Other output sources may post a short acknowledgement
                            // We want only real sentences or direct yes/no/ok
                        string myThat = (getBBHash("TTSText"));
                        if ((myThat.Length > 4)
                            || (myThat.ToLower().Contains("yes"))
                            || (myThat.ToLower().Contains("no"))
                            || (myThat.ToLower().Contains("ok"))
                            )
                        {
                            curUser.blackBoardThat = myThat;
                        }
                        else
                        {
                            curUser.blackBoardThat = "";
                        }
                        //Get fsmstate output as <state>
                        string myState = (getBBHash("fsmstate"));
                        curUser.Predicates.updateSetting("state",myState);

                        // get values off the blackboard
                        curBot.importBBBot();
                        curBot.importBBUser(curUser);

                        if ((myInput.Length > 0) && (!myInput.Equals(lastUtterance)))
                        {
                            Console.WriteLine("Heard: " + myInput);
                            Request r = new Request(myInput, curUser, curBot);
                            Result res = curBot.Chat(r);
                            string myResp = res.Output;
                            Console.WriteLine("Response: " + myResp);
                            if (myResp == null)
                            {
                                myResp = "I don't know how to respond.";
                            }
                            myResp = myResp.Replace("_", " ");
                            Console.WriteLine("*** AIMLOUT = '{0}'", myResp);
                            if (!myResp.ToUpper().Contains("IGNORENOP"))
                            {
                                sayResponse(myResp);
                                setBBHash("lsaprior", myInput);
                            }

                            lastUtterance = myInput;
                        }
                    }
                    catch (Exception e)
                    {
                        Console.WriteLine("{0}\n{1}", e.Message, e.StackTrace);
                    }


                }

            }
        }

        public  void updateTime()
        {
            setBBHash("userdate", DateTime.Now.Date.ToString());
            setBBHash("useryear", DateTime.Now.Year.ToString());
            setBBHash("usermonth", DateTime.Now.Month.ToString());
            setBBHash("userday", DateTime.Now.Day.ToString());
            setBBHash("userdayofweek", DateTime.Now.DayOfWeek.ToString());
            setBBHash("usertimeofday", DateTime.Now.TimeOfDay.ToString());
            setBBHash("userhhour", DateTime.Now.Hour.ToString());
            setBBHash("userminute", DateTime.Now.Minute.ToString());
        }

        public bool safeBB()
        {
            //bbSafe=(curBot.myChemistry != null) && (curBot.myChemistry.m_cBus != null);
            return curBot.bbSafe;
        }
        public void setBBHash(string key, string data)
        {
            //curBot.myChemistry.m_cBus.setHash(key,data);
            curBot.setBBHash(key, data);
        }
        public string getBBHash(string key)
        {
            try
            { 
                //BBDict[key] =curBot.myChemistry.m_cBus.getHash(key)
                return curBot.getBBHash(key);
            }
            catch
            {
                return "";
            }
        }

        public  void sayResponse(string message)
        {
            if (message == null) return;
            if (!message.ToUpper().Contains("IGNORENOP"))
            {
                //myChemistry.m_cBus.setHash("mdollsay", myResp);
                //myChemistry.m_cBus.setHash("mdollsayuttid", lastuutid.ToString());
                setBBHash("TTSText", message);
                Random Rgen = new Random();
           
                int myUUID = Rgen.Next(Int32.MaxValue);
                //curBot.myChemistry.m_cBus.setHash("TTSuuid", lastuutid.ToString());
                setBBHash("TTSuuid", myUUID.ToString());
                Console.WriteLine("sayResponse :{0}:{1}", myUUID.ToString(), message);

                String lsaThat = message.Replace(" ", " TTX");
                lsaThat = lsaThat.Replace(".", " ");
                lsaThat = lsaThat.Replace("?", " ");
                lsaThat = lsaThat.Replace("!", " ");
                lsaThat = lsaThat.Replace(",", " ");
                lsaThat += " " + message;
                setBBHash("lsathat", lsaThat);

            }

        }


        #region Serialization
        public void loadAIMLFromFile(string path)
        {
            Thread.Sleep(10);
            if (skiploading)
            {
                Console.WriteLine(" - WARNING: SERVITOR SKIPLOADING: {0}", path);
                return;
            }
            if (curBot != null)
            {
                curBot.loadAIMLFromFile(path);
            }
            else
            {
                Console.WriteLine(" - WARNING: SERVITOR NO BOT TO LOAD: {0}", path);
            }
        }
        public void loadAIMLFromFiles(string path)
        {
            Thread.Sleep(10);
            if (skiploading)
            {
                Console.WriteLine(" - WARNING: SERVITOR SKIPLOADING: {0}", path);
                return;
            }
            if (curBot != null)
            {
                curBot.loadAIMLFromFiles(path);
            }
            else
            {
                Console.WriteLine(" - WARNING: SERVITOR NO BOT TO LOAD: {0}", path);
            }
        }

        /// <summary>
        /// Saves the whole bot to a binary file to avoid processing the AIML each time the 
        /// bot starts
        /// </summary>
        /// <param name="path">the path to the file for saving</param>
        public void saveToBinaryFile(string path)
        {
            Console.WriteLine("START SERVITOR BINARY SAVE:{0}", path);
            if (savedServitor)
            {
                Console.WriteLine(" - WARNING: PREVIOUS SAVE TO:{0}", path);
            }
            // check to delete an existing version of the file
            FileInfo fi = new FileInfo(path);
            if (fi.Exists)
            {
                Console.WriteLine(" - BINARY SAVE OVERWRITE:{0}", path);
                fi.Delete();
            }
            curBot.saveToBinaryFile(path);
            savedServitor = true;
            Console.WriteLine("SERVITOR BINARY SAVED:{0}", path);
        }

        public void saveToBinaryFile0(string path)
        {
            Console.WriteLine("START SERVITOR BINARY SAVE:{0}", path);
            if (savedServitor)
            {
                Console.WriteLine(" - WARNING: PREVIOUS SAVE TO:{0}", path);
            }
            // check to delete an existing version of the file
            FileInfo fi = new FileInfo(path);
            if (fi.Exists)
            {
                Console.WriteLine(" - BINARY SAVE OVERWRITE:{0}", path);
                fi.Delete();
            }
            if (curBot.noSerialzation) return;
            FileStream saveFile = File.Create(path);
            BinaryFormatter bf = new BinaryFormatter();
            bf.Serialize(saveFile, curBot);
            saveFile.Close();
            savedServitor = true;
            Console.WriteLine("SERVITOR BINARY SAVED:{0}", path);
        }

        /// <summary>
        /// Loads a dump of whole bot into memory so avoiding processing the AIML files again
        /// </summary>
        /// <param name="path">the path to the dump file</param>
        public void loadFromBinaryFile(string path)
        {
            Console.WriteLine("START SERVITOR BINARY LOAD:{0}", path);
            curBot.loadFromBinaryFile(path);
            Console.WriteLine("SERVITOR BINARY LOAD:{0}", path);
        }
        public void loadFromBinaryFile0(string path)
        {
            Console.WriteLine("START SERVITOR BINARY LOAD:{0}", path);
            FileStream loadFile = File.OpenRead(path);
            BinaryFormatter bf = new BinaryFormatter();
            curBot = (AltBot)bf.Deserialize(loadFile);
            loadFile.Close();
            Console.WriteLine("SERVITOR BINARY LOAD:{0}", path);
        }

        #endregion

        public void shutdown()
        {
            if (tmTalkThread != null) tmTalkThread.Abort();
            if (tmFSMThread != null) tmFSMThread.Abort();
            if (tmBehaveThread != null) tmBehaveThread.Abort();
            if (myCronThread != null) myCronThread.Abort();
        }
    }

    
}
