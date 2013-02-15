using System;
using System.Collections.Generic;
using System.Collections;
using System.Globalization;
using System.Text.RegularExpressions;
using System.IO;
using System.Xml;
using System.Text;
using System.Runtime.Serialization.Formatters.Binary;
using System.Reflection;
using System.Net.Mail;
using AIMLbot;
#if false
using DcBus;
using Aima.Core.Logic.Propositional.Algorithms;
using Aima.Core.Logic.Propositional.Parsing;
using Aima.Core.Logic.Propositional.Parsing.AST;
using LAIR.ResourceAPIs.WordNet;
using MushDLR223.ScriptEngines;
using MushDLR223.Utilities;
using MushDLR223.Virtualization;
using org.opencyc.api;
using RTParser.AIMLTagHandlers;
using RTParser.Database;
using RTParser.Normalize;
using RTParser.Utils;
using RTParser.Variables;
using AIMLLoader = AltAIMLbot.Utils.AIMLLoaderS;
using AIMLTagHandlerU=AltAIMLbot.Utils.AIMLTagHandlerU;
using bot=AltAIMLbot.AIMLTagHandlers.bot;
using CustomTagAttribute=AltAIMLbot.Utils.CustomTagAttribute;
using Gender=AltAIMLbot.Utils.Gender;
using MatchState=AltAIMLbot.Utils.MatchState;
using Node=AltAIMLbot.Utils.Node;
using recursiveVerbatum=AltAIMLbot.AIMLTagHandlers.recursiveVerbatum;
using TagHandler=AltAIMLbot.Utils.TagHandler;
using verbatum=AltAIMLbot.AIMLTagHandlers.verbatum;
#endif
using AltAIMLbot.Database;
using AltAIMLbot.Utils;
using AltAIMLbot.Variables;
using DcBus;
using LogicalParticleFilter1;
using Action=System.Action;

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
using System.Collections;
using System.Collections.Generic;
using System.Globalization;
using System.IO;
using System.Net.Mail;
using System.Reflection;
using System.Runtime.Serialization.Formatters.Binary;
using System.Text.RegularExpressions;
using System.Web;
using System.Xml;
using AIMLbot;
using AltAIMLbot;
using LAIR.ResourceAPIs.WordNet;
using MushDLR223.ScriptEngines;
using MushDLR223.Utilities;
using MushDLR223.Virtualization;
using org.opencyc.api;
#if USE_SWIPROLOG
using PrologScriptEngine;
#endif
using AltAIMLbot.Web;
using Console=System.Console;
using UPath = AltAIMLbot.Unifiable;
using UList = System.Collections.Generic.List<AltAIMLbot.Utils.TemplateInfo>;

namespace AltAIMLbot
{
    /// <summary>
    /// Return a Response object
    /// </summary>
    /// <param name="cmd"></param>
    /// <param name="requestOrNull"></param>
    /// <returns></returns>
    public delegate object SystemExecHandler(string cmd, Request requestOrNull);

    /// <summary>
    /// Encapsulates a Proccessor. If no settings.xml file is found or referenced the Proccessor will try to
    /// default to safe settings.
    /// </summary>
    [Serializable]
    public partial class AltBot : StaticAIMLUtils, IChatterBot
    {
        #region Attributes

        /// <summary>
        /// A chemistry connection object
        /// </summary>
        //public RChem myChemistry = new RChem(myConst.MEMHOST, true);
        //public Qchem realChem = new Qchem(myConst.MEMHOST);
        
        /// <summary>
        /// @TODO @WORKAROUND Currently adding some padding around Template expanded tags
        /// </summary>
        public static bool PadAroundTemplateTags = false;

        public RChem myChemistry = null;

#if false
        public Qchem realChem = null;
        public ChemTrace myChemTrace = null;
        public QfsmSet myFSMS = new QfsmSet();
        public BehaviorSet myBehaviors;
        public Cron myCron = null;
        public bool inCritical = false;
        private bool _blockCron = false;

        public bool blockCron
        {
            get
            {
                if (_blockCron) return true;
                if (servitor.IsBackgroundDisabled) return true;
                return false;
            }
            set
            {
                _blockCron = value;
            }
        }
        public bool loadChanging = true;
        public RandomMemory myRandMem = new RandomMemory();
#endif

        [NonSerialized]
        public readonly SIProlog prologEngine = SIProlog.CurrentProlog;

        static public object BotInitLock = new object();
        static public object WordNetEngineLock
        {
            get { return BotInitLock; }
        }
        static private WordNetEngine _wordNetEngine;
        public WordNetEngine wordNetEngine
        {
            get
            {
                lock (WordNetEngineLock) return _wordNetEngine;
            }
            set
            {
                LockInfo.CheckLocked(WordNetEngineLock);
                _wordNetEngine = _wordNetEngine ?? value;
            }
        }

#if false
        [NonSerialized]
        public KnowledgeBase myKB = new KnowledgeBase();
        [NonSerialized]
        public KnowledgeBase myBaseKB = new KnowledgeBase();
        [NonSerialized]
        public WalkSAT myWalkSAT = new WalkSAT();
        [NonSerialized]
        public Model myModel = null;
        [NonSerialized]
        public Model myActiveModel = null;
        public string myPositiveSATModleString = null;

        public object guestEvalObject = null;
        public Queue<string> outputQueue = new Queue<string>();

        private Servitor _myServitor;
        public Servitor myServitor
        {
            get
            {
                if (_myServitor != null) return _myServitor;
                return _myServitor;
            }
            set
            {
                _myServitor = value;
                if (value != null) RegisterObject("robot", value);
            }
        }
#endif

        public static bool IncludeMeNeValue;
        public static Dictionary<string, AltBot> Robots = new Dictionary<string, AltBot>();
        static AltBot()
        {
            MushDLR223.ScriptEngines.OKAssemblyResolve.ResolverEnabled = true;
        }
        public static AltBot FindOrCreateRobot(string text)
        {
            AltBot robot;
            lock (Robots)
            {
                if (TryGetValueLocked(Robots, Robots, text, out robot))
                {
                    return robot;
                }
                Robots[text] = robot = new AltBot();
            }
            robot.SetName(text);
            return robot;
        }

        private readonly List<XmlNodeEvaluator> XmlNodeEvaluators = new List<XmlNodeEvaluator>();
        private TestCaseRunner testCaseRunner;

        private static int skipMany;
        public static bool UseBreakpointOnError;

        public bool ListeningToSelf
        {
            get
            {
                return true;
                if (GlobalSettings != null)
                {
                    Unifiable lts = GlobalSettings.grabSettingNoDebug("ListeningToSelf");
                    if (IsUnknown(lts)) return false;
                    if (IsFalse(lts)) return false;
                    if (IsTrue(lts)) return true;
                    return true;
                }

                return false;
            }
        }

        public bool ProcessHeardPreds
        {
            get
            {
                return true;
                if (GlobalSettings != null)
                {
                    Unifiable lts = GlobalSettings.grabSettingNoDebug("ProcessHeardPreds");
                    if (IsUnknown(lts)) return false;
                    if (IsFalse(lts)) return false;
                    if (IsTrue(lts)) return true;
                    return true;
                }

                return false;
            }
        }

        public override string ToString()
        {
            string s = GetType().Name;
            if (!string.IsNullOrEmpty(NameAsSet)) return s + " nameAsSet=" + NameAsSet;
            if (GlobalSettings != null)
            {
                s += " name=" + GlobalSettings.grabSettingNoDebug("name") + " (" + NamePath + ")";
            }
            if (!string.IsNullOrEmpty(NamePath)) return s + " NamePath=" + NamePath;
            return s;
        }
        public Queue<string> outputQueue = new Queue<string>();

        /// <summary>
        /// Will ensure the same loader options are used between loaders
        /// </summary>
        public bool StaticLoader = true;

        public User BotAsUser;
        public User ExemplarUser;
        public string NamePath;
        public string NameAsSet;
        //public Request BotAsRequestUsed = null;
        public Request GetBotRequest(string s)
        {
            var botAsUser1 = BotAsUser ?? LastUser;
            s = Trim(s);
            if (!s.StartsWith("<")) s = "<!-- " + s.Replace("<!--", "<#").Replace("-->", "#>") + " -->";
            var r = new AIMLbot.MasterRequest(s, botAsUser1, Unifiable.EnglishNothing, botAsUser1, this, null,
                                              DefaultStartGraph);
            //r.ChatOutput.RawText = s;
            r.writeToLog = writeToLog;
            //Result res = new AIMLbot.MasterRequest(s, botAsUser1, this, r, null, null);            
            //r.CurrentQuery = new SubQuery(s, res, r);
            OnBotCreated(() =>
                             {
                                 User BotAsUser1 = this.BotAsUser;
                                 ((Request)r).SetSpeakerAndResponder(BotAsUser1, BotAsUser1);
                             });
            r.IsTraced = this.IsTraced;
            r.depth = 0;
            // times out in 15 minutes
            r.TimeOutFromNow = TimeSpan.FromMinutes(15);
            return r;
        }

        private AIMLLoader _loader;
        private AIMLLoader _loaderOnceLeast;
        public AIMLLoader Loader
        {
            set
            {
                _loader = value;
                if (value == null)
                {
                    _loaderOnceLeast = value;
                }
            }
            get
            {
                return _loader ?? _loaderOnceLeast;
            }
        }


        public List<CrossAppDomainDelegate> ReloadHooks = new List<CrossAppDomainDelegate>();

        /// <summary>
        /// A dictionary object that looks after all the settings associated with this Proccessor
        /// </summary>
        public SettingsDictionaryReal GlobalSettings;

        public SettingsDictionary SharedGlobalSettings;

        #endregion

        /// <summary>
        /// A dictionary of all the gender based substitutions used by this bot
        /// </summary>
        public SettingsDictionary GenderSubstitutions;

        /// <summary>
        /// A dictionary of all the first person to second person (and back) substitutions
        /// </summary>
        public SettingsDictionary Person2Substitutions;

        /// <summary>
        /// A dictionary of first / third person substitutions
        /// </summary>
        public SettingsDictionary PersonSubstitutions;

        /// <summary>
        /// Generic substitutions that take place during the normalization process
        /// </summary>
        public SettingsDictionary InputSubstitutions;

        /// <summary>
        /// Output substitutions that take place before the bot speaks
        /// </summary>
        static public SettingsDictionary OutputSubstitutions;

        /// <summary>
        /// The default predicates to set up for a user
        /// </summary>
        public SettingsDictionary DefaultPredicates;

        /// <summary>
        /// A weak name/value association list of what has happened in dialog  
        /// </summary>
        public SettingsDictionary HeardPredicates;

        /// <summary>
        /// A name+prop/value association list of things like  look.set-return, look.format-whword,
        /// look.format-assert, look.format-query, look.format-etc,
        /// </summary>
        public SettingsDictionary RelationMetaProps;

        /// <summary>
        /// When a tag has no name like <icecream/> it is transformed to <bot name="icecream"></bot>
        /// </summary>
        public static bool UnknownTagsAreBotVars = true;

        /// <summary>
        ///  Substitution blocks for graphmasters
        /// </summary>
        public Dictionary<string, ISettingsDictionary> AllDictionaries = new Dictionary<string, ISettingsDictionary>();

        /// <summary>
        /// An List<> containing the tokens used to split the input into sentences during the 
        /// normalization process
        /// </summary>
        static public List<string> Splitters = new List<string>();


        public void processOutputQueue()
        {
            if (!isPerformingOutput)
            {
                if (outputQueue.Count > 0)
                {
                    logText("BOT OUTPUT GOING TO NOT COME OUT:" + outputQueue.Count);
                }
                return;
            }
            while (outputQueue.Count > 0)
            {
                string msg = outputQueue.Dequeue();
                if (sayProcessor != null)
                {
                    sayProcessor(msg);
                }
                else
                {
                    Console.WriteLine("Missing sayProcessor! BOT OUTPUT:{0}", msg);
                }
                logText("BOT OUTPUT:" + msg);

            }
        }

        public void flushOutputQueue()
        {
            outputQueue.Clear();
            logText("BOT flushOutputQueue:");
            string flushsignal = GlobalSettings.grabSetting("flushsignal", false);
            if ((flushsignal != null) && (flushsignal.Length > 2))
            {
                postOutput(flushsignal);
            }

        }

        Regex SentRegex = new Regex(@"(\S.+?[.!?,\)])(?=\s+|$)");

        public void postOutput(string msg)
        {
            if (string.IsNullOrEmpty(msg)) return;

            if (msg.Length < 256)
            {
                // just post output
                outputQueue.Enqueue(msg);
                logText("BOT postOutput:" + msg);
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
                    outputQueue.Enqueue(s);
                    logText("BOT postOutput:" + s);

                }

            }
        }
        public void sendOutput(string msg)
        {
            // posts and processes
            if (msg.Length < 1024)
            {
                // just post output
                outputQueue.Enqueue(msg);
                logText("BOT sendOutput:" + msg);
            }
            else
            {
                string[] sents = msg.Split('.');
                foreach (string s in sents)
                {
                    outputQueue.Enqueue(s);
                    logText("BOT sendOutput:" + s);

                }
            }
            processOutputQueue();
        }

        public string getPendingOutput()
        {
            string outmsg = "";
            while (outputQueue.Count > 0)
            {
                string msg = outputQueue.Dequeue();
                outmsg += msg + "\r\n";
            }
            return outmsg;
        }

        /// <summary>
        /// Flag to show if the bot is willing to accept user input
        /// </summary>
        public bool isAcceptingUserInput = true;

        /// <summary>
        /// Flag to show if the bot is producing output
        /// </summary>
        public bool isPerformingOutput
        {
            get { return _isPerformingOutput; }
            set
            {
                if (value == false  && outputQueue.Count > 0)
                {
                    if (_isPerformingOutput == true)
                    {
                        writeToLog("ERROR Was preformingOUTUT! ");
                    }
                    processOutputQueue();
                }
                _isPerformingOutput = value;
            }
        }

        public bool _isPerformingOutput = false;
        /// <summary>
        /// A dictionary of all inherited settings betten users
        /// </summary>
        public SettingsDictionary AllUserPreds;

        /// <summary>
        /// A dictionary of all settings from anyone .. just a fallback
        /// </summary>
        public SettingsDictionary EnginePreds;

        readonly public TagHandlerProcessor TagHandling = new TagHandlerProcessor();
        /// <summary>
        /// A buffer to hold log messages to be written out to the log file when a max size is reached
        /// </summary>
        private readonly List<string> LogBuffer = new List<string>();

        /// <summary>
        /// A list of Topic states that are set currently (for use of guarding content)
        /// </summary>
        public List<Unifiable> CurrentStates = new List<Unifiable>();

        /// <summary>
        /// How big to let the log buffer get before writing to disk
        /// </summary>
        private int MaxLogBufferSize
        {
            get { return Convert.ToInt32(GlobalSettings.grabSetting("maxlogbuffersize")); }
        }

        /// <summary>
        /// The message to show if a user tries to use the Proccessor whilst it is set to not process user input
        /// </summary>
        private Unifiable NotAcceptingUserInputMessage
        {
            get { return GlobalSettings.grabSettingNoDebug("notacceptinguserinputmessage"); }
        }

        /// <summary>
        /// The maximum amount of time a request should take (in milliseconds)
        /// </summary>
        public double TimeOut
        {
            get
            {
                return 70000;
                if (GlobalSettings == null || !GlobalSettings.containsSettingCalled("timeout"))
                {
                    return 2000000;
                }
                String s = ToValueString(GlobalSettings.grabSetting("timeout"));
                return Convert.ToDouble(s);
            }
        }

        /// <summary>
        /// The message to display in the event of a timeout
        /// </summary>
        public string TimeOutMessage
        {
            get { return GlobalSettings.grabSetting("timeoutmessage"); }
        }

        /// <summary>
        /// The locale of the bot as a CultureInfo object
        /// </summary>
        public CultureInfo Locale
        {
            get
            {
                return new CultureInfo(this.GlobalSettings.grabSetting("culture"));
            }
        }
         

        /// <summary>
        /// Will match all the illegal characters that might be inputted by the user
        /// </summary>
        public Regex Strippers
        {
            get
            {
                return new Regex(GlobalSettings.grabSettingNoDebug("stripperregex"),
                                 RegexOptions.IgnorePatternWhitespace);
            }
        }

        /// <summary>
        /// The email address of the botmaster to be used if WillCallHome is set to true
        /// </summary>
        public string AdminEmail
        {
            get
            {
                return this.GlobalSettings.grabSetting("adminemail");
            }
            set
            {
                if (value.Length > 0)
                {
                    // check that the email is valid
                    string patternStrict = @"^(([^<>()[\]\\.,;:\s@\""]+"
                                           + @"(\.[^<>()[\]\\.,;:\s@\""]+)*)|(\"".+\""))@"
                                           + @"((\[[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}"
                                           + @"\.[0-9]{1,3}\])|(([a-zA-Z\-0-9]+\.)+"
                                           + @"[a-zA-Z]{2,}))$";
                    Regex reStrict = new Regex(patternStrict);

                    if (reStrict.IsMatch(value))
                    {
                        // update the settings
                        this.GlobalSettings.addSetting("adminemail", value);
                    }
                    else
                    {
                        throw (new Exception("The AdminEmail is not a valid email address"));
                    }
                }
                else
                {
                    GlobalSettings.addSetting("adminemail", Unifiable.MISSING);
                }
            }
        }

        /// <summary>
        /// Flag to denote if the bot is writing messages to its logs
        /// </summary>
        public bool IsLogging
        {
            get
            {
                if (GlobalSettings == null) return true;
                return true;
                string islogging = ((string) GlobalSettings.grabSetting("islogging")) ?? "true";
                if (islogging.ToLower() == "true")
                {
                    return true;
                }
                else
                {
                    return false;
                }
            }
        }

        /// <summary>
        /// Flag to denote if the bot will email the botmaster using the AdminEmail setting should an error
        /// occur
        /// </summary>
        public bool WillCallHome
        {
            get
            {
                Unifiable willcallhome = GlobalSettings.grabSetting("willcallhome");
                return (IsTrue(willcallhome));
            }
        }

        /// <summary>
        /// When the Bot was initialised
        /// </summary>
        public DateTime StartedOn = DateTime.Now;

        /// <summary>
        /// The supposed sex of the bot
        /// </summary>
        public Gender Sex
        {
            get
            {
                int sex = Convert.ToInt32(this.GlobalSettings.grabSetting("gender"));
                Gender result;
                switch (sex)
                {
                    case -1:
                        result = Gender.Unknown;
                        break;
                    case 0:
                        result = Gender.Female;
                        break;
                    case 1:
                        result = Gender.Male;
                        break;
                    default:
                        result = Gender.Unknown;
                        break;
                }
                return result;
            }
        }

        private string _PathToUserFiles;

        public string PathToUserDir
        {
            get
            {
                if (_PathToUserFiles != null) return _PathToUserFiles;
                if (GlobalSettings.containsSettingCalled("usersdirectory"))
                {
                    Unifiable dir = GlobalSettings.grabSetting("usersdirectory");
                    HostSystem.CreateDirectory(dir);
                    _PathToUserFiles = dir;
                    return HostSystem.ToRelativePath(dir, RuntimeDirectory);
                }
                foreach (string s in new[] { PersonalAiml, PathToAIML, PathToConfigFiles, RuntimeDirectory })
                {
                    if (s == null) continue;
                    string exists = HostSystem.Combine(s, "users");
                    if (HostSystem.DirExists(exists))
                    {
                        exists = HostSystem.ToRelativePath(exists, RuntimeDirectory);
                        _PathToUserFiles = exists;
                        return exists;
                    }
                }
                string tryplace = HostSystem.Combine(PathToAIML, "users");
                HostSystem.CreateDirectory(tryplace);
                _PathToUserFiles = tryplace;
                return tryplace;
            }
        }

        private string _PathToBotPersonalFiles;

        protected string PersonalReadWriteDirectory
        {
            get { return _PathToBotPersonalFiles; }
            set
            {
                lock (_RuntimeDirectories)
                {
                    if (_PathToBotPersonalFiles != null)
                    {
                        _RuntimeDirectories.Remove(_PathToBotPersonalFiles);
                    }
                    _PathToBotPersonalFiles = value;
                    if (value != null)
                    {
                        _RuntimeDirectories.Remove(value);
                        _RuntimeDirectories.Insert(0, value);
                        if (value.Contains("shared_aiml"))
                        {
                            Console.WriteLine("WARN - using shared aiml as personal aiml");
                        }
                    }
                }
            }
        }
        protected string PersonalAiml
        {
            get { return _PathToBotPersonalFiles; }
            set
            {
                lock (_RuntimeDirectories)
                {
                    if (_PathToBotPersonalFiles != null)
                    {
                        _RuntimeDirectories.Remove(_PathToBotPersonalFiles);
                    }
                    _PathToBotPersonalFiles = value;
                    if (value != null)
                    {
                        _RuntimeDirectories.Remove(value);
                        _RuntimeDirectories.Insert(0, value);
                        if (value.Contains("shared_aiml"))
                        {
                            Console.WriteLine("WARN - using shared aiml as personal aiml");
                        }
                    }
                }
            }
        }

        protected bool HavePersonalPath
        {
            get { return !string.IsNullOrEmpty(_PathToBotPersonalFiles); }
        }

        /// <summary>
        /// The directory to look in for the AIML files
        /// </summary>
        public string PathToAIML
        {
            get { return GetPathSetting("aimldirectory", "aiml"); }
        }

        private readonly object RuntimeDirectoriesLock = new object();

        public List<string> RuntimeDirectories
        {
            get { lock (RuntimeDirectoriesLock) return new List<string>(_RuntimeDirectories); }
        }

        private string _rapStoreDirectory;

        private string _dataDir = Environment.CurrentDirectory;

        protected string RuntimeDirectory
        {
            get { return _dataDir ?? Environment.CurrentDirectory; }
            set { _dataDir = value; }
        }

        internal bool _UseRapstoreDB = true;
        public bool UseRapstoreDB
        {
            get
            {
                if (_UseRapstoreDB)
                {
                    if (_rapStoreDirectory == null)
                    {
                        Console.WriteLine("WARN Check: this.bot.rapStoreDirectory == null");
                    }
                }
                return _UseRapstoreDB;
            }
            set { _UseRapstoreDB = value; }
        }
        public bool UseRapstore(string graphName)
        {
            if (!UseRapstoreDB) return false;
            // later on decide if we will set some graphs as non rapstoreusing
            return true;
        }

        /// <summary>
        /// in the <say> tag should the sapi be passed as-is (using innerXML) or not (using innerText)
        /// usually set when the sayProcessor delegate is set
        /// </summary>
        public bool saySapi = false;


        #region Delegates

        public sayProcessorDelegate sayProcessor;
        public systemPersonaDelegate personaProcessor = null;

        public string lastBehaviorChatInput;
        public string lastBehaviorChatOutput;
        public User lastBehaviorUser;
        public Queue<string> chatInputQueue = new Queue<string>();

        #endregion

        /// <summary>
        /// The directory to look in for the various XML configuration files
        /// </summary>
        public string PathToConfigFiles
        {
            get { return GetPathSetting("configdirectory", "config"); }
        }

        /// <summary>
        /// The directory into which the various log files will be written
        /// </summary>
        public string PathToLogs
        {
            get { return GetPathSetting("logdirectory", null); }
        }

        public object loglock = new object();
        
        /// <summary>
        /// If set to false the input from AIML files will undergo the same normalization process that
        /// user input goes through. If true the Proccessor will assume the AIML is correct. Defaults to true.
        /// </summary>
        public bool TrustAIML = true;

        /// <summary>
        /// The maximum number of characters a "that" element of a path is allowed to be. Anything above
        /// this length will cause "that" to be "*". This is to avoid having the graphmaster process
        /// huge "that" elements in the path that might have been caused by the Proccessor reporting third party
        /// data.
        /// </summary>
        public int MaxThatSize = 256;

        //#endregion

        #region Delegates

        public delegate void LogMessageDelegate();

        #endregion

        #region Events

        public event LogMessageDelegate WrittenToLog;

        #endregion

        public static int BotNumberCreated;


        public static readonly Dictionary<string, string[]> SettingsAliases = new Dictionary<string, string[]>();

        public bool IsTraced
        {
            get { return qsbase.IsTraced; }
            set { qsbase.IsTraced = value; }
        }

        private readonly QuerySettings qsbase;
        public QuerySettings GetQuerySettings()
        {
            return qsbase;
        }

        /// <summary>
        /// Ctor
        /// </summary>
        public AltBot()
            : base()
        {
            rtpbotcommands = new RTPBotCommands(this);
            qsbase = QuerySettings.CogbotDefaults;
            _RuntimeDirectories = new List<string>();
            PushSearchPath(HostSystem.GetAbsolutePath(AppDomain.CurrentDomain.RelativeSearchPath));
            PushSearchPath(HostSystem.GetAbsolutePath(AppDomain.CurrentDomain.DynamicDirectory));
            PushSearchPath(HostSystem.GetAbsolutePath(AppDomain.CurrentDomain.BaseDirectory));
            PushSearchPath(HostSystem.GetAbsolutePath(Environment.CurrentDirectory));
            PushSearchPath(HostSystem.GetAbsolutePath(_dataDir));
            PushSearchPath(HostSystem.GetAbsolutePath(RuntimeDirectory));
            _dataDir = PushSearchPath(RuntimeDirectory);
            lock (OneAtATime)
            {
                EnsureStaticInit();
                BotNumberCreated++;
                EnsureBotInit(BotNumberCreated == 1);
            }
        }

        public string PopSearchPath(string directory)
        {
            if (directory == null) return null;
            directory = Trim(directory);
            if (directory.Length == 0)
            {
                directory = ".";
            }
            directory = HostSystem.ToCanonicalDirectory(directory);
            lock (_RuntimeDirectories)
            {
                string e = _RuntimeDirectories[0];
                if (e == directory)
                {
                    _RuntimeDirectories.RemoveAt(0);
                    return e;
                }
                bool found = _RuntimeDirectories.Remove(directory);
                return found ? directory : null;
            }
        }

        public string PushSearchPath(string directory)
        {
            if (directory == null) return null;
            directory = Trim(directory);
            if (directory.Length == 0)
            {
                directory = ".";
            }
            directory = HostSystem.ToCanonicalDirectory(directory);
            lock (_RuntimeDirectories)
            {
                bool found = false; // _RuntimeDirectories.Remove(directory);
                _RuntimeDirectories.Insert(0, directory);
                // ReSharper disable ConditionIsAlwaysTrueOrFalse
                return found ? directory : null;
                // ReSharper restore ConditionIsAlwaysTrueOrFalse
            }
        }

        public void EnsureBotInit(bool wasFirst)
        {
            //LocalGraphsByName["default"] =
            //EnsureLocalGraphs();
            TheNLKB = new NatLangDb(this);
            //            BotAsRequestUsed = new AIMLbot.Request("-bank-input-", BotAsUser, this, null);
            AddExcuteHandler("aiml", EvalAIMLHandler);
            AddExcuteHandler("bot", LightWeigthBotDirective);

            testCaseRunner = new TestCaseRunner(null);
            XmlNodeEvaluators.Add(testCaseRunner);

            if (TheCyc == null) TheCyc = new CycDatabase(this);
            CycAccess v = TheCyc.GetCycAccess;


            clojureInterpreter = new ClojureInterpreter(this);
            clojureInterpreter.Init(this);
            clojureInterpreter.Intern("MyBot", this);
            clojureInterpreter.Intern("Users", BotUsers);
            AddExcuteHandler("cloj", ClojExecHandler);
#if USE_SWIPROLOG
            try
            {
                if (!IsMonoRuntime)
                {
                    swiInterpreter = new PrologScriptInterpreter(this);
                        //ScriptManager.LoadScriptInterpreter("PrologScriptInterpreter", this);
                    swiInterpreter.Init(this);
                    AddExcuteHandler("swi", SWIExecHandler);
                    //swiInterpreter.Intern("MyBot", this);
                    //swiInterpreter.Intern("Users", BotUsers);
                }
            }
            catch (Exception e)
            {
              //  writeToLog(e);
            }
#endif
#if !(NOT_FAKE_LISTENERS)

            if (!clojureInterpreter.IsSubscriberOf("thisClient"))
            {
                clojureInterpreter.Intern("thisClient", this);
                clojureInterpreter.Intern("True", true);
                clojureInterpreter.Intern("False", false);
                listeners["AIMLBotModule"] = this;
            }
#endif
            setup();
            GlobalSettings.IsTraced = true;
        }

        protected bool IsMonoRuntime
        {
            get { return true; }
        }


#if !(NOT_FAKE_LISTENERS)
        public Dictionary<string, object> listeners = new Dictionary<string, object>();

        public AltBot MyBot
        {
            get { return this; }
        }
#endif

        #region Settings methods

        /// <summary>
        /// Loads AIML from .aiml files into the graphmaster "brain" of the Proccessor
        /// </summary>
        public void loadAIMLFromDefaults()
        {
        }

        public void loadAIMLFromDefaults0()
        {
            loadConfigs(this, PathToConfigFiles, GetBotRequest("-loadAimlFromDefaults-"));
            loadAIMLAndSettings(PathToAIML);
        }

        /// <summary>
        /// Loads AIML from .aiml files into the graphmaster "brain" of the Proccessor
        /// </summary>
        public void loadAIMLFromURI(string path, Request request)
        {
            bool prev = request.GraphsAcceptingUserInput;
            LoaderOptions savedOptions = request.LoadOptions;
            try
            {
                request.GraphsAcceptingUserInput = false;
                request.Filename = path;
                LoaderOptions options = request.LoadOptions;
                request.Loader.loadAIMLURI(path, options);
                request.Loader.DumpErrors(DLRConsole.DebugWriteLine, false);
                ReloadHooks.Add(() => request.Loader.loadAIMLURI(path, options));
            }
            finally
            {
                request.GraphsAcceptingUserInput = prev;
                request.LoadOptions = savedOptions;
            }
        }


        /// <summary>
        /// Loads AIML from .aiml files into the graphmaster "brain" of the Proccessor
        /// </summary>
        public void loadAIMLAndSettings(string path)
        {
            Request request = GetBotRequest("-loadAIMLAndSettings-" + path + "-");
            request.LoadingFrom = null;
            bool prev = request.GraphsAcceptingUserInput;
            try
            {
                request.GraphsAcceptingUserInput = false;
                // maybe loads settings files if they are there
                string settings = HostSystem.Combine(path, "Settings.xml");
                if (HostSystem.FileExists(settings)) loadSettingsFile(settings, request);
                //loading settings first
                loadConfigs(this, path, request);
                loadAIMLFromURI(path, request);
            }
            finally
            {
                request.GraphsAcceptingUserInput = prev;
            }
        }


        internal AIMLLoader GetLoader(Request request)
        {
            AltBot bot = this;
            AIMLLoader loader = bot.Loader;
            if (!bot.StaticLoader || loader == null)
            {
                loader = new AIMLLoader(bot, request);
            }
            bot.Loader = loader;
            return loader;
        }

        /// <summary>
        /// Allows the Proccessor to load a new XML version of some AIML
        /// </summary>
        /// <param name="newAIML">The XML document containing the AIML</param>
        /// <param name="filename">The originator of the XML document</param>
        public void loadAIMLFromXML(XmlDocument newAIML, LoaderOptions filename, Request request)
        {
            bool prev = request.GraphsAcceptingUserInput;
            try
            {
                request.GraphsAcceptingUserInput = false;
                AIMLLoader loader = GetLoader(request);
                loader.loadAIMLNode(newAIML.DocumentElement, filename, request);
            }
            finally
            {
                request.GraphsAcceptingUserInput = prev;
            }
        }
        public void logText(string msg)
        {

            lock (loglock)
            {
                try
                {
                    string miniLog = String.Format(@"./aiml/BTTrace.txt");
                    System.IO.File.AppendAllText(miniLog, msg + "\n");
                    Console.WriteLine(msg);
                }
                catch
                { }
            }
        }

        public SettingsDictionary GetRelationMetaProps()
        {
            return RelationMetaProps;
        }

        /// <summary>
        /// Instantiates the dictionary objects and collections associated with this class
        /// </summary>
        private void setup()
        {
            bool prev = isAcceptingUserInput;
            try
            {
                isAcceptingUserInput = false;
                RelationMetaProps = MakeSettingsDictionary("chat.relationprops");
                RegisterDictionary("meta", RelationMetaProps);
                RegisterDictionary("metaprops", RelationMetaProps);

                GlobalSettings = MakeSettingsDictionary("bot.globalsettings");
                GlobalSettings.InsertMetaProvider(GetRelationMetaProps);

                GenderSubstitutions = MakeSubstsDictionary("nl.substitutions.gender");
                RegisterSubstitutions("gender", GenderSubstitutions);
                Person2Substitutions = MakeSubstsDictionary("nl.substitutions.person2"); 
                RegisterSubstitutions("person2", Person2Substitutions);
                PersonSubstitutions = MakeSubstsDictionary("nl.substitutions.person");
                RegisterSubstitutions("person", PersonSubstitutions);
                InputSubstitutions = MakeSubstsDictionary("nl.substitutions.input");
                InputSubstitutions.IsTraced = true;
                RegisterSubstitutions("input", InputSubstitutions);
                OutputSubstitutions = MakeSubstsDictionary("nl.substitutions.output");
                RegisterSubstitutions("output", OutputSubstitutions);


                //ParentProvider provider = new ParentProvider(() => GlobalSettings);
                DefaultPredicates = MakeSettingsDictionary("bot.defaultpredicates");
                DefaultPredicates = MakeSettingsDictionary("defaults");
                DefaultPredicates.InsertMetaProvider(GetRelationMetaProps);
                HeardPredicates = MakeSettingsDictionary("chat.heardpredicates");
                RegisterDictionary("heard", HeardPredicates);
                AllUserPreds = MakeSettingsDictionary("bot.alluserpred");
                RegisterDictionary("predicates", AllUserPreds);
                EnginePreds = AllUserPreds;
                RegisterDictionary("enginepreds", EnginePreds);

                AllUserPreds.InsertMetaProvider(GetRelationMetaProps);


                User guser = ExemplarUser = LastUser = new MasterUser("unknown partner", "globalPreds", this);
                lock (microBotUsersLock)
                {
                    BotUsers["globalpreds"] = guser;
                }
                guser.IsRoleAcct = true;
                guser.Predicates.clearSettings();
                guser.Predicates.clearHierarchy();
                guser.Predicates.InsertFallback(() => HeardPredicates);
                var pred = ((SettingsDictionaryReal) guser.Predicates);
                pred.maskSetting("name");
                pred.maskSetting("id");

                // try a safe default setting for the settings xml file
                // Checks for some important default settings
                GlobalSettings.IsIdentityReadOnly = false;
                SetSaneGlobals(GlobalSettings);
                string pathToSettings = HostSystem.Combine(RuntimeDirectory,
                                                           HostSystem.Combine("config", "Settings.xml"));
                Request request = GetBotRequest("<!-- Loads settings from: '" + pathToSettings + "' -->");
                loadSettingsFile(pathToSettings, request);
                // RE-Checks for some important default settings
                SetSaneGlobals(GlobalSettings);
                SetupConveration();
                GlobalSettings.IsIdentityReadOnly = true;
            }
            finally
            {
                isAcceptingUserInput = prev;
            }
        }

        private static string MakeMtName(string named)
        {
            int len = named.Length;
            if (len > 2)
            {
                string toUpper = named.ToUpper();

                string suffix = toUpper.Substring(named.Length - 2);
                if (suffix == "KB" || suffix == "MT" || suffix == "DB")
                    named = named.Substring(0, named.Length - 2);
            }
            named = ToMtCase(named);
            return named;
        }

        public SettingsDictionaryReal MakeSubstsDictionary(string named)
        {
            return new SettingsDictionaryReal(named, this, new KeyValueListCSharp(new List<string>(), new Dictionary<string, string>())) { IsSubsts = true, TrimKeys = false };
        }

        public static string ToMtCase(string fullname)
        {
            string fn2 = Parser.ToCamelCase(fullname.Replace(" ", "_").Replace(".", "_").Replace("-", "_")).Replace(
                "_", "");
            return fn2;
        }
        public SettingsDictionaryReal MakeSettingsDictionary(string named)
        {
            named = named.Trim();
            named = MakeMtName(named);
            string mtName = "chat" + named + "Mt";
            KeyValueListSIProlog v = new KeyValueListSIProlog(() => prologEngine, mtName, "chatVar");
            var dict = new SettingsDictionaryReal(mtName, this, (KeyValueList)v);
            dict.bbPrefix = "user";
            if (mtName != named)
            {
                RegisterDictionary(named, dict);
            }
            return dict;
        }

        /// <summary>
        /// Loads settings based upon the default location of the Settings.xml file
        /// </summary>
        public void loadGlobalBotSettings()
        {
        }

        public void ReloadAll()
        {
            // Setup creates too many things from scratch andis uneeded for refreshing
            //setup();
            var todo = new List<CrossAppDomainDelegate>(ReloadHooks);
            ReloadHooks.Clear();
            foreach (CrossAppDomainDelegate list in todo)
            {
                try
                {
                    list();
                }
                catch (Exception e)
                {
                    writeToLog(e);
                    writeToLog("ReloadAll: " + e);
                }
            }
        }

        // Load the dictionaries for this AltBot from the various configuration files
        public static void loadConfigs(AltBot thiz, string pathToSettings, Request request)
        {
            if (!HostSystem.DirExists(pathToSettings))
            {
                thiz.writeToLog("Not loading configs from non-existent dir: " + pathToSettings);
                return;
            }

            var files = new List<string>(HostSystem.GetFiles(pathToSettings, "*.xml"));

            var HostSystemCombine = new Func<string, string, string>((arg1, arg2) =>
                                                                         {
                                                                             if (arg2 == null) return null;
                                                                             string s = HostSystem.Combine(arg1, arg2);
                                                                             int i =
                                                                                 files.RemoveAll(
                                                                                     obj =>
                                                                                     obj.ToLower().Replace("\\", "/").
                                                                                         EndsWith("/" + arg2.ToLower()));
                                                                             if (i == 0)
                                                                             {
                                                                                 return null;
                                                                             }
                                                                             if (i == 1)
                                                                             {
                                                                                 //good
                                                                                 return s;
                                                                             }
                                                                             //not so good
                                                                             return s;
                                                                         });

            SettingsDictionary GlobalSettings = thiz.GlobalSettings;
            GlobalSettings.IsTraced = true;

            if (request == null) request = thiz.GetBotRequest("<!- Loads Configs from: '" + pathToSettings + "' -->");

            // Checks for some important default settings
            GlobalSettings.loadSettings(HostSystemCombine(pathToSettings, "settings.xml"), request);
            GlobalSettings.loadSettings(HostSystemCombine(pathToSettings, "core.xml"), request);
            GlobalSettings.loadSettings(
                HostSystemCombine(pathToSettings, GlobalSettings.grabSetting("programd.startup-file-path")), request);
            thiz.SetSaneGlobals(GlobalSettings);

            // these are ignores
            HostSystemCombine(pathToSettings, GlobalSettings.grabSetting("programd.conf-location.plugins"));
            HostSystemCombine(pathToSettings, "listeners.xml");
            HostSystemCombine(pathToSettings, "log4j.xml");

            thiz.DefaultPredicates.loadSettings(HostSystemCombine(pathToSettings, "predicates.xml"), request);
            thiz.DefaultPredicates.loadSettings(HostSystemCombine(pathToSettings, "properties.xml"), request);


            thiz.Person2Substitutions.loadSettings(
                HostSystemCombine(pathToSettings, GlobalSettings.grabSetting("person2substitutionsfile")), request);
            thiz.PersonSubstitutions.loadSettings(
                HostSystemCombine(pathToSettings, GlobalSettings.grabSetting("personsubstitutionsfile")), request);
            thiz.GenderSubstitutions.loadSettings(
                HostSystemCombine(pathToSettings, GlobalSettings.grabSetting("gendersubstitutionsfile")), request);
            thiz.InputSubstitutions.loadSettings(
                HostSystemCombine(pathToSettings, GlobalSettings.grabSetting("substitutionsfile")), request);
            thiz.Person2Substitutions.IsTraced =
                thiz.PersonSubstitutions.IsTraced =
                thiz.GenderSubstitutions.IsTraced = thiz.InputSubstitutions.IsTraced = false;

            thiz.DefaultPredicates.loadSettings(
                HostSystemCombine(pathToSettings, GlobalSettings.grabSetting("defaultpredicates")), request);

            thiz.InputSubstitutions.loadSettings(
                HostSystemCombine(pathToSettings, GlobalSettings.grabSetting("substitutions")), request);
            thiz.InputSubstitutions.loadSettings(HostSystemCombine(pathToSettings, "substitutions.xml"), request);
            thiz.InputSubstitutions.IsTraced = true;

            // Grab the splitters for this Proccessor
            thiz.loadSplitters(HostSystemCombine(pathToSettings, GlobalSettings.grabSetting("splittersfile")));
            thiz.loadSplitters(HostSystemCombine(pathToSettings, GlobalSettings.grabSetting("sentence-splitters")));
            thiz.loadSplitters(HostSystemCombine(pathToSettings, "sentence-splitters.xml"));

            // genformat.xml
            thiz.RelationMetaProps.loadSettings(HostSystemCombine(pathToSettings, "genformat.xml"), request);


            string gpss ="globalpreds.xml";
            User guser = thiz.FindUser("globalPreds");
            if (HostSystem.FileExists(HostSystemCombine(pathToSettings, gpss)))
            {
                SettingsDictionaryReal.loadSettingsNow((ISettingsDictionary) guser.Predicates, pathToSettings,
                                                       gpss, SettingsPolicy.Default, request);
            }
            thiz.writeToLog("Files left to process = " + files.Count);
            foreach (string list in files)
            {
                writeDebugLine("AIMLLOADER: loadSettings " + list);
                GlobalSettings.IsTraced = true;
                GlobalSettings.loadSettings(list, request);
            }
        }

        /// <summary>
        /// Loads settings and configuration info from various xml files referenced in the settings file passed in the args. 
        /// Also generates some default values if such values have not been set by the settings file.
        /// </summary>
        /// <param name="pathToSettings">Path to the settings xml file</param>
        public void loadSettingsFile(string pathToSettings, Request request)
        {
            if (request == null) request = GetBotRequest("<!-- Loads settings from: '" + pathToSettings + "' -->");
            ReloadHooks.Add(() => loadSettingsFile(pathToSettings, request));
            GlobalSettings.loadSettings(pathToSettings, request);
        }

        private void SetSaneGlobals(ISettingsDictionary settings)
        {
            SaneLocalSettings(settings, "notopic", Unifiable.EnglishNothing);
            SaneLocalSettings(settings, "version", Environment.Version.ToString());
            SaneLocalSettings(settings, "name", "Unknown");
            SaneLocalSettings(settings, "botmaster", "Unknown");
            SaneLocalSettings(settings, "author", "Nicholas H.Tollervey");
            SaneLocalSettings(settings, "location", "Unknown");
            SaneLocalSettings(settings, "gender", "-1");
            SaneLocalSettings(settings, "birthday", "2006/11/08");
            SaneLocalSettings(settings, "birthplace", "Towcester, Northamptonshire, UK");
            SaneLocalSettings(settings, "website", "http://sourceforge.net/projects/aimlbot");
            AdminEmail = SaneLocalSettings(settings, "adminemail", "");
            SaneLocalSettings(settings, "islogging", "False");
            SaneLocalSettings(settings, "willcallhome", "False");
            SaneLocalSettings(settings, "timeout", "5000");
            SaneLocalSettings(settings, "timeoutmessage", "ERROR: The request has timed out.");
            SaneLocalSettings(settings, "culture", "en-US");
            SaneLocalSettings(settings, "splittersfile", "Splitters.xml");
            SaneLocalSettings(settings, "person2substitutionsfile", "Person2Substitutions.xml");
            SaneLocalSettings(settings, "personsubstitutionsfile", "PersonSubstitutions.xml");
            SaneLocalSettings(settings, "gendersubstitutionsfile", "GenderSubstitutions.xml");
            SaneLocalSettings(settings, "defaultpredicates", "DefaultPredicates.xml");
            SaneLocalSettings(settings, "substitutionsfile", "Substitutions.xml");
            SaneLocalSettings(settings, "aimldirectory", "aiml");
            SaneLocalSettings(settings, "configdirectory", "config");
            SaneLocalSettings(settings, "logdirectory", "logs");
            SaneLocalSettings(settings, "maxlogbuffersize", "64");
            SaneLocalSettings(settings, "notacceptinguserinputmessage",
                              "This Proccessor is currently set to not accept user input.");
            SaneLocalSettings(settings, "stripperregex", "[^0-9a-zA-Z]");

            SaneLocalSettings(settings, "systemlang", "bot");
            SaneLocalSettings(settings, "interp", "cloj");
        }

        internal static Unifiable SaneLocalSettings(ISettingsDictionary settings, string name, object value)
        {
            if (!settings.containsLocalCalled(name))
            {
                Unifiable sane = Unifiable.Create(value);
                settings.addSetting(name, sane);
                return sane;
            }
            Unifiable res = settings.grabSetting(name);
            return res;
        }

        /// <summary>
        /// Loads the splitters for this Proccessor from the supplied config file (or sets up some safe defaults)
        /// </summary>
        /// <param name="pathToSplitters">Path to the config file</param>
        private void loadSplitters(string pathToSplitters)
        {
            if (DontUseSplitters) return;
            if (HostSystem.FileExists(pathToSplitters))
            {
                XmlDocumentLineInfo splittersXmlDoc = new XmlDocumentLineInfo(pathToSplitters, true);
                Stream stream = HostSystem.OpenRead(pathToSplitters);
                try
                {
                    splittersXmlDoc.Load(stream);
                }
                finally
                {
                    HostSystem.Close(stream);
                }

                // the XML should have an XML declaration like this:
                // <?xml version="1.0" encoding="utf-8" ?> 
                // followed by a <root> tag with children of the form:
                // <item value="value"/>
                if (splittersXmlDoc.ChildNodes.Count == 2)
                {
                    if (splittersXmlDoc.LastChild.HasChildNodes)
                    {
                        foreach (XmlNode myNode in splittersXmlDoc.LastChild.ChildNodes)
                        {
                            if ((myNode.Name == "item") & (myNode.Attributes.Count == 1))
                            {
                                Unifiable value = Unifiable.Create(myNode.Attributes["value"].Value);
                                Splitters.Add(value);
                            }
                        }
                    }
                }
            }
            if (Splitters.Count == 0)
            {
                // if we process lisp and other things
                if (true) return;
                // we don't have any splitters, so lets make do with these...
                Splitters.Add(".");
                Splitters.Add("!");
                //this.Splitters.Add("?");
                Splitters.Add(";");
            }
        }

        #endregion

        // Persistent user tracking
        public readonly Dictionary<string, User> BotUsers = new Dictionary<string, User>();

        public void SetChatOnOff(string username, bool value)
        {
            lock (microBotUsersLock)
            {
                foreach (User u in BotUsers.Values)
                {
                    if (u.UserID.Contains(username) || username.Contains(u.UserID))
                        u.RespondToChat = value;
                }
            }
        }

        public ICollection<User> SetOfUsers
        {
            get
            {
                List<User> list = new List<User>();
                lock (BotUsers) foreach (var user in BotUsers.Values)
                    {
                        if (list.Contains(user)) continue;
                        list.Add(user);
                    }
                return list;
            }
        }

        public void AddAiml(string aimlText)
        {
            AddAiml(DefaultStartGraph, aimlText);
        }

        public void AddAiml(GraphMaster graph, string aimlText)
        {
            AddAiml(graph, aimlText, GetBotRequest("AddAiml into '" + graph + "' '" + aimlText + "'"));
        }

        public void AddAiml(GraphMaster graph, string aimlText, Request request)
        {
            GraphMaster prev = request.Graph;
            try
            {
                request.Graph = graph;
                LoaderOptions loader = request.LoadOptions.Value; // LoaderOptions.GetDefault(request);
                loader.CtxGraph = graph;
                loader.Loading0 = "from_text";
                string s = string.Format("<aiml graph=\"{0}\">{1}</aiml>", graph.ScriptingName, aimlText);
                request.Loader.loadAIMLString(s, loader);
            }
            catch (Exception e)
            {
                writeDebugLine("" + e);
                writeChatTrace("" + e);
                writeToLog(e);
                throw e;
            }
            finally
            {
                request.Graph = prev;
            }
        }


        public IEnumerable<XmlNode> EvalAiml(XmlNode currentNode, Request request, OutputDelegate del)
        {
            var nodes = new HashSet<XmlNode>();
            bool evaledNode = false;
            del = del ?? request.WriteLine;
            var getEvaluators = GetEvaluators(currentNode);
            foreach (XmlNodeEval funct in getEvaluators)
            {
                evaledNode = true;
                var newNode = funct(currentNode, request, del);
                if (newNode != null)
                {
                    evaledNode = true;
                    foreach (XmlNode node in newNode)
                    {
                        nodes.Add(node);
                    }
                }
            }
            if (evaledNode)
            {
                del("evaledNode=" + evaledNode);
                del("nodes.Count=" + nodes.Count);
                int nc = 1;
                foreach (XmlNode n in nodes)
                {
                    del("node {0}:{1}", nc, n);
                    nc++;
                }
                return nodes;
            }
            return XmlNodeEvaluatorImpl.NO_XmlNode;
        }

        #region Serialization

        /// <summary>
        /// Loads a dump of all graphmaster into memory so avoiding processing the AIML files again
        /// </summary>
        /// <param name="path">the path to the dump file</param>
        public void ScanAndLoadGraphs()
        {
            loadFromBinaryFile(GraphsSaveDir);
        }
        /// <summary>
        /// Saves the graphmaster node (and children) to a binary file to avoid processing the AIML each time the 
        /// Proccessor starts
        /// </summary>
        /// <param name="path">the path to the file for saving</param>
        public void SaveLoadedGraphs()
        {
            saveToBinaryFile(GraphsSaveDir);
        }

        private static string GraphsSaveDir
        {
            get { return "graphbins"; }
        }
        /// <summary>
        /// Saves the graphmaster node (and children) to a binary file to avoid processing the AIML each time the 
        /// Proccessor starts
        /// </summary>
        /// <param name="path">the path to the file for saving</param>
        public void saveToBinaryFile(string path)
        {
            BinaryFormatter bf = Unifiable.GetBinaryFormatter();
            string binext = ".gfxbin";
            string localdir = Path.Combine(path, NamePath);
            Unifiable.SaveUnifiables(Path.Combine(path, "unifiables"), bf);
            foreach (var name in SetOfGraphs)
            {
                bf = Unifiable.GetBinaryFormatter();
                name.saveToBinaryFile(Path.Combine(path, name.ScriptingName + binext), bf);
            }
            if (!Directory.Exists(localdir)) Directory.CreateDirectory(localdir);
            foreach (var name in SetOfLocalGraphs)
            {
                name.saveToBinaryFile(Path.Combine(localdir, name.ScriptingName + binext), bf);
            }
        }

        /// <summary>
        /// Loads a dump of the graphmaster into memory so avoiding processing the AIML files again
        /// </summary>
        /// <param name="path">the path to the dump file</param>
        public void loadFromBinaryFile(string path)
        {
            BinaryFormatter bf = Unifiable.GetBinaryFormatter();
            string binext = ".gfxbin";
            string localdir = Path.Combine(path, NamePath);
            Unifiable.LoadUnifiables(Path.Combine(path, "unifiables"), bf);
            foreach (string s in Directory.GetFiles(path, "*" + binext))
            {
                var graphname = Path.GetFileNameWithoutExtension(s).ToLower();
                var G = GetGraph(graphname, null);
                G.loadFromBinaryFile(s, bf);
                foreach (string gn in G.GraphNames)
                {
                    GraphsByName[gn] = G;
                }
            }
            if (Directory.Exists(localdir))
            {
                foreach (string s in Directory.GetFiles(localdir, "*" + binext))
                {
                    var graphname = Path.GetFileNameWithoutExtension(s).ToLower();
                    var G = GetGraph(graphname, null);
                    G.loadFromBinaryFile(s, bf);
                    foreach (string gn in G.GraphNames)
                    {
                        LocalGraphsByName[gn] = G;
                    }
                }
            }
        }

        #endregion


        #region Phone Home

        /// <summary>
        /// Attempts to send an email to the botmaster at the AdminEmail address setting with error messages
        /// resulting from a query to the Proccessor
        /// </summary>
        /// <param name="errorMessage">the resulting error message</param>
        /// <param name="request">the request object that encapsulates all sorts of useful information</param>
        public void phoneHome(Unifiable errorMessage, Request request)
        {
            if (AdminEmail == "")
            {
                return;
            }
            MailMessage msg = new MailMessage("donotreply@aimlbot.com", AdminEmail);
            msg.Subject = "WARNING! AIMLBot has encountered a problem...";
            string message =
                @"Dear Botmaster,

This is an automatically generated email to report errors with your Proccessor.

At *TIME* the Proccessor encountered the following error:

""*MESSAGE*""

whilst processing the following input:

""*RAWINPUT*""

from the user with an id of: *USER*

The normalized paths generated by the raw input were as follows:

*PATHS*

Please check your AIML!

Regards,

The AIMLbot program.
";
            message = message.Replace("*TIME*", DateTime.Now.ToString());
            message = message.Replace("*MESSAGE*", errorMessage);
            message = message.Replace("*RAWINPUT*", request.rawInput);
            message = message.Replace("*USER*", request.Requester.UserID);
            StringAppendableUnifiableImpl paths = Unifiable.CreateAppendable();
            foreach (Unifiable path in request.CurrentResult.InputPaths)
            {
                paths.Append(path.LegacyPath + Environment.NewLine);
            }
            message = message.Replace("*PATHS*", Unifiable.ToVMString(paths));
            msg.Body = message;
            msg.IsBodyHtml = false;
            try
            {
                if (msg.To.Count > 0)
                {
                    SmtpClient client = new SmtpClient();
                    client.Send(msg);
                }
            }
            catch
            {
                // if we get here then we can't really do much more
            }
        }

        #endregion

        private object EvalAIMLHandler(string cmd, Request user)
        {
            XmlNode node = StaticAIMLUtils.getTemplateNode(cmd);
            LineInfoElementImpl.unsetReadonly(node);
            if (Loader == null)
            {
                Loader = new AIMLLoader(this, GetBotRequest("EvalAIMLHandler " + cmd));
            }
            var res = ImmediateAiml(node, user, Loader);
            return res;
        }


        private object ClojExecHandler(string cmd, Request user)
        {
            ClojureInterpreter cloj = clojureInterpreter;
            lock (cloj)
            {
                bool hasUser = cloj.IsSubscriberOf("MyUser");

                if (hasUser)
                {
                    object o = cloj.GetSymbol("MyUser");
                    object r = cloj.Eval(o);
                    if (user.Requester != null && r != user.Requester)
                    {
                        cloj.Intern("MyUser", user.Requester);
                    }
                }
                else
                {
                    if (user.Requester != null)
                    {
                        cloj.Intern("MyUser", user.Requester);
                    }
                }

                StringReader stringCodeReader = new StringReader(cmd);
                object lispCode = cloj.Read("ClojExecHandler", stringCodeReader, writeToLog);
                if (cloj.Eof(lispCode))
                    return "EOF on " + lispCode ?? "NULL";
                return cloj.Eval(lispCode);
            }
        }        
#if USE_SWIPROLOG
        private object SWIExecHandler(string cmd, Request user)
        {
            ScriptInterpreter swi = swiInterpreter;
            if (swi == null) return null;
            lock (swi)
            {
                bool hasUser = swi.IsSubscriberOf("MyUser");

                if (hasUser)
                {
                    object o = swi.GetSymbol("MyUser");
                    object r = swi.Eval(o);
                    if (user.Requester != null && r != user.Requester)
                    {
                        swi.Intern("MyUser", user.Requester);
                    }
                }
                else
                {
                    if (user.Requester != null)
                    {
                        swi.Intern("MyUser", user.Requester);
                    }
                }

                StringReader stringCodeReader = new StringReader(cmd);
                object lispCode = swi.Read("SWIExecHandler", stringCodeReader, writeToLog);
                if (swi.Eof(lispCode))
                    return "EOF on " + lispCode ?? "NULL";
                return swi.Eval(lispCode);
            }
        }
#endif
        internal Unifiable SystemExecute(Unifiable cmd, Unifiable langu, Request user)
        {
            if (IsNullOrEmpty(langu))
            {
                langu = GlobalSettings.grabSettingOrDefault("systemlang", "bot");
            }
            else
            {
                langu = ToLower(Trim(langu));
            }
            Unifiable s = "The system tag should be doing '" + cmd + "' lang=" + langu;
            writeToLog(s.AsString());
            SystemExecHandler handler;
            if (SettingsDictionaryReal.TryGetValue(ExecuteHandlers, langu, out handler))
            {
                try
                {
                    object o = handler(cmd, user);
                    return Unifiable.Create(o);
                }
                catch (Exception e)
                {
                    writeToLog(e);
                    return Unifiable.Empty;
                }
            }
            else
            {
                try
                {
                    object self = user;
                    ScriptInterpreter si = ScriptManager.LoadScriptInterpreter(langu, self, null);
                    object o = ScriptManager.EvalScriptInterpreter(cmd.ToValue(user.CurrentQuery), langu, self, si,
                                                                   writeToLog);
                    string siStr = si.Str(o);
                    return Unifiable.Create(siStr);
                }
                catch (Exception e)
                {
                    writeToLog(e);
                }
            }
            writeToLog(s);
            return Unifiable.Empty;
        }


        internal readonly Dictionary<string, SystemExecHandler> ExecuteHandlers =
            new Dictionary<string, SystemExecHandler>();

        public void AddExcuteHandler(string lang, SystemExecHandler handler)
        {
            lang = ToLower(Trim(lang));
            ExecuteHandlers[lang] = handler;
        }


        /// <summary>
        /// Returns the value of a setting given the name of the setting
        /// </summary>
        /// <param name="name">the name of the setting whose value we're interested in</param>
        /// <returns>the value of the setting</returns>
        public Unifiable GetBotSetting(Unifiable name)
        {
            return GlobalSettings.grabSetting(name);
        }

        public Unifiable NOTOPIC
        {
            get
            {
                if (!GlobalSettings.containsSettingCalled("notopic")) return Unifiable.EnglishNothing;
                return GlobalSettings.grabSettingNoDebug("notopic");
            }
        }


        static public IEnumerable<GraphMaster> SetOfGraphs
        {
            get
            {
                lock (AltBot.GraphsByName) return new ListAsSet<GraphMaster>(GraphMaster.CopyOf(AltBot.GraphsByName).Values);
            }
        }

        public IEnumerable<GraphMaster> SetOfLocalGraphs
        {
            get
            {
                lock (LocalGraphsByName) return new ListAsSet<GraphMaster>(GraphMaster.CopyOf(LocalGraphsByName).Values);
            }
        }


        public static Dictionary<string, GraphMaster> GraphsByName = new Dictionary<string, GraphMaster>();
        public Dictionary<string, GraphMaster> LocalGraphsByName = new Dictionary<string, GraphMaster>();
        public CycDatabase TheCyc;
        public NatLangDb TheNLKB;
        public bool UseInlineThat = true;

        public bool CycEnabled
        {
            get { return TheCyc.CycEnabled; }
            set { TheCyc.CycEnabled = value; }
        }

        public static bool SaveProofs;

        public GraphMaster GetUserGraph(string graphPath)
        {
            var changed = ToGraphPathName(graphPath, BotID);
            if (changed != graphPath)
            {
                writeToLog("ERROR GetUserGraph " + graphPath + " -> " + changed);
                graphPath = changed;
            }
            lock (GraphsByName)
            {
                GraphMaster g;
                if (LocalGraphsByName.TryGetValue(graphPath, out g))
                {
                    return g;
                }
            }
            return FindGraph(graphPath, null);
        }

        static public GraphMaster FindGlobalGraph(string graphPath)
        {
            GraphMaster g;
            lock (GraphsByName) GraphsByName.TryGetValue(graphPath, out g);
            return g;
        }

        public GraphMaster GetGraph(string graphPath, GraphMaster current)
        {
            GraphMaster g = FindGraph(graphPath, current);
            if (g != null) return g;            
            if (graphPath == null)
            {
                if (current == null)
                {
                    writeToLog("ERROR GetGraph Null");
                }
                return current;
            }

            string lower = graphPath.ToLower();
            int graphPathLength = graphPath.IndexOf(".");
            if (graphPathLength > 0)
            {
                string sv = graphPath.Substring(0, graphPathLength);
                string left = graphPath.Substring(graphPathLength + 1);
                var vg = GetGraph(sv, current);
                return GetGraph(left, vg);
            }

            graphPath = ToScriptableName(graphPath);
            lock (GraphsByName)
            {
                if (LocalGraphsByName.TryGetValue(graphPath, out g))
                {
                    return g;
                }
                if (!GraphsByName.TryGetValue(graphPath, out g))
                {
                    g = LocalGraphsByName[graphPath] = GraphMaster.FindOrCreate(graphPath);
                }
            }
            return g;
        }

        public GraphMaster FindGraph(string graphPath, GraphMaster current)
        {            
            if (graphPath == null || (",current,*,,".Contains("," + graphPath.ToLower() + ",")))
            {
                if (current == null)
                {
                    writeToLog("ERROR GetGraph Null");
                }
                return current;
            }

            string lower = graphPath.ToLower();
            int graphPathLength = graphPath.IndexOf(".");
            if (graphPathLength > 0)
            {
                string sv = graphPath.Substring(0, graphPathLength);
                string left = graphPath.Substring(graphPathLength + 1);
                var vg = FindGraph(sv, current);
                return FindGraph(left, vg);
            }

            graphPath = ToGraphPathName(graphPath, NamePath);
            if (true)
            {
                if (_g != null && graphPath == "default")
                {
                    return DefaultStartGraph;
                }
                if (_g != null && graphPath == "*")
                {
                    return DefaultStartGraph;
                }
                if (_h != null && graphPath == "heardselfsay")
                {
                    return DefaultHeardSelfSayGraph;
                }
                if (TheUserListenerGraph != null && graphPath == "heardyousay")
                {
                    return DefaultHeardYouSayGraph;
                }
            }
            if (graphPath == "parent" || graphPath == "parallel")
            {
                if (current == null) return null;
                if (current.CannotHaveParallel) return current;
                return current.Parallel;
            }

            GraphMaster g;
            lock (GraphsByName)
            {
                if (LocalGraphsByName.TryGetValue(graphPath, out g))
                {
                    return g;
                }
                if (!GraphsByName.TryGetValue(graphPath, out g))
                {
                    return null;
                }
            }
            return g;
        }

        public static string ToGraphPathName(string graphPath0, string botname)
        {
            var graphPath = HelperForMerge.RemoveEnd(graphPath0);
            if (string.IsNullOrEmpty(graphPath) || graphPath == "*" || graphPath == "current")
            {
                return null;
            }
            graphPath = ToScriptableName(graphPath);
            if (botname != null)
            {
                var sbotname = ToScriptableName(botname);
                if (botname == graphPath || botname == graphPath)
                {
                    return "default";
                }
            }
            if (graphPath == "default" || graphPath == "base" || graphPath == "*" || graphPath == "start" ||
                graphPath == "root")
            {
                return "default";
            }
            if (graphPath == "heardselfsay" || graphPath == "heardself")
            {
                return "heardselfsay";
            }
            if (graphPath == "heardyousay" || graphPath == "heardyou")
            {
                return "listener";
            }
            return graphPath;
        }

        public static string ToScriptableName(string path)
        {
            string sk = "";
            if (path.StartsWith("is_")) path = path.Substring(3);
            if (path.StartsWith("was_")) path = path.Substring(4);
            foreach (char s in path)
            {
                if (IsOkForNameChar(s))
                    sk += s;
            }
            path = OlderReference(path, sk);
            return NoSpaceLowerCaseName(path);
        }

        public static int DivideString(string args, string sep, out string left, out string right)
        {
            if (args == null)
            {
                left = "";
                right = null;
                return 0;
            }
            args = args.Trim();
            if (args.Length == 0)
            {
                left = args;
                right = null;
                return 1;
            }
            int lastIndex = args.IndexOf(sep);
            if (lastIndex == -1)
            {
                left = args;
                right = null;
                return 1;
            }
            int seplen = sep.Length;
            left = Trim(args.Substring(0, lastIndex));
            right = Trim(args.Substring(lastIndex + seplen));
            if (right.Length == 0) return 1;
            return 2;
        }

        public string GetUserMt(User user, SubQuery subquery)
        {
            Unifiable ret = user.Predicates.grabSettingNoDebug("mt");
            if (!IsNullOrEmpty(ret))
            {
                string v = ret.ToValue(subquery);
                if (v != null && v.Length > 1) return TheCyc.Cyclify(v);
            }
            //GetAttribValue("mt","");
            return "#$BaseKB";
        }

        public void WriteConfig()
        {
            lock (BotUsers) ///lock (OnBotCreatedHooks)
            {
                TheCyc.WriteConfig();
                DefaultStartGraph.WriteConfig();
                writeDebugLine("Bot loaded");
            }
        }

        public string LoadPersonalDirectory(string myName)
        {
            return LoadPersonalDirectory0(myName);
            //return UserOper(() => LoadPersonalDirectory0(myName), QuietLogger);
        }

        private string LoadPersonalDirectory0(string myName)
        {
            ReloadHooks.Add(() => LoadPersonalDirectory(myName));
            string loaded = null;

            // this is the personal "config file" only.. aiml stored elsewhere
            string file = HostSystem.Combine("config", myName);
            Request request = GetBotRequest("loading personal directory " + myName);
            if (HostSystem.DirExists(file))
            {
                loaded = file;
                loadSettingsFileAndDir(HostSystem.Combine(file, "Settings.xml"), request);
            }

            file = HostSystem.Combine("aiml", myName);
            if (HostSystem.DirExists(file))
            {
                UsePersonalDir(file);
                ;
                loaded = file;
            }

            // this is the personal "config file" only.. aiml stored elsewhere
            file = HostSystem.Combine(myName, "config");
            if (HostSystem.DirExists(file))
            {
                loaded = file;
                loadSettingsFileAndDir(HostSystem.Combine(file, "Settings.xml"), request);
            }

            file = HostSystem.Combine(myName, "aiml");
            if (HostSystem.DirExists(file))
            {
                UsePersonalDir(file);
                ;
                loaded = file;
            }
            return loaded;
        }

        private void loadSettingsFileAndDir(string file, Request request)
        {
            writeToLog("LoadPersonalDirectories: '{0}'", file);
            loadSettingsFile(HostSystem.Combine(file, "Settings.xml"), request);
            loadConfigs(this, file, request);
        }

        public void UsePersonalDir(string file)
        {
            //lock (BotUsers) lock (OnBotCreatedHooks) 
            UsePersonalDir0(file);
        }

        private void UsePersonalDir0(string file)
        {
            if (!HostSystem.DirExists(file))
            {
                writeToLog("ERROR - cannot use non existent personal dir = " + file);
                return;
            }
            PushSearchPath(file);
            _PathToBotPersonalFiles = file;
            string s = string.Format("-LoadPersonalDirectories: '{0}'-", file);
            Request request = GetBotRequest(s);
            request.LoadingFrom = file;
            writeToLog(s);
            bool prev = request.GraphsAcceptingUserInput;
            try
            {
                // loading of personal configs must be done before and after the AIML files
                loadConfigs(this, file, request);
                request.GraphsAcceptingUserInput = false;
                loadAIMLFromURI(file, request);
                foreach (string s1 in HostSystem.GetFiles(file, "Settings*.xml"))
                {
                    loadSettingsFile(s1, request);
                }
                loadConfigs(this, file, request);
                lock (RuntimeDirectoriesLock)
                {
                    _RuntimeDirectories = RuntimeDirectories;
                }
            }
            finally
            {
                request.GraphsAcceptingUserInput = prev;
            }
        }

        public string SetName(string myName)
        {
            lock (OnBotCreatedHooks)
            {
                return SetName0(myName);
                //return UserOper(() => SetName0(myName), writeDebugLine);
            }
        }

        private string SetName0(string myName)
        {
            //char s1 = myName[1];
            Robots[myName] = this;
            NameAsSet = myName;
            //new AIMLbot.User("heardselfsay", this)
            var thisBotAsUser = FindOrCreateUser(myName);
            this.BotAsUser = thisBotAsUser;
            clojureInterpreter.Intern("BotAsUser", thisBotAsUser);
            thisBotAsUser.IsRoleAcct = true;
            SharedGlobalSettings = this.GlobalSettings;
            thisBotAsUser.Predicates = MakeSettingsDictionary(myName);
            ///, this, () => SharedGlobalSettings)
            thisBotAsUser.Predicates.InsertFallback(() => AllUserPreds);
            AllUserPreds.InsertFallback(() => SharedGlobalSettings);

            GlobalSettings.IsTraced = true;
            GlobalSettings = (SettingsDictionaryReal) thisBotAsUser.Predicates;
            //BotAsUser.UserDirectory = "aiml/users/heardselfsay";
            //BotAsUser.UserID = "heardselfsay";
            //BotAsUser.UserName = "heardselfsay";
            //BotUsers["heardselfsay"] = BotAsUser;            
            thisBotAsUser.UserName = myName;
            AllDictionaries["bot"] = thisBotAsUser.Predicates;
            thisBotAsUser.removeSetting("userdir");
            NamePath = ToScriptableName(NameAsSet);
            thisBotAsUser.UserID = NamePath;
            this.StartHttpServer();

            //var OnTaskAtATimeHandler = HeardSelfSayQueue = thisBotAsUser.OnTaskAtATimeHandler;
            //OnTaskAtATimeHandler.Name = "TaskQueue For " + myName;

            thisBotAsUser.SaveDirectory(thisBotAsUser.UserDirectory);
            lock (GraphsByName)
            {
                foreach (var cn in new object[]
                                       {
                                           DefaultEventGraph, 
                                           DefaultPredicates, 
                                           HeardPredicates,
                                           DefaultStartGraph,
                                           DefaultHeardSelfSayGraph,
                                       })
                {
                    if (cn == null)
                    {

                    }
                }
                GraphMaster listeningGraph = DefaultHeardSelfSayGraph;
                if (listeningGraph != null) BotAsUser.HeardSelfSayGraph = listeningGraph;
            }
            lock (OnBotCreatedHooks)
            {
                foreach (Action list in OnBotCreatedHooks)
                {
                    try
                    {
                        list();
                    }
                    catch (Exception e)
                    {
                        writeDebugLine("OnBotCreatedHooks ERROR: " + list);
                    }
                }
                OnBotCreatedHooks.Clear();
            }
            loadAIMLFromDefaults0();
            EnsureDefaultUsers();
            string official = LoadPersonalDirectories(myName);
            thisBotAsUser.SaveDirectory(thisBotAsUser.UserDirectory);
            AddExcuteHandler(NamePath, ChatWithThisBot);
            return official ?? thisBotAsUser.UserDirectory;
        }

        public static bool StaticInitStarted;
        public static object OneAtATime = new object();

        private static void EnsureStaticInit()
        {
            lock (OneAtATime)
            {
                var tc = DLRConsole.TransparentCallers;
                lock (tc)
                {
                    tc.Add(typeof(AltBot));
                    tc.Add(typeof(AIMLbot.MasterRequest));
                    // ReSharper disable AssignNullToNotNullAttribute
                    tc.Add(typeof(MasterResult).BaseType);
                    // ReSharper restore AssignNullToNotNullAttribute
                    tc.Add(typeof(Request));
                }

                TagHandlerProcessor.InitTagHandlers();

                if (StaticInitStarted) return;
                StaticInitStarted = true;
                GraphsByName["listener"] = TheUserListenerGraph = GraphMaster.FindOrCreate("listener");
                TheUserListenerGraph.SilentTagsInPutParallel = false;
                // var defaultGraph = GraphsByName["default"] = GraphMaster.FindOrCreate("default");
                // defaultGraph.RemovePreviousTemplatesFromNodes = false;
                ///GraphsByName["heardselfsay"] = Defau;////new GraphMaster("heardselfsay");
                AddSettingsAliases("lastuserid", "you");
                AddSettingsAliases("lastusername", "you");
                AddSettingsAliases("you", "lastusername");
                AddSettingsAliases("he", "him");
                AddSettingsAliases("she", "her");
            }
        }

        private static void AddSettingsAliases(string real, string aliases)
        {
            SettingsAliases.Add(real, aliases.Split(",".ToCharArray(), StringSplitOptions.RemoveEmptyEntries));
        }

        public string LoadPersonalDirectories(string myName)
        {
            return LoadPersonalDirectories0(myName);
        }

        public string LoadPersonalDirectories0(string myName)
        {
            string loaded = LoadPersonalDirectory(myName);
            if (string.IsNullOrEmpty(loaded))
            {
                myName = ToScriptableName(myName);
                loaded = LoadPersonalDirectory(myName);
            }
            if (string.IsNullOrEmpty(loaded))
            {
                writeToLog("Didnt find personal directories with stem: '{0}'", myName);
            }
            return loaded;
        }

        readonly public static OutputDelegate writeDebugLine = writeDebugLine_0_;
        internal static void writeDebugLine_0_(string message, params object[] args)
        {
            bool printIt = false;
            lock (LoggedWords)
            {
                printIt = LoggedWords.writeDebugLine(DLRConsole.DebugWriteLine, message, args);
            }
            //
            {
                try
                {
                    bool wasStopped = true;
                    string real = SafeFormat(message, args);
                    message = real.ToUpper();
                    if (!real.StartsWith("WARNING"))
                    {
                        if (message.ContainsAny("warn", "= null", "error", "bad") > -1)
                        {
                            writeToLogWarn("BAD " + message, args);
                            return;
                        }
                    }

                    if (message.Contains("ERROR") && !message.Contains("TIMEOUTMESSAGE"))
                    {
                        wasStopped = Breakpoint(real);
                    }
                    else if (message.Contains("EXCEPTION"))
                    {
                        wasStopped = Breakpoint(real);
                    }
                    if (!printIt)
                    {
                        if (!wasStopped)
                        {
                            DLRConsole.DebugWriteLine(real);
                            return;
                        }
                        UnseenWriteline(real);
                    }
                }
                catch (Exception)
                {
                }
            }
        }

        public static bool Breakpoint(string err)
        {
            if (skipMany > 0)
            {
                skipMany--;
                return false;
            }
            DLRConsole.SystemWriteLine("" + err);
            if (!UseBreakpointOnError)
            {
                return false;
            }
            DLRConsole.SystemWriteLine("press enter of enter a number to skip breakpoints");
            string p = DLRConsole.ReadLine();
            int skipNext;
            if (int.TryParse(p, out skipNext))
            {
                skipMany = skipNext;
            }
            return true;
        }

        public bool SameUser(string old, string next)
        {
            old = old ?? "";
            next = next ?? "";
            old = Trim(ToLower(old));
            next = Trim(ToLower(next));
            return FindUser0(old) == FindUser0(next);
        }

        private ClojureInterpreter clojureInterpreter;
#if USE_SWIPROLOG
        private PrologScriptInterpreter swiInterpreter;
#endif
        private List<string> _RuntimeDirectories;
        ICollectionRequester _objr;
        private readonly List<Action> PostObjectRequesterSet = new List<Action>();
        //private AltBot TheAltBot;

        public ICollectionRequester ObjectRequester
        {
            get
            {
                if (_objr == null)
                {
                    logText("Warn no ObjectRequester");
                }
                return _objr;
            }
            set
            {
                _objr = value;
                if (value != null)
                {
                    lock (PostObjectRequesterSet)
                    {
                        foreach (var set in PostObjectRequesterSet)
                        {
                            set();
                        }
                        PostObjectRequesterSet.Clear();
                    }
                }
            }
        }
        #region Overrides of QuerySettings

        /*
        /// <summary>
        /// The Graph to start the query on
        /// </summary>
        public override string GraphName
        {
            get { return GraphMaster.ScriptingName; }
            set { throw new NotImplementedException(); }
        }
        */
        public string UserID
        {
            get
            {
                if (BotAsUser != null) return BotAsUser.UserID;
                SettingsDictionary dict = GlobalSettings;
                if (dict != null)
                {
                    Unifiable botid = dict.grabSettingNoDebug("id");
                    return botid;
                }
                return null;
            }
        }

        public string BotID
        {
            get
            {
                if (BotAsUser != null) return BotAsUser.UserID;
                return UserID ?? "-BOT-ID-NULL-";
            }
            set { throw new NotImplementedException(); }
        }

        public ISettingsDictionary Predicates
        {
            get { return GlobalSettings; }
        }

        #endregion

        public ISettingsDictionary GetDictionary(string name)
        {
            var idict = GetDictionary0(name);
            if (idict!=null) return idict;
            var rtpbotobjCol = ScriptManager.ResolveToObject(this, name);
            if (rtpbotobjCol == null || rtpbotobjCol.Count == 0)
            {
                return null;
            }
            //if (tr)
            foreach (object o in rtpbotobjCol)
            {
                ParentProvider pp = o as ParentProvider;
                ISettingsDictionary pi = o as ISettingsDictionary;
                User pu = o as User;
                if (pp != null)
                {
                    pi = pp() as ISettingsDictionary;
                }
                if (pi != null)
                {
                    return pi;
                }
                if (pu != null)
                {
                    return pu;
                }
            }
            return null;
        }

        public ISettingsDictionary GetDictionary0(string named)
        {
            Func<ISettingsDictionary, SettingsDictionaryReal> SDCAST = SettingsDictionaryReal.ToSettingsDictionary;
            //dict = FindDict(type, query, dict);
            if (named == null) return null;
            string key = named.ToLower().Trim();
            if (key == "") return null;
            lock (AllDictionaries)
            {
                ISettingsDictionary dict;
                if (AllDictionaries.TryGetValue(key, out dict))
                {
                    return dict;
                }
            }
            if (key == "predicates")
            {
                return SDCAST(this.AllUserPreds);
            }
            // try to use a global blackboard predicate
            User gUser = ExemplarUser;
            if (key == "globalpreds") return SDCAST(gUser);
            if (key == "allusers") return SDCAST(AllUserPreds);
            var path = named.Split(new[] { '.' });
            if (path.Length == 1)
            {
                User user = FindUser(key);
                if (user != null) return user;
            }
            else
            {
                if (path[0] == "bot" || path[0] == "users" || path[0] == "char" || path[0] == "nl")
                {
                    ISettingsDictionary f = GetDictionary(string.Join(".", path, 1, path.Length - 1));
                    if (f != null) return SDCAST(f);
                }
                if (path[0] == "substitutions")
                {
                    ISettingsDictionary f = GetDictionary(string.Join(".", path, 1, path.Length - 1), "substitutions",
                                                          true);
                    if (f != null) return SDCAST(f);
                }
                else
                {
                    ISettingsDictionary f = GetDictionary(path[0]);
                    if (f != null)
                    {
                        SettingsDictionaryReal sd = SDCAST(f);
                        ParentProvider pp = sd.FindDictionary(string.Join(".", path, 1, path.Length - 1), null);
                        if (pp != null)
                        {
                            object pi;
                            pi = pp();
                            if (pi != null) return SDCAST((ISettingsDictionary) pi);
                        }
                    }
                }
            }
            return null;
        }

        public ISettingsDictionary GetDictionary(string named, string type, bool createIfMissing)
        {
            lock (AllDictionaries)
            {
                string key = (type + "." + named).ToLower();
                ISettingsDictionary dict;
                if (!AllDictionaries.TryGetValue(key, out dict))
                {
                    ISettingsDictionary sdict = GetDictionary(named);
                    if (sdict != null) return sdict;
                    if (createIfMissing)
                    {
                        dict = AllDictionaries[key] = AllDictionaries[named] = MakeSettingsDictionary(named);
                        User user = ExemplarUser ?? BotAsUser;
                        Request r = //user.CurrentRequest ??
                                    user.CreateRequest(
                                        "@echo <!-- loadDictionary '" + named + "' from '" + type + "' -->", Unifiable.EnglishNothing, BotAsUser);
                        loadDictionary(dict, named, type, r);
                    }
                }
                return dict;
            }
        }

        private void loadDictionary(ISettingsDictionary dictionary, string path, string type, Request r0)
        {
            User user = LastUser ?? 
                ExemplarUser ?? BotAsUser;
            Request r = r0 ??
                        //user.CurrentRequest ??
                        user.CreateRequest(
                            "@echo <!-- loadDictionary '" + dictionary + "' from '" + type + "' -->", Unifiable.EnglishNothing,
                            BotAsUser);
            int loaded = 0;
            foreach (string p in GetSearchRoots(r))
            {
                foreach (string s0 in new[] { "", type, type + "s", })
                {
                    foreach (string s1 in new[] { "", "." + type, ".xml", ".subst", ".properties", })
                    {
                        string named = HostSystem.Combine(p, path + s0 + s1);
                        if (HostSystem.FileExists(named))
                        {
                            try
                            {
                                SettingsDictionaryReal.loadSettingsNow(
                                    dictionary, Path.GetDirectoryName(named), Path.GetFileName(named),
                                    SettingsPolicy.Default, r);
                                loaded++;
                                break;
                            }
                            catch (Exception e)
                            {
                                writeToLog("ERROR {0}", e);
                                //continue;
                                throw;
                            }
                        }
                    }
                }
                if (loaded > 0) return;
            }
            if (loaded == 0)
            {
                writeToLog("WARNING: Cannot find " + path + " for " + type);
            }
        }

        public void RegisterDictionary(ISettingsDictionary dict)
        {
            RegisterDictionary(dict.NameSpace, dict);
        }
        public void RegisterDictionary(string named, ISettingsDictionary dict)
        {
            named = named.ToLower().Trim().Replace("  ", " ");
            string key = named.Replace(" ", "_");
            RegisterDictionary(named, dict, true);
        }

        public void RegisterDictionary(string key, ISettingsDictionary dict, bool always)
        {
            Action needsExit = LockInfo.MonitorTryEnter("RegisterDictionary " + key, AllDictionaries, MaxWaitTryEnter);
            try
            {
                var path = key.Split(new[] { '.' });
                if (always || !AllDictionaries.ContainsKey(key)) AllDictionaries[key] = dict;
                if (path.Length > 1)
                {
                    if (path[0] == "bot" || path[0] == "users" || path[0] == "char" || path[0] == "nl")
                    {
                        string join = string.Join(".", path, 1, path.Length - 1);
                        RegisterDictionary(join, dict, false);
                    }
                }
            }
            finally
            {
                needsExit();
            }
        }

        private void RegisterSubstitutions(string named, ISettingsDictionary dict)
        {
            dict.IsTraced = false;
            RegisterDictionary("substitutions" + "." + named, dict);
        }

        protected IEnumerable GetSearchRoots(Request request)
        {
            lock (RuntimeDirectoriesLock)
            {
                var searchWas = RuntimeDirectories;

                PushSearchPath(PathToUserDir);
                PushSearchPath(PathToConfigFiles);
                PushSearchPath(RuntimeDirectory);
                PushSearchPath(PathToAIML);
                PushSearchPath(_PathToBotPersonalFiles);
                PushSearchPath(GetUserDir(request.Requester.UserID));

                var searchAt = RuntimeDirectories;
                _RuntimeDirectories = searchWas;
                return searchAt;
            }
        }


        public IEnumerable<XmlNodeEval> GetEvaluators(XmlNode node)
        {
            var nodes = new List<XmlNodeEval>();
            foreach (XmlNodeEvaluator xmlNodeEvaluator in XmlNodeEvaluators)
            {
                var nodeE = xmlNodeEvaluator.GetEvaluators(node);
                nodes.AddRange(nodeE);
            }
            return nodes;
        }

        #region IChatterBot Members

        public SystemExecHandler ChatWithHandler(string userName)
        {
            User theUser = FindOrCreateUser(userName);
            return (txt, req) =>
                       {
                           req.SetSpeakerAndResponder(theUser, BotAsUser);
                           var ret = ChatWithThisBot(txt, req);
                           return ret;
                       };
        }
        #endregion

        public long RunLowMemHooks()
        {
            long total = Unifiable.LowMemExpireUnifiableCaches();
            foreach (var graph in SetOfGraphs)
            {
                total += graph.RunLowMemHooks();
            }
            return total;
        }

        #region BlackBoard

        public void importBBBotSettings(string bbKey, string settingKey)
        {
            string myValue = myChemistry.m_cBus.getHash(bbKey);
            if (myValue.Length > 0)
            {
                GlobalSettings.updateSetting(settingKey, myValue);
            }
        }

        public void importBBUserSettings(User myUser, string bbKey, string settingKey)
        {
            string myValue = myChemistry.m_cBus.getHash(bbKey);
            if (myValue.Length > 0)
            {
                myUser.Predicates.updateSetting(settingKey, myValue);
            }
        }

        public void importBBUser(User myUser)
        {
            importBBUserSettings(myUser, "username", "name");
            importBBUserSettings(myUser, "userage", "age");
            importBBUserSettings(myUser, "userbirthday", "birthday");
            importBBUserSettings(myUser, "userboyfriend", "boyfriend");
            importBBUserSettings(myUser, "usergirlfriend", "girlfriend");
            importBBUserSettings(myUser, "userbrother", "brother");
            importBBUserSettings(myUser, "usersister", "sister");
            importBBUserSettings(myUser, "usercat", "cat");
            importBBUserSettings(myUser, "userdog", "dog");
            importBBUserSettings(myUser, "userfather", "father");
            importBBUserSettings(myUser, "userfavcolor", "favcolor");
            importBBUserSettings(myUser, "userfavmovie", "favmovie");
            importBBUserSettings(myUser, "userfriend", "friend");
            importBBUserSettings(myUser, "userfullname", "fullname");
            importBBUserSettings(myUser, "usergender", "gender");
            importBBUserSettings(myUser, "usergirlfriend", "girlfriend");
            importBBUserSettings(myUser, "userhas", "has");
            importBBUserSettings(myUser, "userheard", "heard");
            importBBUserSettings(myUser, "userhusband", "husband");
            importBBUserSettings(myUser, "useris", "is");
            importBBUserSettings(myUser, "userjob", "job");
            importBBUserSettings(myUser, "userlastname", "lastname");
            importBBUserSettings(myUser, "userlike", "like");
            importBBUserSettings(myUser, "userlocation", "location");
            importBBUserSettings(myUser, "userlooklike", "looklike");
            importBBUserSettings(myUser, "usermemory", "memory");
            importBBUserSettings(myUser, "usernickname", "nickname");
            importBBUserSettings(myUser, "usermiddlename", "middlename");
            importBBUserSettings(myUser, "usermother", "mother");
            importBBUserSettings(myUser, "userpersonality", "personality");
            importBBUserSettings(myUser, "usersign", "sign");
            importBBUserSettings(myUser, "userthought", "thought");
            importBBUserSettings(myUser, "userwant", "want");

            // Dynamic state info for condition/li
            importBBUserSettings(myUser, "fsmstate", "state");
            importBBUserSettings(myUser, "pmotion", "pmotion");
            importBBUserSettings(myUser, "porientation", "porientation");
            importBBUserSettings(myUser, "facename", "facename");
            importBBUserSettings(myUser, "TTSText", "TTSText");
            importBBUserSettings(myUser, "TTSVoice", "TTSVoice");


        }
        public void importBBBot()
        {
            importBBBotSettings("dollcharname", "name");
            importBBBotSettings("botmaster", "botmaster");
            importBBBotSettings("master", "master");
            importBBBotSettings("botlocation", "location");
            importBBBotSettings("botgender", "gender");
            importBBBotSettings("botbirthday", "birthday");
            importBBBotSettings("botbirthplace", "birthplace");
            importBBBotSettings("botnationality", "nationality");
            importBBBotSettings("botsign", "sign");

            importBBBotSettings("botgenus", "genus");
            importBBBotSettings("botspecies", "species");
            importBBBotSettings("botorder", "order");
            importBBBotSettings("botfamily", "family");
            importBBBotSettings("botphylum", "phylum");
            importBBBotSettings("botclass", "class");

            importBBBotSettings("botreligion", "religion");
            importBBBotSettings("botetype", "etype");
            importBBBotSettings("botorientation", "orientation");
            importBBBotSettings("botethics", "ethics");
            importBBBotSettings("botemotions", "emotions");
            importBBBotSettings("botfeelings", "feelings");
            importBBBotSettings("botwear", "wear");
            importBBBotSettings("botlooklike", "looklike");

            importBBBotSettings("botforfun", "forfun");
            importBBBotSettings("botfriend", "friend");
            importBBBotSettings("botboyfriend", "boyfriend");
            importBBBotSettings("botgirlfriend", "girlfriend");
            importBBBotSettings("botfriends", "friends");
            importBBBotSettings("bottalkabout", "talkabout");
            importBBBotSettings("botquestion", "question");

            importBBBotSettings("botparty", "party");
            importBBBotSettings("botpresident", "president");

            importBBBotSettings("botfavoritefood", "favoritefood");
            importBBBotSettings("botfavoritecolor", "favoritecolor");

            importBBBotSettings("botkindmusic", "kindmusic");
            importBBBotSettings("botfavoriteband", "favoriteband");

            importBBBotSettings("botfavoriteauthor", "favoriteauthor");
            importBBBotSettings("botfavoriteartist", "favoriteartist");
            importBBBotSettings("botfavoritemovie", "favoritemovie");
            importBBBotSettings("botfavoriteactor", "favoriteactor");
            importBBBotSettings("botfavoriteactress", "favoriteactress");

            importBBBotSettings("botcelebrity", "celebrity");
            importBBBotSettings("botcelebrities", "celebrities");

            importBBBotSettings("botfavoritesport", "favoritesport");
            importBBBotSettings("bothockeyteam", "hockeyteam");
            importBBBotSettings("botfootballteam", "footballteam");
            importBBBotSettings("botbaseballteam", "baseballteam");
        }

        public void exportBB()
        {
        }


        // The dictionary that is a mirror of the blackboard
        // We hit this for local applications 
        public bool bbSafe = false;
        public bool useMemcache = false;

        public Dictionary<string, string> BBDict = new Dictionary<string, string>();
        public void setBBHash(string key, string data)
        {
            if (key == null) return;
            string okey = key;
            try
            {
                //(SettingsDictionaryReal)
                if (key.StartsWith("bot"))
                {
                    var botAsUser = BotAsUser;
                    key = key.Substring(3);
                    if (botAsUser != null) botAsUser.Predicates.addSetting(key, data);
                }
                else if (key.StartsWith("user"))
                {
                    key = key.Substring(4);
                    if (LastUser != null) LastUser.Predicates.addSetting(key, data);
                }
                else
                {
                    if (GlobalSettings != null) GlobalSettings.addSetting(key, data);
                }
            }
            catch (Exception e)
            {
                Console.WriteLine("CHECK setBBHash({0},{1}) ERR:{2}", key, data, e.Message);
            }
            setBBHash0(okey, data);
        }
        public void setBBHash0(string key, string data)
        {
            //curBot.myChemistry.m_cBus.setHash(key,data);
            lock (BBDict) BBDict[key] = data;
            if (useMemcache)
            {
                if ((myChemistry != null) && (myChemistry.m_cBus != null))
                {
                    myChemistry.m_cBus.setHash(key, data);
                }
                else
                {
                    //                   Console.WriteLine("CHECK setBBHash0 :NO BUS ({0},{1})", key,data);
                }
            }
            else
            {
                //                Console.WriteLine("CHECK setBBHash0: useMemcache=false({0},{1})", key, data);
            }
        }
        public string getBBHash(string key)
        {
            if (string.IsNullOrEmpty(key)) return "";

            string gs = getBBHash0(key);
            if (!string.IsNullOrEmpty(gs)) return gs;
            if (GlobalSettings != null)
            {
                gs = GlobalSettings.grabSetting(key, false);
                if (!string.IsNullOrEmpty(gs)) return gs;
            }
            if (key.StartsWith("bot"))
            {
                key = key.Substring(3);
                gs = BotAsUser.Predicates.grabSetting(key, false);
                if (!string.IsNullOrEmpty(gs)) return gs;
            }
            else if (key.StartsWith("user"))
            {
                key = key.Substring(4);
                gs = LastUser.Predicates.grabSetting(key, true);
                if (!string.IsNullOrEmpty(gs)) return gs;
            }
            return "";
        }

        public string getBBHash0(string key)
        {
            try
            {
                string val;
                if (useMemcache)
                {
                    if ((myChemistry != null) && (myChemistry.m_cBus != null))
                    {
                        val = myChemistry.m_cBus.getHash(key);
                        lock (BBDict) BBDict[key] = val;
                        if (!string.IsNullOrEmpty(val)) return val;
                        return "";
                    }
                }
                lock (BBDict) if (BBDict.TryGetValue(key, out val)) return val;
                return "";
            }
            catch
            {
                return "";
            }
        }
        #endregion

        public string ToValueString(object oldSetting)
        {
            return "" + oldSetting;
        }
    }

    public delegate void sayProcessorDelegate(string message);
    public delegate void systemProcessorDelegate(string message);
    public delegate void systemPersonaDelegate(string message);
}