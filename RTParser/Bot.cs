using System;
using System.Collections;
using System.Collections.Generic;
using System.Globalization;
using System.IO;
using System.Net.Mail;
using System.Reflection;
using System.Text.RegularExpressions;
using System.Web;
using System.Xml;
using AIMLbot;
using LAIR.ResourceAPIs.WordNet;
using MushDLR223.ScriptEngines;
using MushDLR223.Utilities;
using MushDLR223.Virtualization;
using org.opencyc.api;
using RTParser.AIMLTagHandlers;
using RTParser.Database;
using RTParser.Prolog;
using RTParser.Utils;
using RTParser.Variables;
using RTParser.Web;
using Console=System.Console;
using UPath = RTParser.Unifiable;
using UList = System.Collections.Generic.List<RTParser.Utils.TemplateInfo>;

namespace RTParser
{
    public delegate object SystemExecHandler(string cmd, Request user);

    /// <summary>
    /// Encapsulates a Proccessor. If no settings.xml file is found or referenced the Proccessor will try to
    /// default to safe settings.
    /// </summary>
    public partial class RTPBot : StaticAIMLUtils
    {
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
                //return true;
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
            s = s.Trim();
            if (!s.StartsWith("<")) s = "<!-- " + s.Replace("<!--", "<#").Replace("-->", "#>") + " -->";
            var r = new AIMLbot.Request(s, BotAsUser, this, null, BotAsUser);
            r.writeToLog = writeToLog;
            Result res = new AIMLbot.Result(BotAsUser, this, r, null);
            res.writeToLog = writeToLog;
            res._CurrentQuery = new SubQuery(s, res, r);
            OnBotCreated(() => { res.Requestor = r.Requester = BotAsUser; });
            r.IsTraced = this.IsTraced;            
            r.StartedOn = DateTime.Now;
            r.depth = 0;
            // times out in 15 minutes
            r.TimeOut = TimeSpan.FromMinutes(15);
            return r;
        }

        public AIMLLoader Loader;

        #region Attributes

        public List<CrossAppDomainDelegate> ReloadHooks = new List<CrossAppDomainDelegate>();

        /// <summary>
        /// A dictionary object that looks after all the settings associated with this Proccessor
        /// </summary>
        public SettingsDictionary GlobalSettings;

        #endregion

        /// <summary>
        /// A dictionary of all the gender based substitutions used by this Proccessor
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
        public SettingsDictionary OutputSubstitutions;

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

        /// <summary>
        /// Flag to show if the Proccessor is willing to accept user input
        /// </summary>
        public bool isAcceptingUserInput = true;

        /// <summary>
        /// A dictionary of all inherited settings betten users
        /// </summary>
        public SettingsDictionary AllUserPreds;


        /// <summary>
        /// Holds information about the available custom tag handling classes (if loaded)
        /// Key = class name
        /// Value = TagHandler class that provides information about the class
        /// </summary>
        private Dictionary<string, TagHandler> CustomTags;

        /// <summary>
        /// Holds references to the assemblies that hold the custom tag handling code.
        /// </summary>
        private readonly Dictionary<string, Assembly> LateBindingAssemblies = new Dictionary<string, Assembly>();

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
                if (GlobalSettings == null || !GlobalSettings.containsSettingCalled("timeout"))
                {
                    return 20000;
                }
                String s = GlobalSettings.grabSettingNoDebug("timeout").ToValue(null);
                return Convert.ToDouble(s);
            }
        }

        /// <summary>
        /// The message to display in the event of a timeout
        /// </summary>
        public Unifiable TimeOutMessage
        {
            get { return GlobalSettings.grabSetting("timeoutmessage"); }
        }

        /// <summary>
        /// The locale of the Proccessor as a CultureInfo object
        /// </summary>
        public CultureInfo Locale
        {
            get { return new CultureInfo(GlobalSettings.grabSetting("culture")); }
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
            get { return GlobalSettings.grabSetting("adminemail"); }
            set
            {
                if (value.Length > 0)
                {
                    // check that the email is valid
                    Unifiable patternStrict = @"^(([^<>()[\]\\.,;:\s@\""]+"
                                              + @"(\.[^<>()[\]\\.,;:\s@\""]+)*)|(\"".+\""))@"
                                              + @"((\[[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}"
                                              + @"\.[0-9]{1,3}\])|(([a-zA-Z\-0-9]+\.)+"
                                              + @"[a-zA-Z]{2,}))$";
                    Regex reStrict = new Regex(patternStrict);

                    if (reStrict.IsMatch(value))
                    {
                        // update the settings
                        GlobalSettings.addSetting("adminemail", value);
                    }
                    else
                    {
                        throw (new Exception("The AdminEmail is not a valid email address"));
                    }
                }
                else
                {
                    GlobalSettings.addSetting("adminemail", Unifiable.Empty);
                }
            }
        }

        /// <summary>
        /// Flag to denote if the Proccessor is writing messages to its logs
        /// </summary>
        public bool IsLogging
        {
            get
            {
                // otherwse we use up too much ram
                if (true) return false;
                if (!GlobalSettings.containsSettingCalled("islogging")) return false;
                Unifiable islogging = GlobalSettings.grabSettingNoDebug("islogging");
                if (IsTrue(islogging))
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
        /// Flag to denote if the Proccessor will email the botmaster using the AdminEmail setting should an error
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
        /// When the RTPBot was initialised
        /// </summary>
        public DateTime StartedOn = DateTime.Now;

        /// <summary>
        /// The supposed sex of the Proccessor
        /// </summary>
        public Gender Sex
        {
            get
            {
                int sex = Convert.ToInt32(GlobalSettings.grabSetting("gender"));
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
                if (GlobalSettings.containsSettingCalled("userdirectory"))
                {
                    Unifiable dir = GlobalSettings.grabSettingNoDebug("userdirectory");
                    HostSystem.CreateDirectory(dir);
                    _PathToUserFiles = dir;
                    return HostSystem.ToRelativePath(dir, RuntimeDirectory);
                }
                foreach (string s in new[] {PersonalAiml, PathToAIML, PathToConfigFiles, RuntimeDirectory})
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

        protected string PersonalAiml
        {
            get { return _PathToBotPersonalFiles; }
            set
            {
                lock (_RuntimeDirectories)
                {
                    if (_PathToUserFiles != null) _RuntimeDirectories.Remove(_PathToUserFiles);
                    _PathToUserFiles = value;
                    _RuntimeDirectories.Insert(0, value);
                }
            }
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

        private string _dataDir = Environment.CurrentDirectory;

        protected string RuntimeDirectory
        {
            get { return _dataDir ?? Environment.CurrentDirectory; }
            set { _dataDir = value; }
        }

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
        public RTPBot()
            : base()
        {
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
            directory = directory.Trim();
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
            directory = directory.Trim();
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

            TheCyc = new CycDatabase(this);
            CycAccess v = TheCyc.GetCycAccess;


            clojureInterpreter = new ClojureInterpreter(this);
            clojureInterpreter.Init();
            clojureInterpreter.Intern("MyBot", this);
            clojureInterpreter.Intern("Users", BotUsers);
            AddExcuteHandler("cloj", ClojExecHandler);

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


#if !(NOT_FAKE_LISTENERS)
        public Dictionary<string, object> listeners = new Dictionary<string, object>();

        public RTPBot MyBot
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
            RTPBot bot = this;
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

                RelationMetaProps = new SettingsDictionary("chat.relationprops", this, null);
                RegisterDictionary("meta", RelationMetaProps);
                RegisterDictionary("metaprops", RelationMetaProps);

                GlobalSettings = new SettingsDictionary("bot.globalsettings", this, null);
                GlobalSettings.InsertMetaProvider(GetRelationMetaProps);

                GenderSubstitutions = new SettingsDictionary("nl.substitutions.gender", this, null);
                RegisterSubstitutions("gender", GenderSubstitutions);
                Person2Substitutions = new SettingsDictionary("nl.substitutions.person2", this, null);
                RegisterSubstitutions("person2", Person2Substitutions);
                PersonSubstitutions = new SettingsDictionary("nl.substitutions.person", this, null);
                RegisterSubstitutions("person", PersonSubstitutions);
                InputSubstitutions = new SettingsDictionary("nl.substitutions.input", this, null);
                RegisterSubstitutions("input", InputSubstitutions);
                OutputSubstitutions = new SettingsDictionary("nl.substitutions.output", this, null);
                RegisterSubstitutions("output", OutputSubstitutions);


                //ParentProvider provider = new ParentProvider(() => GlobalSettings);
                DefaultPredicates = new SettingsDictionary("bot.defaultpredicates", this, null);
                DefaultPredicates = new SettingsDictionary("defaults", this, null);                
                DefaultPredicates.InsertMetaProvider(GetRelationMetaProps);
                HeardPredicates = new SettingsDictionary("chat.heardpredicates", this, null);
                RegisterDictionary("heard", HeardPredicates);
                AllUserPreds = new SettingsDictionary("bot.alluserpred", this, null);
                RegisterDictionary("predicates", AllUserPreds);

                AllUserPreds.InsertMetaProvider(GetRelationMetaProps);


                User guser = ExemplarUser = LastUser = new AIMLbot.User("globalPreds", this);
                BotUsers["globalpreds"] = guser;
                guser.IsRoleAcct = true;
                guser.Predicates.clearSettings();
                guser.Predicates.clearHierarchy();
                guser.Predicates.InsertFallback(() => HeardPredicates);
                guser.Predicates.maskSetting("name");
                guser.Predicates.maskSetting("id");

                CustomTags = new Dictionary<string, TagHandler>();
                //this.GraphMaster = new GraphMaster();
                //this.HeardSelfSayGraph = new GraphMaster();
                if (HostSystem.FileExists("AIMLbot.dll")) loadCustomTagHandlers("AIMLbot.dll");
                if (HostSystem.FileExists("AIMLbot.exe")) loadCustomTagHandlers("AIMLbot.exe");


                // try a safe default setting for the settings xml file
                // Checks for some important default settings
                SetSaneGlobals(GlobalSettings);
                string pathToSettings = HostSystem.Combine(RuntimeDirectory,
                                                           HostSystem.Combine("config", "Settings.xml"));
                Request request = GetBotRequest("<!-- Loads settings from: '" + pathToSettings + "' -->");
                loadSettingsFile(pathToSettings, request);
                // RE-Checks for some important default settings
                SetSaneGlobals(GlobalSettings);
                SetupConveration();
            }
            finally
            {
                isAcceptingUserInput = prev;
            }
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

        // Load the dictionaries for this RTPBot from the various configuration files
        public static void loadConfigs(RTPBot thiz, string pathToSettings, Request request)
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


            // Grab the splitters for this Proccessor
            thiz.loadSplitters(HostSystemCombine(pathToSettings, GlobalSettings.grabSetting("splittersfile")));
            thiz.loadSplitters(HostSystemCombine(pathToSettings, GlobalSettings.grabSetting("sentence-splitters")));
            thiz.loadSplitters(HostSystemCombine(pathToSettings, "sentence-splitters.xml"));

            // genformat.xml
            thiz.RelationMetaProps.loadSettings(HostSystemCombine(pathToSettings, "genformat.xml"), request);


            User guser = thiz.FindUser("globalPreds");
            SettingsDictionary.loadSettings(guser.Predicates, HostSystemCombine(pathToSettings, "globalpreds.xml"),
                                            true, false, request);
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
            SaneLocalSettings(settings, "notopic", "Nothing");
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
            lock (BotUsers)
            {
                foreach (User u in BotUsers.Values)
                {
                    if (u.UserID.AsString().Contains(username) || username.Contains(u.UserID))
                        u.RespondToChat = value;
                }
            }
        }


        public void AddAiml(string aimlText)
        {
            AddAiml(GraphMaster, aimlText);
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

        internal AIMLTagHandler GetTagHandler(User user, SubQuery query, Request request, Result result, XmlNode node,
                                              AIMLTagHandler handler)
        {
            AIMLTagHandler tag = GetTagHandler00(user, query, request, result, node, true);
            if (tag == null)
            {
                writeToLog("NULL TAG " + node.OuterXml);
            }
            if (tag != null)
            {
                tag.SetParent(handler);
                TemplateInfo ti = tag.templateInfo;
                if (ti == null && query != null)
                {
                    ti = query.CurrentTemplate;
                }
                if (ti == null && handler != null)
                {
                    ti = handler.GetTemplateInfo();
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
            string nodeNameLower = node.LocalName.ToLower();
            if (Equals(null, tagHandler))
            {
                switch (nodeNameLower)
                {
                    case "template":
                        tagHandler = new template(this, user, query, request, result, node);
                        break;
                    case "aiml":
                        tagHandler = new aiml(this, user, query, request, result, node);
                        break;
                    case "aimlexec":
                    case "eval":
                        tagHandler = new aimlexec(this, user, query, request, result, node);
                        break;
                    case "vars":
                    case "root":
                    case "predicates":
                        tagHandler = new root(this, user, query, request, result, node, (() => request.TargetSettings));
                        break;
                    case "properties":
                    case "bots":
                        tagHandler = new root(this, user, query, request, result, node,
                                              (() => request.TargetBot.GlobalSettings));
                        break;
                    case "substitutions":
                        tagHandler = new root(this, user, query, request, result, node,
                                              (() => request.TargetBot.InputSubstitutions));
                        break;
                    case "topic":
                        tagHandler = new topic(this, user, query, request, result, node);
                        break;
                    case "category":
                        tagHandler = new category(this, user, query, request, result, node);
                        break;
                    case "and":
                        tagHandler = new and(this, user, query, request, result, node);
                        break;
                    case "or":
                        tagHandler = new or(this, user, query, request, result, node);
                        break;
                    case "optional":
                        tagHandler = new optional(this, user, query, request, result, node);
                        break;
                    case "isa":
                        tagHandler = new isa(this, user, query, request, result, node);
                        break;
                    case "bot":
                        tagHandler = new bot(this, user, query, request, result, node);
                        break;
                    case "condition":
                        tagHandler = new condition(this, user, query, request, result, node);
                        break;
                    case "li":
                        if (liText)
                            tagHandler = new liif(this, user, query, request, result, node);
                        break;
                    case "if":
                        tagHandler = new liif(this, user, query, request, result, node);
                        break;
                    case "personf":
                        tagHandler = new format(this, user, query, request, result, node,
                                                new Func<string, string>((s) => HttpUtility.UrlEncode(s)),
                                                null);
                        break;
                    case "date":
                        tagHandler = new date(this, user, query, request, result, node);
                        break;
                    case "formal":
                        tagHandler = new formal(this, user, query, request, result, node);
                        break;
                    case "gender":
                        tagHandler = new gender(this, user, query, request, result, node);
                        break;
                    case "get":
                        tagHandler = new get(this, user, query, request, result, node);
                        break;
                    case "gossip":
                        tagHandler = new gossip(this, user, query, request, result, node);
                        break;
                    case "get_ip":
                    case "id":
                        tagHandler = new id(this, user, query, request, result, node);
                        break;
                    case "request":
                        tagHandler = new input(this, user, query, request, result, node, 1);
                        break;
                    case "input":
                        tagHandler = new input(this, user, query, request, result, node, 1);
                        break;
                    case "justthat": // <input index="2"/> 
                        tagHandler = new input(this, user, query, request, result, node, 2);
                        break;
                    case "beforethat": // <input index="3"/> 
                        tagHandler = new input(this, user, query, request, result, node, 3);
                        break;
                    case "javascript":
                        tagHandler = new javascript(this, user, query, request, result, node);
                        break;
                    case "learn":
                    case "load":
                    case "noload": // the commented version of <load>
                        tagHandler = new learn(this, user, query, request, result, node);
                        break;
                    case "lowercase":
                        tagHandler = new lowercase(this, user, query, request, result, node);
                        break;
                    case "person":
                        tagHandler = new person(this, user, query, request, result, node);
                        break;
                    case "person2":
                        tagHandler = new person2(this, user, query, request, result, node);
                        break;
                    case "random":
                        tagHandler = new random(this, user, query, request, result, node);
                        break;
                    case "sentence":
                        tagHandler = new sentence(this, user, query, request, result, node);
                        break;
                    case "set":
                        tagHandler = new set(this, user, query, request, result, node);
                        break;
                    case "size":
                    case "getsize":
                        tagHandler = new size(this, user, query, request, result, node);
                        break;
                    case "sr":
                        tagHandler = new sr(this, user, query, request, result, node);
                        break;
                    case "srai":
                        tagHandler = new srai(this, user, query, request, result, node);
                        break;
                    case "star":
                        tagHandler = new star(this, user, query, request, result, node);
                        break;
                    case "system":
                        tagHandler = new system(this, user, query, request, result, node);
                        break;
                    case "that": //default <that index="1,1"/>
                        tagHandler = new that(this, user, query, request, result, node, 1);
                        break;
                    case "justbeforethat": //treated as <that index="2,1"/>
                        tagHandler = new that(this, user, query, request, result, node, 2);
                        break;
                    case "response": //treated as <that index="1,1"/>
                        tagHandler = new that(this, user, query, request, result, node, 2);
                        break;
                    case "thatstar":
                        tagHandler = new thatstar(this, user, query, request, result, node);
                        break;
                    case "think":
                        tagHandler = new think(this, user, query, request, result, node);
                        break;
                    case "topicstar":
                        tagHandler = new topicstar(this, user, query, request, result, node);
                        break;
                    case "uppercase":
                        tagHandler = new uppercase(this, user, query, request, result, node);
                        break;
                    case "version":
                    case "getversion":
                        tagHandler = new version(this, user, query, request, result, node);
                        break;
                    case "cycsystem":
                        tagHandler = new cycsystem(this, user, query, request, result, node);
                        break;
                    case "cycretract":
                        tagHandler = new cycretract(this, user, query, request, result, node);
                        break;
                    case "cycassert":
                        tagHandler = new cycassert(this, user, query, request, result, node);
                        break;
                    case "cycterm":
                        tagHandler = new cycterm(this, user, query, request, result, node);
                        break;
                    case "cycquery":
                        tagHandler = new cycquery(this, user, query, request, result, node);
                        break;
                    case "cyccondition":
                        tagHandler = new cyccondition(this, user, query, request, result, node);
                        break;
                    case "cycphrase":
                        tagHandler = new cycphrase(this, user, query, request, result, node);
                        break;
                    case "cycparaphrase":
                        tagHandler = new cycphrase(this, user, query, request, result, node);
                        break;
                    case "guard":
                        tagHandler = new guard(this, user, query, request, result, node);
                        break;
                    case "guardstar":
                        tagHandler = new guardstar(this, user, query, request, result, node);
                        break;
                    case "cycrandom":
                        tagHandler = new cycrandom(this, user, query, request, result, node);
                        break;
                    case "space":
                        tagHandler = new space(this, user, query, request, result, node);
                        break;
                    case "markov":
                        tagHandler = new markov(this, user, query, request, result, node);
                        break;
                    case "soundcode":
                        tagHandler = new soundcode(this, user, query, request, result, node);
                        break;

                        // MSM
                    case "msm":
                        tagHandler = new msm(this, user, query, request, result, node);
                        break;
                    case "processmsm":
                        tagHandler = new process_msm(this, user, query, request, result, node);
                        break;
                    case "setstate":
                        tagHandler = new setstate(this, user, query, request, result, node);
                        break;
                    case "state":
                        tagHandler = new state(this, user, query, request, result, node);
                        break;
                    case "transition":
                        tagHandler = new transition(this, user, query, request, result, node);
                        break;
                    case "setevidence":
                        tagHandler = new setevidence(this, user, query, request, result, node);
                        break;
                    case "evidenceassoc":
                        tagHandler = new evidence_assoc(this, user, query, request, result, node);
                        break;
                    case "evidencepattern":
                        tagHandler = new evidence_pattern(this, user, query, request, result, node);
                        break;
                    case "evidencestate":
                        tagHandler = new evidencestate(this, user, query, request, result, node);
                        break;
                    case "dependentmachine":
                        tagHandler = new dependentmachine(this, user, query, request, result, node);
                        break;
                    case "responsetopic":
                        tagHandler = new response_topic(this, user, query, request, result, node);
                        break;

                    case "push":
                        tagHandler = new push(this, user, query, request, result, node);
                        break;
                    case "pop":
                        tagHandler = new pop(this, user, query, request, result, node);
                        break;
                    case "peekstack":
                        tagHandler = new peekstack(this, user, query, request, result, node);
                        break;

                    case "lex":
                        tagHandler = new lex(this, user, query, request, result, node);
                        break;
                    case "lexset":
                        tagHandler = new lexset(this, user, query, request, result, node);
                        break;
                    case "lexis":
                        tagHandler = new lexis(this, user, query, request, result, node);
                        break;

                    case "dbpush":
                        tagHandler = new dbpush(this, user, query, request, result, node);
                        break;
                    case "dbquery":
                        tagHandler = new dbquery(this, user, query, request, result, node);
                        break;
                    case "dbupdate":
                        tagHandler = new dbupdate(this, user, query, request, result, node);
                        break;
                    case "dbdelete":
                        tagHandler = new dbdelete(this, user, query, request, result, node);
                        break;
                    case "dbload":
                        tagHandler = new dbload(this, user, query, request, result, node);
                        break;


                    case "regex":
                        tagHandler = new regex(this, user, query, request, result, node);
                        break;

                    case "bind": // <bind>#$isa</bind>
                        tagHandler = new bind(this, user, query, request, result, node);
                        break;

                    case "#text":
                        if (!liText) return null;
                        return new verbatum(node.InnerText, this, user, query, request, result, node);
                    case "#comment":
                        return new verbatum(node.OuterXml, this, user, query, request, result, node);
                    case "br":
                        return new verbatum("\n", this, user, query, request, result, node);
                    case "pre":
                        return new verbatum(node.InnerXml, this, user, query, request, result, node);
                    case "p":
                        return new verbatum("\n\n", this, user, query, request, result, node);
                    case "meta":
                        return new verbatum(node.OuterXml, this, user, query, request, result, node);
                    default:
                        break;
                }
            }
            if (IsHtmlTag(node.Name))
            {
                return new recursiveVerbatum(node, this, user, query, request, result, node, true);
            }
            if (tagHandler == null)
            {
                // "bot", "favorite", "fav" 
                foreach (KeyValuePair<string, string> prefix in new[]
                                                                    {
                                                                        new KeyValuePair<string, string>("get_", "get"),
                                                                        new KeyValuePair<string, string>("get", "get"),
                                                                        new KeyValuePair<string, string>("set_", "set"),
                                                                        new KeyValuePair<string, string>("set", "set"),
                                                                        new KeyValuePair<string, string>("bot_", "bot"),
                                                                        new KeyValuePair<string, string>("bot", "bot"),
                                                                        new KeyValuePair<string, string>("favorite_",
                                                                                                         "bot"),
                                                                        new KeyValuePair<string, string>("favorite",
                                                                                                         "bot"),
                                                                        new KeyValuePair<string, string>("fav_", "bot"),
                                                                        new KeyValuePair<string, string>("fav", "bot"),
                                                                    })
                {
                    if (nodeNameLower.StartsWith(prefix.Key) && node.Name.Length > prefix.Key.Length)
                    {
                        string name = node.Name.Substring(prefix.Key.Length);
                        XmlNode pn = node.ParentNode;
                        LineInfoElementImpl newnode = CopyNode(prefix.Value, node, false);
                        XmlAttributeLineInfo atr = (XmlAttributeLineInfo) newnode.OwnerDocument.CreateAttribute("name");
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
                return new bot(this, user, query, request, result, node);
            }
            tagHandler = new lazyClosure(this, user, query, request, result, node);
            writeToLog("AIMLLOADER:  lazyClosure: " + node.OuterXml);
            return tagHandler;
        }

        /// <summary>
        /// Searches the CustomTag collection and processes the AIML if an appropriate tag handler is found
        /// </summary>
        /// <param name="user">the user who originated the request</param>
        /// <param name="query">the query that produced this node</param>
        /// <param name="request">the request from the user</param>
        /// <param name="result">the result to be sent to the user</param>
        /// <param name="node">the node to evaluate</param>
        /// <returns>the output Unifiable</returns>
        public AIMLTagHandler getBespokeTags(User user, SubQuery query, Request request, Result result, XmlNode node)
        {
            string nodename = node.Name.ToLower();
            if (CustomTags != null)
            {
                return null;
                try
                {
                    lock (CustomTags)


                        if (CustomTags.ContainsKey(nodename))
                        {
                            TagHandler customTagHandler = CustomTags[node.Name.ToLower()];

                            AIMLTagHandler newCustomTag = customTagHandler.Instantiate(LateBindingAssemblies, user,
                                                                                       query,
                                                                                       request, result, node, this);
                            if (Equals(null, newCustomTag))
                            {
                                return null;
                            }
                            else
                            {
                                return newCustomTag;
                            }
                        }
                }
                catch (Exception e)
                {
                    writeToLog("WARNING IN GET BESPOKE TAGS: " + e);
                }
            }
            {
                try
                {
                    if (nodename.StartsWith("#")) return null;
                    String typeName = GetType().Namespace + ".AIMLTagHandlers." + nodename;
                    Type t = Type.GetType(typeName);
                    if (t == null) return null;
                    ConstructorInfo c = t.GetConstructor(TagHandler.CONSTRUCTOR_TYPES);
                    return (AIMLTagHandler) c.Invoke(new object[] {this, user, query, request, result, node});
                }
                catch (Exception e)
                {
                    writeToLog("ERROR getBespokeTags: " + e);
                    return null;
                }
            }
        }

        #region Serialization

        /// <summary>
        /// Saves the graphmaster node (and children) to a binary file to avoid processing the AIML each time the 
        /// Proccessor starts
        /// </summary>
        /// <param name="path">the path to the file for saving</param>
        public void saveToBinaryFile(Unifiable path)
        {
            GraphMaster.saveToBinaryFile(path);
        }

        /// <summary>
        /// Loads a dump of the graphmaster into memory so avoiding processing the AIML files again
        /// </summary>
        /// <param name="path">the path to the dump file</param>
        public void loadFromBinaryFile(Unifiable path)
        {
            GraphMaster.loadFromBinaryFile(path);
        }

        #endregion

        #region Latebinding custom-tag dll handlers

        /// <summary>
        /// Loads any custom tag handlers found in the dll referenced in the argument
        /// </summary>
        /// <param name="pathToDLL">the path to the dll containing the custom tag handling code</param>
        public void loadCustomTagHandlers(string pathToDLL)
        {
            // return;
            Assembly tagDLL = Assembly.LoadFrom(pathToDLL);
            var tagDLLTypes = tagDLL.GetTypes();
            for (int i = 0; i < tagDLLTypes.Length; i++)
            {
                Type type = tagDLLTypes[i];
                var typeCustomAttributes = type.GetCustomAttributes(false);
                if (typeCustomAttributes.Length == 0 && typeof (AIMLTagHandler).IsAssignableFrom(type) &&
                    !type.IsAbstract && !type.IsInterface)
                {
                    AddTagHandler(type);
                    continue;
                }
                for (int j = 0; j < typeCustomAttributes.Length; j++)
                {
                    if (typeCustomAttributes[j] is CustomTagAttribute)
                    {
                        // We've found a custom tag handling class
                        // so store the assembly and store it away in the Dictionary<,> as a TagHandler class for 
                        // later usage
                        AddTagHandler(type);
                    }
                }
            }
        }

        private void AddTagHandler(Type type)
        {
            Assembly tagDLL = type.Assembly;
            // store Assembly
            if (!LateBindingAssemblies.ContainsKey(tagDLL.FullName))
            {
                LateBindingAssemblies.Add(tagDLL.FullName, tagDLL);
            }
            // create the TagHandler representation
            TagHandler newTagHandler = new TagHandler(type);
            newTagHandler.AssemblyName = tagDLL.FullName;
            newTagHandler.ClassName = type.FullName;
            newTagHandler.TagName = type.Name.ToLower();
            if (CustomTags.ContainsKey(newTagHandler.TagName))
            {
                throw new Exception("ERROR! Unable to add the custom tag: <" + newTagHandler.TagName + ">, found in: " +
                                    tagDLL.Location + " as a handler for this tag already exists.");
            }
            else
            {
                CustomTags.Add(newTagHandler.TagName, newTagHandler);
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
            foreach (Unifiable path in request.CurrentResult.NormalizedPaths)
            {
                paths.Append(path.LegacyPath + Environment.NewLine);
            }
            message = message.Replace("*PATHS*", paths.ToString());
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
            string evalTemplate = "<template>" + cmd + "</template>";
            XmlNode node = getNode(evalTemplate);
            LineInfoElementImpl.unsetReadonly(node);
            if (Loader == null)
            {
                Loader = new AIMLLoader(this, GetBotRequest("EvalAIMLHandler " + cmd));
            }
            AIMLbot.Result res = ImmediateAiml(node, user, Loader, null);
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

        internal Unifiable SystemExecute(Unifiable cmd, Unifiable langu, Request user)
        {
            if (IsNullOrEmpty(langu))
            {
                langu = "bot";
            }
            else
            {
                langu = langu.AsString().ToLower().Trim();
            }
            Unifiable s = "The system tag should be doing '" + cmd + "' lang=" + langu;
            writeToLog(s.AsString());
            SystemExecHandler handler;
            if (ExecuteHandlers.TryGetValue(langu, out handler))
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
                    ScriptInterpreter si = ScriptManager.LoadScriptInterpreter(langu, self);
                    object o = ScriptManager.EvalScriptInterpreter(cmd.ToString(), langu, self, writeToLog);
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


        private readonly Dictionary<string, SystemExecHandler> ExecuteHandlers =
            new Dictionary<string, SystemExecHandler>();

        public void AddExcuteHandler(string lang, SystemExecHandler handler)
        {
            lang = lang.ToLower().Trim();
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
                if (!GlobalSettings.containsSettingCalled("notopic")) return "Nothing";
                return GlobalSettings.grabSettingNoDebug("notopic");
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
            graphPath = graphPath.ToLower().Trim();
            GraphMaster g;
            lock (GraphsByName)
            {
                if (LocalGraphsByName.TryGetValue(graphPath, out g))
                {
                    return g;
                }
                if (!GraphsByName.TryGetValue(graphPath, out g))
                {
                    g = GraphsByName[graphPath] = new GraphMaster(graphPath);
                }
            }
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
                }
                return current;
            }

            if (graphPath.ToLower().EndsWith(".parent"))
            {
                int graphPathLength = graphPath.Length - 7;
                return GetGraph(graphPath.Substring(0, graphPathLength), current).Parent;
            }

            graphPath = ToScriptableName(graphPath.Trim());
            lock (GraphsByName)
            {
                if (LocalGraphsByName.TryGetValue(graphPath, out g))
                {
                    return g;
                }
                if (!GraphsByName.TryGetValue(graphPath, out g))
                {
                    g = GraphsByName[graphPath] = new GraphMaster(graphPath);
                }
            }
            return g;
        }

        public GraphMaster FindGraph(string graphPath, GraphMaster current)
        {
            if (graphPath == null)
            {
                return current;
            }

            if (graphPath.ToLower().EndsWith(".parent"))
            {
                int graphPathLength = graphPath.Length - 7;
                var G = FindGraph(graphPath.Substring(0, graphPathLength), current);
                if (G != null) return G.Parent;
            }

            graphPath = ToScriptableName(graphPath.Trim());

            if (graphPath == "current" || graphPath == "")
            {
                return current;
            }

            if (false)
            {
                if (_g != null && graphPath == "default")
                {
                    return _g;
                }

                if (_h != null && graphPath == "heardselfsay")
                {
                    return _h;
                }
            }
            if (graphPath == "parent")
            {
                if (current == null) return null;
                return current.Parent;
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

        public static string ToScriptableName(string path)
        {
            string sk = "";
            foreach (char s in path)
            {
                if (IsOkForNameChar(s))
                    sk += s;
            }
            path = sk;
            return path.ToLower().Trim().Replace(" ", "_").Replace(".", "_").Replace("-", "_").Replace("__", "_");
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
            left = args.Substring(0, lastIndex).Trim();
            right = args.Substring(lastIndex + 1).Trim();
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
                GraphMaster.WriteConfig();
                writeDebugLine("Bot loaded");
            }
        }

        public string LoadPersonalDirectory(string myName)
        {
            return UserOper(() => LoadPersonalDirectory0(myName), QuietLogger);
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
            lock (BotUsers) lock (OnBotCreatedHooks) UsePersonalDir0(file);
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
                foreach (string s1 in HostSystem.GetFiles(file, "*Settings*.xml"))
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
            return UserOper(() => SetName0(myName), writeDebugLine);
        }

        private string SetName0(string myName)
        {
            //char s1 = myName[1];

            NameAsSet = myName;
            //new AIMLbot.User("heardselfsay", this)
            BotAsUser = FindOrCreateUser(myName);
            clojureInterpreter.Intern("BotAsUser", BotAsUser);
            BotAsUser.IsRoleAcct = true;
            BotAsUser.Predicates = GlobalSettings;
            GlobalSettings.IsTraced = true;
            //BotAsUser.UserDirectory = "aiml/users/heardselfsay";
            //BotAsUser.UserID = "heardselfsay";
            //BotAsUser.UserName = "heardselfsay";
            //BotUsers["heardselfsay"] = BotAsUser;            
            BotAsUser.UserName = myName;
            BotAsUser.addSetting("name", myName);
            BotAsUser.removeSetting("userdir");
            NamePath = ToScriptableName(NameAsSet);
            BotAsUser.UserID = NamePath;

            var OnTaskAtATimeHandler = BotAsUser.OnTaskAtATimeHandler = HeardSelfSayQueue;
            OnTaskAtATimeHandler.Name = "TaskQueue For " + myName;

            BotAsUser.SaveDirectory(BotAsUser.UserDirectory);
            string dgn = NamePath + "_default";
            string hgn = NamePath + "_heardselfsay";
            lock (GraphsByName)
            {
                if (String.IsNullOrEmpty(NamePath))
                {
                    throw new NullReferenceException("SetName! = " + myName);
                }
                if (_g == null)
                {
                    _g = new GraphMaster(dgn);
                    _h = new GraphMaster(hgn);
                    _g.AddGenlMT(GraphsByName["default"], writeToLog);
                    _h.AddGenlMT(GraphsByName["heardselfsay"], writeToLog);
                    _h.AddGenlMT(GraphsByName["listener"], writeToLog);
                    GraphsByName.Add(dgn, _g);
                    GraphsByName.Add(hgn, _h);
                }
            }
            GraphMaster vv = HeardSelfSayGraph;
            if (vv != null) BotAsUser.ListeningGraph = vv;
            lock (OnBotCreatedHooks)
            {
                foreach (Action list in OnBotCreatedHooks)
                {
                    list();
                }
                OnBotCreatedHooks.Clear();
            }
            loadAIMLFromDefaults0();
            EnsureDefaultUsers();
            string official = LoadPersonalDirectories(myName);
            BotAsUser.SaveDirectory(BotAsUser.UserDirectory);
            return official ?? BotAsUser.UserDirectory;
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
                    tc.Add(typeof (RTPBot));
                    tc.Add(typeof (AIMLbot.Request));
                    tc.Add(typeof (RequestImpl));
                    tc.Add(typeof (Request));
                }
                if (StaticInitStarted) return;
                StaticInitStarted = true;
                GraphsByName["listener"] = TheUserListernerGraph = new GraphMaster("listener");
                GraphsByName["default"] = new GraphMaster("default");
                GraphsByName["heardselfsay"] = new GraphMaster("heardselfsay");
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
                myName = ToScriptableName(myName.Trim());
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
                    string real = DLRConsole.SafeFormat(message, args);
                    message = real.ToUpper();
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
            old = old.ToLower().Trim();
            next = next.ToLower().Trim();
            return FindUser(old) == FindUser(next);
        }

        private ClojureInterpreter clojureInterpreter;
        private List<string> _RuntimeDirectories;

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
            get { return UserID ?? "-BOT-ID-NULL-"; }
            set { throw new NotImplementedException(); }
        }

        public ISettingsDictionary Predicates
        {
            get { return GlobalSettings; }
        }

        #endregion

        public ISettingsDictionary GetDictionary(string name)
        {
            var rtpbotobjCol = ScriptManager.ResolveToObject(this, name);
            if (rtpbotobjCol == null || rtpbotobjCol.Count == 0)
            {
                lock (AllDictionaries) return GetDictionary0(name);
            }
            //if (tr)
            foreach (object o in rtpbotobjCol)
            {
                ParentProvider pp = o as ParentProvider;
                ISettingsDictionary pi = o as ISettingsDictionary;
                User pu = o as User;
                if (pp != null)
                {
                    pi = pp();
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
            Func<ISettingsDictionary, SettingsDictionary> SDCAST = SettingsDictionary.ToSettingsDictionary;
            //dict = FindDict(type, query, dict);
            if (named == null) return null;
            string key = named.ToLower().Trim();
            if (key == "") return null;
            lock (AllDictionaries)
            {
                ISettingsDictionary dict;
                if (AllDictionaries.TryGetValue(key, out dict))
                {
                    return SDCAST(dict);
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
            var path = named.Split(new[] {'.'});
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
                        SettingsDictionary sd = SDCAST(f);
                        ParentProvider pp = sd.FindDictionary(string.Join(".", path, 1, path.Length - 1), null);
                        if (pp != null)
                        {
                            ISettingsDictionary pi = pp();
                            if (pi != null) return SDCAST(pi);
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
                        dict = AllDictionaries[key] = AllDictionaries[named] = new SettingsDictionary(named, this, null);
                        User user = LastUser ?? ExemplarUser ?? BotAsUser;
                        Request r = user.CurrentRequest ??
                                    user.CreateRequest(" loadDictionary" + named + " from " + type, BotAsUser);
                        loadDictionary(dict, named, type, r);
                    }
                }
                return dict;
            }
        }

        private void loadDictionary(ISettingsDictionary dictionary, string path, string type, Request r0)
        {
            User user = LastUser ?? ExemplarUser ?? BotAsUser;
            Request r = r0 ??
                        user.CurrentRequest ??
                        user.CreateRequest(" loadDictionary" + dictionary + " from " + type, BotAsUser);
            int loaded = 0;
            foreach (string p in GetSearchRoots(r))
            {
                foreach (string s0 in new[] {"", type, type + "s",})
                {
                    foreach (string s1 in new[] {"", "." + type, ".xml", ".subst", ".properties",})
                    {
                        string named = HostSystem.Combine(p, path + s0 + s1);
                        if (HostSystem.FileExists(named))
                        {
                            try
                            {
                                SettingsDictionary.loadSettings(dictionary, named, true, false, r);
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
            RegisterDictionary(dict.NameSpace,dict);
        }
        public void RegisterDictionary(string named, ISettingsDictionary dict)
        {
            named = named.ToLower().Trim().Replace("  ", " ");
            string key = named.Replace(" ", "_");
            RegisterDictionary(named, dict, true);
        }

        public void RegisterDictionary(string key, ISettingsDictionary dict, bool always)
        {
            lock (AllDictionaries)
            {
                var path = key.Split(new[] {'.'});
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
    }
}