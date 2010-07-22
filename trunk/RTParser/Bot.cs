using System;
using System.Collections;
using System.Collections.Generic;
using System.Diagnostics;
using System.Globalization;
using System.Text.RegularExpressions;
using System.IO;
using System.Threading;
using System.Xml;
using System.Reflection;
using System.Net.Mail;
using AIMLbot;
using MushDLR223.ScriptEngines;
using MushDLR223.Utilities;
using MushDLR223.Virtualization;
using RTParser.AIMLTagHandlers;
using RTParser.Database;
using RTParser.Normalize;
using RTParser.Utils;
using RTParser.Variables;
using RTParser.Web;
using UPath = RTParser.Unifiable;
using UList = System.Collections.Generic.List<RTParser.Utils.TemplateInfo>;

namespace RTParser
{
    public delegate object SystemExecHandler(string cmd, Request user);

    /// <summary>
    /// Encapsulates a Proccessor. If no settings.xml file is found or referenced the Proccessor will try to
    /// default to safe settings.
    /// </summary>
    public partial class RTPBot : QuerySettings
    {
        public bool ListeningToSelf = false;
        public override string ToString()
        {
            string s = GetType().Name;
            if (GlobalSettings != null)
            {
                s += " name=" + GlobalSettings.grabSettingNoDebug("name");
            }
            return s;
        }
        /// <summary>
        /// Will ensure the same loader options are used between loaders
        /// </summary>
        public bool StaticLoader = true;

        public static string AIMLDEBUGSETTINGS = "clear -spam +user +error +aimltrace +cyc -dictlog -tscore +loaded";
        readonly public static TextFilter LoggedWords = new TextFilter() { "*", "-dictlog" }; //maybe should be ERROR", "STARTUP
        public User LastUser;
        readonly public User BotAsUser;
        //public Request BotAsRequestUsed = null;
        public Request GetBotRequest(string s)
        {
            var r = new AIMLbot.Request(s, BotAsUser, this, null);
            r.IsTraced = true;
            r.StartedOn = DateTime.Now;
            // times out in 15 minutes
            r.TimesOutAt = DateTime.Now + new TimeSpan(0, 15, 0);
            return r;
        }
        public AIMLLoader Loader;
        #region Attributes
        public List<CrossAppDomainDelegate> ReloadHooks = new List<CrossAppDomainDelegate>();
        /// <summary>
        /// A dictionary object that looks after all the settings associated with this Proccessor
        /// </summary>
        public SettingsDictionary GlobalSettings;

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
        public SettingsDictionary Substitutions;

        /// <summary>
        /// The default predicates to set up for a user
        /// </summary>
        public SettingsDictionary DefaultPredicates;

        /// <summary>
        /// The default predicates to set up for a user
        /// </summary>
        public SettingsDictionary HeardPredicates;

        /// <summary>
        /// Holds information about the available custom tag handling classes (if loaded)
        /// Key = class name
        /// Value = TagHandler class that provides information about the class
        /// </summary>
        private Dictionary<string, TagHandler> CustomTags;

        /// <summary>
        /// Holds references to the assemblies that hold the custom tag handling code.
        /// </summary>
        private Dictionary<string, Assembly> LateBindingAssemblies = new Dictionary<string, Assembly>();

        /// <summary>
        /// An List<> containing the tokens used to split the input into sentences during the 
        /// normalization process
        /// </summary>
        public List<string> Splitters = new List<string>();

        /// <summary>
        /// A buffer to hold log messages to be written out to the log file when a max size is reached
        /// </summary>
        private List<string> LogBuffer = new List<string>();

        /// <summary>
        /// A list of Topic states that are set currently (for use of guarding content)
        /// </summary>
        public List<Unifiable> CurrentStates = new List<Unifiable>();

        /// <summary>
        /// How big to let the log buffer get before writing to disk
        /// </summary>
        private int MaxLogBufferSize
        {
            get
            {
                return Convert.ToInt32(this.GlobalSettings.grabSetting("maxlogbuffersize"));
            }
        }

        /// <summary>
        /// Flag to show if the Proccessor is willing to accept user input
        /// </summary>
        public bool isAcceptingUserInput = true;

        /// <summary>
        /// The message to show if a user tries to use the Proccessor whilst it is set to not process user input
        /// </summary>
        private Unifiable NotAcceptingUserInputMessage
        {
            get
            {
                return this.GlobalSettings.grabSetting("notacceptinguserinputmessage");
            }
        }

        /// <summary>
        /// The maximum amount of time a request should take (in milliseconds)
        /// </summary>
        public double TimeOut
        {
            get
            {
                if (this.GlobalSettings == null || !this.GlobalSettings.containsSettingCalled("timeout"))
                {
                    return 20000;
                }
                String s = this.GlobalSettings.grabSettingNoDebug("timeout").ToValue(null);
                return Convert.ToDouble(s);
            }
        }

        /// <summary>
        /// The message to display in the event of a timeout
        /// </summary>
        public Unifiable TimeOutMessage
        {
            get
            {
                return this.GlobalSettings.grabSetting("timeoutmessage");
            }
        }

        /// <summary>
        /// The locale of the Proccessor as a CultureInfo object
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
                return new Regex(this.GlobalSettings.grabSettingNoDebug("stripperregex"), RegexOptions.IgnorePatternWhitespace);
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
                    Unifiable patternStrict = @"^(([^<>()[\]\\.,;:\s@\""]+"
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
                    this.GlobalSettings.addSetting("adminemail", Unifiable.Empty);
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
                if (true) return false;
                if (!this.GlobalSettings.containsSettingCalled("islogging")) return false;
                Unifiable islogging = this.GlobalSettings.grabSettingNoDebug("islogging");
                if (Unifiable.IsTrue(islogging))
                {
                    return true;
                }
                else
                {
                    return false;
                }
            }
        }

        public Object LoggingLock = new object();

        /// <summary>
        /// Flag to denote if the Proccessor will email the botmaster using the AdminEmail setting should an error
        /// occur
        /// </summary>
        public bool WillCallHome
        {
            get
            {
                Unifiable willcallhome = this.GlobalSettings.grabSetting("willcallhome");
                return (Unifiable.IsTrue(willcallhome));
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

        private string _PathToUserFiles = null;
        public string PathToUserDir
        {
            get
            {
                if (_PathToUserFiles != null) return _PathToUserFiles;
                if (this.GlobalSettings.containsSettingCalled("userdirectory"))
                {
                    var dir = this.GlobalSettings.grabSettingNoDebug("userdirectory");
                    HostSystem.CreateDirectory(dir);
                    _PathToUserFiles = dir;
                    return HostSystem.ToRelativePath(dir);
                }
                foreach (var s in new[] { PersonalAiml, PathToAIML, PathToConfigFiles, RuntimeDirectory })
                {
                    if (s == null) continue;
                    string exists = Path.Combine(s, "users");
                    if (HostSystem.DirExists(exists))
                    {
                        exists = HostSystem.ToRelativePath(exists);
                        _PathToUserFiles = exists;
                        return exists;
                    }
                }
                string tryplace = Path.Combine(PathToAIML, "users");
                HostSystem.CreateDirectory(tryplace);
                _PathToUserFiles = tryplace;
                return tryplace;

            }
        }

        private string _PathToBotPersonalFiles = null;
        protected string PersonalAiml
        {
            get { return _PathToBotPersonalFiles; }
            set { _PathToUserFiles = value; }
        }

        /// <summary>
        /// The directory to look in for the AIML files
        /// </summary>
        public string PathToAIML
        {
            get
            {
                return Path.Combine(RuntimeDirectory, this.GlobalSettings.grabSetting("aimldirectory"));
            }
        }

        protected string RuntimeDirectory
        {
            get { return Environment.CurrentDirectory; }
        }

        /// <summary>
        /// The directory to look in for the various XML configuration files
        /// </summary>
        public string PathToConfigFiles
        {
            get
            {
                return Path.Combine(RuntimeDirectory, this.GlobalSettings.grabSetting("configdirectory"));
            }
        }

        /// <summary>
        /// The directory into which the various log files will be written
        /// </summary>
        public string PathToLogs
        {
            get
            {
                return Path.Combine(RuntimeDirectory, this.GlobalSettings.grabSetting("logdirectory"));
            }
        }

        /// <summary>
        /// The number of categories this Proccessor has in its graphmaster "brain"
        /// </summary>
        public int Size
        {
            get { return GraphMaster.Size + HeardSelfSayGraph.Size; }
        }


        private GraphMaster _g;

        private GraphMaster _h;


        /// <summary>
        /// The "brain" of the Proccessor
        /// </summary>
        public GraphMaster GraphMaster
        {
            get { return GetGraph("default", _g); }
        }

        public GraphMaster HeardSelfSayGraph
        {
            get { return GetGraph("heardselfsay", _h); }
        }


        /// <summary>
        /// The Markovian "brain" of the Proccessor for generation
        /// </summary>
        public MBrain MBrain { get { return mbrain; } }
        MBrain mbrain = new MBrain();
        public MBrain STM_Brain { get { return stm_brain; } }
        MBrain stm_brain = new MBrain();

        /// <summary>
        /// Proccessor for phonetic HMM
        /// </summary>
        // emission and transition stored as double hash tables
        public PhoneticHmm pHMM { get { return phoneHMM; } }
        PhoneticHmm phoneHMM = new PhoneticHmm();

        /// <summary>
        /// Proccessor for action Markov State Machine
        /// </summary>
        // emission and transition stored as double hash tables
        public actMSM pMSM { get { return botMSM; } }
        actMSM botMSM = new actMSM();
        //public string lastDefMSM;
        //public string lastDefState;

        public Stack<string> conversationStack = new Stack<string>();

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

        #endregion

        #region Delegates

        public delegate void LogMessageDelegate();

        #endregion

        #region Events

        public event LogMessageDelegate WrittenToLog;

        #endregion

        /// <summary>
        /// Ctor
        /// </summary>
        public RTPBot()
            : base(QuerySettings.CogbotDefaults)
        {
            _g = new GraphMaster("default", this);
            _h = new GraphMaster("heardselfsay", this);
            GraphsByName.Add("default", _g);
            GraphsByName.Add("heardselfsay", _h);
            TheNLKB = new NatLangDb(this);
            this.setup();
            BotAsUser = new AIMLbot.User("MySelf", this);
            BotAsUser.IsRoleAcct = true;
            BotAsUser.ListeningGraph = HeardSelfSayGraph;
            BotAsUser.Predicates = GlobalSettings;
            //            BotAsRequestUsed = new AIMLbot.Request("-bank-input-", BotAsUser, this, null);
            AddExcuteHandler("aiml", EvalAIMLHandler);
            AddExcuteHandler("bot", LightWeigthBotDirective);
            this.TheCyc = new CycDatabase(this);
            var v = TheCyc.GetCycAccess;
            this.clojureInterpreter = new ClojureInterpreter(this);
            clojureInterpreter.Init();
            clojureInterpreter.Intern("MyBot", this);
            clojureInterpreter.Intern("BotAsUser", BotAsUser);
            clojureInterpreter.Intern("Users", BotUsers);
#if !(NOT_FAKE_LISTENERS)

            if (!clojureInterpreter.IsSubscriberOf("thisClient"))
            {
                clojureInterpreter.Intern("thisClient", this);
                clojureInterpreter.Intern("True", true);
                clojureInterpreter.Intern("False", false);
                listeners["AIMLBotModule"] = this;
            }
#endif
            AddExcuteHandler("cloj", ClojExecHandler);
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
            var prev = isAcceptingUserInput;
            try
            {
                Request getBotRequest = GetBotRequest("-loadAIMLFromDefaults-");
                isAcceptingUserInput = false;
                AIMLLoader loader = Loader;
                if (!StaticLoader || loader == null)
                {
                    loader = new AIMLLoader(this, getBotRequest);
                }
                Loader = loader;
                loader.loadAIML(getBotRequest);

            }
            finally
            {
                isAcceptingUserInput = prev;
            }
        }

        /// <summary>
        /// Loads AIML from .aiml files into the graphmaster "brain" of the Proccessor
        /// </summary>
        public void loadAIMLFromURI(string path, Request request)
        {
            request = request ?? GetBotRequest("-loadAIMLFromURI-" + path + "-");
            var prev = isAcceptingUserInput;
            try
            {
                isAcceptingUserInput = false;
                loadAIMLFromURI(path, LoaderOptions.GetDefault(request), request);
            }
            finally
            {
                isAcceptingUserInput = prev;
            }
        }


        /// <summary>
        /// Loads AIML from .aiml files into the graphmaster "brain" of the Proccessor
        /// </summary>
        public void loadAIMLFromURI(string path, LoaderOptions options, Request request)
        {
            var prev = isAcceptingUserInput;
            var rb = options.TheRequest;
            try
            {
                isAcceptingUserInput = false;
                options.TheRequest = request;
                AIMLLoader loader = Loader;
                if (!StaticLoader || loader == null)
                {
                    loader = new AIMLLoader(this, request);
                }
                Loader = loader;
                loader.loadAIML(path, options, request);
                // maybe loads settings files if they are there
                string settings = HostSystem.Combine(path, "Settings.xml");
                if (HostSystem.FileExists(settings)) loadSettings(settings);
            }
            finally
            {
                options.TheRequest = rb;
                isAcceptingUserInput = prev;
            }
        }

        /// <summary>
        /// Allows the Proccessor to load a new XML version of some AIML
        /// </summary>
        /// <param name="newAIML">The XML document containing the AIML</param>
        /// <param name="filename">The originator of the XML document</param>
        public void loadAIMLFromXML(XmlDocument newAIML, LoaderOptions filename, Request r)
        {
            var prev = isAcceptingUserInput;
            try
            {
                isAcceptingUserInput = false;
                AIMLLoader loader = Loader;
                if (!StaticLoader || loader == null)
                {
                    loader = new AIMLLoader(this, r);
                }
                loader.loadAIMLNode(newAIML.DocumentElement, filename, r);
            }
            finally
            {
                isAcceptingUserInput = prev;
            }

        }

        /// <summary>
        /// Instantiates the dictionary objects and collections associated with this class
        /// </summary>
        private void setup()
        {
            if (Loader == null)
            {
                Loader = new AIMLLoader(this, GetBotRequest("-bot setup-"));
            }
            var prev = isAcceptingUserInput;
            try
            {
                isAcceptingUserInput = false;
                this.GlobalSettings = new SettingsDictionary("bot.globalsettings", this, null);
                ParentProvider provider = new ParentProvider(() => GlobalSettings);
                this.GenderSubstitutions = new SettingsDictionary("bot.gendersubstitutions", this, null);
                this.Person2Substitutions = new SettingsDictionary("bot.person2substitutions", this, null);
                this.PersonSubstitutions = new SettingsDictionary("bot.personsubstitutions", this, null);
                this.Substitutions = new SettingsDictionary("bot.substitutions", this, null);
                this.DefaultPredicates = new SettingsDictionary("bot.defaultpredicates", this, null);
                this.HeardPredicates = new SettingsDictionary("bot.heardpredicates", this, null);

                this.CustomTags = new Dictionary<string, TagHandler>();
                //this.GraphMaster = new GraphMaster();
                //this.HeardSelfSayGraph = new GraphMaster();
                if (HostSystem.FileExists("AIMLbot.dll")) loadCustomTagHandlers("AIMLbot.dll");
                if (HostSystem.FileExists("AIMLbot.exe")) loadCustomTagHandlers("AIMLbot.exe");

                string names_str = "markovx.trn 5ngram.ngm";
                string[] nameset = names_str.Split(' ');
                foreach (string name in nameset)
                {

                    int loadcount = 0;
                    string file = Path.Combine("trn", name);
                    if (HostSystem.FileExists(file))
                    {
                        StreamReader sr = new StreamReader(file);
                        writeToLog(" **** Markovian Brain LoadMarkovLTM: '{0}'****", file);
                        this.MBrain.Learn(sr);
                        sr.Close();
                        writeToLog(" **** Markovian Brain initialized.: '{0}' **** ", file);
                        loadcount++;
                    }

                    file = Path.Combine("ngm", name);
                    if (HostSystem.FileExists(file))
                    {
                        StreamReader sr = new StreamReader(file);
                        writeToLog(" **** Markovian Brain LoadMarkovLTM: '{0}'**** ", file);
                        this.MBrain.LearnNgram(sr);
                        sr.Close();
                        writeToLog(" **** Markovian Brain N-Gram initialized '{0}'. **** ", file);
                        loadcount++;
                    }

                    if (loadcount == 0)
                    {
                        writeToLog(" **** WARNING: No Markovian Brain Training nor N-Gram file found for '{0}' . **** ", name);
                    }
                }

                if (pHMM.hmmCorpusLoaded == 0)
                {
                    string file = Path.Combine("bgm", "corpus.txt");
                    //if (HostSystem.DirExists(file))
                    if (HostSystem.FileExists(file))
                    {
                        writeToLog("Load Corpus Bigrams: '{0}'", file);
                        StreamReader sr = new StreamReader(file);
                        pHMM.LearnBigramFile(sr);
                        sr.Close();
                        pHMM.hmmCorpusLoaded++;
                        writeToLog("Loaded Corpus Bigrams: '{0}'", file);
                    }
                }
            }
            finally
            {
                isAcceptingUserInput = prev;
            }


        }

        /// <summary>
        /// Loads settings based upon the default location of the Settings.xml file
        /// </summary>
        public void loadSettings()
        {
            // try a safe default setting for the settings xml file
            string path = Path.Combine(RuntimeDirectory, Path.Combine("config", "Settings.xml"));
            this.loadSettings(path);
        }

        public void ReloadAll()
        {
            setup();
            List<CrossAppDomainDelegate> todo = new List<CrossAppDomainDelegate>(ReloadHooks);
            ReloadHooks.Clear();
            foreach (var list in todo)
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

        /// <summary>
        /// Loads settings and configuration info from various xml files referenced in the settings file passed in the args. 
        /// Also generates some default values if such values have not been set by the settings file.
        /// </summary>
        /// <param name="pathToSettings">Path to the settings xml file</param>
        public void loadSettings(string pathToSettings)
        {
            ReloadHooks.Add(() => loadSettings(pathToSettings));
            this.GlobalSettings.loadSettings(pathToSettings);

            // Checks for some important default settings
            SetSaneGlobals(this.GlobalSettings);

            // Load the dictionaries for this RTPBot from the various configuration files
            this.Person2Substitutions.loadSettings(Path.Combine(this.PathToConfigFiles, this.GlobalSettings.grabSetting("person2substitutionsfile")));
            this.PersonSubstitutions.loadSettings(Path.Combine(this.PathToConfigFiles, this.GlobalSettings.grabSetting("personsubstitutionsfile")));
            this.GenderSubstitutions.loadSettings(Path.Combine(this.PathToConfigFiles, this.GlobalSettings.grabSetting("gendersubstitutionsfile")));
            this.DefaultPredicates.loadSettings(Path.Combine(this.PathToConfigFiles, this.GlobalSettings.grabSetting("defaultpredicates")));
            this.Substitutions.loadSettings(Path.Combine(this.PathToConfigFiles, this.GlobalSettings.grabSetting("substitutionsfile")));
            Person2Substitutions.NoDebug = PersonSubstitutions.NoDebug = GenderSubstitutions.NoDebug = Substitutions.NoDebug = true;
            // Grab the splitters for this Proccessor
            this.loadSplitters(Path.Combine(this.PathToConfigFiles, this.GlobalSettings.grabSetting("splittersfile")));
            LastUser = FindOrCreateUser(UNKNOWN_PARTNER);
            LastUser.IsRoleAcct = true;
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
            this.AdminEmail = SaneLocalSettings(settings, "adminemail", "");
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
            SaneLocalSettings(settings, "notacceptinguserinputmessage", "This Proccessor is currently set to not accept user input.");
            SaneLocalSettings(settings, "stripperregex", "[^0-9a-zA-Z]");
        }

        private Unifiable SaneLocalSettings(ISettingsDictionary settings, string name, object value)
        {
            var res = settings.grabSetting(name);
            if (!settings.containsLocalCalled(name))
            {
                settings.addSetting(name, Unifiable.Create(value));
            }
            return res;
        }

        /// <summary>
        /// Loads the splitters for this Proccessor from the supplied config file (or sets up some safe defaults)
        /// </summary>
        /// <param name="pathToSplitters">Path to the config file</param>
        private void loadSplitters(string pathToSplitters)
        {
            if (HostSystem.FileExists(pathToSplitters))
            {
                XmlDocument splittersXmlDoc = new XmlDocument();
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
                                this.Splitters.Add(value);
                            }
                        }
                    }
                }
            }
            if (this.Splitters.Count == 0)
            {
                // if we process lisp and other things
                if (true) return;
                // we don't have any splitters, so lets make do with these...
                this.Splitters.Add(".");
                this.Splitters.Add("!");
                //this.Splitters.Add("?");
                this.Splitters.Add(";");
            }
        }
        #endregion

        #region Logging methods

        /// <summary>
        /// The last message to be entered into the log (for testing purposes)
        /// </summary>
        public string LastLogMessage = String.Empty;

        public OutputDelegate outputDelegate;

        public void writeToLog(Exception e)
        {
            if (e == null) return;
            string s = "ERROR: " + e.Message + " " + e.StackTrace;
            Exception inner = e.InnerException;
            if (inner != null && inner != e)
                writeToLog(inner);
            writeDebugLine(s);
        }

        /// <summary>
        /// Writes a (timestamped) message to the Processor's log.
        /// 
        /// Log files have the form of yyyyMMdd.log.
        /// </summary>
        /// <param name="message">The message to log</param>
        public void writeToLog(string message, params object[] args)
        {
            bool writeToConsole = outputDelegate == null;
            try
            {
                if (args != null && args.Length != 0) message = String.Format(message, args);
            }
            catch (Exception)
            {
                writeToConsole = true;
            }
            if (String.IsNullOrEmpty(message)) return;

            message = message.Trim() + Environment.NewLine;

            if (outputDelegate != null)
            {
                try
                {
                    outputDelegate(message);
                }
                catch (Exception)
                {
                    writeToConsole = true;
                }
                if (writeToConsole || true) writeDebugLine(message);
                message = string.Format("[{0}]: {1}", DateTime.Now.ToString(), message.Trim());
            }
            else
            {
                //if (writeToConsole)
                writeDebugLine(message);
                //                message = string.Format("[{0}]: {1}{2}", DateTime.Now.ToString(), message.Trim(), Environment.NewLine);
                message = string.Format("[{0}]: {1}", DateTime.Now.ToString(), message.Trim());
                //string m = message.AsString().ToLower();
                //if (m.Contains("error") || m.Contains("excep"))
            }
            writeToLog0(message);
        }

        public void writeToLog0(string message)
        {

            if (message.Contains("rocreate by cloning, or software copy"))
            {

            }
            this.LastLogMessage = message;
            if (!this.IsLogging) return;
            lock (this.LoggingLock)
            {
                //  this.LogBuffer.Add(DateTime.Now.ToString() + ": " + message + Environment.NewLine);
                this.LogBuffer.Add(message);
                if (this.LogBuffer.Count > this.MaxLogBufferSize - 1)
                {
                    // Write out to log file
                    HostSystem.CreateDirectory(this.PathToLogs);

                    Unifiable logFileName = DateTime.Now.ToString("yyyyMMdd") + ".log";
                    FileInfo logFile = new FileInfo(Path.Combine(this.PathToLogs, logFileName));
                    StreamWriter writer;
                    if (!logFile.Exists)
                    {
                        writer = logFile.CreateText();
                    }
                    else
                    {
                        writer = logFile.AppendText();
                    }

                    foreach (Unifiable msg in this.LogBuffer)
                    {
                        writer.WriteLine(msg);
                    }
                    writer.Close();
                    this.LogBuffer.Clear();
                }
            }
            if (!Equals(null, this.WrittenToLog))
            {
                this.WrittenToLog();
            }
        }

        #endregion

        // Persistent user tracking
        public readonly Dictionary<string, User> BotUsers = new Dictionary<string, User>();

        public void SetChatOnOff(string username, bool value)
        {
            lock (BotUsers)
            {
                foreach (var u in BotUsers.Values)
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
            Request request = GetBotRequest("Add aiml '" + aimlText + "' to " + graph);
            var prev = request.Graph;
            try
            {
                request.Graph = graph;
                LoaderOptions loader = LoaderOptions.GetDefault(request);
                loader.Graph = graph;
                string s = string.Format("<aiml graph=\"{0}\">{1}</aiml>", graph.ScriptingName, aimlText);
                Loader.loadAIMLString(s, loader, request);
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

        #region Conversation methods

        /// <summary> 
        /// Given some raw input string username/unique ID creates a response for the user
        /// </summary>
        /// <param name="rawInput">the raw input</param>
        /// <param name="UserGUID">a usersname</param>
        /// <returns>the result to be output to the user</returns>        
        public string ChatString(string rawInput, string username)
        {
            var r = GetRequest(rawInput, username);
            r.IsTraced = true;
            return Chat(r).Output;
        }


        public RequestImpl GetRequest(string rawInput, string username)
        {
            var r = new AIMLbot.Request(rawInput, FindOrCreateUser(username), this, null);
            r.IsTraced = true;
            return r;
        }

        /// <summary>
        /// Given some raw input and a unique ID creates a response for a new user
        /// </summary>
        /// <param name="rawInput">the raw input</param>
        /// <param name="UserGUID">an ID for the new user (referenced in the result object)</param>
        /// <returns>the result to be output to the user</returns>
        public Result Chat(string rawInput, string UserGUID)
        {
            Request request = GetRequest(rawInput, UserGUID);
            request.IsTraced = true;
            return this.Chat(request);
        }


        private void LoadInputPaths(Request request, AIMLLoader loader, Unifiable[] rawSentences, AIMLbot.Result result)
        {
            int maxInputs = request.MaxInputs;
            int numInputs = 0;
            int sentenceNum = 0;
            int topicNum = 0;
            int thatNum = 0;
            string lastInput = "";
            {
                foreach (Unifiable sentence0 in rawSentences)
                {
                    string sentence = sentence0;
                    result.InputSentences.Add(sentence);
                    sentence = sentence.Trim();
                    while (sentence.EndsWith(".") || sentence.EndsWith(","))
                    {
                        sentence = sentence.Substring(0, sentence.Length - 1).Trim();
                    }
                    if (sentence.Length == 0)
                    {
                        writeToLog("skipping input sentence " + sentence0);
                        continue;
                    }
                    sentenceNum++;
                    topicNum = 0;
                    if (maxInputs == 1)
                    {
                        Unifiable path = loader.generatePath(sentence,
                                                             //thatNum + " " +
                                                             request.user.getLastBotOutputForThat(), request.Flags,
                                                             //topicNum + " " +
                                                             request.user.TopicSetting, true);
                        numInputs++;
                        result.NormalizedPaths.Add(path);
                        if (numInputs >= maxInputs) return;
                        continue;
                    }
                    foreach (Unifiable topic0 in request.Topics)
                    {
                        Unifiable topic = topic0;
                        topicNum++;
                        if (topic.IsLongWildCard())
                        {
                            topic = NOTOPIC;
                        }
                        thatNum = 0;
                        foreach (Unifiable that in request.BotOutputs)
                        {
                            thatNum++;
                            string thats = that.AsString();
                            Unifiable path = loader.generatePath(sentence, //thatNum + " " +
                                                                 thats, request.Flags,
                                //topicNum + " " +
                                                                 topic, true);
                            if (that.IsLongWildCard())
                            {
                                if (thatNum > 1)
                                {
                                    continue;
                                }
                                if (topic.IsLongWildCard())
                                {
                                    topic = "NOTHAT";
                                }
                            }
                            string thisInput = path.LegacyPath.AsString().Trim().ToUpper();
                            if (thisInput == lastInput) continue;
                            lastInput = thisInput;
                            numInputs++;
                            result.NormalizedPaths.Add(path);
                            if (numInputs >= maxInputs) return;

                        }
                    }
                }
            }
        }

        List<Thread> ThreadList = new List<Thread>();

        private void RunTask(ThreadStart action, string name, int maxTime)
        {
            Thread t = RunTask(action, name);
            Thread tr = new Thread(() =>
                                       {
                                           try
                                           {
                                               try
                                               {
                                                   Thread.Sleep(10000);
                                                   lock (ThreadList) if (t.IsAlive) t.Interrupt();

                                               }
                                               catch (Exception e)
                                               {
                                                   //RTPBot.writeDebugLine("ERROR " + name + " " + e);
                                               }
                                           }
                                           finally
                                           {
                                               lock (ThreadList) ThreadList.Remove(Thread.CurrentThread);
                                           }
                                       }) { Name = "Killer of " + name };
            tr.Start();
        }

        private Thread RunTask(ThreadStart action, string name)
        {
            Thread tr = new Thread(() =>
            {
                try
                {
                    try
                    {
                        action();
                    }
                    catch (Exception e)
                    {
                        RTPBot.writeDebugLine("ERROR " + name + " " + e);
                    }
                }
                finally
                {
                    lock (ThreadList) ThreadList.Remove(Thread.CurrentThread);
                }
            }) { Name = name };
            lock (ThreadList) ThreadList.Add(tr);
            tr.Start();
            return tr;
        }

        private JoinedTextBuffer currentEar = new JoinedTextBuffer();
        public AIMLbot.Result HeardSelfSay(string message)
        {
            currentEar.AddMore(message);
            if (!currentEar.IsReady())
            {
                return null;
            }
            message = currentEar.GetMessage();
            currentEar = new JoinedTextBuffer();
            //return null;
            RunTask(() => HeardSelfSayNow(message), "heardSelfSay: " + message, 500);
            return null;
        }

        public AIMLbot.Result HeardSelfSayNow(string message)
        {
            if (message == null) return null;
            message = message.Trim();
            if (message == "") return null;
            if (message.Contains("<"))
            {
                var v = AIMLTagHandler.getNode("<pre>" + message + "</pre>");
                writeDebugLine("AIMLTRACE: " + message + " -> " + v.InnerText);
                message = v.InnerText;
            }
            //message = swapPerson(message);
            //writeDebugLine("HEARDSELF SWAP: " + message);
            if (LastUser != null)
            {
                lock (LastUser.QueryLock)
                {
                    var LR = LastUser.LastResult;
                    if (LR != null)
                        LR.AddOutputSentences(null, message);
                }
            }

            writeDebugLine("-----------------------------------------------------------------");
            AddHeardPreds(message, HeardPredicates);
            writeDebugLine("-----------------------------------------------------------------");
            if (!ListeningToSelf)
            {
                writeDebugLine("SELF: " + message);
                writeDebugLine("-----------------------------------------------------------------");
                return null;
            }
            RTPBot.writeDebugLine("HEARDSELF: " + message);
            writeDebugLine("-----------------------------------------------------------------");
            try
            {
                if (message == null || message.Length < 4) return null;
                //return null;
                try
                {
                    Request r = new AIMLbot.Request(message, BotAsUser, this, null);
                    r.IsTraced = false;
                    return Chat0(r, HeardSelfSayGraph);
                }
                catch (Exception e)
                {
                    writeToLog(e);
                    return null;
                }
            }
            finally
            {
                BotAsUser.Predicates.addSetting("lastsaid", message);
            }
        }

        private void AddHeardPreds(string message, SettingsDictionary dictionary)
        {
            if (message == null) return;
            writeDebugLine("-----------------------------------------------------------------");
            foreach (var s in message.Trim().Split(new char[] { '!', '?', '.' }, StringSplitOptions.RemoveEmptyEntries))
            {
                AddHeardPreds0(s, dictionary);
            }
            writeDebugLine("-----------------------------------------------------------------");
            //RTPBot.writeDebugLine("" + dictionary.ToDebugString());
        }
        private void AddHeardPreds0(Unifiable unifiable, SettingsDictionary dictionary)
        {
            if (unifiable.IsEmpty) return;
            Unifiable first = unifiable.First();
            if (first.IsEmpty) return;
            Unifiable rest = unifiable.Rest();
            if (rest.IsEmpty) return;
            dictionary.addSetting(first, rest);
            AddHeardPreds0(rest, dictionary);
        }

        private string swapPerson(string inputString)
        {
            string temp = Loader.Normalize(inputString, true);
            //temp = ApplySubstitutions.Substitute(this, this.PersonSubstitutions, temp);
            temp = ApplySubstitutions.Substitute(this, this.Person2Substitutions, temp);
            return temp;
        }

        public Unifiable CleanupCyc(string text)
        {
            if (text == null) return null;
            if (text == "")
            {
                return null;
            }
            text = text.Trim();
            if (text == "")
            {
                writeToLog(" had white string ");
                return null;
            }
            if (TheCyc != null)
            {
                text = TheCyc.CleanupCyc(text);
            }
            return text.Replace("#$", " ").Replace("  ", " ").Trim();
        }

        /// <summary>
        /// Given a request containing user input, produces a result from the Proccessor
        /// </summary>
        /// <param name="request">the request from the user</param>
        /// <returns>the result to be output to the user</returns>
        /// 
        public AIMLbot.Result Chat(Request request)
        {
            try
            {
                return Chat(request, request.Graph ?? GraphMaster);
            }
            finally
            {
                AddHeardPreds(request.rawInput, HeardPredicates);
            }
        }

        public AIMLbot.Result Chat(Request request, GraphMaster G)
        {
            var prev = request.Graph;
            request.Graph = G;
            try
            {

                var v = Chat0(request, G);
                return v;
            }
            finally
            {
                request.Graph = prev;

            }
        }

        public AIMLbot.Result Chat0(Request request, GraphMaster G)
        {
            bool isTraced = request.IsTraced || G == null;
            User user = request.user ?? LastUser;
            UndoStack undoStack = UndoStack.GetStackFor(request);
            AIMLbot.Result res;
            undoStack.pushValues(user.Predicates, "rawinput", request.rawInput);
            undoStack.pushValues(user.Predicates, "input", request.rawInput);
            lock (user.QueryLock)
            {
                res = Chat0000(request, user, G);
            }
            undoStack.UndoAll();
            return res;

        }

        private AIMLbot.Result Chat0000(Request request, User user, GraphMaster G)
        {
            //LastUser = user;
            AIMLbot.Result result;

            bool isTraced = request.IsTraced || G == null;
            //chatTrace = null;

            streamDepth++;

            string rawInputString = request.rawInput.AsString();

            if (rawInputString.StartsWith("@"))
            {
                result = request.CreateResult(request);
                if (chatTrace) result.IsTraced = isTraced;
                bool myBotBotDirective = BotDirective(request.user, rawInputString, result.WriteLine);
                if (myBotBotDirective)
                {
                    return result;
                }
            }

            var orig = request.BotOutputs;
            if (AIMLLoader.ContainsAiml(rawInputString))
            {
                try
                {
                    result = ImmediateAiml(AIMLTagHandler.getNode(rawInputString), request, Loader, null);
                    request.rawInput = result.Output;
                }
                catch (Exception e)
                {
                    isTraced = true;
                    writeToLog(e);
                    writeToLog("ImmediateAiml: ERROR: " + e);
                }
            }

            if (this.isAcceptingUserInput)
            {
                // Normalize the input
                AIMLLoader loader = Loader;
                if (!StaticLoader || loader == null)
                {
                    loader = new AIMLLoader(this, request);
                }
                Loader = loader;
                //RTParser.Normalize.SplitIntoSentences splitter = new RTParser.Normalize.SplitIntoSentences(this);
                Unifiable[] rawSentences = new Unifiable[] { request.rawInput };//splitter.Transform(request.rawInput);
                result = request.CreateResult(request);
                if (chatTrace) result.IsTraced = isTraced;
                LoadInputPaths(request, loader, rawSentences, result);
                int NormalizedPathsCount = result.NormalizedPaths.Count;

                if (isTraced && NormalizedPathsCount != 1)
                {
                    foreach (Unifiable path in result.NormalizedPaths)
                    {

                        writeToLog("  i: " + path.LegacyPath);
                    }
                    writeToLog("NormalizedPaths.Count = " + NormalizedPathsCount);
                }

                G = G ?? GraphMaster;
                // grab the templates for the various sentences from the graphmaster
                foreach (Unifiable path in result.NormalizedPaths)
                {
                    QueryList ql = G.gatherQueriesFromGraph(path, request, MatchState.UserInput);
                    if (ql.TemplateCount > 0)
                    {
                        request.TopLevel = ql;
                        result.AddSubqueries(ql);
                    }
                }




                //todo pick and chose the queries
                // if (result.SubQueries.Count != 1)
                {
                    if (isTraced)
                    {
                        string s = "AIMLTRACE: SubQueries.Count = " + result.SubQueries.Count;
                        foreach (var path in result.SubQueries)
                        {
                            s += Environment.NewLine;
                            s += "  " + Unifiable.ToVMString(path.FullPath);
                        }
                        writeToLog(s);
                        Console.Out.Flush();
                    }
                }

                // process the templates into appropriate output
                int solutions;
                bool hasMoreSolutions;
                ProccessTemplates(request, result, out solutions, out hasMoreSolutions);


                if (isTraced || result.OutputSentenceCount != 1)
                {
                    if (isTraced)
                    {
                        Console.Out.Flush();
                        string s = "AIMLTRACE: result.OutputSentenceCount = " + result.OutputSentenceCount;
                        foreach (var path in result.OutputSentences)
                        {
                            s += Environment.NewLine;
                            s += "  " + Unifiable.ToVMString(path);
                        }
                        s += Environment.NewLine;
                        writeToLog(s);
                        Console.Out.Flush();
                    }

                    foreach (SubQuery path in result.SubQueries)
                    {
                        //string s = "AIMLTRACE QUERY:  " + path.FullPath;

                        //writeToLog("\r\n tt: " + path.Request.Graph);
                        if (chatTrace)
                        {
                            //bot.writeChatTrace("\"L{0}\" -> \"{1}\" ;\n", result.SubQueries.Count, path.FullPath.ToString());
                            writeChatTrace("\"L{0}\" -> \"L{1}\" ;\n", result.SubQueries.Count - 1, result.SubQueries.Count);
                            writeChatTrace("\"L{0}\" -> \"REQ:{1}\" ;\n", result.SubQueries.Count, request.ToString());
                            writeChatTrace("\"REQ:{0}\" -> \"PATH:{1}\" [label=\"{2}\"];\n", request.ToString(), result.SubQueries.Count, path.FullPath.ToString());
                            writeChatTrace("\"REQ:{0}\" -> \"RPY:{1}\" ;\n", request.ToString(), result.RawOutput.ToString());
                        }
                    }
                }
            }
            else
            {
                string nai = NotAcceptingUserInputMessage;
                if (isTraced)
                    writeToLog("ERROR {0} getting back {1}", request, nai);
                result = request.CreateResult(request);
                result.AddOutputSentences(null, nai);
                return result;
            }

            // populate the Result object
            result.Duration = DateTime.Now - request.StartedOn;
            User popu = request.user ?? result.user;
            if (result.ParentResult == null)
            {
                // toplevel result
                popu.addResult(result);
            }
            popu.addResultTemplates(request);
            streamDepth--;
            return result;
        }


        private void ProccessTemplates(Request request, Result result, out int solutions, out bool hasMoreSolutions)
        {
            hasMoreSolutions = true;
            solutions = 0;
            foreach (SubQuery query in result.SubQueries)
            {
                result.CurrentQuery = query;
                UList queryTemplates = query.Templates;
                if (queryTemplates != null && queryTemplates.Count > 0)
                {
                    foreach (TemplateInfo s in queryTemplates)
                    {
                        // Start each the same
                        s.Rating = 1.0;
                        try
                        {
                            s.Query = query;
                            query.CurrentTemplate = s;
                            bool createdOutput;
                            bool templateSucceeded;
                            proccessResponse(query, request, result, s.Output, s.Guard, out createdOutput,
                                             out templateSucceeded, null, s);


                            if (createdOutput)
                            {
                                solutions++;
                            }
                            if (request.IsComplete(result))
                            {
                                hasMoreSolutions = false;
                                return;
                            }
                            //break; // KHC: single vs. Multiple
                            if ((createdOutput) && (request.ProcessMultipleTemplates == false)) break;
                        }
                        catch (Exception e)
                        {
                            writeToLog(e);
                            if (this.WillCallHome)
                            {
                                this.phoneHome(e.Message, request);
                            }
                            this.writeToLog("WARNING! A problem was encountered when trying to process the input: " +
                                            request.rawInput + " with the template: \"" + s + "\"");
                        }

                    }
                }
            }
        }


        public bool chatTrace = true;
        private int streamDepth;
        StreamWriter chatTraceW = null;
        Object chatTraceO = new object();

        public void writeChatTrace(string message, params object[] args)
        {
            if (!chatTrace) return;
            try
            {
                lock (chatTraceO)
                {
                    if (chatTraceW == null)
                    {
                        chatTraceW = new StreamWriter("bgm\\chatTrace.dot");
                        chatTraceW.WriteLine("digraph G {");
                        streamDepth = 1;
                    }
                    if (streamDepth < 0) streamDepth = 0;
                    int w = streamDepth;
                    while (w-- < 0)
                    {
                        chatTraceW.Write("  ");
                    }
                    if (args != null && args.Length != 0) message = String.Format(message, args);
                    chatTraceW.WriteLine(message);
                    //writeDebugLine(message);
                    if (streamDepth <= 0 && chatTraceW != null)
                    {
                        chatTraceW.WriteLine("}");
                        chatTraceW.Close();
                        streamDepth = 0;
                        chatTraceW = null;
                    }
                }
            }
            catch (Exception)
            {
            }
        }

        public static string GetAttribValue(XmlNode templateNode, string attribName, string defaultIfEmpty)
        {
            attribName = attribName.ToLower();
            foreach (XmlAttribute attrib in templateNode.Attributes)
            {
                if (attrib.Name.ToLower() == attribName)
                {
                    return attrib.Value;
                }
            }
            return defaultIfEmpty;
        }


        public AIMLbot.Result ImmediateAiml(XmlNode templateNode, Request request0, AIMLLoader loader, AIMLTagHandler handler)
        {
            var prev = isAcceptingUserInput;
            try
            {
                isAcceptingUserInput = true;
                return ImmediateAIML0(request0, templateNode, handler);
            }
            finally
            {
                isAcceptingUserInput = prev;
            }
        }

        private AIMLbot.Result ImmediateAIML0(Request parentRequest, XmlNode templateNode, AIMLTagHandler handler)
        {
            string requestName = "<aiml>" + templateNode.OuterXml + "</aiml>";
            RTPBot request0Proccessor = this;
            GuardInfo sGuard = null;
            Request request = null;
            User user = BotAsUser;

            if (parentRequest != null)
            {
                user = parentRequest.user;
                requestName = parentRequest.rawInput;
            }

            //  if (request == null)

            request = parentRequest;// new AIMLbot.Request(requestName, user, request0Proccessor, (AIMLbot.Request)parentRequest);

            if (parentRequest != null)
            {
                request.Graph = parentRequest.Graph;
                request.depth = parentRequest.depth + 1;
            }

            AIMLbot.Result result = request.CreateResult(request);
            request.CurrentResult = result;
            if (request.CurrentResult != result)
            {
                writeToLog("ERROR did not set the result!");
            }
            if (request.Graph != result.Graph)
            {
                writeToLog("ERROR did not set the result!");
            }
            TemplateInfo templateInfo = null;//
            if (false)
            {
                templateInfo = new TemplateInfo(templateNode, null, null, null, null);
            }
            bool templateSucceeded;
            bool createdOutput;
            SubQuery query = request.CurrentQuery;
            bool doUndos = false;
            if (query == null)
            {
                query = new SubQuery(requestName, result, request);
                request.IsTraced = true;
                doUndos = true;
            }
            proccessResponse(query, request, result, templateNode, sGuard, out createdOutput, out templateSucceeded,
                             handler, templateInfo);
            if (doUndos) query.UndoAll();
            return result;
        }

        public void proccessResponse(SubQuery query, Request request, Result result, XmlNode templateNode, GuardInfo sGuard, out bool createdOutput, out bool templateSucceeded, AIMLTagHandler handler, TemplateInfo templateInfo)
        {
            request.CurrentResult = result;
            query = query ?? request.CurrentQuery;
            UndoStack undoStack = UndoStack.GetStackFor(query);
            templateInfo = templateInfo ?? query.CurrentTemplate;
            //request.CurrentQuery = query;
            if (!request.CanUseTemplate(templateInfo, result))
            {
                templateSucceeded = false;
                createdOutput = false;
                return;
            }
            proccessResponse0(query, request, result, templateNode, sGuard, out createdOutput, out templateSucceeded,
                             handler, templateInfo);

            undoStack.UndoAll();

        }
        public void proccessResponse0(SubQuery query, Request request, Result result, XmlNode sOutput, GuardInfo sGuard, out bool createdOutput, out bool templateSucceeded, AIMLTagHandler handler, TemplateInfo templateInfo)
        {
            bool isTraced = request.IsTraced || result.IsTraced || !isAcceptingUserInput;
            //XmlNode guardNode = AIMLTagHandler.getNode(s.Guard.InnerXml);
            bool usedGuard = sGuard != null && sGuard.Output != null;
            sOutput = sOutput ?? templateInfo.Output;
            string output = sOutput.OuterXml;
            LineInfoElement templateNode = LineInfoElement.Cast(sOutput);
            if (usedGuard)
            {
                string guardStr = "<when>" + sGuard.Output.InnerXml + " GUARDBOM " + sOutput.OuterXml + "</when>";
                templateNode = AIMLTagHandler.getNode(guardStr, sOutput);
            }

            string outputSentence = this.processNode(templateNode, query, request, result, request.user, handler);
            templateSucceeded = !Unifiable.IsFalse(outputSentence);

            int f = outputSentence.IndexOf("GUARDBOM");
            if (f < 0)
            {
                string o = AsOutputSentence(outputSentence);
                if (IsOutputSentence(o))
                {
                    if (isTraced)
                        writeToLog("AIMLTRACE '{0}' TEMPLATE={1}", o, AIMLLoader.CatTextInfo(templateNode));
                    createdOutput = true;
                    templateSucceeded = true;
                    result.AddOutputSentences(templateInfo, o);
                }
                else
                {
                    createdOutput = false;
                }
                if (!createdOutput && isTraced && isAcceptingUserInput)
                    writeToLog("UNUSED '{0}' TEMPLATE={1}", o, AIMLLoader.CatTextInfo(templateNode));
                return;
            }

            try
            {
                string left = outputSentence.Substring(0, f).Trim();
                templateSucceeded = !Unifiable.IsFalse(left);
                if (!templateSucceeded)
                {
                    createdOutput = false;
                    return;
                }
                string lang = GetAttribValue(sGuard.Output, "lang", "cycl").ToLower();

                try
                {
                    Unifiable ss = SystemExecute(left, lang, request);
                    if (Unifiable.IsFalse(ss))
                    {
                        if (isTraced)
                            writeToLog("GUARD FALSE '{0}' TEMPLATE={1}", request,
                                       AIMLLoader.CatTextInfo(templateNode));
                        templateSucceeded = false;
                        createdOutput = false;
                        return;
                    }
                    else
                    {
                        templateSucceeded = true;
                    }
                }
                catch (Exception e)
                {
                    writeToLog(e);
                    templateSucceeded = false;
                    createdOutput = false;
                    return;
                }

                //part the BOM
                outputSentence = outputSentence.Substring(f + 9);
                string o = AsOutputSentence(outputSentence);
                if (IsOutputSentence(o))
                {
                    if (isTraced)
                        writeToLog(query.Graph + ": GUARD SUCCESS '{0}' TEMPLATE={1}", o, AIMLLoader.CatTextInfo(templateNode));
                    templateSucceeded = true;
                    createdOutput = true;
                    result.AddOutputSentences(templateInfo, o);
                    return;
                }
                else
                {
                    writeToLog("GUARD SKIP '{0}' TEMPLATE={1}", outputSentence,
                               AIMLLoader.CatTextInfo(templateNode));
                }
                templateSucceeded = false;
                createdOutput = false;
                return;
            }
            catch (System.Exception ex)
            {
                writeToLog(ex);
                templateSucceeded = false;
                createdOutput = false;
                return;
            }
        }

        private bool IsOutputSentence(string sentence)
        {
            if (sentence == null) return false;
            string o = AsOutputSentence(sentence);
            if (o == null) return false;
            return o.Length > 0;
        }

        public string AsOutputSentence(string sentence)
        {
            if (sentence == null) return null;
            return CleanupCyc(sentence);
        }

        /// <summary>
        /// Recursively evaluates the template nodes returned from the Proccessor
        /// </summary>
        /// <param name="node">the node to evaluate</param>
        /// <param name="query">the query that produced this node</param>
        /// <param name="request">the request from the user</param>
        /// <param name="result">the result to be sent to the user</param>
        /// <param name="user">the user who originated the request</param>
        /// <returns>the output Unifiable</returns>
        public string processNode(XmlNode node, SubQuery query, Request request, Result result, User user, AIMLTagHandler parent)
        {
            // check for timeout (to avoid infinite loops)
            if (request != null && DateTime.Now > request.TimesOutAt)
            {
                request.Proccessor.writeToLog(
                    "WARNING! Request timeout. User: {0} raw input: \"{1}\" processing template: \"{2}\"",
                    request.user.UserID, request.rawInput,
                    (query == null ? "-NOQUERY-" : query.Templates.Count.ToString()));
                request.hasTimedOut = true;
                return Unifiable.Empty;
            }

            // process the node
            AIMLTagHandler tagHandler = GetTagHandler(user, query, request, result, node, parent);
            if (ReferenceEquals(null, tagHandler))
            {
                if (node.NodeType == XmlNodeType.Comment) return Unifiable.Empty;
                if (node.NodeType != XmlNodeType.Text)
                {
                    writeToLog("XML ?? " + node.NodeType + "  " + node.OuterXml);
                }
                string s = node.InnerText.Trim();
                if (String.IsNullOrEmpty(s))
                {
                    return Unifiable.Empty;
                }
                return s;
            }
            tagHandler.SetParent(parent);
            var cp = tagHandler.CompleteAimlProcess();
            return cp;
        }


        internal AIMLTagHandler GetTagHandler(User user, SubQuery query, Request request, Result result, XmlNode node, AIMLTagHandler handler)
        {
            var tag = GetTagHandler00(user, query, request, result, node);
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
            }
            return tag;
        }
        internal AIMLTagHandler GetTagHandler00(User user, SubQuery query, Request request, Result result, XmlNode node)
        {
            AIMLTagHandler tagHandler = this.getBespokeTags(user, query, request, result, node);
            if (Equals(null, tagHandler))
            {
                switch (node.Name.ToLower())
                {
                    case "template":
                        tagHandler = new AIMLTagHandlers.template(this, user, query, request, result, node);
                        break;
                    case "aiml":
                        tagHandler = new AIMLTagHandlers.aiml(this, user, query, request, result, node);
                        break;
                    case "aimlexec":
                        tagHandler = new AIMLTagHandlers.aimlexec(this, user, query, request, result, node);
                        break;
                    case "root":
                        tagHandler = new AIMLTagHandlers.root(this, user, query, request, result, node);
                        break;
                    case "topic":
                        tagHandler = new AIMLTagHandlers.topic(this, user, query, request, result, node);
                        break;
                    case "category":
                        tagHandler = new AIMLTagHandlers.category(this, user, query, request, result, node);
                        break;
                    case "and":
                        tagHandler = new AIMLTagHandlers.and(this, user, query, request, result, node);
                        break;
                    case "or":
                        tagHandler = new AIMLTagHandlers.or(this, user, query, request, result, node);
                        break;
                    case "optional":
                        tagHandler = new AIMLTagHandlers.optional(this, user, query, request, result, node);
                        break;
                    case "isa":
                        tagHandler = new AIMLTagHandlers.isa(this, user, query, request, result, node);
                        break;
                    case "bot":
                        tagHandler = new AIMLTagHandlers.bot(this, user, query, request, result, node);
                        break;
                    case "condition":
                        tagHandler = new AIMLTagHandlers.condition(this, user, query, request, result, node);
                        break;
                    case "date":
                        tagHandler = new AIMLTagHandlers.date(this, user, query, request, result, node);
                        break;
                    case "formal":
                        tagHandler = new AIMLTagHandlers.formal(this, user, query, request, result, node);
                        break;
                    case "gender":
                        tagHandler = new AIMLTagHandlers.gender(this, user, query, request, result, node);
                        break;
                    case "get":
                        tagHandler = new AIMLTagHandlers.get(this, user, query, request, result, node);
                        break;
                    case "gossip":
                        tagHandler = new AIMLTagHandlers.gossip(this, user, query, request, result, node);
                        break;
                    case "id":
                        tagHandler = new AIMLTagHandlers.id(this, user, query, request, result, node);
                        break;
                    case "input":
                        tagHandler = new AIMLTagHandlers.input(this, user, query, request, result, node);
                        break;
                    case "javascript":
                        tagHandler = new AIMLTagHandlers.javascript(this, user, query, request, result, node);
                        break;
                    case "learn":
                        tagHandler = new AIMLTagHandlers.learn(this, user, query, request, result, node);
                        break;
                    case "lowercase":
                        tagHandler = new AIMLTagHandlers.lowercase(this, user, query, request, result, node);
                        break;
                    case "person":
                        tagHandler = new AIMLTagHandlers.person(this, user, query, request, result, node);
                        break;
                    case "person2":
                        tagHandler = new AIMLTagHandlers.person2(this, user, query, request, result, node);
                        break;
                    case "random":
                        tagHandler = new AIMLTagHandlers.random(this, user, query, request, result, node);
                        break;
                    case "sentence":
                        tagHandler = new AIMLTagHandlers.sentence(this, user, query, request, result, node);
                        break;
                    case "set":
                        tagHandler = new AIMLTagHandlers.set(this, user, query, request, result, node);
                        break;
                    case "size":
                        tagHandler = new AIMLTagHandlers.size(this, user, query, request, result, node);
                        break;
                    case "sr":
                        tagHandler = new AIMLTagHandlers.sr(this, user, query, request, result, node);
                        break;
                    case "srai":
                        tagHandler = new AIMLTagHandlers.srai(this, user, query, request, result, node);
                        break;
                    case "star":
                        tagHandler = new AIMLTagHandlers.star(this, user, query, request, result, node);
                        break;
                    case "system":
                        tagHandler = new AIMLTagHandlers.system(this, user, query, request, result, node);
                        break;
                    case "that":
                        tagHandler = new AIMLTagHandlers.that(this, user, query, request, result, node);
                        break;
                    case "thatstar":
                        tagHandler = new AIMLTagHandlers.thatstar(this, user, query, request, result, node);
                        break;
                    case "think":
                        tagHandler = new AIMLTagHandlers.think(this, user, query, request, result, node);
                        break;
                    case "topicstar":
                        tagHandler = new AIMLTagHandlers.topicstar(this, user, query, request, result, node);
                        break;
                    case "uppercase":
                        tagHandler = new AIMLTagHandlers.uppercase(this, user, query, request, result, node);
                        break;
                    case "version":
                        tagHandler = new AIMLTagHandlers.version(this, user, query, request, result, node);
                        break;
                    case "cycsystem":
                        tagHandler = new AIMLTagHandlers.cycsystem(this, user, query, request, result, node);
                        break;
                    case "cycretract":
                        tagHandler = new AIMLTagHandlers.cycretract(this, user, query, request, result, node);
                        break;
                    case "cycassert":
                        tagHandler = new AIMLTagHandlers.cycassert(this, user, query, request, result, node);
                        break;
                    case "cycterm":
                        tagHandler = new AIMLTagHandlers.cycterm(this, user, query, request, result, node);
                        break;
                    case "cycquery":
                        tagHandler = new AIMLTagHandlers.cycquery(this, user, query, request, result, node);
                        break;
                    case "cyccondition":
                        tagHandler = new AIMLTagHandlers.cyccondition(this, user, query, request, result, node);
                        break;
                    case "cycphrase":
                        tagHandler = new AIMLTagHandlers.cycphrase(this, user, query, request, result, node);
                        break;
                    case "cycparaphrase":
                        tagHandler = new AIMLTagHandlers.cycphrase(this, user, query, request, result, node);
                        break;
                    case "guard":
                        tagHandler = new AIMLTagHandlers.guard(this, user, query, request, result, node);
                        break;
                    case "guardstar":
                        tagHandler = new AIMLTagHandlers.guardstar(this, user, query, request, result, node);
                        break;
                    case "cycrandom":
                        tagHandler = new AIMLTagHandlers.cycrandom(this, user, query, request, result, node);
                        break;
                    case "space":
                        tagHandler = new AIMLTagHandlers.space(this, user, query, request, result, node);
                        break;
                    case "markov":
                        tagHandler = new AIMLTagHandlers.markov(this, user, query, request, result, node);
                        break;
                    case "soundcode":
                        tagHandler = new AIMLTagHandlers.soundcode(this, user, query, request, result, node);
                        break;

                    // MSM
                    case "msm":
                        tagHandler = new AIMLTagHandlers.msm(this, user, query, request, result, node);
                        break;
                    case "processmsm":
                        tagHandler = new AIMLTagHandlers.process_msm(this, user, query, request, result, node);
                        break;
                    case "setstate":
                        tagHandler = new AIMLTagHandlers.setstate(this, user, query, request, result, node);
                        break;
                    case "state":
                        tagHandler = new AIMLTagHandlers.state(this, user, query, request, result, node);
                        break;
                    case "transition":
                        tagHandler = new AIMLTagHandlers.transition(this, user, query, request, result, node);
                        break;
                    case "setevidence":
                        tagHandler = new AIMLTagHandlers.setevidence(this, user, query, request, result, node);
                        break;
                    case "evidenceassoc":
                        tagHandler = new AIMLTagHandlers.evidence_assoc(this, user, query, request, result, node);
                        break;
                    case "evidencepattern":
                        tagHandler = new AIMLTagHandlers.evidence_pattern(this, user, query, request, result, node);
                        break;
                    case "evidencestate":
                        tagHandler = new AIMLTagHandlers.evidencestate(this, user, query, request, result, node);
                        break;
                    case "dependentmachine":
                        tagHandler = new AIMLTagHandlers.dependentmachine(this, user, query, request, result, node);
                        break;
                    case "responsetopic":
                        tagHandler = new AIMLTagHandlers.response_topic(this, user, query, request, result, node);
                        break;

                    case "push":
                        tagHandler = new AIMLTagHandlers.push(this, user, query, request, result, node);
                        break;
                    case "pop":
                        tagHandler = new AIMLTagHandlers.pop(this, user, query, request, result, node);
                        break;
                    case "peekstack":
                        tagHandler = new AIMLTagHandlers.peekstack(this, user, query, request, result, node);
                        break;


                    case "regex":
                        tagHandler = new AIMLTagHandlers.regex(this, user, query, request, result, node);
                        break;


                    case "#text":
                        return null;
                    case "#comment":
                        return new AIMLTagHandlers.verbatum(node.OuterXml, this, user, query, request, result, node);
                    case "br":
                        return new AIMLTagHandlers.verbatum("\n", this, user, query, request, result, node);
                    case "p":
                        return new AIMLTagHandlers.verbatum("\n\n", this, user, query, request, result, node);
                    default:
                        break;
                }
            }
            if (tagHandler == null)
            {
                // "bot", "favorite", "fav" 
                foreach (KeyValuePair<string, string> prefix in new[]
                                                                    {
                                                                        new KeyValuePair<string, string>("get", "get"),
                                                                        new KeyValuePair<string, string>("get_", "get"),
                                                                        new KeyValuePair<string, string>("set_", "set"),
                                                                        new KeyValuePair<string, string>("set", "set"),
                                                                        new KeyValuePair<string, string>("bot_", "bot"),
                                                                        new KeyValuePair<string, string>("bot", "bot"),
                                                                        new KeyValuePair<string, string>("favorite_", "bot"),
                                                                        new KeyValuePair<string, string>("favorite", "bot"),
                                                                        new KeyValuePair<string, string>("fav_", "bot"),
                                                                        new KeyValuePair<string, string>("fav", "bot"),
                                                                    })
                {
                    if (node.Name.StartsWith(prefix.Key) && node.Name.Length > prefix.Key.Length)
                    {
                        string name = node.Name.Substring(prefix.Key.Length);
                        var pn = node.ParentNode;
                        string outside = node.OuterXml.Replace("<" + prefix.Key + name, "<" + prefix.Value + " name=\"" + name + "\"");
                        writeToLog("AIMLLOADER: ! convert " + node.OuterXml + " -> " + outside);
                    }
                }
                tagHandler = new AIMLTagHandlers.verbatum(node.OuterXml, this, user, query, request, result, node);
                writeToLog("AIMLLOADER:  Verbatum: " + node.OuterXml);
            }
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
            if (this.CustomTags.ContainsKey(nodename))
            {
                TagHandler customTagHandler = (TagHandler)this.CustomTags[node.Name.ToLower()];

                AIMLTagHandler newCustomTag = customTagHandler.Instantiate(this.LateBindingAssemblies, user, query, request, result, node, this);
                if (Equals(null, newCustomTag))
                {
                    return null;
                }
                else
                {
                    return newCustomTag;
                }
            }
            else
            {
                try
                {
                    if (nodename.StartsWith("#")) return null;
                    String typeName = GetType().Namespace + ".AIMLTagHandlers." + nodename;
                    Type t = Type.GetType(typeName);
                    if (t == null) return null;
                    var c = t.GetConstructor(TagHandler.CONSTRUCTOR_TYPES);
                    return (AIMLTagHandler)c.Invoke(new object[] { user, query, request, result, node, this });
                }
                catch (Exception e)
                {
                    writeToLog("ERROR getBespokeTags: " + e);
                    return null;
                }
            }
        }

        #endregion

        #region Serialization

        /// <summary>
        /// Saves the graphmaster node (and children) to a binary file to avoid processing the AIML each time the 
        /// Proccessor starts
        /// </summary>
        /// <param name="path">the path to the file for saving</param>
        public void saveToBinaryFile(Unifiable path)
        {
            this.GraphMaster.saveToBinaryFile(path);
        }

        /// <summary>
        /// Loads a dump of the graphmaster into memory so avoiding processing the AIML files again
        /// </summary>
        /// <param name="path">the path to the dump file</param>
        public void loadFromBinaryFile(Unifiable path)
        {
            this.GraphMaster.loadFromBinaryFile(path);
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
            Type[] tagDLLTypes = tagDLL.GetTypes();
            for (int i = 0; i < tagDLLTypes.Length; i++)
            {
                Type type = tagDLLTypes[i];
                object[] typeCustomAttributes = type.GetCustomAttributes(false);
                if (typeCustomAttributes.Length == 0 && typeof(AIMLTagHandler).IsAssignableFrom(type) && !type.IsAbstract && !type.IsInterface)
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
            if (!this.LateBindingAssemblies.ContainsKey(tagDLL.FullName))
            {
                this.LateBindingAssemblies.Add(tagDLL.FullName, tagDLL);
            }
            // create the TagHandler representation
            TagHandler newTagHandler = new TagHandler(type);
            newTagHandler.AssemblyName = tagDLL.FullName;
            newTagHandler.ClassName = type.FullName;
            newTagHandler.TagName = type.Name.ToLower();
            if (this.CustomTags.ContainsKey(newTagHandler.TagName))
            {
                throw new Exception("ERROR! Unable to add the custom tag: <" + newTagHandler.TagName + ">, found in: " + tagDLL.Location + " as a handler for this tag already exists.");
            }
            else
            {
                this.CustomTags.Add(newTagHandler.TagName, newTagHandler);
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
            MailMessage msg = new MailMessage("donotreply@aimlbot.com", this.AdminEmail);
            msg.Subject = "WARNING! AIMLBot has encountered a problem...";
            string message = @"Dear Botmaster,

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
            message = message.Replace("*USER*", request.user.UserID);
            Unifiable paths = Unifiable.CreateAppendable();
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
            XmlNode node = AIMLTagHandler.getNode(evalTemplate);
            var res = ImmediateAiml(node, user, Loader, null);
            return res;
        }


        private object ClojExecHandler(string cmd, Request user)
        {
            var cloj = clojureInterpreter;
            lock (cloj)
            {
                bool hasUser = cloj.IsSubscriberOf("MyUser");

                if (hasUser)
                {
                    object o = cloj.GetSymbol("MyUser");
                    object r = cloj.Eval(o);
                    if (user.user != null && r != user.user)
                    {
                        cloj.Intern("MyUser", user.user);
                    }
                }
                else
                {
                    if (user.user != null)
                    {
                        cloj.Intern("MyUser", user.user);
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
            if (Unifiable.IsNullOrEmpty(langu))
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
            writeToLog(s);
            return Unifiable.Empty;
        }


        readonly Dictionary<string, SystemExecHandler> ExecuteHandlers = new Dictionary<string, SystemExecHandler>();

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
            return (Unifiable)GlobalSettings.grabSetting(name);
        }

        public Unifiable NOTOPIC
        {
            get
            {
                if (!GlobalSettings.containsSettingCalled("notopic")) return "Nothing";
                return GlobalSettings.grabSettingNoDebug("notopic");
            }
        }


        public Dictionary<string, GraphMaster> GraphsByName = new Dictionary<string, GraphMaster>();
        public CycDatabase TheCyc;
        public NatLangDb TheNLKB;
        public bool UseInlineThat = true;

        public bool CycEnabled
        {
            get { return TheCyc.CycEnabled; }
            set { TheCyc.CycEnabled = value; }
        }

        public static bool SaveProofs;

        public GraphMaster GetUserGraph(string graphPath, GraphMaster current)
        {
            GraphMaster g;
            lock (GraphsByName)
            {
                if (!GraphsByName.TryGetValue(graphPath, out g))
                {
                    g = GraphsByName[graphPath] = new GraphMaster(graphPath, this);
                    g.AddGenlMT(current);
                }
            }
            return g;
        }
        public GraphMaster GetGraph(string graphPath, GraphMaster current)
        {
            if (graphPath == null)
            {
                return current;
            }
            graphPath = graphPath.ToLower().Trim();

            if (graphPath == "current" || graphPath == "")
            {
                return current;
            }

            if (graphPath == "default")
            {
                return _g;
            }

            if (graphPath == "heardselfsay")
            {
                return _h;
            }

            if (graphPath == "parent")
            {
                return current.Parent;
            }

            GraphMaster g;
            lock (GraphsByName)
            {
                if (!GraphsByName.TryGetValue(graphPath, out g))
                {
                    g = GraphsByName[graphPath] = new GraphMaster(graphPath, this);
                }
            }
            return g;
        }

        static void MainConsoleWriteLn(string fmt, params object[] ps)
        {
            writeDebugLine("-" + fmt, ps);
        }
        public static void Main(string[] args)
        {
            RTPBot myBot = new Bot();
            OutputDelegate writeLine = MainConsoleWriteLn;
            bool usedHttpd = false;
            foreach (var s in args)
            {
                if (s == "--httpd")
                {
                    usedHttpd = true;
                }
            }

            string[] oArgs;
            if (usedHttpd)
            {
                ScriptExecutorGetter geter = new WebScriptExecutor(myBot);
                new ClientManagerHttpServer(geter, 5580);
            }
            Main(args, myBot, writeLine);
        }
        public static void Main(string[] args, RTPBot myBot, OutputDelegate writeLine)
        {
            myBot.outputDelegate = null;/// ?? Console.Out.WriteLine;

            // writeLine = MainConsoleWriteLn;
            bool gettingUsername = false;
            myBot.loadSettings();
            string myName = "BinaBot Daxeline";
            //myName = "Test Suite";
            //myName = "Kotoko Irata";
            //myName = "Nephrael Rae";
            if (args != null)
            {
                string newName = "";
                foreach (var s in args)
                {
                    if (s == "--aiml" || s == "--botname")
                    {
                        gettingUsername = true;
                        continue;
                    }
                    if (s.StartsWith("-"))
                    {
                        gettingUsername = false;
                        continue;
                    }
                    if (gettingUsername)
                    {
                        newName += " " + s;
                    }
                }
                newName = newName.Trim();
                if (newName.Length > 1)
                {
                    myName = newName;
                }
            }
            writeLine(Environment.NewLine);
            writeLine("Botname: " + myName);
            writeLine(Environment.NewLine);
            myBot.isAcceptingUserInput = false;
            myBot.loadAIMLFromDefaults();
            writeLine("-----------------------------------------------------------------");
            myBot.LoadPersonalDirectories(myName);
            myBot.isAcceptingUserInput = true;

            string evidenceCode = "<topic name=\"collectevidencepatterns\"> " +
                                  "<category><pattern>HOW ARE YOU</pattern><template>" +
                                  "<think><setevidence evidence=\"common-greeting\" prob=1.0 /></think>" +
                                  "</template></category></topic>" +
                                  "";
            //Added from AIML content now
            // myBot.AddAiml(evidenceCode);
            User myUser = myBot.LastUser;
            myBot.BotDirective(myUser, "@log " + AIMLDEBUGSETTINGS, Console.Error.WriteLine);
            writeLine("-----------------------------------------------------------------");
            myBot.BotDirective(myUser, "@help", writeLine);
            writeLine("-----------------------------------------------------------------");

            String botJustSaid = null;
            string meneValue = null;
            var userJustSaid = String.Empty;
            myBot.LastUser = myUser;
            while (true)
            {
                myUser = myBot.LastUser;
                writeLine("-----------------------------------------------------------------");
                string input = TextFilter.ReadLineFromInput(Console.Write, myUser.ShortName + "> ");
                if (input == null)
                {
                    Environment.Exit(0);
                }
                input = input.Trim();
                if (input.ToLower() == "@quit")
                {
                    return;
                }
                if (input.ToLower() == "@exit")
                {
                    Environment.Exit(Environment.ExitCode);
                }
                writeLine("-----------------------------------------------------------------");
                if (String.IsNullOrEmpty(input))
                {
                    writeLine(myName + "> " + botJustSaid);
                    continue;
                }
                try
                {
                    bool myBotBotDirective = false;
                    if (input.StartsWith("@"))
                    {
                        myBotBotDirective = myBot.BotDirective(myUser, input, writeLine);
                        //if (!myBotBotDirective) 
                        continue;
                    }
                    if (!myBotBotDirective)
                    {
                        userJustSaid = input;
                        //  myUser.TopicSetting = "collectevidencepatterns";
                        myBot.pMSM.clearEvidence();
                        myBot.pMSM.clearNextStateValues();
                        Request r = new AIMLbot.Request(input, myUser, myBot, null);
                        r.IsTraced = true;
                        ///r.ProcessMultipleTemplates = false; stored in user settings
                        writeLine("-----------------------------------------------------------------");
                        Result res = myBot.Chat(r);
                        if (!res.IsEmpty)
                        {
                            botJustSaid = res.Output;
                            meneValue = "" + res.Score;
                            writeLine("-----------------------------------------------------------------");
                            myBot.HeardSelfSayNow(botJustSaid);
                            writeLine("-----------------------------------------------------------------");

                        }
                        else
                        {
                            botJustSaid = "NULL";
                        }
                    }
                    writeLine("-----------------------------------------------------------------");
                    writeLine("{0}: {1}", myUser.ShortName, userJustSaid);
                    writeLine("---------------------");
                    writeLine("{0}: {1}   mene value={2}", myName, botJustSaid, meneValue);
                    writeLine("-----------------------------------------------------------------");
                }
                catch (Exception e)
                {
                    writeLine("Error: {0}", e);
                }
            }

        }


        public object LightWeigthBotDirective(string input, Request request)
        {
            var sw = new StringWriter();
            bool b = BotDirective(request.user, input, request.WriteLine);
            var sws = sw.ToString();
            writeDebugLine(sws);
            return Unifiable.Empty;
        }

        public bool BotDirective(User myUser, string input, OutputDelegate console)
        {
            if (input == null) return false;
            input = input.Trim();
            if (input == "") return false;
            if (input.StartsWith("@"))
            {
                input = input.TrimStart(new[] { ' ', '@' });
            }
            myUser = myUser ?? LastUser ?? FindOrCreateUser(null);
            int firstWhite = input.IndexOf(' ');
            if (firstWhite == -1) firstWhite = input.Length - 1;
            string cmd = input.Substring(0, firstWhite + 1).Trim().ToLower();
            string args = input.Substring(firstWhite + 1).Trim();
            bool showHelp = false;
            if (cmd == "help")
            {
                showHelp = true;
                console("Commands are prefixed with @cmd");
                console("@help shows help -- command help comming soon!");
                console("@quit -- exits the aiml subsystem");
            }

            if (showHelp) console("@withuser <user> - <text>  -- (aka. simply @)  runs text/command not intentionally setting LastUser");
            if (cmd == "withuser" || cmd == "@")
            {
                int lastIndex = args.IndexOf("-");
                if (lastIndex > -1)
                {
                    string user = args.Substring(0, lastIndex).Trim();
                    string value = args.Substring(lastIndex + 1).Trim();
                    var r = GetRequest(value, user);
                    r.IsTraced = true;
                    var res = Chat0(r, r.Graph);
                    double scored = res.Score;
                    Unifiable resOutput = res.Output;
                    string useOut = "Interesting.";
                    string oTest = resOutput.AsString();
                    if (!string.IsNullOrEmpty(oTest))
                    {
                        useOut = MyBot.CleanupCyc(oTest).AsString();   
                    }
                    if (string.IsNullOrEmpty(useOut))
                    {
                        useOut = "Interesting.";
                        scored = 0.5;
                    }
                    else useOut = useOut.Replace(" _", " ");
                    if (!useOut.Contains("mene value=")) useOut = useOut + " mene value=" + scored;
                    console(useOut);
                }
                return true;
            }

            if (showHelp) console("@aimladd [graphname] <aiml/> -- inserts aiml content into graph (default LastUser.ListeningGraph )");
            if (cmd == "aimladd")
            {
                int indexof = args.IndexOf("<");
                string gn = args.Substring(0, indexof - 1);
                args = args.Substring(indexof).Trim();
                GraphMaster g = GetGraph(gn, myUser.ListeningGraph);
                AddAiml(g, args);
                console("Done with " + args);
                return true;
            }

            if (showHelp) console("@prolog <load.pl>");
            if (cmd == "prolog")
            {
                Prolog.CSPrologMain.Main(new string[] { args });
            }

            if (showHelp) console("@reload -- reloads any changed files ");
            if (cmd == "reload")
            {
                ReloadAll();
                return true;
                //                return;//Success("WorldSystemModule.MyBot.ReloadAll();");
            }

            if (showHelp) console("@load <uri>");
            if (cmd == "load")
            {
                if (Loader == null)
                {
                    Loader = new AIMLLoader(this, GetBotRequest(cmd + " " + args));
                }
                Loader.loadAIML(args);
                console("Done with " + args);
                return true;
            }
            if (showHelp) console("@say <text> -- fakes that the bot just said it");
            if (cmd == "say")
            {
                HeardSelfSayNow(args);
                console("say> " + args);
                myUser.SetOutputSentences(args);
                return true;
            }

            if (showHelp) console("@set [type] [name [value]] -- emulates get/set tag in AIML.  'type' defaults to =\"user\" therefore same as @user");
            if (cmd == "set")
            {
                console(DefaultPredicates.ToDebugString());
                return myUser.DoUserCommand(args, console);
                return true;
            }

            if (showHelp) console("@bot [var [value]] -- lists or changes the bot GlobalPredicates.");
            if (cmd == "bot")
            {
                console(HeardPredicates.ToDebugString());
                return BotAsUser.DoUserCommand(args, console);
            }


            if (showHelp) console("@proof [[clear]|[save [filename.aiml]]] - clears or prints a content buffer being used");
            if (cmd == "proof")
            {
                console("-----------------------------------------------------------------");
                var ur = GetRequest(args, myUser.ShortName);
                int i;
                Result r = myUser.LastResult;
                if (args.StartsWith("save"))
                {
                    args = args.Substring(4).Trim();
                    string hide = AIMLLoader.GetTemplateSource(myUser.UsedTemplates);
                    console(hide);
                    if (args.Length > 0) HostSystem.AppendAllText(args, hide + "\n");
                    return true;
                }
                if (int.TryParse(args, out i))
                {
                    r = myUser.GetResult(i);
                    console("-----------------------------------------------------------------");
                    if (r != null)
                        AIMLLoader.PrintResult(r, console);
                }
                else
                {
                    List<TemplateInfo> CI = myUser.UsedTemplates;
                    console("-----------------------------------------------------------------");
                    AIMLLoader.PrintTemplates(CI, console);
                    if (args == "clear") CI.Clear();
                    console("-----------------------------------------------------------------");
                }

                return true;
            }


            if (showHelp) console("@query <text> - conducts a findall using all tags");
            if (cmd == "query")
            {

                console("-----------------------------------------------------------------");
                if (args == "")
                {
                    var ur0 = myUser;
                    if (ur0.MinOutputs != UNLIMITED)
                    {
                        console("- query mode on -");
                        QuerySettings.ApplySettings(QuerySettings.FindAll, myUser);
                    }
                    else
                    {
                        console("- query mode off -");
                        QuerySettings.ApplySettings(QuerySettings.CogbotDefaults, myUser);
                    }
                    return true;
                }

                var ur = GetRequest(args, myUser.ShortName);
                // Adds findall to request
                if (true)
                {
                    ApplySettings(QuerySettings.FindAll, ur);
                }
                else
                {
                    ur.ProcessMultipleTemplates = true;
                    ur.MaxOutputs = 99;
                    ur.MaxPatterns = 99;
                    ur.MaxTemplates = 99;
                    ur.ProcessMultiplePatterns = true;
                }
                ur.IsTraced = true;
                console("-----------------------------------------------------------------");
                var result = Chat0(ur, myUser.ListeningGraph);
                console("-----------------------------------------------------------------");
                AIMLLoader.PrintResult(result, console);
                console("-----------------------------------------------------------------");
                return true;
            }

            if (showHelp) console("@user [var [value]] -- lists or changes the current users get/set vars.");
            if (cmd == "user")
            {
                return myUser.DoUserCommand(args, console);
            }

            if (showHelp) console("@chgraph <graph> - changes the users graph");
            if (cmd == "graph" || cmd == "chgraph")
            {
                if (args == "")
                {
                    console("-----------------------------------------------------------------");
                    foreach (var ggg in GraphMaster.CopyOf(GraphsByName))
                    {
                        console("-----------------------------------------------------------------");
                        string n = ggg.Key;
                        GraphMaster gm = ggg.Value;
                        console("" + gm + " key='" + n + "'");
                        gm.WriteMetaHeaders(console);
                        console("-----------------------------------------------------------------");
                    }
                    console("-----------------------------------------------------------------");
                    console("ListeningGraph=" + myUser.ListeningGraph);
                    console("-----------------------------------------------------------------");
                    return true;
                }
                myUser.ListeningGraph = GetGraph(args, myUser.ListeningGraph);
                return true;
            }

            if (showHelp) console("@log " + AIMLDEBUGSETTINGS);
            if (cmd.StartsWith("log"))
            {
                LoggedWords.UpateLogging(args, console);
                return true;
            }
            if (cmd == "on" || cmd == "off")
            {
                return true;
            }

            if (showHelp) console("@eval <source>  --- runs source based on users language setting interp='" + myUser.Predicates.grabSetting("interp") + "'");
            if (cmd == "eval")
            {
                cmd = "call";
                args = "@cloj " + args;
            }
            if (showHelp) console("@call <lang> <source>  --- runs script source");
            if (cmd == "call")
            {
                string source;// myUser ?? LastUser.ShortName ?? "";
                string slang;
                if (args.StartsWith("@"))
                {
                    args = args.Substring(1);
                    int lastIndex = args.IndexOf(" ");
                    if (lastIndex > 0)
                    {
                        source = args.Substring(lastIndex + 1).Trim();
                        slang = args.Substring(0, lastIndex);
                    }
                    else
                    {
                        source = args;
                        slang = null;
                    }
                }
                else
                {
                    source = args;
                    slang = null;
                }
                var ur = GetRequest(args, myUser.ShortName);
                if (source != null)
                {
                    try
                    {
                        console(SystemExecute(source, slang, ur));
                    }
                    catch (Exception e)
                    {
                        console("SystemExecute " + source + " " + slang + " caused " + e);
                    }
                }
                return true;
            }

            bool uc = BotUserDirective(myUser, input, console);
            if (showHelp || uc) return true;
            console("unknown: @" + input);
            return false;
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
            if (lastIndex==-1)
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
            var ret = user.Predicates.grabSettingNoDebug("mt");
            if (!Unifiable.IsNullOrEmpty(ret))
            {
                string v = ret.ToValue(subquery);
                if (v != null && v.Length > 1) return TheCyc.Cyclify(v);
            }
            //GetAttribValue("mt","");
            return "#$BaseKB";
        }

        public void WriteConfig()
        {
            TheCyc.WriteConfig();
            GraphMaster.WriteConfig();
            RTPBot.writeDebugLine("Bot loaded");
        }
        public bool LoadPersonalDirectory(string myName)
        {
            ReloadHooks.Add(() => LoadPersonalDirectory(myName));
            bool loaded = false;

            // this is the personal "config file" only.. aiml stored elsewhere
            string file = HostSystem.Combine("config", myName);
            if (HostSystem.DirExists(file))
            {
                writeToLog("LoadPersonalDirectories: '{0}'", file);
                loaded = true;
                loadSettings(Path.Combine(file, "Settings.xml"));
            }

            file = HostSystem.Combine("aiml", myName);
            if (HostSystem.DirExists(file))
            {
                UsePersonalDir(file); ;
                loaded = true;
            }

            // this is the personal "config file" only.. aiml stored elsewhere
            file = HostSystem.Combine(myName, "config");
            if (HostSystem.DirExists(file))
            {
                writeToLog("LoadPersonalDirectories: '{0}'", file);
                loaded = true;
                loadSettings(Path.Combine(file, "Settings.xml"));
            }

            file = HostSystem.Combine(myName, "aiml");
            if (HostSystem.DirExists(file))
            {
                UsePersonalDir(file); ;
                loaded = true;
            }
            return loaded;
        }

        public void UsePersonalDir(string file)
        {
            if (!HostSystem.DirExists(file))
            {
                writeToLog("ERROR - cannot use non existent personal dir = " + file);
                return;
            }
            _PathToBotPersonalFiles = file;
            string s = string.Format("-LoadPersonalDirectories: '{0}'-", file);
            writeToLog(s);
            bool prev = isAcceptingUserInput;
            try
            {
                isAcceptingUserInput = false;
                loadAIMLFromURI(file, GetBotRequest(s));
            }
            finally
            {
                isAcceptingUserInput = prev;
            }
        }

        public void LoadPersonalDirectories(string myName)
        {
            bool loaded = LoadPersonalDirectory(myName);
            if (!loaded)
            {
                myName = myName.Replace(" ", "_").ToLower();
                loaded = LoadPersonalDirectory(myName);
            }
            if (!loaded)
            {
                writeToLog("Didnt find personal directories with stem: '{0}'", myName);
            }
        }

        internal static void writeDebugLine(string message, params object[] args)
        {
            lock (LoggedWords) LoggedWords.writeDebugLine(Console.WriteLine, message, args);
        }

        public bool SameUser(string old, string next)
        {
            old = old ?? "";
            next = next ?? "";
            old = old.ToLower().Trim();
            next = next.ToLower().Trim();
            return FindUser(old) == FindUser(next);
        }



        private Dictionary<string, DateTime> LoadedFiles = new Dictionary<string, DateTime>();
        private ClojureInterpreter clojureInterpreter;

        public bool AddFileLoaded(string filename)
        {
            var fi = new FileInfo(filename);
            string fullName = fi.FullName;
            DateTime dt;
            lock (LoadedFiles)
            {
                if (!LoadedFiles.TryGetValue(fullName, out dt))
                {
                    LoadedFiles[fullName] = fi.LastWriteTime;
                    return true;
                }
                if (fi.LastWriteTime > dt)
                {
                    LoadedFiles[fi.FullName] = fi.LastWriteTime;
                    return true;
                }
                return false;
            }
        }

        public bool RemoveFileLoaded(string filename)
        {
            var fi = new FileInfo(filename);
            string fullName = fi.FullName;
            DateTime dt;
            lock (LoadedFiles)
            {
                return LoadedFiles.Remove(fullName);
            }
        }

        public bool IsFileLoaded(string filename)
        {
            var fi = new FileInfo(filename);
            string fullName = fi.FullName;
            DateTime dt;
            lock (LoadedFiles)
            {
                if (!LoadedFiles.TryGetValue(fullName, out dt))
                {
                    return false;
                }
                if (fi.LastWriteTime > dt)
                {
                    return false;
                }
                return true;
            }
        }

        #region Overrides of QuerySettings

        /// <summary>
        /// The Graph to start the query on
        /// </summary>
        public override GraphMaster Graph
        {
            get { return GraphMaster; }
            set { throw new NotImplementedException(); }
        }

        #endregion
    }
}
