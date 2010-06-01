using System;
using System.Collections;
using System.Collections.Generic;
using System.Globalization;
using System.Text.RegularExpressions;
using System.IO;
using System.Threading;
using System.Xml;
using System.Text;
using System.Runtime.Serialization.Formatters.Binary;
using System.Reflection;
using System.Net.Mail;
using AIMLbot;
using RTParser.AIMLTagHandlers;
using RTParser.Database;
using RTParser.Normalize;
using RTParser.Utils;
using UPath = RTParser.Unifiable;

namespace RTParser
{
    public delegate object SystemExecHandler(string cmd, Request user);

    /// <summary>
    /// Encapsulates a Proccessor. If no settings.xml file is found or referenced the Proccessor will try to
    /// default to safe settings.
    /// </summary>
    public class RTPBot
    {
        public User LastUser;
        readonly public User BotAsUser;
        readonly public Request BotAsRequest;
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
        private List<Unifiable> LogBuffer = new List<Unifiable>();

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
                return 20000;
                if (!this.GlobalSettings.containsSettingCalled("timeout"))
                {
                    return 60000;
                }
                String s = this.GlobalSettings.grabSettingNoDebug("timeout").ToValue();
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

        /// <summary>
        /// The directory to look in for the AIML files
        /// </summary>
        public string PathToAIML
        {
            get
            {
                return Path.Combine(Environment.CurrentDirectory, this.GlobalSettings.grabSetting("aimldirectory"));
            }
        }

        /// <summary>
        /// The directory to look in for the various XML configuration files
        /// </summary>
        public string PathToConfigFiles
        {
            get
            {
                return Path.Combine(Environment.CurrentDirectory, this.GlobalSettings.grabSetting("configdirectory"));
            }
        }

        /// <summary>
        /// The directory into which the various log files will be written
        /// </summary>
        public string PathToLogs
        {
            get
            {
                return Path.Combine(Environment.CurrentDirectory, this.GlobalSettings.grabSetting("logdirectory"));
            }
        }

        /// <summary>
        /// The number of categories this Proccessor has in its graphmaster "brain"
        /// </summary>
        public int Size
        {
            get { return GraphMaster.Size + HeardSelfSayGraph.Size; }
        }


        GraphMaster _g = new GraphMaster();

        private GraphMaster _h = new GraphMaster();

        /// <summary>
        /// The "brain" of the Proccessor
        /// </summary>
        public GraphMaster GraphMaster
        {
            get { return GetGraph("default", _g); }
        }

        public GraphMaster HeardSelfSayGraph
        {
            get { return GetGraph("HeardSelfSay", _h); }
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
        {
            GraphsByName.Add("default", _g);
            GraphsByName.Add("HeardSelfSay", _h);
            this.setup();
            BotAsUser = new User("Self", this);
            BotAsUser.Predicates = GlobalSettings;
            BotAsRequest = new AIMLbot.Request("-bank-input-", BotAsUser, this, null);
            AddExcuteHandler("aiml", EvalAIMLHandler);
            this.TheCyc = new CycDatabase(this);
            var v = TheCyc.GetCycAccess;
        }

        #region Settings methods

        /// <summary>
        /// Loads AIML from .aiml files into the graphmaster "brain" of the Proccessor
        /// </summary>
        public void loadAIMLFromDefaults()
        {
            var prev = isAcceptingUserInput;
            try
            {
                isAcceptingUserInput = false;
                AIMLLoader loader = new AIMLLoader(this, BotAsRequest);
                Loader = loader;
                loader.loadAIML(BotAsRequest);

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
            request = request ?? this.BotAsRequest;
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
            try
            {
                isAcceptingUserInput = false;
                options.request = request;
                AIMLLoader loader = new AIMLLoader(this, request);
                Loader = loader;
                loader.loadAIML(path, options, request);
                // maybe loads settings files if they are there
                string settings = Path.Combine(path, "Settings.xml");
                if (File.Exists(settings)) loadSettings(settings);
            }
            finally
            {
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

                AIMLLoader loader = new AIMLLoader(this, r);
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
                this.CustomTags = new Dictionary<string, TagHandler>();
                //this.GraphMaster = new GraphMaster();
                //this.HeardSelfSayGraph = new GraphMaster();
                if (File.Exists("AIMLbot.dll")) loadCustomTagHandlers("AIMLbot.dll");
                if (File.Exists("AIMLbot.exe")) loadCustomTagHandlers("AIMLbot.exe");

                string names_str = "markovx.trn 5ngram.ngm";
                string[] nameset = names_str.Split(' ');
                foreach (string name in nameset)
                {

                    int loadcount = 0;
                    string file = Path.Combine("trn", name);
                    if (File.Exists(file))
                    {
                        StreamReader sr = new StreamReader(file);
                        writeToLog(" **** Markovian Brain LoadMarkovLTM: '{0}'****", file);
                        this.MBrain.Learn(sr);
                        sr.Close();
                        writeToLog(" **** Markovian Brain initialized.: '{0}' **** ", file);
                        loadcount++;
                    }

                    file = Path.Combine("ngm", name);
                    if (File.Exists(file))
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
                    //if (Directory.Exists(file))
                    if (File.Exists(file))
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
            string path = Path.Combine(Environment.CurrentDirectory, Path.Combine("config", "Settings.xml"));
            this.loadSettings(path);
        }

        public void ReloadAll()
        {
            setup();
            List<CrossAppDomainDelegate> todo = new List<CrossAppDomainDelegate>(ReloadHooks);
            ReloadHooks.Clear();
            foreach (var list in todo)
            {
                list();
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
        }

        private void SetSaneGlobals(SettingsDictionary settings)
        {
            if (!settings.containsSettingCalled("notopic"))
            {
                GlobalSettings.addSetting("notopic", "Nothing");
            }
            if (!settings.containsSettingCalled("version"))
            {
                settings.addSetting("version", Environment.Version.ToString());
            }
            if (!settings.containsSettingCalled("name"))
            {
                settings.addSetting("name", "Unknown");
            }
            if (!settings.containsSettingCalled("botmaster"))
            {
                settings.addSetting("botmaster", "Unknown");
            }
            if (!settings.containsSettingCalled("master"))
            {
                settings.addSetting("botmaster", "Unknown");
            }
            if (!settings.containsSettingCalled("author"))
            {
                settings.addSetting("author", "Nicholas H.Tollervey");
            }
            if (!settings.containsSettingCalled("location"))
            {
                settings.addSetting("location", "Unknown");
            }
            if (!settings.containsSettingCalled("gender"))
            {
                settings.addSetting("gender", "-1");
            }
            if (!settings.containsSettingCalled("birthday"))
            {
                settings.addSetting("birthday", "2006/11/08");
            }
            if (!settings.containsSettingCalled("birthplace"))
            {
                settings.addSetting("birthplace", "Towcester, Northamptonshire, UK");
            }
            if (!settings.containsSettingCalled("website"))
            {
                settings.addSetting("website", "http://sourceforge.net/projects/aimlbot");
            }
            if (settings.containsSettingCalled("adminemail"))
            {
                Unifiable emailToCheck = settings.grabSetting("adminemail");
                this.AdminEmail = emailToCheck;
            }
            else
            {
                settings.addSetting("adminemail", "");
            }
            if (!settings.containsSettingCalled("islogging"))
            {
                settings.addSetting("islogging", "False");
            }
            if (!settings.containsSettingCalled("willcallhome"))
            {
                settings.addSetting("willcallhome", "False");
            }
            if (!settings.containsSettingCalled("timeout"))
            {
                settings.addSetting("timeout", "2000");
            }
            if (!settings.containsSettingCalled("timeoutmessage"))
            {
                settings.addSetting("timeoutmessage", "ERROR: The request has timed out.");
            }
            if (!settings.containsSettingCalled("culture"))
            {
                settings.addSetting("culture", "en-US");
            }
            if (!settings.containsSettingCalled("splittersfile"))
            {
                settings.addSetting("splittersfile", "Splitters.xml");
            }
            if (!settings.containsSettingCalled("person2substitutionsfile"))
            {
                settings.addSetting("person2substitutionsfile", "Person2Substitutions.xml");
            }
            if (!settings.containsSettingCalled("personsubstitutionsfile"))
            {
                settings.addSetting("personsubstitutionsfile", "PersonSubstitutions.xml");
            }
            if (!settings.containsSettingCalled("gendersubstitutionsfile"))
            {
                settings.addSetting("gendersubstitutionsfile", "GenderSubstitutions.xml");
            }
            if (!settings.containsSettingCalled("defaultpredicates"))
            {
                settings.addSetting("defaultpredicates", "DefaultPredicates.xml");
            }
            if (!settings.containsSettingCalled("substitutionsfile"))
            {
                settings.addSetting("substitutionsfile", "Substitutions.xml");
            }
            if (!settings.containsSettingCalled("aimldirectory"))
            {
                settings.addSetting("aimldirectory", "aiml");
            }
            if (!settings.containsSettingCalled("configdirectory"))
            {
                settings.addSetting("configdirectory", "config");
            }
            if (!settings.containsSettingCalled("logdirectory"))
            {
                settings.addSetting("logdirectory", "logs");
            }
            if (!settings.containsSettingCalled("maxlogbuffersize"))
            {
                settings.addSetting("maxlogbuffersize", "64");
            }
            if (!settings.containsSettingCalled("notacceptinguserinputmessage"))
            {
                settings.addSetting("notacceptinguserinputmessage", "This Proccessor is currently set to not accept user input.");
            }
            if (!settings.containsSettingCalled("stripperregex"))
            {
                settings.addSetting("stripperregex", "[^0-9a-zA-Z]");
            }
        }

        /// <summary>
        /// Loads the splitters for this Proccessor from the supplied config file (or sets up some safe defaults)
        /// </summary>
        /// <param name="pathToSplitters">Path to the config file</param>
        private void loadSplitters(string pathToSplitters)
        {
            FileInfo splittersFile = new FileInfo(pathToSplitters);
            if (splittersFile.Exists)
            {
                XmlDocument splittersXmlDoc = new XmlDocument();
                splittersXmlDoc.Load(pathToSplitters);
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

        public delegate void OutputDelegate(string s, params object[] args);

        /// <summary>
        /// Writes a (timestamped) message to the Processor's log.
        /// 
        /// Log files have the form of yyyyMMdd.log.
        /// </summary>
        /// <param name="message">The message to log</param>
        public void writeToLog(string message, params object[] args)
        {
            try
            {
                if (args != null && args.Length != 0) message = String.Format(message, args);
            }
            catch (Exception)
            {
            }
            message = ("[" + DateTime.Now.ToString() + "]: " + message.Trim() + Environment.NewLine);
            writeToLog0(Unifiable.Create(message));
        }
        public void writeToLog0(Unifiable message)
        {

            if (outputDelegate != null)
            {
                try
                {
                    outputDelegate(message);
                }
                catch (Exception)
                {
                }
                Console.WriteLine(message);
            }
            else
            {
                //string m = message.AsString().ToLower();
                //if (m.Contains("error") || m.Contains("excep"))
                Console.WriteLine(message);
            }
            this.LastLogMessage = message;
            if (this.IsLogging)
            {
                //  this.LogBuffer.Add(DateTime.Now.ToString() + ": " + message + Environment.NewLine);
                this.LogBuffer.Add(message);
                if (this.LogBuffer.Count > this.MaxLogBufferSize - 1)
                {
                    // Write out to log file
                    DirectoryInfo logDirectory = new DirectoryInfo(this.PathToLogs);
                    if (!logDirectory.Exists)
                    {
                        logDirectory.Create();
                    }

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
        readonly Dictionary<string, User> BotUsers = new Dictionary<string, User>();

        public void SetChatOnOff(string username, bool value)
        {
            lock (BotUsers)
            {
                foreach (var u in BotUsers.Values)
                {
                    if (u.UserID.ToValue().Contains(username) || username.Contains(u.UserID))
                        u.RespondToChat = value;
                }
            }
        }

        public User FindOrCreateUser(string fromname)
        {
            fromname = fromname ?? "UNKNOWN_PARTNER";
            bool b;
            return FindOrCreateUser(fromname, out b);
        }

        public User FindOrCreateUser(string fromname, out bool newlyCreated)
        {
            newlyCreated = false;
            lock (BotUsers)
            {
                if (BotUsers.ContainsKey(fromname)) return BotUsers[fromname];
                newlyCreated = true;
                AIMLbot.User myUser = new AIMLbot.User(fromname, this);
                BotUsers[fromname] = myUser;
                //AIMLbot.Request r = new AIMLbot.Request("My name is " + fromname, myUser, this);
                //AIMLbot.Result res = Chat(r);
                myUser.Predicates.addSetting("name", fromname);
                return myUser;
            }
        }

        public void AddAiml(string aimlText)
        {
            Request request = BotAsRequest;
            Loader.loadAIMLString("<aiml>"+aimlText+"</aiml>",LoaderOptions.GetDefault(request), request);
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
            return Chat(new AIMLbot.Request(rawInput, FindOrCreateUser(username), this, null)).Output;
        }


        /// <summary>
        /// Given some raw input and a unique ID creates a response for a new user
        /// </summary>
        /// <param name="rawInput">the raw input</param>
        /// <param name="UserGUID">an ID for the new user (referenced in the result object)</param>
        /// <returns>the result to be output to the user</returns>
        public Result Chat(Unifiable rawInput, Unifiable UserGUID)
        {
            AIMLbot.Request request = new AIMLbot.Request(rawInput, new User(UserGUID, this), this, null);
            return this.Chat(request);
        }


        private void LoadInputPaths(Request request, AIMLLoader loader, Unifiable[] rawSentences, AIMLbot.Result result)
        {
            int maxInputs = 1;
            int numInputs = 0;
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
                    int topicNum = 0;
                    if (false)
                    {
                        Unifiable path = loader.generatePath(sentence, //thatNum + " " +
                                      request.user.getLastBotOutput(), request.Flags,
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
                        int thatNum = 0;
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

        public AIMLbot.Result HeardSelfSay(string message)
        {
            currentEar.AddMore(message);
            if (!currentEar.IsReady())
            {
                return null;
            }
            message = currentEar.GetMessage();
            currentEar = new JoinedTextBuffer();
            if (LastUser!=null)
            {
                if (LastUser.LastResult != null)
                    LastUser.LastResult.AddOutputSentences(message);
            }
            message = swapPerson(message);
            RunTask(() => HeardSelfSay0(message), "heardSelfSay: " + message, 500);
            return null;
        }

        List<Thread> ThreadList = new List<Thread>();

        private void RunTask(ThreadStart action, string name, int maxTime)
        {
            Thread t = RunTask(action, name);      
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
                        Console.WriteLine("ERROR " + name + " " + e);
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

        public AIMLbot.Result HeardSelfSay0(string message)
        {
            Console.WriteLine("HEARDSELF SWAP: " + message);
            try
            {
                return Chat(new AIMLbot.Request(message, BotAsUser, this, null));
            }
            catch (Exception e)
            {
                Console.WriteLine("" + e);
                return null;
            }
            //return Chat(message,BotAsUser.ID);
            AIMLbot.Request request = new AIMLbot.Request(message, BotAsUser, this, null);
            request.Graph = HeardSelfSayGraph;
            AIMLbot.Result result = new AIMLbot.Result(request.user, this, request);

            if (this.isAcceptingUserInput)
            {
                // Normalize the input
                AIMLLoader loader = new AIMLLoader(this, request);
                Loader = loader;
                //RTParser.Normalize.SplitIntoSentences splitter = new RTParser.Normalize.SplitIntoSentences(this);
                Unifiable[] rawSentences = new Unifiable[] { request.rawInput };//splitter.Transform(request.rawInput);
                LoadInputPaths(request, loader, rawSentences, result);
                int NormalizedPathsCount = result.NormalizedPaths.Count;
                if (NormalizedPathsCount != 1)
                {
                    writeToLog("NormalizedPaths.Count = " + NormalizedPathsCount);
                    foreach (Unifiable path in result.NormalizedPaths)
                    {
                        writeToLog("  i: " + path.LegacyPath);
                    }
                }

                // grab the templates for the various sentences from the graphmaster
                foreach (Unifiable path in result.NormalizedPaths)
                {
                    Utils.SubQuery query = new SubQuery(path, result, request);
                    //query.Templates = 
                    request.Graph.evaluate(path, query, request, query.InputStar, MatchState.UserInput,0, Unifiable.CreateAppendable());
                    result.SubQueries.Add(query);
                }

                //todo pick and chose the queries
                if (result.SubQueries.Count != 1)
                {
                    if (true)
                    {
                        writeToLog("SubQueries.Count = " + result.SubQueries.Count);
                        foreach (var path in result.SubQueries)
                        {
                            writeToLog(" sq: " + path.FullPath);
                        }
                    }
                }

                // process the templates into appropriate output
                foreach (SubQuery query in result.SubQueries)
                {
                    if (processTemplate(query, request, result, null))
                    {

                    }
                }
                if (true || result.SubQueries.Count != 1)
                {
                    writeToLog("SubQueries.Count = " + result.SubQueries.Count);
                    foreach (var path in result.SubQueries)
                    {
                        writeToLog("\r\n tt: " + path.ToString().Replace("\n", " ").Replace("\r", " ").Replace("  ", " "));
                    }
                }
            }
            else
            {
                result.AddOutputSentences(this.NotAcceptingUserInputMessage);
            }

            // populate the Result object
            result.Duration = DateTime.Now - request.StartedOn;
            request.user.addResult(result);

            return result;
        }

        private string swapPerson(string inputString)
        {
            string temp = Loader.Normalize(inputString, true);
            //temp = ApplySubstitutions.Substitute(this, this.PersonSubstitutions, temp);
            temp = ApplySubstitutions.Substitute(this, this.Person2Substitutions, temp);
            return temp;
        }


        /// <summary>
        /// Given a request containing user input, produces a result from the Proccessor
        /// </summary>
        /// <param name="request">the request from the user</param>
        /// <returns>the result to be output to the user</returns>
        /// 
        public StreamWriter chatTrace;
        public int streamDepth = 0;

        public AIMLbot.Result Chat(Request request)
        {
            return Chat(request, request.Graph ?? GraphMaster);
        }

        public AIMLbot.Result Chat(Request request, GraphMaster G)
        {
            LastUser = request.user ?? LastUser;
            //chatTrace = null;
            if (chatTrace == null)
            {
                chatTrace = new StreamWriter("bgm\\chatTrace.dot");
                chatTrace.WriteLine("digraph G {");
                streamDepth=0;
            }
            streamDepth++;

            AIMLbot.Result result = new AIMLbot.Result(request.user, this, request);

            var orig = request.BotOutputs;
            if (AIMLLoader.ContainsAiml(request.rawInput))
            {
                try
                {
                    result = ImmediateAiml(AIMLTagHandler.getNode(request.rawInput), request, Loader, null);
                    request.rawInput = result.Output;
                }
                catch (Exception e)
                {
                    writeToLog("ImmediateAiml: " + e);
                }
            }

            if (this.isAcceptingUserInput)
            {
                // Normalize the input
                AIMLLoader loader = new AIMLLoader(this, request);
                Loader = loader;
                //RTParser.Normalize.SplitIntoSentences splitter = new RTParser.Normalize.SplitIntoSentences(this);
                Unifiable[] rawSentences = new Unifiable[] { request.rawInput };//splitter.Transform(request.rawInput);
                LoadInputPaths(request, loader, rawSentences, result);
                int NormalizedPathsCount = result.NormalizedPaths.Count;
                if (NormalizedPathsCount != 1)
                {
                    foreach (Unifiable path in result.NormalizedPaths)
                    {
                        writeToLog("  i: " + path.LegacyPath);
                    }
                    writeToLog("NormalizedPaths.Count = " + NormalizedPathsCount);
                }

                // grab the templates for the various sentences from the graphmaster
                foreach (Unifiable path in result.NormalizedPaths)
                {
                    Utils.SubQuery query = new SubQuery(path, result, request);
                    //query.Templates = 
                    G.evaluate(path, query, request, query.InputStar, MatchState.UserInput,0, Unifiable.CreateAppendable());
                    result.SubQueries.Add(query);
                }

                //todo pick and chose the queries
                if (result.SubQueries.Count != 1)
                {
                    if (false)
                    {
                        string s = "SubQueries.Count = " + result.SubQueries.Count;
                        foreach (var path in result.SubQueries)
                        {
                            s += "\r\n" + path.FullPath;
                        }
                        writeToLog(s);
                    }
                }

                // process the templates into appropriate output
                foreach (SubQuery query in result.SubQueries)
                {
                    if (processTemplate(query, request, result, null))
                    {

                    }
                }
                if (true || result.SubQueries.Count != 1)
                {
                    writeToLog("SubQueries.Count = " + result.SubQueries.Count);
                    foreach (var path in result.SubQueries)
                    {
                        writeToLog("\r\n tt: " + path.ToString().Replace("\n", " ").Replace("\r", " ").Replace("  ", " "));
                        if (chatTrace != null)
                        {
                            //chatTrace.WriteLine("\"L{0}\" -> \"{1}\" ;\n", result.SubQueries.Count, path.FullPath.ToString());
                            chatTrace.WriteLine("\"L{0}\" -> \"L{1}\" ;\n", result.SubQueries.Count - 1, result.SubQueries.Count);
                            chatTrace.WriteLine("\"L{0}\" -> \"REQ:{1}\" ;\n", result.SubQueries.Count, request.ToString());
                            chatTrace.WriteLine("\"REQ:{0}\" -> \"PATH:{1}\" [label=\"{2}\"];\n", request.ToString(), result.SubQueries.Count, path.FullPath.ToString());
                            chatTrace.WriteLine("\"REQ:{0}\" -> \"RPY:{1}\" ;\n", request.ToString(), result.RawOutput.ToString());
                        }
                    }
                }
            }
            else
            {
                result.AddOutputSentences(this.NotAcceptingUserInputMessage);
            }

            // populate the Result object
            result.Duration = DateTime.Now - request.StartedOn;
            request.user.addResult(result);
            streamDepth--;

            if (streamDepth<=0 && chatTrace != null)
                {
                    chatTrace.WriteLine("}");
                    chatTrace.Close();
                    chatTrace = null;
                }

            return result;
        }

        /// <summary>
        /// Return false if the guard fails
        /// </summary>
        /// <param name="query"></param>
        /// <param name="request"></param>
        /// <param name="result"></param>
        /// <returns></returns>
        public bool processTemplate(SubQuery query, Request request, Result result, AIMLTagHandler handler)
        {
            UList queryTemplate = query.Templates;
            if (queryTemplate != null && queryTemplate.Count > 0)
            {
                try
                {
                    if (false && !Monitor.TryEnter(queryTemplate, 4000))
                    {
                        this.writeToLog("WARNING! Giving up on input: " + request.rawInput);
                        return false;
                    }
                    return processTemplateUlocked(queryTemplate, query, request, result, handler);
                }
                finally
                {
                    // Monitor.Exit(queryTemplate);
                }

            }
            return false;
        }

        private bool processTemplateUlocked(UList queryTemplate, SubQuery query, Request request, Result result, AIMLTagHandler handler)
        {
            bool found = false;
            if (queryTemplate != null && queryTemplate.Count > 0)
            {
                foreach (TemplateInfo s in queryTemplate)
                {
                    // Start each the same
                    s.Rating = 1.0;
                    try
                    {
                        bool found0;
                        query.CurrentTemplate = s;
                        if (proccessResponse(query, request, result, s.Output, s.Guard, out found0, handler)) break;
                        if (found0) found = true;
                    }
                    catch (Exception e)
                    {
                        writeToLog("" + e);
                        if (this.WillCallHome)
                        {
                            this.phoneHome(e.Message, request);
                        }
                        this.writeToLog("WARNING! A problem was encountered when trying to process the input: " +
                                        request.rawInput + " with the template: \"" + s + "\"");
                        return false;
                    }

                }
            }
            return found;
        }

        public static Unifiable GetAttribValue(XmlNode templateNode, string attribName, Unifiable defaultIfEmpty)
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

        public bool proccessResponse(SubQuery query, Request request, Result result, XmlNode sOutput, GuardInfo sGuard, out bool found, AIMLTagHandler handler)
        {
            found = false;
            //XmlNode guardNode = AIMLTagHandler.getNode(s.Guard.InnerXml);
            string output = sOutput.OuterXml;
            bool usedGuard = sGuard != null;
            if (usedGuard)
            {
                output = output.Trim();
                if (output.StartsWith("<template>"))
                {
                    output = "<template>" + sGuard.InnerXml + " GUARDBOM " + output.Substring(10);
                }
            }
            LineInfoElement templateNode = AIMLTagHandler.getNode(output, sOutput);

            string outputSentence = this.processNode(templateNode, query, request, result, request.user, handler);
            int f = outputSentence.IndexOf("GUARDBOM");
            if (f < 0)
            {
                if (outputSentence.Length > 0)
                {
                    result.AddOutputSentences(outputSentence.Trim().Replace("  ", " ").Replace("  ", " "));
                    found = true;
                }
                return false;
            }

            try
            {
                string left = outputSentence.Substring(0, f).Trim();

                if (Unifiable.IsFalse(left))
                {
                    return false;
                }
                string lang = GetAttribValue(sGuard.Output, "lang", "cycl").AsString().ToLower();

                try
                {
                    Unifiable ss = SystemExecute(left, lang, request);
                    if (Unifiable.IsFalse(ss))
                    {
                        return false;
                    }
                }
                catch (Exception e)
                {
                    string s = "" + e;
                    Console.WriteLine(s);
                    writeToLog(s);
                    return false;
                }

                outputSentence = outputSentence.Substring(f + 9);
                if (outputSentence.Length > 0)
                {
                    result.AddOutputSentences(outputSentence);
                    found = true;
                    return true;
                }
                return false;
            }
            catch (System.Exception ex)
            {
                return false;
            }
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
        public Unifiable processNode(XmlNode node, SubQuery query, Request request, Result result, User user, AIMLTagHandler parent)
        {
            // check for timeout (to avoid infinite loops)
            if (request != null && request.StartedOn.AddMilliseconds(request.Proccessor.TimeOut) < DateTime.Now)
            {
                request.Proccessor.writeToLog("WARNING! Request timeout. User: " + request.user.UserID +
                                              " raw input: \"" + request.rawInput + "\" processing template: \"" +
                                              query.Templates + "\"");
                request.hasTimedOut = true;
                return Unifiable.Empty;
            }

            // process the node
            AIMLTagHandler tagHandler = GetTagHandler(user, query, request, result, node, parent);
            if (ReferenceEquals(null, tagHandler))
            {
                if (node.NodeType == XmlNodeType.Comment) return Unifiable.Empty;
                string s = node.InnerText.Trim();
                if (String.IsNullOrEmpty(s))
                {
                    return Unifiable.Empty;
                }
                return s;
            }
            tagHandler.SetParent(parent);
            return tagHandler.CompleteProcess();
        }


        internal AIMLTagHandler GetTagHandler(User user, SubQuery query, Request request, Result result, XmlNode node, AIMLTagHandler handler)
        {
            var tag = GetTagHandler0(user, query, request, result, node);
            if (tag != null) tag.SetParent(handler);
            return tag;
        }
        internal AIMLTagHandler GetTagHandler0(User user, SubQuery query, Request request, Result result, XmlNode node)
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
                    case "responsetopic":
                        tagHandler = new AIMLTagHandlers.response_topic(this, user, query, request, result, node);
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
                        tagHandler = new AIMLTagHandlers.verbatum(node.OuterXml, this, user, query, request, result, node);
                        writeToLog("Verbatum: " + node.OuterXml);
                        break;
                }
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
                    writeToLog("getBespokeTags: " + e);
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
            this.BotAsRequest.Graph.saveToBinaryFile(path);
        }

        /// <summary>
        /// Loads a dump of the graphmaster into memory so avoiding processing the AIML files again
        /// </summary>
        /// <param name="path">the path to the dump file</param>
        public void loadFromBinaryFile(Unifiable path)
        {
            this.BotAsRequest.Graph.loadFromBinaryFile(path);
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
            foreach (Unifiable path in request.result.NormalizedPaths)
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
            XmlNode node = AIMLTagHandler.getNode("<template>" + cmd + "</template>");
            var res = ImmediateAiml(node, BotAsRequest, Loader, null);
            return res;
        }
        internal Unifiable processNodeInside(XmlNode templateNode, SubQuery query, Request request, Result result, User user, AIMLTagHandler handler)
        {
            return processNode(templateNode, query, request, result, user, handler);
        }

        internal Unifiable SystemExecute(Unifiable cmd, Unifiable langu, Request user)
        {
            if (Unifiable.IsNullOrEmpty(langu))
            {
                langu = "bot";
            }
            Unifiable s = "The system tag should be doing '" + cmd + "' lang=" + langu ;
            writeToLog(s.AsString());
            SystemExecHandler handler = null;
            if (ExecuteHandlers.ContainsKey(langu))
            {
                handler = ExecuteHandlers[langu];
                try
                {
                    return "" + handler(cmd, user);
                }
                catch (Exception e)
                {
                    writeToLog("" + e);
                    return Unifiable.Empty;
                }
            }
            return s;

        }


        readonly Dictionary<string, SystemExecHandler> ExecuteHandlers = new Dictionary<string, SystemExecHandler>();

        public void AddExcuteHandler(string lang, SystemExecHandler handler)
        {
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

        public AIMLbot.Result ImmediateAiml(XmlNode templateNode, Request request0, AIMLLoader loader, AIMLTagHandler handler)
        {
            Request request = new AIMLbot.Request(templateNode.OuterXml, request0.user, request0.Proccessor, (AIMLbot.Request)request0);
            request.Graph = request0.Graph;
            request.depth = request0.depth + 1;
            AIMLbot.Result result = new AIMLbot.Result(request.user, this, request);
            if (true)
            {
                bool found0;
                proccessResponse(null, request, result, templateNode, null, out found0, handler);
                return result;
            }
            //if (true)
            //{
            //    Unifiable path = loader.generatePath("no stars", request.user.getLastBotOutput(), request.Flags, request.Topic, true);
            //    Utils.SubQuery query = new SubQuery(path, result, request);
            //    string outputSentence = this.processNode(templateNode, query, request, result, request.user, handler);
            //    return result;

            //}
            //if (this.isAcceptingUserInput)
            {
                string sentence = "aimlexec " + templateNode.OuterXml;
                //RTParser.Normalize.SplitIntoSentences splitter = new RTParser.Normalize.SplitIntoSentences(this);
                //Unifiable[] rawSentences = new Unifiable[] { request.rawInput };//splitter.Transform(request.rawInput);
                //foreach (Unifiable sentence in rawSentences)
                {
                    result.InputSentences.Add(sentence);
                    Unifiable path = loader.generatePath(sentence, request.user.getLastBotOutput(), request.Flags,
                                                         request.Topic, true);
                    result.NormalizedPaths.Add(path);
                }

                // grab the templates for the various sentences from the graphmaster
                foreach (Unifiable path in result.NormalizedPaths)
                {
                    Utils.SubQuery query = new SubQuery(path, result, request);
                    //query.Templates = 
                    request.Graph.evaluate(path, query, request, query.InputStar, MatchState.UserInput, 0, Unifiable.CreateAppendable());
                    result.SubQueries.Add(query);
                }

                //todo pick and chose the queries
                if (result.SubQueries.Count != 1)
                {
                    writeToLog("Found " + result.SubQueries.Count + " queries");
                }

                // process the templates into appropriate output
                foreach (SubQuery query in result.SubQueries)
                {
                    if (processTemplate(query, request, result, handler))
                    {

                    }
                }
            }
            //else
            {
                //  result.OutputSentences.Add(this.NotAcceptingUserInputMessage);
            }

            // populate the Result object
            result.Duration = DateTime.Now - request.StartedOn;
            request.user.addResult(result);

            return result;
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
        public bool UseInlineThat = true;

        public bool CycEnabled
        {
            get { return TheCyc.CycEnabled; }
            set { TheCyc.CycEnabled = value; }
        }

        public GraphMaster GetGraph(string botname, GraphMaster current)
        {
            if (botname == null)
            {
                return current;
            }
            botname = botname.ToLower();
            if (botname == "default")
            {
                return _g;
            }
            if (botname == "current")
            {
                return current;
            }
            if (botname == "parent")
            {
                return current.Parent;
            }
            GraphMaster g;
            lock (GraphsByName)
            {
                if (!GraphsByName.TryGetValue(botname, out g))
                {
                    g = GraphsByName[botname] = new GraphMaster();
                }
            }
            return g;
        }
        static void Main(string[] args)
        {
            Bot myBot = new Bot();
            myBot.loadSettings();
            string myName = "BinaBot Daxeline";
            if (args != null && args.Length > 0)
            {
                myName = String.Join(" ", args);
            }
            User myUser = myBot.FindOrCreateUser(null);
            myBot.isAcceptingUserInput = false;
            myBot.loadAIMLFromFiles();
            myBot.LoadPersonalDirectories(myName);
            myBot.isAcceptingUserInput = true;
            String s = null;
            while (true)
            {
                Console.Write("You: ");
                Console.Out.Flush();
                string input = Console.ReadLine();
                if (input == null || input.ToLower() == "quit")
                {
                    Environment.Exit(0);
                }
                if (String.IsNullOrEmpty(input))
                {
                    Console.WriteLine("Bot: " + s);
                    continue;
                }
                if (input == "set")
                {
                    Console.WriteLine(myBot.GlobalSettings.ToString());
                    Console.WriteLine(myUser.Predicates.ToString());                    
                    Console.WriteLine("Bot: " + s);
                    continue;
                }
                try
                {
                    if (input.StartsWith("load"))
                    {
                        s = input.Substring(4).Trim();
                        myBot.Loader.loadAIML(s);
                        continue;
                    }
                    if (input.StartsWith("self"))
                    {
                        s = input.Substring(4).Trim();
                        myBot.HeardSelfSay0(s);
                        Console.WriteLine("Bot: " + s);
                    }
                    else
                    {
                        Request r = new AIMLbot.Request(input, myUser, myBot, null);
                        Result res = myBot.Chat(r);
                        s = res.Output;
                    }
                    Console.WriteLine("Bot> " + s);
                }
                catch (Exception e)
                {
                    Console.WriteLine("Error: " + e);
                }
            }

        }

        public string GetUserMt(User user)
        {
            var ret = user.Predicates.grabSettingNoDebug("mt");
            if (!Unifiable.IsNullOrEmpty(ret))
            {
                string v = ret.ToValue();
                if (v != null && v.Length > 1) return TheCyc.Cyclify(v);
            }
            //GetAttribValue("mt","");
            return "#$BaseKB";
        }

        public void WriteConfig()
        {
            TheCyc.WriteConfig();
            GraphMaster.WriteConfig();
            Console.WriteLine("Bot loaded");
        }
        public bool LoadPersonalDirectory(string myName)
        {
            bool loaded = false;
            string file = Path.Combine("config", myName);
            if (Directory.Exists(file))
            {
                writeToLog("LoadPersonalDirectories: '{0}'", file);
                loaded = true;
                loadSettings(Path.Combine(file, "Settings.xml"));
            }
            file = Path.Combine("aiml", myName);
            if (Directory.Exists(file))
            {
                writeToLog("LoadPersonalDirectories: '{0}'", file);
                loaded = true;
                isAcceptingUserInput = false;
                loadAIMLFromURI(file, BotAsRequest);
                isAcceptingUserInput = true;
            }

            file = Path.Combine(myName, "config");
            if (Directory.Exists(file))
            {
                writeToLog("LoadPersonalDirectories: '{0}'", file);
                loaded = true;
                loadSettings(Path.Combine(file, "Settings.xml"));
            }

            file = Path.Combine(myName, "aiml");
            if (Directory.Exists(file))
            {
                writeToLog("LoadPersonalDirectories: '{0}'", file);
                loaded = true;
                isAcceptingUserInput = false;
                loadAIMLFromURI(file, BotAsRequest);
                isAcceptingUserInput = true;
            }
            return loaded;
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
    }

    internal class JoinedTextBuffer
    {
        static int count(string s,string t)
        {
            int f = s.IndexOf(t);
            if (f < 0) return 0;
            return 1 + count(s.Substring(f + 1), t);
        }
        private String message = "";
        public void AddMore(string m)
        {
            if (Noise(m)) return;
            message += " " + m;
            message = message.Trim().Replace("  ", " ");
        }

        private bool Noise(string s)
        {
            if (s == "you know,") return true;
            if (message.EndsWith(s)) return true;
            return false;
        }

        public bool IsReady()
        {
            if (message.EndsWith(",")) return false;
            if (message.EndsWith(".")) return true;
            if (count(message, " ") > 2) return true;
            return false;           
        }

        public string GetMessage()
        {
            return message;
        }
    }
}
