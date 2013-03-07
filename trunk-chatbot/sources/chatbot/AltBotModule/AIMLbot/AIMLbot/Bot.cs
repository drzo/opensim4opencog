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
using AltAIMLbot.AIMLTagHandlers;
using AltAIMLbot.Database;
using AltAIMLbot.Normalize;
using AltAIMLbot.Utils;
using AltAIMLParser;
using AltAIMLbot.Variables;
using DcBus;
using Aima.Core.Logic.Propositional.Algorithms;
using Aima.Core.Logic.Propositional.Parsing;
using Aima.Core.Logic.Propositional.Parsing.AST;
using LAIR.ResourceAPIs.WordNet;
using MushDLR223.ScriptEngines;
using MushDLR223.Utilities;
using MushDLR223.Virtualization;
using org.opencyc.api;
using CustomTagAttribute = AltAIMLbot.Utils.CustomTagAttribute;
using Gender=AltAIMLbot.Utils.Gender;
using MatchState=AltAIMLbot.Utils.MatchState;
using Node=AltAIMLbot.Utils.Node;
using recursiveVerbatum=AltAIMLbot.AIMLTagHandlers.recursiveVerbatum;
using TagHandler=AltAIMLbot.Utils.TagHandler;
using verbatum=AltAIMLbot.AIMLTagHandlers.verbatum;
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

namespace AltAIMLbot
{
    [Serializable]
    public partial class AltBot
    {
        #region Attributes

        /// <summary>
        /// A chemistry connection object
        /// </summary>
        //public RChem myChemistry = new RChem(myConst.MEMHOST, true);
        //public Qchem realChem = new Qchem(myConst.MEMHOST);


        static public bool MemcachedServerKnownDead = false;

        public RChem myChemistry = null;
        public Qchem realChem = null;
        public ChemTrace myChemTrace = null;
        public QfsmSet myFSMS = new QfsmSet();
        public BehaviorSet myBehaviors;
        public Cron myCron = null;
        public bool inCritical = false;
        internal bool _blockCron = false;

        public bool blockCron
        {
            get
            {
                if (_blockCron) return true;
                if (servitor.IsBackgroundDisabled) return true;
                return false;
            }
            set { _blockCron = value; }
        }

        public bool loadChanging = true;
        public RandomMemory myRandMem = new RandomMemory();

        public SIProlog prologEngine
        {
            get { return SIProlog.CurrentProlog; }
        }

        public static object BotInitLock = new object();

        public static object WordNetEngineLock
        {
            get { return BotInitLock; }
        }

        private static WordNetEngine _wordNetEngine;

        public WordNetEngine wordNetEngine
        {
            get { lock (WordNetEngineLock) return _wordNetEngine; }
            set
            {
                LockInfo.CheckLocked(WordNetEngineLock);
                _wordNetEngine = _wordNetEngine ?? value;
            }
        }

        [NonSerialized] public KnowledgeBase myKB = new KnowledgeBase();
        [NonSerialized] public KnowledgeBase myBaseKB = new KnowledgeBase();
        [NonSerialized] public WalkSAT myWalkSAT = new WalkSAT();
        [NonSerialized] public Model myModel = null;

        [NonSerialized] internal Model _myActiveModel;

        public Model myActiveModel
        {
            get { return BotBehaving.myActiveModel; }
            set { BotBehaving.myActiveModel = value; }
        }

        public string myPositiveSATModleString = null;

        public object guestEvalObject = null;

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

        /// <summary>
        /// A dictionary object that looks after all the settings associated with this bot
        /// </summary>
        public SettingsDictionaryReal GlobalSettings;

        public SettingsDictionary AllUserPreds;

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

        public void RegisterDictionaryNoSpaces(string named, ISettingsDictionary dict)
        {
            named = named.ToLower().Trim().Replace("  ", " ");
            string key = named.Replace(" ", "_");
            RegisterDictionary(named, dict, true);
        }

        public void RegisterDictionaryAddP(string key, ISettingsDictionary dict, bool always)
        {
            SettingsDictionaryReal.AddPseudonym(dict, key);
            Action needsExit = LockInfo.MonitorTryEnter("RegisterDictionary " + key, AllDictionaries, MaxWaitTryEnter);
            try
            {
                var path = key.Split(new[] {'.'});
                if (always || !AllDictionaries.ContainsKey(key))
                {
                    AllDictionaries[key] = dict;
                }
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

        /// <summary>
        /// An List<> containing the tokens used to split the input into sentences during the 
        /// normalization process
        /// </summary>
        public static IEnumerable<string> Splitters
        {
            get
            {
                lock (Splitters0)
                {
                    if (Splitters0.Count > 0) return Splitters0.ToArray();
                }
                var Splitters1 = new List<string>();
                Splitters1.Add(".");
                Splitters1.Add("!");
                Splitters1.Add("?");
                Splitters1.Add(";");
                return Splitters1;
            }

        }

        public static List<string> Splitters0 = new List<string>();

        /// <summary>
        /// A buffer to hold log messages to be written out to the log file when a max size is reached
        /// </summary>
        private List<string> LogBuffer = new List<string>();

        public string PersonalizePathLogged(string path)
        {
            var pp = PersonalizePath(path);
            writeDebugLine("Personalized path: " + path + "->" + pp);
            return pp;
        }

        public string PersonalizePath(string path)
        {
            if (String.IsNullOrEmpty(path)) return path;
            if (BotUserID == null)
            {
                Console.WriteLine("Error Cant pertsonalize path");
            }
            if (path.Contains(BotUserID)) return path;
            if (path.Contains(BotID)) return path;
            if (path.Contains(NamePath)) return path;
            if (path.Contains(PersonalReadWriteDirectory)) return path;
            string orig = path;
            path = path.Replace("\\", "/");
            path = path.Replace("//", "/");
            while (path.StartsWith("./")) path = path.Substring(2);
            while (path.StartsWith("aiml/")) path = path.Substring(5);
            return HostSystem.FileSystemPath(HostSystem.Combine(PersonalReadWriteDirectory ?? PersonalAiml, path));
        }

        private string _rapStoreDirectory;

        public string rapStoreDirectoryStem
        {
            get
            {
                if (!UseRapstoreDB)
                {
                    // legacy behavour
                    // return null;                   
                }
                else
                {
                    if (string.IsNullOrEmpty(_rapStoreDirectory))
                    {
                        Console.WriteLine(" - WARN - Bot rapStoreDirectory Empty");
                    }
                }
                return PersonalizePath(_rapStoreDirectory) ?? PersonalizePath("./rapstore");
            }
            set
            {
                if (string.IsNullOrEmpty(value))
                {
                    Console.WriteLine("setting Bot rapStoreDirectory Empty");
                    UseRapstoreDB = false;
                    _rapStoreDirectory = null;
                    return;
                }
                _rapStoreDirectory = value;
            }
        }

        public int rapStoreSlices = 0;
        public int rapStoreTrunkLevel = 0;

        public string CalcRapStoreDirectoryStem()
        {
            var rapStoreDirectoryStem = _rapStoreDirectory ?? "./rapstore";
            if (Graphmaster.IsMicrosoftCLR())
            {
                rapStoreDirectoryStem = rapStoreDirectoryStem.TrimEnd("/\\".ToCharArray());
            }
            else
            {
                rapStoreDirectoryStem = rapStoreDirectoryStem.TrimEnd(Path.DirectorySeparatorChar);
            }
            rapStoreDirectoryStem = PersonalizePath(rapStoreDirectoryStem);
            if (rapStoreDirectoryStem != _rapStoreDirectory && _UseRapstoreDB)
            {
                Console.WriteLine("Setting rapstore directory stem to " + rapStoreDirectoryStem);
                _rapStoreDirectory = rapStoreDirectoryStem;
            }
            return rapStoreDirectoryStem;
        }

        public void StampRaptstoreValid(bool validOrNot)
        {
            lock (ExternDB.mylock)
            {
                if (UseRapstoreDB)
                {
                    string rapStoreDirectoryStem = Graphmaster.CalcRapStoreDirectory();
                    string stampFile = Path.Combine(rapStoreDirectoryStem, "valid.txt");
                    var fi = new FileInfo(stampFile);
                    if (validOrNot == fi.Exists) return;
                    if (validOrNot)
                    {
                        File.WriteAllText(stampFile, "" + DateTime.Now);
                    }
                    else
                    {
                        File.Delete(stampFile);
                    }
                }
            }
        }

        public bool GetRaptstoreValid()
        {
            lock (ExternDB.mylock)
            {
                if (_UseRapstoreDB)
                {
                    if (!this.HavePersonalPath)
                    {
                        Console.WriteLine(
                            "Error No personal directory.. so maybe should not check the rapstore base directory yet");
                    }
                    string rapStoreDirectoryNonStem = Graphmaster.CalcRapStoreDirectory();
                    if (!Directory.Exists(rapStoreDirectoryNonStem))
                    {
                        return false;
                    }
                    string stampFile = Path.Combine(rapStoreDirectoryNonStem, "valid.txt");
                    var fi = new FileInfo(stampFile);
                    return fi.Exists;
                }
                return false;
            }
        }



        public void DeleteInvalidRaptstore()
        {
            lock (ExternDB.mylock)
            {
                if (UseRapstoreDB)
                {
                    string rapStoreDirectoryNonStem = Graphmaster.CalcRapStoreDirectory();
                    Logger.Warn("deleting " + rapStoreDirectoryNonStem);
                    if (Directory.Exists(rapStoreDirectoryNonStem))
                    {
                        Directory.Delete(rapStoreDirectoryNonStem, true);
                    }
                    else
                    {
                        if (PersonalReadWriteDirectory == null)
                        {
                            Logger.Warn("ERRROR! I dont know my personal path!");
                            return;
                        }
                    }
                    Directory.CreateDirectory(rapStoreDirectoryNonStem);

                }
            }
        }

        /// <summary>
        /// How big to let the log buffer get before writing to disk
        /// </summary>
        private int MaxLogBufferSize
        {
            get
            {
                if (GlobalSettings == null) return 1000;
                string size = this.GlobalSettings.grabSetting("maxlogbuffersize");
                return Convert.ToInt32(size);
            }
        }

        /// <summary>
        /// Flag to show if the bot is willing to accept user input
        /// </summary>
        public bool isAcceptingUserInput = true;

        /// <summary>
        /// Flag to show if the bot is producing output
        /// </summary>
        public bool isPerformingOutput { get; set; }

        static public object loglock = new object();

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

        /// <summary>
        /// The message to show if a user tries to use the bot whilst it is set to not process user input
        /// </summary>
        private string NotAcceptingUserInputMessage
        {

            get { return this.GlobalSettings.grabSetting("notacceptinguserinputmessage"); }
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
            get { return this.GlobalSettings.grabSetting("timeoutmessage"); }
        }

        /// <summary>
        /// The locale of the bot as a CultureInfo object
        /// </summary>
        public CultureInfo Locale
        {
            get { return new CultureInfo(this.GlobalSettings.grabSetting("culture")); }
        }


        /// <summary>
        /// Will match all the illegal characters that might be inputted by the user
        /// </summary>
        public Regex Strippers
        {
            get { return new Regex(this.GlobalSettings.grabSetting("stripperregex"), RegexOptions.IgnorePatternWhitespace); }
        }

        /// <summary>
        /// The email address of the botmaster to be used if WillCallHome is set to true
        /// </summary>
        public string AdminEmail
        {
            get { return this.GlobalSettings.grabSetting("adminemail"); }
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
                    this.GlobalSettings.addSetting("adminemail", "");
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
                if (GlobalSettings == null) return true;
                string willcallhome = this.GlobalSettings.grabSetting("willcallhome") ?? "true";
                if (willcallhome.ToLower() == "true")
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
        /// The number of categories this bot has in its graphmaster "brain"
        /// </summary>
        public int SizeC;

        /// <summary>
        /// The default "brain" of the bot (also "*")
        /// </summary>
        public GraphMaster Graphmaster
        {
            get { return DefaultStartGraph; }
        }

        /// <summary>
        /// The named "brains" of the bot
        /// default graphmaster should be listed under "*"
        /// </summary>
        public Dictionary<string, GraphMaster> Graphs
        {
            get { return LocalGraphsByName; }
        }

        private string _PathToUserFiles;

        public string PathToUsersDir
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
        /// A general stack to remember things to mention later
        /// </summary>
        public Stack<string> conversationStack = new Stack<string>();

        /// <summary>
        /// in the <say> tag should the sapi be passed as-is (using innerXML) or not (using innerText)
        /// usually set when the sayProcessor delegate is set
        /// </summary>
        public bool saySapi = false;

        #endregion

        #region Delegates

        public delegate void LogMessageDelegate();

        #endregion

        internal BehaviorContext servitorBot
        {
            get
            {
                if (_behaviorContext != null) return _behaviorContext;
                return this.BotBehaving;
            }
        }

        private BehaviorContext _behaviorContext;
        /// <summary>
        /// When behaviour trees run they need a body/situational context like is "current user", "last heard" etc
        /// </summary>
        internal BehaviorContext BotBehaving
        {
            get
            {
                if (_behaviorContext == null)
                {
                    writeToLogWarn("Bot *NOT* Behaving == null");
                }
                return _behaviorContext;
            }
            set
            {
                if (value == _behaviorContext) return;
                if (value==null)
                {
                    _behaviorContext.Release();
                    return;
                }
                else
                {
                    _behaviorContext = value;
                    _behaviorContext.bot = this;
                }
            }
        }

        public sayProcessorDelegate sayProcessor
        {
            get
            {
                if (BotBehaving != null)
                {
                    return BotBehaving.sayProcessor;
                }
                return _sayProcessor;
            }
            set
            {
                if (BotBehaving != null)
                {
                    BotBehaving.sayProcessor = value;
                    return;
                }
                _sayProcessor = value;
            }
        }
        internal sayProcessorDelegate _sayProcessor;
        internal systemPersonaDelegate _personaProcessor;
        public systemPersonaDelegate personaProcessor
        {
            get
            {
                if (BotBehaving != null)
                {
                    return BotBehaving.personaProcessor;
                }
                return _personaProcessor;
            }
            set
            {
                if (BotBehaving != null)
                {
                    BotBehaving.personaProcessor = value;
                    return;
                }
                _personaProcessor = value;
            }
        }

        //public User lastBehaviorUser{get { return BehaviorUser.lastBehaviorUser; }}

        #region Events

        public event LogMessageDelegate WrittenToLog;

        #endregion

        /// <summary>
        /// Loads AIML from .aiml files into the graphmaster "brain" of the bot
        /// Loads the AIML from files found in the bot's AIMLpath into the bot's brain
        /// </summary>
        private void loadAIMLFromFiles_unlocked()
        {
            loadGlobalBotSettings();
            string botname = GlobalSettings.grabSetting("name");
            SetName(botname);
            loadAIMLFromFiles(PathToAIML);
        }

        /// <summary>
        /// Loads AIML from .aiml at dirPath files into the graphmaster "brain" of the bot
        /// </summary>
        public void loadAIMLFromFiles(string dirPath)
        {
            loadAIMLFromFiles(dirPath, Graphmaster.ScriptingName);
        }
        /// <summary>
        /// Loads AIML from .aiml at dirPath files into the graphmaster "brain" of the bot
        /// </summary>
        public void loadAIMLFromFiles(string dirPath, string graphName)
        {
            var aloader = GetLoader(GetBotRequest("loadAIMLFromFiles: " + dirPath + " into " + graphName));
            var loader = aloader.loadOpts;
            loader.graphName = graphName;
            loader.CurrentlyLoadingFrom = dirPath;
            aloader.loadAIMLURI(dirPath);
        }

        public void loadAIMLFromFile(string filePath)
        {
            loadAIMLFromFiles(filePath);
        }

        /// <summary>
        /// Allows the bot to load a new XML version of some AIML
        /// </summary>
        /// <param name="newAIML">The XML document containing the AIML</param>
        /// <param name="filename">The originator of the XML document</param>
        public void loadAIMLFromXML(XmlNode newAIML, string filename)
        {
            Console.WriteLine("Check:loadAIMLFromXML(0)");
            var loader = GetLoader(GetBotRequest("loadAIMLFromFiles: " + filename));
            loader.loadOpts.graphName = this.Graphmaster.ScriptingName;
            loader.loadAIMLFromXML(newAIML, filename);
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
                {
                }
            }
        }


        /// <summary>
        /// Instantiates the dictionary objects and collections associated with this class
        /// </summary>
        private void setup()
        {
            this.myCron = new Cron(this);
            //LocalGraphsByName["default"] =
            //EnsureLocalGraphs();
            //TheNLKB = TheNLKB ?? new NatLangDb(this);
            //            BotAsRequestUsed = new AIMLbot.Request("-bank-input-", BotAsUser, this, null);
            AddExcuteHandler("aiml", EvalAIMLHandler);
            AddExcuteHandler("bot", LightWeigthBotDirective);
            AddExcuteHandler("csharp", CSharpExec);
            AddExcuteHandler("cs", CSharpExec);
            AddExcuteHandler("siprolog", SIPrologExec);

            if (prologEngine != prologEngine)
            {
                Console.WriteLine("Cannot maintain a stable wormhole while loading SIPro)");
            }
            if (TheCycS == null)
            {
                TheCycS = new CycDatabase(this);
            }
            CycAccess v = TheCyc.GetCycAccess;
            SettingsDictionaryReal.WarnOnNull = false;
            this.RelationMetaProps = MakeSettingsDictionary("RelationMetaPropsMt");
            RegisterDictionary("chat.relationprops", RelationMetaProps);
            SettingsDictionaryReal.WarnOnNull = true;
            this.GenderSubstitutions = MakeSubstsDictionary("substituions.gender");
            this.Person2Substitutions = MakeSubstsDictionary("substituions.person2");
            this.PersonSubstitutions = MakeSubstsDictionary("substituions.person");
            this.InputSubstitutions = MakeSubstsDictionary("substituions.input");
            this.DefaultPredicates = MakeSettingsDictionary("DefaultPredicatesMt");
            this.AllUserPreds = MakeSettingsDictionary("AllUserPredsMt");
            RegisterDictionary("allusers", AllUserPreds);
            if (_behaviorContext==null)
            {
                _behaviorContext = new BehaviorContext(this, null);
            }
            User guser =
                ExemplarUser =
                LastUser =
                ExemplarUser ??
                FindUser("ExemplarUser") ?? new MasterUser("ExemplarUser", "ExemplarUser", this, DefaultPredicates);
            _behaviorContext._user = guser;
            guser.BehaviorContext = _behaviorContext;
            guser.SetMeMyselfAndI("ExemplarUser");
            RegisterDictionary("defaults", DefaultPredicates);
            DefaultPredicates.InsertFallback(() => this.AllUserPreds);
            this.GlobalSettings = MakeSettingsDictionary("GlobalSettingsMt");
            this.GlobalSettings.bbPrefix = "bot";
            RegisterDictionary("bot", GlobalSettings);
            string botName = NameAsSet ?? BotUserID ?? NamePath ?? BotID;
            var namePath = NamePath;
            _botAsUser = _botAsUser ??
                         FindUser(botName) ??
                         new MasterUser(namePath, botName, this, GlobalSettings);
            this.HeardPredicates = MakeSettingsDictionary("HeardPredicatesMt");
            RegisterDictionary("chat.heardpredicates", HeardPredicates);
            RegisterDictionary("bot.alluserpred", this.AllUserPreds);
            this.AllUserPreds["dictname"] = "bot.AllUserPreds";
            this.HeardPredicates["dictname"] = "bot.HeardPredicates";
            this.AllUserPreds["iamAllUsers"] = "True";
            this.HeardPredicates["iamAllUsers"] = "False";
            this.AllUserPreds["iamHeardPredicates"] = "False";
            this.HeardPredicates["iamHeardPredicates"] = "True";
            this.HeardPredicates["iamHeardPredicatesLocally"] = "True";
            this.CustomTags = new Dictionary<string, TagHandler>();
            //this.Graphs = new Dictionary<string, GraphMaster>();
            //this.Graphmaster = new GraphMaster("base", this);
            Graphmaster.AddName(this,"*");
            Graphmaster.AddName(this, "default");
            Graphmaster.AddName(this, "base");
            //this._h= new GraphMaster("heardselfsay", this);
            this.setupDictionaries();
            GlobalSettings.IsTraced = true;
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
            KeyValueListSIProlog v = new KeyValueListSIProlog(() => this.prologEngine, mtName, "chatVar");
            var dict = new SettingsDictionaryReal(mtName, this, (KeyValueList) v);
            dict.bbPrefix = "user";
            if (mtName != named)
            {
                RegisterDictionary(named, dict);
            }
            return dict;
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
            return new SettingsDictionaryReal(named, this,
                                              new KeyValueListCSharp(new List<string>(),
                                                                     new Dictionary<string, string>()))
                       {IsSubsts = true, TrimKeys = false};
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

        /// <summary>
        /// Loads settings and configuration info from various xml files referenced in the settings file passed in the args. 
        /// Also generates some default values if such values have not been set by the settings file.
        /// </summary>
        /// <param name="pathToSettings">Path to the settings xml file</param>
        public void loadSettings(string pathToSettings)
        {
            this.GlobalSettings.loadSettings(pathToSettings);

            // Checks for some important default settings
            if (!this.GlobalSettings.containsSettingCalled("version"))
            {
                this.GlobalSettings.addSetting("version", Environment.Version.ToString());
            }
            if (!this.GlobalSettings.containsSettingCalled("name"))
            {
                this.GlobalSettings.addSetting("name", "Unknown");
            }
            if (!this.GlobalSettings.containsSettingCalled("botmaster"))
            {
                this.GlobalSettings.addSetting("botmaster", "Unknown");
            }
            if (!this.GlobalSettings.containsSettingCalled("master"))
            {
                this.GlobalSettings.addSetting("botmaster", "Unknown");
            }
            if (!this.GlobalSettings.containsSettingCalled("author"))
            {
                this.GlobalSettings.addSetting("author", "Nicholas H.Tollervey");
            }
            if (!this.GlobalSettings.containsSettingCalled("location"))
            {
                this.GlobalSettings.addSetting("location", "Unknown");
            }
            if (!this.GlobalSettings.containsSettingCalled("gender"))
            {
                this.GlobalSettings.addSetting("gender", "-1");
            }
            if (!this.GlobalSettings.containsSettingCalled("birthday"))
            {
                this.GlobalSettings.addSetting("birthday", "2006/11/08");
            }
            if (!this.GlobalSettings.containsSettingCalled("birthplace"))
            {
                this.GlobalSettings.addSetting("birthplace", "Towcester, Northamptonshire, UK");
            }
            if (!this.GlobalSettings.containsSettingCalled("website"))
            {
                this.GlobalSettings.addSetting("website", "http://sourceforge.net/projects/AltAIMLbot");
            }
            if (this.GlobalSettings.containsSettingCalled("adminemail"))
            {
                string emailToCheck = this.GlobalSettings.grabSetting("adminemail");
                this.AdminEmail = emailToCheck;
            }
            else
            {
                this.GlobalSettings.addSetting("adminemail", "");
            }
            if (!this.GlobalSettings.containsSettingCalled("islogging"))
            {
                this.GlobalSettings.addSetting("islogging", "False");
            }
            if (!this.GlobalSettings.containsSettingCalled("willcallhome"))
            {
                this.GlobalSettings.addSetting("willcallhome", "False");
            }
            if (!this.GlobalSettings.containsSettingCalled("timeout"))
            {
                this.GlobalSettings.addSetting("timeout", "200000");
            }
            if (!this.GlobalSettings.containsSettingCalled("timeoutmessage"))
            {
                this.GlobalSettings.addSetting("timeoutmessage", "ERROR: The request has timed out.");
            }
            if (!this.GlobalSettings.containsSettingCalled("culture"))
            {
                this.GlobalSettings.addSetting("culture", "en-US");
            }
            if (!this.GlobalSettings.containsSettingCalled("splittersfile"))
            {
                this.GlobalSettings.addSetting("splittersfile", "Splitters.xml");
            }
            if (!this.GlobalSettings.containsSettingCalled("person2substitutionsfile"))
            {
                this.GlobalSettings.addSetting("person2substitutionsfile", "Person2Substitutions.xml");
            }
            if (!this.GlobalSettings.containsSettingCalled("personsubstitutionsfile"))
            {
                this.GlobalSettings.addSetting("personsubstitutionsfile", "PersonSubstitutions.xml");
            }
            if (!this.GlobalSettings.containsSettingCalled("gendersubstitutionsfile"))
            {
                this.GlobalSettings.addSetting("gendersubstitutionsfile", "GenderSubstitutions.xml");
            }
            if (!this.GlobalSettings.containsSettingCalled("defaultpredicates"))
            {
                this.GlobalSettings.addSetting("defaultpredicates", "DefaultPredicates.xml");
            }
            if (!this.GlobalSettings.containsSettingCalled("substitutionsfile"))
            {
                this.GlobalSettings.addSetting("substitutionsfile", "Substitutions.xml");
            }
            if (!this.GlobalSettings.containsSettingCalled("aimldirectory"))
            {
                this.GlobalSettings.addSetting("aimldirectory", "aiml");
            }
            if (!this.GlobalSettings.containsSettingCalled("configdirectory"))
            {
                this.GlobalSettings.addSetting("configdirectory", "config");
            }
            if (!this.GlobalSettings.containsSettingCalled("logdirectory"))
            {
                this.GlobalSettings.addSetting("logdirectory", "logs");
            }
            if (!this.GlobalSettings.containsSettingCalled("maxlogbuffersize"))
            {
                this.GlobalSettings.addSetting("maxlogbuffersize", "64");
            }
            if (!this.GlobalSettings.containsSettingCalled("notacceptinguserinputmessage"))
            {
                this.GlobalSettings.addSetting("notacceptinguserinputmessage",
                                               "This bot is currently set to not accept user input.");
            }
            if (!this.GlobalSettings.containsSettingCalled("stripperregex"))
            {
                this.GlobalSettings.addSetting("stripperregex", "[^0-9a-zA-Z]");
            }
            this.DefaultPredicates.bbPrefix = "user";
            this.GlobalSettings.bbPrefix = "bot";

            // Load the dictionaries for this Bot from the various configuration files
            this.Person2Substitutions.loadSettings(Path.Combine(this.PathToConfigFiles,
                                                                this.GlobalSettings.grabSetting(
                                                                    "person2substitutionsfile")));
            this.PersonSubstitutions.loadSettings(Path.Combine(this.PathToConfigFiles,
                                                               this.GlobalSettings.grabSetting("personsubstitutionsfile")));
            this.GenderSubstitutions.loadSettings(Path.Combine(this.PathToConfigFiles,
                                                               this.GlobalSettings.grabSetting("gendersubstitutionsfile")));
            this.DefaultPredicates.loadSettings(Path.Combine(this.PathToConfigFiles,
                                                             this.GlobalSettings.grabSetting("defaultpredicates")));
            this.InputSubstitutions.loadSettings(Path.Combine(this.PathToConfigFiles,
                                                              this.GlobalSettings.grabSetting("substitutionsfile")));

            // Grab the splitters for this bot
            this.loadSplitters(Path.Combine(this.PathToConfigFiles, this.GlobalSettings.grabSetting("splittersfile")));
        }

        /// <summary>
        /// Loads the splitters for this bot from the supplied config file (or sets up some safe defaults)
        /// </summary>
        /// <param name="pathToSplitters">Path to the config file</param>
        private void loadSplitters(string pathToSplitters)
        {
            lock (Splitters0) loadSplitters_ul(pathToSplitters);
        }

        private void loadSplitters_ul(string pathToSplitters)
        {
            if (pathToSplitters == null) return;
            FileInfo splittersFile = new FileInfo(pathToSplitters);
            var Splitters = Splitters0;
            if (splittersFile.Exists)
            {
                XmlDocument splittersXmlDoc = new XmlDocument();
                splittersXmlDoc.Load(pathToSplitters);
                // the XML should have an XML declaration like this:
                // <?xml version="1.0" encoding="utf-8" ?> 
                // followed by a <root> tag with children of the form:
                // <item value="value"/>
                //if (splittersXmlDoc.ChildNodes.Count == 2)
                {
                    if (splittersXmlDoc.LastChild.HasChildNodes)
                    {
                        foreach (XmlNode myNode in splittersXmlDoc.LastChild.ChildNodes)
                        {
                            if ((myNode.Name == "item") & (myNode.Attributes.Count == 1))
                            {
                                string value = myNode.Attributes["value"].Value;
                                Splitters.Add(value);
                            }
                        }
                    }
                }
            }
            if (Splitters.Count == 0)
            {
                // we don't have any splitters, so lets make do with these...
                Splitters.Add(".");
                Splitters.Add("!");
                Splitters.Add("?");
                Splitters.Add(";");
            }
        }

        // #endregion

        #region Logging methods

        /// <summary>
        /// The last message to be entered into the log (for testing purposes)
        /// </summary>
        // public string LastLogMessage=string.Empty;

        /// <summary>
        /// Writes a (timestamped) message to the bot's log.
        /// 
        /// Log files have the form of yyyyMMdd.log.
        /// </summary>
        /// <param name="message">The message to log</param>

        #endregion
        /*
        #region Conversation methods

        public void processOutputQueue()
        {
            if (!isPerformingOutput)
            {
                if (outputQueue.Count > 0)
                {
                    myBehaviors.logText("BOT OUTPUT GOING TO NOT COME OUT:" + outputQueue.Count);
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
                myBehaviors.logText("BOT OUTPUT:" + msg);

            }
        }

        public void flushOutputQueue()
        {
            if (outputQueue.Count>0)
            {
                while (outputQueue.Count > 0)
                {
                    string msg = outputQueue.Dequeue();
                    msg = string.Format("flushOutputQueue BOT OUTPUT:{0}", msg);
                    Console.WriteLine(msg);
                    myBehaviors.logText("BOT OUTPUT:" + msg);
                }
                
            }
            outputQueue.Clear();
            myBehaviors.logText("BOT flushOutputQueue:");
            string flushsignal = GlobalSettings.grabSetting("flushsignal", false);
            if ((flushsignal != null) && (flushsignal.Length > 2))
            {
                postOutput(flushsignal);
            }

        }

        private Regex SentRegex = new Regex(@"(\S.+?[.!?,\)])(?=\s+|$)");

        public void postOutput(string msg)
        {
            if (string.IsNullOrEmpty(msg)) return;

            if (msg.Length < 256)
            {
                // just post output
                outputQueue.Enqueue(msg);
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
                    outputQueue.Enqueue(s);
                    myBehaviors.logText("BOT postOutput:" + s);

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

        public string getPendingOutput()
        {
            string outmsg = "";
            while (outputQueue.Count > 0)
            {
                string msg = outputQueue.Dequeue();
                outmsg += msg + "\r\n";
            }
            return outmsg;
        }*/

        public void loadCrons()
        {
            lock (ExternDB.mylock)
            {

                if (UseRapstoreDB)
                {
                    var GM = Graphmaster ?? GetGraph("default");
                    var chatDB = GM.ensureEdb();

                    chatDB.loadCronList(myCron);
                    GM.Close();
                }
            }
        }

        public void saveCrons()
        {
            lock (ExternDB.mylock)
            {

                if (UseRapstoreDB)
                {
                    var GM = Graphmaster;
                    var chatDB = GM.ensureEdb();
                    chatDB.saveCronList(myCron);
                    GM.Close();
                }
            }
        }


        /// <summary>
        /// Given some raw input and a unique ID creates a response for a new user
        /// </summary>
        /// <param name="rawInput">the raw input</param>
        /// <param name="UserGUID">an ID for the new user (referenced in the result object)</param>
        /// <returns>the result to be output to the user</returns>
        public AltAIMLbot.Result Chat(string rawInput, string UserGUID)
        {
            User user = FindOrCreateUser(UserGUID);
            Request request = new Request(rawInput, user, user.That, this, true, RequestKind.ChatRealTime);
            return this.Chat(request);
        }

        public AltAIMLbot.Result Chat(Request request)
        {
            return Chat(request, request.graphName);
        }
        
        /// <summary>
        /// TryRestorableUserRequest==true  means restore everything!
        /// </summary>
        public bool TryRestorableUserRequest = false;

        /// <summary>
        /// Given a request containing user input, produces a result from the bot.
        /// Sensitive to request.user.Qstate
        /// </summary>
        /// <param name="request">the request from the user</param>
        /// <returns>the result to be output to the user</returns>
        public Result Chat(Request request, string graphID)
        {
            if (TryRestorableUserRequest)
            {
                GraphMaster ourGraphMaster = this.GetGraph(graphID) ?? Graphmaster;
                request.Graph = ourGraphMaster;
                // actully calls ChatSensitiveToQState but sets up and restores the context
                return ChatWR(request);                
            }
            Result res = null;
            try
            {
                res = ChatSensitiveToQState(request, request.graphName, request.IsToplevelRequest);
            }
            finally
            {
                var onExit = request.OnResultComplete;
                if (onExit != null)
                {
                    request.OnResultComplete = null;
                    onExit(res);
                }
            }
            return res;
        }

        public Result ChatSensitiveToQState(Request request, string graphID, bool isToplevel)
        {
            GraphMaster ourGraphMaster = this.GetGraph(graphID) ?? Graphmaster;
            request.Graph = ourGraphMaster;
            graphID = ourGraphMaster.ScriptingName;

            User user = request.user;
            var result = new MasterResult(user, this, request);
            bool saveResult = isToplevel && request.SaveResultsOnJustHeard;
            bool isBtx = request.RequestType.ContainsAny(RequestKind.BTX);
            lock (ExternDB.mylock)
            {
                if (this.isAcceptingUserInput)
                {
                    // Mark the input time
                    bool markInputTime = saveResult || isBtx;
                    if (markInputTime) myBehaviors.keepTime("lastchatinput", RunStatus.Success);
                    if (markInputTime) myBehaviors.activationTime("lastchatinput", RunStatus.Success);

                    // Normalize the input
                    AIMLLoader loader = request.Loader;
                    SplitIntoSentences splitter = new SplitIntoSentences(null, request.rawInput);
                    string[] rawSentences = splitter.Transform();


                    if (UseRapstore(graphID))
                    {
                        ourGraphMaster.ensureEdb();
                    }
                    if (user.Qstate.Count == 0)
                    {
                        result.ClearInput();
                        Console.WriteLine("DEBUG:Using Normal Search");
                        // Standard operation
                        foreach (string sentence in rawSentences)
                        {
                            result.InputSentences.Add(sentence);
                            List<string> paths = gatherPaths(request.graphName, user, sentence,
                                                             user.getPreStates(), user.getPostStates(),
                                                             loader);
                            SortPaths(paths, ourGraphMaster.getPathScore);
                            foreach (var path in paths)
                            {
                                result.GraphMasterPaths.Add(path);
                            }
                        }
                    }
                    else
                    {
                        // non-deterministic search
                        Console.WriteLine("DEBUG:Using non-deterministic Search");
                        foreach (string sentence in rawSentences)
                        {
                            result.InputSentences.Add(sentence);
                            // see which state would give the best score and keep that path
                            // one problem might be a "_" in a <state> overriding other patterns
                            // but that's the semantics

                            // The dictionary could contain probability values associated with
                            // the states, so you could use that information in the selection process
                            // or to order the choices (given two paths of equal score use the
                            // state probability as the tie breaker)
                            string bestpath = "";
                            string beststate = "";
                            double bestv = -1;
                            foreach (string nstate in user.Qstate.Keys)
                            {
                                var states = new[] {nstate};
                                List<string> paths = gatherPaths(request.graphName, user, sentence, states,
                                                                 states, loader);
                                SortPaths(paths, ourGraphMaster.getPathScore);

                                string path = paths[0];
                                //double statev = this.Graphmaster.getPathScore(path);
                                double statev = ourGraphMaster.getPathScore(path);

                                if (statev == bestv)
                                {
                                    if (user.Qstate[nstate] > user.Qstate[beststate])
                                    {
                                        bestpath = path;
                                        beststate = nstate;
                                        bestv = statev;
                                    }
                                }
                                else if (statev > bestv)
                                {
                                    bestpath = path;
                                    beststate = nstate;
                                    bestv = statev;
                                }
                            }
                            if (!result.GraphMasterPaths.Contains(bestpath))
                            {
                                result.GraphMasterPaths.Add(bestpath);
                            }
                            Console.WriteLine("DEBUG: path = " + bestpath);
                        }


                    }

                    // grab the templates for the various sentences from the graphmaster
                    foreach (string path in result.GraphMasterPaths)
                    {
                        AltAIMLbot.Utils.SubQuery query = new SubQuery(path, result, request);

                        var queryTemplate = ourGraphMaster.evaluate(path, query, request, MatchState.Pattern,
                                                                    new StringBuilder());
                        if (string.IsNullOrEmpty(queryTemplate))
                        {
                            myBehaviors.SkipLog = false;
                            if (DLRConsole.Trace("failed to find response to " + path))
                            {
                                queryTemplate = ourGraphMaster.evaluate(path, query, request, MatchState.Pattern, new StringBuilder());
                                MainConsoleWriteLn("RETRY PATH: " + path + " = " + queryTemplate);
                            }
                            //myBehaviors.SkipLog = true;
                            myBehaviors.logText("failed to find response to " + path);
                            continue;
                        }
                        else
                        {
                            myBehaviors.SkipLog = true;
                        }
                        query.Template = queryTemplate;
                        //query.Template = this.Graphmaster.evaluate(path, query, request, MatchState.UserInput, new StringBuilder());
                        //query.Template = ourGraphMaster.evaluate(path, query, request, MatchState.UserInput, new StringBuilder());
                        Console.WriteLine("DEBUG: TemplatePath = " + query.TemplatePath);
                        Console.WriteLine("DEBUG: Template = " + query.Template);

                        myBehaviors.logText("CHAT QueryPath:" + path);
                        myBehaviors.logText("CHAT TemplatePath:" + query.TemplatePath);
                        myBehaviors.logText("CHAT Template:\n" + query.Template);

                        result.SubQueries.Add(query);
                    }
                    if (UseRapstore(graphID))
                    {
                        var chatDB = ourGraphMaster.chatDB;
                        if (chatDB != null)
                        {
                            if (servitor.PruneSize > 0) chatDB.prune(servitor.PruneSize);
                            //chatDB.Close();
                            //chatDB = null;
                        }
                    }
                    if (result.SubQueries.Count == 0)
                    {
                        Console.WriteLine("DEBUG: MISSING SubQueries");
                        DLRConsole.Trace("failed to find response to " + request);
                    }
                    // process the templates into appropriate output
                    foreach (SubQuery query in result.SubQueries)
                    {
                        if (query.Template.Length > 0)
                        {
                            try
                            {
                                string outputSentence = GetOutputSentence(query.Template, null, query, request, result,
                                                                          user, true);
                                if (outputSentence.Length > 0)
                                {
                                    bool useOutput = true;
                                    //Result userLastResult = user.LastResult;
                                    Result userLastResult = result;
                                    if (servitor.ChatOptions.SqueltchRepeatedLastOutput && userLastResult != null)
                                    {
                                        var oses = userLastResult.OutputSentences;
                                        foreach (var os in oses)
                                        {
                                            if (os.AsString().Contains(outputSentence))
                                            {
                                                useOutput = false;
                                            }
                                        }
                                    }
                                    if (useOutput)
                                    {
                                        result.AddOutputSentences(outputSentence);
                                        saveResult = true;
                                    }
                                    else
                                    {
                                        result.resultCount++;
                                    }
                                }
                                else
                                {
                                    result.resultCount++;
                                }
                            }
                            catch (Exception e)
                            {
                                if (this.WillCallHome)
                                {
                                    this.phoneHome(e.Message, request);
                                }
                                this.writeToLog(
                                    "WARNING! A problem was encountered when trying to process the input: " +
                                    request.rawInput + " with the template: \"" + query.Template + "\"");
                                Console.WriteLine("ERR:" + e.Message);
                                Console.WriteLine("ERR:" + e.StackTrace);
                            }
                        }
                    }
                    if (result.resultCount == 0)
                    {
                        Console.WriteLine("DEBUG: MISSING resultCount");
                    }
                }
                else
                {
                    result.AddOutputSentences(this.NotAcceptingUserInputMessage);
                }

                // populate the Result object
                result.Duration = DateTime.Now - request.StartedOn;
                if (!saveResult)
                {
                    //                   Console.WriteLine("*** CHATOUTPUT1(" + result.Output+")");

                    return result;
                }
                if (isToplevel) user.addResult(result);
            }
//            Console.WriteLine("*** CHATOUTPUT2(" + result.Output + ")");

            return result;
        }


        private static void SortPaths(List<string> paths, Func<string, double> func)
        {
            paths.Sort((s1, s2) =>
                           {
                               var diff = func(s1).CompareTo(func(s2));
                               if (diff != 0) return diff;
                               diff = s1.Length.CompareTo(s2.Length);
                               if (diff != 0) return diff;
                               return s1.CompareTo(s2);
                           });
        }

        private static List<string> gatherPaths(string graphName, User user, string sentence,
                                                IEnumerable<string> usergetPreStates,
                                                IEnumerable<string> usergetPostStates,
                                                AIMLLoader loader)
        {
            List<string> normalizedPaths = new List<string>();
            int maxThats = 2;
            int thats = 0;
            foreach (var that in user.getThats())
            {
                if (thats++ >= maxThats) break;
                foreach (var topic in user.getTopics())
                {
                    foreach (var prestates in usergetPreStates)
                    {
                        foreach (var poststates in usergetPostStates)
                        {
                            string path = loader.generateCPath(graphName, sentence, that, "*", topic,
                                                                   prestates, poststates,
                                                                   true, null, loader.bot);
                            if (!normalizedPaths.Contains(path))
                            {
                                normalizedPaths.Add(path);
                            }
                            Console.WriteLine("DEBUG: path = " + path);
                        }
                    }
                }
            }
            return normalizedPaths;
        }
        public object evalTemplateNodeInnerXml(XmlNode templateNodeInnerXML, RequestKind requestType)
        {
            return evalTemplateNode(templateNodeInnerXML, requestType, BotBehaving);
        }
        internal object evalTemplateNodeInnerXml(XmlNode templateNodeInnerXML, RequestKind requestType, BehaviorContext buBehaviorContext)
        {
            if (true)
            {
                StringBuilder SB = new StringBuilder();
                int childsDone = 0;
                Unifiable vv = null;
                foreach (XmlNode childNode in templateNodeInnerXML)
                {
                    childsDone++;
                    // otherwise take the tag content as a srai (to trip say a random reply)
                    const bool expandOnNoHits = true; // actually WordNet
                    const float threshold = 0.0f;
                    bool childSuccess;
                    AIMLTagHandler tagHandlerUChild = buBehaviorContext.GetChildTagHandler(childNode);
                    if (childsDone == 2)
                    {
                        SB.Append("" + vv);
                    }
                    vv = AIMLTagHandler.ProcessTagHandlerNode(childNode, true, false, out childSuccess,
                                                              tagHandlerUChild);
                    if (childsDone >= 2)
                    {
                        SB.Append("" + vv);
                    }

                }
                if (childsDone == 1)
                {
                    return vv;
                }
                vv = SB.ToString();
                return vv;
            }
            return evalTemplateXml(templateNodeInnerXML.InnerXml, requestType, buBehaviorContext);
        }

        internal object evalTemplateXml(string templateNodeString, RequestKind requestType, BehaviorContext buBehaviorContext)
        {
            if (!templateNodeString.StartsWith("<template"))
            {
                templateNodeString = string.Format("<template>{0}</template>", templateNodeString);
            }
            XmlNode resultTemplateNode = AIMLTagHandler.getNode(templateNodeString);
            return evalTemplateNode(resultTemplateNode, requestType, buBehaviorContext);
        }

        /// <summary>
        /// given an template side XML, try evaluating it
        /// </summary>       
        internal object evalTemplateNode(XmlNode templateNode, RequestKind requestType, BehaviorContext buBehaviorContext)
        {
            if (StaticXMLUtils.IsBlank(templateNode)) return "";

            string s = "evalTemplateNode: " + templateNode.OuterXml;
            s = Unifiable.Trim(s);
            if (!s.StartsWith("<")) s = "<!-- " + s.Replace("<!--", "<#").Replace("-->", "#>") + " -->";

            Request request = GetBotRequest(s, requestType | RequestKind.TemplateExpander);            
            Result result = new MasterResult(request.user, this, request);
            SubQuery query = new SubQuery("SubQuery " + s, result, request);


            string outputSentence = this.processNode(templateNode, query, request, result, request.user, true);
            return outputSentence;
        }

        /// <summary>
        /// Recursively evaluates the template nodes returned from the bot
        /// </summary>
        /// <param name="node">the node to evaluate</param>
        /// <param name="query">the query that produced this node</param>
        /// <param name="request">the request from the user</param>
        /// <param name="result">the result to be sent to the user</param>
        /// <param name="user">the user who originated the request</param>
        /// <returns>the output string</returns>
        public string processNode(XmlNode node, SubQuery query, Request request, AltAIMLbot.Result result,
                                  AltAIMLbot.User user, bool allowProcess)
        {
            // check for timeout (to avoid infinite loops)
            if (request.MayTimeOut && request.StartedOn.AddMilliseconds(request.bot.TimeOut) < DateTime.Now)
            {
                request.bot.writeToLog("WARNING! Request timeout. User: " + request.user.UserID + " raw input: \"" +
                                       request.rawInput + "\" processing template: \"" + query.Template + "\"");
                request.hasTimedOut = true;
                return string.Empty;
            }
            CheckRequestResultQuery(request, result, query);

            // DMILES: for KHC, a local way we can switch back and forth
            OutputDelegate TRACE = DEVNULL;

            // process the node
            string tagName = node.LocalName.ToLower();
            myBehaviors.logNode("AIML", node);

            if (tagName == "template" || tagName == "li")
            {
                string pt = GetOutputSentence(node.OuterXml, node, query, request, result, user, allowProcess);
                return pt;
            }
            if (tagName == "#text")
            {
                if (node.Value != node.InnerText || node.Value != node.OuterXml)
                {
                    writeToLogWarn("Text node contaning entities " + node.OuterXml);
                }
                return node.InnerText;
                return node.Value;
            }
            if (tagName == "#whitespace")
            {
                return " ";
            }

            AIMLTagHandler tagHandler = GetTagHandler(node, query, request, result, user, false);
            if (tagHandler == null)
            {
                Console.WriteLine("MISSING HANDLER FOR:{0}", tagName);
            }
            if (tagHandler == null || tagHandler.IsTraced)
            {
                TRACE = Console.WriteLine;
            }
            if (object.Equals(null, tagHandler))
            {
                string debug = node.OuterXml;
                if (node.NodeType == XmlNodeType.Text) return node.InnerText;
                if (node.NodeType == XmlNodeType.Whitespace) return node.InnerText;
                if (node.NodeType == XmlNodeType.SignificantWhitespace) return node.InnerText;
                if (node.NodeType == XmlNodeType.Comment) return " , ";
                TRACE(" -- Result0 {0} : {1}", tagName, node.InnerText);
                return node.InnerText;
            }
            string resultNodeInnerXML;
            if (tagHandler.SelfProcessing)
            {
                resultNodeInnerXML = tagHandler.Transform();
            }
            else if (tagHandler.isRecursive)
            {
                if (node.HasChildNodesNonText())
                {
                    string resultNodeInnerInnerXML = tagHandler.TransformInner();
                    // we should have filled in the inner text by now
                    resultNodeInnerXML = tagHandler.Transform();
                    //string resultNodeInnerXML = node.InnerXml;
                    TRACE(" -- Result1 {0} : '{1}' {2}", tagName, resultNodeInnerXML, tagHandler);
                }
                else
                {
                    // this is for "recursive" taghandlers with no children!

                    // tagHandler.isRecursive && !node.HasChildNodes
                    if (tagHandler.IsStillStarAtomically)
                    {
                        string debug = node.OuterXml;
                        // atomic tag?!
                        var TemplateNodeInnerText0 = tagHandler.GetStarContent();
                        node.InnerXml = TemplateNodeInnerText0;
                        resultNodeInnerXML = tagHandler.Transform();
                        TRACE(" -- Result1b-IsStillStarAtomically {0} : '{1}' {2}", tagName, resultNodeInnerXML,
                              tagHandler);

                    }
                    else
                    {
                        resultNodeInnerXML = tagHandler.Transform();
                        TRACE(" -- Result1b-NoChildrenRecursive {0} : '{1}' {2}", tagName, resultNodeInnerXML,
                              tagHandler);
                    }
                }
            }
            else
            {
                resultNodeInnerXML = tagHandler.Transform();
                TRACE(" -- Result1b-NonRecursive {0} : '{1}' {2}", tagName, resultNodeInnerXML,
                      tagHandler);
            }



            bool isVerbatum = tagHandler.isVerbatum;

            //Console.WriteLine(" -- Result1 {0} : {1}", tagName, resultNodeInnerXML);
            if (resultNodeInnerXML == null)
            {
                if (isVerbatum)
                {
                    TRACE(" -- WARN VERBATUM NULL {0} : {1}", tagName, tagHandler);
                    return null;
                }
                return string.Empty;
            }
            if (resultNodeInnerXML.Trim() == "")
            {
                if (isVerbatum)
                {
                    TRACE(" -- WARN returning Empty {0} : '{1}' {2}", tagName, resultNodeInnerXML, tagHandler);
                    return resultNodeInnerXML;
                }
                return string.Empty;
            }
            if (isVerbatum)
            {
                return resultNodeInnerXML;
            }
            if (!StaticXMLUtils.ContainsXml(resultNodeInnerXML))
            {
                return resultNodeInnerXML;
            }
            string recursiveResult = GetOutputSentence(resultNodeInnerXML, null, query, request, result, user,
                                                       allowProcess);
            TRACE(" -- Result2 {0} : {1}", recursiveResult, tagHandler);
            return recursiveResult;
        }

        private void CheckRequestResultQuery(Request request, Result result, SubQuery query)
        {
            if (Changes(request._result, result))
            {
                request._result = result;
            }
            if (Changes(result.request, request))
            {
                result.request = request;
            }
            if (Changes(request._CurrentQuery, query))
            {
                request._CurrentQuery = query;
            }
            if (Changes(result._CurrentQuery, query))
            {
                result._CurrentQuery = query;
            }
            if (Changes(query.Request, request))
            {
                query.Request = request;
            }
        }

        private bool Changes(object befor, object after)
        {
            if (befor == null) return true;
            if (befor == after) return false;
            return true;
        }


        bool MustSame(string s1, string s2)
        {
            if (s1 == s2) return true;
            return false;
        }

        public string GetOutputSentence(string template, XmlNode resultNode000, SubQuery query, Request request, Result result, User user, bool allowProcess)
        {

            if (template == null)
            {
                template = resultNode000.OuterXml;
            }
            if (!StaticXMLUtils.ContainsXml(template))
            {
                return template;
            }
            XmlNode resultNode = resultNode000 ?? AIMLTagHandler.getNode("<li>" + template + "</li>");
            if (ChatOptions.AIML_TEMPLATE_CALLS_IMMEDIATE)
            {
                AIMLTagHandler th = null;
                if (ChatOptions.AIML_TEMPLATE_REEVAL)
                {
                    th = new AltAIMLbot.AIMLTagHandlers.format(this, user, query, request, result, resultNode,
                                                               (Func<string, string>)null, null);
                }
                else
                {
                    th = new AltAIMLbot.AIMLTagHandlers.template(this, user, query, request, result, resultNode);
                }
                return th.Transform();
            }
            while (true)
            {
                if (resultNode.HasChildNodes)
                {
                    if (resultNode.ChildNodes.Count == 1)
                    {
                        var cn = resultNode.FirstChild;
                        string cnName = resultNode.Name;
                        if (cnName == "template" || cnName == "li")
                        {
                            //resultNode = cn;
                            //continue;
                        }
                        cnName = cn.Name;
                        if (cnName == "template" || cnName == "li")
                        {
                            resultNode = cn;
                            continue;
                        }
                    }
                }
                break;
            }
            string lrn = resultNode.LocalName;
            string resultNodeString = resultNode.OuterXml;
            if (lrn.IsOneOf("li", "template") < 0)
            {
                if (!resultNode.HasChildNodes && resultNode.NodeType != XmlNodeType.Element)
                {
                    return resultNodeString;
                }
                if (resultNode000 != null)
                {
                    resultNode = resultNode000;
                }
                // writeToLogWarn("not sure what kind of 0utput this is " + resultNode);
            }
            if (!resultNode.HasChildNodes && resultNode.NodeType != XmlNodeType.Element)
            {
                //  Console.WriteLine(" -- GetOutputSentence R2 ({0}) :---> '{1}'", template, resultNode.InnerXml);
                return StaticAIMLUtils.InnerXmlText(resultNode);
            }
            StringBuilder recursiveResult = new StringBuilder();
            // recursively check
            bool needSpace = false;
            string sep = "";
            string lastResult = "";
            int childs = 0;
            foreach (XmlNode childNode in resultNode.ChildNodes)
            {
                string oneChildString;
                oneChildString = this.processNode(childNode, query, request, result, user, allowProcess);
                if (string.IsNullOrEmpty(oneChildString))
                {
                    continue;
                }
                if (needSpace)
                {
                    if (!lastResult.EndsWith(sep)) recursiveResult.Append(sep);
                }
                else
                {
                    needSpace = true;
                }
                lastResult = oneChildString;
                childs++;
                recursiveResult.Append(oneChildString);
            }
            string resultString;
            if (childs == 1)
            {
                resultString = lastResult;
            }
            else
            {
                resultString = recursiveResult.ToString();
            }
            //if (resultString.Length > 4) resultString = resultString.Replace(" , ", " ");
            //if (resultString.Length > 0) resultString = resultString.Replace("\n", " ");
            //if (resultString.Length > 0) resultString = resultString.Replace("\r", " ");
            // Console.WriteLine(" -- GetOutputSentence R1 ({0}) :---> '{1}'", template, resultString);
            return resultString;
        }

        public AIMLTagHandler GetTagHandler(XmlNode node, SubQuery query, Request request, AltAIMLbot.Result result, AltAIMLbot.User user, bool liText)
        {
            AIMLTagHandler tagHandler = this.getBespokeTags(user, query, request, result, node);
            string nodeNameLower = node.Name.ToLower();
            if (object.Equals(null, tagHandler))
            {
                //Console.WriteLine("  -- Process :" + tagName);
                { switch (nodeNameLower)
                {
                    case "bot":
                        return new AltAIMLbot.AIMLTagHandlers.bot(this, user, query, request, result, node);
                        
                    case "condition":
                        return new AltAIMLbot.AIMLTagHandlers.condition_aima(this, user, query, request, result, node);
                        
                    case "date":
                        return new AltAIMLbot.AIMLTagHandlers.date(this, user, query, request, result, node);
                        
                    case "formal":
                        return new AltAIMLbot.AIMLTagHandlers.formal(this, user, query, request, result, node);
                        
                    case "gender":
                        return new substitute(this, user, query, request, result, node);
                        
                    case "get":
                        return new AltAIMLbot.AIMLTagHandlers.get(this, user, query, request, result, node);
                        
                    case "gossip":
                        return new AltAIMLbot.AIMLTagHandlers.gossip(this, user, query, request, result, node);
                        
                    case "id":
                        return new AltAIMLbot.AIMLTagHandlers.id(this, user, query, request, result, node);
                        
                    case "input":
                        return new AltAIMLbot.AIMLTagHandlers.input(this, user, query, request, result, node);
                        
                    case "javascript":
                        return new AltAIMLbot.AIMLTagHandlers.javascript(this, user, query, request, result, node);
                        
                    case "learn":
                        return new AltAIMLbot.AIMLTagHandlers.learn(this, user, query, request, result, node);
                        
                    case "lowercase":
                        return new AltAIMLbot.AIMLTagHandlers.lowercase(this, user, query, request, result, node);
                        
                    case "person":
                        return new substitute(this, user, query, request, result, node);
                        
                    case "person2":
                        return new substitute(this, user, query, request, result, node);
                        
                    case "random":
                        return new AltAIMLbot.AIMLTagHandlers.random(this, user, query, request, result, node);
                        
                    case "sentence":
                        return new AltAIMLbot.AIMLTagHandlers.sentence(this, user, query, request, result, node);
                        
                    case "set":
                        return new AltAIMLbot.AIMLTagHandlers.set(this, user, query, request, result, node);
                        
                    case "size":
                        return new AltAIMLbot.AIMLTagHandlers.size(this, user, query, request, result, node);
                        
                    case "sr":
                        return new AltAIMLbot.AIMLTagHandlers.sr(this, user, query, request, result, node);
                        
                    case "srai":
                        //return new AltAIMLbot.AIMLTagHandlers.srai_odd(this, user, query, request, result, node);
                        return new AltAIMLbot.AIMLTagHandlers.srai(this, user, query, request, result, node);
                        
                    case "star":
                        return new AltAIMLbot.AIMLTagHandlers.star(this, user, query, request, result, node);
                        
                    case "system":
                        return new system(this, user, query, request, result, node);
                        
                    case "that":
                        return new AltAIMLbot.AIMLTagHandlers.that(this, user, query, request, result, node);
                        
                    case "thatstar":
                        return new AltAIMLbot.AIMLTagHandlers.thatstar(this, user, query, request, result, node);
                        
                    case "think":
                        return new AltAIMLbot.AIMLTagHandlers.think(this, user, query, request, result, node);
                        
                    case "topicstar":
                        return new AltAIMLbot.AIMLTagHandlers.topicstar(this, user, query, request, result, node);
                        
                    case "uppercase":
                        return new AltAIMLbot.AIMLTagHandlers.uppercase(this, user, query, request, result, node);
                        
                    case "version":
                    case "name":
                        return new AltAIMLbot.AIMLTagHandlers.botsetting(this, user, query, request, result, node, nodeNameLower);
                        


                    case "push":
                        return new AltAIMLbot.AIMLTagHandlers.push(this, user, query, request, result, node);
                        
                    case "pop":
                        return new AltAIMLbot.AIMLTagHandlers.pop(this, user, query, request, result, node);
                        
                    case "postcache":
                        return new AltAIMLbot.AIMLTagHandlers.postcache(this, user, query, request, result, node);
                        
                    case "refserver":
                        return new AltAIMLbot.AIMLTagHandlers.refserver(this, user, query, request, result, node);

                    case "logictext":
                        return new AltAIMLbot.AIMLTagHandlers.logictext(this, user, query, request, result, node);
                    case "ketext":
                        return new AltAIMLbot.AIMLTagHandlers.ketext(this, user, query, request, result, node);
                    
                    case "prolist":
                        return new AltAIMLbot.AIMLTagHandlers.prolist(this, user, query, request, result, node);

                    case "pannouserver":
                        return new AltAIMLbot.AIMLTagHandlers.pannouserver(this, user, query, request, result, node);
                        
                    case "trueknowledgeserver":
                        return new AltAIMLbot.AIMLTagHandlers.trueknowledgeserver(this, user, query, request, result, node);
                        
                    case "wolframserver":
                        return new AltAIMLbot.AIMLTagHandlers.wolframserver(this, user, query, request, result, node);
                        
                    case "filterqa":
                        return new AltAIMLbot.AIMLTagHandlers.filterqa(this, user, query, request, result, node);
                        
                    case "summerize":
                        return new AltAIMLbot.AIMLTagHandlers.summerize(this, user, query, request, result, node);
                        

                    case "inject":
                        return new AltAIMLbot.AIMLTagHandlers.inject(this, user, query, request, result, node);
                        
                    case "chemsys":
                        return new AltAIMLbot.AIMLTagHandlers.chemsys(this, user, query, request, result, node);
                        
                    case "nop":
                        return new AltAIMLbot.AIMLTagHandlers.nop(this, user, query, request, result, node);
                    
                    case "peekinput":
                        return new AltAIMLbot.AIMLTagHandlers.peekinput(this, user, query, request, result, node);
                        
                    case "scxml":
                        return new AltAIMLbot.AIMLTagHandlers.scxml(this, user, query, request, result, node);
                        
                    case "btxml":
                        return new AltAIMLbot.AIMLTagHandlers.btxml(this, user, query, request, result, node);
                        
                    case "say":
                        return new AltAIMLbot.AIMLTagHandlers.say(this, user, query, request, result, node);
                        
                    case "sapi":
                        return new AltAIMLbot.AIMLTagHandlers.sapi(this, user, query, request, result, node);
                        
                    case "satisfied":
                        return new AltAIMLbot.AIMLTagHandlers.satisfied(this, user, query, request, result, node);
                        
                    case "behavior":
                        return new AltAIMLbot.AIMLTagHandlers.behavior(this, user, query, request, result, node);
                        
                    case "rbehavior":
                        return new AltAIMLbot.AIMLTagHandlers.rbehavior(this, user, query, request, result, node);
                        
                    case "rndint":
                        return new AltAIMLbot.AIMLTagHandlers.rndint(this, user, query, request, result, node);
                        
                    case "rnddbl":
                        return new AltAIMLbot.AIMLTagHandlers.rnddbl(this, user, query, request, result, node);
                        
                    case "crontag":
                        return new AltAIMLbot.AIMLTagHandlers.crontag(this, user, query, request, result, node);
                        
                    case "subaiml":
                        return new AltAIMLbot.AIMLTagHandlers.subaiml(this, user, query, request, result, node);
                        
                    case "template":
                    case "li":
                        return new AltAIMLbot.AIMLTagHandlers.format(this, user, query, request, result, node,
                                                                     (Func<string, string>) null, null);

                    case "#whitespace":
                    case "#text":
                        if (!liText) return null;
                        return new verbatum(node.Value, this, user, query, request, result, node);
                        
                    case "#comment_unused":
                        return new verbatum(node.OuterXml, this, user, query, request, result, node);
                    case "br":
                        return new verbatum("<br/>", this, user, query, request, result, node);
                    case "pre":
                        return new verbatum(StaticXMLUtils.InnerXmlText(node), this, user, query, request, result, node);
                    case "p":
                    case "rverbatum":
                        return new recursiveVerbatum(node, this, user, query, request, result, node, true);
                    case "verbatum":
                        return new recursiveVerbatum(node, this, user, query, request, result, node, false);
                    case "meta":
                        return new verbatum(node.OuterXml, this, user, query, request, result, node);
                    default:
                        break;
                        
                }}
            }
            if (tagHandler != null) return tagHandler;
            if (StaticXMLUtils.IsHtmlTag(node.Name))
            {
                return new recursiveVerbatum(node, this, user, query, request, result, node, true);
            }
            if (tagHandler == null)
            {
                // "bot", "favorite", "fav" 
                foreach (KeyValuePair<string, string> prefix in new[]
                                                                    {
                                                                        new KeyValuePair<string, string>("get_", "get"),
                                                                        new KeyValuePair<string, string>("set_", "set"),
                                                                        new KeyValuePair<string, string>("bot_", "bot"),
                                                                        new KeyValuePair<string, string>("favorite_", "bot"), 
                                                                        new KeyValuePair<string, string>("favorite", "bot"),
                                                                        new KeyValuePair<string, string>("fav_", "bot"),
                                                                        new KeyValuePair<string, string>("fav", "bot"),
                                                                                                                       
                                                                        new KeyValuePair<string, string>("get", "get"),
                                                                        new KeyValuePair<string, string>("set", "set"),
                                                                        new KeyValuePair<string, string>("bot", "bot"),
                                                                    })
                {
                    if (nodeNameLower.StartsWith(prefix.Key) && node.Name.Length > prefix.Key.Length)
                    {
                        string name = node.Name.Substring(prefix.Key.Length);
                        XmlNode pn = node.ParentNode;
                        LineInfoElementImpl newnode = StaticXMLUtils.CopyNode(prefix.Value, node, false);
                        XmlAttributeLineInfo atr = (XmlAttributeLineInfo)newnode.OwnerDocument.CreateAttribute("name");
                        atr.ReadOnly = false;
                        atr.Value = name;
                        newnode.Attributes.Append(atr);
                        if (node.Name.ToLower() != newnode.Name.ToLower())
                        {
                            writeToLog("AIMLLOADER: converted " + node.OuterXml + " -> " + newnode.OuterXml);
                            return GetTagHandler(newnode, query, request, result, user, liText);
                        }
                        writeToLog("AIMLLOADER: ! convert " + node.OuterXml + " -> " + newnode.OuterXml);
                    }
                }
            }
            if (tagHandler != null) return tagHandler;
            tagHandler = TagHandling.GetTagHandlerU(user, query, request, result, node, true);
            if (tagHandler != null) return tagHandler;
            if (nodeNameLower == "name")
            {
                return new bot(this, user, query, request, result, node);
            }
            if (nodeNameLower == "#comment")
            {
                return null;
            }
            tagHandler = new lazyClosure(this, user, query, request, result, node);
            writeToLog("AIMLLOADER:  lazyClosure?!: " + node.OuterXml);
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
        /// <returns>the output string</returns>
        public AIMLTagHandler getBespokeTags(AltAIMLbot.User user, SubQuery query, Request request, AltAIMLbot.Result result, XmlNode node)
        {
            if (this.CustomTags.ContainsKey(node.Name.ToLower()))
            {
                TagHandler customTagHandler = (TagHandler)this.CustomTags[node.Name.ToLower()];

                AIMLTagHandler newCustomTag = customTagHandler.Instantiate(LateBindingAssemblies, user,
                                                                                       query,
                                                                                       request, result, node, this);
                if(object.Equals(null,newCustomTag))
                {
                    return null;
                }
                else
                {
                    newCustomTag.user = user;
                    newCustomTag.query = query;
                    newCustomTag.request = request;
                    newCustomTag.result = result;
                    newCustomTag.templateNode = node;
                    newCustomTag.bot = this;
                    return newCustomTag;
                }
            }
            else
            {
                return null;
            }
        }
        
        #region Serialization

        /// <summary>
        /// Saves the graphmaster node (and children) to a binary file to avoid processing the AIML each time the 
        /// bot starts
        /// </summary>
        /// <param name="path">the path to the file for saving</param>
        public void saveToBinaryFile(string path)
        {
            saveToBinaryFile0(path + ".btxbin");
            saveToBinaryFile1(path);
        }

        public void saveToBinaryFile0(string path)
        {
            if (noSerialzation) return;            
            // check to delete an existing version of the file
            FileInfo fi = new FileInfo(path);
            Directory.CreateDirectory(fi.DirectoryName);
            if (fi.Exists)
            {
                fi.Delete();
            }
            FileStream saveFile = File.Create(path);
            try
            {
                BinaryFormatter bf = new BinaryFormatter();
                //this.myBehaviors.preSerial();
                //bf.Serialize(saveFile, this.Graphmaster);
                bf.Serialize(saveFile, this.myCron);
                bf.Serialize(saveFile, this.myRandMem);
                bf.Serialize(saveFile, this.Graphs);
                // bf.Serialize(saveFile, this.myBehaviors);
            }
            catch (Exception e)
            {
                Console.WriteLine("ERR:" + e.Message);
                Console.WriteLine("ERR:" + e.StackTrace);
            }
            saveFile.Close();
        }

        /// <summary>
        /// Loads a dump of the graphmaster into memory so avoiding processing the AIML files again
        /// </summary>
        /// <param name="path">the path to the dump file</param>
        public void loadFromBinaryFile(string path)
        {
            loadFromBinaryFile0(path + ".btxbin");
            loadFromBinaryFile1(path);
        }
        public void loadFromBinaryFile0(string path)
        {
            FileStream loadFile = File.OpenRead(path);
            BinaryFormatter bf = new BinaryFormatter();
            //this.Graphmaster = (GraphMaster)bf.Deserialize(loadFile);
            this.myCron = (Cron)bf.Deserialize(loadFile);
            this.myRandMem = (RandomMemory)bf.Deserialize(loadFile);
            this.LocalGraphsByName = (Dictionary<string, GraphMaster>)bf.Deserialize(loadFile);
            this.Graphmaster._root = this.Graphs["*"].root;
            //this.myBehaviors = (BehaviorSet)bf.Deserialize(loadFile);

            loadFile.Close();
            this.myCron.myBot = this;
            // this.myBehaviors.postSerial(this);

        }

        #endregion

        #region Latebinding custom-tag dll handlers

        /// <summary>
        /// Loads any custom tag handlers found in the dll referenced in the argument
        /// </summary>
        /// <param name="pathToDLL">the path to the dll containing the custom tag handling code</param>
        public void loadCustomTagHandlers(string pathToDLL)
        {
            Assembly tagDLL = Assembly.LoadFrom(pathToDLL);
            Type[] tagDLLTypes = tagDLL.GetTypes();
            for (int i = 0; i < tagDLLTypes.Length; i++)
            {
                object[] typeCustomAttributes = tagDLLTypes[i].GetCustomAttributes(false);
                for (int j = 0; j < typeCustomAttributes.Length; j++)
                {
                    if (typeCustomAttributes[j] is CustomTagAttribute)
                    {
                        // We've found a custom tag handling class
                        // so store the assembly and store it away in the Dictionary<,> as a TagHandler class for 
                        // later usage
                        
                        // store Assembly
                        if (!this.LateBindingAssemblies.ContainsKey(tagDLL.FullName))
                        {
                            this.LateBindingAssemblies.Add(tagDLL.FullName, tagDLL);
                        }

                        // create the TagHandler representation
                        TagHandler newTagHandler = new TagHandler();
                        newTagHandler.AssemblyName = tagDLL.FullName;
                        newTagHandler.ClassName = tagDLLTypes[i].FullName;
                        newTagHandler.TagName = tagDLLTypes[i].Name.ToLower();
                        if (this.CustomTags.ContainsKey(newTagHandler.TagName))
                        {
                            throw new Exception("ERROR! Unable to add the custom tag: <" + newTagHandler.TagName + ">, found in: " + pathToDLL + " as a handler for this tag already exists.");
                        }
                        else
                        {
                            this.CustomTags.Add(newTagHandler.TagName, newTagHandler);
                        }
                    }
                }
            }
        }
        #endregion

        #region Phone Home
        /// <summary>
        /// Attempts to send an email to the botmaster at the AdminEmail address setting with error messages
        /// resulting from a query to the bot
        /// </summary>
        /// <param name="errorMessage">the resulting error message</param>
        /// <param name="request">the request object that encapsulates all sorts of useful information</param>
        public void phoneHome(string errorMessage, Request request)
        {
            MailMessage msg = new MailMessage("donotreply@AltAIMLbot.com",this.AdminEmail);
            msg.Subject = "WARNING! AltAIMLbot has encountered a problem...";
            string message = @"Dear Botmaster,

This is an automatically generated email to report errors with your bot.

At *TIME* the bot encountered the following error:

""*MESSAGE*""

whilst processing the following input:

""*RAWINPUT*""

from the user with an id of: *USER*

The normalized paths generated by the raw input were as follows:

*PATHS*

Please check your AIML!

Regards,

The AltAIMLbot program.
";
            message = message.Replace("*TIME*", DateTime.Now.ToString());
            message = message.Replace("*MESSAGE*", errorMessage);
            message = message.Replace("*RAWINPUT*", request.rawInput);
            message = message.Replace("*USER*", request.user.UserID);
            StringBuilder paths = new StringBuilder();
            foreach(string path in request.result.GraphMasterPaths)
            {
                paths.Append(path + Environment.NewLine);
            }
            message = message.Replace("*PATHS*", paths.ToString());
            msg.Body = message;
            msg.IsBodyHtml=false;
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

        #region BlackBoard

        public void importBBBotSettings(string bbKey,string settingKey)
        {
            if (myChemistry == null) return;
            string myValue = myChemistry.m_cBus.getHash(bbKey);
            if (myValue.Length > 0)
            {
                GlobalSettings.updateSetting(settingKey, myValue);
            }
        }
        public void exportBBBotSettings(string bbKey, string settingKey)
        {
            var val = GlobalSettings.grabSetting(settingKey, false);
            setBBHash0(bbKey, val, this.BotBehaving);
        }
        public void exportBBUserSettings(User myUser, string bbKey, string settingKey)
        {
            var val = myUser.Predicates.grabSetting(settingKey, false);
            setBBHash0(bbKey, val, this.BotBehaving);
        }
        public void importBBUserSettings(User myUser, string bbKey, string settingKey)
        {
            if (myChemistry == null) return; 
            string myValue = myChemistry.m_cBus.getHash(bbKey);
            if (myValue.Length > 0)
            {
                myUser.Predicates.updateSetting(settingKey, myValue);
            }
        }

        public void WithBBUser(User myUser, Action<User, string, string> importBBUserSettings)
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
        public void WithBBBot(Action<string, string> importBBBotSettings)
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
        internal bool bbSafe = false;
        internal bool useMemcache = false;

        public Dictionary<string, string> BBDict = new Dictionary<string, string>();
        internal void setBBHash(string key, string data, BehaviorContext bu)
        {
            if (key == null) return;
            string okey = key;
            try
            {
                //(SettingsDictionaryReal)
                if (key.StartsWith("bot"))
                {
                    key = key.Substring(3);
                    if (_botAsUser != null) _botAsUser.Predicates.addSetting("bb_" + key, data);
                }
                else if (key.StartsWith("user"))
                {
                    key = key.Substring(4);
                    var lu = LastUser;
                    User buu = null;
                    if (bu != null)
                    {
                        buu = bu._user;
                        if (buu != null)
                            if (buu == lu)
                            {
                                buu.Predicates.addSetting("bb_" + key, data);
                                return;
                            }
                    }
                    Console.WriteLine("CHECK setBBHash Voided({0},{1}) ('{2}'!='{3}')", key, data, buu, lu);
                }
                else
                {
                    if (GlobalSettings != null) GlobalSettings.addSetting("bb_" + key, data);
                }
            }
            catch (Exception e)
            {
                Console.WriteLine("CHECK setBBHash({0},{1}) ERR:{2}", key, data, e);
            }
            finally
            {
                setBBHash0(okey, data, bu);
            }
        }
        internal void setBBHash0(string key, string data, BehaviorContext bu)
        {
            //curBot.myChemistry.m_cBus.setHash(key,data);
            if (bu == null || bu._user == null)
            {
                if (key.StartsWith("user")) return;
                if (!key.StartsWith("bot"))
                {
                    return;
                }
            }
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
        internal string getBBHash(string key, BehaviorContext bu)
        {
            if (string.IsNullOrEmpty(key)) return null;

            string gs = getBBHash0(key, bu);
            if (!string.IsNullOrEmpty(gs)) return gs;
            if (GlobalSettings != null)
            {
                gs = GlobalSettings.grabSetting("bb_" + key, false);
                if (gs != null) return gs;
            }
            if (key.StartsWith("bot"))
            {
                key = key.Substring(3);
                gs = _botAsUser.Predicates.grabSetting("bb_" + key, false);
                if (gs != null) return gs;
            }
            else if (key.StartsWith("user"))
            {
                key = key.Substring(4);
                gs = LastUser.Predicates.grabSetting("bb_" + key, true);
                if (!string.IsNullOrEmpty(gs)) return gs;
            }
            return null;
        }

        public bool bbDisabled = false;
        public bool memcachedDisabledBetweenRunsTest = true;
        internal string getBBHash0(string key, BehaviorContext bu)
        {
            try
            {
                string val;
                if (useMemcache && !memcachedDisabledBetweenRunsTest)
                {
                    if ((myChemistry != null) && (myChemistry.m_cBus != null))
                    {
                        val = myChemistry.m_cBus.getHash(key);
                        lock (BBDict) BBDict[key] = val;
                        if (!string.IsNullOrEmpty(val)) return val;
                        return null;
                    }
                }
                lock (BBDict) if (BBDict.TryGetValue(key, out val)) return val;
                return null;
            }
            catch(Exception e)
            {
                writeToLog(e);
                return null;
            }
        }
        #endregion

        #region FSM

        public void advanceFSM()
        {
            myFSMS.runBotMachines(this);
        }
        public void defineFSM(string name,string FSMXML)
        {
            myFSMS.defineMachine(name,FSMXML);
        }
        #endregion

        #region behavior
        public void performBehaviors()
        {
            myBehaviors.runBotBehaviors(this);
        }
        public void defineBehavior(string name, string BehaveXML)
        {
            myBehaviors.defineBehavior(name, BehaveXML);
        }
        #endregion

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
        public static bool UnknownTagsAreBotVars = false;


        public GraphMaster GetGraph(string graphName)
        {
            return GetGraph(graphName, Graphmaster);
        }

        public ISettingsDictionary GetDictionary(string name)
        {
            var idict = GetDictionary0(name, false);
            if (idict != null) return idict;
            var AltBotobjCol = ScriptManager.ResolveToObject(this, name);
            if (AltBotobjCol == null || AltBotobjCol.Count == 0)
            {
                return null;
            }
            //if (tr)
            foreach (object o in AltBotobjCol)
            {
                ParentProvider pp = o as ParentProvider;
                ISettingsDictionary pi = o as ISettingsDictionary;
                User pu = o as User;
                if (pp != null)
                {
                    pi = (ISettingsDictionary) pp();
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

        public ISettingsDictionary GetDictionary0(string named, bool createUser)
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
                return null;
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
                                                          true, true, null);
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
                            ISettingsDictionary pi = (ISettingsDictionary) pp();
                            if (pi != null) return SDCAST(pi);
                        }
                    }
                }
            }
            return null;
        }

        public ISettingsDictionary GetDictionary(string named, string type, bool createIfMissing, bool isSubsts, Request request)
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
                        dict = AllDictionaries[key] = AllDictionaries[named] =
                                                      (isSubsts
                                                           ? MakeSubstsDictionary(key)
                                                           : MakeSettingsDictionary(named));
                        User user = ExemplarUser ?? BotAsUser;

                        Request r = GetBotRequest("loadDictionary '" + named + "' from '" + type + "' ");
                        r = r ??
                            user.CreateRequest("@echo <!-- loadDictionary '" + named + "' from '" + type + "' -->",
                                               BotAsUser, Unifiable.EnglishNothing, null, request, false,
                                               RequestKind.AIMLLoader);
                        loadDictionary(dict, named, type, r);
                    }
                }
                return dict;
            }
        }

        public Dictionary<string, ISettingsDictionary> AllDictionaries = new Dictionary<string, ISettingsDictionary>();
        public bool noSerialzation = true;
        public string ToValueString(object vv)
        {
            var v = ToValue(vv);
            if (vv == null) return null;
            return "" + v;
        }

        public object ToValue(object v)
        {
            return v;
        }

        public void RegisterObject(string named, object obj)
        {
               prologEngine.RegisterObject(named, obj);
        }

        public bool LoadFromStreamLoader { get; set; }

        public void RemoveCurrentTask(TaskItem taskItem, TaskList taskList, bool b)
        {
            if (HaveBotBehaving)
            {
                BotBehaving.RemoveCurrentTask(taskItem, taskList, b);
            }
        }

        protected bool HaveBotBehaving
        {
            get { return _behaviorContext != null; }
            set { throw new NotImplementedException(); }
        }

        public void AddCurrentTask(TaskItem taskItem, TaskList taskList, bool b)
        {
            if (HaveBotBehaving)
            {
                BotBehaving.AddCurrentTask(taskItem, taskList, b);
            }
        }

        public void SetCurrentTask(TaskItem taskItem, TaskList taskList, bool b)
        {
            if (HaveBotBehaving)
            {
                BotBehaving.SetCurrentTask(taskItem, taskList, b);
            }
        }
    }
}

namespace AltAIMLbot
{
    /// <summary>
    /// Encapsulates a bot. If no settings.xml file is found or referenced the bot will try to
    /// default to safe settings.
    /// </summary>
    /// 
        public delegate void sayProcessorDelegate(string message);
        public delegate void systemProcessorDelegate(string message);
        public delegate void systemPersonaDelegate(string message);

    public class myConst
    {
        public static string MEMHOST = "127.0.0.1";
        //public static string MEMHOST = "192.168.2.141";
    }
    /*public class UserDuringProcessing : User
    {
        public UserDuringProcessing(string UserID, AltBot bot)
            : base(UserID,bot)
        {
        }
    }
    public class User : MasterUser
    {
        public User(string UserID, AltBot bot)
            : base(UserID,bot)
        {
        }
    }*/

   // public class Unifiable 
   // {
    //
    //}
    /*
    public class Utterance
    {
        public int maxResults;
        public bool IsSpeakerInputGleaned = false;
        public string OrignalRawText;
        /// <summary>
        /// The last Utterance containing That
        /// </summary>
        public Utterance InResponse
        {
            get
            {
                if (_inResponse != null) return _inResponse;
                throw new NotImplementedException();
            }
            set { _inResponse = value; }
        }

        private Utterance _inResponse;

        /// <summary>
        /// The user that made this Utterance
        /// </summary>
        public User Speaker { get; set; }
        /// <summary>
        /// The user responding to the request
        /// </summary>
        public User ToWhom { get; set; }

        private readonly Func<string, string> OutputSentencesToEnglish;

        public Utterance(Func<string, string> generatePhrase, User speaker, User toWhom, string rawText, int maxSentences)
        {
            Speaker = speaker;
            ToWhom = toWhom;
            OrignalRawText = rawText;
            OutputSentencesToEnglish = generatePhrase;
            maxResults = maxSentences + 10;
        }

    }*/
}
