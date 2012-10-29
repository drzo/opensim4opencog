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
using AltAIMLbot;
using AltAIMLbot.Utils;
using AltAIMLParser;
using DcBus;
using Aima.Core.Logic.Propositional.Algorithms;
using Aima.Core.Logic.Propositional.Parsing;
using Aima.Core.Logic.Propositional.Parsing.AST;
using LAIR.ResourceAPIs.WordNet;
using MushDLR223.ScriptEngines;
using MushDLR223.Utilities;
using MushDLR223.Virtualization;
using RTParser;
using RTParser.AIMLTagHandlers;
using RTParser.Normalize;
using RTParser.Utils;
using RTParser.Variables;
using AIMLLoader=AltAIMLbot.Utils.AIMLLoader;
using AIMLTagHandler=AltAIMLbot.Utils.AIMLTagHandler;
using bot=AltAIMLbot.AIMLTagHandlers.bot;
using CustomTagAttribute=AltAIMLbot.Utils.CustomTagAttribute;
using Gender=AltAIMLbot.Utils.Gender;
using MatchState=AltAIMLbot.Utils.MatchState;
using Node=AltAIMLbot.Utils.Node;
using recursiveVerbatum=AltAIMLbot.AIMLTagHandlers.recursiveVerbatum;
using TagHandler=AltAIMLbot.Utils.TagHandler;
using verbatum=AltAIMLbot.AIMLTagHandlers.verbatum;

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

namespace RTParser
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
        public RChem myChemistry = null;
        public Qchem realChem = null;
        public QfsmSet myFSMS = new QfsmSet();
        public BehaviorSet myBehaviors = new BehaviorSet();
        public Cron myCron = null;
        public bool inCritical = false;
        public bool blockCron = false;
        public RandomMemory myRandMem = new RandomMemory();

        static private WordNetEngine _wordNetEngine;
        public WordNetEngine wordNetEngine
        {
            get { return _wordNetEngine; }
            set { _wordNetEngine = _wordNetEngine ?? value; }
        }

        [NonSerialized ]
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

        public Servitor myServitor = null;

        /// <summary>
        /// A dictionary object that looks after all the settings associated with this bot
        /// </summary>
        public SettingsDictionary GlobalSettings;

        public SettingsDictionary AllUserPreds
        {
            get
            {
                return DefaultPredicates;
            }
        }

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

        public void RegisterDictionary(string named, ISettingsDictionary dict)
        {
            named = named.ToLower().Trim().Replace("  ", " ");
            string key = named.Replace(" ", "_");
            RegisterDictionary(named, dict, true);
        }

        public void RegisterDictionary(string key, ISettingsDictionary dict, bool always)
        {
            SettingsDictionary.AddPseudonym(dict, key);
            Action needsExit = LockInfo.MonitorTryEnter("RegisterDictionary " + key, AllDictionaries, MaxWaitTryEnter);
            try
            {
                var path = key.Split(new[] { '.' });
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
        static public List<string> Splitters = new List<string>();

        /// <summary>
        /// A buffer to hold log messages to be written out to the log file when a max size is reached
        /// </summary>
        private List<string> LogBuffer = new List<string>();

        private string _rapStoreDirectory;
        public string rapStoreDirectory
        {
            get
            {
                return _rapStoreDirectory;
            }
            set
            {
                if (Environment.MachineName == "ENKI")
                {
                   // return;
                }
                _rapStoreDirectory = value;
            }
        }
        public int rapStoreSlices = 0;
        public int rapStoreTrunkLevel = 0;

        /// <summary>
        /// How big to let the log buffer get before writing to disk
        /// </summary>
        private int MaxLogBufferSize
        {
            get
            {
                if (GlobalSettings == null) return 1000;
                return Convert.ToInt32(this.GlobalSettings.grabSetting("maxlogbuffersize"));
            }
        }

        /// <summary>
        /// Flag to show if the bot is willing to accept user input
        /// </summary>
        public bool isAcceptingUserInput = true;

        /// <summary>
        /// Flag to show if the bot is producing output
        /// </summary>
        public bool isPerformingOutput = true;

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

        /// <summary>
        /// The message to show if a user tries to use the bot whilst it is set to not process user input
        /// </summary>
        private string NotAcceptingUserInputMessage
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
                return 70000;
                if (GlobalSettings == null || !GlobalSettings.containsSettingCalled("timeout"))
                {
                    return 2000000;
                }
                String s = GlobalSettings.grabSettingNoDebug("timeout").ToValue(null);
                return Convert.ToDouble(s);
            }
        }

        /// <summary>
        /// The message to display in the event of a timeout
        /// </summary>
        public string TimeOutMessage
        {
            get
            {
                return this.GlobalSettings.grabSetting("timeoutmessage");
            }
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
                return new Regex(this.GlobalSettings.grabSetting("stripperregex"),RegexOptions.IgnorePatternWhitespace);
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
                        result=Gender.Unknown;
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
        public GraphMaster Graphmaster;

        /// <summary>
        /// The named "brains" of the bot
        /// default graphmaster should be listed under "*"
        /// </summary>
        public Dictionary<string, GraphMaster> Graphs;

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

        protected string PersonalAiml
        {
            get { return _PathToBotPersonalFiles; }
            set
            {
                lock (_RuntimeDirectories)
                {
                    if (_PathToUserFiles != null) _RuntimeDirectories.Remove(_PathToUserFiles);
                    _PathToUserFiles = value;
                    _RuntimeDirectories.Remove(value);
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
        /// A general stack to remember things to mention later
        /// </summary>
        public Stack<string> conversationStack = new Stack<string>();

        /// <summary>
        /// in the <say> tag should the sapi be passed as-is (using innerXML) or not (using innerText)
        /// usually set when the sayProcessor delegate is set
        /// </summary>
        public bool saySapi = false;


        public ExternDB chatDB = null;

        #endregion

        #region Delegates

        public delegate void LogMessageDelegate();

        public sayProcessorDelegate sayProcessor;

        public string lastBehaviorChatInput;
        public string lastBehaviorChatOutput;
        public AltAIMLbot.User lastBehaviorUser;
        public Queue<string> chatInputQueue = new Queue<string>();

        #endregion

        #region Events

        public event LogMessageDelegate WrittenToLog;

        #endregion

        /// <summary>
        /// Loads AIML from .aiml files into the graphmaster "brain" of the bot
        /// Loads the AIML from files found in the bot's AIMLpath into the bot's brain
        /// </summary>
        public void loadAIMLFromFiles()
        {
            AIMLLoader loader = new AIMLLoader(this);
            loader.loadAIML(PathToAIML);
        }
    
        /// <summary>
        /// Loads AIML from .aiml at dirPath files into the graphmaster "brain" of the bot
        /// </summary>
        public void loadAIMLFromFiles(string dirPath)
        {
            AIMLLoader loader = new AIMLLoader(this);
            loader.loadAIML(dirPath );
        }

        public void loadAIMLFromFile(string filePath)
        {
            AIMLLoader loader = new AIMLLoader(this);
            loader.loadAIMLFile(filePath);
        }

        /// <summary>
        /// Allows the bot to load a new XML version of some AIML
        /// </summary>
        /// <param name="newAIML">The XML document containing the AIML</param>
        /// <param name="filename">The originator of the XML document</param>
        public void loadAIMLFromXML_Unused(XmlDocument newAIML, string filename)
        {
            AIMLLoader loader = new AIMLLoader(this);
            loader.loadAIMLFromXML(newAIML, filename);
        }
        /// <summary>
        /// Allows the bot to load a new XML version of some AIML
        /// </summary>
        /// <param name="newAIML">The XML document containing the AIML</param>
        /// <param name="filename">The originator of the XML document</param>
        public void loadAIMLFromXML(XmlNode newAIML, string filename)
        {
            AIMLLoader loader = new AIMLLoader(this);
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
                { }
            }
        }


        /// <summary>
        /// Instantiates the dictionary objects and collections associated with this class
        /// </summary>
        private void setup()
        {
            this.myCron = new Cron(this);

            SettingsDictionary.WarnOnNull = false;
            this.RelationMetaProps = new SettingsDictionary("chat.relationprops", this);
            SettingsDictionary.WarnOnNull = true;
            this.GlobalSettings = new SettingsDictionary("bot", this);
            this.GenderSubstitutions = new SettingsDictionary("substituions.gender", this);
            this.Person2Substitutions = new SettingsDictionary("substituions.person2", this);
            this.PersonSubstitutions = new SettingsDictionary("substituions.person", this);
            this.InputSubstitutions = new SettingsDictionary("substituions.input", this);
            this.DefaultPredicates = new SettingsDictionary("allusers",this);
            this.HeardPredicates = new SettingsDictionary("chat.heardpredicates", this);
            RegisterDictionary("bot.alluserpred", this.AllUserPreds);
            this.CustomTags = new Dictionary<string, TagHandler>();
            this.Graphs = new Dictionary<string, GraphMaster>();
            this.Graphmaster = new GraphMaster("default");
            this.Graphs.Add("*", this.Graphmaster);
            this.DefaultHeardSelfSayGraph = new GraphMaster("heardselfsay");
            this.setupDictionaries();
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
                this.GlobalSettings.addSetting("notacceptinguserinputmessage", "This bot is currently set to not accept user input.");
            }
            if (!this.GlobalSettings.containsSettingCalled("stripperregex"))
            {
                this.GlobalSettings.addSetting("stripperregex", "[^0-9a-zA-Z]");
            }
            this.DefaultPredicates.bbPrefix = "user";
            this.GlobalSettings.bbPrefix = "bot";

            // Load the dictionaries for this Bot from the various configuration files
            this.Person2Substitutions.loadSettings(Path.Combine(this.PathToConfigFiles, this.GlobalSettings.grabSetting("person2substitutionsfile")));
            this.PersonSubstitutions.loadSettings(Path.Combine(this.PathToConfigFiles, this.GlobalSettings.grabSetting("personsubstitutionsfile")));
            this.GenderSubstitutions.loadSettings(Path.Combine(this.PathToConfigFiles, this.GlobalSettings.grabSetting("gendersubstitutionsfile")));
            this.DefaultPredicates.loadSettings(Path.Combine(this.PathToConfigFiles, this.GlobalSettings.grabSetting("defaultpredicates")));
            this.InputSubstitutions.loadSettings(Path.Combine(this.PathToConfigFiles, this.GlobalSettings.grabSetting("substitutionsfile")));

            // Grab the splitters for this bot
            this.loadSplitters(Path.Combine(this.PathToConfigFiles,this.GlobalSettings.grabSetting("splittersfile")));
        }

        /// <summary>
        /// Loads the splitters for this bot from the supplied config file (or sets up some safe defaults)
        /// </summary>
        /// <param name="pathToSplitters">Path to the config file</param>
        private void loadSplitters(string pathToSplitters)
        {
            if (pathToSplitters == null) return;
            FileInfo splittersFile = new FileInfo(pathToSplitters);
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

        #region Conversation methods

        public void processOutputQueue()
        {
            if (!isPerformingOutput) return;
            while (outputQueue.Count > 0)
            {
                string msg = outputQueue.Dequeue();
                if (sayProcessor != null)
                {
                    sayProcessor(msg);
                }
                else
                {
                    Console.WriteLine("BOT OUTPUT:{0}", msg);
                }
                myBehaviors.logText("BOT OUTPUT:" + msg);

            }
        }

        public void flushOutputQueue()
        {
            outputQueue.Clear();
            myBehaviors.logText("BOT flushOutputQueue:");
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
        }
        /// <summary>
        /// Given some raw input and a unique ID creates a response for a new user
        /// </summary>
        /// <param name="rawInput">the raw input</param>
        /// <param name="UserGUID">an ID for the new user (referenced in the result object)</param>
        /// <returns>the result to be output to the user</returns>
        public AltAIMLbot.Result Chat(string rawInput, string UserGUID)
        {
            Request request = new Request(rawInput, FindOrCreateUser(UserGUID), this);
            return this.Chat(request);
        }

        public AltAIMLbot.Result Chat(Request request)
        {
            return Chat(request, "*");
        }
        /// <summary>
        /// Given a request containing user input, produces a result from the bot.
        /// Sensitive to request.user.Qstate
        /// </summary>
        /// <param name="request">the request from the user</param>
        /// <returns>the result to be output to the user</returns>
        public Result Chat(Request request, string graphID)
        {
            GraphMaster ourGraphMaster = this.GetGraph(graphID) ?? Graphmaster;
            request.CurrentGraph = ourGraphMaster;

            User user = request.user;
            var result = new MasterResult(user, this, request);
            bool saveResult = false;

            lock (ExternDB.mylock)
            {
                if (this.isAcceptingUserInput)
                {
                    // Mark the input time
                    myBehaviors.keepTime("lastchatinput", RunStatus.Success);
                    myBehaviors.activationTime("lastchatinput", RunStatus.Success);

                    // Normalize the input
                    AIMLLoader loader = new AIMLLoader(this);
                    SplitIntoSentences splitter = new SplitIntoSentences(null, request.rawInput);
                    string[] rawSentences = splitter.Transform();


                    if (rapStoreDirectory != null)
                    {
                        if (chatDB == null)
                        {
                            chatDB = new ExternDB(rapStoreDirectory);
                            chatDB.bot = this;
                            chatDB._dbdir = rapStoreDirectory;
                            if (rapStoreSlices > 0) chatDB.slices = rapStoreSlices;
                            if (rapStoreSlices > 0) chatDB.trunkLevel = rapStoreTrunkLevel;
                            chatDB.OpenAll();
                        }
                    }
                    if (user.Qstate.Count == 0)
                    {
                        result.NormalizedPaths.Clear();
                        Console.WriteLine("DEBUG:Using Normal Search");
                        // Standard operation
                        foreach (string sentence in rawSentences)
                        {
                            result.InputSentences.Add(sentence);
                            List<string> paths = gatherPaths(user, sentence, user.getPreStates(), user.getPostStates(),
                                                             loader);
                            SortPaths(paths, ourGraphMaster.getPathScore);
                            foreach (var path in paths)
                            {
                                result.NormalizedPaths.Add(path);                                
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
                                List<string> paths = gatherPaths(user, sentence, states, states, loader);                                
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
                            if (!result.NormalizedPaths.Contains(bestpath))
                            {
                                result.NormalizedPaths.Add(bestpath);
                            }
                            Console.WriteLine("DEBUG: path = " + bestpath);
                        }


                    }

                    // grab the templates for the various sentences from the graphmaster
                    foreach (string path in result.NormalizedPaths)
                    {
                        AltAIMLbot.Utils.SubQuery query = new SubQuery(path, result, request);
                        if (chatDB == null)
                        {
                            query.Template = ourGraphMaster.evaluate(path, query, request, MatchState.Pattern, new StringBuilder());
                        }
                        else
                        {
                            Node dbGraphMaster = chatDB.fetchNode("", true);
                            query.Template = dbGraphMaster.evaluateDB(path, query, request, MatchState.Pattern, new StringBuilder(), "", chatDB);
                        }
                        //query.Template = this.Graphmaster.evaluate(path, query, request, MatchState.UserInput, new StringBuilder());
                        //query.Template = ourGraphMaster.evaluate(path, query, request, MatchState.UserInput, new StringBuilder());
                        Console.WriteLine("DEBUG: TemplatePath = " + query.TemplatePath);
                        Console.WriteLine("DEBUG: Template = " + query.Template);
                        myBehaviors.SkipLog = true;
                        myBehaviors.logText("CHAT QueryPath:" + path);
                        myBehaviors.logText("CHAT TemplatePath:" + query.TemplatePath);
                        myBehaviors.logText("CHAT Template:\n" + query.Template);

                        result.SubQueries.Add(query);
                    }
                    if (rapStoreDirectory != null)
                    {
                        if (chatDB != null)
                        {
                            chatDB.prune(1024);
                            //chatDB.Close();
                            //chatDB = null;
                        }
                    }
                    if (result.SubQueries.Count == 0)
                    {
                        Console.WriteLine("DEBUG: MISSING SubQueries");
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
                                    result.AddOutputSentences(outputSentence);
                                    saveResult = true;
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
                                this.writeToLog("WARNING! A problem was encountered when trying to process the input: " + request.rawInput + " with the template: \"" + query.Template + "\"");
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
                    return result;
                }
                user.addResult(result);
            }
            return result;
        }


        private static void SortPaths(List<string> paths, Func<string, double> func)
        {
            paths.Sort((s1, s2) =>
                           {
                               var diff =  func(s1).CompareTo(func(s2));
                               if (diff != 0) return diff;
                               diff = s1.Length.CompareTo(s2.Length);
                               if (diff != 0) return diff;
                               return s1.CompareTo(s2);
                           });
        }

        private static List<string> gatherPaths(User user, string sentence,IEnumerable<string> usergetPreStates, IEnumerable<string> usergetPostStates, AIMLLoader loader)
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
                            string path = loader.generatePath(sentence, that, topic, prestates, poststates, true);
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
    
        /// <summary>
        /// given an template side XML, try evaluating it
        /// </summary>       
        public void evalTemplateNode(XmlNode templateNode)
        {
            if (StaticXMLUtils.IsBlank(templateNode)) return;
            var imaginaryUser = FindOrCreateUser("evalTemplateNode User");
            Request request = new Request("evalTemplateNode Request", imaginaryUser, this);
            AltAIMLbot.Result result = new MasterResult(request.user, this, request);
            AltAIMLbot.Utils.SubQuery query = new SubQuery("evalTemplateNode SubQuery", result, request);
            
 
            string outputSentence = this.processNode(templateNode, query, request, result, request.user, true);
       
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
        public string processNode(XmlNode node, SubQuery query, Request request, AltAIMLbot.Result result, AltAIMLbot.User user, bool allowProcess)
        {
            // check for timeout (to avoid infinite loops)
            if (request.MayTimeOut && request.StartedOn.AddMilliseconds(request.bot.TimeOut) < DateTime.Now)
            {
                request.bot.writeToLog("WARNING! Request timeout. User: " + request.user.UserID + " raw input: \"" + request.rawInput + "\" processing template: \""+query.Template+"\"");
                request.hasTimedOut = true;
                return string.Empty;
            }
                        
            // process the node
            string tagName = node.Name.ToLower();
            myBehaviors.logNode("AIML", node);

            if (tagName == "template" || tagName == "li")
            {
                string pt = GetOutputSentence(null, node, query, request, result, user, allowProcess);
                return pt;
            }
            else
            {

                AIMLTagHandler tagHandler = GetTagHandler(node, query, request, result, user, false);
                if (object.Equals(null, tagHandler))
                {
                    string debug = node.OuterXml;
                    if (node.NodeType == XmlNodeType.Text) return node.InnerText;
                    if (node.NodeType == XmlNodeType.Whitespace) return node.InnerText;
                    if (node.NodeType == XmlNodeType.Comment) return " , ";
                    Console.WriteLine(" -- Result0 {0} : {1}", tagName, node.InnerText);
                    return node.InnerText;
                }
                else
                {
                    if (tagHandler.isRecursive)
                    {
                        if (node.HasChildNodes)
                        {
                            // recursively check
                            foreach (XmlNode childNode in node.ChildNodes)
                            {
                                if (childNode.NodeType == XmlNodeType.Element)
                                {
                                    try
                                    {
                                        string value = this.processNode(childNode, query, request, result, user, allowProcess);
                                        if (value == null)
                                        {
                                            value = this.processNode(childNode, query, request, result, user, allowProcess);
                                            value = String.Empty;
                                        }
                                        string debug = childNode.OuterXml;
                                        if (!allowProcess)
                                        {
                                            continue;
                                        }
                                        if (value.Contains("<"))
                                        {
                                            childNode.InnerXml = value;
                                        }
                                        else
                                        {
                                            childNode.InnerXml = value;
                                        }
                                    }
                                    catch (Exception e)
                                    {
                                        Console.WriteLine("AIML processNode ERR {0}: {1}", tagName, e.Message);
                                        Console.WriteLine("AIML processNode OuterXML: {0}", node.OuterXml);
                                        childNode.InnerXml = childNode.InnerText;
                                    }
                                }
                            }
                            // we should have filled in the inner text by now
                            string resultNodeInnerXML = tagHandler.Transform();

                            //string resultNodeInnerXML = node.InnerXml;
                            Console.WriteLine(" -- Result1 {0} : {1}", tagName, resultNodeInnerXML);
                            return resultNodeInnerXML;

                        }
                        else
                        {
                            // tagHandler.isRecursive && !node.HasChildNodes
                            if (tagHandler.IsStarAtomically)
                            {
                                string debug = node.OuterXml;
                                // atomic tag?!
                                var TemplateNodeInnerText0 = tagHandler.GetStarContent();
                                node.InnerXml = TemplateNodeInnerText0;
                            }
                            string resultNodeInnerXML = tagHandler.Transform();
                            Console.WriteLine(" -- Result1b {0} : {1}", tagName, resultNodeInnerXML);
                            return resultNodeInnerXML;

                        }
                    }
                    //else
                    else
                    {
                        string resultNodeInnerXML = tagHandler.Transform();

                        //Console.WriteLine(" -- Result1 {0} : {1}", tagName, resultNodeInnerXML);
                        if (resultNodeInnerXML == null)
                        {
                            return string.Empty;
                        }
                        if (resultNodeInnerXML.Trim() == "")
                        {
                            return string.Empty;
                        }
                        if (tagHandler.isVerbatum)
                        {
                            return resultNodeInnerXML;
                        }
                        if (!StaticXMLUtils.ContainsXml(resultNodeInnerXML))
                        {
                            return resultNodeInnerXML;
                        }
                        string recursiveResult = GetOutputSentence(resultNodeInnerXML, null, query, request, result, user, allowProcess);
                        Console.WriteLine(" -- Result2 {0} : {1}", tagName, recursiveResult.ToString());
                        if (tagName=="random")
                        {
                            
                        }
                        return recursiveResult;
                    }
                }
            }
            Console.WriteLine(" -- Result3 default exit {0} : {1}", tagName, node.OuterXml);
            return string.Empty;

        }

        public string GetOutputSentence(string template, XmlNode resultNode, SubQuery query, Request request, AltAIMLbot.Result result, AltAIMLbot.User user, bool allowProcess)
        {
            if (template == null) template = resultNode.OuterXml;
            if (!StaticXMLUtils.ContainsXml(template))
            {
                return template;
            }
            while (template.Contains("<template><template>"))
            {
                template = template.Replace("<template><template>", "<template>");
                template = template.Replace("</template></template>", "</template>");
            }
            resultNode = resultNode ?? AIMLTagHandler.getNode("<node>" + template + "</node>");
            if (resultNode.HasChildNodes)
            {
                StringBuilder recursiveResult = new StringBuilder();
                // recursively check
                bool needSpace = false;
                string sep = " ";
                string lastResult = "";
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
                    else needSpace = true;
                    lastResult = oneChildString;
                    recursiveResult.Append(oneChildString);
                }
                var resultString = recursiveResult.ToString();
                if (resultString.Length > 4) resultString = resultString.Replace(" , ", " ");
                return resultString;
            }
            else
            {
                //Console.WriteLine(" -- Result3 {0} : {1}", tagName, resultNode.InnerXml);
                return resultNode.InnerXml;
            }
        }

        private AIMLTagHandler GetTagHandler(XmlNode node, SubQuery query, Request request, AltAIMLbot.Result result, AltAIMLbot.User user, bool liText)
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
                        return new AltAIMLbot.AIMLTagHandlers.condition(this, user, query, request, result, node);
                        
                    case "date":
                        return new AltAIMLbot.AIMLTagHandlers.date(this, user, query, request, result, node);
                        
                    case "formal":
                        return new AltAIMLbot.AIMLTagHandlers.formal(this, user, query, request, result, node);
                        
                    case "gender":
                        return new AltAIMLbot.AIMLTagHandlers.gender(this, user, query, request, result, node);
                        
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
                        return new AltAIMLbot.AIMLTagHandlers.person(this, user, query, request, result, node);
                        
                    case "person2":
                        return new AltAIMLbot.AIMLTagHandlers.person2(this, user, query, request, result, node);
                        
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
                        return new AltAIMLbot.AIMLTagHandlers.srai(this, user, query, request, result, node);
                        
                    case "star":
                        return new AltAIMLbot.AIMLTagHandlers.star(this, user, query, request, result, node);
                        
                    case "system":
                        return new RTParser.AIMLTagHandlers.system(this, user, query, request, result, node);
                        
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
                        return new AltAIMLbot.AIMLTagHandlers.format(this, user, query, request, result, node, null);

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
            tagHandler = TagHandling.GetTagHandlerU(user, query, request, result, node, false);
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

                AIMLTagHandler newCustomTag = customTagHandler.Instantiate(this.LateBindingAssemblies);
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

        #endregion



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
            // check to delete an existing version of the file
            FileInfo fi = new FileInfo(path);
            if (fi.Exists)
            {
                fi.Delete();
            }
            FileStream saveFile = File.Create(path);
            try
            {
                BinaryFormatter bf = new BinaryFormatter();
                //this.myBehaviors.preSerial();
                bf.Serialize(saveFile, this.Graphmaster);
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
            this.Graphmaster = (GraphMaster)bf.Deserialize(loadFile);
            this.myCron = (Cron)bf.Deserialize(loadFile);
            this.myRandMem = (RandomMemory)bf.Deserialize(loadFile);
            this.Graphs = (Dictionary<string, GraphMaster>)bf.Deserialize(loadFile);
            //this.myBehaviors = (BehaviorSet)bf.Deserialize(loadFile);

            loadFile.Close();
            this.myBehaviors.bot = this;
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
            foreach(string path in request.result.NormalizedPaths)
            {
                paths.Append(path+Environment.NewLine);
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
            string myValue = myChemistry.m_cBus.getHash(bbKey);
            if (myValue.Length > 0)
            {
                GlobalSettings.updateSetting(settingKey, myValue);
            }
        }

        public void importBBUserSettings(AltAIMLbot.User myUser, string bbKey, string settingKey)
        {
            string myValue = myChemistry.m_cBus.getHash(bbKey);
            if (myValue.Length > 0)
            {
                myUser.Predicates.updateSetting(settingKey, myValue);
            }
        }

        public void importBBUser(AltAIMLbot.User myUser)
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
            //curBot.myChemistry.m_cBus.setHash(key,data);
            BBDict[key] = data;
            if (useMemcache)
            {
                if ((myChemistry != null) && (myChemistry.m_cBus != null))
                {
                    myChemistry.m_cBus.setHash(key, data);
                }
            }
        }
        public string getBBHash(string key)
        {
            try
            {
                if (useMemcache)
                {
                    if ((myChemistry != null) && (myChemistry.m_cBus != null))
                    {
                        BBDict[key] = myChemistry.m_cBus.getHash(key);
                    }
                }
                return BBDict[key];
            }
            catch
            {
                return "";
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
        public static bool UnknownTagsAreBotVars = true;


        public GraphMaster GetGraph(string graphName)
        {
            return GetGraph(graphName, Graphmaster);
        }

        public ISettingsDictionary GetDictionary(string name)
        {
            var idict = GetDictionary0(name);
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
                User user = FindOrCreateUser(key);
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

        public Dictionary<string, ISettingsDictionary> AllDictionaries = new Dictionary<string, ISettingsDictionary>();
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
    public class UserConversationScope : MasterUser
    {
        public UserConversationScope(string UserID, AltBot bot)
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
