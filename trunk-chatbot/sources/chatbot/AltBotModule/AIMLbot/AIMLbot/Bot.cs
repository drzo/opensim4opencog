using System;
using System.Collections.Generic;
using System.Globalization;
using System.Text.RegularExpressions;
using System.IO;
using System.Xml;
using System.Text;
using System.Runtime.Serialization.Formatters.Binary;
using System.Reflection;
using System.Net.Mail;

using AltAIMLbot.Utils;
using DcBus;
using Aima.Core.Logic.Propositional.Algorithms;
using Aima.Core.Logic.Propositional.Parsing;
using Aima.Core.Logic.Propositional.Parsing.AST;
/******************************************************************************************
AltAIMLBot -- Copyright (c) 2011-2012,Kino Courssey, Daxtron Labs

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
    /// Encapsulates a bot. If no settings.xml file is found or referenced the bot will try to
    /// default to safe settings.
    /// </summary>
    /// 
        public delegate void sayProcessorDelegate(string message);
        public delegate void systemProcessorDelegate(string message);

    public class AltBot
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
        public RandomMemory myRandMem = new RandomMemory();

        public KnowledgeBase myKB = new KnowledgeBase();
        public KnowledgeBase myBaseKB = new KnowledgeBase();
        public WalkSAT myWalkSAT = new WalkSAT();
        public Model myModel = null;
        public Model myActiveModel = null;


        /// <summary>
        /// A dictionary object that looks after all the settings associated with this bot
        /// </summary>
        public SettingsDictionary GlobalSettings;

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
        /// Flag to show if the bot is willing to accept user input
        /// </summary>
        public bool isAcceptingUserInput = true;

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
                return Convert.ToDouble(this.GlobalSettings.grabSetting("timeout"));
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
                string islogging = this.GlobalSettings.grabSetting("islogging");
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
                string willcallhome = this.GlobalSettings.grabSetting("willcallhome");
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
        /// The number of categories this bot has in its graphmaster "brain"
        /// </summary>
        public int Size;

        /// <summary>
        /// The "brain" of the bot
        /// </summary>
        public AltAIMLbot.Utils.Node Graphmaster;

        /// <summary>
        /// If set to false the input from AIML files will undergo the same normalization process that
        /// user input goes through. If true the bot will assume the AIML is correct. Defaults to true.
        /// </summary>
        public bool TrustAIML=true;

        /// <summary>
        /// The maximum number of characters a "that" element of a path is allowed to be. Anything above
        /// this length will cause "that" to be "*". This is to avoid having the graphmaster process
        /// huge "that" elements in the path that might have been caused by the bot reporting third party
        /// data.
        /// </summary>
        public int MaxThatSize = 256;


        /// <summary>
        /// A general stack to remember things to mention later
        /// </summary>
        public Stack<string> conversationStack = new Stack<string>();

        #endregion

        #region Delegates

        public delegate void LogMessageDelegate();

        public sayProcessorDelegate sayProcessor;

        #endregion

        #region Events

        public event LogMessageDelegate WrittenToLog;

        #endregion

        /// <summary>
        /// Ctor
        /// </summary>
        public AltBot()
        {
            this.setup();  
        }

        #region Settings methods
        /// <summary>
        /// Loads AIML from .aiml files into the graphmaster "brain" of the Proccessor
        /// </summary>
        public void loadAIMLFromDefaults()
        {
        }

        /// <summary>
        /// Loads AIML from .aiml files into the graphmaster "brain" of the bot
        /// </summary>
        public void loadAIMLFromFiles()
        {
            AIMLLoader loader = new AIMLLoader(this);
            loader.loadAIML();
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
        public void loadAIMLFromXML(XmlDocument newAIML, string filename)
        {
            AIMLLoader loader = new AIMLLoader(this);
            loader.loadAIMLFromXML(newAIML, filename);
        }

        /// <summary>
        /// Instantiates the dictionary objects and collections associated with this class
        /// </summary>
        private void setup()
        {
            this.myCron = new Cron(this);

            this.GlobalSettings = new SettingsDictionary(this);
            this.GenderSubstitutions = new SettingsDictionary(this);
            this.Person2Substitutions = new SettingsDictionary(this);
            this.PersonSubstitutions = new SettingsDictionary(this);
            this.InputSubstitutions = new SettingsDictionary(this);
            this.DefaultPredicates = new SettingsDictionary(this);
            this.CustomTags = new Dictionary<string, TagHandler>();
            this.Graphmaster = new AltAIMLbot.Utils.Node();
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
                this.GlobalSettings.addSetting("timeout", "2000");
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
                                string value = myNode.Attributes["value"].Value;
                                this.Splitters.Add(value);
                            }
                        }
                    }
                }
            }
            if (this.Splitters.Count == 0)
            {
                // we don't have any splitters, so lets make do with these...
                this.Splitters.Add(".");
                this.Splitters.Add("!");
                this.Splitters.Add("?");
                this.Splitters.Add(";");
            }
        }
        #endregion

        #region Logging methods

        /// <summary>
        /// The last message to be entered into the log (for testing purposes)
        /// </summary>
        public string LastLogMessage=string.Empty;

        /// <summary>
        /// Writes a (timestamped) message to the bot's log.
        /// 
        /// Log files have the form of yyyyMMdd.log.
        /// </summary>
        /// <param name="message">The message to log</param>
        public void writeToLog(string message)
        {
            Console.WriteLine("Log:" + message);

            this.LastLogMessage = message;
            if (this.IsLogging)
            {
                this.LogBuffer.Add(DateTime.Now.ToString() + ": " + message + Environment.NewLine);
                if (this.LogBuffer.Count > this.MaxLogBufferSize-1)
                {
                    // Write out to log file
                    DirectoryInfo logDirectory = new DirectoryInfo(this.PathToLogs);
                    if (!logDirectory.Exists)
                    {
                        logDirectory.Create();
                    }

                    string logFileName = DateTime.Now.ToString("yyyyMMdd")+".log";
                    FileInfo logFile = new FileInfo(Path.Combine(this.PathToLogs,logFileName));
                    StreamWriter writer;
                    if (!logFile.Exists)
                    {
                        writer = logFile.CreateText();
                    }
                    else
                    {
                        writer = logFile.AppendText();
                    }

                    foreach (string msg in this.LogBuffer)
                    {
                        writer.WriteLine(msg);
                    }
                    writer.Close();
                    this.LogBuffer.Clear();
                }
            }
            if (!object.Equals(null, this.WrittenToLog))
            {
                this.WrittenToLog();
            }
        }

        #endregion

        #region Conversation methods

        /// <summary>
        /// Given some raw input and a unique ID creates a response for a new user
        /// </summary>
        /// <param name="rawInput">the raw input</param>
        /// <param name="UserGUID">an ID for the new user (referenced in the result object)</param>
        /// <returns>the result to be output to the user</returns>
        public Result Chat(string rawInput, string UserGUID)
        {
            Request request = new Request(rawInput, new User(UserGUID, this), this);
            return this.Chat(request);
        }

        /// <summary>
        /// Given a request containing user input, produces a result from the bot.
        /// Sensitive to request.user.Qstate
        /// </summary>
        /// <param name="request">the request from the user</param>
        /// <returns>the result to be output to the user</returns>
        public Result Chat(Request request)
        {
            Result result = new Result(request.user, this, request);

            if (this.isAcceptingUserInput)
            {
                // Normalize the input
                AIMLLoader loader = new AIMLLoader(this);
                AltAIMLbot.Normalize.SplitIntoSentences splitter = new AltAIMLbot.Normalize.SplitIntoSentences(this);
                string[] rawSentences = splitter.Transform(request.rawInput);

                if (request.user.Qstate.Count == 0)
                {
                    Console.WriteLine("DEBUG:Using Normal Search");
                    // Standard operation
                    foreach (string sentence in rawSentences)
                    {
                        result.InputSentences.Add(sentence);
                        string path = loader.generatePath(sentence, request.user.getLastBotOutput(), request.user.Topic, request.user.State, request.user.State, true);
                        result.NormalizedPaths.Add(path);
                        Console.WriteLine("DEBUG: path = " + path);
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
                        foreach (string nstate in request.user.Qstate.Keys)
                        {
                            string path = loader.generatePath(sentence, request.user.getLastBotOutput(), request.user.Topic, nstate, nstate, true);
                            double statev = this.Graphmaster.getPathScore(path);

                            if (statev == bestv)
                            {
                                if (request.user.Qstate[nstate] > request.user.Qstate[beststate])
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
                        result.NormalizedPaths.Add(bestpath);
                        Console.WriteLine("DEBUG: path = " + bestpath);
                    }                


                }

                // grab the templates for the various sentences from the graphmaster
                foreach (string path in result.NormalizedPaths)
                {
                    Utils.SubQuery query = new SubQuery(path);
                    query.Template = this.Graphmaster.evaluate(path, query, request, MatchState.UserInput, new StringBuilder());
                    Console.WriteLine("DEBUG: TemplatePath = " + query.TemplatePath);
                    Console.WriteLine("DEBUG: Template = " + query.Template);
                    result.SubQueries.Add(query);
                }

                // process the templates into appropriate output
                foreach (SubQuery query in result.SubQueries)
                {
                    if (query.Template.Length > 0)
                    {
                        try
                        {
                            XmlNode templateNode = AIMLTagHandler.getNode(query.Template);
                            string outputSentence = this.processNode(templateNode, query, request, result, request.user);
                            if (outputSentence.Length > 0)
                            {
                                result.OutputSentences.Add(outputSentence);
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
            }
            else
            {
                result.OutputSentences.Add(this.NotAcceptingUserInputMessage);
            }

            // populate the Result object
            result.Duration = DateTime.Now - request.StartedOn;
            request.user.addResult(result);

            return result;
        }

        /// <summary>
        /// given an template side XML, try evaluating it
        /// </summary>       
        public void evalTemplateNode(XmlNode templateNode)
        {
            User imaginaryUser = new User("internal",this);
            Request request = new Request("", imaginaryUser, this);
            Utils.SubQuery query = new SubQuery("");
            Result result = new Result(request.user, this, request);
 
            string outputSentence = this.processNode(templateNode, query, request, result, request.user);
       
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
        private string processNode(XmlNode node, SubQuery query, Request request, Result result, User user)
        {
            // check for timeout (to avoid infinite loops)
            if (request.StartedOn.AddMilliseconds(request.bot.TimeOut) < DateTime.Now)
            {
                request.bot.writeToLog("WARNING! Request timeout. User: " + request.user.UserID + " raw input: \"" + request.rawInput + "\" processing template: \""+query.Template+"\"");
                request.hasTimedOut = true;
                return string.Empty;
            }
                        
            // process the node
            string tagName = node.Name.ToLower();
            if (tagName == "template")
            {
                StringBuilder templateResult = new StringBuilder();
                if (node.HasChildNodes)
                {
                    // recursively check
                    foreach (XmlNode childNode in node.ChildNodes)
                    {
                        templateResult.Append(this.processNode(childNode, query, request, result, user));
                    }
                }
                return templateResult.ToString();
            }
            else
            {
                AIMLTagHandler tagHandler = null;
                tagHandler = this.getBespokeTags(user, query, request, result, node);
                if (object.Equals(null, tagHandler))
                {
                    Console.WriteLine("  -- Process :" + tagName);
                    switch (tagName)
                    {
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


                        case "push":
                            tagHandler = new AIMLTagHandlers.push(this, user, query, request, result, node);
                            break;
                        case "pop":
                            tagHandler = new AIMLTagHandlers.pop(this, user, query, request, result, node);
                            break;
                        case "postcache":
                            tagHandler = new AIMLTagHandlers.postcache(this, user, query, request, result, node);
                            break;
                        case "refserver":
                            tagHandler = new AIMLTagHandlers.refserver(this, user, query, request, result, node);
                            break;

                        case "inject":
                            tagHandler = new AIMLTagHandlers.inject(this, user, query, request, result, node);
                            break;
                        case "chemsys":
                            tagHandler = new AIMLTagHandlers.chemsys(this, user, query, request, result, node);
                            break;
                        case "nop":
                            tagHandler = new AIMLTagHandlers.nop(this, user, query, request, result, node);
                            break;
                        case "scxml":
                            tagHandler = new AIMLTagHandlers.scxml(this, user, query, request, result, node);
                            break;
                        case "btxml":
                            tagHandler = new AIMLTagHandlers.scxml(this, user, query, request, result, node);
                            break;
                        case "say":
                            tagHandler = new AIMLTagHandlers.say(this, user, query, request, result, node);
                            break;
                        case "satisfied":
                            tagHandler = new AIMLTagHandlers.satisfied(this, user, query, request, result, node);
                            break;
                        case "behavior":
                            tagHandler = new AIMLTagHandlers.behavior(this, user, query, request, result, node);
                            break;
                        case "rndint":
                            tagHandler = new AIMLTagHandlers.rndint(this, user, query, request, result, node);
                            break;
                        case "rnddbl":
                            tagHandler = new AIMLTagHandlers.rnddbl(this, user, query, request, result, node);
                            break;
                        case "crontag":
                            tagHandler = new AIMLTagHandlers.crontag(this, user, query, request, result, node);
                            break;

                        default:
                            tagHandler = null;
                            break;
                    }
                }
                if (object.Equals(null, tagHandler))
                {
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
                                if (childNode.NodeType != XmlNodeType.Text)
                                {
                                    childNode.InnerXml = this.processNode(childNode, query, request, result, user);
                                }
                            }
                        }
                        string tresult = tagHandler.Transform();
                        Console.WriteLine(" -- Result1 {0} : {1}", tagName, tresult);
                        return tresult;
                    }
                    else
                    {
                        string resultNodeInnerXML = tagHandler.Transform();
                        XmlNode resultNode = AIMLTagHandler.getNode("<node>" + resultNodeInnerXML + "</node>");
                        if (resultNode.HasChildNodes)
                        {
                            StringBuilder recursiveResult = new StringBuilder();
                            // recursively check
                            foreach (XmlNode childNode in resultNode.ChildNodes)
                            {
                                recursiveResult.Append(this.processNode(childNode, query, request, result, user));
                            }
                            Console.WriteLine(" -- Result2 {0} : {1}", tagName, recursiveResult.ToString());
                            return recursiveResult.ToString();
                        }
                        else
                        {
                            Console.WriteLine(" -- Result3 {0} : {1}", tagName, resultNode.InnerXml);
                            return resultNode.InnerXml;
                        }
                    }
                }
            }
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
        public AIMLTagHandler getBespokeTags(User user, SubQuery query, Request request, Result result, XmlNode node)
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

        public string EnsureEnglish(string arg)
        {
            return arg;

        }



        #region Serialization

        /// <summary>
        /// Saves the graphmaster node (and children) to a binary file to avoid processing the AIML each time the 
        /// bot starts
        /// </summary>
        /// <param name="path">the path to the file for saving</param>
        public void saveToBinaryFile(string path)
        {
            // check to delete an existing version of the file
            FileInfo fi = new FileInfo(path);
            if (fi.Exists)
            {
                fi.Delete();
            }

            FileStream saveFile = File.Create(path);
            BinaryFormatter bf = new BinaryFormatter();
            bf.Serialize(saveFile, this.Graphmaster);
            saveFile.Close();
        }

        /// <summary>
        /// Loads a dump of the graphmaster into memory so avoiding processing the AIML files again
        /// </summary>
        /// <param name="path">the path to the dump file</param>
        public void loadFromBinaryFile(string path)
        {
            FileStream loadFile = File.OpenRead(path);
            BinaryFormatter bf = new BinaryFormatter();
            this.Graphmaster = (Node)bf.Deserialize(loadFile);
            loadFile.Close();
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
    }
    public class myConst
    {
        public static string MEMHOST = "127.0.0.1";
        //public static string MEMHOST = "192.168.2.141";
    }
    public class UserDuringProcessing : User
    {
        public UserDuringProcessing(string UserID, AltAIMLbot.AltBot bot)
            : base(UserID,bot)
        {
        }
    }
    public class UserConversationScope : User
    {
        public UserConversationScope(string UserID, AltAIMLbot.AltBot bot)
            : base(UserID,bot)
        {
        }
    }

   // public class Unifiable 
   // {
    //
    //}

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

    }
}
