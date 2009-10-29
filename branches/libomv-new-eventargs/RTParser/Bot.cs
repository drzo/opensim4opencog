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
using AIMLbot;
using RTParser.Normalize;
using RTParser.Utils;
using org.opencyc.api;
using org.opencyc.cycobject;

namespace RTParser
{
    public delegate object SystemExecHandler(string cmd, User user);

    /// <summary>
    /// Encapsulates a Proccessor. If no settings.xml file is found or referenced the Proccessor will try to
    /// default to safe settings.
    /// </summary>
    public class RTPBot
    {
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
        private Dictionary<Unifiable, TagHandler> CustomTags;

        /// <summary>
        /// Holds references to the assemblies that hold the custom tag handling code.
        /// </summary>
        private Dictionary<Unifiable, Assembly> LateBindingAssemblies = new Dictionary<Unifiable, Assembly>();

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
                return Convert.ToDouble(this.GlobalSettings.grabSetting("timeout").ToValue());
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
                Unifiable islogging = this.GlobalSettings.grabSetting("islogging");
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
        /// The number of categories this Proccessor has in its graphmaster "brain"
        /// </summary>
        public int Size;

        /// <summary>
        /// The "brain" of the Proccessor
        /// </summary>
        public RTParser.Utils.Node Graphmaster;

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
            this.setup();  
        }

        #region Settings methods

        /// <summary>
        /// Loads AIML from .aiml files into the graphmaster "brain" of the Proccessor
        /// </summary>
        public void loadAIMLFromFiles()
        {
            AIMLLoader loader = new AIMLLoader(this);
            loader.loadAIML();
        }

        /// <summary>
        /// Loads AIML from .aiml files into the graphmaster "brain" of the Proccessor
        /// </summary>
        public void loadAIMLFromFiles(string path)
        {
            AIMLLoader loader = new AIMLLoader(this);
            loader.loadAIML(path);
            // maybe loads settings files if they are there
            string settings = Path.Combine(path, "Settings.xml");
            if (File.Exists(settings)) loadSettings(settings);
        }

        /// <summary>
        /// Allows the Proccessor to load a new XML version of some AIML
        /// </summary>
        /// <param name="newAIML">The XML document containing the AIML</param>
        /// <param name="filename">The originator of the XML document</param>
        public void loadAIMLFromXML(XmlDocument newAIML, Unifiable filename)
        {
            AIMLLoader loader = new AIMLLoader(this);
            loader.loadAIMLFromXML(newAIML, filename);
        }

        /// <summary>
        /// Instantiates the dictionary objects and collections associated with this class
        /// </summary>
        private void setup()
        {
            this.GlobalSettings = new SettingsDictionary(this, null);
            ParentProvider provider = new ParentProvider(() => GlobalSettings);
            this.GenderSubstitutions = new SettingsDictionary(this, provider);
            this.Person2Substitutions = new SettingsDictionary(this, provider);
            this.PersonSubstitutions = new SettingsDictionary(this, provider);
            this.Substitutions = new SettingsDictionary(this, provider);
            this.DefaultPredicates = new SettingsDictionary(this, provider);
            this.CustomTags = new Dictionary<Unifiable, TagHandler>();
            this.Size = 0;
            this.Graphmaster = new RTParser.Utils.Node(null);
            loadCustomTagHandlers("AIMLbot.dll");
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

            // Grab the splitters for this Proccessor
            this.loadSplitters(Path.Combine(this.PathToConfigFiles,this.GlobalSettings.grabSetting("splittersfile")));
        }

        private void SetSaneGlobals(SettingsDictionary settings)
        {
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
        public string LastLogMessage = string.Empty;

        public OutputDelegate outputDelegate;

        public delegate void OutputDelegate(string s, params object[] args);

        /// <summary>
        /// Writes a (timestamped) message to the Processor's log.
        /// 
        /// Log files have the form of yyyyMMdd.log.
        /// </summary>
        /// <param name="message">The message to log</param>
        public void writeToLog(string message)
        {
            message = (DateTime.Now.ToString() + ": " + message + Environment.NewLine);
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
            }
            else
            {
                string m = message.AsString().ToLower();
               if (m.Contains("error") || m.Contains("excep")) Console.WriteLine(message);
            }
            this.LastLogMessage = message;
            if (this.IsLogging)
            {
                //  this.LogBuffer.Add(DateTime.Now.ToString() + ": " + message + Environment.NewLine);
                this.LogBuffer.Add(message);
                if (this.LogBuffer.Count > this.MaxLogBufferSize-1)
                {
                    // Write out to log file
                    DirectoryInfo logDirectory = new DirectoryInfo(this.PathToLogs);
                    if (!logDirectory.Exists)
                    {
                        logDirectory.Create();
                    }

                    Unifiable logFileName = DateTime.Now.ToString("yyyyMMdd")+".log";
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

                    foreach (Unifiable msg in this.LogBuffer)
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
                AIMLbot.Request r = new AIMLbot.Request("My name is " + fromname, myUser, this);
                AIMLbot.Result res = Chat(r);
                myUser.Predicates.addSetting("name", fromname);
                return myUser;
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
            return Chat(new AIMLbot.Request(rawInput, FindOrCreateUser(username), this)).Output;
        }


        /// <summary>
        /// Given some raw input and a unique ID creates a response for a new user
        /// </summary>
        /// <param name="rawInput">the raw input</param>
        /// <param name="UserGUID">an ID for the new user (referenced in the result object)</param>
        /// <returns>the result to be output to the user</returns>
        public Result Chat(Unifiable rawInput, Unifiable UserGUID)
        {
            AIMLbot.Request request = new AIMLbot.Request(rawInput, new User(UserGUID, this), this);
            return this.Chat(request);
        }

        /// <summary>
        /// Given a request containing user input, produces a result from the Proccessor
        /// </summary>
        /// <param name="request">the request from the user</param>
        /// <returns>the result to be output to the user</returns>
        public AIMLbot.Result Chat(Request request)
        {
            AIMLbot.Result result = new AIMLbot.Result(request.user, this, request);

            if (this.isAcceptingUserInput)
            {
                // Normalize the input
                AIMLLoader loader = new AIMLLoader(this);
                //RTParser.Normalize.SplitIntoSentences splitter = new RTParser.Normalize.SplitIntoSentences(this);
                Unifiable[] rawSentences = new Unifiable[] { request.rawInput };//splitter.Transform(request.rawInput);
                foreach (Unifiable sentence in rawSentences)
                {
                    result.InputSentences.Add(sentence);
                    Unifiable path = loader.generatePath(sentence, request.user.getLastBotOutput(), request.user.Topic, true);
                    result.NormalizedPaths.Add(path);
                }

                // grab the templates for the various sentences from the graphmaster
                foreach (Unifiable path in result.NormalizedPaths)
                {
                    Utils.SubQuery query = new SubQuery(path,result);
                    query.Template = this.Graphmaster.evaluate(path, query, request, MatchState.UserInput, Unifiable.CreateAppendable());
                    result.SubQueries.Add(query);
                }

                //todo pick and chose the queries
                if (result.SubQueries.Count != 1) Console.WriteLine("Found " + result.SubQueries.Count + " queries");

                // process the templates into appropriate output
                foreach (SubQuery query in result.SubQueries)
                {
                    if (processTemplate(query, request, result)) ;
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
        /// Return false if the guard fails
        /// </summary>
        /// <param name="query"></param>
        /// <param name="request"></param>
        /// <param name="result"></param>
        /// <returns></returns>
        private bool processTemplate(SubQuery query, Request request, Result result)
        {
            bool found = false;
            if (query.Template != null && query.Template.Count > 0)
            {
                foreach (Template s in query.Template)
                {
                    try
                    {
                        //XmlNode guardNode = AIMLTagHandler.getNode(s.Guard.InnerXml);
                        string output = s.Output.OuterXml;
                        bool usedGuard = false;
                        if (s.Guard != null)
                        {
                            usedGuard = true;
                            output = output.Trim();
                            if (output.StartsWith("<template>"))
                            {
                                output = "<template>" + s.Guard.InnerXml + " GUARDBOM " + output.Substring(10);
                            }

                        }
                        XmlNode templateNode = AIMLTagHandler.getNode(output);
                        string outputSentence = this.processNode(templateNode, query, request, result, request.user);
                        int f = outputSentence.IndexOf("GUARDBOM");
                        if (f < 0)
                        {
                            if (outputSentence.Length > 0)
                            {
                                result.OutputSentences.Add(outputSentence.Trim().Replace("  ", " ").Replace("  ", " "));
                                found = true;
                            }
                        }
                        else
                        {
                            try
                           {
	                           string left = outputSentence.Substring(0, f);
	                            Unifiable ss = EvalSubL("(cyc-query '" + left + " #$EverythingPSC)", null);
	                            if (Unifiable.IsFalse(ss)) continue;
	                            outputSentence = outputSentence.Substring(f + 9);
	                            if (outputSentence.Length > 0)
	                            {
	                                result.OutputSentences.Add(outputSentence);
	                                found = true;
	                                break;
	                            }
                           }
                           catch (System.Exception ex)
                           {
                               continue;
                           }
                        }
                    }
                    catch (Exception e)
                    {
                        Console.WriteLine("" + e);
                        if (this.WillCallHome)
                        {
                            this.phoneHome(e.Message, request);
                        }
                        this.writeToLog("WARNING! A problem was encountered when trying to process the input: " +
                                        request.rawInput + " with the template: \"" + query.Template + "\"");
                        return false;
                    }
                }
            }
            return found;
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
        public Unifiable processNode(XmlNode node, SubQuery query, Request request, Result result, User user)
        {
            // check for timeout (to avoid infinite loops)
            if (request != null && request.StartedOn.AddMilliseconds(request.Proccessor.TimeOut) < DateTime.Now)
            {
                request.Proccessor.writeToLog("WARNING! Request timeout. User: " + request.user.UserID +
                                              " raw input: \"" + request.rawInput + "\" processing template: \"" +
                                              query.Template + "\"");
                request.hasTimedOut = true;
                return Unifiable.Empty;
            }

            // process the node
            AIMLTagHandler tagHandler = GetTagHandler(user, query, request, result, node);
            if (object.Equals(null, tagHandler))
            {
                return node.InnerText;
            }
            return tagHandler.CompleteProcess();
        }


        internal AIMLTagHandler GetTagHandler(User user, SubQuery query, Request request, Result result, XmlNode node)
        {
            AIMLTagHandler tagHandler = null;
            tagHandler = this.getBespokeTags(user, query, request, result, node);
            if (object.Equals(null, tagHandler))
            {
                switch (node.Name.ToLower())
                {
                    case "template":
                        tagHandler = new AIMLTagHandlers.template(this, user, query, request, result, node);
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

                    default:
                        tagHandler = null;
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
                    newCustomTag.Proc = this;
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
        /// Proccessor starts
        /// </summary>
        /// <param name="path">the path to the file for saving</param>
        public void saveToBinaryFile(Unifiable path)
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
        public void loadFromBinaryFile(Unifiable path)
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
            return;
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
        /// resulting from a query to the Proccessor
        /// </summary>
        /// <param name="errorMessage">the resulting error message</param>
        /// <param name="request">the request object that encapsulates all sorts of useful information</param>
        public void phoneHome(Unifiable errorMessage, Request request)
        {
            MailMessage msg = new MailMessage("donotreply@aimlbot.com",this.AdminEmail);
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
            foreach(Unifiable path in request.result.NormalizedPaths)
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

        #region CYC Interaction


        private CycAccess cycAccess;
        public bool CycEnabled
        {
            get
            {
                cycAccess = GetCycAccess;
                return UseCyc;
            }
        }
        private bool UseCyc = true;
        public CycAccess GetCycAccess
        {
            get
            {
                if (UseCyc && (cycAccess == null || cycAccess.isClosed()))
                {
                    try
                    {
                        cycAccess = new CycAccess("CycServer", 3600);
                        cycAccess.converseInt("(+ 1 1)");
                        cycAccess.createIndividual("AimlContextMt",
                                                    "#$AimlContextMt contains storage location in OpenCyc for AIML variables",
                                                    "UniversalVocabularyMt", "DataMicrotheory");
                    }
                    catch (Exception e)
                    {
                        UseCyc = false;
                    }
                    //if (cycAccess.isClosed()) cycAccess.persistentConnection = true;
                }

                return cycAccess;
            }
            set { cycAccess = value; }
        }
        public Unifiable EvalSubL(Unifiable cmd, Unifiable filter)
        {
            Unifiable result = "(EVAL-SUBL " + cmd + ")";
            CycAccess access = GetCycAccess;
            if (!UseCyc) return result;
            try
            {
                Console.Write(result);
                Console.Out.Flush();
                string str = "(list " + cmd + ")";
                Object oresult = access.converseList(str).first();
                Console.WriteLine( " => " + oresult);
                result = "" + oresult;
                if (oresult is CycObject)
                {
                    result = ((CycObject)oresult).cyclifyWithEscapeChars();
                }
                if (!String.IsNullOrEmpty(filter) && filter == "paraphrase")
                {
                    return this.Paraphrase(result);
                }
            }
            catch (Exception e)
            {
                Console.WriteLine(""+e);
                Console.Out.Flush();
                return null;
            }
            return result;
        }

        internal Unifiable processNodeInside(XmlNode templateNode, SubQuery query, Request request, Result result, User user)
        {
            return processNode(templateNode, query, request, result, user);
        }

        internal bool IsaFilter(Unifiable term, Unifiable filter)
        {
            if (term.IsEmpty) return false;
            if (term == "NIL") return false;
            if (!filter.IsEmpty)
            {
                if (Unifiable.IsFalse(filter)) return true;
                if (this.EvalSubL(String.Format("(ask-template 'T `(#$isa {0} {1}) #$EverythingPSC)", term, Cyclify(filter)), null) == "NIL")
                    return false;
            }
            return true;
        }

        internal Unifiable Paraphrase(string text)
        {
            text = Cyclify(text);
            if (text.StartsWith("("))
            {   //todo is a list then?
                text = String.Format("'{0}", text);
            }
            //return text;
            try
            {
	            return EvalSubL(String.Format("(generate-phrase {0})", text), null);
            }
            catch (System.Exception ex)
            {
                return text;
            }
        }

        #endregion

        internal Unifiable SystemExecute(Unifiable cmd, Unifiable langu, User user)
        {
            if (String.IsNullOrEmpty(langu))
            {
                langu = "bot";  
            }
            Unifiable s = "<The system tag should be doing '" + cmd + "' lang=" + langu + ">";
            writeToLog(s);
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
                    Console.WriteLine("" + e);
                    return Unifiable.Empty;
                }
            }
            if (langu == "subl") return EvalSubL(cmd, null);
            return s;
            
        }

        internal Unifiable Cyclify(string mt)
        {
            mt = mt.Trim();
            if (mt.Length == 0) return mt;
            if (System.Char.IsLetter(mt.ToCharArray()[0])) return "#$"+mt;
            return mt;

            if (System.Char.IsDigit(mt.ToCharArray()[0])) return mt;
            if (mt.StartsWith("(") || mt.StartsWith("#$")) return mt;
            return "#$" + mt;
        }

        readonly Dictionary<string,SystemExecHandler> ExecuteHandlers = new Dictionary<string, SystemExecHandler>();
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
            return (Unifiable) GlobalSettings.grabSetting(name);
        }

    }
}
