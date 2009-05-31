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
        public List<Unifiable> Splitters = new List<Unifiable>();

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
                return Convert.ToDouble(this.GlobalSettings.grabSetting("timeout"));
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
        public Unifiable AdminEmail
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
                    this.GlobalSettings.addSetting("adminemail", "");
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
        /// Flag to denote if the Proccessor will email the botmaster using the AdminEmail setting should an error
        /// occur
        /// </summary>
        public bool WillCallHome
        {
            get
            {
                Unifiable willcallhome = this.GlobalSettings.grabSetting("willcallhome");
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
        public Unifiable PathToAIML
        {
            get
            {
                return Path.Combine(Environment.CurrentDirectory, this.GlobalSettings.grabSetting("aimldirectory"));
            }
        }

        /// <summary>
        /// The directory to look in for the various XML configuration files
        /// </summary>
        public Unifiable PathToConfigFiles
        {
            get
            {
                return Path.Combine(Environment.CurrentDirectory, this.GlobalSettings.grabSetting("configdirectory"));
            }
        }

        /// <summary>
        /// The directory into which the various log files will be written
        /// </summary>
        public Unifiable PathToLogs
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
            this.GlobalSettings = new SettingsDictionary(this);
            this.GenderSubstitutions = new SettingsDictionary(this);
            this.Person2Substitutions = new SettingsDictionary(this);
            this.PersonSubstitutions = new SettingsDictionary(this);
            this.Substitutions = new SettingsDictionary(this);
            this.DefaultPredicates = new SettingsDictionary(this);
            this.CustomTags = new Dictionary<Unifiable, TagHandler>();
            this.Graphmaster = new RTParser.Utils.Node(null);
            loadCustomTagHandlers("RTParser.dll");
        }

        /// <summary>
        /// Loads settings based upon the default location of the Settings.xml file
        /// </summary>
        public void loadSettings()
        {
            // try a safe default setting for the settings xml file
            Unifiable path = Path.Combine(Environment.CurrentDirectory, Path.Combine("config", "Settings.xml"));
            this.loadSettings(path);          
        }

        /// <summary>
        /// Loads settings and configuration info from various xml files referenced in the settings file passed in the args. 
        /// Also generates some default values if such values have not been set by the settings file.
        /// </summary>
        /// <param name="pathToSettings">Path to the settings xml file</param>
        public void loadSettings(Unifiable pathToSettings)
        {
            this.GlobalSettings.loadSettings(pathToSettings.AsString());

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
                this.GlobalSettings.addSetting("website", "http://sourceforge.net/projects/aimlbot");
            }
            if (this.GlobalSettings.containsSettingCalled("adminemail"))
            {
                Unifiable emailToCheck = this.GlobalSettings.grabSetting("adminemail");
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
                this.GlobalSettings.addSetting("notacceptinguserinputmessage", "This Proccessor is currently set to not accept user input.");
            }
            if (!this.GlobalSettings.containsSettingCalled("stripperregex"))
            {
                this.GlobalSettings.addSetting("stripperregex", "[^0-9a-zA-Z]");
            }

            // Load the dictionaries for this RTPBot from the various configuration files
            this.Person2Substitutions.loadSettings(Path.Combine(this.PathToConfigFiles, this.GlobalSettings.grabSetting("person2substitutionsfile")));
            this.PersonSubstitutions.loadSettings(Path.Combine(this.PathToConfigFiles, this.GlobalSettings.grabSetting("personsubstitutionsfile")));
            this.GenderSubstitutions.loadSettings(Path.Combine(this.PathToConfigFiles, this.GlobalSettings.grabSetting("gendersubstitutionsfile")));
            this.DefaultPredicates.loadSettings(Path.Combine(this.PathToConfigFiles, this.GlobalSettings.grabSetting("defaultpredicates")));
            this.Substitutions.loadSettings(Path.Combine(this.PathToConfigFiles, this.GlobalSettings.grabSetting("substitutionsfile")));

            // Grab the splitters for this Proccessor
            this.loadSplitters(Path.Combine(this.PathToConfigFiles,this.GlobalSettings.grabSetting("splittersfile")));
        }

        /// <summary>
        /// Loads the splitters for this Proccessor from the supplied config file (or sets up some safe defaults)
        /// </summary>
        /// <param name="pathToSplitters">Path to the config file</param>
        private void loadSplitters(Unifiable pathToSplitters)
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
                                Unifiable value = myNode.Attributes["value"].Value;
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
                //this.Splitters.Add("?");
                this.Splitters.Add(";");
            }
        }
        #endregion

        #region Logging methods

        /// <summary>
        /// The last message to be entered into the log (for testing purposes)
        /// </summary>
        public Unifiable LastLogMessage=Unifiable.Empty;

        public OutputDelegate outputDelegate;
        public delegate void OutputDelegate(string str);

        /// <summary>
        /// Writes a (timestamped) message to the Processor's log.
        /// 
        /// Log files have the form of yyyyMMdd.log.
        /// </summary>
        /// <param name="message">The message to log</param>
        public void writeToLog(Unifiable message)
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

        #region Conversation methods

        /// <summary>
        /// Given some raw input and a unique ID creates a response for a new user
        /// </summary>
        /// <param name="rawInput">the raw input</param>
        /// <param name="UserGUID">an ID for the new user (referenced in the result object)</param>
        /// <returns>the result to be output to the user</returns>
        public Result Chat(Unifiable rawInput, Unifiable UserGUID)
        {
            Request request = new Request(rawInput, new User(UserGUID, this), this);
            return this.Chat(request);
        }

        /// <summary>
        /// Given a request containing user input, produces a result from the Proccessor
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
                RTParser.Normalize.SplitIntoSentences splitter = new RTParser.Normalize.SplitIntoSentences(this);
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
                    Utils.SubQuery query = new SubQuery(path);
                    query.Template = this.Graphmaster.evaluate(path, query, request, MatchState.UserInput, new UUnifiable());
                    result.SubQueries.Add(query);
                }

                //todo pick and chose the queries
                Console.WriteLine("Found " + result.SubQueries.Count + " queries");

                // process the templates into appropriate output
                foreach (SubQuery query in result.SubQueries)
                {
                    processTemplate(query, request, result);

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

        private void processTemplate(SubQuery query, Request request, Result result)
        {
            if (query.Template != null && query.Template.Count > 0)
            {
                foreach (Template s in query.Template)
                {

                    try
                    {
                        //XmlNode guardNode = AIMLTagHandler.getNode(s.Guard.InnerXml);
                        Unifiable output = s.Output.OuterXml;
                        bool usedGuard = false;
                        if (s.Guard != null)
                        {
                            usedGuard = true;
                            output = output.Trim();
                            if (output.StartsWith("<template>"))
                            {
                                output = "<template>" + s.Guard.InnerXml + "GUARDBOM " + output.Substring(10);
                            }

                        }
                        XmlNode templateNode = AIMLTagHandler.getNode(output);
                        Unifiable outputSentence = this.processNode(templateNode, query, request, result, request.user);
                        int f = outputSentence.IndexOf("GUARDBOM");
                        if (f < 0)
                        {
                            if (outputSentence.Length > 0)
                            {
                                result.OutputSentences.Add(outputSentence);
                            }
                        }
                        else
                        {
                            try
                           {
	                           Unifiable left = outputSentence.Substring(0, f);
	                            Unifiable ss = EvalSubL("(cyc-query '" + left + " #$EverythingPSC)", null);
                                if (Unifiable.IsNull(ss)) continue;
	                            ss = ss.Trim();
	                            if (ss == "" || ss == "NIL") continue;
	                            outputSentence = outputSentence.Substring(f + 9);
	                            if (outputSentence.Length > 0)
	                            {
	                                result.OutputSentences.Add(outputSentence);
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
                    }
                }
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
        public Unifiable processNode(XmlNode node, SubQuery query, Request request, Result result, User user)
        {
            // check for timeout (to avoid infinite loops)
            if (request.StartedOn.AddMilliseconds(request.Proccessor.TimeOut) < DateTime.Now)
            {
                request.Proccessor.writeToLog("WARNING! Request timeout. User: " + request.user.UserID + " raw input: \"" + request.rawInput + "\" processing template: \""+query.Template+"\"");
                request.hasTimedOut = true;
                return Unifiable.Empty;
            }
                        
            // process the node
            Unifiable tagName = node.Name.ToLower();
            if (tagName == "template")
            {
                UUnifiable templateResult = new UUnifiable();
                if (node.HasChildNodes)
                {
                    // recursively check
                    foreach (XmlNode childNode in node.ChildNodes)
                    {
                        templateResult.Append(this.processNode(childNode, query, request, result, user));
                    }
                }
                return templateResult;//.ToString();
            }
            else
            {
                AIMLTagHandler tagHandler = null;
                tagHandler = this.getBespokeTags(user, query, request, result, node);
                if (object.Equals(null, tagHandler))
                {
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
                if (object.Equals(null, tagHandler))
                {
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
                        return tagHandler.Transform();
                    }
                    else
                    {
                        Unifiable resultNodeInnerXML = tagHandler.Transform();
                        XmlNode resultNode = AIMLTagHandler.getNode(String.Format("<node>{0}</node>", resultNodeInnerXML));
                        if (resultNode.HasChildNodes)
                        {
                            UUnifiable recursiveResult = new UUnifiable();
                            // recursively check
                            foreach (XmlNode childNode in resultNode.ChildNodes)
                            {
                                recursiveResult.Append(this.processNode(childNode, query, request, result, user));
                            }
                            return recursiveResult.ToString();
                        }
                        else
                        {
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
            Unifiable message = @"Dear Botmaster,

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
            UUnifiable paths = new UUnifiable();
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
                Object oresult = access.converseList("(list " + cmd + ")").first();
                Console.WriteLine( " => " + oresult);
                while (oresult is CycList)
                {
                    CycList lresuult = (CycList) oresult;



                    if (lresuult.first() is CycSymbol)
                    {
                        oresult = lresuult; //lresuult.rest();
                        break;
                    }
                    else
                        if (lresuult.first() is CycVariable)
                            oresult = lresuult.rest();
                        else
                            oresult = lresuult.first();
                }
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
            if (term.Length < 0) return false;
            if (term == "NIL") return false;
            if (filter.Length > 0)
            {
                if (filter == "NIL") return true;
                if (this.EvalSubL(String.Format("(ask-template 'T '(#$isa {0} {1}) #$EverythingPSC)", term, Cyclify(filter)),null) == "NIL")
                    return false;
            }
            return true;
        }

        internal Unifiable Paraphrase(Unifiable text)
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
                    return String.Empty;
                }
            }
            if (langu == "subl") return EvalSubL(cmd, null);
            return s;
            
        }

        internal Unifiable Cyclify(Unifiable mt)
        {
            mt = mt.Trim();
            if (mt.Length == 0) return mt;
            if (System.Char.IsLetter(mt.ToCharArray()[0])) return "#$"+mt;
            return mt;

            if (System.Char.IsDigit(mt.ToCharArray()[0])) return mt;
            if (mt.StartsWith("(") || mt.StartsWith("#$")) return mt;
            return "#$" + mt;
        }

        static readonly Dictionary<Unifiable,SystemExecHandler> ExecuteHandlers = new Dictionary<Unifiable, SystemExecHandler>();
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
