using System;
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
using AltAIMLbot.Utils;
using AltAIMLParser;
using LAIR.ResourceAPIs.WordNet;
using MushDLR223.ScriptEngines;
using MushDLR223.Utilities;
using MushDLR223.Virtualization;
using org.opencyc.api;
#if USE_SWIPROLOG
using PrologScriptEngine;
#endif
using RTParser.AIMLTagHandlers;
using RTParser.Database;
using RTParser.Utils;
using RTParser.Variables;
using RTParser.Web;
using Console = System.Console;
using UPath = RTParser.Unifiable;
using UList = System.Collections.Generic.List<RTParser.Utils.TemplateInfo>;
using AltAIMLbot;
using Gender = RTParser.Utils.Gender;
using MasterRequest = AltAIMLParser.Request;

namespace RTParser
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
    public partial class AltBot : StaticAIMLUtils, IChatterBot
    {
        /* public static implicit operator AltBot(AltBot ab)
         {
             return ab.TheAltBot;
         }
         public static implicit operator AltBot(AltBot rtp)
         {
             return rtp.TheAltBot ?? rtp.servitor.curBot;
         }
         */
        public static bool IncludeMeNeValue;
        public static Dictionary<string, AltBot> Robots = new Dictionary<string, AltBot>();

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
        public static AltBot FindRobot(string text)
        {
            AltBot robot;
            lock (Robots)
            {
                if (TryGetValueLocked(Robots, Robots, text, out robot))
                {
                    return robot;
                }
                return null;
            }
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
                    Unifiable lts = GlobalSettings.grabSetting("ListeningToSelf");
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
                    Unifiable lts = GlobalSettings.grabSetting("ProcessHeardPreds");
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
                s += " name=" + GlobalSettings.grabSetting("name") + " (" + NamePath + ")";
            }
            if (!string.IsNullOrEmpty(NamePath)) return s + " NamePath=" + NamePath;
            return s;
        }

        /// <summary>
        /// Will ensure the same loader options are used between loaders
        /// </summary>
        public bool StaticLoader = true;

        private User _botAsUser;
        public User BotAsUser
        {
            get
            {
                if (_botAsUser == null)
                {
                    _botAsUser = FindOrCreateUser(NameAsSet);
                }
                return _botAsUser;
            }
        }
        public User ExemplarUser;
        public string NamePath;
        public string NameAsSet;


        //public Request BotAsRequestUsed = null;
        public Request GetBotRequest(string s)
        {
            var botAsUser1 = BotAsUser ?? LastUser;
            s = Trim(s);
            if (!s.StartsWith("<")) s = "<!-- " + s.Replace("<!--", "<#").Replace("-->", "#>") + " -->";
            var r = new MasterRequest(s, botAsUser1, Unifiable.EnglishNothing, botAsUser1, this, null,
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

        private AIMLLoaderU _loader;
        private AIMLLoaderU _loaderOnceLeast;
        public AIMLLoaderU Loader
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

        #region Attributes

        public List<CrossAppDomainDelegate> ReloadHooks = new List<CrossAppDomainDelegate>();

        /// <summary>
        /// A dictionary object that looks after all the settings associated with this Proccessor
        /// </summary>
        //public SettingsDictionary GlobalSettings { get { return  TheAltBot.GlobalSettings; } }

        /// <summary>
        /// A dictionary object that looks after all the settings in all processors
        /// </summary>
        public static SettingsDictionary SharedGlobalSettings;
        #endregion

        /// <summary>
        /// Output substitutions that take place before the bot speaks
        /// </summary>
        public static SettingsDictionary OutputSubstitutions;

        /// <summary>
        /// A dictionary of all settings from anyone .. just a fallback
        /// </summary>
        public SettingsDictionary EnginePreds;

        readonly public TagHandlerProcessor TagHandling = new TagHandlerProcessor();

        /// <summary>
        /// A list of Topic states that are set currently (for use of guarding content)
        /// </summary>
        public List<Unifiable> CurrentStates = new List<Unifiable>();



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

        public Servitor servitor
        {
            get { return myServitor; }
            set { myServitor = value; }
        }
        public bool useServitor
        {
            get { return true; }
        }
        public bool useNonServitor = false;
        public void sayConsole(string message)
        {
            //Default output
            Console.WriteLine("SERVITOR SAYS:{0}", message);
        }
        public void reloadServitor()
        {
            servitor.NeedsLoad = false;
            string rapDir = PersonalizePath(GlobalSettings.grabSetting("rapstore"));
            servitor.rapStoreDirectory = rapDir;

            string servRoot = GlobalSettings.grabSetting("serverRoot");
            if ((servRoot != null) && (servRoot.Length > 7))
            {
                WebServitor.serverRoot = servRoot;
            }

            string rapstorSL = GlobalSettings.grabSetting("rapstoreslices");
            if ((rapstorSL != null))
            {
                servitor.rapStoreSlices = int.Parse(rapstorSL);
            }
            string rapstorTL = GlobalSettings.grabSetting("rapstoretrunklevel");
            if ((rapstorTL != null))
            {
                servitor.rapStoreTrunkLevel = int.Parse(rapstorTL);
            }


            string behaviorcache = PersonalizePath(GlobalSettings.grabSetting("behaviorcache"));
            if ((behaviorcache != null) && (behaviorcache.Length > 0))
            {
                servitor.curBot.myBehaviors.persistantDirectory = behaviorcache;
            }

            servitor.curBot.loadCrons();
            if (servitor.skiploadingServitorState)
            {
                return;
            }

            string graphcache = PersonalizePath(GlobalSettings.grabSetting("graphcache"));
            if (File.Exists(graphcache))
            {
                try
                {
                    bool localCritical = servitor.curBot.inCritical;
                    servitor.curBot.inCritical = true;
                    servitor.loadAIMLFromFile(graphcache);
                    servitor.curBot.inCritical = localCritical;
                }
                catch (Exception e)
                {
                    Console.WriteLine("***** ERR reloadServitor():{0} ERR ******", e.Message);
                }
                //servitor.skiploading = true;
                Console.WriteLine("***** reloadServitor():{0} COMPLETE ******", graphcache);
                return;
            }
            else
            {
                Console.WriteLine("No file exists for reloadServitor(graphcache)");
            }

            string servitorbin = GlobalSettings.grabSetting("servitorbin");
            if (File.Exists(servitorbin))
            {
                servitor.loadFromBinaryFile(servitorbin);
                servitor.skiploadingServitorState = true;
            }
            else
            {
                servitor.skiploadingAimlFiles = false;
                Console.WriteLine("No file exists for reloadServitor()");
            }
        }

        public void saveServitor()
        {
            try
            {
                saveServitor0();
            }
            catch (Exception e)
            {
                Console.WriteLine("ERROR: saveServitor()" + e);
            }
        }
        public void saveServitor0()
        {
            if (servitor == null) return;
            string rapDir = PersonalizePath(GlobalSettings.grabSetting("rapstore"));
            servitor.rapStoreDirectory = rapDir;

            List<string> allPaths = new List<string>();
            List<string> allCrons = new List<string>();
            List<string> allBehaviors = new List<string>();
            //servitor.curBot.Graphmaster.collectPaths("",allPaths);
            //File.WriteAllLines(@"./aiml/graphmap.txt", allPaths.ToArray());
            servitor.curBot.saveCrons();

            string graphcache = GlobalSettings.grabSetting("graphcache");
            graphcache = PersonalizePath(graphcache);
            if (File.Exists(graphcache)) // Always not overwrite for now
            {
                Console.WriteLine("***** saveServitor():{0} SKIPPING ******", graphcache);
                if (servitor != null) servitor.skiploadingServitorState = true;
                return;
            }
            string[] header = { "<?xml version=\"1.0\" encoding=\"UTF-8\"?>", "<aiml version=\"1.0\">", " <state name=\"*\">" };
            string[] footer = { " </state>", "</aiml>" };
            //@todo fix servitor.curBot.Graphmaster.collectFullPaths("", allPaths);
            allCrons = servitor.curBot.myCron.cronXmlList();
            allBehaviors = servitor.curBot.myBehaviors.behaviorXmlList();

            //StreamWriter sw = File.CreateText(@"./aiml/servitorgraphmap.aiml");
            if (graphcache != null && !File.Exists(graphcache))
            {
                StreamWriter sw = null;
                try
                {
                    sw = File.CreateText(graphcache);
                    foreach (string line in header)
                    {
                        sw.WriteLine(line);
                    }
                    foreach (string line in allCrons)
                    {
                        sw.WriteLine(line);
                    }
                    foreach (string line in allPaths.ToArray())
                    {
                        sw.WriteLine(line);
                    }
                    foreach (string line in allBehaviors)
                    {
                        //    sw.WriteLine(line);
                    }

                    foreach (string line in footer)
                    {
                        sw.WriteLine(line);
                    }
                    sw.Flush();
                    sw.Close();
                }
                catch (Exception e)
                {

                }
                finally
                {
                    if (sw != null)
                        sw.Dispose();
                }
            }

            string servitorbin = GlobalSettings.grabSetting("servitorbin");
            if (servitorbin != null)
            {
                if (!File.Exists(servitorbin))
                {
                    if (servitorbin != null) servitor.saveToBinaryFile(servitorbin);
                    servitor.skiploadingServitorState = true;
                }
                else
                {
                    Console.WriteLine("Skipping saveServitor(): already exists!!!");
                }
            }

        }
        public void updateServitor2RTP(User activeUser)
        {
            if (useServitor == false) return;
            updateServitor2RTP();
            //User specific code (ALTBOT USER->RTPUSER  )
            try
            {
                if (activeUser.Predicates != null)
                    foreach (string key in servitor.curUser.Predicates.Keys)
                    {
                        string v = servitor.curUser.Predicates.grabSetting(key);
                        activeUser.Predicates.updateSetting(key, v);
                        /// Console.WriteLine("ALT->RTP Predicates[{0}] = {1}", key, v);
                    }

                int rcount = servitor.curUser.SailentResultCount;
                for (int n = 0; n < rcount; n++)
                {

                    AltAIMLbot.Result historicResult = servitor.curUser.GetResult(n);
                    if (historicResult == null) continue;
                    lock (historicResult.OutputSentences)
                    {
                        for (int sent = 0; sent < historicResult.OutputSentences.Count; sent++)
                        {
                            string data = historicResult.OutputSentences[sent];
                            activeUser.setOutputSentence(n, sent, data);
                            Console.WriteLine("ALT->RTP setOutputSentence[{0},{1}] = {2}", n, sent, data);
                        }
                    }
                }
            }
            catch (Exception e)
            {
                Console.WriteLine(" **** ERR : {0} {1} ", e.Message, e.StackTrace);
            }
        }


        public void updateServitor2RTP()
        {
            startServitor();
            if (servitor.NeedsLoad)
            {
                reloadServitor();
            }
        }
        public void startServitor()
        {
            if (useServitor == false) return;
            if (_myServitor == null)
            {

                myServitor = new Servitor(this, null);
                servitor.curBot = this;
                servitor.curBot.sayProcessor = new sayProcessorDelegate(sayConsole);
            }
            if (myServitor.NeedsStarted)
            {
                myServitor.Start(sayProcessor);
            }

        }
        public void updateRTP2Sevitor(User activeUser)
        {
            if (useServitor == false) return;
            updateRTP2Sevitor();
            try
            {
                //User specific code (RTPUSER -> ALTBOT USER)
                string that = activeUser.getThat(activeUser);
                servitor.curUser.setUserID(activeUser.UserID);
                if (activeUser.Predicates != null)
                    foreach (string key in activeUser.Predicates.Keys)
                    {
                        string v = activeUser.Predicates[key];
                        servitor.curUser.Predicates.updateSetting(key, v);
                        ///Console.WriteLine("RTP->ALT Predicates[{0}] = {1}", key, v);
                    }

                int rcount = activeUser.SailentResultCount;
                for (int n = 0; n < rcount; n++)
                {

                    Result historicResult = activeUser.GetResult(n, true, null);
                    if (historicResult == null) continue;
                    for (int sent = 0; sent < historicResult.OutputSentenceCount; sent++)
                    {
                        string data = historicResult.GetOutputSentence(sent);
                        servitor.curUser.setOutputSentence(n, sent, data);
                        Console.WriteLine("RTP->ALT setOutputSentence[{0},{1}] = {2}", n, sent, data);
                    }
                }
                // An alternate way is at the request level
                //   Result historicResult = activeUser.GetResult(n, true, activeUser);
                //   AltAIMLbot .Result duplicateResult = new AltAIMLbot.Result (servitor.curUser, servitor .curBot
                //                                              ,servitor.curBot.LastRequest);
            }
            catch (Exception e)
            {
                Console.WriteLine(" **** ERR : {0} {1} ", e.Message, e.StackTrace);
            }

        }
        public void updateRTP2Sevitor()
        {
            if (useServitor == false) return;
            updateServitor2RTP();
            // fill in the blanks
            //servitor.curBot.AdminEmail = this.AdminEmail;
            //servitor.curBot.conversationStack = this.conversationStack;
            //servitor.curBot.isAcceptingUserInput = this.isAcceptingUserInput;
            //servitor.curBot.LastLogMessage = this.LastLogMessage;
            //servitor.curBot.MaxThatSize = this.MaxThatSize;
            //servitor.curBot.StartedOn = this.StartedOn;
            //servitor.curBot.TrustAIML = this.TrustAIML;
            //servitor.curBot.StartedOn = this.StartedOn;
            servitor.curBot.GlobalSettings.updateSetting("aimldirectory", PathToAIML);

            if (SharedGlobalSettings != null)
            {
                foreach (string key in LockInfo.CopyOf(SharedGlobalSettings.Keys))
                {
                    string v = SharedGlobalSettings[key];
                    // servitor.curBot.GlobalSettings.updateSetting(key, v);
                    servitor.curBot.setBBHash0(key, v);
                }
            }

            if (GlobalSettings != null && GlobalSettings != SharedGlobalSettings)
            {
                foreach (string key in LockInfo.CopyOf(GlobalSettings.Keys))
                {
                    string v = GlobalSettings[key];
                    //servitor.curBot.GlobalSettings.updateSetting(key, v);
                    servitor.curBot.setBBHash0(key, v);
                }
            }

            /*if ((GenderSubstitutions != null) && (servitor.curBot.GenderSubstitutions.Count != GenderSubstitutions.Count))
            foreach (string key in GenderSubstitutions.Keys)
            {
                string v = GenderSubstitutions[key];
                servitor.curBot.GenderSubstitutions.updateSetting(key, v);
            }

            if ((Person2Substitutions != null)
                && (servitor.curBot.Person2Substitutions.Count != Person2Substitutions.Count))
                foreach (string key in Person2Substitutions.Keys)
            {
                string v = Person2Substitutions[key];
                servitor.curBot.Person2Substitutions.updateSetting(key, v);
            }

            if ((PersonSubstitutions != null)
                && (servitor.curBot.PersonSubstitutions.Count != PersonSubstitutions.Count))
                foreach (string key in PersonSubstitutions.Keys)
            {
                string v = PersonSubstitutions[key];
                servitor.curBot.PersonSubstitutions.updateSetting(key, v);
            }

            if ((InputSubstitutions != null)
                && (servitor.curBot.InputSubstitutions.Count != InputSubstitutions.Count))
                foreach (string key in InputSubstitutions.Keys)
            {
                string v = InputSubstitutions[key];
                servitor.curBot.InputSubstitutions.updateSetting(key, v);
            }

            if ((DefaultPredicates != null)
                && (servitor.curBot.DefaultPredicates.Count != DefaultPredicates.Count))
                foreach (string key in DefaultPredicates.Keys)
            {
                string v = DefaultPredicates[key];
                servitor.curBot.DefaultPredicates.updateSetting(key, v);
            }
             * */


        }
        /// <summary>
        /// Ctor
        /// </summary>
        //protected AltBot()
        public AltBot()
            : base()
        {
            myBehaviors = new BehaviorSet(this);
            servitor = new Servitor(this, null);
            
            qsbase = QuerySettings.CogbotDefaults;
            AltBotcommands = new AltBotCommands(this);
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

                _RuntimeDirectories.Remove(directory);
                _RuntimeDirectories.Insert(0, directory);
                // ReSharper disable ConditionIsAlwaysTrueOrFalse
                return found ? directory : null;
                // ReSharper restore ConditionIsAlwaysTrueOrFalse
            }
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

        private bool initialSettingsLoaded = false;
        private readonly object initialSettingsLoadedLock = new object();
        /// <summary>
        /// Loads AIML from .aiml files into the graphmaster "brain" of the Proccessor
        /// </summary>
        public void loadGlobalBotSettings()
        {
            lock (initialSettingsLoadedLock)
            {
                if (initialSettingsLoaded) return;
                initialSettingsLoaded = true;
                setup();
                Request globalSettingsRequest = GetBotRequest("-loadAimlFromDefaults-");
                loadConfigs(this, PathToConfigFiles, globalSettingsRequest);
                loadConfigs(this, HostSystem.Combine(PathToAIML, "shared_aiml"), globalSettingsRequest);
                //startServitor();
                this.StartHttpServer();
                EnsureDefaultUsers();
            }
            //MyBot.GlobalSettings.addSetting("name", client.BotLoginParams.FirstName+ " " + client.BotLoginParams.LastName);
        }

        public void loadAIMLFromDefaults()
        {
            lock (initialSettingsLoadedLock)
            {
                loadAIMLFromFiles();
            }
        }


        public bool LoadIntoOldAIML
        {
            get { return ((useServitor && useNonServitor) || (!useServitor)); }
        }
        /// <summary>
        /// Loads AIML from .aiml files into the graphmaster "brain" of the Proccessor
        /// </summary>
        public void loadAIMLFromURI(string path, Request request)
        {
            if (useServitor)
            {
                updateServitor2RTP();
                if (HostSystem.FileOrDirExists(path))
                {
                    servitor.loadAIMLFromFiles(path);
                }
            }
            if (!LoadIntoOldAIML) return;
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
        private void loadAIMLAndSettings(string path, bool skipSettings)
        {
            Request request = GetBotRequest("-loadAIMLAndSettings-" + path + "-");
            request.LoadingFrom = null;
            bool prev = request.GraphsAcceptingUserInput;
            try
            {
                request.GraphsAcceptingUserInput = false;
                // maybe loads settings files if they are there
                string settings = HostSystem.Combine(path, "Settings.xml");
                if (HostSystem.FileExists(settings)) loadSettings(settings, request);
                //loading settings first
                if (!skipSettings) loadConfigs(this, path, request);

                //this loads into servator
                loadAIMLFromURI(path, request);

            }
            finally
            {
                request.GraphsAcceptingUserInput = prev;
            }
        }


        internal AIMLLoaderU GetLoader(Request request)
        {
            AltBot bot = this;
            AIMLLoaderU loader = bot.Loader;
            if (!bot.StaticLoader || loader == null)
            {
                loader = new AIMLLoaderU(bot, request);
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
            if (useServitor)
            {
                if (HostSystem.FileExists(filename.ToString()))
                {
                    servitor.curBot.loadAIMLFromXML(newAIML, filename.ToString());
                    return;
                }
            }
            bool prev = request.GraphsAcceptingUserInput;
            try
            {
                request.GraphsAcceptingUserInput = false;
                AIMLLoaderU loader = GetLoader(request);
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
        private void setupDictionaries()
        {
            bool prev = isAcceptingUserInput;
            try
            {

                //isAcceptingUserInput = false;
                RegisterDictionary("chat.relationprops", RelationMetaProps);
                RegisterDictionary("meta", RelationMetaProps);
                RegisterDictionary("metaprops", RelationMetaProps);

                RegisterDictionary("bot.globalsettings", GlobalSettings);
                GlobalSettings.InsertMetaProvider(GetRelationMetaProps);

                RegisterSubstitutions("gender", GenderSubstitutions);
                RegisterSubstitutions("person2", Person2Substitutions);
                RegisterSubstitutions("person", PersonSubstitutions);
                InputSubstitutions["issubsts"] = true.ToString();
                InputSubstitutions.IsTraced = true;
                RegisterSubstitutions("input", InputSubstitutions);
                OutputSubstitutions = MakeSubstsDictionary("nl.substitutions.output");
                RegisterSubstitutions("output", OutputSubstitutions);


                //ParentProvider provider = new ParentProvider(() => GlobalSettings);
                //DefaultPredicates = new SettingsDictionary("bot.defaultpredicates", this, null);
                //DefaultPredicates = new SettingsDictionary("defaults", this, null);
                RegisterDictionary("bot.defaultpredicates", DefaultPredicates);
                RegisterDictionary("defaults", DefaultPredicates);
                DefaultPredicates.InsertMetaProvider(GetRelationMetaProps);
                RegisterDictionary("heard", HeardPredicates);

                RegisterDictionary("predicates", AllUserPreds);
                EnginePreds = AllUserPreds;
                RegisterDictionary("enginepreds", EnginePreds);

                AllUserPreds.InsertMetaProvider(GetRelationMetaProps);


                User guser = ExemplarUser = LastUser = ExemplarUser ?? FindOrCreateUser("Default");
                lock (microBotUsersLock)
                {
                    AllDictionaries["globalpreds"] = guser;
                }
                guser.IsRoleAcct = true;
                guser.Predicates.clearSettings();
                guser.Predicates.clearHierarchy();
                guser.Predicates.InsertFallback(() => HeardPredicates);
                //guser.Predicates.maskSetting("name");
                ///guser.Predicates.maskSetting("currentaction");
                //guser.Predicates.maskSetting("id");

                // try a safe default setting for the settings xml file
                // Checks for some important default settings
                //GlobalSettings.IsIdentityReadOnly = false;
                SetSaneGlobals(GlobalSettings);
                string pathToSettings = HostSystem.Combine(RuntimeDirectory,
                                                           HostSystem.Combine("config", "Settings.xml"));
                Request request = GetBotRequest("<!-- Loads settings from: '" + pathToSettings + "' -->");
                loadSettings(pathToSettings, request);
                // RE-Checks for some important default settings
                SetSaneGlobals(GlobalSettings);
                //SetupConveration();
                //GlobalSettings.SetReadonly( = true;
            }
            finally
            {
                isAcceptingUserInput = prev;
            }
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
            thiz.InputSubstitutions.loadSettings(HostSystemCombine(pathToSettings, "substitutions2.xml"), request);
            thiz.InputSubstitutions.loadSettings(HostSystemCombine(pathToSettings, "substitutions3.xml"), request);
            thiz.InputSubstitutions.IsTraced = true;

            // Grab the splitters for this Proccessor
            thiz.loadSplitters(HostSystemCombine(pathToSettings, GlobalSettings.grabSetting("splittersfile")));
            thiz.loadSplitters(HostSystemCombine(pathToSettings, GlobalSettings.grabSetting("sentence-splitters")));
            thiz.loadSplitters(HostSystemCombine(pathToSettings, "sentence-splitters.xml"));

            // genformat.xml
            thiz.RelationMetaProps.loadSettings(HostSystemCombine(pathToSettings, "genformat.xml"), request);


            User guser = thiz.ExemplarUser ?? thiz.FindUser("Default");
            if (HostSystem.FileExists(HostSystem.Combine(pathToSettings, "globalpred" + "s.xml")))
            {
                SettingsDictionaryReal.loadSettingsNow(guser.Predicates, pathToSettings, "globalpred" + "s.xml",
                                                   SettingsPolicy.Default, request);
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
        public void loadSettings(string pathToSettings, Request request)
        {
            if (request == null) request = GetBotRequest("<!-- Loads settings from: '" + pathToSettings + "' -->");
            ReloadHooks.Add(() => loadSettings(pathToSettings, request));
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
        public void saveToBinaryFile1(string path)
        {
            if (this.noSerialzation) return;
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
        public void loadFromBinaryFile1(string path)
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


        internal readonly Dictionary<string, SystemExecHandler> ExecuteHandlers =
            new Dictionary<string, SystemExecHandler>();

        public void AddExcuteHandler(string lang, SystemExecHandler handler)
        {
            lang = ToLower(Trim(lang));
            lock (ExecuteHandlers) ExecuteHandlers[lang] = handler;
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
                if (GlobalSettings==null || !GlobalSettings.containsSettingCalled("notopic")) return Unifiable.EnglishNothing;
                return GlobalSettings.grabSetting("notopic");
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
        public Dictionary<string, GraphMaster> LocalGraphsByName
        {
            get { return Graphs; }
        }

        public static CycDatabase TheCycS;
        public CycDatabase TheCyc
        {
            get
            {
                return TheCycS;
            }
        }
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
            graphPath = "default";
            graphPath = GraphMaster.DeAliasGraphName(graphPath);
            if (false && !graphPath.Contains("_to_"))
            {
                graphPath = ToLower(ConsolidSpaces(Trim(graphPath + "_to_" + this.NamePath)));
            }
            GraphMaster g;
            lock (GraphsByName)
            {
                if (LocalGraphsByName.TryGetValue(graphPath, out g))
                {
                    return g;
                }
                g = GraphsByName[graphPath] = GraphMaster.FindOrCreate(graphPath, this);
                //GraphMaster dtob = Utils.GraphMaster.FindOrCreate("default_to_" + this.NamePath);
                //g.AddGenlMT(dtob, writeToLog);
                //ㄴdtob.AddGenlMT(Utils.GraphMaster.FindOrCreate("default"), writeToLog);
            }
            return g;
        }

        static public GraphMaster FindGlobalGraph(string graphPath)
        {
            graphPath = GraphMaster.DeAliasGraphName(graphPath);
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
                    throw new NullReferenceException("graphPath=" + graphPath);
                }
                return current;
            }

            graphPath = GraphMaster.DeAliasGraphName(graphPath);
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
                    g = GraphsByName[graphPath] = GraphMaster.FindOrCreate(graphPath, this);
                }
            }
            return g;
        }

        public GraphMaster FindGraph(string graphPath, GraphMaster current)
        {
            if (string.IsNullOrEmpty(graphPath) || graphPath == "*") return current;
            graphPath = GraphMaster.DeAliasGraphName(graphPath);
            if (graphPath == null)
            {
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

            string orig = graphPath;
            graphPath = ToScriptableName(graphPath);

            if (graphPath == "current" || graphPath == "")
            {
                return current;
            }

            if (graphPath == "parent" || graphPath == "parallel")
            {
                if (current == null) return null;
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
            Unifiable ret = user.Predicates.grabSetting("mt");
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
                prologEngine.connectMT(GlobalSettings.NameSpace, AllUserPreds.NameSpace);
                prologEngine.connectMT(BotAsUser.Predicates.NameSpace, AllUserPreds.NameSpace);
                prologEngine.connectMT(BotAsUser.Predicates.NameSpace, GlobalSettings.NameSpace);
                prologEngine.connectMT(LastUser.Predicates.NameSpace, AllUserPreds.NameSpace);
                prologEngine.connectMT(DefaultPredicates.NameSpace, AllUserPreds.NameSpace);
                updateRTP2Sevitor();
                saveServitor();
            }
        }

        public string LoadPersonalDirectory(string myName, bool loadXML, bool loadAiml)
        {
            ReloadHooks.Add(() => LoadPersonalDirectory(myName, loadXML, loadAiml));
            string loaded = null;

            // this is the personal "config file" only.. aiml stored elsewhere
            string file = HostSystem.Combine("config", myName);
            Request request = GetBotRequest("loading personal directory " + myName);
            if (HostSystem.DirExists(file))
            {
                loaded = file;
                loadSettingsFileAndDir(file, request, loadXML, loadXML);
            }

            file = HostSystem.Combine("aiml", myName);
            if (HostSystem.DirExists(file))
            {
                UsePersonalDir(file, loadXML, loadAiml);
                ;
                loaded = file;
            }

            // this is the personal "config file" only.. aiml stored elsewhere
            file = HostSystem.Combine(myName, "config");
            if (HostSystem.DirExists(file))
            {
                loaded = file;
                loadSettingsFileAndDir(file, request, loadXML, loadXML);
            }

            file = HostSystem.Combine(myName, "aiml");
            if (HostSystem.DirExists(file))
            {
                UsePersonalDir(file, loadXML, loadAiml);
                ;
                loaded = file;
            }
            return loaded;
        }

        private void loadSettingsFileAndDir(string file, Request request, bool loadSets, bool loadXML)
        {
            writeToLog("LoadPersonalDirectories: '{0}'", file);
            if (loadXML) loadConfigs(this, file, request);
            if (loadSets) loadSettings(HostSystem.Combine(file, "Settings.xml"), request);
        }

        public void UsePersonalDir(string file, bool loadXML, bool loadAiml)
        {
            if (!HostSystem.DirExists(file))
            {
                writeToLog("ERROR - cannot use non existent personal dir = " + file);
                return;
            }
            PushSearchPath(file);
            string s = string.Format("-LoadPersonalDirectories: '{0}'-", file);
            Request request = GetBotRequest(s);
            request.LoadingFrom = file;
            writeToLog(s);
            bool prev = request.GraphsAcceptingUserInput;
            try
            {
                // loading of personal configs must be done before and after the AIML files
                if (loadXML) loadConfigs(this, file, request);
                request.GraphsAcceptingUserInput = false;
                if (loadAiml) loadAIMLFromURI(file, request);
                foreach (string s1 in HostSystem.GetFiles(file, "Settings*.xml"))
                {
                    if (loadXML) loadSettings(s1, request);
                }
                if (loadXML) loadConfigs(this, file, request);
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
            string ret;
            lock (IsNameSetLock) lock (OnBotCreatedHooks)
                {
                    ret = SetNameForConfig(myName);
                    //return UserOper(() => SetName0(myName), writeDebugLine);
                }
            startServitor();
            updateServitor2RTP();
            LoadPersonality();
            if (useServitor)
            {
                saveServitor();
            }
            return ret;
        }

        private string IsNameSet = null;
        private object IsNameSetLock = new object();
        private string SetNameForConfig(string myName)
        {
            if (IsNameSet == myName && _botAsUser != null)
            {
                return BotAsUser.UserDirectory;
            }
            IsNameSet = myName;
            Robots[myName] = this;
            NameAsSet = myName;
            NamePath = ToScriptableName(NameAsSet);

            loadGlobalBotSettings();
            //char s1 = myName[1];
            //new AIMLbot.User("heardselfsay", this)
            var thisBotAsUser = _botAsUser = BotAsUser ?? FindOrCreateUser(myName);
            this.BotAsUser.UserName = myName;// thisBotAsUser;

            ExternalIntern("BotAsUser", thisBotAsUser);
            thisBotAsUser.IsRoleAcct = true;
            SharedGlobalSettings = AllUserPreds;// this.GlobalSettings;
            thisBotAsUser.Predicates.InsertFallback(() => SharedGlobalSettings);
            thisBotAsUser.Predicates.InsertFallback(() => AllUserPreds);
            if (SharedGlobalSettings != GlobalSettings && SharedGlobalSettings != AllUserPreds)
            {
                AllUserPreds.InsertFallback(() => SharedGlobalSettings);
            }

            GlobalSettings.IsTraced = true;
            thisBotAsUser.Predicates.InsertProvider(() => GlobalSettings);
            //BotAsUser.UserDirectory = "aiml/users/heardselfsay";
            //BotAsUser.UserID = "heardselfsay";
            //BotAsUser.UserName = "heardselfsay";
            //BotUsers["heardselfsay"] = BotAsUser;            
            thisBotAsUser.UserName = myName;
            AllDictionaries["bot"] = thisBotAsUser.Predicates;
            thisBotAsUser.removeSetting("userdir");

            thisBotAsUser.UserID = NamePath;
            GlobalSettings.addSetting("name", String.Format("{0}", myName));

            //var OnTaskAtATimeHandler = HeardSelfSayQueue = thisBotAsUser.OnTaskAtATimeHandler;
            //OnTaskAtATimeHandler.Name = "TaskQueue For " + myName;

            //thisBotAsUser.SaveDirectory(thisBotAsUser.UserDirectory);

            string shared = LoadPersonalDirectory("shared_aiml", true, false);
            string official = LoadPersonalDirectory(NamePath, true, false);
            if (string.IsNullOrEmpty(official))
            {
                official = LoadPersonalDirectory("default_bot", true, false);
            }
            PersonalAiml = official;
            thisBotAsUser.SaveDirectory(thisBotAsUser.UserDirectory);
            AddExcuteHandler(NamePath, ChatWithThisBot);
            RunOnBotCreatedHooks();
            return official ?? thisBotAsUser.UserDirectory;
        }

        private void RunOnBotCreatedHooks()
        {
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
        }

        public bool needAimlFilesLoaded = true;
        public void LoadPersonality()
        {
            if (!needAimlFilesLoaded) return;
            needAimlFilesLoaded = false;
            string official = PersonalAiml;
            LoadPersonalDirectory("shared_aiml", false, true);
            if (!string.IsNullOrEmpty(official))
            {
                loadAIMLFromFiles(official);
            }
            RunOnBotCreatedHooks();
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
                    tc.Add(typeof(MasterRequest));
                    // ReSharper disable AssignNullToNotNullAttribute
                    tc.Add(typeof(MasterResult).BaseType);
                    // ReSharper restore AssignNullToNotNullAttribute
                    tc.Add(typeof(Request));
                }

                TagHandlerProcessor.InitTagHandlers();

                if (StaticInitStarted) return;
                StaticInitStarted = true;
                TheListenerGraph = GraphMaster.FindOrCreate("listener", null);
                TheListenerGraph.SilentTagsInPutParallel = false;
                // var defaultGraph = GraphsByName["default"] = GraphMaster.FindOrCreate("default");
                // defaultGraph.RemovePreviousTemplatesFromNodes = false;               
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

        readonly public static OutputDelegate writeDebugLine = writeDebugLine_0_;
        internal static void writeDebugLine_0_(string message, params object[] args)
        {
            bool printIt = false;
            lock (LoggedWords)
            {
                printIt = LoggedWords.writeDebugLine(DLRConsole.SystemWriteLine, message, args);
            }
            //
            {
                try
                {
                    bool wasStopped = true;
                    string real = SafeFormat(message, args);
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
            old = Trim(ToLower(old));
            next = Trim(ToLower(next));
            return FindUser0(old) == FindUser0(next);
        }

        private List<string> _RuntimeDirectories;
        ICollectionRequester _objr;
        private readonly List<Action> PostObjectRequesterSet = new List<Action>();
        //private AltBot TheAltBot;

        public ICollectionRequester ObjectRequester
        {
            get
            {
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
        public string BotUserID
        {
            get
            {
                if (BotAsUser != null) return BotAsUser.UserID;
                SettingsDictionary dict = GlobalSettings;
                if (dict != null)
                {
                    Unifiable botid = dict.grabSetting("id");
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
                return BotUserID ?? "-BOT-ID-NULL-";
            }
            set { throw new NotImplementedException(); }
        }

        public ISettingsDictionary Predicates
        {
            get { return GlobalSettings; }
        }

        #endregion

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
                                SettingsDictionaryReal.loadSettingsNow(dictionary, null, named, SettingsPolicy.Default, r);
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

        private void RegisterSubstitutions(string named, SettingsDictionary dict0)
        {
            SettingsDictionaryReal dict = SettingsDictionaryReal.ToSettingsDictionary(dict0);
            dict.IsTraced = false;
            dict.IsSubsts = true;
            RegisterDictionary("substitutions" + "." + named, dict);
            string dictNameSpace = "nl.substitutions" + "." + named;
            RegisterDictionary(dictNameSpace, dict);
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
            foreach (GraphMaster graph in SetOfGraphs)
            {
                total += graph.RunLowMemHooks();
            }
            return total;
        }
    }
}