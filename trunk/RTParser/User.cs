using System;
using System.Collections;
using System.Collections.Generic;
using System.IO;
using System.Runtime.CompilerServices;
using System.Text;
using System.Threading;
using com.hp.hpl.jena.graph;
using MushDLR223.ScriptEngines;
using MushDLR223.Utilities;
using RTParser;
using RTParser.AIMLTagHandlers;
using RTParser.Utils;
using MushDLR223.Virtualization;
using RTParser.Variables;

namespace RTParser
{
    /// <summary>
    /// Encapsulates information and history of a user who has interacted with the bot
    /// </summary>
    abstract public class User : StaticAIMLUtils, IDisposable, ISettingsDictionary
    {
        public static bool ThatIsStoredBetweenUsers = true;
        public readonly object QueryLock = new object();

        #region Attributes

        public ListAsSet<TemplateInfo> UsedTemplates = new ListAsSet<TemplateInfo>();
        public ListAsSet<TemplateInfo> DisabledTemplates = new ListAsSet<TemplateInfo>();
        public ICollection<GraphMaster> DisallowedGraphs = new HashSet<GraphMaster>();
        public ListAsSet<GraphQuery> AllQueries = new ListAsSet<GraphQuery>();

        public DateTime LastResponseGivenTime = DateTime.Now;
        public bool RespondToChat = true;
        public int MaxRespondToChatPerMinute = 10;
        public DateTime NameUsedOrGivenTime = DateTime.Now;

        public static int DefaultMaxResultsSaved = 5;
        public static bool NeverSaveUsers = false;
        public int MaxResultsSaved = DefaultMaxResultsSaved;
        public bool IsRoleAcct = false;

        /// <summary>
        /// The local instance of the GUID that identifies this user to the bot
        /// </summary>
        private Unifiable id;

        /// <summary>
        /// The bot this user is using
        /// </summary>
        public RTParser.RTPBot bot;

        public TaskQueueHandler OnTaskAtATimeHandler;
        public Timer SaveTimer;

        public void Enqueue(ThreadStart evt)
        {
            if (OnTaskAtATimeHandler == null)
            {
                OnTaskAtATimeHandler = new TaskQueueHandler("TaskQueue For " + UserID, 10);
                OnTaskAtATimeHandler.Start();
            }
            OnTaskAtATimeHandler.Enqueue(evt);
        }

        readonly object SaveLock = new object();
        private void SaveOften(object state)
        {
            if (IsRoleAcct) return;
            if (NeverSave) return;
            lock (SaveLock)
            {
                if (!needsSave)
                {
                    WriteLine("Skipping save");
                    return;
                }
                needsSave = false;
            }
            try
            {
                if (IsRoleAcct) return;
                SaveDirectory(_saveToDirectory);
            }
            catch (Exception exception)
            {
                WriteLine("ERROR saving " + exception);
            }
            lock (SaveLock)
            {
                needsSave = true;
            }
        }

        /// <summary>
        /// The grahmaster this user is using
        /// // this stil is not "listener"
        /// </summary>
        public GraphMaster ListeningGraph
        {
            get
            {
                if (Predicates.containsSettingCalled("graphname"))
                {
                    GraphMaster _Graph = null;
                    Predicates.IsTraced = false;
                    var v = Predicates.grabSettingNoDebug("graphname");
                    _Graph = bot.GetUserGraph(v);
                    if (_Graph != null)
                    {
                        return _Graph;
                    }
                    bot.writeToLog("ERROR CANT FIND graphname");
                    return bot.GraphMaster;
                }
                return bot.GraphMaster;
            }
            set
            {
                if (!Predicates.containsLocalCalled("graphname"))
                    Predicates.addSetting("graphname", value.ScriptingName);
                else Predicates.updateSetting("graphname", value.ScriptingName);
                GraphMaster lg = ListeningGraph;
                if (lg != value)
                {
                    bot.writeToLog("ERROR CANT FIND " + value.ScriptingName + " from " + lg);
                }
            }
        }
        public string GraphName
        {
            get
            {
                string qsbaseGraphName = qsbase.GraphName;
                if (qsbaseGraphName!=null)
                {
                    return qsbaseGraphName;
                }
                return ListeningGraph.ScriptingName;
            }
            set
            {
                ListeningGraph = bot.GetGraph(value, ListeningGraph);
                qsbase.GraphName = value;
            }
        }

        public int MaxInputs
        {
            get
            {
                if (Predicates.containsSettingCalled("maxinputs"))
                {
                    var mi = Predicates.grabSetting("maxinputs");
                    int miv;
                    if (int.TryParse(mi.AsString(), out miv))
                    {
                        return miv;
                    }
                }
                return 1;
            }
            set { Predicates.addSetting("maxinputs", "" + value); }
        }


        /// <summary>
        /// The GUID that identifies this user to the bot
        /// </summary>
        public string UserID
        {
            get
            {
                string uid = this.id;
                if (string.IsNullOrEmpty(uid))
                {
                    uid = Predicates.grabSettingNoDebug("id");
                }
                return uid;
            }
            set
            {
                if (value == null) return;
                string puser = UserID;
                if (puser != null && puser.ToLower() == value.ToLower())
                {
                    return;
                }
                if (!LegalNameCheck(value)) return;
                this.id = value;
                if (Predicates == null) return;
                var prev = Predicates.IsIdentityReadOnly;
                try
                {
                    Predicates.IsIdentityReadOnly = false;
                    Predicates.addSetting("id", value);
                }
                finally
                {
                    Predicates.IsIdentityReadOnly = prev;
                }
            }
        }

        private bool LegalNameCheck(string value)
        {
            if (value.ToLower() == "friend")
            {
                WriteLine("Friend: " + value);
                return false;
            }
            return true;
        }

        public string UserName
        {
            get
            {
                return GetValueORElse(this.Predicates, "name", () => UserID.Replace("_", " "));
            }

            set
            {
                if (value == null) return;
                string puser = UserName;
                if (puser != null && puser.ToLower() == value.ToLower())
                {
                    return;
                }
                if (!LegalNameCheck(value))
                {
                    return;
                }
                if (bot.IsLastKnownUser(value))
                {
                    return;
                }
                string saved = value.Replace("_", " ").Trim();
                if (saved.Length == 0) return;
                if (Predicates == null) return;
                var prev = Predicates.IsIdentityReadOnly;
                try
                {
                    Predicates.IsIdentityReadOnly = false;
                    Predicates.addSetting("name", value);
                }
                finally
                {
                    Predicates.IsIdentityReadOnly = prev;
                }
            }
        }

        private string GetValueORElse(ISettingsDictionary dictionary, string settingname, Func<String> func)
        {
            if (dictionary != null && dictionary.containsLocalCalled(settingname))
            {
                string value = dictionary.grabSetting(settingname);
                if (!Unifiable.IsNullOrEmpty(value)) return value.Trim();
            }
            return func();
        }

        /// <summary>
        /// A collection of all the result objects returned to the user in this session
        /// (in reverse order of time)
        /// </summary>
        private readonly List<Result> Results = new List<Result>();


        List<Unifiable> _topics = new List<Unifiable>();
        public IList<Unifiable> Topics
        {
            get
            {
                if (_topics.Count == 0) return new List<Unifiable>() { Topic };
                return _topics;
            }
        }

        /// <summary>
        /// the value of the "topic" predicate
        /// </summary>
        public Unifiable Topic
        {
            get
            {
                return TopicSetting;
            }

        }

        public Unifiable TopicSetting
        {
            get
            {
                if (!this.Predicates.containsSettingCalled("topic"))
                {
                    return bot.NOTOPIC;
                }
                var t = this.Predicates.grabSetting("topic");
                return t;
            }
            set
            {
                Predicates.addSetting("topic", value);
            }
        }

        /// <summary>
        /// the predicates associated with this particular user
        /// </summary>
        public SettingsDictionary Predicates;
        //public SettingsDictionary Predicates0;

        /// <summary>
        /// The most recent result to be returned by the bot
        /// </summary>
        public Result LastResult
        {
            get
            {
                return GetResult(0, true);
            }
        }


        public IEnumerable<Unifiable> BotOutputs
        {
            get
            {
                var raws = new List<Unifiable>();
                int added = 0;
                string lastOutput = "";
                if (this.SailentResultCount > 0)
                {
                    lock (Results) foreach (var result in Results)
                        {
                            if (result.Responder == this) continue;
                            string thisOutput = result.RawOutput;
                            if (thisOutput == "*") continue;
                            if (thisOutput == lastOutput) continue;
                            lastOutput = thisOutput;
                            raws.Add(result.RawOutput);
                            added++;
                            if (added > 2) break;
                        }
                }
                if (raws.Count == 0) raws.Add("HELLO"); //since nothing is known yet!
                if (raws.Count == 0) raws.Add(Unifiable.STAR);
                return raws;
            }
        }


        #endregion

        #region Methods

        public void InsertProvider(ParentProvider pp)
        {
            Predicates.InsertProvider(pp);
        }
        /// <summary>
        /// Ctor
        /// </summary>
        /// <param name="UserID">The GUID of the user</param>
        /// <param name="bot">the bot the user is connected to</param>
        internal User(string userID, RTParser.RTPBot bot)
            : this(userID, bot, null)
        {
        }

        /// <summary>
        /// Ctor
        /// </summary>
        /// <param name="userID">The GUID of the user</param>
        /// <param name="bot">the bot the user is connected to</param>
        protected User(string userID, RTParser.RTPBot bot, ParentProvider provider)
           // : base(bot)
        {
            WriteLine = WriteLine0;
            qsbase = new QuerySettingsImpl(bot.GetQuerySettings());
            if (userID.Length > 0)
            {
                WriterOptions = new PrintOptions();
                this.id = userID;
                this.bot = bot;
                qsbase.IsTraced = IsTraced = bot.IsTraced;
                // we dont inherit the BotAsUser we inherit the bot's setings
                // ApplySettings(bot.BotAsUser, this);
                this.Predicates = new SettingsDictionary(userID + ".predicates", this.bot, provider);
                this.Predicates.IsTraced = qsbase.IsTraced;
                this.Predicates.AddPrefix("user.", () => this);
                this.bot.DefaultPredicates.Clone(this.Predicates);
                //this.Predicates.AddGetSetProperty("topic", new CollectionProperty(_topics, () => bot.NOTOPIC));
                this.Predicates.addSetting("topic", bot.NOTOPIC);
                //this.Predicates.InsertFallback(() => bot.DefaultPredicates);
                UserID = userID; 
                UserName = userID;
                //this.Predicates.addSetting("topic", "NOTOPIC");
                SaveTimer = new Timer(SaveOften, this, new TimeSpan(0, 5, 0), new TimeSpan(0, 5, 0));
                needsSave = true;
                StampResponseGiven();
            }
            else
            {
                throw new Exception("The UserID cannot be empty");
            }
        }

        /// <summary>
        /// If the user is being traced/debugged
        /// </summary>
        public bool IsTraced
        {
            get { return qsbase.IsTraced; }
            set { qsbase.IsTraced = value; }
        }

        private readonly QuerySettings qsbase;
        public QuerySettings GetQuerySettings()
        {
            //if (CurrentRequest != null)
            //{
             //   return CurrentRequest;
            //}
            return qsbase;
        }

        public QuerySettings GetQuerySettingsSRAI()
        {
            return QuerySettings.SRAIDefaults;
        }

        public override string ToString()
        {
            return UserID;
        }

        /// <summary>
        /// Performs application-defined tasks associated with freeing, releasing, or resetting unmanaged resources.
        /// </summary>
        /// <filterpriority>2</filterpriority>
        public void Dispose()
        {
            if (SaveTimer != null)
            {
                try
                {
                    SaveTimer.Dispose();
                }
                catch (Exception)
                {

                }
                lock (ShutdownHooks)
                {
                    foreach (var threadStart in ShutdownHooks)
                    {
                        try
                        {
                            threadStart.Invoke();
                        }
                        catch (Exception exception)
                        {
                            writeDebugLine("ERROR: shutdownhook " + threadStart + " " + exception);
                        }
                        ShutdownHooks.Clear();
                    }
                }
            }
            SaveOften(this);
        }

        /// <summary>
        /// Returns the Unifiable to use for the next that part of a subsequent path
        /// </summary>
        /// <returns>the Unifiable to use for that</returns>
        private Unifiable getLastBotOutput(User responder)
        {
            if (this.SailentResultCount > 0)
            {
                Result v = GetResult(0, true, responder);
                return v.RawOutput;
            }
            else
            {
                return Unifiable.STAR;
            }
        }

        /// <summary>
        /// Returns the first sentence of the last output from the bot
        /// </summary>
        /// <returns>the first sentence of the last output from the bot</returns>
        public Unifiable getThat(User responder)
        {
            return this.getThat(0, 0, responder);
        }

        /// <summary>
        /// Returns the first sentence of the output "n" steps ago from the bot
        /// </summary>
        /// <param name="n">the number of steps back to go</param>
        /// <returns>the first sentence of the output "n" steps ago from the bot</returns>
        public Unifiable getThat(int n, User responder)
        {
            return this.getThat(n, 0, responder);           
        }

        /// <summary>
        /// Returns the sentence numbered by "sentence" of the output "n" steps ago from the bot
        /// </summary>
        /// <param name="n">the number of steps back to go</param>
        /// <param name="sentence">the sentence number to get</param>
        /// <returns>the sentence numbered by "sentence" of the output "n" steps ago from the bot</returns>
        public Unifiable getThat(int n, int sentence, User responder)
        {
            if ((n >= 0) & (n < this.SailentResultCount))
            {
                Result historicResult = GetResult(n, true, responder);
                if (historicResult == null) return "";
                if ((sentence >= 0) & (sentence < historicResult.OutputSentenceCount))
                {
                    return (Unifiable) historicResult.GetOutputSentence(sentence);
                }
            }
            return Unifiable.Empty;
        }

        /// <summary>
        /// Returns the first sentence of the last output from the bot
        /// </summary>
        /// <returns>the first sentence of the last output from the bot</returns>
        public Unifiable getInputSentence(User responder)
        {
            return this.getInputSentence(0, 0, responder);
        }

        /// <summary>
        /// Returns the first sentence from the output from the bot "n" steps ago
        /// </summary>
        /// <param name="n">the number of steps back to go</param>
        /// <returns>the first sentence from the output from the bot "n" steps ago</returns>
        public Unifiable getInputSentence(int n, User responder)
        {
            return this.getInputSentence(n, 0, responder);
        }


        /// <summary>
        /// Returns the identified sentence number from the input from the bot "n" steps ago
        /// </summary>
        /// <param name="n">the number of steps back to go</param>
        /// <param name="sentence">the sentence number to return</param>
        /// <returns>the identified sentence number from the input from the bot "n" steps ago</returns>
        public Unifiable getInputSentence(int n, int sentence, User responder)
        {
            if (n == 0)
            {
                return CurrentRequest.rawInput;
            }
            n = n - 1;
            if ((n >= 0) & (n < this.SailentResultCount))
            {
                Result historicInput = GetResult(n, false, responder);
                if ((sentence >= 0) & (sentence < historicInput.InputSentences.Count))
                {
                    return (Unifiable) historicInput.InputSentences[sentence];
                }
            }
            return Unifiable.Empty;
        }

        /// <summary>
        /// Returns the identified sentence number from the input from the bot "n" steps ago
        /// </summary>
        /// <param name="n">the number of steps back to go</param>
        /// <param name="sentence">the sentence number to return</param>
        /// <returns>the identified sentence number from the input from the bot "n" steps ago</returns>
        public Unifiable getRequestSentence(int n, int sentence, User responder)
        {
            if (n == 0)
            {
                return CurrentRequest.rawInput;
            }
            n = n - 1;
            if ((n >= 0) & (n < this.SailentResultCount))
            {
                Result historicInput = GetResult(n, false, responder);
                if ((sentence >= 0) & (sentence < historicInput.InputSentences.Count))
                {
                    return (Unifiable)historicInput.InputSentences[sentence];
                }
            }
            return Unifiable.Empty;
        }

        /// <summary>
        /// Returns the first sentence of the last output from the bot
        /// </summary>
        /// <returns>the first sentence of the last output from the bot</returns>
        public Unifiable getResultSentence(User responder)
        {
            return this.getResultSentence(0, 0, responder);
        }

        /// <summary>
        /// Returns the first sentence from the output from the bot "n" steps ago
        /// </summary>
        /// <param name="n">the number of steps back to go</param>
        /// <returns>the first sentence from the output from the bot "n" steps ago</returns>
        public Unifiable getResultSentence(int n, User responder)
        {
            return this.getResultSentence(n, 0, responder);
        }

        /// <summary>
        /// Returns the identified sentence number from the output from the bot "n" steps ago
        /// </summary>
        /// <param name="n">the number of steps back to go</param>
        /// <param name="sentence">the sentence number to return</param>
        /// <returns>the identified sentence number from the output from the bot "n" steps ago</returns>
        public Unifiable getResultSentence(int n, int sentence, User responder)
        {
            if ((n >= 0) & (n < this.SailentResultCount))
            {
                Result historicResult = GetResult(n, false, responder);
                if ((sentence >= 0) & (sentence < historicResult.InputSentences.Count))
                {
                    return (Unifiable) historicResult.InputSentences[sentence];
                }
            }
            return Unifiable.Empty;
        }

        public bool SuspendAddResultToUser;
        private int SailentResultCount
        {
            get
            {
                lock (Results)
                {
                    return Results.Count;
                }
            }
        }

        /// <summary>
        /// Adds the latest result from the bot to the Results collection
        /// </summary>
        /// <param name="latestResult">the latest result from the bot</param>
        public void addResult(Result latestResult)
        {
            if (SuspendAddResultToUser)
            {
                Request r = latestResult.request;
                if (r != null)
                {
                    if (r.IsTraced)
                        RTPBot.writeDebugLine("AIMLTRACE: SuspendAddResultToUser, " + latestResult);
                }
                return;
            }
            lock (Results)
            {
                if (Results.Contains(latestResult))
                {
                    writeDebugLine("DEBUG9 Trying to resave results ! " + latestResult);
                    return;
                }
                this.Results.Insert(0, latestResult);
                latestResult.CollectRequest();
                int rc = this.SailentResultCount;
                if (rc > MaxResultsSaved)
                {
                    this.Results.RemoveRange(MaxResultsSaved, rc - MaxResultsSaved);
                }
            }
            addResultTemplates(latestResult);
        }
        #endregion

        public Unifiable That
        {
            get
            {
                string something;
                Result r = GetResult(0, true) ?? GetResult(0, false, LastReponder);
                if (r != null && IsSomething(r.NormalizedOutput, out something)) return something;
                if (LastReponder != null && IsSomething(LastReponder.JustSaid, out something)) return something;
                if (User.ThatIsStoredBetweenUsers)
                {                  
                    return "Nothing";
                }
                RequestImpl fr = CurrentRequest;
                while
                    (fr != null)
                {
                    var frithat = fr.ithat;
                    if (IsSomething(frithat, out something)) return something;
                    fr = fr.ParentRequest as RequestImpl;
                }
                if (IsSomething(getLastBotOutputForThat(), out something)) return something;
                return "Nothing";
            }
            set
            {
                if (IsNullOrEmpty(value)) throw new NullReferenceException("set_That: " + this);
                if (CurrentRequest != null)
                {
                    CurrentRequest.ithat = value;
                }
                if (LastReponder != null)
                {
                    LastReponder.JustSaid = value;
                }
            }

        }

        private string getLastBotOutputForThat()
        {
            string something;
            Result r = GetResult(0, true) ?? GetResult(0, false, LastReponder);
            if (r != null && IsSomething(r.NormalizedOutput, out something)) return something;
            if (LastReponder != null && IsSomething(LastReponder.JustSaid, out something)) return something; 
            return "Nothing";
        }

        private string _JustSaid;
        public string JustSaid
        {
            get
            {
                string something;
                if (IsSomething(_JustSaid, out something)) return something;
                if (LastReponder != null)
                {
                    return LastReponder.ResponderJustSaid;
                }
                return "Nothing";
            }
            set
            {
                if (IsNullOrEmpty(value)) throw new NullReferenceException("set_That: " + this);
                if (value.Contains("TAG-"))
                {
                    if (IsNullOrEmpty(value))
                        throw new NullReferenceException("set_That: TAG: " + value + " for " + this);
                }
                if (_JustSaid != value)
                {
                    Predicates.addSetting("lastsaid", value);
                    _JustSaid = value;
                    if (LastReponder != null)
                    {
                        LastReponder.ResponderJustSaid = value;
                    }
                }
            }
        }

        public string ResponderJustSaid
        {
            get
            {
                {
                    if (LastReponder != null) return LastReponder.JustSaid;
                    return That;
                }
            }
            set
            {
                if (IsNullOrEmpty(value)) throw new NullReferenceException("set_That: " + this);
                if (LastReponder != null)
                {
                    LastReponder.JustSaid = value;
                }
                Predicates["lastheard"] = value;
                That = value;
            }
        }

        public User LastReponder
        {
            get
            {
                foreach (var name in NamesStrings("you,lastusername,lastuserid"))
                {
                    string name1 = name;
                    string lname =
                        WithoutTrace(Predicates,
                                     () =>
                                     {
                    string realName;
                                         return SettingsDictionary.grabSettingDefaultDict(Predicates, name1,
                                                                             out realName);
                                     });
                    if (!string.IsNullOrEmpty(lname))
                    {
                        User user = bot.FindUser(lname);
                        if (user != null && user != this)
                        {
                            return user;
                        }
                    }
                }
                if (LastResult != null)
                {
                    User lastResultResponder = LastResult.Responder;
                    if (lastResultResponder != this && lastResultResponder != null) return lastResultResponder;
                    lastResultResponder = LastResult.Requester;
                    if (lastResultResponder != this && lastResultResponder != null) return lastResultResponder;
                }

                return null;
            }
            set
            {
                if (value == null || value == this) return;
                Predicates["lastuserid"] = value.UserID;
                Predicates["lastusername"] = value.UserName;
                Predicates["you"] = value.UserName;
            }
        }

        public bool DoUserCommand(string input, OutputDelegate console)
        {
            if (input == null) return false;
            input = input.Trim();
            if (input.StartsWith("@"))
            {
                input = input.TrimStart(new[] { ' ', '@' });
                switch (input)
                {
                    case "save":
                        {
                            SaveOften(this);
                        }
                        return true;
                    default:
                        break;
                }
            }
            //if (input == "") return false;
            input = input + " ";
            int firstWhite = input.IndexOf(' ');
            string var = input.Substring(0, firstWhite).Trim();
            string value = input.Substring(firstWhite + 1).Trim();
            if (var == "")
            {
                console(Predicates.ToDebugString());
                RTPBot.WriteUserInfo(console, "", this);
                return true;
            }
            if (value == "")
            {
                console(var + " = " + Predicates.grabSettingNoDebug(var));
                return true;
            }
            console("addSetting: " + Predicates.addSetting(var, value));
            return true;
        }

        internal OutputDelegate userTrace;
        internal OutputDelegate WriteLine;
        internal void WriteLine0(string s, params object[] objects)
        {
            try
            {
                s = string.Format("{0} {1}", UserName ?? UserID, string.Format(s, objects));
                if (s.ToUpper().Contains("ERROR"))
                {
                    bot.writeToLog(s, objects);
                }
                if (userTrace != null)
                {
                    userTrace(s);
                    return;
                }
                bot.writeToUserLog(s);
            }
            catch (Exception exception)
            {
                bot.writeToLog(exception);
            }
        }

        public void addResultTemplates(Result result)
        {
            lock (UsedTemplates)
            {
                lock (result.UsedTemplates)
                    UsedTemplates.AddRange(result.UsedTemplates);
            }
        }

        public void addResultTemplates(Request request)
        {
            lock (UsedTemplates)
            {
                lock (request.UsedResults)
                {
                    foreach (var list in request.UsedResults)
                    {
                        addResultTemplates(list);
                    }
                }
            }
        }

        public Result GetResult(int i, bool mustBeSalient)
        {            
            return GetResult(i, mustBeSalient, null);
        }

        public Result GetResult(int i, bool mustBeSalient, User responder)
        {
            bool mustBeResponder = responder != null;
            if (i == -1) return CurrentRequest.CurrentResult;
            lock (Results)
            {
                if (i >= Results.Count)
                {
                    return null;
                }
                {
                    foreach (var r in Results)
                    {
                        if (r.Responder == this) continue;
                        if (mustBeResponder) if (r.Responder != responder) continue;
                        if (mustBeSalient && !r.IsSailent) continue;
                        if (i == 0) return r;
                        i--;
                    }
                    return null;
                }
            }
        }

        public RequestImpl CurrentRequest;

        public Result GetResult(int i)
        {
            return GetResult(i, false);
        }

        public void SetOutputSentences(string args, User responder)
        {
            args = ForOutputTemplate(args);
            Result result = GetResult(0, false, responder);
            if (result != null)
                result.SetOutput = args;
            else
            {
                bot.writeToLog("no last result in SetOutputSentences " + args + " for " + this);
            }
        }

        public bool CanUseTemplate(TemplateInfo info, Result request)
        {
            lock (UsedTemplates)
            {
                if (UsedTemplates.Contains(info))
                {
                    if (info.CategoryInfo != null && info.CategoryInfo.Pattern != null && info.CategoryInfo.Pattern.FullPath != null)
                    {
                        if (info.CategoryInfo.Pattern.FullPath.AsString() == "*")
                        {
                            return true;
                        }
                    }
                    // return false;
                }
            }
            return true;
        }


        public string UserDirectory
        {
            get { return _saveToDirectory; }
            set
            {
                _saveToDirectory = value;
                Predicates.addSetting("userdir", value);
            }
        }

        private string _saveToDirectory;
        private bool needsSave;
        private bool insideSave;
        private bool noLoad = true;
        public bool NeverSave = NeverSaveUsers;
        private bool needAiml = false;
        private readonly List<CrossAppDomainDelegate> ShutdownHooks = new List<CrossAppDomainDelegate>();
        private readonly List<CrossAppDomainDelegate> OnNeedAIML = new List<CrossAppDomainDelegate>();
        public int depth;

        public void SyncDirectory(string userdir)
        {
            if (_saveToDirectory == null) _saveToDirectory = userdir;
            noLoad = false;
            lock (SaveLock)
            {
                if (insideSave) return;
            }
            LoadDirectory(userdir);
            if (userdir != null) Predicates.addSetting("userdir", userdir);
            if (IsRoleAcct) return;
            SaveDirectory(userdir);
        }

        public void SaveDirectory(string userdir)
        {
            _saveToDirectory = userdir;
            needsSave = true;
            lock (SaveLock)
            {
                if (insideSave) return;
                insideSave = true;
            }
            try
            {
                if (NeverSave) return;
                SaveDirectory0(userdir);
            }
            finally
            {
                lock (SaveLock)
                {
                    insideSave = false;
                }
            }
        }

        private void SaveDirectory0(string userdir)
        {
            if (userdir == null && _saveToDirectory == null)
            {
                WriteLine("no saveto dir specified");
                return;
            }
            if (!HostSystem.CreateDirectory(userdir))
            {
                WriteLine("Cannot SaveDirectory {0}", userdir);
                return;
            }
            // or WriteLine but this is spammy 
            OutputDelegate logger = DEVNULL;
            logger("DEBUG9 Saving User Directory {0}", userdir);
            Predicates.SaveTo(userdir, "user.predicates", "UserPredicates.xml");
            GraphMaster gm = bot.GetGraph(UserID, ListeningGraph);
            gm.WriteToFile(UserID, HostSystem.Combine(userdir, UserID) + ".saved", PrintOptions.SAVE_TO_FILE, logger);
        }

        public void LoadDirectory(string userdir)
        {
            lock (SaveLock)
            {
                if (insideSave) return;
                insideSave = true;
            }
            try
            {
                LoadDirectory0(userdir);
            }
            finally
            {
                lock (SaveLock)
                {
                    insideSave = false;
                }
            }
        }

        private void LoadDirectory0(string userdir)
        {
            if (HostSystem.DirExists(userdir))
            {
                if (noLoad) return;
                LoadUserSettings(userdir);
                LoadUserAiml(userdir);
                WriteLine("USERTRACE: LoadDirectory " + userdir + " -DEBUG9");
            }
            else
            {
                WriteLine("USERTRACE: Not LoadDirectory " + userdir);
            }
        }

        public void LoadUserSettings(string userdir)
        {
            if (HostSystem.DirExists(userdir))
            {
                if (noLoad) return;
                foreach (string s in HostSystem.GetFiles(userdir, "*.xml"))
                {
                    LoadUserSettings(s);
                }
                return;
            }
            if (HostSystem.FileExists(userdir))
            {
                if (!userdir.EndsWith(".xml"))
                {
                    return;
                }
                if (userdir.EndsWith("Predicates.xml"))
                {
                    SettingsDictionary.loadSettings(Predicates, userdir, true, true, null);
                }
                return;
            }
        }

        public void LoadUserAiml(string userdir)
        {
            if (HostSystem.DirExists(userdir))
            {
                AddTodoItem(() =>
                            {
                                string[] hostSystemGetFiles = HostSystem.GetFiles(userdir, "*.aiml");
                                if (hostSystemGetFiles != null && hostSystemGetFiles.Length > 0)
                                {
                                    var request1 = new AIMLbot.MasterRequest("load user aiml ", this, bot, null,null);
                                    request1.TimesOutAt = DateTime.Now + new TimeSpan(0, 15, 0);
                                    request1.Graph = ListeningGraph;
                                    request1.LoadingFrom = userdir;
                                    var options1 = request1.LoadOptions; //LoaderOptions.GetDefault(request);
                                    var gs = bot.GlobalSettings;
                                    try
                                    {
                                        bot.GlobalSettings = Predicates;
                                        request1.Loader.loadAIMLURI(userdir, options1.Value);
                                    }
                                    finally
                                    {
                                        bot.GlobalSettings = gs;
                                    }
                                    return;
                                }
                            });
                return;
            }
            // process previous todo list
            DoPendingTodoList();
            if (!HostSystem.FileExists(userdir) || !userdir.EndsWith(".aiml")) return;
            var request = new AIMLbot.MasterRequest("load user aiml ", this, bot, null, null);
            request.TimesOutAt = DateTime.Now + new TimeSpan(0, 15, 0);
            request.Graph = ListeningGraph;
            request.LoadingFrom = userdir;
            var options = request.LoadOptions; //LoaderOptions.GetDefault(request);
            request.Loader.loadAIMLURI(userdir, options.Value);
        }

        public void AddTodoItem(CrossAppDomainDelegate action)
        {
            lock (OnNeedAIML)
            {
                if (needAiml)
                {
                    action();
                    return;
                }
                OnNeedAIML.Add(action);
            }
        }

        private void DoPendingTodoList()
        {
            lock (OnNeedAIML)
            {
                if (needAiml) return;
                needAiml = true;
                DoPendingUntilComplete(OnNeedAIML);
            }
        }

        internal static void DoPendingUntilComplete(ICollection<CrossAppDomainDelegate> todoList)
        {
            if (todoList == null) return;
            while (true)
            {
                var todo = new List<CrossAppDomainDelegate>();
                lock (todoList)
                {
                    if (todoList.Count == 0) return;
                    todo.AddRange(todoList);
                    todoList.Clear();
                }
                foreach (var list in todo)
                {
                    try
                    {
                        list();
                    }
                    catch (Exception e)
                    {
                        writeDebugLine("DoPendingUntilComplete: " + list + " - " + e);
                    }
                }
            }
        }

        #region Implementation of ISettingsDictionary

        /// <summary>
        /// Adds a bespoke setting to the Settings class (accessed via the grabSettings(string name)
        /// method.
        /// </summary>
        /// <param name="name">The name of the new setting</param>
        /// <param name="value">The value associated with this setting</param>
        public bool addSetting(string name, Unifiable value)
        {
            return Predicates.addSetting(name, value);
        }

        /// <summary>
        /// Removes the named setting from this class
        /// </summary>
        /// <param name="name">The name of the setting to remove</param>
        public bool removeSetting(string name)
        {
            return Predicates.removeSetting(name);
        }

        /// <summary>
        /// Updates the named setting with a new value whilst retaining the position in the
        /// dictionary
        /// </summary>
        /// <param name="name">the name of the setting</param>
        /// <param name="value">the new value</param>
        public bool updateSetting(string name, Unifiable value)
        {
            return Predicates.updateSetting(name, value);
        }

        /// <summary>
        /// Returns the value of a setting given the name of the setting
        /// </summary>
        /// <param name="name">the name of the setting whose value we're interested in</param>
        /// <returns>the value of the setting</returns>
        public Unifiable grabSetting(string name)
        {
            return Predicates.grabSetting(name);
        }

        /// <summary>
        /// Checks to see if a setting of a particular name exists
        /// </summary>
        /// <param name="name">The setting name to check</param>
        /// <returns>Existential truth value</returns>
        public bool containsLocalCalled(string name)
        {
            return Predicates.containsLocalCalled(name);
        }

        public bool containsSettingCalled(string name)
        {
            return Predicates.containsSettingCalled(name);
        }

        public string NameSpace
        {
            get
            {
                return UserID;
            }
        }

        public PrintOptions WriterOptions { get; set; }

        public actMSM botActionMSM
        {
            get { return bot.pMSM; }
        }

        public IEnumerable<string> SettingNames(int depth)
        {
            //get 
            { return Predicates.SettingNames(depth); }
        }

        #endregion

        public Request CreateRequest(string s, User target)
        {
            DoPendingTodoList();
            depth = 0;

            var newRequest = new AIMLbot.MasterRequest(s, this, bot, null, target);
            newRequest.IsToplevelRequest = true;
            //newRequest.ParentRequest = CurrentRequest;
            return newRequest;
        }

        public void StampResponseGiven()
        {
            LastResponseGivenTime = DateTime.Now;
        }

        public bool CanGiveResponseNow()
        {
            // KHC: TODO Revisit, response time seemed more like minute fractions than totalseconds for whole struct
            // could be
            DateTime thisResponseTime = DateTime.Now;
            TimeSpan TSP = thisResponseTime.Subtract(LastResponseGivenTime);
            double timedelay = Math.Abs(TSP.TotalSeconds);
            double secPerChat = (double)((double)60 / (double)MaxRespondToChatPerMinute);
            return (timedelay >
                // ReSharper disable PossibleLossOfFraction
                     secPerChat
             );
            // ReSharper restore PossibleLossOfFraction
        }

        public void Enter(AIMLTagHandler srai)
        {
            depth++;
        }

        public void Exit(AIMLTagHandler srai)
        {
            depth--;
            if (depth == 0)
            {
                SubQuery.PurgeTagHandlers();
            }
        }
    }
}