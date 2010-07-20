using System;
using System.Collections.Generic;
using System.IO;
using System.Runtime.CompilerServices;
using System.Text;
using System.Threading;
using com.hp.hpl.jena.graph;
using RTParser;
using RTParser.Utils;

namespace RTParser
{
    /// <summary>
    /// Encapsulates information and history of a user who has interacted with the bot
    /// </summary>
    abstract public class User : QuerySettings, QuerySettingsReadOnly,IDisposable
    {
        public readonly object QueryLock = new object();

        #region Attributes

        public List<TemplateInfo> UsedTemplates = new List<TemplateInfo>();

        public List<QueryList> AllQueries = new List<QueryList>();

        public int LastResponseGivenTime = 0;
        public bool RespondToChat = true;
        public int MaxRespondToChatPerMinute = 10;

        public static int DefaultMaxResultsSaved = 3;
        public int MaxResultsSaved = DefaultMaxResultsSaved;

        /// <summary>
        /// The local instance of the GUID that identifies this user to the bot
        /// </summary>
        private Unifiable id;

        /// <summary>
        /// The bot this user is using
        /// </summary>
        public RTParser.RTPBot bot;


        public Timer SaveTimer;

        readonly object SaveLock = new object();
        private void SaveOften(object state)
        {
            lock (SaveLock)
            {
                if (!needsSave) return;
                needsSave = false;
            }
            SaveDirectory(_LoadedDirectory);
            lock (SaveLock)
            {
                needsSave = true;
            }
        }

        /// <summary>
        /// The grahmaster this user is using
        /// </summary>
        public GraphMaster ListeningGraph
        {
            get
            {
                if (Predicates.containsSettingCalled("graphname"))
                {
                    GraphMaster _Graph = null;
                    var v = Predicates.grabSettingNoDebug("graphname");
                    _Graph = bot.GetUserGraph(v, bot.GraphMaster);
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
        public override GraphMaster Graph
        {
            get { return ListeningGraph;  }
            set { ListeningGraph = value; }
        }

        /// <summary>
        /// The GUID that identifies this user to the bot
        /// </summary>
        public Unifiable UserID
        {
            get { return this.id; }
        }

        public string UserName;

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
        public RTParser.Utils.SettingsDictionary Predicates;

        /// <summary>
        /// The most recent result to be returned by the bot
        /// </summary>
        public Result LastResult
        {
            get
            {
                return GetResult(0);
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
                    foreach (var result in Results)
                    {
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

        public string ShortName
        {
            get
            {
                if (this.Predicates != null && Predicates.containsSettingCalled("name")) return Predicates.grabSettingNoDebug("name");
                return UserID.AsString().ToLower().Replace(" ", "_");
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
        public User(string UserID, RTParser.RTPBot bot)
            : this(UserID, bot, new ParentProvider(() => bot.GlobalSettings))
        {
        }

        /// <summary>
        /// Ctor
        /// </summary>
        /// <param name="UserID">The GUID of the user</param>
        /// <param name="bot">the bot the user is connected to</param>
        public User(string UserID, RTParser.RTPBot bot, ParentProvider provider)
            : base(bot)
        {
            if (UserID.Length > 0)
            {
                this.id = UserID;
                this.bot = bot;
                // we dont inherit the BotAsUser we inherit the bot's setings
                // ApplySettings(bot.BotAsUser, this);
                this.Predicates = new RTParser.Utils.SettingsDictionary(ShortName + ".predicates", this.bot, provider);
                this.bot.DefaultPredicates.Clone(this.Predicates);
                //this.Predicates.AddGetSetProperty("topic", new CollectionProperty(_topics, () => bot.NOTOPIC));
                this.Predicates.addSetting("topic", bot.NOTOPIC);
                this.Predicates.InsertFallback(() => bot.DefaultPredicates);
                this.Predicates.InsertFallback(() => bot.HeardPredicates);
                Predicates.addSetting("id", UserID);
                //this.Predicates.addSetting("topic", "NOTOPIC");
                SaveTimer = new Timer(SaveOften, this, new TimeSpan(0, 5, 0), new TimeSpan(0, 5, 0));            
            }
            else
            {
                throw new Exception("The UserID cannot be empty");
            }
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
            SaveOften(this);
        }

        /// <summary>
        /// Returns the Unifiable to use for the next that part of a subsequent path
        /// </summary>
        /// <returns>the Unifiable to use for that</returns>
        private Unifiable getLastBotOutput()
        {
            if (this.SailentResultCount > 0)
            {
                var v = GetResult(0).RawOutput;
                return v;
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
        public Unifiable getThat()
        {
            return this.getThat(0, 0);
        }

        /// <summary>
        /// Returns the first sentence of the output "n" steps ago from the bot
        /// </summary>
        /// <param name="n">the number of steps back to go</param>
        /// <returns>the first sentence of the output "n" steps ago from the bot</returns>
        public Unifiable getThat(int n)
        {
            return this.getThat(n, 0);
        }

        /// <summary>
        /// Returns the sentence numbered by "sentence" of the output "n" steps ago from the bot
        /// </summary>
        /// <param name="n">the number of steps back to go</param>
        /// <param name="sentence">the sentence number to get</param>
        /// <returns>the sentence numbered by "sentence" of the output "n" steps ago from the bot</returns>
        public Unifiable getThat(int n, int sentence)
        {
            if ((n >= 0) & (n < this.SailentResultCount))
            {
                Result historicResult = GetResult(n);
                if ((sentence >= 0) & (sentence < historicResult.OutputSentenceCount))
                {
                    return (Unifiable)historicResult.GetOutputSentence(sentence);
                }
            }
            return Unifiable.Empty;
        }

        /// <summary>
        /// Returns the first sentence of the last output from the bot
        /// </summary>
        /// <returns>the first sentence of the last output from the bot</returns>
        public Unifiable getInputSentence()
        {
            return this.getInputSentence(0, 0);
        }

        /// <summary>
        /// Returns the first sentence from the output from the bot "n" steps ago
        /// </summary>
        /// <param name="n">the number of steps back to go</param>
        /// <returns>the first sentence from the output from the bot "n" steps ago</returns>
        public Unifiable getInputSentence(int n)
        {
            return this.getInputSentence(n, 0);
        }

        /// <summary>
        /// Returns the identified sentence number from the input from the bot "n" steps ago
        /// </summary>
        /// <param name="n">the number of steps back to go</param>
        /// <param name="sentence">the sentence number to return</param>
        /// <returns>the identified sentence number from the input from the bot "n" steps ago</returns>
        public Unifiable getInputSentence(int n, int sentence)
        {
            if ((n >= 0) & (n < this.SailentResultCount))
            {
                Result historicInput = GetResult(n);
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
        public Unifiable getResultSentence()
        {
            return this.getResultSentence(0, 0);
        }

        /// <summary>
        /// Returns the first sentence from the output from the bot "n" steps ago
        /// </summary>
        /// <param name="n">the number of steps back to go</param>
        /// <returns>the first sentence from the output from the bot "n" steps ago</returns>
        public Unifiable getResultSentence(int n)
        {
            return this.getResultSentence(n, 0);
        }

        /// <summary>
        /// Returns the identified sentence number from the output from the bot "n" steps ago
        /// </summary>
        /// <param name="n">the number of steps back to go</param>
        /// <param name="sentence">the sentence number to return</param>
        /// <returns>the identified sentence number from the output from the bot "n" steps ago</returns>
        public Unifiable getResultSentence(int n, int sentence)
        {
            if ((n >= 0) & (n < this.SailentResultCount))
            {
                Result historicResult = GetResult(n);
                if ((sentence >= 0) & (sentence < historicResult.InputSentences.Count))
                {
                    return (Unifiable)historicResult.InputSentences[sentence];
                }
            }
            return Unifiable.Empty;
        }

        public bool SuspendAdd;
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
            if (SuspendAdd)
            {
                Request r = latestResult.request;
                if (r != null)
                {
                    if (r.IsTraced)
                        RTPBot.writeDebugLine("AIMLTRACE: skipping result " + latestResult);
                }
                return;
            }
            lock (Results)
            {
                this.Results.Insert(0, latestResult);
                int rc = this.SailentResultCount;
                if (rc > MaxResultsSaved)
                {
                    this.Results.RemoveRange(MaxResultsSaved, rc - MaxResultsSaved);
                }
            }
            addResultTemplates(latestResult);
        }
        #endregion

        public string getLastBotOutputForThat()
        {
            lock (Results)
            {
                int resltUsed = 0;
                foreach (Result r in Results)
                {
                    resltUsed++;
                    if (!r.IsSailent) continue;
                    String sentence = r.RawOutput;
                    sentence = MainSentence(sentence);
                    sentence = sentence.Trim(new char[] {'.', ' ', '!', '?'});
                    if (sentence.Length == 0) continue;
                    String ssentence = bot.Loader.Normalize(sentence, true);
                    if (sentence.Length == 0) continue;
                    return sentence;
                }

            }
            return "Nothing";// Unifiable.STAR;
        }

        static string MainSentence(string sentence)
        {
            string prev = "";
            while (sentence != prev)
            {
                prev = sentence;
                sentence = sentence.Trim();
                int sl = sentence.Length - 1;

                if (sl < 0) return sentence;

                char c = sentence[sl];
                if (char.IsPunctuation(c))
                {
                    sentence = sentence.Substring(0, sl);
                }
                sentence = sentence.TrimEnd();
            }
            int sf = sentence.LastIndexOfAny(new[] { '?' });

            if (sf > 0)
            {
                String newClip = sentence.Substring(0, sf - 1);
                RTPBot.writeDebugLine("AIMLTRACE !REWRITE THAT QUESTION " + sentence + " => " + newClip);
                if (newClip.Length > 4) sentence = newClip;
            }
            sentence = sentence.Trim(new char[] { '.', ' ', '!', '?' });
            sf = sentence.LastIndexOfAny(new[] { '.', '!' });
            if (sf > 0)
            {
                String newClip = sentence.Substring(sf).Trim();
                while (char.IsPunctuation(newClip[0]))
                {
                    newClip = newClip.Substring(1).TrimStart();
                }
                RTPBot.writeDebugLine("AIMLTRACE !REWRITE THAT SENT " + sentence + " => " + newClip);
                if (newClip.Length > 4) sentence = newClip;
            }
            return sentence;
        }

        public bool DoUserCommand(string input, RTPBot.OutputDelegate console)
        {
            if (input == null) return false;
            input = input.Trim();
            if (input.StartsWith("@"))
            {
                input = input.TrimStart(new[] { ' ', '@' });
            }
            //if (input == "") return false;
            input = input + " ";
            int firstWhite = input.IndexOf(' ');
            string var = input.Substring(1, firstWhite);
            string value = input.Substring(firstWhite + 1).Trim();
            if (var == "")
            {
                console(Predicates.ToDebugString());
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

        public void WriteLine(string s, params object[] objects)
        {
            try
            {
                bot.writeToLog("USERTRACE: " + string.Format(s, objects));
            }
            catch(Exception exception)
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
            if (i == -1) return CurrentRequest.CurrentResult;
            lock (Results)
            {
                if (i >= Results.Count) return null;
                if (mustBeSalient)
                {
                    foreach (var r in Results)
                    {
                        if (r.IsSailent)
                        {
                            if (i == 0) return r;
                            i--;
                        }
                    }
                    return null;
                }
                return Results[i];
            }
        }

        public Request CurrentRequest;

        public Result GetResult(int i)
        {
            return GetResult(i, false);
        }

        public void SetOutputSentences(string args)
        {
            Result result = LastResult;
            if (result != null)
                result.SetOutput = args;
            else
            {
                bot.writeToLog("no last result in SetOutputSentences " + args + " for " + this);
            }
        }

        public bool CanUseTemplate(TemplateInfo info, Result request)
        {
            return true;
        }


        public string UserDirectory
        {
            get { return _LoadedDirectory; }
            set { LoadDirectory(value); }
        }

        private string _LoadedDirectory;
        private bool needsSave;

        public void SyncDirectory(string userdir)
        {
            LoadDirectory(userdir);
            Predicates.addSetting("userdir", userdir);
            SaveDirectory(userdir);
        }

        public void SaveDirectory(string userdir)
        {
            HostSystem.CreateDirectory(userdir);
            Predicates.SaveTo(userdir, "user.predicates", "UserPredicates.xml");
            GraphMaster gm = bot.GetGraph(UserID, ListeningGraph);
            gm.WriteToFile(UserID, Path.Combine(userdir, UserID) + ".saved");

        }

        public void LoadDirectory(string userdir)
        {
            if (Directory.Exists(userdir))
            {
                _LoadedDirectory = userdir;
                LoadUserSettings(userdir);
                LoadUserAiml(userdir);
            }
        }

        public void LoadUserSettings(string userdir)
        {
            if (Directory.Exists(userdir))
            {
                foreach (string s in Directory.GetFiles(userdir))
                {
                    LoadUserSettings(s);
                }
                return;
            }
            if (File.Exists(userdir))
            {
                if (!userdir.EndsWith(".xml"))
                {
                    return;
                }
                if (userdir.EndsWith("Predicates.xml"))
                {
                    Predicates.loadSettings(userdir);
                }
                return;
            }
        }

        public void LoadUserAiml(string userdir)
        {
            if (Directory.Exists(userdir))
            {
                foreach (string s in Directory.GetFiles(userdir))
                {
                    LoadUserAiml(s);
                }
                return;
            }
            if (!File.Exists(userdir) || !userdir.EndsWith(".aiml")) return;
            var request = new AIMLbot.Request("load user aiml ", this, bot, null);
            request.TimesOutAt = DateTime.Now + new TimeSpan(0, 15, 0);
            request.Graph = ListeningGraph;
            var options = LoaderOptions.GetDefault(request);
            bot.loadAIMLFromURI(userdir, options, request);
        }
    }
}