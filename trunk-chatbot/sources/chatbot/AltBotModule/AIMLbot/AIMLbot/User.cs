#if false 
using System;
using System.Collections;
using System.Collections.Generic;
using System.Text;
using AltAIMLbot.Utils;
using AltAIMLParser;
using MushDLR223.ScriptEngines;
using RTParser;
using RTParser.Variables;

namespace AltAIMLbot
{
    /// <summary>
    /// Encapsulates information and history of a user who has interacted with the bot
    /// </summary>
    [Serializable]
    public partial class User
    {
        #region Attributes

        /// <summary>
        /// The local instance of the GUID that identifies this user to the bot
        /// </summary>
        private string id;

        /// <summary>
        /// The bot this user is using
        /// </summary>
        public AltBot bot;

        /// <summary>
        /// The GUID that identifies this user to the bot
        /// </summary>
        public string UserID
        {
            get{return this.id;}
        }
        public void setUserID(string uid)
        {
            id = uid;
        }
        /// <summary>
        /// A collection of all the result objects returned to the user in this session
        /// </summary>
        private List<Result> Results = new List<Result>();

		/// <summary>
		/// the value of the "topic" predicate
		/// </summary>
        public string Topic
        {
            get
            {
                return GetValueOr("topic", "*");
            }
        }

        private string GetValueOr(string varname, string or)
        {
            var t = this.Predicates.grabSetting(varname);
            if (string.IsNullOrEmpty(t)) return or;
            return t;
        }

        /// <summary>
        /// the value of the "state" predicate
        /// </summary>
        public string State
        {
            get
            {
                return GetValueOr("state", "*");
            }
        }

        /// <summary>
        /// List of possible non-determinstic "states". When present will select the one with 
        /// the highest score
        /// </summary>
        public Dictionary<string, double> Qstate = new Dictionary<string, double>();

        /// <summary>
        /// the value of the "that" on the blackboard predicate
        /// </summary>
        public string blackBoardThat="";

		/// <summary>
		/// the predicates associated with this particular user
		/// </summary>
        public SettingsDictionary Predicates;

        /// <summary>
        /// The most recent result to be returned by the bot
        /// </summary>
        public Result LastResult
        {
            get
            {
                if (this.Results.Count > 0)
                {
                    return (Result)this.Results[0];
                }
                else
                {
                    return null;
                }
            }
        }

		#endregion
		
		#region Methods

        /// <summary>
        /// Ctor
        /// </summary>
        /// <param name="UserID">The GUID of the user</param>
        /// <param name="bot">the bot the user is connected to</param>
		public User(string UserID, AltBot bot)
		{
            bot.AddUser(UserID, this);
            if (UserID.Length > 0)
            {
                this.id = UserID;
                this.bot = bot;
                this.Predicates = new SettingsDictionary(UserID, this.bot, null);
                this.bot.DefaultPredicates.Clone(this.Predicates);
                this.Predicates.bbPrefix = "user";
                this.Predicates.addSetting("topic", "*");
                this.Predicates.addSetting("state", "*");
            }
            else
            {
                throw new Exception("The UserID cannot be empty");
            }
		}

        /// <summary>
        /// Returns the string to use for the next that part of a subsequent path
        /// </summary>
        /// <returns>the string to use for that</returns>
        public string getLastBotOutput()
        {
            if (blackBoardThat.Length > 0)
            {
                return blackBoardThat;
            }
            foreach (Result result in Results)
            {
                string s = result.LastSentence;
                s = s.TrimStart(" .,".ToCharArray());
                if (!string.IsNullOrEmpty(s))
                {
                    return s;
                }
            }
            return "*";
            
        }

        /// <summary>
        /// Returns the first sentence of the last output from the bot
        /// </summary>
        /// <returns>the first sentence of the last output from the bot</returns>
        public string getThat()
        {
            return this.getThat(0, 0);
        }

        /// <summary>
        /// Returns the first sentence of the output "n" steps ago from the bot
        /// </summary>
        /// <param name="n">the number of steps back to go</param>
        /// <returns>the first sentence of the output "n" steps ago from the bot</returns>
        public string getThat(int n)
        {
            return this.getThat(n, 0);
        }

        public int SailentResultCount
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
        /// Returns the sentence numbered by "sentence" of the output "n" steps ago from the bot
        /// </summary>
        /// <param name="n">the number of steps back to go</param>
        /// <param name="sentence">the sentence number to get</param>
        /// <returns>the sentence numbered by "sentence" of the output "n" steps ago from the bot</returns>
        public string getThat(int n, int sentence)
        {
            Result historicResult = GetResult(n);
            if (historicResult!=null)
            {
                return historicResult.GetOutputSentence(sentence);
            }
            return string.Empty;
        }

        public void setOutputSentence(int n, int sent, string data)
        {
            if (n >= this.Results.Count)
            {
                this.Results[n] = new Result(this, this.bot, new Request("", this, this.bot));
            }
            Result historicResult = (Result)this.Results[n];
            historicResult.OutputSentences[sent] = data;

        }
        /// <summary>
        /// Returns the first sentence of the last output from the bot
        /// </summary>
        /// <returns>the first sentence of the last output from the bot</returns>
        public string getInputSentence()
        {
            return this.getInputSentence(0, 0);
        }

        /// <summary>
        /// Returns the first sentence from the output from the bot "n" steps ago
        /// </summary>
        /// <param name="n">the number of steps back to go</param>
        /// <returns>the first sentence from the output from the bot "n" steps ago</returns>
        public string getInputSentence(int n)
        {
            return this.getInputSentence(n, 0);
        }

        /// <summary>
        /// Returns the identified sentence number from the output from the bot "n" steps ago
        /// </summary>
        /// <param name="n">the number of steps back to go</param>
        /// <param name="sentence">the sentence number to return</param>
        /// <returns>the identified sentence number from the output from the bot "n" steps ago</returns>
        public string getInputSentence(int n, int sentence)
        {
            Result historicResult = GetResult(n);
            if (historicResult != null)
            {
                return historicResult.GetInputSentence(sentence);
            }
            return string.Empty;
        }


        public Result GetResult(int i)
        {
            return GetResult(i, false);
        }
        public Result GetResult(int i, bool mustBeSalient)
        {
            return GetResult(i, mustBeSalient, null);
        }
        public Result GetResult(int i, bool mustBeSalient, UserConversationScope responder)
        {
            bool mustBeResponder = responder != null;
            //if (i == -1) return CurrentRequest.CurrentResult;
            lock (Results)
            {
                if (i >= Results.Count)
                {
                    return null;
                }
                {
                    foreach (var r in Results)
                    {
                        //if (r.Responder == this) continue;
                        //if (mustBeResponder) if (r.Responder != responder) continue;
                        //if (mustBeSalient && !r.IsSalient) continue;
                        if (i == 0) return r;
                        i--;
                    }
                    return null;
                }
            }
        }


        /// <summary>
        /// Adds the latest result from the bot to the Results collection
        /// </summary>
        /// <param name="latestResult">the latest result from the bot</param>
        public void addResult(Result latestResult)
        {
            this.Results.Insert(0, latestResult);
        }
        public void setResult(int n, Result desiredResult)
        {
            this.Results[n]= desiredResult;
        }
        #endregion

        public IEnumerable<string> getThats()
        {
            return new[] { getLastBotOutput() };
        }
        public IEnumerable<string> getTopics()
        {
            return new[] {Topic};
        }
        public IEnumerable<string> getPreStates()
        {
            return new[] { State };
        }
        public IEnumerable<string> getPostStates()
        {
            return new[] { State };
        }
    }
}
#endif