using System;
using System.Collections.Generic;
using System.Text;
using RTParser;
using RTParser.Utils;

namespace RTParser
{
    /// <summary>
    /// Encapsulates information and history of a user who has interacted with the bot
    /// </summary>
    public class User
    {
        #region Attributes

        public int LastResponseGivenTime = 0;
        public bool RespondToChat = true;
        public int MaxRespondToChatPerMinute = 10;

        /// <summary>
        /// The local instance of the GUID that identifies this user to the bot
        /// </summary>
        private Unifiable id;

        /// <summary>
        /// The bot this user is using
        /// </summary>
        public RTParser.RTPBot bot;

        /// <summary>
        /// The GUID that identifies this user to the bot
        /// </summary>
        public Unifiable UserID
        {
            get{return this.id;}
        }

        /// <summary>
        /// A collection of all the result objects returned to the user in this session
        /// </summary>
        private List<Result> Results = new List<Result>();

		/// <summary>
		/// the value of the "topic" predicate
		/// </summary>
        public Unifiable Topic
        {
            get
            {
                return this.Predicates.grabSetting("topic");
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
        {
            if (UserID.Length > 0)
            {
                this.id = UserID;
                this.bot = bot;
                this.Predicates = new RTParser.Utils.SettingsDictionary(this.bot, provider);
                this.bot.DefaultPredicates.Clone(this.Predicates);
                this.Predicates.addSetting("topic", Unifiable.STAR);
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
        /// Returns the Unifiable to use for the next that part of a subsequent path
        /// </summary>
        /// <returns>the Unifiable to use for that</returns>
        public Unifiable getLastBotOutput()
        {
            if (this.Results.Count > 0)
            {
                return ((Result)Results[0]).RawOutput;
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
            return this.getThat(0,0);
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
            if ((n >= 0) & (n < this.Results.Count))
            {
                Result historicResult = (Result)this.Results[n];
                if ((sentence >= 0) & (sentence < historicResult.OutputSentences.Count))
                {
                    return (Unifiable)historicResult.OutputSentences[sentence];
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
            if ((n >= 0) & (n < this.Results.Count))
            {
                Result historicResult = (Result)this.Results[n];
                if ((sentence >= 0) & (sentence < historicResult.InputSentences.Count))
                {
                    return (Unifiable)historicResult.InputSentences[sentence];
                }
            }
            return Unifiable.Empty;
        }

        /// <summary>
        /// Adds the latest result from the bot to the Results collection
        /// </summary>
        /// <param name="latestResult">the latest result from the bot</param>
        public void addResult(Result latestResult)
        {
            this.Results.Insert(0, latestResult);
        }
        #endregion
    }
}