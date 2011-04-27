using System.Collections.Generic;
using RTParser.Utils;

namespace RTParser
{
    public interface UserConversationScope : UserStaticModel, IUser
    {
        /// <summary>
        /// Returns the sematantic meaning to use for the next <that/> part of a subsequent path
        /// </summary>
        /// <returns>the Unifiable to use for that</returns>
        Unifiable LastSaidByReponder(UserConversationScope responder);

        /// <summary>
        /// Returns the first sentence of the last output from the bot
        /// </summary>
        /// <returns>the first sentence of the last output from the bot</returns>
        Unifiable getThat(User responder);

        /// <summary>
        /// Returns the first sentence of the output "n" steps ago from the bot
        /// </summary>
        /// <param name="n">the number of steps back to go</param>
        /// <returns>the first sentence of the output "n" steps ago from the bot</returns>
        Unifiable getThat(int n, User responder);

        /// <summary>
        /// Returns the sentence numbered by "sentence" of the output "n" steps ago from the bot
        /// </summary>
        /// <param name="n">the number of steps back to go</param>
        /// <param name="sentence">the sentence number to get</param>
        /// <returns>the sentence numbered by "sentence" of the output "n" steps ago from the bot</returns>
        Unifiable getThat(int n, int sentence, User responder);

        /// <summary>
        /// Returns the first sentence of the last output from the bot
        /// </summary>
        /// <returns>the first sentence of the last output from the bot</returns>
        Unifiable getInputSentence(User responder);

        /// <summary>
        /// Returns the first sentence from the output from the bot "n" steps ago
        /// </summary>
        /// <param name="n">the number of steps back to go</param>
        /// <returns>the first sentence from the output from the bot "n" steps ago</returns>
        Unifiable getInputSentence(int n, User responder);

        /// <summary>
        /// Returns the identified sentence number from the input from the bot "n" steps ago
        /// </summary>
        /// <param name="n">the number of steps back to go</param>
        /// <param name="sentence">the sentence number to return</param>
        /// <returns>the identified sentence number from the input from the bot "n" steps ago</returns>
        Unifiable getInputSentence(int n, int sentence, User responder);

        /// <summary>
        /// Returns the identified sentence number from the input from the bot "n" steps ago
        /// </summary>
        /// <param name="n">the number of steps back to go</param>
        /// <param name="sentence">the sentence number to return</param>
        /// <returns>the identified sentence number from the input from the bot "n" steps ago</returns>
        Unifiable getRequestSentence(int n, int sentence, User responder);

        /// <summary>
        /// Returns the first sentence of the last output from the bot
        /// </summary>
        /// <returns>the first sentence of the last output from the bot</returns>
        Unifiable getResponseSentence(User responder);

        /// <summary>
        /// Returns the first sentence from the output from the bot "n" steps ago
        /// </summary>
        /// <param name="n">the number of steps back to go</param>
        /// <returns>the first sentence from the output from the bot "n" steps ago</returns>
        Unifiable getResponseSentence(int n, User responder);

        /// <summary>
        /// Returns the identified sentence number from the output from the bot "n" steps ago
        /// </summary>
        /// <param name="n">the number of steps back to go</param>
        /// <param name="sentence">the sentence number to return</param>
        /// <returns>the identified sentence number from the output from the bot "n" steps ago</returns>
        Unifiable getResponseSentence(int n, int sentence, User responder);

        Result GetResult(int i);
        Result GetResult(int i, bool mustBeSalient);
        Result GetResult(int i, bool mustBeSalient, UserConversationScope responder);

        Result LastResult { get; }
        int SailentResultCount { get; }
        Unifiable That { get; set; }
        Unifiable ResponderJustSaid { get; set; }
        UserConversationScope LastResponder { get; set; }
        Unifiable JustSaid { get; set; }
        /// <summary>
        /// the value of the "topic" predicate
        /// </summary>

        Unifiable Topic { get; }
        Unifiable TopicSetting { get; set; }
        IList<Unifiable> Topics { get; }

        IEnumerable<Unifiable> BotOutputs { get; }

        PrintOptions PrintOptions { get; set; }
    }
}