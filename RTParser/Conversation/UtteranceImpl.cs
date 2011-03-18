using RTParser.Utils;

namespace RTParser
{
    public class UtteranceImpl : Utterance
    {
        
        #region Implementation of Utterance

        /// <summary>
        /// The user that made this request
        /// </summary>
        public UserConversationScope Requester { get; private set; }

        /// <summary>
        /// The user respoinding to the request
        /// </summary>
        public UserConversationScope Responder { get; private set; }

        /// <summary>
        /// 
        /// </summary>
        public Unifiable That { get; private set; }

        /// <summary>
        /// The raw input from the user
        /// </summary>
        public Unifiable RawInput { get; private set; }

        #endregion
    }
}