namespace RTParser.Utils
{
    public interface Utterance
    {
        /// <summary>
        /// The user that made this request
        /// </summary>
        UserConversationScope Requester { get; }
        /// <summary>
        /// The user respoinding to the request
        /// </summary>
        UserConversationScope Responder { get; }
        /// <summary>
        /// The last meaning unit extracted from what the responder said
        /// </summary>
        Unifiable That { get; }
        /// <summary>
        /// The raw input from the user
        /// (also the last thing the requester said)
        /// </summary>
        Unifiable RawInput { get; }
    }
}