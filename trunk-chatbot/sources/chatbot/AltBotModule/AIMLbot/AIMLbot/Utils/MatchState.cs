using System;

namespace AltAIMLbot.Utils
{
    /// <summary>
    /// Denotes what part of the input path a node represents.
    /// 
    /// Used when pushing values represented by wildcards onto collections for
    /// the star, thatstar and topicstar AIML values.
    /// </summary>
    public enum MatchState
    {
        Pattern,
        That,
        Topic,
        State,
        Flag,
    }
}
