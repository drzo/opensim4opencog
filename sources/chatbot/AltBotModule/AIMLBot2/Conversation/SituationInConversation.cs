using System.Collections.Generic;
using AltAIMLbot;
using AltAIMLbot.Utils;

namespace AltAIMLbot
{
    public interface SituationInConversation
    {
        UserConversationScope Requester { get; }
        Utterance TheUtterence { get; }
        IEnumerable<UserConversationScope> Receivers { get; }
        SubQuery CurrentQuery { get; set; }
    }
}