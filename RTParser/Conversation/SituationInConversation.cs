using System.Collections.Generic;
using RTParser.Utils;

namespace RTParser
{
    public interface SituationInConversation
    {
        UserConversationScope Requester { get; }
        Utterance TheUtterence { get; }
        IEnumerable<UserConversationScope> Receivers { get; }
        SubQuery CurrentQuery { get; set; }
    }
}