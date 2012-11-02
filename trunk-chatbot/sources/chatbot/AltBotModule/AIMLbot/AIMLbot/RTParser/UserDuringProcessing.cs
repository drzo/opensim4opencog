using System.Collections.Generic;
using AltAIMLbot;
using AltAIMLParser;
using MushDLR223.Utilities;
using RTParser.AIMLTagHandlers;
using RTParser.Utils;
using RTParser.Variables;

namespace RTParser
{
    public interface UserDuringProcessing : ISettingsDictionary, IUser, UserConversationScope
    {
        actMSM botActionMSM { get; }

        //void SetOutputSentences(string args, User responder);
        bool CanUseTemplate(TemplateInfo info, Result request);

        ListAsSet<TemplateInfo> VisitedTemplates { get; set; }
        ListAsSet<TemplateInfo> UsedChildTemplates { get; set; }
        ListAsSet<TemplateInfo> DisabledTemplates { get; set; }
        ListAsSet<TemplateInfo> ProofTemplates { get; set; }
        ICollection<GraphMaster> DisallowedGraphs { get; set; }
#if DEBUG_ALLQUERIES
        ListAsSet<GraphQuery> AllQueries { get; set; }
#endif
        //int depth { get; set; }

        string grabSettingNoDebug(string arg);
        void Enter(ConversationScopeHolder srai);
        void Exit(ConversationScopeHolder srai);

        GraphMaster HeardSelfSayGraph { get; set; }
        void addResultTemplates(Result result);
        void addRequestTemplates(Request request);
        void addResult(Result result);

        string StartGraphName { get; set; }
        SettingsDictionary Predicates { get; /*set;*/ }
        void InsertProvider(ParentProvider pp);
        Request CurrentRequest { get; set; }
        bool SuspendAddResultToUser { get; set; }
        //string UserID { get; set; }
        //  UserDuringProcessing LastResponder { get; }
        //  Unifiable ResponderJustSaid { get; set; }
        //  Unifiable JustSaid { get; set; }
        bool IsValid { get; set; }
        //  string UserName { get; set; }
    }
}