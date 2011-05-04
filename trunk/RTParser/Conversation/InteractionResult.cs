using System;
using System.Collections.Generic;
using RTParser.Utils;
using RTParser.Variables;

namespace RTParser
{
    public interface InteractionResult
    {
        /// <summary>
        /// The bot that is providing the answer
        /// </summary>
        //     RTPBot TargetBot { get; set; }
        /// The user that is providing the <that/> answer
        //        User Responder { get; set; }
        //        string WhyComplete { get; set; }
        bool IsTraced { get; set; }

        /// <summary>
        /// The individual sentences that constitute the raw input from the user
        /// </summary>
        List<Unifiable> InputSentences { get; }

        /// <summary>
        /// The normalized sentence(s) (paths) fed into the graphmaster
        /// </summary>
        //List<Unifiable> InputPaths { get; }

        /// <summary>
        /// The individual sentences produced by the bot that form the complete response
        /// </summary>
        List<Unifiable> OutputSentences { get; }

        /// <summary>
        /// The request from the user
        /// </summary>
        Request request { get; }
        Result result { get; }

        Utterance ChatOutput { get; }

        /// <summary>
        /// The user for whom this is a result
        /// </summary>
        // OutputDelegate writeToLog { get; set; }
        int TemplatesSucceeded { get; set; }

        int OutputsCreated { get; set; }
        //   GraphQuery TopLevel { get; set; }
        double Score { get; }

        /// <summary>
        /// If the query is being traced
        /// </summary>
        //    bool IsTraced { get; set; }
        string SetOutput { set; }

        /// <summary>
        /// The raw input from the user
        /// </summary>
        //   Unifiable rawInput { get; }

        /// <summary>
        /// The result from the bot with logging and checking
        /// </summary>
        Unifiable Output { get; }

        string EnglishOutput { get; }

        /// <summary>
        /// Returns the raw sentences without any logging 
        /// </summary>
        Unifiable RawOutput { get; }

        //bool IsEmpty { get; }

        ISettingsDictionary RequesterChanges { get; }
        ISettingsDictionary ResponderChanges { get; }
        bool IsSalient { get; }
        InteractionResult PreviousInteraction { get; }
        InteractionResult NextInteraction { get; }
        Unifiable NormalizedOutput { get; }
        double TemplateRating { get; set; }
        bool Started { get; set; }
        TimeSpan Durration { get; }
        string WhyResultComplete { get; set; }
        UserConversationScope Requester { get; }
        //   User Requester { get; set; }
        //   User Responder { get; }
        string ToString();
        Unifiable GetInputSentence(int sentence);
        Unifiable GetOutputSentence(int sentence);
        void FreeRequest();
        void FreeResult();
    }
}