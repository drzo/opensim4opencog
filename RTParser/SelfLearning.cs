using System;
using System.Collections;
using System.Collections.Generic;
using System.Globalization;
using System.IO;
using System.Threading;
using System.Xml;
using LAIR.ResourceAPIs.WordNet;
using MushDLR223.ScriptEngines;
using MushDLR223.Utilities;
using MushDLR223.Virtualization;
using RTParser.AIMLTagHandlers;
using RTParser.Database;
using RTParser.Normalize;
using RTParser.Utils;
using RTParser.Variables;
using UPath = RTParser.Unifiable;
using UList = System.Collections.Generic.List<RTParser.Utils.TemplateInfo>;


namespace RTParser
{
    public partial class RTPBot
    {
        private bool AllreadyUnderstandingSentences = false;
        readonly private object AllreadyUnderstandingSentencesLock = new object();
        public static bool UnderstandSentenceOutsideQueue = true;
        public void HeardSelfSayVerbal(User theFactSpeaker, User toWhom, string message, Result result, ThreadControl control)
        {
            message = ToHeard(message);
            if (string.IsNullOrEmpty(message)) return;
            currentEar.AddMore(message);
            if (!currentEar.IsReady())
            {
                return;
            }
            //responderJustSaid = message;
            message = currentEar.GetMessage();
            currentEar = new JoinedTextBuffer();
            result = result ?? (toWhom == null ? null : toWhom.LastResult);
            if (control == null && !UnderstandSentenceOutsideQueue)
            {
                ManualResetEvent newManualResetEvent = new ManualResetEvent(false);
                control = new ThreadControl(newManualResetEvent);
            }
            HeardSelfSay1Sentence(theFactSpeaker, toWhom, message, result, control);
        }

        public Result HeardSelfSayResponse(User theFactSpeaker, User toWhom, string message, Result resultOfToWhomSpeakingToFactSpeaker, ThreadControl control)
        {
            var result = resultOfToWhomSpeakingToFactSpeaker;
            Result LR = null;
            if (message == null) return result;
            bool lts = ListeningToSelf;
            bool prochp = ProcessHeardPreds;
            if (!lts && !prochp) return result;
            message = ToHeard(message);
            if (string.IsNullOrEmpty(message)) return result;

            var v = new SplitIntoSentences(null, message);

            bool stopProcessing = false;
            if (control != null) control.AbortOrInteruptedRaised += (ctl, ex) => { stopProcessing = true; };
            var sentences = v.Transform();
            for (int i = 0; i < sentences.Length; i++)
            {
                Unifiable sentence = sentences[i];
                if (stopProcessing)
                {
                    Unifiable unifiableJoin = Unifiable.Join(" <br> ", sentences, i, sentences.Length - 1);
                    return AbortedResult(unifiableJoin, result, control);
                }
                result = HeardSelfSay1Sentence(theFactSpeaker, toWhom, sentence, result, control);
            }
            return result;
        }


        internal Result AbortedResult(string message, Result result, ThreadControl control)
        {
            return result;
        }
            
        public Result HeardSelfSay1Sentence(User theFactSpeaker, User toWhom, string message, Result result, ThreadControl control)
        {
            string whatListenerLastSaid = toWhom.JustSaid;
            Result res = HeardSelfSay11Sentence(theFactSpeaker, toWhom, message, result, control);
            toWhom.ResponderJustSaid = message;
            theFactSpeaker.JustSaid = message;
            toWhom.JustSaid = whatListenerLastSaid;
            return res;
        }

        public Result HeardSelfSay11Sentence(User theFactSpeaker, User toWhom, string message, Result result, ThreadControl control)
        {
            Result LR = result;
            if (message == null) return LR;

            bool lts = ListeningToSelf;
            bool prochp = ProcessHeardPreds;
            if (!lts && !prochp) return LR;

            message = ToHeard(message);
            if (string.IsNullOrEmpty(message)) return LR;
            //message = swapPerson(message);
            //writeDebugLine("HEARDSELF SWAP: " + message);
            User JLU = LastUser;
            if (JLU != null)
            {
                string uname = JLU.UserName;
                if (uname == null || uname.Length < 2)
                {
                }
                else
                {
                    if (message.Replace(" ", "_").ToLower().Contains(uname.Replace(" ", "_").ToLower()))
                    {
                        JLU.NameUsedOrGivenTime = DateTime.Now;
                    }
                }
            }


            if (prochp)
            {
                writeDebugLine("-----------------------------------------------------------------");
                AddHeardPreds(message, HeardPredicates);
                writeDebugLine("-----------------------------------------------------------------");
            }
            if (!lts)
            {
                writeDebugLine("-----------------------------------------------------------------");
                writeDebugLine("SELF: " + message);
                writeDebugLine("-----------------------------------------------------------------");
                return null;
            }
            bool wasQuestion = NatLangDb.WasQuestion(message);
            string desc = string.Format("ROBOT {1} USER: {0}", message, wasQuestion ? "ASKS" : "TELLS");
            string realLast = toWhom.JustSaid;
            var res = RememberSpoken(theFactSpeaker, toWhom, desc, message, result, control);
            theFactSpeaker.JustSaid = message;
            toWhom.JustSaid = realLast;
            return res;
        }

        private Result RememberSpoken(User theFactSpeaker, User toWhom, string desc, string message, Result result, ThreadControl control)
        {
            message = ToHeard(message);
            if (string.IsNullOrEmpty(message)) return result;
            writeChatTrace(desc);
            if (control == null || UnderstandSentenceOutsideQueue)
            {
                var res = UnderstandSentence(theFactSpeaker, toWhom, desc, message, result, control);
                theFactSpeaker.JustSaid = message;
                return res;
            }

            control.AbortRaised += (ctl, abrtedE) => writeToLog("Aborted " + desc);
            HeardSelfSayQueue.EnqueueWithTimeLimit(()
                                                   => result = UnderstandSentence(theFactSpeaker, toWhom, desc, message, result, control), desc,
                                                   TimeSpan.FromSeconds(10), control);
            control.WaitUntilComplete();
            return result;
        }

        private Result UnderstandSentence(User theFactSpeaker, User toWhom, string desc, string message, Result res, ThreadControl control)
        {
            string whatListenerLastSaid = toWhom == null ? null : toWhom.JustSaid;
            theFactSpeaker.JustSaid = message;
            Result LR = res;
            if (LR != null && toWhom != null)
            {
                //lock (toWhom.QueryLock)
                {
                    LR = toWhom.GetResult(0, true, theFactSpeaker);
                    if (LR != null)
                    {
                        LR.request.AddOutputSentences(null, message, res);
                    }
                }
            }
            if (AllreadyUnderstandingSentences)
            {
                writeDebugLine("HEARDSELF: TOOOO FAST - " + message);
                return res;
            }
            lock (AllreadyUnderstandingSentencesLock)
            {
                if (AllreadyUnderstandingSentences)
                {
                    writeDebugLine("HEARDSELF: TWOOOO FAST - " + message);
                    return res;
                }
                AllreadyUnderstandingSentences = true;
            }
            writeDebugLine("-----------------------------------------------------------------");
            writeDebugLine("HEARDSELF: " + message);
            writeDebugLine("-----------------------------------------------------------------");
            //return LR;
            try
            {
                if (message == null || message.Length < 4) return null;
                Request resrequest = null;
                if (res != null)
                {
                    resrequest = res.request;
                    resrequest.ParentMostRequest.DisallowedGraphs.Clear();
                }
                Request r = theFactSpeaker.CreateRequest(message, toWhom, HeardSelfSayGraph, null);
                // irregardless we only mentally played what the responder responded with
                r.ResponderSelfListens = false;
                // because the  listener cant hear this inner dialog
                r.SaveResultsOnJustHeard = false;
                if (control != null) control.AbortRaised += (ctl, abrtedE) =>
                {
                    r.WhyComplete = "THREADABORTED";
                };
                var G = HeardSelfSayGraph ?? GraphMaster;
                r.Graph = G;
                r.IsTraced = true;
                r.IsToplevelRequest = true;
                try
                {
                    Result res2 = ChatWithRequest(r);
                    writeDebugLine("-----------------------------------------------------------------");
                    writeDebugLine("COMPLETED HEARDSELF: " + message + " res = " + res2);
                    writeDebugLine("-----------------------------------------------------------------");
                    return res2;
                }
                catch (ChatSignal e)                
                {
                    writeDebugLine("-----------------------------------------------------------------");
                    writeDebugLine("INCOMPLETE HEARDSELF: " + message + " req = " + r + " " + e);
                    writeDebugLine("-----------------------------------------------------------------");
                    throw;
                }
                catch (Exception e)
                {
                    writeToLog(e);
                    throw;
                    //return null;
                }
            }
            finally
            {
                AllreadyUnderstandingSentences = false;
                // FYI: this "JustSaid" sets "lastsaid" in dictioanry
                if (toWhom != null) toWhom.JustSaid = whatListenerLastSaid;
                theFactSpeaker.JustSaid = message;
            }
        }

        private void AddHeardPreds(string message, SettingsDictionary dictionary)
        {
            if (message == null) return;
            writeDebugLine("-----------------------------------------------------------------");
            foreach (string s in Trim(message).Split(new[] { '!', '?', '.' }, StringSplitOptions.RemoveEmptyEntries))
            {
                AddHeardPreds0(s, dictionary);
            }
            writeDebugLine("-----------------------------------------------------------------");
            //RTPBot.writeDebugLine("" + dictionary.ToDebugString());
        }

        private void AddHeardPreds0(Unifiable unifiable, SettingsDictionary dictionary)
        {
            if (IsNullOrEmpty(unifiable)) return;
            Unifiable first = unifiable.First();
            if (IsNullOrEmpty(first)) return;
            Unifiable rest = unifiable.Rest();
            if (IsNullOrEmpty(rest)) return;
            dictionary.addSetting(first, rest);
            AddHeardPreds0(rest, dictionary);
        }

        private GraphMaster SetupUserWithGraph(string newname, string newkey, User newuser)
        {
            GraphMaster graph = GetUserGraph(newkey);
            graph.AddParallelMT(RTPBot.TheUserListernerGraph, newuser.WriteToUserTrace);
            newuser.ListeningGraph = graph;
            newuser.UserID = newkey;
            newuser.UserName = newname;
            OnBotCreated(() => graph.AddGenlMT(GraphMaster, newuser.WriteToUserTrace));
            newuser.Predicates.IsIdentityReadOnly = false;
            newuser.Predicates.addSetting("name", newname);
            newuser.Predicates.addSetting("id", newkey);
            newuser.Predicates.InsertFallback(() => AllUserPreds);
            newuser.SyncDirectory(GetUserDir(newkey));
            if (graph.Size == 0) graph.UnTraced = true;
            newuser.Predicates.IsIdentityReadOnly = true;
            return graph;
        }
    }
}