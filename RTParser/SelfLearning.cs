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
        public static bool UnderstandSentenceOutsideQueue = false;
        public bool TurnOffSelfListening = false;
        public bool WaitUntilVerbalOutput = false;

        public void HeardSelfSayVerbal(User theFactSpeaker, User toWhom, string message, Result result, ThreadControl control)
        {
            message = ToHeard(message);
            if (string.IsNullOrEmpty(message)) return;
            message = message.TrimStart();
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
            HeardSomeoneSay1Sentence(theFactSpeaker, toWhom, message, result, control);
        }

        public Result HeardSelfSayResponse(User theFactSpeaker, User toWhom, string message, Result resultOfToWhomSpeakingToFactSpeaker, ThreadControl control)
        {
            if (this.TurnOffSelfListening) return resultOfToWhomSpeakingToFactSpeaker;
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
                result = HeardSomeoneSay1Sentence(theFactSpeaker, toWhom, sentence, result, control);
            }
            return result;
        }


        internal Result AbortedResult(string message, Result result, ThreadControl control)
        {
            return result;
        }
            
        public Result HeardSomeoneSay1Sentence(User theFactSpeaker, User toWhom, string message, Result result, ThreadControl control)
        {
            bool toWhomNonNull = toWhom != null;
            string whatListenerLastSaid = null;
            if (toWhomNonNull) whatListenerLastSaid = toWhom.JustSaid;
            var log = ConversationLog.GetConversationLog(theFactSpeaker, toWhom, true);
            Utterance ele = log.AddSpoken(this, theFactSpeaker, toWhom, message);
            Result res = HeardSome1Say11Sentence(theFactSpeaker, toWhom, ele, message, result, control);
            if (toWhomNonNull) toWhom.ResponderJustSaid = message;
            theFactSpeaker.JustSaid = message;
            if (toWhomNonNull) toWhom.JustSaid = whatListenerLastSaid;
            return res;
        }

        public Result HeardSome1Say11Sentence(User theFactSpeaker, User toWhom, Utterance ele, string message, Result result, ThreadControl control)
        {
            if (this.TurnOffSelfListening) return result;
            Result LR = result;
            if (message == null) return LR;

            bool lts = ListeningToSelf;
            bool prochp = ProcessHeardPreds;
            if (!lts && !prochp) return LR;
            
            message = GetUserMessage(message, ref theFactSpeaker, ref toWhom);

            bool toWhomNonNull = toWhom != null;
            string debug = "HeardSelfSay11Sentence: \"" + theFactSpeaker ?? "theFactSpeaker" + ": " + toWhom ?? "toWhom" + ", " + message + "\"";
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
                writeDebugLine(theFactSpeaker + ": " + message);
                writeDebugLine("-----------------------------------------------------------------");
                return null;
            }
            bool wasQuestion = NatLangDb.WasQuestion(message);
            string desc = string.Format("ROBOT {1} USER: {0}", message, wasQuestion ? "ASKS" : "TELLS");
            string realLast =null;
            if (toWhomNonNull)  realLast = toWhom.JustSaid;
            var res = RememberSpoken(ele, theFactSpeaker, toWhom, desc, message, result, control);
            theFactSpeaker.JustSaid = message;
            if (toWhomNonNull) toWhom.JustSaid = realLast;
            return res;
        }

        private string GetUserMessage(string message, ref User theFactSpeaker, ref User toWhom)
        {
            string newMessage;
            string newToWhom;
            string newFromWhom;
            if (theFactSpeaker == BotAsUser &&
                TextPatternUtils.MessagePrefixName(":", message, out newToWhom, out newFromWhom, out newMessage))
            {
                message = newMessage;
                theFactSpeaker = FindOrCreateUser(newFromWhom);
                toWhom = FindOrCreateUser(newToWhom);
            }
            return message;
        }

        public string GetUserMessage(string message, ref string theFactSpeaker, ref string toWhom)
        {
            string newMessage;
            string newToWhom;
            string newFromWhom;
            if (BotAsUser.IsNamed(theFactSpeaker) &&
                TextPatternUtils.MessagePrefixName(":", message, out newToWhom, out newFromWhom, out newMessage))
            {
                message = newMessage;
                theFactSpeaker = newFromWhom;
                toWhom = newToWhom;
            }
            return message;
        }

        private Result RememberSpoken(Utterance ele, User theFactSpeaker, User toWhom, string desc, string message, Result result, ThreadControl control)
        {
            if (this.TurnOffSelfListening) return result;
            message = ToHeard(message);
            if (string.IsNullOrEmpty(message)) return result;
            writeChatTrace(desc);
            if (UnderstandSentenceOutsideQueue)
            {
                var res = UnderstandSentence(ele, theFactSpeaker, toWhom, desc, message, result, control);
                theFactSpeaker.JustSaid = message;
                return res;
            }

            if (control != null) control.AbortRaised += (ctl, abrtedE) => writeToLog("Aborted " + desc);
            HeardSelfSayQueue.EnqueueWithTimeLimit(()
                                                   => result = UnderstandSentence(ele, theFactSpeaker, toWhom, desc, message, result, control), desc,
                                                   TimeSpan.FromSeconds(10), control);
            //if (control != null) control.WaitUntilComplete();
            return result;
        }

        private Result UnderstandSentence(Utterance ele, User theFactSpeaker, User toWhom, string desc, string message, Result res, ThreadControl control)
        {
            if (this.TurnOffSelfListening) return res;
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
                        LR.request.AddOutputSentences(null, message, res, res.request.TopLevelScore);
                    }
                }
            }
            if (AllreadyUnderstandingSentences)
            {
                writeDebugLineBannered("HEARDSELF: TOOOO FAST - " + message);
                return res;
            }
            lock (AllreadyUnderstandingSentencesLock)
            {
                if (AllreadyUnderstandingSentences)
                {
                    writeDebugLineBannered("HEARDSELF: TWOOOO FAST - " + message);
                    return res;
                }
                AllreadyUnderstandingSentences = true;
            }
            writeDebugLineBannered("HEARDSELF: " + message);
            //return LR;
            try
            {
                if (message == null || message.Length < 4) return null;
                Request r = theFactSpeaker.CreateRequest(message, toWhom, DefaultHeardSelfSayGraph, null);
                // irregardless we only mentally played what the responder responded with
                r.ResponderSelfListens = false;
                // because the  listener cant hear this inner dialog
                r.SaveResultsOnJustHeard = false;
                if (control != null) control.AbortRaised += (ctl, abrtedE) =>
                {
                    r.WhyComplete = "THREADABORTED";
                };
                var G = DefaultHeardSelfSayGraph;
                if (G == null)
                {
                    writeDebugLineBannered("HeardSelfSayGraph == null: " + message + " res = " + res);
                    return res;
                }
                r.Graph = G;
                r.IsTraced = true;
                r.IsToplevelRequest = true;
                try
                {
                    Result res2 = ChatWithRequest(r);                   
                    ele.IsSpeakerInputGleaned = true;
                    writeDebugLineBannered("COMPLETED HEARDSELF: " + message + " res = " + res2);
                    return res2;
                }
                catch (ChatSignal e)                
                {
                    writeDebugLineBannered("INCOMPLETE HEARDSELF: " + message + " req = " + r + " " + e);
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

        internal static void writeDebugLineBannered(string message)
        {
            writeDebugLine("-----------------------------------------------------------------");
            writeDebugLine(message);
            writeDebugLine("-----------------------------------------------------------------");
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
            Unifiable first = unifiable.First;
            if (IsNullOrEmpty(first)) return;
            Unifiable rest = unifiable.Rest;
            if (IsNullOrEmpty(rest)) return;
            dictionary.addSetting(first, rest);
            AddHeardPreds0(rest, dictionary);
        }
        private GraphMaster SetupUserWithGraph(string newname, string newkey, User newuser)
        {
            GraphMaster graph = GetUserGraph(newkey);
            SetupUserWithGraph0(graph, newname, newkey, newuser);
            return graph;
        }
        private void SetupUserWithGraph0(GraphMaster graph, string newname, string newkey, User newuser)
        {            
            //graph.AddParallelMT(RTPBot.TheUserListenerGraph, newuser.WriteToUserTrace);
            newuser.StartGraph = graph;
            newuser.UserID = newkey;
            newuser.UserName = newname;
            OnBotCreated(() => graph.AddGenlMT(DefaultStartGraph, newuser.WriteToUserTrace));
            newuser.Predicates.IsIdentityReadOnly = false;
            newuser.Predicates.addSetting("name", newname);
            newuser.Predicates.addSetting("id", newkey);
            newuser.Predicates.InsertFallback(() => AllUserPreds);
            newuser.AddTodoItem(() =>
                                    {
                                        newuser.SyncDirectory(GetUserDir(newkey));
                                        if (graph.Size == 0) graph.UnTraced = true;
                                    });
            newuser.Predicates.IsIdentityReadOnly = true;
        }

        public static string NoSpaceLowerCaseName(string path)
        {
            path = ToLower(Trim(ConsolidSpaces(path)));
            return OlderReference(
                path,
                path
                    .Replace(" ", "_").Replace(".", "_")
                    .Replace("-", "_").Replace("__", "_"));
        }

        private bool DoLogCmd(OutputDelegate console, bool showHelp, string cmd1, string args1)
        {
            if (showHelp) console("@log " + RTPBot.AIMLDEBUGSETTINGS);
            if (cmd1.StartsWith("log"))
            {
                RTPBot.LoggedWords.UpateLogging(args1, console);
                return true;
            }
            if (cmd1 == "on" || cmd1 == "off")
            {
                return true;
            }
            return false;
        }
    }
}