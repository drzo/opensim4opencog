using System;
using System.Collections;
using System.Collections.Generic;
using System.Globalization;
using System.IO;
using System.Threading;
using System.Xml;
using AltAIMLbot;
using AltAIMLParser;
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
    public partial class AltBot
    {
        private bool AllreadyUnderstandingSentences = false;
        readonly private object AllreadyUnderstandingSentencesLock = new object();
        public static bool UnderstandSentenceOutsideQueue = false;
        public static bool TryUnderstandSentence = false;
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
            if (this.TurnOffSelfListening) return null;
            Result LR = null;
            if (message == null) return null;
            bool lts = ListeningToSelf;
            bool prochp = ProcessHeardPreds;
            if (!lts && !prochp) return null;
            message = ToHeard(message);
            if (string.IsNullOrEmpty(message)) return null;

            Result result = null;
            var v = new SplitIntoSentences(null, message);

            bool stopProcessing = false;
            if (control != null) control.AbortOrInteruptedRaised += (ctl, ex) => { stopProcessing = true; };
            var sentences = v.TransformU();
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
            if (MyBot.useServitor) return null;
            bool toWhomNonNull = toWhom != null;
            string whatListenerLastSaid = null;
            if (toWhomNonNull) whatListenerLastSaid = toWhom.JustSaid;
            var log = ConversationLog.GetConversationLog(theFactSpeaker, toWhom, true);
            Utterance spoken = log.AddSpoken(this, theFactSpeaker, toWhom, message);
            Result res = HeardSome1Say11Sentence(theFactSpeaker, toWhom, spoken, message, result, control);
            if (toWhomNonNull) toWhom.ResponderJustSaid = message;
            ((MasterUser)theFactSpeaker).JustSaid = message;
            if (toWhomNonNull) ((MasterUser)toWhom)._JustSaid = whatListenerLastSaid;
            return res;
        }

        public Result HeardSome1Say11Sentence(User theFactSpeaker, User toWhom, Utterance ele, string message, Result prevResult, ThreadControl control)
        {
            if (this.TurnOffSelfListening) return null;
            if (MyBot.useServitor) return null;
            //Result LR = result;
            if (message == null) return null;

            bool lts = ListeningToSelf;
            bool prochp = ProcessHeardPreds;
            if (!lts && !prochp) return null;
            
            message = GetUserMessage(message, ref theFactSpeaker, ref toWhom);

            bool toWhomNonNull = toWhom != null;
            string debug = "HeardSelfSay11Sentence: \"" + theFactSpeaker ?? "theFactSpeaker" + ": " + toWhom ?? "toWhom" + ", " + message + "\"";
            message = ToHeard(message);
            if (string.IsNullOrEmpty(message)) return null;
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
                writeDebugLine("NO LTS " + theFactSpeaker + ": " + message);
                writeDebugLine("-----------------------------------------------------------------");
                return null;
            }
            bool wasQuestion = NatLangDb.WasQuestion(message);
            string desc = string.Format("ROBOT {1} USER: {0}", message, wasQuestion ? "ASKS" : "TELLS");
            string realLast = null;
            //if (toWhomNonNull)  realLast = toWhom.JustSaid;
            var res = RememberSpoken(ele, theFactSpeaker, toWhom, desc, message, prevResult, control);
            //theFactSpeaker.JustSaid = message;
            //if (toWhomNonNull) toWhom.JustSaid = realLast;
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

        private Result RememberSpoken(Utterance ele, User theFactSpeaker, User toWhom, string desc, string message, Result prevResult, ThreadControl control)
        {
            if (!TryUnderstandSentence) return null;
            if (this.TurnOffSelfListening) return null;
            if (MyBot.useServitor) return null;
            if (NonSalientMessage(message)) return null;
            message = ToHeard(message);
            if (string.IsNullOrEmpty(message)) return null;
            writeChatTrace(desc);
            if (UnderstandSentenceOutsideQueue)
            {
                Result res = UnderstandSentence(ele, theFactSpeaker, toWhom, desc, message, prevResult, control);
                theFactSpeaker.JustSaid = message;
                return res;
            }

            if (control != null) control.AbortRaised += (ctl, abrtedE) => writeToLog("Aborted " + desc);
            Result result;
            HeardSelfSayQueue.EnqueueWithTimeLimit(()
                                                   => result = UnderstandSentence(ele, theFactSpeaker, toWhom, desc, message, prevResult, control), desc,
                                                   TimeSpan.FromSeconds(10), control);
            //if (control != null) control.WaitUntilComplete();
            return null;
        }

        private static bool NonSalientMessage(string message)
        {
            return message.Contains("*") || message.StartsWith("/") || message.StartsWith("@");
        }

        private Result UnderstandSentence(Utterance ele, User theFactSpeaker, User toWhom, string desc, string message, Result prevResult, ThreadControl control)
        {
            if (!TryUnderstandSentence) return null;
            if (this.TurnOffSelfListening) return null;
            if (MyBot.useServitor) return null;
            string whatListenerLastSaid = toWhom == null ? null : toWhom.JustSaid;
            string whatSpeakerLastSaid = theFactSpeaker == null ? null : theFactSpeaker.JustSaid;
            //theFactSpeaker.JustSaid = message;
            if (toWhom != null)
            {
                //lock (toWhom.QueryLock)
                {
                    prevResult = prevResult ?? toWhom.GetResult(0, true, theFactSpeaker);
                    if (prevResult != null)
                    {
                        prevResult.request.AddOutputSentences(null, message, prevResult, prevResult.request.TopLevelScore);
                    }
                }
            }
            if (AllreadyUnderstandingSentences)
            {
                writeDebugLineBannered("HEARDSELF: TOOOO FAST - " + message);
                return null;
            }
            lock (AllreadyUnderstandingSentencesLock)
            {
                if (AllreadyUnderstandingSentences)
                {
                    writeDebugLineBannered("HEARDSELF: TWOOOO FAST - " + message);
                    return null;
                }
                AllreadyUnderstandingSentences = true;
            }
            writeDebugLineBannered("HEARDSELF: " + theFactSpeaker + "->" + toWhom + ": " + message);
            //return LR;
            try
            {
                var HG = DefaultHeardSelfSayGraph;
               
                if (message == null || message.Length < 4) return null;
                Request r = theFactSpeaker.CreateRequest(message, toWhom, null, HG, null);
                // irregardless we only mentally played what the responder responded with
                r.ResponderSelfListens = false;
                // because the  listener cant hear this inner dialog
                r.SaveResultsOnJustHeard = false;
                if (control != null) control.AbortRaised += (ctl, abrtedE) =>
                {
                    r.WhyComplete = "THREADABORTED";
                };
                var G = HG;
                if (G == null)
                {
                    writeDebugLineBannered("HeardSelfSayGraph == null: " + message + " res = " + prevResult);
                    return null;
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
                theFactSpeaker.JustSaid = whatSpeakerLastSaid;
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
            //AltBot.writeDebugLine("" + dictionary.ToDebugString());
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
            //graph.AddParallelMT(AltBot.TheUserListenerGraph, newuser.WriteToUserTrace);
            newuser.StartGraph = graph;
            newuser.UserID = newkey;
            newuser.UserName = newname;
            OnBotCreated(() => graph.AddGenlMT(DefaultStartGraph, newuser.WriteToUserTrace));
            //newuser.Predicates.IsIdentityReadOnly = false;
            newuser.Predicates.addSetting("name", newname);
            newuser.Predicates.addSetting("id", newkey);
            newuser.Predicates.InsertFallback(() => AllUserPreds);
            newuser.AddTodoItem(() =>
                                    {
                                        newuser.SyncDirectory(GetUserDir(newkey));
                                        if (graph.Size == 0) graph.UnTraced = true;
                                    });
            //newuser.Predicates.IsIdentityReadOnly = true;
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
            if (showHelp) console("@log " + AltBot.AIMLDEBUGSETTINGS);
            if (cmd1.StartsWith("log"))
            {
                AltBot.LoggedWords.UpateLogging(args1, console);
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