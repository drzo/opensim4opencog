using System;
using System.Collections.Generic;
using System.Text;
using AltAIMLParser;
using MushDLR223.ScriptEngines;
using MushDLR223.Utilities;
using RTParser.Database;
using RTParser.Normalize;
using RTParser.Utils;

namespace RTParser
{
    public class Utterance
    {

        /// <summary>
        /// The individual sentences that constitute the raw input from the user
        /// </summary>
        public readonly List<Unifiable> EnglishSentences = new List<Unifiable>();

        public int maxResults;

        private readonly Func<string, string> OutputSentencesToEnglish;
        public readonly List<Unifiable> SemanticSentences = new List<Unifiable>();

        public bool IsSpeakerInputGleaned = false;

        /// <summary>
        /// The raw input from the user
        /// </summary>
        private Unifiable English;

        //private Func<string, string> EnglishToNormalized;
        public Action OnGetParsed;
        public Unifiable OrignalRawText;

        public Utterance(Func<string, string> generatePhrase,UserConversationScope speaker, UserConversationScope toWhom, Unifiable rawText, int maxSentences)
        {
            Speaker = speaker;
            ToWhom = toWhom;
            OrignalRawText = rawText;
            OutputSentencesToEnglish = generatePhrase;
            maxResults = maxSentences + 10;
        }


        /// <summary>
        /// The normalized sentence(s) (paths) fed into the graphmaster
        /// </summary>
        public List<Unifiable> NormalizedPaths
        {
            get
            {
                lock (SemanticSentences)
                {
                    if (SemanticSentences.Count == 0)
                    {
                        if (OnGetParsed != null)
                        {
                            OnGetParsed();
                            OnGetParsed = null;
                        }
                    }
                    return SemanticSentences;
                }
            }
        }

        /// <summary>
        /// The user that made this Utterance
        /// </summary>
        public UserConversationScope Speaker { get; set; }
        /// <summary>
        /// The user responding to the request
        /// </summary>
        public UserConversationScope ToWhom { get; set; }

        /// <summary>
        /// The last meaning unit extracted from what the ToWhom said previous
        /// </summary>
        public Unifiable That
        {
            get { return InResponse.RawText; }
        }

        /// <summary>
        /// The last Utterance containing That
        /// </summary>
        public Utterance InResponse
        {
            get
            {
                if (_inResponse != null) return _inResponse;
                throw new NotImplementedException();
            }
            set { _inResponse = value; }
        }

        private Utterance _inResponse;
        /// <summary>
        /// Returns the raw sentences without any logging 
        /// </summary>
        public Unifiable RawText
        {
            get
            {
                if (English != null)
                {
                    return English;
                }
                if (OrignalRawText != null)
                {
                    return OrignalRawText;
                }
                if (EnglishSentences.Count == 0)
                {
                    Convert(SemanticSentences, EnglishSentences, OutputSentencesToEnglish);
                }
                var result = new StringBuilder();
                int gather = maxResults;
                foreach (Unifiable list in EnglishSentences)
                {
                    if (gather == 0) break;
                    string list0 = StaticXMLUtils.Trim(list);
                    if (list0.Length > 0)
                    {
                        result.AppendLine(list);
                    }
                    if (list0.Length > 5)
                    {
                        gather--;
                    }
                }
                string resultS = result.ToString();
                return resultS.TrimEnd();
            }
            set { English = value; }
        }

        /// <summary>
        /// TheMainSentence is the last question (if it contains one)
        ///  else the last sentence in collection
        /// </summary>
        public Unifiable TheMainSentence
        {
            get
            {
                if (true)
                    foreach (Unifiable output in SemanticSentences)
                    {
                        String sentenceIn = output;
                        String sentence = OutputSentencesToEnglish(sentenceIn);
                        sentence = MainSentence(sentence);
                        sentence = TextPatternUtils.SymTrim(sentence);
                        if (sentence.Length < 2) continue;
                        return sentence;
                    }
                return MainSentence(RawText);
            }
        }

        public Unifiable GetSentence(int sentenceNum, bool mustBeSailent)
        {
            if (sentenceNum == 0)
            {
                return RawText;
            }
            String sentenceIn = RawText;
            foreach (Unifiable output in SemanticSentences)
            {
                sentenceIn = output;
                String sentence = OutputSentencesToEnglish(sentenceIn);
                sentence = MainSentence(sentence);
                sentence = TextPatternUtils.SymTrim(sentence);
                if (sentence.Length < 2) continue;
                sentenceNum--;
                if (sentenceNum == 0) return sentence;
            }
            return sentenceIn;
        }

        public override string ToString()
        {
            return "M: " + TheMainSentence + " S: <" + TextPatternUtils.CollectionString(SemanticSentences) + "> E: <" +
                   TextPatternUtils.CollectionString(EnglishSentences) + ">";
        }

        public static string MainSentence(string sentence)
        {
            string prev = "";
            while (sentence != prev)
            {
                prev = sentence;
                sentence = StaticXMLUtils.Trim(sentence);
                int sl = sentence.Length - 1;

                if (sl < 0) return sentence;

                char c = sentence[sl];
                if (Char.IsPunctuation(c))
                {
                    sentence = sentence.Substring(0, sl);
                }
                sentence = sentence.TrimEnd();
            }
            int sf = sentence.LastIndexOfAny(new[] {'?'});

            if (sf > 0)
            {
                String newClip = sentence.Substring(0, sf - 1);
                // AltBot.writeDebugLine("AIMLTRACE !REWRITE THAT QUESTION " + sentence + " => " + newClip);
                if (newClip.Length > 4) sentence = newClip;
            }
            sentence = TextPatternUtils.SymTrim(sentence, '?');
            sf = sentence.LastIndexOfAny(new[] {'.', '!'});
            if (sf > 0)
            {
                String newClip = StaticXMLUtils.Trim(sentence.Substring(sf));
                while (Char.IsPunctuation(newClip[0]))
                {
                    newClip = newClip.Substring(1).TrimStart();
                }
                //   AltBot.writeDebugLine("AIMLTRACE !REWRITE THAT SENT " + sentence + " => " + newClip);
                if (newClip.Length > 4) sentence = newClip;
            }
            return sentence;
        }

        internal static void Convert(IEnumerable<Unifiable> fromList, ICollection<Unifiable> toList,
                                     Func<string, string> OutputSentencesToEnglish)
        {
            lock (fromList)
                foreach (string sentence in fromList)
                {
                    String sentenceForOutput = OutputSentencesToEnglish(sentence);
                    if (String.IsNullOrEmpty(sentenceForOutput)) continue;
                    toList.Add(sentenceForOutput);
                }
        }

        public static void NormalizedInputPaths(Request request, IEnumerable<Unifiable> rawSentences,
                                                ICollection<Unifiable> result, Func<string, string> ToInputSubsts)
        {
            if (request.Stage > SideEffectStage.PARSING_INPUT) return;

            //ParsedSentences result = request.UserInput;
            AltBot thiz = request.TargetBot;
            int maxInputs = request.MaxInputs;
            int numInputs = 0;
            int sentenceNum = 0;
            int topicNum = 0;
            int thatNum = 0;
            AIMLLoaderU loader = thiz.GetLoader(request);
            Func<Unifiable, bool, Unifiable> normalizerT =
                (inputText, isUserInput) => loader.NormalizeU(inputText, isUserInput).Trim();
            string lastInput = "";
            {
                foreach (Unifiable sentenceURaw in rawSentences)
                {
                    string sentenceRaw = sentenceURaw;
                    NatLangDb.bot = NatLangDb.bot ?? request.bot;
                    if (NatLangDb.WasQuestion(sentenceRaw))
                    {
                        AltBot.writeDebugLine("Question: " + sentenceRaw);
                    }
                    char[] toCharArray = " .,!:".ToCharArray();
                    string sentence = TextPatternUtils.SymTrim(sentenceRaw, toCharArray);
                    sentence = ToInputSubsts(sentence);
                    //result.InputSentences.Add(sentence);
                    sentence = sentence.Trim(toCharArray);
                    if (sentence.Length == 0)
                    {
                        AltBot.writeDebugLine("skipping input sentence " + sentenceRaw);
                        continue;
                    }
                    sentenceNum++;
                    topicNum = 0;
                    if (maxInputs == 1)
                    {
                        Unifiable requestThat = request.That;
                        if (TextPatternUtils.IsNullOrEmpty(requestThat))
                        {
                            requestThat = request.That;
                            //throw new NullReferenceException("set_That: " + request);
                        }
                        requestThat = Utterance.MainSentence(requestThat);
                        Unifiable path = loader.generatePath(sentence,
                                                             //thatNum + " " +
                                                             requestThat, request.Flags,
                                                             //topicNum + " " +
                                                             request.Requester.TopicSetting, true, normalizerT);
                        if (TextPatternUtils.IsNullOrEmpty(path))
                        {
                            path = loader.generatePath(sentence,
                                                       //thatNum + " " +
                                                       requestThat, request.Flags,
                                                       //topicNum + " " +
                                                       request.Requester.TopicSetting, false, normalizerT);
                        }
                        if (TextPatternUtils.IsNullOrEmpty(path)) continue;
                        numInputs++;
                        result.Add(path);
                        if (numInputs >= maxInputs) return;
                        continue;
                    }
                    foreach (Unifiable topic0 in request.Topics)
                    {
                        Unifiable topic = topic0;
                        topicNum++;
                        if (topic.IsCatchAll)
                        {
                            topic = thiz.NOTOPIC;
                        }
                        thatNum = 0;
                        foreach (Unifiable that in request.ResponderOutputs)
                        {
                            thatNum++;
                            string thats = that.AsString();
                            thats = Utterance.MainSentence(thats);
                            Unifiable path = loader.generatePath(sentence, //thatNum + " " +
                                                                 thats, request.Flags,
                                                                 //topicNum + " " +
                                                                 topic, true, normalizerT);
                            if (that.IsCatchAll)
                            {
                                if (thatNum > 1)
                                {
                                    continue;
                                }
                                if (topic.IsCatchAll)
                                {
                                    topic = "TOTOPIC";
                                }
                            }
                            string thisInput = path.LegacyPath.AsString().Trim().ToUpper();
                            if (thisInput == lastInput) continue;

                            lastInput = thisInput;
                            numInputs++;
                            result.Add(path);
                            if (numInputs >= maxInputs) return;
                        }
                    }
                }
            }
        }

        public static Utterance GetParsedSentences(Request request, bool isTraced, OutputDelegate writeToLog)
        {
            Utterance utterance = request.ChatInput;

            int NormalizedPathsCount = utterance.NormalizedPaths.Count;

            if (isTraced && NormalizedPathsCount != 1)
            {
                foreach (Unifiable path in utterance.NormalizedPaths)
                {
                    writeToLog("  i: " + path.LegacyPath);
                }
                writeToLog("NormalizedPaths.Count = " + NormalizedPathsCount);
            }
            request.Stage = SideEffectStage.PARSE_INPUT_COMPLETE;
            return utterance;
        }

        public static Utterance GetParsedUserInputSentences(Request request, Unifiable fromUInput)
        {
            Func<string, string> GenEnglish = (str) => request.TargetBot.EnsureEnglish(str);
            string fromInput = EnsureEnglishPassThru(fromUInput);
            // Normalize the input
            IEnumerable<Unifiable> rawSentences = SplitIntoSentences.Split(fromInput);
            var parsedSentences = new Utterance(GenEnglish, request.Requester, request.Responder, fromUInput, -1);
            List<Unifiable> userInputSentences = parsedSentences.EnglishSentences;
            userInputSentences.AddRange(rawSentences);
            Func<string, string> englishToNormaizedInput = arg => EngishToNormalizedInput(request, arg);
            // parsedSentences.EnglishToNormalized = englishToNormaizedInput;
            parsedSentences.OnGetParsed = () =>
                                              {
                                                  if (request.Stage < SideEffectStage.PARSING_INPUT)
                                                      request.Stage = SideEffectStage.PARSING_INPUT;
                                                  Convert(
                                                      parsedSentences.EnglishSentences,
                                                      parsedSentences.SemanticSentences, englishToNormaizedInput);
                                              };
            return parsedSentences;
        }

        private static string EngishToNormalizedInput(Request request, string startout)
        {
            var Normalized = new List<Unifiable>();
            Func<string, string> ToInputSubsts = request.TargetBot.ToInputSubsts;

            NormalizedInputPaths(request, new Unifiable[] {startout}, Normalized, ToInputSubsts);
            if (Normalized.Count == 0)
            {
                return null;
            }
            if (Normalized.Count == 1) return Normalized[0];
            return Normalized[0];
        }

        public static string EnsureEnglishPassThru(string arg)
        {
            return arg;
        }

        internal void ClearOutput()
        {
            EnglishSentences.Clear();
            if (NormalizedPaths.Count > 0)
            {
                NormalizedPaths.Clear();
            }
            if (SemanticSentences.Count > 0)
            {
                SemanticSentences.Clear();
            }
        }
    }
}