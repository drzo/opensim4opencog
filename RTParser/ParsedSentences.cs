using System;
using System.Collections.Generic;
using System.Text;
using AIMLbot.Utils;
using MushDLR223.ScriptEngines;
using RTParser.Database;
using RTParser.Normalize;
using RTParser.Utils;
using AIMLLoader=RTParser.Utils.AIMLLoader;

namespace RTParser
{
    public class ParsedSentences
    {
        private readonly Func<string, string> OutputSentencesToEnglish;
        private Func<string, string> EnglishToNormalized;
        private readonly int maxResults;

        public ParsedSentences(Func<string, string> generatePhrase, int maxSentences)
        {
            OutputSentencesToEnglish = generatePhrase;
            maxResults = maxSentences;
        }


        /// <summary>
        /// The raw input from the user
        /// </summary>
        private Unifiable English;

        /// <summary>
        /// The individual sentences that constitute the raw input from the user
        /// </summary>
        readonly public List<Unifiable> EnglishSentences = new List<Unifiable>();

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
        /// Returns the raw sentences without any logging 
        /// </summary>
        public Unifiable RawText
        {
            get
            {
                if (English != null) return English;
                if (EnglishSentences.Count == 0)
                {
                    Convert(SemanticSentences, EnglishSentences, OutputSentencesToEnglish);
                }
                var result = new StringBuilder();
                int gather = maxResults;
                foreach (var list in EnglishSentences)
                {
                    if (gather == 0) break;
                    string list0 = list.Trim();
                    if (list0.Length > 0)
                    {
                        result.AppendLine(list);
                        gather--;
                    }
                }
                return result.ToString();
            }
            set
            {
                English = value;
            }
        }

        public readonly List<Unifiable> SemanticSentences = new List<Unifiable>();

        public Action OnGetParsed;
        public string TheMainSentence
        {
            get
            {
                if (false) foreach (var output in SemanticSentences)
                    {
                        String sentenceIn = output;
                        String sentence = OutputSentencesToEnglish(sentenceIn);
                        sentence = MainSentence(RawText);
                        sentence = sentence.Trim(new char[] { '.', ' ', '!', '?' });
                        if (sentence.Length == 0) return null;
                        return sentence;

                    }
                return MainSentence(RawText);
            }
        }

        private int Count
        {
            get { return EnglishSentences.Count; }
        }

        static public string MainSentence(string sentence)
        {
            string prev = "";
            while (sentence != prev)
            {
                prev = sentence;
                sentence = sentence.Trim();
                int sl = sentence.Length - 1;

                if (sl < 0) return sentence;

                char c = sentence[sl];
                if (Char.IsPunctuation(c))
                {
                    sentence = sentence.Substring(0, sl);
                }
                sentence = sentence.TrimEnd();
            }
            int sf = sentence.LastIndexOfAny(new[] { '?' });

            if (sf > 0)
            {
                String newClip = sentence.Substring(0, sf - 1);
                // RTPBot.writeDebugLine("AIMLTRACE !REWRITE THAT QUESTION " + sentence + " => " + newClip);
                if (newClip.Length > 4) sentence = newClip;
            }
            sentence = sentence.Trim(new char[] { '.', ' ', '!', '?' });
            sf = sentence.LastIndexOfAny(new[] { '.', '!' });
            if (sf > 0)
            {
                String newClip = sentence.Substring(sf).Trim();
                while (Char.IsPunctuation(newClip[0]))
                {
                    newClip = newClip.Substring(1).TrimStart();
                }
                //   RTPBot.writeDebugLine("AIMLTRACE !REWRITE THAT SENT " + sentence + " => " + newClip);
                if (newClip.Length > 4) sentence = newClip;
            }
            return sentence;
        }

        internal static void Convert(IEnumerable<Unifiable> fromList, ICollection<Unifiable> toList, Func<string, string> OutputSentencesToEnglish)
        {
            lock (fromList)
                foreach (string sentence in fromList)
                {
                    String sentenceForOutput = OutputSentencesToEnglish(sentence);
                    if (String.IsNullOrEmpty(sentenceForOutput)) continue;
                    toList.Add(sentenceForOutput);
                }
        }

        public static void NormalizedInputPaths(Request request, IEnumerable<Unifiable> rawSentences, ICollection<Unifiable> result, Func<string, string> ToInputSubsts)
        {
            if (request.Stage > SideEffectStage.PARSING_INPUT) return;

            //ParsedSentences result = request.UserInput;
            RTPBot thiz = request.TargetBot;
            int maxInputs = request.MaxInputs;
            int numInputs = 0;
            int sentenceNum = 0;
            int topicNum = 0;
            int thatNum = 0;
            AIMLLoader loader = thiz.GetLoader(request);
            Func<Unifiable, bool, Unifiable> normalizerT =
                (inputText, isUserInput) => loader.Normalize(inputText, isUserInput).Trim();
            string lastInput = "";
            {
                foreach (Unifiable sentenceURaw in rawSentences)
                {
                    string sentenceRaw = sentenceURaw;
                    if (NatLangDb.WasQuestion(sentenceRaw))
                    {
                        RTPBot.writeDebugLine("Question: " + sentenceRaw);
                    }
                    string sentence = sentenceRaw.Trim(" .,!:".ToCharArray());
                    sentence = ToInputSubsts(sentence);
                    //result.InputSentences.Add(sentence);
                    sentence = sentence.Trim(" .,!:".ToCharArray());
                    if (sentence.Length == 0)
                    {
                        RTPBot.writeDebugLine("skipping input sentence " + sentenceRaw);
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

                        Unifiable path = loader.generatePath(sentence,
                                                             //thatNum + " " +
                                                             requestThat, request.Flags,
                                                             //topicNum + " " +
                                                             request.Requester.TopicSetting, true, normalizerT);
                        if (path.IsEmpty)
                        {
                            path = loader.generatePath(sentence,
                                                       //thatNum + " " +
                                                       requestThat, request.Flags,
                                                       //topicNum + " " +
                                                       request.Requester.TopicSetting, false, normalizerT);
                        }
                        if (path.IsEmpty) continue;
                        numInputs++;
                        result.Add(path);
                        if (numInputs >= maxInputs) return;
                        continue;
                    }
                    foreach (Unifiable topic0 in request.Topics)
                    {
                        Unifiable topic = topic0;
                        topicNum++;
                        if (topic.IsLongWildCard())
                        {
                            topic = thiz.NOTOPIC;
                        }
                        thatNum = 0;
                        foreach (Unifiable that in request.ResponderOutputs)
                        {
                            thatNum++;
                            string thats = that.AsString();
                            Unifiable path = loader.generatePath(sentence, //thatNum + " " +
                                                                 thats, request.Flags,
                                                                 //topicNum + " " +
                                                                 topic, true, normalizerT);
                            if (that.IsLongWildCard())
                            {
                                if (thatNum > 1)
                                {
                                    continue;
                                }
                                if (topic.IsLongWildCard())
                                {
                                    topic = "NOTHAT";
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

        public static ParsedSentences GetParsedSentences(Request request, bool isTraced, OutputDelegate writeToLog)
        {
            ParsedSentences parsedSentences = request.ChatInput;

            int NormalizedPathsCount = parsedSentences.NormalizedPaths.Count;

            if (isTraced && NormalizedPathsCount != 1)
            {
                foreach (Unifiable path in parsedSentences.NormalizedPaths)
                {
                    writeToLog("  i: " + path.LegacyPath);
                }
                writeToLog("NormalizedPaths.Count = " + NormalizedPathsCount);
            }
            request.Stage = SideEffectStage.PARSE_INPUT_COMPLETE;
            return parsedSentences;
        }

        static public ParsedSentences GetParsedUserInputSentences(Request request, Unifiable fromUInput)
        {

            Func<string, string> GenEnglish = (str) => request.TargetBot.EnsureEnglish(str);
            string fromInput = EnsureEnglishPassThru(fromUInput);
            // Normalize the input
            var rawSentences = SplitIntoSentences.Split(fromInput);
            var parsedSentences = new ParsedSentences(GenEnglish, -1);
            var userInputSentences = parsedSentences.EnglishSentences;
            userInputSentences.AddRange(rawSentences);
            Func<string, string> englishToNormaizedInput = arg => EngishToNormalizedInput(request, arg);
            // parsedSentences.EnglishToNormalized = englishToNormaizedInput;
            parsedSentences.OnGetParsed = () =>
            {
                if (request.Stage < SideEffectStage.PARSING_INPUT)
                    request.Stage = SideEffectStage.PARSING_INPUT;
                ParsedSentences.Convert(
                    parsedSentences.EnglishSentences,
                    parsedSentences.SemanticSentences, englishToNormaizedInput);
            };
            return parsedSentences;
        }

        static private string EngishToNormalizedInput(Request request, string startout)
        {
            var Normalized = new List<Unifiable>();
            Func<string, string> ToInputSubsts = request.TargetBot.ToInputSubsts;
                       
            NormalizedInputPaths(request, new Unifiable[] { startout }, Normalized, ToInputSubsts);
            if (Normalized.Count == 0)
            {
                return null;
            }
            if (Normalized.Count == 1) return Normalized[0];
            return Normalized[0];
        }

        private static char[] toCharArray = "@#$%^&*()_+<>,/{}[]\\\";'~~".ToCharArray();
        static public string EnsureEnglishPassThru(string arg)
        {
            return arg;
        }
    }
}