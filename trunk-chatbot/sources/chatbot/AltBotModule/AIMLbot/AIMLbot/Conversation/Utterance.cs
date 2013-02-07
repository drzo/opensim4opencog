using System;
using System.Collections.Generic;
using System.Linq;
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

        private readonly Func<Unifiable, Unifiable> OutputSentencesToEnglish;

        public bool IsSpeakerInputGleaned = false;

        /// <summary>
        /// The raw input from the user
        /// </summary>
        private Unifiable English;

        //private Func<string, string> EnglishToNormalized;
        public Action OnGetParsed;
        public Unifiable OrignalRawText;

        public Utterance(Func<Unifiable, Unifiable> generatePhrase, UserConversationScope speaker, UserConversationScope toWhom, Unifiable rawText, int maxSentences)
        {
            Speaker = speaker;
            ToWhom = toWhom;
            OrignalRawText = rawText;
            OutputSentencesToEnglish = generatePhrase;
            maxResults = maxSentences + 10;
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
                    foreach (var output in EnglishSentences)
                    {
                        String sentenceIn = output;
                        var sentence = OutputSentencesToEnglish(output);
                        sentence = MainSentence(sentence);
                        sentence = TextPatternUtils.SymTrim(sentence);
                        if (sentence.AsString().Length < 2) continue;
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
            foreach (Unifiable output in EnglishSentences)
            {
                sentenceIn = output;
                String sentence = OutputSentencesToEnglish(output);
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
            return "M: " + TheMainSentence + " E: <" + TextPatternUtils.CollectionString(EnglishSentences) + ">";
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

        internal static void ConvertFromTo<F,T>(IEnumerable<F> fromList, ICollection<T> toList,
                                     Func<F, T> OutputSentencesToEnglish)
        {
            var enumerable = fromList as F[] ?? fromList.ToArray();
            lock (enumerable)
                foreach (F sentence in enumerable)
                {
                    T sentenceForOutput = OutputSentencesToEnglish(sentence);
                    if (string.IsNullOrEmpty(sentenceForOutput.ToString())) continue;
                    toList.Add(sentenceForOutput);
                }
        }

        internal void ClearSentences()
        {
            EnglishSentences.Clear();
        }
    }
}