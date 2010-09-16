using System;
using System.Collections.Generic;
using System.Text;
using AIMLbot.Utils;

namespace RTParser
{
    public class ParsedSentences
    {
        private readonly Func<string, string> OutputSentencesToEnglish;
        public Func<string, string> EnglishToNormaizedInput;
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
        readonly public List<Unifiable> InputSentences = new List<Unifiable>();

        /// <summary>
        /// The normalized sentence(s) (paths) fed into the graphmaster
        /// </summary>
        public List<Unifiable> NormalizedPaths
        {
            get
            {
                lock (TemplateOutputs)
                {
                    if (TemplateOutputs.Count == 0)
                    {
                        if (OnGetParsed != null)
                        {
                            OnGetParsed();
                            OnGetParsed = null;
                        }
                    }
                    return TemplateOutputs;
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
                if (InputSentences.Count == 0)
                {
                    Convert(TemplateOutputs, InputSentences, OutputSentencesToEnglish);
                }
                var result = new StringBuilder();
                int gather = maxResults;
                foreach (var list in InputSentences)
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

        public readonly List<Unifiable> TemplateOutputs = new List<Unifiable>();

        public Action OnGetParsed;
        public string TheMainSentence
        {
            get
            {
                if (false) foreach (var output in TemplateOutputs)
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

        public int Count
        {
            get { return InputSentences.Count; }
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
                if (char.IsPunctuation(c))
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
                while (char.IsPunctuation(newClip[0]))
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
                    if (string.IsNullOrEmpty(sentenceForOutput)) continue;
                    toList.Add(sentenceForOutput);
                }
        }

        public static void NormalizedInputPaths(Request request, IEnumerable<Unifiable> rawSentences, ICollection<Unifiable> result, Func<string, string> ToInputSubsts)
        {
            RTPBot.NormalizedInputPaths(request, rawSentences, result, ToInputSubsts);
        }
    }
}