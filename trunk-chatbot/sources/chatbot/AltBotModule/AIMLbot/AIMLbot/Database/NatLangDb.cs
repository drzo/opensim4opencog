using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using AltAIMLParser;
using MushDLR223.Utilities;
using System.IO;
using System.Runtime.Serialization;
using System.Runtime.Serialization.Formatters.Binary;
using System.Collections;
using System.Text.RegularExpressions;
using MushDLR223.Virtualization;
using RTParser;
using RTParser.AIMLTagHandlers;
using RTParser.Database.NLP;

//using NLPlib = RTParser.Database.NatLangDb;
///uses System.Runtime.Serialization.Formatters.Soap.dll
/// 
namespace RTParser.Database
{
    public class NatLangDb
    {
        private GoogleTranslator googleTranslator = null;
        public static NatLangDb NatLangProc;
        static public AltBot bot;
        public NatLangDb(RTParser.AltBot p)
        {
            bot = p;
            bot.AddExcuteHandler("pos", (SystemExecHandler)NatLangTestHandler);
            googleTranslator = new GoogleTranslator(p);
            NatLangProc = this;
            initNatLangDb();
        }

        public static readonly string BeAUX = " IS WAS BE AM ARE WERE ";
        private static readonly string Interjections = " hi hello yeah ok ";
        readonly static string StopWords =
          " a been get least our them whether about before getting left ourselves then which after being go" +
          " less out there while again between goes let over these who ago but going like per they whoever all by gone make" +
          " put this whom almost came got many putting those whose also can gotten may" +
          " same through why always cannot had maybe saw till will am come has me see to with an could have mine seen too within and did having more shall" +
          " two without another do he most she unless won't wont any does her much should until would anybody doing here my so up wouldn't wouldnt anyhow" +
          " done him myself some upon yet anyone down his never somebody us you anything each how no someone very your anyway else i none something was are" +
          " even if not stand we as ever in now such went at every into of sure were away everyone is off take what back everything isn't isnt on than" +
          " whatever be for it one that what's whats became from just onto the when because front last or their where ";

        // 368 answers for ?ARG1 : (isa ?ARG1 TermPhrasesConstraint)

        static readonly string TermPhrasesConstraint =
            " Adjective Adjective-Ed Adjective-Gradable Adjective-Ing Adverb AdverbOfFrequency AdverbOfManner AdverbOfPlace AdverbOfTime AgentiveNoun Appositive AtomicParaphrase AttributiveOnlyAdjective Aux-Negated AuxVerb BeAux CharacterString CommonNoun Complementizer ConjunctAdverb Conjunction CoordinatingConjunction CopularVerbPhrase CountNoun CountNoun-Feminine CountNoun-Generic CountNoun-Masculine CountNoun-Neuter DeAdjectivalAdjective DeAdjectivalAdverb DeAdjectivalNoun DeAdjectivalVerb DeVerbalNoun DefiniteNounPhrase DenominalAdjective DenominalAdverb DenominalNoun DenominalVerb Determiner Determiner-Central Determiner-ClassA Determiner-ClassB Determiner-ClassC Determiner-Definite Determiner-Indefinite DeverbalAdjective DoAux DualPronoun ExpletivePronoun Feminine-NLAttr FemininePronoun FirstPerson-NLAttr Generic-NLAttr GerundiveCountNoun GerundiveNoun GerundiveNoun-NumberAgnostic HaveAux IndefiniteNounPhrase IndefinitePronoun InfinitiveComp Interjection-SpeechPart MainVerb Masculine-NLAttr MasculinePronoun Mass-NLAttr MassNoun MassNoun-Generic Modal Modal-Contracted Modal-Negated NLSentence NLWordForm Neuter-NLAttr NeuterPronoun NonIntersectiveAdjective NongradableAdjective Noun NounCompoundPhrase NounPhrase Number-SP ObjectPronoun OrdinalAdjective Plural-NLAttr PluralPronoun PossessiveMarker PossessiveMarker-Pl PossessiveMarker-Sg PossessivePhrase PossessivePronoun PossessivePronoun-Post PossessivePronoun-Pre PostQuant-SP Postdeterminer PredicativeOnlyAdjective Preposition Preposition-Directional Preposition-Directional-Atelic Preposition-Directional-Telic Preposition-Duration Preposition-Duration-Atelic Preposition-Duration-Telic Preposition-Locative Preposition-Of Preposition-Spatial Preposition-Temporal Preposition-TimePoint PrepositionalPhrase Pronoun Pronoun-SubjectOrObject ProperCountNoun ProperMassNoun ProperNameString ProperNoun Punctuation-SP Quantifier-SP QuantifyingIndexical RDFTriple ReciprocalPronoun ReflexivePronoun SecondPerson-NLAttr SententialConstituent Singular-NLAttr SingularPronoun StructuredParaphrase SubjectPronoun SubordinatingConjunction ThePrototypicalGenericSpeechPartPredicate ThePrototypicalNLPhraseType ThePrototypicalProperNamePredicate-Loose ThePrototypicalProperNamePredicate-Strict ThePrototypicalProperNamePredicateForNPParser ThePrototypicalSententialConstituentType ThePrototypicalSpeechPart ThePrototypicalSpeechPartPredicate ThePrototypicalStringIndexingSlot There-Existential ThirdPerson-NLAttr Ungendered-NLAttr UnmarkedNumber-NLAttr Verb Verb-Contracted VerbParticle VerbPhrase WHAdverb WHDeterminer WHPronoun WHPronoun-Object WHPronoun-Possessive WHPronoun-Subject WHWord abbreviationString abbreviationString-PN accessFilename accessFilenameOfAIS acronymString adjStrings adjStrings-NonGraded adverbStrings agentive-Mass agentive-Pl agentive-Sg agentiveNounStrings airportHasIATACode aisFileAbsolutePathname alias atomicSymbol bAAHasID businessName-WithTickerSymbol callForProposalsPostedAt chemicalCASRegistry chemicalFormulaString chemicalNATODesignation commonNounStrings comparativeAdverb comparativeDegree congressionalBiographyID countryCodeDigraph countryCodeTrigraph countryName-LocalLongForm countryName-LocalShortForm countryName-LongForm countryName-ShortForm currencyCode cwEntitled cycSecureLabelString definiteDescriptions determinerStrings epistleAddresseeText executableProgramName familyName familyNameWithFollowingInitials fileFormatHasSuffix firstName firstPersonSg-Generic firstPersonSg-Past firstPersonSg-Present firstPersonSg-PresentPerfect folderTitle formerName fullName futurePerfect-Generic futurePerfect-Universal futureTense-Generic geopoliticalEntityCodeDigraph gerund gerund-Generic gerund-Plural gerund-Singular givenNames goodStringsForTopic gospelName-Short hipHopMoniker infinitive initialismString initialsString internetCountryCode investmentSymbol ksTermString lastName massNumber massNumber-Generic middleName movieTitleString naicsCodeOfCompany naicsCodeOfIndustryType naicsTitle nameSpelling nameString nameString-Plural nameStringForUSGSTopoMap networkName nicknames nonGradableAdjectiveForm nonPlural-Generic nonSingular-Generic nonThirdSg-Present nounStrings ordinalAdjectiveForm organizationName-Official organizationName-Standard originalStringFromINFn passiveParticiple pastPerfect-Generic pastPerfect-Universal pastTense-Generic pastTense-Universal perfect perfectAspect-Generic perfective-Generic placeName-LocalLongForm placeName-LocalShortForm placeName-ShortForm placeName-Standard placeName-WithRegion placeName-WithRegionAbbreviation plural plural-Feminine plural-Generic plural-Masculine plural-Neuter pluralVerb-Generic pluralVerb-Past pluralVerb-Present pluralVerb-PresentPerfect pnMassNumber pnNonPlural-Generic pnPlural pnSingular possessiveMarkerStrings preferredNameString preferredTermStrings prenominalModifier prepositionStrings presentIndicative presentIndicative1stPlural presentIndicative1stSingular presentIndicative2ndPlural presentIndicative2ndSingular presentIndicative3rdPlural presentIndicative3rdSingular presentParticiple presentPerfect-Generic presentPerfect-Universal presentTense-Generic presentTense-Universal prettyString prettyString-Canonical programStrings programTypeStrings pronounStrings properNameStrings properNounStrings pseudonym rdfs:label referenceWorkEntryTitle regularAdverb regularDegree schoolSymbolName-Female schoolSymbolName-Male scientificName secondPersonSg-Generic secondPersonSg-Past secondPersonSg-Present secondPersonSg-PresentPerfect sectionTitle simpleAspect-Generic simpleFuture-Generic simpleNounStrings simplePast-Generic simplePresent-Generic singular singular-Feminine singular-Generic singular-Masculine singular-Neuter singularVerb-Generic skos:altLabel skos:prefLabel skosName subSectionIndexingStrings superlativeAdverb superlativeDegree tensed termStrings termStrings-Guessed termStrings-GuessedFromName termStringsViaWordNet thirdPersonSg-Generic thirdPersonSg-Past thirdPersonSg-Present thirdPersonSg-PresentPerfect titleOfWork tribalName uninflectedWordStrings untensed urlStringOfCW verbFormWithAuxiliary verbStrings webSearchableStrings wordStrings ".ToLower();

        // 118 answers for ?ARG1 : (isa ?ARG1 SpeechPart)
        static readonly string SpeechPart =
            " Adjective Adjective-Ed Adjective-Gradable Adjective-Ing Adverb AdverbOfFrequency AdverbOfManner AdverbOfPlace AdverbOfTime AgentiveNoun AttributiveOnlyAdjective Aux-Negated AuxVerb BeAux CommonNoun Complementizer ConjunctAdverb Conjunction CoordinatingConjunction CountNoun CountNoun-Feminine CountNoun-Generic CountNoun-Masculine CountNoun-Neuter DeAdjectivalAdjective DeAdjectivalAdverb DeAdjectivalNoun DeAdjectivalVerb DeVerbalNoun DenominalAdjective DenominalAdverb DenominalNoun DenominalVerb Determiner Determiner-Central Determiner-ClassA Determiner-ClassB Determiner-ClassC Determiner-Definite Determiner-Indefinite DeverbalAdjective DoAux DualPronoun ExpletivePronoun FemininePronoun GerundiveCountNoun GerundiveNoun GerundiveNoun-NumberAgnostic HaveAux IndefinitePronoun InfinitiveComp Interjection-SpeechPart MainVerb MasculinePronoun MassNoun MassNoun-Generic Modal Modal-Contracted Modal-Negated NLWordForm NeuterPronoun NonIntersectiveAdjective NongradableAdjective Noun Number-SP ObjectPronoun OrdinalAdjective PluralPronoun PossessiveMarker PossessiveMarker-Pl PossessiveMarker-Sg PossessivePronoun PossessivePronoun-Post PossessivePronoun-Pre PostQuant-SP Postdeterminer PredicativeOnlyAdjective Preposition Preposition-Directional Preposition-Directional-Atelic Preposition-Directional-Telic Preposition-Duration Preposition-Duration-Atelic Preposition-Duration-Telic Preposition-Locative Preposition-Of Preposition-Spatial Preposition-Temporal Preposition-TimePoint Pronoun Pronoun-SubjectOrObject ProperCountNoun ProperMassNoun ProperNoun Punctuation-SP Quantifier-SP QuantifyingIndexical ReciprocalPronoun ReflexivePronoun SingularPronoun SubjectPronoun SubordinatingConjunction ThePrototypicalSpeechPart There-Existential Verb Verb-Contracted VerbParticle WHAdverb WHDeterminer WHPronoun WHPronoun-Object WHPronoun-Possessive WHPronoun-Subject WHWord ".ToLower();

        static readonly string SpeechPartPredicate =
            " ThePrototypicalGenericSpeechPartPredicate ThePrototypicalSpeechPartPredicate adjStrings adjStrings-NonGraded adverbStrings agentive-Mass agentive-Pl agentive-Sg agentiveNounStrings commonNounStrings comparativeAdverb comparativeDegree determinerStrings firstPersonSg-Generic firstPersonSg-Past firstPersonSg-Present firstPersonSg-PresentPerfect futurePerfect-Generic futurePerfect-Universal futureTense-Generic gerund gerund-Generic gerund-Plural gerund-Singular infinitive massNumber massNumber-Generic nameSpelling nonGradableAdjectiveForm nonPlural-Generic nonSingular-Generic nonThirdSg-Present nounStrings ordinalAdjectiveForm passiveParticiple pastPerfect-Generic pastPerfect-Universal pastTense-Generic pastTense-Universal perfect perfectAspect-Generic perfective-Generic plural plural-Feminine plural-Generic plural-Masculine plural-Neuter pluralVerb-Generic pluralVerb-Past pluralVerb-Present pluralVerb-PresentPerfect pnMassNumber pnNonPlural-Generic pnPlural pnSingular possessiveMarkerStrings prenominalModifier prepositionStrings presentIndicative presentIndicative1stPlural presentIndicative1stSingular presentIndicative2ndPlural presentIndicative2ndSingular presentIndicative3rdPlural presentIndicative3rdSingular presentParticiple presentPerfect-Generic presentPerfect-Universal presentTense-Generic presentTense-Universal pronounStrings properNounStrings regularAdverb regularDegree secondPersonSg-Generic secondPersonSg-Past secondPersonSg-Present secondPersonSg-PresentPerfect simpleAspect-Generic simpleFuture-Generic simpleNounStrings simplePast-Generic simplePresent-Generic singular singular-Feminine singular-Generic singular-Masculine singular-Neuter singularVerb-Generic superlativeAdverb superlativeDegree tensed thirdPersonSg-Generic thirdPersonSg-Past thirdPersonSg-Present thirdPersonSg-PresentPerfect uninflectedWordStrings untensed verbFormWithAuxiliary verbStrings wordStrings ".ToLower();

        public static bool IsStopWord(string s)
        {
            return StopWords.Contains(" " + s.ToLower() + " ");
        }
        public static bool IsSpeechPartPredicate(string s)
        {
            return SpeechPartPredicate.Contains(" " + s.ToLower() + " ");
        }
        public static bool IsSpeechPart(string s)
        {
            return SpeechPart.Contains(" " + s.ToLower() + " ");
        }
        public static bool IsTermPhrasesConstraint(string s)
        {
            return TermPhrasesConstraint.Contains(" " + s.ToLower() + " ");
        }


        static readonly Dictionary<string, string> StringCachePOSWORD = new Dictionary<string, string>();
        public static bool IsWordClass(string s, string wclass)
        {
            wclass = " " + wclass.ToLower() + " ";
            return IsWordClassCont(s, wclass);
        }

        public static bool IsWordClassCont(string s, string wclass)
        {
            s = s.ToLower();
            string dict;
            lock (StringCachePOSWORD)
            {
                if (!StringCachePOSWORD.TryGetValue(s, out dict))
                {
                    dict = StringCachePOSWORD[s] = " " + GetWordInfo(s) + " ";
                    DLRConsole.DebugWriteLine("" + s + " => " + dict);
                }
            }
            var b = dict.Contains(wclass);
            // ReSharper disable ConditionIsAlwaysTrueOrFalse
            if (b)
            {
                return b;
            }
            return b;
            // ReSharper restore ConditionIsAlwaysTrueOrFalse
        }

        private static string GetWordInfo(string s)
        {
            CycDatabase TheCyc = CycDatabase.TheStaticCyc;
            s = ToLowerAnsii(s);
            Unifiable rs;
            if (IsStopWord(s))
                rs =
                    TheCyc.EvalSubL("(cconcatenate (pos-of-string \"" + s + "\")(words-of-string \"" + s + "\"))",
                                        null);
            else
                rs =
                    TheCyc.EvalSubL(
                        "(cconcatenate (pos-of-string \"" + s + "\")(words-of-string \"" + s +
                        "\")(all-parsing-denots-of-string \"" + s + "\"))", null);

            string ss = rs.AsString();
            if (ss.Contains("Pronoun"))
            {
                ss = ss.Replace("Det", "");
            }
            if (ss.Contains("#$Determ") || Interjections.Contains(" " + s + " "))
            {
                ss = ss.Replace("Noun", "");
            }
            ss = ss.ToLower().Replace('(', ' ').Replace(')', ' ').Replace('$', ' ');
            return ss;
        }

        private static string ToLowerAnsii(string s)
        {
            s = s.ToLowerInvariant();
            String s1 = "";
            foreach (char c in s)
            {
                char c0 = c;

                while (c0 > 128)
                {
                    c0 = (char)(c0 - 32);
                }
                s1 += c0;
            }

            return s1;
        }

        const string LEXICON = "LEXICON";
        private static Hashtable lexHash = null;
        private static BrillTagger brillTagger;

        static void initNatLangDb()
        {

            if (lexHash != null)
            {
                if (lexHash.Contains("OK"))
                {
                    var typef = lexHash["OK"];
                }
                return; // singleton pattern           
            }

            try
            {
                StringCachePOSWORD["hello"] =
                    StringCachePOSWORD["hi"] = " # Interjection-SpeechPart # Hi-TheWord # Hello-TheWord ";
                lexHash = new Hashtable();

                string outfile = MakeLex(LEXICON);
                Stream file = HostSystem.Open(outfile, FileMode.Open);
                try
                {
                    var formatter = (IFormatter)new BinaryFormatter();
                    lexHash = formatter.Deserialize(file) as Hashtable;
                }
                finally
                {
                    file.Close();
                }
                DLRConsole.DebugWriteLine("Initialized lexHash from serialized data.");
            }
            catch (Exception exception)
            {
                DLRConsole.DebugWriteLine("ERROR: " + exception);
            }
            NLPLibTest();
        }

        private static object NatLangTestHandler(string cmd, Request requestOrNull)
        {

            var toks = Tokenize(cmd);
            var pos1 = POSTag(toks);
            var pos2 = GetWordInfos(toks);
            string collect = "";
            for (int i = 0; i < toks.Count; i++)
            {
                string tw = (string)toks[i];
                string p1 = (string)pos1[i];
                string p2 = (string)pos2[i];
                string o = tw + " " + p1 + " " + p2;
                collect += o + "\n";
            }
            var tout = BrillTagger.CeateBrillTags(cmd, true, true, true);
            return collect +"\n" + tout.ToString();
        }

        private static ArrayList GetWordInfos(ArrayList toks)
        {
            ArrayList v = new ArrayList();
            foreach(string t in toks)
            {
                v.Add(GetWordInfo(t));
            }
            return v;
        }

        static public ArrayList Tokenize(string s)
        {
            initNatLangDb();

            ArrayList v = new ArrayList();
            foreach (var tok in MBrain.Tokenize(s, true))
            {
                if (tok == null)
                {
                    string vtok = tok ?? "";
                    continue;
                }
                v.Add(tok);
            }
            return v;

            Regex reg = new Regex(@"(\S+)\s");
            MatchCollection m = reg.Matches(s);
            foreach (Match m2 in m)
            {
                if (m2.Length != 0)
                {
                    string z = m2.ToString().Trim();
                    if (z.EndsWith(";") || z.EndsWith(",") ||
                        z.EndsWith("?") || z.EndsWith(")") ||
                        z.EndsWith(":") || z.EndsWith("."))
                    {
                        z = z.Substring(0, z.Length - 1);
                    }
                    v.Add(z);
                }
            }
            return v;
        }



        public static ArrayList POSTag(ArrayList words)
        {
            initNatLangDb();
            ArrayList ret = new ArrayList();
            for (int i = 0, size = words.Count; i < size; i++)
            {
                ret.Add("NN");  // default
                string s = (string)lexHash[words[i]];
                // 1/22/2002 mod (from Lisp code): if not in hash, try lower case:
                if (s == null)
                    s = (string)lexHash[((string)words[i]).ToLower()];
                if (s == null)
                    s = (string)lexHash[((string)words[i]).ToUpper()];
                if (s != null)
                {
                    int index = s.IndexOf(" ");
                    if (index > -1) ret[i] = s.Substring(0, index).Trim();
                    else ret[i] = s;
                }
            }
            /**
             * Apply transformational rules
             **/
            for (int i = 0; i < words.Count; i++)
            {
                //  rule 1: DT, {VBD | VBP} --> DT, NN
                if (i > 0 && ret[i - 1].Equals("DT"))
                {
                    if (ret[i].Equals("VBD")
                        || ret[i].Equals("VBP")
                        || ret[i].Equals("VB"))
                    {
                        ret[i] = "NN";
                    }
                }
                // rule 2: convert a noun to a number (CD) if "." appears in the word
                if (((string)ret[i]).StartsWith("N"))
                {
                    if (((string)words[i]).IndexOf(".") > -1)
                        ret[i] = "CD";
                }
                // rule 3: convert a noun to a past participle if ((string)words[i]) ends with "ed"
                if (((string)ret[i]).StartsWith("N") && ((string)words[i]).EndsWith("ed"))
                    ret[i] = "VBN";
                // rule 4: convert any type to adverb if it ends in "ly";
                if (((string)words[i]).EndsWith("ly"))
                    ret[i] = "RB";
                // rule 5: convert a common noun (NN or NNS) to a adjective if it ends with "al"
                if (((string)ret[i]).StartsWith("NN") && ((string)words[i]).EndsWith("al"))
                    ret[i] = "JJ";
                // rule 6: convert a noun to a verb if the preceeding work is "would"
                if (i > 0
                    && ((string)ret[i]).StartsWith("NN")
                    && ((string)words[i - 1]).ToLower().Equals("would"))
                    ret[i] = "VB";
                // rule 7: if a word has been categorized as a common noun and it ends with "s",
                //         then set its type to plural common noun (NNS)
                if (((string)ret[i]).Equals("NN") && ((string)words[i]).EndsWith("s"))
                    ret[i] = "NNS";
                // rule 8: convert a common noun to a present prticiple verb (i.e., a gerand)
                if (((string)ret[i]).StartsWith("NN") && ((string)words[i]).EndsWith("ing"))
                    ret[i] = "VBG";
            }
            return ret;
        }

        public static void NLPLibTest()
        {
           // NatLangDb tagger = new NatLangDb(bot);
            string s = "The dog's paw was bit. We blame the cat; is that fair? ";
            ArrayList v = Tokenize(s);
            ArrayList t = POSTag(v);
            for (int i = 0; i < v.Count; i++)
            {
                DLRConsole.DebugWriteLine((string)v[i] + "/" + (string)t[i]);
            }
        }

        public static string MakeLex(string lexfileStem)
        {
            string infile = lexfileStem + ".brill";
            string outfile = lexfileStem + ".dat";
            if (!HostSystem.FileExists(outfile))
            {
                MakeLex(infile, outfile);
            }
            return outfile;
        }

        public static void MakeLex(string infile, string outfile)
        {
            try
            {
                Hashtable hash = new Hashtable();
                //int count = 0;
                StreamReader reader = HostSystem.GetStreamReader(infile);
                string line;
                do
                {
                    line = reader.ReadLine();
                    if (line == null) break;
                    int index = line.IndexOf(" ");
                    //Console.WriteLine("line: " + line + " index: " + index);
                    string word = line.Substring(0, index).Trim();
                    string tags = line.Substring(index).Trim();
                    //Console.WriteLine("word: " + word + ", tags: " + tags);
                    //count++;
                    if (hash[word] == null) hash.Add(word, tags);
                    // ReSharper disable ConditionIsAlwaysTrueOrFalse
                } while (line != null);
                // ReSharper restore ConditionIsAlwaysTrueOrFalse
                reader.Close();
                Stream file = HostSystem.Open(outfile, FileMode.Create);
                IFormatter formatter = (IFormatter)new BinaryFormatter();
                // Serialize the object hashto stream
                formatter.Serialize(file, hash);
                file.Close();
            }
            catch (Exception e2)
            {
                DLRConsole.DebugWriteLine("Error: " + e2);
            }
        }

        public static bool WasQuestion(string message)
        {
            if (message.Contains("?")) return true;
            string lower = message.ToLower();
            ArrayList tokenize = NatLangDb.Tokenize(message);
            if (tokenize == null || tokenize.Count == 0) return false;
            var brillPOS = NatLangDb.POSTag(tokenize);
            if (brillPOS[0].ToString() == "VBP")
            {
                return true;
            }
            if (lower.StartsWith("wh")) return true;
            if (lower.StartsWith("ar")) return true;
            if (lower.StartsWith("wa")) return true;
            if (lower.StartsWith("wi")) return true;

            return false;
        }

        internal static Unifiable MakePossesive(string p)
        {
            if (p.Contains("'")) return p;
            if (p.EndsWith("s")) return p + "'";
            return p + "'s";
        }
    }
}
