using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using RTParser;

namespace RTParser.Database
{
    public class NatLangDb
    {
        public static NatLangDb NatLangProc;
        static RTPBot bot;
        public NatLangDb(RTParser.RTPBot p)
        {
            bot = p;
            NatLangProc = this;
        }
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
            s = s.ToLower();
            string dict;
            lock (StringCachePOSWORD)
            {
                if (!StringCachePOSWORD.TryGetValue(s, out dict))
                {
                    StringCachePOSWORD[s] = GetWordInfo(s);
                }
            }
            return dict.Contains(wclass);
        }

        private static string GetWordInfo(string s)
        {
            string ss;
            if (IsStopWord(s))
                ss =
                    bot.TheCyc.EvalSubL("(cconcatenate (pos-of-string \"" + s + "\")(words-of-string \"" + s + "\"))",
                                        null).AsString();
            else
                ss =
                    bot.TheCyc.EvalSubL(
                        "(cconcatenate (pos-of-string \"" + s + "\")(words-of-string \"" + s +
                        "\")(all-parsing-denotes-of-string \"" + s + "\"))", null).AsString();
            if (ss.Contains("Pronoun"))
            {
                ss = ss.Replace("Det", "");
            }
            if (ss.Contains("#$Determ"))
            {
                ss = ss.Replace("Noun", "");
            }
            ss = ss.ToLower().Replace('(', ' ').Replace(')', ' ').Replace('$', ' ');
            return ss;
        }
    }
}
