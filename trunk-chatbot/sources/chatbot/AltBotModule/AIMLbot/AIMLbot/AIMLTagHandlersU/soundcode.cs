using System;
using System.Collections;
using System.Diagnostics;
using System.Diagnostics.CodeAnalysis;
using System.IO;
using System.Text;
using System.Xml;
using AltAIMLbot.Utils;
using MushDLR223.Virtualization;

namespace AltAIMLbot.AIMLTagHandlers
{
    public class soundcode : UnifibleTagHandler
    {

        public soundcode(AltBot bot,
                         User user,
                         SubQuery query,
                         Request request,
                         Result result,
                         XmlNode templateNode)
            : base(bot, user, query, request, result, templateNode)
        {
        }

        public override float CanUnify(Unifiable with)
        {
            throw new NotImplementedException();
        }

        protected override Unifiable ComputeInnerOrNull()
        {
            if (templateNode.Name.ToLower() == "soundcode")
            {
                string line = "<SXQX> "+ templateNodeInnerText.ToValue(query).ToLower()+" <FNXS>";
                line = line.Replace(".", " ");
                line = line.Replace("!", " ");
                line = line.Replace("?", " ");
                line = line.Replace(",", " ");

                string[] lineSet = line.Split(' ');
                string soundLine = "";
                string lastLineWord = "<SXQX>";
                string prevLastLineWord = "";

                if (Proc.pHMM.hmmCorpusLoaded == 0)
                {
                    string file = Path.Combine("bgm", "corpus.txt");
                    //if (Directory.Exists(file))
                    if (HostSystem.FileExists(file))
                    {
                        AltBot.writeDebugLine("Load Corpus Bigrams: '{0}'", file);
                        StreamReader sr = new StreamReader(file);
                        Proc.pHMM.LearnBigramFile(sr);
                        sr.Close();
                        Proc.pHMM.hmmCorpusLoaded++;
                        AltBot.writeDebugLine("Loaded Corpus Bigrams: '{0}'", file);
                    }
                }

                // makes things simpler
                //string scode = DoubleMetaphoneStringExtension.GenerateDoubleMetaphone(lastLineWord);
                //addEmission(lastLineWord, scode, 0.9);
                double smoother = 1 / Proc.pHMM.transSum;
                foreach (string lineWord in lineSet)
                {
                    if (lineWord.Length > 0)
                    {
                        string scode = Proc.pHMM.getSoundCode(lineWord);
                        soundLine += scode + " ";
                        
                        // one emission for (lineWord->mcode)=0.9
                        Proc.pHMM.addEmission(lineWord, scode, 0.9);

                        // one transition from (lastLineWord -> lineWord)=0.9
                        Proc.pHMM.incrTransition(lastLineWord, lineWord, 10 * smoother);

                        // insert null transitions paths
                        // if we delete this word, would the results be a valid bigram ?
                        string first_observedSymbol = Proc.pHMM.getSoundCode(lastLineWord);
                        string next_observedSymbol = scode; // Proc.pHMM.getSoundCode(lineWord);

                        Hashtable first_emitter = ((Hashtable)Proc.pHMM.emissionProbability[first_observedSymbol]);
                        Hashtable next_emitter = ((Hashtable)Proc.pHMM.emissionProbability[next_observedSymbol]);
                        foreach (string nextStateKey in next_emitter.Keys)
                        {
                            foreach (string stateKey in first_emitter.Keys)
                            {

                                string skipAtomic = stateKey + " " + nextStateKey;
                                if ((Proc.pHMM.atomicTransitionCount.ContainsKey(skipAtomic))&&(Proc.pHMM.transitionCounts.ContainsKey(prevLastLineWord)))
                                {
                                    double skipTransP = (double)Proc.pHMM.atomicTransitionCount[skipAtomic] / (double)Proc.pHMM.transitionCounts[prevLastLineWord];
                                    // fixed at a discount
                                    Proc.pHMM.addTransition(lastLineWord, "<nullx>", skipTransP * 0.2);
                                    Proc.pHMM.addTransition("<nullx>", lineWord, skipTransP * 0.2);
                                }
                                prevLastLineWord = lastLineWord;
                                lastLineWord = lineWord;
                            }
                        }
                    }
                }
                string guess = "";
                try
                {
                    guess = Proc.pHMM.guessFromSounds(soundLine, line);
                }
                catch (Exception ex)
                {
                    StackTrace  st = new StackTrace (ex, true);

                    StackFrame sf = st.GetFrame(0);

                    AltBot.writeDebugLine("Method: "+sf.GetMethod().ToString());
                    AltBot.writeDebugLine("Line: " + sf.GetFileLineNumber().ToString());
                    AltBot.writeDebugLine("Column: " + sf.GetFileColumnNumber().ToString());
                    AltBot.writeDebugLine("Exception : {0}", ex.StackTrace);

                }

                guess = guess.Replace("<SXQX>", "");
                guess = guess.Replace("<FNXS>", "");
                guess = guess.Replace("<nullx>", "");
                //Unifiable result = soundLine +" "+guess;
                Unifiable result = guess;

                // AltBot.writeDebugLine(line);
                return result;
            }
            return Unifiable.Empty;
        }



 
    }


/*
Copyright (c) 2008 Anthony Tong Lee

Permission is hereby granted, free of charge, to any person
obtaining a copy of this software and associated documentation
files (the "Software"), to deal in the Software without
restriction, including without limitation the rights to use,
copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the
Software is furnished to do so, subject to the following
conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
OTHER DEALINGS IN THE SOFTWARE.
*/
//using System;
//using System.Diagnostics.CodeAnalysis;
//using System.Text;

// Original Source
// http://code.google.com/p/doublemetaphone/
// http://doublemetaphone.googlecode.com/svn/tags/1/DoubleMetaphone.cs

//namespace DoubleMetaphone
//{
    /// <summary>
    /// DoubleMetaphone string extension
    /// </summary>
    /// <remarks>
    /// Original C++ implementation:
    ///		"Double Metaphone (c) 1998, 1999 by Lawrence Philips"
    ///		http://www.ddj.com/cpp/184401251?pgno=1
    /// </remarks>
    [SuppressMessage("Microsoft.Naming", "CA1704:IdentifiersShouldBeSpelledCorrectly", MessageId = "Metaphone")]

    public static class DoubleMetaphoneStringExtension
    {
        [SuppressMessage("Microsoft.Naming", "CA1704:IdentifiersShouldBeSpelledCorrectly", MessageId = "Metaphone")]
        [SuppressMessage("Microsoft.Maintainability", "CA1505:AvoidUnmaintainableCode")]
        [SuppressMessage("Microsoft.Maintainability", "CA1502:AvoidExcessiveComplexity")]
        //public static string GenerateDoubleMetaphone(this string self)
        public static string GenerateDoubleMetaphone(string self)
        {

            MetaphoneData metaphoneData = new MetaphoneData();
            int current = 0;

            if (self.Length < 1) {
                return self;
            }
            int last = self.Length - 1; //zero based index

            string workingString = self.ToUpperInvariant() + "     ";
            string prefix = "";
            if (workingString[0] != '<') prefix = workingString[0].ToString();

            bool isSlavoGermanic = (self.IndexOf('W') > -1) || (self.IndexOf('K') > -1) || (self.IndexOf("CZ", StringComparison.OrdinalIgnoreCase) > -1)
                                   || (self.IndexOf("WITZ", StringComparison.OrdinalIgnoreCase) > -1);

            //skip these when at start of word
            if (workingString.StartsWith("GN",StringComparison.OrdinalIgnoreCase)||
                workingString.StartsWith("KN",StringComparison.OrdinalIgnoreCase)||
                workingString.StartsWith("PN",StringComparison.OrdinalIgnoreCase)||
                workingString.StartsWith("WR",StringComparison.OrdinalIgnoreCase)||
                workingString.StartsWith("PS",StringComparison.OrdinalIgnoreCase))
            {
                current += 1;
            }

            //Initial 'X' is pronounced 'Z' e.g. 'Xavier'
            if (workingString[0] == 'X') {
                metaphoneData.Add("S"); //'Z' maps to 'S'
                current += 1;
            }

            while ((metaphoneData.PrimaryLength < 4) || (metaphoneData.SecondaryLength < 4)) {
                if (current >= self.Length) {
                    break;
                }

                switch (workingString[current]) {
                    case 'A':
                    case 'E':
                    case 'I':
                    case 'O':
                    case 'U':
                    case 'Y':
                        if (current == 0) {
                            //all init vowels now map to 'A'
                            metaphoneData.Add("A");
                        }
                        current += 1;
                        break;

                    case 'B':
                        //"-mb", e.g", "dumb", already skipped over...
                        metaphoneData.Add("P");

                        if (workingString[current + 1] == 'B') {
                            current += 2;
                        }
                        else {
                            current += 1;
                        }
                        break;

                    case 'Ã':
                        metaphoneData.Add("S");
                        current += 1;
                        break;

                    case 'C':
                        //various germanic
                        if ((current > 1) && !IsVowel(workingString[current - 2]) && StringAt(workingString, (current - 1), "ACH")
                            && ((workingString[current + 2] != 'I') && ((workingString[current + 2] != 'E') || StringAt(workingString, (current - 2), "BACHER", "MACHER")))) {
                                metaphoneData.Add("K");
                                current += 2;
                                break;
                            }

                        //special case 'caesar'
                        if ((current == 0) && StringAt(workingString, current, "CAESAR")) {
                            metaphoneData.Add("S");
                            current += 2;
                            break;
                        }

                        //italian 'chianti'
                        if (StringAt(workingString, current, "CHIA")) {
                            metaphoneData.Add("K");
                            current += 2;
                            break;
                        }

                        if (StringAt(workingString, current, "CH")) {
                            //find 'michael'
                            if ((current > 0) && StringAt(workingString, current, "CHAE")) {
                                metaphoneData.Add("K", "X");
                                current += 2;
                                break;
                            }

                            //greek roots e.g. 'chemistry', 'chorus'
                            if ((current == 0) && (StringAt(workingString, (current + 1), "HARAC", "HARIS") || StringAt(workingString, (current + 1), "HOR", "HYM", "HIA", "HEM"))
                                && !StringAt(workingString, 0, "CHORE")) {
                                    metaphoneData.Add("K");
                                    current += 2;
                                    break;
                                }

                            //germanic, greek, or otherwise 'ch' for 'kh' sound
                            if ((StringAt(workingString, 0, "VAN ", "VON ") || StringAt(workingString, 0, "SCH")) // 'architect but not 'arch', 'orchestra', 'orchid'
                                || StringAt(workingString, (current - 2), "ORCHES", "ARCHIT", "ORCHID") || StringAt(workingString, (current + 2), "T", "S")
                                || ((StringAt(workingString, (current - 1), "A", "O", "U", "E") || (current == 0)) //e.g., 'wachtler', 'wechsler', but not 'tichner'
                                    && StringAt(workingString, (current + 2), "L", "R", "N", "M", "B", "H", "F", "V", "W", " "))) {
                                        metaphoneData.Add("K");
                                    }
                            else {
                                if (current > 0) {
                                    if (StringAt(workingString, 0, "MC")) {
                                        //e.g., "McHugh"
                                        metaphoneData.Add("K");
                                    }
                                    else {
                                        metaphoneData.Add("X", "K");
                                    }
                                }
                                else {
                                    metaphoneData.Add("X");
                                }
                            }
                            current += 2;
                            break;
                        }
                        //e.g, 'czerny'
                        if (StringAt(workingString, current, "CZ") && !StringAt(workingString, (current - 2), "WICZ")) {
                            metaphoneData.Add("S", "X");
                            current += 2;
                            break;
                        }

                        //e.g., 'focaccia'
                        if (StringAt(workingString, (current + 1), "CIA")) {
                            metaphoneData.Add("X");
                            current += 3;
                            break;
                        }

                        //double 'C', but not if e.g. 'McClellan'
                        if (StringAt(workingString, current, "CC") && !((current == 1) && (workingString[0] == 'M'))) {
                            //'bellocchio' but not 'bacchus'
                            if (StringAt(workingString, (current + 2), "I", "E", "H") && !StringAt(workingString, (current + 2), "HU")) {
                                //'accident', 'accede' 'succeed'
                                if (((current == 1) && (workingString[current - 1] == 'A')) || StringAt(workingString, (current - 1), "UCCEE", "UCCES")) {
                                    metaphoneData.Add("KS");
                                }
                                    //'bacci', 'bertucci', other italian
                                else {
                                    metaphoneData.Add("X");
                                }
                                current += 3;
                                break;
                            }
                            else {
//Pierce's rule
                                metaphoneData.Add("K");
                                current += 2;
                                break;
                            }
                        }

                        if (StringAt(workingString, current, "CK", "CG", "CQ")) {
                            metaphoneData.Add("K");
                            current += 2;
                            break;
                        }

                        if (StringAt(workingString, current, "CI", "CE", "CY")) {
                            //italian vs. english
                            if (StringAt(workingString, current, "CIO", "CIE", "CIA")) {
                                metaphoneData.Add("S", "X");
                            }
                            else {
                                metaphoneData.Add("S");
                            }
                            current += 2;
                            break;
                        }

                        //else
                        metaphoneData.Add("K");

                        //name sent in 'mac caffrey', 'mac gregor
                        if (StringAt(workingString, (current + 1), " C", " Q", " G")) {
                            current += 3;
                        }
                        else if (StringAt(workingString, (current + 1), "C", "K", "Q") && !StringAt(workingString, (current + 1), "CE", "CI")) {
                            current += 2;
                        }
                        else {
                            current += 1;
                        }
                        break;

                    case 'D':
                        if (StringAt(workingString, current, "DG")) {
                            if (StringAt(workingString, (current + 2), "I", "E", "Y")) {
                                //e.g. 'edge'
                                metaphoneData.Add("J");
                                current += 3;
                                break;
                            }
                            else {
                                //e.g. 'edgar'
                                metaphoneData.Add("TK");
                                current += 2;
                                break;
                            }
                        }

                        if (StringAt(workingString, current, "DT", "DD")) {
                            metaphoneData.Add("T");
                            current += 2;
                            break;
                        }

                        //else
                        metaphoneData.Add("T");
                        current += 1;
                        break;

                    case 'F':
                        if (workingString[current + 1] == 'F') {
                            current += 2;
                        }
                        else {
                            current += 1;
                        }
                        metaphoneData.Add("F");
                        break;

                    case 'G':
                        if (workingString[current + 1] == 'H') {
                            if ((current > 0) && !IsVowel(workingString[current - 1])) {
                                metaphoneData.Add("K");
                                current += 2;
                                break;
                            }

                            if (current < 3) {
                                //'ghislane', ghiradelli
                                if (current == 0) {
                                    if (workingString[current + 2] == 'I') {
                                        metaphoneData.Add("J");
                                    }
                                    else {
                                        metaphoneData.Add("K");
                                    }
                                    current += 2;
                                    break;
                                }
                            }
                            //Parker's rule (with some further refinements) - e.g., 'hugh'
                            if (((current > 1) && StringAt(workingString, (current - 2), "B", "H", "D")) //e.g., 'bough'
                                || ((current > 2) && StringAt(workingString, (current - 3), "B", "H", "D")) //e.g., 'broughton'
                                || ((current > 3) && StringAt(workingString, (current - 4), "B", "H"))) {
                                    current += 2;
                                    break;
                                }
                            else {
                                //e.g., 'laugh', 'McLaughlin', 'cough', 'gough', 'rough', 'tough'
                                if ((current > 2) && (workingString[current - 1] == 'U') && StringAt(workingString, (current - 3), "C", "G", "L", "R", "T")) {
                                    metaphoneData.Add("F");
                                }
                                else if ((current > 0) && workingString[current - 1] != 'I') {
                                    metaphoneData.Add("K");
                                }

                                current += 2;
                                break;
                            }
                        }

                        if (workingString[current + 1] == 'N') {
                            if ((current == 1) && IsVowel(workingString[0]) && !isSlavoGermanic) {
                                metaphoneData.Add("KN", "N");
                            }
                            else
                                //not e.g. 'cagney'
                                if (!StringAt(workingString, (current + 2), "EY") && (workingString[current + 1] != 'Y') && !isSlavoGermanic) {
                                    metaphoneData.Add("N", "KN");
                                }
                                else {
                                    metaphoneData.Add("KN");
                                }
                            current += 2;
                            break;
                        }

                        //'tagliaro'
                        if (StringAt(workingString, (current + 1), "LI") && !isSlavoGermanic) {
                            metaphoneData.Add("KL", "L");
                            current += 2;
                            break;
                        }

                        //-ges-,-gep-,-gel-, -gie- at beginning
                        if ((current == 0)
                            && ((workingString[current + 1] == 'Y') || StringAt(workingString, (current + 1), "ES", "EP", "EB", "EL", "EY", "IB", "IL", "IN", "IE", "EI", "ER"))) {
                                metaphoneData.Add("K", "J");
                                current += 2;
                                break;
                            }

                        // -ger-,  -gy-
                        if ((StringAt(workingString, (current + 1), "ER") || (workingString[current + 1] == 'Y')) && !StringAt(workingString, 0, "DANGER", "RANGER", "MANGER")
                            && !StringAt(workingString, (current - 1), "E", "I") && !StringAt(workingString, (current - 1), "RGY", "OGY")) {
                                metaphoneData.Add("K", "J");
                                current += 2;
                                break;
                            }

                        // italian e.g, 'biaggi'
                        if (StringAt(workingString, (current + 1), "E", "I", "Y") || StringAt(workingString, (current - 1), "AGGI", "OGGI")) {
                            //obvious germanic
                            if ((StringAt(workingString, 0, "VAN ", "VON ") || StringAt(workingString, 0, "SCH")) || StringAt(workingString, (current + 1), "ET")) {
                                metaphoneData.Add("K");
                            }
                            else
                                //always soft if french ending
                                if (StringAt(workingString, (current + 1), "IER ")) {
                                    metaphoneData.Add("J");
                                }
                                else {
                                    metaphoneData.Add("J", "K");
                                }
                            current += 2;
                            break;
                        }

                        if (workingString[current + 1] == 'G') {
                            current += 2;
                        }
                        else {
                            current += 1;
                        }
                        metaphoneData.Add("K");
                        break;

                    case 'H':
                        //only keep if first & before vowel or btw. 2 vowels
                        if (((current == 0) || IsVowel(workingString[current - 1])) && IsVowel(workingString[current + 1])) {
                            metaphoneData.Add("H");
                            current += 2;
                        }
                        else //also takes care of 'HH'
                        {
                            current += 1;
                        }
                        break;

                    case 'J':
                        //obvious spanish, 'jose', 'san jacinto'
                        if (StringAt(workingString, current, "JOSE") || StringAt(workingString, 0, "SAN ")) {
                            if (((current == 0) && (workingString[current + 4] == ' ')) || StringAt(workingString, 0, "SAN ")) {
                                metaphoneData.Add("H");
                            }
                            else {
                                metaphoneData.Add("J", "H");
                            }
                            current += 1;
                            break;
                        }

                        if ((current == 0) && !StringAt(workingString, current, "JOSE")) {
                            metaphoneData.Add("J", "A"); //Yankelovich/Jankelowicz
                        }
                        else
                            //spanish pron. of e.g. 'bajador'
                            if (IsVowel(workingString[current - 1]) && !isSlavoGermanic && ((workingString[current + 1] == 'A') || (workingString[current + 1] == 'O'))) {
                                metaphoneData.Add("J", "H");
                            }
                            else if (current == last) {
                                metaphoneData.Add("J", " ");
                            }
                            else if (!StringAt(workingString, (current + 1), "L", "T", "K", "S", "N", "M", "B", "Z") && !StringAt(workingString, (current - 1), "S", "K", "L")) {
                                metaphoneData.Add("J");
                            }

                        if (workingString[current + 1] == 'J') //it could happen!
                        {
                            current += 2;
                        }
                        else {
                            current += 1;
                        }
                        break;

                    case 'K':
                        if (workingString[current + 1] == 'K') {
                            current += 2;
                        }
                        else {
                            current += 1;
                        }
                        metaphoneData.Add("K");
                        break;

                    case 'L':
                        if (workingString[current + 1] == 'L') {
                            //spanish e.g. 'cabrillo', 'gallegos'
                            if (((current == (self.Length - 3)) && StringAt(workingString, (current - 1), "ILLO", "ILLA", "ALLE"))
                                || ((StringAt(workingString, (last - 1), "AS", "OS") || StringAt(workingString, last, "A", "O")) && StringAt(workingString, (current - 1), "ALLE"))) {
                                    metaphoneData.Add("L", " ");
                                    current += 2;
                                    break;
                                }
                            current += 2;
                        }
                        else {
                            current += 1;
                        }
                        metaphoneData.Add("L");
                        break;

                    case 'M':
                        if ((StringAt(workingString, (current - 1), "UMB") && (((current + 1) == last) || StringAt(workingString, (current + 2), "ER"))) //'dumb','thumb'
                            || (workingString[current + 1] == 'M')) {
                                current += 2;
                            }
                        else {
                            current += 1;
                        }
                        metaphoneData.Add("M");
                        break;

                    case 'N':
                        if (workingString[current + 1] == 'N') {
                            current += 2;
                        }
                        else {
                            current += 1;
                        }
                        metaphoneData.Add("N");
                        break;

                    case 'Ð':
                        current += 1;
                        metaphoneData.Add("N");
                        break;

                    case 'P':
                        if (workingString[current + 1] == 'H') {
                            metaphoneData.Add("F");
                            current += 2;
                            break;
                        }

                        //also account for "campbell", "raspberry"
                        if (StringAt(workingString, (current + 1), "P", "B")) {
                            current += 2;
                        }
                        else {
                            current += 1;
                        }
                        metaphoneData.Add("P");
                        break;

                    case 'Q':
                        if (workingString[current + 1] == 'Q') {
                            current += 2;
                        }
                        else {
                            current += 1;
                        }
                        metaphoneData.Add("K");
                        break;

                    case 'R':
                        //french e.g. 'rogier', but exclude 'hochmeier'
                        if ((current == last) && !isSlavoGermanic && StringAt(workingString, (current - 2), "IE") && !StringAt(workingString, (current - 4), "ME", "MA")) {
                            metaphoneData.Add("", "R");
                        }
                        else {
                            metaphoneData.Add("R");
                        }

                        if (workingString[current + 1] == 'R') {
                            current += 2;
                        }
                        else {
                            current += 1;
                        }
                        break;

                    case 'S':
                        //special cases 'island', 'isle', 'carlisle', 'carlysle'
                        if (StringAt(workingString, (current - 1), "ISL", "YSL")) {
                            current += 1;
                            break;
                        }

                        //special case 'sugar-'
                        if ((current == 0) && StringAt(workingString, current, "SUGAR")) {
                            metaphoneData.Add("X", "S");
                            current += 1;
                            break;
                        }

                        if (StringAt(workingString, current, "SH")) {
                            //germanic
                            if (StringAt(workingString, (current + 1), "HEIM", "HOEK", "HOLM", "HOLZ")) {
                                metaphoneData.Add("S");
                            }
                            else {
                                metaphoneData.Add("X");
                            }
                            current += 2;
                            break;
                        }

                        //italian & armenian
                        if (StringAt(workingString, current, "SIO", "SIA") || StringAt(workingString, current, "SIAN")) {
                            if (!isSlavoGermanic) {
                                metaphoneData.Add("S", "X");
                            }
                            else {
                                metaphoneData.Add("S");
                            }
                            current += 3;
                            break;
                        }

                        //german & anglicisations, e.g. 'smith' match 'schmidt', 'snider' match 'schneider'
                        //also, -sz- in slavic language altho in hungarian it is pronounced 's'
                        if (((current == 0) && StringAt(workingString, (current + 1), "M", "N", "L", "W")) || StringAt(workingString, (current + 1), "Z")) {
                            metaphoneData.Add("S", "X");
                            if (StringAt(workingString, (current + 1), "Z")) {
                                current += 2;
                            }
                            else {
                                current += 1;
                            }
                            break;
                        }

                        if (StringAt(workingString, current, "SC")) {
                            //Schlesinger's rule
                            if (workingString[current + 2] == 'H') {
                                //dutch origin, e.g. 'school', 'schooner'
                                if (StringAt(workingString, (current + 3), "OO", "ER", "EN", "UY", "ED", "EM")) {
                                    //'schermerhorn', 'schenker'
                                    if (StringAt(workingString, (current + 3), "ER", "EN")) {
                                        metaphoneData.Add("X", "SK");
                                    }
                                    else {
                                        metaphoneData.Add("SK");
                                    }
                                    current += 3;
                                    break;
                                }
                                else {
                                    if ((current == 0) && !IsVowel(workingString[3]) && (workingString[3] != 'W')) {
                                        metaphoneData.Add("X", "S");
                                    }
                                    else {
                                        metaphoneData.Add("X");
                                    }
                                    current += 3;
                                    break;
                                }
                            }

                            if (StringAt(workingString, (current + 2), "I", "E", "Y")) {
                                metaphoneData.Add("S");
                                current += 3;
                                break;
                            }
                            //else
                            metaphoneData.Add("SK");
                            current += 3;
                            break;
                        }

                        //french e.g. 'resnais', 'artois'
                        if ((current == last) && StringAt(workingString, (current - 2), "AI", "OI")) {
                            metaphoneData.Add("", "S");
                        }
                        else {
                            metaphoneData.Add("S");
                        }

                        if (StringAt(workingString, (current + 1), "S", "Z")) {
                            current += 2;
                        }
                        else {
                            current += 1;
                        }
                        break;

                    case 'T':
                        if (StringAt(workingString, current, "TION")) {
                            metaphoneData.Add("X");
                            current += 3;
                            break;
                        }

                        if (StringAt(workingString, current, "TIA", "TCH")) {
                            metaphoneData.Add("X");
                            current += 3;
                            break;
                        }

                        if (StringAt(workingString, current, "TH") || StringAt(workingString, current, "TTH")) {
                            //special case 'thomas', 'thames' or germanic
                            if (StringAt(workingString, (current + 2), "OM", "AM") || StringAt(workingString, 0, "VAN ", "VON ") || StringAt(workingString, 0, "SCH")) {
                                metaphoneData.Add("T");
                            }
                            else {
                                metaphoneData.Add("O", "T");
                            }
                            current += 2;
                            break;
                        }

                        if (StringAt(workingString, (current + 1), "T", "D")) {
                            current += 2;
                        }
                        else {
                            current += 1;
                        }
                        metaphoneData.Add("T");
                        break;

                    case 'V':
                        if (workingString[current + 1] == 'V') {
                            current += 2;
                        }
                        else {
                            current += 1;
                        }
                        metaphoneData.Add("F");
                        break;

                    case 'W':
                        //can also be in middle of word
                        if (StringAt(workingString, current, "WR")) {
                            metaphoneData.Add("R");
                            current += 2;
                            break;
                        }

                        if ((current == 0) && (IsVowel(workingString[current + 1]) || StringAt(workingString, current, "WH"))) {
                            //Wasserman should match Vasserman
                            if (IsVowel(workingString[current + 1])) {
                                metaphoneData.Add("A", "F");
                            }
                            else {
                                //need Uomo to match Womo
                                metaphoneData.Add("A");
                            }
                        }

                        //Arnow should match Arnoff
                        if (((current == last) && IsVowel(workingString[current - 1])) || StringAt(workingString, (current - 1), "EWSKI", "EWSKY", "OWSKI", "OWSKY")
                            || StringAt(workingString, 0, "SCH")) {
                                metaphoneData.Add("", "F");
                                current += 1;
                                break;
                            }

                        //polish e.g. 'filipowicz'
                        if (StringAt(workingString, current, "WICZ", "WITZ")) {
                            metaphoneData.Add("TS", "FX");
                            current += 4;
                            break;
                        }

                        //else skip it
                        current += 1;
                        break;

                    case 'X':
                        //french e.g. breaux
                        if (!((current == last) && (StringAt(workingString, (current - 3), "IAU", "EAU") || StringAt(workingString, (current - 2), "AU", "OU")))) {
                            metaphoneData.Add("KS");
                        }

                        if (StringAt(workingString, (current + 1), "C", "X")) {
                            current += 2;
                        }
                        else {
                            current += 1;
                        }
                        break;

                    case 'Z':
                        //chinese pinyin e.g. 'zhao'
                        if (workingString[current + 1] == 'H') {
                            metaphoneData.Add("J");
                            current += 2;
                            break;
                        }
                        else if (StringAt(workingString, (current + 1), "ZO", "ZI", "ZA") || (isSlavoGermanic && ((current > 0) && workingString[current - 1] != 'T'))) {
                            metaphoneData.Add("S", "TS");
                        }
                        else {
                            metaphoneData.Add("S");
                        }

                        if (workingString[current + 1] == 'Z') {
                            current += 2;
                        }
                        else {
                            current += 1;
                        }
                        break;

                    default:
                        current += 1;
                        break;
                }
            }

            return prefix+metaphoneData.ToString();
        }


        static bool IsVowel( char self) {
            return (self == 'A') || (self == 'E') || (self == 'I') || (self == 'O') || (self == 'U') || (self == 'Y');
        }


        static bool StartsWith(string self, StringComparison comparison, params string[] strings) {
            foreach (string str in strings) {
                if (self.StartsWith(str, comparison)) {
                    return true;
                }
            }
            return false;
        }

        static bool StringAt( string self, int startIndex, params string[] strings) {
            if (startIndex < 0) {
                startIndex = 0;
            }
            foreach (string str in strings) {
                if (self.IndexOf(str, startIndex, StringComparison.OrdinalIgnoreCase) >= startIndex) {
                    return true;
                }
            }
            return false;
        }


        class MetaphoneData
        {
            readonly StringBuilder _primary = new StringBuilder(5);
            readonly StringBuilder _secondary = new StringBuilder(5);


            #region Properties

            internal bool Alternative { get; set; }
            internal int PrimaryLength {
                get {
                    return _primary.Length;
                }
            }

            internal int SecondaryLength {
                get {
                    return _secondary.Length;
                }
            }

            #endregion


            internal void Add(string main) {
                if (main != null) {
                    _primary.Append(main);
                    _secondary.Append(main);
                }
            }

            internal void Add(string main, string alternative) {
                if (main != null) {
                    _primary.Append(main);
                }

                if (alternative != null) {
                    Alternative = true;
                    if (alternative.Trim().Length > 0) {
                        _secondary.Append(alternative);
                    }
                }
                else {
                    if (main != null && main.Trim().Length > 0) {
                        _secondary.Append(main);
                    }
                }
            }

            public override string ToString() {
                string ret = (Alternative ? _secondary : _primary).ToString();
                //only give back 4 char metaph
                if (ret.Length > 4) {
                    ret = ret.Substring(0, 4);
                }

                return ret;
            }
        }
    }
//}



    [Serializable]
    public class PhoneticHmm
    {

        // emission and transition stored as double hash tables
        public Hashtable emissionProbability = new Hashtable();
        public Hashtable transitionCounts = new Hashtable();
        public Hashtable wordsToPhones = new Hashtable();
        public Hashtable atomicTransitionCount = new Hashtable();
        public double transSum = 0.0001;

        public int hmmCorpusLoaded = 0;

        static PhoneticHmm()
        {
        
        }
    
        public string getSoundCode(string inWord)
        {
            if (wordsToPhones.ContainsKey(inWord))
            {
                try
                {
                    return (string)wordsToPhones[inWord];
                }
                catch
                {
                    wordsToPhones[inWord] = DoubleMetaphoneStringExtension.GenerateDoubleMetaphone(inWord);
                    return (string)wordsToPhones[inWord];
                }
            }
            else
            {
                wordsToPhones[inWord] = DoubleMetaphoneStringExtension.GenerateDoubleMetaphone(inWord);
                return (string)wordsToPhones[inWord];
            }

        }

        public void addTransition(string srcState, string dstState, double prob)
        {
            try
            {

                double newSum = prob;
                if (transitionCounts.ContainsKey(srcState)) { newSum = newSum + (double)transitionCounts[srcState]; }
                transitionCounts[srcState] = newSum;
            
                string atomicKey = srcState + " " + dstState;
                atomicTransitionCount[atomicKey]= prob;
                transSum += prob;
            }
            catch
            {
                AltBot.writeDebugLine("addTransition fail {0} {1} {2}", srcState, dstState, prob);
            }
        }

        public void incrTransition(string srcState, string dstState, double prob)
        {
            double incrP = prob;
            try
            {
                double newSum = prob;
                if (transitionCounts.ContainsKey(srcState)) { newSum = newSum + (double)transitionCounts[srcState]; }
                transitionCounts[srcState] = newSum;


                string atomicKey = srcState + " " + dstState;
                if (atomicTransitionCount.ContainsKey(atomicKey))
                {
                    double curP = (double)atomicTransitionCount[atomicKey];
                    atomicTransitionCount[atomicKey] = incrP +curP;
                }
                else
                {
                    atomicTransitionCount[atomicKey] = incrP;
                }
                transSum += incrP;

            }
            catch
            {
                AltBot.writeDebugLine("incrTransition fail {0} {1} {2}", srcState, dstState, prob);
            }
        }

        public double getTransitionProb(string srcState, string dstState)
        {
            try
            {
                string atomicKey = srcState + " " + dstState;
                double pSum = (double)transitionCounts[srcState];
                double pNom = (double)atomicTransitionCount[atomicKey];
                double tp = (pNom / pSum);
                if (pSum < 0.9) { tp = tp * 0.1; }
                if (tp>0.9) {tp = 0.9;}
                if (tp<0.0001) { tp = 0.0001;}
                return tp;


            }
            catch
            {
                //AltBot.writeDebugLine("   missing Transition {0} -- {1} ", srcState, dstState);
                return (double)0.0001;
            }
        }

        public void addEmission(string srcState, string emitSymbol, double prob)
        {
            try
            {
                if (emissionProbability.ContainsKey(emitSymbol))
                {
                    ((Hashtable)emissionProbability[emitSymbol])[srcState] = prob;
                }
                else
                {
                    Hashtable emitter = new Hashtable();
                    emitter.Add(srcState, prob);
                    emissionProbability.Add(emitSymbol, emitter);
                }
            }
            catch
            {
                AltBot.writeDebugLine("addEmission fail {0} {1} {2}", srcState, emitSymbol, prob);
            }


        }

        public void LearnBigrams(string line)
        {
            string learnline = "";
            double mult = 1;
            if (line.Contains("\t"))
            {
                string[] fields = line.Split('\t');
                if (fields[0].Contains("<SXQX>"))
                {
                    // it came pre-wrapped
                    learnline =fields[0].ToLower();
                    learnline = learnline.Replace("<sxqx>", "<SXQX>");
                    learnline = learnline.Replace("<fnxs>", "<FNXS>");

                }
                else
                {
                    // add wrapping
                    learnline = "<SXQX> " + fields[0].ToLower() + " <FNXS>";
                }
                mult = Math.Log( double.Parse(fields[1]));
            }
            else
            {
                learnline = "<SXQX> " + line.ToLower() + " <FNXS>";
            }
            learnline = learnline.Replace(".", " ");
            learnline = learnline.Replace("!", " ");
            learnline = learnline.Replace("?", " ");
            learnline = learnline.Replace(",", " ");
            string[] learnSet = learnline.Split(' ');
            string lastLearnWord = "<SXQX>";
            foreach (string learnWord in learnSet)
            {
                if (lastLearnWord.Length > 0)
                {
                    string scode = getSoundCode(learnWord);
                    // one emission for (lineWord->mcode)=0.9
                    addEmission(learnWord, scode, 0.9);
                    // one transition from (lastLineWord -> lineWord)=0.9
                    incrTransition(lastLearnWord, learnWord, 0.1*mult );
                    lastLearnWord = learnWord;
                }
            }
            // Insert Nulls
            return;

            lastLearnWord = "<SXQX>";
            foreach (string learnWord in learnSet)
            {
                if (lastLearnWord.Length > 0)
                {
                    string scode = getSoundCode(learnWord);
                    // one emission for (lineWord->mcode)=0.9
                    addEmission("<nullx>", scode, 0.1);

                    // one transition from (lastLineWord -> lineWord)=0.9
                    //incrTransition(lastLearnWord, learnWord, 0.1 * mult);
                    addTransition(lastLearnWord, "<nullx>", 1 / transSum);
                    addTransition("<nullx>", learnWord,  1 / transSum);
                    lastLearnWord = learnWord;
                }
            }


        }

        public void LearnBigramFile(StreamReader sr)
        {

            string line;
            long linecount = 0;
            try
            {
                while ((linecount < 5000000) && ((line = sr.ReadLine()) != null))
                {
                    linecount++;
                    if (linecount % 1000 == 0) { AltBot.writeDebugLine("Sound BG-learn {0}", linecount); }
                    line = line.Trim();
                    if (line.Length != 0 && line[0] != '#')
                    {
                        try
                        {
                            LearnBigrams(line);
                        }
                        catch (Exception e)
                        {
                        }
                    }
                }
            }
            catch (Exception e)
            {
                AltBot.writeDebugLine("Exception {0} {1}",e.Message , e.StackTrace);
            }
            AltBot.writeDebugLine("Last Line NG-learn {0}", linecount);

        }
        public string guessFromSounds(string soundLine, string inLine)
        {
            string[] soundSet = soundLine.Split(' ');
            string[] lineSet = inLine.Split(' ');
            string guessPath = "";
            // emission probabilities are strong . a given word will deterministically generate a metaphone code
            // however, it could be misheard/mispronounced/noise. So it is not 1.0 but 0.9 or some such
            // at least it doesn't have a high branch factor. With double metaphone it is at most two.

            // Implementation inspired by the bigram based decoder at http://en.wikipedia.org/wiki/Viterbi_algorithm
            // Modifed to operate with variable sized hashtables as storage
            StreamWriter decodeTrace = new StreamWriter("bgm\\decodeTrace.dot");
            decodeTrace.WriteLine("digraph G {");

            foreach (string sound in soundSet)
            {
                try
                {
                    string file = Path.Combine("bgm", sound);
                    //if (Directory.Exists(file))
                    if (File.Exists(file))
                    {
                        AltBot.writeDebugLine("Load Bigrams: '{0}'", file);
                        StreamReader sr = new StreamReader(file);
                        LearnBigramFile(sr);
                        sr.Close();
                    }
                }
                catch (Exception e)
                {
                }
            }
            // ok, start guessing
            string lastArgMax = "<SXQX>";
            Hashtable lastStateP = new Hashtable();
            Hashtable lastStatePath = new Hashtable();
            Hashtable lastStateVP = new Hashtable();

            Hashtable nextStateP = new Hashtable();
            Hashtable nextStatePath = new Hashtable();
            Hashtable nextStateVP = new Hashtable();

            lastStateP.Add(lastArgMax, 1.0);
            lastStatePath.Add(lastArgMax, lastArgMax);
            lastStateVP.Add(lastArgMax, 1.0);

            // The storage mechanism
            string[] vPath = new string[soundSet.Length];
            double[] vProbs = new double[soundSet.Length];
            double scaleFactor = 10;
            double smoother = 1 / transSum;

            for (int output = 0; output < soundSet.Length - 2; output++)
            {
                string cur_observedSymbol = soundSet[output];
                string next_observedSymbol = soundSet[output + 1];
                //nextStateP.Clear();
                //nextStatePath.Clear();
                //nextStateVP.Clear();

                AltBot.writeDebugLine("\nTesting hypothesis {0} ({1})", output, cur_observedSymbol);

                decodeTrace.WriteLine("\"t{0}\" -> \"t{1}\" ;", output , output + 1);
                decodeTrace.WriteLine("\"t{0}\" -> \"snd({1})\" ;", output, cur_observedSymbol);
                decodeTrace.WriteLine("\"t{0}\" -> \"snd({1})\" ;", output + 1, next_observedSymbol);

                // Get emission table
                Hashtable cur_emitter = ((Hashtable)emissionProbability[cur_observedSymbol]);
                Hashtable next_emitter = ((Hashtable)emissionProbability[next_observedSymbol]);
                if (!cur_emitter.ContainsKey("<nullx>")) { cur_emitter.Add("<nullx>",(double)0.1); }
                if (!next_emitter.ContainsKey("<nullx>"))    {                next_emitter.Add("<nullx>", (double)0.1);            }
                double highest = 0;
                double nextSum = 0.00001;
                foreach (string nextStateKey in next_emitter.Keys)
                {
                    if (!transitionCounts.ContainsKey(nextStateKey)) transitionCounts[nextStateKey]=(double) 0.01;
                    nextSum += ((double)transitionCounts[nextStateKey]);
                }
                double nextavg = nextSum / (double)next_emitter.Count;

                double frameSum = 0.00001;
                foreach (string stateKey in cur_emitter.Keys)
                {
                    if (!transitionCounts.ContainsKey(stateKey)) transitionCounts[stateKey] = (double)0.01;
                    frameSum += (double)transitionCounts[stateKey];
                }
                double frameAvg = (double)frameSum / (double)cur_emitter.Count;

                //for (int nextState = 0; nextState < next_emitter.Keys.Count; nextState++)
                foreach (string nextStateKey in next_emitter.Keys)
                {
                    double total = 0;
                    string argMaxKey = "";
                    double valMax = 0;
                    //string nextStateKey = next_emitter.Keys.Count[nextState];
                    //AltBot.writeDebugLine("  Estimating probability for future state ({0})", nextStateKey);
                    //for (int state = 0; state < cur_emitter.Keys.Count; state++)
                    foreach (string stateKey in cur_emitter.Keys)
                    {
                        double em_p = 1;// (double)transitionCounts[stateKey] / frameSum;
                        //string stateKey = cur_emitter.Keys[state];
                        //AltBot.writeDebugLine("    The testing state is {0} ", stateKey);
                        double prob = (double)0.0001;
                        string v_path = (string)"";
                        double v_prob = (double)0.0001;
                        if (lastStateP.ContainsKey(stateKey))
                        {
                            prob = (double)lastStateP[stateKey];
                            v_path = (string)lastStatePath[stateKey];
                            v_prob = (double)lastStateVP[stateKey];

                        }
                        double eP = (em_p * (double)cur_emitter[stateKey]);

                        // Our faster transition lookup
                        double transP = 0.00001; // should be smarter
                        string atomicKey = stateKey + " " + nextStateKey;
                        if (atomicTransitionCount.ContainsKey(atomicKey))
                        {
                            transP = (double)atomicTransitionCount[atomicKey] / (double)transitionCounts[stateKey];
                            AltBot.writeDebugLine(" P({0}) = {1}", atomicKey, transP);
                            decodeTrace.WriteLine("\"{0}\" -> \"{1}\" ;",stateKey,nextStateKey);
                            decodeTrace.WriteLine("\"{0}\" -> \"{1}\" ;", cur_observedSymbol, stateKey);
                            decodeTrace.WriteLine("\"{0}\" -> \"{1}\" ;", next_observedSymbol, nextStateKey);
                            decodeTrace.WriteLine("\"{0}\" -> \"{1}\" ;", cur_observedSymbol, atomicKey);
                            decodeTrace.WriteLine("\"{0}\" -> \"{1}\" ;", next_observedSymbol, atomicKey);

                        }
                        else
                        {
                            // use default smoother
                            transP = smoother;
                            if ((stateKey == "<nullx>") || (nextStateKey == "<nullx>")) 
                            { 
                                transP = transP * 3; 
                            }
                        }

                        double p =  eP* transP * scaleFactor;
                        prob *= p;
                        v_prob *= p;
                        total += prob;

                        if (v_prob > valMax)
                        {
                            valMax = v_prob;
                            argMaxKey = v_path + " " + nextStateKey;
                        }
                        //AltBot.writeDebugLine("    VProbability of {0} --{1} is {2} with scale {3}^{4}  ep={5} tp={6}",stateKey, nextStateKey, v_prob, scaleFactor, output + 1,eP,transP);
                        if (v_prob > highest)
                        {
                            highest = v_prob;
                            vPath[output] = nextStateKey;
                            vProbs[output] = v_prob;
                        }
                    }// for state
                    //AltBot.writeDebugLine(" Fin NextState :{0}", nextStateKey);
                    nextStateP[nextStateKey] = total;
                    nextStatePath[nextStateKey] = argMaxKey; // lastStatePath[argMaxKey] + " " + argMaxKey;
                    nextStateVP[nextStateKey] = valMax;
                
                }
                //AltBot.writeDebugLine(" Fin outputstep  :{0} [{1}]", output,vPath[output]);
                lastStateP = nextStateP;
                lastStatePath = nextStatePath;
                lastStateVP = nextStateVP;
                AltBot.writeDebugLine("The highest probability was {0} in state {1} [{2}](scale factor of {3}^{4})", highest, vPath[output], (string)lastStatePath[vPath[output]], scaleFactor, output + 1);
                guessPath = (string)lastStatePath[vPath[output]]; //+= vPath[output] + " ";
                decodeTrace.WriteLine("\"m{0}\" -> \"{1}\" ;", output, vPath[output]);
                decodeTrace.WriteLine("\"m{0}\" -> \"m{1}\" ;", output,output+1);
            }
            AltBot.writeDebugLine(" Guess = [{0}]", guessPath);

            // optionally use this guess as an training input to adjust the 
            // model towards the current discussion
            LearnBigrams(guessPath);
            decodeTrace.WriteLine("}");
            decodeTrace.Close();

            return guessPath;
        }
    }
}