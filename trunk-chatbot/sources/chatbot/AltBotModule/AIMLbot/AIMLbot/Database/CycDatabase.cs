using System;
using System.Collections.Generic;
using System.IO;
using System.Text;
using System.Xml;
using AltAIMLbot;
using AltAIMLbot.Utils;
using AltAIMLParser;
using MushDLR223.Utilities;
using MushDLR223.Virtualization;
using org.opencyc.api;
using org.opencyc.cycobject;

namespace AltAIMLbot.Database
{
    abstract public class CycTagHandler:AIMLTagHandler
    {
        protected CycTagHandler(AltBot bot,
                        User user,
                        SubQuery query,
                        Request request,
                        Result result,
                        XmlNode templateNode)
            : base(bot, user, query, request, result, templateNode)
        {
        }

        public CycDatabase TheCyc
        {
            get { return Proc.TheCyc; }
        }

        #region Overrides of TextTransformer

        /// <summary>
        /// The method that does the actual processing of the text.
        /// </summary>
        /// <returns>The resulting processed text</returns>
        //protected abstract Unifiable ProcessOuterChange();

        #endregion
    }
    public class CycDatabase
    {
        static public CycDatabase TheStaticCyc;

        private AltBot TheBot;
        public CycDatabase(AltBot bot)
        {
            TheStaticCyc = this;
            TheBot = bot;   
        }
        #region CYC Interaction


        private CycAccess cycAccess;
        public bool CycEnabled
        {
            get
            {
                if (!UseCyc) return false;
                cycAccess = GetCycAccess;
                return UseCyc;
            }
            set
            {
                UseCyc = value;
            }
        }

        public static bool AllUseCyc = false;
        private bool UseCyc = AllUseCyc;
        static object UseCycLock = new object();

        public CycAccess GetCycAccess
        {
            get
            {
                lock (UseCycLock)
                {
                    if (!UseCyc) return null;
                    if (!isCycAccessCorrect())
                    {
                        try
                        {
                            if (cycAccess == null) cycAccess = CycAccess.current();
                        }
                        catch (Exception)
                        {
                        }
                        try
                        {
                            if (!isCycAccessCorrect())
                            {
                                cycAccess = new CycAccess(CycHostName, CycBasePort);
                                CycAccess.setSharedCycAccessInstance(cycAccess);
                            }
                            TestConnection();
                            populateFromCyc();
                            TheStaticCyc = this;
                        }
                        catch (Exception e)
                        {
                            writeToLog("UseCyc==false " + e.Message + "\n " + e.StackTrace + "\n " + e.InnerException);
                            CycAccess.setSharedCycAccessInstance(null);
                            UseCyc = false;
                        }
                        //if (cycAccess.isClosed()) cycAccess.persistentConnection = true;
                    }
                    return cycAccess;
                }
            }
            set { cycAccess = value; }
        }

        private void TestConnection()
        {
            lock (UseCycLock)
            {
                try
                {
                    cycAccess.converseInt("(+ 1 1)");
                    cycAccess.createIndividual("AimlContextMt",
                                               "#$AimlContextMt contains storage location in OpenCyc for AIML variables",
                                               "UniversalVocabularyMt", "DataMicrotheory");

                }
                catch (Exception e)
                {
                    UseCyc = false;
                }
            }
        }

        private bool isCycAccessCorrect()
        {
            lock (UseCycLock)
            {
                if (cycAccess == null) return false;
                if (cycAccess.isClosed()) return false;
                if (cycAccess.getCycConnection().getHostName() != CycHostName) return false;
                if (cycAccess.getCycConnection().getBasePort() != CycBasePort) return false;
                return true;
            }
        }

        private int cycBasePort = -1;
        public int CycBasePort
        {
            get
            {
                lock (UseCycLock)
                {
                    if (cycBasePort > 0) return cycBasePort;
                    if (cycAccess != null) return cycBasePort = cycAccess.getCycConnection().getBasePort();
                    return cycBasePort = CycConnection.DEFAULT_BASE_PORT;
                }
            }
            set
            {
                lock (UseCycLock)
                {
                    if (cycBasePort == -1) cycBasePort = value;
                    if (CycBasePort == value) return;
                    cycBasePort = value;
                    cycAccess = null;
                }
            }
        }

        // So people dont have to use their own cyc instance (unless they set it)
        private String cycHostName = "logicmoo.dyndns.org";
        public string CycHostName
        {
            get
            {
                lock (UseCycLock)
                {
                    if (cycHostName != null) return cycHostName;
                    if (cycAccess != null) return cycHostName = cycAccess.getCycConnection().getHostName();
                    return cycHostName = CycConnection.DEFAULT_HOSTNAME;
                }
            }
            set
            {
                lock (UseCycLock)
                {
                    if (cycHostName == null) cycHostName = value;
                    if (cycHostName == value) return;
                    cycHostName = value;
                    cycAccess = null;
                }
            }
        }

        
        static private Dictionary<string, Unifiable> stringTOResult = new Dictionary<string, Unifiable>();
        static private Unifiable NILTerm = "NIL";

        public bool Lookup(Unifiable textIn, Unifiable filter, out Unifiable term, SubQuery subquery)
        {
            return Lookup1(textIn, filter, out term, subquery);
        }

        public bool Lookup1(Unifiable textIn,Unifiable filter,out Unifiable term, SubQuery subquery)
        {
            if (Unifiable.IsNullOrEmpty(textIn))
            {
                term = null;
                return false;
            }
            if (textIn.AsString().Contains("#$"))
            {
                term = textIn;
                if (!Unifiable.IsNullOrEmpty(term))
                {
                    if (!IsaFilter(term, filter))
                    {
                        return false;
                    }
                }
                return true;
            }
            lock (stringTOResult)
            {
                string key = "" + textIn + "=" + filter;
                if (stringTOResult.TryGetValue(key, out term))
                {
                    if (term == NILTerm) return false;
                    return true;
                }
                bool t = lookup0(textIn, filter, out term, subquery);
                bool meansFalse = Unifiable.IsFalse(term);
                if (t != meansFalse)
                {
                    if (meansFalse)
                    {
                        writeToLog("NILTerm true=" + t + " term=" + term + " textIn" + textIn);
                        term = NILTerm;
                    }
                    else
                    {
                        string paraphrase = Paraphrase(term);
                        writeToLog("true={0} term={1} textIn{2}  paraPhrase={3}", t, term, textIn, paraphrase);
                    }
                }
                stringTOResult[key] = term;
                return t;                
            }
        }

        private bool lookup0(Unifiable textIn, Unifiable filter, out Unifiable term, SubQuery subquery)
        {
            string text = textIn.ToValue(subquery);
            if (text.Length < 2)
            {
                term = text;
                return false;
            }
            string textStr = text.StartsWith("\"") ? text : String.Format("\"{0}\"", text);
            if (!CycEnabled)
            {
                term = textStr;
                return true;
            }
            filter = Cyclify(filter);
            bool nospaces = !text.Contains(" ");

            string nqtext = textStr.Trim("\"".ToCharArray());
            string ptext = nqtext.Substring(0, 1).ToUpper() + nqtext.Substring(2);

            if (nospaces)
            {
                if (Unifiable.IsFalse(EvalSubL(String.Format("(car (fi-complete \"{0}-TheWord\"))", ptext), null)))
                {
                    //no word used
                    nospaces = false;
                }
            }
            if(false
            || lookupCycTerm("(#$nameString ?CYCOBJECT \"%s\")", text, filter, out term)
            || (nospaces && (lookupCycTerm("(#$denotation #$%s-TheWord ?TEXT ?TYPE ?CYCOBJECT)", ptext, filter, out term)
            || lookupCycTerm("(#$denotationRelatedTo #$%s-TheWord ?TEXT ?TYPE ?CYCOBJECT)", ptext, filter, out term)))
            || lookupCycTerm("(#$initialismString ?CYCOBJECT \"%s\")", text, filter,out term)
            || lookupCycTerm("(#$abbreviationString-PN ?CYCOBJECT \"%s\")", text, filter,out term)
            || lookupCycTerm("(#$preferredNameString ?CYCOBJECT \"%s\")", text, filter,out term)
            || lookupCycTerm("(#$countryName-LongForm ?CYCOBJECT \"%s\")", text, filter,out term)
            || lookupCycTerm("(#$countryName-ShortForm ?CYCOBJECT \"%s\")", text, filter,out term)
            || lookupCycTerm("(#$acronymString ?CYCOBJECT \"%s\")", text, filter,out term)
            || lookupCycTerm("(#$scientificName ?CYCOBJECT \"%s\")", text, filter,out term)
            || lookupCycTerm("(#$termStrings-GuessedFromName ?CYCOBJECT \"%s\")", text, filter, out term)
            || lookupCycTerm("(#$prettyString ?CYCOBJECT \"%s\")", text, filter,out term)
            || lookupCycTerm("(#$preferredTermStrings ?CYCOBJECT \"%s\")", text, filter, out term)
            || lookupCycTerm("(#$and (#$isa ?P #$ProperNamePredicate-Strict)(?P ?CYCOBJECT \"%s\"))", text, filter, out term)
            || lookupCycTerm("(#$and (#$isa ?P #$ProperNamePredicate-General)(?P ?CYCOBJECT \"%s\"))", text, filter, out term)
            || (nospaces && lookupCycTerm("(#$preferredGenUnit ?CYCOBJECT ?POS #$%s-TheWord )", ptext, filter, out term))                        
            || lookupCycTerm("(#$termStrings ?CYCOBJECT \"%s\")", text, filter, out term)
            || lookupCycTerm("(#$nicknames ?CYCOBJECT \"%s\")", text, filter, out term)
            || lookupCycTerm("(#$and (#$wordStrings ?WORD \"%s\") (#$or (#$denotation ?WORD ?TEXT ?TYPE ?CYCOBJECT) (#$denotationRelatedTo ?WORD ?TEXT ?TYPE ?CYCOBJECT) ))", text, filter,out term))            
                return true;

            term = EvalSubL(String.Format("(car (fi-complete \"{0}\"))", nqtext), null);
            // Followed by asking Cyc to guess at the word using (fi-complete \”%s\”)
            if (Unifiable.IsTrue(term))
            {
                if (IsaFilter(term, filter))
                {
                    return true;
                }
            }
            term = EvalSubL(String.Format("(cdr (car (denotation-mapper \"{0}\")))", nqtext), null);
            if (Unifiable.IsTrue(term))
            {
                if (IsaFilter(term, filter))
                {
                    return true;
                }
            }
            term = EvalSubL(String.Format("(car (denots-of-string \"{0}\"))", nqtext), null);
            if (Unifiable.IsTrue(term))
            {
                if (IsaFilter(term, filter))
                {
                    return true;
                }
            }
            // and if that fails returns a Unifiable of using #$\”%s\”
            term = string.Format("#${0}", text);
            return false;
        }

        //(mapcar #'(lambda (x) (pwhen (member col x) ))  (denotation-mapper "isa"))

        private bool lookupCycTerm(string template, string text,Unifiable filter, out Unifiable term)
        {            
            string textStr = text.StartsWith("\"") ? text : ("\"" + text + "\"");
            if (!char.IsLetter(text[0]))
            {
                
            }
            template = template.Replace("\"%s\"", textStr);
            
            //template = template.Replace("#$%s-TheWord", textWord);   
            template = template.Replace("%s", text);
            
            
            try
            {
                term = EvalSubL(TextPatternUtils.SafeFormat("(first (ask-template '?CYCOBJECT '(#$and {0} (#$isa ?CYCOBJECT {1})) #$EverythingPSC))", template, Unifiable.ToVMString(filter)), null);
            }
            catch (System.Exception ex)
            {
                term = null;
                return false;
            }
            if (Unifiable.IsFalse(term)) return false;
            return true;
        }
    

        private void populateFromCyc()
        {
            TheBot.AddExcuteHandler("cycl", ExecCycQuery);
            TheBot.AddExcuteHandler("subl", EvalSubLHandler);

            int id = 1;
            FileStream f = HostSystem.Open("nodes.txt",FileMode.Create);
            TextWriter tw = new StreamWriter(f);
            foreach (var item in cycAccess.converseList("(ask-template '?R '(#$and (#$genls ?R #$Communicating)(#$not (#$isa ?R #$NonVerbalCommunicating))) #$EverythingPSC)"))
            {
                string text = item.ToString();
                string s =
                    "<Shape ID='" + id++ +
                    "' NameU='Decision' Type='Shape' Master='0'><XForm><PinX>0.984251968503937</PinX><PinY>11.02362204724409</PinY><Width Unit='MM' F='Inh'>0.984251968503937</Width><Height Unit='MM' F='Inh'>0.5905511811023623</Height><LocPinX Unit='MM' F='Inh'>0.4921259842519685</LocPinX><LocPinY Unit='MM' F='Inh'>0.2952755905511811</LocPinY><Angle F='Inh'>0</Angle><FlipX F='Inh'>0</FlipX><FlipY F='Inh'>0</FlipY><ResizeMode F='Inh'>0</ResizeMode></XForm><Event><TheData F='No Formula'>0</TheData><TheText F='No Formula'>0</TheText><EventDblClick F='Inh'>0</EventDblClick><EventXFMod F='No Formula'>0</EventXFMod><EventDrop F='No Formula'>0</EventDrop></Event><vx:Event xmlns:vx='http://schemas.microsoft.com/visio/2006/extension'><vx:EventMultiDrop F='No Formula'>0</vx:EventMultiDrop></vx:Event><LayerMem><LayerMember>0</LayerMember></LayerMem><Text>" +
                    text + "</Text></Shape>";
                tw.WriteLine(s);
              
            }
            tw.Close();
            try
            {
                f.Close();
            }
            catch (Exception)
            {                
            }
            AltBot.writeDebugLine("!NonVerbalCommunicating = " + id);
            //cycAccess.setCyclist("CycAdministrator");
        }

        private object ExecCycQuery(string cmd, Request user)
        {
            try
            {
                if (String.IsNullOrEmpty(cmd)) return "NIL";
                Unifiable ss = EvalSubL("(cyc-query '" + cmd + " #$EverythingPSC)", null);
                if (Unifiable.IsFalse(ss))
                {
                    return Unifiable.Empty;
                }
                return ss;
            }
            catch (Exception e)
            {
                TheBot.writeToLog(e);
                string s = "ExecCycQuery: " + e;
                AltBot.writeDebugLine(s);
                writeToLog(s);
                return null;
            }
        }
        private object EvalSubLHandler(string cmd, Request user)
        {
            try
            {
                return EvalSubL(cmd, null);
            }
            catch (Exception e)
            {
                TheBot.writeToLog(e);
                string s = "EvalSubLHandler: " + e;
                AltBot.writeDebugLine(s);
                writeToLog(s);
                return null;
            }
        }

        public Unifiable EvalSubL(Unifiable cmd, Unifiable filter)
        {
            Unifiable result = "(EVAL-SUBL " + cmd + ")";
            CycAccess access = GetCycAccess;
            if (!UseCyc)
            {
                writeToLog("NOT USE CYC " + result);
                return null;// "NIL";
            }
            try
            {
                string str = "(list " + cmd + ")";
                Object oresult = access.converseList(str).first();
                DLRConsole.DebugWriteLine(str + " => " + oresult);
                result = "" + oresult;
                if (oresult is CycObject)
                {
                    result = ((CycObject)oresult).cyclifyWithEscapeChars();
                }
                if (!Unifiable.IsNullOrEmpty(filter) && filter == "paraphrase")
                {
                    return this.Paraphrase(result);
                }
            }
            catch (Exception e)
            {
                DLRConsole.DebugWriteLine(result);
                TheBot.writeToLog(e);
                DLRConsole.DebugWriteLine("\n" + e);
                DLRConsole.SystemFlush();
                return null;
            }
            return result;
        }

        private void writeToLog(string s, params object[] p)
        {
            TheBot.writeToLog("CYCTRACE: " + s, p);
        }


        internal bool IsaFilter(Unifiable term, Unifiable filter)
        {
            if (!Unifiable.IsValue(term)) return false;
            if (term == "NIL") return false;
            if (!Unifiable.IsNullOrEmpty(filter))
            {
                if (Unifiable.IsFalse(filter)) return true;
                if (this.EvalSubL(TextPatternUtils.SafeFormat("(ask-template 'T `(#$isa {0} {1}) #$EverythingPSC)", Cyclify(term), Cyclify(filter)), null) == "NIL")
                    return false;
            }
            return true;
        }

        public Unifiable CycCleanupCyc(string text)
        {
            text = TextPatternUtils.ReTrimAndspace(text);
            string s = CleanupCyc0(text);
            if (s=="NIL")
            {
                writeToLog("ERROR: became nil: " + text);
                return text;
            }
            return s;
        }
        private Unifiable CleanupCyc0(string text)
        {
            text = TextPatternUtils.ReTrimAndspace(text);
            int l_1 = text.Length - 1;
            if (l_1 < 2) return text;
            char c = text[l_1];
            if (!text.Contains("#$")) return text;
            if (char.IsPunctuation(c))
            {
                return CleanupCyc0(text.Substring(0, l_1)) + c;
            }
            int i = text.IndexOf(" ");
            if (i > 0)
            {
                String stext = CleanupCyc0(text.Substring(0, i)).AsString() + " " + CleanupCyc0(text.Substring(i + 1)).AsString();
                return stext;
            }
            if (text.StartsWith("#$") || text.StartsWith("(#$"))
            {
                return TextPatternUtils.ReTrimAndspace(Paraphrase0(text).AsString().Replace("#$", " "));
            }
            return TextPatternUtils.ReTrimAndspace(text.Replace("#$", " "));
        }
        public Unifiable Paraphrase(string text)
        {
            try
            {
                text = Cyclify(text);
            }
            catch (Exception e)
            {
                TheBot.writeToLog(e);
                writeToLog("couldnt CYCLIFY " + text);
            }
            return Paraphrase0(text);
        }
        internal Unifiable Paraphrase0(string text)
        {
            if (!CycEnabled) return TextPatternUtils.ReTrimAndspace(text.Replace("#$", " "));
            if (text.StartsWith("("))
            {   //todo is a list then?
                text = String.Format("'{0}", text);
            } else if (text.StartsWith("'#$"))
            {
                text = text.Substring(1);
            }
            //return text;
            try
            {
                text = text.TrimEnd(".".ToCharArray());
                if (text.Trim().Length==0)
                {
                    
                }
                if (text == "'()" || text == "NIL") return "";
                string res = EvalSubL(String.Format("(generate-phrase {0})", text), null);
                if (String.IsNullOrEmpty(res)) return text;
                return res;
            }
            catch (System.Exception ex)
            {
                return text;
            }
        }

        internal Unifiable Cyclify(string mt)
        {
            if (mt == "NIL") return "NIL";
            mt = mt.Trim();
            if (mt.Length < 3) return mt;

            if (mt.StartsWith("(") || mt.StartsWith("#$") || mt.StartsWith("\"")) return mt;
            if (Char.IsDigit(mt.ToCharArray()[0])) return mt;
            return "#$" + mt;
        }

        #endregion


        public void WriteConfig()
        {
            AltBot.writeDebugLine("Cyc loaded");
        }

        public Unifiable WhenTrue(Unifiable unifiable)
        {
            throw new NotImplementedException();
        }
    }
}
