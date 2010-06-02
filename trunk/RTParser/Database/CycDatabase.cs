using System;
using System.Collections.Generic;
using System.IO;
using System.Text;
using System.Xml;
using org.opencyc.api;
using org.opencyc.cycobject;
using RTParser;
using RTParser.Utils;

namespace RTParser.Database
{
    abstract public class CycTagHandler:AIMLTagHandler
    {
        protected CycTagHandler(RTParser.RTPBot bot,
                        RTParser.User user,
                        RTParser.Utils.SubQuery query,
                        RTParser.Request request,
                        RTParser.Result result,
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
        protected abstract override Unifiable ProcessChange();

        #endregion
    }
    public class CycDatabase
    {
        private RTPBot TheBot;
        public CycDatabase(RTPBot bot)
        {
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

        private bool UseCyc = true;
        public CycAccess GetCycAccess
        {
            get
            {
                if (!UseCyc) return null;
                if (!isCycAccessCorrect())
                {
                    try
                    {
                        if (cycAccess == null) cycAccess = CycAccess.current();
                    }
                    catch (Exception) { }
                    try
                    {
                        if (!isCycAccessCorrect())
                        {
                            cycAccess = new CycAccess(CycHostName, CycBasePort);
                            CycAccess.setSharedCycAccessInstance(cycAccess);
                        }
                        TestConnection();
                        populateFromCyc();
                    }
                    catch (Exception e)
                    {
                        UseCyc = false;
                    }
                    //if (cycAccess.isClosed()) cycAccess.persistentConnection = true;
                }
                return cycAccess;
            }
            set { cycAccess = value; }
        }

        private void TestConnection()
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

        private bool isCycAccessCorrect()
        {
            if (cycAccess == null) return false;
            if (cycAccess.isClosed()) return false;
            if (cycAccess.getCycConnection().getHostName() != CycHostName) return false;
            if (cycAccess.getCycConnection().getBasePort() != CycBasePort) return false;
            return true;
        }

        private int cycBasePort = -1;
        public int CycBasePort
        {
            get
            {
                if (cycBasePort > 0) return cycBasePort;
                if (cycAccess != null) return cycBasePort = cycAccess.getCycConnection().getBasePort();
                return cycBasePort = CycConnection.DEFAULT_BASE_PORT;
            }
            set
            {
                if (cycBasePort == -1) cycBasePort = value;
                if (CycBasePort == value) return;
                cycBasePort = value;
                cycAccess = null;
            }
        }

        // So people dont have to use their own cyc instance (unless they set it)
        private String cycHostName = "logicmoo.ath.cx";
        public string CycHostName
        {
            get
            {
                if (cycHostName != null) return cycHostName;
                if (cycAccess != null) return cycHostName = cycAccess.getCycConnection().getHostName();
                return cycHostName = CycConnection.DEFAULT_HOSTNAME;
            }
            set
            {
                if (cycHostName == null) cycHostName = value;
                if (cycHostName == value) return;
                cycHostName = value;
                cycAccess = null;
            }
        }

        
        static private Dictionary<string, Unifiable> stringTOResult = new Dictionary<string, Unifiable>();
        static private Unifiable NILTerm = "NIL";
        public bool Lookup(Unifiable textIn,Unifiable filter,out Unifiable term)
        {
            lock (stringTOResult)
            {
                string key = "" + textIn + "=" + filter;
                if (stringTOResult.TryGetValue(key, out term))
                {
                    if (term == NILTerm) return false;
                    return true;
                }
                bool t = lookup0(textIn, filter, out term);
                if (term == null) term = NILTerm;
                stringTOResult.Add(key, term);
                return t;                
            }
        }

        private bool lookup0(Unifiable textIn, Unifiable filter, out Unifiable term)
        {
            string text = textIn.ToValue();
            if (text.Length < 2)
            {
                term = text;
                return false;
            }
            if (!CycEnabled)
            {
                term = String.Format("\"{0}\"", text);
                return true;
            }
            filter = Cyclify(filter);
            Unifiable ptext = textIn.ToPropper();
            if(false
            || lookupCycTerm("(#$nameString ?CYCOBJECT \"%s\")", text, filter, out term)
            || lookupCycTerm("(#$denotation #$%s-TheWord ?TEXT ?TYPE ?CYCOBJECT)", ptext, filter, out term)
            || lookupCycTerm("(#$denotationRelatedTo #$%s-TheWord ?TEXT ?TYPE ?CYCOBJECT)", ptext, filter,out term)
            || lookupCycTerm("(#$initialismString ?CYCOBJECT \"%s\")", text, filter,out term)
            || lookupCycTerm("(#$abbreviationString-PN ?CYCOBJECT \"%s\")", text, filter,out term)
            || lookupCycTerm("(#$preferredNameString ?CYCOBJECT \"%s\")", text, filter,out term)
            || lookupCycTerm("(#$countryName-LongForm ?CYCOBJECT \"%s\")", text, filter,out term)
            || lookupCycTerm("(#$countryName-ShortForm ?CYCOBJECT \"%s\")", text, filter,out term)
            || lookupCycTerm("(#$acronymString ?CYCOBJECT \"%s\")", text, filter,out term)
            || lookupCycTerm("(#$scientificName ?CYCOBJECT \"%s\")", text, filter,out term)
            || lookupCycTerm("(#$termStrings ?CYCOBJECT \"%s\")", text, filter,out term)
            || lookupCycTerm("(#$termStrings-GuessedFromName ?CYCOBJECT \"%s\")", text, filter,out term)
            || lookupCycTerm("(#$prettyString ?CYCOBJECT \"%s\")", text, filter,out term)
            || lookupCycTerm("(#$nicknames ?CYCOBJECT \"%s\")", text, filter,out term)
            || lookupCycTerm("(#$preferredTermStrings ?CYCOBJECT \"%s\")", text, filter, out term)
            || lookupCycTerm("(#$and (#$isa ?P #$ProperNamePredicate-Strict)(?P ?CYCOBJECT \"%s\"))", text, filter, out term)
            || lookupCycTerm("(#$and (#$isa ?P #$ProperNamePredicate-General)(?P ?CYCOBJECT \"%s\"))", text, filter, out term)
            || lookupCycTerm("(#$preferredGenUnit ?CYCOBJECT ?POS #$%s-TheWord )", ptext, filter, out term)
            || lookupCycTerm("(#$and (#$wordStrings ?WORD \"%s\") (#$or (#$denotation ?WORD ?TEXT ?TYPE ?CYCOBJECT) (#$denotationRelatedTo ?WORD ?TEXT ?TYPE ?CYCOBJECT) ))", text, filter,out term))            
                return true;
            term = EvalSubL(String.Format("(car (fi-complete \"{0}\"))", text),null);
            // Followed by asking Cyc to guess at the word using (fi-complete \”%s\”)
            if (Unifiable.IsTrue(term))
            {
                if (IsaFilter(term, filter))
                {
                    return true;
                }
            }
            term = EvalSubL(String.Format("(cdr (car (denotation-mapper \"{0}\")))", text), null);
            if (Unifiable.IsTrue(term))
            {
                if (IsaFilter(term, filter))
                {
                    return true;
                }
            }
            term = EvalSubL(String.Format("(car (denots-of-string \"{0}\"))", text), null);
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

        private bool lookupCycTerm(string template, Unifiable text,Unifiable filter, out Unifiable term)
        {
            template = template.Replace("%s", text);            
            try
            {
	            term = EvalSubL(String.Format("(first (ask-template '?CYCOBJECT '(#$and {0} (#$isa ?CYCOBJECT {1})) #$EverythingPSC))", template,filter), null);
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
            FileStream f = File.Open("nodes.txt",FileMode.Create);
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
            Console.WriteLine("!NonVerbalCommunicating = " + id);
            //cycAccess.setCyclist("CycAdministrator");
        }

        private object ExecCycQuery(string cmd, Request user)
        {
            try
            {
                Unifiable ss = EvalSubL("(cyc-query '" + cmd + " #$EverythingPSC)", null);
                if (Unifiable.IsFalse(ss))
                {
                    return Unifiable.Empty;
                }
                return ss;
            }
            catch (Exception e)
            {
                string s = "" + e;
                Console.WriteLine(s);
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
                string s = "" + e;
                Console.WriteLine(s);
                writeToLog(s);
                return null;
            }
        }

        public Unifiable EvalSubL(Unifiable cmd, Unifiable filter)
        {
            Unifiable result = "(EVAL-SUBL " + cmd + ")";
            CycAccess access = GetCycAccess;
            Console.Write(result);
            Console.Out.Flush();
            if (!UseCyc) return "NIL";
            try
            {
                string str = "(list " + cmd + ")";
                Object oresult = access.converseList(str).first();
                writeToLog(str + " => " + oresult);
                result = "" + oresult;
                if (oresult is CycObject)
                {
                    result = ((CycObject)oresult).cyclifyWithEscapeChars();
                }
                if (!String.IsNullOrEmpty(filter) && filter == "paraphrase")
                {
                    return this.Paraphrase(result);
                }
            }
            catch (Exception e)
            {
                writeToLog("" + e);
                Console.Out.Flush();
                return null;
            }
            return result;
        }

        private void writeToLog(string s)
        {
            TheBot.writeToLog(s);
        }


        internal bool IsaFilter(Unifiable term, Unifiable filter)
        {
            if (term.IsEmpty) return false;
            if (term == "NIL") return false;
            if (!filter.IsEmpty)
            {
                if (Unifiable.IsFalse(filter)) return true;
                if (this.EvalSubL(String.Format("(ask-template 'T `(#$isa {0} {1}) #$EverythingPSC)", term, Cyclify(filter)), null) == "NIL")
                    return false;
            }
            return true;
        }

        public Unifiable CleanupCyc(string text)
        {
            text = text.Replace("  ", " ").Trim();
            int i = text.IndexOf(" ");
            if (i < 0)
            {
                if (text.StartsWith("#") || text.StartsWith("("))
                {
                    return Paraphrase0(text).AsString().Replace("#$", " ").Replace("  ", " ");
                }
                return text;
            }
            return text.Substring(0, i) + " " + CleanupCyc(text.Substring(i + 1));
        }
        public Unifiable Paraphrase(string text)
        {
            text = Cyclify(text);
            return Paraphrase0(text);
        }
        internal Unifiable Paraphrase0(string text)
        {
            if (!CycEnabled) return text.Replace("#$", " ").Replace("  ", " ");
            if (text.StartsWith("("))
            {   //todo is a list then?
                text = String.Format("'{0}", text);
            }
            //return text;
            try
            {
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
            if (Char.IsLetter(mt.ToCharArray()[0])) return "#$" + mt;
            if (Char.IsDigit(mt.ToCharArray()[0])) return mt;
            if (mt.StartsWith("(") || mt.StartsWith("#$")) return mt;
            return "#$" + mt;
        }

        #endregion


        public void WriteConfig()
        {
            Console.WriteLine("Cyc loaded");
        }
    }
}
