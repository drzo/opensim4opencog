#define MERGED_RDFSTORE
using System;
using System.Collections.Generic;
using System.Collections;
using System.Data;
using System.Diagnostics;
using System.Linq;
using System.Text;
using System.Text.RegularExpressions;
using System.IO;
using System.Reflection;
using Mono.CSharp;
using MushDLR223.Utilities;
using VDS.RDF;
using VDS.RDF.Parsing;
using VDS.RDF.Query;
using VDS.RDF.Writing.Formatting;
using VDS.RDF.Writing;
using VDS.RDF.Nodes;
using StringWriter = System.IO.StringWriter;
//using TermList = LogicalParticleFilter1.TermListImpl;
//using TermList = System.Collections.Generic.List<LogicalParticleFilter1.SIProlog.Part>;///LogicalParticleFilter1.SIProlog.PartListImpl;
//using PartList = System.Collections.Generic.List<LogicalParticleFilter1.SIProlog.Part>;///LogicalParticleFilter1.SIProlog.PartListImpl;

using TermList = LogicalParticleFilter1.SIProlog.PartListImpl;
using PartList = LogicalParticleFilter1.SIProlog.PartListImpl;
#if MERGED_RDFSTORE
using GraphWithDef = LogicalParticleFilter1.SIProlog.PNode;
#endif

using System.Threading;
//using GraphWithDef = LogicalParticleFilter1.SIProlog.;
//using ProveResult = LogicalParticleFilter1.SIProlog.PEnv;
namespace LogicalParticleFilter1
{
    public partial class SIProlog
    {
        // SIProlog : Simple Interpreted Prolog
        // or CiproLog : Contextually Interperted Prolog
        // ported by: Kino High Coursey
        // Date: 2012-08-12
        // license: BSD
        // inspired by:Jan Grant's  http://ioctl.org/logic/prolog-latest who was working at: University of Bristol
        // extended to include a graph of named kb's inspired by the Cyc microtheories system
        //   queries can be made relative to a node in the knowledge visibility graph

        // Other uses of the .js interperter
        // http://www.w3.org/1999/11/11-WWWProposal/rdfqdemo.html
        // http://ioctl.org/rdf/jsrdf/
        // https://github.com/abresas/prologjs
        // https://github.com/crcx/chrome_prolog

        public static int RdfDeveloperSanityChecks = 0;
        public bool DontRDFSync = false;
        //public QueryContext test; 
        /// <summary>
        ///  A plain old super simple prolog interpreter
        /// </summary>
        public string testruleset;
        public string testquery;
        public bool show = false; //true;
        public bool trace = false;
        // [ThreadStatic]
        public int maxdepth = 20;
        public int deepest = 1;
        public string deepestName = FUNCTOR_NIL;
        public int maxMtSize = 64000;
        public const string FUNCTOR_CONS = "cons";//const
        public const string FUNCTOR_NIL = "nil";// "nil";
        private static Term _TERM_TRUE = null;
        public static string CogbotServerWithPort
        {
            get { return GlobalSharedSettings.CogbotServerWithPort; }
        }

        public delegate void chemSysDelegate(string cmd);
        public chemSysDelegate chemSysCommandProcessor = null;

        public bool lazyTranslate = false; // translate KB text to internal on entry or on first use
        private static ulong _CONSP = 0;
        public static ulong CONSP
        {
            get
            {
                return ++_CONSP;
            }
        }

        public static PGraph GlobalKBGraph = new PGraph();
        public PGraph KBGraph = GlobalKBGraph ?? new PGraph();
        public readonly PDB testdb = new PDB(false);
        //was unused :  private Dictionary<string, string> bindingsDict = new Dictionary<string, string>();

        // natural language to MT name
        public Dictionary<string, string> aliasMap = new Dictionary<string, string>();
        public static SIProlog CurrentProlog
        {
            get
            {
                lock (GlobalKBGraph)
                {
                    if (_currentProlog == null)
                    {
                        _currentProlog = new SIProlog();
                    }
                }
                return _currentProlog;

                
            }
            set
            {
                if (_currentProlog != null && _currentProlog != value)
                {
                    Warn("Setting another current Prolog " + _currentProlog.CreationTrace);
                }
                _currentProlog = value;
            }
        }
        public static SIProlog _currentProlog;
        public readonly string CreationTrace;
        private SIProlog()
        {
            var rs = new StringWriter();
            var fs = new System.Diagnostics.StackTrace(true).GetFrames();
            foreach (StackFrame frame in fs)
            {
                rs.WriteLine("" + frame);
            }
            this.CreationTrace = rs.ToString();
            lock (GlobalKBGraph)
            {
                SIPrologInit();
            }
        }
        public void SIPrologInit()
        {
            CurrentProlog = this;
            DLRConsole.TransparentCallers.Add(GetType());
            DLRConsole.SetIgnoreSender("KEYVALUELISTSIPROLOG", true);
            defineBuiltIns();
            if (rdfDefSync == null) defineRDFExtensions();
            threadLocal.tl_mt = "baseKB";
            connectMT("stdlib", "root");
            insertKB(standardLib(), "stdlib");
        }

        public static string MakeReadbleString(string value)
        {
            var value2 = value.Replace("\\", "\\\\").Replace("\"", "\\\"");
            if (value2 == value)
            {
                if (value.ToLower() == value)
                {
                    // return value;
                }
            }
            return "\"" + value2 + "\"";
        }

        public static string MakeReadbleAtom(string value)
        {
            var value2 = value.Replace("\\", "\\\\").Replace("'", "\\'");
            if (value2 == value)
            {
                if (value.ToLower() == value)
                {
                    return value;
                }
            }
            return "'" + value2 + "'";
        }
        public List<string> GatherMts(string mt)
        {
            if (string.IsNullOrEmpty(mt)) return null;
            var gatherNames = new List<string>();
            if (mt.Contains(","))
            {
                foreach (var name in mt.Split(',', ' '))
                {
                    if (string.IsNullOrEmpty(name)) continue;
                    gatherNames.Add(name);
                }
                return gatherNames;
            }
            if (mt == "*")
            {
                foreach (PNode p in KBGraph.SortedTopLevelNodes)
                {
                    gatherNames.Add(p.id);
                }
                return gatherNames;
            }
            return null;
        }


        internal bool HasKBNamed(string startMt)
        {
            lock (KBGraph) return FindKB(startMt) != null;
        }

        internal static string NameCheck(string name)
        {
            if (name == "cons")
            {
                return "cons";
                throw ErrorBadOp(name);
            }
            if (name == "nil")
            {
                return "nil";
                throw ErrorBadOp(name);
            }
            return name;
        }
        static internal Exception ErrorBadOp(string f, params object[] args)
        {
            string m = string.Format(f, args);
            Warn("ERRORBADOP " + m);
            return new NotImplementedException(m);
        }

        #region babyMT
        //===================================================
        // The baby MT system. A directed graph of KB fragments
        // Should make them PDB's but can collect rulesets

        public void connectMT(string childMT, string parentMT)
        {
            //Console.WriteLine("MTSYS: connectMT({0},{1})", childMT, parentMT);
            KBGraph.Connect(childMT, parentMT);
        }
        public void disconnectMT(string childMT, string parentMT)
        {
            KBGraph.Disconnect(childMT, parentMT);
        }
        public void clearConnectionsToMt(string childMt)
        {
            KBGraph.ClearInConnections(childMt);
        }
        public void clearConnectionsFromMt(string childMt)
        {
            KBGraph.ClearOutConnections(childMt);
        }

        public string visibleKBText(string startMT)
        {
            PNode focus = FindKB(startMT);
            if (focus == null) return null;
            string VKB = focus.GetKBText;
            foreach (PEdge E in focus.OutgoingEdges)
            {
                string parentMT = E.EndNode.Id;
                string collectedKB = visibleKBText(parentMT);
                VKB = VKB + "\n" + collectedKB;
            }
            return VKB;
        }

        public RuleList findVisibleKBRulesSorted(string startMT)
        {
            // Returns rules sorted by MT probability
            // Skips those with zero probibility
            var vlist = findVisibleKBS(startMT, new List<PNode>());
            RuleList VKB = new RuleList();
            if (vlist == null)
            {
                Warn("No found visible KBs from " + startMT);
                return VKB;
            }
            //this reverse is because sort() will reverse equal member
            vlist.Reverse();
            vlist.Sort();
            foreach (PNode focus in vlist)
            {
                if (focus.probability > 0)
                {
                    ensureCompiled(focus, ContentBackingStore.Prolog);
                }

            }
            foreach (PNode focus in vlist)
            {
                lock (focus.pdb.rules)
                {
                    if (focus.probability > 0)
                    {
                        foreach (Rule r in focus.pdb.rules)
                        {
                            VKB.Add(r);
                        }
                    }
                }

            }
            return VKB;
        }

        public List<PNode> findVisibleKBS(string startMT, List<PNode> vlist)
        {
            PNode focus = FindKB(startMT);
            if (focus == null) return null;
            if (vlist.Contains(focus))
            {
                //Warn("Already contains KB named " + startMT);
                return null;
            }
            vlist.Add(focus);
            foreach (PEdge E in focus.OutgoingEdges)
            {
                string parentMT = E.EndNode.Id;
                findVisibleKBS(parentMT, vlist);
            }
            return vlist;
        }

        public RuleList findVisibleKBRules(string startMT)
        {
            return findVisibleKBRules(startMT, new List<string>(), true);
        }

        public RuleList findVisibleKBRules(string startMT, List<string> vlist, bool followGenlMt)
        {
            if (vlist.Contains(startMT))
            {
                // Warn("Already contains KB named " + startMT);
                return null;
            }
            vlist.Add(startMT);
            // Returns the preprocessed list of visible KB entries.
            // We should probably de-duplicate if possible
            // Depth-first or breath-first ?
            // Prefix or postfix ??
            // should have one that takes a KB shopping list
            PNode focus = FindKB(startMT);
            if (focus == null)
            {
                Warn("No KB named " + startMT);
                return null;
            }
            ensureCompiled(focus, ContentBackingStore.Prolog);

            RuleList VKB = new RuleList();
            // Prefix
            lock (focus.CompileLock) lock (focus.pdb.rules) foreach (Rule r in focus.pdb.rules)
                    {
                        VKB.Add(r);
                        r.optHomeMt = startMT;
                    }
            if (!followGenlMt) return VKB;
            foreach (PEdge E in focus.OutgoingEdges)
            {
                string parentMT = E.EndNode.Id;
                var collectedKB = findVisibleKBRules(parentMT, vlist, true);
                if (collectedKB != null)
                {
                    foreach (Rule r in collectedKB)
                    {
                        VKB.Add(r);
                    }
                }
            }
            //Postfix
            //foreach (Rule r in focus.pdb.rules)
            //{
            //    VKB.Add(r);
            //}
            return VKB;
        }


        public RuleList collectKBRules(IEnumerable<string> kbList)
        {
            RuleList VKB = new RuleList();
            foreach (string focusMT in kbList)
            {
                PNode focus = FindKB(focusMT);
                if (focus == null) continue;
                ensureCompiled(focus, ContentBackingStore.Prolog);
                lock (focus.CompileLock) lock (focus.pdb.rules) foreach (Rule r in focus.pdb.rules)
                        {
                            VKB.Add(r);
                        }

            }
            return VKB;
        }

        public void setMtProbability(string focusMT, double prob)
        {
            PNode focus = FindKB(focusMT);
            if (focus == null) return;
            focus.Probability = prob;
        }

        public double getMtProbability(string focusMT)
        {
            PNode focus = FindKB(focusMT);
            if (focus == null) return 0;
            return focus.Probability;
        }

        public void markKBScratchpad(string focusMT)
        {
            PNode focus = FindKB(focusMT);
            if (focus == null) return;
            focus.SourceKind = ContentBackingStore.Prolog;
            focus.SyncFrequency = FrequencyOfSync.Never;
        }
        public void markKBNonScratchPad(string focusMT)
        {
            PNode focus = FindKB(focusMT);
            if (focus == null) return;
            focus.SyncFrequency = FrequencyOfSync.AsNeeded;
        }
        public void markKBSyncType(string focusMT, ContentBackingStore syncType)
        {
            PNode focus = FindKB(focusMT);
            if (focus == null) return;
            focus.SyncFromNow = syncType;
        }

        public void clearKB(string focusMT)
        {
            PNode focus = FindKB(focusMT);
            if (focus == null) return;
            focus.Clear();
        }

        internal void ensureCompiled(PNode focus, ContentBackingStore forType)
        {
            if (DLRConsole.IsOnMonoUnix)
            {
                //    focus.SyncFromNow = ContentBackingStore.None;
                if (forType != ContentBackingStore.Prolog)
                {
                    if (focus.SyncFrequency == FrequencyOfSync.Never) return;
                }
                return; // KHC: in realbot no rdf to sync for now
            }
            lock (focus.CompileLock)
            {
                while (true)
                {
                    if (!focus.IsOutOfSyncFor(forType)) return;
                    var prev = focus.SyncFromNow;
                    syncStep(focus);
                    if (prev == focus.SyncFromNow)
                    {
                        Warn("Synced on " + focus + " still the same: " + prev);
                        break;
                    }
                }
                if (focus.IsOutOfSyncFor(forType))
                {
                    Warn("Cant ready KB " + focus + " for use by " + forType);
                }
            }
        }
        internal void syncStep(PNode focus)
        {
            if (DLRConsole.IsOnMonoUnix)
            {
                if (focus.SourceKind != ContentBackingStore.Prolog)
                {
                    return; // KHC: in realbot no rdf to sync for now
                }
            }

            while (true)
            {
                if (focus.SyncFromNow == ContentBackingStore.None) return;
                if (focus.SyncFrequency == FrequencyOfSync.Never) return;
                if (focus.SyncFromNow == ContentBackingStore.RdfServerURI)
                {
                    focus.SyncFromNow = ContentBackingStore.None;
                    focus.populateRDFMemoryFromRepository();
                    focus.SyncFromNow = ContentBackingStore.RdfMemory;
                    // go to next line
                }
                if (focus.SyncFromNow == ContentBackingStore.RdfMemory)
                {
                    focus.SyncFromNow = ContentBackingStore.None;
                    focus.pushRdfGraphToPrologKB(true);
                    return;
                }
                if (focus.SyncFromNow == ContentBackingStore.Prolog)
                {
                    focus.SyncFromNow = ContentBackingStore.None;
                    focus.pushPrologKBToRdfGraph(true);
                    return;
                }
                return;
            }
        }

        public string retractKB(string fact, string focusMT)
        {
            return replaceInKB(fact, "", focusMT);
        }
        public Rule retractKB(Rule fact, PNode focus)
        {
            return replaceInKB(fact, null, focus);
        }
        public string replaceInKB(string oldFact, string newFact, string focusMT)
        {
            Warn("String using public Rule replaceInKB(Rule oldFact, Rule newFact, PNode focus)");
            PNode focus = FindKB(focusMT);
            if (focus == null) return null;
            lock (focus.CompileLock)
            {
                return replaceInKB_unlocked(oldFact, newFact, focus);
            }
        }
        private string replaceInKB_unlocked(string oldFact, string newFact, PNode focus)
        {
            ensureCompiled(focus, ContentBackingStore.Prolog);
            oldFact = oldFact.Replace(", ", ",");
            oldFact = oldFact.Replace("\n", "");
            string focusMT = focus.id;
            var rules = focus.pdb.rules;
            lock (rules) for (int i = 0; i < rules.Count; i++)
                {
                    Rule r = (Rule)rules[i];
                    string val = r.ToString();
                    if (val.Replace(", ", ",").StartsWith(oldFact))
                    {
                        // we null out ruleset so that the accessor knows all rules are in the PDB
                        if (String.IsNullOrEmpty(newFact))
                        {
                            focus.pdb.index.Clear();
                            rules.RemoveAt(i);
                            focus.SyncFromNow = ContentBackingStore.Prolog;
                            return val;
                        }
                        var or = ParseRule(new Tokeniser(newFact), focusMT);
                        or.optHomeMt = focusMT;
                        rules[i] = or;
                        focus.SyncFromNow = ContentBackingStore.Prolog;
                        return val;
                    }
                }
            return null;
        }
        public Rule replaceInKB(Rule oldFact, Rule newFact, PNode focus)
        {
            if (focus == null) return null;

            lock (focus.CompileLock)
            {
                ensureCompiled(focus, ContentBackingStore.Prolog);
                var rules = focus.pdb.rules;
                lock (rules)
                    for (int i = 0; i < rules.Count; i++)
                    {
                        Rule r = (Rule)rules[i];
                        string val = r.ToString();
                        if (r.SameClause(oldFact))
                        {
                            focus.SyncFromNow = ContentBackingStore.Prolog;
                            focus.pdb.index.Clear();
                            if (newFact == null)
                            {
                                rules.RemoveAt(i);
                            }
                            else
                            {
                                rules[i] = newFact;
                            }
                            return r;
                        }
                    }
            }
            return null;
        }

        /// <summary>
        /// Replaces KB with a fresh rule set
        /// </summary>
        /// <param name="ruleSet"></param>
        /// <param name="startMT"></param>
        public void insertKB(string ruleSet, string startMT)
        {
            PNode focus = FindOrCreateKB(startMT);
            insertKB(ruleSet, focus);
        }
        public void insertKB(string ruleSet, PNode focus)
        {
            //replaces KB with a fresh rule set
            lock (focus.CompileLock) loadIntoKB(ruleSet, focus, true);
        }

        /// <summary>
        /// Appends KB with rule set
        /// </summary>
        /// <param name="ruleSet"></param>
        /// <param name="focus"></param>
        ///
        public void appendKB(RuleList ruleSet, PNode focus)
        {
            lock (focus.CompileLock)
            {
                loadIntoKB(ruleSet, focus, false);
            }
        }
        public void appendKB(string ruleSet, string startMT)
        {
            // Adds a string rule set
            loadIntoKB(ruleSet, startMT, false);
        }

        public void loadIntoKB(string ruleSet, PNode focus, bool clearFirst)
        {
            if (clearFirst) focus.Clear();
            loadIntoKB(ruleSet, focus.Id, clearFirst);
        }
        public void loadIntoKB(string ruleSet, String startMT, bool clearFirst)
        {
            if (ruleSet != null && ruleSet.Trim() == "") return;
            {
                Dictionary<string, RuleList> tempKB = ParseKEText(startMT, ruleSet);
                foreach (string kb in tempKB.Keys)
                {
                    var subKB = FindOrCreateKB(kb);
                    loadIntoKB(tempKB[kb], subKB, clearFirst);
                }
            }
        }
        public void loadIntoKB(RuleList ruleSet, PNode focus, bool clearFirst)
        {
            lock (focus.CompileLock)
            {
                loadIntoKB_unlocked(ruleSet, focus, clearFirst);
                if (RdfDeveloperSanityChecks > 0)
                {
                    ensureCompiled(focus, focus.SourceKind);
                    if (RdfDeveloperSanityChecks > 3)
                    {
                        ensureCompiled(focus, ContentBackingStore.Prolog);
                        ensureCompiled(focus, ContentBackingStore.RdfMemory);
                    }
                }
            }
        }

        public void loadIntoKB_unlocked(RuleList ruleSet, PNode focus, bool clearFirst)
        {
            focus.AddRules(ruleSet, clearFirst);
        }

        public PNode FindOrCreateKB(string startMT)
        {
            return FindOrCreateKB(startMT, true);
        }
        public PNode FindOrCreateKB(string startMT, bool addDefaultGenlMts)
        {
            lock (KBGraph)
            {
                return FindOrCreateKB_unlocked(startMT, addDefaultGenlMts);
            }
        }

        public PNode FindKB(string mt)
        {
            return KBGraph.Contains(mt);
        }

        /// <summary>
        /// loads a file (clear and overwrite)
        /// </summary>
        /// <param name="filename"></param>
        /// <param name="startMT"></param>
        public void loadKB(string filename, string startMT)
        {
            //loads a file (clear and overwrite)
            loadKEText(startMT, FromStream(filename), true);
        }

        private static string FromStream(string filename)
        {
            var fi = new FileInfo(filename);
            if (!File.Exists(filename))
            {
                var newfilename = "aiml/shared_ke/" + filename;
                if (File.Exists(newfilename))
                {
                    filename = newfilename;
                }
            }

            if (File.Exists(filename))
            {
                filename = new FileInfo(filename).FullName;
            }

            StreamReader streamReader = new StreamReader(filename, true);
            string ruleSet = streamReader.ReadToEnd();
            streamReader.Close();
            return ruleSet;
        }

        public string atomize(string atomName)
        {
            if (atomName.Length > 1)
            {
                atomName = atomName.Trim('.', ' ');
                char c0 = atomName[0];
                if (char.IsLetter(c0) && char.IsUpper(c0)) return "'" + atomName + "'";
            }
            return atomName;
        }


        /// <summary>
        /// Appends KB with a file
        /// </summary>
        /// <param name="filename"></param>
        public void loadKEFile(string filename)
        {
            loadKEFile(null, filename);
        }
        /// <summary>
        /// Appends KB with a file
        /// </summary>
        /// <param name="startMT"></param>
        /// <param name="filename"></param>
        public void loadKEFile(string startMT, string filename)
        {
            // Defines a simple KEText like format for the prolog
            // you can say "mt:microtheory" to route the following lines into the MT
            // "genlmt:parentmt" will make a graph connection
            // the rest is to be determined
            string source = FromStream(filename);

            if (source != null)
            {
                loadKEText(startMT, source);
            }
            else
            {
                Warn("File not found {0}", filename);
            }
        }


        /// <summary>
        /// Replaces KBs (use loadKEText(string startMT, string ruleSet, false)) intead
        /// </summary>
        /// <param name="startMT"></param>
        /// <param name="ruleSet"></param>
        public void loadKEText(string startMT, string ruleSet)
        {
            loadKEText(startMT, ruleSet, true);
        }
        public void loadKEText(string startMT, string ruleSet, bool clearFirst)
        {
            if (ruleSet != null)
            {
                var restoreThreadKB = threadLocal.curKB;
                Dictionary<string, RuleList> tempKB = ParseKEText(startMT, ruleSet);
                foreach (string kb in tempKB.Keys)
                {
                    RuleList newRules = tempKB[kb];
                    var focus = FindOrCreateKB(kb);
                    int oldCount = focus.Size;
                    int addCount = newRules.Count;
                    ConsoleWriteLine("{0}APPENDING {1} Rules INTO {2}",
                                     (clearFirst ? ("REMOVING " + oldCount + " and ") : ""),
                                     addCount, focus);
                    loadIntoKB(newRules, focus, clearFirst);
//                    int newCount = focus.Size;
                }
                threadLocal.curKB = restoreThreadKB;
            }
        }


        public string findBestAliasMt(string hint)
        {
            int bestDist = hint.Length * 100;
            string bestGuess = hint;
            foreach (string k in aliasMap.Keys)
            {
                int LD = LevenshteinDistance.Compute(k.ToLower(), hint.ToLower());
                if (LD < bestDist)
                {
                    bestDist = LD;
                    bestGuess = k;
                }
            }
            return bestGuess;
        }

        #endregion

        #region interpreterInterface
        public void parseRuleset()
        {
            inGlobalTest();
            var testKB = "testKB";
            clearKB(testKB);
            var outr = parseRuleset(testruleset, testKB);
            testdb.rules = outr;
        }

        public void parseQuery()
        {
            inGlobalTest();
            PartList qlist = ParseBody(testquery, null);
            if (qlist == null)
            {
                Warn("An error occurred parsing the query '{0}.\n", testquery);
                return;
            }
            Body q = new Body(qlist);
            if (show)
            {
                Action<string> w = Console.Write;
                w("Query is: ");
                q.print(w);
                w("\n\n");
            }

            var vs = varNames(q.plist);
            var test = MakeQueryContext(null, true, testdb);

            testdb.index.Clear();

            // Prove the query.
            prove(
                renameVariables(q.plist, 0, null),
                new PEnv(),
                test,
                1,
                (env) => printContext(vs, env)
                );

        }
        public void defineBuiltIns()
        {
            lock (PDB.builtin) defineBuiltIns0();
        }

        public void defineBuiltIns0()
        {
            if (PDB.builtin.Count > 0) return;
            PDB.builtin["compare/3"] = new builtinDelegate(Comparitor);
            PDB.builtin["dcompare/3"] = new builtinDelegate(DoubleComparitor);
            PDB.builtin["cut/0"] = new builtinDelegate(Cut);
            PDB.builtin["call/1"] = new builtinDelegate(Call);
            PDB.builtin["fail/0"] = new builtinDelegate(Fail);
            PDB.builtin["bagof/3"] = new builtinDelegate(BagOf);
            PDB.builtin["external/3"] = new builtinDelegate(External);
            PDB.builtin["external2/3"] = new builtinDelegate(ExternalAndParse);
            PDB.builtin["unify/2"] = new builtinDelegate(UnifyExt);
        }

        public string standardLib()
        {
            string slib = "";
            slib += "unify(X,X).\n";
            slib += "conjunction([]).\n";
            slib += "conjunction([X | Rest]) :- call(X), conjunction(Rest).\n";
            slib += "disjunction([X | Rest]) :- call(X).\n";
            slib += "disjunction([X | Rest]) :- disjunction(Rest).\n";
            slib += "add(A, B, C) :- external(\"$1 + $2\", [A, B], C).\n";
            slib += "sub(A, B, C) :- external(\"$1 - $2\", [A, B], C).\n";
            slib += "mul(A, B, C) :- external(\"$1 * $2\", [A, B], C).\n";
            slib += "div(A, B, C) :- external(\"$1 / $2\", [A, B], C).\n";
            slib += "qsort([], []).\n";
            slib += "qsort([X|Rest], Answer) :- partition(X, Rest, [], Before, [], After), qsort(Before, Bsort), qsort(After, Asort), append(Bsort, [X | Asort], Answer).\n";

            slib += "partition(X, [], Before, Before, After, After).\n";
            slib += "partition(X, [Y | Rest], B, [Y | Brest], A, Arest) :- leq(X, Y), partition(X, Rest, B, Brest, A, Arest).\n";
            slib += "partition(X, [Y | Rest], B, Brest, A, [Y | Arest]) :- gtr(X, Y), partition(X, Rest, B, Brest, A, Arest).\n";
            // symbolic or string
            slib += "leq(X, Y) :- compare(X, Y, gt).\n";
            slib += "leq(X, Y) :- compare(X, Y, eq).\n";
            slib += "gtr(X, Y) :- compare(X, Y, lt).\n";
            // numeric
            slib += "dleq(X, Y) :- dcompare(X, Y, gt).\n";
            slib += "dleq(X, Y) :- dcompare(X, Y, eq).\n";
            slib += "dgtr(X, Y) :- dcompare(X, Y, lt).\n";
            slib += "dltr(X, Y) :- dcompare(X, Y, gt).\n";
            slib += "deq(X, Y) :- dcompare(X, Y, eq).\n";

            slib += "append([], Z, Z).\n";
            slib += "append([A|B], Z, [A|ZZ]) :- append(B, Z, ZZ).\n";

            slib += "reverse([], []).\n";
            slib += "reverse([A|B], Z) :- reverse(B, Brev), append(Brev, [A], Z).\n";

            slib += "length([], 0).\n";
            slib += "length([H|T], N) :- length(T, M), add(M, 1, N).\n";
            slib += "not(Term) :- call(Term), !, fail.\n";
            slib += "not(Term).\n";
            slib += "var(X) :- bagof(l, varTest(X), [l, l]).\n";
            slib += "varTest(a).\n";
            slib += "tarTest(b).\n";
            return slib;
        }

        public bool isTrueIn(string inQuery, string queryMT)
        {
            List<Dictionary<string, string>> bindingList = new List<Dictionary<string, string>>();

            Dictionary<string, string> bindingsDict = new Dictionary<string, string>();
            string query = inQuery;
            PartList qlist = ParseBody(query, queryMT);
            if (qlist == null)
            {
                Warn("An error occurred parsing the query '{0}.\n", query);
                return false;
            }
            Body q = new Body(qlist);
            if (show)
            {
                Action<string> w = Console.Write;
                w("Query is: ");
                q.print(w);
                w("\n\n");
            }

            var ctx = MakeQueryContext(queryMT, true, null);
            bool isTrue = false;
            // Prove the query.
            prove(
                renameVariables(q.plist, 0, null),
                    new PEnv(),
                ctx,
                1,
                delegate(PEnv env)
                {
                    isTrue = true;
                }
                );
            return isTrue;
        }
        public void askQuery(string inQuery, string queryMT)
        {
            var query = inQuery;
            var qlist = ParseBody(query, queryMT);
            if (qlist == null)
            {
                Warn("An error occurred parsing the query '{0}.\n", query);
                return;
            }
            Body q = new Body(qlist);
            if (show)
            {
                Action<string> w = Console.Write;
                w("Query is: ");
                q.print(w);
                w("\n\n");
            }

            var vs = varNames(q.plist);
            var ctx = MakeQueryContext(queryMT, true, null);
            var db = ctx.db;

            // Prove the query.
            prove(
                renameVariables(q.plist, 0, null),
                new PEnv(),
                db,
                1,
                (env) => printContext(vs, env)
                );

        }

        private PDB MakeQueryContext(string queryMT, bool followGenlMt, PDB start)
        {
            PDB db = start ?? new PDB(false);
            if (queryMT != null)
            {
                if (db.isStorage)
                {
                    var db2 = new PDB(false);
                    db2.rules = db.rules.Copy();
                    db = db2;
                }
                db.startMt = queryMT;
                db.followedGenlMt = followGenlMt;
                db.rules = findVisibleKBRules(queryMT, new List<string>(), followGenlMt);
                db.index.Clear();
            }
            return db;
        }

        public void askQuery(string query, string queryMT, out List<Dictionary<string, string>> outBindings)
        {
            outBindings = new List<Dictionary<string, string>>();
            PartList qlist = ParseBody(query, queryMT);
            if (qlist == null)
            {
                Warn("An error occurred parsing the query '{0}.\n", query);
                //outBindings = bindingList;
                return;
            }
            askQuery(qlist, queryMT, true, null, outBindings);
        }



        public void askQuery(PartList qlist, string queryMT, bool followGenlMt, List<Dictionary<string, Part>> outBindingParts, List<Dictionary<string, string>> outBindingStrings)
        {
            //var bindingList = new List<Dictionary<string, Part>>();
            //var bindingsDict = new Dictionary<string, Part>();
            //try
            {
                //var query = inQuery;
                Body q = new Body(qlist);
                if (show)
                {
                    Action<string> w = Console.Write;
                    w("Query is: ");
                    q.print(w);
                    w("\n\n");
                }

                var context = varNames(q.plist);
                var ctx = MakeQueryContext(queryMT, followGenlMt, null);
                var db = ctx.db;
                if (db.rules == null)
                {
                    //outBindings = bindingList;
                    return;
                }
                db.index.Clear();

                bool doParts = outBindingParts != null;
                bool doStrings = outBindingStrings != null;
                // Prove the query.
                prove(
                    renameVariables(q.plist, 0, null),
                        new PEnv(),
                    ctx,
                    1,
                    delegate(PEnv env)
                    {
                        if (context.Arity == 0)
                        {
                            //TRUE
                        }
                        else
                        {
                            Dictionary<string, Part> bindDictParts = null;
                            Dictionary<string, string> bindDictStrings = null;
                            if (doParts)
                            {
                                bindDictParts = new Dictionary<string, Part>();
                                outBindingParts.Add(bindDictParts);
                            }
                            if (doStrings)
                            {
                                bindDictStrings = new Dictionary<string, string>();
                                outBindingStrings.Add(bindDictStrings);
                            }

                            for (var i = 0; i < context.Arity; i++)
                            {
                                string k = (((Variable)context.ArgList[i]).vname);
                                //string v = ((Atom)value(new Variable(((Variable)context.alist[i]).name + ".0"), env)).ToString();
                                var part = value(new Variable(((Variable)context.ArgList[i]).vname + ".0"), env);
                                if (doParts)
                                {
                                    bindDictParts[k] = part;
                                }
                                if (doStrings)
                                {
                                    string v = part.ToSource(SourceLanguage.Text);
                                    if (v.Contains("http"))
                                    {
                                        Warn("Returning RDF to external code");
                                    }
                                    bindDictStrings[k] = v;
                                }

                            }
                        }
                    }
                    );
            }
            //catch (Exception e)
            //{
            //}
            //outBindings = bindingList;
        }

        public void printContext(PartList which, PEnv env)
        {
            inGlobalTest();
            printVars(which, env);
        }

        private void inGlobalTest()
        {
            //tl_spy_prolog_reader = true;
            /// throw ErrorBadOp("inGlobalTest");
        }
        #endregion
        #region interfaceUtils

        public static PartList termVarNames(Term t)
        {
            PartList outv = varNames(t.ArgList);
            if (t.headIsVar)
            {
                outv.AddPart(new Variable(t.fvname));
            }
            return outv;
        }
        // Return a list of all variables mentioned in a list of Terms.
        public static PartList varNames(PartList plist)
        {
            PartList outv = new PartList();


            TermList termList = plist.ArgList;
            for (var i = 0; i < plist.Arity; i++)
            {
                Part part = termList[i];
                if (((Part)part) is IAtomic) continue;

                if (((Part)part) is Variable)
                {
                    for (var j = 0; j < outv.Arity; j++)
                        if (((Variable)outv.ArgList[j]).vname == ((Variable)part).vname) goto mainc;
                    //outv.InsertPart(outv.Length, plist.alist[i]);
                    outv.AddPart((Variable)part);
                }
                else if (((Part)part) is Term)
                {
                    PartList o2 = varNames(((Term)part).ArgList);

                    for (var j = 0; j < o2.Arity; j++)
                    {
                        for (var k = 0; k < outv.Arity; k++)
                            if (((Variable)o2.ArgList[j]).vname == ((Variable)outv.ArgList[k]).vname) goto innerc;
                        //outv.InsertPart(outv.Length, o2.alist[j]);
                        outv.AddPart(o2.ArgList[j]);
                    innerc: j = j;
                    }
                    if (((Term)part).headIsVar)
                    {
                        outv.AddPart(new Variable(((Term)part).fvname));
                    }
                }
                else if (((Part)part) is PartList)
                {
                    PartList o2 = varNames(((PartList)part));

                    for (var j = 0; j < o2.Arity; j++)
                    {
                        for (var k = 0; k < outv.Arity; k++)
                            if (((Variable)o2.ArgList[j]).vname == ((Variable)outv.ArgList[k]).vname) goto innerc2;
                        //outv.InsertPart(outv.Length, o2.alist[j]);
                        outv.AddPart(o2.ArgList[j]);
                    innerc2: j = j;
                    }

                }
            mainc: i = i;
            }
            return outv;
        }

        // Go through a list of terms (ie, a Body or Partlist's list) renaming variables
        // by appending 'level' to each variable name.
        // How non-graph-theoretical can this get?!?
        // "parent" points to the subgoal, the expansion of which lead to these terms.
        public T renameVariables<T>(T list0, int level, Term parent) where T : Part
        {
            object list = list0;

            if (list is IAtomic)
            {
                return (T)list;
            }
            else if (list is Variable)
            {
                return (T)(object)new Variable(((Variable)list).vname + "." + level.ToString());
            }
            else if (list is Term)
            {
                //What if the pred is a variable ?
                var term = (Term)list;
                string nextName = term.fname;
                var tpl = term.ArgList;
                bool termheadIsVar = term.headIsVar;
                if (termheadIsVar)
                {
                    nextName = nextName + "." + level.ToString();
                }
                else
                {
                    if (tpl.IsGround)
                    {
                        Term outv0 = new Term(nextName, termheadIsVar, tpl);
                        outv0.parent = parent;
                        return (T)(object)outv0;
                    }
                }
                Term outv = new Term(nextName, termheadIsVar, (PartList)renameVariables<PartList>(tpl, level, parent)) { excludeThis = term.excludeThis };
                outv.parent = parent;
                return (T)(object)outv;
            }

            PartList outl = new PartList();
            PartList inL = (PartList)list;
            for (var i = 0; i < inL.Arity; i++)
            {
                outl.AddPart(renameVariables((Part)inL.ArgList[i], level, parent));
                /*
                        if (list[i] is IAtomic) {
                            out[i] = list[i];
                        } else if (list[i] is Variable) {
                            out[i] = new Variable(list[i].name + "." + level);
                        } else if (list[i] is Term) {
                            (out[i] = new Term(list[i].name, renameVariables(list[i].ArgList, level, parent))).parent = parent;
                        }
                */
            }

            return (T)(object)outl;
        }
        #endregion
        #region prover
        // The meat of this thing... js-tinyProlog.
        // Don't expect built-ins at present. To come:
        //	unification of term heads, cut, fail, call, bagof
        //	(in that order, probably).
        public delegate void reportDelegate(PEnv env);
        public delegate ProveResult builtinDelegate(Term t, PartList goals, PEnv env, PDB db, int level, reportDelegate rp);
        public class ProveResult
        {
            public bool TooDeep;
            public bool Done;
            public bool Failed;
            public PEnv Env;

            public static bool ReturnEarly(ProveResult result)
            {
                // non null means is TooDeep or Failure I suppose?
                return result != null;
            }
        }
        // The main proving engine. Returns: null (keep going), other (drop out)
        public ProveResult prove(PartList goalList, PEnv environment, PDB dbIn, int level, reportDelegate reportFunction)
        {
            //DEBUG: print ("in main prove...\n");
            if (goalList.Arity == 0)
            {
                reportFunction(environment);

                //if (!more) return "done";
                return null;
            }
            if (level > deepest)
            {
                deepest = level;
                deepestName = ((Term)goalList.ArgList[0]).fname;
            }
            if (level >= maxdepth)
            {
                return new ProveResult() { TooDeep = true };
                //return null;
            }

            // Prove the first term in the goallist. We do this by trying to
            // unify that term with the rules in our database. For each
            // matching rule, replace the term with the body of the matching
            // rule, with appropriate substitutions.
            // Then prove the new goallist. (recursive call)

            Term thisTerm = (Term)goalList.First();
            if (trace) { Console.Write("Debug:LEVEL {0} thisterm = ", level); thisTerm.print(Console.Write); Console.Write(" Environment:"); environment.print(Console.Write); Console.Write("\n"); }


            PDB db;
            //PDB db;
            if (thisTerm.fname == "callMt")
            {
                // db.rules = findVisibleKBRules(queryMT);
                string queryMT = ((IAtomic)thisTerm.ArgList[0]).AsString();
                db = MakeQueryContext(queryMT, true, null);
                threadLocal.tl_mt = queryMT;
                thisTerm = thisTerm.ArgList[1] as Term;
            }
            else
            {
                db = dbIn;
            }

            // Do we have a builtin?

            builtinDelegate builtin = (builtinDelegate)PDB.builtin[thisTerm.fname + "/" + thisTerm.Arity];

            //if (trace) { Console.Write("Debug: searching for builtin " + thisTerm.name + "/" + ((PartList)((PartList)thisTerm.partlist).list).length + "\n"); }
            if (builtin != null)
            {
                if (trace) { Console.Write("builtin with name " + thisTerm.fname + " found; calling prove() on it...\n"); }
                // Stick the new body list
                PartList newGoals = new PartList();
                int j;
                for (j = 1; j < goalList.Arity; j++)
                {
                    newGoals.InsertPart(j - 1, goalList.ArgList[j]);
                }
                return builtin(thisTerm, newGoals, environment, db, level + 1, reportFunction);
            }

            bool termIsVar = thisTerm.headIsVar;
            if (db.index.Count == 0)
            {
                db.initIndex();
            }

            ICollection<Rule> localRules;
            if (termIsVar)
            {
                // if its a var then sorry, just do it all ...
                localRules = db.rules.arrayList;
            }
            else
            {
                RuleList fndRL;
                if (db.index.TryGetValue(thisTerm.fname, out fndRL)) localRules = fndRL.arrayList;
                else
                    localRules = new List<Rule>();

                // What to do for those rules that  are vars ???
                // for now just copy them over
                var varRules = db.index["_varpred_"];
                foreach (Rule r in varRules)
                {
                    localRules.Add(r);
                }
            }

            if (trace) { Console.Write("Debug: in rule selection. thisTerm = "); thisTerm.print(Console.Write); Console.Write("\n"); }
            //for (var i = 0; i < db.rules.Count; i++)
            lock (localRules)
            {
                int i = -1;
                foreach (Rule rule in localRules)
                {
                    threadLocal.tl_rule_mt = rule.optHomeMt ?? threadLocal.tl_rule_mt;
                    i++;
                    if (thisTerm.excludeRule == i)
                    {
                        if (trace)
                        {
                            Console.Write("DEBUG: excluding rule number " + i + " in attempt to satisfy ");
                            thisTerm.print(Console.Write);
                            Console.Write("\n");
                        }
                        continue;
                    }

                    // We'll need better unification to allow the 2nd-order
                    // rule matching ... later.
                    Term rulehead = rule.head;
                    bool ruleIsVar = rulehead.headIsVar;
                    if ((termIsVar == false) && (ruleIsVar == false))
                    {
                        // normal operation, both are atomic
                        if (rulehead.fname != thisTerm.fname) continue;
                    }
                    if ((termIsVar == false) && (ruleIsVar == true))
                    {
                        // TBD:query is atomic, rule is variable
                        //if (rule.head.name != thisTerm.name) continue;
                    }
                    if ((termIsVar == true) && (ruleIsVar == false))
                    {
                        // TBD:bind Query variable pred to atomic rule pred
                        //if (rule.head.name != thisTerm.name) continue;
                    }
                    if ((termIsVar == true) && (ruleIsVar == true))
                    {
                        // TBD:both are variables
                        //if (rule.head.name != thisTerm.name) continue;
                    }

                    if (trace)
                    {
                        Console.Write("Debug: in rule selection[{0} of {1}]. rule = ", i, localRules.Count);
                        rule.print(Console.Write);
                        Console.Write("\n");
                    }

                    // Rename the variables in the head and body
                    Term renamedHead = null;
                    // not ready for the SWI-Prolog "dont copyterm ground facts yet"
                    if (rule.isGround && false)
                    {
                        renamedHead = rulehead;
                    }
                    else
                    {
                        renamedHead = new Term(rulehead.fname, rulehead.headIsVar,
                                               renameVariables(rulehead.ArgList, level, thisTerm));
                    }
                    // renamedHead.ruleNumber = i;
                    if (trace)
                    {
                        Console.Write("DEBUG:  renamedHead = ");
                        renamedHead.print(Console.Write);
                        Console.Write("\n");
                    }

                    var env2 = unify(thisTerm, renamedHead, environment);
                    if (env2 == null)
                    {
                        if (trace)
                        {
                            Console.Write("DEBUG:  unify( thisTerm=");
                            thisTerm.print(Console.Write);
                            Console.Write(", renamedHead = ");
                            renamedHead.print(Console.Write);
                            Console.Write(" in Env:");
                            environment.print(Console.Write);
                            Console.Write(") failed \n");
                        }
                        continue;
                    }

                    var body = rule.body;
                    PartList newGoals;
                    if (body != null)
                    {
                        newGoals = renameVariables(rule.body.plist, level, renamedHead);
                        // newGoals is a new body list which is a unused so we can sue it in our 'prove'
                        foreach (Term p in newGoals)
                        {
                            if (p.excludeThis) p.excludeRule = i;
                        }
                    }
                    else
                    {
                        // Just prove the rest of the goallist, recursively.
                        newGoals = new PartList();
                    }
                    bool skipNext = true;
                    {
                        foreach (Term p in goalList)
                        {
                            if (skipNext)
                            {
                                skipNext = false;
                                continue;
                            }
                            newGoals.AddPart(p);
                        }
                        var ret = prove(newGoals, env2, dbIn, level + 1, reportFunction);
                        if (ProveResult.ReturnEarly(ret))
                            return ret;
                    }

                    if (renamedHead.cut)
                    {
                        if (trace)
                        {
                            Console.Write("Debug: this goal ");
                            thisTerm.print(Console.Write);
                            Console.Write(" has been cut.\n");
                        }
                        break;
                    }
                    if ((thisTerm.parent != null) && (((Term)thisTerm.parent).cut))
                    {
                        if (trace)
                        {
                            Console.Write("Debug: parent goal ");
                            ((Term)thisTerm.parent).print(Console.Write);
                            Console.Write(" has been cut.\n");
                            ;
                        }
                        break;
                    }
                }
            }

            return null;
        }

        // The alternate prover would take a sorted list of mt's to visit
        // Only problem is all the other pieces of code that call it
        // Maybe a class level prover delegate is called for?


        #endregion
        #region builtins
        // A sample builtin function, including all the bits you need to get it to work
        // within the general proving mechanism.

        // compare(First, Second, CmpValue)
        // First, Second must be bound to strings here.
        // CmpValue is bound to -1, 0, 1
        public ProveResult Comparitor(Term thisTerm, PartList goalList, PEnv environment, PDB db, int level, reportDelegate reportFunction)
        {
            //DEBUG print ("in Comparitor.prove()...\n");
            // Prove the builtin bit, then break out and prove
            // the remaining goalList.

            // if we were intending to have a resumable builtin (one that can return
            // multiple bindings) then we'd wrap all of this in a while() loop.

            // Rename the variables in the head and body
            // var renamedHead = new Term(rule.head.name, renameVariables(rule.head.ArgList, level));

            var first = value((Part)thisTerm.ArgList[0], environment) as IAtomic;
            if (first == null)
            {
                //print("Debug: Comparitor needs First bound to an Atom, failing\n");
                return null;
            }

            var second = value((Part)thisTerm.ArgList[1], environment) as IAtomic;
            if (second == null)
            {
                //print("Debug: Comparitor needs Second bound to an Atom, failing\n");
                return null;
            }

            var cmp = "eq";
            int cmpv = first.CompareTo(second);
            if (cmpv < 0) cmp = "lt";
            if (cmpv > 0) cmp = "gt";
            //if (first.name < second.name) cmp = "lt";
            //else if (first.name > second.name) cmp = "gt";

            var env2 = unify((Part)thisTerm.ArgList[2], Atom.FromSource(cmp), environment);

            if (env2 == null)
            {
                //print("Debug: Comparitor cannot unify CmpValue with " + cmp + ", failing\n");
                return null;
            }

            // Just prove the rest of the goallist, recursively.
            return prove(goalList, env2, db, level + 1, reportFunction);
        }
        public ProveResult DoubleComparitor(Term thisTerm, PartList goalList, PEnv environment, PDB db, int level, reportDelegate reportFunction)
        {
            //DEBUG print ("in Comparitor.prove()...\n");
            // Prove the builtin bit, then break out and prove
            // the remaining goalList.

            // if we were intending to have a resumable builtin (one that can return
            // multiple bindings) then we'd wrap all of this in a while() loop.

            // Rename the variables in the head and body
            // var renamedHead = new Term(rule.head.name, renameVariables(rule.head.partlist.list, level));

            var first = value((Part)thisTerm.ArgList.ArgList[0], environment) as IAtomic;
            if (first == null)
            {
                //print("Debug: Comparitor needs First bound to an Atom, failing\n");
                return null;
            }

            var second = value((Part)thisTerm.ArgList.ArgList[1], environment) as IAtomic;
            if (second == null)
            {
                //print("Debug: Comparitor needs Second bound to an Atom, failing\n");
                return null;
            }

            var cmp = "eq";
            double v1 = first.AsDouble();
            double v2 = second.AsDouble();
            int cmpv = v1.CompareTo(v2);
            if (cmpv < 0) cmp = "gt";
            if (cmpv > 0) cmp = "lt";
            //if (first.name < second.name) cmp = "lt";
            //else if (first.name > second.name) cmp = "gt";

            var env2 = unify((Part)thisTerm.ArgList.ArgList[2], Atom.FromSource(cmp), environment);

            if (env2 == null)
            {
                //print("Debug: Comparitor cannot unify CmpValue with " + cmp + ", failing\n");
                return null;
            }

            // Just prove the rest of the goallist, recursively.
            return prove(goalList, env2, db, level + 1, reportFunction);
        }
        public ProveResult Cut(Term thisTerm, PartList goalList, PEnv environment, PDB db, int level, reportDelegate reportFunction)
        {
            //DEBUG print ("in Comparitor.prove()...\n");
            // Prove the builtin bit, then break out and prove
            // the remaining goalList.

            // if we were intending to have a resumable builtin (one that can return
            // multiple bindings) then we'd wrap all of this in a while() loop.

            // Rename the variables in the head and body
            // var renamedHead = new Term(rule.head.name, renameVariables(rule.head.ArgList, level));

            // On the way through, we do nothing...

            // Just prove the rest of the goallist, recursively.
            var ret = prove(goalList, environment, db, level + 1, reportFunction);

            // Backtracking through the 'cut' stops any further attempts to prove this subgoal.
            //print ("Debug: backtracking through cut/0: thisTerm.parent = "); thisTerm.parent.print(); print("\n");
            ((Term)thisTerm.parent).cut = true;

            return ret;
        }

        public ProveResult UnifyExt(Term thisTerm, PartList goalList, PEnv environment, PDB db, int level, reportDelegate reportFunction)
        {
            Part x = value((Part)thisTerm.ArgList[0], environment);
            Part y = value((Part)thisTerm.ArgList[1], environment);
            var res = unify(x, y, environment);
            if (res == null) return null;

            // Backtracking through the 'cut' stops any further attempts to prove this subgoal.
            //print ("Debug: backtracking through cut/0: thisTerm.parent = "); thisTerm.parent.print(); print("\n");
            // thisTerm.parent.cut = true;

            // Just prove the rest of the goallist, recursively.
            return prove(goalList, res, db, level + 1, reportFunction);
        }

        // Given a single argument, it sticks it on the goal list.
        public ProveResult Call(Term thisTerm, PartList goalList, PEnv environment, PDB db, int level, reportDelegate reportFunction)
        {
            // Prove the builtin bit, then break out and prove
            // the remaining goalList.
            //PartList goalList = (PartList)goalIn;

            // Rename the variables in the head and body
            // var renamedHead = new Term(rule.head.name, renameVariables(rule.head.ArgList, level));

            Term first = (Term)value((Part)thisTerm.ArgList[0], environment);
            if (!(first is Term))
            {
                //print("Debug: Call needs parameter bound to a Term, failing\n");
                return null;
            }

            //var newGoal = new Term(first.name, renameVariables(first.ArgList, level, thisTerm));
            //newGoal.parent = thisTerm;

            // Stick this as a new goal on the start of the goallist
            //var newGoals = [];
            //newGoals[0] = first;

            PartList newGoals = new PartList();
            newGoals.InsertPart(0, first);
            first.parent = thisTerm;

            int j;
            for (j = 0; j < goalList.Arity; j++)
            {
                newGoals.InsertPart(j + 1, goalList.ArgList[j]);
            }

            // Just prove the rest of the goallist, recursively.
            return prove(newGoals, environment, db, level + 1, reportFunction);
        }

        public ProveResult Fail(Term thisTerm, PartList goalList, PEnv environment, PDB db, int level, reportDelegate reportFunction)
        {
            return null;
        }

        public ProveResult BagOf(Term thisTerm, PartList goalList, PEnv environment, PDB db, int level, reportDelegate reportFunction)
        {
            // bagof(Term, ConditionTerm, ReturnList)
            //  PartList goalList = (PartList)goalIn;

            Part collect0 = value((Part)thisTerm.ArgList[0], environment);
            Part subgoal = value((Part)thisTerm.ArgList[1], environment);
            Part into = value((Part)thisTerm.ArgList[2], environment);

            Part collect = renameVariables(collect0, level, thisTerm);
            //var newGoal = new Term(subgoal.name, renameVariables(subgoal.ArgList, level, thisTerm));
            Term newGoal = new Term(subgoal.fname, false,
                                    (PartList)renameVariables(((PartList)subgoal), level, thisTerm));
            newGoal.parent = thisTerm;

            //var newGoals = [];
            //newGoals[0] = newGoal;
            PartList newGoals = new PartList();
            newGoals.AddPart(newGoal);

            // Prove this subgoal, collecting up the environments...
            PartList anslist = new PartList();
            anslist.renumber = -1;
            var ret = prove(newGoals, environment, db, level + 1, BagOfCollectFunction(collect, anslist));

            // Turn anslist into a proper list and unify with 'into'

            // optional here: nil anslist -> fail?
            Part answers = Atom.FromSource(FUNCTOR_NIL);

            /*
            print("Debug: anslist = [");
                for (var j = 0; j < anslist.length; j++) {
                    anslist[j].print();
                    print(", ");
                }
            print("]\n");
            */

            for (int i = anslist.Arity; i > 0; i--)
            {
                answers = MakeList(anslist.ArgList[i - 1], answers);
            }

            //print("Debug: unifying "); into.print(); print(" with "); answers.print(); print("\n");
            var env2 = unify(into, answers, environment);

            if (env2 == null)
            {
                //print("Debug: bagof cannot unify anslist with "); into.print(); print(", failing\n");
                return null;
            }

            // Just prove the rest of the goallist, recursively.
            return prove(goalList, env2, db, level + 1, reportFunction);
        }

        // Aux function: return the reportFunction to use with a bagof subgoal
        public reportDelegate BagOfCollectFunction(Part collect, PartList anslist)
        {
            return delegate(PEnv env)
            {
                /*
                print("DEBUG: solution in bagof/3 found...\n");
                print("Value of collection term ");
                collect.print();
                print(" in this environment = ");
                (value(collect, env)).print();
                print("\n");
                printEnv(env);
                */
                // Rename this appropriately and throw it into anslist
                anslist.AddPart(renameVariables(value(collect, env), anslist.renumber--, null));
            };
        }

        // Call out to external javascript
        // external/3 takes three arguments:
        // first: a template string that uses $1, $2, etc. as placeholders for 

        //var EvalContext = [];
        public string ourEval(string expression)
        {
            Mono.CSharp.Evaluator evaluator = GetEvalInstance();
            string result = "" + evaluator.Evaluate(expression);
            //var evaluator = new Evaluator();
            //evaluator.Run("using System;");

            // TODO: Insert evquivelent of javascript Eval(x) here, return result in string form
            return result;
        }

        private static Evaluator _Evaluator = null;
        private static CompilerContext ctx;
        private static CompilerSettings compset;
        private static ReportPrinter rp;

        static public Evaluator GetEvalInstance()
        {
            if (_Evaluator == null)
            {
                if (ctx == null)
                {
                    if (compset == null) compset = new CompilerSettings();
                    if (rp == null)
                    {
                        rp = new StreamReportPrinter(System.Console.Out);
                    }
                    ctx = new CompilerContext(compset, rp);
                }
                _Evaluator = new Evaluator(ctx);
            }
            return _Evaluator;
        }

        public ProveResult External(Term thisTerm, PartList goalList, PEnv environment, PDB db, int level, reportDelegate reportFunction)
        {
            //print ("DEBUG: in External...\n");
            PartList ourParList = GetTermPartList(thisTerm);

            // Get the first term, the template.
            var first = value((Part)ourParList.ArgList[0], environment) as IAtomic;
            if (first == null)
            {
                //print("Debug: External needs First bound to a string Atom, failing\n");
                return null;
            }
            //var r = first.name.match(/^"(.*)"$/);
            //if (! r) return null;
            //r = r[1];

            // AsString gets the unquoted Template string like: $1 + $2
            string r = ((Atom)first).AsString();

            //print("DEBUG: template for External/3 is "+r+"\n");

            // Get the second term, the argument list.
            Part second = (Part)value((Term)ourParList.ArgList[1], environment);
            Part next;
            int i = 1;


            Part argV, nextTerm;
            while (GetCons(second, out argV, out nextTerm))
            {
                var arg = value(argV, environment) as IAtomic;
                if (arg == null)
                {
                    //print("DEBUG: External/3: argument "+i+" must be an Atom, not "); arg.print(); print("\n");
                    return null;
                }
                //var re = new RegExp("\\$"+i, "g");
                //print("DEBUG: External/3: RegExp is "+re+", arg is "+arg.name+"\n");
                //r = r.Replace(re, arg.name);
                string asString = ((Atom)arg).AsString();
                r = Regex.Replace(r, "\\$" + i, asString);

                //print("DEBUG: External/3: r becomes "+r+"\n");

                second = nextTerm;

                i++;
            }

            //if (second.type != "Atom" || second.name != FUNCTOR_NIL) {
            //print("DEBUG: External/3 needs second to be a list, not "); second.print(); print("\n");
            //	return null;
            //}

            //print("DEBUG: External/3 about to eval \""+r+"\"\n");
            //http://odetocode.com/Articles/80.aspx
            //http://www.codeproject.com/Articles/2160/Runtime-C-Expression-Evaluator

            //var ret;
            //with(EvalContext)
            //	ret = eval(r);
            if (!r.Contains(";")) r = r + ";";
            string ret = ourEval(r);

            //print("DEBUG: External/3 got "+ret+" back\n");

            if (ret == null) ret = FUNCTOR_NIL;


            // Convert back into an atom...
            var env2 = unify((Part)ourParList.ArgList[2], Atom.FromSource(ret), environment);

            if (env2 == null)
            {
                //print("Debug: External/3 cannot unify OutValue with " + ret + ", failing\n");
                return null;
            }

            // Just prove the rest of the goallist, recursively.
            return prove(goalList, env2, db, level + 1, reportFunction);
        }

        public static bool GetCons(Part conslist, out Part first1, out Part second1)
        {
            if (!IsList(conslist))
            {
                first1 = null;
                second1 = null;
                // we like lists terminated with []
                if ((conslist is Atom && conslist.fname == FUNCTOR_NIL))
                {
                    return false;
                }
                //DLRConsole.DebugWriteLine("Poorly formed list passed to GetCons " + conslist);
                return false;
            }
            first1 = conslist.ArgList[0];
            second1 = conslist.ArgList[1];
            return true;
        }

        static public PartList GetTermPartList(Term thisTerm)
        {
            TermList tl = thisTerm.ArgList;
            if (tl.Count == 1 && tl[0] is PartList)
            {
                return (PartList)tl[0];
            }
            if (tl[0] is PartList)
            {
                Warn("SStangly constructed term: " + thisTerm);
            }
            return thisTerm.ArgList;
        }

        public ProveResult ExternalAndParse(Term thisTerm, PartList goalList, PEnv environment, PDB db, int level, reportDelegate reportFunction)
        {
            //print ("DEBUG: in External...\n");
            PartList ourParList = GetTermPartList(thisTerm);

            // Get the first term, the template.
            var first = value((Part)ourParList.ArgList[0], environment) as IAtomic;
            if (first == null)
            {
                //print("Debug: External needs First bound to a string Atom, failing\n");
                return null;
            }
            Match rm = Regex.Match(first.AsString(), @"^\""(.*)\""$");
            if (!rm.Success) return null;
            string r = rm.Groups[1].Value;

            //print("DEBUG: template for External/3 is "+r+"\n");

            // Get the second term, the argument list.
            Part second = (Part)value((Term)ourParList.ArgList[1], environment);
            Part next;
            int i = 1;
            while (second is Term && IsListName(((Term)second).fname))
            {
                next = (PartList)((Term)second).ArgList[0];
                next = (Part)((PartList)next).ArgList[0];
                next = (Part)((PartList)next).ArgList[0];

                Part argV = null;
                Part nextTerm = null;
                if (next is PartList)
                {
                    argV = (Part)((PartList)next).ArgList[0];
                    nextTerm = (Part)((PartList)next).ArgList[1];
                }
                if (next is Variable)
                {
                    argV = next;
                    nextTerm = next;
                }
                // Go through second an argument at a time...
                //Part arg = value((Part)((Term)second).ArgList[0], environment);
                var arg = value(argV, environment) as IAtomic;
                if (arg == null)
                {
                    //print("DEBUG: External/3: argument "+i+" must be an Atom, not "); arg.print(); print("\n");
                    return null;
                }
                //var re = new RegExp("\\$"+i, "g");
                //print("DEBUG: External/3: RegExp is "+re+", arg is "+arg.name+"\n");
                //r = r.Replace(re, arg.name);
                r = Regex.Replace(r, "\\$" + i, ((IAtomic)arg).AsString());

                //print("DEBUG: External/3: r becomes "+r+"\n");

                second = nextTerm;

                i++;
            }
            //if (second.type != "Atom" || second.name != FUNCTOR_NIL) {
            //print("DEBUG: External/3 needs second to be a list, not "); second.print(); print("\n");
            //	return null;
            //}

            //print("DEBUG: External/3 about to eval \""+r+"\"\n");

            //var ret;
            //with(EvalContext)
            //	ret = eval(r);
            string ret = ourEval(r);
            //print("DEBUG: External/3 got "+ret+" back\n");

            if (ret == null) ret = FUNCTOR_NIL;


            // Convert back into a Prolog term by calling the appropriate Parse routine...
            Part retPart = ParsePart(new Tokeniser(ret));
            //print("DEBUG: external2, ret = "); ret.print(); print(".\n");

            var env2 = unify((Part)ourParList.ArgList[2], retPart, environment);

            if (env2 == null)
            {
                //print("Debug: External/3 cannot unify OutValue with " + ret + ", failing\n");
                return null;
            }

            // Just prove the rest of the goallist, recursively.
            return prove(goalList, env2, db, level + 1, reportFunction);
        }
        #endregion
        public string describeObject(string objname, object o)
        {
            string result = "";
            Type type = o.GetType();
            PropertyInfo[] properties = type.GetProperties();
            foreach (PropertyInfo pi in properties)
            {
                if (pi.CanRead)
                {
                    if (
                           (pi.PropertyType == typeof(string))
                        || (pi.PropertyType == typeof(string))
                        || (pi.PropertyType == typeof(float))
                        || (pi.PropertyType == typeof(double))
                        || (pi.PropertyType == typeof(int))
                        || (pi.PropertyType == typeof(bool))
                        || (pi.PropertyType == typeof(Int16))
                        || (pi.PropertyType == typeof(Int32))
                        || (pi.PropertyType == typeof(Int64))
                        )
                    {
                        object propertyValue = pi.GetValue(o, null);
                        string pname = pi.Name.ToLower();
                        string pval = propertyValue.ToString().Trim();
                        string frag = "";
                        if (pval.Contains(" "))
                        {
                            frag = String.Format("triple({0},{1},\"{2}\").\n", objname, pname, pval);

                        }
                        else
                        {
                            frag = String.Format("triple({0},{1},{2}).\n", objname, pname, pval);
                        }

                        result += frag;
                    }
                }
            }
            return result;
        }


        #region auxUtils

        // Some auxiliary bits and pieces... environment-related.

        // Print out an environment's contents.
        public void printEnv(PEnv env)
        {
            Action<string> w = Console.Write;
            if (env == null)
            {
                w("null\n");
                return;
            }
            var k = false;
            //foreach (var i in env)
            foreach (string i in env.Keys)
            {
                k = true;
                w(" " + i + " = ");
                ((Part)env[i]).print(w);
                w("\n");
            }
            if (!k) w("true\n");
        }

        public void printVars(PartList which, PEnv environment)
        {
            Action<string> w = Console.Write;
            // Print bindings.
            if (which.Arity == 0)
            {
                w("true\n");
            }
            else
            {
                for (var i = 0; i < which.Arity; i++)
                {
                    w(((Variable)which.ArgList[i]).vname);
                    w(" = ");
                    //((Atom)value(new Variable(((Variable)which.alist[i]).name + ".0"), environment)).print();
                    value(new Variable(which.ArgList[i].vname + ".0"), environment).print(w);
                    w("\n");
                }
            }
            w("\n");
        }

        // The value of x in a given environment
        static public Part value(Part x, PEnv env)
        {
            if (x is Term)
            {
                PartList p = new PartList(x.ArgList, env);
                return new Term(x.fname, ((Term)x).headIsVar, p);
            }
            if (x is PartList)
            {
                PartList p = new PartList(x.ArgList, env);
                return p;
            }
            if (!(x is Variable))
                return x;		// We only need to check the values of variables...

            Part binding; //** HASH/DICTIONARY NEEDED ** 
            if (!env.TryGetValue(x.vname, out binding))
                return x;
            if (binding == null)
                return x;		// Just the variable, no binding.
            return value(binding, env);
        }

        // Give a new environment from the old with "n" (a string variable name) bound to "z" (a part)
        // Part is IAtomic|Term|Variable
        public PEnv newEnv(string n, Part z, PEnv e)
        {
            // We assume that n has been 'unwound' or 'followed' as far as possible
            // in the environment. If this is not the case, we could get an alias loop.
            var ne = new PEnv();
            ne[n] = z;  //** HASH/DICTIONARY NEEDED ** 
            //for (var i in e)
            //	if (i != n) ne[i] = e[i];
            foreach (string i in e.Keys)
            {
                if (i != n) ne[i] = e[i];
            }

            return ne;
        }

        // More substantial utility functions.

        // Unify two terms in the current environment. Returns a new environment.
        // On failure, returns null.
        /*
        public PEnv unify00(Part x, Part y, PEnv env)
        {
            x = value(x, env);
            y = value(y, env);
            if (trace) { ConsoleWrite("     unify X="); x.print(); ConsoleWrite("  Y="); y.print(); ConsoleWriteLine(); }

            if (x is Variable)
            {
                if (trace) ConsoleWriteLine("     MATCH");
                return newEnv(x.name, y, env);
            }
            if (y is Variable)
            {
                if (trace) ConsoleWriteLine("     MATCH");
                return newEnv(((Variable)y).name, x, env);
             }
            if (x is IAtomic || y is IAtomic)
                if (x.type == y.type && ((Atom)x).name == ((Atom)y).name)
                {
                    if (trace) ConsoleWriteLine("     MATCH");
                    return env;
                }
                else
                    return null;

            // x.type == y.type == Term...
            if (x is Term && y is Term)
            {
                bool xvar = x.headIsVar();
                bool yvar = y.headIsVar();
                if (!xvar && !yvar)
                {
                    if (x.name != y.name)
                        return null;	// Ooh, so first-order.
                }

                if (x.Arity != y.Arity) 
                    return null;

                for (var i = 0; i < x.Arity; i++)
                {
                    env = unify((Part)x.ArgList[i], (Part)y.ArgList[i], env);
                    if (env == null)
                        return null;
                }
                if (!xvar && yvar)
                {
                    if (trace) ConsoleWriteLine("     MATCH");
                    return newEnv(y.name, Atom.Make(x.name), env);
                }
                if (xvar && !yvar)
                {
                    if (trace) ConsoleWriteLine("     MATCH");
                    return newEnv(x.name, Atom.Make(y.name), env);
                }
                if (xvar && yvar)
                {
                    if (trace) ConsoleWriteLine("     MATCH");
                    return newEnv(x.name, new Variable(y.name), env);
                }

            }

            if (x is PartList && y is PartList)
            {

                if (x.name != y.name)
                    return null;	// Ooh, so first-order.
                if (x.Length != y.Length)
                {
                    // TODO: fix list layout. sometimes we get the list with an outer wrapper PartList
                    while (x.Length != y.Length)
                    {
                        if ((x.Length == 1) && x.alist[0] is PartList)
                        {
                            //if (((PartList)x.alist[0]).Length == y.Length)
                           // {
                                x = ((PartList)x.alist[0]);
                           // }
                           // else
                           //     return null;
                        }
                        else
                            return null;
                    }
                    if (trace) { ConsoleWrite("     inner-unify X="); x.print(); ConsoleWrite("  Y="); y.print(); ConsoleWriteLine(); }
                  
                }
                for (var i = 0; i < x.Length; i++)
                {
                    env = unify((Part)x.alist[i], (Part)y.alist[i], env);
                    if (env == null) 
                        return null;
                }
            }
            if (trace) ConsoleWriteLine("     MATCH");
            return env;
        }*/

        public PEnv unify(Part x, Part y, PEnv env)
        {
            TextWriter Console = System.Console.Error;

            // unify will deref nonvars as needed (saves from copyterm semantics)

            if (x is Variable) x = value(x, env);
            if (y is Variable) y = value(y, env);
            if (trace) { Console.Write("     unify X="); x.print(Console.Write); Console.Write("  Y="); y.print(Console.Write); Console.WriteLine(); }

            // both atoms/constants
            if (x is IAtomic && y is IAtomic)
            {
                if (((IAtomic)x).Unify((IAtomic)y))
                {
                    if (trace) Console.WriteLine("     MATCH");
                    return env;
                }
                else
                {
                    return null;
                }

            }
            // both lists, both empty
            if (x is PartList && y is PartList)
            {
                if (x.Arity == 0 && y.Arity == 0)
                {
                    if (trace) ConsoleWriteLine("     MATCH");
                    return env;
                }

            }

            // variables check, should do occurs in check ...
            if (x is Variable)
            {
                if (trace) ConsoleWriteLine("     MATCH");
                return newEnv(x.vname, y, env);
            }
            if (y is Variable)
            {
                if (trace) ConsoleWriteLine("     MATCH");
                return newEnv(((Variable)y).vname, x, env);
            }
            // both lists or terms
            if (x.type != y.type)
            {
                return null;
            }

            if (x is Term)
            {
                if (x.Arity != y.Arity) return null;
                bool xvar = ((Term)x).headIsVar;
                bool yvar = ((Term)y).headIsVar;
                if (!xvar && !yvar)
                {
                    if (x.fname != y.fname)
                        return null; // Ooh, so first-order.
                    return unify(x.ArgList, y.ArgList, env);

                }

                if (xvar && yvar)
                {
                    if (!x.ArgList.IsGround || !y.ArgList.IsGround) return null;
                    PEnv subEnv = unify(new Variable(x.fvname), new Variable(y.fvname), env);
                    if (subEnv == null)
                        return null;
                    return unify(x.ArgList, y.ArgList, subEnv);
                }
                if (xvar)
                {
                    var m = y;
                    y = x;
                    x = m;
                }
                {
                    if (!y.ArgList.IsGround) return null;
                    PEnv subEnv = unify(Atom.FromName(x.fname), new Variable(y.fvname), env);
                    if (subEnv == null)
                        return null;
                    return unify(x.ArgList, y.ArgList, subEnv);
                }
            }
            if (x is PartList)
            {
                while (((x.Arity == 1) && x.ArgList[0] is PartList) && (x.Arity != y.Arity))
                {
                    throw ErrorBadOp("inner partlist");
                    x = ((PartList)x.ArgList[0]);
                }
                while (((y.Arity == 1) && y.ArgList[0] is PartList) && (x.Arity != y.Arity))
                {
                    throw ErrorBadOp("inner partlist");
                    y = ((PartList)y.ArgList[0]);
                }

                if (x.Arity != y.Arity)
                {
                    return null;
                    while (x.Arity != y.Arity)
                    {
                        if (y.Arity < x.Arity)
                        {
                            PartList temp = (PartList)x;
                            x = y;
                            y = temp;

                        }
                        /*if ((x.Length == 1) && x.alist[0] is PartList)
                        {
                            x = ((PartList)x.alist[0]);
                        }
                        else*/
                    }
                    return null;
                }
                if (trace) { Console.Write("     inner-unify X="); x.print(Console.Write); Console.Write("  Y="); y.print(Console.Write); Console.WriteLine(); }
                if (false /*&& (x.name != y.name)*/)
                    return null;	// Ooh, so first-order.

                for (var i = 0; i < x.Arity; i++)
                {
                    env = unify((Part)x.ArgList[i], (Part)y.ArgList[i], env);
                    if (env == null)
                        return null;
                }
            }

            if (trace) Console.WriteLine("     MATCH");
            return env;
        }
        #endregion


    }
}
   

