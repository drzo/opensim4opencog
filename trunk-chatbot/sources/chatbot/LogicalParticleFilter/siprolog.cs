using System;
using System.Collections.Generic;
using System.Collections;
using System.Linq;
using System.Text;
using System.Text.RegularExpressions ;
using System.IO;
using System.Reflection;
using Mono.CSharp;


using VDS.RDF;
using VDS.RDF.Parsing;
using VDS.RDF.Query;
using VDS.RDF.Writing.Formatting;
using VDS.RDF.Writing;




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

        public class QueryContext
        {
            //public string ruleset;
            public string startMT;
            public readonly PDB db;
            //public readonly PartList context = new PartList();

            public QueryContext(string home, PartList list)
            {
                db = new PDB();
                startMT = home;
                //context = list;
            }

            public void InitContext()
            {
                
            }
        }

        //public QueryContext test; 
        /// <summary>
        ///  A plain old super simple prolog interpreter
        /// </summary>
        public string testruleset;
        public string testquery;
        public bool show = false; //true;
        public bool trace = false;
        public int maxdepth = 20;
        public int deepest = 1;
        public string deepestName = FUNCTOR_NIL;
        public int maxMtSize = 64000;
        public const string FUNCTOR_LIST = ".";//const
        public const string FUNCTOR_NIL = "[]";// "nil";
        public delegate void chemSysDelegate(string cmd);
        public chemSysDelegate chemSysCommandProcessor = null;

        public bool lazyTranslate = false; // translate KB text to internal on entry or on first use

        public PGraph KBGraph = new PGraph();
        readonly public PDB testdb = new PDB();
        //was unused :  private Dictionary<string, string> bindingsDict = new Dictionary<string, string>();
        
        // natural language to MT name
        public Dictionary<string, string> aliasMap = new Dictionary<string, string>();


        public SIProlog()
        {
            defineBuiltIns();
            connectMT("stdlib", "root");
            insertKB(standardLib(),"stdlib");
            defineRDFExtensions();
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

        #region babyMT
        //===================================================
        // The baby MT system. A directed graph of KB fragments
        // Should make them PDB's but can collect rulesets

        public void connectMT(string childMT, string parentMT)
        {
            KBGraph.Connect(childMT, parentMT);
        }

        public string visibleKBText(string startMT)
        {
            PNode focus = KBGraph.Contains(startMT);
            if (focus == null) return null;
            string VKB = "";
            foreach (PEdge E in focus.OutgoingEdges)
            {
                string parentMT = E.EndNode.Id;
                string collectedKB = visibleKBText(parentMT);
                VKB = VKB + "\n" + collectedKB;
            }
            return VKB;
        }

        int comparitor(double x, double y)
        {
            return x.CompareTo(y);
        }

        public RuleList findVisibleKBRulesSorted(string startMT)
        {
            // Returns rules sorted by MT probability
            // Skips those with zero probibility
           ArrayList vlist= findVisibleKBS(startMT, new ArrayList());
           RuleList VKB = new RuleList();
           if (vlist == null) vlist = new ArrayList();
           vlist.Sort();
           foreach (PNode focus in vlist)
           {
               if (focus.probability > 0)
               {
                   ensureCompiled(focus);
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

        public ArrayList findVisibleKBS(string startMT, ArrayList vlist)
        {
            PNode focus = KBGraph.Contains(startMT);
            if (focus == null) return null;
            if (vlist.Contains(focus)) return null;
            vlist.Add(focus);
            foreach (PEdge E in focus.OutgoingEdges)
            {
                string parentMT = E.EndNode.Id;
                ArrayList collectedKB = findVisibleKBS(parentMT, vlist);
            }
            return vlist;
        }

        public RuleList findVisibleKBRules(string startMT)
        {
            return findVisibleKBRules(startMT, new ArrayList(), true);
        }

        public RuleList findVisibleKBRules(string startMT, ArrayList vlist, bool followGenlMt)
        {
            if (vlist.Contains(startMT)) return null;
            vlist.Add(startMT);
            // Returns the preprocessed list of visible KB entries.
            // We should probably de-duplicate if possible
            // Depth-first or breath-first ?
            // Prefix or postfix ??
            // should have one that takes a KB shopping list
            PNode focus = KBGraph.Contains(startMT);
            if (focus == null) return null;
                ensureCompiled(focus);

            RuleList VKB = new RuleList();
            // Prefix
            lock (focus.pdb.rules) foreach (Rule r in focus.pdb.rules)
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
        public void webWriter(StreamWriter writer,string action,string query,string mt, string serverRoot)
        {
            serverRoot = "/";
            if (action == "autorefresh")
            {
                writer.WriteLine("<META HTTP-EQUIV=\"REFRESH\" content=\"10\">");
            }
            writer.WriteLine("<html>");
            TOCmenu(writer, serverRoot); 
            webWriter0(writer, action, query, mt, serverRoot, true);
            writer.WriteLine("</html>");

        }

        private void TOCmenu(StreamWriter writer, string serverRoot)
        {
            writer.WriteLine("<a href='{0}siprolog/?q=list'>List Mts</a> ", serverRoot);
            writer.WriteLine("<a href='{0}siprolog/?q=listing'>List All Rules</a> ", serverRoot);
            writer.WriteLine("<a href='{0}/query'>Sparql Query</a><br/>", PFEndpoint.serverRoot);
        }
        public void webWriter0(StreamWriter writer, string action, string queryv, string mt, string serverRoot, bool toplevel)
        {
            try
            {
                if ((action==null) && (queryv==null) && (mt==null))
                {
                    queryv="list";
                }
                

                if (action == null)
                {
                    if (queryv != null)
                    {
                        if (queryv.ToLower() == "list")
                        {
                            writer.WriteLine("<h2>Siprolog Mt List</h2>");
                            foreach (PNode p in KBGraph.SortedTopLevelNodes)
                            {
                                writer.WriteLine(p.ToLink(serverRoot) + "<br/>");
                            }
                            writer.WriteLine("<h2>Siprolog Mt Treed</h2>");
                            KBGraph.PrintToWriterTreeMts(writer, serverRoot);
                            return;
                        }
                        if (queryv.ToLower() == "listing")
                        {
                            List<string> allMts = new List<string>();
                            //writer.WriteLine("<h2>Siprolog Mt List</h2>");
                            foreach (PNode p in KBGraph.SortedTopLevelNodes)
                            {
                                string pname = p.id;
                                allMts.Add(pname);
                                //writer.WriteLine("<a href='{1}siprolog/?mt={0}'>{0}  (prob={2})</a><br/>", pname, serverRoot, p.probability);
                            }
                            writer.WriteLine("<h2>Siprolog Mt Treed</h2>");
                            KBGraph.PrintToWriterTreeMts(writer, serverRoot);
                            foreach (var list in allMts)
                            {
                                writer.WriteLine("<hr/>");
                                webWriter0(writer, action, null, list, null, false);                                
                            }
                            interactFooter(writer, "", serverRoot);
                            return;
                        }
                    }
                    if (mt != null)
                    {
                        WriteMtInfo(writer, mt, serverRoot, toplevel);
                        if (toplevel) interactFooter(writer, mt, serverRoot);
                        return;
                    }
                 }
                else
                {
                    switch (action.ToLower())
                    {
                        case "append":
                            appendKB(queryv, mt);
                            break;
                        case "insert":
                            insertKB(queryv, mt);
                            break;
                        case "query":
                            interactQuery(writer, queryv, mt, serverRoot);
                            break;
                    }
                    webWriter0(writer, null, null, mt, serverRoot, false);
                }
            }
            catch (Exception e)
            {
                writer.WriteLine("<font color='red'>{0} {1} {2}</font>", e.GetType(), e.Message, e.StackTrace);                
                return;
            }


        }


        public void WriteMtInfo(StreamWriter writer, string mt, string serverRoot, bool toplevel)
        {
            writer.WriteLine("<h2>Siprolog Mt {0}</h2>", mt);
            PNode qnode = KBGraph.Contains(mt);
            if (qnode != null)
            {
                writer.WriteLine("<h3> OutgoingEdges </h3>");
                KBGraph.PrintToWriterOutEdges(qnode, 0, writer, serverRoot);
                writer.WriteLine("<h3> IncomingEdges </h3>");
                KBGraph.PrintToWriterInEdges(qnode, 0, writer, serverRoot);
                //KBGraph.ShowGenlMts(qnode, null, 0, writer, serverRoot);
                writer.WriteLine("<h3> KB Operations for {0}</h3>&nbsp;", mt);
                writer.WriteLine("<a href='{0}plot/?mt={1}&q=plot(X,Y)'>Plot Mt</a>&nbsp;", serverRoot, mt);
                writer.WriteLine("<a href='{0}plot/?mt={1}&a=autorefresh&q=plot(X,Y)'>Scope Mt</a> ", serverRoot, mt);
                writer.WriteLine("<a href='{0}siprolog/?mt={1}&a=autorefresh'>Watch Mt</a> ", serverRoot, mt);
                writer.WriteLine("<a href='{0}siprolog/?mt={1}&q=clear'>Clear</a> ", serverRoot, mt);
                writer.WriteLine("<a href='{0}xrdf/?mt={1}&q=pl2rdf'>Prolog2RDF</a> ", serverRoot, mt);
                writer.WriteLine("<a href='{0}xrdf/?mt={1}&q=rdf2pl'>RDF2Prolog</a> ", serverRoot, mt);
                writer.WriteLine("<br/>");
                writer.WriteLine("<h3> KB Contents </h3>");
                if (toplevel) writer.WriteLine("<hr/>");
                ensureCompiled(qnode);
            }
            var kbContents = findVisibleKBRulesSorted(mt);
            foreach (Rule r in kbContents)
            {
                WriteRule(writer, r, qnode);
            }
            var gwf = FindRepositoryKB(mt);
            if (gwf != null)
            {
                var trips = gwf.rdfGraph.Triples;
                writer.WriteLine("<h3> KB Triples {0}</h3>", trips.Count);
                WriteEnumeration(writer, trips);
            } else
            {
                writer.WriteLine("<h3> KB Triples {0}</h3>", "Not synced");                
            }
        }

        private void WriteRule(StreamWriter writer, Rule r, PNode qnode)
        {
            var mt = r.optHomeMt;
            writer.WriteLine("{0}{1}<br/>", r.ToString(), qnode.id == mt ? "" : ("&nbsp;&nbsp;% " + mt));
        }

        public static string StructToString(object t)
        {
            return StructToString(t, 2);
        }

        private static bool HasElements(ICollection props)
        {
            return props != null && props.Count > 0;
        }
        public static string StructToString(object t, int depth)
        {
            if (t == null) return "NULL";
            Type structType = t.GetType();
            if (t is IConvertible || t is String || t is Uri || t is Stream ) return "" + t;
            if (depth < 0) return "^";// +t;
            StringBuilder result = new StringBuilder();
            if (t is ICollection)
            {
                ICollection ic = t as ICollection;
                int max = 10;
                result.Append("CollectionType: " + structType + " Count: " + ic.Count + " " + " Items: [");
                foreach (var i in ic)
                {
                    result.Append(ic.Count + ": " + StructToString(i, depth - 1) + " ");
                    max--;
                    if (max < 1)
                    {
                        result.Append("...");
                        break;

                    }
                }
                result.Append("]");
                return result.ToString().TrimEnd();
            }
            var fpub = BindingFlags.Public | BindingFlags.Instance;
            var fpriv = BindingFlags.NonPublic | BindingFlags.Instance;
            FieldInfo[] fields = structType.GetFields(fpub);
            PropertyInfo[] props = structType.GetProperties(fpub);
            bool hasProps = HasElements(props);
            if (!HasElements(fields) && !hasProps)
            {
                fields = structType.GetFields(fpriv);
            }
            bool needSimpleToString = true;

            if (!HasElements(fields))
            {
                props = hasProps ? props : structType.GetProperties(fpriv);
                foreach (PropertyInfo prop in props)
                {
                    if (prop.GetIndexParameters().Length != 0) continue;
                    needSimpleToString = false;
                    result.Append("{" + prop.Name + ": " + StructToString(prop.GetValue(t, null), depth - 1) + "}");
                }
            }
            if (needSimpleToString)
            {
                foreach (FieldInfo prop in fields)
                {
                    needSimpleToString = false;
                    result.Append("{" + prop.Name + ": " + StructToString(prop.GetValue(t), depth - 1) + "}");
                }
            }

            if (needSimpleToString)
            {
                return "" + t;
            }

            return result.ToString().TrimEnd();
        }

        public void interactQuery(StreamWriter writer, string query, string mt, string serverRoot)
        {
            int testdepth = 64;

            List<Dictionary<string, string>> bingingsList = new List<Dictionary<string, string>>();
            while ((bingingsList.Count == 0) && (testdepth < 1024))
            {
                testdepth = (int)(testdepth * 1.5);
                //Console.WriteLine("Trying depth {0}", testdepth);
                maxdepth = testdepth;
                askQuery(query, mt, out bingingsList);
            }
            writer.WriteLine("<h3>Query:'{0}' in mt={1}</h3><br/>", query, mt);
            if (bingingsList.Count == 0)
            {
                writer.WriteLine("No bindings found at depth {0} in {1}<br/>", testdepth, mt);
            }
            else
            {
                writer.WriteLine("{2} bindings found at depth {0} in {1}<br/>", testdepth,mt, bingingsList.Count);
                int index = 0;
                foreach (Dictionary<string, string> bindings in bingingsList)
                {
                    index++;
                    writer.Write("{0}: ", index);
                    foreach (string k in bindings.Keys)
                    {
                        string v = bindings[k];
                        writer.Write("{0}={1} ",k,v);
                    }
                    writer .WriteLine("<br/>");
                }
                writer.WriteLine("<hr/>");

            }
        }
        public void interactFooter(StreamWriter writer, string mt, string serverRoot)
        {
            writer.WriteLine("<hr/>");

            writer.WriteLine(" <form method='get' ACTION='{1}siprolog/'>", mt, serverRoot);
            writer.WriteLine(" Query: <INPUT TYPE='text' name='q'/>");
            MtSelector(writer, mt);
            writer.WriteLine(" <INPUT TYPE='hidden' name='a' VALUE='query'/>");
            writer.WriteLine(" <INPUT TYPE='submit' VALUE='submit'/>");
            writer.WriteLine(" </FORM>");
            writer.WriteLine(" <form method='get' ACTION='{1}siprolog/'>",mt,serverRoot );
            writer.WriteLine(" Append: <INPUT TYPE='text' name='q'/>");
            MtSelector(writer, mt);
            writer.WriteLine(" <INPUT TYPE='hidden' name='a' VALUE='append'/>");
            writer.WriteLine(" <INPUT TYPE='submit' VALUE='submit'/>");
            writer.WriteLine(" </FORM>");
            writer.WriteLine(" <form method='get' ACTION='{1}siprolog/'>", mt, serverRoot);
            writer.WriteLine(" Overwrite: <INPUT TYPE='text' name='q'/>");
            MtSelector(writer, mt);
            writer.WriteLine(" <INPUT TYPE='hidden' name='a' VALUE='insert'/>");
            writer.WriteLine(" <INPUT TYPE='submit' VALUE='submit'/>");
            writer.WriteLine(" </FORM>");

        }

        private void MtSelector(StreamWriter writer, string mt)
        {
            if (string.IsNullOrEmpty(mt))
            {
                writer.WriteLine(" MT: <INPUT TYPE='text' name='mt' VALUE='{0}'/>", mt);
            }
            else
            {
                writer.WriteLine(" MT: <INPUT TYPE='text' name='mt' VALUE='{0}'/>", mt);
            }
        }

        public ArrayList collectKBRules(ArrayList kbList)
        {
            ArrayList VKB = new ArrayList ();
            foreach (string focusMT in kbList )
            {
                PNode focus = KBGraph.Contains(focusMT);
                if (focus == null)  continue;
                ensureCompiled(focus);
                lock (focus.pdb.rules) foreach (Rule r in focus.pdb.rules)
                {
                    VKB.Add(r);
                }

            }
            return VKB;
        }

        public void setMtProbability(string focusMT, double prob)
        {
            PNode focus = KBGraph.Contains(focusMT);
            if (focus == null) return;
            focus.Probability = prob;
        }

        public double getMtProbability(string focusMT)
        {
            PNode focus = KBGraph.Contains(focusMT);
            if (focus == null) return 0;
            return focus.Probability;
        }

        public void clearKB(string focusMT)
        {
            PNode focus = KBGraph.Contains(focusMT);
            if (focus == null) return;
            focus.ruleset = "";
            focus.pdb.index.Clear();
            lock (focus.pdb.rules) focus.pdb.rules.Clear();
           
            focus.dirty = true;
            ensureCompiled(focus);
        }

        private void ensureCompiled(PNode focus)
        {
            if (focus == null) return;
            if (focus.dirty)
            {
                var outr = parseRuleset(focus.ruleset, focus.Id);
                lock (focus.pdb.rules)
                {
                    focus.pdb.rules = outr;
                    focus.dirty = false;
                }
            }
        }

        public string retractKB(string fact, string focusMT)
        {
            return replaceInKB(fact, "", focusMT);
        }
        public string replaceInKB(string fact, string replace, string focusMT)
        {
            PNode focus = KBGraph.Contains(focusMT);
            if (focus == null) return null;
            ensureCompiled(focus);
            fact = fact.Replace(", ", ",");
            fact = fact.Replace("\n", "");
            focusMT = focus.id;
            var rules = focus.pdb.rules;
            lock (rules) for (int i = 0; i < rules.Count; i++)
            {
                Rule r = (Rule) rules[i];
                string val = r.ToString();
                if (val.Replace(", ", ",").StartsWith(fact))
                {
                    // we null out ruleset so that the accessor knows all rules are in the PDB
                    focus.ruleset = null;
                    focus.pdb.index.Clear();
                    if (String.IsNullOrEmpty(replace))
                    {
                        rules.RemoveAt(i);
                        return val;
                    }
                    var or = ParseRule(new Tokeniser(replace));
                    or.optHomeMt = focusMT;
                    rules[i] = or;
                    return val;
                }
            }
            return null;
        }

        public void insertKB(string ruleSet,string startMT )
        {
            //replaces KB with a fresh rule set
            PNode focus = FindOrCreateKB(startMT);
            focus.ruleset = ruleSet;
            focus.dirty = true;

            if (lazyTranslate) return;
            ensureCompiled(focus);
        }

        public void appendKB(string ruleSet, string startMT)
        {
            // Adds a string rule set
            PNode focus = FindOrCreateKB(startMT);
            startMT = focus.Id;
            if (!focus.dirty)
            {
                if ((focus.ruleset != null) && (focus.ruleset.Length > (maxMtSize*1.2)))
                {
                    focus.ruleset = focus.ruleset + "\n" + ruleSet + "\n";
                    while ((focus.ruleset!=null)&&(focus.ruleset.Length > maxMtSize))
                    {
                        int p1 = focus.ruleset.IndexOf("\n");
                        focus.ruleset = focus.ruleset.Substring(p1 + 1);
                        focus.dirty = true;
                    }
                    if (lazyTranslate) return;
                    ensureCompiled(focus);

                }
                else
                {
                    focus.ruleset = focus.ruleset + "\n" + ruleSet + "\n";
                    focus.pdb.index.Clear();
                    var outr = parseRuleset(ruleSet, startMT);
                    lock (focus.pdb.rules)
                    {
                        foreach (Rule r in outr)
                        {
                            focus.pdb.rules.Add(r);
                        }
                    }
                }
                return;
            }
            focus.ruleset = focus.ruleset + "\n" + ruleSet + "\n";
            if (lazyTranslate) return;
            ensureCompiled(focus);
        }

        public PNode FindOrCreateKB(string startMT)
        {
            PNode focus = KBGraph.Contains(startMT.ToString());
            if (focus == null)
            {
                focus = new PNode(startMT);
                KBGraph.AddNode(focus);
            }
            return focus;
        }

        public void loadKB(string filename, string startMT)
        {
            //loads a file (clear and overwrite)
            if (File.Exists(filename))
            {
                StreamReader streamReader = new StreamReader(filename);
                string textKB = streamReader.ReadToEnd();
                streamReader.Close();
                insertKB(textKB, startMT);
            }
        }

        public string atomize(string atomName)
        {
            if (atomName.Length > 1)
            {
                atomName = atomName.Substring(0, 1).ToLower() + atomName.Substring(1);
            }
            else
            {
                atomName = atomName.Substring(0, 1).ToLower();
            }
            return atomName;
        }
        public void loadKEKB(string filename)
        {
            loadKEKB(null, filename);
        }
        public void loadKEKB(string curKB, string filename)
        {
            // Defines a simple KEText like format for the prolog
            // you can say "mt:microtheory" to route the following lines into the MT
            // "genlmt:parentmt" will make a graph connection
            // the rest is to be determined

            if (File.Exists(filename))
            {
                StreamReader streamReader = new StreamReader(filename);
                string textKB = streamReader.ReadToEnd();
                streamReader.Close();
                curKB = curKB ?? "baseKB";
                loadKEKBText(curKB, textKB);
            }
        }
        public void loadKEKBText(string curKB, string textKB)
        {
            if (textKB != null)
            {
                string[] lines = textKB.Split('\n');
                string curConst = "";
                Dictionary<string, string> tempKB = new Dictionary<string, string>();
                foreach (string line0 in lines)
                {
                    var line = line0.Trim();
                    if (line.StartsWith(";")) continue;
                    if (line.StartsWith("exit:")) break;
                    if (line.Contains(":") && !line.Contains(":-"))
                    {
                        string[] args = line.Split(':');
                        string cmd = args[0].Trim().ToLower();
                        string val = args[1].Trim();
                        if (cmd == "tbc") { continue; }
                        if (cmd == "mt") { curKB = val; continue; }
                        if (cmd == "constant") { curConst = atomize(val).Replace(".", ""); continue; }
                        if (cmd == "const") { curConst = atomize(val).Replace(".", ""); continue; }
                        if (cmd == "genlmt") { connectMT(curKB, val); continue; }
                        if (cmd == "genlmtconst") { connectMT(curKB, curConst); continue; }
                        if (cmd == "alias") { aliasMap[val] = curKB; continue; }
                        if (cmd == "include") { loadKEKB(curKB, val); continue; }
                        if (cmd == "chemsys")
                        {
                            string[] sep = { "chemsys:" };
                            args = line.Split(sep, StringSplitOptions.RemoveEmptyEntries);
                            val = args[0].Trim();
                            if (chemSysCommandProcessor != null)
                            {
                                chemSysCommandProcessor(val);
                            }
                            string [] args2 = val.Split(',');
                            string head = args2[0];
                            string newhead = head + "(";
                            string oldhead = head + ",";
                            string newPred = val.Replace(oldhead, newhead) + ").\n";
                            newPred = newPred.Replace(",,", ",0,");
                            if (!newPred.Contains(":"))
                            {
                                if (!tempKB.ContainsKey(curKB)) tempKB[curKB] = "";
                                tempKB[curKB] = tempKB[curKB] + newPred;

                            }
                            continue;
                        }
                        //default is to make a binary pred of "cmd(curConst,val)."
                        val = val.Replace(".", "");
                        if (val.Length > 0)
                        {
                            val = atomize(val);
                            string binaryPred = String.Format("{0}({1},{2}).\n", cmd, curConst, val);
                            if (!tempKB.ContainsKey(curKB)) tempKB[curKB] = "";
                            tempKB[curKB] = tempKB[curKB] + binaryPred;

                        }
                    }
                    else
                    {
                        if (!tempKB.ContainsKey(curKB)) tempKB[curKB] = "";
                        tempKB[curKB] = tempKB[curKB] + "\n" + line;
                    }
                }
                foreach (string kb in tempKB.Keys)
                {
                    Console.WriteLine("INSERT INTO :{0}", kb);
                    //insertKB(tempKB[kb], kb);
                    appendKB(tempKB[kb], kb);
                }
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
        public string convertTextToProlist(string text)
        {
            text = text.Replace("?", " questionmark ");
            text = text.Replace("!", " exclamationmark ");
            text = text.Replace(".", " periodmark ");
            while (text.Contains("  ")) text = text.Replace("  ", " ");
            text = "[" + text.Replace(" ", ",").Trim() + "]";
            while (text.Contains("[,")) text = text.Replace("[,", "[");
            while (text.Contains(",]")) text = text.Replace(",]", "]");
            while (text.Contains(",,")) text = text.Replace(",,", ",");
            return text;
        }
        public void postListPredToMt(string pred, string text, string mt)
        {
            // will take text, convert to a list, place inside pred and
            // assert to mt

            string gaf = String.Format("{0}({1}).", pred, convertTextToProlist(text));
            insertKB(gaf, mt);
        }
        public void appendListPredToMt(string pred, string text, string mt)
        {
            // will take text, convert to a list, place inside pred and
            // assert to mt

            string gaf = String.Format("{0}({1}).", pred, convertTextToProlist(text));
            appendKB(gaf, mt);
        }

        #endregion

        #region interpreterInterface 
        public void parseRuleset()
        {
            inGlobalTest();
            var outr = parseRuleset(testruleset, "");
            testdb.rules = outr;
        }
        public RuleList parseRuleset(string rulesIn, string homeMt)
        {
            string[] rules = rulesIn.Split('\n');
            RuleList ruleList = new RuleList();
            var outi = 0;
            for (var r = 0; r < rules.Length; r++)
            {
                string rule = rules[r];
                if (rule.Length > 0)
                {
                    if (rule.Substring(0, 1) == "#" || rule == "") continue;

                    var or = ParseRule(new Tokeniser(rule));
                    if (or == null) continue;
                    or.optHomeMt = homeMt;
                    ruleList.Add(or);
                    // print ("Rule "+outi+" is : ");
                    if (show) or.print();
                }
            }
            return ruleList;
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
            PartList qlist = ParseBody(new Tokeniser(query));
            if (qlist == null)
            {
                Console.WriteLine("An error occurred parsing the query '{0}.\n", query);
                return false;
            }
            Body q = new Body(qlist);
            if (show)
            {
                Console.Write("Query is: ");
                q.print();
                Console.WriteLine("\n\n");
            }

            var vs = varNames(q.plist);
            var ctx = new QueryContext(queryMT, vs);
            var db = ctx.db;
            // db.rules = findVisibleKBRules(queryMT);
            db.rules = findVisibleKBRulesSorted(queryMT);
            db.index.Clear();
            bool isTrue = false;
            // Prove the query.
            prove(
                renameVariables(q.plist, 0, null),
                    new PEnv(),
                db,
                1,
                delegate(PEnv env)
                {
                    isTrue = true;
                }
                );
            return isTrue;
        }
        public void askQuery(string inQuery,string queryMT)
        {
            var query = inQuery;
            PartList qlist = ParseBody(new Tokeniser(query));
            if (qlist == null)
            {
                Console.WriteLine("An error occurred parsing the query '{0}.\n", query);
                return;
            }
            Body q = new Body(qlist);
            if (show)
            {
                Console.Write("Query is: ");
                q.print();
                Console.WriteLine("\n\n");
            }

            var vs = varNames(q.plist);
            var ctx = new QueryContext(queryMT, vs);
            var db = ctx.db;
            // db.rules = findVisibleKBRules(queryMT);
            db.rules = findVisibleKBRulesSorted(queryMT);
            db.index.Clear();

            // Prove the query.
            prove(
                renameVariables(q.plist, 0, null),
                new PEnv(),
                db,
                1,
                (env) => printContext(vs, env)
                );

        }
        public void askQuery(string inQuery, string queryMT, out List <Dictionary <string,string>> outBindings)
        {
            askQuery(inQuery, queryMT, true, out outBindings);
        }
        public void askQuery(string inQuery, string queryMT, bool followGenlMt, out List<Dictionary<string, string>> outBindings)
        {
            List<Dictionary<string, string>> bindingList = new List<Dictionary<string, string>>();
            Dictionary<string, string> bindingsDict = new Dictionary<string, string>();
            try
            {
                var query = inQuery;
                PartList qlist = ParseBody(new Tokeniser(query));
                if (qlist == null)
                {
                    Console.WriteLine("An error occurred parsing the query '{0}.\n", query);
                    outBindings = bindingList;
                    return;
                }
                Body q = new Body(qlist);
                if (show)
                {
                    Console.Write("Query is: ");
                    q.print();
                    Console.WriteLine("\n\n");
                }

                var vs = varNames(q.plist);
                var ctx = new QueryContext(queryMT, vs);
                var context = vs;
                var db = ctx.db;
                if (!followGenlMt)
                {
                    db.rules = findVisibleKBRules(queryMT, new ArrayList(), false);
                }
                else
                {
                    db.rules = findVisibleKBRulesSorted(queryMT);
                }
                if (db.rules == null)
                {
                    outBindings = bindingList;
                    return;
                }
                db.index.Clear();

                // Prove the query.
                prove(
                    renameVariables(q.plist, 0, null),
                        new PEnv(),
                    db,
                    1,
                    delegate(PEnv env)
                    {
                        if (context.Length == 0)
                        {
                            //TRUE
                        }
                        else
                        {
                            bindingsDict = new Dictionary<string, string>();
                            for (var i = 0; i < context.Length; i++)
                            {
                                string k = (((Variable)context.list[i]).name);
                                //string v = ((Atom)value(new Variable(((Variable)context.alist[i]).name + ".0"), env)).ToString();
                                string v = value(new Variable(((Variable)context.list[i]).name + ".0"), env).ToPLStringReadable();
                                bindingsDict[k] = v;
                            }
                            bindingList.Add(bindingsDict);
                        }
                    }
                    );
            }
            catch (Exception e)
            {
            }
            outBindings = bindingList;
        }

        public void parseQuery()
        {
            inGlobalTest();
            PartList qlist = ParseBody(new Tokeniser(testquery));
            if (qlist == null)
            {
                Console.WriteLine("An error occurred parsing the query '{0}.\n", testquery);
                return;
            }
            Body q = new Body(qlist);
            if (show)
            {
                Console.Write("Query is: ");
                q.print();
                Console.WriteLine("\n\n");
            }

            var vs = varNames(q.plist);
            var test = new QueryContext(null, vs);

            testdb.index.Clear();

            // Prove the query.
            prove(
                renameVariables(q.plist, 0, null),
                new PEnv(),
                test.db,
                1,
                (env) => printContext(vs, env)
                );

        }

        public void printContext(PartList which, PEnv env)
        {
            inGlobalTest();
            printVars(which, env);
        }

        private void inGlobalTest()
        {
            throw new NotImplementedException();
        }
        #endregion
        #region interfaceUtils

        PartList termVarNames(Term t)
        {
            PartList outv = varNames(t.partlist);
            if (t.headIsVar())
            {
                outv.AddPart(new Variable(t.name));
            }
            return outv;
        }
        // Return a list of all variables mentioned in a list of Terms.
        PartList varNames(PartList plist)
        {
            PartList outv = new PartList();


            TermList termList = plist.list;
            for (var i = 0; i < plist.Length; i++)
            {
                Part part = termList[i];
                if (((Part)part) is Atom) continue;

                if (((Part)part) is Variable)
                {
                    for (var j = 0; j < outv.Length; j++)
                        if (((Variable)outv.list[j]).name == ((Variable)part).name) goto mainc;
                    //outv.InsertPart(outv.Length, plist.alist[i]);
                    outv.AddPart((Variable)part);
                }
                else if (((Part)part) is Term)
                {
                    PartList o2 = varNames(((Term)part).partlist);

                    for (var j = 0; j < o2.Length; j++)
                    {
                        for (var k = 0; k < outv.Length; k++)
                            if (((Variable)o2.list[j]).name == ((Variable)outv.list[k]).name) goto innerc;
                        //outv.InsertPart(outv.Length, o2.alist[j]);
                        outv.AddPart(o2.list[j]);
                    innerc: j = j;
                    }
                    if (((Term)part).headIsVar())
                    {
                        outv.AddPart(new Variable(((Term)part).name));
                    }
                }
                else if (((Part)part) is PartList)
                {
                    PartList o2 = varNames(((PartList)part));

                    for (var j = 0; j < o2.Length; j++)
                    {
                        for (var k = 0; k < outv.Length; k++)
                            if (((Variable)o2.list[j]).name == ((Variable)outv.list[k]).name) goto innerc2;
                        //outv.InsertPart(outv.Length, o2.alist[j]);
                        outv.AddPart(o2.list[j]);
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
        public T renameVariables<T>(T list0, int level, Term parent) where T:Part
        {
            object list = list0;

            if (list is Atom)
            {
                return (T)list;
            }
            else if (list is Variable)
            {
                return (T)(object)new Variable(((Variable)list).name + "." + level.ToString());
            }
            else if (list is Term)
            {
                //What if the pred is a variable ?
                string nextName = ((Term)list).name;
                if (((Term)list).headIsVar())
                {
                    nextName = nextName+ "." + level.ToString();
                }
                Term outv = new Term(nextName,(PartList) renameVariables<PartList>(((Term)list).partlist, level, parent));
                outv.parent = parent;
                return (T)(object)outv;
            }

            PartList outl = new PartList();
            PartList inL = (PartList)list;
            for (var i = 0; i < inL.Length; i++)
            {
                outl.AddPart(renameVariables((Part) inL.list[i], level, parent));
                /*
                        if (list[i] is Atom) {
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
        public delegate ArrayList builtinDelegate(Term t,PartList goals,PEnv env,PDB db,int level,reportDelegate rp);

        // The main proving engine. Returns: null (keep going), other (drop out)
        public ArrayList prove(PartList goalList, PEnv environment, PDB db, int level, reportDelegate reportFunction)
        {
            //DEBUG: print ("in main prove...\n");
            if (goalList.Length == 0)
            {
                reportFunction(environment);

                //if (!more) return "done";
                return null;
            }
            if (level > deepest)
            {
                deepest = level;
                deepestName = ((Term)goalList.list[0]).name;
            }
            if (level >= maxdepth)
            {
                return null;
            }

            // Prove the first term in the goallist. We do this by trying to
            // unify that term with the rules in our database. For each
            // matching rule, replace the term with the body of the matching
            // rule, with appropriate substitutions.
            // Then prove the new goallist. (recursive call)

            Term thisTerm = (Term)goalList.list[0];
            if (trace) { Console.Write("Debug:LEVEL {0} thisterm = ", level); thisTerm.print(); Console.Write(" Environment:"); environment.print(); Console.Write("\n"); }

            // Do we have a builtin?

            builtinDelegate builtin = (builtinDelegate) PDB.builtin[thisTerm.name + "/" + thisTerm.Arity];

       //if (trace) { Console.Write("Debug: searching for builtin " + thisTerm.name + "/" + ((PartList)((PartList)thisTerm.partlist).list).length + "\n"); }
		if (builtin != null) {
            if (trace) { Console.Write("builtin with name " + thisTerm.name + " found; calling prove() on it...\n"); }
			// Stick the new body list
                    PartList newGoals = new PartList();
			int j;
			for (j=1; j<goalList.Length; j++)
            {
                newGoals.InsertPart(j-1, goalList.list[j]);
            }
			return builtin(thisTerm, newGoals, environment, db, level+1, reportFunction);
		}
             
            bool termIsVar = thisTerm.headIsVar();
            if (db.index.Count == 0)
            {
                db.initIndex();
            }

            ArrayList localRules;
            if (termIsVar)
            {
                // if its a var then sorry, just do it all ...
                localRules = db.rules.arrayList;
            }
            else
            {
                if (db.index.ContainsKey(thisTerm.name))
                    localRules = db.index[thisTerm.name];
                else
                    localRules = new ArrayList();

                // What to do for those rules that  are vars ???
                // for now just copy them over
                ArrayList varRules = db.index["_varpred_"];
                foreach (Rule r in varRules)
                {
                    localRules.Add(r);
                }
            }

            if (trace) { Console.Write("Debug: in rule selection. thisTerm = "); thisTerm.print(); Console.Write("\n"); }
            //for (var i = 0; i < db.rules.Count; i++)
            lock (localRules)
            {
                for (var i = 0; i < localRules.Count; i++)
                {
                    if (thisTerm.excludeRule == i)
                    {
                        if (trace)
                        {
                            Console.Write("DEBUG: excluding rule number " + i + " in attempt to satisfy ");
                            thisTerm.print();
                            Console.Write("\n");
                        }
                        continue;
                    }

                    //var rule = (Rule)db.rules[i];
                    var rule = (Rule) localRules[i];

                    // We'll need better unification to allow the 2nd-order
                    // rule matching ... later.
                    bool ruleIsVar = rule.head.headIsVar();
                    if ((termIsVar == false) && (ruleIsVar == false))
                    {
                        // normal operation, both are atomic
                        if (rule.head.name != thisTerm.name) continue;
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
                        rule.print();
                        Console.Write("\n");
                    }

                    // Rename the variables in the head and body
                    Term renamedHead = new Term(rule.head.name,
                                                (PartList)renameVariables((Part) rule.head.partlist, level, thisTerm));
                    // renamedHead.ruleNumber = i;
                    if (trace)
                    {
                        Console.Write("DEBUG:  renamedHead = ");
                        renamedHead.print();
                        Console.Write("\n");
                    }

                    var env2 = unify(thisTerm, renamedHead, environment);
                    if (env2 == null)
                    {
                        if (trace)
                        {
                            Console.Write("DEBUG:  unify( thisTerm=");
                            thisTerm.print();
                            Console.Write(", renamedHead = ");
                            renamedHead.print();
                            Console.Write(" in Env:");
                            environment.print();
                            Console.Write(") failed \n");
                        }
                        continue;
                    }

                    var body = rule.body;
                    if (body != null)
                    {
                        Part newFirstGoals = renameVariables((Part) rule.body.plist, level, renamedHead);
                        // Stick the new body list
                        PartList newGoals = new PartList();                        
                        int j, k;
                        for (j = 0; j < ((PartList) newFirstGoals).Length; j++)
                        {
                            newGoals.InsertPart(j, ((PartList) newFirstGoals).list[j]);
                            if (((Term) rule.body.plist.list[j]).excludeThis) ((Term) newGoals.list[j]).excludeRule = i;
                        }
                        for (k = 1; k < goalList.Length; k++) newGoals.InsertPart(j++, goalList.list[k]);
                        var ret = prove(newGoals, env2, db, level + 1, reportFunction);
                        if (ret != null)
                            return ret;
                    }
                    else
                    {
                        // Just prove the rest of the goallist, recursively.
                        PartList newGoals = new PartList();
                        int j;
                        for (j = 1; j < goalList.Length; j++) newGoals.InsertPart(j - 1, goalList.list[j]);
                        var ret = prove(newGoals, env2, db, level + 1, reportFunction);
                        if (ret != null)
                            return ret;
                    }

                    if (renamedHead.cut)
                    {
                        if (trace)
                        {
                            Console.Write("Debug: this goal ");
                            thisTerm.print();
                            Console.Write(" has been cut.\n");
                        }
                        break;
                    }
                    if ((thisTerm.parent != null) && (thisTerm.parent.cut))
                    {
                        if (trace)
                        {
                            Console.Write("Debug: parent goal ");
                            thisTerm.parent.print();
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
	public ArrayList Comparitor(Term thisTerm, PartList goalList,PEnv environment,PDB db,int  level, reportDelegate reportFunction) {
		//DEBUG print ("in Comparitor.prove()...\n");
		// Prove the builtin bit, then break out and prove
		// the remaining goalList.
	
		// if we were intending to have a resumable builtin (one that can return
		// multiple bindings) then we'd wrap all of this in a while() loop.

		// Rename the variables in the head and body
		// var renamedHead = new Term(rule.head.name, renameVariables(rule.head.ArgList, level));

		var first = value((Part)thisTerm.ArgList[0], environment);
		if (!(first is  Atom)) {
			//print("Debug: Comparitor needs First bound to an Atom, failing\n");
			return null;
		}

		var second = value((Part)thisTerm.ArgList[1], environment);
		if (!(second is Atom)) {
			//print("Debug: Comparitor needs Second bound to an Atom, failing\n");
			return null;
		}

		var cmp = "eq";
        int cmpv=first.name.CompareTo(second .name);
        if (cmpv<0) cmp ="lt";
        if (cmpv>0) cmp="gt";
		//if (first.name < second.name) cmp = "lt";
		//else if (first.name > second.name) cmp = "gt";

		var env2 = unify((Part)thisTerm.ArgList[2], new Atom(cmp), environment);

		if (env2 == null) {
			//print("Debug: Comparitor cannot unify CmpValue with " + cmp + ", failing\n");
			return null;
		}

		// Just prove the rest of the goallist, recursively.
		return prove(goalList, env2, db, level+1, reportFunction);
	}
    public ArrayList DoubleComparitor(Term thisTerm, PartList goalList, PEnv environment, PDB db, int level, reportDelegate reportFunction)
    {
        //DEBUG print ("in Comparitor.prove()...\n");
        // Prove the builtin bit, then break out and prove
        // the remaining goalList.

        // if we were intending to have a resumable builtin (one that can return
        // multiple bindings) then we'd wrap all of this in a while() loop.

        // Rename the variables in the head and body
        // var renamedHead = new Term(rule.head.name, renameVariables(rule.head.partlist.list, level));

        var first = value((Part)thisTerm.partlist.list[0], environment);
        if (!(first is Atom))
        {
            //print("Debug: Comparitor needs First bound to an Atom, failing\n");
            return null;
        }

        var second = value((Part)thisTerm.partlist.list[1], environment);
        if (!(second is Atom))
        {
            //print("Debug: Comparitor needs Second bound to an Atom, failing\n");
            return null;
        }

        var cmp = "eq";
        double v1 = double.Parse (first.name);
        double v2 = double.Parse(second.name);
        int cmpv = v1.CompareTo(v2);
        if (cmpv < 0) cmp = "gt";
        if (cmpv > 0) cmp = "lt";
        //if (first.name < second.name) cmp = "lt";
        //else if (first.name > second.name) cmp = "gt";

        var env2 = unify((Part)thisTerm.partlist.list[2], new Atom(cmp), environment);

        if (env2 == null)
        {
            //print("Debug: Comparitor cannot unify CmpValue with " + cmp + ", failing\n");
            return null;
        }

        // Just prove the rest of the goallist, recursively.
        return prove(goalList, env2, db, level + 1, reportFunction);
    }
	public ArrayList Cut(Term thisTerm, PartList goalList,PEnv environment,PDB db,int  level, reportDelegate reportFunction) {
		//DEBUG print ("in Comparitor.prove()...\n");
		// Prove the builtin bit, then break out and prove
		// the remaining goalList.
	
		// if we were intending to have a resumable builtin (one that can return
		// multiple bindings) then we'd wrap all of this in a while() loop.

		// Rename the variables in the head and body
		// var renamedHead = new Term(rule.head.name, renameVariables(rule.head.ArgList, level));

		// On the way through, we do nothing...

		// Just prove the rest of the goallist, recursively.
		ArrayList ret = prove(goalList, environment, db, level+1, reportFunction);

		// Backtracking through the 'cut' stops any further attempts to prove this subgoal.
		//print ("Debug: backtracking through cut/0: thisTerm.parent = "); thisTerm.parent.print(); print("\n");
		thisTerm.parent.cut = true;

		return ret;
	}

	// Given a single argument, it sticks it on the goal list.
    public ArrayList Call(Term thisTerm, PartList goalList, PEnv environment, PDB db, int level, reportDelegate reportFunction)
    {
		// Prove the builtin bit, then break out and prove
		// the remaining goalList.
	    //PartList goalList = (PartList)goalIn;

		// Rename the variables in the head and body
		// var renamedHead = new Term(rule.head.name, renameVariables(rule.head.ArgList, level));

		Term first = (Term) value((Part)thisTerm.ArgList[0], environment);
		if (!(first is Term)) {
			//print("Debug: Call needs parameter bound to a Term, failing\n");
			return null;
		}

		//var newGoal = new Term(first.name, renameVariables(first.ArgList, level, thisTerm));
		//newGoal.parent = thisTerm;

		// Stick this as a new goal on the start of the goallist
		//var newGoals = [];
		//newGoals[0] = first;

        PartList newGoals = new PartList();        
        newGoals.InsertPart(0,first);
		first.parent = thisTerm;

		int j;
        for (j = 0; j < goalList.Length; j++)
        {
            newGoals.InsertPart(j + 1, goalList.list[j]);
        }

	    // Just prove the rest of the goallist, recursively.
		return prove(newGoals, environment, db, level+1, reportFunction);
	}

    public ArrayList Fail(Term thisTerm, PartList goalList, PEnv environment, PDB db, int level, reportDelegate reportFunction) 
    {
		return null;
	}

    public ArrayList BagOf(Term thisTerm, PartList goalList, PEnv environment, PDB db, int level, reportDelegate reportFunction)
    {
		// bagof(Term, ConditionTerm, ReturnList)
          //  PartList goalList = (PartList)goalIn;

		Part collect0 = value((Part)thisTerm.ArgList[0], environment);
		Part subgoal =(PartList ) value((Part)thisTerm.ArgList[1], environment);
		Part into = value((Part)thisTerm.ArgList[2], environment);

		Part collect = renameVariables(collect0, level, thisTerm);
        //var newGoal = new Term(subgoal.name, renameVariables(subgoal.ArgList, level, thisTerm));
        Term newGoal = new Term(subgoal.name, (PartList)renameVariables(((PartList)subgoal), level, thisTerm));
		newGoal.parent = thisTerm;

		//var newGoals = [];
		//newGoals[0] = newGoal;
        PartList newGoals = new PartList();
        newGoals.AddPart(newGoal);

		// Prove this subgoal, collecting up the environments...
		PartList anslist = new PartList();
		anslist.renumber = -1;
		ArrayList ret = prove(newGoals, environment, db, level+1, BagOfCollectFunction(collect, anslist));

		// Turn anslist into a proper list and unify with 'into'
		
		// optional here: nil anslist -> fail?
		Part answers = new Atom(FUNCTOR_NIL);

		/*
		print("Debug: anslist = [");
			for (var j = 0; j < anslist.length; j++) {
				anslist[j].print();
				print(", ");
			}
		print("]\n");
		*/

		for (int i = anslist.Length; i > 0; i--)
        {
            answers = MakeList(anslist.list[i - 1], answers);
        }

		//print("Debug: unifying "); into.print(); print(" with "); answers.print(); print("\n");
		var env2 = unify(into, answers, environment);

		if (env2 == null) {
			//print("Debug: bagof cannot unify anslist with "); into.print(); print(", failing\n");
			return null;
		}

		// Just prove the rest of the goallist, recursively.
		return prove(goalList, env2, db, level+1, reportFunction);
	}

	// Aux function: return the reportFunction to use with a bagof subgoal
	public reportDelegate   BagOfCollectFunction(Part collect,PartList anslist) {
		return delegate(PEnv env) {
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
			anslist.AddPart( renameVariables(value(collect, env), anslist.renumber--, null));
		};
	}

	// Call out to external javascript
	// external/3 takes three arguments:
	// first: a template string that uses $1, $2, etc. as placeholders for 
	
        //var EvalContext = [];
    public string ourEval( string expression)
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
                if (ctx==null)
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

        public ArrayList  External(Term thisTerm, PartList goalList,PEnv environment,PDB db,int  level, reportDelegate reportFunction) {
		//print ("DEBUG: in External...\n");
	    PartList ourParList =(PartList) ((PartList)thisTerm.ArgList[0]).list[0];
	
		// Get the first term, the template.
        var first = value((Part)ourParList.list[0], environment);
		if (!(first is Atom)) {
			//print("Debug: External needs First bound to a string Atom, failing\n");
			return null;
		}
		//var r = first.name.match(/^"(.*)"$/);
		//if (! r) return null;
		//r = r[1];
        Match rm = Regex.Match(((Atom)first).name, @"^\""(.*)\""$");
        if (!rm.Success) return null;
        string r = rm.Groups[1].Value;


		//print("DEBUG: template for External/3 is "+r+"\n");

		// Get the second term, the argument list.
        Part second = (Part)value((Term)ourParList.list[1], environment);
        Part next;
        int i = 1;


		while (second is Term && ((Term)second).name == FUNCTOR_LIST) {
            next = (PartList)((Term)second).ArgList[0];
            next = (Part)((PartList)next).list[0];
            next = (Part)((PartList)next).list[0];

            Part argV = null;
            Part nextTerm = null;
            if (next is PartList)
            {
                 argV = (Part)((PartList)next).list[0];
                 nextTerm = (Part)((PartList)next).list[1];
            }
            if (next is Variable)
            {
                argV = next;
                nextTerm = next;
            }
			// Go through second an argument at a time...
            //Part arg = value((Part)((Term)second).ArgList[0], environment);
            Part arg = value(argV, environment);
            if (!(arg is Atom))
            {
				//print("DEBUG: External/3: argument "+i+" must be an Atom, not "); arg.print(); print("\n");
				return null;
			}
			//var re = new RegExp("\\$"+i, "g");
			//print("DEBUG: External/3: RegExp is "+re+", arg is "+arg.name+"\n");
			//r = r.Replace(re, arg.name);
            r = Regex .Replace (r,"\\$"+i,((Atom)arg).name);

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
        string ret = ourEval(r);

		//print("DEBUG: External/3 got "+ret+" back\n");

		if (ret==null) ret = FUNCTOR_NIL;


		// Convert back into an atom...
        var env2 = unify((Part)ourParList.list[2], new Atom(ret), environment);

		if (env2 == null) {
			//print("Debug: External/3 cannot unify OutValue with " + ret + ", failing\n");
			return null;
		}

		// Just prove the rest of the goallist, recursively.
		return prove(goalList, env2, db, level+1, reportFunction);
	}

	public ArrayList  ExternalAndParse(Term thisTerm, PartList goalList,PEnv environment,PDB db,int  level, reportDelegate reportFunction) {
		//print ("DEBUG: in External...\n");
        PartList ourParList = (PartList)((PartList)thisTerm.ArgList[0]).list[0];

        // Get the first term, the template.
        var first = value((Part)ourParList.list[0], environment);
        if (!(first is Atom))
        {
			//print("Debug: External needs First bound to a string Atom, failing\n");
			return null;
		}
        Match rm = Regex.Match(first.name, @"^\""(.*)\""$");
        if (!rm.Success) return null;
        string r = rm.Groups[1].Value;

		//print("DEBUG: template for External/3 is "+r+"\n");

        // Get the second term, the argument list.
        Part second = (Part)value((Term)ourParList.list[1], environment);
        Part next;
        int i = 1;
        while (second is Term && second.name == FUNCTOR_LIST)
        {
            next = (PartList)((Term)second).ArgList[0];
            next = (Part)((PartList)next).list[0];
            next = (Part)((PartList)next).list[0];

            Part argV = null;
            Part nextTerm = null;
            if (next is PartList)
            {
                argV = (Part)((PartList)next).list[0];
                nextTerm = (Part)((PartList)next).list[1];
            }
            if (next is Variable)
            {
                argV = next;
                nextTerm = next;
            }
            // Go through second an argument at a time...
            //Part arg = value((Part)((Term)second).ArgList[0], environment);
            Part arg = value(argV, environment);
            if (!(arg is Atom))
            {
                //print("DEBUG: External/3: argument "+i+" must be an Atom, not "); arg.print(); print("\n");
                return null;
            }
            //var re = new RegExp("\\$"+i, "g");
            //print("DEBUG: External/3: RegExp is "+re+", arg is "+arg.name+"\n");
            //r = r.Replace(re, arg.name);
            r = Regex.Replace(r, "\\$" + i, ((Atom)arg).name);

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

		if (ret==null) ret = FUNCTOR_NIL;


		// Convert back into a Prolog term by calling the appropriate Parse routine...
		Part retPart = ParsePart(new Tokeniser(ret));
		//print("DEBUG: external2, ret = "); ret.print(); print(".\n");

        var env2 = unify((Part)ourParList.list[2], retPart, environment);

		if (env2 == null) {
			//print("Debug: External/3 cannot unify OutValue with " + ret + ", failing\n");
			return null;
		}

		// Just prove the rest of the goallist, recursively.
		return prove(goalList, env2, db, level+1, reportFunction);
    }
        #endregion
        #region prologObjects

        // Object (of a style...) definitions:
        // Rule = (Head, Body)
        // Head = Term
        // Body = [Term]
        // Term = (id, Parameters)
        // Parameters {Partlist} = [Part]
        // Part = Variable | Atom | Term

        public class PEnv : Hashtable
        {
            public void print()
            {
                foreach (string k in this.Keys)
                {
                    Console.Write("{0} = ", k); ((Part)this[k]).print(); Console.WriteLine();
                }
            }
            public override string ToString()
            {
                string result = "";
                foreach (string k in this.Keys)
                {
                    result += String.Format("{0} = ", k) + ((Part)this[k]).ToPLStringReadable() + "\n"; 
                }
                return result;
            }

        }

        public class RuleList : IEnumerable
        {
            internal ArrayList arrayList = new ArrayList();
            internal PDB syncPDB;

            public int Count
            {
                get { return arrayList.Count; }
            }

            public void Add(Rule r)
            {
                arrayList.Add(r);
            }
            public RuleList()
            {

            }

            public void RemoveAt(int i)
            {
                Rule r = this[i];
                Release(r);
                arrayList.RemoveAt(i);
            }

            private void Release(Rule r)
            {
                if (r.instanceTriple == null) return;
                throw new NotImplementedException();
            }

            public Rule this[int i]
            {
                get { return (Rule)arrayList[i]; }
                set { arrayList[i] = value; }
            }

            public void Clear()
            {
                if (syncPDB != null)
                {
                    lock (syncPDB.index)
                    {
                        syncPDB.index.Clear();
                    }
                }
                foreach (Rule rule in arrayList)
                {
                    Release(rule);
                }
                arrayList.Clear();
            }

            public IEnumerator GetEnumerator()
            {
                return arrayList.GetEnumerator();
            }
        }
        public class PDB
        {
            static public Hashtable builtin = new Hashtable();
            private RuleList _rules;

            // A fast index for the database
            public Dictionary<string, ArrayList> index = new Dictionary<string, ArrayList>();

            public PDB()
            {
                _rules = new RuleList();
                _rules.syncPDB = this;
            }

            public void initIndex()
            {
                index["_varpred_"] = new ArrayList();
                var rules = this.rules;
                lock (rules) for (int i = 0; i < rules.Count; i++)
                    {
                        Rule rule = (Rule)rules[i];
                        string name = rule.head.name;
                        if (!index.ContainsKey(name)) { index[name] = new ArrayList(); }
                        index[name].Add(rule);
                        if (rule.head.headIsVar())
                        {
                            index["_varpred_"].Add(rule);
                        }
                    }

            }
            public RuleList rules
            {
                get
                {
                    return _rules;
                }
                set
                {
                    if (Object.ReferenceEquals(_rules, value)) return;
                    if (_rules.Count > 0)
                    {
                        _rules.Clear();
                    }
                    _rules = value;
                }
            }
        }

        public abstract class Part
        {
            protected NotImplementedException Missing(string p)
            {
                return new NotImplementedException(type + " Missing '" + p + "' for " + this.ToPLStringReadable());
            }
            public const string LIST_FNAME = "cons";
            public abstract string type { get; }
            public virtual string name
            {
                get { throw Missing("name"); }
            }
            public virtual int Arity
            {
                get { throw Missing("Arity"); }
            }
            public virtual TermList ArgList
            {
                get { throw Missing("ArgList"); }
            }
            public static bool IsListName(string s)
            {
                return s == LIST_FNAME || s == ".";
            }

            public abstract void print();

            public abstract string ToPLStringReadable();

            public override string ToString()
            {
                return ToPLStringReadable();
            }

        }

        public class PartList : Part
        {
            public override string name
            {
                get
                {
                    return base.name; 
                    return null;
                }
            }

            // Parameters {Partlist} = [Part]
            // Part = Variable | Atom | Term
            //public string name;
            public override string type { get { return "PartList"; } }
            private readonly TermList tlist = new TermList();
            public int renumber=0;
            public int Length
            {
                get
                {
                    return tlist.Count;
                }
            }
            public override TermList ArgList
            {
                get
                {
                    return tlist;
                }
            }
            public TermList list
            {
                get { return tlist; }
            }

            //public PartList(string head) { name = head; }
            public PartList(Part l) { tlist.Add(l); }
            public PartList() { }

            public PartList(TermList head, PEnv env)
            {
                for (var i = 0; i < head.Count; i++)
                {
                   tlist.Add(value(head[i], env));
                }

            }

            public override void print()
            {
                bool com = false;
                // Console.Write("plist(");
                foreach (Part p in tlist.ToList())
                {
                    if (com) Console.Write(", ");
                    p.print();
                    com = true;
                }
              //  Console.Write(")");
            }
            public override string ToPLStringReadable()
            {
                string result = "";
                bool com = false;
                //result = "plist(";
                foreach (Part p in tlist.ToList())
                {
                    if (com) result += (", ");
                    result += p.ToPLStringReadable();
                    com = true;
                }
               // result += ")";
                return result;
            }

            public void AddPart(Part term)
            {
                tlist.Add(term);
            }

            public void InsertPart(int i, Part part)
            {
                tlist.Insert(i, part);
            }
        }

        public class Atom : Part
        {
            readonly public string _name;
            public override string name
            {
                get
                {
                    return _name;
                }
            }
            public int hash = 0;
            public override string type { get { return "Atom"; } }
            public Atom(string head)
            {
                head = NameCheck(head);
                _name = head; hash = name.GetHashCode();
            }
            public override void print() { Console.Write(this.name); }
            public override string ToPLStringReadable() { return this.name; }
        }
        public class Variable : Part
        {
            public string _name;
            public override string name { get { return _name; } }
            public override string type { get { return "Variable"; } }
            public Variable(string head) { _name = head; }
            public override void print() { Console.Write(this.name); }
            public override string ToPLStringReadable() { return this.name; }
        }
        public class Term : Part
        {
            public string _name;
            public override string name { get { return _name; } }
            public override string type { get { return "Term"; } }

            public TermList ArgList
            {
                get { return partlist.list; }
            }

            override public int Arity
            {
                get { return partlist.Length; }
            }

            public IEnumerable Args
            {
                get { return partlist.list.ToList(); }
            }

            public readonly PartList partlist;
            public bool excludeThis = false;
            public int excludeRule = -1;
            public bool cut = false;
            public Term parent = null;
            public bool headIsVar()
            {
                // should be [A-Z\_\?]
                return SIProlog.IsVarName(name);
            }
            public Term(string head, PartList list)
            {
                SIProlog.NameCheck(head);
                _name = head;
                partlist = list;
            }
            public override void print()
            {
                if (this.name == FUNCTOR_LIST)
                {
                    Part x = this;
                    while (x is Term && x.name == "cons" && ((Term)x).Arity == 2)
                    {
                        x = ((Term)x).ArgList[1];
                    }
                    if ((x is Atom && x.name == FUNCTOR_NIL) || x is Variable )
                    {
                        x = this;
                        Console.Write("[");
                        var com = false;
                        while (x is Term && x.name == "cons" && ((Term)x).Arity == 2)
                        {
                            if (com) Console.Write(", ");
                            (((Term)x).ArgList[0]).print(); // May need to case var/atom/term
                            com = true;
                            x = ((Term)x).ArgList[1];
                        }
                        if (x is Variable )
                        {
                            Console.Write(" | ");
                            x.print();
                        }
                        Console.Write("]");
                        return;
                    }
                }
                Console.Write("" + this.name + "(");
                this.partlist.print();
                Console.Write(")");
            }


            public override string ToPLStringReadable()
            {
                string result = "";
                if (this.name == FUNCTOR_LIST)
                {
                    Part x = this;
                    while (x is Term && x.name == FUNCTOR_LIST && x.Arity == 2)
                        x = ((Term)x).ArgList[1];
                    if ((x is Atom && x.name == FUNCTOR_NIL) || x is Variable)
                    {
                        x = this;
                        result += "[";
                        var com = false;
                        while (x is Term && x.name == FUNCTOR_LIST && x.Arity == 2)
                        {
                            if (com) result +=", ";
                            result += ((Term)x).ArgList[0].ToPLStringReadable(); // May need to case var/atom/term
                            com = true;
                            x = ((Term)x).ArgList[1];
                        }
                        if (x is Variable)
                        {
                           result += " | ";
                           result += x.ToPLStringReadable();
                        }
                        result += "]";
                        return result;
                    }
                }
                result += "" + this.name + "(";
                result += this.partlist.ToPLStringReadable();
                result += ")";
                return result;
            }
        }

        public static bool IsVarName(string name)
        {
            // should be [A-Z\_\?]
            string firstChar = name.Substring(0, 1);
            if (firstChar == "." || firstChar == "[")
            {
                return false;
            }
            return (firstChar == firstChar.ToUpper());
        }


        public partial class Rule
        {
            public string optHomeMt;
            // Rule = (Head, Body)
            public Term head = null;
            public Body body = null;
            public Rule(Term head)
            {
                this.head = head;
                this.body = null;
            }
            public Rule(Term head, PartList bodylist)
            {
                this.head = head;
                if (bodylist != null)
                    this.body = new Body(bodylist);
                else
                    this.body = null;
            }
            public void print()
            {
                if (this.body == null)
                {
                    this.head.print();
                    Console.WriteLine(".");
                }
                else
                {
                    this.head.print();
                    Console.Write(" :- ");
                    this.body.print();
                    Console.WriteLine(".");
                }
            }
            public override string ToString()
            {
                if (this.body == null)
                {
                    return this.head.ToPLStringReadable() + ".";
                }
                else
                {
                    return this.head.ToPLStringReadable() + " :- " + this.body.ToPLReadableString() + ".";
                }
            }
        }

        public class Body
        {
            // Body = [Term]

            readonly public PartList plist = null;
            public Body(PartList l)
            {
                plist = l;
            }
            public void print()
            {
                for (var i = 0; i < this.plist.Length; i++)
                {
                    ((Term)this.plist.list[i]).print();
                    if (i < this.plist.Length - 1)
                        Console.Write(", ");
                }
            }
            public override string ToString()
            {
                return ToPLReadableString();
            }

            public string ToPLReadableString()
            {
                string result = "";

                for (var i = 0; i < this.plist.Length; i++)
                {
                    result += ((Term)this.plist.list[i]).ToPLStringReadable();
                    if (i < this.plist.Length - 1)
                        result += ", ";
                }
                return result;
            }
        }
    #endregion

        public string describeObject(string objname,object o)
        {
            string result = "";
            Type type = o.GetType();
            PropertyInfo [] properties = type.GetProperties();
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


        #region prologParser
        // The Tiny-Prolog parser goes here.
        public class Tokeniser
        {
            public string remainder;
            public string type;
            public string current;

            public Tokeniser(string input)
            {
                this.remainder = input;
                this.current = null;
                this.type = null;	// "eof", "id", "var", "punc" etc.
                this.consume();	// Load up the first token.
            }

            public void consume()
            {
                if (this.type == "eof") return;
                // Eat any leading WS
                Match r = Regex.Match(this.remainder, @"^\s*(.*)$");
                if (r.Success)
                {
                    this.remainder = r.Groups[1].Value;
                }

                if (this.remainder == "")
                {
                    this.current = null;
                    this.type = "eof";
                    return;
                }

                // Decimal numbers
                r = Regex.Match(this.remainder, @"^([0-9]*\.[0-9]*)(.*)$");
                //r = this.remainder.match(/^(-[0-9][0-9]*)(.*)$/);
                if (r.Success)
                {
                    this.remainder = r.Groups[2].Value;
                    this.current = r.Groups[1].Value;
                    this.type = "id";
                    return;
                }



                r = Regex.Match(this.remainder, @"^([\(\)\.,\[\]\|\!]|\:\-)(.*)$");
                //r = this.remainder.match(/^([\(\)\.,\[\]\|\!]|\:\-)(.*)$/);
                if (r.Success)
                {
                    this.remainder = r.Groups[2].Value;
                    this.current = r.Groups[1].Value;
                    this.type = "punc";
                    return;
                }

                r = Regex.Match(this.remainder, @"^([A-Z_\?][a-zA-Z0-9_\?\-]*)(.*)$");
                //r = this.remainder.match(/^([A-Z_][a-zA-Z0-9_]*)(.*)$/);
                if (r.Success)
                {
                    this.remainder = r.Groups[2].Value;
                    this.current = r.Groups[1].Value;
                    this.type = "var";
                    return;
                }

                // URLs in curly-bracket pairs
                r = Regex.Match(this.remainder, @"^(\{[^\}]*\})(.*)$");
                //r = this.remainder.match(/^(\{[^\}]*\})(.*)$/);
                if (r.Success)
                {
                    this.remainder = r.Groups[2].Value;
                    this.current = r.Groups[1].Value;
                    this.type = "id";
                    return;
                }

                // Quoted strings

                r = Regex.Match(this.remainder, @"^(\""[^\""]*\"")(.*)$");
                // r = Regex .Match( this.remainder,@"^(\"\[\^\"\]*\")(.*)$");
                //r = this.remainder.match(/^("[^"]*")(.*)$/);
                if (r.Success)
                {
                    this.remainder = r.Groups[2].Value;
                    this.current = r.Groups[1].Value;
                    this.type = "id";
                    return;
                }

                r = Regex.Match(this.remainder, @"^([a-zA-Z0-9][a-zA-Z0-9_]*)(.*)$");
                //r = this.remainder.match(/^([a-zA-Z0-9][a-zA-Z0-9_]*)(.*)$/);
                if (r.Success)
                {
                    this.remainder = r.Groups[2].Value;
                    this.current = r.Groups[1].Value;
                    this.type = "id";
                    return;
                }

                r = Regex.Match(this.remainder, @"^(-[0-9][0-9]*)(.*)$");
                //r = this.remainder.match(/^(-[0-9][0-9]*)(.*)$/);
                if (r.Success)
                {
                    this.remainder = r.Groups[2].Value;
                    this.current = r.Groups[1].Value;
                    this.type = "id";
                    return;
                }

                // Negative Decimal numbers
                r = Regex.Match(this.remainder, @"^(-?[\d\.]+)(.*)$");
                //r = this.remainder.match(/^(-[0-9][0-9]*)(.*)$/);
                if (r.Success)
                {
                    this.remainder = r.Groups[2].Value;
                    this.current = r.Groups[1].Value;
                    this.type = "id";
                    return;
                }

                this.current = null;
                this.type = "eof";

            }
        }



        public Rule ParseRule(Tokeniser tk)
        {
            // A rule is a Head followed by . or by :- Body

            Term h = (Term)ParseHead(tk);
            if (h == null) return null;

            if (tk.current == ".")
            {
                // A simple rule.
                return new Rule(h);
            }

            if (tk.current != ":-") return null;
            tk.consume();
            PartList b = ParseBody(tk);

            if (tk.current != ".") return null;

            return new Rule(h, b);
        }

        public object ParseHead(Tokeniser tk)
        {
            // A head is simply a term. (errors cascade back up)
            return ParseTerm(tk);
        }

        public object ParseTerm(Tokeniser tk)
        {
            // Term -> [NOTTHIS] id ( optParamList )

            if (tk.type == "punc" && tk.current == "!")
            {
                // Parse ! as cut/0
                tk.consume();
                return new Term("cut", new PartList());
            }

            var notthis = false;
            if (tk.current == "NOTTHIS")
            {
                notthis = true;
                tk.consume();
            }

            //if (tk.type != "id")  return null;
            if ((tk.type != "id")&&(tk.type !="var")) return null;
            var name = tk.current;
            tk.consume();

            if (tk.current != "(")
            {
                // fail shorthand for fail(), ie, fail/0
                if (name == "fail")
                {
                    return new Term(name, new PartList());
                }
                return null;
            }
            tk.consume();

            PartList p = new PartList();

            var i = 0;
            while (tk.current != ")")
            {
                if (tk.type == "eof") return null;

                var part = ParsePart(tk);
                if (part == null) return null;

                if (tk.current == ",") tk.consume();
                else if (tk.current != ")") return null;

                // Add the current Part onto the list...
                p.AddPart(part);
            }
            tk.consume();

            var term = new Term(name, p);
            if (notthis) term.excludeThis = true;
            return term;
        }

        // This was a beautiful piece of code. It got kludged to add [a,b,c|Z] sugar.
        public Part ParsePart(Tokeniser tk)
        {
            // Part -> var | id | id(optParamList)
            // Part -> [ listBit ] ::-> cons(...)
            if (tk.type == "var")
            {
                var n = tk.current;
                tk.consume();
                return new Variable(n);
            }

            if (tk.type != "id")
            {
                if (tk.type != "punc" || tk.current != "[") return null;
                // Parse a list (syntactic sugar goes here)
                tk.consume();
                // Special case: [] = new atom(nil).
                if (tk.type == "punc" && tk.current == "]")
                {
                    tk.consume();
                    return new Atom(FUNCTOR_NIL);
                }

                // Get a list of parts into l
                ArrayList l = new ArrayList();
                int i = 0;

                while (true)
                {
                    var t = ParsePart(tk);
                    if (t == null) return null;

                    l.Insert(i++, t);
                    if (tk.current != ",") break;
                    tk.consume();
                }

                // Find the end of the list ... "| Var ]" or "]".
                Part append;
                if (tk.current == "|")
                {
                    tk.consume();
                    if (tk.type != "var") return null;
                    append = new Variable(tk.current);
                    tk.consume();
                }
                else
                {
                    append = new Atom(FUNCTOR_NIL);
                }
                if (tk.current != "]") return null;
                tk.consume();
                // Return the new cons.... of all this rubbish.
                for (--i; i >= 0; i--)
                {
                    append = MakeList((Part) l[i], append);
                }
                return append;
            }

            var name = tk.current;
            tk.consume();

            if (tk.current != "(") return new Atom(name);
            tk.consume();

            PartList p = new PartList();
            //int ix = 0;
            while (tk.current != ")")
            {
                if (tk.type == "eof") return null;

                var part = ParsePart(tk);
                if (part == null) return null;

                if (tk.current == ",") tk.consume();
                else if (tk.current != ")") return null;

                // Add the current Part onto the list...
                p.AddPart(part);
            }
            tk.consume();

            return new Term(name, p);
        }

        private Part MakeList(Part li, Part append)
        {
            PartList frag = new PartList();
            frag.AddPart((Part)li);
            frag.AddPart(append);
            append = new Term(FUNCTOR_LIST, frag);
            return append;
        }

        public PartList ParseBody(Tokeniser tk)
        {
            // Body -> Term {, Term...}

            PartList p = new PartList();
            var i = 0;

            Term t;
            while ((t = (Term)ParseTerm(tk)) != null)
            {
                p.AddPart(t);
                i++;
                if (tk.current != ",") break;
                tk.consume();
            }

            if (i == 0) return null;
            return p;
        }
        #endregion
        #region auxUtils

        // Some auxiliary bits and pieces... environment-related.

        // Print out an environment's contents.
        public void printEnv(PEnv env)
        {
            if (env == null)
            {
                Console.WriteLine("null\n");
                return;
            }
            var k = false;
            //foreach (var i in env)
            foreach (string i in env.Keys)
            {
                k = true;
                Console.Write(" " + i + " = ");
                ((Part)env[i]).print();
                Console.WriteLine("\n");
            }
            if (!k) Console.WriteLine("true\n");
        }

        public void printVars(PartList which, PEnv environment)
        {
            // Print bindings.
            if (which.Length == 0)
            {
                Console.WriteLine("true\n");
            }
            else
            {
                for (var i = 0; i < which.Length; i++)
                {
                    Console.Write(((Variable)which.list[i]).name);
                    Console.Write(" = ");
                    //((Atom)value(new Variable(((Variable)which.alist[i]).name + ".0"), environment)).print();
                    value(new Variable(((Variable)which.list[i]).name + ".0"), environment).print();
                    Console.WriteLine("\n");
                }
            }
            Console.WriteLine("\n");
        }

        // The value of x in a given environment
        static public Part value(Part x, PEnv env)
        {
            if (x is Term)
            {
                PartList p = new PartList(((Term)x).ArgList, env);
                return new Term(((Term)x).name, p);
            }
            if (x is PartList)
            {
                PartList p = new PartList(((PartList)x).list, env);
                return p;
            }
            if (!(x is Variable))
                return x;		// We only need to check the values of variables...

            if (!env.ContainsKey(((Variable)x).name))
                return x;
            Part binding = (Part)env[((Variable)x).name]; //** HASH/DICTIONARY NEEDED ** 
            if (binding == null) 
                return x;		// Just the variable, no binding.
            return value(binding, env);
        }

        // Give a new environment from the old with "n" (a string variable name) bound to "z" (a part)
        // Part is Atom|Term|Variable
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
            if (trace) { Console.Write("     unify X="); x.print(); Console.Write("  Y="); y.print(); Console.WriteLine(); }

            if (x is Variable)
            {
                if (trace) Console.WriteLine("     MATCH");
                return newEnv(((Variable)x).name, y, env);
            }
            if (y is Variable)
            {
                if (trace) Console.WriteLine("     MATCH");
                return newEnv(((Variable)y).name, x, env);
             }
            if (x is Atom || y is Atom)
                if (x.type == y.type && ((Atom)x).name == ((Atom)y).name)
                {
                    if (trace) Console.WriteLine("     MATCH");
                    return env;
                }
                else
                    return null;

            // x.type == y.type == Term...
            if (x is Term && y is Term)
            {
                bool xvar = ((Term)x).headIsVar();
                bool yvar = ((Term)y).headIsVar();
                if (!xvar && !yvar)
                {
                    if (x.name != y.name)
                        return null;	// Ooh, so first-order.
                }

                if (((Term)x).Arity != ((Term)y).Arity) 
                    return null;

                for (var i = 0; i < ((Term)x).Arity; i++)
                {
                    env = unify((Part)((Term)x).ArgList[i], (Part)((Term)y).ArgList[i], env);
                    if (env == null)
                        return null;
                }
                if (!xvar && yvar)
                {
                    if (trace) Console.WriteLine("     MATCH");
                    return newEnv(((Term)y).name, new Atom(((Term)x).name), env);
                }
                if (xvar && !yvar)
                {
                    if (trace) Console.WriteLine("     MATCH");
                    return newEnv(((Term)x).name, new Atom(((Term)y).name), env);
                }
                if (xvar && yvar)
                {
                    if (trace) Console.WriteLine("     MATCH");
                    return newEnv(((Term)x).name, new Variable(((Term)y).name), env);
                }

            }

            if (x is PartList && y is PartList)
            {

                if (((PartList)x).name != ((PartList)y).name)
                    return null;	// Ooh, so first-order.
                if (((PartList)x).Length != ((PartList)y).Length)
                {
                    // TODO: fix list layout. sometimes we get the list with an outer wrapper PartList
                    while (((PartList)x).Length != ((PartList)y).Length)
                    {
                        if ((((PartList)x).Length == 1) && ((PartList)x).alist[0] is PartList)
                        {
                            //if (((PartList)((PartList)x).alist[0]).Length == ((PartList)y).Length)
                           // {
                                x = ((PartList)((PartList)x).alist[0]);
                           // }
                           // else
                           //     return null;
                        }
                        else
                            return null;
                    }
                    if (trace) { Console.Write("     inner-unify X="); x.print(); Console.Write("  Y="); y.print(); Console.WriteLine(); }
                  
                }
                for (var i = 0; i < ((PartList)x).Length; i++)
                {
                    env = unify((Part)((PartList)x).alist[i], (Part)((PartList)y).alist[i], env);
                    if (env == null) 
                        return null;
                }
            }
            if (trace) Console.WriteLine("     MATCH");
            return env;
        }*/

        public PEnv unify(Part x, Part y, PEnv env)
        {
            x = value(x, env);
            y = value(y, env);
            if (trace) { Console.Write("     unify X="); x.print(); Console.Write("  Y="); y.print(); Console.WriteLine(); }

            // both atoms/constants
            if (x is Atom && y is Atom)
            {
                if (((Atom)x).name == ((Atom)y).name)
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
                if (((PartList)x).Length ==0 &&  ((PartList)y).Length ==0)
                {
                    if (trace) Console.WriteLine("     MATCH");
                    return env;
                }

            }

            // variables check, should do occurs in check ...
            if (x is Variable)
            {
                if (trace) Console.WriteLine("     MATCH");
                return newEnv(((Variable)x).name, y, env);
            }
            if (y is Variable)
            {
                if (trace) Console.WriteLine("     MATCH");
                return newEnv(((Variable)y).name, x, env);
            }
            // both lists or terms
            if (x.type != y.type)
            {
                return null;
            }

            if (x is Term)
            {
                bool xvar = ((Term)x).headIsVar();
                bool yvar = ((Term)y).headIsVar();
                if (!xvar && !yvar)
                {
                    if (x.name != y.name)
                        return null;	// Ooh, so first-order.
                    return unify(((Term)x).partlist, ((Term)y).partlist, env);

                }
                if (xvar && !yvar)
                {
                    PEnv subEnv = unify(new Variable(((Term)x).name), new Atom(((Term)y).name), env);
                    if (subEnv == null)
                        return null;
                    return unify(((Term)x).partlist, ((Term)y).partlist, subEnv);
                }
                if (!xvar && yvar)
                {
                    PEnv subEnv = unify(new Atom(((Term)x).name), new Variable(((Term)y).name), env);
                    if (subEnv == null)
                        return null;
                    return unify(((Term)x).partlist, ((Term)y).partlist, subEnv);
                }
                if (xvar && yvar)
                {
                    PEnv subEnv = unify(new Variable(((Term)x).name), new Variable(((Term)y).name), env);
                    if (subEnv == null)
                        return null;
                    return unify(((Term)x).partlist, ((Term)y).partlist, subEnv);
                }


            }
            if (x is PartList )
            {
                while (((((PartList)x).Length == 1) && ((PartList)x).list[0] is PartList) && (((PartList)x).Length != ((PartList)y).Length) )
                {
                    throw new NotImplementedException("inner partlist");
                    x = ((PartList)((PartList)x).list[0]);
                }
                while (((((PartList)y).Length == 1) && ((PartList)y).list[0] is PartList) && (((PartList)x).Length != ((PartList)y).Length) )
                {
                    throw new NotImplementedException("inner partlist");
                    y = ((PartList)((PartList)y).list[0]);
                }
                
                if (((PartList)x).Length != ((PartList)y).Length)
                {
                     while (((PartList)x).Length != ((PartList)y).Length)
                    {
                        if (((PartList)y).Length < ((PartList)x).Length)
                        {
                            PartList temp = (PartList) x;
                            x = y;
                            y = temp;

                        }
                        /*if ((((PartList)x).Length == 1) && ((PartList)x).alist[0] is PartList)
                        {
                            x = ((PartList)((PartList)x).alist[0]);
                        }
                        else*/
                            return null;
                    }

                }
                if (trace) { Console.Write("     inner-unify X="); x.print(); Console.Write("  Y="); y.print(); Console.WriteLine(); }
                if (false && (((PartList)x).name != ((PartList)y).name))
                    return null;	// Ooh, so first-order.

                for (var i = 0; i < ((PartList)x).Length; i++)
                {
                    env = unify((Part)((PartList)x).list[i], (Part)((PartList)y).list[i], env);
                    if (env == null)
                        return null;
                }               
            }

            if (trace) Console.WriteLine("     MATCH");
            return env;
        }
        #endregion

        #region mtGraph

        public class PNode:IComparable 
        {
            public string id;
            public PDB pdb = new PDB();
            public string ruleset = null;
            public bool dirty = false;
            public double probability = 1.0;
            public string backend = null;

            List<PEdge> incomingEdgeList = new List<PEdge>();
            List<PEdge> outgoingEdgeList = new List<PEdge>();

            public string Id
            {
                get { return id; }
                set { id = value; }
            }
            object info;

            public object Info
            {
                get { return info; }
                set { info = value; }
            }

            public double Probability
            {
                get { return probability; }
                set { probability = value; }
            }

            public PNode(string id)
                : this(id, null)
            {            
            }
            public override int GetHashCode()
            {
                return base.GetHashCode();
                return id.GetHashCode();
            }
            public override bool Equals(object obj)
            {
                PNode otherNode = obj as PNode;
                if (otherNode == null)
                    return false;

                return otherNode.id == this.id;
            }

            public static bool operator ==(PNode node1, PNode node2)
            {
                if (Object.ReferenceEquals(node1, node2))
                    return true;
                if (Object.ReferenceEquals(node1, null) || Object.ReferenceEquals(node2, null))
                    return false;

                return node1.Equals(node2);
            }

            public static bool operator !=(PNode node1, PNode node2)
            {
                return !(node1 == node2);
            }
            public PNode(string id, object info)
            {
                this.id = id;
                this.info = info;
            }

            public override string ToString()
            {
                return "mt:" + Id + " " + DebugInfo;
            }

            public PEdge CreateEdgeTo(PNode otherNode)
            {
                PEdge edge = new PEdge(this, otherNode);
                return edge;
            }

            public void AddIncomingEdge(PEdge edge)
            {
                incomingEdgeList.Add(edge);
            }

            public void AddOutgoingEdge(PEdge edge)
            {
                outgoingEdgeList.Add(edge);
            }

            public PEdge[] IncomingEdges
            {
                get { return incomingEdgeList.ToArray(); }
            }

            public PEdge[] OutgoingEdges
            {
                get { return outgoingEdgeList.ToArray(); }
            }

            public string DebugInfo
            {
                get { return string.Format("prob={0} size={1}", probability, pdb.rules.Count); }
            }

            public bool EdgeAlreadyExists(PNode otherNode)
            {
                foreach (PEdge e in this.OutgoingEdges)
                {
                    if (e.EndNode == otherNode)
                    {
                        return true;
                    }
                }

                return false;
            }

            /// <summary>
            /// IComparable.CompareTo implementation.
            /// </summary>
            public int CompareTo(object obj)
            {
                if (obj is PNode)
                {
                    PNode temp = (PNode)obj;

                    return probability.CompareTo(temp.probability);
                }

                throw new ArgumentException("object is not a PNode");
            }


            internal string ToLink(string serverRoot)
            {
                return string.Format("<a href='{1}siprolog/?mt={0}'>{0}  ({2})</a>", id, serverRoot, DebugInfo);
            }
        }

        public class PEdge
        {
            PNode startNode;

            public PNode StartNode
            {
                get { return startNode; }
            }
            PNode endNode;

            public PNode EndNode
            {
                get { return endNode; }
            }
            object info;

            public object Info
            {
                get { return info; }
                set { info = value; }
            }

            public PEdge(PNode startNode, PNode endNode)
                : this(startNode, endNode, null)
            {
            }

            public PEdge(PNode startNode, PNode endNode, object info)
            {
                this.startNode = startNode;
                this.startNode.AddOutgoingEdge(this);
                this.endNode = endNode;
                this.endNode.AddIncomingEdge(this);

                this.info = info;
            }
        }

        public class PGraph
        {
            List<PNode> topLevelNodes = new List<PNode>();

            public void Connect(string idSrc, string idDest)
            {
                PNode srcNode = FindOrCreateNode(idSrc);
                PNode destNode = FindOrCreateNode(idDest);

                if (destNode == srcNode)
                {
                    return;
                }
                if (!srcNode.EdgeAlreadyExists(destNode))
                    srcNode.CreateEdgeTo(destNode);
            }

            private PNode FindOrCreateNode(string idSrc)
            {
                PNode srcNode = Contains(idSrc);
                if (srcNode == null)
                {
                    srcNode = new PNode(idSrc);
                    AddNode(srcNode);
                }
                return srcNode;
            }

            public PNode[] TopLevelNodes
            {
                get { return topLevelNodes.ToArray(); }
            }
           public PNode[] SortedTopLevelNodes
            {
                get {
                    //return 
                    //    topLevelNodes.ToArray();
                    PNode[] temp = topLevelNodes.ToArray();
                    Array.Sort(temp, delegate(PNode p1, PNode p2)
                    {
                        return CIC.Compare(p1.id,p2.id );
                    });
                    return temp;
                }
            }

            public void AddNode(PNode node)
            {
                topLevelNodes.Add(node);
            }

            public PNode Contains(string id)
            {
                List<PNode> visitedNodes = new List<PNode>();
                PNode[] tempTopLevelNodes;
                lock (topLevelNodes) { tempTopLevelNodes = topLevelNodes.ToArray(); }

                foreach (PNode node in tempTopLevelNodes)
                {
                    PNode retNode = FindNode(id, node, visitedNodes);
                    if (retNode != null)
                        return retNode;
                }
                return null;
            }

            private static readonly KeyCase CIC = KeyCase.Default;
            private static PNode FindNode(string id, PNode node, List<PNode> visitedNodes)
            {
                if (CIC.Compare(node.Id, id) == 0)
                    return node;

                // Recursively reached the same node again, bail out..
                if (visitedNodes.Contains(node))
                    return null;

                visitedNodes.Add(node);

                foreach (PEdge edge in node.OutgoingEdges)
                {
                    PNode retNode = FindNode(id, edge.EndNode, visitedNodes);
                    if (retNode != null)
                        return retNode;
                }

                return null;
            }

            public List<PEdge>[] FindCycles()
            {
                List<List<PEdge>> cycles = new List<List<PEdge>>();

                foreach (PNode node in GetTopLevelNodes())
                {
                    List<List<PEdge>> cyclesFound = FindCycles(node, new List<PNode>(), new List<PEdge>());
                    if (cyclesFound != null)
                        cycles.AddRange(cyclesFound);
                }

                return cycles.ToArray();
            }

            public PNode[] GetTopLevelNodes()
            {
                List<PNode> rootNodes = new List<PNode>();
                rootNodes.AddRange(topLevelNodes.FindAll(delegate(PNode node)
                {
                    return node.IncomingEdges.Length == 0;
                }));

                // Fully connected graph, return any node
                if (rootNodes.Count == 0 && topLevelNodes.Count > 0)
                {
                    return new PNode[] { topLevelNodes[0] };
                }
                else
                {
                    return new PNode[] { };
                }
            }

            private List<List<PEdge>> FindCycles(PNode node, List<PNode> visitedNodes, List<PEdge> currentPathEdges)
            {
                List<List<PEdge>> cycles = new List<List<PEdge>>();
                if (visitedNodes.Contains(node))
                {
                    cycles.Add(new List<PEdge>(currentPathEdges));
                }

                visitedNodes.Add(node);

                foreach (PEdge e in node.OutgoingEdges)
                {
                    // Don't go along edges already in the path.
                    if (!currentPathEdges.Contains(e))
                    {
                        currentPathEdges.Add(e);
                        List<List<PEdge>> cyclesFound = FindCycles(e.EndNode, visitedNodes, currentPathEdges);
                        cycles.AddRange(cyclesFound);
                        currentPathEdges.Remove(e);
                    }
                }
                return cycles;
            }

            public void PrintToConsole()
            {
                foreach (PNode node in SortedTopLevelNodes)
                {
                    PrintToConsole(node, 0);

                }
            }

            private void PrintToConsole(PNode node, int indentation)
            {
                if (node == null) return;
                if (indentation > 4) return; 
                for (int i = 0; i < indentation; ++i) Console.Write(" ");
                Console.WriteLine(node.Id);

                foreach (PEdge e in node.OutgoingEdges)
                {
                    PrintToConsole(e.EndNode, indentation + 1);
                }
            }

            public void PrintToWriterTreeMts(StreamWriter writer, string serverRoot)
            {
                writer.WriteLine("<ul>");
                foreach (PNode node in SortedTopLevelNodes)
                {
                    PrintToWriterOutEdges(node, 0, writer, serverRoot);

                }
                writer.WriteLine("</ul>");
            }

            public void PrintToWriterOutEdges(PNode node, int indentation, StreamWriter writer, string serverRoot)
            {
                if (node == null) return;
                //writer.Write("<p>");
                //for (int i = 0; i < indentation; ++i) writer.Write(" ");
                //Console.WriteLine(node.Id);
                writer.WriteLine("<li>{0}</li>", node.ToLink(serverRoot));
                writer.WriteLine("<ul>");
                foreach (PEdge e in node.OutgoingEdges)
                {
                    if (indentation < 10) PrintToWriterOutEdges(e.EndNode, indentation + 1, writer, serverRoot);
                }
                writer.WriteLine("</ul>");
            }
            public void PrintToWriterInEdges(StreamWriter writer, string serverRoot)
            {
                writer.WriteLine("<ul>");
                foreach (PNode node in SortedTopLevelNodes)
                {
                    PrintToWriterInEdges(node, 0, writer, serverRoot);

                }
                writer.WriteLine("</ul>");
            }

            public void PrintToWriterInEdges(PNode node, int indentation, StreamWriter writer, string serverRoot)
            {
                if (node == null) return;
                if (indentation > 4) return;
                //writer.Write("<p>");
                //for (int i = 0; i < indentation; ++i) writer.Write(" ");
                //Console.WriteLine(node.Id);
                writer.WriteLine("<li>{0}</li>", node.ToLink(serverRoot));
                writer.WriteLine("<ul>");
                foreach (PEdge e in node.IncomingEdges )
                {
                    PrintToWriterInEdges(e.StartNode , indentation + 1, writer, serverRoot);
                }
                writer.WriteLine("</ul>");
            }
        }
        #endregion

        internal static string NameCheck(string name)
        {
            if (name == "cons")
            {
                return ".";
                throw new NotImplementedException(name);
            }
            if (name == "nil")
            {
                return "[]";
                throw new NotImplementedException(name);
            }
            return name;
        }
    }

    public class KeyCase : IEqualityComparer<string>
    {
        public static KeyCase Default = new KeyCase();
        #region Implementation of IEqualityComparer<string>

        /// <summary>
        /// Determines whether the specified objects are equal.
        /// </summary>
        /// <returns>
        /// true if the specified objects are equal; otherwise, false.
        /// </returns>
        /// <param name="x">The first object of type <paramref name="T"/> to compare.
        ///                 </param><param name="y">The second object of type <paramref name="T"/> to compare.
        ///                 </param>
        public bool Equals(string x, string y)
        {
            return SameKey(x, y);
        }

        /// <summary>
        /// Returns a hash code for the specified object.
        /// </summary>
        /// <returns>
        /// A hash code for the specified object.
        /// </returns>
        /// <param name="obj">The <see cref="T:System.Object"/> for which a hash code is to be returned.
        ///                 </param><exception cref="T:System.ArgumentNullException">The type of <paramref name="obj"/> is a reference type and <paramref name="obj"/> is null.
        ///                 </exception>
        public int GetHashCode(string obj)
        {
            return NormalizeKey(obj).GetHashCode();
        }

        public static string NormalizeKey(object s)
        {
            return s.ToString().Trim().ToLower().Replace(" ", "_");
        }

        #endregion

        public static bool SameKey(object u1, object key)
        {
            if (Equals(u1, key)) return true;
            if (u1.GetType().IsValueType)
            {
                throw new InvalidOperationException("lcase " + u1.GetType());
            }
            if (NormalizeKey(u1) != NormalizeKey(key)) return false;
            return true;
        }

        public int Compare(string c1, string c2)
        {
            return NormalizeKey(c1).CompareTo(NormalizeKey(c2));
        }
    }

    public class CIDictionary<K, V> : Dictionary<K, V>
    {
        static public IEqualityComparer<K> comp
        {
            get
            {
                return (IEqualityComparer<K>)KeyCase.Default;
            }
        }
        public CIDictionary()
            : base((IEqualityComparer<K>)comp)
        {

        }
        public CIDictionary(IDictionary<K, V> dict)
            : base(dict, (IEqualityComparer<K>)comp)
        {

        }
    }

    public class TermList
    {
        ArrayList holder = new ArrayList();
        public int Count
        {
            get { return holder.Count; }
        }

        public SIProlog.Part this[int i]
        {
            get { return (SIProlog.Part) holder[i]; }
        }

        public void Add(SIProlog.Part part)
        {
            if (part is SIProlog.PartList)
            {
                throw new NotImplementedException("inner partlist");
            }
            holder.Add(part);
        }

        public void Insert(int i, SIProlog.Part part)
        {
            if (i == Count)
            {
                Add(part);
                return;
            }
            throw new NotImplementedException("inserting outof order");
        }

        public IEnumerable ToList()
        {
            return holder;
        }
    }
}

