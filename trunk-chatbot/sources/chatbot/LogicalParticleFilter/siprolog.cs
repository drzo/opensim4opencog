#define MERGED_RDFSTORE
using System;
using System.Collections.Generic;
using System.Collections;
using System.Data;
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
        [ThreadStatic]
        public static SourceLanguage tl_console_language = SourceLanguage.Prolog;
        public static string tl_languageName
        {
            get
            {
                if (tl_console_language == null) return null;
                return tl_console_language.Name;
            }
        }
        public static SIProlog CurrentProlog;
        public SIProlog()
        {
            CurrentProlog = this;
            DLRConsole.TransparentCallers.Add(GetType());
            DLRConsole.SetIgnoreSender("KEYVALUELISTSIPROLOG", true);
            defineBuiltIns();
            defineRDFExtensions();
            tl_mt = "baseKB";
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

        public void webWriter(TextWriter writer, string action, string query, string mt, string serverRoot)
        {
            writer = WebLinksWriter.AddWarnWriter(writer);
            serverRoot = "/";
            if ((action == "autorefresh")||(action=="autoquery"))
            {
                writer.WriteLine("<META HTTP-EQUIV=\"REFRESH\" content=\"10\">");
            }
            writer.WriteLine("<html>");
            var s = @"<head>
<script>
function showtip(current,e,text)
{
   if (document.all)
   {
      thetitle=text.split('<br>')
      if (thetitle.length > 1)
      {
        thetitles=""
        for (i=0; i<thetitle.length-1; i++)
           thetitles += thetitle[i] + ""\r\n""
        current.title = thetitles
      }
      else current.title = text
   }

   else if (document.layers)
   {
       document.tooltip.document.write(
           '<layer bgColor=""#FFFFE7"" style=""border:1px ' +
           'solid black; font-size:12px;color:#000000;"">' + text + '</layer>')
       document.tooltip.document.close()
       document.tooltip.left=e.pageX+5
       document.tooltip.top=e.pageY+5
       document.tooltip.visibility=""show""
   }
}

function hidetip()
{
    if (document.layers)
        document.tooltip.visibility=""hidden""
}
</script>
</head>
";
            writer.WriteLine(s);
            TOCmenu(writer, serverRoot);
            webWriter0(writer, action, query, mt, serverRoot, true);
            writer.WriteLine("</html>");
            WebLinksWriter.RemoveWarnWriter(writer);
        }

        private void TOCmenu(TextWriter writer, string serverRoot)
        {
            writer.WriteLine("<a href='{0}siprolog/?q=list'>List Mts</a> ", serverRoot);
            writer.WriteLine("<a href='{0}siprolog/?q=preds'>List Preds</a> ", serverRoot);
            writer.WriteLine("<a href='{0}siprolog/?q=listing'>List All KB Rules</a> ", serverRoot);
            writer.WriteLine("<a href='{0}query'>Sparql Query</a>", PFEndpoint.serverRoot);
            writer.WriteLine("<a href='{0}list/'>Behavour List</a>", serverRoot);
            //writer.WriteLine("<a href='{0}processes/list/'>Processes List</a>", serverRoot);
            writer.WriteLine("<a href='{0}scheduler/?a=liststatus'>Scheduler List</a>", serverRoot);
            writer.WriteLine("<a href='{0}analysisllist/'>Analysis List</a>", serverRoot);
            writer.WriteLine("<a href='{0}graphmaster/?a=list'>Graphmaster List</a>", serverRoot);
            writer.WriteLine("<a href='{0}'>Home</a><br/>", serverRoot);
        }
        public void webWriter0(TextWriter writer, string action, string queryv, string mt, string serverRoot, bool toplevel)
        {
            try
            {
                if ((action == null) && (queryv == null) && (mt == null))
                {
                    queryv = "list";
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
                        if (queryv.ToLower() == "preds")
                        {
                            writer.WriteLine("<h2>Siprolog Preds List</h2>");
                            SharedGlobalPredDefsDirty = true;
                            UpdateSharedGlobalPredDefs();
                            lock (SharedGlobalPredDefs)
                            {
                                foreach (var kpp in SharedGlobalPredDefs)
                                {
                                    kpp.Value.WriteHtmlInfo(writer);
                                }
                            }
                            UpdateSharedGlobalPredDefs();
                            WriteMtInfo(writer, rdfDefMT, serverRoot, false);
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
                        case "clear":
                            clearKB(mt);
                            break;
                        case "query":
                            interactQuery(writer, queryv, mt, serverRoot);
                            break;
                        case "autoquery":
                            interactQuery(writer, queryv, mt, serverRoot);
                            return;
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


        public void WriteMtInfo(TextWriter writer, string mt, string serverRoot, bool toplevel)
        {
            tl_ServerRoot = serverRoot;
            tl_writer = writer;
            writer.WriteLine("<h2>Siprolog Mt {0}</h2>", mt);
            PNode qnode = FindKB(mt);
            if (qnode != null)
            {
                writer.WriteLine("<h3> OutgoingEdges </h3>");
                KBGraph.PrintToWriterOutEdges(qnode, 0, writer, serverRoot);
                writer.WriteLine("<h3> IncomingEdges </h3>");
                KBGraph.PrintToWriterInEdges(qnode, 0, writer, serverRoot);
                mt = qnode.Id;
            }
            tl_mt = mt;
            //KBGraph.ShowGenlMts(qnode, null, 0, writer, serverRoot);
            writer.WriteLine("<h3> KB Operations for {0}</h3>&nbsp;", mt);
            writer.WriteLine("<a href='{0}plot/?mt={1}&q=plot(X,Y)'>Plot Mt</a>&nbsp;", serverRoot, mt);
            writer.WriteLine("<a href='{0}plot/?mt={1}&a=autorefresh&q=plot(X,Y)'>Scope Mt</a> ", serverRoot, mt);
            writer.WriteLine("<a href='{0}siprolog/?mt={1}&a=autorefresh'>Watch Mt</a> ", serverRoot, mt);
            writer.WriteLine("<a href='{0}siprolog/?mt={1}&q=clear'>Clear Prolog KB</a> ", serverRoot, mt);
            writer.WriteLine("<a href='{0}xrdf/?mt={1}&q=clearcache'>Clear RDF Cache for KB</a> ", serverRoot, mt);
            writer.WriteLine("<a href='{0}xrdf/?mt={1}&q=syncfromremote'>Sync From Remote</a> ", serverRoot, mt);
            writer.WriteLine("<a href='{0}xrdf/?mt={1}&q=synctoremote'>Sync To Remote</a> ", serverRoot, mt);
            writer.WriteLine("<a href='{0}xrdf/?mt={1}&q=pl2rdf'>Prolog2RDF</a> ", serverRoot, mt);
            writer.WriteLine("<a href='{0}xrdf/?mt={1}&q=rdf2pl'>RDF2Prolog</a> ", serverRoot, mt);
            writer.WriteLine("<br/>");

            if (qnode != null)
            {
                ensureCompiled(qnode, ContentBackingStore.Prolog);
                if (RdfDeveloperSanityChecks > 2)
                {
                    var kbContents0 = findVisibleKBRulesSorted(mt);
                    ensureCompiled(qnode, ContentBackingStore.RdfMemory);
                    ensureCompiled(qnode, ContentBackingStore.Prolog);
                    var kbContents1 = findVisibleKBRulesSorted(mt);
                    if (kbContents0.Count != kbContents1.Count)
                    {
                        Warn("findVisibleKBRulesSorted changed size {0}->{1}", kbContents0.Count, kbContents1.Count);
                    }
                }
            }
            var kbContents = findVisibleKBRulesSorted(mt);
            int total = kbContents.Count;
            int local = 0;
            int showInheritedCount = 300;
            if (qnode != null) local = qnode.pdb.rules.Count;
            int inherited = total - local;
            bool showInherited = (local > inherited);
            if (local == 0) showInherited = true;
            if (inherited < showInheritedCount)
            {
                showInheritedCount = inherited;
            }
            writer.WriteLine(
                "<h3> KB Contents (<font color='blue'>Blue local</font> {0}) (<font color='darkgreen'>Green Inherited ({2})</font> {1})</h3>",
                local, inherited,
                !showInherited ? "unshown" :
                showInheritedCount >= inherited ? "all shown" : "showing the first " + showInheritedCount);
            if (toplevel) writer.WriteLine("<hr/>");
            int shown = 0;
            var qnodeID = qnode == null ? mt : qnode.id;
            foreach (Rule r in kbContents)
            {
                var rmt = r.optHomeMt;
                bool localMT = (qnodeID == rmt);
                if (!localMT)
                {
                    if (!showInherited)
                        continue;
                    shown++;
                    if (shown > showInheritedCount) showInherited = false;
                }
                WriteRule(writer, r, qnode);
            }
            if (qnode == null) return;

            if (!qnode.IsDataFrom(ContentBackingStore.Prolog))
            {
                WriteMtInfoRDF(writer, qnode, mt, serverRoot, toplevel);
            }
        }
        public void WriteMtInfoRDF(TextWriter writer, PNode qnode, string mt, string serverRoot, bool toplevel)
        {
            if (qnode != null)
            {
                if (qnode.SyncFrequency != FrequencyOfSync.Never) ensureCompiled(qnode, ContentBackingStore.RdfMemory);
            }
            var gwf = FindRepositoryKB(mt);
            if (gwf != null)
            {
                try
                {
                    WriteGraph(writer, gwf.rdfGraph, gwf.definations, mt);
                }
                catch (Exception e)
                {
                    writer.WriteLine("<font color='red'>{0} {1} {2}</font>", e.GetType(), e.Message, e.StackTrace);
                }
            }
            else
            {
                writer.WriteLine("<h3> KB Triples {1}</h3> Not synced <a href='{0}xrdf/?mt={1}&q=pl2rdf'>Sync Now</a> ", serverRoot, mt);
            }
        }

        private void WriteRule(TextWriter writer, Rule r, PNode qnode)
        {
            var mt = r.optHomeMt;
            bool localMT = qnode.id == mt;
            string color = localMT ? "blue" : "darkgreen";
            string ext = localMT ? "" : string.Format("&nbsp;&nbsp;%<a href='{0}xrdf/?mt={1}'>{1}</a>", tl_ServerRoot, mt);


            string toolTip = "";
            /*var rdf = r.RdfRuleValue(); 
            if (rdf != null)
            {
                toolTip = string.Format("onmouseover=\"showtip('{0}')\" ", rdf.ToString().Replace("\"", "\\\"").Replace("'", "\\'"));
            }*/
            writer.WriteLine("<font color='{0}' {1}>{2}</font>{3}<br/>", color, toolTip,
                           WebLinksWriter.EntityFormat(r.ToSource(SourceLanguage.Prolog)), ext);
        }

        [ThreadStatic] static int tl_StructToStringDepth = 4;
        public static string StructToString(object t)
        {
            int before = tl_StructToStringDepth;
            try
            {
                return StructToString1(t, 2);
            }
            finally
            {
                tl_StructToStringDepth = before;
            }
        }

        private static bool HasElements(ICollection props)
        {
            return props != null && props.Count > 0;
        }
        public static string StructToString1(object t, int depth)
        {
            if (t == null) return "NULL";
            Type structType = t.GetType();
            if (t is IConvertible || t is String || t is Uri || t is Stream || t is Part) return "" + t;
            if (tl_StructToStringDepth > depth)
            {
                tl_StructToStringDepth = depth;
            }
            else
            {
                depth = tl_StructToStringDepth;
            }
            if (structType.IsValueType) depth++;
            if (depth < 0) return "^";// +t;
            StringBuilder result = new StringBuilder();
            if (t is IEnumerable)
            {
                IEnumerable ic = t as IEnumerable;
                int max = 10;
                int fnd = 0;
                bool printSomething = true;
                result.Append("Items: [");
                foreach (var i in ic)
                {
                    if (printSomething)
                    {
                        result.Append(fnd + ": " + StructToString1(i, depth - 1) + " ");
                        tl_StructToStringDepth = depth;
                    }
                    fnd++;
                    max--;
                    if (max < 1)
                    {
                        if (printSomething) result.Append("...");
                        printSomething = false;
                    }
                }
                result.Append("]");
                return "CollectionType: " + structType + " Count: " + fnd + " " + result.ToString().TrimEnd();
            }
            const BindingFlags fpub = BindingFlags.Public | BindingFlags.Instance;
            const BindingFlags fpriv = BindingFlags.NonPublic | BindingFlags.Instance;
            FieldInfo[] fields = structType.GetFields(fpub);
            PropertyInfo[] props = structType.GetProperties(fpub);
            bool hasProps = HasElements(props);
            if (!HasElements(fields) && !hasProps)
            {
                fields = structType.GetFields(fpriv);
            }
            if (!HasElements(props))
            {
                props = structType.GetProperties(fpriv);
            }
            bool needSimpleToString = true;

            HashSet<string> unneeded = new HashSet<string>();
            //if (HasElements(fields))
            {
                foreach (PropertyInfo prop in props)
                {
                    if (prop.GetIndexParameters().Length != 0) continue;
                    needSimpleToString = false;
                    string propname = prop.Name;
                    if (propname == "AToString") continue;
                    unneeded.Add(propname.Trim('m', '_').ToUpper());
                    result.Append("{" + propname + ": " + StructToString1(prop.GetValue(t, null), depth - 1) + "}");
                    tl_StructToStringDepth = depth;
                }
            }
            //if (needSimpleToString)
            {
                foreach (FieldInfo prop in fields)
                {
                    string propname = prop.Name;
                    if (unneeded.Contains(propname.Trim('m', '_').ToUpper())) continue;                   
                    needSimpleToString = false;
                    result.Append("{" + propname + ": " + StructToString1(prop.GetValue(t), depth - 1) + "}");
                    tl_StructToStringDepth = depth;
                }
            }

            if (needSimpleToString)
            {
                return "" + t;
            }

            return result.ToString().TrimEnd();
        }

        public void interactQuery(TextWriter writer, string query, string mt, string serverRoot)
        {
            int testdepth = 64;

            List<Dictionary<string, string>> bingingsList = new List<Dictionary<string, string>>();
            while ((bingingsList.Count == 0) && (testdepth < 1024))
            {
                testdepth = (int)(testdepth * 1.5);
                //ConsoleWriteLine("Trying depth {0}", testdepth);
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
                writer.WriteLine("{2} bindings found at depth {0} in {1}<br/>", testdepth, mt, bingingsList.Count);
                int index = 0;
                foreach (Dictionary<string, string> bindings in bingingsList)
                {
                    index++;
                    writer.Write("{0}: ", index);
                    foreach (string k in bindings.Keys)
                    {
                        string v = bindings[k];
                        writer.Write("{0}={1} ", k, v);
                    }
                    writer.WriteLine("<br/>");
                }
                writer.WriteLine("<hr/>");

            }
        }
        public void interactFooter(TextWriter writer, string mt, string serverRoot)
        {
            writer.WriteLine("<hr/>");

            writer.WriteLine(" <form method='get' ACTION='{1}siprolog/'>", mt, serverRoot);
            writer.WriteLine(" Query: <INPUT TYPE='text' name='q'/>");
            MtSelector(writer, mt);
            writer.WriteLine(" <INPUT TYPE='hidden' name='a' VALUE='query'/>");
            writer.WriteLine(" <INPUT TYPE='submit' VALUE='submit'/>");
            writer.WriteLine(" </FORM>");
            writer.WriteLine(" <form method='get' ACTION='{1}siprolog/'>", mt, serverRoot);
            writer.WriteLine(" AutoQuery: <INPUT TYPE='text' name='q'/>");
            MtSelector(writer, mt);
            writer.WriteLine(" <INPUT TYPE='hidden' name='a' VALUE='autoquery'/>");
            writer.WriteLine(" <INPUT TYPE='submit' VALUE='submit'/>");
            writer.WriteLine(" </FORM>");
            writer.WriteLine(" <form method='get' ACTION='{1}siprolog/'>", mt, serverRoot);
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

        private void MtSelector(TextWriter writer, string mt)
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
            lock(focus.CompileLock)
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
                        Rule r = (Rule) rules[i];
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
                    var subKB = MakeRepositoryKB(kb);
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
                    if (RdfDeveloperSanityChecks > 1)
                    {
                        ensureCompiled(focus, ContentBackingStore.Prolog);
                        ensureCompiled(focus, ContentBackingStore.RdfMemory);
                    }
                }
            }
        }

        public void loadIntoKB_unlocked(RuleList ruleSet, PNode focus, bool clearFirst)
        {
            if (clearFirst) focus.Clear();
            if (focus.IsDataFrom(ContentBackingStore.Prolog))
            {
                if (clearFirst)
                {
                    focus.pdb.rules = ruleSet;
                    focus.SyncFromNow = ContentBackingStore.Prolog;
                    return;
                }
                focus.pdb.index.Clear();
                lock (focus.pdb.rules)
                {
                    foreach (Rule r in ruleSet)
                    {
                        focus.pdb.rules.Add(r);
                    }
                }
                focus.SyncFromNow = ContentBackingStore.Prolog;
                return;
            }
            var rdfGraphWithDefs = focus.RdfStore;
            foreach (Rule rule in ruleSet)
            {
                try
                {
                    rdfGraphWithDefs.AddRuleToRDF(rule);
                }
                catch (Exception e)
                {
                    Warn(e);
                }
            }
        }
        public PNode FindOrCreateKB(string startMT)
        {
            lock (KBGraph)
            {
                return FindOrCreateKB_unlocked(startMT);
            }
        }

        private PNode FindOrCreateKB_unlocked(string startMT)
        {
            PNode focus = FindKB(startMT);
            if (focus == null)
            {
                focus = MakeRepositoryKB(startMT);// //new PNode(startMT);
                //KBGraph.AddNode(focus);
            }
            return focus;
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
                var restoreThreadKB = curKB;
                Dictionary<string, RuleList> tempKB = ParseKEText(startMT, ruleSet);
                foreach (string kb in tempKB.Keys)
                {
                    RuleList newRules = tempKB[kb];
                    var focus = MakeRepositoryKB(kb);
                    ConsoleWriteLine("{0} {1} Rules INTO {2}",
                                     clearFirst ? "REPLACING" : "APPENDING",
                                     newRules.Count,
                                     focus);
                    //insertKB(tempKB[kb], kb);
                    loadIntoKB(newRules, focus, clearFirst);
                }
                curKB = restoreThreadKB;
            }
        }

        Dictionary<string, RuleList> ParseKEText(string startMT, string ruleSet)
        {
            // if startMT is null use the global variable
            string parseKB = startMT ?? curKB;
            // code below uses parseKB
            Dictionary<string, RuleList> tempKB = new Dictionary<string, RuleList>();
            ///string[] lines = ruleSet.Split('\n');
            string curConst = "";
            {
                do
                {
                    if (ruleSet.Trim() == "") break;
                    bool readProlog = false;
                    string line = toEndOfLine(ref ruleSet);
                    try
                    {
                        if (line.StartsWith(";doc;"))
                        {
                            var line5 = line.Substring(5).Trim() + " .";
                            Term t = ParseTerm(new Tokeniser(line5), startMT) as Term;
                            DocumentTerm(t, false);
                            continue;
                        }
                        if (line.StartsWith(";") || line.StartsWith("%")) continue;
                        if (line.StartsWith("exit:")) return tempKB;
                        if (line.StartsWith("."))
                        {
                            continue;
                        }
                        do
                        {
                            if (!line.Contains(":") || line.Contains(":-"))
                            {
                                readProlog = true;
                                break;
                            }
                            string[] args = line.Split(':');
                            string cmd = args[0].Trim().ToLower();
                            // if any non letters in our KE text directive parse it as prolog instead
                            if (!Regex.Match(cmd, "^[a-z]+$").Success)
                            {
                                readProlog = true;
                                break;
                            }
                            // we need to trim off period and and spaces before the period
                            string val = TrimAndRemoveTrailingPeriod(args[1]);
                            if (cmd == "tbc")
                            {
                                continue;
                            }
                            if (cmd == "mt")
                            {
                                parseKB = val;
                                curKB = val;
                                continue;
                            }
                            if (cmd == "base")
                            {
                                // todo process base                            
                                continue;
                            }
                            if (cmd == "const" || cmd == "constant" || cmd == "comment")
                            {
                                curConst = val;
                                continue;
                            }
                            if (cmd == "predicate")
                            {
                                Term t = ParseTerm(new Tokeniser(val), startMT) as Term;
                                DocumentTerm(t, false);
                                curConst = t.name;
                                continue;
                            }
                            if (cmd == "genlmt")
                            {
                                connectMT(parseKB, val);
                                continue;
                            }
                            if (cmd == "nonmt")
                            {
                                disconnectMT(parseKB, val);
                                continue;
                            }

                            if (cmd == "genlmtconst")
                            {
                                connectMT(parseKB, curConst);
                                continue;
                            }
                            if (cmd == "alias")
                            {
                                aliasMap[val] = parseKB;
                                continue;
                            }
                            if (cmd == "include")
                            {
                                loadKEFile(parseKB, val);
                                continue;
                            }
                            if (cmd == "chemsys")
                            {
                                string[] sep = {"chemsys:"};
                                args = line.Split(sep, StringSplitOptions.RemoveEmptyEntries);
                                val = args[0].Trim();
                                if (chemSysCommandProcessor != null)
                                {
                                    chemSysCommandProcessor(val);
                                }
                                string[] args2 = val.Split(',');
                                string head = args2[0];
                                string newhead = head + "(";
                                string oldhead = head + ",";
                                string newPred = val.Replace(oldhead, newhead) + ").\n";
                                newPred = newPred.Replace(",,", ",0,");
                                if (!newPred.Contains(":"))
                                {
                                    if (!tempKB.ContainsKey(parseKB)) tempKB[parseKB] = new RuleList();
                                    tempKB[parseKB].Add(ParseRule(new Tokeniser(newPred), parseKB));

                                }
                                continue;
                            }

                            if (cmd == "module")
                            {
                                // A Macro for CEMA/GOAP
                                // same as 
                                //  mt:module_name
                                //  module(module_name).

                                parseKB = val;
                                curKB = val;
                                val = atomize(val);
                                string uniPred = String.Format("module({0}).\n", val);
                                if (!tempKB.ContainsKey(parseKB)) tempKB[parseKB] = new RuleList();
                                tempKB[parseKB].Add(ParseRule(new Tokeniser(uniPred), parseKB));
                                continue;
                            }

                            //default is to make a binary pred of "cmd(curConst,val)."
                            val = TrimAndRemoveTrailingPeriod(val);
                            cmd = TrimAndRemoveTrailingPeriod(args[0]);
                            if (val.Length > 0)
                            {
                                val = atomize(val);
                                string binaryPred = String.Format("{0}({1},{2}).\n", cmd, atomize(curConst), val);
                                var rule = ParseRule(new Tokeniser(binaryPred), parseKB);
                                if (rule != null)
                                {
                                    if (!tempKB.ContainsKey(parseKB)) tempKB[parseKB] = new RuleList();
                                    tempKB[parseKB].Add(rule);
                                    continue;
                                }
                                else
                                {
                                    readProlog = true;
                                    break;
                                    // fall thru to other reader.. the ":" confused us
                                }
                            }
                        } while (false);
                        if (!readProlog)
                        {
                            continue;
                        }
                        //else
                        {
                            string was = line;
                            Tokeniser firstTokenizer = new Tokeniser(was);
                            var rule = ParseRule(firstTokenizer, parseKB);
                            if (rule == null)
                            {
                                if (!was.EndsWith("."))
                                {
                                    firstTokenizer = new Tokeniser(was + ".");
                                    rule = ParseRule(firstTokenizer, parseKB);
                                }
                            }
                            string remainder = firstTokenizer.remainder;
                            if (remainder.Trim().Length > 0)
                            {
                                prolog_reader_debug("remainder found");
                                ruleSet = remainder + "\n" + ruleSet;
                            }
                            if (rule == null)
                            {
                                if (trace)
                                {
                                    Warn("Could not parse: " + was);
                                    continue;
                                }
                                Tokeniser newTokeniser = new Tokeniser(was);
                                string kb = parseKB;
                                tl_spy_prolog_reader = true;
                                rule = ParseRule(newTokeniser, kb);
                                tl_spy_prolog_reader = false;
                                Warn("Could not parse: " + was);
                                continue;
                            }
                            if (!tempKB.ContainsKey(parseKB)) tempKB[parseKB] = new RuleList();
                            tempKB[parseKB].Add(rule);
                        }
                    }
                    catch (Exception e)
                    {
                        Console.WriteLine("EXCEPTION: in line '{0}' caused '{1}'", line, e.Message);
                    }
                } while (ruleSet.Trim() != "");
            }
            return tempKB;
        }

        private string TrimAndRemoveTrailingPeriod(string val)
        {
            return val.Trim().TrimEnd('.').TrimEnd();
        }

        private string toEndOfLine(ref string ruleSet)
        {
            ruleSet = ruleSet.TrimStart();
            int eol = ruleSet.IndexOf('\n');
            if (eol == -1)
            {
                var line0 = ruleSet;
                ruleSet = "";
                return line0;
            }
            var line = ruleSet.Substring(0, eol + 1).TrimEnd('\r', '\n');
            ruleSet = ruleSet.Substring(eol + 1);
            return line;
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
            while (text.Contains("\n")) text = text.Replace("\n", " ");
            while (text.Contains("\r")) text = text.Replace("\r", " ");
            while (text.Contains("  ")) text = text.Replace("  ", " ");
            text = "[\"" + text.Replace(" ", "\",\"").Trim() + "\"]";
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
            var testKB = "testKB";
            clearKB(testKB);
            var outr = parseRuleset(testruleset, testKB);
            testdb.rules = outr;
        }
        public RuleList parseRuleset(string rulesIn, string homeMt)
        {
            var pmt = ParseKEText(homeMt, rulesIn);
            if (pmt != null && pmt.Count == 1)
            {
                var ruleList0 = pmt.Values.FirstOrDefault();
                return ruleList0;
            }
            string[] rules = rulesIn.Split('\n');
            RuleList ruleList = new RuleList();
            var outi = 0;
            for (var r = 0; r < rules.Length; r++)
            {
                string rule = rules[r];
                if (rule.Length > 0)
                {
                    if (rule.Substring(0, 1) == "#" || rule == "") continue;

                    var or = ParseRule(new Tokeniser(rule), homeMt);
                    if (or == null) continue;
                    or.optHomeMt = homeMt;
                    ruleList.Add(or);
                    // print ("Rule "+outi+" is : ");
                    if (show) or.print(Console.Write);
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
                                string k = (((Variable)context.ArgList[i]).name);
                                //string v = ((Atom)value(new Variable(((Variable)context.alist[i]).name + ".0"), env)).ToString();
                                var part = value(new Variable(((Variable)context.ArgList[i]).name + ".0"), env);
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

        public void printContext(PartList which, PEnv env)
        {
            inGlobalTest();
            printVars(which, env);
        }

        private void inGlobalTest()
        {
            tl_spy_prolog_reader = true;
           /// throw ErrorBadOp("inGlobalTest");
        }
        #endregion
        #region interfaceUtils

        public static PartList termVarNames(Term t)
        {
            PartList outv = varNames(t.ArgList);
            if (t.headIsVar)
            {
                outv.AddPart(new Variable(t.name));
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
                        if (((Variable)outv.ArgList[j]).name == ((Variable)part).name) goto mainc;
                    //outv.InsertPart(outv.Length, plist.alist[i]);
                    outv.AddPart((Variable)part);
                }
                else if (((Part)part) is Term)
                {
                    PartList o2 = varNames(((Term)part).ArgList);

                    for (var j = 0; j < o2.Arity; j++)
                    {
                        for (var k = 0; k < outv.Arity; k++)
                            if (((Variable)o2.ArgList[j]).name == ((Variable)outv.ArgList[k]).name) goto innerc;
                        //outv.InsertPart(outv.Length, o2.alist[j]);
                        outv.AddPart(o2.ArgList[j]);
                    innerc: j = j;
                    }
                    if (((Term)part).headIsVar)
                    {
                        outv.AddPart(new Variable(((Term)part).name));
                    }
                }
                else if (((Part)part) is PartList)
                {
                    PartList o2 = varNames(((PartList)part));

                    for (var j = 0; j < o2.Arity; j++)
                    {
                        for (var k = 0; k < outv.Arity; k++)
                            if (((Variable)o2.ArgList[j]).name == ((Variable)outv.ArgList[k]).name) goto innerc2;
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
                return (T)(object)new Variable(((Variable)list).name + "." + level.ToString());
            }
            else if (list is Term)
            {
                //What if the pred is a variable ?
                var term = (Term)list;
                string nextName = term.name;
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
                Term outv = new Term(nextName,termheadIsVar, (PartList)renameVariables<PartList>(tpl, level, parent)) { excludeThis = term.excludeThis };
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
                deepestName = ((Term)goalList.ArgList[0]).name;
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
            if (thisTerm.name == "callMt")
            {
                // db.rules = findVisibleKBRules(queryMT);
                string queryMT = ((IAtomic) thisTerm.ArgList[0]).AsString();
                db = MakeQueryContext(queryMT, true, null);
                tl_mt = queryMT;
                thisTerm = thisTerm.ArgList[1] as Term;
            }
            else
            {
                db = dbIn;
            }

            // Do we have a builtin?

            builtinDelegate builtin = (builtinDelegate)PDB.builtin[thisTerm.name + "/" + thisTerm.Arity];

            //if (trace) { Console.Write("Debug: searching for builtin " + thisTerm.name + "/" + ((PartList)((PartList)thisTerm.partlist).list).length + "\n"); }
            if (builtin != null)
            {
                if (trace) { Console.Write("builtin with name " + thisTerm.name + " found; calling prove() on it...\n"); }
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
                if (db.index.ContainsKey(thisTerm.name))
                    localRules = db.index[thisTerm.name].arrayList;
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
                    tl_rule_mt = rule.optHomeMt ?? tl_rule_mt;
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
                        if (rulehead.name != thisTerm.name) continue;
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
                        renamedHead = new Term(rulehead.name, rulehead.headIsVar,
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
            Term newGoal = new Term(subgoal.name, false,
                                    (PartList) renameVariables(((PartList) subgoal), level, thisTerm));
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
                if ((conslist is Atom && conslist.name == FUNCTOR_NIL))
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
            while (second is Term && IsListName(((Term)second).name))
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
        #region prologObjects

        // Object (of a style...) definitions:
        // Rule = (Head, Body)
        // Head = Term
        // Body = [Term]
        // Term = (id, Parameters)
        // Parameters {Partlist} = [Part]
        // Part = Variable | Atom | Term

        public class PEnv : PlHashtable
        {

            public void print(Action<string> w)
            {
                foreach (string k in this.Keys)
                {
                    w(k); 
                    w(" = ");
                    this[k].print(w);
                    w("\n");
                }
            }
            public override string ToString()
            {
                string result = "";
                foreach (string k in this.Keys)
                {
                    result += String.Format("{0} = ", k) + ((Part)this[k]).ToSource(tl_console_language) + "\n";
                }
                return result;
            }
        }
        public class PlHashtable
        {
            private Hashtable ht = new Hashtable();
            public bool ContainsKey(string name)
            {
                return ht.ContainsKey(name);
            }
            public IEnumerable Keys
            {
                get { return ht.Keys; }
            }
            public Part this[string name]
            {
                get { return (Part)ht[name]; }
                set { ht[name] = value; }
            }

            public bool TryGetValue(string name, out Part o)
            {
                o = this[name];
                return o != null;
            }
        }

        public class RuleList : IEnumerable
        {
            internal List<Rule> arrayList = new List<Rule>();
            public static Func<Rule, Rule, bool> DefaultRuleEquality = SameClauses;

            private static bool SameClauses(Rule arg1, Rule arg2)
            {
                if (arg1 == null || arg2 == null)
                {
                    return arg1 == arg2;
                }
                return arg1.SameClause(arg2);
            }

            internal PDB syncPDB;
            /// <summary>
            /// Returns a <see cref="T:System.String"/> that represents the current <see cref="T:System.Object"/>.
            /// </summary>
            /// <returns>
            /// A <see cref="T:System.String"/> that represents the current <see cref="T:System.Object"/>.
            /// </returns>
            /// <filterpriority>2</filterpriority>
            public override string ToString()
            {
                string named = syncPDB != null ? (syncPDB.startMt+": ") : "ruleList: ";
                return named + "count=" + Count + ToSource(SourceLanguage.Prolog);
            }
            public string AToString
            {
                get { return ToString(); }
            }
            public int Count
            {
                get { lock (Sync) return arrayList.Count; }
            }

            public void Add(Rule r)
            {
                lock (Sync)
                {
                    arrayList.Add(r);
                    ClearPdbIndexes();
                }
            }
            public RuleList()
            {
            }

            public void RemoveAt(int i)
            {
                lock (Sync)
                {
                    Rule r = this[i];
                    Release(r);
                    arrayList.RemoveAt(i);
                    ClearPdbIndexes();
                }
            }

            private static void Release(Rule r)
            {
                var ruleCache = r.rdfRuleCache;
                if (ruleCache == null) return;
                r.rdfRuleCache = null;
                INode tripleInst = ruleCache.RuleNode;
                if (tripleInst != null && tripleInst.NodeType == NodeType.Blank)
                {
                    Release(tripleInst, ruleCache, r);
                    return;
                }
                //ConsoleWriteLine("Remove Rule: " + r);
                IGraph graph = ruleCache.ContainingGraph ?? tripleInst.Graph;
                IEnumerable<Triple> found = ruleCache.ToTriples;
                int fnd = 0;
                foreach (Triple triple in found)
                {
                 //   ConsoleWriteLine("Remove triple: " + triple);
                    triple.Graph.Retract(triple);
                    fnd++;
                }
            }

            private static void Release(INode tripleInst, RdfRules ruleCache, Rule r)
            {

                if (tripleInst.NodeType != NodeType.Blank)
                {
                    Warn("Removing non Bnode " + r);
                }
                //ConsoleWriteLine("Remove Rule: " + r);
                IGraph graph = ruleCache.ContainingGraph ?? tripleInst.Graph;
                IEnumerable<Triple> found = LockInfo.CopyOf(graph.GetTriples(tripleInst));
                int fnd = 0;
                foreach (Triple triple in found)
                {
                 //   ConsoleWriteLine("Remove triple: " + triple);
                    triple.Graph.Retract(triple);
                    fnd++;
                }
                //ConsoleWriteLine("Removed triples: " + fnd);
            }

            public Rule this[int i]
            {
                get { lock (Sync) return (Rule)arrayList[i]; }
                set
                {
                    lock (Sync)
                    {
                        var old = this[i];
                        if (ReferenceEquals(old, value)) return;
                        if (false && value == null)
                        {
                            // nulling should remove the item
                            arrayList.RemoveAt(i);
                            return;
                        }
                        arrayList[i] = value;
                        Release(old);
                        ClearPdbIndexes();
                    }
                }
            }

            public object Sync
            {
                get
                {
                    return arrayList;
                }
            }
            private void ClearPdbIndexes()
            {
                lock (Sync)
                {
                    if (syncPDB != null)
                    {
                        lock (syncPDB.index)
                        {
                            if (syncPDB.index.Count != 0)
                            {
                                syncPDB.index.Clear();
                            }
                        }
                    }
                }
            }
            public void Clear()
            {
                lock (Sync)
                {
                    foreach (Rule rule in arrayList)
                    {
                        Release(rule);
                    }
                    arrayList.Clear();
                    ClearPdbIndexes();
                }
            }

            public IEnumerator GetEnumerator()
            {
                lock (Sync) return arrayList.GetEnumerator();
            }

            public RuleList Copy()
            {
                var ret = new RuleList();
                lock (Sync) ret.arrayList.AddRange(arrayList);
                return ret;
            }

            public string ToSource(SourceLanguage language)
            {
                var ret = new StringWriter();
                lock (Sync) foreach (Rule rule in arrayList)
                {
                    ret.WriteLine(rule.ToSource(language));
                }
                return ret.ToString();
            }
            public bool Contains(Rule rule)
            {
                return IndexOf(rule, -1, DefaultRuleEquality) != -1;
            }
            public int IndexOf(Rule rule)
            {
                return IndexOf(rule, -1, DefaultRuleEquality);
            }

            public int IndexOf(int startAfter, Predicate<Rule> compare)
            {
                lock (Sync)
                {
                    RuleList rules = this;
                    for (int i = startAfter + 1; i < rules.Count; i++)
                    {
                        Rule r = (Rule)rules[i];
                        if (compare(r))
                        {
                            return i;
                        }
                    }
                }
                return startAfter;
            }

            public int IndexOf(Rule rule, int startAfter, Func<Rule, Rule, bool> compare)
            {
                return IndexOf(startAfter, r => compare(rule, r));
            }
        }
        public class PDB
        {
            /// <summary>
            /// Returns a <see cref="T:System.String"/> that represents the current <see cref="T:System.Object"/>.
            /// </summary>
            /// <returns>
            /// A <see cref="T:System.String"/> that represents the current <see cref="T:System.Object"/>.
            /// </returns>
            /// <filterpriority>2</filterpriority>
            public override string ToString()
            {
                return StructToString(this);
            }
            public string AToString
            {
                get { return ToString(); }
            }
            public string startMt;
            public bool followedGenlMt;
            public bool isStorage;
            static public Hashtable builtin = new Hashtable();
            private RuleList _rules;

            // A fast index for the database
            public Dictionary<string, RuleList> index = new Dictionary<string, RuleList>();

            public PDB(bool isStaticKB)
            {
                isStorage = isStaticKB;
                _rules = new RuleList();
                if (isStorage)
                {
                    _rules.syncPDB = this;
                }
            }

            public void initIndex()
            {
                var inx = index["_varpred_"] = new RuleList();
                var rules = this.rules;
                lock (rules) for (int i = 0; i < rules.Count; i++)
                    {
                        Rule rule = (Rule)rules[i];
                        string name = rule.head.name;
                        if (!index.ContainsKey(name)) { index[name] = new RuleList(); }
                        index[name].Add(rule);
                        if (rule.head.headIsVar)
                        {
                            inx.Add(rule);
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
                    _rules.syncPDB = this;
                }
            }

            public PDB db
            {
                get { return this; }
            }
        }

        public delegate Part PartReplacer(Part p, PartReplacer pr);

        public abstract class Part : IHasParent
        {
            public static bool operator ==(Part a, Part b)
            {
                var an = ReferenceEquals(a, null);
                var bn = ReferenceEquals(b, null);
                if (an || bn) return an && bn;
                return a.Equals(b);
            }
            public static bool operator !=(Part a, Part b)
            {
                return !(a == b);
            }
            /// <summary>
            /// Prolog Structure compare '=='/2
            /// </summary>
            /// <param name="term"></param>
            /// <param name="varlist"></param>
            /// <returns></returns>
            public abstract bool SameClause(Part term, IDictionary<string, string> varlist);
            /// <summary>
            /// Like Prolog '=='/2, but it requires variables to have the same identity
            /// </summary>
            /// <param name="obj"></param>
            /// <returns></returns>
            public abstract bool Equals(Part obj);
            sealed public override bool Equals(object obj)
            {
                if (!(obj is Part)) return false;
                return Equals((Part)obj);
            }

            public abstract int GetPlHashCode();
            sealed public override int GetHashCode()
            {
                return GetPlHashCode();
            }
            public virtual bool IsGround
            {
                get { return true; }
            }
            public virtual Part CopyTerm
            {
                get { return this; }
            }
            protected Exception Missing(string p)
            {
                return ErrorBadOp("{0} Missing '{1}' for {2}", type, p, this.ToSource(tl_console_language));
            }
            public abstract string type { get; }
            virtual public string name
            {
                get { throw Missing("Name"); }
            }
            virtual public object Functor0
            {
                get { throw Missing("Functor"); }
            }
            public virtual int Arity
            {
                get { throw Missing("Arity"); }
            }
            public virtual PartList ArgList
            {
                get { throw Missing("ArgList"); }
            }
            public virtual IHasParent TParent
            {
                set { }
                get { return null; }
            }

            public virtual void print(Action<string> w)
            {
                w(ToSource(tl_console_language));            
            }

            public abstract string ToSource(SourceLanguage language);

            sealed public override string ToString()
            {
                return ToSource(tl_console_language);
            }

            public virtual double AsDouble()
            {
                throw Missing("AsDouble");
            }

            public static void ConsolePrint(string format, params object[] args)
            {
                Console.Write(format, args);
            }

            public virtual void Visit(PartReplacer replacer)
            {
                return;
            }

            public virtual string AsString()
            {
                throw Missing("AsString");
            }
        }

        public class PartListImpl : Part, IEnumerable<Part>, IHasParent
        {
            //private string fuctor;
            public override IHasParent TParent
            {
                get
                {
                    var p = parent;
                    if (p != null && p.TParent != null)
                    {
                        p = p.TParent;
                    }
                    return p;
                }
                set
                {
                    parent = value;
                }
            }
            internal IHasParent parent;
            // Parameters {Partlist} = [Part]
            // Part = Variable | Atom | Term
            //public string name;

            public override bool SameClause(Part term, IDictionary<string, string> varlist)
            {
                var term2 = term as PartListImpl;
                if (term2 == null) return false;
                if (term2.Arity != Arity) return false;
                int i = 0;
                foreach (var s in this)
                {
                    if (!term2[i++].SameClause(s, varlist)) return false;
                }
                return true;
            }

            public override bool Equals(Part term)
            {
                var term2 = term as PartListImpl;
                if (term2 == null) return false;
                if (term2.Arity != Arity) return false;
                int i = 0;
                foreach (var s in this)
                {
                    if (!term2[i++].Equals(s)) return false;
                }
                return true;
            }

            public override int GetPlHashCode()
            {
                return Arity;
            }

            override public bool IsGround
            {
                get
                {
                    foreach (var t in this.tlist)
                    {
                        if (t is Atom) continue;
                        if (!t.IsGround)
                        {
                            return false;
                        }
                        if (t is Term)
                        {
                            return false;
                        }
                    }
                    return true;
                }
            }

            public override Part CopyTerm
            {
                get
                {
                    var pl = new PartListImpl();
                    foreach (var t in this)
                    {
                        pl.AddPart(t.CopyTerm);
                    }
                    return pl;
                }
            }

            public override string type { get { return "PartList"; } }
            private readonly IList<Part> tlist;
            public int renumber = 0;
            public int Count
            {
                get { return tlist.Count; }
            }

            public Part this[int i]
            {
                get
                {
                    if (i < 0 || i >= Count)
                    {
                        throw ErrorBadOp("inner partlist");
                    }
                    return (SIProlog.Part)tlist[i];
                }
                set
                {
                    if (i < 0 || i >= Count)
                    {
                        throw ErrorBadOp("inner partlist");
                    }
                    tlist[i] = value;
                }
            }
            public Part this[int i, bool safe]
            {
                get
                {
                    if (i < 0 || i >= Count)
                    {
                        throw ErrorBadOp("inner partlist");
                    }
                    return (SIProlog.Part)tlist[i];
                }
                set
                {
                    if (i < 0 || i >= Count)
                    {
                        throw ErrorBadOp("inner partlist");
                    }
                    tlist[i] = value;
                }
            }
            public void Add(SIProlog.Part part)
            {
                if (part is SIProlog.PartListImpl)
                {
                    throw ErrorBadOp("inner partlist");
                }
                tlist.Add(part);
            }

            public void Insert(int i, SIProlog.Part part)
            {
                if (i == Count)
                {
                    Add(part);
                    return;
                }
                throw ErrorBadOp("inserting outof order");
            }

            override public int Arity
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
                    return this;
                }
            }

            //public PartList(string head) { name = head; }
            public PartListImpl(params Part[] lS)
            {
                this.tlist = new List<Part>(lS);
            }
            public PartListImpl(IList<Part> parts)
            {
                this.tlist = parts;
            }

            public PartListImpl()
            {
                this.tlist = new List<Part>();
            }

            public PartListImpl(TermList head, PEnv env)
            {
                this.tlist = new List<Part>();
                for (var i = 0; i < head.Count; i++)
                {
                    tlist.Add(value(head[i], env));
                }

            }

            public override void print(Action<string> writer)
            {
                bool com = false;
                // ConsoleWrite("plist(");
                foreach (Part p in tlist.ToList())
                {
                    if (com) writer(", ");
                    p.print(writer);
                    com = true;
                }
                //  ConsoleWrite(")");
            }
            public override string ToSource(SourceLanguage language)
            {
                string result = "";
                bool com = false;
                //result = "plist(";
                foreach (Part p in tlist)
                {
                    if (com) result += (", ");
                    result += p.ToSource(language);
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
                if (i != tlist.Count)
                {
                    throw ErrorBadOp("out of order insertion");
                }
                tlist.Insert(i, part);
            }
            public override void Visit(PartReplacer func)
            {
                int argNum = 0;
                foreach (var arg in tlist.ToArray())
                {
                    var argr = func(arg, func);
                    if (ReferenceEquals(argr, null)) return;
                    if (!ReferenceEquals(argr, arg))
                    {
                        this.ArgList[argNum] = argr;
                    }
                    argNum++;
                }
            }

            public IEnumerator<Part> GetEnumerator()
            {
                return tlist.GetEnumerator();
            }

            IEnumerator IEnumerable.GetEnumerator()
            {
                return GetEnumerator();
            }

            #region IList<Part> Members

            public int IndexOf(Part item)
            {
                throw new NotImplementedException();
            }

            public void RemoveAt(int index)
            {
                throw new NotImplementedException();
            }

            #endregion

            #region ICollection<Part> Members


            public void Clear()
            {
                throw new NotImplementedException();
            }

            public bool Contains(Part item)
            {
                throw new NotImplementedException();
            }

            public void CopyTo(Part[] array, int arrayIndex)
            {
                throw new NotImplementedException();
            }

            public bool IsReadOnly
            {
                get { throw new NotImplementedException(); }
            }

            public bool Remove(Part item)
            {
                throw new NotImplementedException();
            }

            #endregion
        }

        public static Dictionary<INode, Atom> AtomTable = new Dictionary<INode, Atom>();

        public interface IAtomic
        {
            int CompareTo(IAtomic atomic);
            INode AsRDFNode();
            /// <summary>
            /// Tries if possible to return a IValuedNJode
            /// </summary>
            /// <returns></returns>
            IValuedNode AsValuedNode();
            string AsString();
            bool Unify(IAtomic atomic);
            double AsDouble();
            object Functor0 { get; }
            bool IsNode { get; }
        }

        public const string SYNTAX_UriQuotes = "<>"; // uri
        public const string SYNTAX_DoubleQuotes = "\"\""; // strings
        public const string SYNTAX_AtomQuotes = "''"; // atoms and unqualified names
        public const string SYNTAX_NoQuotes = ""; // numbers - if an atom is set to have no quotes that is just how it round trips
        public const string SYNTAX_LiteralDataType = "{}";
        public const string MustGuessQuotes = null;

        static public Part MakeTermPostReader(String f, bool isHeadVar, PartList partlist)
        {
            if (partlist.Arity == 0)
            {
                if (isHeadVar) return new Variable(f);
                return Atom.FromName(f);
            }
            if (f == "$obj")
            {
                Part partlist1 = partlist[0];
                if (partlist1.name == "$literal")
                {
                    return Atom.MakeLiteral(partlist[1].AsString(), partlist[2].AsString(), partlist[3].AsString());
                }
                Warn("Not sure how to create a " + partlist);
            }
            return new Term(f, isHeadVar, partlist);
        }
        public class Atom : AtomBase, IAtomic
        {
            public Atom(object head)
                : base(head)
            {
            }

            public static bool operator ==(Atom a, Atom b)
            {
                return NodeEquality(a.AsRDFNode(), b.AsRDFNode());
            }

            public static bool operator !=(Atom a, Atom b)
            {
                return !(a == b);
            }

            public static bool NodeEquality(INode x, INode y)
            {
                return x.Equals(y);
            }
        }
        abstract public class AtomBase : Part
        {
            public override bool SameClause(Part term, IDictionary<string, string> varlist)
            {
                var term2 = term as Atom;
                if (ReferenceEquals(term2, null)) return false;
                return this.Unify(term2);
            }

            public Object objRef;
            string aname;
            string rname;
            public string Namespace
            {
                get
                {
                    if (!IsUri) return null;
                    string localAname;
                    //if (aname != null) return aname;
                    string oprefix, ouri , path = AsValuedNode().AsString();
                    if(!GraphWithDef.DevolveURI(rdfDefinations.NamespaceMap, path, out ouri, out oprefix,
                                                            out localAname)) return null;

                    return oprefix;
                }
            }
            public string LocalName
            {
                get
                {
                    if (!IsUri) return null;
                    string localAname = aname;
                    if (localAname != null) return localAname;
                    string s = AsValuedNode().AsString();
                    string oprefix, ouri, path = s;
                    if (!GraphWithDef.DevolveURI(rdfDefinations.NamespaceMap, path, out ouri, out oprefix,
                                                 out localAname)) return null;

                    aname = localAname = string.Intern(localAname);
                    return localAname;
                }
            }
            private readonly Func<object, object> Functor0Function;
            public override string name
            {
                get
                {
                    if (aname != null) return aname;
                    var f = "" + Functor0;
                    if (f.Contains("#"))
                    {

                    }
                    aname = string.Intern(f);
                    return aname;
                }
            }
            public object Functor0
            {
                get
                {
                    if (Functor00==null)
                    {
                        Functor00 = Functor000;
                    }
                    return Functor00;
                }
            }
            private object Functor00;
            public object Functor000
            {
                get
                {
                    if (Functor0Function != null) return Functor0Function(objRef);
                    return objRef;
                }
            }
            static public object INodeToObject(object obj)
            {
                var _name = ToValueNode((INode) obj);
                if (!(_name is IValuedNode))
                {
                    var vnode = _name.AsValuedNode();
                    if (ReferenceEquals(null, vnode))
                    {
                    }
                    else
                    {
                        if (!ReferenceEquals(vnode, _name))
                        {
                            _name = vnode;
                        }
                    }
                }
                {
                    if (_name is StringNode)
                    {
                        return _name.AsValuedNode().AsString();
                    }
                    if (_name is BooleanNode)
                    {
                        return _name.AsValuedNode().AsBoolean();
                    }
                    if (_name is DateTimeNode)
                    {
                        return ((DateTimeOffset)_name.AsValuedNode().AsDateTime()).DateTime;
                    }
                    if (_name is NumericNode)
                    {
                        if (_name is LongNode)
                        {
                            return _name.AsValuedNode().AsInteger();
                        }
                        if (_name is UnsignedLongNode)
                        {
                            return (ulong)_name.AsValuedNode().AsInteger();
                        }
                        if (_name is SignedByteNode)
                        {
                            return (sbyte)_name.AsValuedNode().AsInteger();
                        }
                        if (_name is DoubleNode)
                        {
                            return _name.AsValuedNode().AsDouble();
                        }
                        if (_name is DecimalNode)
                        {
                            return _name.AsValuedNode().AsDecimal();
                        }
                        if (_name is FloatNode)
                        {
                            return _name.AsValuedNode().AsFloat();
                        }
                        if (_name is ByteNode)
                        {
                            return (byte)_name.AsValuedNode().AsInteger();
                        }
                    }
                    if (!(_name is IUriNode))
                    {
                        if (_name is IBlankNode)
                        {
                            return _name;
                        }
                        if (_name is ILiteralNode)
                        {
                            var vnode = _name.AsValuedNode();
                            return vnode.AsString();
                        }
                        return _name;
                    }
                    string localAname;
                    //if (aname != null) return aname;
                    string path = _name.AsValuedNode().AsString();
                    string prefix, uri;
                    bool devolved = GraphWithDef.DevolveURI(rdfDefinations.NamespaceMap, path, out uri, out prefix,
                                                            out localAname);
                    bool noaname = string.IsNullOrEmpty(localAname);
                    if (devolved && !noaname)
                    {
                        if (string.IsNullOrEmpty(prefix))
                        {
                            return path;
                        }
                        return localAname;
                    }
                    return path;
                }
            }
            private int? hash_code;
            public int hash { get { return GetHashCode(); } }
            public string quoted = null;
            public override string type { get { return "Atom"; } }
            public AtomBase(Object head)
            {
                //quoted = quoting;
                //aname = bareAtomName;
                objRef = head;
                if (head is INode)
                {
                    objRef = ToValueNode((INode)head);
                    Functor0Function = INodeToObject;
                    // call once to populate the data
                    if (RdfDeveloperSanityChecks > 1)
                    {
                        var localAname = "" + INodeToObject(objRef);
                    }
                }
                else if (head is string)
                {
                    aname = (string)head;
                    hash_code = aname.GetHashCode();
                }
                else
                {
                    Warn("unknown atom class: " + objRef.GetType());
                }
                string s = ToSource(SourceLanguage.Text);
                if (!IsString && s.Contains("http"))
                {
                    rname = null;
                    aname = null;

                }
            }
            private static bool MustBeQuoted(string localAname)
            {
                if (Regex.Match(localAname, @"^([a-z][a-zA-Z0-9_]*)$").Success)
                {
                    return false;
                }
                if (Regex.Match(localAname, @"^([-]?[0-9]*[\.]?[0-9]+[0-9]*)$").Success)
                {
                    return false;
                }
                return true;
            }

            public override void print(Action<string> w)
            {
                w(this.ToSource(tl_console_language));
            }

            public bool IsLiteral
            {
                get { return objRef is ILiteralNode; }
            }
            public bool IsNode
            {
                get { return objRef is INode; }
            }
            public bool IsValueNode
            {
                get { return objRef is IValuedNode; }
            }
            public bool IsString
            {
                get { return objRef is String || (!(objRef is IUriNode) && !(objRef is NumericNode)); }
            }
            public bool IsUri
            {
                get { return  objRef is IUriNode; }
            }
            public string IsReadable
            {
                get { return ToSource(tl_console_language) + " as " + objRef.GetType() + "=" + objRef; }
            }
            public override sealed string ToSource(SourceLanguage language)
            {
                if (language == SourceLanguage.Text)
                {
                    return name;
                }
                if (true || this.rname == null)
                {
                    this.rname = string.Intern(StringReadable);
                }
                return this.rname;
            }

            public bool IsLocalPrefix
            {
                get
                {
                    var node = objRef as IUriNode;
                    if (node != null)
                    {
                        if (node.ToString().StartsWith(RoboKindURI)) return true;
                    }
                    return false;
                }
            }
            public string StringReadable
            {
                get
                {
                    string name = this.name;
                    if (name == "nil")
                    {
                        return "[]";
                    }
                    if (objRef is INode)
                    {
                        var node1 = ToValueNode((INode)this.objRef);
                        if (node1 is NumericNode) return name;
                        var node = node1 as IUriNode;
                        if (node != null)
                        {
                            string url = null;
                            var ns = Namespace;
                            if (IsLocalPrefix || ns == RoboKindPrefix || ns == "siprolog")
                            {
                                return aq(LocalName);
                            }
                            if (ns != null)
                            {
                                url = ns + ":" + LocalName;
                            }
                            if (url == null)
                            {
                                url = node.ToString();
                            }
                            if (quoted == null || quoted.Length != 2)
                            {
                                return "<" + url + ">";
                            }
                            return quoted[0] + (url) + quoted[1];
                        }
                        if (node1 is IBlankNode)
                        {
                            return "<" + node1 + ">";
                        }
                        ILiteralNode litnode = node1 as ILiteralNode;
                        // all the below are now Literal Nodes of some type  (we divide into  "strings", numbers and "strings with"^"meaning" and 
                        if (litnode == null)
                        {
                            throw ErrorBadOp("Cant find the nodetype on  " + node);
                        }


                        var ivnode = litnode.AsValuedNode();
                        var value = litnode.Value;
                        var dt = litnode.DataType;
                        var lt = litnode.Language;
                        string et = String.Empty;// ivnode.EffectiveType;
                        if (dt != null) et = dt.AbsoluteUri;

                        if (lt == "" && (dt != null && dt.AbsoluteUri == XmlSpecsHelper.XmlSchemaDataTypeString))
                        {
                            return q(value);
                        }
                        return "'$obj'('$literal'," + q(value) + "," + q(lt) + "," + q(et) + ")";
                    }
                    if (quoted == null || quoted=="")
                    {
                        char fc = name[0];
                        if (char.IsLetter(fc) && char.IsLower(fc))
                        {
                            if (name.IndexOfAny("'\"?!@#$%^&*(){}:;/,<>".ToCharArray()) == -1 && !(name.IndexOfAny("+-".ToCharArray()) > 0))
                            {
                                if (MustBeQuoted(name))
                                {
                                    return q(name);
                                }
                                return name;
                            }
                        }
                        return q(name);
                        quoted = MustBeQuoted(name) ? SYNTAX_DoubleQuotes : "";
                    }
                    if (quoted == "")
                    {
                        return name;
                    }
                    return quoted[0] + name + quoted[1];
                }
            }

            private static string q(string s)
            {
                return "\"" + s.Replace("\\", "\\\\").Replace("\"", "\\\"") + "\"";
            }
            private static string aq(string s)
            {
                if (!Atom.MustBeQuoted(s)) return s;
                return "'" + s.Replace("\\", "\\\\").Replace("'", "\\'") + "'";
            }

            public static Atom MakeString(string s)
            {
                return MakeLiteral(s, "", XmlSpecsHelper.XmlSchemaDataTypeString);
            }

            public static Atom MakeLiteral(string s, string langspec, string dataType)
            {
                string syntax = SYNTAX_LiteralDataType;
                if (string.IsNullOrEmpty(dataType))
                {
                    syntax = SYNTAX_DoubleQuotes;
                    return MakeNodeAtom(rdfDefinations.CreateLiteralNode(s, langspec ?? ""));
                }
                return MakeNodeAtom(rdfDefinations.CreateLiteralNode(s, UriFactory.Create(dataType)));
            }

            public static Atom MakeUri(string s)
            {
                return MakeNodeAtom(rdfDefinations.CreateUriNode(UriFactory.Create(s)));
            }
            public static Atom FromName(string s)
            {
                return MakeNodeAtom(MakeNode(s, SYNTAX_AtomQuotes));
            }

            /// <summary>
            /// Reads the string that came in from the prolog reader and
            ///  then detects what rdf/sparql expresions 
            /// </summary>
            /// <param name="s"></param>
            /// <returns></returns>
            public static Atom FromSource(string s)
            {
                char c0 = s[0];
                int sl = s.Length;
                if (sl == 1) return FromName(s);

                string quoting = MustGuessQuotes;
                if (s == "[]" || s == FUNCTOR_NIL) s = "'rdf:nil'";
                //if (s == "." || s == FUNCTOR_CONS) s = "'robokind:cons'";
                INode makeNode = null;
                bool isNumberMaybe = c0 == '+' || c0 == '-' || char.IsDigit(c0);
                if (isNumberMaybe)
                {
                    makeNode = GraphWithDef.CExtracted(rdfDefinations, s);
                    if (makeNode != null)
                    {
                        return MakeNodeAtom(makeNode);
                    }
                    quoting = SYNTAX_NoQuotes;
                }
                Requote(ref s, ref quoting);
                return FromSourceReader(s, quoting);
            }

            private static void Requote(ref string s, ref string quoting)
            {
                char c0 = s[0];
                int sl = s.Length;
                char cL = s[sl - 1];
                if (sl == 1) cL = '\0';
                if (c0 == '"' && cL == c0)
                {
                    s = s.Substring(1, sl - 2);
                    quoting = SYNTAX_DoubleQuotes;
                }
                if (c0 == '\'' && cL == c0)
                {
                    s = s.Substring(1, sl - 2);
                    quoting = "" + c0 + cL;
                }
                if (c0 == '<' && cL == '>')
                {
                    s = s.Substring(1, sl - 2);
                    quoting = "" + c0 + cL;
                }
                if (c0 == '{' && cL == '}')
                {
                    // Warn("Tried to make atom from: {0}", s);
                    //return null;
                    s = s.Substring(1, sl - 2);
                    quoting = "<>";// +c0 + cL;
                }
            }

            public static Atom FromSourceReader(string s, string syntaxQuoting)
            {
                var prev = tl_console_language;
                try
                {
                    return FromSource0(s, syntaxQuoting);
                }
                finally
                {
                    tl_console_language = prev;
                }
            }
            public static Atom FromSource0(string s, string quoting)
            {
                if (s == "[]" || s == FUNCTOR_NIL)
                {
                    s = "rdf:nil";
                }               
                if (quoting==MustGuessQuotes)
                {
                    if (s.Contains(":") || s.Contains("/"))
                    {
                        quoting = SYNTAX_UriQuotes;
                    }
                }
                if (s == null)
                {
                    Warn("FromSource read NULL");
                    return null;
                }
                if (s == "")
                {
                    if (quoting == SYNTAX_DoubleQuotes)
                    {
                        return MakeString(s);
                    }
                    Warn("FromSource read EOF");
                    return null;
                }
                if (quoting == SYNTAX_DoubleQuotes)
                {
                    return MakeString(s);
                }
                if (quoting == null && s[0] == '$')
                {
                    quoting = SYNTAX_AtomQuotes;
                }
                return MakeNodeAtom(MakeNode(s, quoting));
            }

            public static INode MakeNode(string s, string quoting)
            {
                switch (quoting)
                {
                    case SYNTAX_DoubleQuotes:
                        {
                            return Atom.MakeString(s).AsRDFNode();
                        }
                    case "":
                    case null:
                        {
                            if (IsVarName(s))
                            {
                                return rdfDefinations.CreateVariableNode(s);
                            }
                            return GraphWithDef.CExtracted(rdfDefinations, s);
                        }
                    case "{}":
                    case "<>":
                    case "''":
                        {
                            return GraphWithDef.C(rdfDefinations, s);
                        }
                    default:
                        return GetValuedNode(s);
                        throw ErrorBadOp(s + " " + quoting);
                }
            }

            public static Atom MakeNodeAtom(INode makeNode)
            {
                string s = makeNode.ToString();
                lock (AtomTable)
                {
                    //string atomKey = quoting.ToString() + s;
                    Atom atom = null;
                    if (true ||
                        (!AtomTable.TryGetValue(makeNode, out atom) ||
                         (makeNode != null && (atom.objRef != null && atom.objRef != makeNode))))
                    {
                        atom = new Atom(makeNode);
                        //AtomTable[atomKey] = atom;
                        return atom;
                    }
                    return atom;
                }
            }

            override public string AsString()
            {
                return AsValuedNode().AsString();
            }

            public bool Unify(IAtomic atomic)
            {
                if (ReferenceEquals(null, atomic))
                {
                    return false;
                }
                if (Functor0.Equals(atomic.Functor0))
                {
                    return true;
                }
                return false;
            }

            public override double AsDouble()
            {
                return AsValuedNode().AsDouble();
            }

            public IValuedNode AsValuedNode()
            {
                var node = AsRDFNode();
                if (node == null)
                {
                    Warn("not a valuednode " + this);
                }
                return node.AsValuedNode();
            }

            public int CompareTo(IAtomic atomic)
            {
                return AsRDFNode().CompareTo(atomic.AsRDFNode());
            }

            public INode AsRDFNode()
            {
                if (objRef is INode) return ((INode) objRef);
                Warn("Cant make an RDF node");
                return null;
            }

            /// <summary>
            /// Determines whether the specified <see cref="T:System.Object"/> is equal to the current <see cref="T:System.Object"/>.
            /// </summary>
            /// <returns>
            /// true if the specified <see cref="T:System.Object"/> is equal to the current <see cref="T:System.Object"/>; otherwise, false.
            /// </returns>
            /// <param name="obj">The <see cref="T:System.Object"/> to compare with the current <see cref="T:System.Object"/>. 
            ///                 </param><exception cref="T:System.NullReferenceException">The <paramref name="obj"/> parameter is null.
            ///                 </exception><filterpriority>2</filterpriority>
            public override bool Equals(Part obj)
            {
                return Equals(obj as IAtomic);
            }
            public bool Equals(IAtomic other)
            {
                if (ReferenceEquals(null, other)) return false;
                if (ReferenceEquals(this, other)) return true;
                if (IsNode && other.IsNode)
                {
                    INode mynode = AsRDFNode();
                    INode othernode = other.AsRDFNode();
                    if (mynode.Equals(othernode))
                    {
                        return true;
                    }
                    return false;
                }
                return Equals(other.Functor0, Functor0);
            }


            public override int GetPlHashCode()
            {
                if (!hash_code.HasValue)
                {
                    hash_code = (objRef != null ? objRef.GetHashCode() : 0);
                }
                return hash_code.Value;
            }
        }
        public class Variable : Part
        {
            public string _name;
            public bool Anonymous = false;
            public override string name { get { return _name; } }
            public override string type { get { return "Variable"; } }
            public Variable(string head)
            {
                if (head == null || head == "_")
                {
                    Anonymous = true;
                    head = "ANON_" + SIProlog.CONSP;
                }
                _name = head;
            }
            public override void print(Action<string> w) { w(ToSource(tl_console_language)); }
            public override string ToSource(SourceLanguage language)
            {
                if (language != SourceLanguage.Prolog) return "?" + this.name;
                if (Anonymous) return "_";
                return this.name;
            }

            override public bool IsGround
            {
                get { return false; }
            }
            public override bool Equals(Part term)
            {
                if (Object.ReferenceEquals(this, term)) return true;
                var term2 = term as Variable;
                if (term2 == null) return false;
                string thisname = this.name;
                string term2name = term2.name;
                if (term2name == thisname)
                {
                    return true;
                } 
                return false;
            }
            public override int GetPlHashCode()
            {
                return name.GetHashCode();
            }
            public override bool SameClause(Part term, IDictionary<string, string> varlist)
            {
                if (Object.ReferenceEquals(this, term)) return true;
                var term2 = term as Variable;
                if (term2 == null) return false;
                if (varlist == null)
                {
                    if (Anonymous) return term2.Anonymous;
                    return false;
                }
                return SameVar(term2.name, this.name, varlist);
            }
        }
        public static bool SameVar(string term2name, string thisname, IDictionary<string, string> varlist)
        {
            if (term2name == thisname) return true;
            string thisothername;
            if (!varlist.TryGetValue(thisname, out thisothername))
            {
                if (varlist.ContainsKey(term2name)) return false;
                varlist[thisname] = term2name;
                varlist[term2name] = thisname;
                return true;
            }
            return thisothername == term2name;
        }
        public static bool IsListName(string s)
        {
            return s == "cons" || s == "." || s == FUNCTOR_CONS;
        }
        public static bool IsList(Part x)
        {
            return x is Term && IsListName(x.name) && x.Arity == 2;
        }
        public interface IHasParent
        {
            IHasParent TParent { get; set; }
        }
        public class Term : Part, IHasParent
        {
            public override bool SameClause(Part term, IDictionary<string, string> varlist)
            {
                var term2 = term as Term;
                if (term2 == null) return false;
                if (headIsVar)
                {
                    if (!term2.headIsVar) return false;
                    if (!SameVar(term2.name, this.name, varlist)) return false;
                }
                else
                {
                    if (term2.headIsVar) return false; 
                    if (term2.name != this.name) return false;
                }
                return ArgList.SameClause(term2.ArgList, varlist);
            }
            public override bool Equals(Part term)
            {
                var term2 = term as Term;
                if (term2 == null) return false;
                if (term2.Arity != Arity) return false;
                if (term2.headIsVar != headIsVar) return false;
                if (term2.name != this.name) return false;
                return ArgList.Equals(term2.ArgList);
            }
            public override int GetPlHashCode()
            {
                return name.GetHashCode() ^ Arity;
            }
            //readonly public string _name;
            //public override string name { get { return _name; } }
            public override string type { get { return "Term"; } }

            public override TermList ArgList
            {
                get { return partlist0.ArgList; }
            }

            override public int Arity
            {
                get
                {
                    if (partlist0 == null)
                    {
                        return base.Arity;
                    }
                    return partlist0.Arity;
                }
            }

            override public bool IsGround
            {
                get { return partlist0.IsGround && !headIsVar; }
            }
            public override Part CopyTerm
            {
                get
                {
                    return new Term(name, headIsVar, (PartList)partlist0.CopyTerm) { parent = null, excludeThis = excludeThis };
                }
            }
            private Part _pred = null;
            public readonly PartList partlist0;
            public bool excludeThis = false;
            public int excludeRule = -1;
            public bool cut = false;
            public IHasParent parent = null;
            public override string name
            {
                get
                {
                    {
                    }
                    return _name;
                }
            }

            private string _name;
            public bool headIsVar;
            public Term(string head, bool isVar, PartList a0N)
            {
                _name = head;
                headIsVar = isVar;
                partlist0 = a0N;
                var isVarName = SIProlog.IsVarName(head);
                if (a0N == null)
                {
                    throw ErrorBadOp("Term Arglist NULL: {0}", head);
                }
                if (isVar)
                {
                    if (!isVarName) throw ErrorBadOp("Pred was supposed to be variable: {0}", this);
                }
                else
                {
                    if (isVarName) throw ErrorBadOp("Pred was NOT supposed to be variable: {0}", this);
                }
                if (a0N.Arity > 0)
                {
                    Part a0 = a0N.ArgList[0];
                    if (a0 is PartList)
                    {
                        Warn("Poorly constructed term: {0}", this);
                    }
                }
                a0N.TParent = this;
            }
           

            public override string ToSource(SourceLanguage language)
            {
                language = language.Inner();
                string result = "";
                if (IsListName(this.name) && Arity == 2)
                {
                    Part x = this;
                    {
                        result += "[";
                        var com = false;
                        while (IsList(x))
                        {
                            if (com) result += ", ";
                            result += x.ArgList[0].ToSource(language); // May need to case var/atom/term
                            com = true;
                            x = x.ArgList[1];
                        }
                        if (x.ToSource(SourceLanguage.Prolog) != "[]")
                        {
                            result += " | ";
                            result += x.ToSource(language);
                        }
                        result += "]";
                        return result;
                    }
                }
                if (Arity == 0)
                {
                    if (name == "cut")
                    {
                        return "!";
                    }
                    return ReadableName;
                }
                PartListImpl argList = this.ArgList;
                if (argList == null)
                {
                    return result + "()";
                }
                result += "" + ReadableName + "(";
                result += argList.ToSource(language);
                result += ")";
                return result;
            }

            protected string ReadableName
            {
                get
                {
                    var name = this.name;
                    if (headIsVar) return name;
                    if (IsVarName(name)) return "'" + name + "'";
                    return name;
                }
            }

            override public void Visit(PartReplacer func)
            {
                ArgList.Visit(func);
                /*
                int argNum = 0;
                foreach (var arg in Args)
                {
                    var argr = func(arg, func);
                    if (ReferenceEquals(argr, null)) return;
                    if (!ReferenceEquals(argr, arg))
                    {
                        this.ArgList[argNum] = argr;
                    }
                    argNum++;
                }
               */
            }

            #region IHasParent Members

            public override IHasParent TParent
            {
                get
                {
                    return parent;
                }
                set
                {
                    parent = value;
                }
            }

            #endregion
        }

        public static bool IsVarName(string name)
        {
            // should be [A-Z\_\?]
            if (name.Length == 0) return false;
            char firstChar = name[0];
            if (firstChar == '?' || firstChar == '_')
            {
                return true;
            }
            return char.IsLetter(firstChar) && char.IsUpper(firstChar);
        }


        public partial class Rule : IHasParent
        {
            public bool isGround = false;
            public string optHomeMt;
            // Rule = (Head, Body)
            readonly public Term head = null;
            public Body body = null;
            public Rule(Term head)
            {
                this.head = head;
                isGround = head.IsGround;
                head.parent = this;
            }
            public Rule(Term head, PartList bodylist)
                : this(head)
            {
                if (bodylist != null)
                {
                    this.body = new Body(bodylist);
                    body.parent = this;
                }
                else
                    this.body = null;
            }
            public void print(Action<string> w)
            {
                if (this.body == null)
                {
                    this.head.print(w);
                    w(".");
                }
                else
                {
                    this.head.print(w);
                    w(" :- ");
                    this.body.print(w);
                    w(".");
                }
            }

            public override string ToString()
            {
                return ToSource(tl_console_language ?? SourceLanguage.Prolog);
            }
            public string ToSource(SourceLanguage language)
            {
                if (this.body == null)
                {
                    return this.head.ToSource(language) + ".";
                }
                else
                {
                    return this.head.ToSource(language) + " :- " + this.body.ToSource(language) + ".";
                }
            }

            public IHasParent TParent
            {
                get
                {
                    var p = parent;
                    if (p != null && p.TParent != null)
                    {
                        p = p.TParent;
                    }
                    return p;
                }
                set
                {
                    parent = value;
                }
            }
            internal IHasParent parent;
        }

        public class Body : IHasParent
        {
            #region IHasParent Members

            public IHasParent TParent
            {
                get
                {
                    var p = parent;
                    if (p != null && p.TParent != null)
                    {
                        p = p.TParent;
                    }
                    return p;
                }
                set
                {
                    parent = value;
                }
            }
            internal IHasParent parent;

            #endregion
            // Body = [Term]

            readonly public PartListImpl plist = null;
            public Body(PartListImpl l)
            {
                plist = l;
                plist.parent = this;
            }
            public void print(Action<string> w)
            {
                for (var i = 0; i < this.plist.Arity; i++)
                {
                    ((Term)this.plist.ArgList[i]).print(w);
                    if (i < this.plist.Arity - 1)
                        w(", ");
                }
            }
            public override string ToString()
            {
                return ToSource(tl_console_language);
            }

            public string ToSource(SourceLanguage language)
            {
                string result = "";

                for (var i = 0; i < this.plist.Arity; i++)
                {
                    result += ((Term)this.plist.ArgList[i]).ToSource(language);
                    if (i < this.plist.Arity - 1)
                        result += ", ";
                }
                return result;
            }
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


        #region prologParser
        // The Tiny-Prolog parser goes here.
        public class Tokeniser
        {
            public override string ToString()
            {
                return "now=" + currentTok + " before=" + previousToks + " remainder:" + remainder;
            }
            public string remainder;
            public string typeSyntax;
            public string type;
            public string current;
            public string previousToks = "";

            public string prev_typeSyntax;
            public string prev_type;
            public string prev_current;
            public string initial;

            public string currentTok
            {
                get
                {
                    return "[" + type + ",\"" + current + ",\"" + typeSyntax + "\"],";
                }
            }
            public Tokeniser(Tokeniser copyThis)
            {
                this.initial = copyThis.initial;
                this.previousToks = copyThis.previousToks;
                this.prev_current = copyThis.prev_current;
                this.prev_type = copyThis.prev_type;
                this.prev_typeSyntax = copyThis.prev_typeSyntax;
                this.remainder = copyThis.remainder;
                this.current = copyThis.current;
                this.typeSyntax = copyThis.typeSyntax;
                this.type = copyThis.type;
            }
            public Tokeniser Clone()
            {
                return new Tokeniser(this);
            }
            public Tokeniser(string input)
            {
                this.initial = input;
                Init(input);
            }

            public void Init(string input)
            {
                previousToks = null;
                this.prev_current = null;
                this.prev_type = null;
                this.prev_typeSyntax = null;
                this.remainder = input;
                this.current = null;
                this.typeSyntax = null;
                this.type = null; // "eof", "id", "var", "punc" etc.
                this.consume0(); // Load up the first token.
            }

            public void consume()
            {
                prev_current = current;
                prev_type = type;
                prev_typeSyntax = typeSyntax;
                previousToks = previousToks + currentTok;
                consume0();
            }
            internal void consume0()
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
                    this.typeSyntax = SYNTAX_NoQuotes;
                    this.type = "eof";
                    return;
                }

                // Decimal numbers book suggests: @"^([-+]?[0-9]*\.[0-9]+|[0-9]+)(.*)$".
                r = Regex.Match(this.remainder, @"^([-]?[0-9]*[\.]?[0-9]+[0-9]*)(.*)$");
                //r = this.remainder.match(/^(-[0-9][0-9]*)(.*)$/);
                if (r.Success)
                {
                    this.remainder = r.Groups[2].Value;
                    this.current = r.Groups[1].Value;
                    this.typeSyntax = SYNTAX_NoQuotes;
                    this.type = "id";
                    return;
                }



                r = Regex.Match(this.remainder, @"^([\(\)\.,\[\]\|\!]|\:\-)(.*)$");
                //r = this.remainder.match(/^([\(\)\.,\[\]\|\!]|\:\-)(.*)$/);
                if (r.Success)
                {
                    this.remainder = r.Groups[2].Value;
                    this.current = r.Groups[1].Value;
                    this.typeSyntax = SYNTAX_NoQuotes;
                    this.type = "punc";
                    return;
                }

                r = Regex.Match(this.remainder, @"^([A-Z_\?][a-zA-Z0-9_\?\-]*)(.*)$");
                //r = this.remainder.match(/^([A-Z_][a-zA-Z0-9_]*)(.*)$/);
                if (r.Success)
                {
                    this.remainder = r.Groups[2].Value;
                    this.current = r.Groups[1].Value;
                    this.typeSyntax = SYNTAX_NoQuotes;
                    this.type = "var";
                    return;
                }
                ParseType[] parseTypes = {
                                               new ParseType("{}", true, true), 
                                               new ParseType(SYNTAX_UriQuotes, false, true),
                                               new ParseType(SYNTAX_DoubleQuotes, true, true),
                                               new ParseType(SYNTAX_AtomQuotes, false, false)
                                           };

                int remLen = remainder.Length;
                foreach (ParseType parseType in parseTypes)
                {
                    if (!this.remainder.StartsWith(parseType.StartSequence)) continue;
                    int remIndex = parseType.StartSequence.Length;
                    if (parseType.CantHaveNext.Contains(remainder[remIndex])) continue;
                    string endSeq = parseType.EndSequence;
                    int endLen = endSeq.Length;
                    if (remLen < endLen) continue;
                    char endChar = '\0';
                    if (endLen > 0)
                    {
                        endChar = endSeq[0];
                    }
                    string soFar = "";
                    string isError = null;
                    do
                    {
                        char next = remainder[remIndex];
                        if (parseType.EscapeChars.Contains(next))
                        {
                            remIndex++;
                            next = remainder[remIndex];
                            soFar += next;
                            continue;
                        }
                        if (next == endChar && (endLen == 1 || remainder.Substring(next).StartsWith(endSeq)))
                        {
                            // found endchar
                            isError = null;
                            break;
                        }
                        if (parseType.CantHave.Contains(next))
                        {
                            isError = string.Format("CantHaveChar: '{0}'", next);
                            break;
                        }
                        soFar += next;
                        remIndex++;
                    } while (remIndex < remLen);

                    if (!string.IsNullOrEmpty(isError))
                    {
                        if (tl_spy_prolog_reader)
                        {
                            Warn("not using quote type becasue: " + isError);
                        }
                        continue;
                    }
                    int remainderLen = remainder.Length;
                    if (remIndex + 1 > remainderLen)
                    {
                        if (tl_spy_prolog_reader)
                        {
                            Warn("not using quote type becasue: is is past end of file: " + this);
                        }
                        continue;
                    }
                    current = soFar;
                    remainder = remainder.Substring(remIndex + 1);
                    type = parseType.typeName;
                    typeSyntax = parseType.quoteName;
                    return;
                }


                // URLs in curly-bracket pairs
                r = Regex.Match(this.remainder, @"^(\{[^\}]*\})(.*)$");
                //r = this.remainder.match(/^(\{[^\}]*\})(.*)$/);
                if (r.Success)
                {
                    this.remainder = r.Groups[2].Value;
                    this.current = r.Groups[1].Value;
                    this.typeSyntax = "{}";
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
                    this.typeSyntax = SYNTAX_DoubleQuotes;
                    this.type = "id";
                    return;
                }

                // Strings of alphanumerics with "_" and ":" 
                r = Regex.Match(this.remainder, @"^([a-zA-Z0-9][a-zA-Z0-9_\:]*)(.*)$");
                //r = this.remainder.match(/^([a-zA-Z0-9][a-zA-Z0-9_]*)(.*)$/);
                if (r.Success)
                {
                    this.remainder = r.Groups[2].Value;
                    this.current = r.Groups[1].Value;
                    this.typeSyntax = SYNTAX_NoQuotes;
                    this.type = "id";
                    return;
                }

                // negative integers
                r = Regex.Match(this.remainder, @"^(-[0-9][0-9]*)(.*)$");
                //r = this.remainder.match(/^(-[0-9][0-9]*)(.*)$/);
                if (r.Success)
                {
                    this.remainder = r.Groups[2].Value;
                    this.current = r.Groups[1].Value;
                    this.typeSyntax = SYNTAX_NoQuotes;
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
                    this.typeSyntax = SYNTAX_NoQuotes;
                    this.type = "id";
                    return;
                }

                if (tl_spy_prolog_reader)
                {
                    Warn("Unconsumed " + remainder);
                    return;
                }
                
                this.current = null;               
                this.type = "eof";
                this.typeSyntax = SYNTAX_NoQuotes;

            }
        }



        public Rule ParseRule(Tokeniser tk, string mt)
        {
            // A rule is a Head followed by . or by :- Body

            var p = ParseHead(tk, mt);
            if (p == null)
            {
                prolog_reader_debug("cant parse Rule head: " + tk);
                return null;
            }
            Term h = p as Term;
            if (h == null)
            {
                if (!(p is Atom))
                {
                    prolog_reader_debug("didn't parse Rule head as atom or term: " + tk);
                    // return null;
                }
                h = AsTerm(p);
            }
            if (tk.current == ".")
            {
                // A simple rule.
                tk.consume();
                return new Rule(h);
            }

            if (tk.current != ":-")
            {
                prolog_reader_debug("Rule neck missing: " + tk); 
                return null;
            }
            tk.consume();
            PartList b = ParseBody(tk, mt);
            if (b == null)
            {
                prolog_reader_debug("Rule body not present: " + tk);
                return null;
            }
            if (tk.current == "." || tk.current == "eof")
            {
                tk.consume();
                return new Rule(h, b);
            }
            prolog_reader_debug("Rule body not terminated: " + tk); 
            return null;
        }

        static Term AsTerm(Part p)
        {
            Term h = p as Term;
            if (h == null)
            {
                if (!(p is Atom))
                {
                    Warn("Not an Atom: " + p);
                    // return null;
                }
                h = new Term(p.name, p is Variable, new PartListImpl());
            }
            return h;
        }

        public Term ParseHead(Tokeniser tk, string mt)
        {
            // A head is simply a term. (errors cascade back up)
            return ParseTerm(tk, mt);
        }

        static public Term ParseTerm(Tokeniser tk, string mt)
        {
            Part p = ParsePart(tk);
            return AsTerm(p);
        }

        // This was a beautiful piece of code. It got kludged to add [a,b,c|Z] sugar.
        static public Part ParsePart(Tokeniser tk)
        {
            string mt = CurrentProlog.curKB;
            // Part -> var | id | id(optParamList)
            // Part -> [ listBit ] ::-> cons(...)     
            if (tk.type == "punc" && tk.current == "[")
            {
                // Parse a list (syntactic sugar goes here)
                tk.consume();
                // Special case: [] = Atom.Make(nil).
                if (tk.type == "punc" && tk.current == "]")
                {
                    tk.consume();
                    return Atom.FromSource(FUNCTOR_NIL);
                }

                // Get a list of parts into l
                ArrayList l = new ArrayList();
                int i = 0;

                while (true)
                {
                    var t = ParsePart(tk);
                    if (t == null)
                    {
                        prolog_reader_debug("cant parse List Part " + tk);
                        return null;
                    }

                    l.Insert(i++, t);
                    if (tk.current != ",") break;
                    tk.consume();
                }

                // Find the end of the list ... "| Var ]" or "]".
                Part append;
                if (tk.current == "|")
                {
                    tk.consume();
                    append = ParsePart(tk);
                }
                else
                {
                    append = Atom.FromSource(FUNCTOR_NIL);
                }
                if (tk.current != "]")
                {
                    prolog_reader_debug("Unclosed List " + tk);
                    return null;
                }
                tk.consume();
                // Return the new cons.... of all this rubbish.
                for (--i; i >= 0; i--)
                {
                    append = MakeList((Part)l[i], append);
                }
                return append;
            }
            var notthis = false;
            if (tk.current == "NOTTHIS")
            {
                notthis = true;
                tk.consume();
            }
            var type = tk.type;
            var name = tk.current;
            var quotingType = tk.typeSyntax;
            tk.consume();
            if (quotingType == SYNTAX_DoubleQuotes)
            {
                return Atom.FromSourceReader(name, quotingType);
            }
            // fail shorthand for fail(), ie, fail/0
            if (type == "id" && name == "fail")
            {
                return MakeTerm("fail");
            }
            // Parse ! as cut/0
            if (type == "punc" && name == "!")
            {
                return MakeTerm("cut");
            }
            // Parse [id|var](ParamList)
            if (tk.current == "(")
            {
                tk.consume();

                PartList p = ParseConjuncts(tk, mt, ")", ",", false);
                if (p == null)
                {
                    prolog_reader_debug("cant read termargs " + tk);
                    return null;
                }
                Part term = MakeTermPostReader(name, type == "var", p);
                if (term is Term)
                {
                    if (notthis) ((Term) term).excludeThis = true;
                }
                return term;
            }
            if (type == "var")
            {
                return new Variable(name);
            }
            return Atom.FromSourceReader(name, quotingType);
        }
        static public PartList ParseConjuncts(Tokeniser tk, string mt, string requiredEnd, string sep, bool eofReturnsPart)
        {
            // Body -> Term {, Term...}

            PartList p = new PartList();
            var i = 0;
            while (true)
            {
                if (tk.current == requiredEnd)
                {
                    tk.consume();
                    break;
                }
                if (tk.current == "eof")
                {
                    if (!eofReturnsPart)
                    {
                        prolog_reader_debug("read EOF " + tk);
                        return null;
                    }
                    break;
                }
                if (tk.current == sep)
                {
                    tk.consume();
                    continue;
                }
                Part t = ParsePart(tk);
                if (t == null)
                {
                    prolog_reader_debug("cant read Conjuct item " + tk);
                    return null;
                }
                p.AddPart(t);
                i++;
            }

            if (i == 0)
            {
                prolog_reader_debug("no items read " + tk);
                return null;
            }
            return p;
        }


        [ThreadStatic]
        public static bool tl_spy_prolog_reader = false;

        private static void prolog_reader_debug(string why)
        {
            if (!tl_spy_prolog_reader) return;
            Warn("prolog_reader_debug: " + why);
        }

        static private Part MakeList(Part li, Part append)
        {
            return MakeTerm(FUNCTOR_CONS, li, append);
        }
        public PartList ParseBody(string query, string mt)
        {
            return ParseBody(new Tokeniser(query), mt);
        }
        public PartList ParseBody(Tokeniser tk, string mt)
        {
            // Body -> Term {, Term...}

            PartList p = new PartList();
            var i = 0;

            Term t;
            while ((t = (Term)ParseTerm(tk, mt)) != null)
            {
                p.AddPart(t);
                i++;
                if (tk.current != ",") break;
                tk.consume();
            }

            if (i == 0)
            {
                return null;
            }
            return p;
        }
        #endregion
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
                    w(((Variable)which.ArgList[i]).name);
                    w(" = ");
                    //((Atom)value(new Variable(((Variable)which.alist[i]).name + ".0"), environment)).print();
                    value(new Variable(((Variable)which.ArgList[i]).name + ".0"), environment).print(w);
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
                return new Term(x.name, ((Term) x).headIsVar, p);
            }
            if (x is PartList)
            {
                PartList p = new PartList(x.ArgList, env);
                return p;
            }
            if (!(x is Variable))
                return x;		// We only need to check the values of variables...

            Part binding; //** HASH/DICTIONARY NEEDED ** 
            if (!env.TryGetValue(x.name, out binding))
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
                return newEnv(x.name, y, env);
            }
            if (y is Variable)
            {
                if (trace) ConsoleWriteLine("     MATCH");
                return newEnv(((Variable)y).name, x, env);
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
                    if (x.name != y.name)
                        return null; // Ooh, so first-order.
                    return unify(x.ArgList, y.ArgList, env);

                }

                if (xvar && yvar)
                {
                    if (!x.ArgList.IsGround || !y.ArgList.IsGround) return null;
                    PEnv subEnv = unify(new Variable(x.name), new Variable(y.name), env);
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
                    PEnv subEnv = unify(Atom.FromName(x.name), new Variable(y.name), env);
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

        #region mtGraph

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
                if (startNode == endNode)
                {
                    throw new NullReferenceException("Trying to connect a KB to itself " + startNode);
                }
                this.startNode = startNode;
                this.endNode = endNode;
                this.startNode.AddOutgoingEdge(this);
                this.endNode.AddIncomingEdge(this);

                this.info = info;
            }

            public bool Equals(PEdge other)
            {
                if (ReferenceEquals(null, other)) return false;
                if (ReferenceEquals(this, other)) return true;
                if (other.StartNode == StartNode && other.EndNode == EndNode)
                {
                    return true;
                }
                return false;
            }

            public override bool Equals(object obj)
            {
                if (base.Equals(obj)) return true;
                return Equals(obj as PEdge);
            }


            public override int GetHashCode()
            {
                unchecked
                {
                    int result = (startNode != null ? startNode.GetHashCode() : 0);
                    result = (result * 397) ^ (endNode != null ? endNode.GetHashCode() : 0);
                    return result;
                }
            }
        }

        public class PGraph
        {
            List<PNode> topLevelNodes = new List<PNode>();

            public void ClearInConnections(string idSrc)
            {
                PNode srcNode = FindOrCreateNode(idSrc);
                srcNode.ClearIncomingEdges();
            }
            public void ClearOutConnections(string idSrc)
            {
                PNode srcNode = FindOrCreateNode(idSrc);
                srcNode.ClearOutgoingEdges();
            }

            public void Connect(string idSrc, string idDest)
            {
                PNode srcNode = FindOrCreateNode(idSrc);
                PNode destNode = FindOrCreateNode(idDest);

                if (destNode == srcNode)
                {
                    return;
                }
                lock (srcNode.EdgeLists) lock (destNode.EdgeLists)
                    {
                        if (!srcNode.EdgeAlreadyExists(destNode))
                            srcNode.CreateEdgeTo(destNode);
                    }
            }
            public void Disconnect(string idSrc, string idDest)
            {
                PNode srcNode = FindOrCreateNode(idSrc);
                PNode destNode = FindOrCreateNode(idDest);

                if (destNode == srcNode)
                {
                    return;
                }
                if (!srcNode.EdgeAlreadyExists(destNode)) return;
                srcNode.RemoveEdgeTo(destNode);
            }

            private PNode FindOrCreateNode(string idSrc)
            {
                PNode srcNode = Contains(idSrc);
                if (srcNode == null)
                {
                    srcNode = SIProlog.CurrentProlog.MakeRepositoryKB(idSrc);// //new PNode(startMT);
                    //srcNode = new PNode(idSrc);
                    //AddNode(srcNode);
                }
                return srcNode;
            }

            public PNode[] TopLevelNodes
            {
                get { lock (topLevelNodes) return topLevelNodes.ToArray(); }
            }
            public PNode[] SortedTopLevelNodes
            {
                get
                {
                    PNode[] temp = TopLevelNodes;
                    Array.Sort(temp, delegate(PNode p1, PNode p2)
                    {
                        return CIC.Compare(p1.id, p2.id);
                    });
                    return temp;
                }
            }

            public void AddNode(PNode node)
            {
                lock (topLevelNodes)
                {
                    if (!topLevelNodes.Contains(node))
                    {
                        topLevelNodes.Add(node);
                    }
                }
            }

            public PNode Contains(string id)
            {
                List<PNode> visitedNodes = new List<PNode>();
                PNode[] tempTopLevelNodes = TopLevelNodes;

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
                if (CIC.SameKey(node.Id, id))
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
                lock (topLevelNodes) rootNodes.AddRange(topLevelNodes.FindAll(node => node.IncomingEdges.Length == 0));

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

            public void PrintToWriterTreeMts(TextWriter writer, string serverRoot)
            {
                writer.WriteLine("<ul>");
                foreach (PNode node in SortedTopLevelNodes)
                {
                    PrintToWriterOutEdges(node, 0, writer, serverRoot);

                }
                writer.WriteLine("</ul>");
            }

            public void PrintToWriterOutEdges(PNode node, int indentation, TextWriter writer, string serverRoot)
            {
                if (node == null) return;
                //writer.Write("<p>");
                //for (int i = 0; i < indentation; ++i) writer.Write(" ");
                //ConsoleWriteLine(node.Id);
                if (node == SIProlog.BaseKB && indentation > 1) return;
                writer.WriteLine("<li>{0}</li>", node.ToLink(serverRoot));
                writer.WriteLine("<ul>");
                foreach (PEdge e in node.OutgoingEdges)
                {
                    if (indentation < 10) PrintToWriterOutEdges(e.EndNode, indentation + 1, writer, serverRoot);
                }
                writer.WriteLine("</ul>");
            }
            public void PrintToWriterInEdges(TextWriter writer, string serverRoot)
            {
                writer.WriteLine("<ul>");
                foreach (PNode node in SortedTopLevelNodes)
                {
                    PrintToWriterInEdges(node, 0, writer, serverRoot);

                }
                writer.WriteLine("</ul>");
            }

            public void PrintToWriterInEdges(PNode node, int indentation, TextWriter writer, string serverRoot)
            {
                if (node == null) return;
                if (indentation > 4) return;
                //writer.Write("<p>");
                //for (int i = 0; i < indentation; ++i) writer.Write(" ");
                //ConsoleWriteLine(node.Id);
                if (node == SIProlog.EverythingPSC && indentation > 1) return;
                writer.WriteLine("<li>{0}</li>", node.ToLink(serverRoot));
                writer.WriteLine("<ul>");
                foreach (PEdge e in node.IncomingEdges)
                {
                    PrintToWriterInEdges(e.StartNode, indentation + 1, writer, serverRoot);
                }
                writer.WriteLine("</ul>");
            }
        }
        #endregion

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

        public static Term MakeTerm(string name, params Part[] parts)
        {
            return new Term(name, false, new PartList(parts));
        }
        public static Term TERM_TRUE
        {
            get
            {
                if (_TERM_TRUE==null)
                {
                    _TERM_TRUE = MakeTerm("true");
                }
                return _TERM_TRUE;
            }
        }
            
        public static Part RuleBodyToTerm(PartListImpl rulebody)
        {
            if (IsBodyAlwaysTrue(rulebody))
                return TERM_TRUE;
            return MakeTerm(",", rulebody);
        }
        public static bool IsBodyAlwaysTrue(PartListImpl rulebody)
        {
            return rulebody == null || rulebody.Count == 0 ||
                (rulebody.Count == 1 && TERM_TRUE.Equals(rulebody[0]));
        }
        public static Part RuleToTerm(Part head, PartListImpl rulebody)
        {
            if (IsBodyAlwaysTrue(rulebody))
            {
                return head;
            }
            return MakeTerm(":-", head, SIProlog.RuleBodyToTerm(rulebody));
        }
    }

    internal class ParseType 
    {
        public ParseType(string quoteName0, bool canHaveSpaceAfterStart0, bool multiLine0)
        {
            quoteName = quoteName0;
            StartSequence = "" + quoteName0[0];
            EndSequence = "" + quoteName0[quoteName0.Length - 1];
            CantHaveSpaceAfterStart = canHaveSpaceAfterStart0;
            MultiLine = multiLine0;
        }
        public string StartSequence;
        public string EndSequence;
        public string typeName = "id";
        public string quoteName = "atom";
        public bool MultiLine
        {
            set
            {
                CantHave += "\r\n";
            }
            get
            {
                return CantHave.Contains('\n');
            }
        }
        public bool CantHaveSpaceAfterStart
        {
            set
            {
                CantHaveNext += " ";
            }
            get
            {
                return CantHaveNext.Contains(" ");
            }
        }
        public string EscapeChars = "\\";
        public string CantHave = "";
        public string CantHaveNext = "";
    }

    public class SourceLanguage
    {
        public static SourceLanguage Prolog = new SourceLanguage("prolog");

        public static SourceLanguage Notation3 = new SourceLanguage("notation3");

        public static SourceLanguage Text = new SourceLanguage("text")
                                                {
                                                    InnerLang = Prolog
                                                };
        
        readonly public string Name;
        public SourceLanguage InnerLang;

        public override string ToString()
        {
            return base.ToString() + ":" + Name;
        }
        private SourceLanguage(string lang)
        {
            this.Name = lang;
        }

        internal SourceLanguage Inner()
        {
            return InnerLang ?? this;
        }
    }

    public enum FrequencyOfSync
    {
        AsNeeded = 0,
        Never = 1,
        Always = 2,
    }
    public enum ContentBackingStore
    {
        None = 0,

        /// <summary>
        /// When the KB is dirty
        /// mt.Repository URI is what we'd compile from
        /// </summary>
        RdfServerURI = 3,

        /// <summary>
        /// When the KB is dirty
        /// mt.RDFStore.Triples is what we'd compile from
        /// </summary>
        RdfMemory = 2,

        /// <summary>
        /// When the KB is dirty
        /// mt.ruleset  Sourcecode is what we'd compile from
        /// </summary>
        //PrologSource,

        /// <summary>
        /// When the KB is dirty
        /// mt.pdb.rules  Prolog rule list is what we'd compile from
        /// </summary>
        Prolog = 4,
    }

    public class NonClosingTextWriter : TextWriter
    {
        private TextWriter noClose;
        public NonClosingTextWriter(TextWriter writer)
        {
            noClose = writer;
        }

        public override Encoding Encoding
        {
            get { return noClose.Encoding; }
        }
        public override void Flush()
        {
            noClose.Flush();
        }
        public override void Close()
        {
            noClose.Flush();
        }

        public override void Write(char[] buffer, int index, int count)
        {
            noClose.Write(buffer, index, count);
        }
        public override void Write(object value)
        {
            noClose.Write(value);
        }
    }

    public class KeyCase : IEqualityComparer<string>
    {
        public static KeyCase Default = new KeyCase(NormalizeKeyLowerCase);
        public static KeyCase DefaultFN = new KeyCase(NormalizeKeyLowerCaseNoFileExt);
        #region Implementation of IEqualityComparer<string>

        public Func<object, string> NormalizeKey;
        static private readonly char[] RegexMarkers = "$^.*[|]".ToCharArray();

        public KeyCase(Func<object, string> normalizer)
        {
            NormalizeKey = normalizer;
        }
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
            if (ContainsRegex(obj))
            {
                throw new InvalidOperationException("Keys should not contain Regex!?! " + obj);
            }
            return NormalizeKey(obj).GetHashCode();
        }

        public static string NormalizeKeyLowerCase(object s)
        {
            string fn = s.ToString().Trim().ToLower().Replace(" ", "_");
            return fn;// Path.GetFileNameWithoutExtension(fn);
        }
        public static string NormalizeKeyLowerCaseNoFileExt(object s)
        {
            string fn = s.ToString().Trim().ToLower().Replace(" ", "_");
            return Path.GetFileNameWithoutExtension(fn);
        }
        #endregion

        public bool SameKeyO(object cK, object cP)
        {
            if (Equals(cK, cP)) return true;
            if (cK.GetType().IsValueType)
            {
                throw new InvalidOperationException("lcase " + cK.GetType());
            }
            return SameKey(cK.ToString(), cP.ToString());
        }
        public bool SameKey(string cK, string cP)
        {
            return Compare(cK, cP) == 0;
        }
        public int Compare(string cK, string cP)
        {
            if (cK == cP) return 0;
            var cnK = NormalizeKey(cK);
            var cnP = NormalizeKey(cP);
            bool crK = ContainsRegex(cK);
            bool crP = ContainsRegex(cP);
            if (cnK == cnP) return 0;
            if (!crK && !crP)
            {
                return cnK.CompareTo(cnP);
            }
            if (crK && crP)
            {
                return cnK.CompareTo(cnP);
            }
            if (crK)
            {
                var swap = cK;
                cK = cP;
                cP = swap;
            }
            if (Regex.IsMatch(cK, cP, RegexOptions.IgnoreCase))
            {
                return 0;
            }
            return cnK.CompareTo(cnP);
        }

        private static bool ContainsRegex(string c1)
        {
            if (c1.IndexOfAny(RegexMarkers) < 0)
            {
                return false;
            }
            return true;
        }
    }

    public class CIDictionary<K, V> : Dictionary<K, V>
    {
        public KeyCase myComp
        {
            get
            {
                return (KeyCase)base.Comparer;
            }
        }

        public CIDictionary()
            : base((IEqualityComparer<K>)KeyCase.Default)
        {

        }
        public CIDictionary(IEqualityComparer<K> comp)
            : base(comp)
        {

        }
        public CIDictionary(IDictionary<K, V> dict)
            : base(dict, (IEqualityComparer<K>)KeyCase.Default)
        {

        }
        public CIDictionary(IDictionary<K, V> dict, IEqualityComparer<K> comp)
            : base(dict, comp)
        {

        }

        public V this[K key, Func<V> ifMissing]
        {
            get
            {
                V v;
                if (TryGetValue(key, out v))
                {
                    return v;
                }
                return ifMissing();
            }

            set
            {
                string key1 = "" + key;
                var key2 = this.myComp.NormalizeKey(key);
                if (key2 != key1)
                {
                    //throw BadOp();
                }
                base[key] = value;
            }
        }
    }
    public class CIDictionary2<U1, U2> : IDictionary<U1, U2>
    {

        private KeyValuePair<U1, U2> GetItem(KeyValuePair<U1, U2> item)
        {
            KeyValuePair<U1, U2>? realKeyValue = GetKV(item.Key);
            if (realKeyValue != null)
            {
                item = new KeyValuePair<U1, U2>(realKeyValue.Value.Key, item.Value);
            }
            return item;
        }
        private KeyValuePair<U1, U2>? GetKV(U1 key)
        {
            foreach (var u1 in backing)
            {
                if (KeyCase.Default.SameKeyO(u1.Key, key)) return u1;
            }
            return null;
        }

        readonly IDictionary<U1, U2> backing;

        public CIDictionary2()
        {
            backing = new Dictionary<U1, U2>((IEqualityComparer<U1>)KeyCase.Default);
        }
        public CIDictionary2(IEqualityComparer<U1> comp)
        {
            backing = new Dictionary<U1, U2>(comp);
        }
        public CIDictionary2(IDictionary<U1, U2> dict)
        {
            backing = new Dictionary<U1, U2>((IEqualityComparer<U1>)KeyCase.Default);
            SetValues(dict);
        }
        public CIDictionary2(IDictionary<U1, U2> dict, IEqualityComparer<U1> comp)
        {
            backing = new Dictionary<U1, U2>(comp);
            SetValues(dict);
        }

        public void SetValues(IDictionary<U1, U2> table)
        {
            foreach (KeyValuePair<U1, U2> kv in table)
            {
                this[kv.Key] = kv.Value;
            }
        }

        public IEnumerator<KeyValuePair<U1, U2>> GetEnumerator()
        {
            return backing.GetEnumerator();
        }

        IEnumerator IEnumerable.GetEnumerator()
        {
            return backing.GetEnumerator();
        }

        public void Add(KeyValuePair<U1, U2> item)
        {
            var item2 = GetItem(item);
            backing.Add(item2);
        }

        public void Clear()
        {
            backing.Clear();
        }

        public bool Contains(KeyValuePair<U1, U2> item)
        {
            item = GetItem(item);
            return backing.Contains(item);
        }

        public void CopyTo(KeyValuePair<U1, U2>[] array, int arrayIndex)
        {
            backing.CopyTo(array, arrayIndex);
        }

        public bool Remove(KeyValuePair<U1, U2> item)
        {
            item = GetItem(item);
            return backing.Remove(item);
        }

        public int Count
        {
            get { return backing.Count; }
        }

        public bool IsReadOnly
        {
            get { return backing.IsReadOnly; }
        }

        public bool ContainsKey(U1 key)
        {
            KeyValuePair<U1, U2>? realKeyValue = GetKV(key);
            return (realKeyValue != null);
        }

        public void Add(U1 key, U2 value)
        {
            KeyValuePair<U1, U2>? realKeyValue = GetKV(key);
            if (realKeyValue != null)
            {
                key = realKeyValue.Value.Key;
            }
            backing.Add(key, value);
        }

        public bool Remove(U1 key)
        {
            KeyValuePair<U1, U2>? realKeyValue = GetKV(key);
            if (realKeyValue == null) return false;
            return backing.Remove(realKeyValue.Value.Key);
        }

        public bool TryGetValue(U1 key, out U2 value)
        {
            value = default(U2);
            KeyValuePair<U1, U2>? realKeyValue = GetKV(key);
            if (realKeyValue == null) return false;
            value = realKeyValue.Value.Value;
            return true;
        }

        public U2 this[U1 key]
        {
            get
            {
                U2 v;
                if (TryGetValue(key, out v))
                {
                    return v;
                }
                return backing[key];
            }

            set { backing[key] = value; }
        }

        public ICollection<U1> Keys
        {
            get { return backing.Keys; }
        }

        public ICollection<U2> Values
        {
            get { return backing.Values; }
        }

        protected KeyCase Comparer
        {
            get { throw new NotImplementedException(); }
            set { throw new NotImplementedException(); }
        }
    }
}

