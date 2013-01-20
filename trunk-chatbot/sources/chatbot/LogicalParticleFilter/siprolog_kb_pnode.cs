#define MERGED_RDFSTORE
using System;
using System.Collections;
using System.Collections.Generic;
using System.Globalization;
using System.IO;
using System.Linq;
using System.Reflection;
#if (COGBOT_LIBOMV || USE_STHREADS)
using ThreadPoolUtil;
using Thread = ThreadPoolUtil.Thread;
using ThreadPool = ThreadPoolUtil.ThreadPool;
using Monitor = ThreadPoolUtil.Monitor;
#endif
using System.Threading;
using System.Web;
using LogicalParticleFilter1;
using MushDLR223.Utilities;
using VDS.RDF;
using VDS.RDF.Nodes;
using VDS.RDF.Parsing;
using VDS.RDF.Parsing.Contexts;
using VDS.RDF.Parsing.Handlers;
using VDS.RDF.Parsing.Tokens;
using VDS.RDF.Query;
using VDS.RDF.Query.Expressions;
using VDS.RDF.Writing;
using ListOfBindings = System.Collections.Generic.List<System.Collections.Generic.Dictionary<string, LogicalParticleFilter1.SIProlog.Part>>;
using StringWriter=System.IO.StringWriter;
using VDS.RDF.Writing.Formatting;
using PartList = LogicalParticleFilter1.SIProlog.PartListImpl;

namespace LogicalParticleFilter1
{
    public partial class SIProlog
    {
        static public List<PredicateProperty> localPreds = new List<PredicateProperty>();
        static public List<RDFArgSpec> localArgTypes = new List<RDFArgSpec>();
        static public List<Term> localPredInstances = new List<Term>();

        private PNode FindOrCreateKB_unlocked(string mt, bool addDefaultGenlMts)
        {
            bool newlyCreated;
            var node = FindOrCreateKB_unlocked_Actual(mt, out newlyCreated);
            if (newlyCreated && addDefaultGenlMts)
            {
                EverythingPSC = EverythingPSC ?? FindOrCreateKB_unlocked_Actual("everythingPSC", out newlyCreated);
                BaseKB = BaseKB ?? FindOrCreateKB_unlocked_Actual("baseKB", out newlyCreated);
                if (node != EverythingPSC && node != BaseKB)
                {
                    EverythingPSC.CreateEdgeTo(node);
                    node.CreateEdgeTo(BaseKB);
                }
            }
            return node;
        }

        private PNode FindOrCreateKB_unlocked_Actual(string mt, out bool newlyCreated)
        {
            PNode graph;
            newlyCreated = !GraphForMT.TryGetValue(mt, out graph);
            if (newlyCreated)
            {
                var newGraph = NewGraph(mt);
                Uri nsURI = UriFactory.Create(UriOfMt(mt));                
                newGraph.BaseUri = nsURI;
                lock (rdfDefNS) rdfDefNS.AddNamespace(mt, nsURI);
                EnsureReaderNamespaces(newGraph);
                graph = new PNode(mt, this, newGraph);
                newGraph.pnode = graph;
            }
            return graph.PrologKB;
        }

        public partial class PNode : IComparable
        {
            public string id;
            public object CompileLock
            {
                get
                {
                    var lockobj = this;
                    bool needsUnlock = true;
                    try
                    {
                        needsUnlock = Monitor.TryEnter(lockobj, TimeSpan.FromSeconds(4));
                        if (!needsUnlock)
                        {
                            return new object();
                        }
                        return lockobj;
                    }
                    finally
                    {
                        if (needsUnlock)
                        {
                            Monitor.Exit(lockobj);
                        }
                    }
                }
            }

            readonly public PDB pdb;

            public bool dirty
            {
                get
                {
                    checkSyncLocked();
                    lock (CompileLock)
                    {
                        return IsOutOfSyncFor(SourceKind);
                    }
                }
            }

            private void checkSyncLocked()
            {
                LockInfo.EnsureLocked(CompileLock, Warn);
            }

            public double probability = 1.0;
            public ContentBackingStore SourceKind
            {
                get
                {
                    return _SourceKind;
                }
                set
                {
                    if (!SIProlog.RdfSavedInPDB)
                    {
                        if (value != _SourceKind)
                        {
                            _SourceKind = value;
                        }
                    }
                }
            }
            ContentBackingStore _SourceKind = ContentBackingStore.Prolog;
            public FrequencyOfSync SyncFrequency = FrequencyOfSync.AsNeeded;
            private ContentBackingStore _SyncFromNow = ContentBackingStore.None;
            public ContentBackingStore SyncFromNow
            {
                get { lock (CompileLock) return _SyncFromNow; }
                set
                {
                    if (_SyncFromNow == value) return;
                    if (value != ContentBackingStore.None && _SyncFromNow != ContentBackingStore.None)
                    {
                        Warn("Might be losing Data: SyncFromNow {0}=>{1}", _SyncFromNow, value);
                    }
                    _SyncFromNow = value;
                }
            }

            public ContentBackingStore SyncCurrent
            {
                get
                {
                    lock (CompileLock)
                    {
                        if (SyncFromNow == ContentBackingStore.None)
                            return SourceKind;
                        return SyncFromNow;
                    }

                }
            }

            List<PEdge> incomingEdgeList = new List<PEdge>();
            List<PEdge> outgoingEdgeList = new List<PEdge>();

            public string Id
            {
                get { return id; }
                set { id = value; }
            }
            object _repository;

            public PNode RdfStore
            {
                get { return this; }
            }

            public object Repository
            {
                get { return _repository; }
                set
                {
                    if (value == null)
                    {
                        SourceKind = ContentBackingStore.RdfMemory;
                    }
                    else
                    {
                        SourceKind = ContentBackingStore.RdfServerURI;
                    }
                    if (_repository == value) return;
                    SyncFromNow = SourceKind;
                    _repository = value;
                }
            }

            public double Probability
            {
                get { return probability; }
                set { probability = value; }
            }
            public override int GetHashCode()
            {
                return NormalizeKBName(id).GetHashCode();
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


            public override string ToString()
            {
                return "mt:" + Id + " " + DebugInfo;
            }

            public PEdge CreateEdgeTo(PNode otherNode)
            {
                if (otherNode == this)
                {
                    return null;
                }
                PEdge edge = new PEdge(this, otherNode);
                return edge;
            }

            public void AddIncomingEdge(PEdge edge)
            {
                lock (EdgeLists) if (!incomingEdgeList.Contains(edge)) incomingEdgeList.Add(edge);
            }

            public object EdgeLists
            {
                get { return GlobalEdgeListLock; }
            }

            public static object GlobalEdgeListLock = new object();

            public void AddOutgoingEdge(PEdge edge)
            {
                lock (EdgeLists) if (!outgoingEdgeList.Contains(edge)) outgoingEdgeList.Add(edge);
            }
            public void ClearIncomingEdges()
            {
                lock (EdgeLists) incomingEdgeList.Clear();
            }
            public void ClearOutgoingEdges()
            {
                lock (EdgeLists) outgoingEdgeList.Clear();
            }
            public PEdge[] IncomingEdges
            {
                get { lock (EdgeLists) return incomingEdgeList.ToArray(); }
            }

            public PEdge[] OutgoingEdges
            {
                get { lock (EdgeLists) return outgoingEdgeList.ToArray(); }
            }

            public string DebugInfo
            {
                get
                {
                    string prefix = string.Format("source={0}", SourceKind);
                    if (probability != 1.0)
                    {
                        prefix += string.Format(" prob={0}", probability);
                    }
                    string pq = "";
                    var buri = RdfStore.rdfGraph.BaseUri;
                    if (buri != null) pq = "" + buri;
                    return string.Format("{0} size={1} dirty={2} triples={3} sync={4} repo={5} base={6}",
                                         prefix, pdb.rules.Count, dirty,
                                         RdfStore.rdfGraph.Triples.Count,
                                         SyncFromNow, Repository, pq);
                }
            }

            public IGraph definations
            {
                get { return RdfStore.rdfGraph; }
            }

            public bool RdfCacheShouldGenlPrologMt = false;

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

            public void RemoveEdgeTo(PNode otherNode)
            {
                foreach (PEdge e in this.OutgoingEdges)
                {
                    if (e.EndNode == otherNode)
                    {
                        if (e.StartNode.outgoingEdgeList.Contains(e))
                            e.StartNode.outgoingEdgeList.Remove(e);
                        if (e.EndNode.incomingEdgeList.Contains(e))
                            e.EndNode.incomingEdgeList.Remove(e);
                        return;
                    }
                }

                return;
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
                return string.Format("<a href='{1}siprolog/?mt={0}'>{0}</a>&nbsp;({2})", id, serverRoot, DebugInfo.Replace(" ", "&nbsp;"));
            }
            internal string ToOptionLink(string serverRoot)
            {
                return string.Format("<option value='{1}siprolog/?mt={0}'>{0}</option>", id, serverRoot, DebugInfo.Replace(" ", "&nbsp;"));
            }

            public bool IsDataFrom(ContentBackingStore backingStore)
            {
                return SourceKind == backingStore;
            }

            internal bool ClearRDFCache()
            {
                if (RdfStore.rdfGraph.IsEmpty) return false;
                checkSyncLocked();
                RdfStore.rdfGraph.Clear();
                return true;
            }
            internal bool ClearPrologCache()
            {
                if (pdb.rules.Count == 0) return false;
                checkSyncLocked();
                pdb.index.Clear();
                lock (CompileLock) lock (pdb.rules) pdb.rules.Clear();
                return true;
            }

            internal void Clear()
            {
                lock (CompileLock)
                {
                    ClearPrologCache();
                    ClearRDFCache();
                    SyncFromNow = SourceKind;
                }
            }

            public void pushRdfGraphToPrologKB(bool clearPrologKB)
            {
                if (DLRConsole.IsOnMonoUnix)
                {
                    return; // KHC: in realbot no rdf to sync for now
                }

                if (IsOutOfSyncFor(ContentBackingStore.RdfMemory))
                {
                    Warn("RdfMemory not ready for pushing " + this);
                }
                lock (CompileLock) RdfStore.pushGraphToKB(clearPrologKB);
            }
            public void pushPrologKBToRdfGraph(bool clearRDFMemory)
            {
                if (DLRConsole.IsOnMonoUnix)
                {
                    return; // KHC: in realbot no rdf to sync for now
                }

                if (IsOutOfSyncFor(ContentBackingStore.Prolog))
                {
                    Warn("Prolog not ready for pushing " + this);
                }
                lock (CompileLock) RdfStore.pushRulesToGraph(clearRDFMemory);
            }

            public bool IsOutOfSyncFor(ContentBackingStore type)
            {
                return type != SyncFromNow && SyncFromNow != ContentBackingStore.None;
            }

            public void populateRDFMemoryFromRepository()
            {
                Uri from = Repository as Uri;
                if (from == null)
                {
                    string uri = "" + Repository;
                    from = UriFactory.Create(uri);
                }
                RdfStore.rdfGraph.BaseUri = from;
                RdfStore.LoadFromUri(from);
                SyncFromNow = ContentBackingStore.RdfMemory;
            }
            public string GetKBText
            {
                get
                {
                    StringWriter kbText = new StringWriter();
                    lock (CompileLock)
                    {
                        prologEngine.ensureCompiled(this, ContentBackingStore.Prolog);
                        foreach (Rule r in pdb.rules)
                        {
                            kbText.WriteLine(r.ToSource(SourceLanguage.Prolog));
                        }
                    }
                    return kbText.ToString();
                }
            }


            public string AToString
            {
                get { return ToString(); }
            }

            readonly public Graph _rdfGraph;
            public Graph rdfGraph
            {
                get
                {
                    return _rdfGraph;
                }
            }

            public string prologMt;
            public SIProlog prologEngine;

            public PNode PrologKB
            {
                get { return this; }
            }
            public PNode(string plMt, SIProlog prolog, Graph data)
            {
                this.prologEngine = prolog;
                this.id = plMt;
                prolog.KBGraph.AddNode(this);
                prologEngine.GraphForMT[plMt] = this;
                pdb = new PDB(true);
                pdb.startMt = plMt;
                pdb.followedGenlMt = false;
                PrologKB.id = plMt;
                _rdfGraph = data;
                EnsureGraphPrefixes(rdfGraph);
            }

            internal void pushRulesToGraph(bool clearRDFMemory)
            {
                var focus = PrologKB;
                WarnAndClear(focus, ContentBackingStore.Prolog, clearRDFMemory, focus.ClearRDFCache, ContentBackingStore.RdfMemory);
                prologEngine.pushRulesToGraph(Id, this, focus.RdfCacheShouldGenlPrologMt);
                SaveOffRDF();
            }

            private void SaveOffRDF()
            {
                string rdfdir = "rdfcache";
                if (Directory.Exists(rdfdir))
                {
                    lock (CompileLock)
                    {
                        rdfGraph.SaveToFile(rdfdir + "/" + Id + ".n3", new Notation3Writer());
                    }
                }
            }

            internal void pushGraphToKB(bool clearPrologFirst)
            {
                SaveOffRDF();
                var focus = PrologKB;
                WarnAndClear(focus, ContentBackingStore.RdfMemory, clearPrologFirst, focus.ClearPrologCache,
                             ContentBackingStore.Prolog);
                var trips = Triples;
                if (trips.Count == 0) return;
                lock (trips)
                {
                    focus.pdb.index.Clear();
                    foreach (Triple triple in trips)
                    {
                        RdfRules rules = new RdfRules(rdfGraph);
                        var term = MakeTerm(TripleName,
                                            GraphWithDef.RdfToPart(triple.Subject, rules),
                                            GraphWithDef.RdfToPart(triple.Predicate, rules),
                                            GraphWithDef.RdfToPart(triple.Object, rules));
                        var rule = new Rule(term);
                        rule.rdfRuleCache = rules;
                        if (!focus.pdb.rules.Contains(rule))
                        {
                            focus.pdb.rules.Add(rule);
                        }
                    }
                }
            }
            public BaseTripleCollection Triples
            {
                get { return rdfGraph.Triples; }
            }

            public int Size
            {
                get
                {
                    var cur = SyncCurrent;
                    if (cur == ContentBackingStore.Prolog) return pdb.rules.Count;
                    if (cur == ContentBackingStore.RdfMemory) return rdfGraph.Triples.Count;
                    throw ErrorBadOp("Cant get size of " + this);
                }
            }

            public void LoadFromUri(Uri uri)
            {
                try
                {
                    LoadFromUri0(uri);
                }
                catch (Exception e)
                {
                    Warn("LoadFromURI({0}) Caused: {1}", uri, e);
                }
            }

            private void LoadFromUri0(Uri endpointURI)
            {
                Graph g = new Graph();
                g.NamespaceMap.Import(rdfGraph.NamespaceMap);
                g.LoadFromUri(endpointURI);
                var gt = g.Triples;
                if (gt.Count > 0)
                {
                    rdfGraph.NamespaceMap.Import(g.NamespaceMap);
                    rdfGraph.Assert(gt);
                    if (gt.SubjectNodes.Count() > 1)
                    {
                        // we have actual data
                        return;
                    }
                }
                //Define a remote endpoint
                //Use the DBPedia SPARQL endpoint with the default Graph set to DBPedia
                SparqlRemoteEndpoint endpoint = new SparqlRemoteEndpoint(endpointURI);

                //Use the extension method ExecuteQuery() to make the query against the Graph
                try
                {
                    //Object results = g.ExecuteQuery(query);
                    //Make a SELECT query against the Endpoint
                    SparqlResultSet results = endpoint.QueryWithResultSet("SELECT ?s ?p ?o WHERE { ?s ?p ?o } LIMIT 1400");
                    foreach (SparqlResult set in results)
                    {
                        rdfGraph.Assert(MakeTriple(set["s"], set["p"], set["o"]));
                    }
                }
                catch (RdfQueryException queryEx)
                {
                    //There was an error executing the query so handle it here
                    Warn("While endpointURI={0}\n\n{1}", endpointURI, queryEx);
                }
            }

            public void IncludeRDFUri(string filename)
            {
                Graph g = new Graph();
                var nm = rdfGraph.NamespaceMap;
                lock (nm) g.NamespaceMap.Import(nm);
                g.BaseUri = rdfGraph.BaseUri;
                Uri loadFrom = new Uri(new Uri(filename).AbsoluteUri);
                g.LoadFromUri(loadFrom);
                int loadCount = g.Triples.Count;
                ConsoleWriteLine("Loading " + loadCount + " from " + loadFrom);
                int oldCount = rdfGraph.Triples.Count;
                lock (rdfGraph) rdfGraph.Merge(g, true);
                int addedCount = rdfGraph.Triples.Count - oldCount;
                ConsoleWriteLine("Merge add " + addedCount + " new to " + rdfGraph);
            }

            public void AddRuleToRDF(Rule rule)
            {
                if (rule.rdfRuleCache != null) return;
                if (prologEngine.DontRDFSync)
                {
                    Warn("Trying to Add Rule TO rdf: " + rule);
                    return;
                }
                string before = rule.ToSource(SourceLanguage.Prolog);
                var rdfRules = GraphWithDef.FromRule(this, rule, rdfGraph);
                rdfRules.AssertTriples(rdfGraph, true, true);
                string after = rule.ToSource(SourceLanguage.Prolog);
                if (before != after)
                {
                    SIProlog.Warn("Manipulated rule: " + before + "->" + after);
                }
            }

            public void AddRules(RuleList ruleSet, bool clearFirst)
            {
                var focus = this;
                if (focus.pdb.rules == ruleSet)
                {
                    return;
                }
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
            public void AssertTriple(Triple triple, IGraph listResolves)
            {
                GraphWithDef.InCompiler<bool>(this, null, id, () => AssertTriple_0(triple, listResolves));
            }
            public bool AssertTriple_0(Triple triple, IGraph listResolves)
            {
                var s = triple.Subject;
                var p = triple.Predicate;
                var o = triple.Object;
                string sp = triple.Predicate.ToString();
                int argNum = GraphWithDef.GetInstanceOnArg(sp);
                var pp = Atom.MakeNodeAtom(p);
                if (argNum == -1)
                {
                    // assume it's one to one?
                    argNum = 1;
                }
                if (argNum != 0)
                {
                    //1: [a f (b c)]
                    //2: [b f (a c)]
                    //3: [c f (a b)]
                    PartList parts = new PartListImpl();
                    AddRdfList(parts, o, argNum, s, triple, listResolves);
                    var rule = new Rule(new Term(pp.fname, false, parts));
                    rule.optHomeMt = id;
                    pdb.index.Clear();
                    pdb.rules.Add(rule);
                    return true;
                }
                // pred + args are in the list [db1 f (a b c)]
                return false;
            }

            public static void AddRdfList(PartListImpl parts, INode node, int argNum, INode obj, Triple triple, IGraph listResolves)
            {
                if (parts.Count + 1 == argNum)
                {
                    AddRdfList(parts, obj, -1, null, triple, listResolves);
                    AddRdfList(parts, node, -1, null, triple, listResolves);
                    return;
                }
                switch (node.NodeType)
                {
                    case NodeType.Literal:
                        parts.AddPart(Atom.MakeNodeAtom(node));
                        return;
                    case NodeType.Variable:
                        parts.AddPart(new Variable(node.ToString().Substring(1)));
                        return;
                    case NodeType.Uri:
                    case NodeType.Blank:
                        {
                            INode f, r;
                            if (GetRdfList(node, listResolves, out f, out r))
                            {
                                AddRdfList(parts, f, argNum, obj, triple, listResolves);
                                AddRdfList(parts, r, argNum, obj, triple, listResolves);
                                return;
                            }
                        }
                        parts.AddPart(Atom.MakeNodeAtom(node));
                        return;
                    case NodeType.GraphLiteral:
                        break;
                    default:
                        throw new ArgumentOutOfRangeException();
                }
            }

            public static bool GetRdfList(INode n, IGraph g, out INode f, out INode r)
            {
                f = r = null;
                INode rdfRest = g.CreateUriNode(UriFactory.Create(RdfSpecsHelper.RdfListRest));
                INode rdfFirst = g.CreateUriNode(UriFactory.Create(RdfSpecsHelper.RdfListFirst));
                foreach (var tf in g.GetTriplesWithSubjectPredicate(n, rdfFirst))
                {
                    f = tf.Object;
                    foreach (var tr in g.GetTriplesWithSubjectPredicate(n, rdfRest))
                    {
                        r = tf.Object;
                        return true;
                    }
                    break;
                }
                return false;
            }

            public void AddImmediate(Rule rule)
            {
                pdb.rules.Add(rule);
            }
        }

        public static void WarnAndClear(PNode focus, ContentBackingStore fromContent, bool clearFirst, Func<bool> clearMethod, ContentBackingStore destContent)
        {
            if (focus.IsOutOfSyncFor(fromContent))
            {
                Warn("IsOutOfSyncFor " + fromContent + " due to SyncFromNow==" + focus.SyncFromNow + " in " + focus);
            }
            if (clearFirst)
            {
                if (clearMethod())
                {
                    if (focus.SyncFromNow == destContent)
                    {
                        Warn("Lost Data from " + focus.SyncFromNow + " in " + focus);
                    }
                }
                focus.SyncFromNow = ContentBackingStore.None;
            }
            else
            {
                focus.SyncFromNow = ContentBackingStore.None;
                focus.SyncFromNow = destContent;
            }
        }
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
                    srcNode = SIProlog.CurrentProlog.FindOrCreateKB(idSrc);
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
            public void PrintToWriterSelectMts(TextWriter writer, string serverRoot)
            {
                writer.WriteLine("<select name=\"location\" id=\"location\" onchange=\"setIframeSource()\">");
                foreach (PNode node in SortedTopLevelNodes)
                {
                    PrintToWriterOutEdgesOptions(node, 0, writer, serverRoot);

                }
                writer.WriteLine("</select>");
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
            public void PrintToWriterOutEdgesOptions(PNode node, int indentation, TextWriter writer, string serverRoot)
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
    }
}