#define MERGED_RDFSTORE
using System;
using System.Collections;
using System.Collections.Generic;
using System.Globalization;
using System.IO;
using System.Linq;
using System.Reflection;
#if (COGBOT_LIBOMV || USE_STHREADS || true)
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
using StringWriter = System.IO.StringWriter;
using VDS.RDF.Writing.Formatting;
using PartList = LogicalParticleFilter1.SIProlog.PartListImpl;
using Part = LogicalParticleFilter1.SIProlog.Part;
using System.Text;
using VDS.Common;
using VDS.Common.Trees;

namespace LogicalParticleFilter1
{
    public partial class SIProlog
    {
        public static ManualResetEvent ReplRunning;

        public void EndpointCreated(PFEndpoint endpoint)
        {
            ConsoleWriteLine("Endpoint and bot are created by now");
            DLRConsole.PrintOnlyThisThread = Thread.CurrentThread;
            try
            {
                if (RdfDeveloperSanityChecks > 0) Program.RunAllTests(this);
            }
            finally
            {
                DLRConsole.PrintOnlyThisThread = null;
            }
            if (RdfDeveloperSanityChecks < 2) return;
            ReplRunning = ThreadPool.WaitableQueueUserWorkItem((o) => RunREPL());
        }

        public void RunREPL()
        {
            bool queryMode = true;
            LocalIOSettings threadLocal = GlobalSharedSettings.LocalSettings;
            while (true)
            {
                ConsoleWriteLine("-----------------------------------------------------------------");
                var modeStr = queryMode ? "?-" : "<-";
                string input = TextFilter.ReadLineFromInput(ConsoleWriteLine,
                                                            String.Format("<{0}> {1} ", threadLocal.curKB, modeStr));
                if (input == null)
                {
                    Environment.Exit(0);
                }
                input = input.Trim(' ', '.', '\r', '\n', '\t').Trim();
                if (input.ToLower() == "halt")
                {
                    return;
                }
                if (input.ToLower() == "quit")
                {
                    Environment.Exit(Environment.ExitCode);
                }
                do
                {
                    if (input == "") break;
                    if (input.StartsWith("?-"))
                    {
                        input = input.Substring(2);
                        queryMode = true;
                        continue;
                    }
                    if (input.StartsWith(":-"))
                    {
                        input = input.Substring(2);
                        queryMode = false;
                        continue;
                    }

                    if (queryMode)
                    {
                        askQuery(input, threadLocal.curKB);
                    }
                    else
                    {
                        appendKB(input, threadLocal.curKB);
                    }
                    break;
                } while (true);
            }
        }
        public LiveCallGraph NewGraph(string kbName, string baseURI, bool createFresh, bool replaceNamespace)
        {
            bool newlyCreated;
            var lcg = NewGraphNoReplace(kbName, baseURI, createFresh, out newlyCreated);
            RegisterHomeGraph(baseURI, lcg, replaceNamespace);
            return lcg;
        }

        public LiveCallGraph NewGraphNoReplace(string kbName, string baseURI, bool createFresh, out bool newlyCreated)
        {
            PNode pnode;
            bool found = GraphForMT.TryGetValue(kbName, out pnode);
            if (found && pnode != null)
            {
                if (pnode.SelfHostedRdfGraph)
                {
                    if (!createFresh)
                    {
                        newlyCreated = false;
                        return (LiveCallGraph)pnode.rdfGraph;
                    }
                }
                Warn("Shouldnt Remake " + baseURI);
            }
            Graph oldG;
            if (RdfGraphForURI.TryGetValue(baseURI, out oldG))
            {                
                if (oldG is LiveCallGraph)
                {
                    var oldLcg = (LiveCallGraph)oldG;
                    string pname = oldLcg.ToPrologKBName();
                    if (pname == null || pname == kbName)
                    {
                        if (!createFresh)
                        {
                            newlyCreated = false;
                            return oldLcg;
                        }
                        Warn("Shouldnt Remake " + baseURI);
                    }
                }
            }
            newlyCreated = true;
            LiveCallTripleCollection lcTC = new LiveCallTripleCollection(this);
            lcTC.DefaultKBName = kbName;
            var lcg = new LiveCallGraph(lcTC, rdfDefNS);
            lcg.BaseUri = UriFactory.Create(baseURI);
            if (found)
            {
                Warn("Disjointed graph!");
            }
            return lcg;
        }

        public static void checkNode(INode head)
        {
            if (head is IBlankNode)
            {
                IBlankNode value0 = (IBlankNode)head;
                var baseURI = value0.GraphUri;
                if (baseURI == null)
                {
                    IGraph origin = value0.Graph;
                    if (origin != null)
                    {
                        var uri2 = origin.BaseUri;
                        if (uri2 != null)
                        {
                            baseURI = uri2;
                        }
                    }
                    if (baseURI != null)
                    {
                        head.GraphUri = baseURI;
                      //  return;
                    }
                    Warn("BNode musting a home");
                }
            }
        }
    }
    static public class GraphToKBMethods
    {
        public static Uri Origin(this INode head)
        {
            Uri origin = head.GraphUri;
            var g = head.Graph;
            if (g != null)
            {
                var uri = g.BaseUri;
                if (uri != null)
                {
                    origin = uri;
                }
            }
            return origin;
        }

        public static INode CopyWNode(this INode prev, IGraph g)
        {
            if (g == null)
            {
            }
            SIProlog.checkNode(prev);
            return prev.CopyNode(g, true);
        }
        public static INode CopyWNode(this INode prev, IGraph g, bool keepOriginalGraphUri)
        {
            SIProlog.checkNode(prev);
            return prev.CopyNode(g, keepOriginalGraphUri);
        }

        public static string ToPrologKBName(this IGraph g)
        {
            if (g is LiveCallGraph)
            {
                LiveCallGraph liveCallGraph = (LiveCallGraph) g;
                return liveCallGraph.DefaultKbName;
            }
            var uri = g.BaseUri;
            if (uri!=null)
            {
                string basURI = uri.AbsolutePath;
                var RdfGraphForURI = SIProlog.CurrentProlog.RdfGraphForURI;
                lock (RdfGraphForURI)
                {
                    Graph g2;
                    if (RdfGraphForURI.TryGetValue(basURI, out g2))
                    {
                        if (!ReferenceEquals(g, g2))
                        {
                            return g2.ToPrologKBName();
                        } else
                        {
                            SIProlog.Warn("Proxy Graph " + g2);
                        }
                    }
                }
            }
            return null;
        }
    }
    /// <summary>
    ///  An implemntation of a live triple store that calls into prolog code
    /// </summary>
    /// <remarks>
    /// <para>
    ///  replacement for TreeIndexedTripleCollection
    /// </para>
    /// </remarks>
    public class LiveCallTripleCollection
        : ThreadSafeTripleCollection
    //BaseTripleCollection
    {
        public override string ToString()
        {
            return string.Format("DefaultKBName={1} {0}", base.ToString(), DefaultKBName);
        }
        public bool Monitoring;

        //Main Storage
        readonly private MultiDictionary<Triple, Object> _triplesIndex = new MultiDictionary<Triple, object>(new FullTripleComparer(new FastNodeComparer()));
        //Simple Indexes
        readonly private MultiDictionary<INode, List<Triple>> _s = new MultiDictionary<INode, List<Triple>>(new FastNodeComparer(), MultiDictionaryMode.AVL),
                                                     _p = new MultiDictionary<INode, List<Triple>>(new FastNodeComparer(), MultiDictionaryMode.AVL),
                                                     _o = new MultiDictionary<INode, List<Triple>>(new FastNodeComparer(), MultiDictionaryMode.AVL);
        //Compound Indexes
        readonly private MultiDictionary<Triple, List<Triple>> _sp, _so, _po;


        private class PlVariableNode : VariableNode
        {
            protected internal PlVariableNode(IGraph g, String varname)
                : base(g, varname) { }
        }

        //Placeholder Variables for compound lookups
        private readonly VariableNode _subjVar = new PlVariableNode(null, "s"),
                                      _predVar = new PlVariableNode(null, "p"),
                                      _objVar = new PlVariableNode(null, "o");

        private bool _fullIndexing = false;
        private int _count = 0;

        private SIProlog _prologEngine;
        public SIProlog prologEngine
        {
            get { return _prologEngine ?? SIProlog.CurrentProlog; }
            set { _prologEngine = value; }
        }

        public string DefaultKBName;
        public LiveCallGraph Graph
        {
            get { return _graph; }
            set
            {
                if (value != null && rdfRules == null)
                {
                    rdfRules = new SIProlog.RdfRules(value);
                }
                _graph = value;
            }
        }
        private SIProlog.RdfRules rdfRules = null;
        private LiveCallGraph _graph;
        //Placeholder Variables for compound lookups
        private SIProlog.Variable subjVar = new SIProlog.Variable("S"),
                                  predVar = new SIProlog.Variable("P"),
                                  objVar = new SIProlog.Variable("O");

        protected string QueryKB
        {
            get { return AssertKbName; }
            set { throw new NotImplementedException(); }
        }


        protected string AssertKbName
        {
            get { return DefaultKBName; }
            set { throw new NotImplementedException(); }
        }


        protected bool FollowGenlMt
        {
            get { return false; }
            set { throw new NotImplementedException(); }
        }


        public ListOfBindings AskQuery(SIProlog.Term query)
        {
            var bs = new ListOfBindings();
            prologEngine.askQuery(new PartList(query), QueryKB, FollowGenlMt, bs, null);
            if (bs.Count > 0)
            {
            }
            return bs;
        }

        public bool UsePrologKB
        {
            get
            {
                if (!GlobalSharedSettings.RdfSavedInPDB) return false;
                var v = PrologKB;
                if (v != null) return true;
                return false;
            }
        }
        public SIProlog.PNode PrologKB
        {
            get
            {
                if (DefaultKBName != null)
                {
                    return prologEngine.FindOrCreateKB(DefaultKBName);
                }
                if (Graph != null)
                {
                    var gt = Graph.prologKB;
                    return gt;
                }
                return null;
            }
        }

        /// <summary>
        /// Creates a new Live Call triple collection
        /// </summary>
        public LiveCallTripleCollection(SIProlog prolog)
            : this(MultiDictionaryMode.Unbalanced, prolog)
        {
            _prologEngine = prolog;
        }

        /// <summary>
        /// Creates a new Live Call triple collection
        /// </summary>
        /// <param name="compoundIndexMode">Mode to use for compound indexes</param>
        /// <param name="prolog">SIProlog instance</param>
        public LiveCallTripleCollection(MultiDictionaryMode compoundIndexMode, SIProlog prolog)
        {
            prologEngine = prolog;
            if (Options.FullTripleIndexing)
            {
                this._fullIndexing = true;
                this._sp = new MultiDictionary<Triple, List<Triple>>(t => Tools.CombineHashCodes(t.Subject, t.Predicate), new SubjectPredicateComparer(new FastNodeComparer()), compoundIndexMode);
                this._so = new MultiDictionary<Triple, List<Triple>>(t => Tools.CombineHashCodes(t.Subject, t.Object), new SubjectObjectComparer(new FastNodeComparer()), compoundIndexMode);
                this._po = new MultiDictionary<Triple, List<Triple>>(t => Tools.CombineHashCodes(t.Predicate, t.Object), new PredicateObjectComparer(new FastNodeComparer()), compoundIndexMode);
            }
        }

        /// <summary>
        /// Indexes a Triple
        /// </summary>
        /// <param name="t">Triple</param>
        private void Index(Triple t)
        {
            this.IndexSimple(t.Subject, t, this._s);
            this.IndexSimple(t.Predicate, t, this._p);
            this.IndexSimple(t.Object, t, this._o);

            if (this._fullIndexing)
            {
                this.IndexCompound(t, this._sp);
                this.IndexCompound(t, this._so);
                this.IndexCompound(t, this._po);
            }
        }

        /// <summary>
        /// Helper for indexing triples
        /// </summary>
        /// <param name="n">Node to index by</param>
        /// <param name="t">Triple</param>
        /// <param name="index">Index to insert into</param>
        private void IndexSimple(INode n, Triple t, MultiDictionary<INode, List<Triple>> index)
        {
            List<Triple> ts;
            if (index.TryGetValue(n, out ts))
            {
                if (ts == null)
                {
                    index[n] = new List<Triple> { t };
                }
                else
                {
                    ts.Add(t);
                }
            }
            else
            {
                index.Add(n, new List<Triple> { t });
            }
        }

        /// <summary>
        /// Helper for indexing triples
        /// </summary>
        /// <param name="t">Triple to index by</param>
        /// <param name="index">Index to insert into</param>
        private void IndexCompound(Triple t, MultiDictionary<Triple, List<Triple>> index)
        {
            List<Triple> ts;
            if (index.TryGetValue(t, out ts))
            {
                if (ts == null)
                {
                    index[t] = new List<Triple> { t };
                }
                else
                {
                    ts.Add(t);
                }
            }
            else
            {
                index.Add(t, new List<Triple> { t });
            }
        }

        /// <summary>
        /// Unindexes a triple
        /// </summary>
        /// <param name="t">Triple</param>
        private void Unindex(Triple t)
        {
            this.UnindexSimple(t.Subject, t, this._s);
            this.UnindexSimple(t.Predicate, t, this._p);
            this.UnindexSimple(t.Object, t, this._o);

            if (this._fullIndexing)
            {
                this.UnindexCompound(t, this._sp);
                this.UnindexCompound(t, this._so);
                this.UnindexCompound(t, this._po);
            }
        }

        /// <summary>
        /// Helper for unindexing triples
        /// </summary>
        /// <param name="n">Node to index by</param>
        /// <param name="t">Triple</param>
        /// <param name="index">Index to remove from</param>
        private void UnindexSimple(INode n, Triple t, MultiDictionary<INode, List<Triple>> index)
        {
            List<Triple> ts;
            if (index.TryGetValue(n, out ts))
            {
                if (ts != null) ts.Remove(t);
            }
        }

        /// <summary>
        /// Helper for unindexing triples
        /// </summary>
        /// <param name="t">Triple</param>
        /// <param name="index">Index to remove from</param>
        private void UnindexCompound(Triple t, MultiDictionary<Triple, List<Triple>> index)
        {
            List<Triple> ts;
            if (index.TryGetValue(t, out ts))
            {
                if (ts != null) ts.Remove(t);
            }
        }

        /// <summary>
        /// Adds a Triple to the collection
        /// </summary>
        /// <param name="t">Triple</param>
        /// <returns></returns>
        protected override bool Add(Triple t)
        {
            if (!UsePrologKB) OnExternal();
            if (UsePrologKB)
            {
                // false so we dont have to call Contains anymore
                bool a = PrologKB.AssertTripleFromGraph(t, Graph, false, true);
                if (a) this._count++;
                return a;
            }
            if (!this.Contains(t))
            {
                this._triplesIndex.Add(t, null);
                this.Index(t);
                return true;
            }
            return false;
        }

        /// <summary>
        /// Checks whether the collection contains a given Triple
        /// </summary>
        /// <param name="t">Triple</param>
        /// <returns></returns>
        public override bool Contains(Triple t)
        {
            if (UsePrologKB)
            {
                if (UsePrologKB)
                {
                    return PrologKB.ContainsTriple(t, Graph);
                }
                foreach (var t2 in WithSubjectPredicate(t.Subject, t.Predicate))
                {
                    if (t2.Object.Equals(t.Object))
                    {
                        return true;
                    }
                }
                return false;
            }
            OnExternal();
            return this._triplesIndex.ContainsKey(t);
        }

        /// <summary>
        /// Gets the count of triples in the collection
        /// </summary>
        public override int Count
        {
            get
            {
                //Note we maintain the count manually as traversing the entire tree every time we want to count would get very expensive
                return this._count;
            }
        }

        /// <summary>
        /// Deletes a triple from the collection
        /// </summary>
        /// <param name="t">Triple</param>
        /// <returns></returns>
        protected override bool Delete(Triple t)
        {
            if (UsePrologKB)
            {
                return PrologKB.RetractTriple(t, Graph);
            }
            OnExternal();
            if (this._triplesIndex.Remove(t))
            {
                //If removed then unindex
                this.Unindex(t);
                this.RaiseTripleRemoved(t);
                this._count--;
                return true;
            }
            return false;
        }

        /// <summary>
        /// Gets the specific instance of a Triple in the collection
        /// </summary>
        /// <param name="t">Triple</param>
        /// <returns></returns>
        public override Triple this[Triple t]
        {
            get
            {
                if (UsePrologKB)
                {
                    foreach (var t2 in WithSubjectPredicate(t.Subject, t.Predicate))
                    {
                        if (t2.Object.Equals(t.Object)) return t2;
                    }
                    return t;
                }
                OnExternal();
                Triple actual;
                if (this._triplesIndex.TryGetKey(t, out actual))
                {
                    return actual;
                }
                else
                {
                    throw new KeyNotFoundException("Given triple does not exist in this collection");
                }
            }
        }

        /// <summary>
        /// Gets all the triples with a given object
        /// </summary>
        /// <param name="obj">Object</param>
        /// <returns></returns>
        public override IEnumerable<Triple> WithObject(INode obj)
        {
            if (UsePrologKB)
            {
                var t = MakeTerm(_subjVar, _predVar, obj);
                ListOfBindings result = AskQuery(t);
                List<Triple> trips = new List<Triple>();
                foreach (var rs in result)
                {
                    trips.Add(NewTriple(PartToRdf(rs["S"]), PartToRdf(rs["PRED"]), obj));
                }
                return trips;
            }

            OnExternal();
            List<Triple> ts;
            if (this._o.TryGetValue(obj, out ts))
            {
                return (ts != null ? ts : Enumerable.Empty<Triple>());
            }
            else
            {
                return Enumerable.Empty<Triple>();
            }
        }

        /// <summary>
        /// Gets all the triples with a given predicate
        /// </summary>
        /// <param name="pred">Predicate</param>
        /// <returns></returns>
        public override IEnumerable<Triple> WithPredicate(INode pred)
        {
            if (UsePrologKB)
            {
                string f = SIProlog.GraphWithDef.GetPredFunctor(pred);
                SIProlog.Term t = MakeTerm(_subjVar, pred, _objVar);
                ListOfBindings result = AskQuery(t);
                List<Triple> trips = new List<Triple>();
                foreach (var rs in result)
                {
                    trips.Add(NewTriple(PartToRdf(rs[subjVar.vname]), pred, PartToRdf(rs[objVar.vname])));
                }
                return trips;
            }


            OnExternal();
            List<Triple> ts;
            if (this._p.TryGetValue(pred, out ts))
            {
                return (ts != null ? ts : Enumerable.Empty<Triple>());
            }
            else
            {
                return Enumerable.Empty<Triple>();
            }
        }

        private Triple NewTriple(INode s, INode p, INode o)
        {
            IGraph g = Graph;
            return new Triple(s.CopyWNode(g), p.CopyWNode(g), o.CopyWNode(g));
        }

        /// <summary>
        /// Gets all the triples with a given subject
        /// </summary>
        /// <param name="subj">Subject</param>
        /// <returns></returns>
        public override IEnumerable<Triple> WithSubject(INode subj)
        {
            if (UsePrologKB)
            {
                var t = MakeTerm(subj, _predVar, _objVar);
                ListOfBindings result = AskQuery(t);
                List<Triple> trips = new List<Triple>();
                foreach (var rs in result)
                {
                    trips.Add(NewTriple(subj, PartToRdf(rs["PRED"]), PartToRdf(rs["O"])));
                }
                return trips;
            }
            OnExternal();
            List<Triple> ts;
            if (this._s.TryGetValue(subj, out ts))
            {
                return (ts != null ? ts : Enumerable.Empty<Triple>());
            }
            else
            {
                return Enumerable.Empty<Triple>();
            }
        }

        private SIProlog.Term MakeTerm(INode subj, INode pred, INode obj)
        {
            if (!GlobalSharedSettings.TODO1Completed)
            {
                return new SIProlog.Term(SIProlog.TripleName, false,
                                         new PartList(RdfToPart(subj), RdfToPart(pred), RdfToPart(obj)));
            }
            string f = SIProlog.GraphWithDef.GetPredFunctor(pred);
            bool isVar = pred is IVariableNode || pred is IBlankNode;
            return new SIProlog.Term(f, isVar, new PartList(RdfToPart(subj), RdfToPart(obj)));
        }

        /// <summary>
        /// Gets all the triples with a given predicate and object
        /// </summary>
        /// <param name="pred">Predicate</param>
        /// <param name="obj">Object</param>
        /// <returns></returns>
        public override IEnumerable<Triple> WithPredicateObject(INode pred, INode obj)
        {
            if (UsePrologKB)
            {
                var t = MakeTerm(_subjVar, _predVar, obj);
                ListOfBindings result = AskQuery(t);
                List<Triple> trips = new List<Triple>();
                foreach (var rs in result)
                {
                    trips.Add(NewTriple(PartToRdf(rs["S"]), pred, obj));
                }
                return trips;
            }
            OnExternal();
            if (this._fullIndexing)
            {
                List<Triple> ts;
                if (this._po.TryGetValue(NewTriple(this._subjVar.CopyWNode(pred.Graph), pred, obj.CopyWNode(pred.Graph)), out ts))
                {
                    return (ts != null ? ts : Enumerable.Empty<Triple>());
                }
                else
                {
                    return Enumerable.Empty<Triple>();
                }
            }
            else
            {
                return this.WithPredicate(pred).Where(t => t.Object.Equals(obj));
            }
        }

        /// <summary>
        /// Gets all the triples with a given subject and object
        /// </summary>
        /// <param name="subj">Subject</param>
        /// <param name="obj">Object</param>
        /// <returns></returns>
        public override IEnumerable<Triple> WithSubjectObject(INode subj, INode obj)
        {
            if (UsePrologKB)
            {
                var t = MakeTerm(subj, _predVar, obj);
                ListOfBindings result = AskQuery(t);
                string vname = predVar.vname;
                List<Triple> trips = new List<Triple>();
                foreach (var rs in result)
                {
                    trips.Add(NewTriple(subj, PartToRdf(rs[vname]), obj));
                }
                return trips;
            }
            OnExternal();
            if (this._fullIndexing)
            {
                List<Triple> ts;
                if (this._so.TryGetValue(
                    NewTriple(subj, this._predVar.CopyWNode(subj.Graph), obj.CopyWNode(subj.Graph)), out ts))
                {
                    return (ts != null ? ts : Enumerable.Empty<Triple>());
                }
                else
                {
                    return Enumerable.Empty<Triple>();
                }
            }
            else
            {
                return this.WithSubject(subj).Where(t => t.Object.Equals(obj));
            }
        }

        /// <summary>
        /// Gets all the triples with a given subject and predicate
        /// </summary>
        /// <param name="subj">Subject</param>
        /// <param name="pred">Predicate</param>
        /// <returns></returns>
        public override IEnumerable<Triple> WithSubjectPredicate(INode subj, INode pred)
        {
            if (UsePrologKB)
            {
                bool[] vars = { false, false, true };
                var t = MakeTerm(subj,pred,_objVar);
                ListOfBindings result = AskQuery(t);
                List<Triple> trips = new List<Triple>();
                foreach (var rs in result)
                {
                    trips.Add(NewTriple(subj, pred, PartToRdf(rs["O"])));
                }
                return trips;
            }
            OnExternal();
            if (this._fullIndexing)
            {
                List<Triple> ts;
                if (this._sp.TryGetValue(NewTriple(subj, pred.CopyWNode(subj.Graph), this._objVar.CopyWNode(subj.Graph)), out ts))
                {
                    return (ts != null ? ts : Enumerable.Empty<Triple>());
                }
                else
                {
                    return Enumerable.Empty<Triple>();
                }
            }
            else
            {
                return this.WithSubject(subj).Where(t => t.Predicate.Equals(pred));
            }
        }

        private SIProlog.Part RdfToPart(INode subj)
        {
            if (subj == _subjVar) return subjVar;
            if (subj == _objVar) return objVar;
            if (subj == _predVar) return predVar;
            SIProlog.checkNode(subj);
            return SIProlog.GraphWithDef.RdfToPart(subj.CopyWNode(subj.Graph ?? Graph), rdfRules);
        }

        private INode PartToRdf(SIProlog.Part p)
        {
            INode node = SIProlog.GraphWithDef.PartToRdf(p, rdfRules);
            SIProlog.checkNode(node);
            return node;
        }

        /// <summary>
        /// Gets the Object Nodes
        /// </summary>
        public override IEnumerable<INode> ObjectNodes
        {
            get
            {
                if (UsePrologKB)
                {
                    HashSet<INode> nodes = new HashSet<INode>();
                    foreach (var t in GetTriples())
                    {
                        nodes.Add(t.Object);
                    }
                    return nodes;
                }
                OnExternal();
                return this._o.Keys;
            }
        }

        /// <summary>
        /// Gets the Predicate Nodes
        /// </summary>
        public override IEnumerable<INode> PredicateNodes
        {
            get
            {
                if (UsePrologKB)
                {
                    HashSet<INode> nodes = new HashSet<INode>();
                    foreach (var t in GetTriples())
                    {
                        nodes.Add(t.Predicate);
                    }
                    return nodes;
                }
                OnExternal();
                return this._p.Keys;
            }
        }

        /// <summary>
        /// Gets the Subject Nodes
        /// </summary>
        public override IEnumerable<INode> SubjectNodes
        {
            get
            {
                if (UsePrologKB)
                {
                    HashSet<INode> nodes = new HashSet<INode>();
                    foreach (var t in GetTriples())
                    {
                        nodes.Add(t.Subject);
                    }
                    return nodes;
                }
                OnExternal();
                return this._s.Keys;
            }
        }

        /// <summary>
        /// Disposes of the collection
        /// </summary>
        public override void Dispose()
        {
            this._triplesIndex.Clear();
            this._s.Clear();
            this._p.Clear();
            this._o.Clear();

            if (this._fullIndexing)
            {
                this._so.Clear();
                this._sp.Clear();
                this._po.Clear();
            }
        }

                /// <summary>
        /// Gets the enumerator for the collection
        /// </summary>
        /// <returns></returns>
        public override IEnumerator<Triple> GetEnumerator()
        {
            return GetTriples().GetEnumerator();
        }

        public IEnumerable<Triple> GetTriples()
        {
            if (UsePrologKB)
            {
                List<Triple> trips = new List<Triple>();
                if (GlobalSharedSettings.TODO1Completed)
                {
                    lock (PrologKB.CompileLock)
                    {
                        foreach (var p in PrologKB.pdb.rules.ToArray())
                        {
                            trips.AddRange(p.ToTriples(PrologKB, Graph).ToTriples);
                        }
                    }
                } else
                {
                    var result = AskQuery(MakeTerm(_subjVar, _predVar, _objVar));
                    foreach (var rs in result)
                    {
                        trips.Add(NewTriple(PartToRdf(rs[subjVar.vname]), PartToRdf(rs[predVar.vname]),
                                            PartToRdf(rs[objVar.vname])));
                    }
                }
                return trips;
            }
            OnExternal();
            return this._triplesIndex.Keys;
        }

        private void OnExternal()
        {
            if (!Monitoring) return;
            // set breakpoint here
            return;
        }
    }
    /// <summary>
    /// A Thread Safe version of the <see cref="Graph">Graph</see> class
    /// </summary>
    /// <threadsafety instance="true">Should be safe for almost any concurrent read and write access scenario, internally managed using a <see cref="ReaderWriterLockSlim">ReaderWriterLockSlim</see>.  If you encounter any sort of Threading/Concurrency issue please report to the <a href="mailto:dotnetrdf-bugs@lists.sourceforge.net">dotNetRDF Bugs Mailing List</a></threadsafety>
    /// <remarks>Performance will be marginally worse than a normal <see cref="Graph">Graph</see> but in multi-threaded scenarios this will likely be offset by the benefits of multi-threading.</remarks>
    public class LiveCallGraph
        : Graph, INodeFactory, IGraph
    {
        private bool _isolatedNs;
        public bool IsolatedNs
        {
            get { return _isolatedNs; }
        }

        /// <summary>
        /// Locking Manager for the Graph
        /// </summary>
        protected ReaderWriterLockSlim _lockManager = new ReaderWriterLockSlim(LockRecursionPolicy.SupportsRecursion);
        protected bool monitorObjCreation = false;
        protected INodeFactory _factory = null;
        internal SIProlog.PNode prologKB;
        internal BaseTripleCollection orig;
        public LiveCallTripleCollection LiveTriples
        {
            get
            {
                LiveCallTripleCollection v = orig as LiveCallTripleCollection;
                if (v == null) throw new InvalidCastException("" + orig.GetType());
                return v;
            }
        }

        public bool UsePNode
        {
            get
            {
                if (!GlobalSharedSettings.RdfSavedInPDB) return false;

                if (orig is LiveCallTripleCollection)
                {
                    LiveCallTripleCollection v = (LiveCallTripleCollection)orig;
                    if (!v.UsePrologKB)
                    {
                        return false;
                    }
                }
                return PrologKB != null;
            }
        }
        internal SIProlog.PNode PrologKB
        {
            get
            {
                if (!GlobalSharedSettings.RdfSavedInPDB) return null;
                if (prologKB == null)
                {
                    prologKB = LiveTriples.PrologKB;
                }
                return prologKB;
            }
            set
            {
                prologKB = value;
            }
        }

        public string DefaultKbName
        {
            get
            {
                if (!GlobalSharedSettings.RdfSavedInPDB) return null;
                if (prologKB != null)
                {
                    return prologKB.Id;
                }
                return LiveTriples.DefaultKBName;
            }
            set
            {
                LiveTriples.DefaultKBName = value;
            }
        }

        /// <summary>
        /// Enters the write lock
        /// </summary>
        protected void EnterWriteLock()
        {
#if !NO_RWLOCK
            this._lockManager.EnterWriteLock();
#else
            Monitor.Enter(this._triples);
#endif
        }

        /// <summary>
        /// Exists the write lock
        /// </summary>
        protected void ExitWriteLock()
        {
#if !NO_RWLOCK
            this._lockManager.ExitWriteLock();
#else
            Monitor.Exit(this._triples);
#endif
        }

        protected void WriteOp(Action op)
        {
            try
            {
                EnterWriteLock();
                op();
            }
            finally
            {
                ExitWriteLock();
            }
        }

        protected R WriteFunction<R>(Func<R> op)
        {
            try
            {
                EnterWriteLock();
                return op();
            }
            finally
            {
                ExitWriteLock();
            }
        }

        /// <summary>
        /// Enters the read lock
        /// </summary>
        protected void EnterReadLock()
        {
#if !NO_RWLOCK
            this._lockManager.EnterReadLock();
#else
            Monitor.Enter(this._triples);
#endif
        }

        /// <summary>
        /// Exists the read lock
        /// </summary>
        protected void ExitReadLock()
        {
#if !NO_RWLOCK
            this._lockManager.ExitReadLock();
#else
            Monitor.Exit(this._triples);
#endif
        }

        protected R ReadOp<R>(Func<R> op)
        {
            R b = default(R);
            try
            {
                EnterReadLock();
                b = op();
            }
            finally
            {
                ExitReadLock();
            }
            return b;
        }

        protected void SharedConstructor()
        {
            if (!IsolatedNs)
            {
                this._nsmapper = SIProlog.rdfDefNS;
            }
            if (orig is WrapperTripleCollection)
            {
                WrapperTripleCollection tstc = (WrapperTripleCollection)orig;
                //    var ts = tstc._triples;
            }
            if (orig is LiveCallTripleCollection)
            {
                LiveCallTripleCollection tstc = (LiveCallTripleCollection)orig;
                tstc.Graph = this;
                //    var ts = tstc._triples;
            }
        }

        /// <summary>
        /// Creates a new Thread Safe Graph
        /// </summary>
        public LiveCallGraph()
            : this(new LiveCallTripleCollection(null), false, true)
        {
        }

        /// <summary>
        /// Creates a new instance of a Graph with an optionally empty Namespace Map
        /// </summary>
        /// <param name="emptyNamespaceMap">Whether the Namespace Map should be empty</param>
        public LiveCallGraph(bool emptyNamespaceMap)
            : this(new LiveCallTripleCollection(null), emptyNamespaceMap, true)
        {
        }

        /// <summary>
        /// Creates a new Thread Safe graph using the given Triple Collection
        /// </summary>
        /// <param name="tripleCollection">Triple Collection</param>
        public LiveCallGraph(BaseTripleCollection tripleCollection)
            : this(tripleCollection, false, true)
        {
        }

        /// <summary>
        /// Creates a new instance of a Graph using the given Triple Collection and an optionally empty Namespace Map
        /// </summary>
        /// <param name="tripleCollection">Triple Collection</param>
        /// <param name="emptyNamespaceMap">Whether the Namespace Map should be empty</param>
        /// <param name="isolatedNS">Whether the Namespace is unshared</param>
        public LiveCallGraph(BaseTripleCollection tripleCollection, bool emptyNamespaceMap, bool isolatedNS)
            : base(WrapOnceInTSTCollection(tripleCollection))
        {
            orig = tripleCollection;
            _isolatedNs = isolatedNS;
            SharedConstructor();
            if (emptyNamespaceMap)
            {
                this._nsmapper.Clear();
            }
        }

        public override string ToString()
        {
            return string.Format("IsolatedNs={1} Orig={2} prologKB={3} {0}", base.ToString(), IsolatedNs, orig, prologKB);
        }
        /// <summary>
        /// Creates a new instance of a Graph using the given Triple Collection and an optionally empty Namespace Map
        /// </summary>
        /// <param name="tripleCollection">Triple Collection</param>
        /// <param name="ns">The Namespace Map should be empty</param>
        public LiveCallGraph(BaseTripleCollection tripleCollection, NamespaceMapper ns)
            : base(WrapOnceInTSTCollection(tripleCollection))
        {
            orig = tripleCollection;
            this._nsmapper = ns ?? this._nsmapper;
            _isolatedNs = true;
            SharedConstructor();
            _isolatedNs = false;
        }

        private static ThreadSafeTripleCollection WrapOnceInTSTCollection(BaseTripleCollection tripleCollection)
        {
            if (tripleCollection is ThreadSafeTripleCollection) return (ThreadSafeTripleCollection)tripleCollection;
            return new ThreadSafeTripleCollection(tripleCollection);
        }


        /// <summary>
        /// Gets the Nodes of the Graph
        /// </summary>
        public override IEnumerable<INode> Nodes
        {
            get
            {
                try
                {
                    EnterReadLock();
                    return base.Nodes;
                }
                finally
                {
                    ExitReadLock();
                }
            }
        }

        public override BaseTripleCollection Triples
        {
            get
            {
                BaseTripleCollection b = default(BaseTripleCollection);
                try
                {
                    EnterReadLock();
                    b = base.Triples;
                }
                finally
                {
                    ExitReadLock();
                }
                return b;
            }
        }

        public override bool IsEmpty
        {
            get
            {
                return (this._triples.Count == 0);
            }
        }

        /// <summary>
        /// Helper function for Resolving QNames to URIs
        /// </summary>
        /// <param name="qname">QName to resolve to a Uri</param>
        /// <returns></returns>
        public override Uri ResolveQName(String qname)
        {
            return UriFactory.Create(Tools.ResolveQName(qname, this.NamespaceMap, this.BaseUri));
        }

        public override INamespaceMapper NamespaceMap
        {
            get
            {
                INamespaceMapper b = default(INamespaceMapper);
                try
                {
                    EnterReadLock();
                    b = base.NamespaceMap;
                }
                finally
                {
                    ExitReadLock();
                }
                return b;
            }
        }

        /// <summary>
        /// Merges another Graph into the current Graph
        /// </summary>
        /// <param name="g">Graph to Merge into this Graph</param>
        /// <remarks>The Graph on which you invoke this method will preserve its Blank Node IDs while the Blank Nodes from the Graph being merged in will be given new IDs as required in the scope of this Graph.</remarks>
        public override void Merge(IGraph g)
        {
            this.Merge(g, false);
        }

        /// <summary>
        /// Merges another Graph into the current Graph
        /// </summary>
        /// <param name="g">Graph to Merge into this Graph</param>
        /// <param name="keepOriginalGraphUri">Indicates that the Merge should preserve the Graph URIs of Nodes so they refer to the Graph they originated in</param>
        /// <remarks>
        /// <para>
        /// The Graph on which you invoke this method will preserve its Blank Node IDs while the Blank Nodes from the Graph being merged in will be given new IDs as required in the scope of this Graph.
        /// </para>
        /// <para>
        /// The Graph will raise the <see cref="BaseGraph.MergeRequested">MergeRequested</see> event before the Merge operation which gives any event handlers the oppurtunity to cancel this event.  When the Merge operation is completed the <see cref="Merged">Merged</see> event is raised
        /// </para>
        /// </remarks>
        public override void Merge(IGraph g, bool keepOriginalGraphUri)
        {
            WriteOp(() => MergeB(g, keepOriginalGraphUri));
        }

        public virtual void MergeB(IGraph g, bool keepOriginalGraphUri)
        {
            if (ReferenceEquals(this, g)) throw new RdfException("You cannot Merge an RDF Graph with itself");

            //Check that the merge can go ahead
            if (!this.RaiseMergeRequested()) return;

            if (!keepOriginalGraphUri)
            {
                SIProlog.Warn("Not keep Original GraphUri? ");
            }

            //First copy and Prefixes across which aren't defined in this Graph
            this._nsmapper.Import(g.NamespaceMap);

            if (this.IsEmpty)
            {
                var from = g;
                //Empty Graph so do a quick copy
                foreach (Triple t in g.Triples)
                {
                    this.AssertFromGraph(new Triple(t.Subject.CopyWNode(from, keepOriginalGraphUri),
                                                    t.Predicate.CopyWNode(from, keepOriginalGraphUri),
                                                    t.Object.CopyWNode(from, keepOriginalGraphUri),
                                                    t.Context), g, keepOriginalGraphUri);
                }
            }
            else
            {
                //Prepare a mapping of Blank Nodes to Blank Nodes
                Dictionary<INode, IBlankNode> mapping = new Dictionary<INode, IBlankNode>();

                foreach (Triple t in g.Triples)
                {
                    INode s, p, o;
                    s = MapNode(keepOriginalGraphUri, mapping, t.Subject);
                    p = MapNode(keepOriginalGraphUri, mapping, t.Predicate);
                    o = MapNode(keepOriginalGraphUri, mapping, t.Object);

                    this.AssertFromGraph(new Triple(s, p, o, t.Context), g, keepOriginalGraphUri);
                }
            }

            this.RaiseMerged();
        }

        private void AssertFromGraph(Triple t, IGraph g, bool keepOriginalGraphUri)
        {
            if (!UsePNode)
            {
                this.Assert(t);
                return;
            }
            PrologKB.AssertTripleFromGraph(t, g, false, keepOriginalGraphUri);
        }

        private INode MapNode(bool keepOriginalGraphUri, Dictionary<INode, IBlankNode> mapping, INode tObject)
        {
            if (tObject.NodeType == NodeType.Blank)
            {
                INode o;
                if (!mapping.ContainsKey(tObject))
                {
                    IBlankNode temp = this.CreateBlankNode();
                    if (keepOriginalGraphUri)
                    {
                        temp.GraphUri = tObject.GraphUri;
                    } else
                    {
                        
                    }
                    mapping.Add(tObject, temp);
                }
                o = mapping[tObject];
                return o;
            }
            return tObject.CopyWNode(this, keepOriginalGraphUri);
        }

        /// <summary>
        /// Gets whether a given Triple exists in this Graph
        /// </summary>
        /// <param name="t">Triple to test</param>
        /// <returns></returns>
        public override bool ContainsTriple(Triple t)
        {
            return ReadOp(() => base.ContainsTriple(t));
        }

        /// <summary>
        /// Clears all Triples from the Graph
        /// </summary>
        /// <remarks>
        /// <para>
        /// The Graph will raise the <see cref="BaseGraph.ClearRequested">ClearRequested</see> event at the start of the Clear operation which allows for aborting the operation if the operation is cancelled by an event handler.  On completing the Clear the <see cref="BaseGraph.Cleared">Cleared</see> event will be raised.
        /// </para>
        /// </remarks>
        public override void Clear()
        {
            WriteOp(base.Clear);
        }


        #region Triple Assertion and Retraction

        /// <summary>
        /// Asserts a Triple in the Graph
        /// </summary>
        /// <param name="t">The Triple to add to the Graph</param>
        public override bool Assert(Triple t)
        {
            try
            {
                EnterWriteLock();
                return base.Assert(t);
            }
            finally
            {
                ExitWriteLock();
            }
        }

        /// <summary>
        /// Asserts a List of Triples in the graph
        /// </summary>
        /// <param name="ts">List of Triples in the form of an IEnumerable</param>
        public override bool Assert(IEnumerable<Triple> ts)
        {
            try
            {
                // calls back our code
                EnterWriteLock();
                return base.Assert(ts);
            }
            finally
            {
                ExitWriteLock();
            }
        }

        /// <summary>
        /// Retracts a Triple from the Graph
        /// </summary>
        /// <param name="t">Triple to Retract</param>
        /// <remarks>Current implementation may have some defunct Nodes left in the Graph as only the Triple is retracted</remarks>
        public override bool Retract(Triple t)
        {
            try
            {
                EnterWriteLock();
                return base.Retract(t);
            }
            finally
            {
                ExitWriteLock();
            }
        }

        /// <summary>
        /// Retracts a enumeration of Triples from the graph
        /// </summary>
        /// <param name="ts">Enumeration of Triples to retract</param>
        public override bool Retract(IEnumerable<Triple> ts)
        {
            try
            {
                // calls back our code
                EnterWriteLock();
                return base.Retract(ts);
            }
            finally
            {
                ExitWriteLock();
            }
        }

        #endregion

        /// <summary>
        /// Creates a new Blank Node ID and returns it
        /// </summary>
        /// <returns></returns>
        public override string GetNextBlankNodeID()
        {
            String id = String.Empty;
            try
            {
                if (monitorObjCreation) EnterWriteLock();
                id = base.GetNextBlankNodeID();
            }
            finally
            {
                if (monitorObjCreation) ExitWriteLock();
                if (id.Equals(String.Empty))
                {
                    throw new RdfException("Unable to generate a new Blank Node ID due to a Threading issue");
                }
            }
            return id;
        }

        /// <summary>
        /// Disposes of a Graph
        /// </summary>
        public override void Dispose()
        {
            base.Dispose();
            Dispose();
        }

        #region Node Selection

        /// <summary>
        /// Returns the Blank Node with the given Identifier
        /// </summary>
        /// <param name="nodeId">The Identifier of the Blank Node to select</param>
        /// <returns>Either the Blank Node or null if no Node with the given Identifier exists</returns>
        public override IBlankNode GetBlankNode(string nodeId)
        {
            IBlankNode b = null;
            try
            {
                if (monitorObjCreation) EnterReadLock();
                b = base.GetBlankNode(nodeId);
            }
            finally
            {
                if (monitorObjCreation) ExitReadLock();
            }
            return b;
        }

        /// <summary>
        /// Returns the LiteralNode with the given Value if it exists
        /// </summary>
        /// <param name="literal">The literal value of the Node to select</param>
        /// <returns>Either the LiteralNode Or null if no Node with the given Value exists</returns>
        /// <remarks>The LiteralNode in the Graph must have no Language or DataType set</remarks>
        public override ILiteralNode GetLiteralNode(string literal)
        {
            ILiteralNode l = null;
            try
            {
                if (monitorObjCreation) EnterReadLock();
                l = base.GetLiteralNode(literal);
            }
            finally
            {
                if (monitorObjCreation) ExitReadLock();
            }
            return l;
        }

        /// <summary>
        /// Returns the LiteralNode with the given Value in the given Language if it exists
        /// </summary>
        /// <param name="literal">The literal value of the Node to select</param>
        /// <param name="langspec">The Language Specifier for the Node to select</param>
        /// <returns>Either the LiteralNode Or null if no Node with the given Value and Language Specifier exists</returns>
        public override ILiteralNode GetLiteralNode(string literal, string langspec)
        {
            ILiteralNode l = null;
            try
            {
                if (monitorObjCreation) EnterReadLock();
                l = base.GetLiteralNode(literal, langspec);
            }
            finally
            {
                if (monitorObjCreation) ExitReadLock();
            }
            return l;
        }

        /// <summary>
        /// Returns the LiteralNode with the given Value and given Data Type if it exists
        /// </summary>
        /// <param name="literal">The literal value of the Node to select</param>
        /// <param name="datatype">The Uri for the Data Type of the Literal to select</param>
        /// <returns>Either the LiteralNode Or null if no Node with the given Value and Data Type exists</returns>
        public override ILiteralNode GetLiteralNode(string literal, Uri datatype)
        {
            ILiteralNode l = null;
            try
            {
                if (monitorObjCreation) EnterReadLock();
                l = base.GetLiteralNode(literal, datatype);
            }
            finally
            {
                if (monitorObjCreation) ExitReadLock();
            }
            return l;
        }

        /// <summary>
        /// Returns the UriNode with the given QName if it exists
        /// </summary>
        /// <param name="qname">The QName of the Node to select</param>
        /// <returns></returns>
        public override IUriNode GetUriNode(string qname)
        {
            IUriNode u = null;
            try
            {
                if (monitorObjCreation) EnterReadLock();
                u = base.GetUriNode(qname);
            }
            finally
            {
                if (monitorObjCreation) ExitReadLock();
            }
            return u;
        }
        /// <summary>
        /// Creates a new URI Node with the given URI
        /// </summary>
        /// <param name="uri">URI for the Node</param>
        /// <returns></returns>
        /// <remarks>
        /// Generally we expect to be passed an absolute URI, while relative URIs are permitted the behaviour is less well defined.  If there is a Base URI defined for the Graph then relative URIs will be automatically resolved against that Base, if the Base URI is not defined then relative URIs will be left as is.  In this case issues may occur when trying to serialize the data or when accurate round tripping is required.
        /// </remarks>
        public override IUriNode CreateUriNode(Uri uri)
        {
            var f = this._factory; if (f != null) return f.CreateUriNode(uri);
            if (!uri.IsAbsoluteUri && this._baseuri != null)
            {
                uri = Tools.ResolveUri(uri, this._baseuri);
            }
            IUriNode u = null;
            try
            {
                if (monitorObjCreation) EnterWriteLock();
                u = base.CreateUriNode(uri);
            }
            finally
            {
                if (monitorObjCreation) ExitWriteLock();
            }
            return u;
        }

        /// <summary>
        /// Creates a new URI Node with the given QName
        /// </summary>
        /// <param name="qname">QName for the Node</param>
        /// <returns></returns>
        /// <remarks>Internally the Graph will resolve the QName to a full URI, throws an RDF Exception when this is not possible</remarks>
        public override IUriNode CreateUriNode(String qname)
        {
            IUriNode u = null;
            try
            {
                if (monitorObjCreation) EnterWriteLock();
                u = base.CreateUriNode(qname);
            }
            finally
            {
                if (monitorObjCreation) ExitWriteLock();
            }
            return u;
        }

        #region INodeFactory Members

        /// <summary>
        /// Creates a Blank Node
        /// </summary>
        /// <returns></returns>
        public override IBlankNode CreateBlankNode()
        {
            var f = this._factory; if (f != null) return f.CreateBlankNode();
            return WriteFunction<IBlankNode>(base.CreateBlankNode);
        }

        /// <summary>
        /// Creates a Blank Node with the given ID
        /// </summary>
        /// <param name="nodeId">Node ID</param>
        /// <returns></returns>
        public override IBlankNode CreateBlankNode(string nodeId)
        {
            var f = this._factory; if (f != null) return f.CreateBlankNode(nodeId);
            return WriteFunction(() => base.CreateBlankNode(nodeId));
        }

        /// <summary>
        /// Creates a Graph Literal Node
        /// </summary>
        /// <returns></returns>
        public override IGraphLiteralNode CreateGraphLiteralNode()
        {
            var f = this._factory; if (f != null) return f.CreateGraphLiteralNode();
            return WriteFunction(() => base.CreateGraphLiteralNode());
        }

        /// <summary>
        /// Creates a Graph Literal Node with the given sub-graph
        /// </summary>
        /// <param name="subgraph">Sub-graph</param>
        /// <returns></returns>
        public override IGraphLiteralNode CreateGraphLiteralNode(IGraph subgraph)
        {
            var f = this._factory; if (f != null) return f.CreateGraphLiteralNode(subgraph);
            return WriteFunction(() => base.CreateGraphLiteralNode(subgraph));
        }

        /// <summary>
        /// Creates a Literal Node with the given Datatype
        /// </summary>
        /// <param name="literal">Value</param>
        /// <param name="datatype">Datatype URI</param>
        /// <returns></returns>
        public override ILiteralNode CreateLiteralNode(string literal, Uri datatype)
        {
            var f = this._factory; if (f != null) return f.CreateLiteralNode(literal, datatype);
            return WriteFunction(() => base.CreateLiteralNode(literal, datatype));
        }

        /// <summary>
        /// Creates a Literal Node
        /// </summary>
        /// <param name="literal">Value</param>
        /// <returns></returns>
        public override ILiteralNode CreateLiteralNode(string literal)
        {
            var f = this._factory; if (f != null) return f.CreateLiteralNode(literal);
            return WriteFunction(() => base.CreateLiteralNode(literal));
        }

        /// <summary>
        /// Creates a Literal Node with the given Language
        /// </summary>
        /// <param name="literal">Value</param>
        /// <param name="langspec">Language</param>
        /// <returns></returns>
        public override ILiteralNode CreateLiteralNode(string literal, string langspec)
        {
            var f = this._factory; if (f != null) return f.CreateLiteralNode(literal, langspec);
            return WriteFunction(() => base.CreateLiteralNode(literal, langspec));
        }

        /// <summary>
        /// Creates a Variable Node
        /// </summary>
        /// <param name="varname">Variable Name</param>
        /// <returns></returns>
        public override IVariableNode CreateVariableNode(string varname)
        {
            var f = this._factory; if (f != null) return f.CreateVariableNode(varname);
            return WriteFunction(() => base.CreateVariableNode(varname));
        }

        #endregion

        /// <summary>
        /// Returns the UriNode with the given Uri if it exists
        /// </summary>
        /// <param name="uri">The Uri of the Node to select</param>
        /// <returns>Either the UriNode Or null if no Node with the given Uri exists</returns>
        public override IUriNode GetUriNode(Uri uri)
        {
            IUriNode u = null;
            try
            {
                if (monitorObjCreation) EnterReadLock();
                u = base.GetUriNode(uri);
            }
            finally
            {
                if (monitorObjCreation) ExitReadLock();
            }
            return u;
        }

        #endregion

        #region Triple Selection

        /// <summary>
        /// Gets all the Triples involving the given Node
        /// </summary>
        /// <param name="n">The Node to find Triples involving</param>
        /// <returns>Zero/More Triples</returns>
        public override IEnumerable<Triple> GetTriples(INode n)
        {
            try
            {
                EnterReadLock();
                // we might have smoother impl
                return base.GetTriples(n);
            }
            finally
            {
                ExitReadLock();
            }
        }

        /// <summary>
        /// Gets all the Triples involving the given Uri
        /// </summary>
        /// <param name="uri">The Uri to find Triples involving</param>
        /// <returns>Zero/More Triples</returns>
        public override IEnumerable<Triple> GetTriples(Uri uri)
        {
            List<Triple> triples = new List<Triple>();
            try
            {
                EnterReadLock();
                triples = base.GetTriples(uri).ToList();
            }
            finally
            {
                ExitReadLock();
            }
            return triples;
        }

        /// <summary>
        /// Gets all the Triples with the given Node as the Object
        /// </summary>
        /// <param name="n">The Node to find Triples with it as the Object</param>
        /// <returns></returns>
        public override IEnumerable<Triple> GetTriplesWithObject(INode n)
        {
            List<Triple> triples = new List<Triple>();
            try
            {
                EnterReadLock();
                triples = base.GetTriplesWithObject(n).ToList();
            }
            finally
            {
                ExitReadLock();
            }
            return triples;
        }

        /// <summary>
        /// Gets all the Triples with the given Uri as the Object
        /// </summary>
        /// <param name="u">The Uri to find Triples with it as the Object</param>
        /// <returns>Zero/More Triples</returns>
        public override IEnumerable<Triple> GetTriplesWithObject(Uri u)
        {
            List<Triple> triples = new List<Triple>();
            try
            {
                EnterReadLock();
                triples = base.GetTriplesWithObject(u).ToList();
            }
            finally
            {
                ExitReadLock();
            }
            return triples;
        }

        /// <summary>
        /// Gets all the Triples with the given Node as the Predicate
        /// </summary>
        /// <param name="n">The Node to find Triples with it as the Predicate</param>
        /// <returns></returns>
        public override IEnumerable<Triple> GetTriplesWithPredicate(INode n)
        {
            List<Triple> triples = new List<Triple>();
            try
            {
                EnterReadLock();                
                triples = base.GetTriplesWithPredicate(n).ToList();
            }
            finally
            {
                ExitReadLock();
            }
            return triples;
        }

        /// <summary>
        /// Gets all the Triples with the given Uri as the Predicate
        /// </summary>
        /// <param name="u">The Uri to find Triples with it as the Predicate</param>
        /// <returns>Zero/More Triples</returns>
        public override IEnumerable<Triple> GetTriplesWithPredicate(Uri u)
        {
            List<Triple> triples = new List<Triple>();
            try
            {
                EnterReadLock();
                triples = base.GetTriplesWithPredicate(u).ToList();
            }
            finally
            {
                ExitReadLock();
            }
            return triples;
        }

        /// <summary>
        /// Gets all the Triples with the given Node as the Subject
        /// </summary>
        /// <param name="n">The Node to find Triples with it as the Subject</param>
        /// <returns>Zero/More Triples</returns>
        public override IEnumerable<Triple> GetTriplesWithSubject(INode n)
        {
            List<Triple> triples = new List<Triple>();
            try
            {
                EnterReadLock();
                triples = base.GetTriplesWithSubject(n).ToList();
            }
            finally
            {
                ExitReadLock();
            }
            return triples;
        }

        /// <summary>
        /// Gets all the Triples with the given Uri as the Subject
        /// </summary>
        /// <param name="u">The Uri to find Triples with it as the Subject</param>
        /// <returns>Zero/More Triples</returns>
        public override IEnumerable<Triple> GetTriplesWithSubject(Uri u)
        {
            List<Triple> triples = new List<Triple>();
            try
            {
                EnterReadLock();
                triples = base.GetTriplesWithSubject(u).ToList();
            }
            finally
            {
                ExitReadLock();
            }
            return triples;
        }

        /// <summary>
        /// Selects all Triples with the given Subject and Predicate
        /// </summary>
        /// <param name="subj">Subject</param>
        /// <param name="pred">Predicate</param>
        /// <returns></returns>
        public override IEnumerable<Triple> GetTriplesWithSubjectPredicate(INode subj, INode pred)
        {
            IEnumerable<Triple> triples = new List<Triple>();
            try
            {
                EnterReadLock();
                triples = base.GetTriplesWithSubjectPredicate(subj, pred);
            }
            finally
            {
                ExitReadLock();
            }
            return triples;
        }

        /// <summary>
        /// Selects all Triples with the given Subject and Object
        /// </summary>
        /// <param name="subj">Subject</param>
        /// <param name="obj">Object</param>
        /// <returns></returns>
        public override IEnumerable<Triple> GetTriplesWithSubjectObject(INode subj, INode obj)
        {
            IEnumerable<Triple> triples = new List<Triple>();
            try
            {
                EnterReadLock();
                triples = base.GetTriplesWithSubjectObject(subj, obj);
            }
            finally
            {
                ExitReadLock();
            }
            return triples;
        }

        /// <summary>
        /// Selects all Triples with the given Predicate and Object
        /// </summary>
        /// <param name="pred">Predicate</param>
        /// <param name="obj">Object</param>
        /// <returns></returns>
        public override IEnumerable<Triple> GetTriplesWithPredicateObject(INode pred, INode obj)
        {
            IEnumerable<Triple> triples = new List<Triple>();
            try
            {
                EnterReadLock();
                triples = base.GetTriplesWithPredicateObject(pred, obj);
            }
            finally
            {
                ExitReadLock();
            }
            return triples;
        }
        #endregion

        public bool HostedFrom(SIProlog.PNode pNode)
        {
            return UsePNode && PrologKB == pNode;
        }
    }
}
