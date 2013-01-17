#define MERGED_RDFSTORE
using System;
using System.Collections;
using System.Collections.Generic;
using System.Globalization;
using System.IO;
using System.Linq;
using System.Reflection;
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
using System.Text;
using VDS.Common;
using VDS.Common.Trees;

namespace LogicalParticleFilter1
{
    public partial class SIProlog
    {
        public void EndpointCreated(PFEndpoint endpoint)
        {
            if (RdfDeveloperSanityChecks < 2) return;
            DLRConsole.PrintOnlyThisThread = Thread.CurrentThread;
            ConsoleWriteLine("Endpoint and bot are created by now");
            try
            {
                if (RdfDeveloperSanityChecks > 0) Program.RunAllTests(this);
            }
            finally
            {
                DLRConsole.PrintOnlyThisThread = null;
            }
            RunREPL();
        }

        public void RunREPL()
        {
            bool queryMode = false;
            LocalIOSettings threadLocal = GlobalSharedSettings.LocalSettings;
            while (true)
            {
                ConsoleWriteLine("-----------------------------------------------------------------");
                string input = TextFilter.ReadLineFromInput(ConsoleWriteLine, "<" + threadLocal.curKB + "> ?- ");
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

        public LiveCallGraph NewGraph(string rdfglobaldefsmt)
        {
            LiveCallTripleCollection lcTC = new LiveCallTripleCollection(this);
            lcTC.defaultKB = rdfglobaldefsmt;
            return new LiveCallGraph(lcTC, rdfDefNS);
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
        :
        ThreadSafeTripleCollection
    //BaseTripleCollection
    {
        public bool Monitoring;

        //Main Storage
        private MultiDictionary<Triple, Object> _triples = new MultiDictionary<Triple, object>(new FullTripleComparer(new FastNodeComparer()));
        //Simple Indexes
        private MultiDictionary<INode, List<Triple>> _s = new MultiDictionary<INode, List<Triple>>(new FastNodeComparer(), MultiDictionaryMode.AVL),
                                                     _p = new MultiDictionary<INode, List<Triple>>(new FastNodeComparer(), MultiDictionaryMode.AVL),
                                                     _o = new MultiDictionary<INode, List<Triple>>(new FastNodeComparer(), MultiDictionaryMode.AVL);
        //Compound Indexes
        private MultiDictionary<Triple, List<Triple>> _sp, _so, _po;


        private class PlVariableNode : VariableNode
        {
            protected internal PlVariableNode(IGraph g, String varname)
                : base(g, varname) { }
        }

        //Placeholder Variables for compound lookups
        private VariableNode _subjVar = new PlVariableNode(null, "s");
        private VariableNode _predVar = new PlVariableNode(null, "p"),
                             _objVar = new PlVariableNode(null, "o");

        private bool _fullIndexing = false;
        private int _count = 0;

        private SIProlog _prologEngine;
        public SIProlog prologEngine
        {
            get { return _prologEngine ?? SIProlog.CurrentProlog; }
            set { _prologEngine = value; }
        }

        public string defaultKB;

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
            OnExternal();
            if (!this.Contains(t))
            {
                this._triples.Add(t, null);
                this.Index(t);
                this._count++;
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
            OnExternal();
            return this._triples.ContainsKey(t);
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
            OnExternal();
            if (this._triples.Remove(t))
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
                OnExternal();
                Triple actual;
                if (this._triples.TryGetKey(t, out actual))
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

        /// <summary>
        /// Gets all the triples with a given subject
        /// </summary>
        /// <param name="subj">Subject</param>
        /// <returns></returns>
        public override IEnumerable<Triple> WithSubject(INode subj)
        {
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

        /// <summary>
        /// Gets all the triples with a given predicate and object
        /// </summary>
        /// <param name="pred">Predicate</param>
        /// <param name="obj">Object</param>
        /// <returns></returns>
        public override IEnumerable<Triple> WithPredicateObject(INode pred, INode obj)
        {
            OnExternal();
            if (this._fullIndexing)
            {
                List<Triple> ts;
                if (this._po.TryGetValue(new Triple(this._subjVar.CopyNode(pred.Graph), pred, obj.CopyNode(pred.Graph)), out ts))
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
            OnExternal();
            if (this._fullIndexing)
            {
                List<Triple> ts;
                if (this._so.TryGetValue(new Triple(subj, this._predVar.CopyNode(subj.Graph), obj.CopyNode(subj.Graph)), out ts))
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
            OnExternal();
            if (this._fullIndexing)
            {
                List<Triple> ts;
                if (this._sp.TryGetValue(new Triple(subj, pred.CopyNode(subj.Graph), this._objVar.CopyNode(subj.Graph)), out ts))
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

        /// <summary>
        /// Gets the Object Nodes
        /// </summary>
        public override IEnumerable<INode> ObjectNodes
        {
            get
            {
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
                OnExternal();
                return this._s.Keys;
            }
        }

        /// <summary>
        /// Disposes of the collection
        /// </summary>
        public override void Dispose()
        {
            this._triples.Clear();
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
            OnExternal();
            return this._triples.Keys.GetEnumerator();
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
        protected INodeFactory _factory;

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
            _isolatedNs = isolatedNS;
            SharedConstructor();
            if (emptyNamespaceMap)
            {
                this._nsmapper.Clear();
            }
        }

        /// <summary>
        /// Creates a new instance of a Graph using the given Triple Collection and an optionally empty Namespace Map
        /// </summary>
        /// <param name="tripleCollection">Triple Collection</param>
        /// <param name="ns">The Namespace Map should be empty</param>
        public LiveCallGraph(BaseTripleCollection tripleCollection, NamespaceMapper ns)
            : base(WrapOnceInTSTCollection(tripleCollection))
        {
            this._nsmapper = ns ?? this._nsmapper;
            _isolatedNs = true;
            SharedConstructor();
            _isolatedNs = false;
        }

        private static ThreadSafeTripleCollection WrapOnceInTSTCollection(BaseTripleCollection tripleCollection)
        {
            if (tripleCollection is ThreadSafeTripleCollection) return (ThreadSafeTripleCollection) tripleCollection;
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
                    b =  base.Triples;
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
                    b =  base.NamespaceMap;
                }
                finally
                {
                    ExitReadLock();
                }
                return b;
            }
        }

        public override void Merge(IGraph g, bool keepOriginalGraphUri)
        {
            WriteOp(() => base.Merge(g, keepOriginalGraphUri));
        }

        public override bool ContainsTriple(Triple t)
        {
            return ReadOp(() => base.ContainsTriple(t));
        }

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
                EnterWriteLock();
                return this.Retract(ts);
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
            return WriteFunction(() => base.CreateBlankNode());
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
            List<Triple> triples = new List<Triple>();
            try
            {
                EnterReadLock();
                triples = base.GetTriples(n).ToList();
            }
            finally
            {
                ExitReadLock();
            }
            return triples;
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
    }
}
