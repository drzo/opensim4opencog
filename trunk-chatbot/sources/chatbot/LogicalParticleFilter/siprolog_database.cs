#define MERGED_RDFSTORE
using System;
using System.Collections.Generic;
using System.Collections;
using System.Data;
using System.Linq;
using System.Runtime.CompilerServices;
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
            
            public readonly Hashtable ht = new Hashtable();
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


        public class RuleList : RuleListBase, IEnumerable,IEnumerable<Rule>
        {
            internal List<Rule> arrayList = new List<Rule>();
            private bool _isReadonly;
            public static Func<Rule, Rule, bool> DefaultRuleEquality = SameClauses;

            private static bool SameClauses(Rule arg1, Rule arg2)
            {
                if (arg1 == null || arg2 == null)
                {
                    return arg1 == arg2;
                }
                return arg1.SameClause(arg2);
            }

            /// <summary>
            /// Returns a <see cref="T:System.String"/> that represents the current <see cref="T:System.Object"/>.
            /// </summary>
            /// <returns>
            /// A <see cref="T:System.String"/> that represents the current <see cref="T:System.Object"/>.
            /// </returns>
            /// <filterpriority>2</filterpriority>
            public override string ToString()
            {
                string named = syncPDB != null ? (syncPDB.startMt + ": ") : "ruleList: ";
                return named + "count=" + Count + ToSource(SourceLanguage.Prolog);
            }
            public string AToString
            {
                get { return ToString(); }
            }
            public override int Count
            {
                get { lock (Sync) return arrayList.Count; }
            }

            public override bool Add(Rule r)
            {
                lock (Sync)
                {
                    if (IsReadonly)
                    {
                        throw ErrorBadOp("Attempting to modify readonly " + this + " with " + r);
                    }
                    arrayList.Add(r);
                    if (r.OptionalHomeMt == null)
                    {
                        if (syncPDB != null)
                        {
                            string name = syncPDB.startMt;
                            if (name != null)
                            {
                                r.OptionalHomeMt = name;
                            } else
                            {
                                Warn("cant give a home to " + r);
                            }
                        }
                    }
                    ClearPdbIndexes();
                }
                return true;
            }
            public RuleList()
            {
            }

            override public void RemoveAt(int i)
            {
                lock (Sync)
                {
                    Rule r = this[i];
                    Release(r);
                    arrayList.RemoveAt(i);
                    ClearPdbIndexes();
                }
            }

            private void Release(Rule r)
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
                IEnumerable<Triple> found = ruleCache.ToTriples;
                int fnd = 0;
                foreach (Triple triple in found)
                {
                    //   ConsoleWriteLine("Remove triple: " + triple);
                    if (syncPDB == null || syncPDB.PrologKB.HostsGraph(triple.Graph))
                    {
                        triple.Graph.Retract(triple);
                    }
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

            override public Rule this[int i]
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

            public override object Sync
            {
                get
                {
                    return arrayList;
                }
            }
            private void ClearPdbIndexes()
            {
                if (IsReadonly)
                {
                    throw ErrorBadOp("Attempting to modify readonly " + this);
                }
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
            public override void Clear()
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
            /*
            public IEnumerator GetEnumerator()
            {
                lock (Sync) return arrayList.GetEnumerator();
            }
            */
            public override IEnumerator<Rule> GetRuleEnumer()
            {
                lock (Sync) return arrayList.GetEnumerator();
            }

            #region IEnumerable<Rule> Members

            IEnumerator<Rule> IEnumerable<Rule>.GetEnumerator()
            {
                lock (Sync) return arrayList.GetEnumerator();
            }

            #endregion

            override public RuleList Copy()
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
            public override bool Contains(Rule rule)
            {
                return IndexOf(rule, -1, DefaultRuleEquality) != -1;
            }
            public int IndexOf(Rule rule)
            {
                return IndexOf(rule, -1, DefaultRuleEquality);
            }

            protected internal override bool Delete(Rule rule)
            {
                int index = IndexOf(rule, -1, DefaultRuleEquality);
                if (index == -1) return false;
                RemoveAt(index);
                return false;
            }

            public override Rule this[Rule t]
            {
                get { return this[IndexOf(t)]; }
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

            public override bool IsReadonly
            {
                get { return _isReadonly; }
                set { _isReadonly = value; }
            }

            public override ICollection<Rule> TheList
            {
                get { return arrayList; }
            }

            public override void Dispose()
            {
                throw new NotImplementedException();
            }
        }

        [StructToString(true)]
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
                return this.StructToString();
            }

            public string AToString
            {
                get { return ToString(); }
            }

            public string startMt;
            public bool followedGenlMt;
            public bool isStorage;
            public static Dictionary<string,builtinDelegate> builtin = new Dictionary<string, builtinDelegate>();
            internal RuleList _rules;
            public bool IsLockedRuleList = true;

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
                lock (rules)
                {
                    for (int i = 0; i < rules.Count; i++)
                    {
                        Rule rule = (Rule) rules[i];
                        string name = rule.head.fname;
                        if (!index.ContainsKey(name))
                        {
                            index[name] = new RuleList();
                        }
                        index[name].Add(rule);
                        if (rule.head.headIsVar)
                        {
                            inx.Add(rule);
                        }
                    }
                }
            }

            public RuleList rules
            {
                get { lock (LockOf(_rules)) return _rules; }
                set
                {
                    if ((value == null) && (_rules == null)) return;
                    if (_rules != null)
                    {
                        lock (_rules.Sync)
                            lock (LockOf(_rules))
                            {
                                if (_rules.Count > 0)
                                {
                                    _rules.Clear();
                                }
                                if (ReferenceEquals(_rules, value)) return;
                                if (IsLockedRuleList)
                                {
                                    if (!ReferenceEquals(null, value))
                                    {
                                        value.IsReadonly = true;
                                        foreach (Rule rule in value)
                                        {
                                            _rules.Add(rule);
                                        }
                                    }
                                    return;
                                }
                            }
                    }
                    if (value != null)
                    {
                        value.syncPDB = this;
                    }
                    _rules = value;
                }
            }

            public static object lockIfNull = new object();

            private object LockOf(RuleList ruleList)
            {
                if (ruleList == null) return lockIfNull;
                var l2 = ruleList.syncPDB;
                if (l2 == null) return lockIfNull;
                if (l2.followedGenlMt)
                {
                    return l2;
                }
                var l3 = l2.PrologKB;
                if (l3 == null) return lockIfNull;
                return l3.CompileLock;
            }

            public PDB db
            {
                get { return this; }
            }

            [CompilerGenerated]
            public PNode PrologKB
            {
                get
                {
                    if (startMt == null) return null;
                    if (followedGenlMt)
                    {
                        Warn("Referencing PrologKB that has followed GemlMt");
                        return null;
                    }
                    return CurrentProlog.FindKB(startMt);
                }
            }

            public bool IsTraced = true;

            public void RegisterRuleList(PNode pNode)
            {
                IsLockedRuleList = true;
                ((PrologMT) pNode).RegisterRuleList(this, rules);
            }
        }
        public class PrologKBDiffReport
        {
        }
        public class ITransactionalPrologKB
        {
        }
        public class GenlMtLinkType
        {
        }
        public class BasePrologKBCollection
        {
        }

        /// <summary>
        /// Abstract Base Class for Rule Collections
        /// </summary>
        /// <remarks>
        /// Designed to allow the underlying storage of a Rule Collection to be changed at a later date without affecting classes that use it.
        /// </remarks>
        abstract public partial class RuleListBase
            : IEnumerable<Rule>, IDisposable, IEnumerable
        {
            /// <summary>
            /// Adds a Rule to the Collection
            /// </summary>
            /// <param name="t">Rule to add</param>
            /// <remarks>Adding a Rule that already exists should be permitted though it is not necessary to persist the duplicate to underlying storage</remarks>
            public abstract bool Add(Rule t);

            /// <summary>
            /// Determines whether a given Rule is in the Rule Collection
            /// </summary>
            /// <param name="t">The Rule to test</param>
            /// <returns>True if the Rule already exists in the Rule Collection</returns>
            public abstract bool Contains(Rule t);

            /// <summary>
            /// Gets the Number of Rules in the Rule Collection
            /// </summary>
            public abstract int Count
            {
                get;
            }

            /// <summary>
            /// Deletes a Rule from the Collection
            /// </summary>
            /// <param name="t">Rule to remove</param>
            /// <remarks>Deleting something that doesn't exist should have no effect and give no error</remarks>
            protected abstract internal bool Delete(Rule t);

            /// <summary>
            /// Gets the given Rule
            /// </summary>
            /// <param name="t">Rule to retrieve</param>
            /// <returns></returns>
            /// <exception cref="KeyNotFoundException">Thrown if the given Rule doesn't exist</exception>
            public abstract Rule this[Rule t]
            {
                get;
            }

            public abstract Rule this[int i] { get; set; }

            public PDB syncPDB;
            abstract public object Sync
            {
                get;
            }

            abstract public bool IsReadonly { get; set; }

            public abstract ICollection<Rule> TheList { get; }

            /// <summary>
            /// Diposes of a Rule Collection
            /// </summary>
            public abstract void Dispose();

            /// <summary>
            /// Gets the typed Enumerator for the Triple Collection
            /// </summary>
            /// <returns></returns>
            public IEnumerator<Rule> GetEnumerator()
            {
                return GetRuleEnumer();
            }

            public abstract IEnumerator<Rule> GetRuleEnumer();

            /// <summary>
            /// Gets the non-generic Enumerator for the Triple Collection
            /// </summary>
            /// <returns></returns>
            System.Collections.IEnumerator System.Collections.IEnumerable.GetEnumerator()
            {
                return GetRuleEnumer();
            }

            /// <summary>
            /// Event which occurs when a Rule is added to the Collection
            /// </summary>
            public event RuleEventHandler RuleAdded;

            /// <summary>
            /// Event which occurs when a Rule is removed from the Collection
            /// </summary>
            public event RuleEventHandler RuleRemoved;

            /// <summary>
            /// Helper method for raising the <see cref="RuleAdded">Rule Added</see> event
            /// </summary>
            /// <param name="t">Rule</param>
            protected void RaiseRuleAdded(Rule t)
            {
                RuleEventHandler d = this.RuleAdded;
                if (d != null)
                {
                    d(this, new RuleEventArgs(t, null));
                }
            }

            /// <summary>
            /// Helper method for raising the <see cref="RuleRemoved">Rule Removed</see> event
            /// </summary>
            /// <param name="t">Rule</param>
            protected void RaiseRuleRemoved(Rule t)
            {
                RuleEventHandler d = this.RuleRemoved;
                if (d != null)
                {
                    d(this, new RuleEventArgs(t, null, false));
                }
            }

            abstract public void Clear();

            public abstract void RemoveAt(int i);
            public abstract RuleList Copy();
        }
        public partial class Rule : IHasParent
        {
            public bool isGround = false;

            public string OptionalHomeMt
            {
                get { return _optHomeMt; }
                set
                {
                    if (value == null)
                    {

                    }
                    _optHomeMt = value;
                }
            }

            // Rule = (Head, Body)
            readonly public Term head = null;
            public Body body = null;
            public Rule(Term head)
            {
                this.head = head;
                isGround = head.IsGround;
                head.parent = this;
            }
            public Rule(Term head, PartListImpl bodylist)
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
                language = language.Inner();
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
            private string _optHomeMt;


            public bool SameClause(Rule rule)
            {
                var vars = new Dictionary<string, string>();
                if (!head.SameClause(rule.head, vars))
                {
                    return false;
                }
                int partLen = this.bodyLen;
                int partLen2 = rule.bodyLen;
                if (partLen2 != partLen) return false;
                if (partLen == 0) return true;
                return body.plist.SameClause(rule.body.plist, vars);
            }

            protected int bodyLen
            {
                get
                {
                    if (body == null) return 0;
                    if (IsBodyAlwaysTrue(body.plist)) return 0;
                    return body.plist.Arity;
                }
            }

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
    }

    public class SourceLanguage
    {
        public static SourceLanguage Prolog = new SourceLanguage("prolog");

        public static SourceLanguage Notation3 = new SourceLanguage("notation3")
        {
            NodeFormatter = new Notation3Formatter()
        };

        public static SourceLanguage Turtle = new SourceLanguage("turtle")
        {
            NodeFormatter = new TurtleFormatter()
        };

        public static SourceLanguage Text = new SourceLanguage("text")
                                                {
                                                    InnerLang = Prolog
                                                };
        static SourceLanguage()
        {

        }

        readonly public string Name;
        public SourceLanguage InnerLang;
        public INodeFormatter NodeFormatter;

        public override string ToString()
        {
            return  "slang:" + Name;
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
}

