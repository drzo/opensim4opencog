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
using StringWriter=System.IO.StringWriter;
using VDS.RDF.Writing.Formatting;
using PartList = LogicalParticleFilter1.SIProlog.PartListImpl;

namespace LogicalParticleFilter1
{

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

    public partial class SIProlog
    {

        static string UriOfMt(string plMt)
        {
            return RoboKindMtURI.TrimEnd('#', '/') + "/" + plMt + "#";
        }

        public static Triple MakeTriple(INode s, INode p, INode o)
        {
            return MakeTriple(s, p, o, true);
        }
        public static Triple MakeTriple(INode s, INode p, INode o, bool toplevel)
        {
            IGraph sGraph = o.Graph;
            if (!ReferenceEquals(sGraph, p.Graph))
            {
                p = p.CopyNode(sGraph, true);
            }
            if (!ReferenceEquals(sGraph, o.Graph))
            {
                o = o.CopyNode(sGraph, true);
            }
            if (!ReferenceEquals(sGraph, s.Graph))
            {
                s = s.CopyNode(sGraph, true);
            }
            Triple newTriple = new Triple(s, p, o);
            string warn0 = "";
            Action<string> warn = s0 =>
                                      {
                                          warn0 += s0 + " \n";
                                      };
            if (p.NodeType != NodeType.Uri)
            {
                warn("pred " + p + " is not URI");
            }
            if (s.NodeType == NodeType.GraphLiteral)
            {
                if (toplevel)
                {
                    IUriNode uri = p as IUriNode;
                    if (uri == null || !uri.Uri.ToString().Contains("log"))
                    {
                        warn("s = not logical (" + s + ")");
                    }
                }
            }
            else
            {
                if (s is ILiteralNode)
                {
                    warn("s = LiteralNode (" + s + ")");
                }
                if (toplevel && !GraphWithDef.CanBeSubjectNode(s))
                {
                    warn("s = !CanBeSubjectNode (" + s + ")");
                }
            }
            if (p is ILiteralNode)
            {
                warn("p = LiteralNode (" + p + ")");
            }
            if (warn0 != "")
            {
                warn("bad triple =" + newTriple);
                Warn(warn0);
            }
            return newTriple;
        }
        static public bool rdfGraphAssert(IGraph rdfGraph, Triple triple)
        {
            string bad = rdfGraphAssert(rdfGraph, triple, true, true);
            if (!string.IsNullOrEmpty(bad)) Warn(bad);
            return string.IsNullOrEmpty(bad);
        }

        static public string rdfGraphAssert(IGraph rdfGraph, Triple triple, bool checkWff, bool saveToKB)
        {
            if (checkWff)
            {
                string bad = CheckTriple(triple);
                if (!string.IsNullOrEmpty(bad))
                {
                    return bad;
                }
            }
            if (!saveToKB) return "";
            lock (rdfGraph)
            {
                if (rdfGraph.ContainsTriple(triple)) return null;
                rdfGraph.Assert(triple);
                return "";
            }
        }

        public static string CheckTriple(Triple triple)
        {
            foreach (INode node in triple.Nodes)
            {
                if (node.NodeType == NodeType.Variable)
                {
                    string bad = string.Format("Bad {0} found in triple {1}", node.NodeType, triple);
                    return bad;
                }
            }
            return "";
        }

        static readonly Graph forReaderTripleStoreGraph = new Graph();

        public static PNode BaseKB;
        public static PNode EverythingPSC;

        public static INode ToValueNode(INode obj)
        {
            var _name = (INode)obj;
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
            return _name;
        }

        public static INode GetValuedNode(string s)
        {
            try
            {
                var t = GetNode(s);
                if (t == null) return t;
                try
                {
                    return t.AsValuedNode();
                }
                catch (Exception e)
                {
                    return t;
                }
            }
            catch (Exception e)
            {
                Warn("GetValuedNode '{0}' caused: {1}", s, e);
                return null;
            }
        }

        public static INode GetNode(string s)
        {
            //var obj = TryParseObject(s, (INodeFactory) rdfDefinations);
            //if (obj != null) return obj;

            var forReaderTripleStore = SIProlog.forReaderTripleStoreGraph;
            lock (forReaderTripleStore)
            {
                forReaderTripleStore.Clear();
                EnsureReaderNamespaces(forReaderTripleStore);
                //forReaderTurtleParser.Load(forReaderTripleStore, "{ 1 1 " + s + " }");
                try
                {
                    if ((s.Contains("/") || s.Contains(":")) && !s.StartsWith("<")) s = "<" + s + ">";
                    StringParser.Parse(forReaderTripleStore,
                                       "<http://example.org/a1> <http://example.org/a1> " + s + " . ");
                    var t = forReaderTripleStore.Triples.First().Object;
                    return t;
                }
                catch (Exception e)
                {
                    Warn("GetValuedNode '{0}' caused: {1}", s, e);
                }
                return null;
            }
        }


        public partial class GraphWithDef
        {
            static private PredicateProperty AddDefs(Rule rule)
            {
                PredicateProperty headPP = GetPredicateProperty(rule.head); ;
                if (rule.body != null)
                {
                    foreach (var p in rule.body.plist.ArgList)
                    {
                        if (!(p is Term)) continue;
                        GetPredicateProperty((Term)p);
                    }
                }
                return headPP;
            }

            static public PredicateProperty GetPredicateProperty(Term term)
            {
                DocumentTerm(term, true);
                return GetPredicateProperty(term.fname, term.Arity);
            }
            static public PredicateProperty GetPredicateProperty(string predName0, int arity)
            {
                string predName = Unsymbolize(predName0);
                PredicateProperty def;
                bool newlyCreated;
                var rdef = rdfDefinations;
                lock (SharedGlobalPredDefs)
                {
                    def = GetPredDef(predName, arity, out newlyCreated);
                    if (newlyCreated)
                    {
                        SharedGlobalPredDefsDirty = true;
                        def.RdfDocumentPred(rdef);
                    }
                }
                lock (localPreds)
                {
                    if (!localPreds.Contains(def))
                    {
                        localPreds.Add(def);
                        SharedGlobalPredDefsDirty = true;
                    }
                }
                //if (newlyCreated)
                {
                    SharedGlobalPredDefsDirty = true;
                    foreach (Triple t in def.DefinitionalRDF)
                    {
                        rdfGraphAssert(rdef, t);
                    }
                }
                return def;
            }

            public static RDFArgSpec GetAdef(PredicateProperty def, int argNum1Based, bool okToMake)
            {
                RDFArgSpec adef;
                lock (def.argDefs)
                {
                    if (!def.argDefs.TryGetValue(argNum1Based, out adef))
                    {
                        if (!okToMake) return null;
                        adef = def.argDefs[argNum1Based] = new RDFArgSpec(argNum1Based);
                        adef.AddDomainType(def, argNum1Based + 1);
                    }
                }
                return adef;
            }

            static private string Unsymbolize(string name0)
            {
                if (name0 == "." || name0 == FUNCTOR_CONS) return "cons";
                if (name0 == "[]" || name0 == FUNCTOR_NIL) return "nil";
                string n = HttpUtility.UrlEncode(name0);
                return n;
            }
            static private string Symbolize(string name0)
            {
                if (name0 == "cons") return FUNCTOR_CONS;
                if (name0 == "nil") return FUNCTOR_NIL;
                string n = HttpUtility.UrlDecode(name0);
                return n;
            }

            public static PredicateProperty GetPredDef(string predName, int arity, out bool newlyCreated)
            {
                PredicateProperty def;
                string key = Unsymbolize(predName);// +"_" + arity;
                lock (SharedGlobalPredDefs)
                {
                    if (!SharedGlobalPredDefs.TryGetValue(key, out def))
                    {
                        newlyCreated = true;
                        string predClassName = "PredClass_" + key;
                        SharedGlobalPredDefs[key] =
                            def =
                            new PredicateProperty(arity) { name = predName, keyname = key, classname = predClassName };
                        return def;
                    }
                }
                newlyCreated = false;
                return def;
            }

            public static INode CExtracted(IGraph def, string p)
            {
                bool colm = p.Contains(":");
                int slen = p.Length;
                bool slengt1 = slen > 1;
                if (!colm && p.Length > 0)
                {
                    char c0 = p[0];
                    bool isNumberMaybe = (slengt1 && (c0 == '+' || c0 == '-')) || char.IsDigit(c0);
                    if (isNumberMaybe)
                    {
                        bool decm = slengt1 && p.Contains(".");
                        long intv;
                        if (!decm && long.TryParse(p, out intv))
                        {
                            return new LongNode(rdfDefinations, intv);
                        }
                        double dbl;
                        if (decm && double.TryParse(p, out dbl))
                        {
                            return new DoubleNode(rdfDefinations, dbl);
                        }
                    }
                }
                return C(def, p);
            }
            static public INode C(IGraph def, string p0)
            {
                return ResolveC<INode>(def.NamespaceMap, p0, (a, b, c) => C2(def, a, b, c));
            }
            static public INode C2(IGraph def, string prefix, string p, NodeType nt)
            {
                if (prefix == "_")
                {
                    return def.CreateBlankNode(p);
                }
                if (prefix == "?")
                {
                    return def.CreateVariableNode(p);
                }
                if (nt == NodeType.Literal)
                {
                    if (!string.IsNullOrEmpty(prefix)) return def.CreateLiteralNode(p, UriFactory.Create(prefix));
                    return def.CreateLiteralNode(p);
                }
                if (prefix == basePrefixDefault)
                {
                    return def.CreateUriNode(":" + p);
                }
                bool protop;
                string cc = CombinePrefix(prefix, p, out protop);
                if (protop) return def.CreateUriNode(UriFactory.Create(cc));
                return def.CreateUriNode(cc);
            }

            public static string CombinePrefix(string baseUri, string uriref, out bool protop)
            {
                protop = (baseUri + uriref).Contains(":/");
                return MakeQNameOrUri(uriref, baseUri);
            }

            static public T ResolveC<T>(INamespaceMapper def, string p0, Func<string, string, NodeType, T> CC)
            {
                var p = p0.Trim();
                if (p.StartsWith("_:"))
                {
                    return CC("_", p.Substring(2), NodeType.Blank);
                }
                if (p.StartsWith("?"))
                {
                    return CC("?", p.Substring(1), NodeType.Variable);
                }
                if (p.StartsWith(":"))
                {
                    return CC(basePrefixDefault, p.Substring(1), NodeType.Uri);
                }
                int colm = p.LastIndexOf(":");
                int colmp = p.LastIndexOf(":/");
                int uir = p.LastIndexOf("#");
                if (colm == -1 && uir == -1 && false)
                {
                    lock (GuessedNameSpace)
                    {
                        KeyValuePair<string, string> uriGuess;
                        if (GuessedNameSpace.TryGetValue(p, out uriGuess))
                        {
                            var pref = uriGuess.Value ?? (uriGuess.Key + ":");
                            return CC(pref, p, NodeType.Uri);
                        }
                    }
                    return CC(basePrefixDefault, p, NodeType.Uri);
                }
                int len = p.Length;
                string oprefix, ouri, localAname;
                if (uir < len && DevolveURI(def, p, out ouri, out oprefix,
                    out localAname, true, false) && localAname != null && oprefix != "+")
                {
                    lock (GuessedNameSpace)
                    {
                        KeyValuePair<string, string> uriGuess;
                        bool prev = GuessedNameSpace.TryGetValue(localAname, out uriGuess);
                        var pg = new KeyValuePair<string, string>(oprefix ?? uriGuess.Key,
                                                                    ouri ?? uriGuess.Value);
                        var prefix = pg.Value ?? (pg.Key + ":");
                        if (prefix != localAname)
                        {
                            GuessedNameSpace[localAname] = pg;
                            if (!string.IsNullOrEmpty(prefix))
                            {
                                return CC(prefix, localAname, NodeType.Uri);
                            }
                        }
                    }
                }

                if (colmp > 1)
                    return CC("+", p, NodeType.Uri);

                return CC(basePrefixDefault, p0, NodeType.Uri);
            }

            public static bool IsAbsoluteURI(string s)
            {
                int idx = s.IndexOf(":");
                return idx > 1 && s.Substring(idx).Contains("/") && Uri.IsWellFormedUriString(s, UriKind.Absolute);
            }

            /// <summary>
            /// Generic Helper Function which Resolves Uri References against a Base Uri
            /// </summary>
            /// <param name="uriref">Uri Reference to resolve</param>
            /// <param name="baseUri">Base Uri to resolve against</param>
            /// <returns>Resolved Uri as a String</returns>
            /// <exception cref="RdfParseException">RDF Parse Exception if the Uri cannot be resolved for a know reason</exception>
            /// <exception cref="UriFormatException">Uri Format Exception if one/both of the URIs is malformed</exception>
            public static String MakeQNameOrUri(String uriref, String baseUri)
            {
                if (baseUri == "+" || baseUri == "+:")
                {
                    return uriref;
                }
                Func<string, string> UE = HttpUtility.UrlEncode;
                UE = s => s;
                //uriref = HttpUtility.UrlDecode(uriref);
                if (string.IsNullOrEmpty(baseUri) || baseUri == basePrefixDefault)
                {
                    baseUri = "";
                    if (uriref.StartsWith("#") && baseUri.EndsWith("#"))
                    {
                        uriref = uriref.Substring(1);
                    }
                    if (uriref.StartsWith(":")) uriref = uriref.Substring(1);
                    return string.Format(":{0}", UE(uriref));
                }
                else
                {
                    if (uriref.StartsWith("#") && baseUri.EndsWith("#"))
                    {
                        uriref = uriref.Substring(1);
                    }
                    if (uriref.StartsWith(":")) uriref = uriref.Substring(1);
                    char lc = baseUri[baseUri.Length - 1];
                    bool isAbso = baseUri.Contains(":/") || char.IsSymbol(lc);
                    if (isAbso)
                    {
                        return string.Format("{0}{1}", baseUri, UE(uriref));
                    }
                    return string.Format("{0}:{1}", baseUri, UE(uriref));
                }
            }
            public static INode InstanceOf
            {
                get
                {
                    var ret = C(rdfDefinations, "rdf:type");
                    return ret;
                }
            }
            public static INode SameAs
            {
                get
                {
                    var ret = C(rdfDefinations, "owl:sameAs");
                    return ret;
                }
            }
            public static INode IsDefinedBy
            {
                get
                {
                    var ret = C(rdfDefinations, "rdfs:isDefinedBy");
                    return ret;
                }
            }
            public static INode PrologPredicate
            {
                get { return C(rdfDefinations, "rdf:Property"); }
            }
            public static INode PrologPredicateClass
            {
                get { return C(rdfDefinations, "rdf:Class"); }
            }
            static public INode PartToRdf(Part part, RdfRules triples)
            {
                if (part is Atom)
                {
                    Atom atom = ((Atom)part);
                    var rdf = atom.AsRDFNode();
                    if (rdf != null)
                    {
                        return rdf;
                    }
                    Warn("Atom.AsValuedNode returned NULL" + part);
                }
                if (part is Variable)
                {
                    var definations = triples.def;
                    return definations.CreateVariableNode(((Variable)part).vname);
                }
                Part car, cdr;
                if (GetCons(part, out car, out cdr))
                {
                    var definations = triples.def;
                    var rdf = definations.CreateVariableNode("CONS" + CONSP);
                    triples.AddRequirement(rdf, "rdf:first", car);
                    triples.AddRequirement(rdf, "rdf:rest", cdr);
                    return rdf;
                }
                if (part is Term)
                {
                    var def = triples.def;
                    RdfRules antRules = new RdfRules(def);
                    var subj = CreateAntecedantNode((Term)part, antRules);
                    if (subj == null)
                    {
                        foreach (var ms in antRules.Subjects)
                        {
                            subj = subj ?? ms;
                        }
                    }
                    if (subj != null)
                    {
                        triples.IncludeRules(antRules);
                        return subj;
                    }
                    ICollection<Triple> trips = antRules.Requirements;
                    if (trips.Count == 0)
                    {
                        trips = antRules.ToTriples;
                        if (trips.Count == 0)
                        {
                            Warn("No triples made from: " + part);
                        }
                        var gl1 = ToBracket(def, trips);
                        return gl1;
                    }
                    antRules.Requirements.Clear();
                    triples.IncludeRules(antRules);
                    var gl = ToBracket(def, trips);
                    return gl;
                }
                throw ErrorBadOp("ToRDF on " + part);
            }

            public static readonly Dictionary<string, KeyValuePair<string, string>> GuessedNameSpace = new Dictionary<string, KeyValuePair<string, string>>();
            static public Part RdfToPart(INode node, RdfRules triples)
            {
                if (node is IVariableNode)
                {
                    var vnode = (IVariableNode)node;
                    return new Variable(vnode.VariableName);
                }
                if (node is IGraphLiteralNode)
                {
                    var vnode = (IGraphLiteralNode)node;
                    throw ErrorBadOp("RDFToPart: on " + vnode);
                }
                if (node is IBlankNode)
                {
                    var vnode = (IBlankNode)node;
                    var atom = Atom.MakeNodeAtom(vnode);
                    return atom;
                    node = triples.def.CreateUriNode(UriFactory.Create("_:" + vnode.InternalID));
                }

                if (node is IUriNode)
                {
                    var vnode = (IUriNode)node;
                    var atom = Atom.MakeNodeAtom(vnode);
                    return atom;
                }

                // all the below are now Literal Nodes of some type  (we divide into  "strings", numbers and "strings with"^"meaning" and 
                ILiteralNode litnode = node as ILiteralNode;
                if (litnode == null)
                {
                    throw ErrorBadOp("Cant find the nodetype on  " + node);
                }
                return Atom.MakeNodeAtomFixme(litnode);
            }

            private static HashSet<string> MissingNameSpaces = new HashSet<string>();

            static public void AddNamespace(string prefix, string uri)
            {
                if (MissingNameSpaces.Add(uri))
                {
                    Warn("New namespace that was missing: " + uri);
                }
            }
            static public bool DevolveURI(INamespaceMapper mapper, string s, out string uri, out string prefix, out string atom, bool discoverNamepaces, bool useSpecialPrefix)
            {
                bool ret = DevolveURI(mapper, s, out uri, out prefix, out atom, discoverNamepaces);
                if (useSpecialPrefix && prefix == null && atom == "" && !string.IsNullOrEmpty(uri))
                {
                    atom = uri;
                    prefix = "+";
                }
                return ret;
            }
            static public bool DevolveURI(INamespaceMapper mapper, string s, out string uri, out string prefix, out string atom, bool guessNamespaces)
            {
                if (IsAbsoluteURI(s))
                {
                    string tries = HttpUtility.UrlDecode(s);
                    if (tries != s)
                    {
                        s = tries;
                    }
                }
                bool ret = DevolveURIWithNamespace(mapper, s, out uri, out prefix, out atom);

                if (ret) return true;
                if (!guessNamespaces) return false;
                ret = DevolveURIGuessOnly(s, out uri, out prefix, out atom);
                if (!ret) return false;
                return true;
            }

            static public bool DevolveURIWithNamespace(INamespaceMapper mapper, string s, out string uri, out string prefix, out string atom)
            {
                atom = null;
                uri = null;
                prefix = null;
                foreach (var pfx in LockInfo.WithLock<IEnumerable<string>>(mapper, mapper.Prefixes.ToArray))
                {
                    prefix = pfx;
                    var uril = LockInfo.WithLock(mapper, () => mapper.GetNamespaceUri(pfx));
                    uri = uril.ToString();
                    int len = uri.Length;
                    if (len > 0 && s.StartsWith(uri))
                    {
                        if (string.IsNullOrEmpty(prefix))
                        {
                            prefix = LockInfo.WithLock(mapper, () => mapper.GetPrefix(uril));
                        }
                        atom = s.Substring(len);
                        return true;
                    }
                    string prefixc = prefix + ":";
                    if (s.StartsWith(prefixc))
                    {
                        atom = s.Substring(prefixc.Length);
                        return true;
                    }
                }
                atom = null;
                uri = null;
                prefix = null;

                return false;
            }

            static public bool DevolveURIGuessOnly(string s, out string uri, out string prefix, out string atom)
            {
                atom = null;
                uri = null;
                prefix = null;
                int hash = s.LastIndexOf("#");
                if (hash > 0)
                {
                    uri = s.Substring(0, hash + 1);
                    prefix = null;
                    atom = s.Substring(hash + 1);
                    return true;
                }
                int col2 = s.IndexOf(":/");
                int slash = s.LastIndexOf("/");
                if (col2 > 0)
                {
                    if (slash > col2 + 1)
                    {
                        uri = s.Substring(0, slash + 1);
                        prefix = null;
                        atom = s.Substring(slash + 1);
                        return true;
                    }
                }
                int space = s.LastIndexOf(" ");
                int col = s.IndexOf(":");
                int coll = s.LastIndexOf(":");
                if (slash == -1 && col >= 0)
                {
                    if (space > 0) return false;
                    if (coll != col) return false;
                    uri = null;
                    prefix = s.Substring(0, col);
                    atom = s.Substring(col + 1);
                    return true;
                }
                return false;
            }

            static public string PlReadble(INode subject0, RdfRules rules)
            {
                var subject1 = ToValueNode(subject0);
                Part part1 = RdfToPart(subject1, rules);
                string readable = part1.ToSource(SourceLanguage.Prolog);
                if (SIProlog.RdfDeveloperSanityChecks < 2) return readable;
                INode subject2 = PartToRdf(part1, rules);
                if (subject2 != subject1)
                {
                    if (!subject1.Equals(subject2))
                    {
                        Warn("PlReadble not round tripping! INodes " + NodeDesc(subject1) + "->" + NodeDesc(subject2));
                    }
                }
                Tokeniser oldTokenizer = new Tokeniser(readable);
                Part part2 = ParsePart(oldTokenizer);
                if (part2 == null || !part2.Equals(part1))
                {
                    if (part2 != null)
                    {
                        part2.Equals(part1);
                    }
                    readable = part1.ToSource(SourceLanguage.Prolog);
                    string readable2 = null;
                    if (part2 != null) readable2 = part2.ToSource(SourceLanguage.Prolog);
                    tl_spy_prolog_reader = true;
                    Tokeniser newTokeniser = new Tokeniser(readable);
                    part2 = ParsePart(newTokeniser);
                    tl_spy_prolog_reader = false;
                    Warn("PlReadble not round tripping! re-readablity Node=" + NodeDesc(subject1) + " part1=" +
                         PartDesc(part1) + ".ToPLReadable()->" + readable);
                }
                if ((part2 is Variable && subject1.NodeType != NodeType.Variable))
                {
                    Warn("PlReadble not round tripping! Making a prolog variable? " + NodeDesc(subject1) + " part1= " + PartDesc(part1));
                }
                return readable;
            }

            static string NodeDesc(INode subject)
            {
                return subject + "{type=" + subject.NodeType + " impl=" + subject.GetType() + "}";
            }

            static string PartDesc(Part subject)
            {
                return subject + "{type=" + subject.type + " impl=" + subject.GetType() + "}";
            }
        }

        public static void DocumentTerm(Term term, bool varnamesOnly)
        {
            bool newlyCreated;
            PredicateProperty pp = GraphWithDef.GetPredDef(term.fname, term.Arity, out newlyCreated);
            int argNum = 0;
            foreach (Part part in term.ArgList)
            {
                argNum++;
                if (varnamesOnly) if (!(part is Variable)) continue;
                var argDef = GraphWithDef.GetAdef(pp, argNum, true);
                argDef.AddRangeTypeName(part.Text);
            }
        }

    }
}
    


