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
            return RoboKindURI.TrimEnd('#', '/') + "/" + plMt + "#";
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
        static TurtleParser forReaderTurtleParser = new TurtleParser();
        [ThreadStatic]
        private string tl_ServerRoot;
        [ThreadStatic]
        private string tl_mt;
        [ThreadStatic]
        private string tl_rule_mt;
        private string curKB
        {
            get
            {
                return tl_mt;
            }
            set
            {
                tl_mt = value;
            }
        }
        [ThreadStatic]
        private TextWriter tl_writer;

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
                return GetPredicateProperty(term.name, term.Arity);
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
                if (!colm)
                {
                    bool decm = p.Contains(".");
                    long intv;
                    if (!decm && long.TryParse(p, out intv))
                    {
                        return new LongNode(def, intv);
                    }
                    double dbl;
                    if (decm && double.TryParse(p, out dbl))
                    {
                        return new DoubleNode(def, dbl);
                    }
                }
                return C(def, p);
            }

            static public INode C(IGraph def, string p0)
            {
                var p = p0.Trim();
                if (p.StartsWith("_:"))
                {
                    return def.CreateBlankNode(p.Substring(2));
                }
                if (p.StartsWith("?"))
                {
                    return def.CreateVariableNode(p.Substring(1));
                }
                int colm = p.IndexOf(":");
                bool uir = p.Contains("#") || colm == 0 || colm > 1;
                bool triedURINode = false;


                try
                {
                    Uri newUri;
                    if (colm == -1)
                    {
                        lock (GuessedNameSpace)
                        {
                            KeyValuePair<string, string> uriGuess;
                            if (GuessedNameSpace.TryGetValue(p, out uriGuess))
                            {
                                var pref = uriGuess.Value ?? (uriGuess.Key + ":");
                                return def.CreateUriNode(pref + "" + p);
                            }
                        }
                    }
                    else if (IsAbsoluteURI(p))
                    {
                        newUri = new Uri(p);
                        return def.CreateUriNode(newUri);
                    }
                    string qname = Tools.ResolveQName(p0, def.NamespaceMap, def.BaseUri);
                    return C(def, qname);
                }
                catch (Exception)
                {
                    triedURINode = true;
                }
                int badchars = p.IndexOfAny("? ".ToCharArray());
                if (badchars >= 0)
                {
                    return CreateLiteralNode(def, p);
                }
                if (!triedURINode)
                {
                    var node = CUrlNode(def, p, colm == -1, true);
                    if (node != null) return node;
                }
                badchars = p.IndexOfAny("? ,$.&!@#$%^&*()+".ToCharArray());
                if (badchars >= 0)
                {
                    string p2 = HttpUtility.UrlEncode(HttpUtility.UrlDecode(p));
                    if (p2 != p)
                    {
                        var node = CUrlNode(def, p2, colm == -1, false);
                        if (node != null) return node;
                    }
                }
                return CreateLiteralNode(def, p);
            }

            public static bool IsAbsoluteURI(string s)
            {
                int idx = s.IndexOf(":");
                return idx > 1 && s.Substring(idx).Contains("/") && Uri.IsWellFormedUriString(s, UriKind.Absolute);
            }

            private static INode CreateLiteralNode(INodeFactory def, string p)
            {
                var vnode = GetValuedNode(p);
                if (vnode != null) return vnode;
                return def.CreateLiteralNode(p);
            }

            static public IUriNode CUrlNode(IGraph definations, string p, bool checkSuffixes, bool allowRelative)
            {
                lock (GuessedNameSpace)
                {
                    KeyValuePair<string, string> uriGuess;
                    Uri newUri;
                    if (checkSuffixes && GuessedNameSpace.TryGetValue(p, out uriGuess))
                    {
                        var pref = uriGuess.Value ?? (uriGuess.Key + ":");
                        try
                        {
                            return definations.CreateUriNode(pref + "" + p);
                        }
                        catch (Exception)
                        {
                        }
                    }
                    if (IsAbsoluteURI(p))
                    {
                        try
                        {
                            newUri = new Uri(p);
                            return definations.CreateUriNode(newUri);
                        }
                        catch (Exception)
                        {
                        }
                    }
                    if (allowRelative && Uri.IsWellFormedUriString(p, UriKind.Relative))
                    {
                        try
                        {
                            newUri = new Uri(definations.BaseUri + p, UriKind.Absolute);
                            //var newUri2 = new Uri(definations.BaseUri, newUri);
                            var r = definations.CreateUriNode(newUri);
                            return r;
                        }
                        catch (Exception)
                        {
                        }
                    }
                }
                return null;
            }

            static public bool TryCreateCleanUri(Uri baseUri, string relstr, out Uri result)
            {
                try
                {
                    if (!Uri.TryCreate(baseUri, relstr, out result))
                    {
                        return false;
                    }
                }
                catch (UriFormatException ex)
                {
                    throw new InvalidOperationException(
                        String.Format("Can create URI for base={0}, rel={1}", baseUri.ToString(), relstr), ex);
                }
                return true;
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
                var definations = triples.def;
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
                    return definations.CreateVariableNode(part.name);
                }
                Part car, cdr;
                if (GetCons(part, out car, out cdr))
                {
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

            static readonly Dictionary<string, KeyValuePair<string, string>> GuessedNameSpace = new Dictionary<string, KeyValuePair<string, string>>();
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
                    var atom = MakeUri(vnode.Uri.AbsoluteUri, triples, vnode);
                    return atom;
                }

                // all the below are now Literal Nodes of some type  (we divide into  "strings", numbers and "strings with"^"meaning" and 
                ILiteralNode litnode = node as ILiteralNode;
                if (litnode == null)
                {
                    throw ErrorBadOp("Cant find the nodetype on  " + node);
                }
                return Atom.MakeNodeAtom(litnode);
            }

            public static Atom MakeUri(string s, RdfRules triples, IUriNode node)
            {
                if (node != null && s == null)
                {
                    s = node.Uri.OriginalString;
                    var relitive = node.Uri.IsAbsoluteUri;
                    var uri2 = node.Uri.AbsoluteUri;
                    var quri = node.Uri.Query;
                }

                var tl_language = SourceLanguage.Notation3;
                do
                {
                    if (node != null)
                    {
                        var s2 = node.ToString();
                        var uri2 = node.Uri.AbsoluteUri;
                        if (s2 == uri2 && s == s2)
                        {
                            return Atom.MakeNodeAtom(node);
                        }
                    }

                    var definations = triples.def;
                    string prefix, uri, atom;
                    if (DevolveURI(definations.NamespaceMap, s, out uri, out prefix, out atom) || atom != null)
                    {

                        Atom atomMake = Atom.MakeNodeAtom(node);
                        lock (GuessedNameSpace)
                        {
                            KeyValuePair<string, string> gns;
                            bool fnd = GuessedNameSpace.TryGetValue(atom, out gns);
                            GuessedNameSpace[atom] = new KeyValuePair<string, string>(prefix ?? gns.Key,
                                                                                      uri ?? gns.Value);
                        }
                        return atomMake;
                    }
                } while (false);
                do
                {
                    string quoting = MustGuessQuotes;
                    int hash = s.IndexOf("#");
                    s = s.Substring(1 + hash);
                    if (hash == -1 && !s.Contains(":/"))
                    {
                        hash = s.IndexOf(":");
                        if (hash > 0)
                        {
                            s = s.Substring(1 + hash);
                            quoting = SYNTAX_AtomQuotes;
                        }
                    }
                    if (hash == -1)
                    {
                        quoting = SYNTAX_AtomQuotes;
                    }
                    return Atom.MakeNodeAtom(Atom.MakeNode(s, quoting));
                } while (false);
            }

            private static HashSet<string> MissingNameSpaces = new HashSet<string>();

            static private void DiscoverNameSpace(string uri)
            {
                if (MissingNameSpaces.Add(uri))
                {
                    Warn("New namespace that was missing: " + uri);
                }
            }
            static public bool DevolveURI(INamespaceMapper mapper, string s, out string uri, out string prefix, out string atom)
            {
                if (IsAbsoluteURI(s))
                {
                    string tries = HttpUtility.UrlDecode(s);
                    if (tries != s)
                    {
                        s = tries;
                    }
                }
                bool ret = DevolveURI0(mapper, s, out uri, out prefix, out atom);
                if (atom == "")
                {
                    string idea = prefix + ":" + atom;
                    if (idea.Length == 1)
                    {
                        atom = uri;
                    }
                    else
                    {
                        if (uri.Length > 0)
                        {
                            atom = uri;
                        }
                        else
                        {
                            atom = idea;
                        }
                    }
                }
                if (atom.Length == 0 || (!char.IsLetterOrDigit(atom[0]) && !atom.StartsWith("#C_") && !atom.StartsWith("$")))
                {
                    //    Warn("strange atom='{0}' prefix='{1}' uri='{2}' ", atom, prefix, uri);
                    //string satom = HttpUtility.UrlEncode(atom);
                }
                if (atom == uri) return ret;
                if (prefix == null && uri != null)
                {
                    atom = s;
                    uri = s;
                    return ret;

                    if (!uri.Contains("/ns/") && !uri.Contains("robokind") && atom != uri)
                    {
                        DiscoverNameSpace(uri);
                    }
                    else
                    {
                        atom = uri;
                    }
                }
                return ret;
            }

            static public bool DevolveURI0(INamespaceMapper mapper, string s, out string uri, out string prefix, out string atom)
            {
                atom = null;
                uri = null;
                prefix = null;
                foreach (var pfx in mapper.Prefixes)
                {
                    prefix = pfx;
                    var uril = mapper.GetNamespaceUri(prefix);
                    uri = uril.ToString();
                    if (uri.Length > 0 && s.StartsWith(uri))
                    {
                        if (string.IsNullOrEmpty(prefix))
                        {
                            prefix = mapper.GetPrefix(uril);
                        }
                        atom = s.Substring(uri.Length);
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
                    if (slash != col2 + 1)
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
                INode subject2 = PartToRdf(part1, rules);
                if (subject2 != subject1)
                {
                    Warn("PlReadble not round tripping! INodes " + NodeDesc(subject1) + "->" + NodeDesc(subject2));
                }
                string readable = part1.ToSource(SourceLanguage.Prolog);
                if (SIProlog.RdfDeveloperSanityChecks < 2) return readable;
                Tokeniser oldTokenizer = new Tokeniser(readable);
                Part part2 = ParsePart(oldTokenizer);
                if (part2 == null || !part2.Equals(part1))
                {
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
            PredicateProperty pp = GraphWithDef.GetPredDef(term.name, term.Arity, out newlyCreated);
            int argNum = 0;
            foreach (Part part in term.ArgList)
            {
                argNum++;
                if (varnamesOnly) if (!(part is Variable)) continue;
                var argDef = GraphWithDef.GetAdef(pp, argNum, true);
                argDef.AddRangeTypeName(part.name);
            }
        }

    }
}
    


