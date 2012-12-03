using System;
using System.Collections;
using System.Collections.Generic;
using System.Globalization;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Web;
using LogicalParticleFilter1;
using MushDLR223.Utilities;
using VDS.RDF;
using VDS.RDF.Nodes;
using VDS.RDF.Parsing;
using VDS.RDF.Query;
using VDS.RDF.Query.Expressions;
using VDS.RDF.Writing;
using ListOfBindings = System.Collections.Generic.List<System.Collections.Generic.Dictionary<string, LogicalParticleFilter1.SIProlog.Part>>;
using StringWriter=System.IO.StringWriter;
using VDS.RDF.Writing.Formatting;

namespace LogicalParticleFilter1
{
    public static class RDFExtensions
    {
        //static public Dictionary<SIProlog.GraphWithDef.Rule, INode> rule2Node = new Dictionary<SIProlog.GraphWithDef.Rule, INode>();
        static public Dictionary<SIProlog.Rule, INode> rule2Node = new Dictionary<SIProlog.Rule, INode>();

        public static INode instanceTriple(this SIProlog.Rule rule)
        {
            if (rule == null) return null;
            INode node = rule.rdfRuleCache.RuleNode;
            if (node != null) return node;
            lock (rule2Node) if (rule2Node.TryGetValue(rule, out node))
                {
                    rule.rdfRuleCache.RuleNode = node;
                    return node;
                }
            return null;
        }
        public static int WordCount(this String str)
        {
            return str.Split(new char[] { ' ', '.', '?' },
                             StringSplitOptions.RemoveEmptyEntries).Length;
        }
        public static SIProlog.PartList VarsOf(this SIProlog.Part str)
        {
            // termVarNames
            return SIProlog.varNames(new SIProlog.PartList(str));
        }

        public static SIProlog.RdfRules RdfRuleValue(this SIProlog.Rule prologRule)
        {
            // termVarNames
            SIProlog prolog = SIProlog.CurrentProlog;
            lock (prolog)
            {
                var rr = prologRule.rdfRuleCache;
                if (rr == null)
                {
                    string prologRuleoptHomeMt = prologRule.optHomeMt;
                    SIProlog.PGraph gg = SIProlog.GlobalKBGraph;
                    var kb = prolog.MakeRepositoryKB(prologRuleoptHomeMt);
                    SIProlog.GraphWithDef.FromRule(prologRule, kb.rdfGraph);
                    rr = prologRule.rdfRuleCache;
                }
                return rr;
            }
        }
    }
    public partial class SIProlog
    {        
        static readonly internal IGraph rdfDefinations = new Graph();
        const string rdfDefMT = "rdfGlobalDefsMt";
        private GraphWithDef rdfDefSync;
        public CIDictionary<string, GraphWithDef> GraphForMT = new CIDictionary<string, GraphWithDef>(KeyCase.Default);
        static public string RoboKindURI = "http://cogserver:8123/onto/robokind#";
        public static string RoboKindPrefix = "robokind";
        public static string RoboKindPrefixPrepend = RoboKindPrefix + ":";
        private static CIDictionary<string, PredicateProperty> SharedGlobalPredDefs = new CIDictionary<string, PredicateProperty>(KeyCase.Default);
        private static CIDictionary<string, RDFArgSpec> SharedGlobalArgTypeDefs = new CIDictionary<string, RDFArgSpec>(KeyCase.Default);

        private void defineRDFExtensions()
        {
            var node = FindOrCreateKB(rdfDefMT);
            rdfDefinations.BaseUri = new Uri(RoboKindURI);
            rdfDefSync =
                GraphForMT[rdfDefMT] =
                rdfDefSync ?? new GraphWithDef(rdfDefMT, this, rdfDefinations, rdfDefinations) { PrologKB = node };
            EnsureReaderNamespaces();
            loadKB("aiml/shared_ke/argdefs.txt", rdfDefMT);
            mtest();
        }

        private static void EnsureReaderNamespaces()
        {
            lock (forReaderTripleStore)
            {
                if (!forReaderTripleStore.NamespaceMap.HasNamespace("robokind"))
                    forReaderTripleStore.NamespaceMap.Import(rdfDefinations.NamespaceMap);
            }
        }

        public GraphWithDef MakeRepositoryKB(string mt)
        {
            lock (GraphForMT)
            {
                GraphWithDef graph;
                if (!GraphForMT.TryGetValue(mt, out graph))
                {
                    graph = GraphForMT[mt] = new GraphWithDef(mt, this, new Graph(), rdfDefinations);
                   // var node = graph.PrologKB;
                }
                return graph;
            }
        }

        public void WriteEnumeration<T>(TextWriter writer, IEnumerable<T> triple, Func<T, object> toString)
        {
            writer.WriteLine("<pre>");
            foreach (T t in triple)
            {
                object os = toString(t);
                if (os == null) continue;
                var ts = os.ToString();
                writer.WriteLine(ts);
            }
            writer.WriteLine("</pre>");
        }

        public GraphWithDef FindRepositoryKB(string mt)
        {
            lock (GraphForMT)
            {
                GraphWithDef graph;
                if (!GraphForMT.TryGetValue(mt, out graph))
                {
                    return null;
                }
                return graph;
            }
        }


        #region rdfEndpoint
        public void rdfRemoteEndpointToKB(string endpointURI, string graphKBName, string query, string assertTemplate)
        {
            //Define a remote endpoint
            //Use the DBPedia SPARQL endpoint with the default Graph set to DBPedia
            SparqlRemoteEndpoint endpoint = new SparqlRemoteEndpoint(new Uri(endpointURI));

            var gwd = MakeRepositoryKB(graphKBName);

            string miniMt = "";

            //Use the extension method ExecuteQuery() to make the query against the Graph
            try
            {
                //Object results = g.ExecuteQuery(query);
                //Make a SELECT query against the Endpoint
                SparqlResultSet results = endpoint.QueryWithResultSet(query);
                miniMt = GetMiniMt(results, assertTemplate, gwd, show, null);
            }
            catch (RdfQueryException queryEx)
            {
                //There was an error executing the query so handle it here
                Warn("While endpointURI={0}\n\n{1}", endpointURI, queryEx);
            }
            insertKB(miniMt, graphKBName);
        }

        public void rdfImportToKB(IGraph g, string graphKBName, string query, string assertTemplate)
        {
            string miniMt = "";
            var repo = MakeRepositoryKB(graphKBName);
            //Use the extension method ExecuteQuery() to make the query against the Graph
            try
            {
                Object results = g.ExecuteQuery(query);
                miniMt = GetMiniMt(results, assertTemplate, repo, show, null);
            }
            catch (RdfQueryException queryEx)
            {
                //There was an error executing the query so handle it here
                Warn(queryEx);
            }
            insertKB(miniMt, graphKBName);
        }

        private string GetMiniMt(object results, string assertTemplate, GraphWithDef repo, bool show, List<Rule> rules)
        {
            assertTemplate = assertTemplate ?? "triple($?s$,$?p$,$?o$).\n";
            bool MakeRules = rules != null && assertTemplate.Trim().EndsWith(".");
            var outMap = new Dictionary<string, string>();
            outMap["s"] = "unknownSubject";
            outMap["p"] = "unknownPredicate";
            outMap["o"] = "unknownObject";
            outMap["mt"] = repo.PlReadble(GraphWithDef.CUrlNode(repo.definations, repo.rdfGraph.BaseUri.AbsoluteUri, false, false));

            string miniMt = "";
            if (results is SparqlResultSet)
            {
                //SELECT/ASK queries give a SparqlResultSet
                SparqlResultSet rset = (SparqlResultSet)results;
                if (show)
                {
                    ConsoleWriteLine("SparqlResultSet.Count = {0}", rset.Count);
                    ConsoleWriteLine("SparqlResultSet:{0}", rset.ToString());
                }
                foreach (SparqlResult r in rset)
                {
                    //Do whatever you want with each Result
                    if (show)
                    {
                        ConsoleWriteLine("SparqlResult.Count = {0}", r.Count);
                        ConsoleWriteLine("SparqlResult:{0}", r.ToString());
                    }

                    var assertIt = assertTemplate;
                    //Do whatever you want with each Triple
                    foreach (string vname in r.Variables)
                    {
                        INode value = r[vname];
                        string strVal = repo.PlReadble(value);
                        assertIt = assertIt.Replace("$?" + vname + "$", strVal);
                        if (show) ConsoleWriteLine("BIND: {0} = {1}", vname, strVal);
                    }
                    if (assertIt.Contains("$?s$"))
                    {
                        foreach (KeyValuePair<string, string> map in outMap)
                        {
                            assertIt = assertIt.Replace("$?" + map.Key + "$", map.Value);
                        }
                    }
                    if (MakeRules)
                    {
                        Rule rule = ParseRule(new Tokeniser(assertIt));
                        if (show) ConsoleWriteLine("RULE_IG: {0}", rule);
                        rules.Add(rule);
                    }
                    else
                    {
                        if (show) ConsoleWriteLine("TRIPLE_IG: {0}", assertIt);
                        miniMt += assertIt;
                    }
                }
            }
            else if (results is IGraph)
            {
                //CONSTRUCT/DESCRIBE queries give a IGraph
                IGraph resGraph = (IGraph)results;
                var rset = resGraph.Triples;
                outMap["mt"] = repo.PlReadble(GraphWithDef.CUrlNode(repo.definations, resGraph.BaseUri.AbsoluteUri, false, false));
                if (show)
                {
                    ConsoleWriteLine("IGraphResultSet.Count = {0}", rset.Count);
                    ConsoleWriteLine("IGraphResultSet:{0}", rset.ToString());
                }
                foreach (Triple t in rset)
                {
                    var assertIt = assertTemplate;
                    //Do whatever you want with each Triple
                    outMap["s"] = repo.PlReadble(t.Subject);
                    outMap["p"] = repo.PlReadble(t.Predicate);
                    outMap["o"] = repo.PlReadble(t.Object);
                    foreach (KeyValuePair<string, string> map in outMap)
                    {
                        assertIt = assertIt.Replace("$?" + map.Key + "$", map.Value);
                    }
                    if (MakeRules)
                    {
                        Rule rule = ParseRule(new Tokeniser(assertIt));
                        if (show) ConsoleWriteLine("RULE_IG: {0}", rule);
                        rules.Add(rule);
                    }
                    else
                    {
                        if (show) ConsoleWriteLine("TRIPLE_IG: {0}", assertIt);
                        miniMt += assertIt;
                    }
                }
            }
            else
            {
                //If you don't get a SparqlResutlSet or IGraph something went wrong 
                //but didn't throw an exception so you should handle it here
                Warn("ERROR: Cant understand " + results.GetType() + " " + results);
            }
            return miniMt;
        }

        public void pushRulesToGraph(string mt, GraphWithDef rdfGraphWithDefs, bool includeInherited)
        {
            // Possibly called by the Sparql endpoint before servicing a query
            // Is there anything we want to update rdfGraph with ?
            var bingingsList = new ListOfBindings();
            askQuery("triple(S,P,O)", mt, false, bingingsList, null);
            bool useTripeQuery = true;
            if (bingingsList == null || bingingsList.Count <= 0)
            {
                useTripeQuery = false;
                var rules = findVisibleKBRules(mt, new ArrayList(), includeInherited);
                foreach (Rule rule in rules)
                {
                    try
                    {
                        rdfGraphWithDefs.AddRule(rule);
                    }
                    catch (Exception e)
                    {
                        Warn(e);
                    }
                }
            }

            var rdfGraph = rdfGraphWithDefs.rdfGraph;

            if (!useTripeQuery) return;
            var newTriples = new RdfRules(rdfGraphWithDefs.definations);
            EnsureGraphPrefixes(rdfGraph);
            foreach (Dictionary<string, Part> bindings in bingingsList)
            {
                //foreach (string k in bindings.Keys)
                //{
                rdfGraphAssert(rdfGraph,
                               MakeTriple(GraphWithDef.PartToRdf(bindings["S"], newTriples),
                                          GraphWithDef.PartToRdf(bindings["P"], newTriples),
                                          GraphWithDef.PartToRdf(bindings["O"], newTriples)));
                //string rdfLine = String.Format(@"<{0}> <{1}> <{2}> .", bindings["S"].ToString(), bindings["P"].ToString(), bindings["O"].ToString());
                //StringParser.Parse(rdfGraph, rdfLine);
                // }
            }
            newTriples.AssertTriples(rdfGraphWithDefs.definations, true);
        }


        public class RdfRules
        {
            ///<summary>
            ///</summary>
            public string _aInfo
            {
                get
                {
                    try
                    {
                        return ToString();
                    }
                    catch (Exception)
                    {
                        return ToString();
                    }
                }
            }

            private bool _requirementsMet;
            public IGraph ContainingGraph;
            public IGraph def
            {
                get
                {
                    return ContainingGraph ?? _graph;
                }
            }
            public INode RuleNode;
            public List<INode> Subjects = new List<INode>();
            public List<Triple> Requirements = new List<Triple>();
            public List<Triple> Producing = new List<Triple>();
            public List<Triple> Consequences = new List<Triple>();

            IGraph _graph;
            public override string ToString()
            {
                INamespaceMapper graphNamespace = _graph.NamespaceMap;
                StringWriter sw = new StringWriter();
                Graph ig = new Graph();
                ig.NamespaceMap.Import(graphNamespace);
                AssertTriples(ig, false);
                sw.WriteLine("# subjs= {0} metreq={1}", Subjects.Count, RequirementsMet);
                DumpTriplesPlain(ig.Triples, sw, "{0}", ig);
                //WriteGraph(sw, ig, "rdfs rules", true, false);
                return sw.ToString();
            }

            static void WriteTripleBlock(Graph ig, ICollection ts, INamespaceMapper graphNamespace, StringWriter sw)
            {
                if (ts == null || ts.Count == 0)
                {
                    sw.WriteLine("{ }");
                    return;
                }
                sw.WriteLine("{");
                var formatter = new SparqlFormatter(graphNamespace);
                foreach (Triple trip0 in ts)
                {
                    DumpOneTriple(ig, trip0, formatter, sw, "{0}");
                }
                sw.WriteLine("}");
            }

            public RdfRules(IGraph graph)
            {
                _graph = graph;
                //Producing = Requirements;
            }

            public void AddRequirement(Triple triple)
            {
                if (Contains(triple))
                {
                    return;
                }
               // CheckTriple(triple); ;
                Requirements.Add(triple);
            }
            public void AddProducing(Triple triple)
            {
                if (Contains(triple))
                {
                    return;
                }
                if (triple.Subject.NodeType == NodeType.Variable)
                {
                    AddRequirement(triple);
                    return;
                }
                if (triple.Object.NodeType == NodeType.Variable)
                {
                    AddRequirement(triple);
                    return;
                }
                CheckTriple(triple);               
                Producing.Add(triple);
            }
            public void AddConsequent(Triple triple)
            {
                if (Contains(triple))
                {
                    return;
                }
                Consequences.Add(triple);
            }

            private bool Contains(Triple triple)
            {
                if (Consequences.Contains(triple)) return true;
                if (Producing.Contains(triple)) return true;
                if (Requirements.Contains(triple)) return true;
                return false;
            }

            public bool RequirementsMet
            {
                get
                {
                    if (Requirements.Count == 0) return true;
                    return _requirementsMet;
                }
                set
                {
                    _requirementsMet = value;
                }
            }
            public IEnumerable<Triple> ToTriples
            {
                get
                {
                    bool skipProducing = false;
                    ICollection<Triple> temp = new HashSet<Triple>();
                    if (Requirements.Count > 0)
                    {
                        if (Consequences.Count > 0)
                        {
                            temp.Add(GraphWithDef.CreateImplication(_graph, Requirements, Consequences));
                        }
                        else
                        {
                            temp.Add(GraphWithDef.CreateImplication(_graph, Requirements, Producing));
                            skipProducing = true;
                        }
                    }
                    else
                    {
                        if (Consequences.Count > 0)
                        {
                            temp = Consequences;
                            skipProducing = false;
                        }
                        else
                        {
                          
                        }
                    }
                    if (skipProducing)
                    {
                        return temp;
                    }
                    foreach (var t in Producing)
                    {
                        temp.Add(t);
                    }
                    return temp;
                }
            }

            public void AssertTriples(IGraph kb, bool check)
            {
                if (check && !RequirementsMet)
                {
                    throw ErrorBadOp("Meet requirements please! " + ToString());
                }
                foreach (Triple triple in ToTriples)
                {
                    rdfGraphAssert(kb, triple, check);
                }
                if (check) ContainingGraph = kb;
            }

            internal void AddSubject(INode rdf)
            {
                Subjects.Add(rdf);
            }

            public void Clear()
            {
                Producing.Clear();
                Consequences.Clear();
                Requirements.Clear();
            }

            public void AddRequirement(INode s, string sp, Part o)
            {
                AddRequirement(MakeTriple(s,
                           GraphWithDef.PredicateToProperty(sp),
                           GraphWithDef.PartToRdf(o, this)));
            }
        }

        public static Triple MakeTriple(INode s, INode p, INode o)
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
            string warn = "";
            if (s is ILiteralNode)
            {
                warn += "s = LiteralNode (" + s + ")";
            }
            if (p is ILiteralNode)
            {
                warn += "p = LiteralNode (" + p + ")";
            }
            if (warn != "")
            {
                warn += "bad triple =" + newTriple;
                Warn(warn);
            }
            return newTriple;
        }
        static public bool rdfGraphAssert(IGraph rdfGraph, Triple triple)
        {
            return rdfGraphAssert(rdfGraph, triple, true);
        }

        static public bool rdfGraphAssert(IGraph rdfGraph, Triple triple, bool check)
        {
            if (check)
            {
                CheckTriple(triple);
            }
            lock (rdfGraph)
            {
                if (rdfGraph.ContainsTriple(triple)) return false;
                rdfGraph.Assert(triple);
                return true;
            }
        }

        public static void CheckTriple(Triple triple)
        {
            foreach (INode node in triple.Nodes)
            {
                if (node.NodeType == NodeType.Variable)
                {
                    Warn("Bad {0} found in triple {1}", node.NodeType, triple);
                }
            }
        }

        public void refreshRDFGraphOLD()
        {
            var rdfGraphWithDefs = MakeRepositoryKB("rdfMT");
            var rdfGraph = rdfGraphWithDefs.rdfGraph;
            EnsureGraphPrefixes(rdfGraph);
            // Possibly called by the Sparql endpoint before servicing a query
            // Is there anything we want to update rdfGraph with ?
            var bingingsList = new ListOfBindings();
            askQuery("triple(S,P,O)", "spindleMT", true, bingingsList, null);
            RdfRules newTriples = new RdfRules(rdfGraphWithDefs.definations);
            foreach (var bindings in bingingsList)
            {
                //foreach (string k in bindings.Keys)
                //{
                rdfGraphAssert(rdfGraph,
               MakeTriple(GraphWithDef.PartToRdf(bindings["S"], newTriples),
                          GraphWithDef.PartToRdf(bindings["P"], newTriples),
                          GraphWithDef.PartToRdf(bindings["O"], newTriples)));

                string rdfLine = String.Format(@"<robokind:{0}> <robokind:{1}> <robokind:{2}> .", bindings["S"].ToString(), bindings["P"].ToString(), bindings["O"].ToString());
                StringParser.Parse(rdfGraph, rdfLine);
                // }
            }
            newTriples.AssertTriples(rdfGraphWithDefs.definations, true);
        }
        public IGraph getRefreshedRDFGraph(string queryMT)
        {
            GraphWithDef graph = MakeRepositoryKB(queryMT);
            graph.ClearRDF();
            pushRulesToGraph(queryMT, graph, true);
            return graph.rdfGraph;
        }

        public static void EnsureGraphPrefixes(IGraph graph)
        {
            var nm = graph.NamespaceMap;
            if (nm.HasNamespace(RoboKindPrefix)) { return; }
            string s =
                @"
@prefix : <#> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix daml: <http://www.daml.org/2001/03/daml+oil#> .
@prefix log: <http://www.w3.org/2000/10/swap/log.n3#> .
@prefix foaf: <http://xmlns.com/foaf/0.1/> .
@prefix owl: <http://www.w3.org/2002/07/owl#> .
@prefix virtrdf: <http://www.openlinksw.com/schemas/virtrdf#> .
@prefix sswap: <http://sswapmeet.sswap.info/sswap/#> .
@prefix sioc: <http://rdfs.org/sioc/ns#> .
@prefix sioct: <http://rdfs.org/sioc/types#> .
@prefix atom: <http://atomowl.org/ontologies/atomrdf#> .
@prefix dc: <http://purl.org/dc/elements/1.1/> .
@prefix dct: <http://purl.org/dc/terms/> .
@prefix skos: <http://www.w3.org/2004/02/skos/core#> .
@prefix geo: <http://www.w3.org/2003/01/geo/wgs84_pos#> .
@prefix wikiont: <http://sw.deri.org/2005/04/wikipedia/wikiont.owl#> .
@prefix aowl: <http://atomowl.org/ontologies/atomrdf#> .
@prefix v: <http://www.openlinksw.com/schemas/drupal_v#> .
@prefix sd: <http://www.w3.org/ns/sparql-service-description#> .
@prefix dbpprop: <http://dbpedia.org/property/> .
@prefix dbpedia-owl:	<http://dbpedia.org/ontology/> .
@prefix dc: <http://purl.org/dc/elements/1.1/> .
@prefix grddl: <http://www.w3.org/2003/g/data-view#> .
@prefix xml: <http://www.w3.org/XML/1998/namespace> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
@prefix siprolog: <http://cogbotserver:8123/siprolog#> .
@prefix robokind: <" +
                RoboKindURI + @"> .
";
            StringParser.Parse(graph, s, new Notation3Parser());
            string ss = @"
fn http://www.w3.org/2005/xpath-functions
gmlxbt http://www.opengis.net/gml/3.3/xbt
sch http://www.ascc.net/xml/schematron
# gml http://www.opengis.net/gml/3.2
gml http://www.opengis.net/gml/_
gmd http://www.isotc211.org/2005/gmd
xlink http://www.w3.org/1999/xlink
xsl  http://www.w3.org/1999/XSL/Transform 
rdf  http://www.w3.org/1999/02/22-rdf-syntax-ns# 
p3q  http://www.w3.org/2004/01/rdxh/p3q-ns-example 
p3qr http://www.example.org/P3Q-rdf# 
p3dr http://www.example.org/TR/P3P/base# 
ont  http://www.daml.org/2001/03/daml+oil# 
s http://schema.org/
xsd http://www.w3.org/2001/XMLSchema#
eco http://www.ebusiness-unibw.org/ontologies/eclass/5.1.4/
gr http://purl.org/goodrelations/v1#
dc http://purl.org/dc/elements/1.1/
ao	http://purl.org/ao/core/
aoa	http://purl.org/ao/annotea/
aof	http://purl.org/ao/foaf/
aold	http://biotea.ws/ontologies/aold/
aos	http://purl.org/ao/selectors/
aot	http://purl.org/ao/types/
bibo	http://purl.org/ontology/bibo/
bif	bif:
bio2rdf_mesh	http://bio2rdf.org/ns/mesh#
bio2rdf_ns	http://bio2rdf.org/ns/bio2rdf#
chebi	http://purl.obolibrary.org/obo/CHEBI_
cnt	http://www.w3.org/2011/content#
dawgt	http://www.w3.org/2001/sw/DataAccess/tests/test-dawg#
dbpedia	http://dbpedia.org/resource/
dbpprop	http://dbpedia.org/property/
dc	http://purl.org/dc/elements/1.1/
dcterms	http://purl.org/dc/terms/
doco	http://purl.org/spar/doco/
fma	http://purl.org/obo/owl/FMA#FMA_
fn	http://www.w3.org/2005/xpath-functions/#
foaf	http://xmlns.com/foaf/0.1/
geo	http://www.w3.org/2003/01/geo/wgs84_pos#
go	http://purl.org/obo/owl/GO#GO_
gw_property	http://genewikiplus.org/wiki/Special:URIResolver/Property-3A
gw_wiki	http://genewikiplus.org/wiki/Special:URIResolver/
icd9	http://purl.bioontology.org/ontology/ICD9-9/
math	http://www.w3.org/2000/10/swap/math#
mddb	http://purl.bioontology.org/ontology/MDDB/
meddra	http://purl.bioontology.org/ontology/MDR/
medline	http://purl.bioontology.org/ontology/MEDLINEPLUS/
mesh	http://purl.org/commons/record/mesh/
mf	http://www.w3.org/2001/sw/DataAccess/tests/test-manifest#
mged	http://mged.sourceforge.net/ontologies/MGEDOntology.owl#
ncbitaxon	http://purl.org/obo/owl/NCBITaxon#NCBITaxon_
ncithesaurus	http://ncicb.nci.nih.gov/xml/owl/EVS/Thesaurus.owl#
nddf	http://purl.bioontology.org/ontology/NDDF/
ndfrt	http://purl.bioontology.org/ontology/NDFRT/
obi	http://purl.obolibrary.org/obo/OBI_
obo	http://www.geneontology.org/formats/oboInOwl#
omim	http://purl.bioontology.org/ontology/OMIM/
owl	http://www.w3.org/2002/07/owl#
pav	http://purl.org/swan/pav/provenance/
po	http://purl.bioontology.org/ontology/PO/
product	http://www.buy.com/rss/module/productV2/
protseq	http://purl.org/science/protein/bysequence/
prov	http://www.w3.org/ns/prov#
pw	http://purl.org/obo/owl/PW#PW_
rdf	http://www.w3.org/1999/02/22-rdf-syntax-ns#
rdfa	http://www.w3.org/ns/rdfa#
rdfdf	http://www.openlinksw.com/virtrdf-data-formats#
rdfs	http://www.w3.org/2000/01/rdf-schema#
sc	http://purl.org/science/owl/sciencecommons/
scovo	http://purl.org/NET/scovo#
sioc	http://rdfs.org/sioc/ns#
skos	http://www.w3.org/2004/02/skos/core#
snomed	http://purl.bioontology.org/ontology/SNOMEDCT/
sql	sql:
swivt	http://semantic-mediawiki.org/swivt/1.0#
symptom	http://purl.org/obo/owl/SYMP#SYMP_
taxonomy	http://www.uniprot.org/taxonomy/
umls	http://berkeleybop.org/obo/UMLS:
uniprot	http://purl.uniprot.org/core/
vcard	http://www.w3.org/2001/vcard-rdf/3.0#
vcard2006	http://www.w3.org/2006/vcard/ns#
virtcxml	http://www.openlinksw.com/schemas/virtcxml#
virtrdf	http://www.openlinksw.com/schemas/virtrdf#
void	http://rdfs.org/ns/void#
xf	http://www.w3.org/2004/07/xpath-functions
xml	http://www.w3.org/XML/1998/namespace
xsd	http://www.w3.org/2001/XMLSchema#
xsl10	http://www.w3.org/XSL/Transform/1.0
xsl1999	http://www.w3.org/1999/XSL/Transform
xslwd	http://www.w3.org/TR/WD-xsl
xsp	http://www.owl-ontologies.com/2005/08/07/xsp.owl#
yago	http://dbpedia.org/class/yago/
";

            foreach (string s00 in ss.Split('\n', '\r'))
            {
                if (string.IsNullOrEmpty(s00)) continue;
                var s0 = s00.Replace('\t', ' ').Trim();
                if (s0.StartsWith("#")) continue;
                if (string.IsNullOrEmpty(s0)) continue;
                int spc = s0.IndexOf(' ');
                string prefix = s0.Substring(0, spc).Trim().TrimEnd(' ', ':');
                string uri = s0.Substring(spc).Trim();
                if (nm.HasNamespace(prefix))
                {
                    var prev = nm.GetNamespaceUri(prefix).ToString();
                    if (prev != uri)
                    {
                        if (uri.Length < prev.Length)
                        {
                            continue;
                        }
                    }
                }
                nm.AddNamespace(prefix, new Uri(uri));
            }
        }

        public void mtest()
        {
            IGraph g = new Graph();

            IUriNode dotNetRDF = g.CreateUriNode(UriFactory.Create("http://www.dotnetrdf.org"));
            IUriNode says = g.CreateUriNode(UriFactory.Create("http://example.org/says"));
            ILiteralNode helloWorld = g.CreateLiteralNode("Hello World");
            ILiteralNode bonjourMonde = g.CreateLiteralNode("Bonjour tout le Monde", "fr");

            rdfGraphAssert(g, MakeTriple(dotNetRDF, says, helloWorld));
            rdfGraphAssert(g, MakeTriple(dotNetRDF, says, bonjourMonde));

            foreach (Triple t in g.Triples)
            {
                ConsoleWriteLine(t.ToString());
                ConsoleWriteLine("TRIPLE: triple(\"{0}\",\"{1}\",\"{2}\").", t.Subject.ToString(),
                                  t.Predicate.ToString(), t.Object.ToString());
            }

            NTriplesWriter ntwriter = new NTriplesWriter();
            ntwriter.Save(g, "HelloWorld.nt");

            RdfXmlWriter rdfxmlwriter = new RdfXmlWriter();
            rdfxmlwriter.Save(g, "HelloWorld.rdf");

            rdfImportToKB(g,
                          "testRDF",
                          "SELECT * WHERE { ?s ?p ?o }",
                          null);
            foreach (var endp in
                new[]
                    {
                        // "http://budapest.rkbexplorer.com/sparql",
                        "http://dbpedia.org/sparql",
                //        "http://lod.hebis.de/sparql",                        
                    })
            {
                rdfRemoteEndpointToKB(endp,
                                      "dbpediaKB",
                                      "SELECT DISTINCT ?o WHERE { ?s a ?o } LIMIT 100",
                                      "isa($?o$,'http://www.w3.org/2002/07/owl#Class')");

            }
            return;
            rdfRemoteEndpointToKB("http://dbpedia.org/sparql",
                                  "dbpediaKB2",
                                  "SELECT * WHERE { ?s ?p ?o } LIMIT 1000",
                                  null);

        }

        #endregion
        static Graph forReaderTripleStore = new Graph();
        static TurtleParser forReaderTurtleParser = new TurtleParser();
        [ThreadStatic]
        private string tl_ServerRoot;
        [ThreadStatic]
        private string tl_mt;
        [ThreadStatic]
        private TextWriter tl_writer;

        private static INode MakeNode(string s, string quoting)
        {
            switch (quoting)
            {
                case null:
                    {

                        return GetValuedNode(s);
                    }
                case "\"\"":
                    {
                        return new StringNode(rdfDefinations, s);
                    }
                case "''":
                    {
                        return GraphWithDef.C(rdfDefinations, s);
                    }
                default:
                    return GetValuedNode(s);
                    throw ErrorBadOp(s + " " + quoting);
            }
        }

        private static IValuedNode GetValuedNode(string s)
        {
            lock (forReaderTripleStore)
            {
                forReaderTripleStore.Clear();
                EnsureReaderNamespaces();
                //forReaderTurtleParser.Load(forReaderTripleStore, "{ 1 1 " + s + " }");
                try
                {
                    StringParser.Parse(forReaderTripleStore, "<http://example.org/a1> <http://example.org/a1> " + s + " . ");
                    var t = forReaderTripleStore.Triples.First().Object;
                    return t.AsValuedNode();
                }
                catch (Exception e)
                {
                }
                return null;
            }
        }
        public partial class Rule
        {
            public RdfRules rdfRuleCache;
        }

        public class PredicateProperty
        {
            public string name;
            public int arity;
            public string classname;
            public string keyname;
            public int instanceNumber = 1;
            static public int varNumber = 1;
            public readonly Dictionary<int, RDFArgSpec> argDefs;
            public INode classNode;
            public string assertionMt;
            public List<Triple> inations = new List<Triple>();

            public override string ToString()
            {
                return StructToString(this, 2);
            }

            public int GetArgNumForName(string argName)
            {
                foreach (KeyValuePair<int, RDFArgSpec> def in argDefs)
                {
                    if (def.Value.NameMatches(argName)) return def.Key;
                }
                throw ErrorBadOp(argName + " for " + this);

            }

            public PredicateProperty(int arity1)
            {
                arity = arity1;
                argDefs = new Dictionary<int, RDFArgSpec>(arity1 + 1);
            }


            public void WriteHtmlInfo(TextWriter writer)
            {
                writer.WriteLine("<pre>");
                writer.WriteLine("<strong><font color='green'>{0}</font></strong>", name + "/" + arity);
                foreach (KeyValuePair<int, RDFArgSpec> def in argDefs)
                {
                    writer.WriteLine(" Arg " + def.Key + " = " + def.Value.ArgNameInfo);
                }
                writer.WriteLine("</pre>");
            }
        }

        public class RDFArgSpec
        {
            public RDFArgSpec(int hintArgNum1Based)
            {
                argNumHint = hintArgNum1Based;
            }
            private string predicateArgName = "_";
            public INode predicateNode;
            public int argNumHint = -1;
            public HashSet<string> argNames = new HashSet<string>();
            public List<PredicateProperty> PredicateProperties = new List<PredicateProperty>();
            public string ArgNameInfo
            {
                get
                {
                    return predicateArgName;
                }
            }
            //public string assertionMt;
            public INode GetRefNode(IGraph def)
            {
                if (predicateArgName.EndsWith("_"))
                {
                    predicateArgName += argNumHint;
                    //Warn("Poorly named prolog argument spec will make a pooly named RDF predicate! " + this);
                    
                }
                return predicateNode ?? def.CreateUriNode(RoboKindPrefixPrepend + predicateArgName);
            }
            public override string ToString()
            {
                return StructToString(this, 2);
            }
            public void AddDomainType(PredicateProperty property, int argNumber1Based)
            {
                lock(PredicateProperties)
                {
                    PredicateProperties.Add(property);
                }
                string functor = property.name;
                if (!predicateArgName.ToLower().StartsWith(functor.ToLower()))
                {
                    predicateArgName = functor + "_" + predicateArgName.TrimStart("_".ToCharArray());
                }
                //AddRangeTypeName("_Arg" + argNumber1Based);
            }

            public void AddRangeTypeName(string functor)
            {
                functor = ProperCase(functor);
                if (argNames.Add(functor) || !predicateArgName.ToLower().Contains(functor.ToLower()))
                {
                    predicateArgName = predicateArgName + functor;
                }
            }

            private string ProperCase(string functor)
            {
                functor = functor.Substring(0, 1).ToUpper() + functor.Substring(1);
                return functor;
            }

            public bool NameMatches(string name)
            {
                return predicateArgName.Contains(ProperCase(name));
            }
        }
        public partial class GraphWithDef
        {
            public override string ToString()
            {
                return StructToString(this, 1);
            }

            public IGraph rdfGraph;
            public IGraph definations;
            public string prologMt;

            static public List<PredicateProperty> localPreds = new List<PredicateProperty>();
            static public List<RDFArgSpec> localArgTypes = new List<RDFArgSpec>();
            static public List<Term> localPredInstances = new List<Term>();
            private PNode kbNode;
            public SIProlog prologEngine;

            public PNode PrologKB
            {
                get
                {
                    kbNode = kbNode ?? prologEngine.KBGraph.Contains(prologMt);
                    return kbNode;
                }
                set { kbNode = value; }
            }

            public GraphWithDef(string plMt, SIProlog prolog, IGraph data, IGraph defs)
            {
                this.prologEngine = prolog;
                prologMt = plMt;
                rdfGraph = data;
                string BaseURI = RoboKindURI ?? RoboKindURI.TrimEnd('#', '/') + "/" + plMt + "/";
                data.BaseUri = data.BaseUri ?? new Uri(BaseURI);
                EnsureGraphPrefixes(rdfGraph);
                EnsureGraphPrefixes(defs);
                definations = defs;
            }

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
                        var classNode = def.classNode = def.classNode ?? C(rdef, RoboKindURI + def.classname);
                        def.inations.Add(MakeTriple(classNode, InstanceOf, PrologPredicateClass));
                        for (int i = 0; i < arity; i++)
                        {
                            RDFArgSpec adef = GetAdef(def, i + 1, false);
                            if (adef == null)
                            {
                                adef = GetAdef(def, i + 1, true);
                            }
                            var subj = adef.GetRefNode(rdfDefinations);
                            def.inations.Add(MakeTriple(subj, InstanceOf, PrologPredicate));
                            def.inations.Add(MakeTriple(subj, C(rdef, "rdfs:domain"), classNode));
                            def.inations.Add(MakeTriple(subj, C(rdef, "rdfs:range"), C(rdef, "rdfs:Literal")));
                            if (!localArgTypes.Contains(adef)) localArgTypes.Add(adef);
                        }
                    }
                }
                lock (localPreds)
                {
                    if (!localPreds.Contains(def))
                    {
                        localPreds.Add(def);
                    }
                }
                if (newlyCreated)
                {
                    foreach (Triple t in def.inations)
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
                string key = Unsymbolize(predName) + "_" + arity;
                lock (SharedGlobalPredDefs)
                {
                    if (!SharedGlobalPredDefs.TryGetValue(key, out def))
                    {
                        newlyCreated = true;
                        string predClassName = "PredClass_" + key;
                        SharedGlobalPredDefs[key] =
                            def =
                            new PredicateProperty(arity) {name = predName, keyname = key, classname = predClassName};
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
            public INode C(string p0)
            {
                return C(definations, p0);
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
                    lock (GuessedNameSpace)
                    {
                        KeyValuePair<string, string> uriGuess;
                        if (colm == -1 && GuessedNameSpace.TryGetValue(p, out uriGuess))
                        {
                            var pref = uriGuess.Value ?? (uriGuess.Key + ":");
                            newUri = new Uri(pref + "" + p);
                            return def.CreateUriNode(newUri);
                        }
                    }
                    if (uir || Uri.IsWellFormedUriString(p, UriKind.Absolute))
                    {
                        newUri = new Uri(p);
                        return def.CreateUriNode(newUri);
                    }
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
                        var node = CUrlNode(def,p2, colm == -1, false);
                        if (node != null) return node;
                    }
                }
                return CreateLiteralNode(def,p);
            }

            private static INode CreateLiteralNode(INodeFactory def, string p)
            {
                return def.CreateLiteralNode(p);
            }

            public INode CreateLiteralNode(string s)
            {
                return definations.CreateLiteralNode(s);
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
                            newUri = new Uri(pref + "" + p);
                            return definations.CreateUriNode(newUri);
                        }
                        catch (Exception)
                        {
                        }
                    }
                    if (Uri.IsWellFormedUriString(p, UriKind.Absolute))
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

            public static RdfRules FromRule(Rule rule, IGraph kb)
            {
                if (IsRdfBuiltIn(rule.head))
                {
                    return null;
                }
                PredicateProperty headDef = AddDefs(rule);
                if (rule.rdfRuleCache != null) return rule.rdfRuleCache;

                var rdfRules = rule.rdfRuleCache = new RdfRules(kb);
                AddData(rule, headDef);
                rdfRules.AssertTriples(kb, true);
                return rdfRules;
            }

            public void AddRule(Rule rule)
            {
                if (rule.rdfRuleCache != null) return;
                FromRule(rule, rdfGraph);
            }

            static bool IsRdfBuiltIn(Term thisTerm)
            {
                if (thisTerm.name=="uu") return true;
                //return false;
                if (thisTerm.Arity < 2) return true;
                if (thisTerm.Arity > 2) return false;
                lock (PDB.builtin)
                {
                    return PDB.builtin.ContainsKey(thisTerm.name + "/" + thisTerm.Arity);
                }
            }

            static private void AddData(Rule rule, PredicateProperty headDef)
            {
                Term head = rule.head;
                Body rulebody = rule.body;
                var varNames = new List<string>();
                var newVarNames = new List<string>();
                int newVarCount;
                lock (rule)
                {
                    PartList bpl = AnalyzeHead(head, true, varNames, newVarNames, out newVarCount);
                    if (newVarCount > 0)
                    {
                        if (rulebody != null)
                        {
                            foreach (Part p in rulebody.plist)
                            {
                                bpl.AddPart(p);
                            }
                        }
                        rulebody = new Body(bpl);
                    }
                }
                AddData(rule.rdfRuleCache, rule, head, rulebody, headDef, varNames, newVarCount, newVarNames);
            }

            public static PartList AnalyzeHead(Part head, bool replaceVars, ICollection<string> varNames, ICollection<string> newVarNames, out int newVarsNeeded)
            {
                int newVarCount = 0;
                PartList[] pl = {null};
                head.Visit((a, pr) =>
                               {
                                   if (!(a is Variable))
                                   {
                                       a.Visit(pr);
                                       return a;
                                   }
                                   string an = a.name;
                                   if (newVarNames != null && newVarNames.Contains(an))
                                   {
                                       // Dont copy previously copied vars
                                       return a;
                                   }
                                   if (varNames != null && !varNames.Contains(an))
                                   {
                                       // First time found
                                       varNames.Add(an);
                                       return a;
                                   }
                                   if (!replaceVars)
                                   {
                                       newVarCount++;
                                       return a;
                                   }
                                   // copy the var to: newVarName
                                   string newVarName = an + newVarCount;
                                   if (newVarNames != null) newVarNames.Add(newVarName);
                                   var r = new Variable(newVarName);
                                   // add the unification to the partlist
                                   var lpl = pl[0] = pl[0] ?? new PartList();
                                   newVarCount++;
                                   lpl.AddPart(unifyvar(a, r));
                                   return r;
                               });
                newVarsNeeded = newVarCount;
                return pl[0];
            }

            static private void AddData(RdfRules rdfRules, Rule rule, Term head, Body rulebody, PredicateProperty headDef, List<string> varNames, int newVarCount, List<string> newVarNames)
            {                
                var ruleSubject = CreateConsequentNode(head, headDef, rdfRules, false);
                if (rulebody != null)
                {
                    foreach (Part p in rulebody.plist.ArgList)
                    {
                        GatherTermAntecedants(p, headDef, rdfRules);
                    }
                }
                var definations = rdfRules.def;
                rdfRules.AddProducing(MakeTriple(ruleSubject, definations.CreateUriNode("siprolog:sourceCode"), 
                    definations.CreateLiteralNode(rule.ToString(), "prolog")));
                rdfRules.RequirementsMet = true;
                return;
            }

            static private void GatherTermAntecedants(Part part, PredicateProperty headDefOrNull,RdfRules anteceeds)
            {
                if (part is Term)
                {
                    var rdf = CreateAntecedantNode((Term) part, anteceeds);
                    anteceeds.AddSubject(rdf);
                    if (rdf != null)
                    {

                    }
                    return;
                }
                throw ErrorBadOp("Part is not a Term " + part);
            }

            static private Part unifyvar(Part p1, Variable p2)
            {
                var args = new PartList();
                args.AddPart(p1);
                args.AddPart(p2);
                return new Term("unify", args);
            }

            private bool ContainsUnknowns(RdfRules headtriples)
            {
                foreach (Triple triple in headtriples.ToTriples)
                {
                    
                }
                return false;
            }

            public static Triple CreateImplication(IGraph def, ICollection<Triple> bodytriples, ICollection<Triple> headtriples)
            {
                return MakeTriple(ToBracket(def, bodytriples), def.CreateUriNode("log:implies"),
                                  ToBracket(def, headtriples));
            }

            public static IGraphLiteralNode ToBracket(INodeFactory def, ICollection<Triple> bodytriples)
            {
                Graph subgraph = new Graph();
                foreach (Triple triple in bodytriples)
                {
                    subgraph.Assert(triple);
                }
                var group = def.CreateGraphLiteralNode(subgraph);
                return group;
            }

            static private INode CreateSubject(Term term, RdfRules rules, bool isPrecond)
            {
                var headDef = GetPredicateProperty(term);
                INode subj = CreateInstance(headDef, rules, isPrecond ? NodeType.Variable : NodeType.Uri);
                var conds = AddTriplesSubject( term, rules, subj);
                foreach (Triple triple in conds)
                {
                    if (isPrecond)
                    {
                        rules.AddRequirement(triple);
                    }
                    else
                    {
                        rules.AddConsequent(triple);
                    }
                }
                return subj;
            }

            static private List<Triple> AddTriplesSubject(Term term, RdfRules rules, INode subj)
            {
                int argNum = 1;
                var conds = new List<Triple>();
                var headDef = GetPredicateProperty(term);
                foreach (Part part in term.Args)
                {
                    RDFArgSpec argDef = GetAdef(headDef, argNum, false);
                    if (part is Variable)
                    {
                        if (argDef == null)
                        {
                            argDef = GetAdef(headDef, argNum, true);
                            argDef.AddRangeTypeName(part.name);
                        }
                    }
                    INode obj = PartToRdf(part, rules);
                    if (argDef == null)
                    {
                        argDef = GetAdef(headDef, argNum, true);
                    }
                    INode pred = argDef.GetRefNode(rdfDefinations);
                    conds.Add(MakeTriple(subj, pred, obj));
                    argNum++;
                }
                return conds;
            }

            static private INode CreateConsequentNode(Term term, PredicateProperty headDefOrNull, RdfRules triples, bool isVar)
            {
                if (IsRdfBuiltIn(term))
                {
                    var rdf = BuiltinToRDF(term, triples);
                    triples.AddSubject(rdf);
                    return rdf;
                }
                bool isPrecond = isVar;
                var rdf0 = CreateSubject(term, triples, isPrecond);
                triples.AddSubject(rdf0);
                return rdf0;
            }

            static private INode CreateAntecedantNode(Term term, RdfRules triples)
            {
                if (IsRdfBuiltIn(term))
                {
                    var rdf = BuiltinToRDF(term, triples);
                    return rdf;
                }
                return CreateSubject(term, triples, true);
            }

            static private INode BuiltinToRDF(Term term, RdfRules antecedants)
            {
                var definations = antecedants.def;
                int arity = term.Arity;
                if (arity == 2)
                {
                    antecedants.AddRequirement(MakeTriple(PartToRdf(term.ArgList[0], antecedants),
                                              PredicateToProperty(term.name),
                                              PartToRdf(term.ArgList[1], antecedants)));
                    return definations.CreateUriNode("rdfs:true");
                }
                if (arity == 1)
                {
                    INode partToRdf = PartToRdf(term.ArgList[0], antecedants);
                    var dataType = PredicateToType(term.name);
                    antecedants.AddRequirement(MakeTriple(partToRdf, InstanceOf, dataType));
                    return definations.CreateUriNode("rdfs:true");
                }
                throw ErrorBadOp("Cant reate Bultin from " + term);
                
            }

            static INode PredicateToType(string unaryPred)
            {
                if (unaryPred == "call")
                {
                    return rdfDefinations.CreateUriNode("rdfs:true");
                }

                return rdfDefinations.CreateUriNode("siprolog:" + unaryPred);
            }
            
            static public INode PredicateToProperty(string binaryPred)
            {
                if (binaryPred == "unify")
                {
                    return C(rdfDefinations, "owl:sameAs");
                }

                return rdfDefinations.CreateUriNode("siprolog:" + binaryPred);
            }

            static private INode CreateInstance(PredicateProperty headDef, RdfRules graph, NodeType nodeType)
            {
                var definations = graph.def;
                int nxt = headDef.instanceNumber++;
                string iname = "Pred" + nxt + "_" + headDef.keyname;
                INode iln = null;
                switch (nodeType)
                {
                    case NodeType.Blank:
                        iln = definations.CreateBlankNode(iname);
                        break;
                    case NodeType.Variable:
                        iname = iname.Replace("_", "").Replace("_", "").ToUpper();
                        iln = definations.CreateVariableNode(iname); 
                        break;
                    case NodeType.Uri:
                        iln = C(definations, RoboKindURI + iname);
                        break;
                    case NodeType.Literal:
                        iln =  definations.CreateLiteralNode(iname);
                        break;
                    case NodeType.GraphLiteral:
                        throw new ArgumentOutOfRangeException("nodeType");
                        break;
                    default:
                        throw new ArgumentOutOfRangeException("nodeType");
                }
                var a = InstanceOf;
                var cn = headDef.classNode = headDef.classNode ?? C(rdfDefinations, RoboKindURI + headDef.classname);
                graph.AddProducing(MakeTriple(iln, a, cn));
                return iln;
            }

            public static long CONSP = 555;
            static public INode PartToRdf(Part part, RdfRules triples)
            {
                var definations = triples.def;
                if (part is Atom)
                {
                    Atom atom = ((Atom)part);
                    return atom.AsValuedNode();
                }
                if (part is Variable)
                {
                    return definations.CreateVariableNode(part.name);
                }
                Part car, cdr;
                if (GetCons(part, out car, out cdr))
                {
                    CONSP++;
                    var rdf = definations.CreateVariableNode("CONS" + CONSP);
                    triples.AddRequirement(rdf, "rdf:car", car);
                    triples.AddRequirement(rdf, "rdf:cdr", cdr);
                    return rdf;
                }
                if (part is Term)
                {
                    return CreateAntecedantNode((Term)part, triples);
                }
                throw ErrorBadOp("ToRDF on " + part);
            }

            static readonly Dictionary<string, KeyValuePair<string, string>> GuessedNameSpace = new Dictionary<string, KeyValuePair<string, string>>();
            public Part RdfToPart(INode node, RdfRules triples)
            {
                if (node is StringNode)
                {
                    var atom = Atom.Make(node.ToString());
                    atom.quoted = "\"\"";
                    return atom;
                }
                if (node is IUriNode)
                {
                    IUriNode iuri = (IUriNode)node;
                    string s = iuri.Uri.ToString();
                    Atom atomMake;
                    lock (AtomTable)
                    {
                        if (AtomTable.TryGetValue(s, out atomMake))
                        {
                            return atomMake;
                        }
                    }
                    string prefix, uri, atom;
                    if (DevolveURI(definations.NamespaceMap, s, out uri, out prefix, out atom) || atom != null)
                    {
                        if (!char.IsLetterOrDigit(atom[0]) && !atom.StartsWith("#C_"))
                        {
                            Warn("strange atom='{0}' prefix='{1}' uri='{2}' ", atom, prefix, uri);
                            //string satom = HttpUtility.UrlEncode(atom);
                        }
                        if (prefix == null && uri != null)
                        {
                            DiscoverNameSpace(uri);
                        }
                        atomMake = Atom.Make(atom);
                        lock (AtomTable)
                        {
                            AtomTable[s] = atomMake;
                        }
                        lock (GuessedNameSpace)
                        {
                            KeyValuePair<string, string> gns;
                            bool fnd = GuessedNameSpace.TryGetValue(atom, out gns);
                            GuessedNameSpace[atom] = new KeyValuePair<string, string>(prefix ?? gns.Key,
                                                                                      uri ?? gns.Value);
                        }
                        return atomMake;
                    }
                    int hash = s.IndexOf("#");
                    s = s.Substring(1 + hash);
                    if (hash == -1 && !s.Contains(":/"))
                    {
                        hash = s.IndexOf(":");
                        if (hash > 0)
                        {
                            s = s.Substring(1 + hash);
                        }
                    }
                    if (hash == -1)
                    {
                        s = "<" + s + ">";
                    }
                    return Atom.Make(s);
                }
                if (node is IGraphLiteralNode)
                {
                    return Atom.Make(node.ToString());
                }
                throw ErrorBadOp("ToProlog on " + node);
            }

            private static HashSet<string> MissingNameSpaces = new HashSet<string>();
            private void DiscoverNameSpace(string uri)
            {
                if (MissingNameSpaces.Add(uri))
                {
                    Warn("New namespace that was missing: " + uri);
                }
            }

            static public bool DevolveURI(INamespaceMapper mapper, string s, out string uri, out string prefix, out string atom)
            {
                atom = null;
                uri = null;
                prefix = null;
                foreach (var pfx in mapper.Prefixes)
                {
                    prefix = pfx;
                    var uril = mapper.GetNamespaceUri(prefix);
                    uri = uril.ToString();
                    if (s.StartsWith(uri))
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

            public void pushRulesToGraph()
            {
                prologEngine.pushRulesToGraph(prologMt, this, true);
            }

            public void pushGraphToKB()
            {
                prologEngine.pushRulesToGraph(prologMt, this, true);
            }

            public BaseTripleCollection Triples
            {
                get { return rdfGraph.Triples; }
            }

            public object WriteTriple(Triple arg)
            {
                //Notation3Writer()
                return arg.ToString();
            }

            public string PlReadble(INode subject)
            {
                string quoting = ToQuoting(subject.NodeType);
                string p = subject.ToString();
                Atom newAtom = Atom.MakeAtom(p, quoting, subject);
                string s = newAtom.ToPLStringReadable();
                string s1 = newAtom.name;
                string s2 = newAtom.AsString();
                if (s != s1)
                {

                }
                return s;
            }

            private static string ToQuoting(NodeType type)
            {
                switch (type)
                {
                    case NodeType.Blank:
                        break;
                    case NodeType.Uri:
                        return "''";
                        break;
                    case NodeType.Literal:
                        return "\"\"";
                        break;
                    case NodeType.GraphLiteral:
                        break;
                    case NodeType.Variable:
                        break;
                    default:
                        break;
                }
                throw ErrorBadOp("to quoting on " + type);
            }

            internal void ClearRDF()
            {
                rdfGraph = new Graph();
            }
        }
        public static void Warn(string format, params object[] args)
        {
            string write = DLRConsole.SafeFormat(format, args);
             DLRConsole.DebugWriteLine(write);
        }
        public static void Warn(object arg0)
        {
            Warn("{0}", arg0);
        }
        public static void ConsoleWriteLine(string format, params object[] args)
        {
            string write = DLRConsole.SafeFormat(format, args);
            DLRConsole.DebugWriteLine(write);
        }

        private static void WriteGraph(TextWriter writer, IGraph graph, IGraph defs, string named)
        {
            WriteGraph(writer, graph, "Data for " + named, false, true);
            WriteGraph(writer, defs, "Defs for " + named, true, true);
        }
        internal static void WriteGraph(TextWriter writer, IGraph graph, string named, bool plain, bool inHtml)
        {
            string printerName = "plain";
            if (plain)
            {
                DumpTriples(graph, writer, named);
                return;
            }
            else
            {
                IRdfWriter n3w;
                StringWriter dtt = new StringWriter();
                string dttToString = "error";
                bool prettyWillWorked = false;

                if (prettyWillWorked)
                {
                    try
                    {
                        n3w = new CompressingTurtleWriter()
                                  {DefaultNamespaces = graph.NamespaceMap, PrettyPrintMode = true};
                        n3w.Save(graph, dtt);
                        dttToString = dtt.ToString();
                        prettyWillWorked = true;
                        printerName = n3w.GetType().Name;
                    }
                    catch (Exception e)
                    {
                        dttToString = dtt.ToString();
                        prettyWillWorked = false;
                    }
                }
                if (!prettyWillWorked)
                {
                    n3w = new Notation3Writer() {DefaultNamespaces = graph.NamespaceMap, PrettyPrintMode = true};
                    dtt = new StringWriter();
                    try
                    {
                        n3w.Save(graph, dtt);
                        dttToString = dtt.ToString();
                        printerName = n3w.GetType().Name;
                    }
                    catch (Exception e)
                    {
                        if (inHtml)
                        {
                            writer.WriteLine("<pre><font color='red'>{0} {1} {2}</font></pre>", e.GetType(), e.Message,
                                             e.StackTrace);
                            printerName = "triples";
                            DumpTriples(graph, writer, named);
                        }
                        else
                        {
                            printerName = "plain";
                            DumpTriplesPlain(graph.Triples, writer, "{0}", graph);
                        }
                        return;
                    }
                }
                if (inHtml)
                {
                    writer.WriteLine("<h3>{0} KB {1}</h3>", named, printerName);
                    writer.WriteLine("<pre>");
                }
                dttToString = dttToString.Replace("\r\n", "\n").Replace("\r", "\n");
                foreach (string line in dttToString.Split('\n'))
                {
                    if (string.IsNullOrEmpty(line))
                    {
                        writer.WriteLine();
                        continue;
                    }
                    if (line.StartsWith("@prefix ")) continue;
                    var hline = line;
                    hline = (" " + hline).Replace(" robokind:", " rk:");
                    hline = hline.Replace("<robokind:", "<rk:");
                    if (inHtml) hline = hline.Replace("<", "&lt;").Replace(">", "&gt;");
                    writer.WriteLine(hline);
                }
                if (inHtml)
                {
                    writer.WriteLine("</pre>");
                }
            }
        }

        private static void DumpTriples(IGraph graph, TextWriter writer, string named)
        {
            var trips = graph.Triples;
            writer.WriteLine("<h3>{0} KB Triples {1}</h3>", named, trips.Count);
            writer.WriteLine("<pre>");
            try
            {
                DumpTriplesPlain(trips, writer, "<font color='green'>{0}</font>", graph);
            }
            catch (Exception e)
            {
                Warn(e);
                writer.WriteLine("<font color='red'>{0} {1} {2}</font>", e.GetType(), e.Message, e.StackTrace);
            }
            writer.WriteLine("</pre>");
        }
        private static void DumpTriplesPlain(IEnumerable<Triple> trips, TextWriter writer, string fmt, IGraph from)
        {
            var formatter = new SparqlFormatter/* TurtleW3CFormatter, TurtleFormatter*/(from.NamespaceMap);
            try
            {
                foreach (Triple trip0 in trips)
                {
                    DumpOneTriple(from, trip0, formatter, writer, fmt);
                }
            }
            catch (Exception e)
            {
                Warn(e);
                throw e;
            }
        }

        private static void DumpOneTriple(IGraph from, Triple trip0, ITripleFormatter formatter, TextWriter writer, string fmt)
        {
            Triple trip = trip0;
            if (trip.Graph == null)
            {
                trip = new Triple(trip.Subject, trip.Predicate, trip.Object, from);
                trip.Context = trip0.Context;
            }
            string ts;
            try
            {
                ts = trip.ToString(formatter);
            }
            catch (RdfOutputException)
            {
                ts = trip.ToString();
            }
            var hline = (" " + ts).Replace(" robokind:", " rk:");
            hline = hline.Replace("<robokind:", "<rk:");
            writer.WriteLine(fmt, hline);
        }

        public static void DocumentTerm(Term term, bool varnamesOnly)
        {
            bool newlyCreated;
            PredicateProperty pp = GraphWithDef.GetPredDef(term.name, term.Arity, out newlyCreated);
            int argNum = 0;
            foreach (Part part in term.Args)
            {
                argNum++;
                if (varnamesOnly) if (!(part is Variable)) continue;
                var argDef = GraphWithDef.GetAdef(pp, argNum, true);
                argDef.AddRangeTypeName(part.name);
            }
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
    }


}