using System;
using System.Collections;
using System.Collections.Generic;
using System.IO;
using System.Reflection;
using LogicalParticleFilter1;
using VDS.RDF;
using VDS.RDF.Nodes;
using VDS.RDF.Parsing;
using VDS.RDF.Query;
using VDS.RDF.Writing;

namespace ExtensionMethods
{

    public static class RDFExtensions
    {
        //static public Dictionary<SIProlog.GraphWithDef.Rule, INode> rule2Node = new Dictionary<SIProlog.GraphWithDef.Rule, INode>();
        static public Dictionary<SIProlog.Rule, INode> rule2Node = new Dictionary<SIProlog.Rule, INode>();

        public static INode instanceTriple(this SIProlog.Rule rule)
        {
            INode node;
            if (rule2Node.TryGetValue(rule, out node))
            {
                return node;
            }
            return null;
        }
        public static int WordCount(this String str)
        {
            return str.Split(new char[] { ' ', '.', '?' },
                             StringSplitOptions.RemoveEmptyEntries).Length;
        }
    }
}
namespace LogicalParticleFilter1
{
    using ExtensionMethods;
    public partial class SIProlog
    {       
        readonly private IGraph rdfDefinations = new Graph();
        public Dictionary<string, GraphWithDef> GraphForMT = new Dictionary<string, GraphWithDef>();
        private static Dictionary<string, PredicateProperty> SharedGlobalPredDefs = new Dictionary<string, PredicateProperty>();
        private static Dictionary<string, ArgType> SharedGlobalArgTypeDefs = new Dictionary<string, ArgType>();

        private void defineRDFExtensions()
        {
            const string rdfDefMT = "globalRDFDefinationsMT";
            var node = FindOrCreateKB(rdfDefMT);
            GraphForMT[rdfDefMT] = new GraphWithDef(rdfDefMT, this, rdfDefinations, rdfDefinations) {PrologKB = node};
            mtest();
        }

        public GraphWithDef MakeRepositoryKB(string mt)
        {
            lock (GraphForMT)
            {
                GraphWithDef graph;
                if (!GraphForMT.TryGetValue(mt, out graph))
                {
                    graph = GraphForMT[mt] = new GraphWithDef(mt, this, new Graph(), rdfDefinations);
                    var node = graph.PrologKB;
                }
                return graph;
            }
        }

        public void WriteEnumeration<T>(StreamWriter writer, IEnumerable<T> triple)
        {
            writer.WriteLine("<pre>");
            foreach (var t in triple)
            {
                string ts = t.ToString();
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
        public void rdfRemoteEndpointToKB(string endpointURI, string graphKBName, string query, string assert)
        {
            //Define a remote endpoint
            //Use the DBPedia SPARQL endpoint with the default Graph set to DBPedia
            SparqlRemoteEndpoint endpoint = new SparqlRemoteEndpoint(new Uri(endpointURI));

            var gwd = MakeRepositoryKB(graphKBName);
            ICollection<Triple> neededTriples = new List<Triple>();
            string miniMt = "";

            //Use the extension method ExecuteQuery() to make the query against the Graph
            try
            {
                //Object results = g.ExecuteQuery(query);
                //Make a SELECT query against the Endpoint
                SparqlResultSet results = endpoint.QueryWithResultSet(query);
                if (results is SparqlResultSet)
                {
                    //SELECT/ASK queries give a SparqlResultSet
                    SparqlResultSet rset = (SparqlResultSet)results;
                    foreach (SparqlResult r in rset)
                    {
                        //Do whatever you want with each Result
                        Console.WriteLine("SparqlResult.Count = {0}", r.Count);
                        Console.WriteLine("SparqlResult:{0}", r.ToString());
                        Dictionary<string, string> outMap = new Dictionary<string, string>();
                        outMap["s"] = "unknonwSubject";
                        outMap["p"] = "unknonwPredicate";
                        outMap["o"] = "unknonwObject";

                        foreach (string vname in r.Variables)
                        {
                            INode value = r[vname];
                            string strVal = gwd.RdfToPart(value, neededTriples).ToPLStringReadable();
                            Console.WriteLine("BIND: {0} = {1}", vname, strVal);
                            outMap[vname] = strVal;
                        }
                        var assertIt = assert ?? "triple(?s,?p,?o)";
                        foreach (KeyValuePair<string, string> map in outMap)
                        {
                            assertIt = assertIt.Replace("?" + map.Key, SIProlog.MakeReadbleString(map.Value));
                        }
                        miniMt += String.Format("{0}.\n", assertIt);
                    }
                }
                else if (results is IGraph)
                {
                    /*
                    //CONSTRUCT/DESCRIBE queries give a IGraph
                    IGraph resGraph = (IGraph)results;
                    foreach (Triple t in resGraph.Triples)
                    {
                        //Do whatever you want with each Triple
                        Console.WriteLine("TRIPLE_IG: triple(\"{0}\",\"{1}\",\"{2}\").", t.Subject.ToString(), t.Predicate.ToString(), t.Object.ToString());

                    }
                     */
                }
                else
                {
                    //If you don't get a SparqlResutlSet or IGraph something went wrong 
                    //but didn't throw an exception so you should handle it here
                    Console.WriteLine("ERROR");
                }
            }
            catch (RdfQueryException queryEx)
            {
                //There was an error executing the query so handle it here
                Console.WriteLine(queryEx.Message);
            }
            insertKB(miniMt, graphKBName);
        }

        public void rdfImportToKB(IGraph g, string graphKBName, string query)
        {
            string miniMt = "";

            //Use the extension method ExecuteQuery() to make the query against the Graph
            try
            {
                Object results = g.ExecuteQuery(query);
                if (results is SparqlResultSet)
                {
                    //SELECT/ASK queries give a SparqlResultSet
                    SparqlResultSet rset = (SparqlResultSet)results;
                    foreach (SparqlResult r in rset)
                    {
                        //Do whatever you want with each Result
                        Console.WriteLine("SparqlResult.Count = {0}", r.Count);
                        Console.WriteLine("SparqlResult:{0}", r.ToString());
                        foreach (string vname in r.Variables)
                        {
                            INode value = r[vname];
                            string strVal = value.ToString();
                            Console.WriteLine("BIND: {0} = {1}", vname, strVal);
                        }
                        miniMt += String.Format("triple(\"{0}\",\"{1}\",\"{2}\").\n", r["s"].ToString(), r["p"].ToString(), r["o"].ToString());
                    }
                }
                else if (results is IGraph)
                {
                    //CONSTRUCT/DESCRIBE queries give a IGraph
                    IGraph resGraph = (IGraph)results;
                    foreach (Triple t in resGraph.Triples)
                    {
                        //Do whatever you want with each Triple
                        Console.WriteLine("TRIPLE_IG: triple(\"{0}\",\"{1}\",\"{2}\").", t.Subject.ToString(), t.Predicate.ToString(), t.Object.ToString());

                    }
                }
                else
                {
                    //If you don't get a SparqlResutlSet or IGraph something went wrong 
                    //but didn't throw an exception so you should handle it here
                    Console.WriteLine("ERROR");
                }
            }
            catch (RdfQueryException queryEx)
            {
                //There was an error executing the query so handle it here
                Console.WriteLine(queryEx.Message);
            }
            insertKB(miniMt, graphKBName);
        }
        public void pushRulesToGraph(string mt, GraphWithDef rdfGraphWithDefs)
        {
            // Possibly called by the Sparql endpoint before servicing a query
            // Is there anything we want to update rdfGraph with ?
            List<Dictionary<string, string>> bingingsList = new List<Dictionary<string, string>>();
            askQuery("triple(S,P,O)", mt, out bingingsList);
            bool useTripeQuery = true;
            if (bingingsList == null || bingingsList.Count <= 0)
            {
                useTripeQuery = false;
                var rules = findVisibleKBRulesSorted(mt);

                foreach (Rule rule in rules)
                {
                    rdfGraphWithDefs.AddRule(rule);
                }
            }

            var rdfGraph = rdfGraphWithDefs.rdfGraph;

            if (!useTripeQuery) return;
            StringParser.Parse(rdfGraph, "@prefix robokind: <http://localhost/onto#> .");
            foreach (Dictionary<string, string> bindings in bingingsList)
            {
                //foreach (string k in bindings.Keys)
                //{
                rdfGraphAssert(rdfGraph, new Triple(rdfGraphWithDefs.C(bindings["S"]), rdfGraphWithDefs.C(bindings["P"]),
                                           rdfGraphWithDefs.C(bindings["O"])));
                //string rdfLine = String.Format(@"<{0}> <{1}> <{2}> .", bindings["S"].ToString(), bindings["P"].ToString(), bindings["O"].ToString());
                //StringParser.Parse(rdfGraph, rdfLine);
                // }
            }

        }

        static public bool rdfGraphAssert(IGraph rdfGraph, Triple triple)
        {
            lock (rdfGraph)
            {
                if (rdfGraph.ContainsTriple(triple)) return false;
                rdfGraph.Assert(triple);
                return true;
            }
        }

        public void refreshRDFGraphOLD()
        {
            var rdfGraph = getRefreshedRDFGraph("rdfMT");
            // Possibly called by the Sparql endpoint before servicing a query
            // Is there anything we want to update rdfGraph with ?
            List<Dictionary<string, string>> bingingsList = new List<Dictionary<string, string>>();
            askQuery("triple(S,P,O)", "spindleMT", out bingingsList);
            StringParser.Parse(rdfGraph, "@prefix robokind: <http://localhost/onto#> .");
            foreach (Dictionary<string, string> bindings in bingingsList)
            {
                //foreach (string k in bindings.Keys)
                //{
                string rdfLine = String.Format(@"<robokind:{0}> <robokind:{1}> <robokind:{2}> .", bindings["S"].ToString(), bindings["P"].ToString(), bindings["O"].ToString());
                StringParser.Parse(rdfGraph, rdfLine);
                // }
            }

        }
        public IGraph getRefreshedRDFGraph(string queryMT)
        {
            GraphWithDef graph = MakeRepositoryKB(queryMT);
            pushRulesToGraph(queryMT, graph);
            return graph.rdfGraph;
        }

        public void mtest()
        {
            IGraph g = new Graph();

            IUriNode dotNetRDF = g.CreateUriNode(UriFactory.Create("http://www.dotnetrdf.org"));
            IUriNode says = g.CreateUriNode(UriFactory.Create("http://example.org/says"));
            ILiteralNode helloWorld = g.CreateLiteralNode("Hello World");
            ILiteralNode bonjourMonde = g.CreateLiteralNode("Bonjour tout le Monde", "fr");

            rdfGraphAssert(g, new Triple(dotNetRDF, says, helloWorld));
            rdfGraphAssert(g, new Triple(dotNetRDF, says, bonjourMonde));

            foreach (Triple t in g.Triples)
            {
                Console.WriteLine(t.ToString());
                Console.WriteLine("TRIPLE: triple(\"{0}\",\"{1}\",\"{2}\").", t.Subject.ToString(), t.Predicate.ToString(), t.Object.ToString());
            }

            NTriplesWriter ntwriter = new NTriplesWriter();
            ntwriter.Save(g, "HelloWorld.nt");

            RdfXmlWriter rdfxmlwriter = new RdfXmlWriter();
            rdfxmlwriter.Save(g, "HelloWorld.rdf");

            rdfImportToKB(g, "testRDF", "SELECT * WHERE { ?s ?p ?o }");
            rdfRemoteEndpointToKB("http://dbpedia.org/sparql", "dbpediaKB",
                                  "SELECT DISTINCT ?o WHERE { ?s a ?o } LIMIT 100",
                                  "isa(?o,\"http://www.w3.org/2002/07/owl#Class\")");
            rdfRemoteEndpointToKB("http://dbpedia.org/sparql", "dbpediaKB2", "SELECT * WHERE { ?s ?p ?o } LIMIT 1000",
                                  null);

        }
        #endregion

        public partial class Rule
        {
            public INode instanceTriple;
        }

        public class PredicateProperty
        {
            public string name;
            public int arity;
            public string classname;
            public string keyname;
            public int instanceNumber = 1;
            public readonly Dictionary<int, ArgType> argDefs;
            public ILiteralNode classNode;
            public string assertionMt;
            public ICollection<Triple> inations = new List<Triple>();

            public override string ToString()
            {
                return StructToString(this, 2);
            }

            public PredicateProperty(int arity1)
            {
                arity = arity1;
                argDefs = new Dictionary<int, ArgType>();
            }


        }

        public class ArgType
        {
            public string classname;
            public INode predicateNode;
            public List<string> subNames = new List<string>();
            public string assertionMt;
            public INode GetRefNode(IGraph def)
            {
                return def.CreateLiteralNode(classname);
            }
            public override string ToString()
            {
                return StructToString(this, 2);
            }
            public void AddDomainType(PredicateProperty property)
            {
               // throw new NotImplementedException();
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

            public List<PredicateProperty> localPreds = new List<PredicateProperty>();
            public List<ArgType> localArgTypes = new List<ArgType>();
            public List<Term> localPredInstances = new List<Term>();
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
                definations = defs;
            }

            private PredicateProperty AddDefs(Rule rule)
            {
                PredicateProperty headPP = GetPredicateProperty(rule.head);;
                if (rule.body != null)
                {
                    foreach (var p in rule.body.plist.list.ToList())
                    {
                        if (!(p is Term)) continue;
                        GetPredicateProperty((Term)p);
                    }
                }
                return headPP;
            }

            public PredicateProperty GetPredicateProperty(Term term)
            {
                string predName = term.name;
                int arity = term.partlist.list.Count;
                string key = predName + "_" + arity;
                PredicateProperty def;
                bool newlyCreated = false;
                lock (SharedGlobalPredDefs)
                {
                    if (!SharedGlobalPredDefs.TryGetValue(key, out def))
                    {
                        newlyCreated = true;
                        string predClassName = key + "_PredClass";
                        SharedGlobalPredDefs[key] =
                            def = new PredicateProperty(arity) {name = predName, keyname = key, classname = predClassName};
                    }
                    //if (newlyCreated)
                    {
                        var classNode = def.classNode = definations.CreateLiteralNode(def.classname);
                        def.inations.Add(new Triple(classNode, InstanceOf, PrologPredicateClass));
                        for (int i = 0; i < arity; i++)
                        {
                            string argtypename = key + "_Arg" + (1 + i);
                            ArgType adef;
                            lock (SharedGlobalArgTypeDefs)
                            {
                                if (!SharedGlobalArgTypeDefs.TryGetValue(argtypename, out adef))
                                {
                                    adef = SharedGlobalArgTypeDefs[argtypename] = new ArgType() { classname = argtypename };
                                }
                            }
                            def.argDefs[i] = adef;
                            var subj = adef.predicateNode = adef.GetRefNode(definations);
                            adef.AddDomainType(def);
                            def.inations.Add(new Triple(subj, InstanceOf, PrologPredicate));
                            def.inations.Add(new Triple(subj, C("rdfs:domain"), classNode));
                            def.inations.Add(new Triple(subj, C("rdfs:range"), C("rdfs:Literal")));
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
                        rdfGraphAssert(definations, t);
                    }
                }
                return def;
            }

            public INode C(string p)
            {
                return definations.CreateLiteralNode(p);
            }

            protected INode InstanceOf
            {
                get { return C("a"); }
            }
            protected INode PrologPredicate
            {
                get { return C("rdf:Property"); }
            }
            protected INode PrologPredicateClass
            {
                get { return C("rdf:Class"); }
            }

            public void AddRule(Rule rule)
            {
                var headDef = AddDefs(rule);
                AddData(rule, headDef);
            }

            private void AddData(Rule rule, PredicateProperty headDef)
            {
                if (rule.instanceTriple != null) return;
                ICollection<Triple> headtriples = new List<Triple>();
                lock (rule)
                {
                    if (rule.body == null)
                    {
                        rule.instanceTriple = CreateSubject(rule.head, headDef, headtriples, false);
                        foreach (Triple t in headtriples)
                        {
                            rdfGraphAssert(rdfGraph, t);
                        }
                        return;
                    }
                }
                ICollection<Triple> bodytriples = new List<Triple>();
                rule.instanceTriple = CreateSubject(rule.head, headDef, headtriples, true);
                foreach (Part p in rule.body.plist.ArgList.ToList())
                {
                    var t = PartToRdf(p, bodytriples);

                }
                Triple trule = CreateImplication(bodytriples, headtriples);
                rdfGraphAssert(rdfGraph, trule);
            }

            private Triple CreateImplication(ICollection<Triple> bodytriples, ICollection<Triple> headtriples)
            {
                return new Triple(ToBracket(bodytriples), definations.CreateLiteralNode("log:implies"),
                                  ToBracket(headtriples));
            }

            private INode ToBracket(ICollection<Triple> bodytriples)
            {
                Graph subgraph = new Graph();
                foreach (Triple triple in bodytriples)
                {
                    subgraph.Assert(triple);
                }
                var group = definations.CreateGraphLiteralNode(subgraph);
                return group;
            }

            private INode CreateSubject(Term term, PredicateProperty headDefOrNull, ICollection<Triple> triples, bool isVar)
            {
                var headDef = headDefOrNull ?? GetPredicateProperty(term);
                INode subj = CreateInstance(headDef, triples, isVar);
                AddTriplesSubject(headDef, term, triples, subj);
                return subj;
            }

            private void AddTriplesSubject(PredicateProperty headDef, Term term, ICollection<Triple> list, INode subj)
            {
                int argNum = 0;
                foreach (Part part in term.Args)
                {
                    INode obj = PartToRdf(part, list);
                    INode pred = headDef.argDefs[argNum].GetRefNode(definations);
                    list.Add(new Triple(subj, pred, obj));
                    argNum++;
                }
            }

            private INode CreateAntecedantNode(Term term, ICollection<Triple> triples)
            {
                PredicateProperty pp = GetPredicateProperty(term);
                return CreateSubject(term, pp, triples, true);
            }

            private INode CreateInstance(PredicateProperty headDef, ICollection<Triple> graph, bool isVar)
            {
                int nxt = headDef.instanceNumber++;
                string iname = headDef.keyname + "_PredInst" + nxt;
                INode iln = isVar ? definations.CreateVariableNode(iname) : (INode)definations.CreateLiteralNode(iname);
                var a = InstanceOf;
                var cn = definations.CreateLiteralNode(headDef.classname);
                graph.Add(new Triple(iln, a, cn));
                return iln;
            }

            private INode PartToRdf(Part part, ICollection<Triple> triples)
            {
                if (part is Atom)
                {
                    return C(part.name);
                }
                if (part is Variable)
                {
                    return definations.CreateVariableNode(part.name);
                }
                if (part is Term)
                {
                    return CreateAntecedantNode((Term)part, triples);
                }
                throw new NotImplementedException("ToRDF on " + part);
            }

            public Part RdfToPart(INode node, ICollection<Triple> triples)
            {
                if (node is StringNode)
                {
                    return new Atom(node.ToString());
                } 
                if (node is UriNode)
                {
                    return new Atom(node.ToString());
                }
                throw new NotImplementedException("ToProlog on " + node);
            }

            public void pushRulesToGraph()
            {
                prologEngine.pushRulesToGraph(prologMt, this);
            }

            public void pushGraphToKB()
            {               
                prologEngine.pushRulesToGraph(prologMt, this);
            }

            public BaseTripleCollection Triples
            {
                get { return rdfGraph.Triples; }
            }
        }
    }
}