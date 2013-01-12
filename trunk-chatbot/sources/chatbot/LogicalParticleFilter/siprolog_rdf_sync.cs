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
    public partial class SIProlog
    {
        public void rdfRemoteEndpointToKB(string endpointURI, string graphKBName, string query, string assertTemplate)
        {
            //Define a remote endpoint
            //Use the DBPedia SPARQL endpoint with the default Graph set to DBPedia
            SparqlRemoteEndpoint endpoint = new SparqlRemoteEndpoint(new Uri(endpointURI));

            var focus = FindOrCreateKB(graphKBName);
            RdfRules ruleDefs = new RdfRules(focus.RdfStore.rdfGraph);

            StringWriter miniMt = new StringWriter();

            //Use the extension method ExecuteQuery() to make the query against the Graph
            try
            {
                //Object results = g.ExecuteQuery(query);
                //Make a SELECT query against the Endpoint
                SparqlResultSet results = endpoint.QueryWithResultSet(query);
                GetMiniMt(results, assertTemplate, graphKBName, focus.RdfStore, show, null, miniMt, ruleDefs);
            }
            catch (RdfQueryException queryEx)
            {
                //There was an error executing the query so handle it here
                Warn("While endpointURI={0}\n\n{1}", endpointURI, queryEx);
            }
            insertKB(miniMt.ToString(), graphKBName);
        }

        public void rdfImportToKB(IGraph g, string graphKBName, string query, string assertTemplate)
        {
            EnsureReaderNamespaces(g);
            StringWriter miniMt = new StringWriter();
            var repo = FindOrCreateKB(graphKBName);
            RdfRules ruleDefs = new RdfRules(g);
            //Use the extension method ExecuteQuery() to make the query against the Graph
            try
            {
                Object results = g.ExecuteQuery(query);
                List<Rule> newTriples = new List<Rule>();
                GetMiniMt(results, assertTemplate, graphKBName, repo.RdfStore, show, null, miniMt, ruleDefs);
            }
            catch (RdfQueryException queryEx)
            {
                //There was an error executing the query so handle it here
                Warn(queryEx);
            }
            string miniMtSources = miniMt.ToString();
            insertKB(miniMtSources, graphKBName);
        }

        static private void GetMiniMt(object results, string assertTemplate, string graphKBName, PNode repo, bool show, ICollection<Rule> newRules, TextWriter ruleSources, RdfRules ruleDefs)
        {
            assertTemplate = assertTemplate ?? "triple($?s$,$?p$,$?o$).\n";
            bool MakeRules = newRules != null && assertTemplate.Trim().EndsWith(".");
            var outMap = new Dictionary<string, string>();
            outMap["s"] = "unknownSubject";
            outMap["p"] = "unknownPredicate";
            outMap["o"] = "unknownObject";
            outMap["mt"] = GraphWithDef.PlReadble(repo.definations.CreateUriNode(repo.rdfGraph.BaseUri), ruleDefs);

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
                        string strVal = GraphWithDef.PlReadble(value, ruleDefs);
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
                        Rule rule = CurrentProlog.ParseRule(new Tokeniser(assertIt), graphKBName);
                        if (show) ConsoleWriteLine("RULE_IG: {0}", rule);
                        newRules.Add(rule);
                    }
                    else
                    {
                        if (show) ConsoleWriteLine("TRIPLE_IG: {0}", assertIt);
                    }
                    if (ruleSources != null) ruleSources.WriteLine(assertIt);
                }
            }
            else if (results is IGraph)
            {
                //CONSTRUCT/DESCRIBE queries give a IGraph
                IGraph resGraph = (IGraph)results;
                var rset = resGraph.Triples;
                outMap["mt"] =
                    GraphWithDef.PlReadble(GraphWithDef.CUrlNode(repo.definations, resGraph.BaseUri.AbsoluteUri, false, false),
                                   ruleDefs);
                if (show)
                {
                    ConsoleWriteLine("IGraphResultSet.Count = {0}", rset.Count);
                    ConsoleWriteLine("IGraphResultSet:{0}", rset.ToString());
                }
                foreach (Triple t in rset)
                {
                    var assertIt = assertTemplate;
                    //Do whatever you want with each Triple
                    outMap["s"] = GraphWithDef.PlReadble(t.Subject, ruleDefs);
                    outMap["p"] = GraphWithDef.PlReadble(t.Predicate, ruleDefs);
                    outMap["o"] = GraphWithDef.PlReadble(t.Object, ruleDefs);
                    foreach (KeyValuePair<string, string> map in outMap)
                    {
                        assertIt = assertIt.Replace("$?" + map.Key + "$", map.Value);
                    }
                    if (MakeRules)
                    {
                        Rule rule = CurrentProlog.ParseRule(new Tokeniser(assertIt), graphKBName);
                        if (show) ConsoleWriteLine("RULE_IG: {0}", rule);
                        newRules.Add(rule);
                    }
                    else
                    {
                        if (show) ConsoleWriteLine("TRIPLE_IG: {0}", assertIt);
                    }
                    if (ruleSources != null) ruleSources.WriteLine(assertIt);
                }
            }
            else
            {
                //If you don't get a SparqlResutlSet or IGraph something went wrong 
                //but didn't throw an exception so you should handle it here
                if (results == null) throw ErrorBadOp("ERROR: no Results From NULL Query Object for " + graphKBName);
                throw ErrorBadOp("ERROR: Cant how understand " + results.GetType() + " " + results + " to import to " +
                                 graphKBName);
            }
        }


        public void pushRulesToGraph(string mt, PNode rdfGraphWithDefs, bool includeInherited)
        {
            // Possibly called by the Sparql endpoint before servicing a query
            // Is there anything we want to update rdfGraph with ?
            PNode focus = FindOrCreateKB(mt);
            var bingingsList = new ListOfBindings();
            askQuery(ParseBody("triple(S,P,O)", mt), mt, includeInherited, bingingsList, null);
            bool useTripeQuery = true;
            if (bingingsList == null || bingingsList.Count <= 0)
            {
                useTripeQuery = false;
                var triples = findVisibleKBRules(mt, new List<string>(), includeInherited);
                foreach (Rule rule in triples)
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
                return;
            }

            var rdfGraph = rdfGraphWithDefs.rdfGraph;

            if (!useTripeQuery) return;
            var newTriples = new RdfRules(rdfGraphWithDefs.definations);
            EnsureGraphPrefixes(rdfGraph);
            foreach (Dictionary<string, Part> bindings in bingingsList)
            {
                Part psubj = bindings["S"];
                Part ppred = bindings["P"];
                Part pobj = bindings["O"];
                INode subj = GraphWithDef.PartToRdf(psubj, newTriples);
                INode pred = GraphWithDef.PartToRdf(ppred, newTriples);
                INode obj = GraphWithDef.PartToRdf(pobj, newTriples);
                if (subj is ILiteralNode)
                {
                    Warn("Subj was a literal (not supported) [{0} {1} {2}]", subj, pred, obj);
                    continue;
                }
                if (!(pred is IUriNode))
                {
                    Warn("Pred was not a uri (not supported) [{0} {1} {2}]", subj, pred, obj);
                    continue;
                }
                //foreach (string k in bindings.Keys)
                //{
                rdfGraphAssert(rdfGraph, MakeTriple(subj, pred, obj));
                //string rdfLine = String.Format(@"<{0}> <{1}> <{2}> .", bindings["S"].ToString(), bindings["P"].ToString(), bindings["O"].ToString());
                //StringParser.Parse(rdfGraph, rdfLine);
                // }
            }
            newTriples.RequirementsMet = true;
            newTriples.AssertTriples(rdfGraphWithDefs.definations, true, true);
        }
        public void refreshRDFGraphOLD()
        {
            var rdfGraphWithDefs = FindOrCreateKB("rdfMT");
            var rdfGraph = rdfGraphWithDefs.RdfStore.rdfGraph;
            EnsureGraphPrefixes(rdfGraph);
            // Possibly called by the Sparql endpoint before servicing a query
            // Is there anything we want to update rdfGraph with ?
            var bingingsList = new ListOfBindings();
            string mt = "spindleMT";
            askQuery(ParseBody("triple(S,P,O)", mt), mt, true, bingingsList, null);
            RdfRules newTriples = new RdfRules(rdfGraph);
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
            newTriples.AssertTriples(rdfGraphWithDefs.definations, true, true);
        }
        public IGraph getRefreshedRDFGraph(string queryMT)
        {
            var graph = FindOrCreateKB(queryMT);
            ensureCompiled(graph, ContentBackingStore.RdfMemory);
            return graph.RdfStore.rdfGraph;
        }


        public void mtest()
        {

            IGraph g = new Graph();
            g.BaseUri = UriFactory.Create(RoboKindURI);

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

            FindOrCreateKB("testRDF").SourceKind = ContentBackingStore.Prolog;
            if (RdfDeveloperSanityChecks < 2) return;
            rdfImportToKB(g,
                          "testRDF",
                          "SELECT * WHERE { ?s ?p ?o }",
                          null);
            foreach (var nameAndEndp in
                new[]
                    {
                        //new[] {"http://budapest.rkbexplorer.com/sparql"},
                        new[] {"dbpedia", "http://dbpedia.org/sparql"},
                        //   new[] {"josekiBooks", "http://cogbotserver:2020"},
                        //  new[] {"cogPoint", "http://cogbotserver:8181"},
                        //new[] {"hebis", "http://lod.hebis.de/sparql"},
                    })
            {
                string prefix = nameAndEndp[0];
                string endp = nameAndEndp[1];
                CreateTestTriangle(prefix, endp);
            }
            return;
        }

        private void CreateTestTriangle(string prefix, string endp)
        {
            var prolog100Mt = prefix + "Prolog100KB";
            PNode kb1 = FindOrCreateKB(prolog100Mt);
            kb1.SourceKind = ContentBackingStore.Prolog;
            rdfRemoteEndpointToKB(endp,
                                  prolog100Mt,
                                  "SELECT DISTINCT ?o WHERE { ?s a ?o } LIMIT 100",
                                  "isa($?o$,{http://www.w3.org/2002/07/owl#Class}).\n");

            PNode kb2 = FindOrCreateKB(prefix + "RdfServerURI");
            kb2.SourceKind = ContentBackingStore.RdfServerURI;
            kb2.Repository = endp;
            PNode kb3 = FindOrCreateKB(prefix + "RdfMemory");
            kb3.SourceKind = ContentBackingStore.RdfMemory;
            kb3.Repository = endp;
            testKBConsitancy(kb1);
            testKBConsitancy(kb2);
            testKBConsitancy(kb3);
        }

        private void testKBConsitancy(PNode node)
        {
            TextWriter sw = DLRConsole.Out;
            WriteMtInfo(sw, node.Id, "" + node.RdfStore.rdfGraph.BaseUri, false);
        }

    }
}

  