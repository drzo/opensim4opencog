using System;
using System.Collections;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Net;
using System.Text;
using System.Threading;
using LogicalParticleFilter1;
using MushDLR223.Utilities;
using RTParser;
using VDS.RDF;
using VDS.RDF.Parsing;

namespace AltAIMLbot
{
    // NOTES:
    // In debug mode must start MSVC# as admin
    // Should clear RDF service port (8181) with firewall
    // Can test using dotnet rdf toolkit soh command line query tool:
    //   soh query --service http://localhost:8181/query "SELECT * where {?s ?p ?o} LIMIT 100"

    public class ServitorEndpoint
    {
        public AltBot curBot;
        public Servitor servitor;
        public SIProlog prologEngine;
        public PFEndpoint myServer;
        //public IGraph rdfGraph;

        public ServitorEndpoint(AltBot _bot, Servitor _servitor, SIProlog _prologEngine)
        {
            curBot = _bot;
            servitor = _servitor;
            prologEngine = _prologEngine;
            //rdfGraph = prologEngine.rdfGraph;
        }

        public void StartServer()
        {
            if (myServer != null) return;
            Console.WriteLine("starting testRDFServer");
            myServer = new PFEndpoint();
            //StringParser.Parse(prologEngine.rdfGraph, "<http://example.org/a1> <http://example.org/b1> <http://example.org/c1> .");
            prologEngine.connectMT("spindleMT", "rdfMT");
            prologEngine.connectMT("rdfMT", "baseKB");
            prologEngine.appendKB("triple(this,can,work).\n", "rdfMT");

            myServer.beginService(prologEngine);
        }

        public void webWriter(HttpListenerContext context, TextWriter writer, string action, string query, string path, string mt, string serverRoot)
        {
            writer = WebLinksWriter.AddWarnWriter(writer);
            mt = mt ?? "rdfMT";
            BeginsWith("./xrdf/", ref path);
            SIProlog.PNode graph = prologEngine.MakeRepositoryKB(mt);
            if (query == "rdf2pl")
            {                
                graph.pushRdfGraphToPrologKB(true);
            }
            if (query == "pl2rdf")
            {
                graph.pushPrologKBToRdfGraph(true);
            }
            if (query == "syncfromremote")
            {
                graph.populateRDFMemoryFromRepository();
            }
            if (graph != null)
            {
                graph.RdfStore.prologEngine.webWriter(writer, null, null, graph.id, serverRoot);
            }
            WebLinksWriter.RemoveWarnWriter(writer);
            return;
        }



        private bool BeginsWith(string value, ref string path)
        {
            if (path.StartsWith(value))
            {
                path = path.Substring(value.Length);
                return true;
            }
            return false;
        }

    }
}
