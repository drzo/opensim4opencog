using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading;
using LogicalParticleFilter1;
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
        public IGraph rdfGraph;

        public ServitorEndpoint(AltBot _bot, Servitor _servitor, SIProlog _prologEngine)
        {
            curBot = _bot;
            servitor = _servitor;
            prologEngine = _prologEngine;
            rdfGraph = prologEngine.rdfGraph;
        }

        public void StartServer()
        {
            Console.WriteLine("starting testRDFServer");
            myServer = new PFEndpoint();
            StringParser.Parse(prologEngine.rdfGraph, "<http://example.org/a1> <http://example.org/b1> <http://example.org/c1> .");
            prologEngine.connectMT("spindleMT", "rdfMT");
            prologEngine.appendKB("triple(this,can,work).\n", "rdfMT");

            myServer.beginService(prologEngine);
        }
    }
}
