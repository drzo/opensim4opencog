using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading;
using VDS.RDF.Parsing;

namespace LogicalParticleFilter1
{
    // NOTES:
    // In debug mode must start MSVC# as admin
    // Should clear RDF service port (8181) with firewall
    // Can test using dotnet rdf toolkit soh command line query tool:
    //   soh query --service http://localhost:8181/query "SELECT * where {?s ?p ?o} LIMIT 100"

    class Program
    {
       static SymbolicParticleFilter ourFilter = new SymbolicParticleFilter();
        public static SIProlog prologEngine;

       // test case from http://mason.gmu.edu/~gcaldero/docs/CS687_P4_bayes.lisp
        //
        static private bool w1Loaded = false;

        static void world1()
        {
            if (w1Loaded) return;
            w1Loaded = true;
            ourFilter.prototype.variables.Add("in(r0)", 0.8);
            ourFilter.prototype.variables.Add("in(r1)", 0.05);
            ourFilter.prototype.variables.Add("in(r2)", 0.05);
            ourFilter.prototype.variables.Add("in(r3)", 0.05);
            ourFilter.prototype.variables.Add("in(r4)", 0.05);

            ourFilter.constraintSet.Add("in(r0)|in(r1)|in(r2)|in(r3)|in(r4)");
            ourFilter.addStateSenseProb("in(r0)|sense(even)=0.95");
            ourFilter.addStateSenseProb("in(r0)|sense(odd)=0.05");

            ourFilter.addStateSenseProb("in(r1)|sense(even)=0.05");
            ourFilter.addStateSenseProb("in(r1)|sense(odd)=0.95");

            ourFilter.addStateSenseProb("in(r2)|sense(even)=0.95");
            ourFilter.addStateSenseProb("in(r2)|sense(odd)=0.05");

            ourFilter.addStateSenseProb("in(r3)|sense(even)=0.05");
            ourFilter.addStateSenseProb("in(r3)|sense(odd)=0.95");

            ourFilter.addStateSenseProb("in(r4)|sense(even)=0.95");
            ourFilter.addStateSenseProb("in(r4)|sense(odd)=0.05");

            ourFilter.addStateActTransition("in(r0)|act(forward)=0.5:in(r0)");
            ourFilter.addStateActTransition("in(r0)|act(forward)=0.5:in(r1)");
            ourFilter.addStateActTransition("in(r0)|act(backward)=0.5:in(r0)");
            ourFilter.addStateActTransition("in(r0)|act(backward)=0.5:in(r4)");

            ourFilter.addStateActTransition("in(r1)|act(forward)=0.5:in(r1)");
            ourFilter.addStateActTransition("in(r1)|act(forward)=0.5:in(r2)");
            ourFilter.addStateActTransition("in(r1)|act(backward)=0.5:in(r1)");
            ourFilter.addStateActTransition("in(r1)|act(backward)=0.5:in(r0)");

            ourFilter.addStateActTransition("in(r2)|act(forward)=0.5:in(r2)");
            ourFilter.addStateActTransition("in(r2)|act(forward)=0.5:in(r3)");
            ourFilter.addStateActTransition("in(r2)|act(backward)=0.5:in(r2)");
            ourFilter.addStateActTransition("in(r2)|act(backward)=0.5:in(r1)");

            ourFilter.addStateActTransition("in(r3)|act(forward)=0.5:in(r3)");
            ourFilter.addStateActTransition("in(r3)|act(forward)=0.5:in(r4)");
            ourFilter.addStateActTransition("in(r3)|act(backward)=0.5:in(r3)");
            ourFilter.addStateActTransition("in(r3)|act(backward)=0.5:in(r2)");

            ourFilter.addStateActTransition("in(r4)|act(forward)=0.5:in(r4)");
            ourFilter.addStateActTransition("in(r4)|act(forward)=0.5:in(r0)");
            ourFilter.addStateActTransition("in(r4)|act(backward)=0.5:in(r4)");
            ourFilter.addStateActTransition("in(r4)|act(backward)=0.5:in(r3)");
        }

        static void p1()
        {
            world1();
            // test1
            ourFilter.quickFilter("act(forward)", "sense(odd)");
            ourFilter.quickFilter("act(forward)", "sense(even)");
            ourFilter.quickFilter("act(forward)", "sense(odd)");
            ourFilter.quickFilter("act(forward)", "sense(even)");
            ourFilter.quickFilter("act(forward)", "sense(even)");
            ourFilter.quickFilter("act(forward)", "sense(odd)");
            ourFilter.quickFilter("act(forward)", "sense(even)");
            ourFilter.quickFilter("act(forward)", "sense(odd)");

            ourFilter.quickFilter("act(backward)", "sense(even)");
            ourFilter.quickFilter("act(backward)", "sense(odd)");
            ourFilter.quickFilter("act(backward)", "sense(even)");
            ourFilter.quickFilter("act(backward)", "sense(even)");
            ourFilter.quickFilter("act(backward)", "sense(odd)");
            ourFilter.quickFilter("act(backward)", "sense(even)");
            ourFilter.quickFilter("act(backward)", "sense(odd)");
            ourFilter.quickFilter("act(backward)", "sense(even)");

        }
        static void p2()
        {
            world1();
            //test2
            ourFilter.quickFilter("act(forward)", "sense(even)");
            ourFilter.quickFilter("act(forward)", "sense(odd)");
            ourFilter.quickFilter("act(backward)", "sense(even)");
            ourFilter.quickFilter("act(backward)", "sense(even)");
            ourFilter.quickFilter("act(backward)", "sense(odd)");
        }
        static void p3()
        {
            world1();
            //test2b
            ourFilter.quickFilter("act(forward)", "sense(even)");
            ourFilter.quickFilter("act(forward)", "sense(odd)");
            ourFilter.quickFilter("act(forward)", "sense(even)");
            ourFilter.quickFilter("act(forward)", "sense(odd)");
            ourFilter.quickFilter("act(forward)", "sense(odd)");
            ourFilter.quickFilter("act(forward)", "sense(even)");
            ourFilter.quickFilter("act(forward)", "sense(odd)");
            ourFilter.quickFilter("act(forward)", "sense(even)");
            ourFilter.quickFilter("act(forward)", "sense(odd)");
            ourFilter.quickFilter("act(forward)", "sense(odd)");
            ourFilter.quickFilter("act(forward)", "sense(even)");
            ourFilter.quickFilter("act(forward)", "sense(even)");
            ourFilter.quickFilter("act(forward)", "sense(even)");
            ourFilter.quickFilter("act(forward)", "sense(even)");
        }
        static void testProlog1()
        {            
            // can it do the prolog hello world of logic
            prologEngine.testruleset = "isa(boy,human).\n isa(human,mammal).\n isa(X,Y):-isa(X,A),isa(A,Y).";
            prologEngine.testquery = "isa(boy,WHAT)\n";
            prologEngine.parseRuleset();
            prologEngine.parseQuery();
        }
        static void testProlog2()
        {
            // can we parse numbers (opaque atoms)
            prologEngine.testruleset = "senseProb(0.5):-state(sitting),act(forward).\n senseProb(0.001).\nstate(sitting).\nact(forward).\n";
            prologEngine.testquery = "senseProb(SP)";
            prologEngine.parseRuleset();
            prologEngine.parseQuery();
        }
        static void testKBload()
        {
            prologEngine.loadKEFile("default.ke");
            prologEngine.loadKEFile("cycUpper.ke");
            prologEngine.loadKEFile("mini-cyc-prolog.ke");
            prologEngine.loadKEFile("occEmotions.ke");

        }
        static void testProlog3()
        {
            // we can  do a graph of connected kb's
            // the query is from a point in the graph and the system gathers the KB
            // Wonder if we "logicRank" the graph in someway so we inference over what's relevant
            // Also what kind of connected graphs can we concoct ?
            // Maybe we want to have prove walk the KB graph?
            // Should the KB be constructed depth-first or breath first ?
            //  (It may not matter so long as the items of a given predicate appear in one MT)
            // probably should have classic optomizations like first term indexing for speed
            // Should work on built-ins. (Done except for eval) (Dmiles did eval)

            prologEngine.connectMT("sense1MT", "baseKB");
            prologEngine.connectMT("sense2MT", "baseKB");
            prologEngine.connectMT("spindleMT", "sense1MT");
            prologEngine.connectMT("spindleMT", "sense2MT");
            prologEngine.insertKB("canSense(X):-sense(X).\n", "baseKB");
            prologEngine.appendKB("genls(A,B):-genls(A,X),genls(X,B).\n", "baseKB");
            prologEngine.appendKB("isa(A,B):-isa(A,X),genls(X,B).\n", "baseKB");

            prologEngine.insertKB("sense(dog).\nsense(cat).\n", "sense1MT");
            prologEngine.insertKB("sense(boy).\nsense(girl).\n", "sense2MT");
            prologEngine.appendKB("isa(john,boy).\ngenls(boy,human).\ngenls(human,mammal).\n", "sense2MT");
            prologEngine.askQuery("canSense(WHAT1)", "sense1MT");
            prologEngine.askQuery("canSense(WHAT2)", "sense2MT");
            prologEngine.askQuery("canSense(WHAT3)", "spindleMT");
            prologEngine.askQuery("isa(john,JOHN_IS)", "spindleMT");
            //prologEngine.KBGraph.PrintToConsole();
        }
        static void testProlog4()
        {
            // Do the particle filter part
            p1();
            ourFilter.defMeanParticle();
            ourFilter.meanParticle.normalize(ourFilter.constraintSet);
            Console.WriteLine("meanP norm:{0}", ourFilter.meanParticle.ToString());
            string particleKB = ourFilter.meanParticle.asDataMt(0.5);

            // Do the logic part
            prologEngine.connectMT("sense1MT", "baseKB");
            prologEngine.connectMT("spindleMT", "sense1MT");
            prologEngine.insertKB( particleKB, "sense1MT");
            prologEngine.insertKB("loc(self,X):-in(X).\n", "baseKB");
            //prologEngine.askQuery("loc(self,SELF_AT)", "spindleMT");
            prologEngine.askQuery("loc(self,SELF_AT),prob(in(SELF_AT),PRB)", "spindleMT");

            List<Dictionary<string, string>> bingingsList = new List<Dictionary<string, string>>();
           // Dictionary<string, string> bindings = new Dictionary<string,string> ();
            prologEngine.askQuery("loc(self,SELF_AT),prob(in(SELF_AT),PRB)", "spindleMT", out bingingsList);
            foreach (Dictionary<string, string> bindings in bingingsList)
            {
                foreach (string k in bindings.Keys)
                {
                    Console.WriteLine("BINDING {0} = {1}", k, bindings[k]);
                }
            }

            prologEngine.askQuery("triple(S,P,O)", "spindleMT", out bingingsList);
            foreach (Dictionary<string, string> bindings in bingingsList)
            {
                foreach (string k in bindings.Keys)
                {
                    Console.WriteLine("BINDING {0} = {1}", k, bindings[k]);
                }
            }

        }
        static void testIDCema(string CemaQuery)
        {
            int testdepth = 4;
            List<Dictionary<string, string>> bingingsList = new List<Dictionary<string, string>>();
            // Dictionary<string, string> bindings = new Dictionary<string,string> ();
            while ((bingingsList.Count == 0)&& (testdepth < 256))
            {
                testdepth = (int) ( testdepth * 1.5);
               // Console.WriteLine("Trying depth {0}", testdepth);
                prologEngine.maxdepth = testdepth;
                prologEngine.askQuery(CemaQuery, "testCema", out bingingsList);
            }
            if (bingingsList.Count == 0)
            {
                Console.WriteLine("No Solution found");
            }
            else
            {
                foreach (Dictionary<string, string> bindings in bingingsList)
                {
                    foreach (string k in bindings.Keys)
                    {
                        string v = bindings[k];
                        Console.WriteLine("BINDING {0} = {1}", k, v);
                        if (k == "ANSMOD")
                        {
                            v = v.Replace("cons", " ");
                            v = v.Replace("(", " ");
                            v = v.Replace(")", " ");
                            v = v.Replace(",", " ");
                            v = v.Replace("  ", " ");
                            Console.WriteLine("ANSMOD = '{1}'", k, v);
                        }
                    }
                }
            }

        }
        static void testCema()
        {
            prologEngine.maxdepth = 128;
            prologEngine.trace = false;
            prologEngine.loadKEFile("nanocema.ke");
           // prologEngine.askQuery("member(X,[a,b,c])", "testCema");
            //prologEngine.trace = true;
            //http://www.csupomona.edu/~jrfisher/www/prolog_tutorial/2_7.html
           // prologEngine.askQuery("perm([1,2,3],P)", "testCema");
           // prologEngine.askQuery("member([3,Y], [[1,a],[2,m],[3,z],[4,v],[3,p]])", "testCema");
           // prologEngine.askQuery("union([1,2,3,4],[1,a,b,4],A)", "testCema");
           // prologEngine.askQuery("intersection([1,2,3,4],[1,a,b,4],B)", "testCema");
           //  prologEngine.askQuery("difference([1,2,3,4],[1,a,b,4],B)", "testCema");
             Console.WriteLine("--------------------------------");
             testIDCema("storm2([salt_h2o],[light],[],ANSIN,ANSOUT,ANSMOD)"); //64
            Console.WriteLine("--------------------------------");
            testIDCema("storm2([co2,light,h2o,fert],[meat],[],ANSIN,ANSOUT,ANSMOD)");
            Console.WriteLine("--------------------------------");
            testIDCema("storm2([light,h2o],[ox,veg],[],ANSIN,ANSOUT,ANSMOD)");
            Console.WriteLine("--------------------------------");
            testIDCema("storm2([light,salt_h2o],[meat],[],ANSIN,ANSOUT,ANSMOD)"); //128
            Console.WriteLine("--------------------------------");
            prologEngine.maxdepth = 164;
            testIDCema("storm2([salt_h2o],[meat],[],ANSIN,ANSOUT,ANSMOD)"); //128
            Console.WriteLine("--------------------------------");
            // Dictionary<string, string> bindings = new Dictionary<string,string> ();
            testIDCema("storm2([salt_h2o],[meat],[],ANSIN,ANSOUT,ANSMOD)");

        }
        static void testSat()
        {
            prologEngine.maxdepth = 32;
            prologEngine.trace = false;
            prologEngine.loadKEFile("nanosat.ke");
            //prologEngine.trace = true;
            prologEngine.askQuery("testSat([X,Y,Z])", "flops_satsolver");
        }
        static void testPrologBuiltins()
        {
            string r = prologEngine.ourEval("2+4;");

            prologEngine.connectMT("baseKB", "stdlib");
            prologEngine.connectMT("sense1MT", "baseKB");
            prologEngine.connectMT("spindleMT", "sense1MT");
            prologEngine.askQuery("add(12,34,ADD_ANS)", "spindleMT");
            prologEngine.askQuery("sub(12,34,SUB_ANS)", "spindleMT");
            prologEngine.askQuery("mul(12,34,MUL_ANS)", "spindleMT");
            prologEngine.askQuery("div(12,34,DIV_ANS)", "spindleMT");
        }
        static void testRDFServer()
        {
            Console.WriteLine("starting testRDFServer");
            PFEndpoint myServer = new PFEndpoint();
            var rdfGraph = prologEngine.getRefreshedRDFGraph("spindleMT");
            StringParser.Parse(rdfGraph, "<http://example.org/a1> <http://example.org/b1> <http://example.org/c1> .");
            prologEngine.connectMT("spindleMT", "rdfMT");
            prologEngine.appendKB("triple(this,can,work).\n", "rdfMT");

            myServer.beginService(prologEngine);
            while (true)
            {
                Thread.Sleep(1000);
            }
        }
        static void Main(string[] args)
        {
            runningTests = null;
            RunAllTests(SIProlog.CurrentProlog);
        }

        static SIProlog runningTests;
        public static void RunAllTests(SIProlog testIt)
        {
            if (runningTests != null)
            {
                SIProlog.Warn("Already started tests");
                return;
            }          
            runningTests = testIt;
            prologEngine = testIt;
            //testProlog4();
            testPrologBuiltins();
            //testRDFServer();
            testCema();
            testSat();
            //return;
            testProlog1();
            testProlog2();
            testKBload();
            testProlog3();
            testPrologBuiltins();
            prologEngine.mtest();
            prologEngine.askQuery("triple(SUBJECT,PRED,OBJ)", "testRDF");
            if (prologEngine.HasKBNamed("dbpediaRdfMemory")) prologEngine.askQuery("triple(SUBJECT,PRED,OBJ)", "dbpediaRdfMemory");

            testProlog4();
            //p1();
            p2();
            p3();

            ourFilter.defMeanParticle();
            //ourFilter.dump();
            Console.WriteLine("meanP raw:{0}", ourFilter.meanParticle.ToString());
            ourFilter.meanParticle.normalize(ourFilter.constraintSet);
            Console.WriteLine("meanP norm:{0}", ourFilter.meanParticle.ToString());
            Console.WriteLine("done");
        }
    }
}
