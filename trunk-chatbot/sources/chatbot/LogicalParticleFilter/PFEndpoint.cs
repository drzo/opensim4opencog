using System;
using System.Collections.Generic;
using System.Collections;
using System.Linq;
using System.Text;
using System.Net;
using System.Net.Sockets;
using System.Reflection;
using System.IO;
using System.Threading;
using System.Xml;
using System.Web;
using System.Web.UI;

using VDS.RDF;
using VDS.RDF.Parsing;
using VDS.RDF.Query;
using VDS.RDF.Writing.Formatting;
using VDS.RDF.Writing;
using VDS.RDF.Update;
using VDS.RDF.Configuration;
using VDS.RDF.Update.Commands;
using VDS.RDF.Web;
using VDS.RDF.Web.Configuration.Server;


namespace LogicalParticleFilter1
{
    public class PFEndpoint
    {
        public  HttpListener listener = new HttpListener();
        public  string startUpPath = null;
        public  SIProlog ourEngine = null;
        static public int serverPort = 8181;
        static public string serverRoot
        {
            get
            {
                return "http://CogbotServer:" + serverPort + "/";
            }
        }
        [ThreadStatic]
        public  SparqlServerConfiguration _config ;
        public Dictionary<string, SparqlServerConfiguration> Configs = new Dictionary<string, SparqlServerConfiguration>();


        public  bool IsMicrosoftCLR()
        {
            return (Type.GetType("Mono.Runtime") == null);
        }

        public  void beginService(SIProlog theEngine)
        {
            startUpPath = startUpPath ??
                          System.IO.Path.GetDirectoryName(
                              (Assembly.GetEntryAssembly() ?? typeof(PFEndpoint).Assembly).Location);
            
            if (!HttpListener.IsSupported)
            {
                Console.WriteLine("***** HttpListener is not supported on this platform. *****");
                return;
            }
            ourEngine = theEngine;
           // _config = new SparqlServerConfiguration(ourEngine.rdfGraph, ourNode);
            //listener.Prefixes.Add("http://192.168.2.141:8123/");
            //listener.Prefixes.Add("http://192.168.0.145:8123/");
            lock (listener)
            {
                string pfadd = "";
                try
                {

                    //listener.Prefixes.Add(serverRoot);
                    //Console.WriteLine("Listener Adding:" + serverRoot);
                }
                catch (Exception e)
                {
                    Console.WriteLine("FAIL Listener Adding:" + serverRoot);
                    Console.WriteLine(e.Message);
                }
                try
                {
                    if (IsMicrosoftCLR())
                    {
                        pfadd = "http://+:" + serverPort.ToString() + "/";
                    }
                    else
                    {
                        pfadd = "http://*:" + serverPort.ToString() + "/";
                    }
                    listener.Prefixes.Add(pfadd);
                    Console.WriteLine("Listener Adding:" + pfadd);
                }
                catch (Exception e)
                {
                    Console.WriteLine("FAIL Listener Adding:" + pfadd);
                    Console.WriteLine(e.Message);
                }
                try
                {
                    listener.Start();
                }
                catch (Exception e)
                {
                    Console.WriteLine("FAIL listener.Start()");
                    Console.WriteLine(e.Message);
                }

                //loadAnalyzer();
                Thread t = new Thread(new ThreadStart(clientListener));
                t.Start();
            }
        }

        public  void clientListener()
        {
            while (true)
            {
                try
                {
                    HttpListenerContext request = listener.GetContext();
                    ThreadPool.QueueUserWorkItem(processRequest, request);
                }
                catch (Exception e)
                {
                    Console.WriteLine(e.Message);
                }
            }
        }

        // maybe http://localhost:8888/aiml/zenoaeronaut_resident/bstore/READ_ALICEINWONDERLAND.BTX
        public IGraph ourConfigGraph()
        {
            IGraph g = new Graph();
            TurtleParser parser = new TurtleParser();
            LoadIfExists("configuration.ttl", parser, g);
            LoadIfExists("localConfig.ttl",parser, g);
            //parser.Load(g, @"..\..\plugins\configuration.ttl");
            //parser.Load(g, @"..\..\plugins\localConfig.ttl");

            //g.Assert(new Triple(endnode, pa, dhh));
            parser.Load(g, new StringReader( @"<dotnetrdf:/*> a <dnr:HttpHandler> ."));
            parser.Load(g, new StringReader( @"<dotnetrdf:/*> <dnr:type> ""VDS.RDF.Web.SparqlServer"" ."));
            parser.Load(g, new StringReader(@"<dotnetrdf:/*> <dnr:queryProcessor> <dotnetrdf:qProc> ."));
            parser.Load(g, new StringReader(@"<dotnetrdf:/*> <dnr:updateProcessor> <dotnetrdf:uProc> ."));
            parser.Load(g, new StringReader(@"<dotnetrdf:/*> <dnr:protocolProcessor> <dotnetrdf:pProc> ."));

            parser.Load(g, new StringReader(@"<dotnetrdf:queryProcessor> a <rdf:Property> ."));
            parser.Load(g, new StringReader(@"<dotnetrdf:updateProcessor> a <rdf:Property> ."));
            parser.Load(g, new StringReader(@"<dotnetrdf:protocolProcessor> a <rdf:Property> ."));

            parser.Load(g, new StringReader(@"<dotnetrdf:qProc> a <dnr:SparqlQueryProcessor> ."));
            parser.Load(g, new StringReader(@"<dotnetrdf:qProc> <dnr:type> ""VDS.RDF.Query.LeviathanQueryProcessor"" ."));
            parser.Load(g, new StringReader(@"<dotnetrdf:qProc> <dnr:usingStore> <dotnetrdf:store>."));


            parser.Load(g, new StringReader(@"<dotnetrdf:uProc> a <dnr:SparqlUpdateProcessor> ."));
            parser.Load(g, new StringReader(@"<dotnetrdf:uProc> <dnr:type> ""VDS.RDF.Update.LeviathanUpdateProcessor"" ."));
            parser.Load(g, new StringReader(@"<dotnetrdf:uProc> <dnr:usingStore> <dotnetrdf:store>."));


            parser.Load(g, new StringReader(@"<dotnetrdf:pProc> a <dnr:SparqlHttpProtocolProcessor> ."));
            parser.Load(g, new StringReader(@"<dotnetrdf:pProc> <dnr:type> ""VDS.RDF.Update.Protocol.LeviathanProtocolProcessor"" ."));
            parser.Load(g, new StringReader(@"<dotnetrdf:pProc> <dnr:usingStore> <dotnetrdf:store>."));


            parser.Load(g, new StringReader(@"<dotnetrdf:store> a <dnr:TripleStore> ."));
            parser.Load(g, new StringReader(@"<dotnetrdf:store> <dnr:type> ""VDS.RDF.TripleStore""  ."));

            return g;
        }

        private void LoadIfExists(string path, TurtleParser parser, IGraph graph)
        {
            if (File.Exists(path))
            {
                parser.Load(graph, path);
            }
        }

        private bool EnsureConfigLoaded(HttpListenerContext context)
        {
            //if (this._config != null) return;
            lock (Configs)
            {
                //Generate the expected Path and try and load the Configuration using the appropriate Node
                String expectedPath = context.Request.Url.AbsolutePath;
                if (expectedPath.LastIndexOf('/') > 0)
                {
                    expectedPath = expectedPath.Substring(0, expectedPath.LastIndexOf('/'));
                }
                else
                {
                    expectedPath = "/";
                }
                expectedPath += "*";

                SparqlServerConfiguration cfg;
                if (Configs.TryGetValue(expectedPath, out cfg))
                {
                    this._config = cfg;
                    return true;
                } 
                
                IGraph g = ourConfigGraph();
                //IUriNode objNode = g.CreateUriNode(UriFactory.Create("dotnetrdf:" + expectedPath));
                INode objNode = g.GetUriNode(new Uri("dotnetrdf:" + expectedPath));
                if (objNode == null)
                {
                    Console.WriteLine("The Configuration Graph does not contain a URI Node with the expected URI <dotnetrdf:" + expectedPath + ">");
                    return false;
                }
                //objNode = g.GetUriNode(new Uri("dotnetrdf:" + expectedPath));
                Configs[expectedPath] = this._config = new SparqlServerConfiguration(g, objNode);
                return true;
            }
        }
        public  void processRequest(object listenerContext)
        {
            var context = (HttpListenerContext)listenerContext;
            if (!EnsureConfigLoaded(context))
            {
                this.ShowQueryForm(context);
                context.Response.Close();
                return;
            }
 

            String path = context.Request.Url.AbsolutePath;
            path = path.Substring(path.LastIndexOf('/') + 1);

            switch (path)
            {
                case "query":
                    this.ProcessQueryRequest(context);
                    break;
                case "update":
                    this.ProcessUpdateRequest(context);
                    break;
                case "description":
                    //TODO: Add Service Description support
                    context.Response.StatusCode = (int)HttpStatusCode.NotImplemented;
                    break;
                default:
                    //TODO: Can we easily add Protocol Support or not?
                    //this.ProcessProtocolRequest(context);
                    this.ShowQueryForm(context);
                    //context.Response.StatusCode = (int)HttpStatusCode.BadRequest;
                    break;
            }
            context.Response.Close();
            this._config = null;
            return;


            try
            {

                switch (context.Request.HttpMethod)
                {
                    case "POST":
                        CREATE(context);
                        break;
                    case "GET":
                        READ(context);
                        break;
                    case "PUT":
                        UPDATE(context);
                        break;
                    case "DELETE":
                        DELETE(context);
                        break;
                    default:
                        ERR(context);
                        break;
                }
                context.Response.Close();

            }
            catch
            {
                context.Response.Close();

            }
            finally
            {
                context.Response.Close();
            }
        }
        public  void CREATE(HttpListenerContext context)
        {
        }
        public  void READ(HttpListenerContext context)
        {
        }
        public  void UPDATE(HttpListenerContext context)
        {
        }
        public  void DELETE(HttpListenerContext context)
        {
        }
        public  void ERR(HttpListenerContext context)
        {
            byte[] msg;
            context.Response.StatusCode = (int)HttpStatusCode.NotFound;
            msg = File.ReadAllBytes(startUpPath + "\\webroot\\error.html");
            context.Response.ContentLength64 = msg.Length;
            using (Stream s = context.Response.OutputStream)
                s.Write(msg, 0, msg.Length);
        }



        /// <summary>
        /// Processes Query requests
        /// </summary>
        /// <param name="context">HTTP Context</param>
        public void ProcessQueryRequest(HttpListenerContext context)
        {
            //if (this._config.QueryProcessor == null)
            //{
            //    context.Response.StatusCode = (int)HttpStatusCode.NotImplemented;
            //    return;
            //}

            //Try and parse the Form Variables

            FormVars form = new FormVars(context.Request);
           // if (!form.IsValid)
           // {
           //     context.Response.StatusCode = (int)HttpStatusCode.BadRequest;
           //     return;
           // }

            if (context.Request.HttpMethod.Equals("OPTIONS"))
            {
                context.Response.StatusCode = (int)HttpStatusCode.NotImplemented;
                return;
                //TODO: Support Service Description?
                ////OPTIONS requests always result in the Service Description document
                //IGraph svcDescrip = SparqlServiceDescriber.GetServiceDescription(context, this._config, UriFactory.Create(context.Request.Url.AbsoluteUri), ServiceDescriptionType.Query);
                //HandlerHelper.SendToClient(context, svcDescrip, this._config);
                //return;
            }

            //See if there has been an query submitted
            String queryText = context.Request.QueryString["query"];
            if (queryText == null || queryText.Equals(String.Empty))
            {
                if (context.Request.ContentType != null)
                {
                    if (context.Request.ContentType.Equals(MimeTypesHelper.WWWFormURLEncoded))
                    {
                        queryText =(string) form["query"];
                    }
                    else if (context.Request.ContentType.Equals(MimeTypesHelper.SparqlQuery))
                    {
                        queryText = new StreamReader(context.Request.InputStream).ReadToEnd();
                    }
                }
                else
                {
                    queryText = (string)form["query"];
                }
            }

            //If no Query sent either show Query Form or give a HTTP 400 response
            if (queryText == null || queryText.Equals(String.Empty))
            {
                if (this._config.ShowQueryForm)
                {
                    this.ShowQueryForm(context);
                    return;
                }
                else
                {
                    context.Response.StatusCode = (int)HttpStatusCode.BadRequest;
                    return;
                }
            }

            //Get Other options associated with this query
            List<String> userDefaultGraphs = new List<String>();
            List<String> userNamedGraphs = new List<String>();
            long timeout = 0;
            bool partialResults = this._config.DefaultPartialResults;

            //Get the Default Graph URIs (if any)
            if (context.Request.QueryString["default-graph-uri"] != null)
            {
                userDefaultGraphs.AddRange(context.Request.QueryString.GetValues("default-graph-uri"));
            }
            else if (form["default-graph-uri"] != null)
            {
                userDefaultGraphs.AddRange(form.GetValues("default-graph-uri"));
            }
            //Get the Named Graph URIs (if any)
            if (context.Request.QueryString["named-graph-uri"] != null)
            {
                userNamedGraphs.AddRange(context.Request.QueryString.GetValues("named-graph-uri"));
            }
            else if (form["named-graph-uri"] != null)
            {
                userNamedGraphs.AddRange(form.GetValues("named-graph-uri"));
            }

            //Get Timeout setting (if any)
            if (context.Request.QueryString["timeout"] != null)
            {
                if (!Int64.TryParse(context.Request.QueryString["timeout"], out timeout))
                {
                    timeout = this._config.DefaultTimeout;
                }
            }
            else if (form["timeout"] != null)
            {
                if (!Int64.TryParse((string) form["timeout"], out timeout))
                {
                    timeout = this._config.DefaultTimeout;
                }
            }
            //Get Partial Results Setting (if any);
            if (context.Request.QueryString["partialResults"] != null)
            {
                if (!Boolean.TryParse(context.Request.QueryString["partialResults"], out partialResults))
                {
                    partialResults = this._config.DefaultPartialResults;
                }
            }
            else if (form["partialResults"] != null)
            {
                if (!Boolean.TryParse((string)form["partialResults"], out partialResults))
                {
                    partialResults = this._config.DefaultPartialResults;
                }
            }

            try
            {
                //Now we're going to parse the Query
                SparqlQueryParser parser = new SparqlQueryParser(this._config.QuerySyntax);
                parser.ExpressionFactories = this._config.ExpressionFactories;
                parser.QueryOptimiser = this._config.QueryOptimiser;
                SparqlQuery query = parser.ParseFromString(queryText);
                query.AlgebraOptimisers = this._config.AlgebraOptimisers;

                //TODO: Support Authentication?
                ////Check whether we need to use authentication
                ////If there are no user groups then no authentication is in use so we default to authenticated with no per-action authentication needed
                //bool isAuth = true, requireActionAuth = false;
                //if (this._config.UserGroups.Any())
                //{
                //    //If we have user
                //    isAuth = HandlerHelper.IsAuthenticated(context, this._config.UserGroups);
                //    requireActionAuth = true;
                //}
                //if (!isAuth) return;

                ////Is this user allowed to make this kind of query?
                //if (requireActionAuth) HandlerHelper.IsAuthenticated(context, this._config.UserGroups, this.GetQueryPermissionAction(query));

                //Set the Default Graph URIs (if any)
                if (userDefaultGraphs.Count > 0)
                {
                    //Default Graph Uri specified by default-graph-uri parameter or Web.config settings
                    foreach (String userDefaultGraph in userDefaultGraphs)
                    {
                        if (!userDefaultGraph.Equals(String.Empty))
                        {
                            query.AddDefaultGraph(UriFactory.Create(userDefaultGraph));
                        }
                    }
                }
                else if (!this._config.DefaultGraphURI.Equals(String.Empty))
                {
                    //Only applies if the Query doesn't specify any Default Graph
                    if (!query.DefaultGraphs.Any())
                    {
                        query.AddDefaultGraph(UriFactory.Create(this._config.DefaultGraphURI));
                    }
                }

                //Set the Named Graph URIs (if any)
                if (userNamedGraphs.Count > 0)
                {
                    foreach (String userNamedGraph in userNamedGraphs)
                    {
                        if (!userNamedGraph.Equals(String.Empty))
                        {
                            query.AddNamedGraph(UriFactory.Create(userNamedGraph));
                        }
                    }
                }

                //Set Timeout setting
                if (timeout > 0)
                {
                    query.Timeout = timeout;
                }
                else
                {
                    query.Timeout = this._config.DefaultTimeout;
                }

                //Set Partial Results Setting                 
                query.PartialResultsOnTimeout = partialResults;

                //Set Describe Algorithm
                query.Describer = this._config.DescribeAlgorithm;

                //Now we can finally make the query and return the results
                // KHC: Check our prolog engine graph first and use it as the default

                Object result=null;
                IGraph rdfGraph =  ourEngine.getRefreshedRDFGraph("spindleMT");
                if (rdfGraph.Triples.Count > 0)
                {
                    result = rdfGraph.ExecuteQuery(query);
                }
                if (result == null)
                {
                result= this._config.QueryProcessor.ProcessQuery(query);
                }
                this.ProcessQueryResults(context, result);
            }
            catch (RdfParseException parseEx)
            {
                HandleQueryErrors(context, "Parsing Error", queryText, parseEx, (int)HttpStatusCode.BadRequest);
            }
            catch (RdfQueryTimeoutException timeoutEx)
            {
                HandleQueryErrors(context, "Query Timeout Error", queryText, timeoutEx);
            }
            catch (RdfQueryException queryEx)
            {
                HandleQueryErrors(context, "Query Error", queryText, queryEx);
            }
            catch (RdfWriterSelectionException writerSelEx)
            {
                HandleQueryErrors(context, "Output Selection Error", queryText, writerSelEx, (int)HttpStatusCode.NotAcceptable);
            }
            catch (RdfException rdfEx)
            {
                HandleQueryErrors(context, "RDF Error", queryText, rdfEx);
            }
            catch (Exception ex)
            {
                HandleQueryErrors(context, "Error", queryText, ex);
            }
        }

        /// <summary>
        /// Internal Helper function which returns the Results back to the Client in one of their accepted formats
        /// </summary>
        /// <param name="context">Context of the HTTP Request</param>
        /// <param name="result">Results of the Sparql Query</param>
        protected void ProcessQueryResults(HttpListenerContext context, Object result)
        {
            //Return the Results
            String ctype;
            if (result is SparqlResultSet)
            {
                //Get the appropriate Writer and set the Content Type
                ISparqlResultsWriter sparqlwriter;
                if (context.Request.AcceptTypes != null)
                {
                    sparqlwriter = MimeTypesHelper.GetSparqlWriter(context.Request.AcceptTypes, out ctype);
                }
                else
                {
                    //Default to SPARQL XML Results Format if no accept header
                    sparqlwriter = new SparqlXmlWriter();
                    ctype = "application/sparql-results+xml";
                }
                context.Response.ContentType = ctype;
                if (sparqlwriter is IHtmlWriter)
                {
                    ((IHtmlWriter)sparqlwriter).Stylesheet = this._config.Stylesheet;
                }

                //Send Result Set to Client
                sparqlwriter.Save((SparqlResultSet)result, new StreamWriter(context.Response.OutputStream));
            }
            else if (result is Graph)
            {
                //Get the appropriate Writer and set the Content Type
                IRdfWriter rdfwriter = MimeTypesHelper.GetWriter(context.Request.AcceptTypes, out ctype);
                context.Response.ContentType = ctype;
                if (rdfwriter is IHtmlWriter)
                {
                    ((IHtmlWriter)rdfwriter).Stylesheet = this._config.Stylesheet;
                }

                //Send Graph to Client
                rdfwriter.Save((Graph)result, new StreamWriter(context.Response.OutputStream));
            }
            else
            {
                throw new RdfQueryException("Unexpected Query Result Object of Type '" + result.GetType().ToString() + "' returned");
            }
        }

        /// <summary>
        /// Processes Update requests
        /// </summary>
        /// <param name="context">HTTP Context</param>
        public void ProcessUpdateRequest(HttpListenerContext context)
        {
            if (this._config.UpdateProcessor == null)
            {
                context.Response.StatusCode = (int)HttpStatusCode.NotImplemented;
                return;
            }

            //Try and parse the Form Variables
            FormVars form = new FormVars(context.Request);
            //if (!form.IsValid)
            //{
            //    context.Response.StatusCode = (int)HttpStatusCode.BadRequest;
            //    return;
           // }

            if (context.Request.HttpMethod.Equals("OPTIONS"))
            {
                context.Response.StatusCode = (int)HttpStatusCode.NotImplemented;
                return;
                //TODO: Support Service Description?
                ////OPTIONS requests always result in the Service Description document
                //IGraph svcDescrip = SparqlServiceDescriber.GetServiceDescription(context, this._config, UriFactory.Create(context.Request.Url.AbsoluteUri), ServiceDescriptionType.Update);
                //HandlerHelper.SendToClient(context, svcDescrip, this._config);
                //return;
            }

            //See if there has been an update submitted
            String updateText = null;
            if (context.Request.ContentType != null)
            {
                if (context.Request.ContentType.Equals(MimeTypesHelper.WWWFormURLEncoded))
                {
                    updateText = (string) form["update"];
                }
                else if (context.Request.ContentType.Equals(MimeTypesHelper.SparqlUpdate))
                {
                    updateText = new StreamReader(context.Request.InputStream).ReadToEnd();
                }
            }
            else
            {
                updateText = (string) form["update"];
            }

            //If no Update sent either show Update Form or give a HTTP 400 response
            if (updateText == null || updateText.Equals(String.Empty))
            {
                if (this._config.ShowUpdateForm)
                {
                    this.ShowUpdateForm(context);
                    return;
                }
                else
                {
                    context.Response.StatusCode = (int)HttpStatusCode.BadRequest;
                    return;
                }
            }

            //Get Other options associated with this update
            List<String> userDefaultGraphs = new List<String>();
            List<String> userNamedGraphs = new List<String>();

            //Get the USING URIs (if any)
            var ugs = GetRequestValues("using-graph-uri", context, form);
            if (ugs != null) userDefaultGraphs.AddRange(ugs);

            //Get the USING NAMED URIs (if any)
            var ungs = GetRequestValues("using-named-graph-uri", context, form);
            if (ungs != null) userNamedGraphs.AddRange(ungs);

            try
            {
                //Now we're going to parse the Updates
                SparqlUpdateParser parser = new SparqlUpdateParser();
                parser.ExpressionFactories = this._config.ExpressionFactories;
                SparqlUpdateCommandSet commands = parser.ParseFromString(updateText);

                //TODO: Support Authentication?
                ////Check whether we need to use authentication
                ////If there are no user groups then no authentication is in use so we default to authenticated with no per-action authentication needed
                //bool isAuth = true, requireActionAuth = false;
                //if (this._config.UserGroups.Any())
                //{
                //    //If we have user
                //    isAuth = HandlerHelper.IsAuthenticated(context, this._config.UserGroups);
                //    requireActionAuth = true;
                //}
                //if (!isAuth) return;

                //First check actions to see whether they are all permissible and apply USING/USING NAMED paramaters
                foreach (SparqlUpdateCommand cmd in commands.Commands)
                {
                    //TODO: Support Authentication?
                    ////Authenticate each action
                    //bool actionAuth = true;
                    //if (requireActionAuth) actionAuth = HandlerHelper.IsAuthenticated(context, this._config.UserGroups, this.GetUpdatePermissionAction(cmd));
                    //if (!actionAuth)
                    //{
                    //    throw new SparqlUpdatePermissionException("You are not authorised to perform the " + this.GetUpdatePermissionAction(cmd) + " action");
                    //}

                    //Check whether we need to (and are permitted to) apply USING/USING NAMED parameters
                    if (userDefaultGraphs.Count > 0 || userNamedGraphs.Count > 0)
                    {
                        BaseModificationCommand modify = cmd as BaseModificationCommand;
                        if (modify != null)
                        {
                            if (modify.GraphUri != null || modify.UsingUris.Any() || modify.UsingNamedUris.Any())
                            {
                                //Invalid if a command already has a WITH/USING/USING NAMED
                                throw new SparqlUpdateMalformedException("A command in your update request contains a WITH/USING/USING NAMED clause but you have also specified one/both of the using-graph-uri or using-named-graph-uri parameters which is not permitted by the SPARQL Protocol");
                            }
                            else
                            {
                                //Otherwise go ahead and apply
                                userDefaultGraphs.ForEach(u => modify.AddUsingUri(UriFactory.Create(u)));
                                userNamedGraphs.ForEach(u => modify.AddUsingNamedUri(UriFactory.Create(u)));
                            }
                        }
                    }
                }

                //Then assuming we got here this means all our actions are permitted so now we can process the updates
                this._config.UpdateProcessor.ProcessCommandSet(commands);

                //Flush outstanding changes
                this._config.UpdateProcessor.Flush();
            }
            catch (RdfParseException parseEx)
            {
                HandleUpdateErrors(context, "Parsing Error", updateText, parseEx, (int)HttpStatusCode.BadRequest);
            }
            catch (SparqlUpdatePermissionException permEx)
            {
                HandleUpdateErrors(context, "Permissions Error", updateText, permEx, (int)HttpStatusCode.Forbidden);
            }
            catch (SparqlUpdateMalformedException malEx)
            {
                HandleUpdateErrors(context, "Malformed Update Error", updateText, malEx, (int)HttpStatusCode.BadRequest);
            }
            catch (SparqlUpdateException updateEx)
            {
                HandleUpdateErrors(context, "Update Error", updateText, updateEx);
            }
            catch (RdfException rdfEx)
            {
                HandleUpdateErrors(context, "RDF Error", updateText, rdfEx);
            }
            catch (Exception ex)
            {
                HandleUpdateErrors(context, "Error", updateText, ex);
            }
        }

        private IList<string> GetRequestValues(string name, HttpListenerContext context, FormVars form)
        {
            //Get the USING URIs (if any)
            var query = context.Request.QueryString;
            if (query[name] != null)
            {
                return query.GetValues(name);
            }
            else if (form[name] != null)
            {
                return form.GetValues(name);
            }
            return new string[0];
        }

        #region Error Handling

        /// <summary>
        /// Handles errors in processing SPARQL Query Requests
        /// </summary>
        /// <param name="context">Context of the HTTP Request</param>
        /// <param name="title">Error title</param>
        /// <param name="query">Sparql Query</param>
        /// <param name="ex">Error</param>
        protected virtual void HandleQueryErrors(HttpListenerContext context, String title, String query, Exception ex)
        {
            //HandlerHelper.HandleQueryErrors(context, this._config, title, query, ex);
            LocalHandleQueryErrors(context, this._config, title, query, ex,0);
        }

        /// <summary>
        /// Handles errors in processing SPARQL Query Requests
        /// </summary>
        /// <param name="context">Context of the HTTP Request</param>
        /// <param name="title">Error title</param>
        /// <param name="query">Sparql Query</param>
        /// <param name="ex">Error</param>
        /// <param name="statusCode">HTTP Status Code to return</param>
        protected virtual void HandleQueryErrors(HttpListenerContext context, String title, String query, Exception ex, int statusCode)
        {
            //HandlerHelper.HandleQueryErrors(context, this._config, title, query, ex, statusCode);
            LocalHandleQueryErrors(context, this._config, title, query, ex, statusCode);
        }

        /// <summary>
        /// Handles errors in processing SPARQL Update Requests
        /// </summary>
        /// <param name="context">Context of the HTTP Request</param>
        /// <param name="title">Error title</param>
        /// <param name="update">SPARQL Update</param>
        /// <param name="ex">Error</param>
        protected virtual void HandleUpdateErrors(HttpListenerContext context, String title, String update, Exception ex)
        {
            //HandlerHelper.HandleUpdateErrors(context, this._config, title, update, ex);
            LocalHandleUpdateErrors(context, this._config, title, update, ex,0);
        }

        /// <summary>
        /// Handles errors in processing SPARQL Update Requests
        /// </summary>
        /// <param name="context">Context of the HTTP Request</param>
        /// <param name="title">Error title</param>
        /// <param name="update">SPARQL Update</param>
        /// <param name="ex">Error</param>
        /// <param name="statusCode">HTTP Status code to return</param>
        protected virtual void HandleUpdateErrors(HttpListenerContext context, String title, String update, Exception ex, int statusCode)
        {
            //HandlerHelper.HandleUpdateErrors(context, this._config, title, update, ex, statusCode);
            LocalHandleUpdateErrors(context, this._config, title, update, ex, statusCode);
        }

        #endregion

        #region Forms

        /// <summary>
        /// Generates a SPARQL Query Form
        /// </summary>
        /// <param name="context">HTTP Context</param>
        protected virtual void ShowQueryForm(HttpListenerContext context)
        {
            //Set Content Type
            context.Response.ContentType = "text/html";

            //Get a HTML Text Writer
            HtmlTextWriter output = new HtmlTextWriter(new StreamWriter(context.Response.OutputStream));

            //Page Header
            output.Write("<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">");
            output.RenderBeginTag(HtmlTextWriterTag.Html);
            output.RenderBeginTag(HtmlTextWriterTag.Head);
            output.RenderBeginTag(HtmlTextWriterTag.Title);
            output.WriteEncodedText("SPARQL Query Interface");
            output.RenderEndTag();
            //Add Stylesheet
            if (!this._config.Stylesheet.Equals(String.Empty))
            {
                output.AddAttribute(HtmlTextWriterAttribute.Href, this._config.Stylesheet);
                output.AddAttribute(HtmlTextWriterAttribute.Type, "text/css");
                output.AddAttribute(HtmlTextWriterAttribute.Rel, "stylesheet");
                output.RenderBeginTag(HtmlTextWriterTag.Link);
                output.RenderEndTag();
            }
            output.RenderEndTag();


            //Header Text
            output.RenderBeginTag(HtmlTextWriterTag.Body);
            output.RenderBeginTag(HtmlTextWriterTag.H3);
            output.WriteEncodedText("SPARQL Query Interface");
            output.RenderEndTag();

            //Query Form
            output.AddAttribute(HtmlTextWriterAttribute.Name, "sparqlQuery");
            output.AddAttribute("method", "get");
            output.AddAttribute("action", context.Request.Url.AbsoluteUri);
            output.RenderBeginTag(HtmlTextWriterTag.Form);

            if (!this._config.IntroductionText.Equals(String.Empty))
            {
                output.RenderBeginTag(HtmlTextWriterTag.P);
                output.Write(this._config.IntroductionText);
                output.RenderEndTag();
            }

            output.WriteEncodedText("Query");
            output.WriteBreak();
            output.AddAttribute(HtmlTextWriterAttribute.Name, "query");
            output.AddAttribute(HtmlTextWriterAttribute.Rows, "15");
            output.AddAttribute(HtmlTextWriterAttribute.Cols, "100");
            int currIndent = output.Indent;
            output.Indent = 0;
            output.RenderBeginTag(HtmlTextWriterTag.Textarea);
            output.Indent = 0;
            if (!this._config.DefaultQuery.Equals(String.Empty))
            {
                output.WriteEncodedText(this._config.DefaultQuery);
            }
            output.RenderEndTag();
            output.Indent = currIndent;
            output.WriteBreak();

            output.WriteEncodedText("Default Graph URI: ");
            output.AddAttribute(HtmlTextWriterAttribute.Name, "default-graph-uri");
            output.AddAttribute(HtmlTextWriterAttribute.Type, "text");
            output.AddAttribute(HtmlTextWriterAttribute.Size, "100");
            output.AddAttribute(HtmlTextWriterAttribute.Value, this._config.DefaultGraphURI);
            output.RenderBeginTag(HtmlTextWriterTag.Input);
            output.RenderEndTag();
            output.WriteBreak();

            if (this._config.SupportsTimeout)
            {
                output.WriteEncodedText("Timeout: ");
                output.AddAttribute(HtmlTextWriterAttribute.Name, "timeout");
                output.AddAttribute(HtmlTextWriterAttribute.Type, "text");
                output.AddAttribute(HtmlTextWriterAttribute.Value, this._config.DefaultTimeout.ToString());
                output.RenderBeginTag(HtmlTextWriterTag.Input);
                output.RenderEndTag();
                output.WriteEncodedText(" Milliseconds");
                output.WriteBreak();
            }

            if (this._config.SupportsPartialResults)
            {
                output.AddAttribute(HtmlTextWriterAttribute.Name, "partialResults");
                output.AddAttribute(HtmlTextWriterAttribute.Type, "checkbox");
                if (this._config.DefaultPartialResults) output.AddAttribute(HtmlTextWriterAttribute.Checked, "checked");
                output.AddAttribute(HtmlTextWriterAttribute.Value, "true");
                output.RenderBeginTag(HtmlTextWriterTag.Input);
                output.RenderEndTag();
                output.WriteEncodedText(" Partial Results on Timeout?");
                output.WriteBreak();
            }

            output.AddAttribute(HtmlTextWriterAttribute.Type, "submit");
            output.AddAttribute(HtmlTextWriterAttribute.Value, "Make Query");
            output.RenderBeginTag(HtmlTextWriterTag.Input);
            output.RenderEndTag();

            output.RenderEndTag(); //End Form

            //End of Page
            output.RenderEndTag(); //End Body
            output.RenderEndTag(); //End Html

            output.Flush();
        }

        /// <summary>
        /// Generates a SPARQL Update Form
        /// </summary>
        /// <param name="context">HTTP Context</param>
        protected virtual void ShowUpdateForm(HttpListenerContext context)
        {
            //Set Content Type
            context.Response.ContentType = "text/html";

            //Get a HTML Text Writer
            HtmlTextWriter output = new HtmlTextWriter(new StreamWriter(context.Response.OutputStream));

            //Page Header
            output.Write("<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">");
            output.RenderBeginTag(HtmlTextWriterTag.Html);
            output.RenderBeginTag(HtmlTextWriterTag.Head);
            output.RenderBeginTag(HtmlTextWriterTag.Title);
            output.WriteEncodedText("SPARQL Update Interface");
            output.RenderEndTag();
            //Add Stylesheet
            if (!this._config.Stylesheet.Equals(String.Empty))
            {
                output.AddAttribute(HtmlTextWriterAttribute.Href, this._config.Stylesheet);
                output.AddAttribute(HtmlTextWriterAttribute.Type, "text/css");
                output.AddAttribute(HtmlTextWriterAttribute.Rel, "stylesheet");
                output.RenderBeginTag(HtmlTextWriterTag.Link);
                output.RenderEndTag();
            }
            output.RenderEndTag();


            //Header Text
            output.RenderBeginTag(HtmlTextWriterTag.Body);
            output.RenderBeginTag(HtmlTextWriterTag.H3);
            output.WriteEncodedText("SPARQL Update Interface");
            output.RenderEndTag();

            //Query Form
            output.AddAttribute(HtmlTextWriterAttribute.Name, "sparqlUpdate");
            output.AddAttribute("method", "get");
            output.AddAttribute("action", context.Request.Url.AbsoluteUri);
            output.RenderBeginTag(HtmlTextWriterTag.Form);

            if (!this._config.IntroductionText.Equals(String.Empty))
            {
                output.RenderBeginTag(HtmlTextWriterTag.P);
                output.Write(this._config.IntroductionText);
                output.RenderEndTag();
            }

            output.WriteEncodedText("Update");
            output.WriteBreak();
            output.AddAttribute(HtmlTextWriterAttribute.Name, "update");
            output.AddAttribute(HtmlTextWriterAttribute.Rows, "15");
            output.AddAttribute(HtmlTextWriterAttribute.Cols, "100");
            int currIndent = output.Indent;
            output.Indent = 0;
            output.RenderBeginTag(HtmlTextWriterTag.Textarea);
            output.Indent = 0;
            if (!this._config.DefaultUpdate.Equals(String.Empty))
            {
                output.WriteEncodedText(this._config.DefaultUpdate);
            }
            output.RenderEndTag();
            output.Indent = currIndent;
            output.WriteBreak();

            //output.WriteEncodedText("Default Graph URI: ");
            //output.AddAttribute(HtmlTextWriterAttribute.Name, "default-graph-uri");
            //output.AddAttribute(HtmlTextWriterAttribute.Type, "text");
            //output.AddAttribute(HtmlTextWriterAttribute.Size, "100");
            //output.AddAttribute(HtmlTextWriterAttribute.Value, this._config.DefaultGraphURI);
            //output.RenderBeginTag(HtmlTextWriterTag.Input);
            //output.RenderEndTag();
            //output.WriteBreak();

            output.AddAttribute(HtmlTextWriterAttribute.Type, "submit");
            output.AddAttribute(HtmlTextWriterAttribute.Value, "Perform Update");
            output.RenderBeginTag(HtmlTextWriterTag.Input);
            output.RenderEndTag();

            output.RenderEndTag(); //End Form

            //End of Page
            output.RenderEndTag(); //End Body
            output.RenderEndTag(); //End Html

            output.Flush();
        }


        /// <summary>
        /// Handles errors in processing SPARQL Query Requests
        /// </summary>
        /// <param name="context">Context of the HTTP Request</param>
        /// <param name="config">Handler Configuration</param>
        /// <param name="title">Error title</param>
        /// <param name="query">Sparql Query</param>
        /// <param name="ex">Error</param>
        /// <param name="statusCode">HTTP Status Code to return</param>
        public static void LocalHandleQueryErrors(HttpListenerContext context, SparqlServerConfiguration config, String title, String query, Exception ex, int statusCode)
        {
            //Clear any existing Response and set our HTTP Status Code
            //context.Response.Clear();
            context.Response.StatusCode = statusCode;

            if (config != null)
            {
                //If not showing errors then we won't return our custom error description
                if (!config.ShowErrors) return;
            }

            //Set to Plain Text output and report the error
            context.Response.ContentEncoding = System.Text.Encoding.UTF8;
            context.Response.ContentType = "text/plain";
            //Get a HTML Text Writer
            HtmlTextWriter output = new HtmlTextWriter(new StreamWriter(context.Response.OutputStream));
            
            //Error Title
            output.Write(title + "\n");
            output.Write(new String('-', title.Length) + "\n\n");

            //Output Query with Line Numbers
            if (query != null && !query.Equals(String.Empty))
            {
                String[] lines = query.Split('\n');
                for (int l = 0; l < lines.Length; l++)
                {
                    output.Write((l + 1) + ": " + lines[l] + "\n");
                }
                output.Write("\n\n");
            }

            //Error Message
            output.Write(ex.Message + "\n");

#if DEBUG
            //Stack Trace only when Debug build
            output.Write(ex.StackTrace + "\n\n");
            while (ex.InnerException != null)
            {
                ex = ex.InnerException;
                output.Write(ex.Message + "\n");
                output.Write(ex.StackTrace + "\n\n");
            }
#endif
        }

        /// <summary>
        /// Handles errors in processing SPARQL Update Requests
        /// </summary>
        /// <param name="context">Context of the HTTP Request</param>
        /// <param name="config">Handler Configuration</param>
        /// <param name="title">Error title</param>
        /// <param name="update">SPARQL Update</param>
        /// <param name="ex">Error</param>
        /// <param name="statusCode">HTTP Status Code to return</param>
        public static void LocalHandleUpdateErrors(HttpListenerContext context, SparqlServerConfiguration config, String title, String update, Exception ex, int statusCode)
        {
            //Clear any existing Response
            //context.Response .Clear();

            if (config != null)
            {
                if (!config.ShowErrors)
                {
                    context.Response.StatusCode = statusCode;
                    return;
                }
            }

            //Set to Plain Text output and report the error
            context.Response.ContentEncoding = System.Text.Encoding.UTF8;
            context.Response.ContentType = "text/plain";

            HtmlTextWriter output = new HtmlTextWriter(new StreamWriter(context.Response.OutputStream));
            //Error Title
            output.Write(title + "\n");
            output.Write(new String('-', title.Length) + "\n\n");

            //Output Query with Line Numbers
            if (update != null && !update.Equals(String.Empty))
            {
                String[] lines = update.Split('\n');
                for (int l = 0; l < lines.Length; l++)
                {
                    output.Write((l + 1) + ": " + lines[l] + "\n");
                }
                output.Write("\n\n");
            }

            //Error Message
            output.Write(ex.Message + "\n");

#if DEBUG
            //Stack Trace only when Debug build
            output.Write(ex.StackTrace + "\n\n");
            while (ex.InnerException != null)
            {
                ex = ex.InnerException;
                output.Write(ex.Message + "\n");
                output.Write(ex.StackTrace + "\n\n");
            }
#endif
        }
        public class FormVars : Hashtable
        {

            public FormVars(HttpListenerRequest request)
            {
               // Hashtable formVars = new Hashtable();

                //add request data at bottom of page

                if (request.HasEntityBody)
                {
                    System.IO.Stream body = request.InputStream;
                    System.Text.Encoding encoding = request.ContentEncoding;
                    System.IO.StreamReader reader = new System.IO.StreamReader(body, encoding);

                    if (request.ContentType.ToLower() == "application/x-www-form-urlencoded")
                    {
                        string s = reader.ReadToEnd();
                        string[] pairs = s.Split('&');

                        for (int x = 0; x < pairs.Length; x++)
                        {
                            string[] item = pairs[x].Split('=');
                            this.Add(item[0], System.Web.HttpUtility.UrlDecode(item[1]));
                        }

                    }

                    body.Close();
                    reader.Close();
                }
            }

            public  List <string> GetValues(string key)
            {
                List<string> result = new List<string>();
                if (this[key] != null)
                {
                    result.Add((string)this[key]);
                }
                return result;
            }
        }
        #endregion

    }
}
