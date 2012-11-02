using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Net;
using System.Net.Sockets;
using System.Reflection;
using System.IO;
using System.Threading;
using System.Xml;
using System.Web;

namespace AltAIMLbot
{
    public class WebServitor
    {
        public static HttpListener listener = new HttpListener();

        public static string startUpPath = null;
        public static Servitor ourServitor = null;
        public static string serverRoot = "http://CogbotServer:8123/";
        public static int serverPort = 8123;
        public static string kpfile = @".\wikilink\phraseScore";
        public static string wsfile = @".\wikilink\count.phrase.sense.txt";
        public static string bslfile = @".\wikilink\behavior.stoplist.txt";

        public static Dictionary <string,double> phraseScore = new Dictionary <string,double >();
        public static Dictionary<string, int> senseCount = new Dictionary<string, int>();
        public static Dictionary<string, string> senseLink = new Dictionary<string, string>();
        public static string[] behaviorStoplist = null;

        public static bool IsMicrosoftCLR()
        {
            return (Type.GetType("Mono.Runtime") == null);
        }

        public static void beginService(Servitor theServitor)
        {
            startUpPath = startUpPath ??
                          System.IO.Path.GetDirectoryName(
                              (Assembly.GetEntryAssembly() ?? typeof (WebServitor).Assembly).Location);

            if (!HttpListener.IsSupported)
            {
                Console.WriteLine("***** HttpListener is not supported on this platform. *****");
                return;
            }
            ourServitor = theServitor;
            
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

                loadAnalyzer();
                Thread t = new Thread(new ThreadStart(clientListener));
                t.Start();
            }
        }

        public static void clientListener()
        {
            while(true)
            {
                try
                {
                    HttpListenerContext request=listener.GetContext ();
                    ThreadPool.QueueUserWorkItem(processRequest, request);
                }
                catch(Exception e)
                {
                    Console.WriteLine(e.Message);
                }
            }
        }

        // maybe http://localhost:8888/aiml/zenoaeronaut_resident/bstore/READ_ALICEINWONDERLAND.BTX

        public static void processRequest(object listenerContext)
        {
                var context = (HttpListenerContext)listenerContext;
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
        public static string  filenameToBehaviorname(string name)
        {
            name = name.Replace(".btx", "");
            name = name.Replace(".BTX", "");
            return name;
        }
        public static void ERR(HttpListenerContext context)
        {
            byte[] msg;
            context.Response.StatusCode = (int)HttpStatusCode.NotFound;
            msg = File.ReadAllBytes(startUpPath + "\\webroot\\error.html");
            context.Response.ContentLength64 = msg.Length;
            using (Stream s = context.Response.OutputStream)
                s.Write(msg, 0, msg.Length);
        }

        public static void CREATE(HttpListenerContext context)
        {
            //POST (INSERT to DB)
            // Put the Behavior in the BTX cache
            byte[] msg;
            string[] sections = context.Request.RawUrl.Split('?');
            string justURL = sections[0];
            string filename = Path.GetFileName(justURL);
            string behaviorName = filenameToBehaviorname(filename);
            string behaviorFile = ourServitor.curBot.myBehaviors.behaviorDiskName(behaviorName);
            string behaviorDir = ourServitor.curBot.myBehaviors.persistantDirectory;
            string path = Path.Combine(startUpPath, filename);

            if (path.Contains("./behavior/"))
            {
                path.Replace("./behavior/", behaviorDir);
            }
            if ((File.Exists(behaviorFile)) && (!File.Exists(path)))
            {
                path = behaviorFile;
            }
            // The message body is posted as bytes. read the bytes
            //byte[] PostData = context.Request.BinaryRead(context.Request.ContentLength);
            Stream bodyStream = context.Request.InputStream;
            Encoding encoding = context.Request.ContentEncoding;

            StreamReader streamReader = new StreamReader(bodyStream, encoding);
            if (path.Contains("./interpreter/"))
            {
                // Posting to the interperter directory
                // causes the text to be loaded immediately like a new AIML file

                string infoContentType = context.Request.ContentType;
                long infoContentLength = context.Request.ContentLength64;
                string infoBody = streamReader.ReadToEnd();
                string report = "<ok/>";
                try
                {
                    XmlDocument _xmlfile = new XmlDocument();
                    if (infoBody.Length > 0) _xmlfile.LoadXml(infoBody);
                    ourServitor.curBot.loadAIMLFromXML(_xmlfile, "webservice");
                    context.Response.StatusCode = (int)HttpStatusCode.OK;
                }
                catch(Exception e)
                {
                    context.Response.StatusCode = (int)HttpStatusCode.InternalServerError ;
                    report = String.Format("<error msg=\"{0}\" />{1}</error>", e.Message, e.StackTrace); 
                }
                using (Stream s = context.Response.OutputStream)
                using (StreamWriter writer = new StreamWriter(s))
                    writer.WriteLine("<ok/>");
                return;

            }
            if (context.Request.ContentType != null) 
            {
                string infoContentType = context.Request.ContentType;
                long infoContentLength = context.Request.ContentLength64;
                string infoBody = streamReader.ReadToEnd();
                //Convert the bytes to string using Encoding class
                //string str = Encoding.UTF8.GetString(PostData);
                ourServitor.curBot.myBehaviors.defineBehavior(behaviorName, infoBody);
                context.Response.StatusCode = (int)HttpStatusCode.OK;
                msg = File.ReadAllBytes(path);
                // return the posted contents if any
            }
            else
            {
                context.Response.StatusCode = (int)HttpStatusCode.OK;
                msg = File.ReadAllBytes(path);
            }
            context.Response.ContentLength64 = msg.Length;
            using (Stream s = context.Response.OutputStream)
                s.Write(msg, 0, msg.Length);

        }

        public static void READ(HttpListenerContext context)
        {
            //GET (READ from DB)
            string [] sections = context.Request.RawUrl.Split('?');
            string justURL = sections[0];
            string filename = Path.GetFileName(justURL);
            string behaviorName = filenameToBehaviorname(filename);
            string behaviorFile = ourServitor.curBot.myBehaviors.behaviorDiskName(behaviorName);
            string behaviorDir = ourServitor.curBot.myBehaviors.persistantDirectory;
            //string path = Path.Combine(startUpPath, filename);
            //string path = Path.Combine(startUpPath, context.Request.RawUrl);
            string path = "." + justURL;
            string query = context.Request.QueryString["q"];
            string action = context.Request.QueryString["a"];
            string mt = context.Request.QueryString["mt"];
            Console.WriteLine("WEBGET path={0},action={1},query={2}", path, action, query);

            if (path.Contains("./scheduler/"))
            {
                context.Response.StatusCode = (int)HttpStatusCode.OK;
                using (Stream s = context.Response.OutputStream)
                using (StreamWriter writer = new StreamWriter(s))
                    ourServitor.myScheduler.performAction(writer,action,query, behaviorName);
                return;
            }
            if (path.Contains("./siprolog/"))
            {
                context.Response.StatusCode = (int)HttpStatusCode.OK;
                using (Stream s = context.Response.OutputStream)
                using (StreamWriter writer = new StreamWriter(s))
                    ourServitor.prologEngine.webWriter(writer, action, query,mt, serverRoot);
                return;
            }

            if (path.Contains("./analysis/"))
            {
                path.Replace("./analysis/", behaviorDir);
                if ((File.Exists(behaviorFile)) && (!File.Exists(path)))
                {
                    path = behaviorFile;
                }

                context.Response.StatusCode = (int)HttpStatusCode.OK;
                using (Stream s = context.Response.OutputStream)
                using (StreamWriter writer = new StreamWriter(s))
                    analyse(writer, path, behaviorName, justURL);
                return;
            }

            if (path.Contains("./graphmaster/"))
            {
                path.Replace("./graphmaster/", behaviorDir);
                if ((File.Exists(behaviorFile)) && (!File.Exists(path)))
                {
                    path = behaviorFile;
                }

                context.Response.StatusCode = (int)HttpStatusCode.OK;
                using (Stream s = context.Response.OutputStream)
                using (StreamWriter writer = new StreamWriter(s))
                    if (query != null)
                    {
                        analyseGraphMaster(writer, query, justURL);
                    }
                    else
                    {
                        fetchGraphMaster(writer, justURL);
                    }
                return;
            }

            if (path.Contains("./analysisllist/"))
            {
                string furi = justURL ;
                using (Stream s = context.Response.OutputStream)
                using (StreamWriter writer = new StreamWriter(s))
                try
                {
                    context.Response.StatusCode = (int)HttpStatusCode.OK;
                    string[] fileList = Directory.GetFiles(behaviorDir);
                    string fileListString = "";
                    string uriPrefix = serverRoot + "behavior/";
                    {
                        foreach (string f in fileList)
                        {
                            furi = uriPrefix + Path.GetFileName(f);
                            string behaviorName2 = filenameToBehaviorname(Path.GetFileName(f));
                            string justURL2 = "";
                            analyse(writer, f, behaviorName2, furi);
                        }
                        //msg = System.Text.Encoding.ASCII.GetBytes(fileListString);
                        writer.Close();
                    }
                }
                catch (Exception e)
                {
                    writer.WriteLine("<{0}> <hasErrorMessage> \"{1}\"",furi,e.Message);
                    writer.WriteLine("<{0}> <hasErrorStackTrace> \"{1}\"", furi, 
                                       e.StackTrace.Replace('\n',' ').Replace ('\r',' '));
                    writer.Close();
                }
                return;
            }

            if (path.Contains("./behavior/"))
            {
                path.Replace("./behavior/", behaviorDir);
            }
            byte[] msg;
            if ((File.Exists(behaviorFile)) && (!File.Exists(path)))
            {
                path = behaviorFile;
            }

            if (!File.Exists(path))
            {

                if (path.EndsWith("/list/"))
                {
                    context.Response.StatusCode = (int)HttpStatusCode.OK;
                    //context.Response.ContentLength64 = 0;

                    string [] fileList = Directory.GetFiles(behaviorDir);
                    string fileListString = "";
                    string uriPrefix = serverRoot+"behavior/";
                    using (Stream s = context.Response.OutputStream)
                    using (StreamWriter writer = new StreamWriter(s))
                    {
                        foreach (string f in fileList)
                        {
                            string fout = uriPrefix + Path.GetFileName(f);
                            writer.WriteLine(fout);
                        }
                        //msg = System.Text.Encoding.ASCII.GetBytes(fileListString);
                        writer.Close();
                    }
                    return;
                }
                else
                {
                    Console.WriteLine("Client requesed nonexistant file {0} => {1}, sending error ...",
                        justURL, path);
                    context.Response.StatusCode = (int)HttpStatusCode.NotFound;
                    msg = File.ReadAllBytes(startUpPath + "\\webroot\\error.html");
                }
            }
            else
            {
                context.Response.StatusCode = (int)HttpStatusCode.OK;
                msg = File.ReadAllBytes(path);
            }
            context.Response.ContentLength64 = msg.Length;
            using (Stream s = context.Response.OutputStream)
                s.Write(msg, 0, msg.Length);

        }
        public static void UPDATE(HttpListenerContext context)
        {
            //PUT (Update DB)
            byte[] msg;
            string[] sections = context.Request.RawUrl.Split('?');
            string justURL = sections[0];
            string filename = Path.GetFileName(justURL);
            string path = Path.Combine(startUpPath, filename);
            string behaviorName = filenameToBehaviorname(filename);
            string behaviorFile = ourServitor.curBot.myBehaviors.behaviorDiskName(behaviorName);
            string behaviorDir = ourServitor.curBot.myBehaviors.persistantDirectory;
            string query = context.Request.QueryString["q"];
            string action = context.Request.QueryString["a"];
            if (path.Contains("./behavior/"))
            {
                path.Replace("./behavior/", behaviorDir);
            }
            if ((File.Exists(behaviorFile)) && (!File.Exists(path)))
            {
                path = behaviorFile;
            }
            // The message body is posted as bytes. read the bytes
            //byte[] PostData = context.Request.BinaryRead(context.Request.ContentLength);
            Stream bodyStream = context.Request.InputStream;
            Encoding encoding = context.Request.ContentEncoding;

            StreamReader streamReader = new StreamReader(bodyStream, encoding);
            if (context.Request.ContentType != null)
            {
                string infoContentType = context.Request.ContentType;
                long infoContentLength = context.Request.ContentLength64;
                string infoBody = streamReader.ReadToEnd();
                //Convert the bytes to string using Encoding class
                //string str = Encoding.UTF8.GetString(PostData);
                ourServitor.curBot.myBehaviors.defineBehavior(behaviorName, infoBody);
                context.Response.StatusCode = (int)HttpStatusCode.OK;
                msg = File.ReadAllBytes(path);
                // return the posted contents if any
            }
            else
            {
                context.Response.StatusCode = (int)HttpStatusCode.OK;
                msg = File.ReadAllBytes(path);
            }
            context.Response.ContentLength64 = msg.Length;
            using (Stream s = context.Response.OutputStream)
                s.Write(msg, 0, msg.Length);

        }
        public static void DELETE(HttpListenerContext context)
        {
            //DELETE
            string[] sections = context.Request.RawUrl.Split('?');
            string justURL = sections[0];
            string filename = Path.GetFileName(justURL);
            string path = Path.Combine(startUpPath, filename);
            string behaviorName = filenameToBehaviorname(filename);
            string behaviorFile = ourServitor.curBot.myBehaviors.behaviorDiskName(behaviorName);
            string behaviorDir = ourServitor.curBot.myBehaviors.persistantDirectory;
            string query = context.Request.QueryString["q"];
            string action = context.Request.QueryString["a"];
            if (path.Contains("./behavior/"))
            {
                path.Replace("./behavior/", behaviorDir);
            }
            if ((File.Exists(behaviorFile)) && (!File.Exists(path)))
            {
                path = behaviorFile;
            }


        }

        public static void loadAimlIndex()
        {
            if (ourServitor.myIndex.Externindex.Count == 0)
            {
                List<string> allPaths = new List<string>();
                ourServitor.curBot.Graphmaster.collectFullPaths("", allPaths);
                ourServitor.myIndex.LoadGraphMap(allPaths);
            }
        }
        public static string graphMasterToURI(string gmPath)
        {
            string gmBase = serverRoot + "graphmaster/";
            gmPath = gmPath.Replace("<", "{");
            gmPath = gmPath.Replace(">", "}");
            return gmBase + gmPath;
        }
        public static string URITographMaster(string gmPath)
        {
            //gmPath = HttpUtility.HtmlDecode(gmPath.Trim());
            gmPath = HttpUtility.UrlDecode(gmPath.Trim());
            string gmBase = serverRoot + @"graphmaster/";
            gmPath = gmPath.Replace("{", "<");
            gmPath = gmPath.Replace("}", ">");
            gmPath = gmPath.Replace(gmBase, "");
            gmPath = gmPath.Replace(@"/graphmaster/", "");
            return gmPath;
        }

        public static void fetchGraphMaster(StreamWriter writer, string rawURL)
        {
            //loadAimlIndex();
            string gmPath = URITographMaster(rawURL);
            List<string> allPaths = new List<string>();
            ourServitor.curBot.Graphmaster.searchFullPaths(gmPath, "", allPaths);

            foreach (string frag in allPaths)
            {
                writer.WriteLine("");
                writer.WriteLine("{0}", frag);
            }
            writer.WriteLine("");
            
        }

        public static void analyseGraphMaster(StreamWriter writer, string query, string rawURL)
        {
            loadAimlIndex();
            List<string> rawList = query.Split(' ').ToList();
            List<string> queryList = query.Split(' ').ToList();
            foreach (string x in rawList)
            {
                string y = ourServitor.myIndex.stemmer.Stem(x);
                if (y != x) { queryList.Add(y); }
                string z = DoubleMetaphoneStringExtension.GenerateDoubleMetaphone(x);
                queryList.Add(z);
            }
            
            Dictionary<string, double> pathResults = ourServitor.myIndex.performTfIdfSearch(queryList);
            pathResults = pathResults.OrderByDescending(x => x.Value).ToDictionary(x => x.Key, x => x.Value);
            int resultCount = 0;

            foreach (string path in pathResults.Keys)
            {
                double irScore = pathResults[path];
                List<string> allPaths = new List<string>();
                ourServitor.curBot.Graphmaster.searchFullPaths(path,"", allPaths);
                string gmURI = graphMasterToURI(path);
                foreach (string frag in allPaths)
                {
                    writer.WriteLine("");
                    writer.WriteLine("{0}", frag);
                    List<string> ngrams = tagify(frag);
                    writer.WriteLine("<{0}> <queryScore> \"{1}\" .", gmURI, irScore);
                    writer.WriteLine("<{0}> <query> \"{1}\" .", gmURI, query);
                    if (ngrams.Count > 1)
                    {
                        writer.WriteLine("<{0}> <rdf:type> <dctype:Text> .", gmURI);

                    }
                    foreach (string phrase in ngrams)
                    {
                        double score = phraseScore[phrase];
                        int senseC = 0;
                        string senseL = "";
                        if (senseCount.ContainsKey(phrase))
                        {
                            senseC = senseCount[phrase];
                            senseL = senseLink[phrase];
                        }
                        if (score < 0.004) break;
                        writer.WriteLine("<{0}> <rdfs:comment> \"mentions '{1}'\"@en .", gmURI, phrase);
                        if (senseC > 0)
                        {
                            //writer.WriteLine("<{0}> <http://purl.org/dc/terms/subject> <{1}> .", basicURI, senseL);
                            writer.WriteLine("<{0}> <dcterms:subject> <{1}> .", gmURI, senseL);
                            //writer.WriteLine("<\"{0}\"><hasScore> {1} .", senseL, score);
                        }
                    }
                
                }
                // Max 100
                resultCount++;
                if (resultCount > 100) { break; }
            }
            writer.WriteLine("");
            writer.Close();
        }

        public static void loadAnalyzer()
        {
            if (File.Exists(kpfile))
            {
                string[] lines = System.IO.File.ReadAllLines(kpfile);
                foreach (string line in lines)
                {
                    string[] toks = line.Split(' ');
                    double v = double.Parse(toks[0]);
                    string text = line.Replace(toks[0]+" ","").Trim().ToLower ();
                    phraseScore[text] = v;
                }
            }
            phraseScore["zeno"] = 8;
            if (File.Exists(wsfile))
            {
                string[] lines = System.IO.File.ReadAllLines(wsfile);
                foreach (string line in lines)
                {
                    string[] toks = line.Split('|');
                    int num = int.Parse(toks[0]);
                    string text = toks[1].Trim().ToLower();
                    string sense = "http://dbpedia.org/resource/" + capitalize(toks[2]).Replace(" ", "_");

                    if (sense.Contains("\'")) continue;

                    if (phraseScore.ContainsKey(text)) 
                    {
                        if (!senseCount.ContainsKey(text)) senseCount[text] = 0;
                        if (senseCount[text] < num)
                        {
                            senseCount[text] = num;
                            senseLink[text] = sense;
                        }
                    }
                }
            }
            if (File.Exists(bslfile))
            {
                behaviorStoplist = System.IO.File.ReadAllLines(bslfile);
            }

        }
        public static string capitalize(string txt)
        {
            if ((txt != null) && (txt.Length > 1))
            {
                txt = txt.Substring(0, 1).ToUpper() + txt.Substring(1);
            }
            return txt;
        }
        public static List<string>  tagify(string text)
        {
             text = text.Replace("\"C_", "\" ");
            text = text.Replace("_", " ");

            foreach (string rkey in behaviorStoplist)
            {
                if (rkey.Length > 0)
                {
                    text = text.Replace(rkey, " ");
                }
            }

            // BTXML stoplist
            text = text.Replace(" id=\"", " \"");
            text = text.Replace(" pace=\"", " \"");
            text = text.Replace(" mark=\"", " \"");
            text = text.Replace(" restore=\"", " \"");



            text = text.Replace('"', ' ');
            text = text.Replace('<', ' ');
            text = text.Replace('>', ' ');
            text = text.Replace('=', ' ');
            text = text.Replace('/', ' ');

            while(text.Contains ("  ")) text = text.Replace("  "," ");


            string[] tokens = text.Trim().ToLower().Split(' ');
            List<string> ngrams = new List<string>();
            string ng = "";
            for (int i = 0; i < tokens.Length; i++)
            {
                ng = tokens[i];
                if ((phraseScore.ContainsKey(ng)) && (!ngrams.Contains(ng)))
                    ngrams.Add(ng);
            }
            for (int i = 0; i < tokens.Length-1; i++)
            {
                ng = tokens[i]+" "+tokens[i+1];
                if ((phraseScore.ContainsKey(ng)) && (!ngrams.Contains(ng)))
                    ngrams.Add(ng);
            }
            for (int i = 0; i < tokens.Length-2; i++)
            {
                ng = tokens[i] + " " + tokens[i + 1]+" "+tokens[i+2];
                if ((phraseScore.ContainsKey(ng)) && (!ngrams.Contains(ng)))
                    ngrams.Add(ng);
            }
            for (int i = 0; i < tokens.Length - 3; i++)
            {
                ng = tokens[i] + " " + tokens[i + 1] + " " + tokens[i + 2] + " " + tokens[i + 3];
                if ((phraseScore.ContainsKey(ng)) && (!ngrams.Contains(ng)))
                    ngrams.Add(ng);
            }
             ngrams.Sort(phraseSortFunction);
             return ngrams;
        }



        public static void analyse(
            StreamWriter writer, string path, string behaviorName,string rawURL)
        {
            Console.WriteLine("analyse path={0},behaviorName={1},rawURL={2}", path, behaviorName, rawURL);
            if (!File.Exists(path))
            {
                writer.WriteLine("<fin/>");
                return;
            }
            XmlDocument _xmlfile = new XmlDocument();
            string text = File.ReadAllText(path);
            if (text.Length >0) _xmlfile.LoadXml(text);
            //_xmlfile.Load(path);
            //string text = _xmlfile.OuterXml;

            string basicURI = rawURL;
            basicURI = basicURI.Replace("/analysis/", "/behavior/");

            writer.WriteLine("<{0}> <rdf:type> <dctype:InteractiveResource> .", basicURI);
            writer.WriteLine("<{0}> <dcterms:title> \"{1}\" .", basicURI, behaviorName);

            List<string> ngrams = tagify(text);
            if (ngrams.Count > 1)
            {
                writer.WriteLine("<{0}> <rdf:type> <dctype:Text> .", basicURI);

            }
            foreach (string phrase in ngrams)
            {
                double score = phraseScore[phrase];
                int senseC = 0;
                string senseL = "";
                if (senseCount.ContainsKey(phrase))
                {
                    senseC = senseCount[phrase];
                    senseL = senseLink[phrase];
                }
                if (score < 0.004) break;
                writer.WriteLine("<{0}> <rdfs:comment> \"mentions '{1}'\"@en .", basicURI, phrase);
                if (senseC > 0)
                {
                    //writer.WriteLine("<{0}> <http://purl.org/dc/terms/subject> <{1}> .", basicURI, senseL);
                    writer.WriteLine("<{0}> <dcterms:subject> <{1}> .", basicURI, senseL);
                    //writer.WriteLine("<\"{0}\"><hasScore> {1} .", senseL, score);
                }
            }
            if (_xmlfile.ChildNodes.Count > 0)
            {
                relateToRootParent(writer, _xmlfile, basicURI, "subbehavior", "behavior/", ".BTX",
                    "dcterms:hasPart");
                relateToRootParent(writer, _xmlfile, basicURI, "subbehavior", "behavior/", ".BTX",
                    "dcterms:requires");
            }
            Console.WriteLine("Analysis complete {0}", behaviorName);
            writer.WriteLine ("");

        }
        public static int phraseSortFunction(string obj1, string obj2)
        {
            return phraseScore[obj2].CompareTo(phraseScore[obj1]);
        }

        public static void relateToRootParent(StreamWriter writer, XmlNode xmlNode, string rootURI, string nodeType,string resourcePrefix,string resourceExtension, string relation)
        {
            string selfURI = "";
            if (xmlNode.Name.ToLower() == nodeType)
            {
                selfURI = serverRoot + resourcePrefix + xmlNode.Attributes["id"].Value + resourceExtension;
                writer.WriteLine("<{0}> <{1}> <{2}> .", rootURI, relation, selfURI);
            }
            foreach (XmlNode childNode in xmlNode.ChildNodes)
            {
                relateToRootParent(writer, childNode, rootURI, nodeType, resourcePrefix, resourceExtension, relation);
            }
        }
    }
}
