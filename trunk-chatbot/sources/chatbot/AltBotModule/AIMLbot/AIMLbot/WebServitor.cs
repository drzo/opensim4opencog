using System;
using System.Collections;
using System.Collections.Generic;
using System.Collections.Specialized;
using System.Linq;
using System.Text;
using System.Net;
using System.Net.Sockets;
using System.Reflection;
using System.IO;
#if (COGBOT_LIBOMV || USE_STHREADS)
using ThreadPoolUtil;
using ThreadPoolUtil;
using ThreadStart = System.Threading.ThreadStart;
using AutoResetEvent = System.Threading.AutoResetEvent;
using ManualResetEvent = System.Threading.ManualResetEvent;
using TimerCallback = System.Threading.TimerCallback;
using Timer = System.Threading.Timer;
using Interlocked = System.Threading.Interlocked;
#else
using System.Threading;
#endif
using System.Xml;
using System.Web;
using LogicalParticleFilter1;
using MushDLR223.Utilities;
using MushDLR223.Virtualization;
using AltAIMLbot.Utils;

namespace AltAIMLbot
{
    public class WebServitor
    {
        public static HttpListener listener = null;

        public static string startUpPath = null;
        public static Servitor ourServitor = null;
        protected static BehaviorSet myBehaviors
        {
            get { return ourServitor.curBot.myBehaviors; }
        }
        protected static BehaviorContext curBot
        {
            get { return ourServitor.curBot.BotBehaving; }
        }
        protected static AltBot bot
        {
            get { return ourServitor.curBot; }
        }
        protected static SIProlog prologEngine
        {
            get
            {
                var pl = ourServitor.prologEngine;
                if (pl != null) return pl;
                pl = bot.prologEngine;
                if (pl != null) return pl;
                return SIProlog.CurrentProlog;
            }
        }

        /// <summary>
        /// The idea of tl_serverRoot is it my be set by a http client who knows this machine by a 
        ///  public name such as http://12.1.1.12
        /// 
        /// </summary>
        public static string tl_serverRoot
        {
            get
            {
                return GlobalSharedSettings.tl_serverRoot;
            }
            set
            {
                GlobalSharedSettings.tl_serverRoot = value;
            }
        }
        public static string GlobalServerHostWithPort
        {
            get
            {
                return GlobalSharedSettings.serverWithPort;
            }
            set
            {
                GlobalSharedSettings.serverWithPort = value;
            }
        }
        public static string serverRoot
        {
            get
            {
                if (tl_serverRoot != null) return WithHttp(tl_serverRoot);
                return WithHttp(GlobalServerHostWithPort);
            }
            set { GlobalServerHostWithPort = value; }
        }


        /// <summary>
        /// WithHttp add a http:// prefix if missing
        ///       and removes a trailing slash if present to allow easier concatenation;
        /// </summary>
        /// <param name="root0"></param>
        /// <returns></returns>
        public static string WithHttp(string root0)
        {
            if (root0 == null) return null;
            var root = root0;
            if (!root.StartsWith("http://")) root = "http://" + root;
            if (root.EndsWith("/")) root = root.Substring(0, root.Length - 1);
            return root;
        }

        /// <summary>
        ///  Adds a trailing "/" if needed
        /// </summary>
        /// <param name="root0"></param>
        /// <returns></returns>
        public static string WithSlash(string root0)
        {
            if (root0.EndsWith("/")) return root0;
            return root0 + "/";
        }

        public static string GetServerRoot(string hostSuggest)
        {
            string sr = GlobalServerHostWithPort;
            sr = sr.Replace("127.0.0.1", "localhost");
            sr = sr.Replace("+:", "localhost:");
            sr = sr.Replace("*:", "localhost:");
            var ctx = tl_context;
            if (ctx != null)
            {
                var r = ctx.Request;
                if (r != null)
                {
                    var s = ctx.Request.UserHostAddress;
                    if (s != null)
                    {
                        sr = sr.Replace("localhost:" + serverPort, hostSuggest);
                        sr = sr.Replace(s, hostSuggest);
                    }
                }
            }
            return WithHttp(sr);
        }
        public static int serverPort
        {
            get
            {
                return GlobalSharedSettings.serverPort;
            }
            set
            {
                GlobalSharedSettings.serverPort = value;
            }
        }
        public static string kpfile = @".\wikilink\phraseScore";
        public static string wsfile = @".\wikilink\count.phrase.sense.txt";
        public static string bslfile = @".\wikilink\behavior.stoplist.txt";

        public static Dictionary <string,double> phraseScore = new Dictionary <string,double >();
        public static Dictionary<string, int> senseCount = new Dictionary<string, int>();
        public static Dictionary<string, string> senseLink = new Dictionary<string, string>();
        public static string[] behaviorStoplist = null;
        public static bool provideAnalysis = true;
        public static Thread listenerThread = null;

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
            if (listener == null) listener = new HttpListener();
            lock (listener)
            {
                string pfadd = "";
                try
                {

                    Console.WriteLine("Listener Adding:" + serverRoot);
                    listener.Prefixes.Add(WithSlash(serverRoot));
                }
                catch (Exception e)
                {
                    Console.WriteLine("FAIL Listener Adding:" + serverRoot);
                    Console.WriteLine("" + e);
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
                    Console.WriteLine("Listener Adding:" + pfadd);
                    listener.Prefixes.Add(pfadd);
                    Console.WriteLine("Listener Added:" + pfadd);
                }
                catch (Exception e)
                {
                    Console.WriteLine("FAIL Listener Adding:" + pfadd);
                    Console.WriteLine("" + e);
                }
                try
                {
                    listener.Start();
                    listenerThread = new Thread(new ThreadStart(clientListener));
                    listenerThread.Start();
                }
                catch (Exception e)
                {
                    Console.WriteLine("FAIL listener.Start()");
                    Console.WriteLine("" + e);
                }

            }
            if (provideAnalysis) loadAnalyzer();
        }

        public static void clientListener()
        {
            while(true)
            {
                try
                {
                    if (listener == null)
                    {
                        Thread.Sleep(1000);
                        Console.Error.WriteLine("clientListener : No listener Yet");
                    }
                    else
                    {
                        HttpListenerContext request = listener.GetContext();
                        ThreadPool.QueueUserWorkItem(processRequest, request);
                    }
                }
                catch(Exception e)
                {
                    Console.WriteLine("EXCEPTION: clientListener :"+e.Message);
                }
            }
        }

        // maybe http://localhost:8888/aiml/zenoaeronaut_resident/bstore/READ_ALICEINWONDERLAND.BTX
        public static void processRequest(object listenerContext)
        {
            try
            {
                processRequest0(listenerContext);
            }
            catch
            {

            }
        }

        public static void processRequest0(object listenerContext)
        {
                var context = (HttpListenerContext)listenerContext;
            try
            {
            tl_context = context;
            WebLinksWriter.tl_title = context.Request.Url.AbsoluteUri;
            tl_serverRoot = GetServerRoot(context.Request.UserHostName);

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

                //if ((context != null) && (context.Response != null))
                //    context.Response.Close();
                    
            }
            catch (Exception e)
            {
                Console.WriteLine("EXCEPTION WebServitor: {0}", e);
                if (context != null)
                {
                    var cr = context.Response;
                    cr.StatusCode = (int)HttpStatusCode.InternalServerError;
                    //context.Response.Close();
                    //context.Response.Abort();
                    var os = cr.OutputStream;
                    if (os != null && os.CanWrite)
                    {
                        new StreamWriter(os).WriteLine("<pre>" + e + "</pre>");
                    }
                    return;
                }

            }
           // finally
           // {
            if ((context != null) && (context.Response != null))
            {
                //Console.WriteLine("Webservitor processRequest close");
                context.Response.Close();
            }
            else
            {
                if ((context != null) && (context.Response == null)) Console.WriteLine("Webservitor processRequest (context.Response == null)");
                if (context == null) Console.WriteLine("Webservitor processRequest (context == null)");
            }
          //  }
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
            string qcodes = "";
            if (sections.Length >1) qcodes = sections[1];
            string filename = Path.GetFileName(justURL);
            string behaviorName = filenameToBehaviorname(filename);
            string behaviorFile = myBehaviors.behaviorDiskName(behaviorName);
            string behaviorDir = myBehaviors.persistantDirectory;
            string path = Path.Combine(startUpPath, filename);

            // The message body is posted as bytes. read the bytes
            //byte[] PostData = context.Request.BinaryRead(context.Request.ContentLength);
            Stream bodyStream = context.Request.InputStream;
            Encoding encoding = context.Request.ContentEncoding;
            string infoContentType = context.Request.ContentType;
            long infoContentLength = context.Request.ContentLength64;

            StreamReader streamReader = new StreamReader(bodyStream, encoding);
            string infoBody = streamReader.ReadToEnd();
            if (qcodes.Length > 0) infoBody = infoBody + "&"+qcodes;
            NameValueCollection NVC = WebLinksWriter.ParseQueryString(infoBody);
            if (NVC.Count == 0)
            {
                NVC = WebLinksWriter.ParseQueryString(qcodes);
            }
            if (justURL.Contains("graphmasterlist"))
            {
                WebLinksWriter.tl_AsHTML = false;
                string query = null;
                string matchtemplate = null;
                string matchpattern = null;
                string gensrai = null;
                string src = null;
                if (NVC != null)
                {
                    query = NVC["q"];
                    matchtemplate = NVC["matchtemplate"];
                    matchpattern = NVC["matchpattern"];
                    gensrai = NVC["gensrai"];
                    src = NVC["src"];
                }

                context.Response.StatusCode = (int)HttpStatusCode.OK;
                //+using (Stream s = context.Response.OutputStream )
                using (var writer = HtmlStreamWriter(context))
                {
                    int jtStartIndex = 0;
                    int jtPageSize = 51;
                    int parseTest = 0;
                    if (Int32.TryParse(NVC["jtStartIndex"], out parseTest)) jtStartIndex = parseTest;
                    if (Int32.TryParse(NVC["jtPageSize"], out parseTest)) jtPageSize = parseTest;
                    string jtSorting = NVC["jtSorting"];

                    if (!string.IsNullOrEmpty(query))
                    {

                        analyseGraphMasterJSON(writer, query, justURL, jtStartIndex, jtPageSize, jtSorting,matchpattern ,matchtemplate,gensrai,src );
                    }
                    else
                    {
                        fetchGraphMasterJSON(writer, justURL, jtStartIndex, jtPageSize,jtSorting);
                    }
                }
                return;
            }

            if (justURL.Contains("graphmasterd"))
            {
                // Delete a path
                WebLinksWriter.tl_AsHTML = false;
                string categoryPath = "*";
                Hashtable resultHash = new Hashtable();
                resultHash["Result"] = "OK";
                if (NVC != null)
                {
                    categoryPath = NVC["path"];
                }
                context.Response.StatusCode = (int)HttpStatusCode.OK;
                //+using (Stream s = context.Response.OutputStream )
                using (var writer = HtmlStreamWriter(context))
                {
                    try
                    {

                        Console.WriteLine("WEBPOST DELETE:{0}", categoryPath);
                        string gn = curBot.Graphmaster.ScriptingName;
                        if (bot.UseRapstore(gn))
                        {
                            var extDB = bot.GetGraph(gn).ensureEdb();
                            Node myNode = extDB.fetchNode(categoryPath, false);
                            myNode.templates = null;
                            extDB.saveNode(categoryPath, myNode, true);
                            //Node.addCategoryDB("", categoryPath, "", "", 1, 1, "", extDB);
                            //extDB.Close();
                            extDB.ClearCache();
                        }
                        else
                        {
                            curBot.Graphmaster.addCategory(categoryPath, "", "", 1, 1);

                        }
                    }
                    catch (Exception ex)
                    {
                        resultHash["Result"] = "ERROR";
                        resultHash["Message"] = ex.Message;
                    }
                     string jsonCode = JSON.JsonEncode(resultHash);
                      writer.WriteLine(jsonCode);
                      writer.WriteLine("");
                   }
               
                return;

            }

            if (justURL.Contains("graphmasterc"))
            {
                // Create or Update
                // may need graph
                // may need vfilename

                string state1 = "*";
                string pattern = "*";
                string that = "*";
                string graphName = "*";
                string state2 = "*";
                string topic = "*";
                string template = "<template>ok</template>";
                string vfilename = "vf:dialog";
                Hashtable resultHash = new Hashtable();
                resultHash["Result"] = "OK";
                    if (NVC != null)
                    {
                        state1 = NVC["state1"];
                        pattern = NVC["pattern"];
                        that = NVC["that"];
                        topic = NVC["topic"];
                        state2 = NVC["state2"];
                        template = NVC["template"];
                        vfilename = NVC["vfilename"];
                    }

                    context.Response.StatusCode = (int)HttpStatusCode.OK;
                    //+using (Stream s = context.Response.OutputStream )
                    using (var writer = HtmlStreamWriter(context))
                    {
                     try
                      {

                        WebLinksWriter.tl_AsHTML = false;
                        AIMLLoader loader = bot.GetBotRequest("WebServitor graphmasterc").Loader;
                        string categoryPath = loader.generatePath(graphName, pattern, that, topic, state1, state2, false);
                        Console.WriteLine("WEBPOST CREATE:{0} --> {1}:{2}", categoryPath, template, vfilename);
                        string gn = curBot.Graphmaster.ScriptingName;
                         
                        if (bot.UseRapstore(gn))
                        {
                            var extDB = bot.GetGraph(gn).ensureEdb();
                            Node.addCategoryDB("", categoryPath, template, vfilename, 1, 1, "", extDB);
                            //extDB.Close();
                            extDB.ClearCache();
                        }
                        else
                        {
                            curBot.Graphmaster.addCategory(categoryPath, template, vfilename, 1, 1);
                        }

                        long writeTime = DateTime.Now.Ticks;
                         // Just make a post loaded mod file
                        //string modFile = curBot.PersonalizePath("ZZZZ_" + Guid.NewGuid().ToString() + ".aiml");
                        string modFile = bot.PersonalizePath("ZZZZ_" + writeTime.ToString() + ".aiml");
                        var encoded = HttpUtility.HtmlEncode(categoryPath.Trim());
                        string[] header = { "<?xml version=\"1.0\" encoding=\"UTF-8\"?>", "<aiml version=\"1.0\">", " <state name=\"*\">" };
                        string serTemplate = String.Format("   <ser path=\"{0}\"> {1} </ser>", encoded, template);
                        string[] footer = { " </state>", "</aiml>" };
                        {
                            StreamWriter sw = null;
                            try
                            {
                                sw = File.CreateText(modFile);
                                foreach (string line in header)
                                {
                                    sw.WriteLine(line);
                                }
                                sw.WriteLine(serTemplate);

                                foreach (string line in footer)
                                {
                                    sw.WriteLine(line);
                                }
                                sw.Flush();
                                sw.Close();
                            }
                            catch (Exception e)
                            {

                            }
                            finally
                            {
                                if (sw != null)
                                    sw.Dispose();
                            }
                        }
                    }
                    catch (Exception ex)
                    {
                        resultHash["Result"] = "ERROR";
                        resultHash["Message"] = ex.Message;
                    }
                     string jsonCode = JSON.JsonEncode(resultHash);
                      writer.WriteLine(jsonCode);
                      writer.WriteLine("");
                   }
               
                return;
            }
          
            // Posting to an MT
            if (NVC != null)
            {
                string mt = NVC["mt"];
                if (mt != null)
                {
                    string path2 = "." + justURL;
                    string query = NVC["q"];
                    string action = NVC["a"];
                    WebLinksWriter.tl_title = path;
 //                   Console.WriteLine("WEBPOST path={0},action={1},query={2},btx={3}", path, action, query, behaviorName);
                    if (path2.Contains("./siprolog/"))
                    {
                        WebLinksWriter.tl_AsHTML = false;
                        context.Response.StatusCode = (int)HttpStatusCode.OK;
                        //+using (Stream s = context.Response.OutputStream )
                        using (var writer = HtmlStreamWriter(context))
                            ourServitor.prologEngine.webWriter(writer, action, query, mt, serverRoot);
                        return;
                    }

                }
            }

            if (path.Contains("./behavior/"))
            {
                path.Replace("./behavior/", behaviorDir);
            }
            if ((File.Exists(behaviorFile)) && (!File.Exists(path)))
            {
                path = behaviorFile;
            }


            if (path.Contains("./interpreter/"))
            {
                // Posting to the interperter directory
                // causes the text to be loaded immediately like a new AIML file

                string report = "<ok/>";
                try
                {
                    XmlDocument _xmlfile = new XmlDocument();
                    if (infoBody.Length > 0) _xmlfile.LoadXml(infoBody);
                    curBot.loadAIMLFromXML(_xmlfile, "webservice");
                    context.Response.StatusCode = (int)HttpStatusCode.OK;
                }
                catch(Exception e)
                {
                    context.Response.StatusCode = (int)HttpStatusCode.InternalServerError ;
                    report = String.Format("<error msg=\"{0}\" />{1}</error>", e.Message, e.StackTrace); 
                }
                //+using (Stream s = context.Response.OutputStream )
                using (var writer = HtmlStreamWriter(context))
                    writer.WriteLine("<ok/>");
                return;

            }
            if (context.Request.ContentType != null) 
            {
                //Convert the bytes to string using Encoding class
                //string str = Encoding.UTF8.GetString(PostData);
                myBehaviors.defineBehavior(behaviorName, infoBody);
                context.Response.StatusCode = (int)HttpStatusCode.OK;
                lock (BehaviorTree.FileLock) msg = File.ReadAllBytes(path);
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
            WebLinksWriter.tl_BodyDepth = 0;
            //GET (READ from DB)
            string [] sections = context.Request.RawUrl.Split('?');
            string justURL = sections[0];
            string filename = Path.GetFileName(justURL);
            string behaviorName = filenameToBehaviorname(filename);
            if (string.IsNullOrEmpty(behaviorName))
            {
                behaviorName = context.Request.QueryString["btx"] ?? "";
            }
            string behaviorFile = myBehaviors.behaviorDiskName(behaviorName);
            WebLinksWriter.tl_MultiPage = null;
            WebLinksWriter.tl_AsHTML = true;
            string mt = context.Request.QueryString["mt"];
            if (mt!=null)
            {
                // mulitple Mts in one read
                var mbf = prologEngine.GatherMts(mt);
                if (mbf != null && mbf.Count > 0)
                {
                    //+using (Stream s = context.Response.OutputStream )
                    using (var writer = HtmlStreamWriter(context))
                    {
                        WebLinksWriter.tl_MultiPage = writer;
                        foreach (string mtT in mbf)
                        {
                            mt = mtT;
                            WILDCARD_READ(context, justURL, behaviorName, behaviorFile, mt);
                        }
                        WebLinksWriter.tl_MultiPage = null;
                        writer.Close();
                    }
                    return;
                }
            }
            if (!File.Exists(behaviorFile) && ourServitor.myScheduler != null)
            {
                // mulitple Behavior files in one read
                var mbf = ourServitor.myScheduler.GatherTaskNames(behaviorName);
                if (mbf != null && mbf.Count > 0)
                {
                    //+using (Stream s = context.Response.OutputStream )
                    using (var writer = HtmlStreamWriter(context))
                    {
                        WebLinksWriter.tl_MultiPage = writer;
                        foreach (string behaviorT in mbf)
                        {
                            behaviorName = behaviorT;
                            behaviorFile = myBehaviors.behaviorDiskName(behaviorName);
                            WILDCARD_READ(context, justURL, behaviorName, behaviorFile, mt);
                        }
                        WebLinksWriter.tl_MultiPage = null;
                        writer.Close();
                    }
                    return;
                }
            }
            WILDCARD_READ(context, justURL, behaviorName, behaviorFile, mt);
        }

        private static void WILDCARD_READ(HttpListenerContext context, string justURL, string behaviorName,
            string behaviorFile, string mt)
        {
            string behaviorDir = myBehaviors.persistantDirectory;
            //string path = Path.Combine(startUpPath, filename);
            //string path = Path.Combine(startUpPath, context.Request.RawUrl);
            string path = "." + justURL;
            string query = context.Request.QueryString["q"];
            string action = context.Request.QueryString["a"];
            WebLinksWriter.tl_title = path;
            //Console.WriteLine("WEBGET path={0},action={1},query={2},btx={3},mt={4}", path, action, query, behaviorName,mt);
            //serverRoot
            if (path.Contains("./plot/"))
            {
                WebLinksWriter.tl_AsHTML = false;
                context.Response.StatusCode = (int) HttpStatusCode.OK;
                //+using (Stream s = context.Response.OutputStream )
                using (var writer = HtmlStreamWriter(context))
                    bot.realChem.webWriter(writer, action, query, mt, serverRoot);
                return;
            }
            if (path.Contains("./xrdf/"))
            {
                // AsHTML = false for when function already understands it is printing HTML
                WebLinksWriter.tl_AsHTML = false;
                context.Response.StatusCode = (int) HttpStatusCode.OK;
                //+using (Stream s = context.Response.OutputStream )
                using (var writer = HtmlStreamWriter(context))
                    ourServitor.myServitorEndpoint.webWriter(context, writer, action, query, path, mt, serverRoot);
                return;
            }

            if (path.Contains("./scheduler/"))
            {
                context.Response.StatusCode = (int) HttpStatusCode.OK;
                //+using (Stream s = context.Response.OutputStream )
                using (var writer = HtmlStreamWriter(context))
                    ourServitor.myScheduler.performAction(writer, action, query, behaviorName);
                return;
            }
            if (path.Contains("./siprolog/"))
            {
                WebLinksWriter.tl_AsHTML = false;
                context.Response.StatusCode = (int) HttpStatusCode.OK;
                //+using (Stream s = context.Response.OutputStream )
                try
                {
                    lock (ourServitor.prologEngine)
                    {
                        using (var writer = HtmlStreamWriter(context))
                            ourServitor.prologEngine.webWriter(writer, action, query, mt, serverRoot);
                    }
                 }
                catch
                {
                }
               return;
            }

            if (path.Contains("./analysis/"))
            {
                path.Replace("./analysis/", behaviorDir);
                if ((File.Exists(behaviorFile)) && (!File.Exists(path)))
                {
                    path = behaviorFile;
                }

                context.Response.StatusCode = (int) HttpStatusCode.OK;
                //+using (Stream s = context.Response.OutputStream )
                using (var writer = HtmlStreamWriter(context))
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

                context.Response.StatusCode = (int) HttpStatusCode.OK;
                //+using (Stream s = context.Response.OutputStream )
                using (var writer = HtmlStreamWriter(context))
                {
                    WebLinksWriter.tl_AsHTML = false;
                    if (query != null)
                    {

                        analyseGraphMaster(writer, query, justURL);
                    }
                    else
                    {
                        fetchGraphMaster(writer, justURL);
                    }
                }
                return;
            }
            if ((justURL.Contains("aimltable"))||(justURL.Contains("aimlgraph")))
            {
                WebLinksWriter.tl_AsHTML = false;
                context.Response.StatusCode = (int)HttpStatusCode.OK;
                //+using (Stream s = context.Response.OutputStream )
                try
                {
                        using (var writer = HtmlStreamWriter(context))
                            curBot.Graphmaster.webWriter(writer, action, query, mt, serverRoot);
                }
                catch
                {
                }
                return;
            }
            if (justURL.Contains("graphmasterlist"))
            {
                WebLinksWriter.tl_AsHTML = false;

                context.Response.StatusCode = (int)HttpStatusCode.OK;
                //+using (Stream s = context.Response.OutputStream )
                int jtStartIndex = Int32.Parse (context.Request.QueryString["jtStartIndex"]);
                int jtPageSize = Int32.Parse (context.Request.QueryString["jtPageSize"]);
                string jtSorting = context.Request.QueryString["jtSorting"];
                string matchpattern = context.Request.QueryString["matchpattern"];
                string matchtemplate = context.Request.QueryString["matchtemplate"];
                string gensrai = context.Request.QueryString["gensrai"];
                string src = context.Request.QueryString["src"];
                using (var writer = HtmlStreamWriter(context))
                {
                    if (!string.IsNullOrEmpty(query))
                    {
                        analyseGraphMasterJSON(writer, query, justURL, jtStartIndex, jtPageSize, jtSorting, matchpattern, matchtemplate, gensrai,src);
                    }
                    else
                    {
                        fetchGraphMasterJSON(writer, justURL,jtStartIndex ,jtPageSize,jtSorting );
                    }
                }
                return;
            }


            if (path.Contains("./analysislist/"))
            {
                WebLinksWriter.tl_AsHTML = false;
                string furi = justURL;
                //+using (Stream s = context.Response.OutputStream )
                using (var writer = HtmlStreamWriter(context))
                    try
                    {
                        context.Response.StatusCode = (int) HttpStatusCode.OK;
                        string[] fileList = Directory.GetFiles(behaviorDir);
                        string fileListString = "";
                        string uriPrefix = WithHttp(serverRoot) + "/behavior/";
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
                        writer.WriteLine("<{0}> <hasErrorMessage> \"{1}\"", furi, e.Message);
                        writer.WriteLine("<{0}> <hasErrorStackTrace> \"{1}\"", furi,
                                         e.StackTrace.Replace('\n', ' ').Replace('\r', ' '));
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
                    WebLinksWriter.tl_AsHTML = false;
                    context.Response.StatusCode = (int) HttpStatusCode.OK;
                    //context.Response.ContentLength64 = 0;
                    string[] fileList;
                    lock (BehaviorTree.FileLock)
                    {
                        fileList = Directory.GetFiles(behaviorDir);
                    }
                    string fileListString = "";
                    string uriPrefix = WithHttp(serverRoot) + "/behavior/";

                    //+using (Stream s = context.Response.OutputStream )
                    using (var writer = HtmlStreamWriter(context))
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
                    if (path.EndsWith("/listlinks/"))
                    {
                        WebLinksWriter.tl_AsHTML = true;
                        context.Response.StatusCode = (int)HttpStatusCode.OK;
                        //context.Response.ContentLength64 = 0;
                        string[] fileList;
                        lock (BehaviorTree.FileLock)
                        {
                            fileList = Directory.GetFiles(behaviorDir);
                        }
                        string fileListString = "";
                        string uriPrefix = WithHttp(serverRoot) + "/behavior/";

                        //+using (Stream s = context.Response.OutputStream )
                        using (var writer = HtmlStreamWriter(context))
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

        private static TextWriter HtmlStreamWriter(HttpListenerContext context)
        {
            return WebLinksWriter.HtmlStreamWriter(context);
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
            string behaviorFile = myBehaviors.behaviorDiskName(behaviorName);
            string behaviorDir = myBehaviors.persistantDirectory;
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
                myBehaviors.defineBehavior(behaviorName, infoBody);
                context.Response.StatusCode = (int)HttpStatusCode.OK;
                lock (BehaviorTree.FileLock) msg = File.ReadAllBytes(path);
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
            string behaviorFile = myBehaviors.behaviorDiskName(behaviorName);
            string behaviorDir = myBehaviors.persistantDirectory;
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
                curBot.Graphmaster.collectFullPaths("", allPaths);
                ourServitor.myIndex.LoadGraphMap(allPaths);
            }
        }
        public static string graphMasterToURI(string gmPath)
        {
            string uriPrefix = WithHttp(serverRoot) + "/graphmaster/";

            //string gmBase = serverRoot + "graphmaster/";
            string gmBase = uriPrefix;

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

        public static void fetchGraphMaster(TextWriter writer, string rawURL)
        {
            //loadAimlIndex();
            string gmPath = URITographMaster(rawURL);
            List<string> allPaths = new List<string>();
            curBot.Graphmaster.searchFullPaths(gmPath, "", allPaths);

            foreach (string frag in allPaths)
            {
                writer.WriteLine("");
                string fragtxt = frag;
                fragtxt = fragtxt.Replace("<", "&lt;");
                fragtxt = fragtxt.Replace(">", "&gt;");

                writer.WriteLine("{0}", fragtxt);
            }
            writer.WriteLine("");
            
        }
        public static void fetchGraphMasterJSON(TextWriter writer, string rawURL,int jtStartIndex,int jtPageSize,string jtSorting)
        {
            //loadAimlIndex();
            rawURL = rawURL.Replace("graphmasterlist", "graphmaster");
            string gmPath = URITographMaster(rawURL);
            //ArrayList allPaths = new ArrayList();
            string jsonCode = "";
            try
            {
                jsonCode = curBot.Graphmaster.searchFullPathsJSON(gmPath, "",jtStartIndex,jtPageSize,jtSorting);
            }
            catch (Exception ex)
            {
                Hashtable errorCode = new Hashtable();
                errorCode["Result"] = "ERROR";
                errorCode["Message"] = ex.Message;
                jsonCode= JSON.JsonEncode(errorCode);
            }
            writer.WriteLine(jsonCode);
            writer.WriteLine("");

        }
        public static void analyseGraphMaster(TextWriter writer, string query, string rawURL)
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
                curBot.Graphmaster.searchFullPaths(path,"", allPaths);
                string gmURI = graphMasterToURI(path);

                foreach (string frag in allPaths)
                {
                    writer.WriteLine("");
                    string fragtxt = frag;
                    fragtxt = fragtxt.Replace("<", "&lt;");
                    fragtxt = fragtxt.Replace(">", "&gt;");

                    writer.WriteLine("{0}", fragtxt);
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




        public static void analyseGraphMasterJSON(TextWriter writer, string query, string rawURL, int jtStartIndex, int jtPageSize, string jtSorting,
                string matchpattern,string matchtemplate,string gensrai,string src)
        {
            bool strictfilter = ((matchtemplate == "true") || (matchpattern == "true"));
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
            ArrayList collector = new ArrayList();

            foreach (string path in pathResults.Keys)
            {
                double irScore = pathResults[path];
                ArrayList myCollector = new ArrayList();

                curBot.Graphmaster.root.searchFullPathsShortCategory(path, "", myCollector);
                foreach (Hashtable ht in myCollector )
                {
                    ht["score"]=irScore;
                    if (strictfilter)
                    {
                        if (gensrai == "true")
                        {
                            // For Gensrai we take any mention
                            int matchingTemplatec = 0;
                            int matchingPatternc = 0;
                            foreach (string q in rawList)
                            {
                                if ((((string)ht["template"]).ToLower().Contains(q.ToLower()))) matchingTemplatec++;
                                if ((((string)ht["pattern"]).ToLower().Contains(q.ToLower()))) matchingPatternc++;
                            }
                            // Exclude those that don't match the filter
                            if (matchingTemplatec==0 && matchtemplate == "true") continue;
                            if (matchingPatternc==0 && matchpattern == "true") continue;
                        }
                        else
                        {
                            bool matchingTemplateb = true;
                            bool matchingPatternb = true;
                            foreach (string q in rawList)
                            {
                                matchingTemplateb = matchingTemplateb && (((string)ht["template"]).ToLower().Contains(q.ToLower()));
                                matchingPatternb = matchingPatternb && (((string)ht["pattern"]).ToLower().Contains(q.ToLower()));
                            }
                            // Exclude those that don't match the filter
                            if (!matchingTemplateb && matchtemplate == "true") continue;
                            if (!matchingPatternb && matchpattern == "true") continue;
                        }
                        // survived so must be good
                        if (gensrai == "true")
                        {
                            Hashtable ght = new Hashtable(ht);
                            string target = (string)ght["pattern"];
                            string newTarget = target;
                            string newPattern = LevenshteinDistance.hypotheizeSrai(src, target, out newTarget);
                            string newTemplate = "<template><srai>" + newTarget + "</srai></template>";
                            ght["pattern"] = newPattern;
                            ght["template"] = HttpUtility.HtmlEncode(newTemplate);
                            ght["vfilename"] = "vf:gensrai";
                            // NEED TO FIX "PATH"
                           collector.Add(ght);
                           if (!target.Contains("*") && !target.Contains("_"))
                           {
                               Hashtable ght2 = new Hashtable(ght);
                               string newTemplate2 = "<template><srai>" + target + "</srai></template>";
                               ght2["template"] = HttpUtility.HtmlEncode(newTemplate2);
                               collector.Add(ght2);
                           }
                        }
                        else
                        {
                            collector.Add(ht);
                        }
                    }
                    else
                    {
                        if (gensrai == "true")
                        {
                            Hashtable ght = new Hashtable(ht);
                            string target = (string)ght["pattern"];
                            string newTarget = target;
                            string newPattern = LevenshteinDistance.hypotheizeSrai(src, target, out newTarget);
                            string newTemplate = "<template><srai>" + newTarget + "</srai></template>";
                            ght["pattern"] = newPattern;
                            ght["template"] = HttpUtility.HtmlEncode(newTemplate);
                            ght["vfilename"] = "vf:gensrai";
                            // NEED TO FIX "PATH"
                            collector.Add(ght);
                            if (!target.Contains("*") && !target.Contains("_"))
                            {
                                Hashtable ght2 = new Hashtable(ght);
                                string newTemplate2 = "<template><srai>" + target + "</srai></template>";
                                ght2["template"] = HttpUtility.HtmlEncode(newTemplate2);
                                collector.Add(ght2);
                            }
                        }
                        else
                        {
                            collector.Add(ht);
                        }
                        

                    }
                }
                // Max 100
                if (collector.Count >2000) {break;}
            }

            string jsonString = "";

            ArrayList result = curBot.Graphmaster.selectPageResults(collector, jtStartIndex, jtPageSize, jtSorting);
            Hashtable report = new Hashtable();
            report.Add("Result", "OK");
            report.Add("Records", result);
            report.Add("TotalRecordCount", collector.Count);
            jsonString = JSON.JsonEncode(report);
            writer.WriteLine( jsonString);
            writer.WriteLine("");
            writer.Close();
        }

        public static void loadAnalyzer()
        {
            kpfile = HostSystem.FileSystemPath(kpfile);
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
            wsfile = HostSystem.FileSystemPath(wsfile);
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

            bslfile = HostSystem.FileSystemPath(bslfile);

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

            if (behaviorStoplist != null)
            {
                foreach (string rkey in behaviorStoplist)
                {
                    if (rkey.Length > 0)
                    {
                        text = text.Replace(rkey, " ");
                    }
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

        [ThreadStatic]
        public static HttpListenerContext tl_context;

        public static void analyse(
           TextWriter writer, string path, string behaviorName,string rawURL)
        {
            var multiBehaviorName = ourServitor.myScheduler.GatherTaskNames(behaviorName);
            if (multiBehaviorName != null)
            {
                if (multiBehaviorName.Count == 0)
                {
                    writer.WriteLine("Zero tasks or behaviors from :" + behaviorName);
                    return;
                }
                foreach (string behavorT in multiBehaviorName)
                {
                    analyse(writer, path, behavorT, rawURL);
                }
                return;
            }

            WebLinksWriter.tl_title = string.Format("analyse path={0},behaviorName={1},rawURL={2}", path, behaviorName,
                                     rawURL);
            Console.WriteLine(WebLinksWriter.tl_title);
            path = HostSystem.FileSystemPath(path);
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

        public static void relateToRootParent(TextWriter writer, XmlNode xmlNode, string rootURI, string nodeType,string resourcePrefix,string resourceExtension, string relation)
        {
            string selfURI = "";
            if (xmlNode.Name.ToLower() == nodeType)
            {
                string uriPrefix = "";
                if (serverRoot.Contains("http:"))
                    uriPrefix = serverRoot + resourcePrefix;
                else
                    uriPrefix = "http://" + serverRoot + "/"+ resourcePrefix;

                selfURI = uriPrefix + xmlNode.Attributes["id"].Value + resourceExtension;
                writer.WriteLine("<{0}> <{1}> <{2}> .", rootURI, relation, selfURI);
            }
            foreach (XmlNode childNode in xmlNode.ChildNodes)
            {
                relateToRootParent(writer, childNode, rootURI, nodeType, resourcePrefix, resourceExtension, relation);
            }
        }
    }

    //Using System.Collections.Specialized;
    //Using System.Net;
    //Using System.Text;

    public class RemotePoster
    {
        public string remoteURL = "http://" + WebServitor.GlobalServerHostWithPort + "/siprolog/";

        public RemotePoster(string targetURL)
        {
            remoteURL = targetURL;
        }

        public string postToMt(string action, string value, string mt)
        {
            // for actions see siprolog.webwriter0
            // actions = (append|insert|clear|query)

            var url = remoteURL;
            var nvc = new System.Collections.Specialized.NameValueCollection();
            nvc.Add("a", action);
            nvc.Add("mt", mt);
            nvc.Add("q", value);
            var client = new System.Net.WebClient();
            var data = client.UploadValues(url, nvc);
            var res = System.Text.Encoding.ASCII.GetString(data);
            return res;
        }
        public ArrayList askQuery(string query, string mt)
        {
            string rawResult = postToMt("remotequery", query, mt);
            ArrayList result = (ArrayList)JSON.JsonDecode(rawResult);
            return result;
        }
        public void exampleQuery()
        {
            Console.WriteLine("ExampleQuery Test");
            ArrayList bindingList = askQuery("heard(X)", "speechRecognitionMt");
            if (bindingList != null)
            {
                foreach (Hashtable ht in bindingList)
                {
                    foreach (string k in ht.Keys)
                    {
                        string Val = (string)ht[k];
                        if (k == "X") Console.WriteLine("X = {0}", Val);
                    }
                }
            }
            else
            {
                Console.WriteLine("No bindings returned");
            }
            Console.WriteLine("-----------");
        }
    }

}
