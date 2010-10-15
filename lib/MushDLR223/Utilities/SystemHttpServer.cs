using System;
using System.Collections;
using System.Collections.Specialized;
using System.Diagnostics;
using System.IO;
using System.Net;
using System.Text;
using System.Threading;
using System.Web;
using HttpServer.FormDecoders;
using MushDLR223.ScriptEngines;

namespace MushDLR223.Utilities
{
    public class SystemHttpServer : IDisposable//, JobGiver
    {
        private readonly object StartupLock = new object();
        private readonly HttpListener httpListener = new HttpListener();
        private readonly AutoResetEvent listenForNextRequest = new AutoResetEvent(false);
        private static int requestCounter;
        public int PortNum { get; set; }
        public bool IsStarted { get; set; }
        private ScriptExecutorGetter getter;
        private HttpServer.HttpListener _listener;
        private ScriptExecutorGetter clientManager;
        private string defaultUser = "null";

        public bool SynchronouslyHandle;
        

        internal SystemHttpServer(ScriptExecutorGetter bc, int port)
        {
            clientManager = bc;
            PortNum = port;
            Init();
        }

        internal void Init()
        {
            HttpServerUtil.workArroundReuse(PortNum);
            try
            {
                Start();
                LogInfo("Ready for HTTPD port " + PortNum);
            }
            catch (Exception e)
            {
                LogInfo("NOT OK for HTTPD port " + PortNum + "\n" + e);
            }
        }

        // Waits for and processes one request synchronously.
        private void HandleSynchronously(object sender)
        {
            HandleSynchronously(sender, null);
        }

        private void HandleSynchronously(object sender, EventArgs e)
        {
            while (true)
            {
                // Will block here until receives a request.
                HttpListenerContext context = httpListener.GetContext();
                HttpListenerRequest request = context.Request;

                int requestNumber = Interlocked.Increment(ref requestCounter);

                LogInfo("Start: " + requestNumber + " at " + DateTime.Now);
                HandleTheClientNew(context, requestNumber);
                LogInfo("End: " + requestNumber + " at " + DateTime.Now);
            }
            Stop();
        }

        // Stop listening for new requests.
        public void Stop()
        {
            try
            {
                httpListener.Stop();
                IsStarted = false;
            }
            finally
            {
                IsStarted = httpListener.IsListening;
            }
        }

        public void Start()
        {
            lock (StartupLock)
            {
                if (IsStarted) return;
                IsStarted = true;
                try
                {
                    if (!httpListener.IsListening)
                    {
                        if (httpListener.Prefixes.Count == 0)
                        {
                            httpListener.Prefixes.Add("http://*:" + PortNum + "/");
                            httpListener.Prefixes.Add("http://*:" + PortNum + "/HttpListenerTest/");
                        }
                        httpListener.Start();
                        if (!SynchronouslyHandle)
                        {
                            // Starts up the http listener to process requests asynchronously.
                            // Move our listening loop off to a worker thread so that the GUI doesn't lock up.
                            ThreadPool.QueueUserWorkItem(Listen);
                            return;
                        }
                        else
                        {
                            ThreadPool.QueueUserWorkItem(HandleSynchronously);
                        }
                    }
                }
                catch (Exception)
                {
                    IsStarted = false;
                }
            }
        }

        // Loop here to begin processing of new requests.
        private void Listen(object state)
        {
            while (httpListener.IsListening)
            {
                httpListener.BeginGetContext(ListenerCallback, httpListener);
                listenForNextRequest.WaitOne();
            }
        }


        // Handle the processing of a request in here.
        private void ListenerCallback(IAsyncResult ar)
        {
            HttpListener hl = ar.AsyncState as HttpListener;
            HttpListenerContext context = null;

            if (hl == null) return;


            int requestNumber = Interlocked.Increment(ref requestCounter);

            try
            {
                // The EndGetContext() method, as with all Begin/End asynchronous methods in the .NET Framework,
                // blocks until there is a request to be processed or some type of data is available.
                context = hl.EndGetContext(ar);
            }
            catch (Exception ex)
            {
                // You will get an exception when httpListener.Stop() is called
                // because there will be a thread stopped waiting on the .EndGetContext()
                // method, and again, that is just the way most Begin/End asynchronous
                // methods of the .NET Framework work.
                LogInfo(ex.ToString());
                return;
            }
            finally
            {
                // Once we know we have a request (or exception), we signal the other thread
                // so that it calls the BeginGetContext() (or possibly exits if we're not
                // listening any more) method to start handling the next incoming request
                // while we continue to process this request on a different thread.
                listenForNextRequest.Set();
            }

            // ReSharper disable ConditionIsAlwaysTrueOrFalse
            if (context == null) return;
            // ReSharper restore ConditionIsAlwaysTrueOrFalse

            LogInfo("Start: " + requestNumber + " at " + DateTime.Now);
            ShowRequestProperties(context.Request);
            HandleTheClientNew(context, requestNumber);
            LogInfo("End: " + requestNumber + " at " + DateTime.Now);
        }

        public virtual void HandleTheClient(HttpListenerContext context, int requestNumber)
        {
            HttpListenerRequest request = context.Request;
            string requestData = GetRequestString(request);
            // Simulate a delay in constructing the response.
            //System.Threading.Thread.Sleep(r.Next(10000));
            WriteResponse(context, "Hi there");
        }

        public static void ShowRequestProperties(HttpListenerRequest request)
        {
            LogInfo("KeepAlive: {0}", request.KeepAlive);
            LogInfo("Local end point: '{0}'", request.LocalEndPoint);
            LogInfo("Remote end point: '{0}'", request.RemoteEndPoint);
            LogInfo("Is local? {0}", request.IsLocal);
            LogInfo("HTTP method: {0}", request.HttpMethod);
            LogInfo("Protocol version: {0}", request.ProtocolVersion);
            LogInfo("Is authenticated: {0}", request.IsAuthenticated);
            LogInfo("Is secure: {0}", request.IsSecureConnection);
            LogInfo("ContentType: {0}", request.ContentType);
            LogInfo("ServiceName: {0}", request.ServiceName);
            LogInfo("RawUrl: {0}", request.RawUrl);
            LogInfo("Url: {0}", request.Url);
            LogInfo("QueryString: {0}", request.QueryString);

        }
        public virtual void HandleTheClientNew(HttpListenerContext context, int requestNumber)
        {
            HttpListenerRequest request = context.Request;

            string requestData = GetRequestString(request);
            //  UUID capsID;
            bool success;

            string path = request.Url.PathAndQuery; //.TrimEnd('/');
            string pathd = HttpUtility.UrlDecode(request.Url.PathAndQuery); //.TrimEnd('/');
            LogInfo("_listener " + path + " from " + request.RemoteEndPoint);

            if (request.Url.AbsolutePath.EndsWith(".ico"))
            {
                WriteCode(context, HttpStatusCode.NotFound);
                return;
            }

            NameValueCollection getvars = request.QueryString;
            bool useHtml = false;
            NameValueCollection postvars = null;
            if (request.HttpMethod == "POST")
            {
                string input = null;
                using (StreamReader reader = new StreamReader(request.InputStream))
                {
                    input = reader.ReadToEnd();
                }
                postvars = HttpUtility.ParseQueryString(input);
            }

            string botname = GetVariable(getvars, postvars, "bot", () => GetVariable(getvars, postvars, "botid", null));

            ScriptExecutor _botClient = clientManager.GetScriptExecuter(botname);
            if (_botClient == null)
            {
                WriteCode(context, HttpStatusCode.ServiceUnavailable);
                return;
            }

            var response = new StringWriter(); //new WriteLineToResponse(this, response);

            // Micro-posterboard
            if (pathd.StartsWith("/posterboard"))
            {
                string slot = path;
                string value = "";
                value = _botClient.getPosterBoard(slot) as string;
                if (value != null)
                    if (value.Length > 0)
                    {
                        LogInfo(String.Format(" board response: {0} = {1}", slot, value));
                    }
                AddToBody(response, "<xml>");
                AddToBody(response, "<slot>");
                AddToBody(response, "<path>" + path + "</path>");
                AddToBody(response, "<value>" + (value ?? "") + "</value>");
                AddToBody(response, "</slot>");
                AddToBody(response, "</xml>");

                WriteResponse(context, response.ToString());
                return;
            }



            if (path.StartsWith("/?") || path.ToLower().StartsWith("/test"))
            {
                useHtml = true;
            }
            try
            {
                if (useHtml)
                {
                    AddToBody(response, "<html>");
                    AddToBody(response, "<head>");
                    botname = GetVariable(getvars, postvars, "bot", () => _botClient.GetName());

                    AddToBody(response, "<title>" + botname + "</title>");
                    AddToBody(response, "</head>");
                    AddToBody(response, "<body>");
                    AddToBody(response, "<pre>");
                    WriteKeyValues(getvars, response);
                    WriteKeyValues(postvars, response);
                    AddToBody(response, "</pre>");
                    AddToBody(response, "<a href='" + request.Url.PathAndQuery + "'>"
                                        + request.Url.PathAndQuery + "</a>");
                    AddToBody(response, "<pre>");
                }


                string cmd = GetVariable(getvars, postvars, "cmd", () => "MeNe");

                CmdResult res;
                // this is our default handler
                if (cmd != "MeNe")
                {
                    res = _botClient.ExecuteXmlCommand(cmd + " " + GetVariable(getvars, postvars, "args", () => ""),
                                                       response.WriteLine);

                }
                else
                {
                    AddToBody(response, "<xml>");
                    AddToBody(response, "\n<!-- Begin Response !-->");
                    // this is our MeNe handler
                    string username = GetVariable(getvars, postvars, "username",
                                                  () => GetVariable(getvars, postvars, "ident", null));
                    string saytext = GetVariable(getvars, postvars, "saytext", () => "missed the post");
                    string text = GetVariable(getvars, postvars, "text",
                                              () => GetVariable(getvars, postvars, "entry", () => pathd.TrimStart('/')));
                    if (text.Contains("<sapi>"))
                    {
                        // example fragment
                        // <sapi> <silence msec="100" /> <bookmark mark="anim:hello.csv"/> Hi there </sapi>
                        text = text.Replace("<sapi>", "");
                        text = text.Replace("</sapi>", "");
                        while (text.Contains("<"))
                        {
                            int p1 = text.IndexOf("<");
                            int p2 = text.IndexOf(">", p1);
                            if (p2 > p1)
                            {
                                string fragment = text.Substring(p1, (p2 + 1) - p1);
                                text = text.Replace(fragment, " ");
                            }
                        }

                    }

                    if (String.IsNullOrEmpty(username))
                    {
                        //res = _botClient.ExecuteCommand(cmd + " " + text, wrresp.WriteLine);
                        res = _botClient.ExecuteCommand("aiml @withuser " + defaultUser + " - " + text,
                                                        response.WriteLine);
                    }
                    else
                    {
                        res = _botClient.ExecuteCommand("aiml @withuser " + username + " - " + text, response.WriteLine);
                    }
                    AddToBody(response, "");
                    AddToBody(response, "\n<!-- End Response !-->");
                    AddToBody(response, "</xml>");
                }
                if (useHtml)
                {
                    AddToBody(response, "</pre>");
                    AddToBody(response, "</body>");
                    AddToBody(response, "</html>");
                }
            }
            finally
            {
                try
                {
                    WriteResponse(context, response.ToString());
                }
                catch (Exception e)
                {
                    LogInfo("Exception sening respose: " + e);
                }
            }
        }

        private void WriteKeyValues(NameValueCollection getvars, StringWriter response)
        {
            if (getvars == null) return;
            foreach (var p in getvars.AllKeys)
            {
                foreach (var item in getvars.GetValues(p))
                {
                    AddToBody(response, "" + p + " = " + item);
                }
            }
        }

        private void GetGetVars(HttpListenerRequest Request)
        {
            Request.RawUrl.ToString();

            //if ( Request.QueryString.Count != 0) GetParams = Request.QueryString;
            //return;
            int QuestionmarkPosition = Request.RawUrl.IndexOf('?');
            if (QuestionmarkPosition != -1)
            {
                string AfterQuestionmark = Request.RawUrl.Substring(QuestionmarkPosition + 1);

                string str = System.Web.HttpUtility.UrlDecode(AfterQuestionmark, Encoding.Unicode);
                // Encoding.GetEncoding("windows-1251"));//.UTF8);
                var GetParams = System.Web.HttpUtility.ParseQueryString(AfterQuestionmark, Encoding.UTF8);// Encoding.GetEncoding("windows-1251"));//str);//AfterQuestionmark);
                StringBuilder sb = new StringBuilder();
                //StringBuilder JavascriptArray = new StringBuilder();
                //JavascriptArray.Append("({");
                foreach (string Par in GetParams)
                {
                    sb.Append("{" + Par + "=" + GetParams[Par] + "} ");
                    //if (Par.Contains("ajax") || Par.Contains("div"))
                    //{
                    //    try
                    //    {
                    //        int iPar = -1;
                    //        if (int.TryParse(GetParams[Par].Trim(':', ',', '=', '\'', '(', ')', '{', '&', '\n', '\0', '\r'), out iPar))
                    //        {
                    //            if (iPar != -1)
                    //                JavascriptArray.Append("'" + Par.Trim(':', ',', '=', '\'', '(', ')', '{', '&', '\n', '\0', '\r') + "': '" + iPar.ToString() + "',");
                    //        }
                    //        else
                    //        {
                    //            JavascriptArray.Append("'" + Par.Trim(':', ',', '=', '\'', '(', ')', '{', '&', '\n', '\0', '\r') + "': '" + GetParams[Par].Trim(':', ',', '=', '\'', '(', ')', '{', '&', '\n', '\0', '\r') + "',");
                    //        }
                    //    }
                    //    catch (Exception ex)
                    //    {
                    //        log.Error("WebServer: Get Param Ajax trouble: " + ex.ToString());
                    //    }
                    //}
                }
                LogInfo("WebServer: " + " GET PARAMS " + sb.ToString());
                //JavascriptArray.Remove(JavascriptArray.Length - 1, 1);
                //JavascriptArray.Append("})");
                ////header('X-JSON: ({'.substr($o,0,-1).'})');die();
                //Response.AddHeader("X-JSON", JavascriptArray.ToString());
                //log.Debug("WebServer: " + " X-JSON: " + JavascriptArray.ToString());
            }

        }


        private NameValueCollection Params(HttpListenerRequest request)
        {
            throw new NotImplementedException();
        }

        private void WriteCode(HttpListenerContext context, HttpStatusCode statusCode)
        {
            var responseW = context.Response;
            responseW.StatusCode = (int) statusCode;
            responseW.Close();
        }

        private void AddToBody(StringWriter response, string html)
        {
            response.WriteLine(html);
        }

        private string GetVariable(NameValueCollection get, NameValueCollection post, string varname, Func<string> missing)
        {
            return GetVariable1(get, varname, () => GetVariable1(post, varname, missing));
        }
        public string GetVariable1(NameValueCollection get, string varname, Func<string> missing)
        {
            if (get == null) return missing == null ? null : missing();
            var values =  get.GetValues(varname);
            if (values == null || values.Length == 0)
            {
                return missing();
            }
            return values[0];
        }

        private void WriteResponse(HttpListenerContext context, string responseString)
        {
            // Obtain a response object
            using (HttpListenerResponse response = context.Response)
            {
                // Construct a response.
                responseString = responseString ?? InfoString(context);
                byte[] buffer = Encoding.UTF8.GetBytes(responseString);
                response.ContentLength64 = buffer.LongLength;
                response.OutputStream.Write(buffer, 0, buffer.Length);
            }
        }

        private string InfoString(HttpListenerContext context)
        {
            return
                "<html><head><title>Test From HttpListener</title></head><body>This page was generated from an HttpListener.</body></html>";
        }

        private string GetRequestString(HttpListenerRequest request)
        {
            if (request.HasEntityBody)
            {
                using (StreamReader sr = new StreamReader(request.InputStream, request.ContentEncoding))
                {
                    string requestData = sr.ReadToEnd();
                    LogInfo("GetRequestString: " + requestData);
                    return requestData;
                }
            }
            LogInfo("NoBody: " + request.HttpMethod);
            return null;
        }

        private static void LogInfo(string requestData, params object [] args)
        {
            Debug.WriteLine(DLRConsole.SafeFormat(requestData, args));
        }

        public void Dispose()
        {
            Stop();
        }
    }
}