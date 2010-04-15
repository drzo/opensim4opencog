using System;
using System.Collections.Generic;
using System.Linq;
using System.Net;
using System.Net.Sockets;
using System.Text;
using System.Web;
using cogbot;
using cogbot.Actions;
using HttpServer;
using HttpServer.FormDecoders;
using OpenMetaverse;
using OpenMetaverse.Http;

namespace cogbot.Utilities
{
    //.. tonight i am writing them a webserver in .net 
    public class WriteLineToResponse
    {
        public IHttpResponse response;
        public WriteLineToResponse(IHttpResponse r)
        {
            response = r;
        }
        public void WriteLine(string str, object[] args)
        {
            string s = string.Format(str, args);
            if (response != null)
            {
                response.AddToBody(s + Environment.NewLine);
            }
            else
            {
                Console.WriteLine("no respnse object for " + s);
            }
        }
    }

    public class ClientManagerHttpServer
    {
        HttpServer.HttpListener _listener;
        private int _port;
        private BotClient _botClient;
        public ClientManagerHttpServer(BotClient bc, int port)
        {
            _botClient = bc;
            _port = port;
            Init();
        }
        public void Init()
        {
            _listener = HttpServer.HttpListener.Create(log4netLogWriter.Instance, IPAddress.Any, _port);
            _listener.Accepted += _listener_Accepted;
            _listener.Set404Handler(_listener_404);
            workArroundReuse();
            try
            {
                _listener.Start(10);
                Console.WriteLine("Ready for HTTPD port " + _port);
                _botClient.WriteLine("Ready for HTTPD port " + _port);
            }
            catch (Exception e)
            {
                _botClient.WriteLine("NOT OK for HTTPD port " + _port + "\n" + e);
            }
        }

        private void workArroundReuse()
        {
            try
            {
                TcpClient client = new TcpClient();
                client.Connect("localhost", _port);
            }
            catch
            {

            }
        }

        private void _listener_404(IHttpClientContext context, IHttpRequest request, IHttpResponse response)
        {
            UUID capsID;

            bool success;

            string path = request.Uri.PathAndQuery;//.TrimEnd('/');
            string pathd = HttpUtility.UrlDecode(request.Uri.PathAndQuery);//.TrimEnd('/');
            Console.WriteLine("_listener " + path + " from " + request.RemoteEndPoint);
            if (request.UriPath.EndsWith(".ico"))
            {
                response.Status = HttpStatusCode.NotFound;
                response.Send();
            }
            var wrresp = new WriteLineToResponse(response);

            // Micro-posterboard
            if (pathd.StartsWith("/posterboard"))
            {
                string slot = path;
                string value = "";
                if (_botClient.PosterBoard.Contains(slot))
                {
                    value = (string)_botClient.PosterBoard[slot];
                    _botClient.PosterBoard.Remove(slot); // consume the data from the queue
                }
                AddToBody(response, "<xml>");
                AddToBody(response, "<slot>");
                AddToBody(response, "<path>" + path + "</path>"); 
                AddToBody(response, "<value>" + value + "</value>");
                AddToBody(response, "</slot>");
                AddToBody(response, "</xml>");

                wrresp.response = null;
                response.Status = HttpStatusCode.OK;
                response.Send();

            }

            bool useHtml = false;
            if (request.Method == "POST")
            {
                var fdp = new FormDecoderProvider();
                fdp.Add(new MultipartDecoder());
                fdp.Add(new UrlDecoder());
                request.DecodeBody(fdp);
            }

            if (path.StartsWith("/?") || path.StartsWith("/test"))
            {
                useHtml = true;
            }
            try
            {
                if (useHtml)
                {
                    AddToBody(response, "<html>");
                    AddToBody(response, "<head>");
                    string botname = GetVariable(request, "bot", _botClient.GetName());

                    AddToBody(response, "<title>" + botname + "</title>");
                    AddToBody(response, "</head>");
                    AddToBody(response, "<body>");
                    AddToBody(response, "<pre>");
                    foreach (var p in request.Param.AsEnumerable())
                    {
                        foreach (var item in p.Values)
                        {
                            AddToBody(response, "" + p.Name + " = " + item);
                        }
                    }
                    AddToBody(response, "</pre>");
                    AddToBody(response, "<a href='" + request.Uri.PathAndQuery + "'>"
                                        + request.Uri.PathAndQuery + "</a>");
                    AddToBody(response, "<pre>");
                }


                string cmd = GetVariable(request, "cmd", "MeNe");

                CmdResult res;
                // this is our default handler
                if (cmd != "MeNe")
                {
                    res = _botClient.ExecuteXmlCommand(cmd  + " " + GetVariable(request, "args", ""), wrresp.WriteLine);

                }
                else
                {
                    AddToBody(response, "<xml>");
                    AddToBody(response, "\n<!-- Begin Response !-->");
                    // this is our MeNe handler
                    string username = GetVariable(request, "username", GetVariable(request, "ident", null));
                    string saytext = GetVariable(request, "saytext", "missed the post");
                    string text = GetVariable(request, "text", GetVariable(request, "entry", pathd.TrimStart('/')));
                    if (String.IsNullOrEmpty(username))
                    {
                        //res = _botClient.ExecuteCommand(cmd + " " + text, wrresp.WriteLine);
                        res = _botClient.ExecuteCommand("aiml @ UNKNOWN_PARTNER - " + text, wrresp.WriteLine);
                    }
                    else
                    {
                        res = _botClient.ExecuteCommand("aiml @ " + username + " - " + text, wrresp.WriteLine);
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
                wrresp.response = null;
                response.Status = HttpStatusCode.OK;
                response.Send();
            }
        }

        static public void AddToBody(IHttpResponse response, string text)
        {
            response.AddToBody(text + Environment.NewLine);
        }

        static public string GetVariable(IHttpRequest request, string varName, string defaultValue)
        {
            if (request.Param.Contains(varName))
            {
                var single = request.Param[varName].Value;
                if (!String.IsNullOrEmpty(single)) return single;
                var values = request.Param[varName].Values;
                if (values.Count > 0) return values[0];
            }
            if (request.QueryString.Contains(varName))
            {
                return HttpUtility.UrlDecode(request.QueryString[varName].Value);
            }
            return HttpUtility.UrlDecode(defaultValue);
        }

        private void _listener_Accepted(object sender, ClientAcceptedEventArgs e)
        {
            Console.WriteLine("_listener_Accepted " + e.Socket);
        }
    }
}
