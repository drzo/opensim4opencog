using System;
using System.Collections.Generic;
using System.Linq;
using System.Net;
using System.Text;
using cogbot;
using HttpServer;
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
                response.AddToBody(s);
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
            _listener.Start(10);
        }

        private void _listener_404(IHttpClientContext context, IHttpRequest request, IHttpResponse response)
        {
            UUID capsID;
         
            bool success;

            string path = request.Uri.PathAndQuery;//.TrimEnd('/');
            Console.WriteLine("_listener " + path + " from " + request.RemoteEndPoint);
            if (request.UriPath.EndsWith(".ico"))
            {
                response.Status = HttpStatusCode.NotFound;
                response.Send();                
            }
            bool useHtml = true;
            string botname = GetVariable(request, "bot", _botClient.GetName());
            string cmd = GetVariable(request, "cmd", "aiml");
            string text = GetVariable(request, "text", string.Empty);
            var wrresp = new WriteLineToResponse(response);
            try
            {
                if (useHtml)
                {
                    AddToBody(response, "<html>");
                    AddToBody(response, "<head>");
                    AddToBody(response, "<title>" + botname + "</title>");
                    AddToBody(response, "</head>");
                    AddToBody(response, "<body>");
                    AddToBody(response, "<pre>");
                    AddToBody(response, "bot = " + botname);
                    AddToBody(response, "cmd = " + cmd);
                    AddToBody(response, "text = " + text);
                    AddToBody(response, "</pre>");
                    AddToBody(response, "<a href='" + request.Uri.PathAndQuery + "'>&nbsp;"
                                        + request.Uri.PathAndQuery + "&nbsp;</a>");
                }
                AddToBody(response, "<pre>");
                var res = _botClient.ExecuteCommand(cmd + " " + text, wrresp.WriteLine);
                AddToBody(response, "</pre>");
                if (useHtml)
                {
                    AddToBody(response, "</body>");
                    AddToBody(response, "</html>");
                }
            } finally
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

        static public string GetVariable(IHttpRequest request, string bot, string name)
        {
            if (request.QueryString.Contains(bot))
            {
                return request.QueryString[bot].Value;
            }
            return name;
        }

        private void _listener_Accepted(object sender, ClientAcceptedEventArgs e)
        {
            Console.WriteLine("_listener_Accepted " + e.Socket);
        }
    }
}
