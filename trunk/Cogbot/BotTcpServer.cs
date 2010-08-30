using System;
using System.Collections.Generic;
using System.Text;
using System.Threading;
using System.Collections;
using System.IO;
using System.Net.Sockets;
using System.Net;
using System.Xml;
using cogbot.Actions;
using cogbot.ScriptEngines;
using cogbot.TheOpenSims;
using MushDLR223.ScriptEngines;
using MushDLR223.Utilities;
using OpenMetaverse;
using cogbot.Listeners;

namespace cogbot.Utilities
{

    public class TcpServerCommand : Command, BotSystemCommand
    {
        public TcpServerCommand(BotClient bc)
            : base(bc)
        {
            Name = "tcpserver";
            Description = "";
        }
        public override CmdResult Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
            // BotTcpServer btp = TheBotClient.TheTcpServer;
            throw new NotImplementedException();
        }
    }

    public class SingleBotTcpClient : SimEventSubscriber
    {
        private StreamReader tcpStreamReader = null;// = new StreamReader(ns);        
        private StreamWriter tcpStreamWriter = null;// = new StreamWriter(ns);        
        readonly BotTcpServer Server;
        public Thread AbortThread;
        readonly object readerSwitchLock = new object();
        readonly private SimEventFilterSubscriber filter;
        readonly protected BotClient botclient;        
        bool quitRequested = false;

        public override string ToString()
        {
            return "SingleBotTcpClient for " + botclient.GetName() + " for " + tcp_client;
        }

        public SingleBotTcpClient(TcpClient this_client, BotTcpServer server)
        {
            tcp_client = this_client;
            Server = server;
            filter = new SimEventFilterSubscriber(this);
            // never recieve data updates
            filter.Never.Add(SimEventType.DATA_UPDATE.ToString());
            filter.Never.Add("On-Log-Message");
            
            botclient = server.parent;
        }

        public void DoLoop()
        {
            try
            {
                AbortThread = Thread.CurrentThread;


                NetworkStream ns = tcp_client.GetStream();
                tcpStreamWriter = new StreamWriter(ns);
                tcpStreamWriter.WriteLine("<!-- Welcome to Cogbot "+botclient.GetName()+" !-->");
                Server.parent.AddBotMessageSubscriber(filter);
                tcpStreamWriter.Flush();
                // Start loop and handle commands:
                try
                {
                    while (!quitRequested && tcpStreamWriter!=null)
                    {
                        lock (readerSwitchLock)
                        {
                            tcpStreamReader = new StreamReader(ns);
                        }
                        try
                        {
                            try
                            {
                                tcpStreamWriter.Flush();
                            }
                            catch (Exception)
                            {

                                tcpStreamWriter = null;
                                quitRequested = true;
                            }
                            try
                            {
                                lock (readerSwitchLock)
                                    ProcessOneCommand();                                
                            } catch(Exception e)
                            {
                                botclient.WriteLine(this+": "+e);
                            }

                            try
                            {
                                tcpStreamWriter.Flush();
                            }
                            catch (Exception)
                            {

                                tcpStreamWriter = null;
                                quitRequested = true;
                            }
                        }
                        catch (Exception)
                        {
                            tcpStreamWriter = null;
                            quitRequested = true;
                        }
                    }
                }
                finally
                {
                    Server.parent.RemoveBotMessageSubscriber(filter);
                }

                //data = new byte[1024];
                //receivedDataLength = ns.Read(data, 0, data.Length);
                //WriteLine(Encoding.ASCII.GetString(data, 0, receivedDataLength));
                //ns.Write(data, 0, receivedDataLength);\
                try
                {
                    ns.Close();
                }
                catch (Exception) { }
                try
                {
                    tcp_client.Close();
                }
                catch (Exception) { }
                tcpStreamWriter = null;
            }
            catch (Exception e)
            {
                DLRConsole.DebugWriteLine("ERROR {0}", e);
            }

        }

        public TcpClient tcp_client { get; set; }

        #region SimEventSubscriber Members

        public void OnEvent(SimObjectEvent evt)
        {
            try
            {
                if (tcpStreamWriter != null)
                {
                    tcpStreamWriter.WriteLine(BotTcpServer.EventToString(evt, Server.parent));
                    tcpStreamWriter.Flush();
                }
            }
            catch (IOException)
            {
                quitRequested = true;
                tcpStreamWriter = null;
            }
        }

        public void Dispose()
        {
            tcpStreamWriter = null;
            tcpStreamReader = null;
        }


        private SourceLanguage GetSyntaxType()
        {
            SourceLanguage syntaxType = SourceLanguage.Unknown;
            while (syntaxType == SourceLanguage.Unknown && tcpStreamReader!=null)
            {
                int peeked = tcpStreamReader.Peek();
                if (peeked == -1)
                {
                    System.Windows.Forms.Application.DoEvents();
                    Thread.Sleep(100);
                    // PushbackReader r;
                    continue;
                }
                char ch = (char)peeked;

                if (Char.IsWhiteSpace(ch))
                {
                    peeked = tcpStreamReader.Read();
                    continue;
                }
                if (ch == '(')
                {
                    syntaxType = SourceLanguage.Lisp;
                    break;
                }
                if (ch == '<')
                {
                    syntaxType = SourceLanguage.Xml;
                    break;
                }
                syntaxType = SourceLanguage.Text;
            }
            return syntaxType;
        }
        
        public void ProcessOneCommand()
        {

            SourceLanguage syntaxType = GetSyntaxType();
            if (syntaxType==SourceLanguage.Unknown) return;

            Server.parent.WriteLine("SockClient: {0}", syntaxType);
            if (syntaxType == SourceLanguage.Lisp)
            {
                try
                {
                    tcpStreamWriter.WriteLine("200 " + botclient.evalLispReaderString(new ScopedTextReader(this.tcpStreamReader)));
                }
                catch (Exception e)
                {
                    tcpStreamWriter.WriteLine("500 \"" + e.ToString().Replace("\\", "\\\\").Replace("\"", "\\\"") + "\"");
                }
                return;
            }
            if (syntaxType == SourceLanguage.Xml)
            {
                try
                {
                    tcpStreamWriter.WriteLine(botclient.evalXMLString(new ScopedTextReader(this.tcpStreamReader)));
                }
                catch (Exception e)
                {
                    WriteLine("error occured: " + e.Message);
                    WriteLine("        Stack: " + e.StackTrace.ToString());
                    tcpStreamWriter.WriteLine("<error><response></response><errormsg>" + e.Message.ToString() +
                                              "</errormsg>\n<stack>\n" + e + "\n</stack>\n</error>");
                }
                return;
            }
            string clientMessage = tcpStreamReader.ReadLine().Trim();
            try
            {
                if (clientMessage.Contains("xml") || clientMessage.Contains("http:"))
                {
                    tcpStreamWriter.WriteLine(botclient.XmlInterp.EvaluateXmlDocument(clientMessage));
                }
                else
                {
                    tcpStreamWriter.WriteLine(EvaluateCommand(clientMessage));
                }
            }
            catch (Exception e)
            {
                tcpStreamWriter.WriteLine("500 \"" + e.ToString().Replace("\\", "\\\\").Replace("\"", "\\\"") + "\"");
            }

        }

        public enum SourceLanguage:ushort
        {
            Unknown = 0,
            Xml = '<',
            Lisp = '(',
            Text = 'A'
        } 

        private void WriteLine(string p)
        {
            botclient.WriteLine(p);
        }


        public string EvaluateCommand(string cmd)
        {
            String lowerCmd = cmd.ToLower();
            if (lowerCmd == "bye")
            {
                quitRequested = true;
                return "goodbye";
            }
            if (lowerCmd == "currentevents")
            {
                Server.GetWhileAwayAndClear(tcpStreamWriter);
                return "";
            }
            using (StringWriter wl = new StringWriter())
            {
                CmdResult s = botclient.ExecuteCommand(cmd, wl.WriteLine);
                return wl.ToString() + s;
            }
        }

        #endregion
    }

    public class ScopedTextReader : TextReader
    {
        readonly StreamReader scoped;
        public ScopedTextReader(StreamReader reader)
        {
            scoped = reader;
        }
        public override void Close()
        {
        }
        public override int Read()
        {
            return scoped.Read();
        }
        public override string ReadToEnd()
        {
            return ReadLine();
        }
        public override int Peek()
        {
            return scoped.Peek();
        }
        public override int ReadBlock(char[] buffer, int index, int count)
        {
            return scoped.ReadBlock(buffer, index, count);
        }
        public override string ReadLine()
        {
            if (NoMoreInput())
            {
                return "";
            }
            return scoped.ReadLine();
        }

        private bool NoMoreInput()
        {
            return (scoped.Peek() == -1);
        }
    }

    public class BotTcpServer : SimEventSubscriber
    {
        public bool DisableEventStore = true;// TODO this needs to be falso but running out of memory
        public Thread thrSvr;
        public BotClient parent;
        GridClient client;
        Queue<String> whileClientIsAway = new Queue<string>();
        HashSet<SingleBotTcpClient> singleBotTcpClients = new HashSet<SingleBotTcpClient>();

        public BotTcpServer(int port, BotClient botclient)
        {
            parent = botclient;
            client = botclient.gridClient;
            serverPort = port;
            botclient.AddBotMessageSubscriber(this);

            //            config = parent.config;
        }



        int serverPort = -1;
        ///Configuration config;

        public void startSocketListener()
        {
            // The thread that accepts the Client and awaits messages

            thrSvr = new Thread(tcpSrv);
            thrSvr.Name = ToString();
            // The thread calls the tcpSvr() method

            thrSvr.Start();



        }

        public override string ToString()
        {
            if (parent == null) return "BotTcpServer for NULL " + GetHashCode();
            return "BotTcpServer for " + parent.GetName();
        }
        //------------------------------------ 
        // External XML socket server
        //------------------------------------
        TcpListener tcp_socket = null;
        private bool IsDisposing;
        //    TcpClient tcp_client = null;
        private void tcpSrv()
        {

            try
            {

                //int receivedDataLength;
                byte[] data = new byte[1024];

                int PortNumber = serverPort; // 5555;
                // ReSharper disable AssignNullToNotNullAttribute
                tcp_socket = new TcpListener(IPAddress.Parse("0.0.0.0"), PortNumber);
                // ReSharper restore AssignNullToNotNullAttribute
                parent.WriteLine("About to initialize port.");
                tcp_socket.Start();
                parent.WriteLine("Listening for a connection... port=" + PortNumber);
                while (!IsDisposing)
                {
                    try
                    {

                        {
                            TcpClient ClientHandle = tcp_socket.AcceptTcpClient();
                            var clt = new SingleBotTcpClient(ClientHandle, this);
                            singleBotTcpClients.Add(clt);
                            Thread t = new Thread(new ThreadStart(clt.DoLoop));
                            t.Name = "ClientHandle thread for " + ClientHandle + " " + ToString();
                            t.Start();

                        }
                    }
                    catch (Exception e)
                    {
                        WriteLine(e.ToString());
                    }
                }
                //  tcp_socket.Stop();
                // thrSvr.Abort();

            }
            catch (Exception ee)
            {
                WriteLine(ee.ToString());
            }
        }

        private void WriteLine(string s)
        {
            parent.WriteLine(s);
        }

        public void GetWhileAwayAndClear(TextWriter tw)
        {
            lock (whileClientIsAway)
            {
                while (whileClientIsAway.Count > 0)
                {
                    tw.Write("<msgClient>");
                    tw.Write(whileClientIsAway.Dequeue());
                    tw.WriteLine("</msgClient>");
                }
            }
            tw.Flush();
        }


        public void closeTcpListener()
        {
            //    if (ns != null) ns.Close();
            //  if (tcp_client != null) tcp_client.Close();
            IsDisposing = true;
            if (thrSvr != null) thrSvr.Abort();
            if (tcp_socket != null) tcp_socket.Stop();
            // if (parent.thrJobQueue != null) parent.thrJobQueue.Abort();

        }

        #region SimEventSubscriber Members

        void SimEventSubscriber.OnEvent(SimObjectEvent evt)
        {
            if (DisableEventStore) return;
            whileClientIsAway.Enqueue(EventToString(evt, parent));
        }

        static internal string EventToString(SimObjectEvent evt, BotClient parent)
        {
            return string.Format("({0} {1})", evt.GetVerb(), parent.argsListString(evt.GetArgs()));
        }

        void SimEventSubscriber.Dispose()
        {
            IsDisposing = true;
            ((BotTcpServer)this).closeTcpListener();
        }

        #endregion
    }
}
