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
        readonly object ShutDownLock = new object();
        public TcpClient tcp_client { get; set; }
        public bool ShutdownImplStarted = false;
        private NetworkStream networkStream;

        public override string ToString()
        {
            return "SingleBotTcpClient for " + botclient.GetName() + " for " + tcp_client;
        }

        public SingleBotTcpClient(TcpClient this_client, BotTcpServer server)
        {
            tcp_client = this_client;
            Server = server;
            filter = new SimEventFilterSubscriber(this, false);
            // never recieve data updates
            filter.Never.Add(SimEventType.DATA_UPDATE.ToString());
            filter.Never.Add("On-Log-Message");
            botclient = server.parent;
            WriteLine("making SingleBotTcpClient " + this_client);
        }


        public void DoLoop()
        {
            try
            {
                WriteLine("TCPDEBUG: Entering Do Loop");
                // Start loop and handle commands:
                AbortThread = Thread.CurrentThread;
                Server.parent.AddBotMessageSubscriber(filter);
                // NETWORK STREAM MUST BE OBTAINED FROM THIS THREAD
                networkStream = tcp_client.GetStream();
                tcpStreamWriter = new StreamWriter(networkStream);
                EventsEnabled = true;
                try
                {
                    tcpStreamWriter.WriteLine("<!-- Welcome to Cogbot " + botclient.GetName() + " !-->");
                    tcpStreamWriter.Flush();
                }
                catch (Exception e)
                {
                    WriteError(e);
                    return;
                }
                // rE sHARPER is wrong!
                // ReSharper disable ConditionIsAlwaysTrueOrFalse
                while (!quitRequested && tcpStreamWriter != null)
                // ReSharper restore ConditionIsAlwaysTrueOrFalse
                {
                    try
                    {
                        lock (readerSwitchLock)
                        {
                            tcpStreamReader = new StreamReader(networkStream);
                        }
                    }
                    catch (Exception e)
                    {
                        WriteError(e);
                        return;
                    }
                    try
                    {
                        tcpStreamWriter.Flush();
                    }
                    catch (Exception e)
                    {
                        WriteError(e);
                        return;
                    }
                    try
                    {
                        WriteLine("TCPDEBUG: ProcessOneCommand");
                        lock (readerSwitchLock)
                            ProcessOneCommand();
                    }
                    catch (Exception e)
                    {
                        WriteError(e);
                    }
                    try
                    {
                        tcpStreamWriter.Flush();
                    }
                    catch (Exception e)
                    {
                        WriteError(e);
                        return;
                    }
                    continue;
                }
            }
            finally
            {
                Shutdown();
            }
        }

        private void WriteError(Exception e)
        {
            WriteLine("error occured: " + e.Message);
            WriteLine("        Stack: " + e.StackTrace.ToString());
        }

        private void Shutdown()
        {
            lock (readerSwitchLock) lock (ShutDownLock) ShutdownImpl();
        }

        private void ShutdownImpl()
        {
            if (ShutdownImplStarted) return;
            ShutdownImplStarted = true;
            quitRequested = true;
            try
            {
                EventsEnabled = false;
            }
            catch (Exception) { }
            try
            {
                Server.parent.RemoveBotMessageSubscriber(filter);
            }
            catch (Exception) { }
            if (tcpStreamWriter != null)
            {
                try
                {
                    tcpStreamWriter.Flush();
                }
                catch (Exception) { }
                try
                {
                    tcpStreamWriter.Close();
                }
                catch (Exception) { }
                tcpStreamWriter = null;
            }
            try
            {
                if (tcpStreamReader != null)
                {
                    tcpStreamReader.Close();
                    tcpStreamReader = null;
                }
            }
            catch (Exception) { }
            try
            {
                networkStream.Close();
            }
            catch (Exception) { }
            try
            {
                tcp_client.Close();
            }
            catch (Exception) { }
        }

        #region SimEventSubscriber Members

        public void OnEvent(SimObjectEvent evt)
        {
            if (!EventsEnabled) return; 
            try
            {
                if (tcpStreamWriter != null)
                {
                    tcpStreamWriter.WriteLine(BotTcpServer.EventToString(evt, Server.parent));
                    tcpStreamWriter.Flush();
                }
            }
            catch (IOException e)
            {
                WriteLine("OnEvent: " + e);
                Shutdown();
            }
        }

        public void Dispose()
        {
            Shutdown();
            tcpStreamReader = null;
        }

        public bool EventsEnabled
        {
            get { return filter.EventsEnabled; }
            set { filter.EventsEnabled = value; }
        }

        private SourceLanguage GetSyntaxType()
        {
            SourceLanguage syntaxType = SourceLanguage.Unknown;
            lock (tcpStreamReader) while (syntaxType == SourceLanguage.Unknown && tcpStreamReader != null)
            {
                int peeked = tcpStreamReader.Peek();
                if (peeked == -1)
                {
                    // probe peekchar
                    System.Windows.Forms.Application.DoEvents();
                    Thread.Sleep(200);
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
            if (syntaxType == SourceLanguage.Unknown) return;

            WriteLine("TCPDEBUG: SockClient type=" + syntaxType);
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
            WriteLine("TCPDEBUG: SockClient do read text");
            string clientMessage = tcpStreamReader.ReadLine().Trim();
            WriteLine("TCPDEBUG: SockClient read text " + clientMessage);
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
            if (!p.StartsWith("TCP"))
            {
                p = "TCPDEBUG: " + p;
            }
            DLRConsole.SYSTEM_ERR_WRITELINE_REAL(p);
            botclient.WriteLine(p);
        }


        public string EvaluateCommand(string cmd)
        {
            String lowerCmd = cmd.ToLower().Trim();
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
            if (lowerCmd == "hideon")
            {
                EventsEnabled = false;
            }
            if (lowerCmd == "hideoff")
            {
                EventsEnabled = true;
            }
            if (lowerCmd == "filters")
            {
                foreach (var c in filter.Never)
                {
                    tcpStreamWriter.WriteLine("never +" + c);
                }
                foreach (var c in filter.Always)
                {
                    tcpStreamWriter.WriteLine("always +" + c);
                }            
                return "use: [always|never] [+|-]Verb";
            }
            if (lowerCmd.StartsWith("always"))
            {
                lowerCmd = lowerCmd.Substring(6).TrimStart();
                if (lowerCmd.StartsWith("+"))
                {
                    lowerCmd = lowerCmd.Substring(1).TrimStart();
                    filter.Always.Add(lowerCmd);
                    return "added to always: " + lowerCmd;
                }
                if (lowerCmd.StartsWith("-"))
                {
                    lowerCmd = lowerCmd.Substring(1).TrimStart();
                    filter.Always.Remove(lowerCmd);
                    return "removed from always: " + lowerCmd;
                }
            }
            if (lowerCmd.StartsWith("never"))
            {
                lowerCmd = lowerCmd.Substring(6).TrimStart();
                if (lowerCmd.StartsWith("+"))
                {
                    lowerCmd = lowerCmd.Substring(1).TrimStart();
                    filter.Never.Add(lowerCmd);
                    return "added to never: " + lowerCmd;
                }
                if (lowerCmd.StartsWith("-"))
                {
                    lowerCmd = lowerCmd.Substring(1).TrimStart();
                    filter.Never.Remove(lowerCmd);
                    return "removed from never: " + lowerCmd;
                }
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
            ServerPort = port;
            botclient.AddBotMessageSubscriber(this);

            //            config = parent.config;
        }



        public int ServerPort = -1;
        public int ServerPortIncr = 1;
        ///Configuration config;

        public void startSocketListener()
        {
            // The thread that accepts the Client and awaits messages

            int maxRebinds = 10;
            this.Bound = TryBind(ServerPort);
            while (maxRebinds > 0 && !this.Bound)
            {
                ServerPort = ServerPort + ServerPortIncr;
                this.Bound = TryBind(ServerPort);
                maxRebinds--;
            }
            if (!Bound)
            {
                WriteLine("Gave up on TCPServer");
                return;
            }
            else
            {
                WriteLine("Realy bound " + ServerPort + "!");
            }
            thrSvr = new Thread(tcpSrv);
            thrSvr.Name = ToString();
            // The thread calls the tcpSvr() method=
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
        private bool Bound;
        //    TcpClient tcp_client = null;
        private void tcpSrv()
        {

            try
            {
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

        private bool TryBind(int PortNumber)
        {
            // ReSharper disable AssignNullToNotNullAttribute
            tcp_socket = new TcpListener(IPAddress.Parse("0.0.0.0"), PortNumber);
            // ReSharper restore AssignNullToNotNullAttribute
            WriteLine("About to initialize port " + PortNumber);
            try
            {
                tcp_socket.Start();
                ServerPort = PortNumber;
                WriteLine("Listening for a connection... port=" + tcp_socket.LocalEndpoint);
                return true;
            }
            catch (Exception e)
            {
                WriteLine("Failed binding to " + PortNumber);
                return true;
            }
        }

        private void WriteLine(string s)
        {
            s = "TCPDEBUG: " + s;
            DLRConsole.SYSTEM_ERR_WRITELINE_REAL(s);
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
            if (!EventsEnabled) return;
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

        public bool EventsEnabled
        {
            get { return !IsDisposing; }
            set { throw new NotImplementedException(); }
        }

        #endregion
    }
}
