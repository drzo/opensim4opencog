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
using cogbot.TheOpenSims;
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
        readonly protected BotClient botclient;        
        bool quitRequested = false;
        
        public SingleBotTcpClient(TcpClient this_client, BotTcpServer server)
        {
            tcp_client = this_client;
            Server = server;
            Server.parent.AddBotMessageSubscriber(this);
            botclient = server.parent;
        }

        public void DoLoop()
        {
            try
            {
                AbortThread = Thread.CurrentThread;


                NetworkStream ns = tcp_client.GetStream();
                tcpStreamReader = new StreamReader(ns);
                tcpStreamWriter = new StreamWriter(ns);

                tcpStreamWriter.WriteLine("<!-- Welcome to Cogbot "+botclient.GetName()+" !-->");
                tcpStreamWriter.Flush();
                // Start loop and handle commands:
                try
                {
                    while (!quitRequested && tcpStreamWriter!=null)
                    {
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
                    Server.parent.RemoveBotMessageSubscriber(this);
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
                Console.WriteLine("" + e);
            }

        }

        public TcpClient tcp_client { get; set; }

        #region SimEventSubscriber Members

        public void OnEvent(SimObjectEvent evt)
        {
            if (tcpStreamWriter != null)
            {
                tcpStreamWriter.WriteLine(BotTcpServer.EventToString(evt, Server.parent));
                tcpStreamWriter.Flush();
            }
        }

        public void Dispose()
        {
            tcpStreamWriter = null;
            tcpStreamReader = null;
        }


        
        public void ProcessOneCommand()
        {
            ScopedTextReader tcpStreamReader = new ScopedTextReader(this.tcpStreamReader);
            SourceLanguage syntaxType = SourceLanguage.Unknown;
            while (syntaxType == SourceLanguage.Unknown)
            {
                int peeked = tcpStreamReader.Peek();
                if (peeked == -1)
                {
                    Thread.Sleep(100);
                    continue;
                }
                char ch = (char) peeked;

                if (Char.IsWhiteSpace(ch) || Char.IsControl(ch))
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

            Server.parent.WriteLine("SockClient: {0}", syntaxType);
            if (syntaxType == SourceLanguage.Lisp)
            {
                try
                {
                    tcpStreamWriter.WriteLine("200 " + botclient.evalLispReaderString(tcpStreamReader));
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
                    tcpStreamWriter.WriteLine(evalXMLString(tcpStreamReader));
                }
                catch (Exception e)
                {
                    WriteLine("error occured: " + e.Message);
                    WriteLine("        Stack: " + e.StackTrace.ToString());
                    tcpStreamWriter.WriteLine("<error><response></response><errormsg>" + e.Message.ToString() +
                                              "</errormsg>\n<stack>\n" + e + "\n</stack>\n</error>");
                }
            }
            string clientMessage = tcpStreamReader.ReadLine().Trim();
            if (clientMessage.Contains("xml") || clientMessage.Contains("http:"))
            {
                tcpStreamWriter.WriteLine(EvaluateXmlCommand(clientMessage));
            }
            else
            {
                tcpStreamWriter.WriteLine(EvaluateCommand(clientMessage));
            }

        }

        public enum SourceLanguage:ushort
        {
            Unknown = 0,
            Xml = '<',
            Lisp = '(',
            Text = 'A'
        } 

        private string evalXMLString(TextReader message)
        {
            XmlReader reader = new XmlTextReader(message);
            XmlDocument xdoc = new XmlDocument();
            xdoc.Load(reader);
            throw new NotImplementedException();
        }

        public string EvaluateXmlCommand(string xcmd)
        {
            WriteLine("EvaluateXmlCommand :" + xcmd);

            string response = "<request>\r\n <cmd>" + xcmd + "</cmd>\r\n <response>null</response>\r\n</request>";
            try
            {
                if (xcmd.Contains(".xlsp"))
                {
                    return XML2Lisp(xcmd);
                }


                int depth = 0;
                XmlDocument xdoc = new XmlDocument();
                XmlTextReader reader;
                StringReader stringReader;
                if (xcmd.Contains("http:") || xcmd.Contains(".xml"))
                {
                    // assuming its a file
                    xcmd = xcmd.Trim();
                    reader = new XmlTextReader(xcmd);
                    xdoc.Load(xcmd);
                }
                else
                {
                    // otherwise just use the string
                    stringReader = new System.IO.StringReader(xcmd);
                    reader = new XmlTextReader(stringReader);
                    xdoc.LoadXml(xcmd);
                }

                Hashtable[] attributeStack = new Hashtable[16];


                string[] strURI = new String[16];
                string[] strName = new String[16];
                string[] strPath = new String[16];

                string totalResponse = "";
                for (int i = 0; i < 16; i++) { attributeStack[i] = new Hashtable(); }

                while (reader.Read())
                {
                    depth = reader.Depth + 1;
                    switch (reader.NodeType)
                    {

                        case XmlNodeType.Element:
                            //Hashtable attributes = new Hashtable();
                            strURI[depth] = reader.NamespaceURI;
                            strName[depth] = reader.Name;
                            strPath[depth] = strPath[depth - 1] + "." + strName[depth];
                            if (reader.HasAttributes)
                            {
                                for (int i = 0; i < reader.AttributeCount; i++)
                                {
                                    reader.MoveToAttribute(i);
                                    string attributeName = reader.Name;
                                    string attributeValue = reader.Value;
                                    string attributePath = "";
                                    if ((attributeName == "name") && ((strName[depth] == "param") || (strName[depth] == "feeling")))
                                    {
                                        // so you can have multiple named params
                                        strPath[depth] = strPath[depth] + "." + attributeValue;
                                    }
                                    if (depth > 1)
                                    {
                                        attributePath = strPath[depth] + "." + attributeName;
                                    }
                                    else
                                    {
                                        attributePath = attributeName;
                                    }
                                    overwrite2Hash(attributeStack[depth], attributeName, attributeValue);
                                    // zero depth contains the fully qualified nested dotted value
                                    // i.e. pet-action-plan.action.param.vector.x
                                    // i.e. pet-action-plan.action.param.entity.value
                                    overwrite2Hash(attributeStack[0], attributePath, attributeValue);
                                }
                            }
                            overwrite2Hash(attributeStack[depth], "ElementName", strName[depth]);
                            overwrite2Hash(attributeStack[depth], "Path", strPath[depth]);
                            xStartElement(strURI[depth], strName[depth], attributeStack[depth], depth, attributeStack);
                            if (reader.IsEmptyElement)
                            {
                                // do whatever EndElement would do
                                response = xEndElement(strURI[depth], strName[depth], attributeStack[depth], depth, attributeStack);
                                totalResponse += response + "\r\n";

                            }
                            break;
                        //
                        //you can handle other cases here
                        //

                        case XmlNodeType.Text:
                            // Todo
                            WriteLine(" TextNode: depth=" + depth.ToString() + "  path = " + strPath[depth - 1]); ;
                            if (reader.Name == "param")
                            {
                                overwrite2Hash(attributeStack[depth], strPath[depth - 1] + ".param." + strName[depth] + ".InnerText", reader.Value);
                                overwrite2Hash(attributeStack[0], strPath[depth - 1] + ".param." + strName[depth] + ".InnerText", reader.Value);
                            }
                            else
                            {

                                overwrite2Hash(attributeStack[depth], strPath[depth - 1] + ".InnerText", reader.Value);
                                overwrite2Hash(attributeStack[0], strPath[depth - 1] + ".InnerText", reader.Value);
                            }
                            break;

                        case XmlNodeType.EndElement:
                            response = xEndElement(strURI[depth], strName[depth], attributeStack[depth], depth, attributeStack);
                            totalResponse += response + "\r\n";
                            // Todo
                            //depth--;
                            break;
                        default:
                            break;
                    } //switch
                } //while
                string finalResponse = "<pet-petaverse-msg>\r\n" + totalResponse + "</pet-petaverse-msg>\r\n";
                return finalResponse;
            } //try
            catch (Exception e)
            {
                WriteLine("error occured: " + e.Message);
                WriteLine("        Stack: " + e.StackTrace.ToString());
                return "<error><response>" + response + "</response><errormsg>" + e.Message.ToString() + "</errormsg> </error>";
            }
        }

        public void xStartElement(string strURI, string strName, Hashtable attributes, int depth, Hashtable[] attributeStack)
        {
            WriteLine("   xStartElement: strURI =(" + strURI + ") strName=(" + strName + ") depth=(" + depth + ")");
        }

        private void WriteLine(string p)
        {
            botclient.WriteLine(p);
        }

        public string xEndElement(string strURI, string strName, Hashtable attributes, int depth, Hashtable[] attributeStack)
        {
            try
            {
                WriteLine("   xEndElement: strURI =(" + strURI + ") strName=(" + strName + ") depth=(" + depth + ")");
                if (strName == "action")
                {
                    string act = attributes["name"].ToString();
                    string seqid = attributes["sequence"].ToString();
                    string planID = getWithDefault(attributeStack[1], "id", "unknown");

                    if (act == "say")
                    {
                        string actCmd = act + " " + getWithDefault(attributeStack[0], ".pet-action-plan.action.InnerText", "");
                        string evalReply = EvaluateCommand(actCmd);
                        string actSignal = genActReport(planID, seqid, act, "done");
                        return actSignal;
                    }
                    if (act == "wear")
                    {
                        string actCmd = act + " " + getWithDefault(attributeStack[0], ".pet-action-plan.action.InnerText", "");
                        string evalReply = EvaluateCommand(actCmd);
                        string actSignal = genActReport(planID, seqid, act, "done");
                        return actSignal;
                    }
                    if (act == "follow")
                    {
                        string TargetName = getWithDefault(attributeStack[0], ".pet-action-plan.action.param.target.entity.value", "");

                        string actCmd = act + " " + TargetName;
                        string evalReply = EvaluateCommand(actCmd);
                        string actSignal = genActReport(planID, seqid, act, "done");
                        return actSignal;
                    }

                }
                /*
                 * if (strName == "param")
                {
                    string paramName = attributes["name"].ToString();
                    string paramType = attributes["type"].ToString();
                    string paramValue = attributes["value"].ToString();
                    string paramText = attributes["InnerText"].ToString();
                }
                 */

                return "<response>null</response>";
            }
            catch (Exception e)
            {
                WriteLine("error occured: " + e.Message);
                WriteLine("        Stack: " + e.StackTrace.ToString());
                return "<error>" + e.Message + "</error>";
            }
        }

        public string genActReport(string planID, string seqID, string act, string status)
        {
            DateTime dt = DateTime.Now;
            string actReport = "  <pet-signal pet-name='" + botclient.Self.Name.ToString()
                                       + "' pet-id='" + botclient.Self.AgentID.ToString()
                                       + "' timestamp='" + dt.ToString()
                                       + "' action-plan-id='" + planID
                                       + "' sequence='" + seqID
                                       + "' name='" + act
                                       + "' status='" + status + "'/>";
            WriteLine("actReport:" + actReport);
            return actReport;
        }

        public void overwrite2Hash(Hashtable hashTable, string key, string value)
        {
            if (hashTable.ContainsKey(key)) hashTable.Remove(key);
            hashTable.Add(key, value);
            //WriteLine("  +Hash :('" + key + "' , " + value + ")");
        }

        public string getWithDefault(Hashtable hashTable, string key, string defaultValue)
        {
            if (hashTable.ContainsKey(key)) return hashTable[key].ToString();
            return defaultValue;
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

        /// <summary>
        /// (thisClient.XML2Lisp2 "http://myserver/myservice/?q=" chatstring) 
        /// </summary>
        /// <param name="URL"></param>
        /// <param name="args"></param>
        /// <returns></returns>
        public string XML2Lisp2(string URL, string args)
        {
            return botclient.XML2Lisp2(URL, args);
        } // method: XML2Lisp2


        public string XML2Lisp(string xcmd)
        {
            return botclient.XML2Lisp(xcmd);
        }

        // private void enqueueLispTask(string lispCodeString)
        // {
        //    parent.enqueueLispTask(lispCodeString);
        //}

        //#region BotMessageSubscriber Members

        //void BotClient.BotMessageSubscriber.msgClient(string serverMessage)
        //{
        //    ////   System.Console.Out.WriteLine("msgClient: " + serverMessage);
        //    //if (!IsEventClientConnected())
        //    //{
        //        lock (whileClientIsAway)
        //            whileClientIsAway.Enqueue(serverMessage);
        //        return;
        //    //}
        //    //if (IsEventClientConnected())
        //    //{
        //    //    lock (tcpStreamWriter)
        //    //    {
        //    //        if (serverMessage != "")
        //    //            tcpStreamWriter.WriteLine(serverMessage);

        //    //        tcpStreamWriter.WriteLine();
        //    //        ns.Write(Encoding.ASCII.GetBytes(serverMessage.ToCharArray()),
        //    //                 0, serverMessage.Length);
        //    //    }
        //    //}

        //}

        //#endregion

        //#region BotMessageSubscriber Members


        //void BotClient.BotMessageSubscriber.Dispose()
        //{
        //    ((BotTcpServer)this).closeTcpListener();
        //}

        // #endregion

        //public bool IsClientConnected()
        //{
        //    return (ns != null) && (tcpStreamWriter != null);
        //}

        //internal void taskTick(string serverMessage)
        //{
        //    if (serverMessage != "")
        //    {
        //        ((BotClient.BotMessageSubscriber)this).msgClient(serverMessage);
        //    }        
        //}

        #endregion
    }

    public class ScopedTextReader : TextReader
    {
        readonly TextReader scoped;
        public ScopedTextReader(TextReader reader)
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
            return scoped.ReadLine();
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
            return scoped.ReadLine();
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
            thrSvr.Name = "BotTcpServer for " + client;
            // The thread calls the tcpSvr() method

            thrSvr.Start();



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
                            t.Name = "ClientHandle thread for " + ClientHandle;
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
