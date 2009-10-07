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

    public class TcpServerCommand : Command,BotSystemCommand
    {
        public TcpServerCommand(BotClient bc):base(bc)
        {
            Name = "tcpserver";
            Description = "";
        }
        public override string Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
           // BotTcpServer btp = TheBotClient.TheTcpServer;
            throw new NotImplementedException();
        }
    }

    public class BotTcpServer : SimEventSubscriber
    {
        public bool DisableEventStore = true;// TODO this needs to be falso but running out of memory
        public Thread thrSvr;
        BotClient parent;
        GridClient client;
        Queue<String> whileClientIsAway = new Queue<string>();

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
                while (true)
                {
                    try
                    {
                        lock (ClientHandlerLock)
                        {
                            ClientHandle = tcp_socket.AcceptTcpClient();
                            Thread t = new Thread(new ThreadStart(OneClient));
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

        void OneClient()
        {
            TcpClient this_client = ClientHandle;
            try
            {
                NewClient(this_client);
            }
            catch (Exception e)
            {
                WriteLine(""+this_client+ " caused " + e.ToString());
            }
        }

        TcpClient ClientHandle;
        object ClientHandlerLock = new object();

        private void NewClient(TcpClient tcp_client)
        {
            string clientMessage = string.Empty;

            StreamReader tcpStreamReader = null;// = new StreamReader(ns);
            StreamWriter tcpStreamWriter = null;// = new StreamWriter(ns);
            NetworkStream ns = tcp_client.GetStream();
            tcpStreamReader = new StreamReader(ns);
            tcpStreamWriter = new StreamWriter(ns);

            tcpStreamWriter.WriteLine("<comment>Welcome to Cogbot</comment>");
            tcpStreamWriter.Flush();
            // Start loop and handle commands:
            bool quitRequested = false;
            while (!quitRequested)
            {

                clientMessage = tcpStreamReader.ReadLine().Trim();
                parent.WriteLine("SockClient: {0}", clientMessage);
                tcpStreamWriter.WriteLine();
                String lowerCmd = clientMessage.ToLower();

                try
                {
                    if (lowerCmd == "bye")
                    {
                        quitRequested = true;
                    }
                    else ProcessHttpCommand(tcpStreamWriter, clientMessage);
                }
                catch (Exception e)
                {
                    tcpStreamWriter.WriteLine("500 {0}", parent.argString(e.ToString()));
                }
                tcpStreamWriter.Flush();
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
        }

        public void ProcessHttpCommand(TextWriter tcpStreamWriter, string clientMessage)
        {
            clientMessage = clientMessage.Trim();
            if (clientMessage.ToLower() == "currentevents")
            {
                GetWhileAwayAndClear(tcpStreamWriter);
            }
            else
            {
                if (clientMessage.StartsWith("("))
                {
                    tcpStreamWriter.WriteLine("200 " + parent.evalLispString(clientMessage));

                }
                else
                    if (clientMessage.Contains("xml") || clientMessage.Contains("http:"))
                    {
                        tcpStreamWriter.WriteLine(EvaluateXmlCommand(clientMessage));
                    }
                    else
                    {
                        tcpStreamWriter.WriteLine(EvaluateCommand(clientMessage));
                    }
            }
        }


        public void GetWhileAwayAndClear(TextWriter tw)
        {                  
            lock (whileClientIsAway)
            {
                while (whileClientIsAway.Count>0)
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
            if (thrSvr != null) thrSvr.Abort();
            if (tcp_socket != null) tcp_socket.Stop();
           // if (parent.thrJobQueue != null) parent.thrJobQueue.Abort();

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
            parent.WriteLine(p);
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
            string actReport = "  <pet-signal pet-name='" + client.Self.Name.ToString()
                                       + "' pet-id='" + client.Self.AgentID.ToString()
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
            using (StringWriter wl = new StringWriter())
            {
                string s = parent.ExecuteCommand(cmd, wl.WriteLine);
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
            return parent.XML2Lisp2(URL,args);
        } // method: XML2Lisp2


        public string XML2Lisp(string xcmd)
        {
            return parent.XML2Lisp(xcmd);
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


        //void BotClient.BotMessageSubscriber.ShuttingDown()
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



        #region SimEventSubscriber Members

        void SimEventSubscriber.OnEvent(SimObjectEvent evt)
        {
            if (DisableEventStore) return;
            whileClientIsAway.Enqueue("("+evt.GetVerb()+" "+parent.argsListString(evt.GetArgs())+")");
        }

        void SimEventSubscriber.ShuttingDown()
        {
            ((BotTcpServer)this).closeTcpListener();
        }

        #endregion
    }
}
