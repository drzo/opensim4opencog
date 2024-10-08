/*
 * This is so the bot can interpret script commands written in XML.
 * It uses XMLInterpreter as a general service.
 */

using System;
using System.Collections;
using System.Collections.Generic;
using System.IO;
using System.Text;
using System.Xml;
using System.Xml.Schema;
using Cogbot.Actions;
using DotLisp;
using MushDLR223.ScriptEngines;
using MushDLR223.Utilities;

namespace Cogbot.ScriptEngines
{
    public class XmlScriptInterpreter : CommonScriptInterpreter, ScriptInterpreter
    {
        public BotClient BotClient;

        override public object Impl
        {
            get { return this; }
        }
        public override object Self
        {
            get { return BotClient; }
            set { if (value is BotClient) BotClient = value as BotClient; }
        }

        public override bool LoadsFileType(string filename)
        {
            return filename.EndsWith("xml") ||
                   base.LoadsFileType(filename);
        }

        public override void InternType(Type t)
        {
            if (BotClient == null)
            {
                if (OriginalSelf != null) return;
                ScriptManager.WriteLine(this + "cannot intern type " + t);
                return;
            }
            BotClient.InternType(t);
        }

        public override void Dispose()
        {
            BotClient.Dispose();
        }

        public override object GetSymbol(string eventName)
        {
            eventName = eventName.ToLower();
            CommandInstance o;
            BotClient.Commands.TryGetValue(eventName, out o);
            return o;
        }

        public override bool IsSubscriberOf(string eventName)
        {
            return GetSymbol(eventName) != null;
        }

        public XmlScriptInterpreter(object bc)
            : base()
        {
            if (bc is ClientManager) bc = ((ClientManager)bc).LastBotClient ?? bc;
            BotClient = bc as BotClient;
        }

        public override void Init(object self)
        {
        }
        /// <summary>
        /// 
        /// 
        /// </summary>
        /// <param name="filename"></param>
        /// <returns></returns>
        public override bool LoadFile(string filename, OutputDelegate WriteLine)
        {
            if (!File.Exists(filename)) return false;
            System.IO.FileStream f = System.IO.File.OpenRead(filename);
            StreamReader r = new StreamReader(f);
            r.BaseStream.Seek(0, SeekOrigin.Begin);
            return Read(filename, new StringReader(r.ReadToEnd()), WriteLine) != null;
        }

        /// <summary>
        /// 
        /// </summary>
        /// <param name="context_name"></param>
        /// <param name="stringCodeReader"></param>
        /// <returns></returns>
        public override object Read(string context_name, System.IO.TextReader stringCodeReader, OutputDelegate WritResulteLine)
        {
            CmdResult res = null;
            int line = 0;
            while (stringCodeReader.Peek() != -1)
            {
                line++;
                res = BotClient.ExecuteCommand(stringCodeReader.ReadLine(), context_name, WriteLine, CMDFLAGS.ForceCompletion);
            }
            return res;
        } // method: Read


        /// <summary>
        /// 
        /// </summary>
        /// <param name="codeTree"></param>
        /// <returns></returns>
        public override bool Eof(object codeTree)
        {
            if (codeTree == null) return true;
            String str = codeTree.ToString().Trim();
            return String.IsNullOrEmpty((String)codeTree);
        } // method: Eof


        /// <summary>
        /// 
        /// </summary>
        /// <param name="varname"></param>
        /// <param name="textForm"></param>
        public override void Intern(string varname, object value)
        {
            BotClient.Intern(varname, value);
        } // method: Intern


        /// <summary>
        /// 
        /// </summary>
        /// <param name="code"></param>
        /// <returns></returns>
        public override object Eval(object code)
        {
            return BotClient.ExecuteCommand(code.ToString(), CMDFLAGS.Foregrounded);
        } // method: Eval


        /// <summary>
        /// 
        /// </summary>
        /// <param name="code"></param>
        /// <returns></returns>
        public override string Str(object code)
        {
            return ScriptEventListener.argString(code);
        } // method: Str


        /// <summary>
        /// 
        /// </summary>
        /// <returns></returns>
        public override ScriptInterpreter newInterpreter(object thiz)
        {
            BotClient bc = thiz as BotClient;
            if (bc == null) bc = BotClient;

            XmlScriptInterpreter si;
            if (BotClient == null || BotClient == thiz) si = this;
            else
                si = new XmlScriptInterpreter(bc);
            si.BotClient = thiz as BotClient;
            return si;
        } // method: newInterpreter


        public string evalXMLString(TextReader message)
        {
            XmlDocumentLineInfo xdoc = new XmlDocumentLineInfo("evalXMLString: " + message, true);
            try
            {
                //readerSettings.ValidationEventHandler += new ValidationEventHandler(LLSDXmlSchemaValidationHandler);
                XmlReader reader = new XmlTextReader(message);
                xdoc.Load(reader);
            }
            catch (Exception e)
            {
                throw e;
            }
            if (xdoc.DocumentElement == null) return "no document";
            return EvaluateXmlDocument(xdoc.DocumentElement.OuterXml);
        }

        public string EvaluateXmlDocument(string xcmd)
        {
            WriteLine("EvaluateXmlDocument :" + xcmd);

            string response = "<request>\r\n <cmd>" + xcmd + "</cmd>\r\n <response>null</response>\r\n</request>";
            try
            {
                if (xcmd.Contains(".xlsp"))
                {
                    return XML2Lisp(xcmd);
                }


                int depth = 0;
                var xdoc = new XmlDocumentLineInfo();
                XmlTextReader reader;
                StringReader stringReader;
                if (xcmd.Contains("http:") || xcmd.Contains(".xml"))
                {
                    // assuming its a file
                    xcmd = xcmd.Trim();
                    reader = XmlDocumentLineInfo.CreateXmlTextFileReader(xcmd);
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

        private void WriteLine(string s, params object[] args)
        {
            BotClient.WriteLine(s, args);
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

        /// <summary>
        /// (thisClient.XML2Lisp2 "http://myserver/myservice/?q=" chatstring) 
        /// </summary>
        /// <param name="URL"></param>
        /// <param name="args"></param>
        /// <returns></returns>
        //------------------------------------ 
        // External XML socket server
        //------------------------------------

        //public void msgClient(string serverMessage)
        //{
        //    if (debugLevel>1) {
        //        WriteLine(serverMessage);             
        //    }
        //    lock (lBotMsgSubscribers)
        //    {
        //        foreach (BotMessageSubscriber ms in lBotMsgSubscribers)
        //        {
        //            ms.msgClient(serverMessage);
        //        }
        //    }
        //}

        /// <summary>
        /// (thisClient.XML2Lisp2 "http://myserver/myservice/?q=" chatstring) 
        /// </summary>
        /// <param name="URL"></param>
        /// <param name="args"></param>
        /// <returns></returns>
        public string XML2Lisp2(string URL, string args)
        {
            args = args.Replace("\\", "");
            args = args.Replace("\"", "");
            string xcmd = URL + args;
            return XML2Lisp(xcmd);
        } // method: XML2Lisp2


        public string XML2Lisp(string xcmd)
        {
            String lispCodeString = "";

            try
            {
                XmlTextReader reader;
                StringReader stringReader;

                if (xcmd.Contains("http:") || xcmd.Contains(".xml") || xcmd.Contains(".xlsp"))
                {
                    // assuming its a file
                    xcmd = xcmd.Trim();
                    reader = new XmlTextReader(xcmd);
                }
                else
                {
                    // otherwise just use the string
                    stringReader = new System.IO.StringReader(xcmd);
                    reader = new XmlTextReader(stringReader);
                }

                Hashtable[] attributeStack = new Hashtable[64];

                for (int i = 0; i < 64; i++)
                {
                    attributeStack[i] = new Hashtable();
                }
                int depth = 0;

                while (reader.Read())
                {
                    depth = reader.Depth + 1;
                    if (attributeStack[depth] == null)
                    {
                        attributeStack[depth] = new Hashtable();
                    }
                    string tagname = reader.Name;
                    switch (reader.NodeType)
                    {

                        case XmlNodeType.Element:
                            if (reader.HasAttributes)
                            {
                                for (int i = 0; i < reader.AttributeCount; i++)
                                {
                                    reader.MoveToAttribute(i);
                                    string attributeName = reader.Name;
                                    string attributeValue = reader.Value;

                                    overwrite2Hash(attributeStack[depth], attributeName, attributeValue);
                                }
                            }
                            // WriteLine(" X2L Begin(" + depth.ToString() + ") " + attributeStack[depth]["name"].ToString());
                            if (tagname == "op")
                            {
                                lispCodeString += "(" + getWithDefault(attributeStack[depth], "name", " ");
                            }
                            if (tagname == "opq")
                            {
                                lispCodeString += "'(" + getWithDefault(attributeStack[depth], "name", " ");
                            }

                            break;
                        //
                        //you can handle other cases here
                        //

                        case XmlNodeType.Text:
                            //WriteLine(" X2L TEXT(" + depth.ToString() + ") " + reader.Name);

                            // Todo
                            lispCodeString += " " + reader.Value.ToString();
                            break;

                        case XmlNodeType.EndElement:

                            if (tagname == "op")
                            {
                                lispCodeString += " )";
                            }
                            if (tagname == "opq")
                            {
                                lispCodeString += " )";
                            }

                            // Todo
                            //depth--;
                            break;

                        default:
                            break;
                    } //switch
                } //while
                WriteLine("XML2Lisp =>'" + lispCodeString + "'");
                //string results = evalLispString(lispCodeString);
                //string results = "'(enqueued)";
                return BotClient.evalLispString(lispCodeString).ToString();
                //return results;
            } //try
            catch (Exception e)
            {
                WriteLine("error occured: " + e.Message);
                WriteLine("        Stack: " + e.StackTrace.ToString());
                WriteLine("        lispCodeString: " + lispCodeString);
                return "()";
            }


        }


        private string EvaluateCommand(string cmd)
        {
            return BotClient.ExecuteCommand(cmd, CMDFLAGS.Foregrounded).ToString();
        }

        public string genActReport(string planID, string seqID, string act, string status)
        {
            DateTime dt = DateTime.Now;
            string actReport = "  <pet-signal pet-name='" + BotClient.Self.Name.ToString()
                                       + "' pet-id='" + BotClient.Self.AgentID.ToString()
                                       + "' timestamp='" + dt.ToString()
                                       + "' action-plan-id='" + planID
                                       + "' sequence='" + seqID
                                       + "' name='" + act
                                       + "' status='" + status + "'/>";
            WriteLine("actReport:" + actReport);
            return actReport;
        }

        public void xStartElement(string strURI, string strName, Hashtable attributes, int depth, Hashtable[] attributeStack)
        {
            WriteLine("   xStartElement: strURI =(" + strURI + ") strName=(" + strName + ") depth=(" + depth + ")");
        }

        public CmdResult ExecuteXmlCommand(string cmd, object session, OutputDelegate outputDelegate)
        {
            outputDelegate = outputDelegate ?? WriteLine;
            CmdResult res = null;
            outputDelegate("<xml>");
            outputDelegate("<cmdtext>" + cmd + "</cmdtext>"); //strinbg
            outputDelegate("<output>"); //string
            try
            {
                res = BotClient.ExecuteCommand(cmd, session, outputDelegate, CMDFLAGS.Foregrounded);
            }
            finally
            {
                string verb = GetType().Name;
                res = res ?? ACmdResult.Complete(verb, "cannot process " + cmd, false);
                outputDelegate("\n</output>");
                outputDelegate("<message>" + res.Message + "</message>"); //string
                outputDelegate("<success>" + res.Success + "</success>"); //True/False
                outputDelegate("<invalidArgs>" + res.InvalidArgs + "</invalidArgs>"); //True/False
                outputDelegate("<completedSynchronously>" + res.CompletedSynchronously + "</completedSynchronously>");
                //True/False
                outputDelegate("<isCompleted>" + res.IsCompleted + "</isCompleted>"); //True/False
                outputDelegate("</xml>");
            }
            return res;
        }
    }
}
