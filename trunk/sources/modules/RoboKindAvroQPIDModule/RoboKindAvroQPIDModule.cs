﻿using System;
using System.Collections;
using System.Collections.Generic;
using System.IO;
using System.Text;
using System.Threading;
using Apache.Qpid.Messaging;
using Avro;
using Avro.Generic;
using Avro.Specific;
using MushDLR223.ScriptEngines;
using MushDLR223.Utilities;

namespace RoboKindChat
{
    public class RoboKindAvroQPIDModuleMain : YesAutoLoad, IDisposable
    {

        public static string RK_QPID_URI = "amqp://guest:guest@default/test?brokerlist='tcp://localhost:5672'";

        /// <summary> Holds the routing key for the topic to receive test messages on. </summary>
        public static string COGBOT_CONTROL_ROUTING_KEY = "cogbot_control";

        /// <summary> Holds the routing key for the topic to receive test messages on. </summary>
        public static string COGBOT_EVENT_ROUTING_KEY = "cogbot_event";

        /// <summary> Holds the routing key for the queue to send reports to. </summary>
        public static string ROBOKIND_RESPONSE_ROUTING_KEY = "response";


        readonly List<object> cogbotSendersToNotSendToCogbot = new List<object>();

        public static bool DISABLE_AVRO = false;
        public static string REPORT_REQUEST = "REPORT_REQUEST";
        private IMessageConsumer RK_listener;
        private RoboKindConnectorQPID RK_publisher;

        public bool LogEventFromCogbot(object sender, CogbotEvent evt)
        {
            if (DISABLE_AVRO) return true;
            EnsureStarted();
            if (evt.Sender == this) return false;
            if (!IsQPIDRunning) return false;
            if (!cogbotSendersToNotSendToCogbot.Contains(sender)) cogbotSendersToNotSendToCogbot.Add(sender);          
            string ss = evt.ToEventString();
            var im = RK_publisher.CreateTextMessage(ss);
            int num = 0;
            foreach (var s in evt.Parameters)
            {
                string sKey = s.Key;
                if (!im.Headers.Contains(sKey))
                {
                    num = 0;
                }
                else
                {
                    num++;
                    sKey = sKey + "_" + num;
                    while (im.Headers.Contains(sKey))
                    {
                        num++;
                        sKey = s.Key + "_" + num;
                    }
                }
                im.Headers.SetString(sKey, "" + s.Value);
            }
            RK_publisher.SendMessage(COGBOT_EVENT_ROUTING_KEY, im);
            return false;
        }

        protected bool IsQPIDRunning
        {
            get { return RK_listener != null && RK_publisher != null; }
        }

        public bool LogEventFromRoboKind(object sender, CogbotEvent evt)
        {
            if (DISABLE_AVRO) return false;
            EnsureStarted();
            if (cogbotSendersToNotSendToCogbot.Contains(sender))
            {
                return false;
            }
            evt.Sender = sender ?? evt.Sender;
            //@todo client.SendPipelineEvent(evt);
            return true;
        }

        public void OnEvent(CogbotEvent evt)
        {
            if (DISABLE_AVRO) return;
            EnsureStarted();
            if (evt.IsEventType(SimEventType.DATA_UPDATE)) return;
            LogEventFromCogbot(evt.Sender, evt);
        }

        public void Dispose()
        {
            EventsEnabled = false;
            if (RK_listener != null)
            {
                RK_listener.Close();
                RK_listener.OnMessage -= AvroReceived;
                RK_listener.Dispose();
            }
            if (RK_publisher != null)
            {
                RK_publisher.Shutdown();
            }
            RK_publisher = null;
        }

        public bool EventsEnabled { get; set; }


        private bool EnsuredStarted = false;
        private void EnsureStarted()
        {
            if (DISABLE_AVRO) return;
            if (EnsuredStarted) return;
            EnsuredStarted = true;
            try
            {
                EnsureLoginToQIPD();
                //@todo client.EachSimEvent += SendEachSimEvent;
                //@todo client.AddBotMessageSubscriber(this);
                EventsEnabled = true;
            }
            catch (Exception)
            {
                EnsuredStarted = false;
            }
        }

        private void EnsureLoginToQIPD()
        {
            if (RK_listener != null) return;
            var uri = RoboKindAvroQPIDModuleMain.RK_QPID_URI;
            try
            {
                LoginToQPID(uri);
            }
            catch (Exception e)
            {
                System.Diagnostics.Process proc = new System.Diagnostics.Process(); // Declare New Process
                proc.StartInfo.FileName = @"QPIDServer\StartQPID.bat";
                proc.StartInfo.WindowStyle = System.Diagnostics.ProcessWindowStyle.Minimized;
                proc.StartInfo.CreateNoWindow = false;
                proc.StartInfo.ErrorDialog = true;
                //proc.StartInfo.Domain = AppDomain.CurrentDomain.Id;
                proc.Start();
                Thread.Sleep(10000);
                LoginToQPID(uri);
            }
        }

        private void LoginToQPID(string uri)
        {
            if (RK_listener != null)
            {
                RK_listener.OnMessage -= AvroReceived;
            }
            try
            {
                RK_publisher = new RoboKindConnectorQPID(uri);
                RK_publisher.GetPublisher(COGBOT_EVENT_ROUTING_KEY, null);
                RK_listener = RK_publisher.CreateListener(COGBOT_CONTROL_ROUTING_KEY, ExchangeNameDefaults.DIRECT,
                                                          AvroReceived);
            }
            catch (Exception e)
            {
                RK_listener = null;
                RK_publisher = null;
                throw e;
            }
        }

        private void SendEachSimEvent(object sender, EventArgs e)
        {
            if (!IsQPIDRunning || !EventsEnabled) return;
            if (e is CogbotEvent)
            {
                CogbotEvent cbe = (CogbotEvent)e;
                OnEvent(cbe);
                return;
            }
        }

        private void AvroReceived(IMessage msg)
        {
            if (DISABLE_AVRO) return;
            EnsureStarted();
            LogEventFromRoboKind(this, MsgToCogEvent(this, msg));
        }

        private CogbotEvent MsgToCogEvent(object sender, IMessage msg)
        {
            var evt = ACogbotEvent.CreateEvent(sender, SimEventType.Once, msg.Type, SimEventType.UNKNOWN | SimEventType.PERSONAL | SimEventType.DATA_UPDATE);
            List<NamedParam> from = ScriptManager.GetMemberValues("", msg);
            foreach (var f in from)
            {
                if (f.Type != typeof(byte[])) evt.AddParam(f.Key, f.Value);
            } 
            if (msg is IBytesMessage)
            {
                IBytesMessage ibm = (IBytesMessage)msg;
                var actual = RK_publisher.DecodeMessage(ibm);
                foreach(var f in ScriptManager.GetMemberValues("", actual))
                {
                    if (f.Type != typeof(byte[])) evt.AddParam(f.Key, f.Value);                         
                }
            }
            DLRConsole.DebugWriteLine("msg=" + evt);
            return evt;
        }


        public void Spy()
        {
            EnsureStarted();
            /*RK_publisher.CreateListener("speechRecEvent", ExchangeNameDefaults.DIRECT, (o) => Eveything(o, "direct"));
            RK_publisher.CreateListener("#", ExchangeNameDefaults.DIRECT, (o) => Eveything(o, "#direct"));
            RK_publisher.CreateListener("speechRecEvent", ExchangeNameDefaults.TOPIC, (o) => Eveything(o, "topic"));
            RK_publisher.CreateListener("#", ExchangeNameDefaults.TOPIC, (o) => Eveything(o, "#topic"));*/
            RK_publisher.CreateListener("#", "speechRecEvent", (o) => Eveything(o, "#speechRecEvent"));
            //RK_publisher.CreateListener("speechRecEvent", "speechRecEvent", (o) => Eveything(o, "speechRecEvent"));
            //RK_publisher.CreateListener("speechRecEvent", "", (o) => Eveything(o, "blank"));
            Console.WriteLine("spying on AQM");
            while (true)
            {
                System.Console.Error.Flush();
                System.Console.ReadLine();
            }
        }

        private void Eveything(IMessage msg, string type)
        {
            try
            {
                Dictionary<string, object> map = RK_publisher.DecodeMessage(msg);
                foreach (KeyValuePair<string, object> o in map)
                {
                    Console.WriteLine(o.Key + "=" + ToStr(o.Value));
                }
            }
            catch (Exception e)
            {
                Console.WriteLine(type + "=" + msg);
            }           
            System.Console.Out.Flush();
        }

        private string ToStr(object value)
        {
            if (value == null) return "<NULL>";
            if (value is IConvertible) return "" + value;
            if (value is IEnumerable)
            {
                string ars = " [ ";
                bool needsSep = false;
                foreach (object c in (IEnumerable)value)
                {
                    if (needsSep) ars += ","; else needsSep = true;
                    ars += ToStr(c);
                }
                return ars + " ] ";                
            }
            var t1 = value as KeyValuePair<string, object>?;
            if (t1.HasValue)
            {
                var o = t1.Value;
                return o.Key + "=" + ToStr(o.Value);
            }
            return "" + value;
        }
    }
}