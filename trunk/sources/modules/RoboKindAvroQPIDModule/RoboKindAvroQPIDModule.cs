using System;
using System.Collections.Generic;
using System.Text;
using System.Threading;
using Apache.Qpid.Messaging;
using Cogbot;
using Cogbot;
using Cogbot.World;
using MushDLR223.Utilities;
using OpenMetaverse;

namespace RoboKindAvroQPIDModule
{
    public class RoboKindAvroQPIDModuleMain : WorldObjectsModule, YesAutoLoad, SimEventSubscriber
    {

        public static string RK_QPID_URI = "amqp://guest:guest@default/test?brokerlist='tcp://localhost:5672'";

        /// <summary> Holds the routing key for the topic to receive test messages on. </summary>
        public static string COGBOT_CONTROL_ROUTING_KEY = "cogbot_control";

        /// <summary> Holds the routing key for the topic to receive test messages on. </summary>
        public static string COGBOT_EVENT_ROUTING_KEY = "cogbot_event";

        /// <summary> Holds the routing key for the queue to send reports to. </summary>
        public static string ROBOKIND_RESPONSE_ROUTING_KEY = "response";


        public RoboKindAvroQPIDModuleMain(BotClient _parent)
            : base(_parent)
        {

        }

        readonly List<object> cogbotSendersToNotSendToCogbot = new List<object>();

        public static bool DISABLE_AVRO = true;
        public static string REPORT_REQUEST = "REPORT_REQUEST";
        private RoboKindListener RK_listener;
        private RoboKindPublisher RK_publisher;
        public bool DisableSpam = true;


        public bool LogEventFromCogbot(object sender, CogbotEvent evt)
        {
            if (DISABLE_AVRO) return true;
            EnsureStarted();
            if (evt.Sender == this) return false;
            if (!IsQPIDRunning) return false;
            if (!cogbotSendersToNotSendToCogbot.Contains(sender)) cogbotSendersToNotSendToCogbot.Add(sender);
            if (DisableSpam) return false;           
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
            im.Headers.SetString("verb", "" + evt.Verb);
            im.Headers.SetBoolean("personal", evt.IsPersonal != null);
            im.Headers.SetString("evstatus", "" + evt.EventStatus);
            im.Timestamp = evt.Time.ToFileTime();
            im.Type = "" + evt.EventType1;
            RK_publisher.SendMessage(im);
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
            client.SendPipelineEvent(evt);
            return true;
        }

        public void OnEvent(CogbotEvent evt)
        {
            if (DISABLE_AVRO) return;
            EnsureStarted();
            if (evt.IsEventType(SimEventType.DATA_UPDATE)) return;
            LogEventFromCogbot(evt.Sender, evt);
        }

        public override void Dispose()
        {
            EventsEnabled = false;
            if (RK_listener != null)
            {
                RK_listener.Shutdown();
                RK_listener.OnAvroMessage -= AvroReceived;
                RK_listener = null;
            }
            if (RK_publisher != null)
            {
                RK_publisher.Shutdown();
            }
            RK_publisher = null;
        }

        public bool EventsEnabled { get; set; }

        public override string GetModuleName()
        {
            return this.GetType().Namespace;
        }

        public override void StartupListener()
        {
            EnsureStarted();
        }

        private bool EnsuredStarted = false;
        private void EnsureStarted()
        {
            if (DISABLE_AVRO) return;
            if (EnsuredStarted) return;
            EnsuredStarted = true;           
            EnsureLoginToQIPD();
            client.EachSimEvent += SendEachSimEvent;
            client.AddBotMessageSubscriber(this);
            EventsEnabled = true;
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
                RK_listener.OnAvroMessage -= AvroReceived;
            }
            try
            {
                RK_listener = new RoboKindListener(uri);
                RK_publisher = new RoboKindPublisher(uri);
                RK_listener.OnAvroMessage += AvroReceived;
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
            if (msg is IBytesMessage)
            {
                IBytesMessage ibm = (IBytesMessage) msg;
                byte[] bytes = new byte[ibm.BodyLength];

            }
            DLRConsole.DebugWriteLine("msg=" + msg);
            return ACogbotEvent.CreateEvent(sender, SimEventType.Once, msg.Type, SimEventType.UNKNOWN | SimEventType.PERSONAL | SimEventType.DATA_UPDATE);
        }
    }
}