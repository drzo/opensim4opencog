/*
 *
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 * 
 *   http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 *
 */
using System;
using System.Collections;
using System.Collections.Generic;
using System.IO;
using System.Reflection;
using System.Threading;
using System.Xml.Serialization;
using Apache.Qpid.Client;
using Apache.Qpid.Client.Message;
using Apache.Qpid.Framing;
using Avro;
using Avro.Generic;
using Avro.Specific;
using log4net;
using Apache.Qpid.Messaging;
using Apache.Qpid.Client.Qms;
using MushDLR223.ScriptEngines;
using MushDLR223.Utilities;
using Apache.Qpid.Buffer;
using JSONPROXY = System.Collections.Generic.Dictionary<string,object>;
using IJSONPROXY = System.Collections.Generic.IDictionary<string,object>;

namespace RoboKindAvroQPID
{
    public class RoboKindConnectorQPID
    {

        public void SpyOnQueueAndTopic(string stubName, Action<JSONPROXY> onObject)
        {
            SpyOnQueueAndTopic(stubName + "Command", stubName + "Event", onObject);
            SpyOnQueueAndTopic(stubName + "Event", stubName + "Event", onObject);
        }

        public void SpyOnQueueAndTopic(string queueName, string topicName, Action<JSONPROXY> onObject)
        {
            SpyOnQueueAndTopic(queueName, ExchangeNameDefaults.TOPIC, topicName, onObject);
            SpyOnQueueAndTopic(queueName, ExchangeNameDefaults.DIRECT, queueName, onObject);
        }

        public void SpyOnQueueAndTopic(string queueName, string exchangeName, string routingKeyOrTopicName,
                                       Action<JSONPROXY> onObject)
        {
            // queue/dest = visionproc0Command  topic == visionproc0Event  fomrnat = ImageRegionListRecord.class
            //AMQDestination cmdDest = new AMQDestination("camera0Command");

            var channel = connection.CreateChannel(false, AcknowledgeMode.NoAcknowledge);
            channel.DeclareQueue(queueName, false, false, false);
            channel.Bind(queueName, exchangeName, routingKeyOrTopicName);
            IMessageConsumer consumer = channel.CreateConsumerBuilder(queueName).Create();
            consumer.OnMessage += (mesg) =>
                                      {
                                          var map = new JSONPROXY();
                                          map["JMS_queueName"] = queueName;
                                          map["JMS_exchangeName"] = exchangeName;
                                          map["JMS_routingKey"] = routingKeyOrTopicName;
                                          DecodeIMessage(mesg, map);
                                          onObject(map);
                                      };
        }

        /// <summary>
        /// Set up a queue to listen for reports on.
        /// </summary>
        public IMessageConsumer CreateQListener(string routingKey, string exchangeNameDefaults,
                                                MessageReceivedDelegate handler)
        {
            string responseQueueName = null;
            return CreateListener(responseQueueName, routingKey, exchangeNameDefaults,
                                  ToExchangeClass(exchangeNameDefaults), false, true, true, handler);
            responseQueueName = responseQueueName ?? channel.GenerateUniqueName();
            channel.DeclareQueue(responseQueueName, false, true, true);
            // Set this listener up to listen for reports on the response queue.
            channel.Bind(responseQueueName, exchangeNameDefaults, routingKey);
            //channel.Bind(responseQueueName, "<<default>>", RESPONSE_ROUTING_KEY);
            IMessageConsumer consumer = channel.CreateConsumerBuilder(responseQueueName).Create();
            consumer.OnMessage += new MessageReceivedDelegate(handler);
            return consumer;
        }

        private string ToExchangeClass(string defaults)
        {
            if (!string.IsNullOrEmpty(defaults))
            {
                defaults = defaults.ToLower();
                if (defaults.Contains("topic")) return ExchangeClassConstants.TOPIC;
                return ExchangeClassConstants.DIRECT;
            }
            return ExchangeClassConstants.TOPIC;
        }

        /// <summary>
        /// Set up a queue to listen for reports on.
        /// </summary>
        public IMessageConsumer CreateListener(string queueName, string routingKey, string exchangeName,
                                               string exchangeClass,
                                               bool isDurable, bool isExclusive, bool autoDelete,
                                               MessageReceivedDelegate handler)
        {
            exchangeClass = exchangeClass ?? ToExchangeClass(exchangeName);
            EnsureExchange(exchangeName, exchangeClass);
            if (queueName == null)
            {
                queueName = GenerateUniqueQueue();
            }
            else
            {
                EnsureQueue(queueName, isDurable, isExclusive, autoDelete);
            }
            // Set this listener up to listen for reports on the response queue.
            try
            {
                channel.Bind(queueName, exchangeName, routingKey);
                //channel.Bind(responseQueueName, "<<default>>", RESPONSE_ROUTING_KEY);
                IMessageConsumer consumer = channel.CreateConsumerBuilder(queueName).WithExclusive(isExclusive).Create();
                consumer.OnMessage += new MessageReceivedDelegate(handler);
                return consumer;
            }
            catch (Exception)
            {
                return null;
            }
        }

        private void EnsureExchange(string exchangeName, string exchangeClass)
        {
            if (true)
            {
                return;
                channel.DeclareExchange(exchangeName, exchangeClass);
                return;
            }
            AmqChannel.StaticDeclareExchange(exchangeName, exchangeClass, false, true, false, false, true, null);
        }


        public string GenerateUniqueQueue()
        {
            string queueName = channel.GenerateUniqueName();
            bool isDurable = false;
            bool isExclusive = true;
            bool autoDelete = true;
            channel.DeclareQueue(queueName, isDurable, isExclusive, autoDelete);
            return queueName;
        }

        private bool EnsureQueue(string queueName, bool isDurable, bool isExclusive, bool autoDelete)
        {
            AmqChannel.DefaultInstance.DoQueueDeclare(queueName, isDurable, isExclusive, autoDelete, null, false);
            return true;
        }

        private static ILog log = LogManager.GetLogger(typeof (RoboKindConnectorQPID));

        /// <summary> Holds the default test timeout for broker communications before tests give up. </summary>
        private const int TIMEOUT = 10000;

        /// <summary> Holds the number of messages to send in each test run. </summary>
        //private int numMessages;

        /// <summary> Holds the number of subscribers listening to the messsages. </summary>
        //private int numSubscribers;

        /// <summary> A monitor used to wait for all reports to arrive back from consumers on. </summary>
        //private AutoResetEvent allReportsReceivedEvt = new AutoResetEvent(false);

        /// <summary> Holds the connection to listen on. </summary>
        private IConnection connection;

        public IConnection Connection
        {
            get { return connection; }
        }

        public IChannel Channel
        {
            get { return channel; }
        }

        /// <summary> Holds the channel for all test messages.</summary>
        private IChannel channel;

        /// <summary> Holds the producer to send test messages on. </summary>
        private Dictionary<string, IMessagePublisher> Publishers = new Dictionary<string, IMessagePublisher>();

        /// <summary>
        /// Creates a topic publisher that will send the specifed number of messages and expect the specifed number of report back from test
        /// subscribers.
        /// </summary>
        /// 
        /// <param name="connectionUri">The broker URL.</param>
        public RoboKindConnectorQPID(string connectionUri)
        {
            log.Debug("TopicPublisher(string connectionUri = " + connectionUri + "): called");

            // Create a connection to the broker.
            IConnectionInfo connectionInfo0 = QpidConnectionInfo.FromUrl(connectionUri);
            connection = new AMQConnection(connectionInfo0);

            // Establish a session on the broker.
            channel = connection.CreateChannel(false, AcknowledgeMode.AutoAcknowledge, 1);

            // Set up a queue to listen for reports on.
            //CreateDirectListener(channel, RoboKindAvroQPIDModuleMain.ROBOKIND_RESPONSE_ROUTING_KEY, OnMessage);
            //TopicDefault = botcontrol;


            connection.Start();
            EnsureSchemasLoaded();
            //Console.WriteLine("Sending messages and waiting for reports...");
        }

        /// <summary>
        /// Sends the test messages and waits for all subscribers to reply with a report.
        /// </summary>
        public void SendBytesMessage(string topic, byte[] value)
        {
            log.Debug("public void DoTest(): called");

            // Create a test message to send.
            IBytesMessage testMessage = channel.CreateBytesMessage();
            testMessage.WriteBytes(value);
            var publisher = GetPublisher(topic, null);
            // Send the desired number of test messages.
            //for (int i = 0; i < numMessages; i++)
            {
                publisher.Send(testMessage);
            }
            return;
        }

        /// <summary>
        /// Sends the test messages and waits for all subscribers to reply with a report.
        /// </summary>
        public ITextMessage CreateTextMessage(string text)
        {
            return channel.CreateTextMessage(text);
        }

        public void SendMessage(string topic, IMessage msg)
        {

            GetPublisher(topic, ExchangeNameDefaults.TOPIC).Send(msg);
        }

        public IMessagePublisher GetPublisher(string topic, string exchangeNameDefault)
        {
            lock (Publishers)
            {
                IMessagePublisher val;
                if (Publishers.TryGetValue(topic, out val)) return val;
                // Set up this with a producer to send the test messages and report requests on.
                var publisher = channel.CreatePublisherBuilder();
                if (exchangeNameDefault != null)
                    publisher = publisher.WithExchangeName(exchangeNameDefault);
                publisher = publisher.WithRoutingKey(topic);
                val = publisher.Create();
                Publishers[topic] = val;
                return val;
            }
        }

        /// <summary>
        /// Sends the test messages and waits for all subscribers to reply with a report.
        /// </summary>
        public void SendTextMessage(string topic, string text)
        {
            // Create a test message to send.
            IMessage testMessage = channel.CreateTextMessage(text);

            // Send the desired number of test messages.
            //for (int i = 0; i < numMessages; i++)
            {
                GetPublisher(topic, null).Send(testMessage);
            }
            return;
        }

        public void SendTestMessage(string topic, string text)
        {
            log.Debug("public void DoTest(): called");

            IMessage testMessage = channel.CreateTextMessage(text);

            // Send the desired number of test messages.
            //for (int i = 0; i < numMessages; i++)
            {
                GetPublisher(topic, null).Send(testMessage);
            }
            //log.Debug("Sent " + numMessages + " test messages.");

            // Send the report request.
            IMessage reportRequestMessage = channel.CreateTextMessage("Report request message.");
            reportRequestMessage.Headers["TYPE"] = RoboKindEventModule.REPORT_TEST;

            reportRequestMessage.Headers.SetBoolean("BOOLEAN", false);
            //reportRequestMessage.Headers.SetByte("BYTE", 5);
            reportRequestMessage.Headers.SetDouble("DOUBLE", 3.141);
            reportRequestMessage.Headers.SetFloat("FLOAT", 1.0f);
            reportRequestMessage.Headers.SetInt("INT", 1);
            reportRequestMessage.Headers.SetLong("LONG", 1);
            reportRequestMessage.Headers.SetString("STRING", "hello");
            reportRequestMessage.Headers.SetShort("SHORT", 2);

            GetPublisher(topic, null).Send(reportRequestMessage);

            // Create a test bytes message to send.
            var tbm = channel.CreateBytesMessage();


            log.Debug("Sent the report request message, waiting for all replies...");

            // Wait until all the reports come in.
            // allReportsReceivedEvt.WaitOne(TIMEOUT, true);

            // Check if all reports were really received or if the timeout occurred.
            /*  if (numSubscribers == 0)
              {
                  log.Debug("Got all reports.");
              }
              else
              {
                  log.Debug("Waiting for reports timed out, still waiting for " + numSubscribers + ".");
              }*/

            // Send the termination request.
            IMessage terminationRequestMessage = channel.CreateTextMessage("Termination request message.");
            terminationRequestMessage.Headers["TYPE"] = "TERMINATION_REQUEST";
            //publisher.Send(terminationRequestMessage);

            log.Debug("Sent the termination request message.");

            // Close all message producers and consumers and the connection to the broker.
            Shutdown();
        }

        /// <summary>
        /// Start a test subscriber. The broker URL must be specified as the first command line argument.
        /// </summary>
        /// 
        /// <param name="argv">The command line arguments, broker URL first.</param>
        public static void Main0(String[] argv)
        {
            // Create an instance of this publisher with the command line parameters.
            RoboKindConnectorQPID publisher = new RoboKindConnectorQPID(RoboKindEventModule.RK_QPID_URI);
            //, RoboKindAvroQPIDModuleMain.COGBOT_CONTROL_ROUTING_KEY

            // Publish the test messages.
            publisher.SendTestMessage(RoboKindEventModule.REPORT_TEST, "DoTest");
        }

        /// <summary> Stops the message consumers and closes the connection. </summary>
        public void Shutdown()
        {
            if (connection != null) connection.Stop();
            if (Publishers != null)
            {
                lock (Publishers)
                {
                    foreach (KeyValuePair<string, IMessagePublisher> pair in Publishers)
                    {
                        pair.Value.Dispose();
                    }
                }
            }
            if (channel != null) channel.Dispose();
            if (connection != null) connection.Dispose();
            shutdownReceivedEvt.Set();
        }


        /// <summary> Holds the producer to send report messages on. </summary>
        //private IMessagePublisher publisher;

        /// <summary> A monitor used to wait for shutdown. </summary>
        private AutoResetEvent shutdownReceivedEvt = new AutoResetEvent(false);

        /// <summary> Holds a flag to indicate that a timer has begun on the first message. Reset when report is sent. </summary> */
        private bool init;

        /// <summary> Holds the count of messages received by this listener. </summary> */
        private int count;


        /// <summary>
        /// Fired when a message is received from the broker by the consumer
        /// </summary>
        public event MessageReceivedDelegate OnAvroMessage; // { get; set; }

        /// <summary> Creates a topic listener using the specified broker URL. </summary>
        /// 
        /// <param name="connectionUri">The broker URL to listen on.</param>
        /*public RoboKindListener(string connectionUri, string botcontrol)
        {
            LogDebug("TopicListener(string connectionUri = " + connectionUri + "): called");

            // Create a connection to the broker.
            IConnectionInfo connectionInfo = QpidConnectionInfo.FromUrl(connectionUri);
            connection = new AMQConnection(connectionInfo);

            // Establish a session on the broker.
            channel = connection.CreateChannel(false, AcknowledgeMode.AutoAcknowledge, 1);
            
            // Set up a queue to listen for test messages on.
            string topicQueueName = channel.GenerateUniqueName();
            channel.DeclareQueue(topicQueueName, false, true, true);

            // Set this listener up to listen for incoming messages on the test topic queue.
            channel.Bind(topicQueueName, ExchangeNameDefaults.TOPIC, botcontrol);
            IMessageConsumer consumer = channel.CreateConsumerBuilder(topicQueueName)
                .Create();
            consumer.OnMessage += OnMessage;

            // Set up this listener with a producer to send the reports on.
            publisher = channel.CreatePublisherBuilder()
                .WithExchangeName(ExchangeNameDefaults.DIRECT)
                .WithRoutingKey(RoboKindAvroQPIDModuleMain.ROBOKIND_RESPONSE_ROUTING_KEY)
                .Create();
            publisher.Close();
            publisher = null;

            connection.Start();

        }*/

        public static void Main0L(String[] argv)
        {
            // Create an instance of this listener with the command line parameters.
            var rkl = new RoboKindConnectorQPID(RoboKindEventModule.RK_QPID_URI);
            //rkl.

            Console.WriteLine("Waiting for messages...");
            while (true)
            {
                if (rkl.shutdownReceivedEvt.WaitOne(TIMEOUT, true))
                {
                    Console.WriteLine("message was received");
                }
                else
                {
                    Console.WriteLine("timeout elapsed");
                }
            }
        }

        /// <summary> 
        /// Handles all message received by this listener. Test messages are counted, report messages result in a report being sent and
        /// shutdown messages result in this listener being terminated.
        /// </summary>
        /// 
        /// <param name="message">The received message.</param>
        public void OnMessage(IMessage message)
        {
            LogDebug("public void onMessage(Message message = " + message + "): called");

            if (OnAvroMessage != null)
            {
                OnAvroMessage(message);
                return;
            }
            // Take the start time of the first message if this is the first message.
            if (!init)
            {
                count = 0;
                init = true;
            }

            // Check if the message is a control message telling this listener to shut down.
            if (IsShutdown(message))
            {
                LogDebug("Got a shutdown message.");
                Shutdown();
            }
                // Check if the message is a report request message asking this listener to respond with the message count.
            else if (IsForCogbot(message))
            {
                LogDebug("Got a report request message.");

                // Send the message count report.
                SendReport();

                // Reset the initialization flag so that the next message is considered to be the first.
                init = false;
            }
                // Otherwise it is an ordinary test message, so increment the message count.
            else
            {
                count++;
            }

            shutdownReceivedEvt.Set();
        }

        private void LogDebug(string s)
        {
            log.Debug(s);
            Console.WriteLine(s);
        }

        /// <summary> Checks a message to see if it is a shutdown control message. </summary>
        /// 
        /// <param name="m">The message to check.</param>
        /// 
        /// <returns><tt>true</tt> if it is a shutdown control message, <tt>false</tt> otherwise.</returns>
        private bool IsShutdown(IMessage m)
        {
            bool result = CheckTextField(m, "TYPE", "TERMINATION_REQUEST");

            //LogDebug("isShutdown = " + result);

            return result;
        }

        /// <summary> Checks a message to see if it is a report request control message. </summary>
        /// 
        /// <param name="m">The message to check.</param>
        /// 
        /// <returns><tt>true</tt> if it is a report request control message, <tt>false</tt> otherwise.</returns>
        private bool IsForCogbot(IMessage m)
        {
            bool result = CheckTextField(m, "TYPE", RoboKindEventModule.REPORT_TEST);

            //LogDebug("isReport = " + result);

            return result;
        }

        /// <summary> Checks whether or not a text field on a message has the specified value. </summary>
        /// 
        /// <param name="m">The message to check.</param>
        /// <param name="fieldName">The name of the field to check.</param>
        /// <param name="value">The expected value of the field to compare with.</param>
        /// 
        /// <returns> <tt>true</tt>If the specified field has the specified value, <tt>fals</tt> otherwise. </returns>
        private static bool CheckTextField(IMessage m, string fieldName, string value)
        {
            /*LogDebug("private static boolean checkTextField(Message m = " + m + ", String fieldName = " + fieldName
                      + ", String value = " + value + "): called");*/

            string comp = m.Headers.GetString(fieldName);

            return (comp != null) && comp == value;
        }

        /// <summary> Sends the report message to the response location. </summary>
        private void SendReport()
        {
            string report = "Received " + count + ".";

            IMessage reportMessage = channel.CreateTextMessage(report);

            reportMessage.Headers.SetBoolean("BOOLEAN", false);
            //reportMessage.Headers.SetByte("BYTE", 5);
            reportMessage.Headers.SetDouble("DOUBLE", 3.141);
            reportMessage.Headers.SetFloat("FLOAT", 1.0f);
            reportMessage.Headers.SetInt("INT", 1);
            reportMessage.Headers.SetLong("LONG", 1);
            reportMessage.Headers.SetString("STRING", "hello");
            reportMessage.Headers.SetShort("SHORT", 2);

            throw new NotImplementedException();
            //publisher.Send(reportMessage);

            Console.WriteLine("Sent report: " + report);
        }

        public JSONPROXY DecodeMessage(IMessage message)
        {
            return DecodeIMessage(message, new JSONPROXY());
        }

        public JSONPROXY DecodeObject(Object message)
        {
            return DecodeMessage(message, new JSONPROXY());
        }

        public void InitTypeFilters()
        {
            if (ForSubTypes.Count == 0)
            {
                ForSubTypes.Add(typeof (ValueType));
                ForSubTypes.Add(typeof (IConvertible));
            }
            if (ExceptForSubTypes.Count == 0)
            {
                //ExceptForSubTypes.Add(typeof(Array));
                //ExceptForSubTypes.Add(typeof(byte[]));
                ExceptForSubTypes.AddRange(new[]
                                               {
                                                   typeof (byte[]),
                                                   typeof (ByteBuffer),
                                               });
            }
        }

        public JSONPROXY DecodeMessage(Object message, JSONPROXY dict)
        {
            if (message is IMessage)
            {
                return DecodeIMessage((IMessage) message, dict);
            }
            var exceptObjects = new List<object>() {message};
            return DecodeObject("", message, dict, exceptObjects);

        }

        public JSONPROXY DecodeIMessage(IMessage message, JSONPROXY dict)
        {
            var messageHeaders = message.Headers as QpidHeaders;
            var bm = message as AbstractQmsMessage;
            if (bm == null)
            {
                Console.Error.WriteLine("Non AbstractQmsMessage: " + message.GetType());
            }
            var exceptObjects = new List<object>() {message, messageHeaders};

            if (bm != null) exceptObjects.Add(bm.ContentHeaderProperties);

            GetMemberValues("Message_", message, dict, exceptObjects);
            if (bm != null)
            {
                Apache.Qpid.Framing.BasicContentHeaderProperties props = bm.ContentHeaderProperties;
                Apache.Qpid.Framing.FieldTable propsHeaders = props.Headers;
                exceptObjects.Add(propsHeaders);
                var dict2 = propsHeaders.AsDictionary();
                exceptObjects.Add(dict2);
                GetMemberValues("Message_", props, dict, exceptObjects);
                GetMemberValues("Header_", dict2, dict, exceptObjects);
            }
            if (messageHeaders != null)
            {
                var hdrs = messageHeaders._headers;
                foreach (System.Collections.DictionaryEntry hdr in hdrs)
                {
                    AddValue(JoinedName("Header_", "" + hdr.Key), hdr.Value, dict);
                }
            }
            if (message is ITextMessage)
            {
                return dict;
            }
            var o = DecodeIBytesMessage((IBytesMessage) message, dict);
            string valuePrefix = "Value_";
            return DecodeObject(valuePrefix, o, dict, exceptObjects);
        }

        public JSONPROXY DecodeObject(String valuePrefix, Object o, JSONPROXY dict, List<object> exceptObjects)
        {


            if (o is IConvertible)
            {
                dict[JoinedName(valuePrefix, "Value")] = o;
                return dict;
            }
            if (o is GenericRecord)
            {
                GenericRecord gr = (GenericRecord) o;
                IJSONPROXY contents = gr.GetContents();
                contents["SchemeNS"] = gr.Schema.Namespace;
                contents["Scheme"] = gr.Schema.Name;
                o = contents;
            }
            if (o is IJSONPROXY)
            {
                foreach (var kv in (IJSONPROXY) o)
                {
                    string joinedName = JoinedName(valuePrefix, kv.Key);
                    AddValue(joinedName, kv.Value, dict);
                }
            }
            else if (o is IDictionary)
            {
                foreach (DictionaryEntry kv in (IDictionary) o)
                {
                    string joinedName = JoinedName(valuePrefix, "" + kv.Key);
                    AddValue(joinedName, kv.Value, dict);
                }
            }
            else
            {
                GetMemberValues(valuePrefix, o, dict, exceptObjects);
            }
            return dict;
        }

        private object DeRef(object o)
        {
            bool neededDecode;
            var o2 = DecodeValue("", o, out neededDecode);
            if (!neededDecode) return o;
            return o2;
        }

        private object DecodeValue(string valuePrefix, object o, out bool neededDecode)
        {
            neededDecode = false;
            if (o is IConvertible) return o;
            if (o is NullType)
            {
                neededDecode = true;
                return null;
            }
            bool needed0;
            if (o is AMQTypedValue)
            {
                var tv = (AMQTypedValue) o;
                var tvv = tv.Value;
                if (tvv == null)
                {
                    return tv;
                }
                neededDecode = true;
                return DecodeValue(valuePrefix, tvv, out needed0);
            }
            if (o is GenericRecord)
            {
                neededDecode = true;
                GenericRecord gr = (GenericRecord) o;
                IJSONPROXY contents = gr.GetContents();
                contents["SchemeNS"] = gr.Schema.Namespace;
                contents["Scheme"] = gr.Schema.Name;
                return DecodeValue(valuePrefix, contents, out needed0);
            }
            if (o is FieldTable)
            {
                neededDecode = true;
                var contents = ((FieldTable) o).AsDictionary();
                return DecodeValue(valuePrefix, contents, out needed0);
            }
            if (o is IJSONPROXY)
            {
                IJSONPROXY dict = new JSONPROXY();
                foreach (var kv in (IJSONPROXY) o)
                {
                    string joinedName = JoinedName(valuePrefix, "" + kv.Key);
                    AddValue(joinedName, DecodeValue(joinedName + "_", kv.Value, out needed0), dict);
                    if (needed0) neededDecode = true;
                }
                if (!neededDecode)
                {
                    return o;
                }
                return dict;
            }
            if (o is IDictionary)
            {
                IJSONPROXY dict = new JSONPROXY();
                foreach (DictionaryEntry kv in (IDictionary) o)
                {
                    string joinedName = JoinedName(valuePrefix, "" + kv.Key);
                    AddValue(joinedName, DecodeValue(joinedName + "_", kv.Value, out needed0), dict);
                    if (needed0) neededDecode = true;
                }
                if (!neededDecode)
                {
                    return o;
                }
                return dict;
            }
            if (o is IEnumerable)
            {
                List<object> obj = new List<object>();
                foreach (object kv in (IEnumerable) o)
                {
                    obj.Add(DecodeValue(valuePrefix, kv, out needed0));
                    if (needed0) neededDecode = true;
                }
                if (!neededDecode)
                {
                    return o;
                }
                if (o is Array) return obj.ToArray();
                return obj;
            }
            string t = o.GetType().Name;
            return o;
        }



        private object DecodeIBytesMessage(IBytesMessage msg, JSONPROXY map)
        {
            int len = (int) msg.BodyLength;
            byte[] bytes = new byte[len];
            msg.ReadBytes(bytes, len);
            MemoryStream ins = new MemoryStream(bytes);
            Schema rs = SchemeFor(msg.ContentType, map);
            Schema ws = rs;
            var sdr = new DefaultReader(rs, ws);
            Avro.IO.Decoder dc = new Avro.IO.BinaryDecoder(ins);
            try
            {
                object actual = sdr.Read<object>(null, dc);
                Type t = actual.GetType();
                return actual;
            }
            catch (Exception e)
            {
                Console.Error.WriteLine(e);
                throw;
            }
            finally
            {
                ins.Close();
            }
        }

        private Schema SchemeFor(string type, JSONPROXY map)
        {
            var scheme = SchemeForWM(type, map);
            if (scheme == null)
            {
                object val;
                if (map.TryGetValue("JMS_queueName", out val))
                {
                    string sval = "" + val;
                    scheme = SchemeForWM(sval, map);
                    if (scheme!=null) return scheme;
                }
                Console.WriteLine("Cant find type: " + type);         
                var Loaded = RoboKindConnectorQPID.Loaded.Values;
                foreach (Schema schema0 in Loaded)
                {
                    Console.WriteLine(schema0.Name);
                }


            }
            return scheme;
        }

        private Schema SchemeForWM(string type, JSONPROXY map)
        {
            var Loaded = RoboKindConnectorQPID.Loaded.Values;
            lock (Loaded) EnsureSchemasLoaded();
            type = type.Substring(type.IndexOf('/') + 1);
            lock (Loaded)
            {
                var s = SchemeFor00(type, Loaded);
                if (s != null) return s;
                if (type.Contains("-"))
                {
                    return SchemeFor(type.Replace("-", ""), map);
                }
                if (type.Contains("."))
                {
                    return SchemeFor(type.Replace(".", ""), map);
                }
                return null;
            }
        }

        private static Schema SchemeFor00(string type, 
            IEnumerable<Schema> candidates )
        {
            type = type.Substring(type.IndexOf('/') + 1);
            var s = SchemeFor0(type, candidates);
            if (s != null) return s;
            string suffix = "Record";
            if (!type.EndsWith(suffix))
            {
                type = type + suffix;
            }
            else
            {
                type = type.Substring(0, type.Length - suffix.Length);
            }
            s = SchemeFor0(type, candidates);
            if (s != null) return s;
            return null;
        }

        private static Schema SchemeFor0(string type, IEnumerable<Schema> candidates)
        {
            foreach (Schema loaded in candidates)
            {
                if (loaded.Name == type) return loaded;
            }
            type = type.ToLower();
            foreach (Schema loaded in candidates)
            {
                if (loaded.Name.ToLower() == type) return loaded;
            }
            return null;
        }

        private static Dictionary<string, Schema> Loaded = new Dictionary<string, Schema>();
        private static Dictionary<string, Protocol> Protos = new Dictionary<string, Protocol>();

        private void EnsureSchemasLoaded()
        {
            if (Loaded.Count > 0) return;
            string startAt = @"D:\dev\hrk\apollo_trunk\avro"; // @".\HR\apollomind\avro\interpreter";

            LoadAvroFiles(startAt, false);
            LoadAvroFiles("./avro/current", true);
            //SaveOutProtocals();
        }

        private void SaveOutProtocals()
        {
            CodeGen cg = new CodeGen();
            foreach (Protocol loaded in Protos.Values)
            {
                cg.AddProtocol(loaded);
                foreach (var s in loaded.Types)
                {
                    cg.AddSchema(s);
                }
            }
            foreach (var s in Loaded.Values)
            {
                cg.AddSchema(s);
            }
            cg.GenerateCode();
            string at = @"..\sources\modules\RoboKindAvroQPIDModule\";
            Directory.CreateDirectory(at);
            cg.WriteTypes(at);
        }

        private void SaveOutScheme(Protocol protocol, Schema schema)
        {
            protocol = protocol ?? findProtocall(Protos.Values, schema);
            RecordSchema rs = schema as RecordSchema;
            if (rs != null)
            {
                string filename = "avro/" + (rs.Namespace ?? ".") + "/" + rs.Name + rs + "." + ".cs";
                //var fw = File.OpenWrite();
                TextWriter tw = new StringWriter();
                SaveOutSchemeR(protocol, rs, tw);
                var fc = tw.ToString();
                File.WriteAllText(filename, fc);
                return;
            }
        }

        private void SaveOutSchemeR(Protocol protocol, RecordSchema schema, TextWriter tw)
        {
            protocol = protocol ?? findProtocall(Protos.Values, schema);
            foreach (Field field in schema)
            {
            }
        }

        private Protocol findProtocall(IEnumerable<Protocol> protocols, Schema schema)
        {
            throw new NotImplementedException();
        }

        private void LoadAvroFiles(string startAt, bool subdirs)
        {
            LoadAvroFiles(startAt, subdirs, false, true);
            LoadAvroFiles(startAt, subdirs, true, false);
        }

        private void LoadAvroFiles(string startAt, bool subdirs, bool loadJson, bool loadavrp)
        {
            if (loadavrp)
                foreach (var file in Directory.GetFiles(startAt, "*.avpr"))
                {
                    string text = File.ReadAllText(file);
                    //var s = Schema.Parse(text);
                    var p = Protocol.Parse(text);
                    string pName = p.Name;
                    if (Protos.ContainsKey(pName))
                    {
                        continue;
                    }
                    Protos.Add(pName, p);
                    foreach (var s in p.Types)
                    {
                        AddScheme(s);
                    }
                }
            if (loadJson)
                foreach (var file in Directory.GetFiles(startAt, "*.json"))
                {
                    string text = File.ReadAllText(file);
                    var s = Schema.Parse(text);
                    AddScheme(s);
                }
            if (subdirs)
            {
                foreach (var dir in Directory.GetDirectories(startAt))
                {
                    LoadAvroFiles(dir, subdirs, loadJson, loadavrp);
                }
            }
        }

        private void AddScheme(Schema s)
        {
            if (Loaded.ContainsKey(s.Name)) return;
            Loaded.Add(s.Name, s);
        }

        public void GetMemberValues(string prefix, Object properties, JSONPROXY dict,
                                    List<Object> exceptFor)
        {
            if (properties == null)
            {
                return;
            }
            var o0 = properties;
            bool neededDecode;
            if (o0 is IJSONPROXY)
            {
                foreach (var kv in (IJSONPROXY) o0)
                {
                    string joinedName = JoinedName(prefix, kv.Key);
                    AddValue(joinedName, DecodeValue(joinedName + "_", kv.Value, out neededDecode), dict);
                }
                return;
            }
            else if (o0 is IDictionary)
            {
                foreach (DictionaryEntry kv in (IDictionary) o0)
                {
                    string joinedName = JoinedName(prefix, "" + kv.Key);
                    AddValue(joinedName, kv.Value, dict);
                }
                return;
            }
            Type t = properties.GetType();
            KeyValuePair<List<PropertyInfo>, List<FieldInfo>> vvv = GetPropsForTypes(t);
            List<string> lowerProps = new List<string>();
            BindingFlags flags = BindingFlags.Instance | BindingFlags.Public; //BindingFlags.NonPublic
            foreach (PropertyInfo o in vvv.Key)
            {
                try
                {
                    Type pt = o.PropertyType;
                    if (!IsOKType(pt)) continue;
                    var v = o.GetValue(properties, null);
                    if (v == null)
                    {
                        v = new NullType(properties, o);
                    }
                    pt = v.GetType();
                    if (!IsOKType(pt)) continue;
                    if (exceptFor.Contains(v)) continue;
                    AddValue(JoinedName(prefix, o.Name), v, dict);
                }
                catch (Exception e)
                {
                    DLRConsole.DebugWriteLine("" + e);
                }
            }
            foreach (FieldInfo o in vvv.Value)
            {
                try
                {
                    Type pt = o.FieldType;
                    if (!IsOKType(pt)) continue;
                    var v = o.GetValue(properties);
                    if (v == null)
                    {
                        v = new NullType(properties, o);
                    }
                    if (!IsOKType(pt)) continue;
                    if (exceptFor.Contains(v)) continue;
                    AddValue(JoinedName(prefix, o.Name), v, dict);
                }
                catch (Exception e)
                {
                    DLRConsole.DebugWriteLine("" + e);
                }
            }
        }

        private void AddValue(object key, object value, IDictionary dict)
        {

            var valuePrefix = "" + key;
            if (value is IJSONPROXY)
            {
                foreach (var kv in (IJSONPROXY) value)
                {
                    string joinedName = JoinedName(valuePrefix, kv.Key);
                    AddValue(joinedName, kv.Value, dict);
                }
                return;
            }
            if (value is IDictionary)
            {
                foreach (DictionaryEntry kv in (IDictionary) value)
                {
                    string joinedName = JoinedName(valuePrefix, "" + kv.Key);
                    AddValue(joinedName, kv.Value, dict);
                }
                return;
            }

            bool neededDecode;
            var o2 = DecodeValue(valuePrefix + "_", value, out neededDecode);
            if (neededDecode)
            {
                value = o2;
            }
            dict[key] = value;
        }

        private void AddValue(string key, object value, IJSONPROXY dict)
        {

            var valuePrefix = "" + key;
            if (value is IJSONPROXY)
            {
                foreach (var kv in (IJSONPROXY) value)
                {
                    string joinedName = JoinedName(valuePrefix, kv.Key);
                    AddValue(joinedName, kv.Value, dict);
                }
                return;
            }
            if (value is IDictionary)
            {
                foreach (DictionaryEntry kv in (IDictionary) value)
                {
                    string joinedName = JoinedName(valuePrefix, "" + kv.Key);
                    AddValue(joinedName, kv.Value, dict);
                }
                return;
            }

            bool neededDecode;
            var o2 = DecodeValue(valuePrefix + "_", value, out neededDecode);
            if (neededDecode)
            {
                value = o2;
            }
            dict[key] = value;
        }

        private static string JoinedName(string prefix, string word)
        {
            prefix = prefix ?? "";
            word = word ?? "";
            prefix = MushDLR223.ScriptEngines.Parser.ToMapKey(prefix.Trim('_'));
            word = MushDLR223.ScriptEngines.Parser.ToMapKey(word.Trim('_'));
            if (prefix == word || string.IsNullOrEmpty(prefix))
            {
                return word;
            }
            if (!string.IsNullOrEmpty(word)) return prefix + "_" + word;
            return prefix;
        }

        private bool IsOKType(Type pt)
        {
            InitTypeFilters();
            if (IsSubType(ForSubTypes, pt)) return true;
            if (IsSubType(ExceptForSubTypes, pt)) return false;
            return true;
        }

        private static bool IsSubType(List<Type> oneOf, Type pt)
        {
            foreach (Type one in oneOf)
            {
                if (one.IsAssignableFrom(pt))
                {
                    return true;
                }
            }
            return false;
        }

        private static readonly Dictionary<Type, KeyValuePair<List<PropertyInfo>, List<FieldInfo>>> PropForTypes =
            new Dictionary<Type, KeyValuePair<List<PropertyInfo>, List<FieldInfo>>>();

        private readonly List<Type> ForSubTypes = new List<Type>();
        private readonly List<Type> ExceptForSubTypes = new List<Type>();

        private static KeyValuePair<List<PropertyInfo>, List<FieldInfo>> GetPropsForTypes(Type t)
        {
            KeyValuePair<List<PropertyInfo>, List<FieldInfo>> kv;

            if (PropForTypes.TryGetValue(t, out kv)) return kv;

            lock (PropForTypes)
            {
                if (!PropForTypes.TryGetValue(t, out kv))
                {
                    kv = new KeyValuePair<List<PropertyInfo>, List<FieldInfo>>(new List<PropertyInfo>(),
                                                                               new List<FieldInfo>());
                    var ta = t.GetCustomAttributes(typeof (XmlTypeAttribute), false);
                    bool specialXMLType = false;
                    if (ta != null && ta.Length > 0)
                    {
                        XmlTypeAttribute xta = (XmlTypeAttribute) ta[0];
                        specialXMLType = true;
                    }
                    List<string> lowerProps = new List<string>();
                    BindingFlags flags = BindingFlags.Instance | BindingFlags.Public; //BindingFlags.NonPublic
                    foreach (
                        PropertyInfo o in t.GetProperties(flags))
                    {
                        if (o.CanRead)
                        {

                            if (o.Name.StartsWith("_")) continue;
                            if (o.DeclaringType == typeof (Object)) continue;
                            if (lowerProps.Contains(o.Name)) continue;
                            lowerProps.Add(o.Name.ToLower());
                            if (o.GetIndexParameters().Length > 0)
                            {
                                continue;
                            }
                            if (specialXMLType)
                            {
                                var use = o.GetCustomAttributes(typeof (XmlArrayItemAttribute), false);
                                if (use == null || use.Length < 1) continue;
                            }
                            kv.Key.Add(o);

                        }
                    }
                    foreach (FieldInfo o in t.GetFields(flags))
                    {
                        if (o.Name.StartsWith("_")) continue;
                        if (o.DeclaringType == typeof (Object)) continue;
                        if (lowerProps.Contains(o.Name)) continue;
                        lowerProps.Add(o.Name.ToLower());
                        if (specialXMLType)
                        {
                            var use = o.GetCustomAttributes(typeof (XmlArrayItemAttribute), false);
                            if (use == null || use.Length < 1) continue;
                        }
                        kv.Value.Add(o);
                    }
                }
                return kv;
            }
        }

    }
}
