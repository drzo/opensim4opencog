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
using Avro;
using Avro.Generic;
using Avro.Specific;
using log4net;
using Apache.Qpid.Messaging;
using Apache.Qpid.Client.Qms;
using MushDLR223.ScriptEngines;
using MushDLR223.Utilities;

namespace RoboKindChat
{
    public class RoboKindConnectorQPID
    {

        /// <summary>
        /// Set up a queue to listen for reports on.
        /// </summary>
        public IMessageConsumer CreateListener(string name, string exchangeNameDefaults, MessageReceivedDelegate handler)
        {
            string responseQueueName = channel.GenerateUniqueName();
            channel.DeclareQueue(responseQueueName, false, true, true);
            // Set this listener up to listen for reports on the response queue.
            channel.Bind(responseQueueName, exchangeNameDefaults, name);
            //channel.Bind(responseQueueName, "<<default>>", RESPONSE_ROUTING_KEY);
            IMessageConsumer consumer = channel.CreateConsumerBuilder(responseQueueName).Create();
            consumer.OnMessage += new MessageReceivedDelegate(handler);
            return consumer;
        }

        private static ILog log = LogManager.GetLogger(typeof(RoboKindConnectorQPID));

        /// <summary> Holds the default test timeout for broker communications before tests give up. </summary>
        const int TIMEOUT = 10000;

        /// <summary> Holds the number of messages to send in each test run. </summary>
        //private int numMessages;

        /// <summary> Holds the number of subscribers listening to the messsages. </summary>
        //private int numSubscribers;

        /// <summary> A monitor used to wait for all reports to arrive back from consumers on. </summary>
        //private AutoResetEvent allReportsReceivedEvt = new AutoResetEvent(false);

        /// <summary> Holds the connection to listen on. </summary>
        private IConnection connection;

        /// <summary> Holds the channel for all test messages.</summary>
        private IChannel channel;

        /// <summary> Holds the producer to send test messages on. </summary>
        private Dictionary<string, IMessagePublisher> Publishers = new Dictionary<string, IMessagePublisher>();

        private string TopicDefault;

        /// <summary>
        /// Creates a topic publisher that will send the specifed number of messages and expect the specifed number of report back from test
        /// subscribers.
        /// </summary>
        /// 
        /// <param name="connectionUri">The broker URL.</param>
        /// <param name="botcontrol">The channel name to publish on.</param>
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
            reportRequestMessage.Headers["TYPE"] = RoboKindAvroQPIDModuleMain.REPORT_REQUEST;

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
            RoboKindConnectorQPID publisher = new RoboKindConnectorQPID(RoboKindAvroQPIDModuleMain.RK_QPID_URI);
            //, RoboKindAvroQPIDModuleMain.COGBOT_CONTROL_ROUTING_KEY

            // Publish the test messages.
            publisher.SendTestMessage(RoboKindAvroQPIDModuleMain.REPORT_REQUEST, "DoTest");
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
        private IMessagePublisher publisher;

        /// <summary> A monitor used to wait for shutdown. </summary>
        private AutoResetEvent shutdownReceivedEvt = new AutoResetEvent(false);

        /// <summary> Holds a flag to indicate that a timer has begun on the first message. Reset when report is sent. </summary> */
        private bool init;
    
        /// <summary> Holds the count of messages received by this listener. </summary> */
        private int count;


        /// <summary>
        /// Fired when a message is received from the broker by the consumer
        /// </summary>
        public event MessageReceivedDelegate OnAvroMessage;// { get; set; }

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
            var rkl = new RoboKindConnectorQPID(RoboKindAvroQPIDModuleMain.RK_QPID_URI);
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
            bool result = CheckTextField(m, "TYPE", RoboKindAvroQPIDModuleMain.REPORT_REQUEST);

            //LogDebug("isReport = " + result);

            return result;
        }

        /// <summary> Checks whether or not a text field on a message has the specified value. </summary>
        /// 
        /// <param name="e">The message to check.</param>
        /// <param name="e">The name of the field to check.</param>
        /// <param name="e">The expected value of the field to compare with.</param>
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
            publisher.Send(reportMessage);

            Console.WriteLine("Sent report: " + report);
        }

        public Dictionary<string, object> DecodeMessage(IMessage message)
        {
            return DecodeMessage(message, new Dictionary<string, object>());
        }
        public Dictionary<string, object> DecodeMessage(IMessage message, Dictionary<string, object> dict)
        {
            if (ForSubTypes.Count == 0)
            {
                ForSubTypes.Add(typeof(ValueType));
                ForSubTypes.Add(typeof(IConvertible));
            }
            if (ExceptForSubTypes.Count == 0)
            {
                ExceptForSubTypes.Add(typeof(Array));
                ExceptForSubTypes.Add(typeof(byte[]));
            }
            var messageHeaders = message.Headers as QpidHeaders;
            var exceptObjects = new List<object>() {message, messageHeaders};
            GetMemberValues("Message_", message, dict, exceptObjects);
            if (messageHeaders!=null)
            {
                var hdrs = messageHeaders._headers;
                foreach (System.Collections.DictionaryEntry hdr in hdrs)
                {
                    dict["Header_" + hdr.Key] = hdr.Value;
                }
            }
            if (message is ITextMessage)
            {
                return dict;
            }
            string valuePrefix = "Value_";                            
            var o = DecodeMessage((IBytesMessage)message);
            if (o is GenericRecord)
            {
                GenericRecord gr = (GenericRecord)o;
                IDictionary<string, object> contents = gr.GetContents();
                contents["SchemeNS"] = gr.Schema.Namespace;
                contents["Scheme"] = gr.Schema.Name;
                o = contents;
            }
            if (o is IConvertible)
            {
                dict[valuePrefix + "Value"] = o;
            }
            bool neededDecode;
            if (o is IDictionary<string, object>)
            {
                foreach (var kv in (IDictionary<string, object>)o)
                {
                    dict[valuePrefix + kv.Key] = DecodeValue(valuePrefix + kv.Key + "_", kv.Value, out neededDecode);
                }
            }
            else if (o is IDictionary)
            {
                foreach (DictionaryEntry kv in (IDictionary)o)
                {
                    dict[valuePrefix + kv.Key] = DecodeValue(valuePrefix + kv.Key + "_", kv.Value, out neededDecode);
                }
            }
            else
            {
                GetMemberValues(valuePrefix, o, dict, exceptObjects);
            }
            return dict;
        }

        private object DecodeValue(string valuePrefix, object o, out bool neededDecode)
        {
            neededDecode = false;
            if (o is IConvertible) return o;
            bool needed0;
            if (o is IDictionary<string, object>)
            {
                IDictionary<string, object> dict = new Dictionary<string, object>();
                foreach (var kv in (IDictionary<string, object>) o)
                {
                    dict[valuePrefix + kv.Key] = DecodeValue(valuePrefix + kv.Key + "_", kv.Value, out needed0);
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
                IDictionary<string, object> dict = new Dictionary<string, object>();
                foreach (DictionaryEntry kv in (IDictionary) o)
                {
                    dict[valuePrefix + kv.Key] = DecodeValue(valuePrefix + kv.Key + "_", kv.Value, out needed0);
                    if (needed0) neededDecode = true;
                }
                if (!neededDecode)
                {
                    return o;
                }
                return dict;
            }
            if (o is GenericRecord)
            {
                neededDecode = true;
                GenericRecord gr = (GenericRecord) o;
                IDictionary<string, object> contents = gr.GetContents();
                contents["SchemeNS"] = gr.Schema.Namespace;
                contents["Scheme"] = gr.Schema.Name;
                return DecodeValue(valuePrefix, contents, out needed0);
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
            return o;
        }

        private object DecodeMessage(IBytesMessage msg)
        {
            int len = (int) msg.BodyLength;
            byte[] bytes = new byte[len];
            msg.ReadBytes(bytes, len);
            MemoryStream ins = new MemoryStream(bytes);
            Schema rs = SchemeFor(msg.ContentType);
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

        private Schema SchemeFor(string type)
        {
            lock (Loaded) EnsuleSchemesLoaded();
            type = type.Substring(type.IndexOf('/') + 1);
            foreach (Schema loaded in Loaded)
            {
                if (loaded.Name == type) return loaded;
            }
            type = type.ToLower();
            foreach (Schema loaded in Loaded)
            {
                if (loaded.Name.ToLower() == type) return loaded;
            }
            type = type + "record";
            foreach (Schema loaded in Loaded)
            {
                if (loaded.Name.ToLower() == type) return loaded;
            }
            return null;
        }

        static List<Schema> Loaded = new List<Schema>();
        private void EnsuleSchemesLoaded()
        {
            if (Loaded.Count > 0) return;
            foreach (var file in Directory.GetFiles("./avro/", "*.json"))
            {
                string text = File.ReadAllText(file);
                var s = Schema.Parse(text);
                Loaded.Add(s);
            }
        }

        public void GetMemberValues(string prefix, Object properties, Dictionary<string, object> dict, List<Object> exceptFor)
        {
            if (properties == null)
            {
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
                    dict[prefix + o.Name] = v;
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
                    dict[prefix + o.Name] = v;
                }
                catch (Exception e)
                {
                    DLRConsole.DebugWriteLine("" + e);
                }
            }
        }

        private bool IsOKType(Type pt)
        {
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

        static readonly Dictionary<Type, KeyValuePair<List<PropertyInfo>, List<FieldInfo>>> PropForTypes = new Dictionary<Type, KeyValuePair<List<PropertyInfo>, List<FieldInfo>>>();
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
                    var ta = t.GetCustomAttributes(typeof(XmlTypeAttribute), false);
                    bool specialXMLType = false;
                    if (ta != null && ta.Length > 0)
                    {
                        XmlTypeAttribute xta = (XmlTypeAttribute)ta[0];
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
                            if (o.DeclaringType == typeof(Object)) continue;
                            if (lowerProps.Contains(o.Name)) continue;
                            lowerProps.Add(o.Name.ToLower());
                            if (o.GetIndexParameters().Length > 0)
                            {
                                continue;
                            }
                            if (specialXMLType)
                            {
                                var use = o.GetCustomAttributes(typeof(XmlArrayItemAttribute), false);
                                if (use == null || use.Length < 1) continue;
                            }
                            kv.Key.Add(o);

                        }
                    }
                    foreach (FieldInfo o in t.GetFields(flags))
                    {
                        if (o.Name.StartsWith("_")) continue;
                        if (o.DeclaringType == typeof(Object)) continue;
                        if (lowerProps.Contains(o.Name)) continue;
                        lowerProps.Add(o.Name.ToLower()); 
                        if (specialXMLType)
                        {
                            var use = o.GetCustomAttributes(typeof(XmlArrayItemAttribute), false);
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
