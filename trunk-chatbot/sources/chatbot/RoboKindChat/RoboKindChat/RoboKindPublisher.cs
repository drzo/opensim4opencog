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
using System.Collections.Generic;
using System.Threading;
using Apache.Qpid.Client;
using log4net;
using Apache.Qpid.Messaging;
using Apache.Qpid.Client.Qms;

namespace RoboKindChat
{
    public class RoboKindPublisher
    {
        private static ILog log = LogManager.GetLogger(typeof(RoboKindPublisher));

        /// <summary> Holds the default test timeout for broker communications before tests give up. </summary>
        const int TIMEOUT = 10000;

        /// <summary> Holds the number of messages to send in each test run. </summary>
        //private int numMessages;

        /// <summary> Holds the number of subscribers listening to the messsages. </summary>
        //private int numSubscribers;

        /// <summary> A monitor used to wait for all reports to arrive back from consumers on. </summary>
        private AutoResetEvent allReportsReceivedEvt = new AutoResetEvent(false);

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
        public RoboKindPublisher(string connectionUri, string botcontrol)
        {
            log.Debug("TopicPublisher(string connectionUri = " + connectionUri + "): called");

            // Create a connection to the broker.
            IConnectionInfo connectionInfo0 = QpidConnectionInfo.FromUrl(connectionUri);
            connection = new AMQConnection(connectionInfo0);

            // Establish a session on the broker.
            channel = connection.CreateChannel(false, AcknowledgeMode.AutoAcknowledge, 1);

            // Set up a queue to listen for reports on.
            CreateDirectListener(channel, RoboKindAvroQPIDModuleMain.ROBOKIND_RESPONSE_ROUTING_KEY, OnMessage);
            TopicDefault = botcontrol;


            connection.Start();
            Console.WriteLine("Sending messages and waiting for reports...");
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
            allReportsReceivedEvt.WaitOne(TIMEOUT, true);

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
        /// Set up a queue to listen for reports on.
        /// </summary>
        static public IMessageConsumer CreateDirectListener(IChannel channel, string name, MessageReceivedDelegate handler)
        {
            string responseQueueName = channel.GenerateUniqueName();
            channel.DeclareQueue(responseQueueName, false, true, true);
            // Set this listener up to listen for reports on the response queue.
            channel.Bind(responseQueueName, ExchangeNameDefaults.DIRECT, name);
            //channel.Bind(responseQueueName, "<<default>>", RESPONSE_ROUTING_KEY);
            IMessageConsumer consumer = channel.CreateConsumerBuilder(responseQueueName).Create();
            consumer.OnMessage += new MessageReceivedDelegate(handler);
            return consumer;
        }

        /// <summary>
        /// Start a test subscriber. The broker URL must be specified as the first command line argument.
        /// </summary>
        /// 
        /// <param name="argv">The command line arguments, broker URL first.</param>
        public static void Main0(String[] argv)
        {
            // Create an instance of this publisher with the command line parameters.
            RoboKindPublisher publisher = new RoboKindPublisher(RoboKindAvroQPIDModuleMain.RK_QPID_URI, RoboKindAvroQPIDModuleMain.COGBOT_CONTROL_ROUTING_KEY);

            // Publish the test messages.
            publisher.SendTestMessage(RoboKindAvroQPIDModuleMain.REPORT_REQUEST, "DoTest");
        }

        /// <summary>
        /// Handles all report messages from subscribers. This decrements the count of subscribers that are still to reply, until this becomes
        /// zero, at which time waiting threads are notified of this event.
        /// </summary>
        /// 
        /// <param name="message">The received report message.</param>
        public void OnMessage(IMessage message)
        {
            log.Debug("public void OnMessage(IMessage message = " + message + "): called");

            // Decrement the count of expected messages and release the wait monitor when this becomes zero.
            //if (--numSubscribers == 0)
            {
                log.Debug("Got reports from all subscribers.");
                allReportsReceivedEvt.Set();
            }
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
        }
    }
}
