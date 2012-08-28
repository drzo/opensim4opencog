/*
 * Copyright (c) 2006-2008, openmetaverse.org
 * All rights reserved.
 *
 * - Redistribution and use in source and binary forms, with or without
 *   modification, are permitted provided that the following conditions are met:
 *
 * - Redistributions of source code must retain the above copyright notice, this
 *   list of conditions and the following disclaimer.
 * - Neither the name of the openmetaverse.org nor the names
 *   of its contributors may be used to endorse or promote products derived from
 *   this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */

using System;
using System.Collections.Generic;
using System.Threading;
using OpenMetaverse;
using OpenMetaverse.Packets;
using MushDLR223.ScriptEngines;
using OpenMetaverse.Utilities;

namespace Cogbot.Actions.Voice
{
    public class VoiceException : Exception
    {
        public bool LoggedIn = false;

        public VoiceException(string msg) : base(msg)
        {
        }

        public VoiceException(string msg, bool loggedIn) : base(msg)
        {
            LoggedIn = loggedIn;
        }
    }

    public class VoiceTestCommand : Command, RegionMasterCommand, AsynchronousCommand
    {
        private static AutoResetEvent EventQueueRunningEvent = new AutoResetEvent(false);
        private static AutoResetEvent ProvisionEvent = new AutoResetEvent(false);
        private static AutoResetEvent ParcelVoiceInfoEvent = new AutoResetEvent(false);
        private static string VoiceAccount = String.Empty;
        private static string VoicePassword = String.Empty;
        private static string VoiceRegionName = String.Empty;
        private static int VoiceLocalID = 0;
        private static string VoiceChannelURI = String.Empty;

        public VoiceTestCommand(BotClient testClient)
        {
            Name = "VoiceTest";
            TheBotClient = testClient;
        }

        public override void MakeInfo()
        {
            Description = "VoiceTest [firstname] [lastname] [password] [loginuri]";
            Category = CommandCategory.Voice;
        }

        public override CmdResult ExecuteRequest(CmdRequest args)
        {
            if (args.Length > 4)
            {
                return ShowUsage("VoiceTest [firstname] [lastname] [password] [loginuri]");
            }

            bool doLogins = args.Length > 0;
            string firstName = args.Length > 0 ? args[0] : Client.Self.FirstName;
            string lastName = args.Length > 1 ? args[1] : Client.Self.LastName;
            string password = args.Length > 2 ? args[2] : Client.BotLoginParams.Password;
            string loginURI = Client.Settings.LOGIN_SERVER;


            GridClient client;
            if (doLogins)
            {
                client = new GridClient();
                client.Settings.MULTIPLE_SIMS = false;
                Settings.LOG_LEVEL = Helpers.LogLevel.None;
                client.Settings.LOG_RESENDS = false;
                client.Settings.STORE_LAND_PATCHES = true;
                client.Settings.ALWAYS_DECODE_OBJECTS = true;
                client.Settings.ALWAYS_REQUEST_OBJECTS = true;
                client.Settings.SEND_AGENT_UPDATES = true;
                if (4 == args.Length)
                {
                    loginURI = args[3];
                }
            }
            else
            {
                client = Client;
            }

            VoiceManager voice = new VoiceManager(client);
            voice.OnProvisionAccount += voice_OnProvisionAccount;
            voice.OnParcelVoiceInfo += voice_OnParcelVoiceInfo;

            client.Network.EventQueueRunning += client_OnEventQueueRunning;

            try
            {
                if (!voice.ConnectToDaemon()) throw new VoiceException("Failed to connect to the voice daemon");

                List<string> captureDevices = voice.CaptureDevices();

                WriteLine("Capture Devices:");
                for (int i = 0; i < captureDevices.Count; i++)
                    WriteLine(String.Format("{0}. \"{1}\"", i, captureDevices[i]));
                //WriteLine();

                List<string> renderDevices = voice.RenderDevices();

                WriteLine("Render Devices:");
                for (int i = 0; i < renderDevices.Count; i++)
                    WriteLine(String.Format("{0}. \"{1}\"", i, renderDevices[i]));
                // WriteLine();


                if (doLogins)
                {
                    // Login
                    WriteLine("Logging into the grid as " + firstName + " " + lastName + "...");
                    LoginParams loginParams =
                        client.Network.DefaultLoginParams(firstName, lastName, password, "Voice Test", "1.0.0");
                    loginParams.URI = loginURI;
                    if (!client.Network.Login(loginParams))
                        throw new VoiceException("Login to SL failed: " + client.Network.LoginMessage);
                    WriteLine("Logged in: " + client.Network.LoginMessage);
                }

                WriteLine("Creating voice connector...");
                int status;
                string connectorHandle = voice.CreateConnector(out status);
                if (String.IsNullOrEmpty(connectorHandle))
                    throw new VoiceException("Failed to create a voice connector, error code: " + status, true);
                WriteLine("Voice connector handle: " + connectorHandle);

                if (doLogins)
                {
                    WriteLine("Waiting for OnEventQueueRunning");
                    if (!EventQueueRunningEvent.WaitOne(45*1000, false))
                        throw new VoiceException("EventQueueRunning event did not occur", true);
                    WriteLine("EventQueue running");
                }

                WriteLine("Asking the current simulator to create a provisional account...");
                if (!voice.RequestProvisionAccount())
                    throw new VoiceException("Failed to request a provisional account", true);
                if (!ProvisionEvent.WaitOne(120*1000, false))
                    throw new VoiceException("Failed to create a provisional account", true);
                WriteLine("Provisional account created. Username: " + VoiceAccount +
                          ", Password: " + VoicePassword);


                WriteLine("Logging in to voice server " + voice.VoiceServer);
                string accountHandle = voice.Login(VoiceAccount, VoicePassword, connectorHandle, out status);
                if (String.IsNullOrEmpty(accountHandle))
                    throw new VoiceException("Login failed, error code: " + status, true);
                WriteLine("Login succeeded, account handle: " + accountHandle);


                if (!voice.RequestParcelVoiceInfo())
                    throw new Exception("Failed to request parcel voice info");
                if (!ParcelVoiceInfoEvent.WaitOne(45*1000, false))
                    throw new VoiceException("Failed to obtain parcel info voice", true);


                WriteLine("Parcel Voice Info obtained. Region name {0}, local parcel ID {1}, channel URI {2}",
                          VoiceRegionName, VoiceLocalID, VoiceChannelURI);

                if (doLogins)
                {
                    client.Network.Logout();
                }
            }
            catch (Exception e)
            {
                WriteLine(e.Message);
                if (e is VoiceException && (e as VoiceException).LoggedIn)
                {
                    client.Network.Logout();
                }
            }
            finally
            {
                voice.OnProvisionAccount -= voice_OnProvisionAccount;
                voice.OnParcelVoiceInfo -= voice_OnParcelVoiceInfo;
                client.Network.EventQueueRunning -= client_OnEventQueueRunning;
            }
            return Success("voice test complete!");
        }

        private static void client_OnEventQueueRunning(object sender, EventQueueRunningEventArgs e)
        {
            EventQueueRunningEvent.Set();
        }

        private static void voice_OnProvisionAccount(string username, string password)
        {
            VoiceAccount = username;
            VoicePassword = password;

            ProvisionEvent.Set();
        }

        private static void voice_OnParcelVoiceInfo(string regionName, int localID, string channelURI)
        {
            VoiceRegionName = regionName;
            VoiceLocalID = localID;
            VoiceChannelURI = channelURI;

            ParcelVoiceInfoEvent.Set();
        }
    }
}