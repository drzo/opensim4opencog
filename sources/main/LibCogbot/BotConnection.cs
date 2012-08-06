using System;
using System.Collections.Generic;
using System.Collections.Specialized;
using System.ComponentModel;
using System.Reflection;
using System.Windows.Forms;
using System.Xml;
using Cogbot.Actions.Land;
using Cogbot.Actions.Movement;
using Cogbot.Actions.Scripting;
using Cogbot.Actions.System;
using Cogbot.Actions.WebUtil;
using Cogbot.Library;
using Cogbot.Utilities;
using MushDLR223.ScriptEngines;
using MushDLR223.Utilities;
using OpenMetaverse;
using OpenMetaverse.Packets;
using OpenMetaverse.Utilities;
using Cogbot.Actions;
using System.Threading;
using System.Collections;
using Cogbot.ScriptEngines;
using System.IO;
using Cogbot;
using Cogbot.World;
using System.Drawing;
using Settings=OpenMetaverse.Settings;
using Cogbot.Actions.Agent;
using System.Text;
using Type=System.Type;
#if USE_SAFETHREADS
using Thread = MushDLR223.Utilities.SafeThread;
#endif
//using RadegastTab = Radegast.SleekTab;

// older LibOMV
//using TeleportFlags = OpenMetaverse.AgentManager.TeleportFlags;
//using TeleportStatus = OpenMetaverse.AgentManager.TeleportStatus;

namespace Cogbot
{
    public partial class BotClient
    {
        public bool IsLoggedInAndReady
        {
            get
            {
                var gridClient = this.gridClient;
                if (gridClient == null) return false;
                var net = gridClient.Network;
                if (net == null) return false;
                if (net != Network) return false;
                if (net.CurrentSim == null) return false;
                if (!Network.Connected) return false;
                if (gridClient.Self.AgentID == UUID.Zero) return false;
                //something is oput of date!?
                if (gridClient != this.gridClient) return false;
                return true;
            }
        }

        // Reflect events into lisp
        //        
        int LoginRetriesFresh = 2; // for the times we are "already logged in"
        int LoginRetries; // set to Fresh in constructor
        public bool ExpectConnected;
        public void Login()
        {
            LoginRetries = LoginRetriesFresh;
            Login(false);
        }
        static object OneClientAtATime = new object();
        public void LoginBlocked()
        {
            LoginRetries = LoginRetriesFresh;
            lock (OneClientAtATime) Login(true);
            if (IsLoggedInAndReady)
            {
                RunOnLogin();
            }
        }

        public void Login(bool blocking)
        {
            if (IsLoggedInAndReady) return;
            if (ExpectConnected) return;
            if (Network.CurrentSim != null)
            {
                if (Network.CurrentSim.Connected) return;
            }
            //if (ClientManager.simulator.periscopeClient == null)
            //{
            //    ClientManager.simulator.periscopeClient = this;
            //    ClientManager.simulator.Start();
            //    Settings.LOG_LEVEL = Helpers.LogLevel.Info;
            //}
            try
            {
                Settings.LOGIN_SERVER = BotLoginParams.URI;
                if (TheRadegastInstance != null)
                {
                    LoginViaRadegast(blocking);
                    return;
                }

                Settings.USE_LLSD_LOGIN = false;
                if (DLRConsole.IsOnMonoUnix)
                {
                    DebugWriteLine("Should use LLSD Login but cant!");
                    // TODO Settings.USE_LLSD_LOGIN = true;
                }
                //SetLoginOptionsFromRadegast();
                BotLoginParams.Timeout = 2*60000;
                BotLoginParams.Channel = "Cogbot";
                if (BotLoginParams.CalledLoginYet) return;
                BotLoginParams.CalledLoginYet = true;
                if (!blocking)
                {

                    Network.BeginLogin(BotLoginParams.loginParams);
                }
                else
                {
                    Network.Login(BotLoginParams.loginParams);
                }
            }
            catch (Exception ex)
            {
                LogException("Login", ex);
            }

        }

        private void LoginViaRadegast(bool blocking)
        {
            CogbotGUI.SetRadegastLoginOptions(TheRadegastInstance, this);
            var LoginEvent = BotLoginParams.LoginEvent;
            LoginEvent.Reset();
            // non blocking
            CogbotGUI.InvokeGUI(TheRadegastInstance.MainForm, TheRadegastInstance.Netcom.Login);

            if (blocking)
            {
                bool madeIt = LoginEvent.WaitOne(BotLoginParams.loginParams.Timeout, false);

            }
        }

        public void LogException(string p, Exception ex)
        {
            Logger.Log(GetName() + ": Exception " + p + "\n" + ex, Helpers.LogLevel.Error, ex);
            StringWriter sw = new StringWriter();
            sw.WriteLine("ERROR !Exception: " + ex.GetBaseException().Message);
            sw.WriteLine("error occured: " + ex.Message);
            sw.WriteLine("        Stack: " + ex.StackTrace.ToString());
            Exception inner = ex.InnerException;
            if (inner != null && inner != ex)
            {
                LogException("Inner of " + p, inner);
            }
            WriteLine("{0}", sw.ToString());
        }

        readonly int thisTcpPort;
        public LoginDetails BotLoginParams
        {
            get
            {
                return _BotLoginParams;
            }
        }

        private LoginDetails _BotLoginParams = null;
        void SetLoginName(string firstName, string lastName)
        {
            if (!string.IsNullOrEmpty(firstName) || !string.IsNullOrEmpty(lastName))
            {
                var details = ClientManager.FindOrCreateAccount(firstName, lastName);
                SetLoginAcct(details);
            }
            else
            {
                DebugWriteLine("nameless still");
            }
        }

        public void SetLoginAcct(LoginDetails details)
        {
            details.Client = this;
            _BotLoginParams = details;
            ClientManager.OnBotClientUpdatedName(GetName(), this);
        }


        // EVENT CALLBACK SECTION
        void Network_OnDisconnected(object sender, DisconnectedEventArgs e)
        {
            var message = e.Message;
            var reason = e.Reason;
            try
            {
                if (message.Length > 0)
                    WriteLine("Disconnected from server. Reason is " + message + ". " + reason);
                else
                    WriteLine("Disconnected from server. " + reason);

                SendNetworkEvent("On-Network-Disconnected", reason, message);
            }
            catch (Exception ex)
            {
                LogException("Network_OnDisconnected", ex);
            }
            EnsureConnectedCheck(reason);
        }

        private void EnsureConnectedCheck(NetworkManager.DisconnectType reason)
        {
            if (ExpectConnected && reason != NetworkManager.DisconnectType.ClientInitiated)
            {
                return;
                List<Simulator> sims = new List<Simulator>();
                lock (Network.Simulators)
                {
                    sims.AddRange(Network.Simulators);
                }
                return;
                ExpectConnected = false;
                foreach (var s in sims)
                {
                    //lock (s)
                    {
                        if (s.Connected) s.Disconnect(true);
                    }

                }
                //gridClient = new GridClient();
                //Settings.USE_LLSD_LOGIN = true;
                new Thread(() =>
                {
                    Thread.Sleep(10000);
                    Login(true);
                }).Start();
            }
        }

        void Network_OnConnected(object sender)
        {
            try
            {

                //System.Threading.Thread.Sleep(3000);

                //  describeAll();
                //  describeSituation();
                SendNetworkEvent("On-Network-Connected");
                ExpectConnected = true;
            }
            catch (Exception e)
            {
                LogException("Network-On-Connected", e);
            }
        }


        void Network_OnSimDisconnected(object sender, SimDisconnectedEventArgs e)
        {
            var simulator = e.Simulator;
            var reason = e.Reason;
            SendNetworkEvent("On-Sim-Disconnected", this, simulator, reason);
            if (simulator == Network.CurrentSim)
            {
                EnsureConnectedCheck(reason);
            }
        }

        void Network_OnEventQueueRunning(object sender, EventQueueRunningEventArgs e)
        {
            var simulator = e.Simulator;
            SendNetworkEvent("On-Event-Queue-Running", simulator);
        }

        void Network_OnSimConnected(object sender, SimConnectedEventArgs e)
        {
            ExpectConnected = true;
            var simulator = e.Simulator;
            if (simulator == Network.CurrentSim)
            {
                if (!Settings.SEND_AGENT_APPEARANCE)
                {
                    Appearance.RequestAgentWearables();
                }
                Self.RequestMuteList();
            }
            if (Self.AgentID != UUID.Zero)
            {
                SetSecurityLevel(Self.AgentID, Self.Name, BotPermissions.Owner);
            }
            SendNetworkEvent("On-Simulator-Connected", simulator);
            //            SendNewEvent("on-simulator-connected",simulator);
        }

        bool Network_OnSimConnecting(Simulator simulator)
        {
            SendNetworkEvent("On-Simulator-Connecing", simulator);
            return true;
        }

        void Network_OnLogoutReply(object sender, LoggedOutEventArgs e)
        {
            SendNetworkEvent("On-Logout-Reply", e.InventoryItems);
        }


        public void logout()
        {
            ExpectConnected = false;
            if (Network.Connected)
                Network.Logout();
        }
        public override string ToString()
        {
            return "'(thisClient \"" + GetName() + "\")";
        }

        /// <summary>
        /// Initialize everything that needs to be initialized once we're logged in.
        /// </summary>
        /// <param name="login">The status of the login</param>
        /// <param name="message">Error message on failure, MOTD on success.</param>
        public void Network_OnLogin(object sender, LoginProgressEventArgs e)
        {
            var message = e.Message;
            var login = e.Status;
            if (_BotLoginParams == null)
            {
                SetLoginName(gridClient.Self.FirstName, gridClient.Self.LastName);
            }
            else
            {
                _BotLoginParams.Status = login;
            }
            if (login == LoginStatus.Success)
            {
                // Start in the inventory root folder.
                if (Inventory.Store != null)
                    CurrentDirectory = Inventory.Store.RootFolder; //.RootFolder;
                else
                {
                    Logger.Log("Cannot get Inventory.Store.RootFolder", OpenMetaverse.Helpers.LogLevel.Error);
                    CurrentDirectory = null;
                }
                OneAtATimeQueue.Enqueue(RunOnLogin);
            } // anyhitng other than success NeedRunOnLogin
            else
            {
                NeedRunOnLogin = true;
            }
            //            WriteLine("ClientManager Network_OnLogin : [" + login.ToString() + "] " + message);
            //SendNewEvent("On-Login", login, message);

            if (login == LoginStatus.Failed)
            {
                ExpectConnected = false;
                SendNetworkEvent("On-Login-Fail", login, message);
                WriteLine("Login Failed " + message + " LoginRetries: " + LoginRetries);
                if (LoginRetries <= 0)
                {
                    if (_BotLoginParams != null)
                    {
                        _BotLoginParams.LoginEvent.Set();
                    }
                    return;
                }
                LoginRetries--;
                Login(false);
            }
            else if (login == LoginStatus.Success)
            {
                if (!ClientManager.StarupLispCreatedBotClients)
                {
                    CogbotGUI.GetLoginOptionsFromRadegast(TheRadegastInstance, this);
                }
                LoginRetries = 0; // maybe LoginRetriesFresh??
                WriteLine("Logged in successfully");
                ExpectConnected = true;
                SendNetworkEvent("On-Login-Success", login, message);
                //                SendNewEvent("on-login-success",login,message);
                if (_BotLoginParams != null)
                {
                    _BotLoginParams.LoginEvent.Set();
                }
            }
            else
            {
                SendNetworkEvent("On-Login", login, message);
            }

        }

        public string GetName()
        {
            string n = Self.Name;
            if (n != null && !String.IsNullOrEmpty(n.Trim())) return n;
            if (_BotLoginParams == null)
            {
                return "Noname" + GetHashCode();
            }
            if (String.IsNullOrEmpty(BotLoginParams.FirstName))
            {
                return string.Format("Unnamed Robot: {0} {1}", BotLoginParams.FirstName, BotLoginParams.LastName);
                throw new NullReferenceException("GEtName");
            }
            return string.Format("{0} {1}", BotLoginParams.FirstName, BotLoginParams.LastName);
        } 

        protected UUID TheAvatarID
        {
            get { return Self.AgentID; }
        }

        [SkipMemberTree]
        public SimAvatarClient TheSimAvatar
        {
            get
            {
                if (CogbotHelpers.IsNullOrZero(Self.AgentID) && WorldSystem.m_TheSimAvatar == null) return null;
                return (SimAvatarClient)WorldSystem.TheSimAvatar;
            }
        }
        internal object GetAvatar()
        {
            if (!CogbotHelpers.IsNullOrZero(Self.AgentID) || WorldSystem.m_TheSimAvatar != null)
            {
                return TheSimAvatar;
            }
            return this;
        }
    }
}