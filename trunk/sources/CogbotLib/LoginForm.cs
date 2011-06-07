using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.IO;
using System.Text;
using System.Windows.Forms;
using System.Xml.Serialization;
using MushDLR223.Utilities;
using OpenMetaverse; //using libsecondlife;
using System.Threading;

namespace cogbot
{
#pragma warning disable 0168
    public partial class LoginForm : Form
    {
        public GridClient client;
        public ClientManager parent=null;
        public int attempt = 0;


        private delegate void CloseItDelegate();
        private void CloseIt()
        {
            if (firstNameText.InvokeRequired)    
            {        
                // This is a worker thread so delegate the task.        
                this.Invoke(new CloseItDelegate(this.CloseIt));    
            }    
            else    
            {        // This is the UI thread so perform the task.
                parent.config.firstName = firstNameText.Text;// "K";
                parent.config.lastName = lastNameText.Text; //"I";
                parent.config.password = passwordText.Text; //"dbot";
                Close();
            }
        }

        public LoginForm(GridClient _client)
        {
            client = _client;
            client.Network.SimConnected += Network_OnConnected;
            InitializeComponent();

       }



        public LoginForm(BotClient _client, ClientManager _parent)
        {
            client = _client.gridClient;
            parent = ClientManager.SingleInstance;
            client.Network.SimConnected += Network_OnConnected;
            client.Network.LoginProgress += Network_OnLogin;
            
            InitializeComponent();
            firstNameText.Text = parent.config.firstName;// "K";
            lastNameText.Text = parent.config.lastName; //"I";
            passwordText.Text = parent.config.password; //"dbot";

        }

        void Network_OnLogin(object sender, LoginProgressEventArgs e)
        {
            try
            {
                //throw new NotImplementedException();

                Text = "Logging In [" + e.Message.ToString() + "]";
            }
            catch (Exception)
            {
            }
            if (e.Status == LoginStatus.Success) CloseIt();
       }

        void Network_OnConnected(object sender, SimConnectedEventArgs e)
        {
            try
            {
            if (parent != null) parent.WriteLine("LoginForm Network_OnConnected :" + attempt.ToString());
            }
            catch (Exception)
            {
            }
            CloseIt();
        }

        private void cancelButton_Click(object sender, EventArgs e)
        {
            CloseIt();
        }

        private void logInButton_Click(object sender, EventArgs e)
        {
            int myattempt=0;
            Text = "Loggin In [Attempting]";
            for (int i = 1; i <= 1; i++)
            {
                myattempt = (attempt++);
                if (parent != null) parent.WriteLine("LoginForm Start Attempt " + myattempt.ToString());

                logInButton.Enabled = false;
                string startLocation = NetworkManager.StartLocation("Citadel", 140, 140, 23);
                client.Settings.LOGIN_SERVER = parent.config.simURL; // "http://127.0.0.1:8002/";

                if (client.Network.Login(firstNameText.Text,
                    lastNameText.Text, passwordText.Text,
                    "TextSL", startLocation, "UNR"))
                {
                    if (parent != null) parent.WriteLine("LoginForm Logged in :" + myattempt.ToString() + " " + client.Network.LoginMessage);
                    CloseIt();
                    return;
                }
                else
                {
                    if (parent != null) parent.WriteLine("LoginForm NOT logged in for reason:" + myattempt.ToString() + " " + client.Network.LoginMessage);
                    //System.Threading.Thread.Sleep(30000); // give it a rest for 30 sec then try again
                }
            }
            logInButton.Enabled = true;
        }

        private void LoginForm_Load(object sender, EventArgs e)
        {

        }
    }
#pragma warning restore 0168

    #region -- Configuration Class --
    /// <summary>
    /// This Configuration class is basically just a set of 
    /// properties with a couple of static methods to manage
    /// the serialization to and deserialization from a
    /// simple XML file.
    /// </summary>
    [Serializable]
    public class Configuration
    {
        int _Version;
        int _TcpPort;
        int _TcpPortOffset;
        string _TcpIpAddress;
        string _FirstName;
        string _LastName;
        string _Password;
        string _SimURL;
        string _startupLisp;
        string _startupClientLisp;
        string _onLogin;
        string _onLogout;

        public Configuration()
        {
            _Version = 2;
            _FirstName = "";
            _LastName = "";
            _Password = "";
            _SimURL = "http://127.0.0.1:8002/"; // The sim server we talk to
            _TcpPort = 5555; // The TCP port WE provide
            _TcpPortOffset = 10; // The TCP offset between bots
            _TcpIpAddress = "127.0.0.1"; // The IPAddress WE Listen on
            _startupLisp = "";
            _startupClientLisp = "";
        }

        public static void Serialize(string file, Configuration c)
        {
            System.Xml.Serialization.XmlSerializer xs
               = new System.Xml.Serialization.XmlSerializer(c.GetType());
            StreamWriter writer = File.CreateText(file);
            xs.Serialize(writer, c);
            writer.Flush();
            writer.Close();
        }
        public static Configuration Deserialize(string file)
        {
            System.Xml.Serialization.XmlSerializer xs
               = new System.Xml.Serialization.XmlSerializer(
                  typeof(Configuration));
            xs.UnknownElement += new XmlElementEventHandler(On_Unknown);
            StreamReader reader = File.OpenText(file);
            Configuration c = (Configuration)xs.Deserialize(reader);
            reader.Close();
            return c;
        }

        private static void On_Unknown(object sender, XmlElementEventArgs e)
        {
            DLRConsole.DebugWriteLine("UNKOWN XML ELEMENT: " + e.Element + " " + e.ExpectedElements + " @ line " +
                                      e.LineNumber);
        }

        public void loadConfig()
        {
            try
            {
                loadConfig("botconfig.xml");

            }
            catch (Exception e)
            {
                DLRConsole.DebugWriteLine(e);
            }

        }
        public void loadConfig(string fileName)
        {
            try
            {
                Configuration c2 = Configuration.Deserialize(fileName);
                this.Version = c2.Version;
                this.firstName = c2.firstName;
                this.lastName = c2.lastName;
                this.password = c2.password;
                this.simURL = c2.simURL;
                this.tcpPort = c2.tcpPort;
                this.tcpIPAddress = c2.tcpIPAddress;
                this.startupLisp = c2.startupLisp;
                this.startupClientLisp = c2.startupClientLisp;
                this.onLogin = c2.onLogin;

            }
            catch (Exception e)
            {
                DLRConsole.DebugWriteLine(e);
                throw e;
            }

        }
        public void saveConfig()
        {
            try
            {
                Configuration.Serialize("botconfig.xml", this);
            }
            catch (Exception e)
            {
                DLRConsole.DebugWriteLine(e);
            }

        }

        // XMLElements
        public int Version
        {
            get { return _Version; }
            set { _Version = value; }
        }
        public int tcpPort
        {
            get { return _TcpPort; }
            set { _TcpPort = value; }
        }
        public int tcpPortOffset
        {
            get { return _TcpPortOffset; }
            set { _TcpPortOffset = value; }
        }
        public string tcpIPAddress
        {
            get { return _TcpIpAddress; }
            set { _TcpIpAddress = value; }
        }
        public string firstName
        {
            get { return _FirstName; }
            set { _FirstName = value; }
        }
        public string lastName
        {
            get { return _LastName; }
            set { _LastName = value; }
        }
        public string password
        {
            get { return _Password; }
            set { _Password = value; }
        }
        public string simURL
        {
            get { return _SimURL; }
            set { _SimURL = value; }
        }
        public string startupLisp
        {
            get { return _startupLisp; }
            set { _startupLisp = value; }
        }
        public string startupClientLisp
        {
            get { return _startupClientLisp; }
            set { _startupClientLisp = value; }
        }
        public string onLogin
        {
            get { return _onLogin;; }
            set { _onLogin = value; }
        }

    }
    #endregion

#pragma warning restore 0168

}