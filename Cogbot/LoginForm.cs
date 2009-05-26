using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;
using OpenMetaverse; //using libsecondlife;
using System.Threading;

namespace cogbot
{
#pragma warning disable 0168
    public partial class LoginForm : Form
    {
        public GridClient client;
        public TextForm parent=null;
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
            client.Network.OnConnected += new NetworkManager.ConnectedCallback(Network_OnConnected);
            InitializeComponent();

       }



        public LoginForm(BotClient _client, TextForm _parent)
        {
            client = _client.gridClient;
            parent = _parent;
            client.Network.OnConnected += new NetworkManager.ConnectedCallback(Network_OnConnected);
            client.Network.OnLogin += new NetworkManager.LoginCallback(Network_OnLogin);
            
            InitializeComponent();
            firstNameText.Text = parent.config.firstName;// "K";
            lastNameText.Text = parent.config.lastName; //"I";
            passwordText.Text = parent.config.password; //"dbot";

        }

        void Network_OnLogin(LoginStatus login, string message)
        {
            try
            {
                //throw new NotImplementedException();

                Text = "Logging In [" + login.ToString() + "]";
            }
            catch (Exception e)
            {
            }
            if (login == LoginStatus.Success) CloseIt();
       }

        void Network_OnConnected(object sender)
        {
            try
            {
            if (parent != null) parent.output("LoginForm Network_OnConnected :" + attempt.ToString());
            }
            catch (Exception e)
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
                if (parent != null) parent.output("LoginForm Start Attempt " + myattempt.ToString());

                logInButton.Enabled = false;
                string startLocation = NetworkManager.StartLocation("Citadel", 140, 140, 23);
                client.Settings.LOGIN_SERVER = parent.config.simURL; // "http://127.0.0.1:8002/";

                if (client.Network.Login(firstNameText.Text,
                    lastNameText.Text, passwordText.Text,
                    "TextSL", startLocation, "UNR"))
                {
                    if (parent != null) parent.output("LoginForm Logged in :" + myattempt.ToString() + " " + client.Network.LoginMessage);
                    CloseIt();
                    return;
                }
                else
                {
                    if (parent != null) parent.output("LoginForm NOT logged in for reason:" + myattempt.ToString() + " " + client.Network.LoginMessage);
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
}