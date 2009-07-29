using System;
using System.Collections.Generic;
using System.Drawing;
using System.IO;
using System.Reflection;
using System.Threading;
using System.Windows.Forms;
using cogbot.Actions;
using cogbot.ScriptEngines;
using OpenMetaverse; //using libsecondlife;
using Action = cogbot.Actions.Action;

namespace cogbot
{
#pragma warning disable 0168
    
    //the Simian stuff in the opensim4opencog stopped being used for a bit.. it was going to do scene management.. but still
    public delegate void OutputDelegate(string str, params object[] args);

    public partial class TextForm : Form
    {


        public TextForm()
        {
            SingleInstance = this;
            InitializeComponent();
            this.Size = new Size(this.Size.Width + 300, this.Size.Height);
            this.ResizeEnd += TextForm_ResizeEnd;
            TextForm_ResizeEnd(null, null);
            Show();
            consoleInputText.Enabled = true;
            consoleInputText.Focus();
        }

        private delegate void EnableItDelegate();
        private void EnableIt()
        {
            try
            {
                if (consoleInputText.InvokeRequired)
                {
                    // This is a worker thread so delegate the task.        
                    this.Invoke(new EnableItDelegate(EnableIt));
                }
                else
                {        // This is the UI thread so perform the task.
                    consoleInputText.Enabled = true;
                    loginToolStripMenuItem.Enabled = false;
                    logoutToolStripMenuItem.Enabled = true;
                    submitButton.Enabled = true;

                    consoleInputText.Focus();

                    System.Threading.Thread.Sleep(3000);

                    // describeSituation();

                }
            }
            catch (Exception e)
            {

            }
        }
      

        private void TextForm_FormClosed(object sender, FormClosedEventArgs e)
        {
            ClientManager.SingleInstance.ShutDown();
        }


        private void exitToolStripMenuItem_Click(object sender, EventArgs e)
        {
            Close(); //            TextForm_FormClosed(sender, null);
        }

        private void logoutToolStripMenuItem_Click(object sender, EventArgs e)
        {
            ClientManager.SingleInstance.logout();
        }

        public void WriteLine(string str, params object[] args)
        {
            try
            {
                if (str == null) return;
                if (args != null && args.Length > 0) str = String.Format(str, args);
                str = str.Trim();
                if (str == "") return;

                str = str.Replace("\r\n", "\n").Replace("\r", "\n").Replace("\n", "\r\n");
                //if (str.ToLower().Contains("look")) return;
                if (IsDisposed) return; // for (un)clean exits
                if (this.InvokeRequired)
                {
                    //new Thread(() =>
                        this.Invoke(new OutputDelegate(doOutput), str, new object[0]);
                    //).Start();
                                       
                }
                else
                {
                   // new Thread(() => this.Invoke(new OutputDelegate(doOutput), str, new object[0])).Start();//  new Thread(() => doOutput(str)).Start();
                    doOutput(str);
                }
            }
            catch (Exception e)
            {
                Console.WriteLine("" + e);                
            }
        }

        private void doOutput(string str, params object[] args)
        {
            if (str == null) return;
            if (args.Length > 0) str = String.Format(str, args);
            str = str.Trim(); 
            if (str == "") return;
            try
            {
               // lock (consoleText)
                {
                    if (consoleText.IsDisposed) return; // for (un)clean exits
                    consoleText.AppendText(str + "\r\n");
                }
            }
            catch (Exception e)
            {
                Console.WriteLine("" + e);
                // probably dead anyway ...
            }
            return;
            string oldInput = consoleInputText.Text;
            consoleInputText.Clear();
            consoleInputText.Text = str;
            consoleInputText.SelectAll();
            System.Threading.Thread.Sleep(100);
            consoleInputText.Text = oldInput;
        }



        public void acceptConsoleInput()
        {
            string text = consoleInputText.Text;
            if (text.Length > 0)
            {
                //WriteLine(text);
                consoleInputText.Text = "";

                //new Thread(()=>
                WriteLine(ClientManager.SingleInstance.ExecuteCommand(text, WriteLine));//).Start();

                //if (describeNext)
                //{
                //    describeNext = false;
                //    describeSituation();
                //}
            }
        }

        private void consoleInputText_KeyPress(object sender, KeyPressEventArgs e)
        {
            if (Convert.ToInt32(e.KeyChar) == 13)
                acceptConsoleInput();
        }

  

        private void TextForm_Load(object sender, EventArgs e)
        {
            //cycConnection = new DotCYC.CycConnectionForm();
        }

        private void consoleText_TextChanged(object sender, EventArgs e)
        {

        }

        private void TextForm_ResizeBegin(object sender, EventArgs e)
        {

        }
        private void TextForm_ResizeEnd(object sender, EventArgs e)
        {
            consoleText.Size = new Size(this.Size.Width - 35, this.Size.Height - 100);
            consoleInputText.Top = this.Size.Height - 60;
            consoleInputText.Size = new Size(this.Size.Width - 125, consoleInputText.Height);
            this.submitButton.Location = new System.Drawing.Point(this.Size.Width - 90, consoleInputText.Top);
        }

        /// <summary>
        /// Initialize everything that needs to be initialized once we're logged in.
        /// </summary>
        /// <param name="login">The status of the login</param>
        /// <param name="message">Error message on failure, MOTD on success.</param>
        public void LoginHandler(LoginStatus login, string message)
        {
            EnableIt();
        }

        private void bot1ToolStripMenuItem_Click(object sender, EventArgs e)
        {

        }
        static public Dictionary<string, BotClient> BotByName = new Dictionary<string, BotClient>();
        public static TextForm SingleInstance;


        public void AddTool(string name, string text, EventHandler threadStart)
        {
               base.Invoke(new AddToolDelegate(AddTool0), new object[] {name, text, threadStart});
        }

        public delegate void AddToolDelegate(string name, string text, EventHandler threadStart);
        public void AddTool0(string name, string text, EventHandler threadStart)
        {           
            SuspendLayout();
            ToolStripMenuItem stripMenuItem =
                new ToolStripMenuItem {Name = name, Size = new System.Drawing.Size(168, 22), Text = text};
            stripMenuItem.Click += threadStart;
            this.toolsToolStripMenuItem.DropDownItems.AddRange(new System.Windows.Forms.ToolStripItem[] {stripMenuItem});
            ResumeLayout();
        }

        private void consoleInputText_TextChanged(object sender, EventArgs e)
        {

        }

        private void submitButton_Click(object sender, EventArgs e)
        {
            acceptConsoleInput();
        }

        private void loginToolStripMenuItem_Click(object sender, EventArgs e)
        {
            ClientManager.SingleInstance.Login(new string[0]);
        }
    }
}
