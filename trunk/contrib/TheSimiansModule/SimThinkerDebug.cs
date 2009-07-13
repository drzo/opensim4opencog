using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.Text;
using System.Threading;
using System.Windows.Forms;
using cogbot;
using Exception=System.Exception;
using String=System.String;

namespace TheSimiansModule
{
    public partial class SimThinkerDebug : Form
    {
        private delegate void ShowDelegate();
        public void Show()
        {
            if (this.InvokeRequired)
            {
                Invoke(new ShowDelegate(Show), new object[] { });
                return;
            } 
            Reactivate();
            TextForm_ResizeEnd(null, null);
        }

        private void Reactivate()
        {
            try
            {
                baseShow();

                //if (this.WindowState == FormWindowState.Minimized)              
                base.WindowState = FormWindowState.Normal;
                if (!base.Visible) base.Visible = true;
                base.Activate();
            }
            catch (Exception e)
            {
                Console.WriteLine("" + e);
            }
        }

        private void baseShow()
        {
            try
            {
                base.Show();
            }
            catch (Exception e)
            {
                Console.WriteLine("" + e);
            }
        }


        private readonly BotClient client;
        public SimThinkerDebug(BotClient bc)
        {
            client = bc;
            InitializeComponent();
            this.Size = new Size(this.Size.Width + 300, this.Size.Height);
            this.Text = string.Format("SimThinker Debug {0}", client.WorldSystem.TheSimAvatar.ToString());
            this.ResizeEnd += TextForm_ResizeEnd;
            Show();
            consoleInputText.Enabled = true;
            consoleInputText.Focus();
        }

        private void TextForm_ResizeEnd(object sender, EventArgs e)
        {
            consoleText.Size = new Size(this.Size.Width - 35, this.Size.Height - 100);
            consoleInputText.Top = this.Size.Height - 60;
            consoleInputText.Size = new Size(this.Size.Width - 125, consoleInputText.Height);
            this.submitButton.Location = new System.Drawing.Point(this.Size.Width - 90, consoleInputText.Top);
        }

        private void TextForm_Load(object sender, EventArgs e)
        {
            //throw new NotImplementedException();
        }

        private void TextForm_FormClosed(object sender, FormClosedEventArgs e)
        {           
            //throw new NotImplementedException();
        }

        private void consoleInputText_KeyPress(object sender, KeyPressEventArgs e)
        {
            if (Convert.ToInt32(e.KeyChar) == 13)
                acceptConsoleInput();
        }


        public void acceptConsoleInput()
        {
            string text = consoleInputText.Text;
            if (text.Length > 0)
            {
                //WriteLine(text);
                consoleInputText.Text = "";
                ExecuteCommand(text);
                //if (describeNext)
                //{
                //    describeNext = false;
                //    describeSituation();
                //}
            }
        }

        private void ExecuteCommand(string text)
        {
            new Thread(()=> WriteLine(client.ExecuteCommand(text, WriteLine))).Start();
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

        public void doOutput(string str, params object[] args)
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
        }

        private void logoutToolStripMenuItem_Click(object sender, EventArgs e)
        {
            //throw new NotImplementedException();
        }

        private void exitToolStripMenuItem_Click(object sender, EventArgs e)
        {
           // throw new NotImplementedException();
        }

        private void NamedItemClick(object sender, EventArgs e)
        {
            ExecuteCommand("" + sender);
        }

        private void submitButton_Click(object sender, EventArgs e)
        {
            acceptConsoleInput();
        }

        private void consoleInputText_TextChanged(object sender, EventArgs e)
        {

        }

        private void clientToolStripMenuItem_Click(object sender, EventArgs e)
        {

        }
    }
}
