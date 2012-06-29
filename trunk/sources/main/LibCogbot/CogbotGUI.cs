using System;
using System.Collections.Generic;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Threading;
using System.Windows.Forms;
using Cogbot;
using MushDLR223.Utilities;
using OpenMetaverse;
using Radegast;
using Radegast.Netcom;

namespace Cogbot
{
    public class CogbotGUI
    {
        public static object EnsuringRadegastLock = new object();

        static public void EnsureForm0(Form mf, string name)
        {
            if (!mf.IsHandleCreated)
            {
                // mf.Visible = false;
                //  mf.Show();
                //mf.Visible = false;
                ClientManager.InSTAThread(new ThreadStart(() =>
                                                {
                                                    if (!mf.IsHandleCreated) Application.Run(mf);
                                                }), "EnsureForm0 " + name);
            }
            else
            {
                //mf.BeginInvoke(new MethodInvoker(() => mf.Visible = true));
                if(false)
                {
                    Application.Run(mf);
                }  
            }
        }

        public static RichTextBox TheDebugConsoleRTB = null;

        public static void EnsureBotClientHasRadegast(BotClient bc)
        {
            GridClient gridClient = bc.gridClient;
            RadegastInstance inst = bc.TheRadegastInstance;
            lock (ClientManager._wasFirstGridClientLock)
            {
                if (ClientManager._wasFirstGridClient)
                {
                    ClientManager._wasFirstGridClient = false;
                    if (ClientManagerConfig.UsingCogbotFromRadgast)
                    {
                        inst = GlobalRadegastInstance;
                    }
                    if (inst != null)
                    {
                        gridClient = inst.Client;
                    }
                }
            }
            string name = "EnsureBotClientHasRadegast: " + bc.GetName();
            ClientManager.InSTAThread(() =>
                            {
                                EnsureRadegastForm(bc, bc.TheRadegastInstance, name);
                                //bc.TheRadegastInstance.MainForm.WindowState = FormWindowState.Minimized;
                            }, name);
        }

        public static void SetDebugConsole(RadegastInstance inst)
        {
            if (inst==null) return;
            if (TheDebugConsoleRTB == null)
            {
                SetDebugConsole0(inst.TabConsole.GetTab("debug"));
                if (TheDebugConsoleRTB == null)
                {
                    SetDebugConsole0(inst.TabConsole.GetTab("cogbot"));
                    if (TheDebugConsoleRTB == null) SetDebugConsole0(inst.TabConsole.GetTab("chat"));
                }
            }
        }

        static public void SetDebugConsole0(RadegastTab tab)
        {
            if (tab == null) return;
            var dc = tab.Control as DebugConsole;
            if (dc != null)
            {
                TheDebugConsoleRTB = dc.rtbLog;
                return;
            }
            var dc2 = tab.Control as ChatConsole;
            if (dc2 != null)
            {
                TheDebugConsoleRTB = dc2.rtbChat;
                return;
            }
        }

        static public void EnsureRadegastForm(BotClient bc, RadegastInstance instance, string name)
        {
            lock (EnsuringRadegastLock)
            {
                if (MainProgram.CommandLine == null) MainProgram.CommandLine = new CommandLine();
                instance = bc.TheRadegastInstance = instance ??
                                                    bc.TheRadegastInstance ?? new RadegastInstance(bc.gridClient);
                var mf = instance.MainForm;

                if (bc.EnsuredRadegastRunning) return;
                bc.EnsuredRadegastRunning = true;
                EnsureForm0(mf, name);
            }
        }

        public static RadegastInstance GlobalRadegastInstance;

        static public BotClient BotClientFor(RadegastInstance instance)
        {
            return ClientManager.GetBotByGridClient(instance.Client);
        }

        static public void InvokeGUI(Control mf, ThreadStart o)
        {
            try
            {
                if (false && mf.IsHandleCreated)
                {
                }
                if (mf.InvokeRequired)
                {
                    mf.Invoke(o);
                }
                else o();
            }
            catch (Exception e)
            {
                ClientManager.GlobalWriteLine("ERROR! InvokeGUI " + e);
            }
        }

        static public void SetRadegastLoginOptions(RadegastInstance instance, BotClient TheBot)
        {
            if (instance == null) return;           
            var BotLoginParams = TheBot.BotLoginParams;
            EnsureRadegastForm(TheBot, instance, "EnsureRadegastForm from SetRadegastLoginOptions " + TheBot.GetName());
            var to = instance.Netcom.LoginOptions;
            to.FirstName = BotLoginParams.FirstName;
            to.LastName = BotLoginParams.LastName;
            to.Password = BotLoginParams.Password;
            string loginURI = BotLoginParams.URI;

            MainProgram.CommandLine.LoginUri = loginURI;
            MainProgram.CommandLine.Location = BotLoginParams.Start;
            int gidx;
            Grid G = GetGridIndex(instance, loginURI, out gidx);
            if (G == null)
            {
                G = new Grid(BotLoginParams.URI, BotLoginParams.URI, loginURI);
                to.GridCustomLoginUri = loginURI;
            }
            else
            {
                BotLoginParams.URI = G.LoginURI;
            }
            instance.Netcom.LoginOptions.Grid = G;
            to.Grid = G;
            string botStartAt = BotLoginParams.Start;

            if (botStartAt == "home")
            {
                to.StartLocation = StartLocationType.Home;
            }
            else if (botStartAt == "last")
            {
                to.StartLocation = StartLocationType.Last;
            }
            else
            {
                to.StartLocation = StartLocationType.Custom;
                to.StartLocationCustom = botStartAt;
            }
            to.Version = BotLoginParams.Version;
            to.Channel = BotLoginParams.Channel;
            RadegastTab tab;
            if (instance.TabConsole.Tabs.TryGetValue("login", out tab))
            {
                tab.AllowDetach = true;
                tab.AllowClose = false;
                tab.AllowMerge = false;
                tab.AllowHide = false;
                LoginConsole form = (LoginConsole)tab.Control;
                try
                {
                    if (form.IsDisposed) return;
                    DLRConsole.InvokeControl((Control)instance.MainForm, (MethodInvoker)(() => SetRadegastLoginForm(instance, form, to)));                    
                }
                catch (Exception e) 
                {
                    TheBot.LogException("Set Radegast Login Form", e);
                }
            }
        }

        public static Grid GetGridIndex(RadegastInstance instance, String gridName, out int gridIx)
        {
            gridIx = -1;
            for (int i = 0; i < instance.GridManger.Count; i++)
            {
                Grid testGrid = instance.GridManger[i];
                // cbxGrid.Items.Add(instance.GridManger[i]);
                if (gridName == testGrid.ID || gridName == testGrid.LoginURI || gridName == testGrid.Name)
                {
                    gridIx = i;
                    return testGrid;
                }
            }
            return null;
        }

        static public void SetRadegastLoginForm(RadegastInstance instance, LoginConsole console, LoginOptions options)
        {
            // var instance = TheRadegastInstance;
            if (console.IsDisposed) return;
            console.cbxUsername.Text = (String.Format("{0} {1}", options.FirstName, options.LastName)).Trim();

            switch (options.StartLocation)
            {
                case StartLocationType.Last:
                    //console.cbxLocation.Text = options.StartLocationCustom = "last";
                    console.cbxLocation.SelectedIndex = 1;
                    break;
                case StartLocationType.Home:
                    //console.cbxLocation.Text = options.StartLocationCustom = "home";
                    console.cbxLocation.SelectedIndex = 0;
                    break;
                default:
                    console.cbxLocation.SelectedIndex = -1;
                    console.cbxLocation.Text = options.StartLocationCustom;
                    break;
            }
            console.cbTOS.Checked = true;
            var G = options.Grid;
            string gridName = options.GridCustomLoginUri;
            int gridIx = -1;
            String LoginURI = null;
            G = GetGridIndex(instance, gridName, out gridIx) ?? G;
            if (gridIx == -1)
            {
                if (G != null && !String.IsNullOrEmpty(G.ID))
                {
                    LoginURI = G.LoginURI;
                    if (LoginURI != null) console.txtCustomLoginUri.Text = LoginURI;
                    console.cbxGrid.Text = G.Name ?? G.ID;
                    instance.Netcom.LoginOptions.Grid = G;
                }
                else { console.cbxGrid.Text = "Custom"; }
                if (LoginURI == null) console.txtCustomLoginUri.Text = options.GridCustomLoginUri;
            }
            else
            {
                console.txtCustomLoginUri.Text = G.LoginURI;
                console.cbxGrid.SelectedIndex = gridIx;
            }
        }

        static public void GetLoginOptionsFromRadegast(RadegastInstance instance, BotClient TheBot)
        {
            var BotLoginParams = TheBot.BotLoginParams;
            var from = instance.Netcom.LoginOptions;
            BotLoginParams.FirstName = from.FirstName;
            BotLoginParams.LastName = from.LastName;
            BotLoginParams.Password = from.Password;
            BotLoginParams.URI = from.GridCustomLoginUri;
            Grid g = from.Grid;
            if (g != null)
            {
                BotLoginParams.URI = g.LoginURI;
            }
            switch (from.StartLocation)
            {
                case StartLocationType.Last:
                    BotLoginParams.Start = "last";
                    break;
                case StartLocationType.Home:
                    BotLoginParams.Start = "home";
                    break;
                default:
                    BotLoginParams.Start = from.StartLocationCustom;
                    break;
            }
            BotLoginParams.Version = from.Version;
            BotLoginParams.Channel = from.Channel;
            SetRadegastLoginOptions(instance, TheBot);
        }

        public static void WriteDebugLine(string mesg, Color color, string timeStamp, string named)
        {
            if (CogbotGUI.TheDebugConsoleRTB == null)
            {
                var O = Console.Out;
                if (O == null) return;
                O.WriteLine(mesg);
                return;
            }
            RichTextBox rtbLog = TheDebugConsoleRTB;
            if (rtbLog.InvokeRequired)
            {
                if (rtbLog.IsHandleCreated)
                    rtbLog.BeginInvoke(new MethodInvoker(() => WriteDebugLine(mesg, color, timeStamp, named)));
                return;
            }
            Color prev = rtbLog.SelectionColor;
            try
            {
                rtbLog.SelectionColor = Color.FromKnownColor(KnownColor.WindowText);
                if (!String.IsNullOrEmpty(timeStamp))
                {
                    rtbLog.AppendText(String.Format("{0} ", timeStamp));
                }
                if (!String.IsNullOrEmpty(named))
                {
                    rtbLog.AppendText("[");
                    rtbLog.SelectionColor = color;
                    rtbLog.AppendText(named);
                    rtbLog.SelectionColor = Color.FromKnownColor(KnownColor.WindowText);
                    rtbLog.AppendText("]: - ");
                }
                rtbLog.AppendText(String.Format("{0}{1}", mesg, Environment.NewLine));
            }
            finally
            {
                rtbLog.SelectionColor = prev;
            }
        }
    }
}
