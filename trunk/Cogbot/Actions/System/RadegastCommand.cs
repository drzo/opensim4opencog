using System;
using System.ComponentModel;
using System.Threading;
using System.Windows.Forms;
using cogbot;
using cogbot.Actions;
using OpenMetaverse;
using org.opencyc.xml;
using Radegast;

namespace cogbot.Actions.System
{
    public class ShowRadCommand : Command, BotSystemCommand
    {
        public ShowRadCommand(BotClient testClient)
        {
            Name = "showgui";
            Description = "Shows the Radegast UI";
            Category = CommandCategory.BotClient;
        }
        internal frmMain PanelGUI;

        public override CmdResult Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
            if (args==null || args.Length==0) args =new []{"show"};
            if (PanelGUI == null)
            {
                PanelGUI = Client.TheRadegastInstance.MainForm;
                // (new Thread(() =>
                {
                    PanelGUI.Closing += new CancelEventHandler(delegate(object sender, CancelEventArgs e)
                    {
                        PanelGUI = null;
                    });
                    //Application.EnableVisualStyles();
                    ////PanelGUI.Invoke(new MethodInvoker(PanelGUI.Show));
                    //PanelGUI.BeginInvoke(new MethodInvoker(() =>
                    //                                           {
                    //                                               (new Thread(() => {
                    //                                               Application.Run(PanelGUI);
                    //                                               })).Start();
                    //                                           }));
                    //Application.Run(PanelGUI);
                }
                // )).Start();
            }
            string arg0 = args[0].ToLower();


            switch (arg0)
            {
                case "show":
                    {
                        //(new Thread(() => {
                        BeginInvoke(new MethodInvoker(() =>
                        {
                            PanelGUI.Visible = true;
                            PanelGUI.Show();
                        }));
                        //PanelGUI.Show();
                        //})).Start();
                        return Success("radegast shown");
                    }
                case "hide":
                    {
                        if (!PanelGUI.IsHandleCreated) return Success("No handle to " + arg0);
                        BeginInvoke(new MethodInvoker(() =>
                        {
                            PanelGUI.Visible = false;
                        }));
                        return Success("radegast " + arg0);
                    }
                case "maximize":
                    {
                        if (!PanelGUI.IsHandleCreated) return Success("No handle to " + arg0);
                        BeginInvoke(new MethodInvoker(() =>
                        {
                            PanelGUI.WindowState = FormWindowState.Maximized;
                            PanelGUI.Visible = true;
                        }));
                        return Success("radegast " + arg0);
                    }
                case "minimize":
                    {
                        if (!PanelGUI.IsHandleCreated) return Success("No handle to " + arg0);
                        BeginInvoke(new MethodInvoker(() =>
                        {
                            PanelGUI.WindowState = FormWindowState.Minimized;
                        }));
                        return Success("radegast " + arg0);
                    }
                case "normal":
                    {
                        if (!PanelGUI.IsHandleCreated) return Success("No handle to " + arg0);
                        BeginInvoke(new MethodInvoker(() =>
                        {
                            PanelGUI.WindowState = FormWindowState.Normal;
                            PanelGUI.Visible = true;
                        }));
                        return Success("radegast " + arg0);
                    }
                default:
                    return Success("Unknow state");
            }
        }

        private void BeginInvoke(MethodInvoker invoker)
        {
            if (!PanelGUI.InvokeRequired)
            {
                Success("No invoke required ");
                invoker();
            }
            else
            {
                PanelGUI.BeginInvoke(invoker);
            }
        }
    }
}
