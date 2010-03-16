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
            {
                //(new Thread(() => {
                PanelGUI.Invoke(new MethodInvoker(PanelGUI.Show));
                //PanelGUI.Show();
                //})).Start();
                return Success("radegast shown");
            }
        }
    }
}
