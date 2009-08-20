using System.ComponentModel;
using System.Threading;
using System.Windows.Forms;
using cogbot;
using cogbot.Actions;
using OpenMetaverse;
using org.opencyc.xml;
using Radegast;

namespace cogbot.Actions
{
    public class ShowRadCommand : Command, BotSystemCommand
    {
        public ShowRadCommand(BotClient testClient)
        {
            Name = "showgui";
            Description = "Shows the Radegast UI";
            Category = CommandCategory.TestClient;
        }
        internal frmMain PanelGUI;

        public override string Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
            if (PanelGUI == null)
            {
                PanelGUI = Client.TheRadegastInstance.MainForm;
                (new Thread(() =>
                {
                    PanelGUI.Closing += new CancelEventHandler(delegate(object sender, CancelEventArgs e)
                    {
                        PanelGUI = null;
                    });
                    Application.EnableVisualStyles();
                    PanelGUI.Show();
                    Application.Run(PanelGUI);
                })).Start();
                return "radegast UI started";
            }
            else
            {
                PanelGUI.Show();
                return "radegast shown";
            }
        }
    }
}
