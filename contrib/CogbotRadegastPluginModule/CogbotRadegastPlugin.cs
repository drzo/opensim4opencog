using System;
using System.Collections.Generic;
using System.Threading;
using System.Windows.Forms;
using cogbot;
using cogbot.Listeners;
using OpenMetaverse;
using Radegast;

namespace CogbotRagegastPluginModule
{
    public class CogbotRadegastPlugin : IRadegastPlugin
    {
        public CogbotRadegastPlugin()
        {
        }

        public static RadegastInstance RadegastInstance;
        private CogbotTabWindow chatConsole;
        private SleekTab tab;
        private ClientManager clientManager;

        public void StartPlugin(RadegastInstance inst)
        {

            RadegastInstance = inst;
            clientManager = new ClientManager();
            chatConsole = new CogbotTabWindow(inst, clientManager);
            chatConsole.Dock = DockStyle.Fill;
            chatConsole.Visible = false;

            //toolStripContainer1.ContentPanel.Controls.Add(chatConsole);

            tab = inst.TabConsole.AddTab("Cogbot Main", "Cogbot", chatConsole);
            tab.AllowClose = false;
            tab.AllowDetach = false;
            clientManager.outputDelegate = chatConsole.WriteLine;
           // control = ClientManager.SingleInstance;
//            control.SelectIMInput();       
            inst.Client.Network.OnSimConnecting += Network_OnSimConnecting;
            clientManager.StartUpLisp();
        }

        private bool Network_OnSimConnecting(Simulator simulator)
        {
            return true;
        }

        public void StopPlugin(RadegastInstance inst)
        {
        }
    }
}