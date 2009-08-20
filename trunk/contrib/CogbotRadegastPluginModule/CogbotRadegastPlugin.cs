using System;
using System.ComponentModel;
using System.Threading;
using System.Windows.Forms;
using cogbot;
using cogbot.Actions;
using OpenMetaverse;
using Radegast;

namespace CogbotRadegastPluginModule
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
            if (ClientManager.UsingRadgastFromCogbot)
            {
                // just unregister events for now
                inst.Netcom.Dispose();
                return;
            }
            ClientManager.UsingCogbotFromRadgast = true;
            clientManager = new ClientManager();
            chatConsole = new CogbotTabWindow(inst, clientManager)
                              {
                                  Dock = DockStyle.Fill,
                                  Visible = false
                              };
            tab = inst.TabConsole.AddTab("Cogbot", "Cogbot", chatConsole);
            tab.AllowClose = false;
            tab.AllowDetach = true;        

            clientManager.outputDelegate = WriteLine;
            inst.Client.Network.OnSimConnecting += Network_OnSimConnecting;
            clientManager.StartUpLisp();
        }

        private void WriteLine(string str, object[] args)
        {
            MergeSplitWorkArround();
            chatConsole.WriteLine(str, args);
        }

        private bool _mergeSplitWorkArrounded = false;
        private void MergeSplitWorkArround()
        {
            if (_mergeSplitWorkArrounded) return;
            _mergeSplitWorkArrounded = false;
            //RadegastInstance.TabConsole.tabs["chat"].MergeWith(tab);
            //RadegastInstance.TabConsole.SplitTab(tab);
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