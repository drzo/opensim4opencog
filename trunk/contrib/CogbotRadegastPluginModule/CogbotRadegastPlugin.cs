using System;
using System.ComponentModel;
using System.Threading;
using System.Windows.Forms;
using cogbot;
using cogbot.Actions;
using cogbot.GUI;
using OpenMetaverse;
using Radegast;

namespace CogbotRadegastPluginModule
{
    public class CogbotRadegastPlugin : IRadegastPlugin
    {
        public CogbotRadegastPlugin()
        {
        }

        public RadegastInstance RadegastInstance;
        private CogbotTabWindow chatConsole;
        private SleekTab tab;
        private ClientManager clientManager;
        private CogbotRadegastInterpreter cogbotRadegastInterpreter;


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
            cogbotRadegastInterpreter = new CogbotRadegastInterpreter(clientManager);
            RadegastInstance.CommandsManager.LoadInterpreter(cogbotRadegastInterpreter);
            chatConsole = new CogbotTabWindow(inst, clientManager)
                              {
                                  Dock = DockStyle.Fill,
                                  Visible = false
                              };
            tab = inst.TabConsole.AddTab("cogbot", "Cogbot", chatConsole);
            tab.AllowClose = false;
            tab.AllowDetach = true;

            RadegastInstance.Client.Network.OnConnected += Plugin_OnConnected;

            clientManager.outputDelegate = WriteLine;
            inst.Client.Network.OnSimConnecting += Network_OnSimConnecting;
            clientManager.StartUpLisp();
        }

        private void Plugin_OnConnected(object sender)
        {
            clientManager.LastBotClient.Invoke(() => clientManager.LastBotClient.AddTab("aspects", "Aspects",new SimAspectConsole(RadegastInstance),null));
           
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