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
        public CogbotContextMenuListener CogbotContextMenuListener;
        private CogbotTabWindow chatConsole;
        private SleekTab tab;
        private ClientManager clientManager;
        private CogbotRadegastInterpreter cogbotRadegastInterpreter;

        public void StartPlugin(RadegastInstance inst)
        {
            try
            {
                StartPlugin0(inst);
            }
            catch (Exception ex)
            {
                Console.WriteLine(""+ex);
            }
        }

        public void StartPlugin0(RadegastInstance inst)
        {
            RadegastInstance = inst;
            CogbotContextMenuListener = new CogbotContextMenuListener();
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
            SleekTab tab1 = RadegastInstance.TabConsole.GetTab("chat");
            tab1.AllowDetach = true;
            SleekTab tab2 = RadegastInstance.TabConsole.GetTab("login");
            tab2.AllowDetach = true;

            //RadegastInstance.Client.Network.OnConnected += Plugin_OnConnected;

            clientManager.outputDelegate = WriteLine;
            inst.Client.Network.OnSimConnecting += Network_OnSimConnecting;
          //  tab.Select();
            clientManager.StartUpLisp();
        }

        private void Plugin_OnConnected(object sender)
        {
            //clientManager.LastBotClient.Invoke(() => clientManager.Lai didnt get it stBotClient.AddTab("aspects", "Aspects", new SimAspectConsole(RadegastInstance), null));
            //clientManager.LastBotClient.Invoke(() => RadegastInstance.TabConsole.AddContextMenu(new AspectContextAction(RadegastInstance)
            //{
            //}));

  
        }

        private void WriteLine(string str, object[] args)
        {
            chatConsole.WriteLine(str, args);
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