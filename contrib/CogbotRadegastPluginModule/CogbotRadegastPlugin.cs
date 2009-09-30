using System;
using System.ComponentModel;
using System.Threading;
using System.Windows.Forms;
using cogbot;
using OpenMetaverse;
using Radegast;
//using RadegastTab = Radegast.SleekTab;

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
        private RadegastTab tab;
        private ClientManager clientManager;
        private CogbotRadegastInterpreter cogbotRadegastInterpreter;
        private CogbotNotificationListener CogbotNoticeuListener;

        public void StartPlugin(RadegastInstance inst)
        {
            RadegastInstance = inst;
            CogbotContextMenuListener = new CogbotContextMenuListener();
            CogbotNoticeuListener = new CogbotNotificationListener();
            if (ClientManager.UsingRadgastFromCogbot)
            {
                // just unregister events for now
                inst.Netcom.Dispose();
                return;
            }
            ClientManager.UsingCogbotFromRadgast = true;
            inst.Client.Settings.MULTIPLE_SIMS = true;
            clientManager = new ClientManager();
            cogbotRadegastInterpreter = new CogbotRadegastInterpreter(clientManager);
            RadegastInstance.CommandsManager.LoadInterpreter(cogbotRadegastInterpreter);
            clientManager.outputDelegate = WriteLine;
            clientManager.StartUpLisp();

            StartPlugin0(inst);
            return;

            if (inst.MainForm.IsHandleCreated)
            {
                inst.MainForm.Invoke(new MethodInvoker(() => StartPlugin0(inst)));
            } else
            inst.MainForm.Load += MainForm_Load;
        }

        private void MainForm_Load(object sender, EventArgs e)
        {
            RadegastInstance.MainForm.Load -= MainForm_Load;
            try
            {
                // inst.MainForm.Invoke(new MethodInvoker(() => StartPlugin0(inst)));               
                StartPlugin0(RadegastInstance);
            }
            catch (Exception ex)
            {
                Logger.Log(" exception " + ex, Helpers.LogLevel.Error, ex);
            }
        }

        public void StartPlugin0(RadegastInstance inst)
        {

            chatConsole = new CogbotTabWindow(inst, clientManager)
                              {
                                  Dock = DockStyle.Fill,
                                  Visible = false
                              };
            tab = inst.TabConsole.AddTab("cogbot", "Cogbot", chatConsole);
            tab.AllowClose = false;
            tab.AllowDetach = true;
            RadegastTab tab1 = RadegastInstance.TabConsole.GetTab("chat");
            tab1.AllowDetach = true;
            RadegastTab tab2 = RadegastInstance.TabConsole.GetTab("login");
            tab2.AllowDetach = true;
        }

        private void WriteLine(string str, object[] args)
        {
            if (chatConsole==null)
            {
                Console.WriteLine(str,args);
            }
            else
            {
                chatConsole.WriteLine(str, args);                
            }
        }

        public void StopPlugin(RadegastInstance inst)
        {
        }

    }
}