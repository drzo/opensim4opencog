using System;
using System.ComponentModel;
using System.Threading;
using System.Windows.Forms;
using cogbot;
using cogbot.Listeners;
using cogbot.TheOpenSims;
using OpenMetaverse;
using Radegast;
//using RadegastTab = Radegast.SleekTab;

namespace CogbotRadegastPluginModule
{
    public class CogbotRadegastPlugin : IRadegastPlugin
    {
        public CogbotRadegastPlugin(RadegastInstance instance)
        {
            RadegastInstance = instance;
        }       

        public RadegastInstance RadegastInstance;
        public CogbotContextMenuListener CogbotContextMenuListener;
        private CogbotTabWindow chatConsole;
        private SimObjectsConsole _simObjectsConsole;
        private RadegastTab tab;
        private ClientManager clientManager;
        private BotClient _theBot;
        private CogbotRadegastInterpreter cogbotRadegastInterpreter;
        private CogbotNotificationListener CogbotNoticeuListener;
        private CommandContextAction _commandContextAction;
        private SimUsageContextAction _simUsageContextAction;
        private AspectContextAction _aspectContextAction;

        public BotClient TheBot
        {
            get
            {
                if (_theBot != null) return _theBot;
                return clientManager.LastBotClient;
            }
            set
            {
                //   clientManager.LastBotClient = value;
                _theBot = value;
            }
        }

        public void StartPlugin(RadegastInstance inst)
        {
            RadegastInstance = inst;
            try
            {
                // inst.MainForm.Invoke(new MethodInvoker(() => StartPlugin0(inst)));               
                StartPlugin0(RadegastInstance);
            }
            catch (Exception ex)
            {
                Logger.Log("[COGBOT PLUGIN] exception " + ex, Helpers.LogLevel.Error, ex);
            }
        }

        public void StartPlugin0(RadegastInstance inst)
        {
            RadegastInstance = inst;
            RadegastInstance.MainForm.Closing += MainForm_Closing;
            CogbotContextMenuListener = new CogbotContextMenuListener();
            CogbotNoticeuListener = new CogbotNotificationListener();
            if (ClientManager.UsingRadgastFromCogbot)
            {
                // just unregister events for now
                inst.Netcom.Dispose();
                clientManager = ClientManager.SingleInstance;
                _theBot = clientManager.LastBotClient;
            }
            else
            {
                ClientManager.UsingCogbotFromRadgast = true;
                clientManager = ClientManager.SingleInstance ?? new ClientManager();
            }
            cogbotRadegastInterpreter = new CogbotRadegastInterpreter(clientManager);
            RadegastInstance.CommandsManager.LoadInterpreter(cogbotRadegastInterpreter);
            _commandContextAction = new CommandContextAction(inst, this);
            inst.TabConsole.RegisterContextAction(_commandContextAction);
            _aspectContextAction = new AspectContextAction(inst, this);
            inst.TabConsole.RegisterContextAction(_aspectContextAction);
            _simUsageContextAction = new SimUsageContextAction(inst, this);
            inst.TabConsole.RegisterContextAction(_simUsageContextAction);

            if (ClientManager.UsingRadgastFromCogbot) return;
            inst.Client.Settings.MULTIPLE_SIMS = true;
            clientManager.outputDelegate = WriteLine;
            chatConsole = new CogbotTabWindow(inst, this)
                              {
                                  Dock = DockStyle.Fill,
                                  Visible = false
                              };
            tab = inst.TabConsole.AddTab("cogbot", "Cogbot", chatConsole);
            tab.AllowClose = false;
            tab.AllowDetach = true;

            _simObjectsConsole = new SimObjectsConsole(inst, this)
                                     {
                                         Dock = DockStyle.Fill,
                                         // Visible = false
                                     };
            tab = inst.TabConsole.AddTab("simobjects", "SimObjects", _simObjectsConsole);
            tab.AllowClose = false;
            tab.AllowDetach = true;

            RadegastTab tab1 = RadegastInstance.TabConsole.GetTab("chat");
            tab1.AllowDetach = true;
            ChatConsole rchatConsole = (ChatConsole)tab1.Control;
            rchatConsole.cbxInput.Enabled = true;
            rchatConsole.btnSay.Enabled = true;
            //  rchatConsole.btnShout.Enabled = true;
            RadegastTab tab2 = RadegastInstance.TabConsole.GetTab("login");
            tab2.AllowDetach = true;
            //RadegastTab tab3 = RadegastInstance.TabConsole.GetTab("search");
            //tab3.Control = new METAbolt.SearchConsole(inst);
            var sc = new METAbolt.SearchConsole(inst)
            {
                Dock = DockStyle.Fill,
                // Visible = false
            };
            tab = inst.TabConsole.AddTab("cogbotsearch", "CogbotSearch", sc);
            clientManager.ProcessCommandArgs();
            new Thread(() =>
                           {
                               clientManager.StartUpLisp();
                               clientManager.EnsureManagerStarting();
                           }).Start();
        }

        private void MainForm_Closing(object sender, CancelEventArgs e)
        {
            clientManager.ShutDown();
        }

        private void WriteLine(string str, object[] args)
        {
            if (args == null || args.Length == 0)
            {
                args = new object[] { str };
                str = "{0}";
            }
            if (chatConsole == null)
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

        public void DisplayNotificationInChat(string format)
        {
            TheBot.DisplayNotificationInChat(format);
        }
    }
}