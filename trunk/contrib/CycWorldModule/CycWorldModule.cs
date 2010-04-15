using System;
using cogbot;
using cogbot.Listeners;
using CycWorldModule.DotCYC;
using Radegast;

namespace CycWorldModule
{
    public class CycWorldModule : WorldObjectsModule
    {
        readonly internal SimCyclifier cyclifier;
        public static CycWorldModule CycModule;
        private CycConnectionForm cycConnectionForm;
        readonly static object oneInstanceLock = new object();
        private RadegastTab cycTab;
        private CycBrowser cycBrowser;

        public Radegast.RadegastInstance RadegastInstance
        {
            get
            {
                return client.TheRadegastInstance;
            }
        }
        public CycWorldModule(BotClient _parent)
            : base(_parent)
        {
            lock (oneInstanceLock)
                if (CycModule == null)
                {
                    CycModule = this;
                    _parent.ClientManager.AddTool("CycWorldModule","CycWorldModule", ShowCycForm);
                    cyclifier = new SimCyclifier(this);
                    client.WorldSystem.OnAddSimObject += cyclifier.World_OnSimObject;
                    client.AddBotMessageSubscriber(cyclifier.eventFilter);
                    Console.WriteLine("CycWorldModule Loaded");

                }
        }

        public CycConnectionForm CycConnectionForm
        {
            get
            {
                if (cycConnectionForm == null || cycConnectionForm.IsDisposed) 
                    cycConnectionForm = new CycConnectionForm();
                return cycConnectionForm;
            }
        }

        private void ShowCycForm(object sender, EventArgs e)
        {
            CycConnectionForm.Reactivate();
        }


        public override string GetModuleName()
        {
            return "CycWorldModule";
        }

        public override void StartupListener()
        {
            cycBrowser = new CycBrowser(RadegastInstance);
            cycTab = client.TheRadegastInstance.TabConsole.AddTab("cyc", "CYC", cycBrowser);
        }

        public override void Dispose()
        {
            //TODO throw new NotImplementedException();
        }

        public void Show(Object position)
        {
            object fort = cyclifier.ToFort(position);
            var noQ = SimCyclifier.cycInfoMapSaver.NoQueue;
            try
            {
                SimCyclifier.cycInfoMapSaver.NoQueue = true;
                cyclifier.DataUpdate(position);
            }
            finally
            {
                SimCyclifier.cycInfoMapSaver.NoQueue = noQ;
            }
            cycBrowser.Show(fort);
        }
    }
}
