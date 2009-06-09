using System;
using System.Collections.Generic;
using System.Text;
using System.Threading;
using cogbot;
using cogbot.Listeners;
using CycWorldModule.DotCYC;

namespace CycWorldModule
{
    public class CycWorldModule : WorldObjectsModule
    {
        private SimCyclifier cyclifier;
        private static CycWorldModule cycModule;
        private CycConnectionForm cycConnectionForm;
        readonly static object oneInstanceLock = new object();
        public CycWorldModule(BotClient _parent)
            : base(_parent)
        {
            lock (oneInstanceLock)
                if (cycModule == null)
                {
                    cycModule = this;
                    _parent.ClientManager.AddTool("CycWorldModule","CycWorldModule", ShowCycForm);
                    cyclifier = new SimCyclifier(this);

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
            //TODO throw new NotImplementedException();
        }

        public override void ShutdownListener()
        {
            //TODO throw new NotImplementedException();
        }
    }
}
