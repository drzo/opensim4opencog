using System;
using System.Collections.Generic;
using System.Text;
using System.Threading;
using cogbot;
using cogbot.Listeners;
using cogbot.DotCYC;

namespace CycWorldModule
{
    public class CycWorldModule : WorldObjectsModule
    {
        private static CycWorldModule cycModule;
        readonly static object oneInstanceLock = new object();
        public CycWorldModule(BotClient _parent)
            : base(_parent)
        {
            lock (oneInstanceLock)
                if (cycModule == null)
                {
                    cycModule = this;
                    _parent.ClientManager.AddTool("CycWorldModule","CycWorldModule", ShowCycForm);
                }
        }

        private void ShowCycForm(object sender, EventArgs e)
        {
            throw new NotImplementedException();
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
