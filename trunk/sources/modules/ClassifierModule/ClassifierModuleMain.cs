using System;
using System.Collections.Generic;
using System.Text;
using System.Threading;
using cogbot;
using cogbot.Listeners;
using OpenMetaverse;

namespace ClassifierModule
{
    class ClassifierModuleMain : WorldObjectsModule
    {
        public ClassifierModuleMain(BotClient _parent)
            : base(_parent)
        {

        }

        public override void Dispose()
        {
           // throw new NotImplementedException();
        }

        public override string GetModuleName()
        {
            return this.GetType().Namespace;
        }

        public override void StartupListener()
        {
           // throw new NotImplementedException();
        }
    }
}
