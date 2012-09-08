using System;
using System.Collections.Generic;
using System.Text;
using Cogbot.World;
using Cogbot.Utilities;
using MushDLR223.ScriptEngines;
using MushDLR223.Utilities;
using OpenMetaverse;
using System.Threading;
#if USE_SAFETHREADS
using Thread = MushDLR223.Utilities.SafeThread;
#endif

namespace Cogbot
{
    public class SimEventTextSubscriber : SimEventSubscriber
    {
        readonly BotClient From;
        readonly OutputDelegate textForm;
        public SimEventTextSubscriber(OutputDelegate _text, BotClient from)
        {
            From = from;
            textForm = _text;
            EventsEnabled = true;
        }

        #region SimEventSubscriber Members

        void SimEventSubscriber.OnEvent(CogbotEvent evt)
        {
            if (!EventsEnabled) return;
            const SimEventType du = SimEventType.DATA_UPDATE;
            if (evt.IsEventType(du)) return;

            if (evt.IsEventType(SimEventType.EFFECT))
            {
                if (evt.Verb == "LookAtType-Idle") return;
                if (evt.Verb == "LookAtType-FreeLook") return;
            }
            String eventName = evt.Verb;
            object[] args = evt.GetArgs();

            String msg = "["+ From.GetName() + ": " + eventName.ToLower()+"]";
            int start = 0;
            if (args.Length > 1)
            {
                if (args[0] is Simulator)
                {
                   // start = 1;
                }
            }
            for (int i = start; i < args.Length; i++)
            {
                msg += " ";
                msg += From.argString(args[i]);
            }
            if (msg.Contains("Transfer failed with status code")) return;
            msg += "";
            
            textForm(msg);
        }

        void SimEventSubscriber.Dispose()
        {
            EventsEnabled = false;
            textForm("SimEventTextSubscriber shutdown for " + From);
        }

        public bool EventsEnabled { get; set; }

        #endregion
    }
}
