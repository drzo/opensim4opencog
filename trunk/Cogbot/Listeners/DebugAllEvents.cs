using System;
using System.Collections.Generic;
using System.Text;
using MushDLR223.Utilities;
using OpenMetaverse;

namespace cogbot.Listeners
{
    sealed public class DebugAllEvents: AllEvents
    {
        public DebugAllEvents(BotClient bc)
            : base(bc)
        {
          // the subclass must now run this
            if (this.GetType()==typeof(DebugAllEvents))
            RegisterAll();
        }

        ~DebugAllEvents()
        {
            UnregisterAll();
        }

        public override string GetModuleName()
        {
            return "DebugAllEvents";
        }

        public override void StartupListener()
        {
            RegisterAll();
        }

        public override void Dispose()
        {
            UnregisterAll();
        }

        public override bool BooleanOnEvent(string eventName, string[] paramNames, Type[] paramTypes, params object[] parameters)
        {

            if (eventName.EndsWith("On-Image-Receive-Progress")) return true;
            if (eventName.EndsWith("Look-At")) return true;
            DLRConsole.DebugWriteLine("\n" + eventName);
            for (int i = 0; i < paramNames.Length;i++ )
            {
                DLRConsole.DebugWriteLine(" " + paramNames[i] + ": " + client.argString(parameters[i]));
            }
            return true;
        }
    }
}
