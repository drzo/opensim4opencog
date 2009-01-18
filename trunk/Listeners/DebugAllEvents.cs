using System;
using System.Collections.Generic;
using System.Text;

namespace cogbot.Listeners
{
    public class DebugAllEvents: AllEvents
    {
        public DebugAllEvents(BotClient bc)
            : base(bc)
        {
            RegisterAll();
        }



        public override bool BooleanOnEvent(string eventName, string[] paramNames, Type[] paramTypes, params object[] parameters)
        {

            if (eventName.EndsWith("On-Image-Receive-Progress")) return true;
            if (eventName.EndsWith("Look-At")) return true;
            Console.WriteLine("\n"+eventName);
            for (int i = 0; i < paramNames.Length;i++ )
            {
                Console.WriteLine(" " + paramNames[i] + ": " + client.argString( parameters[i]));
            }
            return true;
        }
    }
}
