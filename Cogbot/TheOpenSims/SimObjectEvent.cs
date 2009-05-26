using System;
using org.opencyc.cycobject;

namespace cogbot.TheOpenSims
{
    public class SimObjectEvent: BotMentalAspect
    {
        public string EventName;
        public object[] Parameters;
        public SimObjectEvent(string eventName, object[] args, int argN)
        {
            EventName = eventName;
            Parameters = args;
        }
        private CycFort fort;
        public CycFort GetCycFort()
        {
            if (fort == null)
            {
                fort = TextForm.Cyclifier.FindOrCreateCycFort(this);
            }
            return fort;
        }

    }
}