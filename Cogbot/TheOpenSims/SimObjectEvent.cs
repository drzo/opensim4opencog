using System;

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
        
        public FirstOrderTerm GetTerm()
        {
            throw new NotImplementedException();
        }

    }
}