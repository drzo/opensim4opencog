using System;
using System.Collections.Generic;
using cogbot.TheOpenSims;
using OpenMetaverse;

namespace cogbot.Actions
{
    class EventInfoCommand : Command
    {

        public EventInfoCommand(BotClient Client)
        {
            Name = "evinfo";
            helpString = "Shows the events that have been associated with an object.";
            usageString = "evinfo [primid]";
        }

        public override string Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
            //   base.acceptInput(verb, args);

            BotClient Client = TheBotClient;
            string subject = String.Join(" ", args);
            if (subject.Length == 0)
            {
                return TheSimAvatar.DebugInfo();
            }
            Client.describeNext = false;
            float range;
            if (float.TryParse(subject, out range))
            {
                SimAvatar simAva = WorldSystem.TheSimAvatar;
                if (simAva != null)
                {
                    List<SimObject> objs = ((SimObjectImpl)simAva).GetNearByObjects((double)range, false);
                    if (objs.Count > 0)
                    {
                        foreach (SimObject o in objs)
                        {
                            string s = DebugInfo(o);
                            WriteLine(s);
                        }
                        return "simEventComplete";
                    }
                }
            }
            else
            {
                if (TheBotClient.describers.ContainsKey(subject))
                    TheBotClient.describers[subject].Invoke(true, WriteLine);
                else
                {
                    int count = 0;
                    SimAvatar simAva = WorldSystem.TheSimAvatar;
                    if (simAva != null)
                    {
                        List<SimObject> objs = simAva.GetKnownObjects();
                        if (objs.Count > 0)
                        {
                            foreach (SimObject o in objs)
                            {
                                if (o.Matches(subject))
                                {
                                    count++;
                                    string s = DebugInfo(o);
                                    WriteLine(s);
                                }
                            }
                        }
                    }
                    if (count==0)
                    {
                        foreach (SimObject o in WorldSystem.GetAllSimObjects())
                        {
                            if (o.Matches(subject))
                            {
                                count++;
                                string s = DebugInfo(o);
                                WriteLine(s);
                            }
                        }                        
                    }
                }
            }
            return "simEventComplete";
        }

        private string DebugInfo(SimObject o)
        {
            string s = o.DebugInfo();
            return s.Replace("{", "{{").Replace("}", "}}");
        }
    }
}