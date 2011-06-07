using System;
using System.Collections.Generic;
using cogbot.TheOpenSims;
using OpenMetaverse;

using MushDLR223.ScriptEngines;

namespace cogbot.Actions.Inventory
{
    class EventInfoCommand : Command, RegionMasterCommand
    {

        public EventInfoCommand(BotClient Client)
        {
            Name = "evinfo";
            Description = "Shows the events that have been associated with an object.";
            Usage = "evinfo [primid]";
            Parameters = new [] {  new NamedParam(typeof(SimObject), typeof(UUID)) };
        }

        public override CmdResult Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
            //   base.acceptInput(verb, args);

            BotClient Client = TheBotClient;
            string subject = String.Join(" ", args);
            if (subject.Length == 0)
            {
                return Success(TheSimAvatar.DebugInfo());
            }
            Client.describeNext = false;
            float range;
            int blanks = 0;
            int nonblanks = 0;

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
                            if (o.ActionEventQueue.Count == 0)
                            {
                                blanks++;
                                if (!(o is SimAvatar)) continue;
                            } 
                            string s = DebugInfo(o);
                            WriteLine(s);
                            nonblanks++;
                        }
                    }
                }
            }
            else
            {
                int argsUsed = 0;
                var PS = WorldSystem.GetPrimitives(Parser.SplitOff(args, argsUsed), out argsUsed);
                foreach (SimObject o in PS)
                {
                    string s = DebugInfo(o);
                    WriteLine(s);
                    if (o.ActionEventQueue.Count == 0)
                    {
                        blanks++;
                        continue;
                    }
                    nonblanks++;
                }
            }
            return Success("simEventComplete blanks=" + blanks + " nonblanks=" + nonblanks);
        }

        private string DebugInfo(SimObject o)
        {
            var ActionEventQueue = o.ActionEventQueue;
            string s = o.ToString();
            lock (ActionEventQueue)
            {
                s += " ActionCount= " + ActionEventQueue.Count;
                foreach (SimObjectEvent s1 in ActionEventQueue)
                {
                    s += "\n " + s1;
                }
            }
            return s.Replace("{", "{{").Replace("}", "}}");
        }
    }
}