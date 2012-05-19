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
            Description = "Shows the events that have been associated with an object. See <a href='wiki/BotCommands#Events'>Events</a> for info about events.";
            Usage = @"<p>evinfo &lt;primspec&gt;</p><p>example: evinfo tacosofgod  <i>tacosofgod is a nearby plywood cube</i></p>
<pre>
[09:12] tacosofgod Box 70b5e8ab-3308-4bc6-bbf8-4f313cd7d518 (localID 2036105563)(ch0)(PrimFlagsFalse InventoryEmpty, ObjectOwnerModify)[](!IsPassable)
[09:12] evinfo: Success: simEventComplete blanks=1 nonblanks=0
[09:12] evinfo: Success: simEventComplete blanks=1 nonblanks=0
</pre>";
            Parameters = NamedParam.CreateParams("object", typeof(PrimSpec),
                "The object whose events we want, as specified in <a href='wiki/BotCommands#PrimSpec'>Prim Spec</a>");
            ResultMap = NamedParam.CreateParams(
                 "message", typeof(string), "if success was false, the reason why",
                 "success", typeof(bool), "true if we got the events");
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
                            if (o.ActionEventQueue == null || o.ActionEventQueue.Count == 0)
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
                    if (o.ActionEventQueue == null || o.ActionEventQueue.Count == 0)
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
            if (ActionEventQueue != null)
            {
                lock (ActionEventQueue)
                {
                    s += " ActionCount= " + ActionEventQueue.Count;
                    foreach (SimObjectEvent s1 in ActionEventQueue)
                    {
                        s += "\n " + s1;
                    }
                }
            }
            return s.Replace("{", "{{").Replace("}", "}}");
        }
    }
}