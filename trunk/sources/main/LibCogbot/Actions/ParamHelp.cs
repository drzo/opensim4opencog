using System;
using System.Collections.Generic;
using System.Text;
using OpenMetaverse;

using MushDLR223.ScriptEngines;

namespace cogbot.Actions.System
{
    class ParamHelp : Command, SystemApplicationCommand
    {
        public ParamHelp(BotClient Client)
            : base(Client)
        {
            Description = "Searches the cogbot help system for botcmds containing a specific string";
            Details = 
@"<p>paramhelp &lt;string&gt;</p>
<p>Example:</p>
<pre>
paramhelp move
[19:15] autopilot: Moves the avatar to the specified global position using simulator autopilot.  Usage: autopilot x y z
[19:15] back: Sends the move back command to the server for a single packet or a given number of seconds.  Usage: back [seconds]
[19:15] deletefolder: Moves a folder to the Trash Folder  Usage:
[19:15] forward: Sends the move forward command to the server for a single packet or a given number of seconds.  Usage: forward [seconds]
[19:15] left: Sends the move left command to the server for a single packet or a given number of seconds.  Usage: left [seconds]
[19:15] move: Move to a person or object, or in a direction: west, east, north or south.  Usage: Type " + "\"west/east/north/south\"" + 
@" to move 5 meters in a direction. Or Type " +"\"west distance/east distance/north distance/south distance\"" + 
@" to move a specific distance in that direction.
[19:15] moveprim: move prim to the relative specified position.  Usage: moveprim <prim> <position>
[19:15] moveto: Moves the avatar to the specified global position using robot turnto and walk.  Usage: moveto x y z
[19:15] n/s/e/w: Move to a person or object, or in a direction: west, east, north or south.  Usage: Type " + "\"west/east/north/south\"" +
@"to move 5 meters in a direction. Or Type " + "\"west distance/east distance/north distance/south distance\"" +
@"to move a specific distance in that direction.
[19:15] stopmoving: stops all movement threads  Usage:
[19:15] thread: Executes a command in its own thread. For example, thread anim 30 crouch returns immediately, but the bot continues to crouch for 30 seconds  Usage: <p>thread &lt;command&gt;</p><p>example: thread anim 30 crouch</p><p>example: thread moveto FluffyBunny Resident</p>
[19:15] paramhelp: Success: Help complete
[19:15] paramhelp: Success: Help complete
</pre>";
            Parameters = CreateParams("term", typeof(string), "term to search for");
            ResultMap = CreateParams(
                 "message", typeof(string), "if term not found, will be <i>I don't know about ...</i>",
                 "success", typeof(bool), "true if it found a match");
            Category = CommandCategory.BotClient;
            Name = "paramhelp";
        }

        public override CmdResult acceptInput(string verb, Parser args, OutputDelegate WriteLine)
        {
            string mustHave = ": "; // everything
            if (args.Length > 0)
            {
                mustHave = args.str.ToLower();
            }
            int found = 0;
            foreach (string action in TheBotClient.Commands.Keys)
            {
                string s = action + ": " + TheBotClient.Commands[action].Description;
                if (!s.ToLower().Contains(mustHave))
                {
                    continue;
                }
                found++;
                WriteLine(s);
            }
            foreach (string tutorial in TheBotClient.tutorials.Keys)
            {
                string s = tutorial + ": " + TheBotClient.tutorials[tutorial].makeHelpString();
                {
                    continue;
                }
                found++;
                WriteLine(s);
            }
            if (found == 0) WriteLine("I don't know about the verb " + args.objectPhrase + ".");
            Client.describeNext = false;
            return Success("Help complete");
        }
    }
}
