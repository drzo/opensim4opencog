using System;
using System.Collections.Generic;
using System.Text;
using System.Threading;
using MushDLR223.Utilities;
using OpenMetaverse; //using libsecondlife;

using MushDLR223.ScriptEngines;

namespace cogbot.Actions
{
    class Crouch : Command, BotPersonalCommand
    {
       // bool isCrouching = false;

        public Crouch(BotClient Client)
            : base(Client)
        {
            Description = "crouch [on|off] 'no argumennt=for 500ms' ";
            Usage = "crouch [on|off] time-ms";
            Name = "Crouch";
            Category = CommandCategory.Movement;
            Parameters = new[]
                             {
                                 new NamedParam("on-off", typeof (string), "on", "on", "off"),
                                 new NamedParam("time-ms", typeof (TimeSpan), 500)
                             };
        }

        public override CmdResult acceptInput(string verb, Parser args, OutputDelegate WriteLine)
        {
            Client.describeNext = true;
            string[] tokens = args.tokens;
            //base.acceptInput(verb, args);
            if (tokens.Length == 0)
            {
                Client.Self.Crouch(true);
                Thread.Sleep(500);
               // isCrouching = false;
                Client.Self.Crouch(false);
                return Success("$bot crouched.");
            }
            else
                if (tokens[0].Equals("on"))
                {
                    Client.Self.Crouch(true);
                 //   isCrouching = true;
                    return Success("$bot started crouching.");
                }
                else
                {
                    Client.Self.Crouch(true);
                 //   isCrouching = false;
                    Client.Self.Crouch(false);
                    return Success("$bot done crouching.");
                }
        }

         
        
    }
}
