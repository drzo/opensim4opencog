using System;
using System.Collections.Generic;
using System.Text;
using OpenMetaverse; //using libsecondlife;

namespace cogbot.Actions
{
    class Crouch : Command
    {
       // bool isCrouching = false;

        public Crouch(BotClient Client)
            : base(Client)
        {
            Description = "crouch [on|off] 'no argumennt=for 500ms' ";
            Usage = "crouch [on|off]";
            Name = "Crouch";
            Parameters = new[] {new NamedParam(typeof (GridClient),typeof(string), null, "on", "off", typeof (int))};
        }

        public override string acceptInput(string verb, Parser args, OutputDelegate WriteLine)
        {
            Client.describeNext = true;
            string[] tokens = args.tokens;
            //base.acceptInput(verb, args);
            if (tokens.Length == 0)
            {
                Client.Self.Crouch(true);
                System.Threading.Thread.Sleep(500);
               // isCrouching = false;
                Client.Self.Crouch(false);
                return ("$bot crouched.");
            }
            else
                if (tokens[0].Equals("on"))
                {
                    Client.Self.Crouch(true);
                 //   isCrouching = true;
                    return ("$bot started crouching.");
                }
                else
                {
                    Client.Self.Crouch(true);
                 //   isCrouching = false;
                    Client.Self.Crouch(false);
                    return ("$bot done crouching.");
                }
        }

         
        
    }
}
