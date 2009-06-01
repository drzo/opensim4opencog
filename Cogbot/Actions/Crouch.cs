using System;
using System.Collections.Generic;
using System.Text;
using OpenMetaverse; //using libsecondlife;

namespace cogbot.Actions
{
    class Crouch : Action
    {
       // bool isCrouching = false;

        public Crouch(BotClient Client)
            : base(Client)
        {
            helpString = "crouch [on|off] 'no argumennt=for 500ms' ";
            usageString = "crouch [on|off]";
        }

        public override string acceptInput(string verb, Parser args)
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
                return (Client.Self.Name + " crouched.");
            }
            else
                if (tokens[0].Equals("on"))
                {
                    Client.Self.Crouch(true);
                 //   isCrouching = true;
                    return (Client.Self.Name + " started crouching.");
                }
                else
                {
                    Client.Self.Crouch(true);
                 //   isCrouching = false;
                    Client.Self.Crouch(false);
                    return (Client.Self.Name + " done crouching.");
                }
        }

         
        
    }
}
