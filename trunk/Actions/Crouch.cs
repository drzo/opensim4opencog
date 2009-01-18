using System;
using System.Collections.Generic;
using System.Text;
using OpenMetaverse; //using libsecondlife;

namespace cogbot.Actions
{
    class Crouch : Action
    {
        bool isCrouching = false;

        public Crouch(BotClient Client)
            : base(Client)
        {
            helpString = "Crouch.";
            usageString = "To Crouch type \"crouch\"";
        }

        public override void acceptInput(string verb, Parser args)
        {
            Client.describeNext = true;
            string[] tokens = args.tokens;
            //base.acceptInput(verb, args);
            if (tokens.Length == 0)
            {
                Client.Self.Crouch(true);
                System.Threading.Thread.Sleep(500);
                WriteLine(Client.Self.Name + " crouched.");
                isCrouching = false;
                Client.Self.Crouch(false);
            }
            else
                if (tokens[0].Equals("on"))
                {
                    Client.Self.Crouch(true);
                    isCrouching = true;
                    WriteLine(Client.Self.Name + " started crouching.");
                }
                else
                {
                    Client.Self.Crouch(true);
                    isCrouching = false;
                    WriteLine(Client.Self.Name + " done crouching.");
                    Client.Self.Crouch(false);
                }
        }

         
        
    }
}
