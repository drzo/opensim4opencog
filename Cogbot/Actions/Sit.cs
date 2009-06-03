using System;
using System.Collections.Generic;
using System.Text;
using OpenMetaverse; //using libsecondlife;

namespace cogbot.Actions
{
    class Sit : Action
    {
        public bool sittingOnGround = false;

        bool registeredCallback = false;
        public Sit(BotClient Client)
            : base(Client)
        {
            helpString = "Sit on the ground or on an object.";
            usageString = "To sit on ground, type \"sit\" \r\n" +
                          "To sit on an object, type \"sit on <object name>\"" ;
        }

        void Objects_OnAvatarSitChanged(Simulator simulator, Avatar avatar, uint sittingOn, uint oldSeat)
        {
            if (avatar.Name == Client.Self.Name)
            {
                if (sittingOn != 0)
                {
                    WriteLine("$bot sat down.");
                }
                else
                    WriteLine("$bot stood up.");
            }
            else
            {
                if (sittingOn != 0)
                    WriteLine(avatar.Name + " sat down.");
                else
                    WriteLine(avatar.Name + " stood up.");
            }
        }

        public override string acceptInput(string verb, Parser args, OutputDelegate WriteLine)
        {
            acceptInput0(verb, args, WriteLine);
            return verb + " complete";
        }

        void acceptInput0(string verb, Parser args, OutputDelegate WriteLine)
        {
            //base.acceptInput(verb, args);

            if (!registeredCallback)
            {
                registeredCallback = true;
                Client.Objects.OnAvatarSitChanged += Objects_OnAvatarSitChanged;
            }

            if (Client.Self.SittingOn != 0 || sittingOnGround)
                WriteLine("$bot is already sitting.");
            else
            {
                if (args.prepPhrases["on"].Length > 0)
                {
                    string on = args.prepPhrases["on"];
                    Primitive prim;
                    if (WorldSystem.tryGetPrim(on, out prim))
                    {
                        WriteLine("Trying to sit on {0}.", prim.Properties.Name);
                        Client.Self.RequestSit(prim.ID, Vector3.Zero);
                        Client.Self.Sit();
                        sittingOnGround = false;
                    }
                    else
                    {
                        WriteLine("I don't know what " + on + " is.");
                    }
                }
                else
                {
                    WriteLine("$bot sit on the ground.");
                    Client.Self.SitOnGround();
                    sittingOnGround = true;
                }
            }

            TheBotClient.describeNext = true;
        }
    }
}
