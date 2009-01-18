using System;
using System.Collections.Generic;
using System.Text;
using OpenMetaverse; //using libsecondlife;

namespace cogbot.Actions
{
    class Sit : Action
    {
        public bool sittingOnGround = false;

        public Sit(BotClient Client)
            : base(Client)
        {
            Client.Objects.OnAvatarSitChanged += new ObjectManager.AvatarSitChanged(Objects_OnAvatarSitChanged);

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
                    WriteLine("You sat down.");
                }
                else
                    WriteLine("You stood up.");
            }
            else
            {
                if (sittingOn != 0)
                    WriteLine(avatar.Name + " sat down.");
                else
                    WriteLine(avatar.Name + " stood up.");
            }
        }

        public override void acceptInput(string verb, Parser args)
        {
            //base.acceptInput(verb, args);

            if (Client.Self.SittingOn != 0 || sittingOnGround)
                WriteLine("You are already sitting.");
            else
            {
                if (args.prepPhrases["on"].Length > 0)
                {
                    string on = args.prepPhrases["on"];
                    Primitive prim;
                    if (Client.WorldSystem.tryGetPrim(on, out prim))
                    {
                        WriteLine("Trying to sit on " + prim.Properties.Name + ".");
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
                    WriteLine("You sit on the ground.");
                    Client.Self.SitOnGround();
                    sittingOnGround = true;
                }
            }

            Client.describeNext = true;
        }
    }
}
