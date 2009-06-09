using System;
using System.Collections.Generic;
using System.Text;
using System.Threading;
using cogbot.TheOpenSims;
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
                    //probly going to standing pos
                    sittingOnGround = false;
                   // WriteLine("$bot sat down.");
                }
                //else
                   // WriteLine("$bot stood up.");
            }
            else
            {
                //if (sittingOn != 0)
                //    WriteLine(avatar.Name + " sat down.");
                //else
                //    WriteLine(avatar.Name + " stood up.");
            }
        }

        public override string acceptInput(string verb, Parser args, OutputDelegate WriteLine)
        {
            //base.acceptInput(verb, args);

            if (!registeredCallback)
            {
                registeredCallback = true;
                //Client.Objects.OnAvatarSitChanged += Objects_OnAvatarSitChanged;
            }

            TheBotClient.describeNext = true;

            //if (Client.Self.SittingOn != 0 || sittingOnGround)
               // return ("$bot is already sitting.");
           //else
            {
                if (args.prepPhrases["on"].Length > 0)
                {
                    string on = args.prepPhrases["on"];
                    Primitive prim;
                    if (WorldSystem.tryGetPrim(on, out prim))
                    {
                        SimObject obj = WorldSystem.GetSimObject(prim);
                        WriteLine("Trying to sit on {0}.", obj);
                        if (!WorldSystem.TheSimAvatar.SitOn(obj))
                        {
                            return ("$bot did not yet sit on " + obj);
                        }
                        else
                        {
                            sittingOnGround = false;
                            return ("$bot did sit on " + obj);
                        }
                    }
                    else
                    {
                        return ("I don't know what " + on + " is.");
                    }
                }
                else
                {
                    sittingOnGround = WorldSystem.TheSimAvatar.SitOnGround();
                    if (!sittingOnGround) return ("$bot did not yet sit on the ground.");
                    return ("$bot sat on the ground.");
                }
            }

 
        }
    }
}
