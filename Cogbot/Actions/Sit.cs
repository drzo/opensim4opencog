using System;
using System.Collections.Generic;
using System.Text;
using System.Threading;
using cogbot.TheOpenSims;
using OpenMetaverse; //using libsecondlife;

namespace cogbot.Actions
{
    class Sit : Command
    {
        public bool sittingOnGround = false;

        bool registeredCallback = false;
        public Sit(BotClient Client)
            : base(Client)
        {
            Description = "Sit on the ground or on an object.";
            Usage = "To sit on ground, type \"sit\" \r\n" +
                          "To sit on an object, type \"sit on <object name>\"";
            Parameters = new [] {  new NamedParam(typeof(GridClient), null) };
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

        public override CmdResult acceptInput(string verb, Parser args, OutputDelegate WriteLine)
        {
            //base.acceptInput(verb, args);

            if (!registeredCallback)
            {
                registeredCallback = true;
                //Client.Objects.OnAvatarSitChanged += Objects_OnAvatarSitChanged;
            }

            TheBotClient.describeNext = true;

            string on = args.prepPhrases["on"];

            if (args.Length == 0)
            {
                sittingOnGround = WorldSystem.TheSimAvatar.SitOnGround();
                return !sittingOnGround
                           ? Failure("$bot did not yet sit on the ground.")
                           : Success("$bot sat on the ground.");
            }

            //if (Client.Self.SittingOn != 0 || sittingOnGround)
            // return ("$bot is already sitting.");

            if (on == "it")
            {
                int argsUsed;
                SimAvatar master =
                    (SimAvatar)
                    WorldSystem.GetSimObject(WorldSystem.GetPrimitive(Client.MasterName.Split(new char[] { ' ' }),
                                                                      out argsUsed));
                List<SimObject> knows = master.GetKnownObjects();
                if (knows.Count > 0)
                {
                    var obj = knows[0];
                    if (!WorldSystem.TheSimAvatar.SitOn(obj))
                    {
                        return Failure("$bot did not yet sit on " + obj);
                    }
                }
            }

            if (on.Length == 0)
            {
                on = String.Join(" ", args.tokens);
            }
            Primitive prim;
            if (WorldSystem.tryGetPrim(on, out prim))
            {
                SimObject obj = WorldSystem.GetSimObject(prim);
                WriteLine("Trying to sit on {0}.", obj);
                if (!WorldSystem.TheSimAvatar.SitOn(obj))
                {
                    return Failure("$bot did not yet sit on " + obj);
                }
                sittingOnGround = false;
                return Success("$bot did sit on " + obj);
            }

            return Failure("I don't know what " + on + " is.");
        }
    }
}