using System;
using System.Collections.Generic;
using System.Text;
using OpenMetaverse; //using libsecondlife;
using System.Threading;
using System.Windows.Forms;
using cogbot.TheOpenSims;
using PathSystem3D.Navigation;

namespace cogbot.Actions
{
    class Follow : Action
    {


        public Follow(BotClient Client)
            : base(Client)
        {
            helpString = "Start or stop following a user.";
            usageString = "To start following an avatar, type \"follow <avatar name>\" \r\n" +
                          "To stop following an avatar, type \"stop-following <avatar name>\"";
            Parameters = new Type[] { typeof(Primitive), typeof(UUID) };
            Name = "Follower";
        }


        public override string acceptInput(string verb, Parser pargs, OutputDelegate WriteLine)
        {
            TheBotClient.describeNext = true;
            // base.acceptInput(verb, args);
            string[] args = pargs.tokens;
            UUID primID;
            if (verb == "follow" && args.Length > 0)
            {

                string name = pargs.objectPhrase;
                if (String.IsNullOrEmpty(name.Trim())) name = "avatar";
                Primitive avatar;
                if (WorldSystem.tryGetPrim(name, out avatar))
                {
                    SimObject followAvatar = WorldSystem.GetSimObject(avatar);
                    String str = "" + Client + " start to follow " + followAvatar + ".";
                    WriteLine(str);
                    SimActor me = WorldSystem.TheSimAvatar;

                    // The thread that accepts the Client and awaits messages
                    me.CurrentAction = new FollowerAction(me, followAvatar);
                    return "$bot started following " + followAvatar;
                }
                else
                {
                    return ("$bot don't know who " + name + " is.");
                }
            }
            else if (verb == "stop-following")
            {

                SimPosition ap = WorldSystem.TheSimAvatar.ApproachPosition;
                if (WorldSystem.TheSimAvatar.CurrentAction is FollowerAction)
                {
                    WorldSystem.TheSimAvatar.CurrentAction = null;
                }
                WorldSystem.TheSimAvatar.SetMoveTarget(null, 10);
                WorldSystem.TheSimAvatar.StopMoving();
                return ("$bot stops following " + ap + ".");
            }
            else
            {
                return "$bot ApproachPosition: " + WorldSystem.TheSimAvatar.ApproachPosition;
            }

        }

    }
}
