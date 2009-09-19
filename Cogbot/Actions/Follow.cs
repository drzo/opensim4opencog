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
    class Follow : Command
    {


        public Follow(BotClient Client)
            : base(Client)
        {
            Description = "Start or stop following a user.";
            Usage = "To start following an avatar, type \"follow <avatar name>\" \r\n" +
                          "To stop following an avatar, type \"stop-following <avatar name>\"";
            Parameters = new [] {  new NamedParam(typeof(SimObject), typeof(UUID)) };
            Name = "Follow*";
        }


        public override string acceptInput(string verb, Parser pargs, OutputDelegate WriteLine)
        {
            TheBotClient.describeNext = true;
            // base.acceptInput(verb, args);
            string[] args = pargs.tokens;
            UUID primID;
            if (verb == "stop-following")
            {

                SimPosition ap = WorldSystem.TheSimAvatar.ApproachPosition;
                if (WorldSystem.TheSimAvatar.CurrentAction is FollowerAction)
                {
                    WorldSystem.TheSimAvatar.CurrentAction = null;
                }
                WorldSystem.TheSimAvatar.SetMoveTarget(null, 10);
                WorldSystem.TheSimAvatar.StopMoving();
            }
            else if (args.Length > 0)
            {

                string name = pargs.objectPhrase;
                if (String.IsNullOrEmpty(name.Trim())) name = "avatar 1";
                int argsUsed;
                SimPosition position = WorldSystem.GetVector(pargs.tokens, out argsUsed);

                Primitive avatar;
                if (position != null)
                {
                    String str = "" + Client + " start to follow " + position + ".";
                    WriteLine(str);
                    SimActor me = WorldSystem.TheSimAvatar;
                    // The thread that accepts the Client and awaits messages
                    me.CurrentAction = new FollowerAction(me, position);
                    return "$bot started following " + position;
                }
                else
                {
                    return ("$bot don't know who " + name + " is.");
                }
            }
            {
                return "$bot ApproachPosition: " + WorldSystem.TheSimAvatar.ApproachPosition;
            }

        }

    }
}
