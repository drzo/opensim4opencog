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
    class Follow : Command, BotPersonalCommand
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


        public override CmdResult acceptInput(string verb, Parser pargs, OutputDelegate WriteLine)
        {
            TheBotClient.describeNext = true;
            // base.acceptInput(verb, args);
            string[] args = pargs.tokens;
            UUID primID;
            SimActor TheSimAvatar = this.TheSimAvatar;
            if (verb == "stop-following")
            {

                SimPosition ap = TheSimAvatar.ApproachPosition;
                if (TheSimAvatar.CurrentAction is MoveToLocation)
                {
                    TheSimAvatar.CurrentAction = null;
                }
                TheSimAvatar.SetMoveTarget(null, 10);
                TheSimAvatar.StopMoving();
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
                    // The thread that accepts the Client and awaits messages
                    TheSimAvatar.CurrentAction = new FollowerAction(TheSimAvatar, position);
                    return Success("$bot started following " + position);
                }
                else
                {
                    return Failure("$bot don't know who " + name + " is.");
                }
            }
            {
                return Success("$bot ApproachPosition: " + TheSimAvatar.ApproachPosition);
            }

        }

    }
}
