using System;
using System.Collections.Generic;
using System.Text;
using OpenMetaverse; //using libsecondlife;
using System.Threading;
using System.Windows.Forms;
using cogbot.TheOpenSims;
using PathSystem3D.Navigation;

using MushDLR223.ScriptEngines;

namespace cogbot.Actions.Movement
{
    class Follow : Command, BotPersonalCommand
    {


        public Follow(BotClient Client)
            : base(Client)
        {
            Description = "Follow an avatar. This command is modeless.";
            Usage = "<p>follow <avatar name></p>" +
                    "<p>stop-following <avatar name>\"</p>";
            Category = CommandCategory.Movement;
            Parameters = NamedParam.CreateParams("avatar", typeof(SimAvatar), "Avatar to follow");
            ResultMap = NamedParam.CreateParams(
                 "message", typeof(string), "if we could not follow, the reason why",
                 "success", typeof(bool), "true if we are following");

            Name = "Follow*";
        }


        public override CmdResult acceptInput(string verb, Parser pargs, OutputDelegate WriteLine)
        {
            TheBotClient.describeNext = true;
            // base.acceptInput(verb, args);
            string[] args = pargs.tokens;
            UUID primID = UUID.Zero;
            SimActor TheSimAvatar = this.TheSimAvatar;
            if (verb == "stop-following")
            {

               // SimPosition ap = TheSimAvatar.ApproachPosition;
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
                return Success("$bot ApproachPosition: " + TheSimAvatar.CurrentAction);
            }

        }

    }
}
