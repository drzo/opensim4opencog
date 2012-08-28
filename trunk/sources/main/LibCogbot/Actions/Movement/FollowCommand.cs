using System;
using System.Collections.Generic;
using System.Text;
using OpenMetaverse; //using libsecondlife;
using System.Threading;
using System.Windows.Forms;
using Cogbot.World;
using PathSystem3D.Navigation;
using MushDLR223.ScriptEngines;

namespace Cogbot.Actions.Movement
{
    internal class Follow : Command, BotPersonalCommand
    {
        public Follow(BotClient Client)
            : base(Client)
        {
        }

        public override void MakeInfo()
        {
            Description = "Follow an avatar. This command is modeless.";
            Details = "<p>follow* <avatar name></p>" +
                      "<p>stop-following <avatar name>\"</p>";
            Category = CommandCategory.Movement;
            Parameters = CreateParams("target", typeof (AgentSpec), "Avatar to follow");
            ResultMap = CreateParams(
                "message", typeof (string), "if we could not follow, the reason why",
                "success", typeof (bool), "true if we are following");

            Name = "Follow*";
        }


        public override CmdResult ExecuteRequest(CmdRequest args)
        {
            string verb = args.CmdName;
            // base.acceptInput(verb, args);
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
            SimPosition position;
            if (!args.TryGetValue("target", out position))
            {
                return Failure("$bot don't know who " + args.GetString("target") + " is.");
            }
            {
                if (position != null)
                {
                    String str = "" + Client + " start to follow " + position + ".";
                    WriteLine(str);
                    // The thread that accepts the Client and awaits messages
                    TheSimAvatar.CurrentAction = new FollowerAction(TheSimAvatar, position);
                    return Success("$bot started following " + position);
                }
            }
            {
                return Success("$bot ApproachPosition: " + TheSimAvatar.CurrentAction);
            }
        }
    }
}