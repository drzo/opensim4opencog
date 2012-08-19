using System;
using System.Collections.Generic;
using System.Threading;
using Cogbot.World;
using OpenMetaverse;
using OpenMetaverse.Packets;

using MushDLR223.ScriptEngines;

namespace Cogbot.Actions.Agent
{
    public class ProfileCommand : Command, GridMasterCommand, GUICommand
    {

        public ProfileCommand(BotClient testClient)
        {

            Name = "Profile";
            Description = "Shows the Avatars profile in a UI component. Usage: profile <avatar>";
            Category = CommandCategory.Friends;
            AddVersion(CreateParams("agent", typeof(SimAvatar), "agent you are going to " + Name),
                       "shows the profile specified by agent's uuid");

        }

        public override CmdResult ExecuteRequest(CmdRequest args)
        {
            if (args.Length < 1)
                return ShowUsage();
            int argsUsed;
            foreach (var p in WorldSystem.GetPrimitives(args, out argsUsed))
            {
                if (p is SimAvatar)
                {
                    SimAvatar a = (SimAvatar)p;
                    AddSuccess("Showing " + a);
                    TheBotClient.InvokeGUI(() =>
                                        new Radegast.frmProfile(TheBotClient.TheRadegastInstance, a.GetName(), a.ID).Show());
                }
            }            
            return SuccessOrFailure();
        }
    }
}
