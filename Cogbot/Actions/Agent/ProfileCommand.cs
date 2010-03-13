using System;
using System.Collections.Generic;
using System.Threading;
using cogbot.TheOpenSims;
using OpenMetaverse;
using OpenMetaverse.Packets;

namespace cogbot.Actions.Agent
{
    public class ProfileCommand : Command, GridMasterCommand
    {

        public ProfileCommand(BotClient testClient)
        {

            Name = "Profile";
            Description = "Shows the Avatars profile in a UI component. Usage: profile <avatar>";
            Category = CommandCategory.Friends;
            Parameters = new [] {  new NamedParam(typeof(Avatar), typeof(UUID)) };
        }

        public override CmdResult Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
            if (args.Length < 1)
                return ShowUsage();
            int argsUsed;
            foreach (var p in WorldSystem.GetPrimitives(args, out argsUsed))
            {
                if (p is SimAvatar)
                {
                    SimAvatar a = (SimAvatar)p;
                    Success("Showing " + a);
                    TheBotClient.Invoke(() =>
                                        new Radegast.frmProfile(TheBotClient.TheRadegastInstance, a.GetName(), a.ID).Show());
                }
            }            
            return SuccessOrFailure();
        }
    }
}
