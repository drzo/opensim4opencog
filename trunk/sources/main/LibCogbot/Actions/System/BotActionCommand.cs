using System;
using Cogbot.World;
using MushDLR223.ScriptEngines;
using OpenMetaverse;

namespace Cogbot.Actions.System
{
    public class BotActionCommand : Command, BotPersonalCommand
    {
        public BotActionCommand(BotClient testClient)
        {
            Name = "botact";
        }

        public override void MakeInfo()
        {
            Description =
                "Invoke a command a bot interuptable action (interupts previous foreground action).  Usage: botact anim KISS";
            Category = CommandCategory.BotClient;
            Parameters = CreateParams("act", typeof(string[]), "act for " + Name);
        }

        public override CmdResult ExecuteRequest(CmdRequest args)
        {
            if (args.Length < 1) return ShowUsage();
            string botcmd = args.GetString("act");
            TheSimAvatar.CurrentAction = new CommandAction(TheSimAvatar, botcmd);
            return Success(string.Format("{0} CurrentAction = {1}", TheSimAvatar, botcmd));
        }
    }
}