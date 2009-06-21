using System;
using cogbot.TheOpenSims;
using OpenMetaverse;

namespace cogbot.Actions
{
    public class BotActionCommand : Command
    {
        public BotActionCommand(BotClient testClient)
        {
            Name = "botact";
            Description = "Invoke a command a bot interuptable action (interupts previous foreground action).  Usage: botact anim KISS";
            Category = CommandCategory.TestClient;
        }

        public override string Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {
            if (args.Length < 1) return Description;
            string botcmd = String.Join(" ", args, 0, args.Length).Trim();
            TheSimAvatar.CurrentAction = new CommandAction(TheSimAvatar, botcmd);
            return string.Format("{0} CurrentAction = {1}", TheSimAvatar, botcmd);
        }
    }
}