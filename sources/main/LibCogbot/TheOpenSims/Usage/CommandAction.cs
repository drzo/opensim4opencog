using System;
using OpenMetaverse;
using PathSystem3D.Navigation;

namespace cogbot.TheOpenSims
{
    public class CommandAction : BotAction
    {
        public override SimPosition Target { get; set; }
        readonly static BotNeeds ProposedChanges = new BotNeeds(0.0f);
        readonly String command;
        public CommandAction(SimAvatar impl, String command)
            : base("ExecuteCommand " + impl + " -> " + command)
        {
            TheBot = impl;
            this.command = command;
        }

        public override BotNeeds ProposedChange()
        {
            return ProposedChanges;
        }

        public override void InvokeReal()
        {
            GetBotClient().ExecuteCommand(command);
        }

        public override Vector3 GetUsePostion()
        {
            return TheBot.SimPosition;
        }

        public override FirstOrderTerm GetTerm()
        {
            throw new NotImplementedException();
        }

        public override void Abort()
        {

        }
    }
}