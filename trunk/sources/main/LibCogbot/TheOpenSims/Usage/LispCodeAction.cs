using System;
using OpenMetaverse;
using PathSystem3D.Navigation;

namespace cogbot.TheOpenSims
{
    public class LispCodeAction : BotAction
    {
        public override SimPosition Target { get; set; }
        readonly static BotNeeds ProposedChanges = new BotNeeds(0.0f);
        readonly object command;
        public LispCodeAction(SimAvatar impl, object command)
            : base("ExecuteCommand " + impl + " -> " + command)
        {
            TheBot = (SimControllableAvatar)impl;
            this.command = command;
        }

        public override BotNeeds ProposedChange()
        {
            return ProposedChanges;
        }

        public override void InvokeReal()
        {
            GetBotClient().evalLispCode(command);
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