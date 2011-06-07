using System;
using OpenMetaverse;
using PathSystem3D.Navigation;

namespace cogbot.TheOpenSims
{
    public class MoveToLocation : BotAction
    {
        public override SimPosition Target { get; set; }
        readonly static BotNeeds ProposedChanges = new BotNeeds(0.0f);
        public MoveToLocation(SimAvatar impl, SimPosition position)
            : base("MoveTo " + impl + " -> " + impl.DistanceVectorString(position))
        {
            TheBot = impl;
            Target = position;
        }

        public override BotNeeds ProposedChange()
        {
            return ProposedChanges;
        }

        public override void InvokeReal()
        {
            TheBot.GotoTarget(Target);
        }

        public override Vector3 GetUsePostion()
        {
            return Target.SimPosition;
        }

        public override FirstOrderTerm GetTerm()
        {
            throw new NotImplementedException();
        }

        public override void Abort()
        {
            TheBot.StopMoving();
        }
    }
}