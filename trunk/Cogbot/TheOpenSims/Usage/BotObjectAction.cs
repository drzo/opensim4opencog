using System;
using OpenMetaverse;
using PathSystem3D.Navigation;

namespace cogbot.TheOpenSims
{
    public class BotObjectAction : BotAction
    {

        public override FirstOrderTerm GetTerm()
        {
            throw new NotImplementedException();
        }

        public override Vector3 GetUsePostion()
        {
            return TargetUse.GetUsePosition();
        }

        public override void Abort()
        {
            // throw new NotImplementedException();
        }

        public SimObjectUsage TargetUse;
        public BotObjectAction(SimAvatar who, SimObjectUsage whattarget)
            : base(whattarget.ToString())
        {
            TheBot = who;
            //TypeUsage = whattarget.Usage;
            TargetUse = whattarget;// new SimObjectUsage(what, target);
        }

        public override BotNeeds ProposedChange()
        {
            return TargetUse.GetProposedChange();
        }
        public override string ToString()
        {
            return "BotObjectAction:( " + TheBot.GetName() + " " + TargetUse.ToString() + ")";
        }

        public override void InvokeReal()
        {
            TargetUse.InvokeReal((SimActor)TheBot);
        }



        public override SimPosition Target
        {
            get
            {
                return TargetUse.Target;
            }
            set
            {
                if (value is SimObject)
                {
                    TargetUse.Target = (SimObject) value;
                }
            }
        }
    }
}