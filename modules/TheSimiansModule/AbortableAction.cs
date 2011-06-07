using System;
using cogbot.TheOpenSims;
using OpenMetaverse;
using PathSystem3D.Navigation;

namespace TheSimiansModule
{
    public class AbortableAction : BotAction
    {
        readonly BotAction Act;
        readonly SimAborter Aborter;
        public AbortableAction(BotAction action, SimAborter thinker)
            : base(action.ToString())
        {
            Act = action;
            Aborter = thinker;
        }


        public override SimPosition Target
        {
            get
            {
                return Act.Target;
            }
            set
            {
                Act.Target = value;
            }
        }

        public override BotNeeds ProposedChange()
        {
            return Act.ProposedChange();
        }

        public override void InvokeReal()
        {
            Act.InvokeReal();
        }

        public override Vector3 GetUsePostion()
        {
            return Act.GetUsePostion();
        }

        public override void Abort()
        {
            Aborter.Abort();
            Act.Abort();
        }

        public override FirstOrderTerm GetTerm()
        {
            return Act.GetTerm();
        }
    }

    public interface SimAborter
    {
        void Abort();
    }
}