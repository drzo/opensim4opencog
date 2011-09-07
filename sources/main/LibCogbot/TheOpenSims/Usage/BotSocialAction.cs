using System;
using System.Threading;
using OpenMetaverse;
using PathSystem3D.Navigation;

namespace cogbot.TheOpenSims
{
    public class BotSocialAction : BotAction
    {
        public override SimPosition Target { get { return Victem; } set
        {
            if (value is SimAvatar) Victem = (SimAvatar) value;
        }
        }

        public override FirstOrderTerm GetTerm()
        {
            throw new NotImplementedException();
        }

        public override Vector3 GetUsePostion()
        {
            return Victem.SimPosition;
        }

        public override void Abort()
        {
            throw new NotImplementedException();
        }

        static Random rand = new Random(DateTime.Now.Millisecond);
        public SimAvatar Victem;
        public SimTypeUsage TypeUsage;
        public BotMentalAspect CurrentTopic;
        public int TimeRemaining;
        public BotSocialAction(SimAvatar who, SimTypeUsage what, SimAvatar target)
            : base(what.UsageName + " " + target)
        {
            TheBot = (SimControllableAvatar) who;
            TypeUsage = what;
            Victem = target;
            CurrentTopic = null;
            TimeRemaining = rand.Next(1, 3); // one to tree cycles
        }

        public override BotNeeds ProposedChange()
        {
            return TypeUsage.ChangePromise;
        }

        public override float RateIt(BotNeeds current)
        {
            return ProposedChange().TotalSideEffect(current);
        }

        public override void InvokeReal()
        {
            SimActor TheBot = (SimActor)this.TheBot;
            TimeRemaining = rand.Next(1, 3); // one to tree cycles
            while (TimeRemaining-- > 0)
            {
                String use = TypeUsage.UsageName;
                TheBot.Approach(Victem, 5);
                TheBot.Debug(ToString());
                CurrentTopic = TheBot.LastAction;
                TheBot.TalkTo(Victem, CurrentTopic);
                Thread.Sleep(8000);
                //User.ApplyUpdate(use, simObject);
            }
            BotNeeds CurrentNeeds = (BotNeeds)TheBot["CurrentNeeds"];
            BotNeeds needsBefore = CurrentNeeds.Copy();
            BotNeeds simNeeds = TypeUsage.ChangeActual;
            //TODO rate interaction
            CurrentNeeds.AddFrom(simNeeds);
            CurrentNeeds.SetRange(0.0F, 100.0F);
            BotNeeds difNeeds = CurrentNeeds.Minus(needsBefore);
            TheBot.Debug(ToString() + " => " + difNeeds.ShowNonZeroNeeds());
        }
        public override string ToString()
        {
            return "BotSocialAction:( " + TypeUsage.UsageName + " " + Victem + ")";
        }
    }
}