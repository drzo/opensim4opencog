using OpenMetaverse;
using PathSystem3D.Navigation;

namespace cogbot.TheOpenSims
{
    abstract public class BotAction : SimUsage
    {
        public BotAction(string s)
            : base(s)
        {
        }

        public abstract SimPosition Target { get; set; }

        public override float RateIt(BotNeeds current)
        {
            return ProposedChange().TotalSideEffect(current);
        }

        // the actor
        public SimAvatar TheBot;


        public GridClient GetGridClient()
        {
            return ((SimControllableAvatar)TheBot).GetGridClient();
        }
        public BotClient GetBotClient()
        {
            return ((SimControllableAvatar)TheBot).GetBotClient();
        }

        // Returns how much the needs should be changed;
        public abstract BotNeeds ProposedChange();
        // the needs are really changed;
        public abstract void InvokeReal();
        // use assumptions
        //public virtual float RateIt()
        //{
        //    BotNeeds bn = TheBot.CurrentNeeds.Copy();
        //    BotNeeds pc = ProposedChange();
        //    bn.AddFrom(pc);
        //    bn.SetRange(0f, 100f);
        //    return bn.Total() - (Vector3.Distance(TheBot.GetSimPosition(),GetLocation()));
        //}

        public abstract Vector3 GetUsePostion();


        public abstract void Abort();
    }
}