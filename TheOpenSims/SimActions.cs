using System;
using System.Collections.Generic;
using System.Text;
using OpenMetaverse;
using System.Threading;

namespace cogbot.TheOpenSims
{
    abstract public class BotAction : BotMentalAspect
    {
        public BotAction(string s)
            : base(s)
        {
        }

        // the actor
        public SimAvatar TheBot;
        // Returns how much the needs should be changed;
        public abstract BotNeeds ProposedChange();
        // the needs are really changed;
        public abstract void InvokeReal();
        // use assumptions
        public float RateIt()
        {
            BotNeeds bn = TheBot.CurrentNeeds.Copy();
            BotNeeds pc = ProposedChange();
            bn.AddFrom(pc);
            bn.SetRange(0f, 100f);
            return bn.Total();
        }
    }

    public class SimObjectUsage
    {
        public SimTypeUsage TypeUsage;
        public SimObject Target;

        public override string ToString()
        {
            String verb = TypeUsage.TextName;
            if (String.IsNullOrEmpty(verb))
                verb = TypeUsage.UsageName;

            return verb + " " + Target;
        }

        public SimObjectUsage(SimTypeUsage use, SimObject target)
        {
            TypeUsage = use;
            Target = target;
        }

        public void InvokeReal(BotObjectAction TheBotAct)
        {
            SimAvatar TheBot = TheBotAct.TheBot;
            String use = TypeUsage.UsageName;
            Target.MakeEnterable();
            // Approach Target
            float howClose = TheBot.Approach(Target, TypeUsage.maximumDistance);

            if (howClose > TypeUsage.maximumDistance)
            {
                // maybe get closer or fail
            }

            // Create the Side-Effect closure
            ThreadStart closure = new ThreadStart(delegate()
            {
                TheBot.Debug(ToString());
                //User.ApplyUpdate(use, simObject);
                BotNeeds CurrentNeeds = TheBot.CurrentNeeds;
                BotNeeds needsBefore = CurrentNeeds.Copy();
                BotNeeds update = Target.GetActualUpdate(TypeUsage.UsageName);
                //TODO rate interaction and update TheBot.Assumptions
                CurrentNeeds.AddFrom(update);
                CurrentNeeds.SetRange(0.0F, 100.0F);
                BotNeeds difNeeds = CurrentNeeds.Minus(needsBefore);
                TheBot.Debug(TheBotAct.ToString() + "\n\t=> " + difNeeds.ShowNonZeroNeeds());
                TheBot.ExecuteLisp(this, TypeUsage.LispScript);
                Thread.Sleep(TypeUsage.totalTimeMS);
            });

            bool animFound = TypeUsage.UseSit;
            // IF UseAnim was specified
            if (!String.IsNullOrEmpty(TypeUsage.UseAnim))
            {
                UUID animID = TheBot.FindAnimUUID(TypeUsage.UseAnim);
                if (animID != UUID.Zero)
                {
                    closure = TheBot.WithAnim(animID, closure);
                    animFound = true;
                }
            }
            // else
            if (!animFound)
            {
                //ELSE look for Verb coverage for an anim
                UUID animID = TheBot.FindAnimUUID(use);
                if (animID != UUID.Zero)
                    closure = TheBot.WithAnim(animID, closure);
            }

            // Surround with tough/grab if needed
            if (use == "touch" || use == "grab" || TypeUsage.UseGrab)
                closure = TheBot.WithGrabAt(Target, closure);

            // Surround with Sit if needed
            if (use == "sit" || TypeUsage.UseSit)
                closure = TheBot.WithSitOn(Target, closure);

            closure.Invoke();
            Target.RestoreEnterable();
        }


        public BotNeeds GetProposedChange()
        {
            return Target.GetProposedUpdate(TypeUsage.UsageName);
        }
    }

    // most object have use that advertises ChangePromise but actually calls ChangeActual
    public class SimTypeUsage
    {
        public SimTypeUsage(String name)
        {
            UsageName = name;
        }
        // the maximum distance the user can be away scaled on object size
        public String UsageName;
        public string TextName = ""; // the scripting usename name
        public int maximumDistance = 1;
        public int totalTimeMS = 14000;  // the time this usage takes
        public BotNeeds ChangePromise = new BotNeeds(0.0F); // what most users think will happen by default
        public BotNeeds ChangeActual = new BotNeeds(0.0F); //what really happens ofter 1 minute use
        // if true the avatar will attempt to sit on the object for the duration
        public bool UseSit = false;
        public bool UseSitSpecified = false;
        // if true the client will attempt to invoke the "touch/grab" for the duration
        public bool UseGrab = false;
        public bool UseGrabSpecified = false;
        // if "KICK" or another Anim the avatar will play this anim
        public String UseAnim = null;
        // if set the client will attempt to run
        public String LispScript = null; // the lisp code that does the animation effects

        public string ToDebugString()
        {
            String str = UsageName;
            str += " TextName: '" + TextName;
            str += "' totalTimeMS: " + totalTimeMS;
            str += " maximumDistance: " + maximumDistance;
            str += " ChangePromise:" + ChangePromise.ShowNonZeroNeeds();
            str += " ChangeActual:" + ChangeActual.ShowNonZeroNeeds();
            if (UseSitSpecified) str += " UseSit: " + UseSit;
            if (UseGrabSpecified) str += " UseGrab: " + UseGrab;
            if (!String.IsNullOrEmpty(UseAnim)) str += " UseAnim: " + UseAnim;
            if (!String.IsNullOrEmpty(LispScript)) str += " LispScript: " + LispScript;
            return str;
        }
    }


    public class AnimThread
    {
        GridClient Client;
        UUID anim;
        bool repeat = true;
        Thread animLoop;
        public AnimThread(GridClient c, UUID amin0)
        {
            Client = c;
            anim = amin0;
        }
        public void Start()
        {
            animLoop = new Thread(new ThreadStart(LoopAnim));
            animLoop.Start();
        }
        void LoopAnim()
        {
            try
            {
                Client.Self.AnimationStart(anim, true);
                while (repeat)
                {
                    // some anims will only last a short time so we have to 
                    // remind the server we still want to be ussing it 
                    // like Laugh .. lasts for about .9 seconds
                    //12000 is a estimate avage
                    Client.Self.AnimationStop(anim, true);
                    Client.Self.AnimationStart(anim, true);
                    Thread.Sleep(3200);
                }
            }
            catch (Exception) { } // for the Abort 
        }
        public void Stop()
        {
            repeat = false;
            if (animLoop != null)
            {
                try
                {
                    if (animLoop.IsAlive) animLoop.Abort();
                }
                catch (Exception) { }
                animLoop = null;
            }
            Client.Self.AnimationStop(anim, true);
        }
    }

    public class BotObjectAction : BotAction
    {
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
            TargetUse.InvokeReal(this);
        }
    }

    public class BotSocialAction : BotAction
    {
        static Random rand = new Random();
        public SimAvatar Victem;
        public SimTypeUsage TypeUsage;
        public BotMentalAspect CurrentTopic;
        public int TimeRemaining;
        public BotSocialAction(SimAvatar who, SimTypeUsage what, SimAvatar target)
            : base(what.UsageName + " " + target)
        {
            TheBot = who;
            TypeUsage = what;
            Victem = target;
            CurrentTopic = null;
            TimeRemaining = rand.Next(1, 3); // one to tree cycles
        }

        public override BotNeeds ProposedChange()
        {
            return TypeUsage.ChangePromise;
        }

        public override void InvokeReal()
        {
            TimeRemaining = rand.Next(1, 3); // one to tree cycles
            while (TimeRemaining-- > 0)
            {
                String use = TypeUsage.UsageName;
                TheBot.Approach(Victem, 5);
                TheBot.Debug(ToString());
                CurrentTopic = TheBot.GetNextInterestingObject();
                TheBot.TalkTo(Victem, CurrentTopic);
                Thread.Sleep(8000);
                //User.ApplyUpdate(use, simObject);
            }
            BotNeeds CurrentNeeds = TheBot.CurrentNeeds;
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
