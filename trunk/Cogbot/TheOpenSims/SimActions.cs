using System;
using System.Threading;
using cogbot.Listeners;
using OpenMetaverse;
using PathSystem3D.Navigation;

namespace cogbot.TheOpenSims
{

    abstract public class SimUsage : BotMentalAspect
    {
        public abstract FirstOrderTerm GetTerm();

        public String UsageName;

        public SimUsage(string name)
           // : base(name)
        {
            UsageName = name;
        }

        public static bool operator ==(SimUsage use1, SimUsage use2)
        {
            if (Object.ReferenceEquals(use1, null) && Object.ReferenceEquals(use2, null)) return true;
            if (Object.ReferenceEquals(use1, null) || Object.ReferenceEquals(use2, null)) return false;
            return use1.UsageName == use2.UsageName;
        }

        public static bool operator !=(SimUsage use1, SimUsage use2)
        {
            if (use1 == null && use2 == null) return false;
            if (use1 == null || use2 == null) return true;
            return use1.UsageName != use2.UsageName;
        }
        public override bool Equals(object obj)
        {
            if (!(obj is SimUsage)) return false;
            return this == (SimUsage)obj;
        }

        public override int GetHashCode()
        {
            return UsageName.GetHashCode();
        }

        public override string ToString()
        {
            return String.Format("{0}::{1}", GetType().Name, UsageName);
        }

        public abstract float RateIt(BotNeeds current);
    }

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


        public BotClient GetGridClient()
        {
            return TheBot.GetGridClient();
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
            return Target.GetSimPosition();
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
            GetGridClient().ExecuteCommand(command);
        }

        public override Vector3 GetUsePostion()
        {
            return TheBot.GetSimPosition();
        }

        public override FirstOrderTerm GetTerm()
        {
            throw new NotImplementedException();
        }

        public override void Abort()
        {

        }
    }
    public class LispCodeAction : BotAction
    {
        public override SimPosition Target { get; set; }
        readonly static BotNeeds ProposedChanges = new BotNeeds(0.0f);
        readonly object command;
        public LispCodeAction(SimAvatar impl, object command)
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
            GetGridClient().evalLispCode(command);
        }

        public override Vector3 GetUsePostion()
        {
            return TheBot.GetSimPosition();
        }

        public override FirstOrderTerm GetTerm()
        {
            throw new NotImplementedException();
        }

        public override void Abort()
        {

        }
    }

    public class FollowerAction : BotAction
    {
        public override SimPosition Target { get; set; }
        readonly static BotNeeds ProposedChanges = new BotNeeds(0.0f);
        readonly private float maxDistance;
        readonly private Thread FollowThread;
        private bool KeepFollowing = true;
        public static bool UsePathfinder = false;

        public FollowerAction(SimAvatar impl, SimPosition position)
            : base("" + impl.GetName() + ": Follow " + position + " -> " + impl.DistanceVectorString(position))
        {
            TheBot = impl;
            maxDistance = 3;// position.GetSizeDistance();
            Target = position;
            FollowThread = new Thread(FollowLoop);
        }

        public override string ToString()
        {
            return "" + TheBot.GetName() + ": Follow " + Target + " -> " + TheBot.DistanceVectorString(Target);
        }

        public override BotNeeds ProposedChange()
        {
            return ProposedChanges;
        }

        public override void InvokeReal()
        {
            // start if not already started
            if (!FollowThread.IsAlive)
            {
                FollowThread.Start();
                FollowThread.Join();
            }
        }

        public void FollowLoop()
        {
            while (KeepFollowing)
            {
                Thread.Sleep(2000);
                if (TheBot.Distance(Target) > maxDistance)
                {
                    for (int i = 0; i < 3; i++)
                    {
                        if (Target.IsRegionAttached())
                       // TheBot.TurnToward(Target);
                        TheBot.SetMoveTarget(Target, maxDistance);
                        else Console.WriteLine(""+this+" Not regions attached " + Target);
                        Thread.Sleep(2000);
                        TheBot.StopMoving();
                    }
                    if (UsePathfinder && TheBot.Distance(Target) > maxDistance + 2)
                        TheBot.GotoTarget(Target);
                }   
                else
                {
                    TheBot.TurnToward(Target);
                    Thread.Sleep(1000); // total 3 seconds
                }
            }
        }


        public override void Abort()
        {
            KeepFollowing = false;
            try
            {
                FollowThread.Abort();
            }
            catch (Exception)
            {
            }
        }

        public override Vector3 GetUsePostion()
        {
            return Target.GetSimPosition();
        }

        public override FirstOrderTerm GetTerm()
        {
            throw new NotImplementedException();
        }

    }

    public class SimObjectUsage : SimUsage
    {
        public SimTypeUsage TypeUsage;
        public SimObject Target { get; set;}

        
        override public FirstOrderTerm GetTerm()
        {
            throw new NotImplementedException();
        }

        public override string ToString()
        {
            String verb = TypeUsage.TextName;
            if (String.IsNullOrEmpty(verb))
                verb = TypeUsage.UsageName;

            return verb + " " + Target;
        }

        public SimObjectUsage(SimTypeUsage use, SimObject target) : base (""+ use.ToString() + " " + target.ToString())
        {
            TypeUsage = use;
            Target = target;
        }

        public void InvokeReal(SimActor TheBot)
        {
   
            String use = TypeUsage.UsageName;

            // Create the Side-Effect closure
            ThreadStart closure = new ThreadStart(delegate()
            {
                InvokeBotSideEffect(TheBot);
            });

            bool animFound = TypeUsage.UseSit;
            // IF UseAnim was specified
            if (!String.IsNullOrEmpty(TypeUsage.UseAnim))
            {
                UUID animID = WorldObjects.GridMaster.SimAssetSystem.GetAssetUUID(TypeUsage.UseAnim, AssetType.Animation);
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
                UUID animID = WorldObjects.GridMaster.SimAssetSystem.GetAssetUUID(use, AssetType.Animation);
                if (animID != UUID.Zero)
                    closure = TheBot.WithAnim(animID, closure);
            }

            // Surround with tough/grab if needed
            if (use == "touch" || use == "grab" || TypeUsage.UseGrab)
                closure = TheBot.WithGrabAt(Target, closure);

            // Surround with Sit if needed
            if (use == "sit" || TypeUsage.UseSit)
                closure = TheBot.WithSitOn(Target, closure);


            // Approach Target
            try
            {
                double howClose = TheBot.Approach(Target, TypeUsage.maximumDistance);
                TheBot.ApproachPosition = Target;
                TheBot.TurnToward(Target);
                if (howClose - 1 > TypeUsage.maximumDistance + 0.5f)
                {
                    TheBot.Debug("Too far away " + howClose + " from " + this);
                    return;
                }
                Target.MakeEnterable(TheBot);
                closure.Invoke();
                if (Target == TheBot.ApproachPosition)
                {
                    TheBot.ApproachPosition = null;
                }
            }
            finally
            {
                Target.RestoreEnterable(TheBot);
            }
        }

        public virtual void InvokeBotSideEffect(SimAvatar TheBot)
        {
            TheBot.Debug(ToString());
            //User.ApplyUpdate(use, simObject);
            BotNeeds CurrentNeeds = (BotNeeds)TheBot["CurrentNeeds"];
            BotNeeds needsBefore = CurrentNeeds.Copy();
            BotNeeds update = Target.GetActualUpdate(TypeUsage.UsageName);
            //TODO rate interaction and update TheBot.Assumptions
            CurrentNeeds.AddFrom(update);
            CurrentNeeds.SetRange(0.0F, 100.0F);
            BotNeeds difNeeds = CurrentNeeds.Minus(needsBefore);
            TheBot.Debug(TheBot + " " + ToString() + "\n\t " +
                TheBot.DistanceVectorString(Target)
                + "=> " + difNeeds.ShowNonZeroNeeds());
            if (TheBot is SimActor) ((SimActor)TheBot).ExecuteLisp(this, TypeUsage);
            Thread.Sleep(TypeUsage.totalTimeMS);
        }


        public BotNeeds GetProposedChange()
        {
            return Target.GetProposedUpdate(TypeUsage.UsageName);
        }

        public Vector3 GetUsePosition()
        {
            return Target.GetSimPosition();
        }

        public override float RateIt(BotNeeds simAvatar)
        {
            return GetProposedChange().TotalSideEffect(simAvatar);
        }
    }

    // most object have use that advertises ChangePromise but actually calls ChangeActual
    public class SimTypeUsage : SimUsage
    {

        public override FirstOrderTerm GetTerm()
        {
            throw new NotImplementedException();
        }

        public SimTypeUsage(String name):base(name)
        {
            if (name == "Passable")
            {
                throw new ArgumentException();
            }
            
        }

        // the scripting usename name
        public string TextName = "";

        // the maximum distance the user can be away *excluding* the object size
        public int maximumDistance = 1;

        // How much time the effect should take total
        public int totalTimeMS = 14000;  // the time this usage takes

        // Side effects On "use"
        public string IsTransformedOnUse = null; // new type it converts to
        public bool IsDestroyedOnUse;

        //what really happens ofter 1 minute use
        public BotNeeds ChangeActual = new BotNeeds(0.0F);

        // what most users think will happen by default
        public BotNeeds ChangePromise = new BotNeeds(0.0F);

        public ListAsSet<string> SpecifiedProperties = new ListAsSet<string>();
        // if true the avatar will attempt to sit on the object for the duration
        public bool UseSit = false;
        // if true the client will attempt to invoke the "touch/grab" in SL for the duration
        public bool UseGrab = false;
        // if "KICK" or another Anim the avatar will play this anim
        public String UseAnim = null;
        /// if set the client will attempt to run
        /// the lisp code that does the animation effects
        public Object LispScript = null; 

        public string ToDebugString()
        {
            String str = UsageName;
            str += " TextName: '" + TextName;
            str += "' totalTimeMS: " + totalTimeMS;
            str += " maximumDistance: " + maximumDistance;
            str += " ChangePromise:" + ChangePromise.ShowNonZeroNeeds();
            str += " ChangeActual:" + ChangeActual.ShowNonZeroNeeds();
            if (SpecifiedProperties.Contains("UseSit")) str += " UseSit: " + UseSit;
            if (SpecifiedProperties.Contains("UseGrab")) str += " UseGrab: " + UseGrab;
            if (SpecifiedProperties.Contains("UseAnim")) str += " UseAnim: " + UseAnim;
            if (SpecifiedProperties.Contains("LispScript")) str += " LispScript: " + LispScript;
            return str;
        }

        public SimTypeUsage OverrideProperties(SimTypeUsage use)
        {
            SimTypeUsage newUse = this;
            foreach (string prop in use.SpecifiedProperties)
            {
                newUse.SpecifiedProperties.AddTo(prop);
                System.Reflection.FieldInfo fi = newUse.GetType().GetField(prop);
                if (fi.FieldType==typeof(BotNeeds)) continue;
                SimTypeSystem.SetValue(fi, newUse, fi.GetValue(use));
            }
            //if (use.SpecifiedProperties.Contains("TextName"))
            //    newUse.TextName = use.TextName;
            //if (use.SpecifiedProperties.Contains("UseGrab"))
            //    newUse.UseGrab = use.UseGrab;
            //if (use.SpecifiedProperties.Contains("UseSit"))
            //    newUse.UseSit = use.UseSit;
            //if (use.SpecifiedProperties.Contains("LispScript"))
            //    newUse.LispScript = use.LispScript;
            //if (use.SpecifiedProperties.Contains("UseAnim"))
            //    newUse.UseAnim = use.UseAnim;

            newUse.ChangeActual = newUse.ChangeActual.Copy();
            newUse.ChangeActual.AddFrom(use.ChangeActual);
            newUse.ChangePromise = newUse.ChangePromise.Copy();
            newUse.ChangePromise.AddFrom(use.ChangePromise);
            return newUse;
        }

        public override float RateIt(BotNeeds needs)
        {
            return ChangePromise.TotalSideEffect(needs);
        }
    }


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
            return Victem.GetSimPosition();
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
