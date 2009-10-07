using System;
using System.Collections;
using System.Collections.Generic;
using System.Reflection;
using System.Threading;
using cogbot.TheOpenSims;
using cogbot.Utilities;
using PathSystem3D.Navigation;
using Exception=System.Exception;
using String=System.String;
using OpenMetaverse;
using cogbot;

namespace TheSimiansModule
{
    //public class SimianThinkerModule : WorldObjectsModule
    //{
    //    static Dictionary<SimAvatar,SimThinker> Thinkers =  new Dictionary<SimAvatar, SimThinker>();
    //    public SimianThinkerModule(BotClient _parent)
    //        : base(_parent)
    //    {
            
    //    }
                                                                                                                                

    //    public override string GetModuleName()
    //    {
    //        return "SimianThinkerModule";
    //    }

    //    public override void StartupListener()
    //    {
    //        if (!client.WorldSystem.IsRegionMaster) return;
    //       // throw new NotImplementedException();
    //    }

    //    public override void Dispose()
    //    {
    //      //  throw new NotImplementedException();
    //    }
    //}

    public class SimThinker : BotAction, SimAborter
    {
        private BotAction CurrentAction;
        private Thread avatarThinkerThread;
        /// <summary>
        ///  Actions observed
        ///  Actions the bot might do next cycle.
        /// </summary>
        //private readonly List<BotAction> KnownBotAcions = new List<BotAction>();

        /// <summary>
        ///  which will be skewed with how much one bot like a Mental Aspect
        /// notice this also stores object types that pleases the bot as well as people
        ///  (so how much one bot likes another avatar is stored here as well)
        /// </summary> 
        public readonly Dictionary<BotMentalAspect, int> AspectEnjoyment = new Dictionary<BotMentalAspect, int>();

        /// <summary>
        ///  Assumptions about stubs
        /// </summary>
        public readonly Dictionary<SimObjectType, BotNeeds> Assumptions = new Dictionary<SimObjectType, BotNeeds>();

        /// <summary>
        ///  When seeking out objects for use
        ///  the whole region at least - this is different than the sight distance
        /// </summary>
        public double MaxThinkAboutDistance = 256d;

        /// <summary>
        ///  When seeking out objects for use -  
        ///  this is only limited due to the pathfinder demo for the moment
        /// </summary>
        public double MaxSupportedZChange = 2d;

        private ListAsSet<SimObject> InterestingObjects = new ListAsSet<SimObject>();

        public SimThinker(SimActor a)
            : base(String.Format("AvatarThinkerThread for {0}", a))
        {
            Actor = a;
            Actor["CurrentNeeds"] = new BotNeeds(90.0f);
            avatarHeartbeatThread = new Thread(Aging)
                                        {
                                            Name = String.Format("AvatarHeartbeatThread for {0}", Actor),
                                            Priority = ThreadPriority.Lowest
                                        };
            avatarHeartbeatThread.Start();
            ShowDebug();
        }


        public void Think()
        {
            while (true)
            {
                try
                {
                    Thread.Sleep(3000);
                    if (!IsAvatarActive())
                    {
                        ThinkOnce();
                    }
                }
                catch (Exception e)
                {
                    Debug(e.ToString());
                }
            }
        }

        private bool IsAvatarActive()
        {
            BotAction cur = Actor.CurrentAction;
            if (cur == this) return false;
            if (cur == null) return false;
            if (cur == CurrentAction) return false;
            return true;
        }

        private void Debug(string p)
        {
            if (_debugWindow != null && !_debugWindow.IsDisposed) _debugWindow.WriteLine(p);
            Actor.Debug(p);
        }

        public void StartThinking()
        {
            if (avatarThinkerThread == null)
            {
                avatarThinkerThread = new Thread(Think) { Name = String.Format("AvatarThinkerThread for {0}", Actor) };
                if (Actor.IsControllable)
                {
                    ///  only think for ourselves
                    avatarThinkerThread.Priority = ThreadPriority.Normal;
                    avatarThinkerThread.Start();
                }
            }
            else if (!avatarThinkerThread.IsAlive) avatarThinkerThread.Resume();
        }

        public bool IsThinking()
        {
            return (avatarThinkerThread != null && avatarThinkerThread.IsAlive);
        }
        public void PauseThinking()
        {
            Abort();
        }

        public override void Abort()
        {
            if (avatarThinkerThread != null)
            {
                try
                {
                    ///  avatarThinkerThread.Suspend();
                    avatarThinkerThread.Abort();
                    avatarThinkerThread = null;
                }
                catch (Exception)
                {
                }
            }
            if (IsControlling)
            {
                Actor.CurrentAction = null;
            }
        }

        protected bool IsControlling
        {
            get
            {
                return Actor.CurrentAction == CurrentAction || Actor.CurrentAction == this ||
                       Actor.CurrentAction is AbortableAction;
            }
        }

        public SimObject GetNextInterestingObject()
        {
            SimObject mostInteresting = null;
            if (InterestingObjects.Count < 2)
            {
                InterestingObjects = Actor.GetKnownObjects();
                lock (InterestingObjects) InterestingObjects.Remove(Actor);
            }
            int count = InterestingObjects.Count - 2;
            foreach (BotMentalAspect cAspect in InterestingObjects)
            {
                if (cAspect is SimObject)
                {
                    if (mostInteresting == null)
                    {
                        mostInteresting = (SimObject)cAspect;
                        ///  break;
                    }
                    else
                    {
                        mostInteresting = (SimObject)CompareTwo(mostInteresting, cAspect);
                    }
                    count--;
                    if (count < 0) break;
                }
            }
            lock (InterestingObjects)
            {
                InterestingObjects.Remove(mostInteresting);
                InterestingObjects.Add(mostInteresting);                
            }
            return mostInteresting;
        }

        private readonly Random MyRandom = new Random(DateTime.Now.Millisecond);


        /// <summary>
        ///   TODO Real Eval routine
        /// </summary>
        /// <param name="mostInteresting"></param>
        /// <param name="cAspect"></param>
        /// <returns></returns>
        public BotMentalAspect CompareTwo(BotMentalAspect mostInteresting, BotMentalAspect cAspect)
        {
            if ((mostInteresting is SimObject) && (cAspect is SimObject))
            {
                int rate = CompareObjects((SimObject)mostInteresting, (SimObject)cAspect);
                if (rate > 0) return cAspect;
                if (rate < 0) return mostInteresting;
            }
            return (MyRandom.Next(1, 2) == 1) ? mostInteresting : cAspect;
        }


        //   public Thread avatarThinkerThread;

        readonly private SimActor Actor;

        public SimAvatar InDialogWith { get; set; }

        //public override void InvokeReal()
        //{
        //    if (avatarThinkerThread == null)
        //    {
        //        avatarThinkerThread = new Thread(Think) { Name = String.Format( };
        //        if (Actor.IsControllable)
        //        {
        //            ///  only think for ourselves
        //            avatarThinkerThread.Priority = ThreadPriority.Normal;
        //            avatarThinkerThread.Start();
        //        }
        //    }
        //    if (!avatarThinkerThread.IsAlive) avatarThinkerThread.Resume();
        //}

        public override void InvokeReal()
        {
            while (true)
            {
                try
                {
                    StartThinking();
                }
                catch (Exception e)
                {
                    Actor.Debug(e.ToString());
                }
            }
        }


        public void ThinkOnce()
        {
            Actor.ScanNewObjects(2, Actor.SightRange, false);
            CurrentAction = GetNextAction();
            Actor.CurrentAction = new AbortableAction(CurrentAction, this);
        }


        public int CompareUsage(SimUsage act1, SimUsage act2)
        {
            return (int)(act2.RateIt(CurrentNeeds) - act1.RateIt(CurrentNeeds));
        }


        public int CompareObjects(SimObject act1, SimObject act2)
        {
            if (act1 == act2) return 0;
            if (act1 == null) return -1;
            if (act2 == null) return 1;
            return (int)(act2.RateIt(CurrentNeeds) - act1.RateIt(CurrentNeeds));
        }


        public string DebugInfo()
        {
            String s = String.Format("\n{0}", ToString());

            int show = 10;

            List<SimObject> KnowsAboutList = Actor.GetKnownObjects().CopyOf();
            lock (KnowsAboutList)
            {
                KnowsAboutList.Sort(CompareObjects);
                s += String.Format("\nKnowsAboutList: {0}", KnowsAboutList.Count);
                foreach (SimObject item in KnowsAboutList)
                {
                    show--;
                    if (show < 0) break;
                    /// if (item is ISimAvatar) continue;
                    s += String.Format("\n   {0} {1}", item, Actor.DistanceVectorString(item));
                }
            }

            show = 10;
            List<SimTypeUsage> KnownTypeUsages = new List<SimTypeUsage>(Actor.KnownTypeUsages);
            KnownTypeUsages.Sort(CompareUsage);
            s += String.Format("\nKnownTypeUsages: {0}", KnownTypeUsages.Count);
            foreach (SimTypeUsage item in KnownTypeUsages)
            {
                show--;
                if (show < 0) break;
                /// if (item is ISimAvatar) continue;
                s += String.Format("\n   {0} {1}", item, item.RateIt(CurrentNeeds));
            }

            s += String.Format("\nCurrentNeeds: {0}", CurrentNeeds);
            s += String.Format("\nNextAction: {0}", GetNextAction());
            s += String.Format("\nLastAction: {0}", Actor.LastAction);
            s += String.Format("\nCurrentAction: {0}", Actor.CurrentAction);
            return s;
        }


        public Thread avatarHeartbeatThread;

        public void Aging()
        {

            BotNeeds CurrentNeeds = this.CurrentNeeds;

            // Initially randomize
            foreach (FieldInfo c in CurrentNeeds.GetType().GetFields())
            {
                if (c.FieldType == typeof(float))
                {
                    CurrentNeeds.SetValue(c, CurrentNeeds, MyRandom.Next(100));
                }
            }

            BotNeeds OneMinute = SimTypeSystem.GetObjectType("OnMinuteTimer").GetUsageActual("OnMinuteTimer");
            while (true)
            {
                Actor.ScanNewObjects(2, Actor.SightRange, false);
                CurrentNeeds.AddFrom(OneMinute);
                CurrentNeeds.SetRange(0.0F, 100.0F);
                //SimPosition to = WorldObjects.Master.m_TheSimAvatar;
                //if (to != null)
                //{
                //    Console.WriteLine("Aging: " + this + " " + to.DistanceVectorString(this));
                //}
                Thread.Sleep(60000); ///  one minute

                ///  Debug(CurrentNeeds.ToString());
            }
        }


        public BotNeeds CurrentNeeds
        {
            get { return (BotNeeds)Actor["CurrentNeeds"]; }
        }




        public void DoBestUse(SimObject someObject)
        {
            if (someObject == null) return;
            SimTypeUsage use = someObject.GetBestUse(CurrentNeeds);
            if (use == null)
            {
                double closeness = Actor.Approach(someObject, someObject.GetSizeDistance());
                //AgentManager ClientSelf = Client.Self;
                Actor.Touch(someObject);
                if (closeness < 3)
                {
                    Actor.SitOn(someObject);
                }
                return;
            }
            Actor.Do(use, someObject);
            return;
        }

        public void UseAspect(BotMentalAspect someAspect)
        {
            try
            {
                if (someAspect is BotAction)
                {
                    BotAction act = (BotAction)someAspect;
                    act.InvokeReal();
                    return;
                }
                if (InDialogWith != null)
                {
                    Actor.TalkTo(InDialogWith, someAspect);
                    return;
                }

                if (someAspect is SimObject)
                {
                    SimObject someObject = (SimObject)someAspect;
                    DoBestUse(someObject);
                }
            }
            finally
            {
                //if (IsThinking)
                //    CurrentAction = this;
            }
        }

        public List<BotAction> GetPossibleActions(double maxXYDistance, double maxZDist)
        {
            List<SimObject> KnownObjects = Actor.GetKnownObjects();
            double myZ = Actor.GlobalPosition.Z;
            List<SimObject> useObjects = new List<SimObject>();
            foreach (SimObject O in KnownObjects)
            {
                if (!O.IsRegionAttached) continue;
                if (O.Distance(Actor) > maxXYDistance) continue;
                if (Math.Abs(O.GlobalPosition.Z - myZ) > maxZDist) continue;
                useObjects.Add(O);
            }
            // useObjects.Sort(Actor.CompareDistance);


            List<SimTypeUsage> KnownTypeUsages = new List<SimTypeUsage>(Actor.KnownTypeUsages);
            KnownTypeUsages.Sort(CompareUsage);


            List<BotAction> KnownBotAcions = new List<BotAction>();

            lock (KnownTypeUsages)
                foreach (SimTypeUsage use in KnownTypeUsages)
                {
                    lock (useObjects) foreach (SimObject obj in useObjects)
                        {
                            if (CurrentAction != null)
                            {
                                if (obj == CurrentAction.Target) continue;
                            }
                            if (obj.GetTypeUsages().Contains(use))
                            {
                                KnownBotAcions.Add(new BotObjectAction(Actor, new SimObjectUsage(use, obj)));
                            }
                        }
                }
            return KnownBotAcions;
        }



        public BotAction GetNextAction()
        {
            BotAction act = CurrentAction;
            List<BotAction> KnownBotAcions = GetPossibleActions(MaxThinkAboutDistance, MaxSupportedZChange);

            lock (KnownBotAcions)
            {
                if (KnownBotAcions.Count > 0)
                {
                    act = KnownBotAcions[0];// (BotAction)FindBestUsage(KnownBotAcions);
                }
                if (act == null)
                {
                    SimRegion R = Actor.GetSimRegion();
                    if (R==null) return new CommandAction(Actor,"anim shrug");
                    Vector3d v3d =
                       R.LocalToGlobal(new Vector3(MyRandom.Next(250) + 5, MyRandom.Next(250) + 5,
                                                                       Actor.SimPosition.Z));
                    Actor.Debug("MoveToLocation: " + Actor.DistanceVectorString(v3d));
                    SimPosition WP = SimWaypointImpl.CreateGlobal(v3d);
                    act = new MoveToLocation(Actor, WP);
                }
                return act;
            }
        }

        public SimUsage FindBestUsage(IEnumerable KnownBotAcions)
        {
            SimUsage bestAct = null;
            if (KnownBotAcions != null)
            {
                lock (KnownBotAcions)
                {
                    {
                        IEnumerator enumer = KnownBotAcions.GetEnumerator();
                        double bestRate = double.MinValue;
                        while (enumer.MoveNext())
                        {
                            SimUsage b = (SimUsage)enumer.Current;
                            double brate = b.RateIt(CurrentNeeds);
                            if (brate > bestRate)
                            {
                                bestAct = b;
                                bestRate = brate;
                            }
                        }
                    }
                }
            }
            return bestAct;
        }


        public override BotNeeds ProposedChange()
        {
            return CurrentAction.ProposedChange();
        }

        public override string ToString()
        {
            if (CurrentAction == null) return base.ToString();
            return CurrentAction.ToString();

        }
        public override Vector3 GetUsePostion()
        {
            if (CurrentAction == null) return Actor.SimPosition;
            return CurrentAction.GetUsePostion();
        }

        public override FirstOrderTerm GetTerm()
        {
            throw new NotImplementedException();
        }

        public override SimPosition Target
        {
            get
            {
                if (CurrentAction != null) return CurrentAction.Target;
                return null;
            }
            set
            {
                if (CurrentAction != null) CurrentAction.Target = value;
            }
        }

        private readonly object _debugWindowLock = new object();
        private SimThinkerDebug _debugWindow;
        internal void ShowDebug()
        {
            lock (_debugWindowLock)
                if (_debugWindow == null || _debugWindow.IsDisposed)
                {
                    BotClient gc = Actor.GetGridClient();
                    gc.Invoke((() =>
                                                    {
                                                        try
                                                        {
                                                            string name = "ST: " + gc.GetName();
                                                            _debugWindow = new SimThinkerDebug(name,gc);
                                                            gc.AddTab(name, name, _debugWindow, CloseDebug);
                                                        }
                                                        catch (Exception e)
                                                        {
                                                            Console.WriteLine("" + e);
                                                        }
                                                    }
                                         ));
                }
                else
                {
                    _debugWindow.Show();
                }
        }

        private void CloseDebug(object sender, EventArgs e)
        {
            lock (_debugWindowLock)
            {
                _debugWindow = null;
            }
        }
    }
}