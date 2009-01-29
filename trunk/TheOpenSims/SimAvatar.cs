using System;
using System.Collections.Generic;
using System.Text;
using OpenMetaverse;
using DotLisp;
using System.Reflection;
using cogbot.Listeners;
using System.Threading;
//Complex outcomes may be a result of simple causes, or they may just be complex by nature. 
//Those complexities that turn out to have simple causes can be simulated and studied, 
//thus increasing our knowledge without needing direct observation.
namespace cogbot.TheSims
{
    // Mental Aspects
    abstract public class BotMentalAspect
    {
        public BotMentalAspect(string s)
        {
            AspectName = s;
        }
        public string AspectName;
        public UUID SLRef; // the overhead bubble
        public override string ToString()
        {
            return GetType().Name + "::" + AspectName;
        }
    }

    //TheSims-like object
    public class SimObject : BotMentalAspect
    {
        public string DebugInfo()
        {
            return ToString();
        }

        public Primitive thePrim; // the prim in Secondlife
        Vector3 lastPos = Vector3.Zero;
        public float scaleOnNeeds = 0.99F; // the bonus or handicap the object has compared to the defination (more expensive chair might have more effect)

        public float RateIt(BotNeeds againsNeeds, SimAvatar avatar)
        {
            GetMenu(avatar);
            return ObjectType.RateIt(againsNeeds, GetDefaultUse()) * scaleOnNeeds;
        }


        public List<string> GetMenu(SimAvatar avatar)
        {
            //props.Permissions = new Permissions(objectData.BaseMask, objectData.EveryoneMask, objectData.GroupMask,
            //  objectData.NextOwnerMask, objectData.OwnerMask);
            List<string> list = new List<string>();
            if (thePrim.Properties != null)
            {
                //  if (thePrim.Properties.TouchName != "")
                list.Add("touch");
                //   if (thePrim.Properties.SitVerb != "")
                list.Add("sit");
                PermissionMask mask = thePrim.Properties.Permissions.EveryoneMask;
                if (thePrim.OwnerID == avatar.Client.Self.AgentID) { mask = thePrim.Properties.Permissions.OwnerMask; }
                PermissionMask result = mask | thePrim.Properties.Permissions.BaseMask;
                if ((result & PermissionMask.Copy)!=0)
                    list.Add("copy");
                if ((result & PermissionMask.Modify)!=0)
                    list.Add("modify");
                if ((result & PermissionMask.Move)!=0)
                    list.Add("move");
                if ((result & PermissionMask.Transfer) != 0)
                    list.Add("transfer");
                if ((result & PermissionMask.Damage) != 0)
                    list.Add("damage");
            }
            return list;
        }
        public SimObject(string name, Primitive prim)
            : base(name)
        {
            thePrim = prim;
            m_objectType = BotRegionModel.BotWorld.GetObjectType(name);
            //SimObjectType ot = ObjectType;
        }
        readonly private SimObjectType m_objectType;
        public SimObjectType ObjectType
        {
            get
            {
                m_objectType.SuperTypes = BotRegionModel.BotWorld.GuessSimObjectTypes(thePrim);
                return m_objectType;
            }
        } // the class definations

        public override string ToString()
        {
            String s = base.ToString();
            return s + "(" + ObjectType.ToDebugString() + ")";
        }


        public virtual Vector3 GetPosition()
        {
            if (lastPos == Vector3.Zero) lastPos = thePrim.Position;
            return thePrim.Position;
        }

        public BotNeeds GetActualUpdate(string p)
        {
            return ObjectType.GetUsageActual(p).Magnify(scaleOnNeeds);
        }


        public string GetDefaultUse()
        {
            return ObjectType.GetDefaultUse();
        }

        public Vector3 GetUsePosition()
        {
            return GetPosition();
        }

        internal BotNeeds GetProposedUpdate(string p)
        {
            return ObjectType.GetUsagePromise(p).Magnify(scaleOnNeeds);
        }

        public SimObjectUsage GetSimObjectUsage(SimAvatar simAvatar)
        {
            return ObjectType.GetSimObjectUsage(GetDefaultUse());
        }
    }
    // most object have use that advertises ChangePromise but actually calls ChangeActual
    public class SimObjectUsage
    {
        public int maximumDistance = 1;  // the maximum distance the user can be away
        public BotNeeds ChangePromise = new BotNeeds(0.0F); // what most users think will happen by default
        public BotNeeds ChangeActual = new BotNeeds(0.0F); //what really happens ofter 1 minute use
        public string TextName = "use"; // the scripting verb name
        public Cons script = null; // the lisp code that does the animation effects
    }

    abstract public class BotAction : BotMentalAspect
    {
        public BotAction(string s)
            : base(s)
        {
        }
        public SimAvatar TheBot;
        public SimObjectUsage Usage;
        // Returns how much the needs should be changed;
        public abstract BotNeeds ProposedChange();
        // the needs are really changed;
        public abstract void InvokeReal();

    }

    public class BotObjectAction : BotAction
    {
        public SimObject Target;
        public BotObjectAction(SimAvatar who, SimObjectUsage what, SimObject target)
            : base(what.TextName + " " + target)
        {
            TheBot = who;
            Usage = what;
            Target = target;
        }

        public override void InvokeReal()
        {
            String use = Usage.TextName;
            TheBot.Approach(use, Target);
            TheBot.Debug(ToString());
            Thread.Sleep(8000);
            //User.ApplyUpdate(use, simObject);
            BotNeeds CurrentNeeds = TheBot.CurrentNeeds;
            BotNeeds needsBefore = CurrentNeeds.Copy();
            BotNeeds update = Target.GetActualUpdate(Usage.TextName);
            //TODO rate interaction and update TheBot.Assumptions
            CurrentNeeds.AddFrom(update);
            CurrentNeeds.SetRange(0.0F, 100.0F);
            BotNeeds difNeeds = CurrentNeeds.Minus(needsBefore);
            TheBot.Debug(ToString() + "\n\t=> " + difNeeds.ShowNonZeroNeeds());
        }

        public override BotNeeds ProposedChange()
        {
            return Target.GetProposedUpdate(Usage.TextName);
        }
        public override string ToString()
        {
            return "BotObjectAction:( " + Usage.TextName + " " + Target+")";
        }
    }

    public class BotSocialAction : BotAction
    {
        static Random rand = new Random();
        public SimAvatar Victem;
        public BotMentalAspect CurrentTopic;
        public int TimeRemaining;
        public BotSocialAction(SimAvatar who, SimObjectUsage what, SimAvatar target)
            :base(what.TextName+ " " + target)
        {
            TheBot = who;
            Usage = what;
            Victem = target;
            CurrentTopic = null;
            TimeRemaining = rand.Next(1, 3); // one to tree cycles
        }

        public override BotNeeds ProposedChange()
        {
            return Usage.ChangePromise;
        }

        public override void InvokeReal()
        {
            TimeRemaining = rand.Next(1, 3); // one to tree cycles
            while (TimeRemaining-- > 0)
            {
                String use = Usage.TextName;
                TheBot.Approach(use, Victem);
                TheBot.Debug(ToString());
                CurrentTopic = TheBot.GetNextInterestingObject();
                TheBot.TalkAbout(Victem,CurrentTopic);
                Thread.Sleep(8000);
                //User.ApplyUpdate(use, simObject);
            }
            BotNeeds CurrentNeeds = TheBot.CurrentNeeds;
            BotNeeds needsBefore = CurrentNeeds.Copy();
            BotNeeds simNeeds = Usage.ChangeActual;
            //TODO rate interaction
            CurrentNeeds.AddFrom(simNeeds);
            CurrentNeeds.SetRange(0.0F, 100.0F);
            BotNeeds difNeeds = CurrentNeeds.Minus(needsBefore);
            TheBot.Debug(ToString() + " => " + difNeeds.ShowNonZeroNeeds());
        }
        public override string ToString()
        {
            return "BotSocialAction:( " + Usage.TextName + " " + Victem + ")";
        }
    }


    public class SimAvatar : SimObject
    {
        public BotClient Client;
        public Thread avatarThinkerThread = null;
        public Thread avatarHeartbeatThread = null;

        public Avatar theAvatar = null;

        public SimObject InDialogWith = null;

        public BotNeeds CurrentNeeds;


        // things the bot cycles through mentally
        public ListAsSet<SimObject> KnowsAboutList = new ListAsSet<SimObject>();

        // which will result in 
        public ListAsSet<BotAction> KnowsActList = new ListAsSet<BotAction>();

        // which will be skewed with how much one bot like a Mental Aspect
        public Dictionary<BotMentalAspect, int> AspectEnjoyment = new Dictionary<BotMentalAspect, int>();

        //notice this also stores object types that pleases the bot as well as people
        // (so how much one bot likes another avatar is sotred here as well)


        // assuptions
        public Dictionary<SimObjectType, BotNeeds> Assumptions = new Dictionary<SimObjectType, BotNeeds>();


        public BotAction CurrentMentalEvent = null;



        public SimAvatar(Avatar slAvatar)
            : base(slAvatar.Name, slAvatar)
        {
            //ObjectType = BotRegionModel.BotWorld.GetObjectType("Avatar");
            theAvatar = slAvatar;
            CurrentNeeds = new BotNeeds(90.0F);
            AspectName = slAvatar.Name;
            Client = BotRegionModel.BotWorld.Client;
            Client.Settings.SEND_AGENT_THROTTLE = true;
            Client.Settings.SEND_AGENT_UPDATES = true;
            avatarHeartbeatThread = new Thread(new ThreadStart(Aging));
            avatarHeartbeatThread.Start();
        }

        internal void StartThinking()
        {
            if (avatarThinkerThread == null)
            {
                avatarThinkerThread = new Thread(new ThreadStart(Think));
                if (theAvatar.LocalID == Client.Self.LocalID)
                {
                    // only think for ourselves
                    avatarThinkerThread.Start();
                }
            }
            if (!avatarThinkerThread.IsAlive) avatarThinkerThread.Resume();
        }

        public bool IsThinking()
        {
            return (avatarThinkerThread!=null);
        }
        internal void PauseThinking()
        {
            if (avatarThinkerThread != null)
            {
                try
                {
                   // avatarThinkerThread.Suspend();
                    avatarThinkerThread.Abort();
                    avatarThinkerThread = null;
                }
                catch (Exception e)
                {
                }
            }
        }

        Vector3 lastgood = new Vector3(128, 128, 25);
        public override Vector3 GetPosition()
        {
            if (theAvatar.Position.X < 59 || theAvatar.Position.Y < 59 || theAvatar.Position.Z < 10)
            {
                return lastgood;
                //throw new UnauthorizedAccessException("" + this + " is not at " + theAvatar.Position);
            }
            lastgood = theAvatar.Position;
            return lastgood;
        }

        public void Think()
        {
            while (true)
            {
                try
                {
                    Thread.Sleep(3000);
                    ThinkOnce();
                }
                catch (Exception e)
                {
                    Debug(e.ToString());
                }
            }
        }
        public void Aging()
        {
            while (true)
            {
                CurrentNeeds.AddFrom(BotRegionModel.BotWorld.GetObjectType("OnMinuteTimer").GetUsageActual("near"));
                CurrentNeeds.SetRange(0.0F, 100.0F);
                Thread.Sleep(60000); // one minute
                Debug(CurrentNeeds.ToString());
            }
        }


        public void ThinkOnce()
        {
            ScanNewObjects();

            Thread.Sleep(2000);
            CurrentMentalEvent = GetNextAction();
            if (CurrentMentalEvent != null)
            {
                UseAspect(CurrentMentalEvent);
            }
        }

        private BotAction GetNextAction()
        {
            BotAction act = CurrentMentalEvent;
            SimObject neat = GetNextInterestingObject();
            if (neat == this)
            {
                neat = GetNextInterestingObject();
            }
            if (neat is SimAvatar)
            {
                SimObjectUsage usage = neat.ObjectType.GetSimObjectUsage("talk");
                act = new BotSocialAction(this, usage, (SimAvatar)neat);
                return act;
            }
            SimObjectUsage use = neat.GetSimObjectUsage(this);
            return new BotObjectAction(this, use, neat);
        }

        public void UseAspect(BotMentalAspect someAspect)
        {
            if (someAspect is BotAction)
            {
                BotAction act = (BotAction)someAspect;
                act.InvokeReal();
                return;
            }
            if (InDialogWith != null)
            {
                TalkAbout(InDialogWith, someAspect);
                return;
            }
            else
            {
                if (someAspect is SimAvatar)
                {
                    // SocialTo("talk",(SimAvatar)someAspect);
                    return;
                }
                //UseObject((SimObject)someAspect);
            }
        }
        //public void SocialTo(String verb,SimAvatar simAvatar)
        //{
        //    Approach(verb,simAvatar);           
        //    InDialogWith = simAvatar;
        //    Debug("TalkTo... "+verb+ " " + simAvatar);
        //    Thread.Sleep(4000);
        //    TalkTimeRemaining = (new Random()).Next(2)+2;
        //    CurrentMentalEvent = null;
        //    ApplyUpdate("talk", InDialogWith);
        //}
        //public void UseObject(SimObject simObject)
        //{
        //    String use = simObject.GetDefaultUse();
        //    Approach(use, simObject);
        //    Debug("Using... " + use + " " + simObject);
        //    Thread.Sleep(8000);
        //    ApplyUpdate(use, simObject);
        //}
        //public void ApplyUpdate(string verb, BotMentalAspect aspect)
        //{
        //    BotNeeds needsBefore = CurrentNeeds.Copy();
        //    BotNeeds simNeeds = ((SimObject)aspect).GetActualUpdate(verb);
        //    //TODO rate interaction
        //    CurrentNeeds.AddFrom(simNeeds);
        //    CurrentNeeds.SetRange(0.0F, 100.0F);
        //    BotNeeds difNeeds = CurrentNeeds.Minus(needsBefore);
        //    Debug(difNeeds.ShowNonZeroNeeds());
        //}

        public SimObject GetNextInterestingObject()
        {
            SimObject mostInteresting = null;
            KnowsAboutList.Remove(this);
            int count = KnowsAboutList.Count - 2;
            foreach (BotMentalAspect cAspect in KnowsAboutList)
            {
                if (cAspect is SimObject)
                {
                    if (mostInteresting == null)
                    {
                        mostInteresting = (SimObject)cAspect;
                        // break;
                    }
                    else
                    {
                        mostInteresting = (SimObject)CompareTwo(mostInteresting, cAspect);
                    }
                    count--;
                    if (count < 0) break;
                }
            }
            KnowsAboutList.Remove(mostInteresting);
            KnowsAboutList.AddTo(mostInteresting);
            return mostInteresting;
        }
        readonly Random MyRandom = new Random();
        // TODO Real Eval routine
        public BotMentalAspect CompareTwo(BotMentalAspect mostInteresting, BotMentalAspect cAspect)
        {
            if ((mostInteresting is SimObject) && (cAspect is SimObject))
            {
                return CompareObjects((SimObject)mostInteresting, (SimObject)cAspect);
            }
            return (MyRandom.Next(1, 2) == 1) ? mostInteresting : cAspect;
        }

        private BotMentalAspect CompareObjects(SimObject simObject, SimObject simObject_2)
        {
            if (simObject == simObject_2) return simObject;
            float a1 = simObject.RateIt(CurrentNeeds,this);
            float a2 = simObject_2.RateIt(CurrentNeeds,this);
            if (a2 > a1) return simObject_2;
            return simObject;
        }

        public void ScanNewObjects()
        {
            ListAsSet<SimObject> objects = BotRegionModel.BotWorld.objects;
            lock (objects) foreach (SimObject obj in objects)
                {
                    if (Vector3.Distance(obj.GetPosition(), GetPosition()) < 30)
                    {
                        lock (KnowsAboutList) if (!KnowsAboutList.Contains(obj))
                            {
                                if (KnowsAboutList.Count < 2) KnowsAboutList.AddTo(obj);
                                else
                                    KnowsAboutList.Insert(1, obj);
                            }
                    }
                }
        }
        public void Approach(string verb, SimObject obj)
        {
            if (Client.Self.SittingOn != 0 || Client.Self.Movement.SitOnGround)
            {
                Client.Self.Stand();
            }
            float dist = 2.0F;
            if (obj is SimAvatar)
            {
                dist = 4.0F;
            }
            Vector3 vector3 = obj.GetUsePosition();
            Debug("Approaching " + vector3 + " for " + verb + " " + obj);
            MovementToVector.MoveTo(Client, vector3, dist);
            Client.Self.Movement.TurnToward(obj.GetPosition());
            Thread.Sleep(2000);
            if (verb == "sit")
            {
                Primitive targetPrim = obj.thePrim;
                Client.Self.RequestSit(targetPrim.ID, Vector3.Zero);
                Client.Self.Sit();
            }
        }
        public void TalkAbout(SimObject avatar, BotMentalAspect talkAbout)
        {
            SimObject wasInDialogWith = InDialogWith;
            InDialogWith = avatar;
            Client.Self.Movement.TurnToward(InDialogWith.GetPosition());
            Client.Talk(InDialogWith + ": " + talkAbout);
            Thread.Sleep(3000);
            InDialogWith = wasInDialogWith;
        }

        public void Debug(string p)
        {
            Console.WriteLine("++" + theAvatar.Name
                + ": " + p);
        }

    }

    public class SimObjectType : BotMentalAspect
    {
        public String ToDebugString() {
            String str = ToString() + "[";
            SuperTypes.ForEach(delegate(SimObjectType item) {
                str += " " + item.ToString();
            });
            return str + "]";
        }

        public string SitVerb = null;
        public string GrabVerb = null;
        Dictionary<string, SimObjectUsage> usageAffect = new Dictionary<string, SimObjectUsage>();
        // Object area effect
        public ListAsSet<SimObjectType> SuperTypes = new ListAsSet<SimObjectType>();

        public SimObjectType(string name):base(name)
        {
            AspectName = name;
            SitVerb = "near";
            // Object "use"
           // AddUse(SitVerb, new SimObjectUsage());
            // Object area effect "near"
            AddUse(SitVerb, new SimObjectUsage());
            GetSimObjectUsage("near").maximumDistance = 10;
        }

        public SimObjectUsage RenameUse(string verb, string newName)
        {
            SimObjectUsage simObjectUsage = GetSimObjectUsage(verb);
            usageAffect.Remove(verb);
            simObjectUsage.TextName = newName;
            if (!usageAffect.ContainsKey(newName))
            {
                usageAffect.Add(newName, simObjectUsage);

            }
            else
            {
                simObjectUsage = GetSimObjectUsage(newName);
            }
            simObjectUsage.TextName = newName;
            return simObjectUsage;
        }

        public void AddUse(string verb, SimObjectUsage simObjectUsage)
        {
            usageAffect.Add(verb, simObjectUsage);
            simObjectUsage.TextName = verb;
        }

        public SimObjectUsage GetSimObjectUsage(string verb)
        {
            if (verb == null || verb == "use") verb = "sit";
            if (usageAffect.ContainsKey(verb))
                return usageAffect[verb];
            SimObjectUsage sou = new SimObjectUsage();
            sou.TextName = verb;
            usageAffect[verb] = sou;
            return sou;
        }

        public ListAsSet<string> GetVerbs()
        {
            ListAsSet<string> verbs = new ListAsSet<string>();
            verbs.AddTo(SitVerb);
            foreach (string key in usageAffect.Keys)
            {
                if (!verbs.Contains(key))
                {
                    verbs.AddTo(key);
                }
            }
            foreach (SimObjectType st in SuperTypes)
            {
                foreach (string v in st.GetVerbs())
                {
                    if (!verbs.Contains(v))
                    {
                        verbs.AddTo(v);
                    }
                }
            }
            return verbs;
        }

        public BotNeeds GetUsagePromise(string verb)
        {
            BotNeeds needUpdate = GetSimObjectUsage(verb).ChangePromise.Copy();
            foreach (SimObjectType st in SuperTypes)
            {
                needUpdate.AddFrom(st.GetUsagePromise(verb));
            }
            return needUpdate;
        }

        public float RateIt(BotNeeds from, string verb)
        {
            BotNeeds sat = GetUsagePromise(verb).Copy();
            sat.AddFrom(from);
            sat.SetRange(0.0F, 100.0F);
            return sat.RateIt();
        }

        public BotNeeds GetUsageActual(string verb)
        {
            BotNeeds needUpdate = GetSimObjectUsage(verb).ChangeActual.Copy();
            foreach (SimObjectType st in SuperTypes)
            {
                needUpdate.AddFrom(st.GetUsageActual(verb));
            }
            return needUpdate;
        }

        public string GetTypeName()
        {
            return AspectName;
        }

        public void ParseAffect(String verb, object[] parseStr)
        {
            SimObjectType type = this;
            int i = 0;
            SimObjectUsage usage = null;
            while (i < parseStr.Length)
            {
                if (parseStr[i] == null) return;
                string s = (string)parseStr[i++];//.ToString();
                if (s == "SuperType")
                {
                    SuperTypes.AddTo(BotRegionModel.BotWorld.GetObjectType(parseStr[i++].ToString()));
                    continue;
                }
                if (s == "SitVerb")
                {
                    s = parseStr[i++].ToString();
                    type.SitVerb = s;
                    verb = "sit";
                    usage = type.GetSimObjectUsage(verb.ToString());
                    usage.TextName = s;
                    continue;
                }
                if (s == "GrabVerb")
                {
                    s = parseStr[i++].ToString();
                    type.GrabVerb = s;
                    verb = "grab";
                    usage = type.GetSimObjectUsage(verb.ToString());
                    usage.TextName = s;
                    continue;
                }
          
                if (s == "Verb")
                {
                    verb = parseStr[i++].ToString();
                    usage = type.GetSimObjectUsage(verb.ToString());
                    usage.TextName = verb;
                    continue;
                }
                // usage / distanceToExcite / etc
                FieldInfo fi = type.GetType().GetField(s);
                if (fi != null)
                {
                    fi.SetValue(type, parseStr[i++]);
                    continue;
                }
                usage = type.GetSimObjectUsage(verb.ToString()); 
                fi = usage.GetType().GetField(s);
                if (fi != null)
                {
                    fi.SetValue(usage, parseStr[i++]);
                    continue;
                }

                // Hygiene / Hunger
                fi = typeof(BotNeeds).GetField(s);
                if (fi != null)
                {
                    float ff = Single.Parse( parseStr[i++].ToString());
                    fi.SetValue(usage.ChangePromise, ff);
                    ff = Single.Parse( parseStr[i++].ToString());
                    fi.SetValue(usage.ChangeActual, ff);
                    continue;
                }
                System.Console.WriteLine("ERROR: MISSING " + s + " ... " + parseStr[i]);
            }
        }


        internal string GetDefaultUse()
        {
            List<String> verbs = GetVerbs();
            if (verbs.Contains("near"))
            {
                verbs.Remove("near");
            }
            int item = (new Random()).Next(0, verbs.Count - 1);
            string def = verbs[item];
            if (SitVerb == "near")
            {
                SitVerb = def;
            }
            return def;
        }
    }


    public class BotRegionModel
    {
        ListAsSet<SimAvatar> avatars = new ListAsSet<SimAvatar>();
        public ListAsSet<SimObject> objects = new ListAsSet<SimObject>();
        ListAsSet<SimObjectType> objectTypes = new ListAsSet<SimObjectType>();
        public BotClient Client;
        public static BotRegionModel BotWorld = null;
//        TheBotsInspector inspector = new TheBotsInspector();
        public BotRegionModel(BotClient bc)
        {
            BotWorld = this;
            Client = bc;
            LoadDefaultTypes();
            CatchUp(Client.Network.CurrentSim);
            Client.Avatars.OnAvatarAnimation += Avatars_OnAvatarAnimation;
            Client.Objects.OnObjectProperties += Objects_OnObjectProperties;          
          ///  inspector.Show();
        }

        private void CatchUp(Simulator simulator)
        {
            simulator.ObjectsAvatars.ForEach(delegate(Avatar item)
            {
                GetSimAvatar(item);
            });
            simulator.ObjectsPrimitives.ForEach(delegate(Primitive item)
            {
                GetSimObject(item);
            });
        }

        void Objects_OnObjectProperties(Simulator simulator, Primitive.ObjectProperties props)
        {
            Primitive prim = Client.WorldSystem.GetPrimitive(props.ObjectID);
            if (prim != null) GetSimObject(prim);
        }

        void Avatars_OnAvatarAnimation(UUID avatarID, InternalDictionary<UUID, int> anims)
        {
            Avatar avatar = Client.WorldSystem.GetAvatar(avatarID);
            if (avatar != null) GetSimAvatar(avatar);
        }

        public SimObject GetSimObject(Primitive prim)
        {
            if (prim is Avatar)
            {
                return GetSimAvatar((Avatar)prim);
            }
            lock (objects) foreach (SimObject obj in objects)
            {
                if (obj.thePrim == prim)                
                    return obj;                
            }
            // not found
            SimObject obj0 = new SimObject(prim.ToString(), prim);
            lock (objects) objects.AddTo(obj0);
            RefreshInspector();
            return obj0;
        }

        private void RefreshInspector()
        {
            String str = "";
           // inspector.SetObjectListText(str);
        }

        public SimAvatar GetSimAvatar(Avatar prim)
        {
            lock (avatars) foreach (SimAvatar obj in avatars)
                {
                    if (obj.theAvatar == prim)
                        return obj;
                }
            SimAvatar obj0 = new SimAvatar(prim);
            lock (avatars) avatars.AddTo(obj0);
            lock (objects) objects.AddTo(obj0);
            return obj0;
        }


        public ListAsSet<SimObjectType> GuessSimObjectTypes(Primitive prim)
        {
            ListAsSet<SimObjectType> possibles = new ListAsSet<SimObjectType>();
            SimObjectType type = null;
            if (prim.Properties != null)
            {
                type = FindObjectType(prim.Properties.Name);
                if (type != null)
                {
                    possibles.AddTo(type);
                }
                type = FindObjectType(prim.Properties.Description);
                if (type != null)
                {
                    possibles.AddTo(type);
                }
            }
            type = FindObjectType(Client.WorldSystem.GetPrimTypeName(prim));
            if (type != null)
            {
                possibles.AddTo(type);
            }
            if (prim.Properties != null)
            {
                string objName = prim.Properties.Name.ToLower();
                string objName2 = prim.Properties.Description.ToLower();
                lock (objectTypes) if (objName.Length > 3) foreach (SimObjectType otype in objectTypes)
                    {
                        String otypeAspectName = otype.AspectName.ToLower();
                        if (objName.Contains(otypeAspectName))
                        {
                            possibles.AddTo(otype);
                        }
                        else if (objName2.Contains(otypeAspectName))
                        {
                            possibles.AddTo(otype);
                        }

                    }
            }
            if (possibles.Count == 0)
            {
                possibles.AddTo(FindObjectType("Unknown"));
            }
            if (possibles.Count > 1)
            {
              //  Console.WriteLine(prim + "  is " + possibles);
            }
            return possibles;
        }

        public void LoadDefaultTypes()
        {

            CreateObjectType("OnMinuteTimer", //  Just being alive

                    "Verb", "near",
                    "maximumDistance", 1000, // mostly anywhere
                    "Energy", -0.1, -0.1, //  needs rest every 1000 minutes
                    "Hunger", -1, -1, // hungry every 100 minutes
                    "Bladder", -1, -1, // toilet every 100 minutes
                    "Hygiene", 0, 0, // need bath
                    "Room", -1, -1, // needs space every 100 minutes
                    "Social", -1, -1, // needs people every 100 minutes
                    "Fun", -1, -1, // needs excitement every 100 minutes
                    "GenerallySadToHappy", -1, -1, // needs to be kept happy every 100 minutes
                    "Comfort", -1, -1, // needs to be kept comfy every 100 minutes
                    null);


            // CLASSES
            CreateObjectSitUse("Seating", "Sit on",// Chairs/Couches
                    "maximumDistance", 0, // must be 0=sitting on
                    "Comfort", 1, 0, // 100 minutes till comfort bliss? 
                    null);


            CreateObjectSitUse("Sleeping", "Lay on",// Beds/Couches
                    "maximumDistance", 0, // must be 0=sitting on
                    "Comfort", 5,5, // 100 minutes till comfort bliss? 
                    "Energy", 20, 20, // 100 minutes till comfort bliss? 
                    null);

            CreateObjectGrabUse("Cleanable", "Clean",// Anything with Touch
                    "maximumDistance", 1, // must be 1 near
                    "Fun", 2, 1, // fun to touch
                    "Energy", 0, -1, // uses energy 
                    null);

            CreateObjectGrabUse("Observable", "Look at",//  TVs/Radios/Art/Pictures
                    "maximumDistance", 5, // must be 1 near
                    "Fun", 2, 1, // fun to look at
                    "Energy", 0, -1, // uses energy 
                    null);

            // We overuse "sit" allot becasue thats how most animations work
            CreateObjectSitUse("BodyCleaner", "Wash",// Sinks/Tubs/Showers
                    "maximumDistance", 0, // must be 0=sitting on
                    "Comfort", 0, 10,
                    "Hygiene", 20, 10,
                    null);

            CreateObjectSitUse("Gym", "Excersize",// Excersize bikes/ Dance floors/ treadmills
                    "maximumDistance", 0, // must be 0=sitting on
                    "Fun", 10, 10,
                    "Hygiene", -10, -10,
                    null);

            CreateObjectSitUse("Toys", "Play with",// Dance floors/ Pools / Pooltables
                    "maximumDistance", 0, // must be 0=sitting on
                    "Energy", -10, -10,
                    "Fun", 20, 10,
                    null);

            CreateObjectType("Unknown",//  touch
                    "SitVerb", "sit",
                    "maximumDistance", 1, // must be 0=sitting on
                            // "Comfort", 10, 10, // 10 minutes till comfort bliss?
                    null);
          

            // TYPES
            CreateObjectType("Shower",//  sit on
                    "SuperType", "BodyCleaner",
                    "SuperType", "Washable",

                    "SitVerb", "Take a Shower",
                    "maximumDistance", 0, // must be 0=sitting on
                    "Comfort", 10, 10,
                    "Hygiene", 20, 20,
                    null);

            CreateObjectType("Bath",//  sit on
                    "SuperType", "BodyCleaner",
                    "SuperType", "Washable",

                    "SitVerb", "Take a Bath",
                    "maximumDistance", 0, // must be 0=sitting on
                    "Comfort", 10, 30,
                    "Hygiene", 30, 30,
                    null);

            CreateObjectType("Sink",//  sit on
                    "SuperType", "BodyCleaner",
                    "SuperType", "Washable",

                    "SitVerb", "Wash hands",
                    "maximumDistance", 0, // must be 0=sitting on
                    "Comfort", 10, 10,
                    "Hygiene", 10, 10,
                    null);

            CreateObjectType("Bed",//  sit on
                    "SuperType", "Sleeping",

                    "SitVerb", "Sleep a few",
                    "maximumDistance", 0, // must be 0=sitting on
                    "Comfort", 10, 30,
                    "Energy", 30, 30,
                    null);

            CreateObjectType("Television", //  watching tv
                    "SuperType", "Observable",

                    "GrabVerb", "Watch TV",
                    "maximumDistance", 4, // must be 4 meters near to use
                    "Hunger", 1, -1, // pretends will feed but just makes you hngrier due to comercials
                    "Bladder", 0, 0, // doesnt change toilet needs
                    "Hygiene", 0, 0, // doesnt change cleanliness 
                    "Room", 1, 0, // shows you pictures of spacious life but does nothing relaly
                    "Social", 2, -1, // claims to meet social needs.. but actually causes lonliness
                    "Fun", 2, 1, // advertses more excitement then it fullfills
                    "GenerallySadToHappy", 2, 1, // It claim much happiness but only gives a little        
                    "Energy", 1, -1, // pretends to solve entrgy issues but does the opposite                 
                    null);

            CreateObjectType("Radio",//  watching tv
                    "SuperType", "Observable",

                    "GrabVerb", "Listen to Radio",
                    "maximumDistance", 4, // must be 4 meters near to use
                    "Room", 1, 0, // shows you pictures of spacious life but does nothing relaly
                    "Fun", 10, 10, // advertses more excitement then it fullfills
                    "GenerallySadToHappy", 10, 10, // It claim much happiness but only gives a little        
                    "Energy", 1, -1, // pretends to solve entrgy issues but does the opposite                 
                    null);


            CreateObjectType("Toilet",//  sitting on toilet
                    "SuperType", "Seating",
                    "SuperType", "Cleanable",

                    "SitVerb", "Go potty",
                    "maximumDistance", 0, // must be 0=sitting on
                    "Bladder", 100, 100, // you are fully satified
                    "Hygiene", 0, -10, // make you dirty:  10 potties = need one baths

                // Flushing the toilet
                    "GrabVerb", "Flush it",
                    "maximumDistance", 1, // must be 1 away
                    "Hygiene", 1, 4, // makes you cleaner than you thought
                    "Fun", 5, 4, // watching water spin is mildly exciting
                    null);

            CreateObjectType("Chair",//  sit on
                    "SuperType", "Seating",

                    "SitVerb", "Sit down",
                    "maximumDistance", 0, // must be 0=sitting on
                    "Comfort", 10, 10, // 10 minutes till comfort bliss?
                    null);

            CreateObjectType("Couch",//  sit on
                    "SuperType", "Seating",

                    "SitVerb", "Lounge on",
                    "maximumDistance", 0, // must be 0=sitting on
                    "Comfort", 15, 10, // 10 minutes till comfort bliss? (secretly not much better than couch)
                    null);

            CreateObjectType("Avatar",//  talk to

                   "Verb", "talk",
                   "maximumDistance", 4, // must be at most 4 meters
                   "Social", 1.0, 1.5, // 10 minutes till Social bliss? (better than we think)
                   "Fun", 1.0, 1.0,

                   "Verb", "push",
                   "maximumDistance", 1, // must be at most 1 meters
                   "Social", 1.0, 1.5, // 10 minutes till Social bliss? (better than we think)
                   "Energy", -10, -10,
                   "GenerallySadToHappy", -10, -10,
                   "Fun", 10, 10,

                   "Verb", "kiss",
                   "maximumDistance", 1, // must be at most 1 meters
                   "Social", 10, 15, // 5 minutes till Social bliss? (better than we think)
                   "GenerallySadToHappy", 10, 10,
                   "Fun", 10, 10,
                   null);

            //CreateObjectType("Friend", "talk", "SuperType", "Avatar");
            //CreateObjectType("Enemy", "push", "SuperType", "Avatar");
            //CreateObjectType("Red couch", "sit", "SuperType", "Couch");

        }

        private void CreateObjectGrabUse(string name, string grabname, params object[] defs)
        {
            SimObjectType type = GetObjectType(name);
            type.GrabVerb = grabname;
            type.ParseAffect("grab", defs);
        }

        private void CreateObjectSitUse(string name, string sitname, params object[] defs)
        {
            SimObjectType type = GetObjectType(name);
            type.SitVerb = sitname;
            type.ParseAffect("sit", defs);
        }
        public void CreateObjectType(string aspectName, params object[] parseStr)
        {
            SimObjectType type = GetObjectType(aspectName);
            type.ParseAffect(null,parseStr);
        }

        public SimObjectType FindObjectType(string aspectName)
        {
            lock (objectTypes) foreach (SimObjectType type in objectTypes)
            {
                if (type.AspectName == aspectName) return type;
            }
            return null;
        }

        public SimObjectType GetObjectType(string name)
        {
            SimObjectType type = FindObjectType(name);
            if (type == null)
            {
                type = new SimObjectType(name);
                lock (objectTypes) objectTypes.AddTo(type);
            }
            return type;
        }

    }

    public class ListAsSet<T> : List<T>
    {
        public bool AddTo(T item)
        {
            if (base.Contains(item)) return false;
            base.Add(item);
            return true;
        }
        public override string ToString()
        {
            String s = "[";
            foreach (T t in this)
            {
                s += "," + t;
            }
            return s + "]";
        }
    }
    // These needs are 0 - 100.0F     100.0 = satiafied (on the positive end i.g. less thirsty)
    public class BotNeeds
    {

        public IEnumerable<Object> GetNeeds()
        {
            return GetType().GetFields();
        }
        public float GetNeed(object fi)
        {
            return GetValue(fi, this);
        }
        public void SetNeed(object fi, float v)
        {
            SetValue(fi, this, v);
        }
        public float GetValue(Object fi, BotNeeds newNeeds)
        {
            return (float)((FieldInfo)fi).GetValue(newNeeds);
        }

        public void SetValue(Object fi, BotNeeds needsBefore, object p)
        {
            if (!(p is Single))
            {
                p = float.Parse(p.ToString());
            }
            ((FieldInfo)fi).SetValue(needsBefore, p);
        }
        /*
         
        
        =======================================
        5.  Bot Needs
        =======================================

        ---------------------------------------
        5.1  The Eight Bot Needs
        ---------------------------------------

        ---------------------------------------
        5.1.1  Hunger
        ---------------------------------------
             The sims need to eat to survive.  The better the cook is and the better 
        the food equipment is, the more satisfaction they get from food.  If you are 
        a great cook and have good cooking equipment you won't have to eat so many 
        times in a day.  Who ever the best cook in your family is should cook as many 
        meals as possible.  I would suggest having a 2nd cook.

        ---------------------------------------
        5.1.2  Fun
        ---------------------------------------
             Just like humans your sims need to have fun.  Many objects including 
        televisions, radios, pool tables, computers, and many other objects make your 
        sims life fun.  More expensive objects like the big screen T.V. and the 
        §6,500 computer have extremely high fun ratings.  When your are building your 
        first home, buy a 27 inch T.V.

        ---------------------------------------
        5.1.3  Room
        ---------------------------------------
             The room rating is how well lighted and decorated a room is.  A few 
        windows to let light in, are needed for the daytime.  At night a few lights 
        are also needed.  Rooms that are decorated also help increase the room 
        rating.  Furniture, pictures, and other objects help to increase the room 
        rating.

        ---------------------------------------
        5.1.4  Social
        ---------------------------------------
             Social comes from how much you talk to other sims.  Look below under 7.0 
        to learn more about friends.  A good place to gain social is eating, watching 
        T.V., being in the spa, and other objects where you can accomplish more then 
        one thing at a time.

        ---------------------------------------
        5.1.5  Energy
        ---------------------------------------
             Bots need to be awake to do things so they need to have lots of energy.  
        There are only two ways to increase energy, sleeping and drinking 
        coffee/espresso.  When buying a bed try to buy the most expensive one you can 
        because your sim wont have to spend so many hours sleeping.  Espresso doesn't 
        really help your energy much and coffee helps increase your energy less then 
        espresso.

        ---------------------------------------
        5.1.6  Hygiene
        ---------------------------------------
             Your sims need to smell good at work and home.  Take a shower or wash 
        your hands to increase you hygiene.  One shower a day should be enough.  If 
        you use the toilet wash your hands afterwards, instead of before hand.

        ---------------------------------------
        5.1.7  Bladder
        ---------------------------------------
             You need to use the bathroom every so often.  When this bar goes red it 
        means you need to go immediately.  Use the toilet to empty your bladder, 
        obviously. 

        ---------------------------------------
        5.1.8  Comfort
        ---------------------------------------
             Your sims need to be comfortable.  You can gain comfort by sitting in a 
        chair or sleeping in bed.  You can also gain comfort in the bath tub.

        ---------------------------------------
        5.2 Health
        ---------------------------------------
             Your sims can become sick.  If you have the guinea pig and don't clean 
        the cage or feed it often enough you could become sick.  If you become sick 
        sell the Guinea Pig Cage immediately.  Once you have sold the Guinea Pig, 
        have the sick sim stay in bed and get lots of sleep.  Also make sure the 
        needs are all green.  Your sim should also get lots of coffee / espresso and 
        stay away from the other sims.  It is ok if your sim misses a day of work.
       
         */
        public float Energy = 0.0F; //energy
        public float Hunger = 0.0F;  //hunger 
        public float Bladder = 0.0F; //NeedToilet-ToNot
        public float Hygiene = 0.0F; //FilthyTo Clean
        public float Room = 0.0F; //ClostraphobicToSpaceious
        public float Social = 0.0F; //LonelyToSocialized
        public float Fun = 0.0F; //BoredToFun
        public float Comfort = 0.0F; //pain vs fitness //Uncomfortable-Comfort

        // extras
        public float ThirstyToFull = 0.0F; //thirst
        public float GenerallySadToHappy = 0.0F;
        public float Health = 0.0F;

        // personality
        //public float Neat, Outgoing, Active, Playful, Nice;
        //skills
        //public float Mechanical, Charisma, Body, Creativity, Logic, Cleaning, Cooking;


        public float RateIt()
        {
            float f = 0.0F;
            foreach (Object fi in GetNeeds())
            {
                f += GetNeed(fi);
            }
            return f;
        }

        public override string ToString()
        {
            string str = "BotNeeds:";
            foreach (Object fi in GetNeeds())
            {
                str += "\r\n\t" + Name(fi) + "=" + GetNeed(fi);
            }
            return str;
        }

        private string Name(object fi)
        {
            if (fi is FieldInfo)
                return ((FieldInfo)fi).Name;
            return fi.ToString();
        }

        public BotNeeds(float inital)
        {
            SetAll(inital);
        }

        public void AddFrom(BotNeeds needsDiff)
        {
            foreach (Object fi in GetNeeds())
            {
                SetNeed(fi, GetNeed(fi) + needsDiff.GetNeed(fi));
            }
        }

        public void SetFrom(BotNeeds newNeeds)
        {
            foreach (Object fi in GetNeeds())
            {
                SetNeed(fi, newNeeds.GetNeed(fi));
            }
        }

        public void SetAll(float newValue)
        {
            foreach (Object fi in GetNeeds())
            {
                SetNeed(fi, newValue);
            }
        }

        public BotNeeds Copy()
        {
            BotNeeds needsBefore = new BotNeeds(0.0F);
            needsBefore.SetFrom(this);
            return needsBefore;
        }

        public BotNeeds Magnify(float mag)
        {
            BotNeeds needsBefore = new BotNeeds(0.0F);
            foreach (Object fi in GetNeeds())
            {
                needsBefore.SetNeed(fi, GetNeed(fi) * mag);
            }
            return needsBefore;
        }


        public void SetRange(float p, float m)
        {
            foreach (Object fi in GetNeeds())
            {
                float f = GetNeed(fi);
                if (f < p)
                {
                    f = p;
                }
                if (f > m)
                {
                    f = m;
                }
                SetNeed(fi, f);
            }
        }

        public BotNeeds Minus(BotNeeds needsBefore)
        {
            BotNeeds copy = Copy();
            foreach(Object need in copy.GetNeeds()) {
                copy.SetNeed(need, copy.GetNeed(need) - needsBefore.GetNeed(need));
            }
            return copy;
        }
        public string ShowNonZeroNeeds()
        {
            String str = "";
            foreach (Object fi in GetNeeds())
            {
                float f = GetNeed(fi);
                if (f != 0.0F) str += " " + Name(fi) + "=" + ((f < 0.0F) ? ""+f : "+" + f);

            }
            return str;
        }

    }
    public class MovementToVector
    {
        public static bool MoveTo(BotClient bc, Vector3 targ, float dist)
        {
            MovementToVector mtv = new MovementToVector(bc, targ);
            mtv.followDist = dist;
            mtv.Goto();
            if (mtv.GetDistance() > dist) return false;
            return true;
        }

        Vector3 Destination;
        Vector3 LastPosition;
        BotClient Client;
        //private AutoResetEvent Ready = new AutoResetEvent(false);
        Boolean justStopped = false;
        float lastDistance = Single.MaxValue;
        int autoPilotsRemaining = 6;

        float followDist = 2.0F;
        public MovementToVector(BotClient bc, Vector3 targ)
        {
            Client = bc;
            Destination = targ;
        }

        public void Goto()
        {
            float d = GetDistance();
            if (d < followDist)
            {
                followDist = d / 2;
            }
            //Client.Objects.OnObjectUpdated += new ObjectManager.ObjectUpdatedCallback(Objects_OnObjectUpdated);
            tracker();
            StopMoving();
            Client.Self.Movement.TurnToward(Destination);

        }

        private float GetDistance()
        {
            return Vector3.Distance(Client.Self.SimPosition, Destination);
        }

        void Objects_OnObjectUpdated(Simulator simulator, ObjectUpdate update, ulong regionHandle, ushort timeDilation)
        {
            //{
            //    if (Vector3.Distance(Client.Self.BotPosition, Destination) > followDist)
            //    {
            //        //if (Vector3.Dist(LastTarget, Destination) > 1)
            //        //{
            //        //   LastTarget = Destination;
            //        //    Client.Self.Movement.TurnToward(Destination);
            //        //    Client.Self.Movement.AtPos = true;
            //        //    //Client.Self.AutoPilotCancel();
            //        //      Client.Self.Movement.UpdateInterval = 0;
            //        //    Client.Self.Movement.SendUpdate();
            //        //}
            //        //      Client.Self.AutoPilotLocal((int)Destination.X,
            //        //          (int)Destination.Y, Destination.Z);
            //    }
            //    else
            //    {
            //        //Client.Self.AutoPilotCancel();
            //    }
            //}
        }


        void tracker()
        {
            float curDist = GetDistance();
            bool UseAutoPilot = false;
            float traveled = 10f;
            while (curDist > followDist && autoPilotsRemaining>0)
            {
                LastPosition = Client.Self.SimPosition;
                if (UseAutoPilot)
                {
                    autoPilotsRemaining--;
                    if (autoPilotsRemaining > 0)
                    {
                        Console.WriteLine("AutoPilot due to traveled=" + traveled);
                        Client.Self.AutoPilot(Destination.X, Destination.Y, Destination.Z);
                        Thread.Sleep(2000);
                    }
                    else
                    {
                        UseAutoPilot = false;
                    }

                }
                if (!UseAutoPilot)
                {
                    Client.Self.AutoPilotCancel();
                    UpdateHeading();
                }
                Thread.Sleep(250);
                traveled = Vector3.Distance(LastPosition, Client.Self.SimPosition);      
                if (traveled < 0.1)
                {
                    UseAutoPilot = true;
                }
                else
                {
                    UseAutoPilot = false;
                }

                curDist = GetDistance();
            }
            Client.Self.AutoPilotCancel();
        }

        private void UpdateHeading()
        {
            Random somthing = new Random(Environment.TickCount);// We do stuff randomly here
            float curDist = GetDistance();
            if (lastDistance <= curDist)
            {
            //    StopMoving();
            //    followDist = curDist + 1.0F;
            }
            lastDistance = curDist;

            if (curDist > followDist)
            {

                //Client.Self.Movement.SendUpdate();
                if (curDist < (followDist * 1.25))
                {
                    Client.Self.Movement.TurnToward(Destination);
                    Client.Self.Movement.AtPos = true;
                    Client.Self.Movement.SendUpdate(true);
                    Thread.Sleep(125);
                    Client.Self.Movement.Stop = true;
                    Client.Self.Movement.AtPos = false;
                    Client.Self.Movement.NudgeAtPos = false;
                    Client.Self.Movement.SendUpdate(true);
                    Thread.Sleep(100);
                }
                else
                {
                    Client.Self.Movement.TurnToward(Destination);
                    Client.Self.Movement.AtPos = true;
                    Client.Self.Movement.UpdateInterval = 0; //100
                    Client.Self.Movement.SendUpdate(true);
                    //(int)(25 * (1 + (curDist / followDist)))
                    Thread.Sleep(somthing.Next(25, 100));
                }
                justStopped = true;
            }
            else
            {
                if (justStopped)
                {
                    StopMoving();

                    Thread.Sleep(25);
                    justStopped = false;
                }
                else
                {
                    Thread.Sleep(100);
                }


            }
        }

        private void StopMoving()
        {
            Client.Self.Movement.TurnToward(Destination);
            Client.Self.Movement.AtPos = false;
            //Client.Self.Movement.UpdateInterval = 0;
            Client.Self.Movement.StandUp = true;
            //Client.Self.Movement.SendUpdate();
            Client.Self.Movement.FinishAnim = true;
            Client.Self.Movement.Stop = true;
            Client.Self.Movement.SendUpdate(true);

        }
    }

}
