using System;
using System.Collections.Generic;
using System.Text;
using System.Reflection;
using cogbot.Listeners;
using OpenMetaverse;

namespace cogbot.TheOpenSims
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

    public class SimObjectType : BotMentalAspect
    {
        public String ToDebugString()
        {
            String str = ToString() + "[";
            SuperTypes.ForEach(delegate(SimObjectType item)
            {
                str +=  item.ToString() + " ";
            });
            return str.Trim() + "]";
        }

        public string SitName = null;
        public string TouchName = null;
        Dictionary<string, SimTypeUsage> usageAffect = new Dictionary<string, SimTypeUsage>();
        // Object area effect
        public ListAsSet<SimObjectType> SuperTypes = new ListAsSet<SimObjectType>();

        public SimObjectType(string name)
            : base(name)
        {
        }

        public SimTypeUsage FindObjectUsage(string usename)
        {

            List<SimTypeUsage> usages = new List<SimTypeUsage>();

            if (usageAffect.ContainsKey(usename))
                usages.Add(usageAffect[usename]);

            foreach (SimObjectType type in SuperTypes)
            {
                SimTypeUsage find = type.FindObjectUsage(usename);
                if (find != null)
                {
                    usages.Add(find);
                }
            }

            if (usages.Count == 0) return null;

            SimTypeUsage newUse = new SimTypeUsage(usename);

            foreach (SimTypeUsage use in usages)
            {
                if (String.IsNullOrEmpty(newUse.TextName))
                    newUse.TextName = use.TextName;
                if (use.UseGrabSpecified)
                    newUse.UseGrab = use.UseGrab;
                if (use.UseSitSpecified)
                    newUse.UseSit = use.UseSit;
                if (String.IsNullOrEmpty(newUse.LispScript))
                    newUse.LispScript = use.LispScript;
                if (String.IsNullOrEmpty(newUse.UseAnim))
                    newUse.UseAnim = use.UseAnim;
                newUse.ChangeActual = newUse.ChangeActual.Copy();
                newUse.ChangeActual.AddFrom(use.ChangeActual);
                newUse.ChangePromise = newUse.ChangePromise.Copy();
                newUse.ChangePromise.AddFrom(use.ChangePromise);
            }
            // maybe store for later
            //usageAffect[usename] = newUse;

            return newUse;
        }

        public SimTypeUsage CreateObjectUsage(string usename)
        {
            if (usageAffect.ContainsKey(usename))
                return usageAffect[usename];
            SimTypeUsage sou = new SimTypeUsage(usename);
            //  sou.TextName = usename;
            usageAffect[usename] = sou;
            return sou;
        }

        public ListAsSet<SimTypeUsage> GetTypeUsages()
        {
            ListAsSet<string> verbs = new ListAsSet<string>();
            foreach (string key in usageAffect.Keys)
            {
                if (!verbs.Contains(key))
                {
                    verbs.AddTo(key);
                }
            }
            foreach (SimObjectType st in SuperTypes)
            {
                foreach (SimTypeUsage v in st.GetTypeUsages())
                {
                    if (!verbs.Contains(v.UsageName))
                    {
                        verbs.AddTo(v.UsageName);
                    }
                }
            }
            ListAsSet<SimTypeUsage> usages = new ListAsSet<SimTypeUsage>();
            foreach (string st in verbs)
            {
                SimTypeUsage use = FindObjectUsage(st);
                use.ToString();
                usages.AddTo(use);
            }

            return usages;
        }

        public BotNeeds GetUsagePromise(string usename)
        {
            SimTypeUsage use = FindObjectUsage(usename);
            if (use == null) return BotNeeds.ZERO;
            return use.ChangePromise;

        }

        public float RateIt(BotNeeds from, SimTypeUsage use)
        {
            BotNeeds sat = GetUsagePromise(use.UsageName).Copy();
            sat.AddFrom(from);
            sat.SetRange(0.0F, 100.0F);
            return sat.RateIt();
        }

        public BotNeeds GetUsageActual(string usename)
        {
            SimTypeUsage use = FindObjectUsage(usename);
            if (use == null) return BotNeeds.ZERO;
            return use.ChangeActual;
        }


        public string GetTypeName()
        {
            return AspectName;
        }

        public void ParseAffect(SimTypeUsage usage, object[] parseStr)
        {
            SimObjectType type = this;
            int i = 0;

            while (i < parseStr.Length)
            {
                if (parseStr[i] == null)
                {
                    i++;
                    continue;
                }
                string s = (string)parseStr[i++];//.ToString();
                if (s == "SuperType")
                {
                    String arg = parseStr[i++].ToString();
                    SimObjectType test = FindObjectType(arg);
                    if (test == null)
                    {
                        throw new Exception("unkown supertype " + arg + " for " + type);
                    }
                    SuperTypes.AddTo(test);
                    usage = type.CreateObjectUsage(arg);
                    continue;
                }
                if (s == "Verb")
                {
                    String arg = parseStr[i++].ToString();
                    SimObjectType superType = GetObjectType(arg);
                    superType.CreateObjectUsage(arg);
                    SuperTypes.AddTo(superType);
                    usage = type.CreateObjectUsage(arg);
                    continue;
                }
                if (s == "SitName")
                {
                    s = parseStr[i++].ToString();
                    type.SitName = s;
                    usage.TextName = s;
  //                  usage.UseSit = true;
                    continue;
                }
                if (s == "TouchName")
                {
                    s = parseStr[i++].ToString();
                    type.TouchName = s;
                    usage.TextName = s;
   //                 usage.UseGrab = true;
                    continue;
                }
                if (s == "UseSit")
                {
                    usage.UseSitSpecified = true;
                }
                if (s == "UseGrab")
                {
                    usage.UseGrabSpecified = true;
                }

                // usage / distanceToExcite / etc
                FieldInfo fi = type.GetType().GetField(s);
                if (fi != null)
                {
                    if (fi.FieldType == typeof(String))
                    {
                        fi.SetValue(type, parseStr[i++].ToString());
                    }
                    else
                    {
                        fi.SetValue(type, parseStr[i++]);
                    }
                    continue;
                }

                fi = usage.GetType().GetField(s);
                if (fi != null)
                {
                    if (fi.FieldType == typeof(String))
                    {
                        fi.SetValue(usage, parseStr[i++].ToString());
                    }
                    else
                    {
                        fi.SetValue(usage, parseStr[i++]);
                    }
                    continue;
                }

                // Hygiene / Hunger
                fi = typeof(BotNeeds).GetField(s);
                if (fi != null)
                {
                    float ff = Single.Parse(parseStr[i++].ToString());
                    fi.SetValue(usage.ChangePromise, ff);
                    ff = Single.Parse(parseStr[i++].ToString());
                    fi.SetValue(usage.ChangeActual, ff);
                    continue;
                }
                System.Console.WriteLine("ERROR: MISSING " + s + " ... " + parseStr[i]);
            }
        }


        public SimTypeUsage GetDefaultUsage()
        {
            List<SimTypeUsage> usages = GetTypeUsages();
            if (usages.Count == 0) return null;
            int item = (new Random()).Next(0, usages.Count - 1);
            return usages[item];
        }

        public string GetTouchName()
        {
            if (!String.IsNullOrEmpty(TouchName)) return TouchName;
            SimObjectType pt = SuperTypes.Find(delegate(SimObjectType sc)
            {
                String tn = sc.GetTouchName();
                return (!String.IsNullOrEmpty(tn));
            });
            return pt == null ? TouchName : pt.GetTouchName();
        }

        public string GetSitName()
        {
            if (!String.IsNullOrEmpty(SitName)) return SitName;
            SimObjectType pt = SuperTypes.Find(delegate(SimObjectType sc)
            {
                String tn = sc.GetSitName();
                return (!String.IsNullOrEmpty(tn));
            });
            return pt == null ? SitName : pt.GetSitName();
        }



        /**
         * 
         * STATIC METHODS
         * 
         * 
         **/
        static ListAsSet<SimObjectType> objectTypes = new ListAsSet<SimObjectType>();


        static public ListAsSet<SimObjectType> GuessSimObjectTypes(Primitive prim)
        {
            ListAsSet<SimObjectType> possibles = new ListAsSet<SimObjectType>();
            SimObjectType type = null;

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
                                SetNames(prim, otype);
                            }
                            else if (objName2.Contains(otypeAspectName))
                            {
                                possibles.AddTo(otype);
                                SetNames(prim, otype);
                            }

                        }
            }
            type = FindObjectType(GetPrimTypeName(prim));
            if (type != null)
            {
                possibles.AddTo(type);
                SetNames(prim, type);

            }
            if (prim.Properties != null)
            {
                type = FindObjectType(prim.Properties.Name);
                if (type != null)
                {
                    possibles.AddTo(type);
                    SetNames(prim, type);
                }
                type = FindObjectType(prim.Properties.Description);
                if (type != null)
                {
                    possibles.AddTo(type);
                    SetNames(prim, type);
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

        static public string GetPrimTypeName(Primitive target)
        {
            if (target.PrimData.PCode == PCode.Prim)
                return target.PrimData.Type.ToString();
            return target.PrimData.PCode.ToString();

        }

        static private void SetNames(Primitive prim, SimObjectType otype)
        {
            if (prim.Properties != null)
            {
                if (String.IsNullOrEmpty(prim.Properties.SitName))
                {
                    prim.Properties.SitName = otype.GetSitName();
                    if (!String.IsNullOrEmpty(prim.Properties.SitName))
                    {
                        Console.WriteLine("[TODO] SetSitName(" + prim + "," + otype.GetSitName());
                    }
                }
                if (String.IsNullOrEmpty(prim.Properties.TouchName))
                {
                    prim.Properties.TouchName = otype.GetTouchName();
                    if (!String.IsNullOrEmpty(prim.Properties.TouchName))
                    {
                        Console.WriteLine("[TODO] SetTextName(" + prim + "," + otype.GetTouchName());
                    }
                }
            }
        }

        static bool IsInited = false;
        static object InitLock = new object();
        static public void LoadDefaultTypes()
        {
            lock (InitLock)
            {
                if (IsInited) return;
                LoadDefaultTypes0();
                IsInited = true;
            }

        }
        static public void LoadDefaultTypes0()
        {
            /*
            
             Format of loader
            
             * 
            
            
             */
            CreateObjectUse("OnMinuteTimer", //  Just being alive
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
            CreateObjectUse("Sittable",
                    "TextName", "Sit on",// Chairs/Couches
                    "maximumDistance", 1, // close enough?
                    "UseSit", true,
                    "UseAnim", Animations.SIT,
                    "Comfort", 1, 0, // 100 minutes till comfort bliss? 
                    null);

            CreateObjectUse("Sleepable",
                    "TextName", "Lay on",// Beds/Couches
                    "maximumDistance", 1, // close enough?
                    "UseSit", true,
                    "UseAnim", Animations.SLEEP,
                    "Comfort", 5, 5, // 100 minutes till comfort bliss? 
                    "Energy", 20, 20, // 100 minutes till comfort bliss? 
                    null);

            CreateObjectUse("Cleanable",
                    "TextName", "Clean",// Anything with Touch
                    "maximumDistance", 1, // must be 1 near
                    "UseAnim", Animations.FINGER_WAG,
                    "Fun", -2, 2, // fun to do but not to think about doing
                    "Energy", 0, -1, // uses energy 
                    null);

            CreateObjectUse("Observable",
                    "TextName", "Observe",//  TVs/Radios/Art/Pictures
                    "maximumDistance", 5, // must be 1 near
                    "UseAnim", Animations.CLAP,
                    "Fun", 2, 1, // fun to look at
                    "Energy", 0, -1, // uses energy 
                    null);

            // We overuse "sit" allot becasue thats how most animations work
            CreateObjectUse("BodyCleaner",
                    "TextName", "Wash",// Sinks/Tubs/Showers
                    "maximumDistance", 1, // close enough?
                    "UseAnim", Animations.RPS_PAPER,
                    "Comfort", 0, 10,
                    "Hygiene", 20, 10,
                    null);

            CreateObjectUse("Excersizable",
                    "TextName", "Excersize",// Excersize bikes/ Dance floors/ treadmills
                    "maximumDistance", 1, // close enough?
                    "UseAnim", Animations.ONETWO_PUNCH,
                    "Fun", 10, 10,
                    "Hygiene", -10, -10,
                    null);

            CreateObjectUse("Playable",
                    "TextName", "Play with",// Dance floors/ Pools / Pooltables
                    "maximumDistance", 1, // close enough?
                    "UseAnim", Animations.SHOOT_BOW_L,
                    "Energy", -10, -10,
                    "Fun", 20, 10,
                    null);


            CreateObjectUse("FoodStore",
                    "TextName", "Eat from",// Refrigerators and cupboards
                    "maximumDistance", 1, // close enough?
                    "UseAnim", Animations.DRINK,
                    "Hygiene", 0, -5, // should wash hands after
                    "Hunger", 40, 20, // fullfills some huger
                    null);

            CreateObjectUse("Fightable",
                    "TextName", "Beat up",// People
                    "maximumDistance", 1, // close enough?
                    "UseSit", false,
                    "UseAnim", Animations.ONETWO_PUNCH,
                    "Energy", -11, -20,
                    null);

            CreateObjectUse("Talkable",
                    "TextName", "Talk to",// People
                    "maximumDistance", 3, // close enough?
                    "UseSit", false,
                    "UseAnim", Animations.TALK,
                    "Social", 11, 20,
                    null);

            CreateObjectUse("Pushable",
                    "TextName", "Beat up",// People
                    "maximumDistance", 1, // close enough?
                    "UseSit", false,
                    "UseAnim", Animations.SWORD_STRIKE,
                    "Energy", -11, -20,
                    null);

            CreateObjectUse("Eatable",//  sit on
                    "TextName", "Eat it",
                    "maximumDistance", 1, // close enough?
                    "UseGrab", true,
                    null);

            CreateObjectUse("Kissable",
                    "TextName", "Kiss",// People
                    "maximumDistance", 1, // close enough?
                    "UseSit", false,
                    "UseAnim", Animations.BLOW_KISS,
                    "Social", 11, 20,
                    "Fun", 21, 20,
                    null);

            CreateObjectUse("Unknown",
                    "TextName", "Think about",
                    "maximumDistance", 1, // close enough?
                    "UseAnim", Animations.SHRUG,
                    null);


            // Body cleaning types
            CreateObjectType("Shower",//  What it is
                    "SuperType", "BodyCleaner", // Use as body cleaner
                    "UseAnim", Animations.KISS_MY_BUTT,
                    "TextName", "Take a Shower", // The name
                    "maximumDistance", 1, // must be near enouch
                    "Comfort", 10, 10, // showers little less than batch
                    "Hygiene", 30, 30,// showers little less than batch
                    "SuperType", "Cleanable", // allow object to be cleaned
                    null);

            CreateObjectType("Bath",//  What it is
                    "SuperType", "BodyCleaner", // Use as body cleaner
                    "TextName", "Take a Bath", // The name
                    "UseSit", true,
                    "maximumDistance", 1, // must be near enouch
                    "Comfort", 20, 20, // showers little less than batch
                    "Hygiene", 100, 100,// showers little less than batch
                    "SuperType", "Cleanable", // allow object to be cleaned
                    null);

            CreateObjectType("Sink",//  What it is
                    "SuperType", "BodyCleaner", // Use as body cleaner
                    "TextName", "Wash Hands", // The name
                    "maximumDistance", 1, // must be near enouch
                    "Comfort", 0, 0, // no comfort
                    "Hygiene", 10, 10,// provides some hygiene
                    "SuperType", "Cleanable", // allow object to be cleaned
                    null);

            // Lounging on types
            CreateObjectType("Bed",// Lay on
                    "SuperType", "Sleepable",
                    "SitName", "Sleep a few",
                    "UseSit", true, // for sleep scripts
                    "UseAnim", Animations.SLEEP, // look like sleeping
                    "maximumDistance", 1, // close enough?
                    "Comfort", 10, 30,
                    "Energy", 100, 80,
                    null);

            CreateObjectType("Chair",//  sit on
                    "SuperType", "Sittable",
                    "SitName", "Sit down",
                    "UseSit", true, // for sit scripts
                    "UseAnim", Animations.SMOKE_IDLE, // look like 
                    "maximumDistance", 1, // close enough?
                    "Comfort", 15, 10, // 10 minutes till comfort bliss? (secretly not much better than couch)
                    "Energy", 10, 20,
                    null);

            CreateObjectType("Couch",//  sit on
                    "SuperType", "Sittable",
                    "SitName", "Sit down",
                    "UseSit", true, // for sit scripts
                    "UseAnim", Animations.SMOKE_IDLE, // look like 
                    "maximumDistance", 1, // close enough?
                    "Comfort", 20, 20,
                    "Energy", 10, 20,
                    null);

            // Observable on types
            CreateObjectType("Television", //  watching tv
                    "SuperType", "Observable",

                    "TextName", "Watch TV",
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
                    "TextName", "Listen to Radio",
                    "maximumDistance", 4, // must be 4 meters near to use
                    "Room", 1, 0, // shows you pictures of spacious life but does nothing relaly
                    "Fun", 10, 10, // advertses more excitement then it fullfills
                    "GenerallySadToHappy", 10, 10, // It claim much happiness but only gives a little        
                    "Energy", 1, -1, // pretends to solve entrgy issues but does the opposite                 
                    null);


            CreateObjectType("Toilet",//  sitting on toilet
                    "SuperType", "Sittable",
                    "SitName", "Go potty",
                    "UseSit",true,
                    "maximumDistance", 1, // close enough?
                    "Bladder", 100, 100, // you are fully satified
                    "Hygiene", 0, -10, // make you dirty:  10 potties = need one baths

                // Flushing the toilet
                    "SuperType", "Cleanable",
                    "TextName", "Flush it",
                    "UseAnim", "POINT_YOU",
                    "UseSit", false,
                    "maximumDistance", 1, // must be 1 away
                    "Hygiene", 1, 4, // makes you cleaner than you thought
                    "Fun", 5, 4, // watching water spin is mildly exciting
                    null);

            CreateObjectType("Fridg",//  sit on
                    "SuperType", "FoodStore",
                    "TextName", "Raid the fridge",
                    "UseAnim",Animations.DRINK,
                    "UseGrab",true,
                    null);

            CreateObjectType("Treadmill",//  sit on
                    "SuperType", "Excersizable",
                    "TextName", "Tread the mill",
                    "UseSit",true,
                    null);

            CreateObjectType("Pooltable",//  sit on
                    "SuperType", "Playable",
                    "TextName", "Play pool",
                    "UseAnim", Animations.AIM_BAZOOKA_R,
                    null);

            CreateObjectType("Dance",//  sit on
                    "SuperType", "Playable",
                    "TextName", "Dance! Dance!",
                    "UseAnim", Animations.DANCE2,
                    null);

            CreateObjectType("Bread",//  sit on
                    "SuperType", "Eatable",
                    "TextName", "Eat the bread",
                    "UseAnim", Animations.DRINK,
                    "LispScript","(progn (TheBot.Eat Target))",
                    null);

            CreateObjectType("Avatar",//  talk to
                   "SuperType", "Talkable",
                   "maximumDistance", 3, // must be at most 3 meters
                   "Social", 10.0, 1.5, // 10 minutes till Social bliss? (better than we think)
                   "Fun", 1.0, 1.0,

                   "SuperType", "Fightable",
                   "maximumDistance", 1, // must be at most 1 meters
                   "Social", 10, 1.5, // 10 minutes till Social bliss? (better than we think)
                   "Energy", 0, -10,
                   "GenerallySadToHappy", 0, -10,
                   "Fun", 20, 10,

                   "SuperType", "Pushable",
                   "maximumDistance", 1, // must be at most 1 meters
                   "Social", 10, 1.5, // 10 minutes till Social bliss? (better than we think)
                   "Energy", 0, -10,
                   "GenerallySadToHappy", 0, -10,
                   "Fun", 20, 10,

                   "SuperType", "Kissable",
                   "maximumDistance", 1, // must be at most 1 meters
                   "Social", 10, 15, // 5 minutes till Social bliss? (better than we think)
                   "GenerallySadToHappy", 10, 10,
                   "Fun", 10, 10,
                   null);

            //CreateObjectType("Friend", "talk", "SuperType", "Avatar");
            //CreateObjectType("Enemy", "push", "SuperType", "Avatar");
            //CreateObjectType("Red couch", "sit", "SuperType", "Couch");

        }

        static void CreateObjectUse(string classname, params object[] defs)
        {
            SimObjectType type = GetObjectType(classname);
            SimTypeUsage usage = type.CreateObjectUsage(classname);
            type.ParseAffect(usage, defs);
        }

        static public void CreateObjectType(string aspectName, params object[] parseStr)
        {
            SimObjectType type = GetObjectType(aspectName);
            type.ParseAffect(null, parseStr);
        }

        static public SimObjectType FindObjectType(string aspectName)
        {
            lock (objectTypes) foreach (SimObjectType type in objectTypes)
                {
                    if (type.AspectName == aspectName) return type;
                }
            return null;
        }

        static public SimObjectType GetObjectType(string name)
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

    // These needs are 0 - 100.0F     100.0 = satiafied (on the positive end i.g. less thirsty)
    public class BotNeeds
    {

        static public BotNeeds ZERO
        {
            get { return new BotNeeds(0.0f); }
        }

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
            foreach (Object need in copy.GetNeeds())
            {
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
                if (f != 0.0F) str += " " + Name(fi) + "=" + ((f < 0.0F) ? "" + f : "+" + f);

            }
            return str;
        }
    }

}
