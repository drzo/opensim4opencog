using System;
using System.Collections.Generic;
using System.Text;
using System.Reflection;
using cogbot.Listeners;
using OpenMetaverse;
using System.IO;
using DotLisp;
using System.Collections;

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
        public static SimObjectType UNKNOWN
        {
            get { return GetObjectType("Unknown"); } 
        }

        public SimObjectType IsSubType(SimObjectType superType)
        {
            if (superType == this) return this;
            foreach (SimObjectType st in SuperTypes)
            {
                if (st == superType) return st;
                if (st.SuperTypes.Contains(superType)) return st;
                SimObjectType found = st.IsSubType(superType);
                if (found!=null) return found;
            }
            return null;
        }

        public String ToDebugString()
        {
            if (cons != null) return cons.ToString();
            String str = ToString() + "[";
            SuperTypes.ForEach(delegate(SimObjectType item)
            {
                str +=  item.ToString() + " ";
            });
            return str.Trim() + "]";
        }

        public string IsWearableOrPortable=null; // what bodypart to attach?  Book=LeftHand
        public string IsTransformedOnUse = null; // new type it converts to
        public List<string> StoreObjectType = new List<string>(); // new type it converts to
        public bool IsDestroyedOnUse;
        public List<string> Match = new List<string>();  // regexpr match
        public List<string> NoMatch = new List<string>(); // wont be if one of these matches
        public string SitName = null;
        public string TouchName = null;
        Cons cons;
        readonly Dictionary<string, SimTypeUsage> usageAffect = new Dictionary<string, SimTypeUsage>();
        // Object area effect
        readonly public ListAsSet<SimObjectType> SuperTypes = new ListAsSet<SimObjectType>();

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
                newUse.AppendUse(use);
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
                verbs.AddTo(key);
            }
            foreach (SimObjectType st in SuperTypes)
            {
                foreach (SimTypeUsage v in st.GetTypeUsages())
                {
                    verbs.AddTo(v.UsageName);
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
            if (use == null) return -100f;
            BotNeeds sat = GetUsagePromise(use.UsageName).Copy();
            sat.AddFrom(from);
            sat.SetRange(0.0F, 100.0F);
            return sat.Total();
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
                if (s == "SuperTypes") 
                    s = "SuperType";
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
                    SetValue(fi, type, parseStr[i++]);
                    continue;
                }

                fi = usage.GetType().GetField(s);
                if (fi != null)
                {
                    SetValue(fi,usage, parseStr[i++]);
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

        private void SetValue(FieldInfo fi, object o, object p)
        {
            Type ftype = fi.FieldType;
            if (ftype.IsInstanceOfType(p))
            {
                fi.SetValue(o, p);
                return;
            }
            if (ftype.IsAssignableFrom(typeof(string))) {
                if (p == null) fi.SetValue(o, null);
                else fi.SetValue(o, p.ToString());
                return;
            }
            if (typeof(IList).IsAssignableFrom(ftype))
            {
                object fv = fi.GetValue(o);
                if (fv == null)
                {
                    fv = ftype.GetConstructor(new Type[0]).Invoke(new object[0]);
                }
                ftype = fv.GetType().GetGenericArguments()[0];
                p = CastTo(ftype,p);
                ((IList)fv).Add(p);
                fi.SetValue(o, fv);
                return;
            }
            fi.SetValue(o, p);
        }

        private object CastTo(Type ftype, object p)
        {
            return p;
        }


        //public SimTypeUsage GetDefaultUsage()
        //{
        //    List<SimTypeUsage> usages = GetTypeUsages();
        //    if (usages.Count == 0) return null;
        //    int item = (new Random(DateTime.Now.Millisecond)).Next(0, usages.Count - 1);
        //    return usages[item];
        //}

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


        static public ListAsSet<SimObjectType> GuessSimObjectTypes(Primitive.ObjectProperties props)
        {
            ListAsSet<SimObjectType> possibles = new ListAsSet<SimObjectType>();
            if (props != null)
            {
                string objName = " "+ props.Name.ToLower() + " | " + props.Description.ToLower() + " ";
                lock (objectTypes) if (objName.Length > 3)
                    {

                    NextOType:
                        foreach (SimObjectType otype in objectTypes)
                        {
                            bool skipIt = false;
                            foreach (string smatch in otype.NoMatch)
                            { // NoMatch
                                String otypeAspectName = smatch.ToLower().Replace("*", "");
                                if (objName.Contains(otypeAspectName))
                                {
                                    skipIt = true;
                                    break;
                                    // continue NextOType;
                                }
                            }
                            if (!skipIt)
                                foreach (string smatch in otype.Match)
                                { // NoMatchOn
                                    String otypeAspectName = smatch.ToLower().Replace("*", "");
                                    if (objName.Contains(otypeAspectName))
                                    {
                                        possibles.AddTo(otype);
                                        SetNames(props, otype);
                                        break;
                                    }
                                }

                        }
                    }

            }
            return possibles;
        }

        //static public string GetPrimTypeName(Primitive target)
        //{
        //    if (target.PrimData.PCode == PCode.Prim)
        //        return target.PrimData.Type.ToString();
        //    return target.PrimData.PCode.ToString();

        //}

        static private void SetNames(Primitive.ObjectProperties props, SimObjectType otype)
        {
             //= prim.Properties;
            if (props != null)
            {
                Primitive prim = null;
                if (String.IsNullOrEmpty(props.SitName))
                {
                    props.SitName = otype.GetSitName();
                    if (!String.IsNullOrEmpty(props.SitName))
                    {
                      // Console.WriteLine("[TODO] SetSitName(" + prim + "," + otype.GetSitName());
                    }
                }
                if (String.IsNullOrEmpty(props.TouchName))
                {
                    props.TouchName = otype.GetTouchName();
                    if (!String.IsNullOrEmpty(props.TouchName))
                    {
                      //  Console.WriteLine("[TODO] SetTextName(" + prim + "," + otype.GetTouchName());
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
            FileInfo fi = new FileInfo("SimBots.ini");
            if (fi.Exists)
            {
                LoadConfig(fi.Name);
                return;
            }
            /*            
             Format of loader           
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
            
        }

        public static void LoadConfig(string filename)
        {
            System.IO.FileStream f = System.IO.File.OpenRead(filename);
            StreamReader r = new StreamReader(f);
            r.BaseStream.Seek(0, SeekOrigin.Begin);
            TextReader tr = r;
            Interpreter interp = new DotLisp.Interpreter();
            while (tr.Peek() != -1)
            {
                Object read = interp.Read(filename, tr);
                if (interp.Eof(read)) return;
                Cons cons = (Cons)read;
                SimObjectType type = LoadConfigCons(cons);
                type.cons = cons;
            }

        }

        public static SimObjectType LoadConfigCons(Cons cons)
        {
            Object first = Cons.First(cons);
            cons =(Cons) Cons.Rest(cons);
            first = ((Symbol)first).ToString();
            if (first.ToString().ToLower() == "createobjectuse")
            {
                Object second = Cons.First(cons);
                return CreateObjectUse(second.ToString(), ConsParams(Cons.Rest(cons)));
            }
            else
                if (first.ToString().ToLower() == "createobjecttype")
                {
                    Object second = Cons.First(cons);
                    return CreateObjectType(second.ToString(), ConsParams(Cons.Rest(cons)));
                }
                else
                {
                    return CreateObjectType(first.ToString(), ConsParams(cons));
                }
        }

        private static object[] ConsParams(object ocons)
        {
            Cons cons = (Cons)ocons;
            object[] consV = Cons.ToVector(cons);
            object[] o = new object[consV.Length];
            for (int i = 0; i < consV.Length; i++)
            {
                object v = consV[i];
                if (v is Cons)
                {
                    v = Cons.First((Cons)v).ToString().Substring(1);
                }

                String s = v.ToString();
                if (v is Symbol)
                {
                    v = s;
                }
                else if (v is IConvertible)
                {
                    //v = s;
                }
                else
                {
                 //   v = v;
                }
                if (s == "true") v = true;
                else
                    if (s == "false") v = false;
                o[i] = v;
            }
            return o;
        }

        static public SimObjectType CreateObjectUse(string classname, params object[] defs)
        {
            SimObjectType type = GetObjectType(classname);
            SimTypeUsage usage = type.CreateObjectUsage(classname);
            type.ParseAffect(usage, defs);
            return type;
        }

        static public SimObjectType CreateObjectType(string aspectName, params object[] parseStr)
        {
            SimObjectType type = GetObjectType(aspectName);
            type.ParseAffect(null, parseStr);
            type.ParseAffect(null, new object[] { "Match", "* " + aspectName + " *" });
            return type;
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

        internal static void ListTypes()
        {
            foreach (SimObjectType type in objectTypes)
            {
                Console.WriteLine();
                Console.WriteLine("\t" + type.ToDebugString());
                foreach (String key in type.usageAffect.Keys)
                {
                    Console.WriteLine("\t\t;;" + type.FindObjectUsage(key).ToDebugString());
                }
            }
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


        public float Total()
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
