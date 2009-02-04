using System;
using System.Collections.Generic;
using System.Text;
using System.Reflection;
using OpenMetaverse;
using DotLisp;
using System.Collections;
using System.IO;

namespace cogbot.TheOpenSims
{
    public class SimObjectType : BotMentalAspect
    {
        public static SimObjectType UNKNOWN
        {
            get { return SimTypeSystem.GetObjectType("Unknown"); }
        }

        // Attachments
        public List<string> StoreObjectType = new List<string>(); // types that can attach to it
        public List<string> AttachesTo = null; // what bodypart to attach?  Book=LeftHand

        // Clasification
        public List<string> Match = new List<string>();  // regexpr match
        public List<string> NoMatch = new List<string>(); // wont be if one of these matches

        // Defines Side-effects to change Prim in SL
        public string SitName = null;
        public string TouchName = null;

        // Configuration overrides
        public ListAsSet<string> SpecifiedProperties = new ListAsSet<string>();

        // Source for Load/Save 
        internal Object cons;

        // Uses for this type
        internal readonly Dictionary<string, SimTypeUsage> usageAffect = new Dictionary<string, SimTypeUsage>();

        // Superclasses
        readonly public ListAsSet<SimObjectType> SuperType = new ListAsSet<SimObjectType>();

        public SimObjectType IsSubType(SimObjectType superType)
        {
            if (superType == this) return this;
            foreach (SimObjectType st in SuperType)
            {
                if (st == superType) return st;
                if (st.SuperType.Contains(superType)) return st;
                SimObjectType found = st.IsSubType(superType);
                if (found != null) return found;
            }
            return null;
        }

        public String ToDebugString()
        {
            if (cons != null) return cons.ToString();
            String str = ToString() + "[";
            SuperType.ForEach(delegate(SimObjectType item)
            {
                str += item.ToString() + " ";
            });
            return str.Trim() + "]";
        }


        public SimObjectType(string name)
            : base(name)
        {
        }

        public SimTypeUsage FindObjectUsage(string usename)
        {

            List<SimTypeUsage> usages = new List<SimTypeUsage>();


            foreach (SimObjectType type in SuperType)
            {
                SimTypeUsage find = type.FindObjectUsage(usename);
                if (find != null)
                {
                    usages.Add(find);
                }
            }
            if (usageAffect.ContainsKey(usename))
                usages.Add(usageAffect[usename]);


            if (usages.Count == 0) return null;

            SimTypeUsage newUse = new SimTypeUsage(usename);

            foreach (SimTypeUsage use in usages)
            {
                newUse.OverrideProperties(use);
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
            foreach (SimObjectType st in SuperType)
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

                if (s == "SuperType")
                {
                    String arg = parseStr[i++].ToString();
                    SimObjectType test = SimTypeSystem.FindObjectType(arg);
                    if (test == null)
                    {
                        throw new Exception("unkown supertype " + arg + " for " + type);
                    }
                    SuperType.AddTo(test);
                    usage = type.CreateObjectUsage(arg);
                    continue;
                }
                if (s == "Verb")
                {
                    String arg = parseStr[i++].ToString();
                    // TODO make suree creation order inernalizes correctly
                    SimObjectType superType = SimTypeSystem.GetObjectType(arg);
                    SuperType.AddTo(superType);
                    superType.CreateObjectUsage(arg);
                    usage = type.CreateObjectUsage(arg);
                    continue;
                }
                // usage / distanceToExcite / etc
                FieldInfo fi = type.GetType().GetField(s);
                if (fi != null)
                {
                    type.SpecifiedProperties.AddTo(fi.Name);
                    SimTypeSystem.SetValue(fi, type, parseStr[i++]);
                    continue;
                }

                fi = usage.GetType().GetField(s);
                if (fi != null)
                {
                    usage.SpecifiedProperties.AddTo(fi.Name);
                    SimTypeSystem.SetValue(fi, usage, parseStr[i++]);
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
                System.Console.WriteLine("ERROR: SimBots.ini-like dirrective unknown: '" + s + "'");
            }
        }

        public string GetTouchName()
        {
            if (!String.IsNullOrEmpty(TouchName)) return TouchName;
            SimObjectType pt = SuperType.Find(delegate(SimObjectType sc)
            {
                String tn = sc.GetTouchName();
                return (!String.IsNullOrEmpty(tn));
            });
            return pt == null ? TouchName : pt.GetTouchName();
        }

        public string GetSitName()
        {
            if (!String.IsNullOrEmpty(SitName)) return SitName;
            SimObjectType pt = SuperType.Find(delegate(SimObjectType sc)
            {
                String tn = sc.GetSitName();
                return (!String.IsNullOrEmpty(tn));
            });
            return pt == null ? SitName : pt.GetSitName();
        }
    }










    public class SimTypeSystem
    {

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
                string objName = " " + props.Name.ToLower() + " | " + props.Description.ToLower() + " ";
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
                //      Primitive prim = null;
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
            cons = (Cons)Cons.Rest(cons);
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
            lock (objectTypes) foreach (SimObjectType type in objectTypes)
                {
                    if (type.SuperType.Count == 0 && type.usageAffect.Count == 0) continue;
                    Console.WriteLine();
                    Console.WriteLine("\t" + type.ToDebugString());
                    foreach (String key in type.usageAffect.Keys)
                    {
                        Console.WriteLine("\t\t;;" + type.FindObjectUsage(key).ToDebugString());
                    }
                }
        }
        static public void SetValue(FieldInfo fi, object o, object p)
        {
            Type ftype = fi.FieldType;
            if (ftype.IsInstanceOfType(p))
            {
                fi.SetValue(o, p);
                return;
            }
            if (ftype.IsAssignableFrom(typeof(string)))
            {
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
                p = CastTo(ftype, p);
                ((IList)fv).Add(p);
                fi.SetValue(o, fv);
                return;
            }
            fi.SetValue(o, CastTo(ftype, p));
        }

        static public object CastTo(Type ftype, object p)
        {
            if (p is String)
            {
                if (p.ToString().ToLower().Equals("null"))
                {
                    return null;
                }
                if (ftype == typeof(SimObjectType))
                {
                    return CreateObjectType(p.ToString());
                }
                if (ftype == typeof(bool))
                {
                    return p.ToString().ToLower().StartsWith("t");
                }
                if (ftype == typeof(bool))
                {
                    return p.ToString().ToLower().StartsWith("t");
                }
            }
            return p;
        }
    }
}
