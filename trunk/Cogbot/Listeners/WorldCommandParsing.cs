using System;
using System.Collections;
using System.Collections.Generic;
using cogbot.TheOpenSims;
using MushDLR223.ScriptEngines;
using MushDLR223.Utilities;
using OpenMetaverse;
using PathSystem3D.Navigation;

namespace cogbot.Listeners
{

    public partial class WorldObjects : AllEvents
    {

        public BotMentalAspect GetObject(string name)
        {
            SimObject prim;
            string[] splitted = Parser.ParseArguments(name);
            int argsUsed;
            if (tryGetPrim(splitted, out prim, out argsUsed))
            {
                return prim;
            }
            return null;
        }

        public bool tryGetPrim(string str, out SimObject prim)
        {
            int argsUsed;

            return tryGetPrim(Parser.ParseArguments(str), out prim, out argsUsed);
        }

        public bool tryGetPrim(string[] splitted, out SimObject prim, out int argsUsed)
        {
            if (splitted == null || splitted.Length == 0)
            {
                prim = null;
                argsUsed = 0;
                return false;
            }
            string name = splitted[0].Trim().Replace("  ", " ");
            uint pickNum = 0;
            UUID uuid;
            if (name.Contains("-") && UUID.TryParse(name, out uuid))
            {
                prim = GetSimObjectFromUUID(uuid);
                argsUsed = 1;
                return prim != null;
            }
            var resolve = name.ToLower();
            /*
            if (resolve.Equals("pointing") || resolve.Equals("it"))
            {
                argsUsed = 1;
                if (!client.TheRadegastInstance.State.IsPointing)
                {
                    prim = null;
                    return false;
                }
                prim = GetPrimitive(client.TheRadegastInstance.State.TargetID, null);
                return prim != null;
            }
             */
            if (resolve.StartsWith("primid"))
            {
                if (name.Length > 6)
                {
                    String s = name.Substring(6);
                    if (uint.TryParse(s, out pickNum))
                    {
                        prim = GetSimObject(pickNum, null);
                        if (prim != null)
                        {
                            argsUsed = 1;
                            return true;
                        }
                    }
                    pickNum = 0;
                }
            }
            if (name.StartsWith("@"))
            {
                int splitOffset = 0;
                name = name.Substring(1);
                if (name.Length == 0)
                {
                    splitOffset = 1;
                    splitted = Parser.SplitOff(splitted, 1);
                }
                else
                {
                    splitted[0] = name;
                }
                int argsUsed0;
                SimPosition position = GetVector(splitted, out argsUsed0);
                if (argsUsed0 == 0)
                {
                    argsUsed = splitOffset;
                    prim = null;
                    return false;
                }
                double dist;
                prim = GetSimObjectFromVector(position.GlobalPosition, out dist);
                argsUsed = argsUsed0 + splitOffset;
                return prim != null && (dist < 2);
            }

            prim = null;
            argsUsed = 0;
            //.resolvreturn false;
            return GetPrimMatches(splitted, name, out prim, pickNum, out argsUsed);
        }


        private bool GetPrimMatches(string[] splitted, string name, out SimObject prim, uint pickNum, out int argsUsed)
        {
            if (splitted.Length >= 2)
            {
                if (UInt32.TryParse(splitted[splitted.Length - 1], out pickNum))
                {
                    name = String.Join("*", splitted, 0, splitted.Length - 1);
                }
            }

            List<SimObject> matches = new List<SimObject>();
            if (m_TheSimAvatar != null)
            {
                List<SimObject> set = TheSimAvatar.GetKnownObjects().CopyOf();
                lock (set)
                    if (set.Count == 0)
                    {
                        TheSimAvatar.ScanNewObjects(5, 100, false);
                        set = TheSimAvatar.GetKnownObjects().CopyOf();
                    }
                lock (set)
                    foreach (SimObject obj in set)
                    {
                        if (obj.Matches(name))
                        {
                            matches.Add(obj);
                        }
                    }
            }
            if (matches.Count == 0)
            {
                matches.AddRange(GetAllSimObjects(name));
            }
            if (matches.Count == 0)
            {
                argsUsed = 0;
                prim = null;
                return false;
            }
            if (splitted.Length > 1 && uint.TryParse(splitted[1], out pickNum))
            {
            }
            if (matches.Count == 1)
            {
                if (pickNum == 0)
                {
                    argsUsed = 1;
                    pickNum = 1;
                }
                else
                {
                    argsUsed = 2;
                }
                prim = matches[0];
                return pickNum == 1 && prim != null;
            }
            bool retVal = false;

            if (m_TheSimAvatar != null) TheSimAvatar.SortByDistance(matches);
            if (pickNum != 0)
            {
                argsUsed = 2;
                prim = pickNum <= matches.Count ? matches[(int)pickNum - 1] : null;
                return prim != null;
            }
            WriteLine("Found " + matches.Count + " matches: ");
            int num = 0;
            prim = null;
            foreach (SimObject obj in matches)
            {
                num++;
                if (m_TheSimAvatar != null)
                    WriteLine(" " + num + ": " + obj + " " + TheSimAvatar.DistanceVectorString(obj));
                if (num == pickNum)
                {
                    prim = obj;
                    retVal = true;
                }
            }
            argsUsed = splitted.Length;
            if (!retVal)
            {
                WriteLine("Use '" + name + " ###'");
                argsUsed = 0;
            }
            return retVal;
        }

        public SimObject GetSimObjectS(string[] args, out int argsUsed)
        {
            List<SimObject> primitives = GetPrimitives(args, out argsUsed);
            if (primitives.Count != 1) return null;
            SimObject prim = primitives[0];
            if (prim != null) return prim;
            foreach (SimAvatar avatar in SimAvatars)
            {
                if (avatar.DebugInfo().Contains(args[0]))
                    return avatar;
            }
            return null;
        }

        public void updateNumberedAvatars(List<Avatar> avatars)
        {
            numberedAvatars.Clear();
            for (int i = 0; i < avatars.Count; ++i)
                numberedAvatars.Add(avatars[i].Name);
        }

        public bool tryGetAvatar(string name, out SimAvatar avatar)
        {
            avatar = null;
            SimObject prim;
            string[] splitted = Parser.ParseArguments(name);
            int argsUsed;
            if (!tryGetPrim(splitted, out prim, out argsUsed)) return false;
            if (prim is SimAvatar)
            {
                avatar = (SimAvatar)prim;
                return true;
            }
            return false;
        }

        public string getAvatarName(Avatar avatar)
        {
            string name = avatar.Name;
            for (int i = 0; i < numberedAvatars.Count; ++i)
                if (numberedAvatars[i] == name)
                    name = (i + 1) + ": " + name;
            return name;
        }

        public ListAsSet<SimObject> GetRelations(ICollection re)
        {
            var more = new ListAsSet<SimObject>();
            foreach (var OP in re)
            {
                SimObject O = AsSimObject(OP);
                if (O == null) continue;
                more.Add(O);
                AsPrimitives(more, O.Children);
                SimObject P = O.Parent;
                if (P != O)
                {
                    AsPrimitives(more, P.Children);
                }
            }
            return more;
        }

        private List<SimObject> GetParents(ICollection primitives)
        {
            var more = new List<SimObject>();
            foreach (var OP in primitives)
            {
                SimObject O = AsSimObject(OP);
                if (O == null) continue;
                SimObject P = O.Parent;
                if (P != O)
                {
                    if (!more.Contains(P))
                        more.Add(P);
                }
            }
            return more;
        }

        private List<SimObject> GetChildren(ICollection primitives)
        {
            var more = new List<SimObject>();
            foreach (var OP in primitives)
            {
                SimObject O = AsSimObject(OP);
                if (O == null) continue;
                AsPrimitives(more, O.Children);
            }
            return more;
        }

        private SimObject AsSimObject(object ov)
        {
            Object o = ov;
            SimObject oo = o as SimObject;
            if (oo != null)
            {
                return oo;
            }
            Primitive p = o as Primitive;
            if (p != null)
            {
                return GetSimObject(p);
            }

            if (o is UUID)
            {
                oo = GetSimObjectFromUUID((UUID) o);
                if (oo != null) return oo;
                o = o.ToString();
            }
            string s = o as string;
            if (!string.IsNullOrEmpty(s))
            {
                SimObject prim;
                int argsUsed;
                if (tryGetPrim(new string[]{s}, out prim, out argsUsed))
                {
                    return prim;
                }
            }
            return null;
        }

        public void AsPrimitives(ICollection<Primitive> prims, IEnumerable positions)
        {
            foreach (var o in positions)
            {
                Primitive p = o as Primitive;
                if (p != null)
                {
                    prims.Add(p);
                    continue;
                }
                SimObject oo = AsSimObject(o);
                if (oo == null) continue;
                p = oo.Prim;
                if (p != null) prims.Add(p);
            }
        }
        public void AsPrimitives(ICollection<SimObject> prims, IEnumerable positions)
        {
            foreach (var o in positions)
            {
                SimObject oo = AsSimObject(o);
                if (oo == null) continue;
                if (!prims.Contains(oo))
                    prims.Add(oo);
            }
        }

        public List<SimObject> GetPrimitives(string[] args, out int argsUsed)
        {
            var prims = new List<SimObject>();
            if (args.Length == 0)
            {
                argsUsed = 0;
                return prims;
            }
            ICollection filterSimObjects = FilterSimObjects(args, out argsUsed, SimObjects.CopyOf());

            AsPrimitives(prims, filterSimObjects);

            if (argsUsed >= args.Length) return prims;
            String arg0Lower = args[argsUsed].ToLower();

            if (arg0Lower == "and" || arg0Lower == ",")
            {
                argsUsed++;
                int moreUsed;
                var more = GetPrimitives(Parser.SplitOff(args, argsUsed), out moreUsed);
                argsUsed += moreUsed;
                AsPrimitives(prims, more);
            }
            return prims;
        }


        List<ICollectionProvider> simGroupProviders = new List<ICollectionProvider>();
        private DefaultWorldGroupProvider _defaultProvider;
        public void AddObjectGroup(string selecteditems, Func<IList> func)
        {
            _defaultProvider.AddObjectGroup(selecteditems, func);
        }
        public void AddGroupProvider(ICollectionProvider bot)
        {
            lock (simGroupProviders)
            {
                simGroupProviders.Add(bot);
            }
        }

        public ICollection ResolveCollection(string arg0Lower, out int argsUsed, ICollectionProvider skip)
        {
            lock (simGroupProviders)
            {
                foreach (var provider in simGroupProviders)
                {
                    if (skip == provider) continue;
                    ICollection v = provider.GetGroup(arg0Lower);
                    if (v == null) continue;
                    argsUsed = 1;
                    return v;
                }
            }
            argsUsed = 0;
            return null;
        }
        public List<SimObject> FilterSimObjects(string[] args, out int argsUsed, ListAsSet<SimObject> prims)
        {
            lock (prims) return FilterSimObjects(args, out argsUsed, prims.CopyOf());
        }
        public List<SimObject> FilterSimObjects(string[] args, out int argsUsed, List<SimObject> prims)
        {
            int consume = args.Length;
            if (consume == 0)
            {
                argsUsed = 0;
                return prims;
            }
            string arg0Lower = args[0].ToLower();
            // Terminals

            var v = ResolveCollection(arg0Lower, out argsUsed, null);

            if (v != null)
            {
                argsUsed = 1;
                prims.Clear();
                AsPrimitives(prims, v);
                return prims;
            }
            if (arg0Lower == "dist")
            {
                prims.Clear();
                double dist = TheSimAvatar.SightRange;
                int used = 1;
                if (double.TryParse(args[1], out dist))
                {
                    used++;
                }
                prims = GetNearByObjects(TheSimAvatar.GlobalPosition, TheSimAvatar, dist, false);
                argsUsed = used;
                return prims;
            }
            // filters
            if (arg0Lower == "family")
            {
                prims = FilterSimObjects(Parser.SplitOff(args, 1), out argsUsed, prims);
                argsUsed++;
                prims = GetRelations(prims).CopyOf();
            }
            else if (arg0Lower == "parentof")
            {
                prims = FilterSimObjects(Parser.SplitOff(args, 1), out argsUsed, prims);
                argsUsed++;
                prims = GetParents(prims);
            }
            else if (arg0Lower == "maxdist")
            {
                double dist = TheSimAvatar.SightRange;
                int used = 1;
                if (double.TryParse(args[1], out dist))
                {
                    used++;
                }
                prims = FilterSimObjects(Parser.SplitOff(args, used), out argsUsed, prims);
                int i = prims.RemoveAll(p => p.Distance(TheSimAvatar) > dist);
                argsUsed += used;
            }
            else if (arg0Lower == "mindist")
            {
                double dist = TheSimAvatar.SightRange;
                int used = 1;
                if (double.TryParse(args[1], out dist))
                {
                    used++;
                }
                prims = FilterSimObjects(Parser.SplitOff(args, used), out argsUsed, prims);
                prims.RemoveAll(p => p.Distance(TheSimAvatar) < dist);
                argsUsed += used;
            }
            else if (arg0Lower == "max")
            {
                int nth = 30;
                int used = 1;
                if (int.TryParse(args[1], out nth))
                {
                    used++;
                }
                prims = FilterSimObjects(Parser.SplitOff(args, used), out argsUsed, prims);
                if (prims.Count > nth)
                {
                    prims.RemoveRange(nth, prims.Count - nth);
                }
                argsUsed += used;
            }
            else if (arg0Lower == "nth")
            {
                int nth = 0;
                int used = 1;
                if (int.TryParse(args[1], out nth))
                {
                    used++;
                }
                prims = FilterSimObjects(Parser.SplitOff(args, used), out argsUsed, prims);
                List<SimObject> prims0 = new List<SimObject>();
                if (prims.Count >= nth)
                {
                    prims0.Add(prims[nth - 1]);
                }
                prims = prims0;
                argsUsed += used;
            }
            else if (arg0Lower == "matches")
            {
                string nth = args[1].ToLower();
                prims = FilterSimObjects(Parser.SplitOff(args, 2), out argsUsed, prims);
                argsUsed += 1;
                prims.RemoveAll(p => !SMatches(p, nth));
            }
            else if (arg0Lower == "!matches")
            {
                string nth = args[1].ToLower();
                prims = FilterSimObjects(Parser.SplitOff(args, 2), out argsUsed, prims);
                argsUsed += 1;
                prims.RemoveAll(p => SMatches(p, nth));
            }
            else if (arg0Lower == "childsof")
            {
                prims = FilterSimObjects(Parser.SplitOff(args, 1), out argsUsed, prims);
                argsUsed++;
                prims = GetChildren(prims);
            }
            else if (arg0Lower == "bydistance")
            {
                prims = FilterSimObjects(Parser.SplitOff(args, 1), out argsUsed, prims);
                argsUsed++;
                List<SimObject> objs = new List<SimObject>();
                AsPrimitives(objs, prims);
                objs.Sort(TheSimAvatar.CompareDistance);
                prims = objs;
            }
            else
            {
                SimObject prim;
                if (tryGetPrim(args, out prim, out argsUsed))
                {
                    return new List<SimObject> {prim};
                }
                else
                {
                    return new List<SimObject>();
                    prims = FilterSimObjects(Parser.SplitOff(args, 1), out argsUsed, prims);
                    argsUsed += 1;
                    prims.RemoveAll(p => !SMatches(p, arg0Lower));
                }
            }
            return prims;
        }

        public List<SimObject> GetAllSimObjects(string name)
        {
            List<SimObject> matches = new List<SimObject>();
            foreach (SimObject obj in GetAllSimObjects())
            {
                if (SMatches(obj, name))
                {
                    matches.Add(obj);
                }
            }
            matches.Sort(TheSimAvatar.CompareDistance);
            return matches;
        }

        private bool SMatches(object o, string name)
        {
            if (o == null || name == null) return false;
            String s = o.ToString();
            if (s == null) return false;
            s = s.ToLower();
            return s.Contains(name) || name.Contains(s);
        }


    }


}