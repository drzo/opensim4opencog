using System;
using System.Collections;
using System.Collections.Generic;
using System.Reflection;
using Cogbot.World;
using MushDLR223.ScriptEngines;
using MushDLR223.Utilities;
using OpenMetaverse;
using PathSystem3D.Navigation;

namespace Cogbot
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

        internal bool tryGetPrim(CmdRequest args, out SimObject prim, out int argsUsed)
        {
            return tryGetPrim(args.tokens, out prim, out argsUsed);
        }

        public List<SimObject> GetSingleArg(string[] splitted, out int argsUsed)
        {
            SimObject prim;
            UUID uuid = UUID.Zero;
            string name = splitted[0].Trim().Replace("  ", " ");
            if (name.IndexOf("-") > 2 && UUID.TryParse(name, out uuid))
            {
                prim = GetSimObjectFromUUID(uuid);
                argsUsed = 1;
                return PrimOrNot(prim);
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
            foreach (string primid in new[] { "primid", "lid" })
                if (resolve.StartsWith(primid))
                {
                    int tl = primid.Length;
                    if (name.Length > tl)
                    {
                        String s = name.Substring(tl);
                        uint localID;
                        if (uint.TryParse(s, out localID))
                        {
                            prim = GetSimObject(localID, null);
                            argsUsed = 1;
                            return PrimOrNot(prim);
                        }
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
                    return PrimOrNot(prim);
                }
                double dist;
                prim = GetSimObjectFromVector(position.GlobalPosition, out dist);
                argsUsed = argsUsed0 + splitOffset;
                if (dist > 2)
                {
                    prim = null;
                }
                return PrimOrNot(prim);
            }

            if (splitted.Length > 1 && !splitted[0].Contains(" "))
            {
                name = splitted[0] + " " + splitted[1];
                prim = GetSimAvatarFromNameIfKnown(name);
                if (prim != null)
                {
                    argsUsed = 2;
                    return PrimOrNot(prim);
                }
                prim = GetSimAvatarFromNameIfKnown(splitted[0]);
                if (prim != null)
                {
                    argsUsed = 2;
                    return PrimOrNot(prim);
                }
            }
            name = splitted[0];
            //if (splitted.Length > 1)
            {
                int useNth = 0;
                List<SimObject> fnd = null;
                int nth;
                if (int.TryParse(name, out nth))
                {
                    splitted = Parser.SplitOff(splitted, 1);
                    name = splitted[0];
                    useNth = 1;
                }
                else
                {
                    nth = 1;
                }
                if (name.StartsWith("$"))
                {
                    fnd = ResolveCollection(name, out argsUsed);
                }
                else if (name.StartsWith("["))
                {
                    fnd = GetPrimitivesExpression(splitted, out argsUsed);
                }
                else if (name.StartsWith("("))
                {
                    argsUsed = 1;
                    var result = GridMaster.client.evalLispCode(name);
                    var acol = SingleNameValue.AsCollection(result);
                    if (acol == null)
                    {
                        argsUsed = 0;
                    }
                    fnd = new List<SimObject>();
                    GridMaster.AsPrimitives(fnd, acol);
                }
                else if (name.StartsWith("@"))
                {
                    double near;
                    SimObject getSimObjectFromVector = GetSimObjectFromVector(
                        GridMaster.GetVector(splitted, out argsUsed).GlobalPosition, out near);
                    fnd = new List<SimObject>();
                    if (near > 0.3)
                    {
                        fnd.Add(getSimObjectFromVector);
                    }
                    else
                    {
                        argsUsed = 0;
                    }
                }
                else
                {
                    argsUsed = 0;
                    fnd = new List<SimObject>();
                    name = name.ToLower();
                    string largs = name.Replace("_", " ");
                    foreach (var o in SimObjects)
                    {
                        if (o.Matches(name))
                        {
                            fnd.Add(o);
                            argsUsed = 1;
                        }
                    }
                }
                if (useNth > 0)
                {
                    argsUsed += useNth;
                    prim = fnd[nth - 1];
                    return PrimOrNot(prim);
                }
                return fnd;
            }
            prim = null;
            argsUsed = 0;
            return PrimOrNot(prim);
        }

        private List<SimObject> PrimOrNot(SimObject prim)
        {
            return prim == null ? null : new List<SimObject>() { prim };
        }

        public List<SimObject> GetPrimitives(string[] splitted, out int argsUsed)
        {
            List<string> missingOk = new List<string>();
            argsUsed = 0;
            var prim = GetPrimitiveFromList(splitted, ref argsUsed, missingOk);
            if (prim != null) return prim.CopyOf();
            return null;
        }

        public ListAsSet<SimObject> GetPrimitiveFromList(string[] objects, ref int argsUsed, List<string> missingOK)
        {
            ListAsSet<SimObject> allTargets = new ListAsSet<SimObject>();
            objects = (string[])objects.Clone();
            while (objects.Length > 0)
            {
                int argsUsed0;
                List<SimObject> PS = GetSingleArg(objects, out argsUsed0);
                if (argsUsed0 == 0)
                {
                    missingOK.Add(objects[0]);
                    argsUsed0 = 1;
                }
                if (argsUsed0 == 0)
                {
                    throw new ParserFilterFormatException("Cant GetSingleArg: ", objects, 0);
                }
                argsUsed += argsUsed0;
                if (PS != null) allTargets.AddRange(PS);
                objects = Parser.SplitOff(objects, argsUsed0);
            }
            if (allTargets.Count == 0) return null;
            return allTargets;
        }

        public bool tryGetPrim(string[] splitted, out SimObject prim, out int argsUsed)
        {

            List<SimObject> getSingleArg = GetSingleArg(splitted, out argsUsed);

            if (getSingleArg != null && getSingleArg.Count == 1)
            {
                prim = getSingleArg[0];
                return true;
            }

            uint pickNum = 0;
            List<SimObject> matches = GetPrimitivesExpression(splitted, out argsUsed);

            if (matches.Count == 0)
            {
                argsUsed = 0;
                prim = null;
                return false;
            }
            string name = string.Join(" ", splitted, 0, argsUsed);
            if (splitted.Length > 1 && uint.TryParse(splitted[argsUsed], out pickNum))
            {
                // got our pickNum?!
                argsUsed++;
            }
            if (matches.Count == 1)
            {
                if (pickNum == 0)
                {
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
                WriteLine("Use '" + name + " bydist nth ###'");
                argsUsed = 0;
            }
            return retVal;
        }

        public SimObject GetSimObjectS(string[] args, out int argsUsed)
        {
            List<SimObject> primitives = GetSingleArg(args, out argsUsed);
            if (primitives.Count != 1) return null;
            SimObject prim = primitives[0];
            if (prim != null) return prim;

            prim = GetSimAvatarFromNameIfKnown(args[0]);
            argsUsed = prim != null ? 1 : 0;
            return prim;
        }

        public static SimPosition GetSimPositionByName(string arg)
        {
            int argsUsed;
            WorldObjects wo = WorldObjects.GridMaster;
            string[] args = Parser.Parse(arg);
            SimPosition prim = wo.GetSimObjectS(args, out argsUsed);
            if (prim != null) return prim;
            prim = wo.GetVector(args, out argsUsed);
            return prim;
        }


        public static SimObject GetSimAvatarFromNameIfKnown(string args)
        {
            args = args.ToLower();
            string largs = args.Replace("_", " ");
            foreach (SimAvatar avatar in SimAccounts)
            {
                string avatarDebugInfo = avatar.DebugInfo().ToLower();
                if (avatarDebugInfo.Contains(args) || avatarDebugInfo.Contains(largs))
                    return avatar;
            }
            return null;
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
                oo = GetSimObjectFromUUID((UUID)o);
                if (oo != null) return oo;
                o = o.ToString();
            }
            string s = o as string;
            if (!string.IsNullOrEmpty(s))
            {
                SimObject prim;
                int argsUsed;
                if (tryGetPrim(new string[] { s }, out prim, out argsUsed))
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

        public delegate T StringParserMethod<T>(string[] args, out int argsUsed);
        static Dictionary<string, List<SimObject>> GetPrimsCache = new Dictionary<string, List<SimObject>>();
        public List<SimObject> GetPrimitivesExpression(string[] args, out int argsUsed)
        {
            return WithCache(args, out argsUsed, GetPrimsCache, GetPrimitivesExpression0);
        }
        public T WithCache<T>(string[] args, out int argsUsed, Dictionary<string, T> cache, StringParserMethod<T> GetPrimitivesExpression0)
        {
            if (args.Length > 0)
            {
                T ret;

                lock (cache)
                {
                    if (cache.TryGetValue(args[0], out ret))
                    {
                        argsUsed = 1;
                        return (T)ret;
                    }
                }
            }
            var val = GetPrimitivesExpression0(args, out argsUsed);
            if (argsUsed == 1)
            {
                lock (cache)
                {
                    cache[args[0]] = val;
                }
            }
            return val;
        }

        public List<SimObject> GetPrimitivesExpression0(string[] args, out int argsUsed)
        {
            int argl = args.Length;
            if (argl == 0)
            {
                argsUsed = 0;
                return new List<SimObject>();
            }
            int argsUsedLocally;
            string arg0Lower = args[0].ToLower();

            List<SimObject> starterSet = ResolveCollection(arg0Lower, out argsUsedLocally);
            if (arg0Lower.StartsWith("$"))
            {
                argsUsed = argsUsedLocally;
                return starterSet;
            }
            if (arg0Lower == "[")
            {
                starterSet = GetPrimitivesExpression0(Parser.SplitOff(args, 1), out argsUsedLocally);
                argsUsed = argsUsedLocally + 1;
                return starterSet;
            }

            if (argl == argsUsedLocally)
            {
                argsUsed = argl;
                return starterSet;
            }
            if (argsUsedLocally > 0)
            {
                args = Parser.SplitOff(args, argsUsedLocally);
            }
            else
            {
                starterSet = SimRootObjects.CopyOf();
            }

            ICollection filterSimObjects = FilterSimObjects(args, out argsUsed, starterSet, false, TheSimAvatar);
            if (argsUsed == 0 && args.Length != 0)
            {
                // filter failed
                throw new ParserFilterFormatException(arg0Lower, args, 0);
            }

            var prims = new List<SimObject>();
            AsPrimitives(prims, filterSimObjects);

            if (argsUsed >= args.Length) return prims;

            if (args[argsUsed - 1] == "]") return prims;

            arg0Lower = args[argsUsed].ToLower();
            if (arg0Lower == "]") return prims;
            if (arg0Lower == "and" || arg0Lower == ",")
            {
                argsUsed++;
                int moreUsed;
                var more = GetPrimitivesExpression(Parser.SplitOff(args, argsUsed), out moreUsed);
                argsUsed += moreUsed;
                AsPrimitives(prims, more);
            }
            argsUsed += argsUsedLocally;
            return prims;
        }

        internal readonly Dictionary<string, IKeyValuePair<string, object>> ObjectGroups = new Dictionary<string, IKeyValuePair<string, object>>();


        private ICollection ResolveGroupVar(string name)
        {
            IKeyValuePair<string, object> func;
            if (ObjectGroups.TryGetValue(name.TrimStart(TrimCollectionStart), out func))
            {
                if (func == null) return null;
                return SingleNameValue.AsCollection(func.Value);
            }
            return null;
        }

        public ICollection ResolveForExternal(string name)
        {
            var expandGroupVar = ResolveGroupVar(name);
            if (expandGroupVar != null) return expandGroupVar;
            var splitted = Parser.ParseArguments(name);
            int argsUsed = 0;
            List<string> missingOk = new List<string>();
            var prim = GetPrimitiveFromList(splitted, ref argsUsed, missingOk);
            if (prim != null || missingOk.Count == 0)
            {
                if (argsUsed > 0)
                {
                    return SingleNameValue.AsCollection(prim);
                }
            }
            return null;
        }

        public List<SimObject> ResolveCollection(string arg0Lower, out int argsUsed)
        {
            ICollection starters = ResolveCollection(arg0Lower, out argsUsed, null);
            if (starters == null) return null;
            var prims = new List<SimObject>();
            AsPrimitives(prims, starters);
            return prims;
        }

        private readonly DefaultWorldGroupProvider _defaultProvider;
        static readonly char[] TrimCollectionStart = new[] { ' ', '\n', '$' };
        public ICollection ResolveCollection(string arg0Lower, out int argsUsed, ICollectionProvider skip)
        {
            arg0Lower = arg0Lower.TrimStart(TrimCollectionStart).ToLower();
            var expandGroupVar = ResolveGroupVar(arg0Lower);
            if (expandGroupVar != null)
            {
                argsUsed = 1;
                return expandGroupVar;
            }
            var req = new RequesterSession(client);
            //req.SkippedProviders.Add(skip);
            client.SessionMananger = req;
            var col = ScriptManager.GetGroup(client, client.GetName(), arg0Lower);
            if (col != null)
            {
                argsUsed = 1;
                return col;
            }
            argsUsed = 0;
            return null;
        }

        public List<SimObject> FilterSimObjects(string[] args, out int argsUsed, List<SimObject> prims, bool removeMatches, MixinSubObjects relativeTo)
        {
            int consume = args.Length;
            if (consume == 0)
            {
                argsUsed = 0;
                return prims;
            }
            string arg0Lower = args[0].ToLower();
            // terminal
            if (arg0Lower == "]")
            {
                argsUsed = 1;
                return prims;
            }
            int used = 1;
            // Negation
            if (arg0Lower == "not")
            {
                prims = FilterSimObjects(Parser.SplitOff(args, 1), out argsUsed, prims, !removeMatches, relativeTo);
                argsUsed += used;
                return prims;
            }
            else if (arg0Lower == "or" || arg0Lower == "concat")
            {
                var secondSet = GetPrimitivesExpression0(Parser.SplitOff(args, 1), out argsUsed);
                prims.AddRange(secondSet);
                argsUsed += used;
                return prims;
            }
            else if (arg0Lower == "[")
            {
                var secondSet = GetPrimitivesExpression0(args, out used);
                prims.AddRange(secondSet);
                prims = FilterSimObjects(Parser.SplitOff(args, used), out argsUsed, prims, removeMatches, relativeTo);
                argsUsed += used;
                return prims;
            }

            // locally negate
            bool negated = removeMatches;
            if (arg0Lower.StartsWith("!"))
            {
                negated = !negated;
            }

            if (arg0Lower.StartsWith("+"))
            {
                int skip = 0;
                if (arg0Lower.Length > 1)
                {
                    args[0] = args[0].Substring(1);
                }
                else
                {
                    skip = 1;
                }
                var list = GetSingleArg(Parser.SplitOff(args, skip), out used);
                prims.AddRange(list);
                used += skip;
                prims = FilterSimObjects(Parser.SplitOff(args, used), out argsUsed, prims, removeMatches, relativeTo);
                argsUsed += used;
                return prims;
            }

            if (arg0Lower == "keep" || arg0Lower == "max")
            {
                int nth;
                if (int.TryParse(args[1], out nth))
                {
                    used++;
                }
                int keep = Math.Abs(nth);
                if (keep > prims.Count) keep = prims.Count;
                if (negated) keep = prims.Count - keep;

                int removeCount = prims.Count - keep;

                if (nth > 0)
                {
                    // keep the first few
                    prims.RemoveRange(keep, removeCount);
                }
                else
                {
                    // keep the last few
                    prims.RemoveRange(0, removeCount);
                }

                prims = FilterSimObjects(Parser.SplitOff(args, used), out argsUsed, prims, removeMatches, relativeTo);
                argsUsed += used;
            }
            else if (arg0Lower == "nth")
            {
                int nth = 0;
                if (int.TryParse(args[1], out nth))
                {
                    used++;
                }
                if (!negated)
                {
                    List<SimObject> prims0 = new List<SimObject>();
                    if (prims.Count >= nth)
                    {
                        prims0.Add(prims[nth - 1]);
                    }
                    prims = prims0;
                }
                else
                {
                    prims = new List<SimObject>(prims);
                    prims.RemoveAt(nth);
                }
                prims = FilterSimObjects(Parser.SplitOff(args, used), out argsUsed, prims, removeMatches, relativeTo);
                argsUsed += used;
            }
            else if (arg0Lower == "bydist")
            {
                List<SimObject> objs = new List<SimObject>();
                AsPrimitives(objs, prims);
                objs.Sort(((SimObject)relativeTo).CompareDistance);
                if (negated)
                {
                    objs.Reverse();
                }
                prims = objs;
                prims = FilterSimObjects(Parser.SplitOff(args, 1), out argsUsed, prims, removeMatches, relativeTo);
                argsUsed += used;
            }
            else if (arg0Lower == "reverse")
            {
                prims.Reverse();
                prims = FilterSimObjects(Parser.SplitOff(args, 1), out argsUsed, prims, removeMatches, relativeTo);
                argsUsed += used;
            }
            else
            {
                bool nonFilter =
                    arg0Lower.StartsWith("$") ||
                    arg0Lower.StartsWith("@") ||
                    arg0Lower.StartsWith("primid") ||
                    arg0Lower.Substring(1).Contains("-");

                ParserFilterFormatException missingFilter = null;
                if (!nonFilter)
                {
                    // filters will re-namgate so use the incoming "removeMatches"
                    try
                    {
                        prims = FilterSpecAttribute.ApplyFilter(args, out used, ChangeType, prims, relativeTo,
                                                                removeMatches, Debug, CompareObjectsChar);
                        prims = FilterSimObjects(Parser.SplitOff(args, used),
                                                 out argsUsed, prims, removeMatches, relativeTo);
                        argsUsed += used;
                        return prims;
                    }
                    catch (ParserFilterFormatException pff)
                    {
                        used = 0;
                        missingFilter = pff;
                    }
                }
                List<SimObject> rcol = ResolveCollection(arg0Lower, out argsUsed);
                if (rcol != null)
                {
                    foreach (var o in prims.ToArray())
                    {
                        bool shared = rcol.Contains(o);
                        if (!negated && shared) continue;
                        prims.Remove(o);
                    }
                }
                else
                {
                    if (missingFilter != null)
                    {
                        argsUsed = 0;
                        // what saort of erro should we make?
                        Debug("no such filter or object: " + arg0Lower);
                        throw missingFilter;
                    }
                }
            }
            return prims;
        }

        private bool CompareObjectsChar(char comparechar, object posresult, object compareto)
        {
            throw new NotImplementedException();
        }

        public IEnumerable<FilterMember> GetFilters(Type type)
        {
            return FilterSpecAttribute.GetFilters(type);
        }

        public object ChangeType(string[] args, out int argsUsed, Type arg2)
        {
            if (typeof(SimObject).IsAssignableFrom(arg2))
            {
                return GetSimObjectS(args, out argsUsed);
            }
            if (typeof(UUID).IsAssignableFrom(arg2))
            {
                argsUsed = 1;
                UUID fnd = GetUserID(args[0]);
                if (fnd == UUID.Zero)
                {
                    return GetSimObjectS(args, out argsUsed).ID;
                }
                return fnd;
            }
            if (typeof(string) == arg2)
            {
                argsUsed = 1;
                return args[0];
            }
            argsUsed = 1;
            return Convert.ChangeType(args[0], arg2);
        }

        public void AddObjectGroup(string selecteditems, Func<IList> func)
        {
            _defaultProvider.AddObjectGroup(selecteditems, func);
        }
        /*
        public ICollection GetGroup(string name)
        {
            return GetGroup(name, this);
        }

        public void AddGroupProvider(ICollectionProvider bot)
        {
            lock (CollectionProviders)
            {
                CollectionProviders.Add(bot);
            }
        }

        public void AddGroupProvider(string name, GetGroupFunc bot)
        {
            lock (simGroupProviders)
            {
                simGroupProviders.Add(name, new GetGroupFuncHolder(name, bot));
            }
        }


        public ICollection GetGroup(string name, object skip)
        {
            lock (simGroupProviders)
            {
                foreach (ICollectionProvider provider in simGroupProviders)
                {
                    if (skip == provider) continue;
                    ICollection v = provider.GetGroup(name);
                    if (v == null) continue;
                    return v;
                }
            }
            return null;
        }*/
    }


}