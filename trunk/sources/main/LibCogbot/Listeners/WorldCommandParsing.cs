using System;
using System.Collections;
using System.Collections.Generic;
using System.Reflection;
using Cogbot.World;
using MushDLR223.ScriptEngines;
using MushDLR223.Utilities;
using OpenMetaverse;
using PathSystem3D.Navigation;
using System.Drawing;

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
            if (splitted.Length == 0)
            {
                argsUsed = 0;
                return new List<SimObject>();
            }
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
            foreach (string primid in new[] {"primid", "lid"})
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

            if (name.StartsWith("$"))
            {
                return ResolveCollection(name, out argsUsed);
            }
            if (name.StartsWith("["))
            {
                var fnd = GetUnion(Parser.SplitOff(splitted, 1), out argsUsed);
                argsUsed++;
                if (argsUsed < splitted.Length)
                {
                    string t = splitted[argsUsed];
                    if (t == "]") argsUsed++;
                }
                return fnd;
            }
            if (name == "+")
            {
                var fnd = GetSingleArg(Parser.SplitOff(splitted, 1), out argsUsed);
                argsUsed++;
                return fnd;
            }
            if (name.StartsWith("("))
            {
                argsUsed = 1;
                var result = GridMaster.client.evalLispCode(name);
                var acol = SingleNameValue.AsCollection(result);
                if (acol == null)
                {
                    argsUsed = 0;
                }
                var fnd = new List<SimObject>();
                GridMaster.AsPrimitives(fnd, acol);
                return fnd;
            }

            int nth;
            if (int.TryParse(name, out nth))
            {
                nth--;
                splitted = Parser.SplitOff(splitted, 1);
                List<SimObject> fnd = null;
                if (splitted.Length == 0)
                {
                    argsUsed = 1;
                    fnd = SimRootObjects.CopyOf();
                }
                else
                {
                    fnd = GetSingleArg(splitted, out argsUsed);
                }
                if (fnd == null) return fnd;
                if (nth >= fnd.Count) nth = fnd.Count - 1;
                if (nth < 0) return null;
                if (m_TheSimAvatar != null) fnd.Sort(((SimObject) m_TheSimAvatar).CompareDistance);
                return new List<SimObject>() {fnd[nth]};
            }

            int len = splitted.Length;
            if (len > 1) len = 2;
            string largs = name.Replace("_", " ");
            if (largs.Contains(" ")) len = 1;
            argsUsed = 0;
            for (int i = len; i > 0; i--)
            {
                var fnd = new List<SimObject>();
                name = string.Join(" ", splitted, 0, i);
                foreach (var o in SimObjects)
                {
                    if (o.Matches(name))
                    {
                        fnd.Add(o);
                        argsUsed = i;
                    }
                }
                if (fnd.Count > 0) return fnd;
            }
            return null;
        }

        private List<SimObject> PrimOrNot(SimObject prim)
        {
            return prim == null ? null : new List<SimObject>() { prim };
        }

        public List<SimObject> GetPrimitives(string[] splitted, out int argsUsed)
        {
            List<string> missingOk = new List<string>();
            var prim = GetPrimitiveFromList(splitted, out argsUsed, missingOk);
            if (prim != null) return prim.CopyOf();
            return null;
        }

        public ListAsSet<SimObject> GetPrimitiveFromList(string[] args, out int argsUsed, List<string> missingOK)
        {
            ListAsSet<SimObject> allTargets = new ListAsSet<SimObject>();
            args = (string[])args.Clone();
            int argsUsedTotal = 0;
            while (args.Length > 0)
            {
                int argsUsed0;
                List<SimObject> PS = GetSingleArg(args, out argsUsed0);
                if (argsUsed0 == 0)
                {
                    missingOK.Add(args[0]);
                    argsUsed0 = 1;
                }
                if (argsUsed0 == 0)
                {
                    throw new ParserFilterFormatException("Cant GetSingleArg: ", args, 0);
                }
                argsUsedTotal += argsUsed0;
                if (PS != null) allTargets.AddRange(PS);
                args = Parser.SplitOff(args, argsUsed0);
            }
            argsUsed = argsUsedTotal;
            if (allTargets.Count == 0) return null;
            return allTargets;
        }

        public bool tryGetPrim(string[] splitted, out SimObject prim, out int argsUsed)
        {

            List<SimObject> getSingleArg = GetSingleArg(splitted, out argsUsed);
            int count = getSingleArg != null ? getSingleArg.Count : -1;
            if (count==1)
            {
                prim = getSingleArg[0];
                return true;
            }
            if (count > 1)
            {
                prim = getSingleArg[0];
                Debug("matched too many objects " + count);
                return true;
            }
            prim = null;
            return false;
        }

        public SimObject GetSimObjectS(string[] args, out int argsUsed)
        {
            SimObject prim;
            List<SimObject> primitives = GetSingleArg(args, out argsUsed);
            if (primitives != null)
            {
                if (primitives.Count != 1) return null;

                prim = primitives[0];
                if (prim != null) return prim;
            }
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
                oo = GetSimObjectFromUUID((UUID) o);
                if (oo != null) return oo;
                o = o.ToString();
            }
            string s = o as string;
            if (!string.IsNullOrEmpty(s))
            {
                SimObject prim;
                int argsUsed;
                if (tryGetPrim(new [] {s}, out prim, out argsUsed))
                {
                    return prim;
                }
            }
            return null;
        }

        public void AsPrimitives<T>(ICollection<T> prims, IEnumerable positions)
        {
            foreach (var o in positions)
            {
                T oo = (T) ConvertType(o, typeof(T));
                if (ReferenceEquals(oo, null)) continue;
                if (!prims.Contains(oo))
                    prims.Add(oo);
            }
        }

        public delegate T StringParserMethod<T>(string[] args, out int argsUsed);
        static Dictionary<string, List<SimObject>> GetPrimsCache = new Dictionary<string, List<SimObject>>();
        public List<SimObject> GetUnion(string[] args, out int argsUsed)
        {
            return StartGetUnionExprsn(args, out argsUsed);
            return WithCache(args, out argsUsed, GetPrimsCache, StartGetUnionExprsn);
        }
        public T WithCache<T>(string[] args, out int argsUsed, Dictionary<string, T> cache, StringParserMethod<T> stringParserMethod)
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
            var val = stringParserMethod(args, out argsUsed);
            if (argsUsed == 1)
            {
                lock (cache)
                {
                    cache[args[0]] = val;
                }
            }
            return val;
        }

        public List<SimObject> StartGetUnionExprsn(string[] args, out int argsUsed)
        {
            int argl = args.Length;
            if (argl == 0)
            {
                argsUsed = 0;
                return new List<SimObject>();
            }
            int argsUsedLocally;
            string arg0Lower = args[0].ToLower();
            List<SimObject> starterSet = null;

            starterSet = ColToList(ResolveGroupVar(arg0Lower));
            if (arg0Lower.StartsWith("$") && false)
            {
                argsUsed = 1;
                return starterSet;
            }
            argsUsedLocally = starterSet != null ? 1 : 0;
            if (argl == argsUsedLocally)
            {
                argsUsed = argl;
                return starterSet;
            }
            if (argsUsedLocally > 0)
            {
                args = Parser.SplitOff(args, argsUsedLocally);
            }
            else if (arg0Lower.StartsWith("+"))
            {
                starterSet = new List<SimObject>();
            }
            else
            {
                starterSet = SimRootObjects.CopyOf();
            }

            ICollection filterSimObjects = GetUnionExprsn(args, out argsUsed, starterSet, false, TheSimAvatar);
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
                var more = GetUnion(Parser.SplitOff(args, argsUsed), out moreUsed);
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
            var prim = GetPrimitiveFromList(splitted, out argsUsed, missingOk);
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
            return ColToList(starters);
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

        public List<SimObject> GetUnionExprsn(string[] args, out int argsUsed, List<SimObject> prims, bool removeMatches, MixinSubObjects relativeTo)
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

            int usedMore = 0;
            args = (string[])args.Clone();
            bool actuallyNegateNext = removeMatches;
            bool isIntersection = true;
            while (args.Length > 0)
            {
                if (args[0] == "]")
                {
                    usedMore++;
                    break;
                }
                int argsUsed0;
                bool negateNext;
                prims = GetUnionArg(args, out argsUsed0, prims, ref isIntersection,
                    actuallyNegateNext, relativeTo, out negateNext);
                if (argsUsed0 == 0)
                {
                    throw new ParserFilterFormatException("Cant GetSingleArg: ", args, 0);
                }
                usedMore += argsUsed0;
                actuallyNegateNext = removeMatches;
                if (negateNext) actuallyNegateNext = !actuallyNegateNext;
                args = Parser.SplitOff(args, argsUsed0);
            }
            argsUsed = usedMore;
            return prims;
        }

        public List<SimObject> GetUnionArg(string[] args, out int argsUsed, List<SimObject> prims,
            ref bool isIntersection,
            bool negated, MixinSubObjects relativeTo, out bool negateNext)
        {

            string arg0Lower = args[0].ToLower();

            negateNext = false;

            if (arg0Lower == "!")
            {
                negateNext = true;
                argsUsed = 1;
                return prims;
            }

            int used = 0;
            // Negation
            if (arg0Lower == "not")
            {
                prims = GetUnionExprsn(Parser.SplitOff(args, 1), out used, prims, !negated, relativeTo);
                argsUsed = 1 + used;
                return prims;
            }
            else if (arg0Lower == "or" || arg0Lower == "concat")
            {
                var secondSet = StartGetUnionExprsn(Parser.SplitOff(args, 1), out used);
                prims.AddRange(secondSet);
                argsUsed = used;
                return prims;
            }
            if (arg0Lower == "[")
            {
                var secondSet = StartGetUnionExprsn(Parser.SplitOff(args, 1), out used);
                argsUsed = 1 + used;
                return JoinLists(prims, negated, isIntersection, secondSet);
            }
            if (arg0Lower == "+")
            {
                var secondSet = GetSingleArg(Parser.SplitOff(args, 1), out used);
                argsUsed = 1 + used;
                return JoinLists(prims, negated, false, secondSet);
            }
            if (arg0Lower == "keep" || arg0Lower == "max")
            {
                int nth;
                used++;
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
                argsUsed = used;
                return prims;
            }
            else if (arg0Lower == "nth")
            {
                used++;
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
                argsUsed = used;
                return prims;
            }
            else if (arg0Lower == "bydist")
            {
                used++;
                List<SimObject> objs = new List<SimObject>();
                AsPrimitives(objs, prims);
                objs.Sort(((SimObject)relativeTo).CompareDistance);
                if (negated)
                {
                    objs.Reverse();
                }
                prims = objs;
                argsUsed = used;
                return prims;
            }
            else if (arg0Lower == "distfrom")
            {
                relativeTo = GetSimObjectS(Parser.SplitOff(args, 1), out used);
                used++;
                List<SimObject> objs = new List<SimObject>();
                AsPrimitives(objs, prims);
                objs.Sort(((SimObject)relativeTo).CompareDistance);
                if (negated)
                {
                    objs.Reverse();
                }
                prims = objs;
                argsUsed = used;
                return prims;
            }
            else if (arg0Lower == "reverse")
            {
                used++;
                prims.Reverse();
                argsUsed = used;
                return prims;
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
                    int usedF;
                    try
                    {
                        return FilterSpecAttribute.ApplyFilter(args, out argsUsed, ChangeType,
                                                                prims, relativeTo,
                                                                negated, Debug, CompareObjectsChar);
                    }
                    catch (ParserFilterFormatException pff)
                    {
                        missingFilter = pff;
                    }
                }
                List<SimObject> rcol = GetSingleArg(args, out argsUsed);
                if (rcol != null)
                {
                    return JoinLists(prims, negated, isIntersection, rcol);
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


        public void AddObjectGroup(string selecteditems,string desc, Func<IList> func)
        {
            _defaultProvider.AddObjectGroup(selecteditems, desc, func);
        }

        private List<SimObject> ColToList(ICollection starters)
        {
            if (starters == null) return null;
            var prims = new List<SimObject>();
            AsPrimitives(prims, starters);
            return prims;
        }

        public static List<T> JoinLists<T>(List<T> original, bool negated, bool isIntersection, List<T> withSet)
        {
            if (isIntersection)
            {
                foreach (T o in original.ToArray())
                {
                    if (negated == withSet.Contains(o)) original.Remove(o);
                }
                return original;
            }
            if (negated)
            {
                foreach (T o in withSet)
                {
                    original.Remove(o);
                }
                return original;
            }
            withSet.AddRange(original);
            return withSet;
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

        static public bool TryEnumParse(Type type, string[] names, int argStart, out int argsUsed, out object value)
        {
            ulong d = 0;
            argsUsed = 0;
            for (int i = argStart; i < names.Length; i++)
            {
                var name = names[i];

                Object e = null;
                try
                {
                    e = Enum.Parse(type, name);
                }
                catch (ArgumentException)
                {

                }
                if (e != null)
                {
                    d += (ulong)e.GetHashCode();
                    argsUsed++;
                    continue;
                }
                try
                {
                    e = Enum.Parse(type, name, true);
                }
                catch (ArgumentException)
                {

                }
                FieldInfo fi = type.GetField(name, BindingFlags.IgnoreCase | BindingFlags.Static);
                if (fi != null)
                {
                    try
                    {
                        e = fi.GetValue(null);
                    }
                    catch (ArgumentException)
                    {

                    }
                }

                if (e == null) foreach (MethodInfo info in type.GetMethods(BindingFlags.Static | BindingFlags.Public))
                {
                    if (info.ReturnType != type) continue;
                    var ps = info.GetParameters();
                    if (ps.Length != 1) continue;
                    if (ps[0].ParameterType != typeof (string)) continue;
                    try
                    {
                        e = info.Invoke(null, new[] {name});
                        if (e != null) break;
                    }
                    catch (Exception)
                    {

                    }
                }

                if (e != null)
                {
                    d += (ulong)e.GetHashCode();
                    argsUsed++;
                    continue;
                }
                ulong numd;
                if (UInt64.TryParse(name, out numd))
                {
                    d += numd;
                    argsUsed++;
                    continue;
                }
                break;
            }
            if (argsUsed == 0)
            {
                value = null;
                return false;
            }
            Type etype = Enum.GetUnderlyingType(type);
            if (typeof(IConvertible).IsAssignableFrom(etype))
            {
                MethodInfo mi = etype.GetMethod("Parse", new Type[] { typeof(string) });
                value = mi.Invoke(null, new object[] { d.ToString() });
                return argsUsed > 0;
            }
            value = d;
            return argsUsed > 0;
        }


        public bool UUIDTryParse(string[] args, int start, out UUID target, out int argsUsed)
        {
            args = Parser.SplitOff(args, start);

            if (args == null || args.Length == 0)
            {
                target = UUID.Zero;
                argsUsed = 0;
                return false;
            }
            string p = args[start];
            if (p.Contains("-") && UUID.TryParse(p, out target))
            {
                argsUsed = 1;
                return true;
            }
            List<SimObject> OS = GetSingleArg(args, out argsUsed);
            if (OS.Count == 1)
            {
                target = OS[0].ID;
                return true;
            }

            target = GetUserID(p);
            if (target != UUID.Zero)
            {
                argsUsed = 1;
                return true;
            }

            target = GetAssetUUID(p, AssetType.Unknown);
            if (target != UUID.Zero)
            {
                argsUsed = 1;
                return true;
            }
            argsUsed = 0;
            return false;
        }


        public Simulator TryGetSim(string[] args, out int argsUsed)
        {
            if (args.Length > 0)
            {
                string s = String.Join(" ", args);
                SimRegion R = SimRegion.GetRegion(s, client);
                if (R == null)
                {
                    argsUsed = 0;
                    WriteLine("cant find sim " + s);
                    return null;
                }

                Simulator sim = R.TheSimulator;
                if (sim == null) WriteLine("not connect to sim" + R);
                argsUsed = args.Length;
                return sim;
            }
            argsUsed = 0;
            return client.Network.CurrentSim;
        }

        private static object ConvertType(object o, Type type)
        {
            try
            {
                return ScriptManager.ChangeType(o, type);
            }
            catch
            {
                return null;
            }
        }

        public object ChangeType(string[] args, out int argsUsed, Type type)
        {
            var o = ChangeType0(args, out argsUsed, type);
            if (o != null) return o;
            o = ConvertType(args, type);
            if (o == null) return o;
            argsUsed = args.Length;
            return o;
        }

        private object TypeChangerProc(object value, Type to, out bool converted)
        {
            if (value == null)
            {
                converted = false;
                return null;
            }
            {
                var v2 = value;
                if (v2 is string[])
                {
                    string[] s2 = (string[]) v2;
                    if (s2.Length == 1)
                    {
                        v2 = s2[0];
                    }
                }
                var o = ConvertType0(v2, to);
                converted = to.IsInstanceOfType(o);
                if (converted) return o;
            }
            if (value is string)
            {
                value = new[] {(string) value};
            }
            if (value is string[])
            {
                int argsUsed;
                object o = ChangeType0((string[])value, out argsUsed, to);
                converted = to.IsInstanceOfType(o);
                return o;
            }
            converted = false;
            return null;
        }

        private object ConvertType0(object o, Type type)
        {
            if (type == typeof(TimeSpan))
            {
                double seconds;
                if (double.TryParse("" + o, out seconds))
                {
                    return TimeSpan.FromSeconds(seconds);
                }
            }
            if (FilterSpecAttribute.IsAssignableFrom(typeof(SimObject), type))
            {
                return AsSimObject(o);
            }
            if (FilterSpecAttribute.IsAssignableFrom(typeof(Primitive), type))
            {
                return AsSimObject(o).Prim;
            }
            if (FilterSpecAttribute.IsAssignableFrom(typeof(UUID), type))
            {
                return AsSimObject(o).ID;
            }
            if (o is IConvertible)
            {
                if (typeof(IConvertible).IsAssignableFrom(type))
                {
                    try
                    {
                        return Convert.ChangeType(o, type);
                    }
                    catch
                    {
                    }
                }
            }
            return ConfigSettingAttribute.FindValueOfType(o, type, 2);
        }
        public object ChangeType0(string[] args, out int argsUsed, Type arg2)
        {
            if (args.Length == 0)
            {
                argsUsed = 0;
                return null;
            }
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
            if (arg2.IsEnum)
            {
                object val;
                if (TryEnumParse(arg2, args, 0, out argsUsed, out val))
                {
                    return val;
                }
            }
            argsUsed = 1;
            return Convert.ChangeType(args[0], arg2);
        }
    }


}