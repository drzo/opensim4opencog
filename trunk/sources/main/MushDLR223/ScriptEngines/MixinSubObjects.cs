using System;
using System.Collections;
using System.Collections.Generic;
using System.Reflection;
using MushDLR223.Utilities;

namespace MushDLR223.ScriptEngines
{
    public delegate object StringArgParser(string[] args, out int argsUsed, Type convertTo);

    public class FilterSpecAttribute : Attribute
    {
        public bool LastArgIsSelf = false;

        public static List<T> ApplyFilter<T>(string[] args, out int argsUsed, StringArgParser changeType, List<T> current, object relativeTo, bool removeMatches, OutputDelegate warn, CompareTestChar compareTestChar)
        {
            if (current.Count == 0)
            {
                argsUsed = args.Length;
                return current;
            }
            var example = current[0];
            Type exampleType = example.GetType();
            string arg0Lower = args[0];
            bool negative = removeMatches;
            if (arg0Lower.StartsWith("!"))
            {
                negative = true;
                arg0Lower = arg0Lower.Substring(1);
            }
            int predAt = arg0Lower.IndexOfAny("<>=*".ToCharArray());
            char compareChar = '=';
            if (predAt > 1)
            {
                compareChar = arg0Lower[predAt];
                arg0Lower = arg0Lower.Substring(predAt);
            }
            var possible = FindFiltersForTarget(arg0Lower, example, relativeTo, warn);
            if (possible == null || possible.Count == 0)
            {
                throw new NotSupportedException(arg0Lower);
            }
            foreach (FilterMember member in possible)
            {
                return ApplyFilterMember(args, out argsUsed, changeType, current, relativeTo,
                                         removeMatches, warn, negative, compareChar, compareTestChar, member);
            }
            throw new NotSupportedException(arg0Lower);
        }

        public static List<T> ApplyFilterMember<T>(string[] args, out int argsUsed, StringArgParser changeType,  List<T> current, object relativeTo, bool removeMatches, OutputDelegate warn, bool negative, char compareChar, CompareTestChar compareObjects, FilterMember fmemb) {
            object arg1 = null;
            if (fmemb.CastArgTo != null)
            {
                int len1 = args.Length - 1;
                string[] destargs = new string[len1];
                Array.Copy(args, 1, destargs, 0, len1);
                int argsUsedLocal;
                arg1 = changeType(destargs, out argsUsedLocal, fmemb.CastArgTo);
                argsUsed = 1 + argsUsedLocal;
            }
            else
            {
                argsUsed = 1;
            }
            List<T> putInto = new List<T>();
            Action<T> WhenTrue;
            if (negative)
            {
                putInto.AddRange(current);
                WhenTrue = (o) => putInto.Remove(o);

            }
            else
            {
                WhenTrue = (o) => putInto.Add(o);
            }
            foreach (T i in current)
            {
                object posresult = fmemb.Function(i, arg1, relativeTo);
                if (fmemb.PreCompare)
                {
                    posresult = compareObjects(compareChar, posresult, arg1);
                }
                if (fmemb.IsCollectionType)
                {
                    if (typeof(T).IsInstanceOfType(posresult))
                    {
                        WhenTrue((T)posresult);
                        continue;
                    }
                    var ie = (IEnumerable)posresult;
                    foreach (var item in ie)
                    {
                        WhenTrue((T)item);
                    }
                    continue;
                }
                else
                {
                    if ((bool)posresult) WhenTrue(i);
                }
            }
            return putInto;
        }

        public static List<FilterMember> FindFiltersForTarget(string arg0Lower, object example, object relativeTo, OutputDelegate warn)
        {

            Type exampleType = example.GetType();
            Type relativeToType = relativeTo == null ? typeof(object) : relativeTo.GetType();
            if ((example is MixinSubObjects))
            {
                List<FilterMember> newPossible = new List<FilterMember>();
                MixinSubObjects mso = (MixinSubObjects)example;
                Type[] types = mso.GetMixedTypes();
                foreach (Type type in types)
                {
                    var possible = FindFiltersForExampleSubType(arg0Lower, exampleType, type, relativeToType, warn);
                    if (possible.Count > 0)
                    {
                        foreach (var p in possible)
                        {
                            FilterMember fm = p;
                            FilterMember member = p;
                            Type subtype = type;
                            fm.Function = (o, arg1, relTo) =>
                                          member.Function(((MixinSubObjects)o).GetInstance(subtype), arg1, relTo);
                            newPossible.Add(fm);
                        }
                    }
                }
                return newPossible;

            }
            return FindFiltersForExampleSubType(arg0Lower, exampleType, exampleType, relativeToType, warn);
        }
        public static List<FilterMember> FindFiltersForExampleSubType(string arg0Lower, Type exampleType, Type t, Type relativeToType, OutputDelegate warn)
        {
            if (arg0Lower != null) arg0Lower = arg0Lower.ToLower();
            var possible = FindFiltersForExampleSubType(arg0Lower, exampleType, t, relativeToType, warn);
            if (arg0Lower == null || possible.Count > 0 || (!arg0Lower.EndsWith("of") && arg0Lower.StartsWith("get")))
            {
                return possible;
            }
            if (arg0Lower.EndsWith("of"))
            {
                arg0Lower = arg0Lower.Substring(0, arg0Lower.Length - 2);
            }
            possible = FindFiltersForExampleSubType(arg0Lower, exampleType, t, relativeToType, warn);
            List<FilterMember> newPossible = new List<FilterMember>();
            foreach (var p in possible)
            {
                FilterMember fm = p;
                fm.IsOf = true;
                newPossible.Add(fm);
            }
            return possible;
        }
        public static List<FilterMember> FindAllFilters(string arg0Lower, Type exampleType, Type t, Type relativeToType, OutputDelegate warn)
        {
            MemberInfo membH = null;

            MemberInfo[] membs = null;
            bool findAll = arg0Lower == null;
            const BindingFlags seachFlags = BindingFlags.Instance | BindingFlags.Static | BindingFlags.Public;
            if (findAll)
            {
                membs = t.GetMembers(seachFlags);
            }
            else
            {
                membs = t.GetMember(arg0Lower, seachFlags | BindingFlags.IgnoreCase);
                if (membs.Length == 0)
                {
                    string es = arg0Lower;
                    bool changedFront = false;
                    string[] prefixs = new[] { "is", "get" };
                    foreach (var c in prefixs)
                    {
                        if (es.StartsWith(c)) es = es.Substring(c.Length);
                        changedFront = true;
                    }
                    if (!changedFront)
                    {
                        foreach (var c in prefixs)
                        {
                            if (!es.StartsWith(c))
                            {
                                membs = t.GetMember(c + es, seachFlags | BindingFlags.IgnoreCase);
                            }
                            if (membs.Length > 0) break;                           
                        }
                    }
                }
            }

            List<FilterMember> found = new List<FilterMember>();
            foreach (MemberInfo memb in membs)
            {
                FilterMember fmemb = new FilterMember {ReflectionMember = memb};
                membH = memb;
                FieldInfo fi = memb as FieldInfo;
                if (fi != null)
                {
                    bool fisStatic = fi.IsStatic;                    
                    bool fisBoolReturn = fi.FieldType == typeof (bool);
                    fmemb.ReturnType = fi.FieldType;
                    if (fisBoolReturn)
                    {
                        fmemb.IsCollectionType = false;
                        fmemb.CastArgTo = null;
                        fmemb.Function = delegate(object o, object arg1, object relativeTo)
                                             {
                                                 return fi.GetValue(fisStatic ? null : o);
                                             };
                        found.Add(fmemb);
                        continue;
                    }
                    fmemb.CastArgTo = fi.FieldType;
                    fmemb.PreCompare = true;
                    fmemb.Function = delegate(object o, object arg1, object relativeTo)
                                         {
                                             return fi.GetValue(fisStatic ? null : o);
                                         };
                    found.Add(fmemb);
                    continue;
                }
                MethodInfo mi = memb as MethodInfo;
                PropertyInfo pi = memb as PropertyInfo;
                if (pi != null) mi = pi.GetGetMethod();
                if (mi == null) continue;
                Type returnType = mi.ReturnType;
                if (returnType == typeof(void)) continue;               
                var ps = mi.GetParameters();
                bool isBoolReturn = returnType == typeof (bool);
                fmemb.ReturnType = returnType;
                bool isStatic = mi.IsStatic;
                Func<int, int> effectiveParamCount = i => isStatic ? i + 1 : i;
                int psl = ps.Length;
                if (psl > effectiveParamCount(2)) continue;
                bool useRelative = psl > effectiveParamCount(0) &&
                                   IsAssignableFrom(ps[psl - 1].ParameterType, relativeToType);
                    
                
                fmemb.RequiresRelativeContextObject = useRelative;
                bool isNonPredicate = IsAssignableFrom(exampleType, returnType) ||
                                      (!typeof (IConvertible).IsAssignableFrom(returnType)
                                       && typeof (IEnumerable).IsAssignableFrom(returnType));
                fmemb.IsCollectionType = isNonPredicate;
                if (isStatic)
                {
                    // static methods need at least on arg
                    if (psl < 1) continue;
                    if (!IsAssignableFrom(ps[0].ParameterType, t))
                    {
                        continue;
                    }
                }
                if (arg0Lower == null)
                {
                    string mname = memb.Name;
                    if (mname.StartsWith("Set") || mname.StartsWith("get_") || mname.StartsWith("set_"))
                    {
                        continue;
                    }
                }
                if (isNonPredicate)
                {
                    if (useRelative)
                    {
                        if (psl == effectiveParamCount(1))
                        {
                            fmemb.CastArgTo = null;
                            fmemb.Function = delegate(object o, object arg1, object relativeTo)
                                                 {
                                                     if (isStatic) return mi.Invoke(null, new[] {o, relativeTo});
                                                     return mi.Invoke(o, new[] {relativeTo});
                                                 };
                            found.Add(fmemb);
                            continue;
                        }
                        if (psl == effectiveParamCount(2))
                        {
                            fmemb.CastArgTo = ps[0].ParameterType;
                            fmemb.Function = delegate(object o, object arg1, object relativeTo)
                                                 {
                                                     if (isStatic) return mi.Invoke(null, new[] {o, arg1, relativeTo});
                                                     return mi.Invoke(o, new[] {arg1, relativeTo});
                                                 };
                            found.Add(fmemb);
                            continue;
                        }
                        continue;
                    }
                    if (psl == effectiveParamCount(1))
                    {
                        fmemb.CastArgTo = ps[0].ParameterType;
                        fmemb.Function = delegate(object o, object arg1, object relativeTo)
                                             {
                                                 if (isStatic) return mi.Invoke(null, new[] {o, arg1});
                                                 return mi.Invoke(o, new[] {arg1});
                                             };
                        found.Add(fmemb);
                        continue;
                    }
                    if (psl == effectiveParamCount(0))
                    {
                        fmemb.CastArgTo = null;
                        fmemb.Function = delegate(object o, object arg1, object relativeTo)
                                             {
                                                 if (isStatic) return mi.Invoke(null, new[] {o});
                                                 return mi.Invoke(o, null);
                                             };
                        found.Add(fmemb);
                        continue;
                    }
                    continue;
                }

                if (!isBoolReturn)
                {

                    if (useRelative)
                    {
                        fmemb.CastArgTo = returnType;
                        if (psl != effectiveParamCount(1)) continue;
                        fmemb.PreCompare = true;
                        fmemb.Function =
                            delegate(object o, object arg1, object relativeTo)
                                {
                                    return isStatic
                                                    ? mi.Invoke(null, new[] {o, relativeTo})
                                                    : mi.Invoke(o, new[] {relativeTo});
                                };
                        found.Add(fmemb);
                        continue;
                    }
                    if (psl != effectiveParamCount(0)) continue;
                    fmemb.CastArgTo = returnType;
                    fmemb.Function = delegate(object o, object arg1, object relativeTo)
                                         {
                                             return isStatic ? mi.Invoke(null, new[] {o}) : mi.Invoke(o, null);
                                         };
                    fmemb.PreCompare = true;
                    found.Add(fmemb);
                    continue;
                }
                if (useRelative)
                {
                    if (psl == effectiveParamCount(1))
                    {
                        fmemb.CastArgTo = null;
                        fmemb.Function = delegate(object o, object arg1, object relativeTo)
                                             {
                                                 if (isStatic) return mi.Invoke(null, new[] {o, relativeTo});
                                                 return mi.Invoke(o, new[] {relativeTo});
                                             };
                        found.Add(fmemb);
                        continue;
                    }
                    if (psl == effectiveParamCount(2))
                    {
                        fmemb.CastArgTo = ps[0].ParameterType;
                        fmemb.Function = delegate(object o, object arg1, object relativeTo)
                                             {
                                                 if (isStatic) return mi.Invoke(null, new[] {o, arg1, relativeTo});
                                                 return mi.Invoke(o, new[] {arg1, relativeTo});
                                             };
                        found.Add(fmemb);
                        continue;
                    }
                    continue;
                }
                if (psl == effectiveParamCount(0))
                {
                    fmemb.CastArgTo = null;
                    fmemb.Function = delegate(object o, object arg1, object relativeTo)
                                         {
                                             if (isStatic) return mi.Invoke(null, new[] {o});
                                             return mi.Invoke(o, null);
                                         };
                    found.Add(fmemb);
                    continue;
                }
                if (psl == effectiveParamCount(1))
                {
                    fmemb.CastArgTo = ps[0].ParameterType;
                    fmemb.Function = delegate(object o, object arg1, object relativeTo)
                                         {
                                             if (isStatic) return mi.Invoke(null, new[] {o, arg1});
                                             return mi.Invoke(o, new[] {arg1});

                                         };
                    found.Add(fmemb);
                    continue;
                }
                continue;
            }
            if (found.Count == 0)
            {
                warn("dont know how to handle: " + arg0Lower + " " + membH);
            }
            return found;
        }

        private static bool IsAssignableFrom(Type lastParam, Type relativeToType)
        {
            if (lastParam.IsAssignableFrom(relativeToType)) return true;
            if (relativeToType == typeof(object))
            {
                return false;
            }
            return relativeToType.IsAssignableFrom(lastParam);
        }

        public static List<FilterMember> GetFilters(Type type)
        {
            if (typeof (MixinSubObjects).IsAssignableFrom(type))
            {

            }
            return FindAllFilters(null, type, type, type, DLRConsole.DebugWriteLine);
        }
    }

    public delegate bool CompareTestChar(char compareChar, object posresult , object compareTo);

    public struct FilterMember
    {
        public FilterFunc Function;
        public Type CastArgTo;
        public MemberInfo ReflectionMember;
        public bool RequiresRelativeContextObject;
        public bool IsCollectionType;
        public Type ReturnType;
        public bool IsTargeted;
        public bool IsOf;
        public bool PreCompare;
    }

    public delegate object FilterFunc(object target, object arg, object compareTo);

    public interface DenotingAnotherType
    {
        Type ImplementationType { get; }
    }

    public class ConvertToAttribute : Attribute, DenotingAnotherType
    {
        #region Implementation of DenotingAnotherType
        public Type ImplementationType { get; set; }
        #endregion
    }

    public interface MixinSubObjects
    {
        Type[] GetMixedTypes();
        object GetInstance(Type subtype);
        T GetInstance<T>();
    }
}