using System;
using System.Collections;
using System.Collections.Generic;
using System.Reflection;

namespace MushDLR223.ScriptEngines
{
    public delegate object StringArgParser(string[] args, out int argsUsed, Type convertTo);

    public class FilterSpecAttribute : Attribute
    {
        public bool LastArgIsSelf = false;

        public static List<T> ApplyFilter<T>(string[] args, out int argsUsed, StringArgParser changeType,  List<T> current, object relativeTo, bool removeMatches, OutputDelegate warn)
        {
            if (current.Count == 0)
            {
                argsUsed = args.Length;
                return current;
            }
            var example = current[0];
            Type exampleType = example.GetType();
            Func<object, object, object> possible = null;
            bool functional;
            bool isCollectionGetter;
            Type castArgTo;
            string arg0Lower = args[0];
            bool negative = removeMatches;
            if (arg0Lower.StartsWith("!"))
            {
                negative = true;
                arg0Lower = arg0Lower.Substring(1);
            }
            possible = IsTrue(arg0Lower, example, relativeTo, warn, out castArgTo,
                                  out isCollectionGetter, out functional);
            object arg1 = null;
            if (castArgTo != null)
            {
                int len1 = args.Length - 1;
                string[] destargs = new string[len1];
                Array.Copy(args, 1, destargs, 0, len1);
                int argsUsedLocal;
                arg1 = changeType(destargs, out argsUsedLocal, castArgTo);
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
                object posresult = possible(i, arg1);
                if (isCollectionGetter)
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

        public static Func<object, object, object> IsTrue(string arg0Lower, object example, object relativeTo, OutputDelegate warn, out Type castArgTo, out bool isCollectionGetter, out bool functional)
        {
            Type exampleType = example.GetType();
            Func<object, object, object> possible = null;
            if ((example is MixinSubObjects))
            {
                MixinSubObjects mso = (MixinSubObjects)example;
                Type[] types = mso.GetMixedTypes();
                foreach (Type type in types)
                {
                    possible = IsTrue1(arg0Lower, exampleType, type, relativeTo, warn, out castArgTo,
                                       out isCollectionGetter, out functional);
                    if (functional)
                    {
                        return (o, arg1) => possible(((MixinSubObjects)o).GetInstance(type), arg1);
                    }
                }
            }
            possible = IsTrue1(arg0Lower, exampleType, exampleType, relativeTo, warn, out castArgTo,
                                  out isCollectionGetter, out functional);
            return possible;
        }

        public static Func<object, object, object> IsTrue1(string arg0Lower, Type exampleType, Type t, object relativeTo, OutputDelegate warn, out Type castArgTo, out bool isNonPredicate, out bool functional)
        {
            int predAt = arg0Lower.IndexOfAny("<>=*".ToCharArray());
            char compareChar = '=';
            if (predAt > 1)
            {
                compareChar = arg0Lower[predAt];
                arg0Lower = arg0Lower.Substring(predAt);
            }
            MemberInfo membH = null;

            if (arg0Lower.EndsWith("of"))
            {
                arg0Lower = arg0Lower.Substring(0, arg0Lower.Length - 2);
            }

            MemberInfo[] membs = t.GetMember(arg0Lower,
                                             BindingFlags.Instance |
                                             BindingFlags.IgnoreCase | 
                                             BindingFlags.Public);
            functional = true;
            foreach (MemberInfo memb in membs)
            {
                membH = memb;
                FieldInfo fi = memb as FieldInfo;
                if (fi != null)
                {
                    if (fi.FieldType == typeof(bool))
                    {
                        isNonPredicate = false;
                        castArgTo = null;
                        return delegate(object o, object arg1)
                                   {
                                       return fi.GetValue(o);
                                   };
                    }
                    continue;
                }
                MethodInfo mi = memb as MethodInfo;
                PropertyInfo pi = memb as PropertyInfo;
                if (pi != null) mi = pi.GetGetMethod();
                if (mi == null) continue;
                Type returnType = mi.ReturnType;
                var ps = mi.GetParameters();
                bool isBoolReturn = returnType == typeof(bool);
                int psl = ps.Length;
                if (psl > 2) continue;
                bool useRelative = psl > 0 && ps[psl - 1].ParameterType.IsInstanceOfType(relativeTo);
                isNonPredicate = exampleType.IsAssignableFrom(returnType) ||
                    returnType.IsAssignableFrom(exampleType) || 
                                     (!typeof(IConvertible).IsAssignableFrom(returnType)
                                      && typeof(IEnumerable).IsAssignableFrom(returnType));
                if (isNonPredicate)
                {
                    if (useRelative)
                    {
                        if (psl == 1)
                        {
                            castArgTo = null;
                            return delegate(object o, object arg1)
                            {
                                return mi.Invoke(o, new[] { relativeTo });
                            };
                        }
                        if (psl == 2)
                        {
                            castArgTo = ps[0].ParameterType;
                            return delegate(object o, object arg1)
                            {
                                return mi.Invoke(o, new[] { arg1, relativeTo });
                            };
                        }
                        continue;
                    }
                    if (psl == 1)
                    {
                        castArgTo = ps[0].ParameterType;
                        return delegate(object o, object arg1)
                                   {
                                       return mi.Invoke(o, new[] { arg1 });
                                   };
                    }
                    if (psl == 0)
                    {
                        castArgTo = null;
                        return delegate(object o, object arg1)
                        {
                            return mi.Invoke(o, null);
                        };
                    }
                    continue;
                }

                if (!isBoolReturn)
                {

                    if (useRelative)
                    {
                        castArgTo = returnType;
                        if (psl != 1) continue;
                        return delegate(object o, object arg1)
                                   {
                                       object tf = mi.Invoke(o, new[] { relativeTo });
                                       return CompareObjects(compareChar, tf, arg1);
                                   };
                    }
                    if (psl != 0) continue;
                    castArgTo = returnType;
                    return delegate(object o, object arg1)
                               {
                                   object tf = mi.Invoke(o, null);
                                   return CompareObjects(compareChar, tf, arg1);
                               };
                }
                if (useRelative)
                {
                    if (psl == 1)
                    {
                        castArgTo = null;
                        return delegate(object o, object arg1)
                                   {
                                       return mi.Invoke(o, new[] { relativeTo });
                                   };
                    }
                    if (psl == 2)
                    {
                        castArgTo = ps[0].ParameterType;
                        return delegate(object o, object arg1)
                                   {
                                       return mi.Invoke(o, new[] { arg1, relativeTo });
                                   };
                    }
                    continue;
                }
                if (psl == 0)
                {
                    castArgTo = null;
                    return delegate(object o, object arg1)
                               {
                                   return mi.Invoke(o, null);
                               };
                }
                if (psl == 1)
                {
                    castArgTo = ps[0].ParameterType;
                    return delegate(object o, object arg1)
                               {
                                   return mi.Invoke(o, new[] { arg1 });

                               };
                }
                continue;
            }
            isNonPredicate = false;
            functional = false;
            castArgTo = null;
            warn("dont know how to handle: " + arg0Lower + " " + membH);
            return delegate(object o, object arg1)
                       {
                           warn("dont know how to handle: " + arg0Lower + " " + membH);
                           return true;
                       };
        }

        private static bool CompareObjects(char comparteChar, object tf, object o)
        {
            throw new NotImplementedException();
        }
    }

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