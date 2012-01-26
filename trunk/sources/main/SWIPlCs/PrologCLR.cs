using System;
using System.Collections;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Reflection.Emit;
using System.Runtime.InteropServices;
using System.Threading;
using System.Xml.Serialization;
using ikvm.extensions;
using IKVM.Internal;
using ikvm.runtime;
using java.net;
using java.util;
//using jpl;
using jpl;
#if USE_MUSHDLR
using MushDLR223.Utilities;
#endif
using SbsSW.SwiPlCs.Callback;
using SbsSW.SwiPlCs.Exceptions;
using SbsSW.SwiPlCs.Streams;
using System.Windows.Forms;
using Hashtable = java.util.Hashtable;
using ClassLoader = java.lang.ClassLoader;
using Class = java.lang.Class;
using sun.reflect.misc;
using ArrayList=System.Collections.ArrayList;
using Util = ikvm.runtime.Util;
using CycFort = SbsSW.SwiPlCs.PlTerm;
using PrologCli = SbsSW.SwiPlCs.PrologClient;

namespace SbsSW.SwiPlCs
{
    public partial class PrologClient
    {
        protected string ClientPrefix { get; set; }
        private string _clientModule = null;
        protected string ClientModule
        {
            get { return _clientModule; }
            set { if (value != "user") _clientModule = value; }
        }

        public PrologClient()
        {
            ClientModule = null;
            ClientPrefix = "cli_";
            SetupProlog();
        }

        public readonly static Type[] ZERO_TYPES = new Type[0];

        public readonly static Object[] ZERO_OBJECTS = new Object[0];

        public static BindingFlags BindingFlagsJustStatic = BindingFlags.Public | BindingFlags.NonPublic |
                                                            BindingFlags.Static;
        public static BindingFlags BindingFlagsInstance = BindingFlags.Public | BindingFlags.NonPublic |
                                                            BindingFlags.Static;
        public static BindingFlags BindingFlagsALL = BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Static |
                                                     BindingFlags.Instance | BindingFlags.IgnoreCase;
        public static BindingFlags InstanceFields = BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.IgnoreCase;


        const string ExportModule = "clipl";

        public static bool Warn(string text, params object[] ps)
        {
            text = PlStringFormat(text, ps);
            return libpl.PL_warning(text) != 0;
        }

        private static string PlStringFormat(string text, params object[] ps)
        {
            ulong prologEvents = EventHandlerInProlog.PrologEvents;
            ulong refCount = libpl.TermRefCount;
            CheckEngine();
            RegisterThread(Thread.CurrentThread);
            try
            {
                if (ps != null && ps.Length > 0) text = String.Format(text, ps);
            }
            catch (Exception)
            {
            }
            return text;
        }

        private static PlTerm ToPlList(PlTerm[] terms)
        {
            int termLen = terms.Length;
            if (termLen == 0) return ATOM_NIL;
            termLen--;
            PlTerm ret = listOfOne(terms[termLen]);
            while (--termLen >= 0)
            {
                ret = PlTerm.PlCompound(".", terms[termLen], ret);
            }
            return ret;
        }

        private static PlTermV ToPlTermV(PlTerm[] terms)
        {
            var tv = NewPlTermV(terms.Length);
            for (int i = 0; i < terms.Length; i++)
            {
                tv[i] = terms[i];
            }
            return tv;
        }

        private static PlTermV NewPlTermV(int length)
        {
            return new PlTermV(length);
        }

        private static PlTermV ToPlTermVParams(ParameterInfo[] terms)
        {
            var tv = NewPlTermV(terms.Length);
            for (int i = 0; i < terms.Length; i++)
            {
                tv[i] = typeToSpec(terms[i].ParameterType);
            }
            return tv;
        }
        private static PlTerm ToPlListParams(ParameterInfo[] terms)
        {
            PlTerm listOf = ATOM_NIL;
            for (int i = terms.Length - 1; i >= 0; i--)
            {
                PlTerm term = typeToSpec(terms[i].ParameterType);
                listOf = PlTerm.PlCompound(".", term, listOf);
            }
            return listOf;
        }
        private static PlTerm ToPlListTypes(Type[] terms)
        {
            PlTerm listOf = ATOM_NIL;
            for (int i = terms.Length - 1; i >= 0; i--)
            {
                PlTerm term = typeToSpec(terms[i]);
                listOf = PlTerm.PlCompound(".", term, listOf);
            }
            return listOf;
        }
        private static PlTermV ToPlTermVSpecs(Type[] terms)
        {
            var tv = NewPlTermV(terms.Length);
            for (int i = 0; i < terms.Length; i++)
            {
                tv[i] = typeToSpec(terms[i]);
            }
            return tv;
        }


        private static PlTerm listOfOne(PlTerm term)
        {
            return PlTerm.PlCompound(".", term, ATOM_NIL);
        }

        protected static PlTerm ATOM_NIL
        {
            get { return PlTerm.PlAtom("[]"); }
        }

#if plvar_pins
        public static Dictionary<Int64, PlRef> termToObjectPins = new Dictionary<Int64, PlRef>();
        public static Dictionary<object, PlRef> objectToPlRef = new Dictionary<object, PlRef>();
        public static Dictionary<string, PlRef> atomToPlRef = new Dictionary<string, PlRef>();
#endif
        public static PlTerm PLNULL { get { return PlTerm.PlCompound("@", PlTerm.PlAtom("null")); } }
        public static PlTerm PLVOID { get { return PlTerm.PlCompound("@", PlTerm.PlAtom("void")); } }
        public static PlTerm PLTRUE { get { return PlTerm.PlCompound("@", PlTerm.PlAtom("true")); } }
        public static PlTerm PLFALSE { get { return PlTerm.PlCompound("@", PlTerm.PlAtom("false")); } }

        public static object CallProlog(object target, string module, string name, int arity, object origin, object[] paramz, Type returnType)
        {
            Thread threadCurrentThread = Thread.CurrentThread;
            RegisterThread(threadCurrentThread);
            uint fid = libpl.PL_open_foreign_frame();
            PlTermV args = NewPlTermV(arity);
            int fillAt = 0;
            if (origin != null)
            {
                args[fillAt++].FromObject((origin));
            }
            for (int i = 0; i < paramz.Length; i++)
            {
                args[fillAt++].FromObject((paramz[i]));
            }
            bool IsVoid = returnType == typeof(void);
            if (!IsVoid)
            {
                //args[fillAt] = PlTerm.PlVar();
            }
            if (!PlQuery.PlCall(module, name, args))
            {
                if (!IsVoid) Warn("Failed Event Handler {0} failed", target);
            }
            if (IsVoid) return null;
            object ret = PrologClient.CastTerm(args[fillAt], returnType);
            libpl.PL_discard_foreign_frame(fid);
            ///libpl.PL_close_foreign_frame(fid);
            DeregisterThread(threadCurrentThread);
            return ret;
        }

        public static int UnifyAtom(uint TermRef, string s)
        {
            uint temp = libpl.PL_new_term_ref();
            libpl.PL_put_atom(temp, libpl.PL_new_atom_wchars(s.Length, s));
            return libpl.PL_unify(temp, TermRef);
        }

        private static bool UnifySpecialObject(PlTerm plTerm, object ret1)
        {
            if (plTerm.IsVar)
            {
                return plTerm.FromObject(ret1);
            }
            else
            {
                var plvar = PlTerm.PlVar();
                plvar.FromObject(ret1);
                return SpecialUnify(plTerm, plvar);
            }
        }

        private static Type[] GetParamSpec(PlTerm memberSpec)
        {
            if (memberSpec.IsInteger)
            {
                Type[] lenType = new Type[memberSpec.intValue()];
                for (int i = 0; i < lenType.Length; i++)
                {
                    lenType[i] = null;
                }
                return lenType;
            }
            if (memberSpec.IsAtomic) return ZERO_TYPES;
            if (memberSpec.IsList)
            {
                memberSpec = memberSpec.Copy();
            }
            var specArray = memberSpec.ToArray();
            int arity = specArray.Length;
            Type[] paramz = new Type[arity];
            for (int i = 0; i < arity; i++)
            {
                PlTerm info = specArray[i];
                paramz[i] = GetType(info);
            }
            return paramz;
        }

        private static EventInfo findEventInfo(PlTerm memberSpec, Type c)
        {
            if (memberSpec.IsVar)
            {
                Warn("findEventInfo IsVar {0} on type {1}", memberSpec, c);
                return null;
            }
            if (memberSpec.IsInteger)
            {
                int ordinal = memberSpec.intValue();
                var mis = c.GetEvents(BindingFlagsALL);
                if (ordinal < 0 || ordinal >= mis.Length) return null;
                return mis[ordinal];
            }
            if (IsTaggedObject(memberSpec))
            {
                var r = tag_to_object(memberSpec[1].Name) as EventInfo;
                if (r != null) return r;
            }
            if (memberSpec.IsCompound)
            {
                if (memberSpec.Name == "e")
                {
                    var arg1 = memberSpec.Arg(0);
                    if (arg1.IsInteger)
                    {
                        return findEventInfo(arg1, c);
                    }
                }
            }
            if (c == null) return null;
            EventInfo ei = c.GetEvent(memberSpec.Name, BindingFlagsALL);
            if (ei != null) return ei;
            var members = c.GetEvents(BindingFlagsALL);
            int arity = memberSpec.Arity;
            foreach (var infos in members)
            {
                ParameterInfo[] getParmeters = GetParmeters(infos);
                if (getParmeters != null && getParmeters.Length == arity)
                {
                    return infos;
                }
            }
            return null;
        }
        private static MemberInfo findMember(PlTerm memberSpec, Type c)
        {
            if (IsTaggedObject(memberSpec))
            {
                var r = tag_to_object(memberSpec[1].Name) as MemberInfo;
                if (r != null) return r;
            }
            return findField(memberSpec, c) ??
                   (MemberInfo)
                   findProperty(memberSpec, c) ??
                   findMethod(memberSpec, -1, c);
            //findConstructor(memberSpec, c));
        }
        private static FieldInfo findField(PlTerm memberSpec, Type c)
        {
            if (c == null)
            {
                Warn("findField no class for {0}", memberSpec);
                return null;
            }
            if (memberSpec.IsVar)
            {
                Warn("findField IsVar {0} on type {1}", memberSpec, c);
                return null;
            }
            if (memberSpec.IsInteger)
            {
                int ordinal = memberSpec.intValue();
                var mis = c.GetFields(BindingFlagsALL);
                if (ordinal < 0 || ordinal >= mis.Length) return null;
                return mis[ordinal];
            }
            if (IsTaggedObject(memberSpec))
            {
                var r = tag_to_object(memberSpec[1].Name) as FieldInfo;
                if (r != null) return r;
            }
            if (memberSpec.IsCompound)
            {
                if (memberSpec.Name != "f")
                {
                    return null;
                }
                return findField(memberSpec.Arg(0), c);
            }
            string fn = memberSpec.Name;
            FieldInfo fi = c.GetField(fn, BindingFlagsALL);
            return fi;
        }
        private static PropertyInfo findProperty(PlTerm memberSpec, Type c)
        {
            if (c == null)
            {
                Warn("findProperty no class for {0}", memberSpec);
                return null;
            }
            if (memberSpec.IsVar)
            {
                Warn("findProperty IsVar {0} on type {1}", memberSpec, c);
                return null;
            }
            if (memberSpec.IsInteger)
            {
                int ordinal = memberSpec.intValue();
                var mis = c.GetProperties(BindingFlagsALL);
                if (ordinal < 0 || ordinal >= mis.Length) return null;
                return mis[ordinal];
            }
            if (IsTaggedObject(memberSpec))
            {
                var r = tag_to_object(memberSpec[1].Name) as PropertyInfo;
                if (r != null) return r;
            }
            if (memberSpec.IsCompound)
            {
                if (memberSpec.Name != "p")
                {
                    return null;
                }
                return findProperty(memberSpec.Arg(0), c);
            }
            string fn = memberSpec.Name;
            return c.GetProperty(fn, BindingFlagsALL) ?? c.GetProperty("Is" + fn, BindingFlagsALL);
        }

        private static MethodInfo findMethod(PlTerm memberSpec, int arity, Type c)
        {
            if (c == null)
            {
                Warn("findMethod no class for {0}", memberSpec);
                return null;
            }
            if (memberSpec.IsVar)
            {
                Warn("findMethod IsVar {0} on type {1}", memberSpec, c);
                return null;
            }
            if (memberSpec.IsInteger)
            {
                var mis = c.GetMethods(BindingFlagsALL);
                return mis[memberSpec.intValue()];
            }
            if (IsTaggedObject(memberSpec))
            {
                var r = tag_to_object(memberSpec[1].Name) as MethodInfo;
                if (r != null) return r;
            }
            string fn = memberSpec.Name;
            var mi = GetMethod(c, fn, BindingFlagsALL);
            if (mi != null) return mi;
            Type[] paramz = GetParamSpec(memberSpec);
            try
            {
                mi = c.GetMethod(fn, paramz);
                if (mi != null) return mi;
            }
            catch (AmbiguousMatchException e)
            {
                Debug("AME: " + e + " fn = " + fn);
            }
            MethodInfo[] members = c.GetMethods(BindingFlagsALL);
            if (arity < 0) arity = paramz.Length;// memberSpec.Arity;
            string fnLower = fn.ToLower();
            MethodInfo candidate = null;
            foreach (var infos in members)
            {
                if (infos.GetParameters().Length == arity)
                {
                    if (infos.Name == fn)
                    {
                        return infos;
                    }
                    if (candidate == null && infos.Name.ToLower() == fnLower)
                    {
                        candidate = infos;
                    }
                }
            }
            if (candidate != null) return candidate;
            return null;
        }


        private static ConstructorInfo findConstructor(PlTerm memberSpec, Type c)
        {
            if (c == null)
            {
                Warn("findConstructor no class for {0}", memberSpec);
                return null;
            }
            if (IsTaggedObject(memberSpec))
            {
                var r = tag_to_object(memberSpec[1].Name) as ConstructorInfo;
                if (r != null) return r;
            }
            if (memberSpec.IsInteger)
            {
                var mis = c.GetConstructors(BindingFlagsALL);
                return mis[memberSpec.intValue()];
            }
            Type[] paramz = GetParamSpec(memberSpec);
            if (paramz != null)
            {
                var mi = c.GetConstructor(paramz);
                if (mi != null) return mi;
            }
            ConstructorInfo[] members = c.GetConstructors(BindingFlagsALL);
            int arity = memberSpec.Arity;
            ConstructorInfo candidate = null;
            foreach (var infos in members)
            {
                if (infos.GetParameters().Length == arity)
                {
                    if (infos.IsStatic)
                    {
                        if (candidate == null)
                        {
                            candidate = infos;
                        }
                    }
                    else return infos;
                }
            }
            return candidate;
        }


        [PrologVisible(ModuleName = ExportModule)]
        static public bool cliMembers(PlTerm clazzOrInstance, PlTerm membersOut)
        {
            Type c = GetTypeFromInstance(null, clazzOrInstance);
            MemberInfo[] members = c.GetMembers(BindingFlagsALL);
            List<PlTerm> list = new List<PlTerm>();
            string cname = c.Name;
            List<MemberInfo> exclude = new List<MemberInfo>();
            int ordinal = 0;
            foreach (var info in c.GetFields(BindingFlagsALL))
            {
                AddMemberToList(info, list, cname, ordinal++);
                exclude.Add(info);
            }
            ordinal = 0;
            foreach (var info in c.GetProperties(BindingFlagsALL))
            {
                AddMemberToList(info, list, cname, ordinal++);
                exclude.Add(info);
            }
            ordinal = 0;
            foreach (var info in c.GetMethods(BindingFlagsALL))
            {
                AddMemberToList(info, list, cname, ordinal++);
                exclude.Add(info);
            }
            ordinal = 0;
            foreach (var info in c.GetConstructors(BindingFlagsALL))
            {
                AddMemberToList(info, list, cname, ordinal++);
                exclude.Add(info);
            }
            ordinal = 0;
            foreach (var info in c.GetEvents(BindingFlagsALL))
            {
                AddMemberToList(info, list, cname, ordinal++);
                exclude.Add(info);
            }

            foreach (MemberInfo info in members)
            {
                break;
                try
                {
                    if (exclude.Contains(info)) continue;
                }
                catch (Exception e)
                {
                    Debug("Warn exclude.Contains " + info + ": " + e);
                    continue;
                }
                AddMemberToList(info, list, cname, ordinal++);
                exclude.Add(info);
            }

            return membersOut.Unify(ToPlList(list.ToArray()));
        }

        private static void AddMemberToList(MemberInfo info, List<PlTerm> list, string cname, int ordinal)
        {

            PlTerm memb = MemberTerm(info, cname, ordinal);
            if (memb.TermRef != 0) list.Add(memb);
        }

        private static PlTerm MemberTerm(MemberInfo info, string cname, int ordinal)
        {
            string mn = info.Name;
            switch (info.MemberType)
            {
                case MemberTypes.Constructor:
                    {
                        var fi = (ConstructorInfo)info;
                        var mi = fi;
                        return PlC("c", new PlTerm(ordinal), PlTerm.PlAtom(mn),
                                   ToPlListParams(fi.GetParameters()),
                                   (mi.IsGenericMethodDefinition ? ToPlListTypes(mi.GetGenericArguments()) : ATOM_NIL),

                                   PlC("decl",
                                       AFlag(mi.IsStatic, "static"),
                                       typeToSpec(fi.DeclaringType)),
                                   PlC("access_pafv",
                                       AFlag(mi.IsPublic),
                                       AFlag(mi.IsAssembly),
                                       AFlag(mi.IsFamily),
                                       AFlag(mi.IsPrivate)));
                    }
                    break;
                case MemberTypes.Event:
                    {
                        var fi = (EventInfo)info;
                        MethodInfo mi = (fi.GetRaiseMethod() ??
                                         (fi.EventHandlerType != null ? fi.EventHandlerType.GetMethod("Invoke") : null) ??
                                         fi.GetAddMethod() ?? fi.GetRemoveMethod());
                        ParameterInfo[] parme = GetParmeters(fi);
                        return PlC("e", new PlTerm(ordinal), PlTerm.PlAtom(mn),
                                   typeToSpec(fi.EventHandlerType),
                                   ToPlListParams(parme),
                                   (mi.IsGenericMethodDefinition ? ToPlListTypes(mi.GetGenericArguments()) : ATOM_NIL),

                                   PlC("decl",
                                       AFlag(mi.IsStatic, "static"),
                                       typeToSpec(fi.DeclaringType)),
                                   PlC("access_pafv",
                                       AFlag(mi.IsPublic),
                                       AFlag(mi.IsAssembly),
                                       AFlag(mi.IsFamily),
                                       AFlag(mi.IsPrivate)));
                    }
                    break;
                case MemberTypes.Field:
                    {
                        var fi = (FieldInfo)info;
                        var mi = fi;
                        return PlC("f", new PlTerm(ordinal), PlTerm.PlAtom(mn),
                                   typeToSpec(fi.FieldType),

                                   PlC("decl",
                                       AFlag(mi.IsStatic, "static"),
                                       typeToSpec(fi.DeclaringType)),
                                   PlC("access_pafv",
                                       AFlag(mi.IsPublic),
                                       AFlag(mi.IsAssembly),
                                       AFlag(mi.IsFamily),
                                       AFlag(mi.IsPrivate)));
                    }
                    break;
                case MemberTypes.Method:
                    {
                        var fi = (MethodInfo)info;
                        var mi = fi;
                        return PlC("m", new PlTerm(ordinal), PlTerm.PlAtom(mn),
                                   typeToSpec(fi.ReturnParameter.ParameterType),
                                   ToPlListParams(fi.GetParameters()),
                                   (mi.IsGenericMethodDefinition ? ToPlListTypes(mi.GetGenericArguments()) : ATOM_NIL),

                                   PlC("decl",
                                       AFlag(mi.IsStatic, "static"),
                                       typeToSpec(fi.DeclaringType)),
                                   PlC("access_pafv",
                                       AFlag(mi.IsPublic),
                                       AFlag(mi.IsAssembly),
                                       AFlag(mi.IsFamily),
                                       AFlag(mi.IsPrivate)));
                    }
                    break;
                case MemberTypes.Property:
                    {
                        var fi = (PropertyInfo)info;
                        MethodInfo mi = (fi.CanRead ? fi.GetGetMethod(true) : fi.GetSetMethod(true));
                        return PlC("p", new PlTerm(ordinal), PlTerm.PlAtom(mn),
                                   typeToSpec(fi.PropertyType),
                                   ToPlListParams(fi.GetIndexParameters()),
                                   (mi.IsGenericMethodDefinition ? ToPlListTypes(mi.GetGenericArguments()) : ATOM_NIL),
                                   AFlag(fi.CanRead, "CanRead"),
                                   AFlag(fi.CanWrite, "CanWrite"),

                                   PlC("decl",
                                       AFlag(mi.IsStatic, "static"),
                                       typeToSpec(fi.DeclaringType)),
                                   PlC("access_pafv",
                                       AFlag(mi.IsPublic),
                                       AFlag(mi.IsAssembly),
                                       AFlag(mi.IsFamily),
                                       AFlag(mi.IsPrivate)));
                    }
                    break;
                case MemberTypes.TypeInfo:
                    break;
                case MemberTypes.Custom:
                    break;
                case MemberTypes.NestedType:
                    break;
                case MemberTypes.All:
                    break;
                default:
                    throw new ArgumentOutOfRangeException();
            }
            return default(PlTerm);
        }

        public static PlTerm PlC(string decl, params PlTerm[] plTerms)
        {
            return PlTerm.PlCompound(decl, plTerms);
        }
        public static PlTerm PlC(string decl, PlTermV termV)
        {
            return PlTerm.PlCompound(decl, termV);
        }

        private static PlTerm AFlag(bool tf, string name)
        {
            PlTerm plTermPlAtom = PlTerm.PlAtom(tf ? "true" : "false");
            return PlC(name, plTermPlAtom);
        }
        private static PlTerm AFlag(bool tf)
        {
            PlTerm plTermPlAtom = PlTerm.PlAtom(tf ? "true" : "false");
            return plTermPlAtom;
        }

        private static ParameterInfo[] GetParmeters(EventInfo ei)
        {
            ParameterInfo[] parme = null;
            var rm = ei.GetRaiseMethod();
            var erm = ei.EventHandlerType;
            if (rm == null && erm != null)
            {
                rm = erm.GetMethod("Invoke");
            }
            if (rm != null)
            {
                parme = rm.GetParameters();
            }
            return parme;
        }



        [PrologVisible(ModuleName = ExportModule)]
        static public bool cliFindConstructor(PlTerm clazzSpec, PlTerm memberSpec, PlTerm methodOut)
        {
            Type c = GetType(clazzSpec);
            ConstructorInfo mi = findConstructor(memberSpec, c);
            if (mi != null)
            {
                return methodOut.FromObject((mi));
            }
            return false;
        }

        /// <summary>
        /// ?- cliNew('java.lang.Long',[long],[44],Out),cliToString(Out,Str).
        /// </summary>
        /// <param name="memberSpec"></param>
        /// <param name="valueIn"></param>
        /// <param name="valueOut"></param>
        /// <returns></returns>
        [PrologVisible(ModuleName = ExportModule)]
        static public bool cliNew(PlTerm clazzSpec, PlTerm memberSpec, PlTerm valueIn, PlTerm valueOut)
        {
            if (!valueOut.IsVar)
            {
                var plvar = PlTerm.PlVar();
                cliNew(clazzSpec, memberSpec, valueIn, plvar);
                return SpecialUnify(valueOut, plvar);
            }
            Type c = GetType(clazzSpec);
            if (c == null)
            {
                Warn("Cant resolve clazzSpec {0}", clazzSpec);
                return false;
            }
            ConstructorInfo mi = findConstructor(memberSpec, c);
            if (mi == null)
            {
                Warn("Cant find constructor {0} on {1}", memberSpec, c);
                return false;
            }
            Action postCallHook;
            object[] values = PlListToCastedArray(valueIn, mi.GetParameters(), out postCallHook);
            var ret = valueOut.FromObject((mi.Invoke(values)));
            postCallHook();
            return ret;
        }

        /// <summary>
        /// ?- cliNewArray(long,10,Out),cliToString(Out,Str).
        /// </summary>
        /// <param name="clazzSpec"></param>
        /// <param name="rank"></param>
        /// <param name="valueOut"></param>
        /// <returns></returns>
        [PrologVisible(ModuleName = ExportModule)]
        static public bool cliNewArray(PlTerm clazzSpec, PlTerm rank, PlTerm valueOut)
        {
            if (!valueOut.IsVar)
            {
                var plvar = PlTerm.PlVar();
                cliNewArray(clazzSpec, rank, plvar);
                return SpecialUnify(valueOut, plvar);
            }
            Type c = GetType(clazzSpec);
            if (c == null)
            {
                Warn("Cant find type {0}", clazzSpec);
                return false;
            }
            var value = c.MakeArrayType(rank.intValue());
            return valueOut.FromObject((value));
        }

        [PrologVisible(ModuleName = ExportModule)]
        static public bool cliGetTerm(PlTerm valueCol, PlTerm valueIn, PlTerm valueOut)
        {
            List<object> objs;
            if (valueCol.IsVar)
            {
                objs = new List<object>();
                valueCol.FromObject(objs);
            }
            else
            {
                objs = (List<object>)CastTerm(valueCol, typeof(ICollection));
            }
            if (!valueOut.IsVar)
            {
                var plvar = PlTerm.PlVar();
                cliGetTerm(valueCol, valueIn, plvar);
                return SpecialUnify(valueOut, plvar);
            }
            if (IsTaggedObject(valueIn))
            {
                object val = GetInstance(valueIn);
                int index = objs.IndexOf(val);
                if (index < 0)
                {
                    index = objs.Count;
                    objs.Add(val);
                    var type = val.GetType();
                    if (type.IsArray)
                    {
                        return valueIn.Unify(valueOut);
                    }
                    return ToFieldLayout("object", typeToName(type), val, type, valueOut, false, false) != libpl.PL_fail;
                }
            }
            return valueIn.Unify(valueOut);
        }

        /// <summary>
        /// ?- cliNewArray(long,10,Out),cliToString(Out,Str).
        /// </summary>
        /// <param name="clazzSpec"></param>
        /// <param name="rank"></param>
        /// <param name="valueOut"></param>
        /// <returns></returns>
        [PrologVisible(ModuleName = ExportModule)]
        static public bool cliArrayToTerm(PlTerm arrayValue, PlTerm valueOut)
        {
            if (!valueOut.IsVar)
            {
                var plvar = PlTerm.PlVar();
                cliArrayToTerm(arrayValue, plvar);
                return SpecialUnify(valueOut, plvar);
            }
            object getInstance = GetInstance(arrayValue);
            if (getInstance == null) return valueOut.Unify(PLNULL);
            Array value = GetArrayValue(getInstance);
            if (value == null)
            {
                Warn("Cant find array from {0} as {1}", arrayValue, getInstance.GetType());
                return false;
            }
            int len = value.Length;
            var termv = NewPlTermV(len);
            for (int i = 0; i < len; i++)
            {
                bool pf = termv[i].FromObject((value.GetValue(i)));
            }
            Type et = value.GetType().GetElementType();
            return valueOut.Unify(PlC("array", typeToSpec(et), PlC("values", termv)));
        }
        [PrologVisible(ModuleName = ExportModule)]
        static public bool cliArrayToTermList(PlTerm arrayValue, PlTerm valueOut)
        {
            if (!valueOut.IsVar)
            {
                var plvar = PlTerm.PlVar();
                cliArrayToTermList(arrayValue, plvar);
                return SpecialUnify(valueOut, plvar);
            }

            object getInstance = GetInstance(arrayValue);
            if (getInstance == null) return valueOut.Unify(PLNULL);
            Array value = GetArrayValue(getInstance);
            if (value == null)
            {
                Warn("Cant find array from {0} as {1}", arrayValue, getInstance.GetType());
                return false;
            }
            int len = value.Length;
            var termv = ATOM_NIL;
            for (int i = len - 1; i >= 0; i--)
            {
                termv = PlC(".", ToProlog((value.GetValue(i))), termv);
            }
            //Type et = value.GetType().GetElementType();
            return valueOut.Unify(termv);
        }
        [PrologVisible(ModuleName = ExportModule)]
        static public bool cliTermToArray(PlTerm arrayValue, PlTerm valueOut)
        {
            if (!valueOut.IsVar)
            {
                var plvar = PlTerm.PlVar();
                cliTermToArray(arrayValue, plvar);
                return SpecialUnify(valueOut, plvar);
            }
            Type elementType = ResolveType(arrayValue.Name);
            if (elementType == null)
            {
                Warn("Cant find vector from {0}", arrayValue);
                return false;
            }
            int arrayValueArity = arrayValue.Arity;
            var value = Array.CreateInstance(elementType, arrayValueArity);
            int argn = 1;
            for (int i = 0; i < arrayValueArity; i++)
            {
                value.SetValue(GetInstance(arrayValue[argn]), i);
                argn++;
            }
            return valueOut.FromObject((value));
        }

        private static Array GetArrayValue(object getInstance)
        {
            if (getInstance == null) return null;
            if (getInstance is Array)
            {
                return (Array)getInstance;
            }
            Type t = getInstance.GetType();
            Type et = typeof(object);
            if (t.IsGenericType)
            {
                Type[] typeArguments = t.GetGenericArguments();
                if (typeArguments.Length == 1)
                {
                    et = typeArguments[1];
                }
            }
            if (getInstance is ArrayList)
            {
                return ((ArrayList)getInstance).ToArray(et);
            }
            if (getInstance is ICollection)
            {
                var collection = ((ICollection)getInstance);
                int count = collection.Count;
                var al = Array.CreateInstance(et, count);
                collection.CopyTo(al, count);
                return al;
            }
            if (getInstance is IEnumerable)
            {
                var collection = ((IEnumerable)getInstance).GetEnumerator();
                var al = new ArrayList();
                while (collection.MoveNext())
                {
                    al.Add(collection.Current);
                }
                return al.ToArray(et);
            }
            else if (getInstance is IEnumerator)
            {
                var collection = ((IEnumerator)getInstance);
                var al = new ArrayList();
                while (collection.MoveNext())
                {
                    al.Add(collection.Current);
                }
                return al.ToArray(et);
            }
            else
            {
                // this one is probly null
                return getInstance as Array;
            }
        }

        [PrologVisible(ModuleName = ExportModule)]
        static public bool cliEnterLock(PlTerm lockObj)
        {
            object getInstance = GetInstance(lockObj);
            Monitor.Enter(getInstance);
            return true;
        }
        [PrologVisible(ModuleName = ExportModule)]
        static public bool cliExitLock(PlTerm lockObj)
        {
            object getInstance = GetInstance(lockObj);
            Monitor.Exit(getInstance);
            return true;
        }
        [PrologVisible(ModuleName = ExportModule)]
        static public bool cliFindMethod(PlTerm clazzOrInstance, PlTerm memberSpec, PlTerm methodOut)
        {
            if (!methodOut.IsVar)
            {
                var plvar = PlTerm.PlVar();
                cliFindMethod(clazzOrInstance, memberSpec, plvar);
                return SpecialUnify(methodOut, plvar);
            }
            object getInstance = GetInstance(clazzOrInstance);
            Type c = GetTypeFromInstance(getInstance, clazzOrInstance);
            MethodInfo mi = findMethod(memberSpec, -1, c);
            if (mi != null)
            {
                return methodOut.FromObject((mi));
            }
            return false;
        }



        [PrologVisible(ModuleName = ExportModule)]
        static public bool cliCallRaw(PlTerm clazzOrInstance, PlTerm memberSpec, PlTerm valueIn, PlTerm valueOut)
        {
            if (!valueOut.IsVar)
            {
                var plvar = PlTerm.PlVar();
                cliCallRaw(clazzOrInstance, memberSpec, valueIn, plvar);
                return SpecialUnify(valueOut, plvar);
            }
            object getInstance = GetInstance(clazzOrInstance);
            Type c = GetTypeFromInstance(getInstance, clazzOrInstance);
            int arity = Arglen(valueIn);
            MethodInfo mi = findMethod(memberSpec, arity, c);
            if (mi == null)
            {
                var ei = findEventInfo(memberSpec, c);
                if (ei != null) return cliRaiseEventHandler(clazzOrInstance, memberSpec, valueIn, valueOut);
                if (valueIn.IsAtom && valueIn.Name == "[]") return cliGetRaw(clazzOrInstance, memberSpec, valueOut);
                Warn("Cant find method {0} on {1}", memberSpec, c);
                return false;
            }
            Action postCallHook;
            object[] value = PlListToCastedArray(valueIn, mi.GetParameters(), out postCallHook);
            object target = mi.IsStatic ? null : getInstance;
            object retval = InvokeCaught(mi, target, value, postCallHook);
            return valueOut.FromObject(retval ?? VoidOrNull(mi));
        }

        //cliNewDelegate
        [PrologVisible(ModuleName = ExportModule)]
        static public bool cliNewDelegate(PlTerm delegateClass, PlTerm prologPred, PlTerm valueOut)
        {
            if (!valueOut.IsVar)
            {
                var plvar = PlTerm.PlVar();
                cliNewDelegate(delegateClass, prologPred, plvar);
                return SpecialUnify(valueOut, plvar);
            }
            object retval = cliDelegateTerm(GetTypeThrowIfMissing(delegateClass), prologPred, true);
            return valueOut.FromObject(retval);
        }
        [PrologVisible(ModuleName = ExportModule)]
        static public Delegate cliDelegateTerm(Type fi, PlTerm prologPred, bool saveKey)
        {
            if (prologPred.IsCompound)
            {
                if (prologPred.Name == "delegate")
                {
                    if (prologPred.Arity == 1)
                    {
                        return cliDelegateTerm(fi, prologPred.Arg(0), saveKey);
                    }
                    Type dt = GetTypeThrowIfMissing(prologPred.Arg(0));
                    var obj = cliDelegateTerm(dt, prologPred.Arg(1), saveKey);
                    return (Delegate)RecastObject(fi, obj, dt);
                }
                if (prologPred.Name == "@")
                {
                    return (Delegate)RecastObject(fi, tag_to_object((string)prologPred.Arg(0)), null);
                }
            }

            var Key = new DelegateObjectInPrologKey();
            Key.Name = prologPred.Name;
            Key.Arity = prologPred.Arity;
            //uint fid = libpl.PL_open_foreign_frame();
            //Key.Origin = prologPred.Copy();
            Key.DelegateType = fi;

            DelegateObjectInProlog handlerInProlog;
            lock (PrologDelegateHandlers)
            {
                if (PrologDelegateHandlers.TryGetValue(Key, out handlerInProlog))
                {
                    //   fi.RemoveEventHandler(getInstance, handlerInProlog.Delegate);
                    PrologDelegateHandlers.Remove(Key);
                }
                handlerInProlog = new DelegateObjectInProlog(Key);
                if (saveKey) PrologDelegateHandlers.Add(Key, handlerInProlog);
                // fi.AddEventHandler(getInstance, handlerInProlog.Delegate);
            }
            return handlerInProlog.Delegate;

        }

        private static int Arglen(PlTerm valueIn)
        {
            if (valueIn.IsList)
            {
                int len = 0;
                PlTerm each = valueIn;
                while (each.IsList && !each.IsAtom)
                {
                    each = each.Arg(1);
                    len++;
                }
                return len;
            }
            if (valueIn.IsCompound) return valueIn.Arity;
            if (valueIn.IsAtom) return 0;
            return -1;
        }

        [PrologVisible(ModuleName = ExportModule)]
        static public bool cliRaiseEventHandler(PlTerm clazzOrInstance, PlTerm memberSpec, PlTerm valueIn, PlTerm valueOut)
        {
            if (!valueOut.IsVar)
            {
                var plvar = PlTerm.PlVar();
                cliRaiseEventHandler(clazzOrInstance, memberSpec, valueIn, plvar);
                return SpecialUnify(valueOut, plvar);
            }
            object getInstance = GetInstance(clazzOrInstance);
            Type c = GetTypeFromInstance(getInstance, clazzOrInstance);
            EventInfo evi = findEventInfo(memberSpec, c);
            if (evi == null)
            {
                return Warn("Cant find event {0} on {1}", memberSpec, c);
            }
            MethodInfo eventHandlerMethodProto = evi.EventHandlerType.GetMethod("Invoke");
            ParameterInfo[] paramInfos = GetParmeters(evi);
            MethodInfo mi = evi.GetRaiseMethod();
            string fn = evi.Name;
            if (mi == null)
            {
                FieldInfo fi = c.GetField(fn, BindingFlagsALL);
                if (fi != null)
                {
                    Delegate del = (Delegate)fi.GetValue(getInstance);
                    if (del != null)
                    {
                        Action postCallHook;
                        var ret = valueOut.FromObject((del.DynamicInvoke(
                                                          PlListToCastedArray(valueIn, paramInfos,
                                                                              out postCallHook))));
                        postCallHook();
                        return ret;
                    }
                }
                string fn1 = fn.Substring(1);
                int len = fn.Length;
                foreach (FieldInfo info in c.GetFields(BindingFlagsALL))
                {
                    if (info.Name.EndsWith(fn1))
                    {
                        if (info.Name.Length - len < 3)
                        {
                            Delegate del = (Delegate)info.GetValue(info.IsStatic ? null : getInstance);
                            if (del != null)
                            {
                                Action postCallHook;
                                var ret = valueOut.FromObject((del.DynamicInvoke(
                                                                  PlListToCastedArray(valueIn, paramInfos,
                                                                                      out postCallHook))));
                                postCallHook();
                                return ret;
                            }
                        }
                    }
                }
            }
            if (mi == null)
            {
                mi = eventHandlerMethodProto;
            }
            if (mi == null)
            {
                Warn("Cant find event raising for  {0} on {1}", evi, c);
                return false;
            }
            Action postCallHook0;
            object[] value = PlListToCastedArray(valueIn, mi.GetParameters(), out postCallHook0);
            object target = mi.IsStatic ? null : getInstance;
            return valueOut.FromObject(InvokeCaught(mi, target, value, postCallHook0) ?? VoidOrNull(mi));
        }

        private static object VoidOrNull(MethodInfo info)
        {
            return info.ReturnType == typeof(void) ? (object)PLVOID : PLNULL;
        }

        public static Dictionary<DelegateObjectInPrologKey, DelegateObjectInProlog> PrologDelegateHandlers =
    new Dictionary<DelegateObjectInPrologKey, DelegateObjectInProlog>();

        public static Dictionary<EventHandlerInPrologKey, EventHandlerInProlog> PrologEventHandlers =
            new Dictionary<EventHandlerInPrologKey, EventHandlerInProlog>();

#if USE_MUSHDLR
        public static TaskQueueHandler PrologEventQueue = new TaskQueueHandler("PrologEventHandler");
#endif

        [PrologVisible(ModuleName = ExportModule)]
        static public bool cliAddEventHandler(PlTerm clazzOrInstance, PlTerm memberSpec, PlTerm prologPred)
        {
            object getInstance = GetInstance(clazzOrInstance);
            Type c = GetTypeFromInstance(getInstance, clazzOrInstance);
            EventInfo fi = findEventInfo(memberSpec, c);
            if (fi == null)
            {
                return Warn("Cant find event {0} on {1}", memberSpec, c);
            }
            var Key = new EventHandlerInPrologKey();
            Key.Name = prologPred.Name;
            Key.Arity = prologPred.Arity;
            Key.Origin = getInstance;
            Key.Event = fi;

            lock (PrologEventHandlers)
            {
                EventHandlerInProlog handlerInProlog;
                if (PrologEventHandlers.TryGetValue(Key, out handlerInProlog))
                {
                    fi.RemoveEventHandler(getInstance, handlerInProlog.Delegate);
                    PrologEventHandlers.Remove(Key);
                }
                handlerInProlog = new EventHandlerInProlog(Key);
                PrologEventHandlers.Add(Key, handlerInProlog);
                fi.AddEventHandler(getInstance, handlerInProlog.Delegate);
            }
            return true;
        }
        [PrologVisible(ModuleName = ExportModule)]
        static public bool cliRemoveEventHandler(PlTerm clazzOrInstance, PlTerm memberSpec, PlTerm prologPred)
        {
            object getInstance = GetInstance(clazzOrInstance);
            Type c = GetTypeFromInstance(getInstance, clazzOrInstance);
            EventInfo fi = findEventInfo(memberSpec, c);//
            if (fi == null)
            {
                return Warn("Cant find event {0} on {1}", memberSpec, c);
            }
            var Key = new EventHandlerInPrologKey();
            Key.Name = prologPred.Name;
            Key.Arity = prologPred.Arity;
            Key.Origin = getInstance;
            Key.Event = fi;
            EventHandlerInProlog handlerInProlog;
            lock (PrologEventHandlers) if (PrologEventHandlers.TryGetValue(Key, out handlerInProlog))
                {
                    fi.RemoveEventHandler(getInstance, handlerInProlog.Delegate);
                    PrologEventHandlers.Remove(Key);
                    return true;
                }
            return Warn("Cant find registered handler {0} for {1} on {2}", prologPred, memberSpec, c);
        }

        [PrologVisible(ModuleName = ExportModule)]
        static public bool cliGetRaw(PlTerm clazzOrInstance, PlTerm memberSpec, PlTerm valueOut)
        {
            if (clazzOrInstance.IsVar)
            {
                return Warn("Cant find instance {0}", clazzOrInstance);
            }
            if (!valueOut.IsVar)
            {
                var plvar = PlTerm.PlVar();
                cliGetRaw(clazzOrInstance, memberSpec, plvar);
                return SpecialUnify(valueOut, plvar);
            }
            object getInstance = GetInstance(clazzOrInstance);
            Type c = GetTypeFromInstance(getInstance, clazzOrInstance);
            if (getInstance == null && c == null)
            {
                Warn("Cant find instance {0}", clazzOrInstance);
                return false;
            }
            bool found;
            object cliGet01 = cliGet0(getInstance, memberSpec, c, out found);
            if (!found) return false;
            return valueOut.FromObject(cliGet01);
        }
        static public object cliGet0(object getInstance, PlTerm memberSpec, Type c, out bool found)
        {
            FieldInfo fi = findField(memberSpec, c);
            if (fi != null)
            {
                object fiGetValue = fi.GetValue(fi.IsStatic ? null : getInstance);
                found = true;
                return (fiGetValue);
            }
            var pi = findProperty(memberSpec, c);
            if (pi != null)
            {
                var mi = pi.GetGetMethod();
                if (mi != null)
                {
                    found = true;
                    return ((InvokeCaught(mi, mi.IsStatic ? null : getInstance, ZERO_OBJECTS) ?? VoidOrNull(mi)));
                }
                WarnMissing("Cant find getter for property " + memberSpec + " on " + c + " for " + pi);
                found = false;
                return null;
            }
            else
            {
                if (memberSpec.IsVar)
                {
                    Warn("cliGet0 on IsVar={0} on {1} for {2}", memberSpec, c, getInstance);
                    found = false;
                    return getInstance;
                }
                string fn = memberSpec.Name;
                MethodInfo mi = findMethod(memberSpec, -1, c) ??
                                GetMethod(c, fn, BindingFlagsALL) ??
                                GetMethod(c, "get_" + fn, BindingFlagsALL) ??
                                GetMethod(c, "Get" + fn, BindingFlagsALL) ??
                                GetMethod(c, "Is" + fn, BindingFlagsALL) ??
                                GetMethod(c, "To" + fn, BindingFlagsALL);
                if (mi == null)
                {
                    WarnMissing("Cant find getter " + memberSpec + " on " + c);
                    found = false;
                    return null;
                }
                Action postCallHook;
                object[] value = PlListToCastedArray(memberSpec, mi.GetParameters(), out postCallHook);
                object target = mi.IsStatic ? null : getInstance;
                object retval = InvokeCaught(mi, target, value, postCallHook) ?? VoidOrNull(mi);
                found = true;
                return retval;
            }
        }

        private static bool WarnMissing(string s)
        {
            if (true)
            {
                Debug(s);
                return false;
            }
            return Warn(s);
        }

        private static MethodInfo GetMethod(Type type, string s, BindingFlags flags)
        {
            try
            {
                return type.GetMethod(s, flags);
            }
            catch (AmbiguousMatchException)
            {
                return null;
            }
        }

        [PrologVisible(ModuleName = ExportModule)]
        static public bool cliSetRaw(PlTerm clazzOrInstance, PlTerm memberSpec, PlTerm valueIn)
        {
            object getInstance = GetInstance(clazzOrInstance);
            Type c = GetTypeFromInstance(getInstance, clazzOrInstance);
            return cliSet0(getInstance, memberSpec, valueIn, c);
        }

        static public bool cliSet0(object getInstance, PlTerm memberSpec, PlTerm valueIn, Type c)
        {

            FieldInfo fi = findField(memberSpec, c);
            if (fi != null)
            {
                object value = CastTerm(valueIn, fi.FieldType);
                object target = fi.IsStatic ? null : getInstance;
                fi.SetValue(target, value);
                return true;
            }
            var pi = findProperty(memberSpec, c);
            if (pi != null)
            {
                var mi = pi.GetSetMethod();
                if (mi != null)
                {
                    object value = CastTerm(valueIn, pi.PropertyType);
                    object target = mi.IsStatic ? null : getInstance;
                    InvokeCaught(mi, target, new[] { value });
                    return true;
                }
                return WarnMissing("Cant find setter for property " + memberSpec + " on " + c);
            }
            else
            {
                string fn = memberSpec.Name;
                MethodInfo mi = findMethod(memberSpec, -1, c) ??
                                GetMethod(c, "set_" + fn, BindingFlagsALL) ??
                                GetMethod(c, "Set" + fn, BindingFlagsALL) ??
                                GetMethod(c, "from" + fn, BindingFlagsALL);
                if (mi == null)
                {
                    WarnMissing("Cant find setter " + memberSpec + " on " + c);
                    return false;
                }
                Action postCallHook;
                object[] value = PlListToCastedArray(valueIn, mi.GetParameters(), out postCallHook);
                object target = mi.IsStatic ? null : getInstance;
                object retval = InvokeCaught(mi, target, value, postCallHook);
                return true;// valueOut.FromObject(retval);
            }
            WarnMissing("Cant find setter " + memberSpec + " on " + c);
            return false;
        }


        /// <summary>
        /// 1 ?- cliToString(-1,X).
        /// X = "4294967295".
        /// </summary>
        /// <param name="obj"></param>
        /// <param name="str"></param>
        /// <returns></returns>
        [PrologVisible(ModuleName = ExportModule)]
        public static bool cliToStringRaw(PlTerm obj, PlTerm str)
        {
            try
            {
                if (!str.IsVar)
                {
                    var plvar = PlTerm.PlVar();
                    cliToStringRaw(obj, plvar);
                    return SpecialUnify(str, plvar);
                }
                if (obj.IsString) return str.Unify(obj);
                if (obj.IsVar) return str.Unify((string)obj);
                object o = GetInstance(obj);
                if (o == null) return str.FromObject("" + obj);
                return str.FromObject("" + o);
            }
            catch (Exception e)
            {
                Warn("cliToString: {0}", e);
                object o = GetInstance(obj);
                if (o == null) return str.FromObject("" + obj);
                return str.FromObject("" + o);
            }
        }

        [PrologVisible(ModuleName = ExportModule)]
        static public bool cliJavaToString(PlTerm valueIn, PlTerm valueOut)
        {
            if (!valueOut.IsVar)
            {
                var plvar = PlTerm.PlVar();
                cliJavaToString(valueIn, plvar);
                return SpecialUnify(valueOut, plvar);
            }
            object getInstance = GetInstance(valueIn);
            if (getInstance == null) return valueOut.Unify(PlTerm.PlString("null"));
            var val = getInstance as java.lang.Object;
            if (val == null)
            {
                Class c = ikvm.runtime.Util.getClassFromObject(getInstance);
                string s = (string)c.getMethod("toString", new Class[0]).invoke(getInstance, ZERO_OBJECTS);
                return valueOut.Unify(PlTerm.PlString(s));
            }
            return valueOut.Unify(PlTerm.PlString(val.toString()));
        }

        [PrologVisible(ModuleName = ExportModule)]
        static public bool cliPropsForType(PlTerm clazzSpec, PlTerm memberSpecs)
        {
            Type type = GetType(clazzSpec);
            var props = GetPropsForTypes(type);
            var value = props.Key.ToArray();
            int len = value.Length;
            var termv = ATOM_NIL;
            for (int i = len - 1; i >= 0; i--)
            {
                termv = PlC(".", ToProlog((value[i].Name)), termv);
            }
            var value2 = props.Value.ToArray();
            len = value2.Length;
            for (int i = len - 1; i >= 0; i--)
            {
                termv = PlC(".", ToProlog((value2[i].Name)), termv);
            }
            return memberSpecs.Unify(termv);
        }
        static readonly Dictionary<Type, KeyValuePair<List<PropertyInfo>, List<FieldInfo>>> PropForTypes = new Dictionary<Type, KeyValuePair<List<PropertyInfo>, List<FieldInfo>>>();

        private static KeyValuePair<List<PropertyInfo>, List<FieldInfo>> GetPropsForTypes(Type t)
        {
            KeyValuePair<List<PropertyInfo>, List<FieldInfo>> kv;

            if (PropForTypes.TryGetValue(t, out kv)) return kv;

            lock (PropForTypes)
            {
                if (!PropForTypes.TryGetValue(t, out kv))
                {
                    kv = new KeyValuePair<List<PropertyInfo>, List<FieldInfo>>(new List<PropertyInfo>(),
                                                                               new List<FieldInfo>());
                    var ta = t.GetCustomAttributes(typeof(XmlTypeAttribute), false);
                    bool specialXMLType = false;
                    if (ta != null && ta.Length > 0)
                    {
                        XmlTypeAttribute xta = (XmlTypeAttribute)ta[0];
                        specialXMLType = true;
                    }
                    HashSet<string> lowerProps = new HashSet<string>();
                    BindingFlags flags = BindingFlags.Instance | BindingFlags.Public; //BindingFlags.NonPublic
                    foreach (
                        PropertyInfo o in t.GetProperties(flags))
                    {
                        if (o.CanRead)
                        {

                            if (o.Name.StartsWith("_")) continue;
                            if (o.DeclaringType == typeof(Object)) continue;
                            if (!lowerProps.Add(o.Name.ToLower())) continue;
                            if (o.GetIndexParameters().Length > 0)
                            {
                                continue;
                            }
                            if (specialXMLType)
                            {
                                var use = o.GetCustomAttributes(typeof(XmlArrayItemAttribute), false);
                                if (use == null || use.Length < 1) continue;
                            }
                            kv.Key.Add(o);

                        }
                    }
                    foreach (FieldInfo o in t.GetFields(flags))
                    {
                        if (o.Name.StartsWith("_")) continue;
                        if (o.DeclaringType == typeof(Object)) continue;
                        if (!lowerProps.Add(o.Name.ToLower())) continue;
                        if (specialXMLType)
                        {
                            var use = o.GetCustomAttributes(typeof(XmlArrayItemAttribute), false);
                            if (use == null || use.Length < 1) continue;
                        }
                        kv.Value.Add(o);
                    }
                }
                return kv;
            }
        }


        private void Trace()
        {
            //throw new NotImplementedException();
        }

        private object ToFort(object o)
        {
            return ToProlog(o);
        }

        private static bool SpecialUnify(PlTerm valueOut, PlTerm plvar)
        {
            bool b = valueOut.Unify(plvar);
            if (b) return true;
            object obj1 = GetInstance(plvar);
            if (ReferenceEquals(obj1, null))
            {
                return false;
            }
            Type t1 = obj1.GetType();
            object obj2 = CastTerm(valueOut, t1);
            if (ReferenceEquals(obj2, null))
            {
                return false;
            }
            Type t2 = obj2.GetType();
            if (obj1.Equals(obj2))
            {
                return true;
            }
            if (t1 == t2)
            {
                return false;
            }
            return false;
        }

        public static Object ToFromConvertLock = new object();
        public static int UnifyToProlog(object o, PlTerm term)
        {
            if (!term.IsVar)
            {
                Warn("Not a free var {0}", term);
                return libpl.PL_fail;
            }
            uint TermRef = term.TermRef;
            if (TermRef == 0)
            {
                Warn("Not a allocated term {0}", o);
                return libpl.PL_fail;
            }
            if (o is PlTerm)
            {
                return libpl.PL_unify(TermRef, ((PlTerm)o).TermRef);
            }
            if (o is Term) return UnifyToProlog(ToPLCS((Term)o), term);
            if (PreserveObjectType)
            {
                return PlSucceedOrFail(UnifyTagged(o, term));
            }
            if (o is string)
            {
                string s = (string)o;
                switch (VMStringsAsAtoms)
                {
                    case libpl.CVT_STRING:
                        {
                            try
                            {
                                return libpl.PL_unify_string_chars(TermRef, (string)o);
                            }
                            catch (Exception)
                            {

                                return UnifyAtom(TermRef, s);
                            }
                        }
                    case libpl.CVT_ATOM:
                        try
                        {
                            return libpl.PL_unify_atom_chars(TermRef, (string)o);
                        }
                        catch (Exception)
                        {

                            return UnifyAtom(TermRef, s);
                        }
                    case libpl.CVT_LIST:
                        return libpl.PL_unify_list_chars(TermRef, (string)o);
                    default:
                        Warn("UNKNOWN VMStringsAsAtoms {0}", VMStringsAsAtoms);
                        return libpl.PL_fail;
                }
            }
            if (o == null)
            {
                return AddTagged(TermRef, "null");
            }

            if (o is Type || o is Class)
            {
                if (true)
                {
                    lock (ToFromConvertLock)
                    {
                        var tag = object_to_tag(o);
                        AddTagged(TermRef, tag);
                        return libpl.PL_succeed;
                    }
                }
                return PlSucceedOrFail(term.Unify(typeToSpec((Type)o)));
            }

            Type t = o.GetType();
            if (t == typeof(void))
            {
                return AddTagged(TermRef, "void");
            }
            if (o is ValueType)
            {
                if (o is bool)
                {
                    bool tf = (bool)o;
                    return AddTagged(TermRef, tf ? "true" : "false");
                }
                if (o is char)
                {
                    try
                    {
                        char ch = (char)o;
                        string cs = new string(ch, 1);
                        switch (VMStringsAsAtoms)
                        {
                            case libpl.CVT_STRING:
                                return libpl.PL_unify_atom_chars(TermRef, cs);
                            case libpl.CVT_ATOM:
                                return libpl.PL_unify_atom_chars(TermRef, cs);
                            case libpl.CVT_LIST:
                                return libpl.PL_unify_integer(TermRef, (int)ch);
                            default:
                                Warn("UNKNOWN VMStringsAsAtoms {0}", VMStringsAsAtoms);
                                return libpl.PL_fail;
                        }
                    }
                    catch (Exception e)
                    {
                        Warn("@TODO unmappable errors? {0} type {1}", o, t);
                        //
                    }
                }
                if (t.IsEnum)
                {
                    int res = FromEnum(TermRef, o, t);
                    term.ToString();
                    return res;
                }
                if (t.IsPrimitive)
                {
                    try
                    {
                        int res = ToVMNumber(o, term);
                        if (res == libpl.PL_succeed) return res;
                        if (res == libpl.PL_fail) return res;
                        if (res != -1)
                        {
                            // Warn("@TODO Missing code for ToVmNumber? " + o + " type " + t);
                            return res;
                        }
                        if (t.IsPrimitive)
                        {
                            Warn("@TODO Missing code for primitive? {0} type {1}", o, t);
                        }
                    }
                    catch (Exception e)
                    {
                        Warn("@TODO unmappable errors? {0} type {1}", o, t);
                    }
                }
            }
            lock (FunctorToLayout)
            {
                PrologTermLayout layout;
                if (TypeToLayout.TryGetValue(t, out layout))
                {
                    MemberInfo[] tGetFields = layout.FieldInfos;// GetStructFormat(t);
                    int len = tGetFields.Length;
                    PlTermV tv = NewPlTermV(len);
                    for (int i = 0; i < len; i++)
                    {
                        object v = GetMemberValue(tGetFields[i], o);
                        tv[i].FromObject((v));
                    }
                    return PlSucceedOrFail(term.Unify(PlC(layout.Name, tv)));
                }
            }
            lock (FunctorToRecomposer)
            {
                PrologTermRecomposer layout = GetTypeMap(t, TypeToRecomposer);
                if (layout != null)
                {
                    lock (ToFromConvertLock)
                    {
                        var tag = object_to_tag(o);
                        uint newref = libpl.PL_new_term_refs(2);
                        AddTagged(newref, tag);
                        PlTerm into = new PlTerm(newref);
                        PlTerm outto = new PlTerm(newref + 1);
                        var ret = PlQuery.PlCall(layout.module, layout.obj2r, new PlTermV(into, outto));
                        if (ret)
                        {
                            return term.Unify(outto) ? libpl.PL_succeed
                                   : libpl.PL_fail;

                        }
                    }
                }
            }
            if (o is IList)
            {

            }
            if (IsStructRecomposable(t))
            {
                return ToFieldLayout("struct", typeToName(t), o, t, term, false, false);
            }
            if (o is EventArgs)
            {
                return ToFieldLayout("event", typeToName(t), o, t, term, false, false);
            }
            lock (ToFromConvertLock)
            {
                var tag = object_to_tag(o);
                AddTagged(TermRef, tag);
                return libpl.PL_succeed;
#if plvar_pins
                PlRef oref;
                if (!objectToPlRef.TryGetValue(o, out oref))
                {
                    objectToPlRef[o] = oref = new PlRef();
                    oref.Value = o;
                    oref.CSType = o.GetType();
                    oref.Tag = tag;
                    lock (atomToPlRef)
                    {
                        PlRef oldValue;
                        if (atomToPlRef.TryGetValue(tag, out oldValue))
                        {
                            Warn("already a value for tag=" + oldValue);
                        }
                        atomToPlRef[tag] = oref;
                    }
#if PLVARBIRTH
                    Term jplTerm = JPL.newJRef(o);
                    oref.JPLRef = jplTerm;

                    Int64 ohandle = TopOHandle++;
                    oref.OHandle = ohandle;
                    // how do we track the birthtime?
                    var plvar = oref.Variable = PlTerm.PlVar();
                    lock (termToObjectPins)
                    {
                        PlRef oldValue;
                        if (termToObjectPins.TryGetValue(ohandle, out oldValue))
                        {
                            Warn("already a value for ohandle=" + oldValue);
                        }
                        termToObjectPins[ohandle] = oref;
                    }
                    //PL_put_integer
                    oref.Term = comp("$cli_object", new PlTerm((long) ohandle), plvar);
#else
                    oref.Term = comp("@", PlTerm.PlAtom(tag));
#endif
                    return -1; // oref.Term;
                }
                else
                {
                    oref.Term = comp("@", PlTerm.PlAtom(tag));
                    return -1; // oref.Term;
                }
#endif // plvar_pins
            }
        }

        public static int PlSucceedOrFail(bool p)
        {
            return p ? libpl.PL_succeed : libpl.PL_fail;
        }

        public static T GetTypeMap<T>(Type t, IDictionary<Type, T> mapped)
        {
            T layout;
            if (mapped.TryGetValue(t, out layout))
            {
                return layout;
            }

            Type bestType = null;
            foreach (KeyValuePair<Type, T> map in mapped)
            {
                Type mapKey = map.Key;
                if (mapKey.IsAssignableFrom(t))
                {
                    if (bestType == null || mapKey.IsAssignableFrom(bestType))
                    {
                        bestType = mapKey;
                        layout = map.Value;
                    }
                }
            }
            return layout;// default(T);
        }

        public static PlTerm C(string collection)
        {
            return PlTerm.PlAtom(collection);
        }

        private static int FromEnum(uint TermRef, object o, Type t)
        {
            uint temp = libpl.PL_new_term_ref();
            libpl.PL_cons_functor_v(temp,
                                    ENUM_2,
                                    new PlTermV(typeToSpec(t), PlTerm.PlAtom(o.ToString())).A0);
            return libpl.PL_unify(TermRef, temp);
        }

        protected static uint ENUM_2
        {
            get
            {
                if (_enum2 == 0)
                {
                    _enum2 = libpl.PL_new_functor(libpl.PL_new_atom("enum"), 2);
                }
                return _enum2;
            }
        }

        private static bool IsStructRecomposable(Type t)
        {
            return t.IsValueType && !t.IsEnum && !t.IsPrimitive &&
                   (!t.Namespace.StartsWith("System") || t == typeof(DateTime)) &&
                   !typeof(IEnumerator).IsAssignableFrom(t) &&
                   !typeof(ICloneable).IsAssignableFrom(t) &&
                   !typeof(IEnumerable).IsAssignableFrom(t) &&
                   !typeof(ICollection).IsAssignableFrom(t);
        }

        private static int AddTagged(uint TermRef, string tag)
        {
            /*
            PlTerm term2 = new PlTerm(TermRef);
            var t1 = term2;
            if (t1.IsCompound)
            {
                t1 = t1[1];
            }
            else if (t1.IsVar)
            {
            }
            //var t2 = new PlTerm(t1.TermRef + 1);

            //libpl.PL_put_atom_chars(t1.TermRef + 1, tag);
            bool ret = t1.Unify(tag); // = t1;*/
            uint nt = libpl.PL_new_term_ref();
            libpl.PL_cons_functor_v(nt,
                                    OBJ_1,
                                    new PlTermV(PlTerm.PlAtom(tag)).A0);
            return libpl.PL_unify(TermRef, nt);


        }

        protected static uint OBJ_1
        {
            get
            {
                if (_obj1 == default(uint))
                {
                    _obj1 = libpl.PL_new_functor(libpl.PL_new_atom("@"), 1);
                }
                return _obj1;
            }
        }

    }
#if plvar_pins
    public class PlRef
    {
        public object Value;
        public PlTerm Term;
        public Int64 OHandle;
        public PlTerm Variable;
        public Type CSType;
        public Term JPLRef;
        public string Tag;
    }
#endif
}