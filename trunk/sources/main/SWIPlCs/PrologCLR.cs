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
using MushDLR223.Utilities;
using SbsSW.SwiPlCs.Callback;
using SbsSW.SwiPlCs.Exceptions;
using SbsSW.SwiPlCs.Streams;
using System.Windows.Forms;
using Hashtable = java.util.Hashtable;
using ClassLoader = java.lang.ClassLoader;
using Class = java.lang.Class;
using sun.reflect.misc;
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


        public static BindingFlags BindingFlagsJustStatic = BindingFlags.Public | BindingFlags.NonPublic |
                                                            BindingFlags.Static;
        public static BindingFlags BindingFlagsInstance = BindingFlags.Public | BindingFlags.NonPublic |
                                                            BindingFlags.Static;
        public static BindingFlags BindingFlagsALL = BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Static |
                                                     BindingFlags.Instance | BindingFlags.IgnoreCase;
        public static BindingFlags InstanceFields = BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.IgnoreCase;
        

        const string ExportModule = "clipl";



        private static Type[] GetParamSpec(PlTerm memberSpec)
        {
            if (memberSpec.IsInteger)
            {
                Type[] lenType = new Type[memberSpec.intValue()];
                for (int i = 0; i < lenType.Length; i++)
                {
                    lenType[i] = typeof (object);
                }
                return lenType;
            }
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
                Warn("findEventInfo IsVar " + memberSpec + " on type " + c);
                return null;
            }
            if (memberSpec.IsInteger)
            {
                int ordinal = memberSpec.intValue();
                var mis = c.GetEvents(BindingFlagsALL);
                if (ordinal < 0 || ordinal >= mis.Length) return null;
                return mis[ordinal];
            }
            if (TaggedObject(memberSpec))
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
            if (TaggedObject(memberSpec))
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
                Warn("findField no class for " + memberSpec);
                return null;
            }
            if (memberSpec.IsVar)
            {
                Warn("findField IsVar " + memberSpec + " on type " + c);
                return null;
            }
            if (memberSpec.IsInteger)
            {
                int ordinal = memberSpec.intValue();
                var mis = c.GetFields(BindingFlagsALL);
                if (ordinal < 0 || ordinal >= mis.Length) return null;
                return mis[ordinal];
            }
            if (TaggedObject(memberSpec))
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
                Warn("findProperty no class for " + memberSpec);
                return null;
            }
            if (memberSpec.IsVar)
            {
                Warn("findProperty IsVar " + memberSpec + " on type " + c);
                return null;
            }
            if (memberSpec.IsInteger)
            {
                int ordinal = memberSpec.intValue();
                var mis = c.GetProperties(BindingFlagsALL);
                if (ordinal < 0 || ordinal >= mis.Length) return null;
                return mis[ordinal];
            }
            if (TaggedObject(memberSpec))
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
                Warn("findMethod no class for " + memberSpec);
                return null;
            }
            if (memberSpec.IsVar)
            {
                Warn("findMethod IsVar " + memberSpec + " on type " + c);
                return null;
            }
            if (memberSpec.IsInteger)
            {
                var mis = c.GetMethods(BindingFlagsALL);
                return mis[memberSpec.intValue()];
            }
            if (TaggedObject(memberSpec))
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
            if (arity < 0) arity = memberSpec.Arity;
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
                Warn("findConstructor no class for " + memberSpec);
                return null;
            }
            if (TaggedObject(memberSpec))
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

        private static bool TaggedObject(PlTerm info)
        {
            return info.IsCompound && info.Name == "@";
        }

        private static Type GetArityType(int paramlen)
        {
            switch (paramlen)
            {
                case 0:
                    return typeof(DelegateParameter0);
                case 1:
                    return typeof(DelegateParameter1);
                case 2:
                    return typeof(DelegateParameter2);
                case 3:
                    return typeof(DelegateParameter3);
                case 4:
                    return typeof(DelegateParameter3);
                default:
                    return null;
            }
        }

        public static Type GetType(PlTerm clazzSpec)
        {
            if (clazzSpec.IsVar)
            {
                Warn("GetType IsVar " + clazzSpec);
                return null;
            }
            if (TaggedObject(clazzSpec))
            {
                object tagObj = tag_to_object(clazzSpec[1].Name);
                var r = tagObj as Type;
                if (r != null) return r;
                Warn("cant find tagged object as class: " + clazzSpec + "=>" + tagObj);
                if (tagObj != null)
                {
                    return tagObj.GetType();
                }
                return null;
            }
            Type type = null;
            if (clazzSpec.IsAtom || clazzSpec.IsString)
            {
                string name = (string)clazzSpec;
                type = ResolveType(name);
                if (type != null) return type;
                Warn("cant find atom/string as class: " + clazzSpec);
                return null;
            }
            if (clazzSpec.IsCompound)
            {
                string clazzName = clazzSpec.Name;
                int arity = clazzSpec.Arity;
                if (clazzName == "arrayOf")
                {
                    return GetType(clazzSpec[1]).MakeArrayType();
                }
                if (clazzName == "static")
                {
                    return GetType(clazzSpec[1]);
                } 
                type = ResolveType(clazzName + "`" + arity);
                if (type != null)
                {
                    // 'Dictionary'('Int32','string').
                    if (type.IsGenericType)
                    {
                        Type[] genr = type.GetGenericArguments();
                        Type[] genrc = null;
                        Type genrb = null;
                        try
                        {
                            if (type.IsGenericParameter)
                            {
                                genrc = type.GetGenericParameterConstraints();
                            }
                        }
                        catch (Exception e)
                        {
                            Warn("GetGenericParameterConstraints: " + e);
                        }
                        try
                        {
                            genrb = type.GetGenericTypeDefinition();
                        }
                        catch (Exception e)
                        {
                            Warn("GetGenericTypeDefinition: " + e);
                        }

                        if (arity == genr.Length)
                        {
                            var vt = GetParamSpec(clazzSpec);
                            return type.MakeGenericType(vt);
                        }
                    }
                    //  return type;
                }
                Warn("cant find compound as class: " + clazzSpec);
            }
            object toObject = GetInstance(clazzSpec);
            if (toObject is Type) return (Type)toObject;
            if (toObject != null)
            {
                return toObject.GetType();
            }
            Warn("@TODO cant figure type from " + clazzSpec);
            return typeof(object);
            //return null;
        }

        [PrologVisible(ModuleName = ExportModule)]
        public static bool cliFindType(PlTerm term1, PlTerm term2)
        {
            //            if (term1.IsAtom)
            {
                string className = (string)term1;//.Name;
                Type s1 = GetType(term1);
                if (s1 != null)
                {
                    var c = s1;// ikvm.runtime.Util.getFriendlyClassFromType(s1);
                    if (c != null)
                    {
                        Console.WriteLine("name:" + className + " type:" + s1.FullName + " class:" + c);
                        string tag = object_to_tag(c);
                        var t1 = term2;
                        if (t1.IsCompound)
                        {
                            t1 = t1[1];
                        }
                        else if (t1.IsVar)
                        {
                            return 0 != AddTagged(t1.TermRef, tag);
                        }
                        //var t2 = new PlTerm(t1.TermRef + 1);

                        //libpl.PL_put_atom_chars(t1.TermRef + 1, tag);
                        bool ret = t1.UnifyAtom(tag); // = t1;
                        return ret;
                    }
                    Console.WriteLine("cant getFriendlyClassFromType " + s1.FullName);
                    return false;
                }
                Console.WriteLine("cant ResolveType " + className);
                return false;
            }
            Console.WriteLine("cant IsAtom " + term1);
            return false;
        }

        [PrologVisible(ModuleName = ExportModule)]
        public static bool cliFindClass(PlTerm clazzName, PlTerm clazzObjectOut)
        {
            if (clazzName.IsAtom)
            {
                string className = clazzName.Name;
                Class c = ResolveClass(className);
                if (c != null)
                {
                    Console.WriteLine("cliFindClass:" + className + " class:" + c);
                    string tag = object_to_tag(c);
                    return AddTagged(clazzObjectOut.TermRef, tag) != 0;
                }
                Console.WriteLine("cant ResolveClass " + className);
                return false;
            }
            Type t = GetType(clazzName);
            if (t != null)
            {
                Class c = ikvm.runtime.Util.getFriendlyClassFromType(t);
                string tag = object_to_tag(c);
                return AddTagged(clazzObjectOut.TermRef, tag) != 0;
            }
            return false;
        }


        /// <summary>
        /// cliLoadAssembly('SwiPlCs.dll').
        /// cliLoadAssembly('Cogbot.exe').
        /// </summary>
        /// <param name="term1"></param>
        /// <returns></returns>
        public static bool cliLoadAssembly(PlTerm term1)
        {
            try
            {
                return cliLoadAssemblyUncaught(term1);
            }
            catch (Exception e)
            {
                Warn("cliLoadAssembly: " + term1 + " caused " + e);
                return false;
            }
        }
        public static bool cliLoadAssemblyUncaught(PlTerm term1)
        {
            try
            {
                if (term1.IsVar)
                {
                    Warn("cliLoadAssembly IsVar " + term1);
                    return false;
                }
                if (TaggedObject(term1))
                {
                    var assemblyOrType = GetInstance(term1);
                    if (assemblyOrType is Assembly)
                    {
                        return ResolveAssembly((Assembly) assemblyOrType);
                    }
                    if (assemblyOrType is Type)
                    {
                        return ResolveAssembly(((Type) assemblyOrType).Assembly);
                    }
                    return Warn("Cannot get assembly from " + assemblyOrType + " for " + term1);
                }
                string name = term1.Name;
                Assembly assembly = null;
                try
                {
                    assembly = Assembly.Load(name);
                    if (assembly != null) return ResolveAssembly(assembly);
                }
                catch (Exception e)
                {
                    //Warn("Load: " + name + " caused " + e);
                }
                var fi = new FileInfo(name);
                if (fi.Exists)
                {
                    string fiFullName = fi.FullName;
                    try
                    {
                        assembly = Assembly.LoadFile(fiFullName);
                        if (assembly != null) return ResolveAssembly(assembly);
                    }
                    catch (Exception e)
                    {
                        Warn("LoadFile: " + fiFullName + " caused " + e);
                    }
                }
                try
                {
                    if (assembly == null) assembly = Assembly.LoadWithPartialName(name);                    
                }
                catch (Exception e)
                {
                    Warn("LoadWithPartialName: " + name + " caused " + e);
                }
                if (assembly == null) return Warn("Cannot get assembly from " + name + " for " + term1);
                return ResolveAssembly(assembly);

            }
            catch (Exception exception)
            {
                throw ToPlException(exception);
            }
            return true;
        }

        [PrologVisible(ModuleName = ExportModule)]
        private static object[] PlListToCastedArray(IEnumerable<PlTerm> term, ParameterInfo[] paramInfos)
        {
            if (term is PlTerm)
            {
                PlTerm tlist = (PlTerm)term;
                term = tlist.Copy();
            }
            int len = paramInfos.Length;
            object[] ret = new object[len];
            int idx = 0;
            foreach (PlTerm arg in term)
            {
                ret[idx] = CastTerm(arg, paramInfos[idx].ParameterType);
                if (idx++ >= len) break;
            }
            return ret;
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
                        var fi = (EventInfo) info;
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
                        var fi = (PropertyInfo) info;
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


        private static IDictionary<string, Type> ShortNameType;
        private static readonly Dictionary<Type, string> TypeShortName = new Dictionary<Type, string>();
        private static PlTerm typeToSpec(Type type)
        {
            if (type.IsArray && type.HasElementType)
            {
                return PlC("arrayOf", typeToSpec(type.GetElementType()));
            }
            if (type.IsGenericParameter)
            {
                Type[] gt = type.GetGenericParameterConstraints();
                return PlC("<" + type.FullName ?? type.Name + ">", ToPlTermVSpecs(gt));
            }
            if (type.IsGenericType)
            {
                Type gt = type.GetGenericTypeDefinition();
                Type[] gtp = type.GetGenericArguments();
                PlTermV vt = ToPlTermVSpecs(gtp);
                string typeName = type.FullName ?? type.Name;
                int gtpLength = gtp.Length;
                int indexOf = typeName.IndexOf("`" + gtpLength);
                if (indexOf > 0)
                {
                    typeName = typeName.Substring(0, indexOf);
                } else
                {
                    Debug("cant chop arity " + gtpLength + " off string '" + typeName + "' ");
                }
                return PlC(typeName, vt);
            }
            return PlTerm.PlAtom(typeToName(type));
        }
        private static string typeToName(Type type)
        {
            if (type.IsArray && type.HasElementType)
            {
                return typeToSpec(type.GetElementType()) + "[]";
            }
            lock (ShortNameType)
            {
                string shortName;
                if (TypeShortName.TryGetValue(type, out shortName))
                {
                    return shortName;
                }
                string typeName = type.Name;
                Type otherType;
                if (ShortNameType.TryGetValue(type.Name, out otherType))
                {
                    if (type == otherType)
                    {
                        return typeName;
                    }
                    return type.FullName;
                }
                ShortNameType[typeName] = type;
                TypeShortName[type] = typeName;
                return typeName;
            }
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
                Warn("Cant resolve clazzSpec " + clazzSpec);
                return false;
            }
            ConstructorInfo mi = findConstructor(memberSpec, c);
            if (mi == null)
            {
                Warn("Cant find constructor " + memberSpec + " on " + c);
                return false;
            }
            object[] values = PlListToCastedArray(valueIn, mi.GetParameters());
            return valueOut.FromObject((mi.Invoke(values)));
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
                Warn("Cant find type " + clazzSpec);
                return false;
            }
            var value = c.MakeArrayType(rank.intValue());
            return valueOut.FromObject((value));
        }

        [PrologVisible(ModuleName = ExportModule)]
        static public bool cliFree(PlTerm taggedObject)
        {
            if (taggedObject.IsVar)
            {
                return false;
            }
            string tag;
            if (taggedObject.IsCompound)
            {
                tag = taggedObject[1].Name;
            }
            else if (taggedObject.IsAtom)
            {
                tag = taggedObject.Name;
            }
            else if (taggedObject.IsString)
            {
                tag = taggedObject.Name;
            }
            else
            {
                return true;
            }
            lock (TagToObj)
            {
                object obj;
                if (TagToObj.TryGetValue(tag, out obj))
                {
                    TagToObj.Remove(tag);
                    if (obj is IDisposable)
                    {
                        try
                        {
                            ((IDisposable)obj).Dispose();
                        }
                        catch (Exception e)
                        {
                            Warn("Dispose of " + obj + " had problem " + e);
                        }
                    }
                    return ObjToTag.Remove(obj);
                }
            }
            return false;
        }

        [PrologVisible(ModuleName = ExportModule)]
        static public bool cliGetTerm(PlTerm valueCol, PlTerm valueIn, PlTerm valueOut)
        {
            List<object> objs;
            if (valueCol.IsVar)
            {
                objs = new List<object>();
                valueCol.FromObject(objs);
            } else
            {
                objs = (List<object>) CastTerm(valueCol, typeof (ICollection));
            }
            if (!valueOut.IsVar)
            {
                var plvar = PlTerm.PlVar();
                cliGetTerm(valueCol, valueIn, plvar);
                return SpecialUnify(valueOut, plvar);
            }
            if (TaggedObject(valueIn))
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
            var value = getInstance as Array;
            if (value == null)
            {
                Warn("Cant find array from " + arrayValue + " as " + getInstance.GetType());
                return false;
            }
            int len = value.Length;
            var termv = NewPlTermV(len);
            for (int i = 0; i < len; i++)
            {
                bool pf = termv[i].FromObject((value.GetValue(i)));
            }
            Type et = value.GetType().GetElementType();
            return valueOut.Unify(PlC(typeToSpec(et).Name, termv));
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
            var value = getInstance as Array;
            if (value == null)
            {
                Warn("Cant find array from " + arrayValue + " as " + getInstance.GetType());
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
                Warn("Cant find vector from " + arrayValue);
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
        static public bool cliCall(PlTerm clazzOrInstance, PlTerm memberSpec, PlTerm valueIn, PlTerm valueOut)
        {
            if (!valueOut.IsVar)
            {
                var plvar = PlTerm.PlVar();
                cliCall(clazzOrInstance, memberSpec, valueIn, plvar);
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
                Warn("Cant find method " + memberSpec + " on " + c);
                return false;
            }
            object[] value = PlListToCastedArray(valueIn, mi.GetParameters());
            object target = mi.IsStatic ? null : getInstance;
            object retval = InvokeCaught(mi, target, value);
            return valueOut.FromObject(retval);
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
                return Warn("Cant find event " + memberSpec + " on " + c);
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
                        return valueOut.FromObject((del.DynamicInvoke(PlListToCastedArray(valueIn, paramInfos))));
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
                                return valueOut.FromObject((del.DynamicInvoke(PlListToCastedArray(valueIn, paramInfos))));
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
                Warn("Cant find event raising for  " + evi + " on " + c);
                return false;
            }
            object[] value = PlListToCastedArray(valueIn, mi.GetParameters());
            object target = mi.IsStatic ? null : getInstance;
            return valueOut.FromObject((InvokeCaught(mi, target, value)));
        }

        public static Dictionary<EventHandlerInPrologKey, EventHandlerInProlog> PrologEventHandlers =
            new Dictionary<EventHandlerInPrologKey, EventHandlerInProlog>();

        public static TaskQueueHandler PrologEventQueue = new TaskQueueHandler("PrologEventHandler");

        [PrologVisible(ModuleName = ExportModule)]
        static public bool cliAddEventHandler(PlTerm clazzOrInstance, PlTerm memberSpec, PlTerm prologPred)
        {
            object getInstance = GetInstance(clazzOrInstance);
            Type c = GetTypeFromInstance(getInstance, clazzOrInstance);
            EventInfo fi = findEventInfo(memberSpec, c);
            if (fi == null)
            {
                return Warn("Cant find event " + memberSpec + " on " + c);
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
                return Warn("Cant find event " + memberSpec + " on " + c);
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
            return Warn("Cant find registered handler " + prologPred + " for " + memberSpec + " on " + c);
        }

        [PrologVisible(ModuleName = ExportModule)]
        static public bool cliCast(PlTerm valueIn, PlTerm clazzSpec, PlTerm valueOut)
        {
            if (valueIn.IsVar)
            {
                return Warn("Cant find instance " + valueIn);
            }
            if (!valueOut.IsVar)
            {
                var plvar = PlTerm.PlVar();
                cliCast(valueIn, clazzSpec, plvar);
                return SpecialUnify(valueOut, plvar);
            }
            Type type = GetType(clazzSpec);
            object retval = CastTerm(valueIn, type);
            return valueOut.FromObject(retval);
        }

        [PrologVisible(ModuleName = ExportModule)]
        static public bool cliGetRaw(PlTerm clazzOrInstance, PlTerm memberSpec, PlTerm valueOut)
        {
            if (clazzOrInstance.IsVar)
            {
                return Warn("Cant find instance " + clazzOrInstance);
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
                Warn("Cant find instance " + clazzOrInstance);
                return false;
            }
            object cliGet01 = cliGet0(getInstance, memberSpec, c);
            return valueOut.FromObject(cliGet01);
        }
        static public object cliGet0(object getInstance, PlTerm memberSpec, Type c)
        {
            FieldInfo fi = findField(memberSpec, c);
            if (fi != null)
            {
                object fiGetValue = fi.GetValue(fi.IsStatic ? null : getInstance);
                return (fiGetValue);
            }
            var pi = findProperty(memberSpec, c);
            if (pi != null)
            {
                var mi = pi.GetGetMethod();
                if (mi != null)
                {
                    return ((InvokeCaught(mi, mi.IsStatic ? null : getInstance, new object[0])));
                }
                Warn("Cant find getter for property " + memberSpec + " on " + c + " for " + pi);
                return null;
            }
            else
            {
                string fn = memberSpec.Name;
                MethodInfo mi = findMethod(memberSpec, -1, c) ??
                                GetMethod(c, fn, BindingFlagsALL) ??
                                GetMethod(c, "get_" + fn, BindingFlagsALL) ??
                                GetMethod(c, "Get" + fn, BindingFlagsALL) ??
                                GetMethod(c, "Is" + fn, BindingFlagsALL) ??
                                GetMethod(c, "To" + fn, BindingFlagsALL);
                if (mi == null)
                {
                    Warn("Cant find getter " + memberSpec + " on " + c);
                    return null;
                }
                object[] value = PlListToCastedArray(memberSpec, mi.GetParameters());
                object target = mi.IsStatic ? null : getInstance;
                object retval = InvokeCaught(mi, target, value);
                return retval;
            }
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
                return Warn("Cant find setter for property " + memberSpec + " on " + c);
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
                    Warn("Cant find setter " + memberSpec + " on " + c);
                    return false;
                }
                object[] value = PlListToCastedArray(valueIn, mi.GetParameters());
                object target = mi.IsStatic ? null : getInstance;
                object retval = InvokeCaught(mi, target, value);
                return true;// valueOut.FromObject(retval);
            }
            Warn("Cant find setter " + memberSpec + " on " + c);
            return false;
        }

        [PrologVisible(ModuleName = ExportModule)]
        static public bool cliGetType(PlTerm valueIn, PlTerm valueOut)
        {
            if (!valueOut.IsVar)
            {
                var plvar = PlTerm.PlVar();
                cliGetType(valueIn, plvar);
                return SpecialUnify(valueOut, plvar);
            }
            object val = GetInstance(valueIn);
            if (val == null)
            {
                Warn("Cannot get object for " + valueIn);
                return true;
            }
            return valueOut.FromObject((val.GetType()));
        }

        [PrologVisible(ModuleName = ExportModule)]
        static public bool cliGetClass(PlTerm valueIn, PlTerm valueOut)
        {
            if (!valueOut.IsVar)
            {
                var plvar = PlTerm.PlVar();
                cliGetClass(valueIn, plvar);
                return SpecialUnify(valueOut, plvar);
            }
            object val = GetInstance(valueIn);
            // extension method
            return valueOut.FromObject((val.getClass()));
        }
        [PrologVisible(ModuleName = ExportModule)]
        static public bool cliClassFromType(PlTerm valueIn, PlTerm valueOut)
        {
            if (!valueOut.IsVar)
            {
                var plvar = PlTerm.PlVar();
                cliClassFromType(valueIn, plvar);
                return SpecialUnify(valueOut, plvar);
            }
            Type val = GetType(valueIn);
            if (val == null) return false;
            Class c = ikvm.runtime.Util.getFriendlyClassFromType(val);
            return valueOut.FromObject((c));
        }
        [PrologVisible(ModuleName = ExportModule)]
        static public bool cliTypeFromClass(PlTerm valueIn, PlTerm valueOut)
        {
            if (!valueOut.IsVar)
            {
                var plvar = PlTerm.PlVar();
                cliTypeFromClass(valueIn, plvar);
                return SpecialUnify(valueOut, plvar);
            }
            Class val = GetType(valueIn);
            if (val == null) return false;
            var c = ikvm.runtime.Util.getInstanceTypeFromClass(val);
            return valueOut.FromObject((c));
        }
        [PrologVisible(ModuleName = ExportModule)]
        static public bool cliShortType(PlTerm valueName, PlTerm valueIn)
        {
            if (!valueName.IsString && !valueName.IsAtom) return Warn("valueName must be string or atom " + valueName);
            string name = valueName.Name;
            Type otherType;
            lock (ShortNameType)
            {
                if (ShortNameType.TryGetValue(name, out otherType))
                {
                    if (valueIn.IsNumber)
                    {
                        ShortNameType.Remove(name);
                        TypeShortName.Remove(otherType);
                        return true;
                    }
                    if (valueIn.IsVar)
                    {
                        return valueIn.UnifyAtom(otherType.FullName);
                    }
                    Type val = GetType(valueIn);
                    if (val == otherType) return true;
                    return false;
                }
                else
                {
                    if (valueIn.IsNumber)
                    {
                        return true;
                    }
                    if (valueIn.IsVar)
                    {
                        return true;
                    }
                    Type val = GetType(valueIn);
                    if (val == null) return false;
                    ShortNameType[name] = val;
                    TypeShortName[val] = name;
                    return true;
                }
            }
        }
        [PrologVisible(ModuleName = ExportModule)]
        static public bool cliToTagged(PlTerm obj, PlTerm str)
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
            return AddTagged(str.TermRef, object_to_tag(o)) != 0;
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
                Warn("cliToString: " + e);
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
                string s = (string)c.getMethod("toString", new Class[0]).invoke(getInstance, new object[0]);
                return valueOut.Unify(PlTerm.PlString(s));
            }
            return valueOut.Unify(PlTerm.PlString(val.toString()));
        }
        [PrologVisible(ModuleName = ExportModule)]
        static public bool cliGetClassname(PlTerm valueIn, PlTerm valueOut)
        {
            if (!valueOut.IsVar)
            {
                var plvar = PlTerm.PlVar();
                cliGetClassname(valueIn, plvar);
                return SpecialUnify(valueOut, plvar);
            }
            Class val = CastTerm(valueIn, typeof(Class)) as Class;
            if (val == null) return false;
            return valueOut.Unify(val.getName());
        }
        [PrologVisible(ModuleName = ExportModule)]
        static public bool cliGetTypename(PlTerm valueIn, PlTerm valueOut)
        {
            if (!valueOut.IsVar)
            {
                var plvar = PlTerm.PlVar();
                cliGetTypename(valueIn, plvar);
                return SpecialUnify(valueOut, plvar);
            }
            Type val = CastTerm(valueIn, typeof(Type)) as Type;
            if (val == null) return false;
            return valueOut.Unify(val.FullName);
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

        [PrologVisible(ModuleName = ExportModule)]
        static public bool cliToFromLayout(PlTerm clazzSpec, PlTerm memberSpec, PlTerm toSpec)
        {
            Type type = GetType(clazzSpec);
            string name = memberSpec.Name;
            int arity = memberSpec.Arity;
            MemberInfo[] fieldInfos = new MemberInfo[arity];
            for (int i = 0; i < arity; i++)
            {
                var arg = memberSpec.Arg(i);
                fieldInfos[i] = findMember(arg, type);
            }
            var toMemb = findMember(toSpec, type);
            AddPrologTermLayout(type, name, fieldInfos, toMemb);
            return true;
        }

        [PrologVisible(ModuleName = ExportModule)]
        static public bool cliAddLayout(PlTerm clazzSpec, PlTerm memberSpec)
        {
            Type type = GetType(clazzSpec);
            string name = memberSpec.Name;
            int arity = memberSpec.Arity;
            MemberInfo[] fieldInfos = new MemberInfo[arity];
            for (int i = 0; i < arity; i++)
            {
                var arg = memberSpec.Arg(i);
                fieldInfos[i] = findMember(arg, type);
            }
            AddPrologTermLayout(type, name, fieldInfos, null);
            return true;
        }

        readonly private static Dictionary<string, PrologTermLayout> FunctorToLayout = new Dictionary<string, PrologTermLayout>();
        readonly private static Dictionary<Type, PrologTermLayout> TypeToLayout = new Dictionary<Type, PrologTermLayout>();

        static public void AddPrologTermLayout(Type type, string name, MemberInfo[] fieldInfos, MemberInfo toType)
        {
            PrologTermLayout layout = new PrologTermLayout();
            layout.FieldInfos = fieldInfos;
            layout.Name = name;
            layout.ObjectType = type;
            layout.ToType = toType;
            int arity = fieldInfos.Length;
            if (toType != null)
            {
                arity = 1;
            }
            layout.Arity = arity;
            lock (FunctorToLayout)
            {
                FunctorToLayout[name + "/" + arity] = layout;
                TypeToLayout[type] = layout;
            }
        }

        public static object GetInstance(PlTerm classOrInstance)
        {
            if (classOrInstance.IsVar)
            {
                Warn("GetInstance(PlVar) " + classOrInstance);
                return null;
            }
            if (!classOrInstance.IsCompound)
            {
                if (classOrInstance.IsAtom)
                {
                    Type t = GetType(classOrInstance);
                    // we do this for static invokations like: cliGet('java.lang.Integer','MAX_VALUE',...)
                    // the arg1 denotes a type, then return null!
                    if (t != null) return null;
                    Warn("GetInstance(atom) " + classOrInstance);
                    // possibly should always return null?!
                }
                else if (classOrInstance.IsString)
                {
                    Debug("GetInstance(string) " + classOrInstance);
                    return (string) classOrInstance;
                }
                else
                {
                    return CastTerm(classOrInstance, null);
                }
                return CastTerm(classOrInstance, null);
            }
            string name = classOrInstance.Name;
            int arity = classOrInstance.Arity;
            return CastCompoundTerm(name, arity, classOrInstance[1], classOrInstance, null);
        }

        /// <summary>
        /// Returns the Type when denoated by a 'namespace.type' (usefull for static instance specification)
        ///    if a @C#234234  the type of the object unless its a a class
        ///    c(a) => System.Char   "sdfsdf" =>  System.String   uint(5) => System.UInt32
        /// 
        ///    instanceMaybe maybe Null.. it is passed in so the method code doesn't have to call GetInstance again
        ///       on classOrInstance
        /// </summary>
        /// <param name="instanceMaybe"></param>
        /// <param name="classOrInstance"></param>
        /// <returns></returns>
        private static Type GetTypeFromInstance(object instanceMaybe, PlTerm classOrInstance)
        {
            if (classOrInstance.IsAtom)
            {
                return GetType(classOrInstance);
            }
            if (classOrInstance.IsString)
            {
                if (instanceMaybe != null) return instanceMaybe.GetType();
                return typeof(string);
            }
            if (classOrInstance.IsCompound)
            {
                if (classOrInstance.Name == "static")
                {
                    return GetType(classOrInstance[1]);
                }
            }

            object val = instanceMaybe ?? GetInstance(classOrInstance);
            if (val is Type) return (Type)val;
            if (val == null)
            {
                Warn("GetTypeFromInstance: " + classOrInstance);
                return null;
            }
            return val.GetType();
        }

        static System.Collections.IEnumerable Unfold(object value, out bool unFolded)
        {
            IList<object> results = new List<object>();
            var type = value.GetType();
            var utype = Enum.GetUnderlyingType(type);
            var values = Enum.GetValues(type);
            if (utype == typeof(byte) || utype == typeof(sbyte) || utype == typeof(Int16) || utype == typeof(UInt16) || utype == typeof(Int32))
            {
                unFolded = true;
                var num = (Int32)Convert.ChangeType(value, typeof(Int32));
                if (num == 0)
                {
                    results.Add(value);
                    return results;
                }
                foreach (var val in values)
                {
                    var v = (Int32)Convert.ChangeType(val, typeof(Int32));
                    if (v == 0) continue;
                    if ((v & num) == v)
                    {
                        results.Add(Enum.ToObject(value.GetType(), val));
                    }
                }
            }
            else if (utype == typeof(UInt32))
            {
                unFolded = true;
                var num = (UInt32)value;
                if (num == 0)
                {
                    results.Add(value);
                    return results;
                }
                foreach (var val in values)
                {
                    var v = (UInt32)Convert.ChangeType(val, typeof(UInt32));
                    if ((v & num) == v) results.Add(Enum.ToObject(value.GetType(), val));
                }
            }
            else if (utype == typeof(Int64))
            {
                unFolded = true;
                var num = (Int64)value;
                if (num == 0)
                {
                    results.Add(value);
                    return results;
                }
                foreach (var val in values)
                {
                    var v = (Int64)Convert.ChangeType(val, typeof(Int64));
                    if (v == 0L)
                    {
                        continue;
                    }
                    if ((v & num) == v) results.Add(Enum.ToObject(value.GetType(), val));
                }
            }
            else if (utype == typeof(UInt64))
            {
                unFolded = true;
                var num = (UInt64)value;
                if (num == 0)
                {
                    results.Add(value);
                    return results;
                }
                foreach (var val in values)
                {
                    var v = (UInt64)Convert.ChangeType(val, typeof(UInt64));
                    if (v == 0U)
                    {
                        continue;
                    }
                    if ((v & num) == v) results.Add(Enum.ToObject(value.GetType(), val));
                }
            }
            else
            {
                throw new NotSupportedException();
            }
            return results;
        }

        private delegate void WithEnum(CycFort p);
        private void ForEachEnumValue(WithEnum withValue, object p)
        {
            Type pType = p.GetType();
            if (!CycTypeInfo.IsFlagType(pType))
            {
                CycFort fort = (CycFort)ToFort(p);
                withValue(fort);
                return;
            }
            Array pTypeValues = System.Enum.GetValues(pType);
            Array.Reverse(pTypeValues);

            if (p is byte)
            {
                byte b = (byte)p;
                if (b == 0)
                {
                    withValue((CycFort)ToFort(p));
                    return;
                }
                foreach (object v in pTypeValues)
                {
                    byte bv = (byte)v;
                    if (bv >= b)
                    {
                        withValue((CycFort)ToFort(v));
                        b -= bv;
                    }
                    if (b == 0) return;
                }
                return;
            }
            if (p is sbyte)
            {
                sbyte b = (sbyte)p;
                if (b == 0)
                {
                    withValue((CycFort)ToFort(p));
                    return;
                }
                foreach (object v in pTypeValues)
                {
                    sbyte bv = (sbyte)v;
                    if (bv >= b)
                    {
                        withValue((CycFort)ToFort(v));
                        b -= bv;
                    }
                    if (b == 0) return;
                }
                return;
            }
            if (p is UInt16)
            {
                ushort b = (UInt16)p;
                if (b == 0)
                {
                    withValue((CycFort)ToFort(p));
                    return;
                }
                foreach (object v in pTypeValues)
                {
                    ushort bv = (ushort)v;
                    if (bv >= b)
                    {
                        withValue((CycFort)ToFort(v));
                        b -= bv;
                    }
                    if (b == 0) return;
                }
                return;
            }
            if (p is Int16)
            {
                short b = (Int16)p;
                if (b == 0)
                {
                    withValue((CycFort)ToFort(p));
                    return;
                }
                foreach (object v in pTypeValues)
                {
                    short bv = (short)v;
                    if (bv >= b)
                    {
                        withValue((CycFort)ToFort(v));
                        b -= bv;
                    }
                    if (b == 0) return;
                }
                return;
            }
            if (p is UInt32)
            {
                uint b = (UInt32)p;
                if (b == 0)
                {
                    withValue((CycFort)ToFort(p));
                    return;
                }
                foreach (object v in pTypeValues)
                {
                    uint bv = (uint)v;
                    if (bv >= b)
                    {
                        withValue((CycFort)ToFort(v));
                        b -= bv;
                    }
                    if (b == 0) return;
                }
                return;
            }
            if (p is Int32)
            {
                int b = (Int32)p;
                if (b == 0)
                {
                    withValue((CycFort)ToFort(p));
                    return;
                }
                foreach (object v in pTypeValues)
                {
                    int bv = (int)v;
                    if (bv >= b)
                    {
                        withValue((CycFort)ToFort(v));
                        b -= bv;
                    }
                    if (b == 0) return;
                }
                return;
            }
            if (p is UInt64)
            {
                ulong b = (UInt64)p;
                if (b == 0)
                {
                    withValue((CycFort)ToFort(p));
                    return;
                }
                foreach (object v in pTypeValues)
                {
                    ulong bv = (ulong)v;
                    if (bv >= b)
                    {
                        withValue((CycFort)ToFort(v));
                        b -= bv;
                    }
                    if (b == 0) return;
                }
                return;
            }
            if (p is Int64)
            {
                long b = (Int64)p;
                if (b == 0)
                {
                    withValue((CycFort)ToFort(p));
                    return;
                }
                foreach (object v in pTypeValues)
                {
                    long bv = (long)v;
                    if (bv >= b)
                    {
                        withValue((CycFort)ToFort(v));
                        b -= bv;
                    }
                    if (b == 0) return;
                }
                return;
            }
            string s = p.ToString();
            bool unfolded;
            foreach (var unfold in Unfold(p, out unfolded))
            {
                withValue((CycFort)ToFort(unfold));
                //return;
            }
            if (unfolded) return;
            Trace();
            if (p is IConvertible)
            {
                withValue((CycFort)ToFort(p));
                return;
            }

            if (p is Enum)
            {
                withValue((CycFort)ToFort(p));
                return;
            }
            withValue((CycFort)ToFort(p));
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
            if (t1 == t2)
            {
                return obj1.Equals(obj2);
            }
            if (obj1.Equals(obj2))
            {
                return true;
            }
            return false;
        }

        public static Object ToFromConvertLock = new object();
        public static int UnifyToProlog(object o, PlTerm term)
        {
            if (!term.IsVar)
            {
                Warn("Not a free var " + term);
                return libpl.PL_fail;
            }
            uint TermRef = term.TermRef;
            if (TermRef == 0)
            {
                Warn("Not a allocated term " + o);
                return libpl.PL_fail;
            }
            if (o is PlTerm) return libpl.PL_unify(TermRef, ((PlTerm) o).TermRef);
            if (o is Term) return UnifyToProlog(ToPLCS((Term) o), term);

            if (o is string)
            {
                string s = (string) o;
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

                                libpl.PL_put_atom(TermRef, libpl.PL_new_atom_wchars(s.Length, s));
                                return libpl.PL_succeed;
                            }
                        }
                    case libpl.CVT_ATOM:
                        try
                        {
                            return libpl.PL_unify_atom_chars(TermRef, (string)o);
                        }
                        catch (Exception)
                        {

                            libpl.PL_put_atom(TermRef, libpl.PL_new_atom_wchars(s.Length, s));
                            return libpl.PL_succeed;
                        }
                    case libpl.CVT_LIST:
                        return libpl.PL_unify_list_chars(TermRef, (string) o);
                    default:
                        Warn("UNKNOWN VMStringsAsAtoms " + VMStringsAsAtoms);
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
                return term.Unify(typeToSpec((Type) o)) ? libpl.PL_succeed : libpl.PL_fail;
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
                    bool tf = (bool) o;
                    return AddTagged(TermRef, tf ? "true" : "false");
                }
                if (o is char)
                {
                    try
                    {
                        char ch = (char) o;
                        string cs = new string(ch, 1);
                        switch (VMStringsAsAtoms)
                        {
                            case libpl.CVT_STRING:
                                return libpl.PL_unify_atom_chars(TermRef, cs);
                            case libpl.CVT_ATOM:
                                return libpl.PL_unify_atom_chars(TermRef, cs);
                            case libpl.CVT_LIST:
                                return libpl.PL_unify_integer(TermRef, (int) ch);
                            default:
                                Warn("UNKNOWN VMStringsAsAtoms " + VMStringsAsAtoms);
                                return libpl.PL_fail;
                        }
                    }
                    catch (Exception e)
                    {
                        Warn("@TODO unmappable errors? " + o + " type " + t);
                        //
                    }
                }
                if (t.IsEnum)
                {
                    return FromEnum(TermRef, o, t);
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
                            Warn("@TODO Missing code for primitive? " + o + " type " + t);
                        }
                    }
                    catch (Exception e)
                    {
                        Warn("@TODO unmappable errors? " + o + " type " + t);
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
                        object v = GetMemberValue(tGetFields[i],o);
                        tv[i].FromObject((v));
                    }
                    return term.Unify(PlC(layout.Name, tv)) ? libpl.PL_succeed : libpl.PL_fail;
                }
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

        public static PlTerm C(string collection)
        {
            return PlTerm.PlAtom(collection);
        }

        private static int FromEnum(uint TermRef, object o, Type t)
        {
            libpl.PL_cons_functor_v(TermRef,
                                    libpl.PL_new_functor(libpl.PL_new_atom("enum"), 2),
                                    new PlTermV(typeToSpec(t), PlTerm.PlAtom(o.ToString())).A0);
            return libpl.PL_succeed;
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
                                    libpl.PL_new_functor(libpl.PL_new_atom("@"), 1),
                                    new PlTermV(PlTerm.PlAtom(tag)).A0);
            return libpl.PL_unify(TermRef, nt);


        }

        private static MemberInfo[] GetStructFormat(Type t)
        {
            if (false)
            {
                lock (FunctorToLayout)
                {
                    PrologTermLayout layout;
                    if (TypeToLayout.TryGetValue(t, out layout))
                    {
                        return layout.FieldInfos;
                    }
                }
            }
            bool specialXMLType = false;
            if (!t.IsEnum)
            {
                var ta = t.GetCustomAttributes(typeof(XmlTypeAttribute), false);
                if (ta != null && ta.Length > 0)
                {
                    XmlTypeAttribute xta = (XmlTypeAttribute)ta[0];
                    specialXMLType = true;
                }
            }
            MemberInfo[] tGetFields = null;
            if (specialXMLType)
            {
                List<MemberInfo> fis = new List<MemberInfo>();
                foreach (var e in t.GetFields(InstanceFields))
                {
                    var use = e.GetCustomAttributes(typeof(XmlArrayItemAttribute), false);
                    if (use == null || use.Length < 1)
                    {
                        continue;
                    }
                    fis.Add(e);
                }
                foreach (var e in t.GetProperties(InstanceFields))
                {
                    var use = e.GetCustomAttributes(typeof(XmlArrayItemAttribute), false);
                    if (use == null || use.Length < 1)
                    {
                        continue;
                    }
                    fis.Add(e);
                }
                tGetFields = fis.ToArray();
            }
            else
            {
                // look for [StructLayout(LayoutKind.Sequential)]

                var ta = t.GetCustomAttributes(typeof (StructLayoutAttribute), false);
                if (ta != null && ta.Length > 0)
                {
                    StructLayoutAttribute xta = (StructLayoutAttribute)ta[0];
                    // ReSharper disable ConditionIsAlwaysTrueOrFalse
                    if (xta.Value == LayoutKind.Sequential || true /* all sequential layouts*/)
                    // ReSharper restore ConditionIsAlwaysTrueOrFalse
                    {
                        tGetFields = t.GetFields(InstanceFields);
                    }
                }
                if (tGetFields == null)
                    tGetFields = t.GetFields(InstanceFields);
            }
            if (tGetFields.Length == 0)
            {
                Warn("No fields in " + t);
            }
            return tGetFields;
        }


        public static int ToFieldLayout(string named, string arg1, object o, Type t, PlTerm term, bool childs, bool addNames)
        {

            MemberInfo[] tGetFields = GetStructFormat(t);

            int len = tGetFields.Length;
            PlTermV tv = NewPlTermV(len + 1);
            tv[0].UnifyAtom(arg1);
            int tvi = 1;
            for (int i = 0; i < len; i++)
            {
                object v = GetMemberValue(tGetFields[i], o);
                tv[tvi++].FromObject((v));
            }
            if (true)
            {
                return term.Unify(PlC(named, tv)) ? libpl.PL_succeed : libpl.PL_fail;
            }
            uint termTermRef = term.TermRef;

            libpl.PL_cons_functor_v(termTermRef,
                                    libpl.PL_new_functor(libpl.PL_new_atom(named), tv.Size),
                                    tv.A0);
            return libpl.PL_succeed;
        }

        private static int ToVMNumber(object o, PlTerm term)
        {

            // signed types
            if (o is short || o is sbyte || o is int)
                return libpl.PL_unify_integer(term.TermRef, (int)Convert.ToInt32(o));
            if (o is long)
                return libpl.PL_unify_integer(term.TermRef, (long)Convert.ToInt64(o));
            if (o is decimal || o is Single || o is float || o is double)
                return libpl.PL_unify_float(term.TermRef, (double)Convert.ToDouble(o));
            // unsigned types
            if (o is ushort || o is byte )
                return libpl.PL_unify_integer(term.TermRef, (int)Convert.ToInt32(o));
            if (o is UInt32)
                return libpl.PL_unify_integer(term.TermRef, (long)Convert.ToInt64(o));
            // potentually too big?!
            if (o is ulong)
            {
                ulong u64 = (ulong)o;
                if (u64 <= Int64.MaxValue)
                {
                    return libpl.PL_unify_integer(term.TermRef, (long)Convert.ToInt64(o));
                }
                return term.Unify(u64) ? libpl.PL_succeed : libpl.PL_fail;
                //return libpl.PL_unify_float(term.TermRef, (double)Convert.ToDouble(o));
            }
            if (o is IntPtr)
            {
                return libpl.PL_unify_intptr(term.TermRef, (IntPtr) o);
            }
            if (o is UIntPtr)
            {
                return libpl.PL_unify_intptr(term.TermRef, (IntPtr) o);
            }
            return -1;
        }

        /*
         
  jpl_is_ref(@(Y)) :-
	atom(Y),        % presumably a (garbage-collectable) tag
	Y \== void,     % not a ref
	Y \== false,    % not a ref
	Y \== true.     % not a ref
         
         */
        private static object CastCompoundTerm(string name, int arity, PlTerm arg1, PlTerm orig, Type pt)
        {
            PrologTermLayout pltl;
            string key = name + "/" + arity;
            lock (FunctorToLayout)
            {
                if (FunctorToLayout.TryGetValue(key, out pltl))
                {
                    Type type = pltl.ObjectType;
                    MemberInfo[] fis = pltl.FieldInfos;
                    MemberInfo toType = pltl.ToType;
                    if (toType != null)
                    {
                        return GetMemberValue(toType, GetInstance(arg1));
                    }
                    return CreateInstance(type, fis, orig, 1);
                }
            }
            if (key == "static/1")
            {
                return null;
            }
            if (key == "{}/1")
            {
                return arg1;
            }
            if (pt == typeof(object)) pt = null;
            //{T}
            //@(_Tag)
            if (key == "@/1" && arg1.IsAtom)
            {
                name = arg1.Name;
                switch (name)
                {
                    case "true":
                        {
                            return true;
                        }
                    case "false":
                        {
                            return false;
                        }
                    case "null":
                        {
                            if (pt != null && pt.IsValueType)
                            {
                                return pt.GetConstructor(new Type[0]).Invoke(new object[0]);
                            }
                            return null;
                        }
                    case "void":
                        {
                            if (pt == typeof(void)) return JPL.JVOID;
                            return null;
                        }
                    default:
                        {
                            lock (ToFromConvertLock)
                            {
                                object o = tag_to_object(name);
                                return o;
#if plvar_pins                                
                                lock (atomToPlRef)
                                {
                                    PlRef oldValue;
                                    if (!atomToPlRef.TryGetValue(name, out oldValue))
                                    {
                                        //Warn("no value for tag=" + name);
                                        if (pt != null && pt.IsInstanceOfType(o))
                                        {
                                            return o;
                                        }
                                        return o;
                                    }
                                    var v = oldValue.Value;
                                    if (pt != null && pt.IsInstanceOfType(v))
                                    {
                                        return v;
                                    }
                                    return v;
                                }
#endif
                            }
                        }
                }
            }
#if plvar_pins
            if (name == "$cli_object")
            {
                lock (ToFromConvertLock)
                {
                    lock (termToObjectPins)
                    {
                        PlRef oldValue;
                        Int64 ohandle = (long)arg1;
                        if (!termToObjectPins.TryGetValue(ohandle, out oldValue))
                        {
                            Warn("no value for ohandle=" + ohandle);
                        }
                        return oldValue.Value;
                    }
                }
            }
#endif
            if (key == "enum/2")
            {
                Type type = GetType(arg1);
                PlTerm arg2 = orig.Arg(1);
                object value = Enum.Parse(type, arg2.Name, true);
                if (value == null) Warn("cant parse enum: " + arg2 + " for type " + type);
                return value;
            }
            if (name == "array")
            {
                Type type = GetType(arg1);
                var arry = Array.CreateInstance(type, arity - 1);
                int copied = FillArray(arry, type, orig, 2);
                return arry;
            }
            if (name == "struct" || name == "event" || name == "object")
            {
                Type type = GetType(arg1);
                MemberInfo[] fis = GetStructFormat(type);
                return CreateInstance(type, fis, orig, 2);
            }
            if (orig.IsList)
            {
                if (arg1.IsInteger || arg1.IsAtom)
                {
                    Debug("maybe this is a string " + orig);
                }
                else
                {
                    var o1 = GetInstance(arg1);
                    if (o1 != null)
                    {
                        // send a list into cliGet0
                        return cliGet0(arg1, orig.Arg(1), o1.GetType());
                    }
                }
            }
            Type t = ResolveType(name);
            if (t == null)
            {
                Warn("Cant GetInstance from " + orig);
                return (string)orig;
            }       
            if (pt == null || pt.IsAssignableFrom(t))
            {
                foreach (var m in t.GetConstructors())
                {
                    ParameterInfo[] mGetParameters = m.GetParameters();
                    if (mGetParameters.Length == arity)
                    {
                        Warn("using contructor " + m);
                        var values = PlListToCastedArray(orig, m.GetParameters());
                        return m.Invoke(values);
                    }
                }
            }
            // Debug("Get Instance fallthru");
            MemberInfo[] ofs = GetStructFormat(t);
            return CreateInstance(t, ofs, orig, 1);
        }

        private static object CreateInstance(Type type, MemberInfo[] fis, PlTerm orig, int plarg)
        {
            int fisLength = fis.Length;
            object[] paramz = new object[fisLength];
            for (int i = 0; i < fisLength; i++)
            {
                MemberInfo fi = fis[i];
                PlTerm origArg = orig[plarg];
                paramz[i] = CastTerm(origArg, FieldType(fi));
                plarg++;
            }
            object newStruct = null;
            try
            {
                newStruct = Activator.CreateInstance(type);
            }
            catch (System.MissingMethodException)
            {
                foreach (ConstructorInfo ci in type.GetConstructors(BindingFlagsALL))
                {
                    if (ci.GetParameters().Length != paramz.Length) continue;
                    if (ci.IsStatic) continue;
                    newStruct = ci.Invoke(paramz);
                }
                if (newStruct != null) return newStruct;
            }                        
            for (int i = 0; i < fis.Length; i++)
            {
                MemberInfo fi = fis[i];
                SetMemberValue(fi, newStruct, paramz[i]);
            }
             

            return newStruct;
        }

        private static object GetMemberValue(MemberInfo field, object o)
        {
            if (field is FieldInfo)
            {
                return ((FieldInfo)field).GetValue(o);
            }
            if (field is PropertyInfo)
            {
                return ((PropertyInfo)field).GetGetMethod(true).Invoke(o, null);
            }
            if (field is MethodInfo)
            {
                MethodInfo mi = (MethodInfo)field;
                if (mi.IsStatic) return mi.Invoke(null, new object[] { o });
                return ((MethodInfo)field).Invoke(o, null);
            }
            if (field is ConstructorInfo)
            {
                return ((ConstructorInfo)field).Invoke(new object[] { o });
            }
            throw new IndexOutOfRangeException("" + field);
        }

        public static void SetMemberValue(MemberInfo field, object o, object value)
        {
            if (field is FieldInfo)
            {
                 ((FieldInfo) field).SetValue(o, value);
                return;
            }
            if (field is PropertyInfo)
            {
                MethodInfo setterMethod = ((PropertyInfo)field).GetSetMethod(true);
                if (setterMethod == null)
                {
                    Warn("No setter method on " + field);
                    return;
                }
                setterMethod.Invoke(o, new object[] { value });
                return;
            }
            if (field is MethodInfo)
            {
                MethodInfo mi = (MethodInfo) field;
                if (mi.IsStatic)
                {
                    mi.Invoke(null, new object[] {o, value});
                    return;
                }
                ((MethodInfo) field).Invoke(o, new object[] {value});
                return;
            }
            throw new IndexOutOfRangeException("" + field);
            if (field is ConstructorInfo)
            {
                ((ConstructorInfo) field).Invoke(new object[] {o, value});
                return;
            }
        }

        private static Type FieldType(MemberInfo field)
        {
            if (field is FieldInfo)
            {
                return ((FieldInfo) field).FieldType;
            }
            if (field is PropertyInfo)
            {
                return ((PropertyInfo) field).PropertyType;
            }
            if (field is MethodInfo)
            {
                MethodInfo mi = (MethodInfo)field;
                return mi.ReturnType;
            }
            if (field is ConstructorInfo)
            {
                return ((ConstructorInfo) field).DeclaringType;
            }
            throw new IndexOutOfRangeException("" + field);
        }

        private static int FillArray(IList fis, Type elementType, PlTerm orig, int plarg)
        {
            int elements = 0;
            for (int i = 0; i < fis.Count; i++)
            {
                fis[i] = CastTerm(orig[i], elementType);
                elements++;
            }
            return elements;
        }
        public static Object CastTerm(PlTerm o, Type pt)
        {
            if (pt == typeof(object)) pt = null;
            object r = CastTerm0(o, pt);
            if (pt == null || r == null)
                return r;
            Type fr = r.GetType();
            if (pt.IsInstanceOfType(r)) return r;
            return RecastObject(pt, r, fr);
        }

        private static object RecastObject(Type pt, object r, Type fr)
        {
            try
            {
                if (r is double && pt == typeof(float))
                {
                    double d = (double)r;
                    return Convert.ToSingle(d);
                }
                //                if (PrologBinder.CanConvertFrom(r.GetType(), pt))
                {
                    var value = Convert.ChangeType(r, pt);
                    if (pt.IsInstanceOfType(value)) return value;
                }
            }
            catch (Exception e)
            {
                Debug("conversion " + fr + " to " + pt + " resulted in " + e);
            }

            try
            {
                ICollection<Func<object, object>> tryAll = new List<Func<object, object>>();
                var fc = findCast(fr, pt, tryAll);
                if (fc != null)
                {
                    try
                    {
                        var value = fc(r);
                        if (pt.IsInstanceOfType(value)) return value;
                    }
                    catch (Exception)
                    {
                    }
                }
                foreach (var ti in tryAll)
                {
                    try
                    {
                        var value = ti(r);
                        if (pt.IsInstanceOfType(value)) return value;
                    }
                    catch (Exception)
                    {
                    }
                }
            }
            catch (Exception e)
            {
                Debug("conversion " + fr + " to " + pt + " resulted in " + e);
            }
            Warn("Having time of it convcerting " + r + " to " + pt);


            return r;
        }

        private static readonly List<Type> ConvertorClasses = new List<Type>();

        private static Func<object, object> findCast(Type from, Type to, ICollection<Func<object, object>> allMethods)
        {
            Func<object, object> meth;
            if (to.IsPrimitive)
            {
                meth = ((r) => Convert.ChangeType(r, to));
                if (allMethods != null) allMethods.Add(meth); else return meth;
            }
            if (to.IsArray && from.IsArray)
            {
                var eto = to.GetElementType();
                var efrom = from.GetElementType();
                meth = ((r) =>
                            {
                                Array ar = ((Array) r);
                                int len = ar.Length;
                                Array ret = Array.CreateInstance(eto, len);
                                for (int i = 0; i < len; i++)
                                {
                                    ret.SetValue(RecastObject(eto, ar.GetValue(i), efrom), i);
                                }
                                return ret;
                            });
                if (allMethods != null) allMethods.Add(meth); else return meth;
            }
            ConstructorInfo ci = to.GetConstructor(new Type[] {from});
            if (ci != null)
            {
                meth =  (r) => ci.Invoke(new object[] { r });
                if (allMethods != null) allMethods.Add(meth); else return meth;
            }
            ConstructorInfo pc = null;
            foreach (ConstructorInfo mi in to.GetConstructors(BindingFlagsALL))
            {
                var ps = mi.GetParameters();
                if (ps.Length == 0 && !mi.IsStatic)
                {
                    pc = mi;
                    continue;
                }
                if (ps.Length == 1)
                {
                    Type pt = ps[0].ParameterType;
                    if (pt.IsAssignableFrom(from))
                    {
                        ConstructorInfo info = mi;
                        meth =  (r) => info.Invoke(new object[] {r});
                        if (allMethods != null) allMethods.Add(meth); else return meth;
                    }
                }
            }
            var someStatic = SomeStaticMethod(to, to, from, allMethods);
            if (someStatic != null) return someStatic;
            someStatic = SomeStaticMethod(to, from, from, allMethods);
            if (someStatic != null) return someStatic;
            foreach (MethodInfo mi in from.GetMethods(BindingFlagsInstance))
            {
                if (!mi.IsStatic)
                {
                    var ps = mi.GetParameters();
                    if (ps.Length == 0)
                    {
                        if (!to.IsAssignableFrom(mi.ReturnType)) continue;
                        Type pt = ps[0].ParameterType;
                        if (pt.IsAssignableFrom(from))
                        {
                            MethodInfo info = mi;
                            meth = (r) => info.Invoke(null, new object[] { r });
                            if (allMethods != null) allMethods.Add(meth); else return meth;
                        }
                    }
                }
            }
            if (pc != null)
            {
                foreach (var f in to.GetFields(BindingFlagsInstance))
                {
                    FieldInfo info = f;
                    meth = (r) =>
                               {
                                   var ret = pc.Invoke(null);
                                   info.SetValue(ret, r);
                                   return ret;
                               };
                    if (allMethods != null) allMethods.Add(meth); else return meth;
                }
            }
            if (ConvertorClasses.Count==0)
            {
                ConvertorClasses.Add(typeof (Convert));
                ConvertorClasses.Add(typeof (PrologConvert));
            }
            foreach (Type convertorClasse in ConvertorClasses)
            {
                var someStaticM = SomeStaticMethod(to, convertorClasse, from, allMethods);
                if (someStaticM != null) return someStatic;
            }
            return null;
        }
        private static Func<object, object> SomeStaticMethod(Type to, Type srch, Type from, ICollection<Func<object, object>> allMethods)
        {
            Func<object, object> meth;
            foreach (MethodInfo mi in srch.GetMethods(BindingFlagsJustStatic))
            {
                if (mi.IsStatic)
                {
                    var ps = mi.GetParameters();
                    if (ps.Length == 1)
                    {
                        if (!to.IsAssignableFrom(mi.ReturnType)) continue;
                        Type pt = ps[0].ParameterType;
                        if (pt.IsAssignableFrom(from))
                        {
                            MethodInfo info = mi;
                            meth = (r) => info.Invoke(null, new object[] { r });
                            if (allMethods != null) allMethods.Add(meth); else return meth;
                        }
                    }
                }
            }
            return null;
        }

        private static readonly Type[] arrayOfStringType = new Type[] {typeof (string)};
        static object ToBigInteger(string value)
        {
            
            // .net 4.0 and Mono
            Type t = ResolveType("System.Numerics.BigInteger");
            if (t != null) return t.GetMethod("Parse", arrayOfStringType).Invoke(null, new object[] { value });
            // Just Mono
            t = ResolveType("Mono.Math.BigInteger");
            if (t != null) return t.GetMethod("Parse", arrayOfStringType).Invoke(null, new object[] { value });
            // IKVM           
            return new java.math.BigInteger(value);
        }
        static object ToBigDecimal(string value)
        {
            Type[] arrayOfStringType = new Type[] { typeof(string) };
            // .net 4.0 and Mono
            Type t = ResolveType("System.Numerics.BigDecimal");
            if (t != null) return t.GetMethod("Parse", arrayOfStringType).Invoke(null, new object[] { value });
            // Just Mono
            t = ResolveType("Java.Math.BigDecimal");
            if (t != null) return t.GetMethod("Parse", arrayOfStringType).Invoke(null, new object[] { value });
            // IKVM           
            return new java.math.BigDecimal(value);
        }
        public static Object CastTerm0(PlTerm o, Type pt)
        {
            if (pt == typeof(PlTerm)) return o;
            if (pt == typeof(string))
            {
                return (string)o;
            }
            switch (o.PlType)
            {
                case PlType.PlUnknown:
                    {
                        return (string)o;
                    }
                    break;
                case PlType.PlVariable:
                    {
                        return o;
                    }
                    break;
                case PlType.PlInteger:
                    {
                        long i = 0;
                        if (0 != libpl.PL_get_long(o.TermRef, ref i))
                            return i;
                        try
                        {
                            return (ulong)o;
                        }
                        catch (Exception)
                        {
                            return ToBigInteger((string) o);                            
                        }
                    }
                    break;
                case PlType.PlFloat:
                    {
                        try
                        {
                            return (double)o;
                        }
                        catch (Exception)
                        {
                            return ToBigDecimal((string)o);
                        }
                    }
                    break;
                case PlType.PlAtom:
                case PlType.PlString:
                    {
                        string s = (string)o;
                        if (pt == null) return s;
                        var constructor = pt.GetConstructor(new[] { typeof(string) });
                        if (constructor != null)
                        {
                            return constructor.Invoke(new object[] { s });
                        }
                        foreach (var m in pt.GetMethods(BindingFlags.Static | BindingFlags.Public | BindingFlags.NonPublic))
                        {

                            ParameterInfo[] mGetParameters = m.GetParameters();
                            if (pt.IsAssignableFrom(m.ReturnType) && mGetParameters.Length == 1 &&
                                mGetParameters[0].ParameterType.IsAssignableFrom(typeof(string)))
                            {
                                Warn("using " + m);
                                return m.Invoke(null, new object[] { s });
                            }
                        }
                        return s;
                    }
                    break;
                case PlType.PlTerm:
                    {
                        lock (ToFromConvertLock)
                        {
                            return CastCompoundTerm(o.Name, o.Arity, o[1], o, pt);
                        }
                    }
                    break;
                default:
                    throw new ArgumentOutOfRangeException();
            }
        }

      }

    internal class PrologConvert //: OpenMetaverse.UUIDFactory
    {
        static public Guid ToGuid(object from)
        {
            return new Guid("" + from);
        }
        static public String ToStr(object from)
        {
            return "" + from;
        }
    }

    internal class PrologTermLayout
    {
        public string Name;
        public int Arity;
        public Type ObjectType;
        public MemberInfo[] FieldInfos;
        public MemberInfo ToType;
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