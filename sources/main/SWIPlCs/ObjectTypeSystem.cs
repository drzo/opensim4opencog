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
        public static List<Assembly> AssembliesLoaded = new List<Assembly>();
        public static List<string> AssembliesLoading = new List<string>();
        public static List<Type> TypesLoaded = new List<Type>();
        public static List<Type> TypesLoading = new List<Type>();

        public static bool ResolveAssembly(Assembly assembly)
        {
            string assemblyName = assembly.FullName;
            lock (AssembliesLoaded)
            {
                if (AssembliesLoading.Contains(assemblyName))
                {
                    return true;
                }
                AssembliesLoading.Add(assemblyName);
                LoadReferencedAssemblies(assembly);
                // push to the front
                AssembliesLoaded.Remove(assembly);
                AssembliesLoaded.Insert(0, assembly);
            }
            LoadTypesFromAssembly(assembly);
            return true;
        }

        private static void LoadReferencedAssemblies(Assembly assembly)
        {
            foreach (var refed in assembly.GetReferencedAssemblies())
            {
                string assemblyName = refed.FullName;
                try
                {
                    Assembly assemblyLoad = Assembly.Load(refed);
                    ResolveAssembly(assemblyLoad);
                }
                catch (Exception e)
                {
                    Warn("LoadReferencedAssemblies:{0} caused {1}", assemblyName, e);
                }
            }
        }

        private static void LoadTypesFromAssembly(Assembly assembly)
        {
            string assemblyName = assembly.FullName;
            try
            {
                foreach (Type t in assembly.GetTypes())
                {
                    try
                    {
                        LoadType(t);
                    }
                    catch (Exception te)
                    {
                        // Warn(e.Message);
                    }
                }
            }
            catch (Exception e)
            {
                // get types problem
                // Warn(e.Message);
            }
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
                Warn("cliLoadAssembly: {0} caused {1}", term1, e);
                return false;
            }
        }
        public static bool cliLoadAssemblyUncaught(PlTerm term1)
        {
            try
            {
                if (term1.IsVar)
                {
                    Warn("cliLoadAssembly IsVar {0}", term1);
                    return false;
                }
                if (IsTaggedObject(term1))
                {
                    var assemblyOrType = GetInstance(term1);
                    if (assemblyOrType is Assembly)
                    {
                        return ResolveAssembly((Assembly)assemblyOrType);
                    }
                    if (assemblyOrType is Type)
                    {
                        return ResolveAssembly(((Type)assemblyOrType).Assembly);
                    }
                    return Warn("Cannot get assembly from {0} for {1}", assemblyOrType, term1);
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
                        Warn("LoadFile: {0} caused {1}", fiFullName, e);
                    }
                }
                try
                {
                    if (assembly == null) assembly = Assembly.LoadWithPartialName(name);
                }
                catch (Exception e)
                {
                    Warn("LoadWithPartialName: {0} caused {1}", name, e);
                }
                if (assembly == null) return Warn("Cannot get assembly from {0} for {1}", name, term1);
                return ResolveAssembly(assembly);

            }
            catch (Exception exception)
            {
                throw ToPlException(exception);
            }
            return true;
        }

        [PrologVisible(Name = "cliLoadType", Arity = 1, TypeOf = null)]
        private static void LoadType(Type t)
        {
            lock (TypesLoaded)
            {
                if (TypesLoaded.Contains(t) || TypesLoading.Contains(t)) return;
                TypesLoading.Add(t);
                foreach (var m in t.GetMethods(BindingFlags.NonPublic | BindingFlags.Public | BindingFlags.Static))
                {
                    object[] f = m.GetCustomAttributes(typeof(PrologVisible), false);
                    if (f != null && f.Length > 0)
                    {
                        try
                        {
                            LoadMethod(m, (PrologVisible) f[0]);
                        }
                        catch (Exception e)
                        {
                            Warn(m + " caused " + e);
                        }
                    }
                }
                TypesLoading.Remove(t);
                TypesLoaded.Add(t);
            }
        }

        public static Type GetTypeThrowIfMissing(PlTerm clazzSpec)
        {
            Type fi = GetType(clazzSpec);
            if (fi == null)
            {
                throw new PlException("cant find class" + clazzSpec);
            }
            return fi;
        }
        public static Type GetType(PlTerm clazzSpec)
        {
            if (clazzSpec.IsVar)
            {
                Warn("GetType IsVar {0}", clazzSpec);
                return null;
            }
            if (IsTaggedObject(clazzSpec))
            {
                object tagObj = tag_to_object(clazzSpec[1].Name);
                var r = tagObj as Type;
                if (r != null) return r;
                Warn("cant find tagged object as class: {0}=>{1}", clazzSpec, tagObj);
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
                Warn("cant find atom/string as class: {0}", clazzSpec);
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
                            Warn("GetGenericParameterConstraints: {0}", e);
                        }
                        try
                        {
                            genrb = type.GetGenericTypeDefinition();
                        }
                        catch (Exception e)
                        {
                            Warn("GetGenericTypeDefinition: {0}", e);
                        }

                        if (arity == genr.Length)
                        {
                            var vt = GetParamSpec(clazzSpec);
                            return type.MakeGenericType(vt);
                        }
                    }
                    //  return type;
                }
                WarnMissing("cant find compound as class: " + clazzSpec);
            }
            object toObject = GetInstance(clazzSpec);
            if (toObject is Type) return (Type)toObject;
            if (toObject != null)
            {
                return toObject.GetType();
            }
            Warn("@TODO cant figure type from {0}", clazzSpec);
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
                        return UnifyTagged(c, term2);
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
                }
                else
                {
                    Debug("cant chop arity " + gtpLength + " off string '" + typeName + "' ");
                }
                return PlC(typeName, vt);
            }
            return PlTerm.PlAtom(typeToName(type));
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
                Warn("Cannot get object for {0}", valueIn);
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
            if (!valueName.IsString && !valueName.IsAtom) return Warn("valueName must be string or atom {0}", valueName);
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

        private static Class ResolveClass(string name)
        {
            if (name == "@" || name == "$cli_object" || name == "array" || name == null) return null;
            Type t = ResolveClassAsType(name);
            Class c = ikvm.runtime.Util.getFriendlyClassFromType((Type)t);
            return c;
        }
        private static Type ResolveClassAsType(string name)
        {
            Type s1 = ResolveType(name);
            if (s1 != null) return s1;
            if (name.EndsWith("[]"))
            {
                Type t1 = ResolveClassAsType(name.Substring(0, name.Length - 2));
                return t1.MakeArrayType();
            }
            var name2 = name.Replace("/", ".");
            if (name2 != name)
            {
                s1 = ResolveType(name2);
                if (s1 != null) return s1;
            }
            name2 = name.Replace("cli.", "");
            if (name2 != name)
            {
                s1 = ResolveType(name2);
                if (s1 != null) return s1;
            }
            return null;
        }

        private static Type ResolveType(string name)
        {
            if (name == "@" || name == "[]" || name == "$cli_object" || name == "array" || name == null) return null;
            if (name.EndsWith("[]"))
            {
                Type t = ResolveType(name.Substring(0, name.Length - 2));
                return t.MakeArrayType();
            }
            if (name.EndsWith("?"))
            {
                return typeof(Nullable<>).MakeGenericType(new[] { ResolveType(name.Substring(0, name.Length - 1)) });
            }
            if (name.EndsWith("&"))
            {
                Type t = ResolveType(name.Substring(0, name.Length - 1));
                return t.MakeByRefType();
            }
            if (name.EndsWith("*"))
            {
                Type t = ResolveType(name.Substring(0, name.Length - 1));
                return t.MakePointerType();
            }
            var s1 = ResolveType0(name);
            if (s1 != null) return s1;
            var name2 = name.Replace("/", ".");
            if (name2 != name)
            {
                s1 = ResolveType0(name2);
                if (s1 != null) return s1;
            }
            name2 = name.Replace("cli.", "");
            if (name2 != name)
            {
                s1 = ResolveType0(name2);
                if (s1 != null) return s1;
            }
            return null;
        }

        public static Type ResolveType0(string typeName)
        {
            Type type = null;
            if (!typeName.Contains("."))
            {
                lock (ShortNameType)
                {
                    if (ShortNameType.TryGetValue(typeName, out type))
                    {
                        return type;
                    }
                }
            }
            type = type ?? Type.GetType(typeName);
            if (type == null)
            {
                foreach (Assembly loaded in AssembliesLoaded)
                {
                    Type t = loaded.GetType(typeName, false);
                    if (t != null) return t;
                }
                Class obj = null;
                try
                {
                    obj = Class.forName(typeName);
                }
                catch (java.lang.ClassNotFoundException e)
                {
                }
                catch (Exception e)
                {
                }
                if (obj != null)
                {
                    type = ikvm.runtime.Util.getInstanceTypeFromClass((Class)obj);
                }
                if (type == null)
                {
                    type = getPrimitiveType(typeName);
                }
                if (type == null)
                {
                    type = Type.GetTypeFromProgID(typeName);
                }
            }
            return type;
        }

        public static Type getPrimitiveType(String name)
        {
            if (name.StartsWith("["))
            {
                Type t = ResolveType(name.Substring(1));
                return t.MakeArrayType();
            }
            switch (name)
            {
                case "byte":
                case "B":
                case "uint8":
                case "ubyte":
                    return typeof(byte);
                case "int16":
                    return typeof(Int16);
                case "int":
                case "int32":
                case "I":
                    return typeof(int);
                case "long":
                case "int64":
                case "J":
                    return typeof(long);
                case "short":
                case "S":
                    return typeof(short);
                case "sbyte":
                case "int8":
                    return typeof(sbyte);
                case "uint":
                case "uint32":
                    return typeof(uint);
                case "uint16":
                    return typeof(UInt16);
                case "uint64":
                case "ulong":
                    return typeof(ulong);
                case "ushort":
                    return typeof(ushort);
                case "decimal":
                    return typeof(decimal);
                case "double":
                case "D":
                    return typeof(double);
                case "float":
                case "F":
                    return typeof(float);
                case "object":
                    return typeof(object);
                case "string":
                    return typeof(string);
                case "void":
                case "V":
                    return typeof(void);
                case "char":
                case "C":
                    return typeof(char);
                case "bool":
                case "boolean":
                case "bit":
                case "Z":
                    return typeof(bool);
                default:
                    return null;
            }
        }


        private static void LoadMethod(MethodInfo m, PrologVisible pm)
        {
            if (pm.Name == null)
            {
                if (char.IsLower(m.Name[0]))
                {
                    pm.Name = m.Name;
                }
            }
            InternMethod(pm.ModuleName, pm.Name, m);
        }
    }
}