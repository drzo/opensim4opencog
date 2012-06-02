/*********************************************************
* 
*  Project: Swicli - Two Way Interface to .NET and MONO 
*  Author:        Douglas R. Miles
*  Copyright (C): 2008, Logicmoo - http://www.kqml.org
*
*  This library is free software; you can redistribute it and/or
*  modify it under the terms of the GNU Lesser General Public
*  License as published by the Free Software Foundation; either
*  version 2.1 of the License, or (at your option) any later version.
*
*  This library is distributed in the hope that it will be useful,
*  but WITHOUT ANY WARRANTY; without even the implied warranty of
*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
*  Lesser General Public License for more details.
*
*  You should have received a copy of the GNU Lesser General Public
*  License along with this library; if not, write to the Free Software
*  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
*
*********************************************************/
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
using SbsSW.SwiPlCs;
using SbsSW.SwiPlCs.Callback;
using SbsSW.SwiPlCs.Exceptions;
#if USE_IKVM
using ikvm.extensions;
using IKVM.Internal;
using ikvm.runtime;
using java.net;
using java.util;
//using jpl;
using jpl;
using Hashtable = java.util.Hashtable;
using ClassLoader = java.lang.ClassLoader;
using Class = java.lang.Class;
using sun.reflect.misc;
using Util = ikvm.runtime.Util;
#else
using Swicli.Library;
using Class = System.Type;
#endif
using ArrayList = System.Collections.ArrayList;
using CycFort = SbsSW.SwiPlCs.PlTerm;
using PrologCli = Swicli.Library.PrologClient;

namespace Swicli.Library
{
    public partial class PrologClient
    {
        [PrologVisible(Name = "cli_load_type", Arity = 1, TypeOf = null)]
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

        public static Type GetTypeThrowIfMissing(CycFort clazzSpec)
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
            return GetType(clazzSpec, false);
        }
        public static Type GetType(PlTerm clazzSpec, bool canBeObjects)
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
                if (!canBeObjects)
                {
                    Warn("cant find tagged object as class: {0}=>{1}", clazzSpec, tagObj);
                }
                if (tagObj != null)
                {
                    return tagObj.GetType();
                }
                return null;
            }
            Type type = null;
            if (clazzSpec.IsAtom || clazzSpec.IsString)
            {
                if (canBeObjects) return typeof (string);
                string name = (string)clazzSpec;
                type = ResolveType(name);
                if (type != null) return type;
                if (!canBeObjects)
                {
                    Warn("cant find atom/string as class: {0}", clazzSpec);
                    type = ResolveType(name);
                }
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
                if (clazzName == "{}")
                {
                    return typeof (PlTerm);
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
                            var vt = GetParamSpec(clazzSpec, false);
                            return type.MakeGenericType(vt);
                        }
                    }
                    //  return type;
                }
                string key = clazzName + "/" + arity;
                lock (FunctorToLayout)
                {
                    PrologTermLayout pltl;
                    if (FunctorToLayout.TryGetValue(key, out pltl))
                    {
                        return pltl.ObjectType;
                    }
                }
                lock (FunctorToRecomposer)
                {
                    PrologTermRecomposer layout;
                    if (FunctorToRecomposer.TryGetValue(key, out layout))
                    {
                        return layout.ToType;
                    }
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
                       // ConsoleTrace("name:" + className + " type:" + s1.FullName + " class:" + c);
                        return UnifyTagged(c, term2);
                    }
                    ConsoleTrace("cant getFriendlyClassFromType " + s1.FullName);
                    return false;
                }
                ConsoleTrace("cant ResolveType " + className);
                return false;
            }
            ConsoleTrace("cant IsAtom " + term1);
            return false;
        }

        public static void ConsoleTrace(object s)
        {
            try
            {
                Console.WriteLine(s);
            }
            catch (Exception)
            {
            }  
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
                    ConsoleTrace("cliFindClass:" + className + " class:" + c);
                    string tag = object_to_tag(c);
                    return AddTagged(clazzObjectOut.TermRef, tag) != 0;
                }
                ConsoleTrace("cant ResolveClass " + className);
                return false;
            }
            Type t = GetType(clazzName);
            if (t != null)
            {
                Class c = null;
#if USE_IKVM
                c = ikvm.runtime.Util.getFriendlyClassFromType(t);
#else
                c = t;
#endif
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
                return cliGetType(valueIn, plvar) && SpecialUnify(valueOut, plvar);
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
                return cliGetClass(valueIn, plvar) && SpecialUnify(valueOut, plvar);
            }
            object val = GetInstance(valueIn);
            // extension method
#if USE_IKVM
            return valueOut.FromObject((val.getClass()));
#else
            return valueOut.FromObject((val.GetType()));
#endif
        }
        [PrologVisible(ModuleName = ExportModule)]
        static public bool cliClassFromType(PlTerm valueIn, PlTerm valueOut)
        {
            if (!valueOut.IsVar)
            {
                var plvar = PlTerm.PlVar();
                return cliClassFromType(valueIn, plvar) && SpecialUnify(valueOut, plvar);
            }
            Type val = GetType(valueIn);
            if (val == null) return false;
#if USE_IKVM
            Class c = ikvm.runtime.Util.getFriendlyClassFromType(val);
            return valueOut.FromObject((c));
#else
            return valueOut.FromObject((val));
#endif
        }
        [PrologVisible(ModuleName = ExportModule)]
        static public bool cliTypeFromClass(PlTerm valueIn, PlTerm valueOut)
        {
            if (!valueOut.IsVar)
            {
                var plvar = PlTerm.PlVar();
                return cliTypeFromClass(valueIn, plvar) && SpecialUnify(valueOut, plvar);
            }
            Class val = GetType(valueIn);
            if (val == null) return false;
#if USE_IKVM
            Type c = ikvm.runtime.Util.getInstanceTypeFromClass(val);
            return valueOut.FromObject((c));
#else
            return valueOut.FromObject(val);
#endif
        }
        [PrologVisible(ModuleName = ExportModule)]
        static public bool cliShorttype(PlTerm valueName, PlTerm valueIn)
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
                return cliGetClassname(valueIn, plvar) && SpecialUnify(valueOut, plvar);
            }
            Class val = CastTerm(valueIn, typeof(Class)) as Class;
            if (val == null) return false;

#if USE_IKVM
            return valueOut.Unify(val.getName());
#else
            return valueOut.Unify(val.GetType().Name);
#endif
        }
        [PrologVisible(ModuleName = ExportModule)]
        static public bool cliGetTypeFullname(PlTerm valueIn, PlTerm valueOut)
        {
            if (!valueOut.IsVar)
            {
                var plvar = PlTerm.PlVar();
                return cliGetTypeFullname(valueIn, plvar) && SpecialUnify(valueOut, plvar);
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
#if USE_IKVM
            Class c = ikvm.runtime.Util.getFriendlyClassFromType((Type)t);
            return c;
#else
            return t;
#endif

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
#if USE_IKVM
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
#endif
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
                    string mName = m.Name;
                    if (ForceJanCase)
                    {
                        pm.Name = ToPrologCase(mName);
                    } else
                    {
                        pm.Name = mName;
                    }
                }
                else
                {
                    string mName = m.Name;
                    pm.Name = ToPrologCase(mName);
                }
            } else
            {
                if (ForceJanCase) pm.Name = ToPrologCase(pm.Name);
            }
            InternMethod(pm.ModuleName, pm.Name, m);
        }
    }
}