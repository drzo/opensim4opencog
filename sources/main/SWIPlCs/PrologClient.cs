using System;
using System.Collections;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Reflection.Emit;
using System.Runtime.InteropServices;
using System.Threading;
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
using Util = ikvm.runtime.Util;

namespace SbsSW.SwiPlCs
{
    public class PrologClient
    {
        public PrologClient()
        {
            ClientModule = null;
            ClientPrefix = "cli_";
            SetupProlog();
        }

        public delegate object AnyMethod(params object[] any);

        /// <summary>
        /// the .Net process (Not OS)
        /// </summary>
        /// <returns></returns>
        internal static bool Is64BitRuntime()
        {
#if _PL_X64
            return true;
#endif
            int bits = IntPtr.Size * 8;
            return bits == 64;
        }

        public static BindingFlags BindingFlagsALL = BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Static |
                                                     BindingFlags.Instance;

        const string ExportModule = "user";
        public static List<Assembly> AssembliesLoaded = new List<Assembly>();
        public static List<Assembly> AssembliesLoading = new List<Assembly>();

        /// <summary>
        /// The OS and not the .Net process
        ///  therefore "Program Files" are either for 64bit or 32bit apps
        /// </summary>
        /// <returns></returns>
        internal static bool Is64BitComputer()
        {
            return Is64BitRuntime() || !String.IsNullOrEmpty(Environment.GetEnvironmentVariable("ProgramFiles(x86)"));
        }

        public static void InternMethod(string module, string pn, AnyMethod d)
        {
            if (!PlEngine.PinDelegate(module, pn.ToString(), -1, d)) return;
            InternMethod(module, pn, d.Method);
        }
        public static void InternMethod(string module, string pn, MethodInfo list)
        {
            InternMethod(module, pn, list, null);
        }
        public static void InternMethod(string module, string pn, MethodInfo list, object defaultInstanceWhenMissing)
        {
            if (list == null)
            {
                return;
            }
            Type type = list.DeclaringType;
            pn = pn ?? (type.Name + "." + list.Name);
            ParameterInfo[] ps = list.GetParameters();
            Type rt = list.ReturnType;
            int paramlen = ps.Length;
            bool nonvoid = rt != typeof(void);
            bool isbool = rt == typeof(bool);
            bool hasReturnValue = nonvoid && !isbool;
            bool isStatic = list.IsStatic;
            if (isbool && isStatic)
            {
                bool isVanilla = true;
                foreach (ParameterInfo info in ps)
                {
                    if (info.ParameterType != typeof(PlTerm))
                    {
                        isVanilla = false;
                        break;
                    }
                }
                if (isVanilla)
                {
                    Delegate d = null;
                    switch (paramlen)
                    {
                        case 0:
                            {
                                d = new DelegateParameter0(() => (bool)InvokeCaught(list, null, new object[0]));
                                PlEngine.RegisterForeign(module, pn, paramlen, d, PlForeignSwitches.None);
                                return;
                            }
                        case 1:
                            PlEngine.RegisterForeign(module, pn, paramlen,
                                                     new DelegateParameter1(
                                                         (p1) => (bool)InvokeCaught(list, null, new object[] { p1 })),
                                                     PlForeignSwitches.None);
                            return;
                        case 2:
                            PlEngine.RegisterForeign(module, pn, paramlen,
                                                     new DelegateParameter2(
                                                         (p1, p2) =>
                                                         (bool)InvokeCaught(list, null, new object[] { p1, p2 })),
                                                     PlForeignSwitches.None);
                            return;
                        case 3:
                            PlEngine.RegisterForeign(module, pn, paramlen,
                                                     new DelegateParameter3(
                                                         (p1, p2, p3) =>
                                                         (bool)InvokeCaught(list, null, new object[] { p1, p2, p3 })),
                                                     PlForeignSwitches.None);
                            return;
                        case 4:
                            PlEngine.RegisterForeign(module, pn, paramlen,
                                                     new DelegateParameter4(
                                                         (p1, p2, p3, p4) =>
                                                         (bool)InvokeCaught(list, null, new object[] { p1, p2, p3, p4 })),
                                                     PlForeignSwitches.None);
                            return;
                        default:
                            break;
                    }
                }
            }
            int plarity = paramlen + (hasReturnValue ? 1 : 0) + (isStatic ? 0 : 1);

            Delegate del
                = new DelegateParameterVarArgs((PlTermV termVector) =>
                {
                    if (termVector.Size != plarity)
                    {
                        return false;
                    }
                    object target = isStatic
                                        ? null
                                        : ToVM(termVector[0], type) ??
                                          defaultInstanceWhenMissing;
                    object[] newVariable = new object[paramlen];

                    int tvargnum = isStatic ? 0 : 1;
                    for (int argnum = 0; argnum < paramlen; argnum++)
                    {
                        newVariable[argnum] = ToVM(termVector[tvargnum],
                                                   ps[argnum].ParameterType);
                        tvargnum++;
                    }

                    object result = InvokeCaught(list, target, newVariable);

                    if (isbool)
                    {
                        return (bool)result;
                    }
                    if (nonvoid)
                    {
                        return termVector[plarity - 1].Unify(ToProlog(result));
                    }
                    return true;

                });

            PlEngine.RegisterForeign(module, pn, plarity, del, PlForeignSwitches.VarArgs);

        }

        private static object InvokeCaught(MethodInfo info, object o, object[] os)
        {
            try
            {
                return info.Invoke(o, os);
            }
            catch (Exception ex)
            {
                var pe = ToPlException(ex);
                string s = pe.ToString() + "\n" + pe.StackTrace;
                throw pe;
            }
        }

        private static PlException ToPlException(Exception ex)
        {
            if (ex is PlException) return (PlException)ex;
            var ie = ex.InnerException;
            if (ie != null)
            {
                if (ie is PlException) return (PlException)ie;
                ex = ie;
            }
            return new PlException(ex.Message, ex);
        }
        private static bool Warn(string text)
        {
            return libpl.PL_warning(text) == 0;
        }

        private static Type[] GetParamSpec(PlTerm memberSpec)
        {
            int arity = memberSpec.Arity;
            Type[] paramz = new Type[arity];
            int i = 0;
            foreach (PlTerm info in memberSpec)
            {
                paramz[i] = GetType(info);
                i++;
                if (i >= arity)
                {
                    break;
                }
            }
            return paramz;
        }

        private static MethodInfo findMethod(PlTerm memberSpec, Type c)
        {
            if (TaggedObject(memberSpec))
            {
                var r = tag_to_object(memberSpec[1].Name) as MethodInfo;
                if (r != null) return r;
            }
            string fn = memberSpec.Name;
            var mi = c.GetMethod(fn, BindingFlagsALL);
            if (mi != null) return mi;
            Type[] paramz = GetParamSpec(memberSpec);
            mi = c.GetMethod(fn, paramz);
            if (mi != null) return mi;
            MethodInfo[] members = c.GetMethods(BindingFlagsALL);
            int arity = memberSpec.Arity;
            foreach (var infos in members)
            {
                if (infos.GetParameters().Length == arity)
                {
                    if (infos.Name == fn) return infos;
                }
            }
            return null;
        }


        private static ConstructorInfo findConstructor(PlTerm memberSpec, Type c)
        {
            if (TaggedObject(memberSpec))
            {
                var r = tag_to_object(memberSpec[1].Name) as ConstructorInfo;
                if (r != null) return r;
            }
            Type[] paramz = GetParamSpec(memberSpec);
            var mi = c.GetConstructor(paramz);
            if (mi != null) return mi;
            ConstructorInfo[] members = c.GetConstructors(BindingFlagsALL);
            int arity = memberSpec.Arity;
            foreach (var infos in members)
            {
                if (infos.GetParameters().Length == arity)
                {
                    return infos;
                }
            }
            return null;
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

        private static Type GetType(PlTerm clazz)
        {
            if (TaggedObject(clazz))
            {
                var r = tag_to_object(clazz[1].Name) as Type;
                if (r != null) return r;
            }
            object toObject = ToVM(clazz, null);
            if (toObject is Type) return (Type)toObject;
            Type type = null;
            if (clazz.IsAtom || clazz.IsString)
            {
                type = ResolveType(clazz.Name);
                if (type != null) return type;
            }
            if (clazz.IsCompound)
            {
                if (clazz.Name == "array")
                {
                    return GetType(clazz[1]).MakeArrayType();
                }
            }
            Warn("@TODO cant figure type from " + clazz);
            return typeof(object);
            //return null;
        }

        [PrologVisible(ModuleName = ExportModule)]
        private static object[] PlListToArray(IEnumerable<PlTerm> term, ParameterInfo[] terms)
        {
            if (term is PlTerm)
            {
                PlTerm tlist = (PlTerm)term;
                term = tlist.Copy();
            }
            int len = terms.Length;
            object[] ret = new object[len];
            int idx = 0;
            foreach (PlTerm arg in term)
            {
                ret[idx] = ToVM(arg, terms[idx].ParameterType);
                if (idx++ >= len) break;
            }
            return ret;
        }

        [PrologVisible(ModuleName = ExportModule)]
        static public bool cliMembers(PlTerm clazzOrInstance, PlTerm membersOut)
        {
            Type c = GetTypeFromInstance(clazzOrInstance);
            MemberInfo[] members = c.GetMembers(BindingFlagsALL);
            List<PlTerm> list = new List<PlTerm>();
            string cname = c.Name;
            foreach (MemberInfo info in members)
            {
                string mn = info.Name;
                switch (info.MemberType)
                {
                    case MemberTypes.Constructor:
                        list.Add(PlTerm.PlCompound(cname, ToPlTermV(((ConstructorInfo)info).GetParameters())));
                        break;
                    case MemberTypes.Event:
                        list.Add(PlTerm.PlCompound("event", PlTerm.PlCompound(mn, ToPlTermV(((EventInfo)info).GetRaiseMethod().GetParameters()))));
                        break;
                    case MemberTypes.Field:
                        list.Add(PlTerm.PlCompound(mn, typeToSpec(((FieldInfo)info).FieldType)));
                        break;
                    case MemberTypes.Method:
                        list.Add(PlTerm.PlCompound(mn, ToPlTermV(((MethodInfo)info).GetParameters())));
                        break;
                    case MemberTypes.Property:
                        list.Add(PlTerm.PlCompound(mn, typeToSpec(((PropertyInfo)info).PropertyType)));
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
            }
            return membersOut.Unify(ToPlList(list.ToArray()));
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
            var tv = new PlTermV(terms.Length);
            for (int i = 0; i < terms.Length; i++)
            {
                tv[i] = terms[i];
            }
            return tv;
        }
        private static PlTermV ToPlTermV(ParameterInfo[] terms)
        {
            var tv = new PlTermV(terms.Length);
            for (int i = 0; i < terms.Length; i++)
            {
                tv[i] = typeToSpec(terms[i].ParameterType);
            }
            return tv;
        }

        private static PlTerm paramInfoToSpecs(ParameterInfo[] terms)
        {
            int termLen = terms.Length;
            if (termLen == 0) return ATOM_NIL;
            if (termLen == 1)
            {
                return listOfOne(typeToSpec(terms[0].ParameterType));
            }
            termLen--;
            PlTerm ret = listOfLeastOne(typeToSpec(terms[0].ParameterType));
            PlTerm current = ret;
            for (int i = 1; i < termLen; i++)
            {
                ParameterInfo info = terms[i];
                var current2 = listOfLeastOne(typeToSpec(info.ParameterType));
                current[2].Unify(current2);
                current = current2;
            }
            current[2].Unify(listOfOne(typeToSpec(terms[termLen].ParameterType)));
            return ret;
        }

        private static PlTerm listOfOne(PlTerm term)
        {
            return PlTerm.PlCompound(".", term, ATOM_NIL);
        }
        private static PlTerm listOfLeastOne(PlTerm term)
        {
            return PlTerm.PlCompound(".", term, PlTerm.PlVar());
        }

        protected static PlTerm ATOM_NIL
        {
            get { return PlTerm.PlAtom("[]"); }
        }

        private static PlTerm typeToSpec(Type type)
        {
            return PlTerm.PlAtom(type.FullName);
        }

        [PrologVisible(ModuleName = ExportModule)]
        static public bool cliFindConstructor(PlTerm memberSpec, PlTerm methodOut)
        {
            Type c = ResolveType(memberSpec.Name);
            ConstructorInfo mi = findConstructor(memberSpec, c);
            if (mi != null)
            {
                return methodOut.Unify(ToProlog(mi));
            }
            return false;
        }

        /// <summary>
        /// ?- cliNew('java.lang.Long'(long),[44],Out),cliToString(Out,Str).
        /// </summary>
        /// <param name="memberSpec"></param>
        /// <param name="valueIn"></param>
        /// <param name="valueOut"></param>
        /// <returns></returns>
        [PrologVisible(ModuleName = ExportModule)]
        static public bool cliNew(PlTerm memberSpec, PlTerm valueIn, PlTerm valueOut)
        {
            Type c = ResolveType(memberSpec.Name);
            ConstructorInfo mi = findConstructor(memberSpec, c);
            if (mi == null)
            {
                Warn("Cant find constructor " + memberSpec + " on " + c);
                return false;
            }
            object[] values = PlListToArray(valueIn, mi.GetParameters());
            return valueOut.Unify(ToProlog(mi.Invoke(values)));
        }

        /// <summary>
        /// ?- cliNewArray(long,10,Out),cliToString(Out,Str).
        /// </summary>
        /// <param name="typeSpec"></param>
        /// <param name="rank"></param>
        /// <param name="valueOut"></param>
        /// <returns></returns>
        [PrologVisible(ModuleName = ExportModule)]
        static public bool cliNewArray(PlTerm typeSpec, PlTerm rank, PlTerm valueOut)
        {
            Type c = GetType(typeSpec);
            if (c == null)
            {
                Warn("Cant find type " + typeSpec);
                return false;
            }
            var value = c.MakeArrayType(rank.intValue());
            return valueOut.Unify(ToProlog(value));
        }

        /// <summary>
        /// ?- cliNewArray(long,10,Out),cliToString(Out,Str).
        /// </summary>
        /// <param name="typeSpec"></param>
        /// <param name="rank"></param>
        /// <param name="valueOut"></param>
        /// <returns></returns>
        [PrologVisible(ModuleName = ExportModule)]
        static public bool cliArrayToVector(PlTerm arrayValue, PlTerm valueOut)
        {
            object getInstance = GetInstance(arrayValue);
            var value = getInstance as Array;
            if (value == null)
            {
                Warn("Cant find array from " + arrayValue + " as " + getInstance.GetType());
                return false;
            }
            int len = value.Length;
            var termv = new PlTermV(len);
            for (int i = 0; i < len; i++)
            {
                bool pf = termv[i].Unify(ToProlog(value.GetValue(i)));
            }
            Type et = value.GetType().GetElementType();
            return valueOut.Unify(PlTerm.PlCompound(typeToSpec(et).Name, termv));
        }
        [PrologVisible(ModuleName = ExportModule)]
        static public bool cliVectorToArray(PlTerm arrayValue, PlTerm valueOut)
        {
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
            return valueOut.Unify(ToProlog(value));
        }
        [PrologVisible(ModuleName = ExportModule)]
        static public bool cliFindMethod(PlTerm clazzOrInstance, PlTerm memberSpec, PlTerm methodOut)
        {
            object getInstance = GetInstance(clazzOrInstance);
            Type c = GetTypeFromInstance(clazzOrInstance);
            MethodInfo mi = findMethod(memberSpec, c);
            if (mi != null)
            {
                return methodOut.Unify(ToProlog(mi));
            }
            return false;
        }



        [PrologVisible(ModuleName = ExportModule)]
        static public bool cliCall(PlTerm clazzOrInstance, PlTerm memberSpec, PlTerm valueIn, PlTerm valueOut)
        {
            object getInstance = GetInstance(clazzOrInstance);
            Type c = GetTypeFromInstance(clazzOrInstance);
            MethodInfo mi = findMethod(memberSpec, c);
            if (mi == null)
            {
                Warn("Cant find method " + memberSpec + " on " + c);
                return false;
            }
            object[] value = PlListToArray(valueIn, mi.GetParameters());
            object target = mi.IsStatic ? null : getInstance;
            return valueOut.Unify(ToProlog(InvokeCaught(mi, target, value)));
        }

        public static Dictionary<EventHandlerInPrologKey, EventHandlerInProlog> PrologEventHandlers =
            new Dictionary<EventHandlerInPrologKey, EventHandlerInProlog>();

        [PrologVisible(ModuleName = ExportModule)]
        static public bool cliAddEventHandler(PlTerm clazzOrInstance, PlTerm memberSpec, PlTerm prologPred)
        {
            object getInstance = GetInstance(clazzOrInstance);
            Type c = GetTypeFromInstance(clazzOrInstance);
            string fn = memberSpec.Name;
            EventInfo fi = c.GetEvent(fn, BindingFlagsALL);
            if (fi == null)
            {
                return Warn("Cant find event " + memberSpec + " on " + c);
            }
            var Key = new EventHandlerInPrologKey();
            Key.Name = prologPred.Name;
            Key.arity = prologPred.Arity;
            Key.Origin = getInstance;
            Key.Event = fi;

            var handlerInProlog = new EventHandlerInProlog(Key);
            lock (PrologEventHandlers) PrologEventHandlers.Add(Key, handlerInProlog);
            fi.AddEventHandler(getInstance, handlerInProlog.Delegate);
            return true;
        }
        [PrologVisible(ModuleName = ExportModule)]
        static public bool cliRemoveEventHandler(PlTerm clazzOrInstance, PlTerm memberSpec, PlTerm prologPred)
        {
            object getInstance = GetInstance(clazzOrInstance);
            Type c = GetTypeFromInstance(clazzOrInstance);
            string fn = memberSpec.Name;
            EventInfo fi = c.GetEvent(fn, BindingFlagsALL);
            if (fi == null)
            {
                return Warn("Cant find event " + memberSpec + " on " + c);
            }
            var Key = new EventHandlerInPrologKey();
            Key.Name = prologPred.Name;
            Key.arity = prologPred.Arity;
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
        static public bool cliGet(PlTerm clazzOrInstance, PlTerm memberSpec, PlTerm valueOut)
        {
            object getInstance = GetInstance(clazzOrInstance);
            Type c = GetTypeFromInstance(clazzOrInstance);
            string fn = memberSpec.Name;
            FieldInfo fi = c.GetField(fn, BindingFlagsALL);
            if (fi != null)
            {
                return valueOut.Unify(ToProlog(fi.GetValue(fi.IsStatic ? null : getInstance)));
            }
            var pi = c.GetProperty(fn, BindingFlagsALL);
            if (pi != null)
            {
                var mi = pi.GetSetMethod();
                if (mi == null)
                {
                    return Warn("Cant find getter for property " + memberSpec + " on " + c);
                }
                return valueOut.Unify(ToProlog(InvokeCaught(mi, mi.IsStatic ? null : getInstance, new object[0])));
            }
            Warn("Cant find getter " + memberSpec + " on " + c);
            return false;
        }

        [PrologVisible(ModuleName = ExportModule)]
        static public bool cliSet(PlTerm clazzOrInstance, PlTerm memberSpec, PlTerm valueIn)
        {
            object getInstance = GetInstance(clazzOrInstance);
            Type c = GetTypeFromInstance(clazzOrInstance);
            string fn = memberSpec.Name;
            FieldInfo fi;
            fi = c.GetField(fn, BindingFlagsALL);
            if (fi != null)
            {
                object value = ToVM(valueIn, fi.FieldType);
                object target = fi.IsStatic ? null : getInstance;
                fi.SetValue(target, value);
                return true;
            }
            var pi = c.GetProperty(fn, BindingFlagsALL);
            if (pi != null)
            {
                var mi = pi.GetSetMethod();
                if (mi == null)
                {
                    return Warn("Cant find setter for property " + memberSpec + " on " + c);
                }
                object value = ToVM(valueIn, pi.PropertyType);
                object target = mi.IsStatic ? null : getInstance;
                InvokeCaught(mi, target, new[] { value });
                return true;
            }
            Warn("Cant find setter " + memberSpec + " on " + c);
            return false;
        }

        [PrologVisible(ModuleName = ExportModule)]
        static public bool cliGetType(PlTerm valueIn, PlTerm valueOut)
        {
            object val = ToVM(valueIn, null);
            if (val == null)
            {
                Warn("Cannot get object for " + valueIn);
                return true;
            }
            return valueOut.Unify(ToProlog(val.GetType()));
        }

        [PrologVisible(ModuleName = ExportModule)]
        static public bool cliGetClass(PlTerm valueIn, PlTerm valueOut)
        {
            object val = ToVM(valueIn, null);
            // extension method
            return valueOut.Unify(ToProlog(val.getClass()));
        }
        [PrologVisible(ModuleName = ExportModule)]
        static public bool cliClassFromType(PlTerm valueIn, PlTerm valueOut)
        {
            Type val = GetType(valueIn);
            if (val == null) return false;
            Class c = ikvm.runtime.Util.getFriendlyClassFromType(val);
            return valueOut.Unify(ToProlog(c));
        }
        [PrologVisible(ModuleName = ExportModule)]
        static public bool cliTypeFromClass(PlTerm valueIn, PlTerm valueOut)
        {
            Class val = GetType(valueIn);
            if (val == null) return false;
            var c = ikvm.runtime.Util.getInstanceTypeFromClass(val);
            return valueOut.Unify(ToProlog(c));
        }
        [PrologVisible(ModuleName = ExportModule)]
        static public bool cliJavaToString(PlTerm valueIn, PlTerm valueOut)
        {
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
            Class val = ToVM(valueIn, null) as Class;
            if (val == null) return false;
            return valueOut.Unify(val.getName());
        }
        [PrologVisible(ModuleName = ExportModule)]
        static public bool cliGetTypename(PlTerm valueIn, PlTerm valueOut)
        {
            Type val = ToVM(valueIn, null) as Type;
            if (val == null) return false;
            return valueOut.Unify(val.FullName);
        }

        private static object GetInstance(PlTerm classOrInstance)
        {
            if (!classOrInstance.IsCompound)
            {
                if (classOrInstance.IsAtom || classOrInstance.IsString) return null;
                return ToVM(classOrInstance, null);
            }
            if (classOrInstance.Name == "@")
            {
                return ToVMLookup("@", 1, classOrInstance[1], classOrInstance, null);
            }
            return null; // GetType(classOrInstance);
        }

        /// <summary>
        /// Returns the Type when denoated by a 'namespace.type' (usefull for static instance specification)
        ///    if a @C#234234  the type of the object unless its a a class
        ///    c(a) => System.Char   "sdfsdf" =>  System.String   uint(5) => System.UInt32
        /// </summary>
        /// <param name="classOrInstance"></param>
        /// <returns></returns>
        private static Type GetTypeFromInstance(PlTerm classOrInstance)
        {
            if (classOrInstance.IsAtom)
            {
                return GetType(classOrInstance);
            }
            if (classOrInstance.IsString)
            {
                return typeof(string);
            }
            object val = ToVM(classOrInstance, null);
            if (val is Type) return (Type)val;
            return val.GetType();
        }

        public static Dictionary<Int64, PlRef> termToObjectPins = new Dictionary<Int64, PlRef>();
        public static Dictionary<object, PlRef> objectToPlRef = new Dictionary<object, PlRef>();
        public static Dictionary<string, PlRef> atomToPlRef = new Dictionary<string, PlRef>();
        public static PlTerm PLVOID { get { return PlTerm.PlCompound("@", PlTerm.PlAtom("null")); } }
        public static PlTerm PLNULL { get { return PlTerm.PlCompound("@", PlTerm.PlAtom("void")); } }
        public static PlTerm PLTRUE { get { return PlTerm.PlCompound("@", PlTerm.PlAtom("true")); } }
        public static PlTerm PLFALSE { get { return PlTerm.PlCompound("@", PlTerm.PlAtom("false")); } }
        public static Object ToFromConvertLock = new object();
        private static PlTerm ToProlog(object o)
        {
            if (o is PlTerm) return (PlTerm)o;
            if (o is string) return VMStringsAsAtoms ? PlTerm.PlAtom((string) o) : PlTerm.PlString((string) o);
            if (o is Term) return ToPLCS((Term)o);
            if (o == null) return PLNULL;
            if (o is ValueType)
            {
                if (o is char)
                {
                    try
                    {
                        return PlTerm.PlAtom(new string((char)o, 1));
                    }
                    catch (Exception e)
                    {
                        // unmappable errors?
                    }
                }
                try
                {
                    bool found;
                    PlTerm res = ToVMNumber(o, out found);
                    if (found) return res;
                }
                catch (Exception e)
                {
                    // conversion errors
                }
            }
            lock (ToFromConvertLock)
            {
                var tag = object_to_tag(o);
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
                            throw new NullReferenceException("already a value for tag=" + oldValue);
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
                            throw new NullReferenceException("already a value for ohandle=" + oldValue);
                        }
                        termToObjectPins[ohandle] = oref;
                    }
                    //PL_put_integer
                    oref.Term = PlTerm.PlCompound("$cli_object", new PlTerm((long) ohandle), plvar);
#else
                    oref.Term = PlTerm.PlCompound("@", PlTerm.PlAtom(tag));
#endif
                    return oref.Term;
                }
                else
                {
                    oref.Term = PlTerm.PlCompound("@", PlTerm.PlAtom(tag));
                    return oref.Term;
                }

            }
        }


        readonly static private Dictionary<object, string> ObjToTag = new Dictionary<object, string>();
        readonly static private Dictionary<string, object> TagToObj = new Dictionary<string, object>();
        public static object tag_to_object(string s)
        {
            lock (ObjToTag)
            {
                object o;
                if (TagToObj.TryGetValue(s, out o))
                {
                    return o;
                }
                return jpl.fli.Prolog.tag_to_object(s);
            }
        }
        public static string object_to_tag(object o)
        {
            lock (ObjToTag)
            {
                string s;
                if (ObjToTag.TryGetValue(o, out s))
                {
                    return s;
                }
                GCHandle gch = GCHandle.Alloc(o, GCHandleType.Normal);
                IntPtr iptr = (IntPtr)gch;
                s = "C#" + iptr.ToInt64();
                ObjToTag[o] = s;
                TagToObj[s] = o;

                return s;
            }
            //return jpl.fli.Prolog.object_to_tag(o);
        }

        private static PlTerm ToVMNumber(object o, out bool converted)
        {
            converted = true;
            if (o is bool)
            {
                if (true.Equals(o)) return PLTRUE;
                if (false.Equals(o)) return PLFALSE;
            }
            // signed types
            if (o is short || o is sbyte || o is int)
                return new PlTerm((int)o);
            if (o is long)
                return new PlTerm((long)o);
            if (o is decimal || o is Single || o is float || o is double)
                return new PlTerm((double)o);
            // unsigned types
            if (o is ushort || o is byte)
                return new PlTerm(Convert.ToInt32(o));
            if (o is UInt32)
                return new PlTerm(Convert.ToInt64(o));
            // potentually too big?!
            if (o is ulong)
            {
                //return new PlTerm(Convert.ToDouble(o));                
            }
            converted = false;
            return default(PlTerm);
        }

        /*
         
  jpl_is_ref(@(Y)) :-
	atom(Y),        % presumably a (garbage-collectable) tag
	Y \== void,     % not a ref
	Y \== false,    % not a ref
	Y \== true.     % not a ref
         
         */
        private static object ToVMLookup(string name, int arity, PlTerm arg1, PlTerm orig, Type pt)
        {
            //{T}
            //@(_Tag)
            if (name == "@" && arity == 1 && arg1.IsAtom)
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
                                lock (atomToPlRef)
                                {
                                    PlRef oldValue;
                                    if (!atomToPlRef.TryGetValue(name, out oldValue))
                                    {
                                        //throw new NullReferenceException("no value for tag=" + name);
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
                            }
                        }
                }
            }
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
                            throw new NullReferenceException("no value for ohandle=" + ohandle);
                        }
                        return oldValue.Value;
                    }
                }
            }
            return (string)orig;
        }

        private static Object ToVM(PlTerm o, Type pt)
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
                        int i = 0;
                        if (0 != libpl.PL_get_integer(o.TermRef, ref i))
                            return i;
                        long lng = (long)o;
                        return lng;
                    }
                    break;
                case PlType.PlFloat:
                    {
                        return (double)o;
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
                            return ToVMLookup(o.Name, o.Arity, o[1], o, pt);
                        }
                    }
                    break;
                default:
                    throw new ArgumentOutOfRangeException();
            }
            return o.ToString();
        }

        internal static Term[] ToJPL(PlTermV args)
        {
            int UPPER = args.Size;
            Term[] target = new Term[UPPER];
            for (int i = 0; i < UPPER; i++)
            {
                target[i] = ToJPL(args[i]);
            }
            return target;
        }

        internal static jpl.fli.term_t ToFLI(PlTermV args)
        {
            return ToFLI(args.A0);
        }

        internal static jpl.fli.term_t ToFLI(PlTerm args)
        {
            return ToFLI(args.TermRef);
        }


        internal static PlTerm ToPLCS(Term args)
        {
            if (args is Atom) return new PlTerm(args.name());
            if (args is jpl.Variable) return new PlTerm((uint)GetField(args, "term_"));
            if (args is jpl.Float) return new PlTerm(args.doubleValue());
            if (args is jpl.Integer) return new PlTerm(args.longValue());
            if (args is jpl.Compound) return PlTerm.PlCompound(args.name(), ToPLCSV(args.args()));
            if (args is jpl.JRef)
            {
                var jref = (jpl.JRef)args;// return new PlTerm(args.doubleValue());
                return ToProlog(jref.@ref());
            }
            throw new ArgumentOutOfRangeException();
        }

        private static PlTermV ToPLCSV(Term[] terms)
        {
            int size = terms.Length;
            PlTermV target = new PlTermV(size);
            for (int i = 0; i < size; i++)
            {
                target[i] = ToPLCS(terms[i]);
            }
            return target;
        }

        private static PlTermV ToPLCSV(PlTerm[] terms)
        {
            int size = terms.Length;
            PlTermV target = new PlTermV(size);
            for (int i = 0; i < size; i++)
            {
                target[i] = terms[i];
            }
            return target;
        }

        private static PlTermV ToPLCSV1(PlTerm a0, PlTerm[] terms)
        {
            int size = terms.Length;
            PlTermV target = new PlTermV(size + 1);
            int to = 1;
            target[0] = a0;
            for (int i = 0; i < size; i++)
            {
                target[to++] = terms[i];
            }
            return target;
        }

        private static object GetField(object term, string s)
        {
            throw new NotImplementedException();
        }

        private static jpl.fli.term_t ToFLI(uint hndle)
        {
            jpl.fli.term_t t = new jpl.fli.term_t();
            t.value = hndle;
            return t;
        }

        internal static Term ToJPL(PlTerm o)
        {
            switch (o.PlType)
            {
                case PlType.PlAtom:
                    {
                        return new Atom((string)o);
                    }
                    break;
                case PlType.PlInteger:
                    {
                        return new jpl.Integer((long)o);
                    }
                    break;
                case PlType.PlFloat:
                    {
                        return new jpl.Float((double)o);
                    }
                    break;
                case PlType.PlString:
                    {
                        return new jpl.Atom((string)o);
                    }
                    break;
                case PlType.PlTerm:
                    {
                        var a = o.Arity;
                        var c = new jpl.Compound(o.Name, a);
                        for (int i = 1; i <= a; i++)
                        {
                            c.setArg(i, ToJPL(o[i]));
                        }
                        return c;
                    }
                    break;
                case PlType.PlVariable:
                    {
                        var v = new jpl.Variable();
                        SetField(v, "term_", o.TermRef);
                        return v;
                    }
                    break;
                case PlType.PlUnknown:
                    {
                        return jpl.Util.textToTerm((string)o);
                    }
                    break;
                default:
                    throw new ArgumentOutOfRangeException();
            }
        }

        public static void SetField(object target, string name, object value)
        {
            FieldInfo field = target.GetType().GetField(name);
            //if (!field.IsPublic) field..IsPublic = true;
            field.SetValue(field.IsStatic ? null : target, value);
        }

        public static jpl.Term InModule(string s, jpl.Term o)
        {
            if (s == null || s == "" || s == "user") return o;
            return new jpl.Compound(":", new Term[] { new jpl.Atom(s), o });
        }

        ///<summary>
        ///</summary>
        ///<exception cref="NotImplementedException"></exception>
        public void Dispose()
        {

        }

        protected string ClientPrefix { get; set; }
        private string _clientModule = null;
        protected string ClientModule
        {
            get { return _clientModule; }
            set { if (value != "user") _clientModule = value; }
        }
        protected PlTerm ThisClientTerm
        {
            get { return ToProlog(this); }
        }

        private bool ModuleCall0(string s, PlTermV termV)
        {
            return PlQuery.PlCall(ClientModule, ClientPrefix + s, termV);
        }

        private bool ModuleCall(string s, params PlTerm[] terms)
        {
            return PlQuery.PlCall(ClientModule, ClientPrefix + s, ToPLCSV1(ThisClientTerm, terms));
        }
        public static PlTerm PlNamed(string name)
        {
            return PlTerm.PlAtom(name);
        }

        public object Eval(object obj)
        {
            PlTerm termout = PlTerm.PlVar();
            if (!ModuleCall("Eval", ToProlog(obj), termout)) return null;
            return ToVM(termout, typeof(System.Object));
        }

        public void Intern(string varname, object value)
        {
            ModuleCall("Intern", PlNamed(varname), ToProlog(value));
        }


        public bool IsDefined(string name)
        {
            return ModuleCall("IsDefined", PlNamed(name));
        }

        public object GetSymbol(string name)
        {
            PlTerm termout = PlTerm.PlVar();
            if (!ModuleCall("GetSymbol", PlNamed(name), termout)) return null;
            return ToVM(termout, typeof(System.Object));
        }

        public object Read(string line, TextWriter @delegate)
        {
            return new Nullable<PlTerm>(PlTerm.PlCompound(line));
        }

        static public void load_swiplcs()
        {

        }

        private static readonly object PrologIsSetupLock = new object();
        private static bool PrologIsSetup;
        public static void SetupProlog()
        {
            lock (PrologIsSetupLock)
            {
                if (PrologIsSetup) return;
                PrologIsSetup = true;
                SetupProlog0();
                RegisterPLCSForeigns();
            }
        }
        public static void SetupProlog0()
        {
            SetupIKVM();

            if (!IsUseableSwiProlog(SwiHomeDir))
            {
                SwiHomeDir = Environment.GetEnvironmentVariable("SWI_HOME_DIR");
                if (!IsUseableSwiProlog(SwiHomeDir))
                {
                    SwiHomeDir = null;
                }
            }
            if (!IsUseableSwiProlog(SwiHomeDir))
            {

                SwiHomeDir = "c:\\Program Files\\pl";

                if (Is64BitComputer() && !Is64BitRuntime())
                {
                    SwiHomeDir = "c:\\Program Files (x86)\\pl";
                }
            }
            AltSwiHomeDir = AltSwiHomeDir ?? ".";
            bool copyPlatFormVersions = false;
            if (!IsUseableSwiProlog(SwiHomeDir))
            {
                SwiHomeDir = AltSwiHomeDir;
                copyPlatFormVersions = true;
            }
            SwiHomeDir = SwiHomeDir ?? AltSwiHomeDir;
            if (IsUseableSwiProlog(SwiHomeDir))
            {
                Environment.SetEnvironmentVariable("SWI_HOME_DIR", SwiHomeDir);
            }
            string platformSuffix = Is64BitRuntime() ? "-x64" : "-x86";
            if (copyPlatFormVersions)
            {
                string destination = Path.Combine(SwiHomeDir, "bin");
                CopyFiles(destination + platformSuffix, destination, true, "*.*", true);
                destination = Path.Combine(SwiHomeDir, "lib");
                CopyFiles(destination + platformSuffix, destination, true, "*.*", true);
            }
            if (IsUseableSwiProlog(SwiHomeDir))
            {
                Environment.SetEnvironmentVariable("SWI_HOME_DIR", SwiHomeDir);
            }
            String path = Environment.GetEnvironmentVariable("PATH");
            if (path != null)
                if (!path.ToLower().StartsWith(SwiHomeDir.ToLower()))
                {
                    Environment.SetEnvironmentVariable("PATH", SwiHomeDir + "\\bin;" + IKVMHome + ";" + path);
                }
            string libpath = "";


            string swiHomeBin = SwiHomeDir + "\\bin";
            libpath += swiHomeBin;
            if (swiHomeBin != IKVMHome)
            {
                libpath += ";";
                libpath += IKVMHome;
            }
            libpath += ";";
            libpath += ".";

            string LD_LIBRARY_PATH = Environment.GetEnvironmentVariable("LD_LIBRARY_PATH");
            if (String.IsNullOrEmpty(LD_LIBRARY_PATH))
            {
                LD_LIBRARY_PATH = libpath;
                Environment.SetEnvironmentVariable("LD_LIBRARY_PATH", LD_LIBRARY_PATH);
            }


            string CLASSPATH = java.lang.System.getProperty("java.class.path");
            string CLASSPATH0 = Environment.GetEnvironmentVariable("CLASSPATH");

            if (String.IsNullOrEmpty(CLASSPATH))
            {
                CLASSPATH = CLASSPATH0;
            }
            string jplcp = clasPathOf(new jpl.JPL());

            if (!JplDisabled)
                CLASSPATH = IKVMHome + "\\SWIJPL.dll" + ";" + IKVMHome + "\\SWIJPL.jar;" + CLASSPATH0;

            Environment.SetEnvironmentVariable("CLASSPATH", CLASSPATH);
            java.lang.System.setProperty("java.class.path", CLASSPATH);
            java.lang.System.setProperty("java.library.path", libpath);
            try
            {
                JPL.setNativeLibraryDir(SwiHomeDir + "\\bin");
                try
                {
                    JPL.loadNativeLibrary();
                }
                catch (Exception e)
                {
                    WriteException(e);
                    JplDisabled = true;
                }
                if (!JplDisabled)
                {
                    SafelyRun(() => jpl.fli.Prolog.initialise());
                }
                SafelyRun(TestClassLoader);

                //if (IsPLWin) return;
                try
                {
                    if (!PlEngine.IsInitialized)
                    {
                        String[] param = { "-q" }; // suppressing informational and banner messages
                        PlEngine.Initialize(param);
                    }
                    if (IsPLWin) return;
                    PlEngine.SetStreamFunctionRead(PlStreamType.Input, new DelegateStreamReadFunction(Sread));
                    PlQuery.PlCall("nl.");
                }
                catch (Exception e)
                {
                    WriteException(e);
                    PlCsDisabled = true;
                }
                //                PlAssert("jpl:jvm_ready");
                //                PlAssert("module_transparent(jvm_ready)");
            }
            catch (Exception exception)
            {
                WriteException(exception);
                return;
            }
        }

        private static bool IsUseableSwiProlog(string swiHomeDir)
        {
            if (string.IsNullOrEmpty(swiHomeDir)) return false;
            if (!Directory.Exists(swiHomeDir)) return false;
            if (File.Exists(swiHomeDir + "\\bin\\libpl.dll"))
            {
                Console.WriteLine("SWI too old: " + swiHomeDir + "\\bin\\libpl.dll");
                return false;
            }
            if (File.Exists(swiHomeDir + "\\bin\\swipl.dll")) return true;
            if (!File.Exists(swiHomeDir + "\\boot32.prc") &&
                !File.Exists(swiHomeDir + "\\boot.prc") &&
                !File.Exists(swiHomeDir + "\\boot64.prc"))
            {
                Console.WriteLine("RC file missing from " + swiHomeDir);
                return false;
            }
            return true;
        }

        //FileInfo & DirectoryInfo are in System.IO
        //This is something you should be able to tweak to your specific needs.
        static void CopyFiles(string source,
                      string destination,
                      bool overwrite,
                      string searchPattern, bool recurse)
        {
            if (Directory.Exists(source))
                CopyFiles(new DirectoryInfo(source), new DirectoryInfo(destination), overwrite, searchPattern, recurse);
        }

        static void CopyFiles(DirectoryInfo source,
                              DirectoryInfo destination,
                              bool overwrite,
                              string searchPattern, bool recurse)
        {
            FileInfo[] files = source.GetFiles(searchPattern);
            if (!destination.Exists)
            {
                destination.Create();
            }
            foreach (FileInfo file in files)
            {
                string destName = Path.Combine(destination.FullName, file.Name);
                try
                {
                    file.CopyTo(destName, overwrite);
                }
                catch (Exception e0)
                {
                    if (!overwrite)
                    {
                        System.Console.Error.WriteLine("file: " + file + " copy to " + destName + " " + e0);
                    }
                    else
                    {
                        try
                        {
                            if (File.Exists(destName))
                            {
                                if (File.Exists(destName + ".dead")) File.Delete(destName + ".dead");
                                File.Move(destName, destName + ".dead");
                                file.CopyTo(destName, false);
                            }
                        }
                        catch (Exception e)
                        {
                            System.Console.Error.WriteLine("file: " + file + " copy to " + destName + " " + e);
                        }
                    }
                }
            }
            if (recurse)
            {
                foreach (var info in source.GetDirectories())
                {
                    string destName = Path.Combine(destination.FullName, info.Name);
                    try
                    {
                        if (!Directory.Exists(destName)) Directory.CreateDirectory(destName);
                        CopyFiles(info, new DirectoryInfo(destName), overwrite, searchPattern, recurse);
                    }
                    catch (Exception e)
                    {
                        System.Console.Error.WriteLine("file: " + info + " copy to " + destName + " " + e);
                    }
                }
            }
        }

        private static void SetupIKVM()
        {
            if (String.IsNullOrEmpty(IKVMHome)) IKVMHome = Environment.GetEnvironmentVariable("IKVM_BINDIR");
            if (String.IsNullOrEmpty(IKVMHome)) IKVMHome = new FileInfo(typeof(ikvm.runtime.Util).Assembly.Location).DirectoryName;
            if (String.IsNullOrEmpty(IKVMHome)) IKVMHome = Environment.CurrentDirectory;
            Environment.SetEnvironmentVariable("IKVM_BINDIR", IKVMHome);
            DirectoryInfo destination = new DirectoryInfo(IKVMHome);
            DirectoryInfo source;
            if (Is64BitRuntime())
            {
                source = new DirectoryInfo(IKVMHome + "\\bin-x64\\");
            }
            else
            {
                source = new DirectoryInfo(IKVMHome + "\\bin-x86\\");
            }
            if (source.Exists) CopyFiles(source, destination, true, "*.*", false);
        }

        public class ScriptingClassLoader : URLClassLoader
        {
            readonly IList<ClassLoader> lc = new List<ClassLoader>();
            IList<AppDomainAssemblyClassLoader> AppDomainAssemblyClassLoaders = new List<AppDomainAssemblyClassLoader>();
            IList<AssemblyClassLoader> AssemblyClassLoaders = new List<AssemblyClassLoader>();
            IList<ClassPathAssemblyClassLoader> ClassPathAssemblyClassLoaders = new List<ClassPathAssemblyClassLoader>();
            IList<URLClassLoader> URLClassLoaders = new List<URLClassLoader>();
            IList<MethodUtil> MethodUtils = new List<MethodUtil>();

            public ScriptingClassLoader(ClassLoader cl)
                : base(new URL[0], cl)
            {
                AddLoader(cl);
            }

            public void AddLoader(ClassLoader cl)
            {
                lock (lc)
                {
                    if (cl != null)
                    {
                        if (lc.Contains(cl)) return;
                        lc.Add(cl);
                        bool added = false;
                        if (cl is AppDomainAssemblyClassLoader)
                        {
                            AppDomainAssemblyClassLoaders.Add((AppDomainAssemblyClassLoader)cl);
                            added = true;
                        }
                        if (cl is AssemblyClassLoader)
                        {
                            AssemblyClassLoaders.Add((AssemblyClassLoader)cl);
                            added = true;
                        }
                        if (cl is ClassPathAssemblyClassLoader)
                        {
                            ClassPathAssemblyClassLoaders.Add((ClassPathAssemblyClassLoader)cl);
                            added = true;
                        }
                        if (!added)
                        {
                            if (cl is MethodUtil)
                            {
                                MethodUtils.Add((MethodUtil)cl);
                                added = true;
                            }
                            else
                                if (cl is URLClassLoader)
                                {
                                    URLClassLoaders.Add((URLClassLoader)cl);
                                    added = true;
                                }
                        }
                        AddLoader(cl.getParent());
                    }
                }
            }

            public static void Check()
            {

            }

            public string FindLibrary(string libname)
            {
                return base.findLibrary(libname);
            }
            public Class LoadClass(string name, bool resolve)
            {
                return base.loadClass(name, resolve);
            }
            public java.lang.Class ResolveClass(java.lang.Class clz)
            {
                base.resolveClass(clz);
                return clz;
            }
        }

        private static void TestClassLoader()
        {
            //using java.lang;
            //IKVM.Internal.BootstrapClassLoader()
            ScriptingClassLoader cl = new ScriptingClassLoader(ClassLoader.getSystemClassLoader());

            string s = "jpl.fli.term_t";
            Class c;
            try
            {
                c = cl.loadClass(s);
            }
            catch (java.lang.ClassNotFoundException e)
            {
            }
            catch (java.security.PrivilegedActionException e)
            {

            }

            foreach (var s1 in new Type[] { 1.GetType(), true.GetType(), "".GetType(), typeof(void), 'a'.GetType(), typeof(Type[]), typeof(IComparable<Type>) })
            {
                c = ikvm.runtime.Util.getFriendlyClassFromType(s1);
                if (c != null)
                {
                    Console.WriteLine("class: " + c + " from type " + s1.FullName);
                    continue;
                }
                Console.WriteLine("cant get " + s1.FullName);
            }

            foreach (var s1 in new jpl.JPL().GetType().Assembly.GetTypes())
            {
                c = ikvm.runtime.Util.getFriendlyClassFromType(s1);
                if (c != null)
                {
                    //Console.WriteLine("" + c);
                    continue;
                }
                Console.WriteLine("cant get " + s1.FullName);
            }
            return;
        }

        private static string clasPathOf(jpl.JPL jpl1)
        {
            string s = null;
            var cl = jpl1.getClass().getClassLoader();
            if (cl != null)
            {
                var r = cl.getResource(".");
                if (r != null)
                {
                    s = r.getFile();
                }
                else
                {
                    var a = jpl1.GetType().Assembly;
                    if (a != null)
                    {
                        s = a.Location;
                    }
                }
            }
            return s;
        }

        //[MTAThread]
        public static void Main(string[] args0)
        {
            bool demo = false;
            SetupProlog();

            if (demo)
            {
                DoQuery("asserta(fff(1))");
                DoQuery("asserta(fff(9))");
                DoQuery("nl");
                DoQuery("flush");

                PlAssert("father(martin, inka)");
                if (!PlCsDisabled)
                {
                    PlQuery.PlCall("assert(father(uwe, gloria))");
                    PlQuery.PlCall("assert(father(uwe, melanie))");
                    PlQuery.PlCall("assert(father(uwe, ayala))");
                    using (PlQuery q = new PlQuery("father(P, C), atomic_list_concat([P,' is_father_of ',C], L)"))
                    {
                        foreach (PlTermV v in q.Solutions)
                            Console.WriteLine(ToCSString(v));

                        foreach (PlQueryVariables v in q.SolutionVariables)
                            Console.WriteLine(v["L"].ToString());


                        Console.WriteLine("all child's from uwe:");
                        q.Variables["P"].Unify("uwe");
                        foreach (PlQueryVariables v in q.SolutionVariables)
                            Console.WriteLine(v["C"].ToString());
                    }
                    //PlQuery.PlCall("ensure_loaded(library(thread_util))");
                    //Warning: [Thread 2] Thread running "thread_run_interactor" died on exception: thread_util:attach_console/0: Undefined procedure: thread_util:win_open_console/5
                    //PlQuery.PlCall("interactor");
                    //Delegate Foo0 = foo0;
                    RegisterPLCSForeigns();
                }

                PlAssert("tc2:-foo2(X,Y),writeq(f(X,Y)),nl,X=5");
                PlAssert("tc3:-foo3(X,Y,Z),Z,writeln(f(X,Y,Z)),X=5");
            }

            ClassFile.ThrowFormatErrors = false;
            libpl.NoToString = true;
            //SafelyRun((() => PlCall("jpl0")));            
            //SafelyRun((() => DoQuery(new Query(new jpl.Atom("jpl0")))));
            libpl.NoToString = false;
            ClassFile.ThrowFormatErrors = true;
            if (args0.Length > 0)
            {
                int i = 0;
                foreach (var s in args0)
                {
                    if (s == "-f")
                    {
                        string s1 = args0[i + 1];
                        args0[i + 1] = "['" + s1 + "']";
                        continue;
                    }
                    PlCall(s);
                    i++;
                }
            }
            if (!JplDisabled)
            {
                var run = new jpl.Atom("prolog");
                while (!IsHalted) SafelyRun(() => DoQuery(new jpl.Query(run)));

            }
            else
            {
                if (!PlCsDisabled)
                    // loops on exception
                    while (!SafelyRun((() => libpl.PL_toplevel()))) ;
            }



            Console.WriteLine("press enter to exit");
            Console.ReadLine();
            SafelyRun((() => PlEngine.PlCleanup()));

            Console.WriteLine("finshed!");


        }

        private static bool SafelyRun(MethodInvoker invoker)
        {
            try
            {
                invoker();
                return true;
            }
            catch (Exception e)
            {
                WriteException(e);
                return false;
            }
        }

        public static void RegisterPLCSForeigns()
        {
            CreatorThread = Thread.CurrentThread;
            PlForeignSwitches Nondeterministic = PlForeignSwitches.Nondeterministic;
            Fn015.Register();
            PlEngine.RegisterForeign(null, "foo2", 2, new DelegateParameterBacktrack2(FooTwo), Nondeterministic);
            PlEngine.RegisterForeign(null, "cliFindClass", 2, new DelegateParameter2(cliFindClass), PlForeignSwitches.None);
            PlEngine.RegisterForeign(null, "cliLoadAssembly", 1, new DelegateParameter1(cliLoadAssembly), PlForeignSwitches.None);
            PlEngine.RegisterForeign(null, "foo3", 3, new DelegateParameterBacktrackVarArgs(FooThree), Nondeterministic | PlForeignSwitches.VarArgs);

            InternMethod(null, "loadAssembly", typeof(PrologClient).GetMethod("LoadAssembly"));
            InternMethod(null, "cwl", typeof(Console).GetMethod("WriteLine", new Type[] { typeof(string) }));
            RegisterJPLForeigns();
            //PLNULL = PlTerm.PlCompound("@", PlTerm.PlAtom("null"));
            //PLVOID = PlTerm.PlCompound("@", PlTerm.PlAtom("void"));
            //PLTRUE = PlTerm.PlCompound("@", PlTerm.PlAtom("true"));
            //PLFALSE = PlTerm.PlCompound("@", PlTerm.PlAtom("false"));
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
                            return t1.Unify(PlTerm.PlCompound("@", PlTerm.PlAtom(tag)));
                        }
                        //var t2 = new PlTerm(t1.TermRef + 1);

                        //libpl.PL_put_atom_chars(t1.TermRef + 1, tag);
                        bool ret = t1.Unify(tag); // = t1;
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
        private static Class cliFindClass1(PlTerm className)
        {
            return ResolveClass(className.Name);
        }
        private static bool cliFindClass(PlTerm term1, PlTerm term2)
        {
            if (term1.IsAtom)
            {
                string className = term1.Name;
                Class c = ResolveClass(className);
                if (c != null)
                {
                    Console.WriteLine("cliFindClass:" + className + " class:" + c);
                    string tag = object_to_tag(c);
                    var t1 = term2;
                    if (t1.IsCompound)
                    {
                        t1 = t1[1];
                    }
                    else if (t1.IsVar)
                    {
                        return t1.Unify(PlTerm.PlCompound("@", PlTerm.PlAtom(tag)));
                    }
                    //var t2 = new PlTerm(t1.TermRef + 1);

                    //libpl.PL_put_atom_chars(t1.TermRef + 1, tag);
                    bool ret = t1.Unify(tag); // = t1;
                    return ret;
                }
                Console.WriteLine("cant ResolveClass " + className);
                return false;
            }
            Console.WriteLine("cant IsAtom " + term1);
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
        private static bool cliToString(PlTerm obj, PlTerm str)
        {
            object o = GetInstance(obj);
            if (o == null) return str.Unify(PlTerm.PlString("" + obj));
            return str.Unify(PlTerm.PlString("" + o));
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
            Type s1 = ResolveType0(name);
            if (s1 != null) return s1;
            if (name.EndsWith("[]"))
            {
                Type t1 = ResolveClassAsType(name.Substring(0, name.Length - 2));
                return t1.MakeArrayType();
            }
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

        private static Type ResolveType(string name)
        {
            if (name == "@" || name == "$cli_object" || name == "array" || name == null) return null;
            if (name.EndsWith("[]"))
            {
                Type t = ResolveType(name.Substring(0, name.Length - 2));
                return t.MakeArrayType();
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
            Type type = Type.GetType(typeName);
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
                    return typeof(byte);
                case "int":
                case "I":
                    return typeof(int);
                case "long":
                case "J":
                    return typeof(long);
                case "short":
                case "S":
                    return typeof(short);
                case "sbyte":
                    return typeof(sbyte);
                case "uint":
                    return typeof(uint);
                case "ulong":
                    return typeof(ulong);
                case "ushort":
                    return typeof(ushort);
                case "decimal":
                    return typeof(decimal);
                case "double":
                    return typeof(double);
                case "float":
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
                case "Z":
                    return typeof(bool);
                default:
                    return null;
            }
        }

        /// <summary>
        /// cliLoadAssembly('SwiPlCs.dll').
        /// cliLoadAssembly('Cogbot.exe').
        /// </summary>
        /// <param name="term1"></param>
        /// <returns></returns>
        private static bool cliLoadAssembly(PlTerm term1)
        {
            try
            {
                if (TaggedObject(term1))
                {
                    var assembly = GetInstance(term1);
                    if (assembly is Assembly)
                    {
                        return LoadAssembly((Assembly)assembly);
                    }
                    if (assembly is Type)
                    {
                        return LoadAssembly(((Type)assembly).Assembly);
                    }
                    return Warn("Cannot get assembly from " + assembly + " for " + term1);
                }
                string name = term1.Name;
                var fi = new FileInfo(name);
                if (fi.Exists)
                {
                    return LoadAssembly(Assembly.LoadFile(fi.FullName));
                }
                else
                {
                    var assembly = Assembly.Load(name);
                    if (assembly == null) assembly = Assembly.LoadWithPartialName(name);
                    if (assembly == null) return Warn("Cannot get assembly from " + name + " for " + term1);
                    return LoadAssembly(assembly);
                }
            }
            catch (Exception exception)
            {
                throw ToPlException(exception);
            }
            return true;
        }

        public static bool LoadAssembly(Assembly assembly)
        {
            lock (AssembliesLoaded)
            {
                if (AssembliesLoading.Contains(assembly))
                {
                    return true;
                }
                AssembliesLoading.Add(assembly);
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
                try
                {
                    LoadAssembly(Assembly.Load(refed));
                }
                catch (Exception e)
                {
                    // Warn(e.Message);
                }
            }
        }

        private static void LoadTypesFromAssembly(Assembly assembly)
        {
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

        [PrologVisible(Name = "cliLoadType", Arity = 1, TypeOf = null)]
        private static void LoadType(Type t)
        {
            foreach (var m in t.GetMethods(BindingFlags.NonPublic | BindingFlags.Public | BindingFlags.Static))
            {
                object[] f = m.GetCustomAttributes(typeof(PrologVisible), false);
                if (f != null && f.Length > 0)
                {
                    cliLoadMethod(m, (PrologVisible)f[0]);
                }
            }
        }

        private static void cliLoadMethod(MethodInfo m, PrologVisible pm)
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

        private static void RegisterJPLForeigns()
        {
            // backup old jpl.pl and copy over it
            if (!JplDisabled)
                SafelyRun(() =>
                {
                    if (File.Exists(IKVMHome + "\\jpl_for_ikvm.phps"))
                    {
                        if (!File.Exists(SwiHomeDir + "\\library\\jpl.pl.old"))
                        {
                            File.Copy(SwiHomeDir + "\\library\\jpl.pl",
                                      SwiHomeDir + "\\library\\jpl.pl.old",
                                      true);
                        }
                        File.Copy(IKVMHome + "\\jpl_for_ikvm.phps", SwiHomeDir + "\\library\\jpl.pl", true);
                    }
                });

            PlEngine.RegisterForeign(null, "link_swiplcs", 1, new DelegateParameter1(link_swiplcs),
                                     PlForeignSwitches.None);
            //JplSafeNativeMethods.install();
            JplSafeNativeMethodsCalled = true;
            //DoQuery(new Query("ensure_loaded(library(jpl))."));
            /*
                             
             
jpl_jlist_demo :-
	jpl_new( 'javax.swing.JFrame', ['modules'], F),
	jpl_new( 'javax.swing.DefaultListModel', [], DLM),
	jpl_new( 'javax.swing.JList', [DLM], L),
	jpl_call( F, getContentPane, [], CP),
	jpl_call( CP, add, [L], _),
	(	current_module( M),
		jpl_call( DLM, addElement, [M], _),
		fail
	;	true
	),
	jpl_call( F, pack, [], _),
	jpl_call( F, getHeight, [], H),
	jpl_call( F, setSize, [150,H], _),
	jpl_call( F, setVisible, [@(true)], _).


% this directive runs the above demo

:- jpl_jlist_demo.

             */
            PlCall("use_module(library(jpl)).");
            PlAssert("jpl0 :- jpl_new( 'java.lang.String', ['hi'], DLM),writeln(DLM)");
            PlAssert("jpl1 :- jpl_new( 'javax.swing.DefaultListModel', [], DLM),writeln(DLM)");
        }

        public static bool PlCall(string s)
        {
            try
            {
                if (!JplDisabled)
                {
                    return DoQuery(s);
                }
                if (PlCsDisabled)
                {
                    WriteDebug("Disabled PlCall " + s);
                    return false;
                }
                return PlQuery.PlCall(s);
            }
            catch (Exception e)
            {
                WriteException(e);
                throw e;
            }
        }

        public static bool PlCall(string m, string f, PlTermV args)
        {
            try
            {
                if (!JplDisabled)
                {
                    return DoQuery(m, f, args);
                }
                if (PlCsDisabled)
                {
                    WriteDebug("Disabled PlCall " + f);
                    return false;
                }
                return PlQuery.PlCall(m, f, args);
            }
            catch (Exception e)
            {
                WriteException(e);
                throw e;
            }
        }

        private static bool link_swiplcs(PlTerm term)
        {
            try
            {
                return true;
                if (JplSafeNativeMethodsCalled)
                {
                    bool enabled = !JplSafeNativeMethodsDisabled;
                    SafelyRun(
                        () => Console.WriteLine("JplSafeNativeMethods called again from " + term + " result=" + enabled));
                    return enabled;
                }
                JplSafeNativeMethodsCalled = true;
                SafelyRun(() => Console.WriteLine("JplSafeNativeMethods call first time from " + term));
                JplSafeNativeMethods.install();
                //var v = new PlTerm("_1");
                //JplSafeNativeMethods.jpl_c_lib_version_1_plc(v.TermRef);
                return true;
            }
            catch (Exception e)
            {
                JplSafeNativeMethodsDisabled = true;
                WriteException(e);
                return false;
            }
        }

        private static bool DoQuery(string query)
        {
            if (JplDisabled) return PlCall(query);
            Query q;
            try
            {
                q = new Query(query);
            }
            catch (Exception e)
            {
                WriteException(e);
                return false;
            }
            return DoQuery(q);
        }

        public static bool DoQuery(string m, string f, PlTermV args)
        {
            if (JplDisabled) return PlCall(m, f, args);
            Query q;
            try
            {
                q = new Query(InModule(m, new Compound(f, ToJPL(args))));
            }
            catch (Exception e)
            {
                WriteException(e);
                return false;
            }
            return DoQuery(q);
        }

        private static bool DoQuery(Query query)
        {
            try
            {
                bool any = false;
                //if (!query.isOpen()) query.open();
                while (query.hasMoreSolutions())
                {
                    any = true;
                    Hashtable ht = query.nextSolution();
                    foreach (var list in ToEnumer(ht.elements()))
                    {
                        string s = "" + list;
                        Console.WriteLine(s);
                    }
                }
                return any;
            }
            catch (Exception exception)
            {
                WriteException(exception);
                return false;
            }

        }

        private static void WriteException(Exception exception)
        {
            java.lang.Exception ex = exception as java.lang.Exception;
            if (ex != null)
            {
                ex.printStackTrace();
            }
            else
            {
                Exception inner = exception.InnerException;
                if (inner != null && inner != exception)
                {
                    WriteException(inner);
                }
                Console.WriteLine("ST: " + exception.StackTrace);
            }

            Console.WriteLine("PrologClient: " + exception);
        }

        private static IEnumerable ToEnumer(java.util.Enumeration enumeration)
        {
            List<object> list = new List<object>();
            while (enumeration.hasMoreElements())
            {
                list.Add(enumeration.nextElement());
            }
            return list;
        }
        private static IEnumerable ToEnumer(java.util.Iterator enumeration)
        {
            List<object> list = new List<object>();
            while (enumeration.hasNext())
            {
                list.Add(enumeration.next());
            }
            return list;
        }

        public static void FooMethod(String print)
        {
            //DoQuery(new Query("asserta(jpl:jvm_ready)."));
            //DoQuery(new Query("asserta(jpl:jpl_c_lib_version(3-3-3-3))."));

            //DoQuery(new Query("module(jpl)."));
            //JplSafeNativeMethods.install();
            //DoQuery("ensure_loaded(library(jpl)).");
            //DoQuery("module(user).");
            //DoQuery(new Query("load_foreign_library(foreign(jpl))."));
            // DoQuery(new Query(new jpl.Compound("member", new Term[] { new jpl.Integer(1), new jpl.Variable("H") })));
            //DoQuery(new Query(new jpl.Atom("interactor")));
            //DoQuery(new Query(new jpl.Compound("writeq", new Term[] { new jpl.Integer(1) })));

            Console.WriteLine(print);
        }

        static internal long Sread(IntPtr handle, System.IntPtr buffer, long buffersize)
        {
            int i = Console.Read();
            if (i == -1) return 0;
            string s = "" + (char)i;
            byte[] array = System.Text.Encoding.Unicode.GetBytes(s);
            System.Runtime.InteropServices.Marshal.Copy(array, 0, buffer, array.Length);
            return array.Length;
        }


        //[TestMethod]
        public void StreamRead()
        {
            DelegateStreamReadFunction rf = new DelegateStreamReadFunction(Sread);
            PlEngine.SetStreamFunctionRead(PlStreamType.Input, rf);
            // NOTE: read/1 needs a dot ('.') at the end
            PlQuery.PlCall("assert( (test_read(A) :- read(A)) )");
            PlTerm t = PlQuery.PlCallQuery("test_read(A)");
            //     Assert.AreEqual(ref_string_read, t.ToString() + ".");
        }
        /*
         
         5.6.1.1 Non-deterministic Foreign Predicates

By default foreign predicates are deterministic. Using the PL_FA_NONDETERMINISTIC attribute (see PL_register_foreign()) it is possible to register a predicate as a non-deterministic predicate. Writing non-deterministic foreign predicates is slightly more complicated as the foreign function needs context information for generating the next solution. Note that the same foreign function should be prepared to be simultaneously active in more than one goal. Suppose the natural_number_below_n/2 is a non-deterministic foreign predicate, backtracking over all natural numbers lower than the first argument. Now consider the following predicate:

quotient_below_n(Q, N) :- natural_number_below_n(N, N1), natural_number_below_n(N, N2), Q =:= N1 / N2, !.

In this predicate the function natural_number_below_n/2 simultaneously generates solutions for both its invocations.

Non-deterministic foreign functions should be prepared to handle three different calls from Prolog:

    * Initial call (PL_FIRST_CALL)
      Prolog has just created a frame for the foreign function and asks it to produce the first answer.
    * Redo call (PL_REDO)
      The previous invocation of the foreign function associated with the current goal indicated it was possible to backtrack. The foreign function should produce the next solution.
    * Terminate call (PL_CUTTED)
      The choice point left by the foreign function has been destroyed by a cut. The foreign function is given the opportunity to clean the environment. 

Both the context information and the type of call is provided by an argument of type control_t appended to the argument list for deterministic foreign functions. The macro PL_foreign_control() extracts the type of call from the control argument. The foreign function can pass a context handle using the PL_retry*() macros and extract the handle from the extra argument using the PL_foreign_context*() macro.

void PL_retry(long)
    The foreign function succeeds while leaving a choice point. On backtracking over this goal the foreign function will be called again, but the control argument now indicates it is a `Redo' call and the macro PL_foreign_context() will return the handle passed via PL_retry(). This handle is a 30 bits signed value (two bits are used for status indication).

void PL_retry_address(void *)
    As PL_retry(), but ensures an address as returned by malloc() is correctly recovered by PL_foreign_context_address().

int PL_foreign_control(control_t)
    Extracts the type of call from the control argument. The return values are described above. Note that the function should be prepared to handle the PL_CUTTED case and should be aware that the other arguments are not valid in this case.

long PL_foreign_context(control_t)
    Extracts the context from the context argument. In the call type is PL_FIRST_CALL the context value is 0L. Otherwise it is the value returned by the last PL_retry() associated with this goal (both if the call type is PL_REDO as PL_CUTTED).

void * PL_foreign_context_address(control_t)
    Extracts an address as passed in by PL_retry_address(). 

Note: If a non-deterministic foreign function returns using PL_succeed or PL_fail, Prolog assumes the foreign function has cleaned its environment. No call with control argument PL_CUTTED will follow.

The code of figure 6 shows a skeleton for a non-deterministic foreign predicate definition.

typedef struct // define a context structure  { ... } context; 
         foreign_t my_function(term_t a0, term_t a1, foreign_t handle) { struct context * ctxt; switch( PL_foreign_control(handle) ) { case PL_FIRST_CALL: ctxt = malloc(sizeof(struct context)); ... PL_retry_address(ctxt); case PL_REDO: ctxt = PL_foreign_context_address(handle); ... PL_retry_address(ctxt); case PL_CUTTED: free(ctxt); PL_succeed; } } 
         
         */
        static public AbstractNondetMethod Fn015 = new ForNext(0, 15);

        // test with (foo2(X,Y)->writeln(p(X,Y));writeln(p(X,Y))),!.
        // test with (foo2(X,Y) *->writeln(p(X,Y));writeln(p(X,Y)),!).
        public static int FooTwo(PlTerm a0, PlTerm a1, IntPtr control)
        {
            var handle = control;
            FRG fc = (FRG)(libpl.PL_foreign_control(control));

            switch (fc)
            {
                case FRG.PL_FIRST_CALL:
                    {
                        var v = NondetContextHandle.ObtainHandle(control, new ForNext(1, a0.intValue()));
                        bool res = v.Setup(new PlTermV(a0, a1));
                        bool more = v.HasMore();
                        if (more)
                        {
                            libpl.PL_retry(v.Handle);
                            return res ? 3 : 0;
                        }
                        return res ? 1 : 0;
                    } break;
                case FRG.PL_REDO:
                    {
                        var v = NondetContextHandle.FindHandle(control);
                        bool res = v.Call(new PlTermV(a0, a1));
                        bool more = v.HasMore();
                        if (more)
                        {
                            libpl.PL_retry(v.Handle);
                            return res ? 3 : 0;
                        }
                        return res ? 1 : 0;
                    } break;
                case FRG.PL_CUTTED:
                    {
                        var v = NondetContextHandle.FindHandle(control);
                        bool res = v.Close(new PlTermV(a0, a1));
                        NondetContextHandle.ReleaseHandle(v);
                        return res ? 1 : 0;
                    } break;
                default:
                    {
                        throw new PlException("no frg");
                        return libpl.PL_fail;
                    }
                    break;
            }
        }

        public static int FooThree(PlTerm a0, int arity, IntPtr control)
        {
            var handle = control;
            FRG fc = (FRG)(libpl.PL_foreign_control(control));

            switch (fc)
            {
                case FRG.PL_FIRST_CALL:
                    {
                        var v = NondetContextHandle.ObtainHandle(control);
                        var tv = new PlTermV(a0, arity);
                        bool res = v.Setup(tv);
                        bool more = v.HasMore();
                        if (more)
                        {
                            libpl.PL_retry(v.Handle);
                            return res ? 3 : 0;
                        }
                        return res ? 1 : 0;
                    } break;
                case FRG.PL_REDO:
                    {
                        var v = NondetContextHandle.FindHandle(control);
                        bool res = v.Call(new PlTermV(a0, arity));
                        bool more = v.HasMore();
                        if (more)
                        {
                            libpl.PL_retry(v.Handle);
                            return res ? 3 : 0;
                        }
                        return res ? 1 : 0;
                    } break;
                case FRG.PL_CUTTED:
                    {
                        var v = NondetContextHandle.FindHandle(control);
                        bool res = v.Close(new PlTermV(a0, arity));
                        NondetContextHandle.ReleaseHandle(v);
                        return res ? 1 : 0;
                    } break;
                default:
                    {
                        throw new PlException("no frg");
                        return libpl.PL_fail;
                    }
                    break;
            }
        }

        private static int CountTo(PlTerm term, PlTerm term2, ref NonDetTest o)
        {
            try
            {

                var c = o.start;
                bool succed = term.Unify("callnum" + c);
                if (!succed)
                {
                    succed = term2.Unify("callnum" + c);
                }

                if (succed)
                {
                    succed = term2.Unify(term);
                }
                if (succed)
                {
                    return libpl.PL_succeed;
                }
                return libpl.PL_fail;
            }
            finally
            {
                o.start++;
            }

        }


        private static string ToCSString(PlTermV termV)
        {
            int s = termV.Size;

            //var a0= termV.A0;
            PlTerm v0 = termV[0];
            PlTerm v1 = termV[1];
            PlQuery.PlCall("write", new PlTermV(v0));
            PlQuery.PlCall("nl");
            PlQuery.PlCall("writeq", new PlTermV(v1));
            PlQuery.PlCall("nl");
            return "";
        }

        private static void PlAssert(string s)
        {
            if (PlCsDisabled)
            {
                WriteDebug("Disabled PlAssert " + s);
                return;
            }
            PlQuery.PlCall("assert((" + s + "))");
        }

        private static void WriteDebug(string s)
        {
            Console.WriteLine(s);
        }

        private static int callNum = 0;


        // [StructLayout(LayoutKind.Sequential)]
        public struct NonDetTest
        {
            public int start;
            public int stop;
            public uint fid;
            //public NonDetDelegate Call;
            //public NonDetDelegate Cutted;
        }

        public class PinnedObject<T> : IDisposable where T : struct
        {
            public T managedObject;
            protected GCHandle handle;
            protected IntPtr ptr;
            protected bool disposed;

            public T ManangedObject
            {
                get
                {
                    return (T)handle.Target;
                }
                set
                {
                    managedObject = value;
                    Marshal.StructureToPtr(value, ptr, false);
                }
            }

            public IntPtr Pointer
            {
                get { return ptr; }
            }

            public PinnedObject()
            {
                handle = GCHandle.Alloc(managedObject, GCHandleType.Pinned);
                ptr = handle.AddrOfPinnedObject();
            }

            ~PinnedObject()
            {
                Dispose();
            }

            public void Dispose()
            {
                if (!disposed)
                {
                    handle.Free();
                    ptr = IntPtr.Zero;
                    disposed = true;
                }
            }

            public void Recopy()
            {
                handle = GCHandle.Alloc(managedObject, GCHandleType.Pinned);
                ptr = handle.AddrOfPinnedObject();
                Marshal.StructureToPtr(managedObject, ptr, false);
            }
        }


        static private PinnedObject<NonDetTest> ndtp;
        public static bool JplDisabled = false;
        public static bool PlCsDisabled = false;
        private static string _ikvmHome;
        public static string IKVMHome
        {
            get { return _ikvmHome; }
            set { _ikvmHome = RemoveTrailingPathSeps(value); }
        }

        private static string _swiHomeDir;// = Path.Combine(".", "swiprolog");
        public static string SwiHomeDir
        {
            get { return _swiHomeDir; }
            set
            {
                _swiHomeDir = RemoveTrailingPathSeps(value); ;
            }
        }

        private static string RemoveTrailingPathSeps(string value)
        {
            if (value != null)
            {
                value = value.TrimEnd("/\\".ToCharArray());
            }
            return value;
        }


        public static string AltSwiHomeDir = "C:\\development\\opensim4opencog";// Path.Combine(".", "swiprolog");
        public static bool JplSafeNativeMethodsDisabled = false;
        public static bool JplSafeNativeMethodsCalled = false;
        public static bool IsHalted = false;
        private static Int64 TopOHandle = 6660000;
        private static readonly Dictionary<string, object> SavedDelegates = new Dictionary<string, object>();
        public static bool FailOnMissingInsteadOfError = true;
        public static Thread CreatorThread;
        public static bool IsPLWin;
        public static bool RedirectStreams = true;
        private static bool VMStringsAsAtoms = false;
        
        // foo(X,Y),writeq(f(X,Y)),nl,X=5.
        public static int Foo(PlTerm t0, PlTerm term2, IntPtr control)
        {
            callNum++;
            if (callNum > 10)
            {
                callNum = 0;
                //return libpl.PL_fail;
            }
            var handle = control;
            FRG fc = (FRG)(libpl.PL_foreign_control(control));

            switch (fc)
            {
                case FRG.PL_FIRST_CALL:
                    unsafe
                    {
                        ndtp = new PinnedObject<NonDetTest>();
                        ndtp.managedObject.start = 1;
                        ndtp.managedObject.stop = 3;
                        //ndtp.managedObject.fid = libpl.PL_open_foreign_frame();

                        ndtp.Recopy();
                        IntPtr ctxt = ndtp.Pointer;
                        goto redo;
                        int succeed = CountTo(t0, term2, ref ndtp.managedObject);
                        if (ndtp.managedObject.start <= ndtp.managedObject.stop)
                        {
                            libpl.PL_retry_address(ctxt);
                        }
                        if (succeed == 0) return 0;
                        return 3;
                    }
                    break;
                case FRG.PL_REDO:
                    unsafe
                    {
                        goto redo;
                        NonDetTest* o = (NonDetTest*)0;
                        IntPtr ctxt = libpl.PL_foreign_context_address(control);
                        if (!ctxt.ToString().Equals("0"))
                        {
                            o = (NonDetTest*)ctxt;
                        }
                        else
                        {
                            o = (NonDetTest*)ndtp.Pointer;
                        }
                        int succeed = CountTo(t0, term2, ref *o);
                        NonDetTest managedObject = *o;
                        if (managedObject.start <= managedObject.stop)
                        {
                            libpl.PL_retry_address(ctxt);
                            if (succeed == 0) return 0;
                            return 3;
                        }
                        if (managedObject.fid != 0)
                        {
                            libpl.PL_close_foreign_frame(managedObject.fid);
                            managedObject.fid = 0;
                        }
                        if (succeed == 0) return 0;
                        return 1;
                    }
                    break;
                case FRG.PL_CUTTED:
                    unsafe
                    {
                        NonDetTest* o = (NonDetTest*)0;
                        IntPtr ctxt = libpl.PL_foreign_context_address(control);
                        if (!ctxt.ToString().Equals("0"))
                        {
                            o = (NonDetTest*)ctxt;
                        }
                        else
                        {
                            o = (NonDetTest*)ndtp.Pointer;
                        }
                        NonDetTest managedObject = *o;
                        if (managedObject.fid != 0)
                        {
                            libpl.PL_close_foreign_frame(managedObject.fid);
                            managedObject.fid = 0;
                        }
                        return libpl.PL_succeed;

                    }
                    break;
                default:
                    {
                        throw new PlException("no frg");
                        return libpl.PL_fail;
                    }
                    break;
            }
        redo:
            unsafe
            {
                NonDetTest* o = (NonDetTest*)0;
                IntPtr ctxt = libpl.PL_foreign_context_address(control);
                var fc0 = libpl.PL_foreign_context(control);
                if (!ctxt.ToString().Equals("0"))
                {
                    o = (NonDetTest*)ctxt;
                }
                else
                {
                    o = (NonDetTest*)ndtp.Pointer;
                }
                int succeed = CountTo(t0, term2, ref *o);
                NonDetTest managedObject = *o;
                if (managedObject.start <= managedObject.stop)
                {
                    libpl.PL_retry_address(ctxt);
                    if (succeed == 0) return 0;
                    return 3;
                }
                if (managedObject.fid != 0)
                {
                    libpl.PL_close_foreign_frame(managedObject.fid);
                    managedObject.fid = 0;
                }
                if (succeed == 0) return 0;
                return 1;
            }
        }

        /* static public void WriteLine(string s, params object[] args)
         {
             Console.DebugWriteLine(s, args);
         }
         */

        public bool Consult(string filename)
        {
            // atomic quote the filename
            string replace = "'" + filename.Replace("\\", "\\\\").Replace("'", "\\'") + "'";
            return PlCall("[" + replace + "]");
        }

        public void InitFromUser()
        {
            ConsultIfExists("cli_swi.pl");
        }

        public void ConsultIfExists(string file)
        {
            if (File.Exists(file)) Consult(file);
        }

    }

    public struct EventHandlerInPrologKey
    {
        public EventInfo Event;
        public Object Origin;
        public String Name;
        public int arity;

    }
    public class EventHandlerInProlog
    {
        public EventHandlerInPrologKey Key;
        readonly public Delegate Delegate;
        readonly public MethodInfo invokeMethod;
        readonly public Type[] parmTypes;
        private readonly Type EventInfoEventHandlerType;

        public object Origin
        {
            get { return Key.Origin; }
        }

        public EventHandlerInProlog(EventHandlerInPrologKey key)
        {
            Key = key;
            var keyEvent = key.Event;
            EventInfoEventHandlerType = keyEvent.EventHandlerType;
            invokeMethod = EventInfoEventHandlerType.GetMethod("Invoke");
            ParameterInfo[] parms = invokeMethod.GetParameters();
            parmTypes = new Type[parms.Length];
            for (int i = 0; i < parms.Length; i++)
            {
                parmTypes[i] = parms[i].ParameterType;
            }
            Delegate = Delegate.CreateDelegate(EventInfoEventHandlerType, this,
                                               GetHandlerMethod(parmTypes, invokeMethod.ReturnType));
        }
        private MethodInfo GetHandlerMethod(Type[] types, Type returnType)
        {
            Type mt = null;
            Type c = GetType();
            int arity = types.Length;
            Type[] typesPlusReturn = new Type[arity + 1];
            for (int i = 0; i < arity; i++)
            {
                typesPlusReturn[i] = types[i];
            }
            typesPlusReturn[arity] = returnType;
            return c.GetMethod("GenericFun" + arity).MakeGenericMethod(typesPlusReturn);
        }

        public R GenericFun0< R>()
        {
            return (R)CallProlog(Origin, new object[] { });
        }
        public R GenericFun1<A, R>(A a)
        {
            return (R)CallProlog(Origin, new object[] { a });
        }
        public R GenericFun2<A, B, R>(A a, B b)
        {
            return (R)CallProlog(Origin, new object[] { a, b });
        }
        public R GenericFun3<A, B, C, R>(A a, B b, C c)
        {
            return (R)CallProlog(Origin, new object[] { a, b, c });
        }
        public R GenericFun4<A, B, C, D, R>(A a, B b, C c, D d)
        {
            return (R)CallProlog(Origin, new object[] { a, b, c, d });
        }

        public object[] GenericFun5<A, B, C, D, E, R>(A a, B b, C c, D d, E e)
        {
            return (R)CallProlog(Origin, new object[] { a, b, c, d, e, f });
        }
        public object[] GenericFun6<A, B, C, D, E, F, R>(A a, B b, C c, D d, E e, F g)
        {
            return (R)CallProlog(Origin, new object[] { a, b, c, d, e, f, g });
        }
        static object CallProlog(object origin, object[] paramz)
        {
            throw new NotImplementedException();
        }

    }

#if false
        void makeHandler()
        {
            // Use Reflection.Emit to create a dynamic assembly that
            // will be run but not saved. An assembly must have at 
            // least one module, which in this case contains a single
            // type. The only purpose of this type is to contain the 
            // event handler method. (In the .NET Framework version 
            // 2.0 you can use dynamic methods, which are simpler 
            // because there is no need to create an assembly, module,
            // or type.)
            AssemblyName aName = new AssemblyName();
            aName.Name = "DynamicTypes";
            AssemblyBuilder ab = AppDomain.CurrentDomain.DefineDynamicAssembly(aName, AssemblyBuilderAccess.Run);
            ModuleBuilder mb = ab.DefineDynamicModule(aName.Name);
            TypeBuilder tb = mb.DefineType("Handler", TypeAttributes.Class | TypeAttributes.Public);
            var localHandler = this.GetType().GetMethod("Invoke");

            ConstructorBuilder Ctor = tb.DefineConstructor(MethodAttributes.Public, CallingConventions.Standard,
                                                           null);
            {
                ILGenerator ilc = Ctor.GetILGenerator();
                ilc.Emit(OpCodes.Ldarg_0);
                ilc.Emit(OpCodes.Call, typeof(object).GetConstructor(new Type[0]));
                ilc.Emit(OpCodes.Ret);
            }
            // Create the method that will handle the event. The name
            // is not important. The method is static, because there is
            // no reason to create an instance of the dynamic type.
            //
            // The parameter types and return type of the method are
            // the same as those of the delegate's Invoke method, 
            // captured earlier.
            MethodBuilder handler = tb.DefineMethod("DynamicHandler",
                                                    MethodAttributes.Public | MethodAttributes.Static,
                                                    invokeMethod.ReturnType, parmTypes);

            // Generate code to handle the event.
            //
            ILGenerator il = handler.GetILGenerator();
            int index = 0;
            foreach (Type type in parmTypes)
            {
                il.Emit(OpCodeFor(type, i));
            }
           // il.Emit(OpCodes.Ldarg_0);
            //il.Emit(OpCodes.Ldarg_1);
            il.EmitCall(OpCodes.Call, localHandler, new Type[] { typeof(object), typeof(object[]) });
            il.Emit(OpCodes.Ret);

            // CreateType must be called before the Handler type can
            // be used. In order to create the delegate that will
            // handle the event, a MethodInfo from the finished type
            // is required.
            Type finished = tb.CreateType();
            eventHandler = finished.GetMethod("DynamicHandler");

        }
#endif


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


    [System.Security.SuppressUnmanagedCodeSecurityAttribute]
    public static class JplSafeNativeMethods
    {
        //private const string DllFileName = @"D:\Lesta\swi-pl\pl\bin\LibPl.dll";
        private const string DllFileName = @"jpl.dll";//"libpl.dll" for 5.7.8; //was 

        public static string DllFileName1
        {
            get { return DllFileName; }
        }
        [DllImport(DllFileName)]
        public static extern void install();

        //[DllImport(DllFileName)]
        //public static extern java.lang.Thread jni_env();

        //[DllImport(DllFileName)]
        //public static extern int jpl_c_lib_version_1_plc(uint term_t);
    }

    public class ForNext : AbstractNondetMethod
    {
        private int start = 0;
        private int end = 0;
        public ForNext(int i, int ii)
        {
            start = i;
            end = ii;
        }

        public override AbstractNondetMethod Clone()
        {
            return new ForNext(start, end);
        }

        #region Overrides of AbstractNondetMethod

        public override bool Setup(PlTermV a0)
        {
            return Call(a0);
        }

        public override bool Call(PlTermV a0)
        {
            bool success = false;
            try
            {
                for (int i = 0; i < a0.Size; i++)
                {
                    if (a0[i].Unify(start))
                    {
                        success = true;
                    }
                }
            }
            finally
            {
                start++;
            }
            return success;

        }

        public override bool Close(PlTermV a0)
        {
            end = start + 1;
            return true;
        }

        public override bool HasMore()
        {
            return start <= end;
        }

        #endregion
    }

    public delegate int NonDetDelegate(PlTerm term, PlTerm term2);

}