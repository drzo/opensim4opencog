//Copyright (c) 2003, Rich Hickey
//licensed under the BSD license - see license.txt
using System;
using System.Reflection;

namespace DotLisp
{

    public abstract class CLSMember : IFunction
    {
        public abstract Object Invoke(params Object[] args);
        internal virtual Object getValue()
        {
            return this;
        }

        internal virtual void setValue(Object val)
        {
            throw new Exception("Can't set value of member symbol");
        }

        internal String name;

        public override String ToString()
        {
            return "CLSMember:" + name;
        }

        internal static Object GetDefaultIndexedProperty(Object target, Object[] args)
        {
            Type targetType = target.GetType();
            return targetType.InvokeMember("", BindingFlags.Default | BindingFlags.GetProperty, null,
                                                     target, args);
        }

        internal static Object SetDefaultIndexedProperty(Object target, Object[] args)
        {
            Type targetType = target.GetType();
            return targetType.InvokeMember("", BindingFlags.Default | BindingFlags.SetProperty, null,
                                                     target, args);
        }

        internal static CLSMember FindMember(String name, SymbolTable symtab, String typename, Boolean isStatic)
        {
            if (typename == null)
            {
                return new CLSLateBoundMember(name);
            }
            Type[] types = symtab.findTypes(typename);
            Exception e = null;
            foreach (Type type in types)
            {
                try
                {
                    return FindMember(name, type, isStatic);
                }
                catch (NotImplementedException nie)
                {
                    if (e == null) e = nie;
                }
                catch (NotSupportedException nie)
                {
                    e = nie;
                }
            }
            throw e ?? new Exception("Can't find " +
                                          (isStatic ? "static" : "instance") +
                                          " member: " + name + " in non Type: " + typename);
        }

        internal static CLSMember FindMember(String name, Type type, Boolean isStatic)
        {
            if (type == null)
            {
                return new CLSLateBoundMember(name);
            }
            //lookup name in type, create approriate derivee
            MemberInfo[] members = type.GetMember(name,
                                                              BindingFlags.Public | BindingFlags.NonPublic |
                                                              (isStatic ? BindingFlags.Static : BindingFlags.Instance)
                                                             ); //all public members with matching isstatic
            if (members.Length == 0)
            {
                throw new NotImplementedException("Can't find " +
                                          (isStatic ? "static" : "instance") +
                                          " member: " + name + " in Type: " + type.FullName);
            }

            //CLS says all same-named members must be same type (field or param or method)
            //so just check first one
            else if (members[0] is EventInfo)
            {
                EventInfo mi = (EventInfo)members[0];
                return new CLSEvent(name, type, mi, false);
            }
            if (members[0] is FieldInfo)
            {
                FieldInfo fi = (FieldInfo)members[0];
                return new CLSField(name, type, fi, fi.IsStatic);
            }

            else if (members[0] is PropertyInfo)
            {
                PropertyInfo pi = (PropertyInfo)members[0];
                //why doesn't PropertyInfo have IsStatic?
                MethodInfo mi = pi.GetGetMethod();
                return new CLSProperty(name, type, members, mi.IsStatic);
            }

            else if (members[0] is MethodInfo)
            {
                MethodInfo mi = (MethodInfo)members[0];
                return new CLSMethod(name, type, members, mi.IsStatic);
            }
            else
            {
                throw new NotSupportedException("Unsupported type of member: " + name + " in Type: "
                                          + type.Name + " MemberType: " + members[0].MemberType);
            }
        }

        static public bool Coerce(object[] argarray, Type[] argtypes, ParameterInfo[] paramInfos, out object[] parameters)
        {
            int paramInfosLength = paramInfos.Length;
            int argarrayLength = argarray.Length;
            bool misMatch = false;
            if (paramInfosLength == argarrayLength)
            {
                bool changed = false;
                if (true)
                {
                    parameters = new object[paramInfosLength];
                    for (int index = 0; index < paramInfosLength; index++)
                    {
                        var paramInfo = paramInfos[index];
                        var param = argarray[index];
                        bool changed0;
                        if (!CoerceOne(param, argtypes[index], paramInfo, out param, out changed0))
                        {
                            misMatch = true;
                        }
                        if (changed0) changed = true;
                        parameters[index] = param;
                    }
                }
                if (!changed)
                {
                    parameters = argarray;
                }
                return !misMatch;
            }
            if (paramInfosLength > argarrayLength)
            {
                parameters = null;
                return false;
            }
            parameters = new object[paramInfosLength];
            int currentParam = 0;
            for (int i = 0; i < paramInfosLength; i++)
            {
                ParameterInfo p = paramInfos[i];
                if (currentParam > argarrayLength)
                {
                    parameters[i] = p.DefaultValue;
                }
                else if (p.ParameterType.IsArray)
                {
                    if (!argtypes[currentParam].IsArray)
                    {
                        // the last arg is an array fill it with the rest and return
                        if (i + 1 == paramInfosLength)
                        {
                            object[] pas = new object[argarrayLength - currentParam];
                            parameters[i] = pas;
                            i = 0;
                            while (currentParam < argarrayLength)
                            {
                                pas[i++] = argarray[currentParam++];
                            }
                            return true;
                        }
                    }

                }
                else
                {
                    parameters[i] = argarray[currentParam];
                }
                currentParam++;
            }
            return true;
        }

        private static bool CoerceOne(object param, Type type, ParameterInfo paramInfo, out object paramOut, out bool changed)
        {
            changed = false;
            paramOut = param;                    
            var parameterType = paramInfo.ParameterType;
            // perfect
            if (parameterType.IsInstanceOfType(param)) return true;
            // can be true param is null
            if (type != null && parameterType.IsAssignableFrom(type)) return true;
            if (param == null)
            {
                return true;
            }
            Closure c = param as Closure;
            if (c!=null)
            {                    
                paramOut = c.Invoke();
                if (paramOut!=null)
                {
                    type = paramOut.GetType();
                }
                changed = CoerceOne(paramOut, type, paramInfo, out paramOut, out changed);
                
            }
          
            return false;
        }

        public static Type[] GetTypeArray(object[] argarray)
        {
            try
            {
                return Type.GetTypeArray(argarray);
            }
            catch (NullReferenceException)
            {
                int argarrayLength = argarray.Length;
                Type[] types = new Type[argarrayLength];
                for (int index = 0; index < argarrayLength; index++)
                {
                    types[index] = GetType(argarray[index]);
                }
                return types;
            }
        }

        private static Type GetType(object o)
        {
            if (o == null) return null;
            return o.GetType();
        }
    }
}
