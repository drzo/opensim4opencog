using System;
using System.Reflection;

namespace DotLisp
{


    internal class CLSEvent : CLSMember
    {
        private Type type;
        private EventInfo eventInfo;
        private Boolean isStatic;

        internal CLSEvent(String name, Type type, EventInfo eventInfo, Boolean isStatic)
        {
            this.isStatic = isStatic;
            this.eventInfo = eventInfo;
            this.type = type;
            this.name = name;
        }

        public override Object Invoke(params Object[] args)
        {
            try
            {
                return Invoke0(args);
            }
            catch (Exception e)
            {
                Console.WriteLine("" + e);
                return Invoke0(args);
            }

        }

        static Binder methodBinder = System.Type.DefaultBinder;

        //
        // Summary:
        //     Invokes the specified member, using the specified binding constraints and
        //     matching the specified argument list.
        //
        // Parameters:
        //   name:
        //     The System.String containing the name of the constructor, method, property,
        //     or field member to invoke.-or- An empty string ("") to invoke the default
        //     member. -or-For IDispatch members, a string representing the DispID, for
        //     example "[DispID=3]".
        //
        //   invokeAttr:
        //     A bitmask comprised of one or more System.Reflection.BindingFlags that specify
        //     how the search is conducted. The access can be one of the BindingFlags such
        //     as Public, NonPublic, Private, InvokeMethod, GetField, and so on. The type
        //     of lookup need not be specified. If the type of lookup is omitted, BindingFlags.Public
        //     | BindingFlags.Instance | BindingFlags.Static are used.
        //
        //   binder:
        //     A System.Reflection.Binder object that defines a set of properties and enables
        //     binding, which can involve selection of an overloaded method, coercion of
        //     argument types, and invocation of a member through reflection.-or- null,
        //     to use the System.Type.DefaultBinder. Note that explicitly defining a System.Reflection.Binder
        //     object may be requird for successfully invoking method overloads with variable
        //     arguments.
        //
        //   target:
        //     The System.Object on which to invoke the specified member.
        //
        //   args:
        //     An array containing the arguments to pass to the member to invoke.
        //
        // Returns:
        //     An System.Object representing the return value of the invoked member.
        //
        // Exceptions:
        //   System.ArgumentNullException:
        //     invokeAttr contains CreateInstance and typeName is null.
        //
        //   System.ArgumentException:
        //     args is multidimensional.-or- invokeAttr is not a valid System.Reflection.BindingFlags
        //     attribute.-or- invokeAttr contains CreateInstance combined with InvokeMethod,
        //     GetField, SetField, GetProperty, or SetProperty.-or- invokeAttr contains
        //     both GetField and SetField.-or- invokeAttr contains both GetProperty and
        //     SetProperty.-or- invokeAttr contains InvokeMethod combined with SetField
        //     or SetProperty.-or- invokeAttr contains SetField and args has more than one
        //     element.-or- This method is called on a COM object and one of the following
        //     binding flags was not passed in: BindingFlags.InvokeMethod, BindingFlags.GetProperty,
        //     BindingFlags.SetProperty, BindingFlags.PutDispProperty, or BindingFlags.PutRefDispProperty.-or-
        //     One of the named parameter arrays contains a string that is null.
        //
        //   System.MethodAccessException:
        //     The specified member is a class initializer.
        //
        //   System.MissingFieldException:
        //     The field or property cannot be found.
        //
        //   System.MissingMethodException:
        //     The method cannot be found.-or- The current System.Type object represents
        //     a type that contains open type parameters, that is, System.Type.ContainsGenericParameters
        //     returns true.
        //
        //   System.Reflection.TargetException:
        //     The specified member cannot be invoked on target.
        //
        //   System.Reflection.AmbiguousMatchException:
        //     More than one method matches the binding criteria.
        //
        //   System.NotSupportedException:
        //     The .NET Compact Framework does not currently support this method.
        //
        //   System.InvalidOperationException:
        //     The method represented by name has one or more unspecified generic type parameters.
        //     That is, the method's System.Reflection.MethodInfo.ContainsGenericParameters
        //     property returns true.
        public Object Invoke0(params Object[] args)
        {
            Object target = null;
            Object[] argarray = args;
            if (!isStatic)
            {
                // instance field gets target from first arg
                // MEH: More informative exception.
                if (args.Length == 0)
                    throw new Exception(".Type:EventInfo requires a target.");

                target = args[0];
                argarray = Util.vector_rest(args);
            }
            MethodInfo m = eventInfo.GetRaiseMethod();

            object[] parameters;
            Exception lastException = null;
            if (m != null)
            {
                try
                {
                    Type[] argtypes = Type.GetTypeArray(argarray);
                    ParameterInfo[] paramInfos = m.GetParameters();
                    if (Coerce(argarray, argtypes, paramInfos, out parameters))
                        return m.Invoke(target, parameters);
                }
                catch (Exception e)
                {
                    lastException = e;
                }
            }
            else
            {
                {
                    FieldInfo fieldInfo = type.GetField(eventInfo.Name,
                                                        BindingFlags.Instance | BindingFlags.NonPublic |
                                                        BindingFlags.Public);
                    if (fieldInfo != null)
                    {
                        Delegate del = fieldInfo.GetValue(target) as Delegate;
                        //   Type[] argtypes = Type.GetTypeArray(argarray);
                        //ParameterInfo[] paramInfos = eventInfo.EventHandlerType;//.GetParameters();
                        // if (Coerce(argarray, argtypes, paramInfos, out parameters))
                        //   return m.Invoke(target, parameters);

                        if (del != null)
                            del.DynamicInvoke(argarray);
                        return true;
                    }
                }
            }
            if (lastException != null)
            {
                Console.WriteLine("rethrowing " + lastException.ToString());
                throw lastException;
            }

            throw new Exception("Can't find matching event: " + name + " for: " + type.Name);
        }

        static bool Coerce(object[] argarray, Type[] argtypes, ParameterInfo[] paramInfos, out object[] parameters)
        {
            int paramInfosLength = paramInfos.Length;
            int argarrayLength = argarray.Length;
            if (paramInfosLength == argarrayLength)
            {
                parameters = argarray;
                return true;
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

    }
}
