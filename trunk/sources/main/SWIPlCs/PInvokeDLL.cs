using System;
using System.Collections.Generic;
using System.Linq;
using System.Reflection;
using System.Reflection.Emit;
using System.Runtime.InteropServices;
using System.Text;

namespace SwiPlCs
{
       #region P/Invoke
      // Adapted from http://tirania.org/blog/archive/2009/Aug-11.html
      // This is a hack to work around Linux not being able to use libld.so to dynamically
      // load .so modules...
    public class PInvokeDLL
    {
        private string m_dllName;
        private AssemblyBuilder m_assemblyBuilder;
        private ModuleBuilder m_moduleBuilder;

        public PInvokeDLL(string name)
        {
            m_dllName = name;
            m_assemblyBuilder = AppDomain.CurrentDomain.DefineDynamicAssembly(new AssemblyName("ctype"), AssemblyBuilderAccess.Run);
            m_moduleBuilder = m_assemblyBuilder.DefineDynamicModule("ctype");
        }

        public Delegate CreateDelegate(string entry_point, Type delegateType)
        {
            MethodInfo methodInfo = delegateType.GetMethod("Invoke");

            // Get parameter signature information from delegate Invokation
            ParameterInfo returnParameter = methodInfo.ReturnParameter;
            ParameterInfo[] parameters = methodInfo.GetParameters();
            Type[] parameterTypes = new Type[parameters.Length];
            Type[][] parameterTypeRequiredCustomModifiers = new Type[parameters.Length][];
            Type[][] parameterTypeOptionalCustomModifiers = new Type[parameters.Length][];
            for (int i = 0; i < parameters.Length; ++i)
            {
                parameterTypes[i] = parameters[i].ParameterType;
                parameterTypeRequiredCustomModifiers[i] = parameters[i].GetRequiredCustomModifiers();
                parameterTypeOptionalCustomModifiers[i] = parameters[i].GetOptionalCustomModifiers();
            }

            TypeBuilder typeBuilder = m_moduleBuilder.DefineType("ctype_" + entry_point);
            typeBuilder.DefinePInvokeMethod("Invoke", m_dllName, entry_point,
               MethodAttributes.Static | MethodAttributes.PinvokeImpl, CallingConventions.Standard,
               returnParameter.ParameterType, returnParameter.GetRequiredCustomModifiers(), returnParameter.GetOptionalCustomModifiers(),
               parameterTypes, parameterTypeRequiredCustomModifiers, parameterTypeOptionalCustomModifiers,
               CallingConvention.StdCall, CharSet.Auto);

            Type builtType = typeBuilder.CreateType();
            MethodInfo returnMethodInfo = builtType.GetMethod("Invoke", BindingFlags.Static | BindingFlags.NonPublic);
            return Delegate.CreateDelegate(delegateType, returnMethodInfo);
        }



        private PInvokeDLL m_pInvokeDll = null;
        private IntPtr m_hDLL;

        [DllImport("kernel32", SetLastError = true)]
        private static extern IntPtr LoadLibrary(string lpFileName);

        [DllImport("kernel32.dll", SetLastError = true)]
        private static extern bool FreeLibrary(IntPtr hModule);
        
        [DllImport("kernel32", EntryPoint = "GetProcAddress", CharSet = CharSet.Ansi, ExactSpelling = true, SetLastError = true)]
        private static extern IntPtr GetProcAddress(IntPtr hModule, string procName);

        private Delegate GetDelegate(string funcName, Type delegateType)
        {
            if (m_pInvokeDll != null)
                return m_pInvokeDll.CreateDelegate(funcName, delegateType);

            IntPtr funcPtr = GetProcAddress(m_hDLL, funcName);
            if (funcPtr.Equals(IntPtr.Zero))
                return null;
            return Marshal.GetDelegateForFunctionPointer(funcPtr, delegateType);
        }
       #endregion
    }
}
