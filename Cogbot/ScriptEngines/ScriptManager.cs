using System;
using System.Collections.Generic;
using System.Reflection;
using System.Text;
using System.Windows.Forms;

namespace cogbot.ScriptEngines
{
    public class ScriptManager
    {
        static public List<ScriptInterpreter> _Interpreters = new List<ScriptInterpreter>();
        static public HashSet<Type> _Types = new HashSet<Type>();
        static public object Lock = new object();

        static public bool AddType(Type type)
        {
            lock (Lock)
            {
                if (_Types.Add(type))
                {
                    foreach (var set in _Interpreters)
                    {
                        set.InternType(type);
                    }
                    return true;
                }
                return false;
            }
        }

        public static ScriptInterpreter LoadScriptInterpreter(string type, object self)
        {
            try
            {
                return LoadScriptInterpreter0(type, self);
            }
            catch (Exception e)
            {
                WriteLine("LoadScriptInterpreter: " + e);
                throw e;
            }
        }
        public static bool SafelyDo(string msg, MethodInvoker self)
        {
            try
            {
                self();
                return true;
            }
            catch (Exception e)
            {
                WriteLine("LoadScriptInterpreter: " + msg + " - " + e);
                return false;
            }
        }
        /// <summary>
        /// 
        /// </summary>
        /// <param name="type"></param>
        /// <returns></returns>
        public static ScriptInterpreter LoadScriptInterpreter0(string type, object self)
        {
            var Interpreters = _Interpreters;
            lock (Interpreters)
            {
                if (Interpreters.Count == 0)
                {
                    foreach (var a in AppDomain.CurrentDomain.GetAssemblies())
                    {
                        Assembly assembly = a;
                        SafelyDo("load assemly " + a, () => ScanAssemblyForScriptInterpretors(assembly, self));
                    }
                    if (false)
                    {

                        //if (self is ClientManager) self = ((ClientManager)self).LastBotClient ?? self;
                        BotScriptInterpreter newBotScriptInterpreter = new BotScriptInterpreter(self);
                        newBotScriptInterpreter = (BotScriptInterpreter) newBotScriptInterpreter.newInterpreter(self);
                        AddInterpreter(newBotScriptInterpreter);
                        DotLispInterpreter dotLispInterpreter = new DotLispInterpreter(self);
                        dotLispInterpreter = (DotLispInterpreter) dotLispInterpreter.newInterpreter(self);
                        AddInterpreter(dotLispInterpreter);
                    }
                }
                foreach (var set in Interpreters)
                {
                    if (set.LoadsFileType(type, self))
                    {
                        if (set is BotScriptInterpreter)
                        {
                            if (self is BotClient)
                            {
                                ((BotScriptInterpreter)set).BotClient = self as BotClient;
                            }
                        }
                        return set;
                    }
                }
                if (type != null)
                {
                    type = type.ToLower();
                    if (type.StartsWith("dotlisp"))
                    {
                        return new DotLispInterpreter(self);
                    }
                    if (type.StartsWith("cyc"))
                    {
                        return new CycInterpreter(self);
                    }
                    if (type.StartsWith("abcl"))
                    {
                        return new ABCLInterpreter(self);
                    }
                }
                //default
                return new DotLispInterpreter(self);
            } // method: LoadScriptInterpreter
        }

        private static void ScanAssemblyForScriptInterpretors(Assembly a, object self)
        {
            Type bt = typeof (object);
            if (!ReferenceEquals(self, null))
            {
                bt = self.GetType();
            }

            foreach (var list in a.GetTypes())
            {
                if (list.IsAbstract) continue;
                if (typeof (ScriptInterpreter).IsAssignableFrom(list))
                {
                    SafelyDo("load type " + list, () =>
                                                      {
                                                          var mi = list.GetConstructor(new Type[] {bt});
                                                          if (mi == null)
                                                              mi = list.GetConstructor(new Type[] {typeof (object)});
                                                          if (mi != null)
                                                          {
                                                              ScriptInterpreter si =
                                                                  (ScriptInterpreter) mi.Invoke(new object[] {self});
                                                              AddInterpreter(si);
                                                          }
                                                          else
                                                          {
                                                              mi = list.GetConstructor(new Type[0]);
                                                              ScriptInterpreter si =
                                                                  (ScriptInterpreter) mi.Invoke(new object[0]);
                                                              si.Self = self;
                                                              AddInterpreter(si);
                                                          }
                                                      });
                }
            }

        }

        public static void AddInterpreter(ScriptInterpreter interpreter)
        {
            lock (Lock)
            {
                if (!_Interpreters.Contains(interpreter))
                {
                    _Interpreters.Add(interpreter);
                    foreach (var set in _Types)
                    {
                        interpreter.InternType(set);
                    }
                }
            }
        }

        public static void RemoveInterpreter(ScriptInterpreter interpreter)
        {
            lock (Lock)
            {
                if (!_Interpreters.Contains(interpreter))
                {
                    _Interpreters.Remove(interpreter);
                }
            }
        }

        public static void WriteLine(string msg, params object[] args)
        {
            Console.WriteLine(msg, args);
        }
    }
}
