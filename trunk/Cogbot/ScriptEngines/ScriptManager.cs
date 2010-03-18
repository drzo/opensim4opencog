using System;
using System.Collections.Generic;
using System.Text;

namespace cogbot.ScriptEngines
{
    public class ScriptManager
    {
        static public HashSet<ScriptInterpreter> Interpreters = new HashSet<ScriptInterpreter>();
        /// <summary>
        /// 
        /// </summary>
        /// <param name="type"></param>
        /// <returns></returns>
        public static ScriptInterpreter LoadScriptInterpreter(string type, object self)
        {
            if (Interpreters.Count==0)
            {
                BotScriptInterpreter newBotScriptInterpreter = new BotScriptInterpreter();
                newBotScriptInterpreter = (BotScriptInterpreter) newBotScriptInterpreter.newInterpreter(self);
                Interpreters.Add(newBotScriptInterpreter);
                DotLispInterpreter dotLispInterpreter = new DotLispInterpreter();
                dotLispInterpreter = (DotLispInterpreter) dotLispInterpreter.newInterpreter(self);
                Interpreters.Add(dotLispInterpreter);
            }
            foreach (var set in Interpreters)
            {
                if (set.LoadsFileType(type, self))
                {
                    if (set is BotScriptInterpreter)
                    {
                        if (self is BotClient)
                        {
                            ((BotScriptInterpreter) set).BotClient = self as BotClient;
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
                    return new DotLispInterpreter();
                }
                if (type.StartsWith("cyc"))
                {
                    return new CycInterpreter();
                }
                if (type.StartsWith("abcl"))
                {
                    return new ABCLInterpreter();
                }
            }
            //default
            return new DotLispInterpreter();
        } // method: LoadScriptInterpreter
    }
}
