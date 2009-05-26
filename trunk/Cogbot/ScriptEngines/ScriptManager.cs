using System;
using System.Collections.Generic;
using System.Text;

namespace cogbot.ScriptEngines
{
    public class ScriptManager
    {
        /// <summary>
        /// 
        /// </summary>
        /// <param name="type"></param>
        /// <returns></returns>
        public static ScriptInterpreter LoadScriptInterpreter(string type)
        {
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
