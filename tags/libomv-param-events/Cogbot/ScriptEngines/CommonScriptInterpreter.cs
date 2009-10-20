using System;
using System.Collections.Generic;
using System.Text;

namespace cogbot.ScriptEngines
{
    abstract public class CommonScriptInterpreter : ScriptInterpreter
    {
        public object ConvertArgToLisp(object code)
        {
            return ScriptEventListener.argString(code);
        }

        public object ConvertArgFromLisp(object code)
        {
            return code;
        }

        #region ScriptInterpreter Members

        public abstract bool LoadFile(string filename);
        public abstract object Read(string context_name, System.IO.StringReader stringCodeReader);
        public abstract bool Eof(object codeTree);
        public abstract void Intern(string varname, object value);
        public abstract object Eval(object code);
        public abstract string Str(object code);
        public abstract ScriptInterpreter newInterpreter();
        public abstract bool IsSubscriberOf(string eventName);
        public abstract object GetSymbol(string eventName);
        public abstract void InternType(Type t);


        #endregion

        public abstract void Dispose();
    }

}
