using System;
using System.Collections.Generic;
using System.Text;

namespace cogbot.ScriptEngines
{
    abstract public class CommonScriptInterpreter : ScriptInterpreter
    {
        protected CommonScriptInterpreter()
        {
            ScriptManager.Interpreters.Add(this); 
        }
        public object ConvertArgToLisp(object code)
        {
            return ScriptEventListener.argString(code);
        }

        public object ConvertArgFromLisp(object code)
        {
            return code;
        }

        #region ScriptInterpreter Members

        public abstract bool LoadFile(string filename, OutputDelegate WriteLine);

        public bool LoadsFileType(string filenameorext, object self)
        {
            return LoadsFileType(filenameorext);
        }

        public virtual bool LoadsFileType(string filename)
        {
             return GetType().Name.Contains(filename);
        }

        public abstract object Read(string context_name, System.IO.StringReader stringCodeReader, OutputDelegate WriteLine);
        public abstract bool Eof(object codeTree);
        public abstract void Intern(string varname, object value);
        public abstract object Eval(object code);
        public abstract string Str(object code);
        public abstract ScriptInterpreter newInterpreter(object self);
        public abstract bool IsSubscriberOf(string eventName);
        public abstract object GetSymbol(string eventName);
        public abstract void InternType(Type t);


        #endregion

        public abstract void Dispose();
    }

}
