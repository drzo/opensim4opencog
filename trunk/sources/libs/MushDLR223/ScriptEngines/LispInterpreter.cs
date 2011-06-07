using System;
using System.Collections.Generic;
using System.IO;
using System.Text;
using MushDLR223.Utilities;

namespace MushDLR223.ScriptEngines
{
    abstract public class LispInterpreter : CommonScriptInterpreter
    {

        public LispInterpreter(object self)
            : base(self)
        {
        }

        sealed public override object Self
        {
            get { return GetSymbol("this"); }
            set
            {
                Intern("*SELF*", value);
                object thiz = value ?? this;
                Intern("this", thiz);
            }
        }

        public override bool LoadsFileType(string filename)
        {
            filename = filename.ToLower();
            bool b = GetType().Name.ToLower().StartsWith(filename);
            if (b)
            {
                ScriptManager.WriteLine("LispInterpreter LoadsFileType " + GetType() + " => " + filename);
            }
            return b;
        }

        /// <summary>
        /// 
        /// </summary>
        /// <returns></returns>
        //public abstract ScriptInterpreter newInterpreter(object self);


        public virtual object ReadFromString(string cmd)
        {

            StringReader stringCodeReader = new StringReader(cmd);
            object lispCode = Read("ReadFromString" + GetType(), stringCodeReader, DLRConsole.DebugWriteLine);
            if (Eof(lispCode))
                return lispCode;
            return lispCode;
        }
    }
}