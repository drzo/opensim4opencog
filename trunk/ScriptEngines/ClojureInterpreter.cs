using System;
using System.Collections.Generic;
using System.Text;

namespace cogbot.ScriptEngines
{
    class ClojureInterpreter : ScriptInterpreter
    {
        DotLisp.Interpreter dotLispInterpreter;

        public bool IsSubscriberOf(string eventName)
        {
            eventName = eventName.ToLower();
            return false;
        }

        public ClojureInterpreter()
        {
            dotLispInterpreter = new DotLisp.Interpreter();
        }
        /// <summary>
        /// 
        /// 
        /// </summary>
        /// <param name="filename"></param>
        /// <returns></returns>
        public bool LoadFile(string filename)
        {
            if (!filename.EndsWith(".lisp"))
            {
                filename = filename + ".lisp";
            }
            System.IO.FileInfo fi = new System.IO.FileInfo(filename);
            if (fi.Exists)
            {
                dotLispInterpreter.LoadFile(filename);
                return true;
            }      
            return false;
        } // method: LoadFile


        /// <summary>
        /// 
        /// </summary>
        /// <param name="context_name"></param>
        /// <param name="stringCodeReader"></param>
        /// <returns></returns>
        public object Read(string context_name, System.IO.StringReader stringCodeReader)
        {
            return dotLispInterpreter.Read(context_name, stringCodeReader);
        } // method: Read


        /// <summary>
        /// 
        /// </summary>
        /// <param name="codeTree"></param>
        /// <returns></returns>
        public bool Eof(object codeTree)
        {
           return dotLispInterpreter.Eof(codeTree);
        } // method: Eof


        /// <summary>
        /// 
        /// </summary>
        /// <param name="varname"></param>
        /// <param name="textForm"></param>
        public void Intern(string varname, object value)
        {
           dotLispInterpreter.Intern(varname, value);
        } // method: Intern


        /// <summary>
        /// 
        /// </summary>
        /// <param name="code"></param>
        /// <returns></returns>
        public object Eval(object code)
        {
            return dotLispInterpreter.Eval(code);
        } // method: Eval


        /// <summary>
        /// 
        /// </summary>
        /// <param name="code"></param>
        /// <returns></returns>
        public string Str(object code)
        {
            return dotLispInterpreter.Str(code);
        } // method: Str


        /// <summary>
        /// 
        /// </summary>
        /// <returns></returns>
        public ScriptInterpreter newInterpreter()
        {
            return new DotLispInterpreter();
        } // method: newInterpreter

    }
}
