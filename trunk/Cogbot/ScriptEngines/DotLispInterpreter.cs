using System;
using System.Collections.Generic;
using System.Text;

namespace cogbot.ScriptEngines
{
    class DotLispInterpreter : CommonScriptInterpreter
    {
        DotLisp.Interpreter dotLispInterpreter;

        public override bool IsSubscriberOf(string eventName)
        {
            if (!eventName.Contains(".")) eventName = eventName.ToLower();
            DotLisp.Symbol o = dotLispInterpreter.intern(eventName);//.Read("DefinedFunction", new System.IO.StringReader(eventName));           
            if (o!=null && o.isDefined()) return true;
            return false;
        }

        public override object GetSymbol(string eventName)
        {
            eventName = eventName.ToLower();
            DotLisp.Symbol o = dotLispInterpreter.intern(eventName);//.Read("DefinedFunction", new System.IO.StringReader(eventName));           
            return o;
        }

        public override void InternType(Type t)
        {
            dotLispInterpreter.InternType(t);
        }

        public DotLispInterpreter()
        {
            dotLispInterpreter = new DotLisp.Interpreter();
        }
        /// <summary>
        /// 
        /// 
        /// </summary>
        /// <param name="filename"></param>
        /// <returns></returns>
        public override bool LoadFile(string filename)
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
        public override object Read(string context_name, System.IO.StringReader stringCodeReader)
        {
            return dotLispInterpreter.Read(context_name, stringCodeReader);
        } // method: Read


        /// <summary>
        /// 
        /// </summary>
        /// <param name="codeTree"></param>
        /// <returns></returns>
        public override bool Eof(object codeTree)
        {
           return dotLispInterpreter.Eof(codeTree);
        } // method: Eof


        /// <summary>
        /// 
        /// </summary>
        /// <param name="varname"></param>
        /// <param name="textForm"></param>
        public override void Intern(string varname, object value)
        {
           dotLispInterpreter.Intern(varname, value);
        } // method: Intern


        /// <summary>
        /// 
        /// </summary>
        /// <param name="code"></param>
        /// <returns></returns>
        public override object Eval(object code)
        {
            return dotLispInterpreter.Eval(code);
        } // method: Eval


        /// <summary>
        /// 
        /// </summary>
        /// <param name="code"></param>
        /// <returns></returns>
        public override string Str(object code)
        {
            return dotLispInterpreter.Str(code);
        } // method: Str


        /// <summary>
        /// 
        /// </summary>
        /// <returns></returns>
        public override ScriptInterpreter newInterpreter()
        {
            return new DotLispInterpreter();
        } // method: newInterpreter


    }
}