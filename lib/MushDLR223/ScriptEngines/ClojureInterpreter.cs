using System;
using System.Collections.Generic;
using System.Text;

namespace MushDLR223.ScriptEngines
{
    public class ClojureInterpreter : DotLispInterpreter
    {

        public override void Dispose()
        {
            dotLispInterpreter.Dispose();
        }

        public override void InternType(Type t)
        {
            dotLispInterpreter.InternType(t);
        }

        public override object GetSymbol(string eventName)
        {
           // object r = Eval(base.ReadFromString(eventName));
            //eventName = eventName.ToLower();
            DotLisp.Symbol o = dotLispInterpreter.intern(eventName);//.Read("DefinedFunction", new System.IO.StringReader(eventName));           
            return o;
        }

        public override bool IsSubscriberOf(string eventName)
        {
            DotLisp.Symbol o = dotLispInterpreter.intern(eventName);//.Read("DefinedFunction", new System.IO.StringReader(eventName));           
            return o.isDefined();
        }

        public ClojureInterpreter(object self)
            : base(self)
        {
        }

        /// <summary>
        /// 
        /// 
        /// </summary>
        /// <param name="filename"></param>
        /// <returns></returns>
        public override bool LoadFile(string filename, OutputDelegate WriteLine)
        {
            if (!filename.EndsWith(".cloj"))
            {
                filename = filename + ".cloj";
            }
            System.IO.FileInfo fi = new System.IO.FileInfo(filename);
            if (fi.Exists)
            {
                dotLispInterpreter.LoadFile(filename);
                return true;
            }      
            return false;
        }

        public override bool LoadsFileType(string filename)
        {
            return filename.EndsWith("cloj") || base.LoadsFileType(filename);
        }

// method: LoadFile


        /// <summary>
        /// 
        /// </summary>
        /// <param name="context_name"></param>
        /// <param name="stringCodeReader"></param>
        /// <returns></returns>
        public override object Read(string context_name, System.IO.TextReader stringCodeReader, OutputDelegate WriteLine)
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
            if (value!=null)
            {
                InternType(value.GetType());
            }
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
        public override ScriptInterpreter newInterpreter(object self)
        {
            var v = new DotLispInterpreter(self);
            v.Intern("*SELF*", self);
            return v;
        } // method: newInterpreter

    }
}
