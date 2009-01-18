using System;
using System.Collections.Generic;
using System.Text;

namespace cogbot.ScriptEngines
{
    public interface ScriptInterpreter
    {
        bool LoadFile(string filename);

        object Read(string context_name, System.IO.StringReader stringCodeReader);

        bool Eof(object codeTree);

        void Intern(string varname, object value);

        object Eval(object code);

        string Str(object code);

        ScriptInterpreter newInterpreter();

        bool DefinedFunction(string eventName);


    }
    //------------------------------------ 
    // OUR NANO LISP JOB QUEUE SYSTEM
    //------------------------------------
    public class subtask
    {
        public Boolean requeue; // should we be re-entered to the queue
        public String code;    // the lisp code as a string
        public String results; // the evaluation results as a string
        public Object codeTree; // the lisp code as an evaluatable object

    }
}
