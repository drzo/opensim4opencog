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

    }
}
