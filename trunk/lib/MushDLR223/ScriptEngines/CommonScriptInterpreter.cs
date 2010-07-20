using System;
using System.Collections;
using System.Collections.Generic;
using System.IO;
using System.Text;

namespace MushDLR223.ScriptEngines
{
    abstract public class CommonScriptInterpreter : ScriptInterpreter
    {
        public virtual object Self
        {
            get { return GetSymbol("this"); }
            set { Intern("this", value); }
        }

        public readonly object OriginalSelf;
        protected CommonScriptInterpreter(object self)
        {
            OriginalSelf = self;
            // ReSharper disable DoNotCallOverridableMethodsInConstructor
            Init();
            Self = self ?? this;
            // ReSharper restore DoNotCallOverridableMethodsInConstructor
            ScriptManager.AddInterpreter(this);
        }

        private CommonScriptInterpreter()
        {
// ReSharper disable DoNotCallOverridableMethodsInConstructor
            Init();
            Self = this;
// ReSharper restore DoNotCallOverridableMethodsInConstructor
            ScriptManager.AddInterpreter(this);
        }

        public abstract void Init();

        public object ConvertArgToLisp(object code)
        {
            return code;//ScriptManager.argString(code);
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

        public abstract bool LoadsFileType(string filenameorext);

        public virtual bool LoadsFileType0(string filename)
        {
            filename = filename.ToLower();
            string myname = GetType().Name.ToLower();
            if (myname == filename) return true;
            bool b = myname.StartsWith(filename);
            if (b)
            {
                ScriptManager.WriteLine("LoadsFileType0 " + GetType() + " => " + filename);                
            }
            return b;
        }

        public virtual Object EvalForObject(Object lispCode, OutputDelegate output)
        {
            if (lispCode == null) return null;
            TextReader stringCodeReader;
            if (lispCode is String)
            {
                stringCodeReader = new StringReader(lispCode.ToString());
            } else if (lispCode is TextReader)
            {
                stringCodeReader = lispCode as TextReader;
            } else
            {
                stringCodeReader = null;
            }

            if (stringCodeReader!=null) lispCode = Read("" + this, stringCodeReader, output);
            output("Eval> " + lispCode);
            if (Eof(lispCode))
                return lispCode.ToString();
            return Eval(lispCode);
        }

        public abstract object Read(string context_name, System.IO.TextReader stringCodeReader, OutputDelegate WriteLine);
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

        static public void overwrite2Hash(Hashtable hashTable, string key, string value)
        {
            if (hashTable.ContainsKey(key)) hashTable.Remove(key);
            hashTable.Add(key, value);
            //WriteLine("  +Hash :('" + key + "' , " + value + ")");
        }

        static public string getWithDefault(Hashtable hashTable, string key, string defaultValue)
        {
            if (hashTable.ContainsKey(key)) return hashTable[key].ToString();
            return defaultValue;
        }

    }

}
