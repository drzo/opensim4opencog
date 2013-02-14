using System;
using System.Collections;
using System.Collections.Generic;
using System.IO;
using System.Text;
using MushDLR223.Utilities;

namespace MushDLR223.ScriptEngines
{
    public interface ScriptedCommand
    {
    }

    abstract public class CommonScriptInterpreter : ScriptInterpreterFactory, ScriptInterpreter
    {

        public readonly Dictionary<object, ScriptInterpreter> interpsForObjects =
            new Dictionary<object, ScriptInterpreter>();

        public ScriptInterpreter FindOrCreate(object key, ScriptInterpreter parent)
        {
            if (parent != null) return parent.newInterpreter(key);
            lock (interpsForObjects)
            {
                ScriptInterpreter mini;
                if (!interpsForObjects.TryGetValue(key, out mini))
                {
                    mini = newInterpreter(key);
                    interpsForObjects[key] = mini;
                }
                return mini;
            }
        }

        public void WriteText(string format, params object[] args)
        {
            DLRConsole.SystemWrite(format, args);
            //       for (int i = 0; i < chars.Length; i++)
            //     {
            //       WriteLine.WriteByte((byte)chars[i]);
            // }
            // if (ironTextBoxControl != null)
            // {
            //   ironTextBoxControl.WriteText(p);
            // }
        }
        public void WriteLine(string format, params object[] args)
        {
            WriteText(format, args);
            WriteText(Environment.NewLine);
            //       for (int i = 0; i < chars.Length; i++)
            //     {
            //       WriteLine.WriteByte((byte)chars[i]);
            // }
            // if (ironTextBoxControl != null)
            // {
            //   ironTextBoxControl.WriteText(p);
            // }
        }


        public object OriginalSelf;

        public virtual object Self
        {
            get { return OriginalSelf ?? GetSymbol("self") ?? OriginalSelf; }
            set
            {
                OriginalSelf = value;
                Intern("self", value);
            }
        }

        public abstract void Init(object self);

        public virtual object ConvertArgToLisp(object code)
        {
            return code;//ScriptManager.argString(code);
        }

        public virtual object ConvertArgFromLisp(object code)
        {
            return code;
        }

        #region ScriptInterpreter Members

        /// <summary>
        /// 
        /// 
        /// </summary>
        /// <param name="filename"></param>
        /// <returns></returns>
        public virtual bool LoadFile(string filename, OutputDelegate WriteLine)
        {
            if (!File.Exists(filename)) return false;
            System.IO.FileStream f = System.IO.File.OpenRead(filename);
            StreamReader r = new StreamReader(f);
            r.BaseStream.Seek(0, SeekOrigin.Begin);
            return Read(filename, new StringReader(r.ReadToEnd()), WriteLine) != null;
        }

        public virtual ScriptInterpreter GetLoaderOfFiletype(string filenameorext)
        {
            return LoadsFileType(filenameorext) ? this : null;
        }

        public virtual bool LoadsFileType(string filename)
        {
            filename = filename.ToLower();
            string myname = GetType().Name.ToLower();
            if (myname == filename) return true;
            bool b = myname.StartsWith(filename);
            if (b)
            {
                ScriptManager.WriteLine("LoadsFileType " + GetType() + " => " + filename);
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
            }
            else if (lispCode is TextReader)
            {
                stringCodeReader = lispCode as TextReader;
            }
            else
            {
                stringCodeReader = null;
            }

            if (stringCodeReader != null) lispCode = Read("" + this, stringCodeReader, output);
            output("Eval> " + lispCode);
            if (Eof(lispCode))
                return lispCode.ToString();
            return Eval(lispCode);
        }

        /// <summary>
        /// 
        /// </summary>
        /// <param name="context_name"></param>
        /// <param name="stringCodeReader"></param>
        /// <returns></returns>
        public virtual object Read(string context_name, System.IO.TextReader stringCodeReader, OutputDelegate WriteLine)
        {
            object res = null;
            int line = 0;
            while (stringCodeReader.Peek() != -1)
            {
                line++;
                res = Eval(stringCodeReader.ReadLine());
            }
            return res;
        } // method: Read


        /// <summary>
        /// 
        /// </summary>
        /// <param name="codeTree"></param>
        /// <returns></returns>
        public virtual bool Eof(object codeTree)
        {
            if (codeTree == null) return true;
            String str = codeTree.ToString().Trim();
            return String.IsNullOrEmpty(str);
        } // method: Eof

        public abstract void Intern(string varname, object value);
        public abstract object Eval(object code);
        public abstract string Str(object code);
        public abstract ScriptInterpreter newInterpreter(object self);
        public virtual bool IsSubscriberOf(string eventName)
        {
            return GetSymbol(eventName) != null;
        }
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


        #region ScriptInterpreter Members

        virtual public object Impl { get { return this; } }
        public virtual bool IsSelf(object self)
        {
            if (self==null) return OriginalSelf == null;
            return OriginalSelf == self;
        }

        #endregion
    }

}
