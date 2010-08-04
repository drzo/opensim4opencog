using System;
using System.Collections.Generic;
using System.IO;
using System.Reflection;
using cogbot;
using cogbot.ScriptEngines;
using MushDLR223.ScriptEngines;
using MushDLR223.Utilities;
using Radegast;
using SbsSW.SwiPlCs;

namespace PrologScriptEngine
{
    public class PrologScriptInterpreter : CommonScriptInterpreter
    {
        public void Main(string[] args)
        {
            _reachAllTypes = true;
            PrologClient.Main(args);
        }
        ///<summary>
        ///</summary>
        public PrologClient prologClient;

        public override bool LoadsFileType(string filename)
        {
            return filename.EndsWith("pl") || filename.EndsWith("swi") || filename.EndsWith("prolog") ||
                   base.LoadsFileType0(filename);
        }
        static HashSet<Type> _types = new HashSet<Type>();
        private static bool _reachAllTypes = false;
        ///<summary>
        ///</summary>
        ///<param name="type"></param>
        ///<exception cref="NotImplementedException"></exception>
        public override void InternType(Type type)
        {
            if (!_reachAllTypes) return;
            InternTypeS(type);
        }

        static void InternTypeS(Type type)
        {
            lock (_types)
            {
                if (!_types.Add(type))
                {
                    string module = type.Namespace;
                    foreach (var list in type.GetMethods(BindingFlags.Static | BindingFlags.NonPublic))
                    {
                        PrologClient.InternMethod(module, null, list);
                    }
                }
            }
        }
        public override void Dispose()
        {
            prologClient.Dispose();
        }

        public override object GetSymbol(string eventName)
        {
            return prologClient.GetSymbol(eventName);
        }

        public override bool IsSubscriberOf(string eventName)
        {
            return prologClient.IsDefined(eventName);
        }

        public PrologScriptInterpreter(object self):base(self)
        {

        }
        public override void Init()
        {
            prologClient = new PrologClient();
        }
        /// <summary>
        /// 
        /// 
        /// </summary>
        /// <param name="filename"></param>
        /// <returns></returns>
        public override bool LoadFile(string filename, OutputDelegate WriteLine)
        {
            if (!File.Exists(filename)) return false;
            System.IO.FileStream f = System.IO.File.OpenRead(filename);
            StreamReader r = new StreamReader(f);
            r.BaseStream.Seek(0, SeekOrigin.Begin);
            return Read(filename, new StringReader(r.ReadToEnd()),WriteLine) != null;
        }

        /// <summary>
        /// 
        /// </summary>
        /// <param name="context_name"></param>
        /// <param name="stringCodeReader"></param>
        /// <returns></returns>
        public override object Read(string context_name, System.IO.TextReader stringCodeReader, OutputDelegate WriteLine)
        {
            object res = null;
            int line = 0;
            while (stringCodeReader.Peek() != -1)
            {
                line++;
                TextWriter tw = new OutputDelegateWriter(WriteLine);
                res = prologClient.Read(stringCodeReader.ReadLine(), tw);
            }
            return res;
        } // method: Read


        /// <summary>
        /// 
        /// </summary>
        /// <param name="codeTree"></param>
        /// <returns></returns>
        public override bool Eof(object codeTree)
        {
            if (codeTree == null) return true;
            String str = codeTree.ToString().Trim();
            return String.IsNullOrEmpty((String)codeTree);
        } // method: Eof


        /// <summary>
        /// 
        /// </summary>
        /// <param name="varname"></param>
        /// <param name="textForm"></param>
        public override void Intern(string varname, object value)
        {
            prologClient.Intern(varname, value);
        } // method: Intern


        /// <summary>
        /// 
        /// </summary>
        /// <param name="code"></param>
        /// <returns></returns>
        public override object Eval(object code)
        {
            return prologClient.Eval(code);
        } // method: Eval


        /// <summary>
        /// 
        /// </summary>
        /// <param name="code"></param>
        /// <returns></returns>
        public override string Str(object code)
        {
            return ScriptEventListener.argString(code);
        } // method: Str


        /// <summary>
        /// 
        /// </summary>
        /// <returns></returns>
        public override ScriptInterpreter newInterpreter(object thiz)
        {
            PrologScriptInterpreter si;
            if (prologClient == null || prologClient == thiz) si = this;
            else
                si = new PrologScriptInterpreter(thiz);
            si.prologClient = thiz as PrologClient;
            return si;
        } // method: newInterpreter
    }
}
