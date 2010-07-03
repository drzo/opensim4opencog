using System;
using System.Collections.Generic;
using System.IO;
using System.Text;
using cogbot;
using cogbot.ScriptEngines;
using Radegast;
using SbsSW.SwiPlCs;

namespace PrologScriptEngine
{
    public class PrologScriptInterpreter : CommonScriptInterpreter, IRadegastPlugin
    {
        ///<summary>
        ///</summary>
        public PrologClient prologClient;

        public override bool LoadsFileType(string filename)
        {
            return filename.EndsWith("bot") || filename.EndsWith("txt") || filename.EndsWith("note") ||
                   base.LoadsFileType(filename);
        }

        public override void InternType(Type t)
        {
            prologClient.InternType(t);
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

        public PrologScriptInterpreter()
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
                si = new PrologScriptInterpreter();
            si.prologClient = thiz as PrologClient;
            return si;
        } // method: newInterpreter

        #region Implementation of IRadegastPlugin

        /// <summary>
        /// Called in plugin initialization
        /// </summary>
        /// <param name="inst">RadegastInstance plugin is loaded into</param>
        public void StartPlugin(RadegastInstance inst)
        {
            // throw new NotImplementedException();
        }

        /// <summary>
        /// Called on plugin shutdown
        /// </summary>
        /// <param name="inst">RadegastInstance plugin is unloaded from</param>
        public void StopPlugin(RadegastInstance inst)
        {
            //throw new NotImplementedException();
        }

        #endregion
    }

    public class OutputDelegateWriter : TextWriter
    {
        private OutputDelegate output;
        private StringWriter sw = new StringWriter();
        private object locker;

        public OutputDelegateWriter(OutputDelegate od)
        {
            output = od;
            locker = od;
        }

        public override void Write(string format, params object[] arg)
        {
            lock (locker) sw.Write(format, arg);           
        }
        public override void Write(char value)
        {
            lock (locker) sw.Write(value);
        }
        public override void Write(char[] buffer, int index, int count)
        {
            lock (locker) sw.Write(buffer, index, count);
        }
        public override void Close()
        {
            //base.Close();
            Flush();
        }
        public override void WriteLine(char[] buffer, int index, int count)
        {
            lock (locker)
            {
                sw.WriteLine(buffer, index, count);
                Flush();
            }
        }
        public override void WriteLine()
        {
            lock (locker)
            {
                WriteLine();
                Flush();
            }
        }
        public override void Flush()
        {
            lock (locker)
            {
                StringWriter s = sw;
                sw = new StringWriter();
                output(s.ToString().TrimEnd());                
            }
        }

        #region Overrides of TextWriter

        /// <summary>
        /// When overridden in a derived class, returns the <see cref="T:System.Text.Encoding"/> in which the output is written.
        /// </summary>
        /// <returns>
        /// The Encoding in which the output is written.
        /// </returns>
        /// <filterpriority>1</filterpriority>
        public override Encoding Encoding
        {
            get { return sw.Encoding; }
        }

        #endregion
    }
}
