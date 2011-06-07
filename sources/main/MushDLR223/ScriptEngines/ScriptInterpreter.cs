using System;
using System.Threading;

namespace MushDLR223.ScriptEngines
{
    public delegate void OutputDelegate(string s, params object[] args);

    public interface ScriptInterpreter : IDisposable
    {
        bool LoadFile(string filename, OutputDelegate WriteLine);

        bool LoadsFileType(string filenameorext, object self);

        object Read(string context_name, System.IO.TextReader stringCodeReader, OutputDelegate WriteLine);

        bool Eof(object codeTree);

        void Intern(string varname, object value);

        object Eval(object code);

        object ConvertArgToLisp(object code);

        object ConvertArgFromLisp(object code);

        string Str(object code);

        ScriptInterpreter newInterpreter(object self);

        bool IsSubscriberOf(string eventName);

        object GetSymbol(string eventName);
       
        void InternType(Type t);

        object Self { get; set; }
    }

    public class subtask
    {
        public Boolean requeue; // should we be re-entered to the queue
        public String code;    // the lisp code as a string
        public String results; // the evaluation results as a string
        public Object codeTree; // the lisp code as an evaluatable object

    }

    public class CmdResult : IAsyncResult
    {
        public String Message;
        public bool Success;
        public bool InvalidArgs;

        public CmdResult(string usage, bool b)
        {
            Message = usage;
            Success = b;
            IsCompleted = true;
            CompletedSynchronously = true;
            InvalidArgs = false;
        }
        public override string ToString()
        {
            if (!Success) return string.Format("ERROR: {0}", Message);
            return Message;
        }

        public bool IsCompleted { get; set; }

        /// <summary>
        /// Gets a System.Threading.WaitHandle that is used to wait for an asynchronous operation to complete.
        /// </summary>
        /// A System.Threading.WaitHandle that is used to wait for an asynchronous operation to complete.
        public WaitHandle AsyncWaitHandle
        {
            get { throw new NotImplementedException(); }
        }

        /// <summary>
        /// Gets a user-defined object that qualifies or contains information about an asynchronous operation.
        /// </summary>
        /// Returns: A user-defined object that qualifies or contains information about an asynchronous operation.
        public object AsyncState
        {
            get
            {
                return this;/* throw new NotImplementedException();*/
            }
        }

        /// <summary>
        /// true if the asynchronous operation completed synchronously; otherwise, false.
        /// </summary>
        public bool CompletedSynchronously { get; set; }
    }
}