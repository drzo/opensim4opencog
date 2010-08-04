using System.IO;
using System.Text;
using MushDLR223.ScriptEngines;

namespace MushDLR223.Utilities
{
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
        public override void WriteLine(string format, params object[] arg)
        {
            lock (locker)
            {
                Flush();
                output(format, arg);
            }
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
                //sw.WriteLine();
                Flush();
            }
        }
        public override void Flush()
        {
            lock (locker)
            {
                StringWriter s = sw;
                sw = new StringWriter();
                s.Flush();
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