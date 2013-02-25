using System;
using System.IO;
using System.Text;
using MushDLR223.ScriptEngines;

namespace MushDLR223.Utilities
{
    internal static class UtilMethods
    {
        public static int ContainsAny(this string graphPath, params string[] ces)
        {
            bool gpnull = string.IsNullOrEmpty(graphPath);
            string ew = gpnull ? "" : graphPath.ToLower();
            for (int index = 0; index < ces.Length; index++)
            {
                var ce = ces[index];
                if (string.IsNullOrEmpty(ce))
                {
                    if (gpnull) return index;
                    continue;
                }
                if (!gpnull && ew.Contains(ce.ToLower()))
                {
                    return index;
                }
            }
            if (gpnull) return -2;
            return -1;
        }
    }

    public class OutputDelegateWriter : TextWriter
    {
        private readonly OutputDelegate output;
        private StringWriter sw = new StringWriter();
        private object locker;

        public OutputDelegateWriter(OutputDelegate od)
        {
            output = od;
            locker = od;
        }

        public override int GetHashCode()
        {
            return output.GetHashCode();
        }

        public override bool Equals(object obj)
        {
            if (base.Equals(obj)) return true;
            OutputDelegateWriter odw = obj as OutputDelegateWriter;
            if (odw == null) return false;
            return odw.output.Equals(output);
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
            DLRConsole.InitialConsoleOut.Flush();
            lock (locker) sw.Write(buffer, index, count);
            Flush();
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
                sw.WriteLine();
                Flush();
            }
        }

        public override void Flush()
        {
            Flush0();
            DLRConsole.InitialConsoleOut.Flush();
        }

        public void Flush0()
        {
            string toWrite = "";
            lock (locker)
            {
                toWrite = sw.ToString();
                int lastlf = toWrite.LastIndexOf('\n');
                if (lastlf == -1)
                {
                    if (!DLRConsole.ContainsSubscribedBreakpointWords(toWrite))
                        return;
                }
                sw = new StringWriter();
                string nextWrite = toWrite.Substring(lastlf).TrimStart();
                sw.Write(nextWrite);
                var thisWrite = toWrite.Substring(0, lastlf).TrimEnd();
                toWrite = thisWrite;
            }
            try
            {
                bool debug = DLRConsole.ContainsSubscribedBreakpointWords(toWrite);
                output(toWrite);
            }
            catch (Exception e)
            {
                DLRConsole.SYSTEM_ERR_WRITELINE("" + e);
                try
                {
                    output(toWrite);
                }
                catch (Exception)
                {
                    DLRConsole.SYSTEM_ERR_WRITELINE("" + toWrite);
                }
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

        public static OutputDelegate OnlyWith(OutputDelegate writeLine, Predicate<string> msgTest)
        {
            return (f, a) =>
                       {
                           string msg = DLRConsole.SafeFormat(f, a);
                           if (!msgTest(msg)) return;
                           writeLine(f, a);
                       };
        }
    }
}