using System;
using System.Collections.Generic;
using System.Text;
using org.armedbear.lisp;
#if (COGBOT_LIBOMV || USE_STHREADS)
using ThreadPoolUtil;
using Thread = ThreadPoolUtil.Thread;
using ThreadPool = ThreadPoolUtil.ThreadPool;
using Monitor = ThreadPoolUtil.Monitor;
#endif
using System.Threading;
using System.IO;

namespace ABCLScriptEngine.UI
{


    public partial class WinformInputStream : java.io.InputStream
    {
        TextReaderStringBuffer csTextReader;
        public WinformInputStream(TextReaderStringBuffer tIn)
        {
            csTextReader = tIn;

        }



        public override int available()
        {
            return csTextReader.available();
        }
        public override int read()
        {
            return csTextReader.Read();


        }
    }
    public class WinformOutputStream : java.io.OutputStream
    {
        LispEngine csTextWriter;
        int lastChar;
        public WinformOutputStream(LispEngine c)
        {
            this.csTextWriter = c;
        }

        public override void close()
        {
        }
        public override void flush()
        {
            //csTextWriter.shellControl1.Flush();
        }
        public override void write(byte[] b)
        {
            write(b, 0, b.Length);
        }
        public override void write(int i)
        {
            if (i == '\n')
            {
                if (lastChar != '\r')
                {
                    csTextWriter.WriteText("\r");
                }
            }
            csTextWriter.WriteText("" + (char)i);
            lastChar = i;
        }
        public override void write(byte[] b, int off, int len)
        {
            while (len-- > 0)
            {
                write((int)b[off++]);
            }
        }

    }



    public class TextReaderStringBuffer : TextReader
    {
        // Summary:
        //     Initializes a new instance of the System.IO.TextReader class.
        public TextReaderStringBuffer()
        {
        }

        // Summary:
        //     Closes the System.IO.TextReader and releases any system resources associated
        //     with the TextReader.
        public override void Close()
        {
        }
        //
        // Summary:
        //     Releases all resources used by the System.IO.TextReader object.
        // public void Dispose();
        //
        // Summary:
        //     Releases the unmanaged resources used by the System.IO.TextReader and optionally
        //     releases the managed resources.
        //
        // Parameters:
        //   disposing:
        //     true to release both managed and unmanaged resources; false to release only
        //     unmanaged resources.
        // protected override void Dispose(bool disposing)
        // {
        // }
        //
        // Summary:
        //     Reads the next character without changing the state of the reader or the
        //     character source. Returns the next available character without actually reading
        //     it from the input stream.
        //
        // Returns:
        //     The next character to be read, or -1 if no more characters are available
        //     or the stream does not support seeking.
        //
        // Exceptions:
        //   System.IO.IOException:
        //     An I/O error occurs.
        //
        //   System.ObjectDisposedException:
        //     The System.IO.TextReader is closed.
        public override int Peek()
        {
            LockBuffer();
            if (myBuffer != null)
            {
            }
            UnlockBuffer();
            return -1;
        }

        String myBuffer;
        //
        // Summary:
        //     Reads the next character from the input stream and advances the character
        //     position by one character.
        //
        // Returns:
        //     The next character from the input stream, or -1 if no more characters are
        //     available. The default implementation returns -1.
        //
        // Exceptions:
        //   System.IO.IOException:
        //     An I/O error occurs.
        //
        //   System.ObjectDisposedException:
        //     The System.IO.TextReader is closed.
        public override int Read()
        {
            int av = available();
            while (av < 1)
            {
                av = available();
            }
            char c = myBuffer.ToCharArray()[0];
            Consume(1);
            return c;
        }

        public int available()
        {
            return myBuffer.Length;
        }
        //
        // Summary:
        //     Reads a maximum of count characters from the current stream and writes the
        //     data to buffer, beginning at index.
        //
        // Parameters:
        //   count:
        //     The maximum number of characters to read. If the end of the stream is reached
        //     before count of characters is read into buffer, the current method returns.
        //
        //   buffer:
        //     When this method returns, contains the specified character array with the
        //     values between index and (index + count - 1) replaced by the characters read
        //     from the current source.
        //
        //   index:
        //     The place in buffer at which to begin writing.
        //
        // Returns:
        //     The number of characters that have been read. The number will be less than
        //     or equal to count, depending on whether the data is available within the
        //     stream. This method returns zero if called when no more characters are left
        //     to read.
        //
        // Exceptions:
        //   System.IO.IOException:
        //     An I/O error occurs.
        //
        //   System.ArgumentOutOfRangeException:
        //     index or count is negative.
        //
        //   System.ArgumentException:
        //     The buffer length minus index is less than count.
        //
        //   System.ArgumentNullException:
        //     buffer is null.
        //
        //   System.ObjectDisposedException:
        //     The System.IO.TextReader is closed.
        public override int Read(char[] buffer, int index, int count)
        {
            int av = available();
            if (count > av)
            {
                count = av;
            }
            else
            {
                av = count;
            }
            while (count-- > 0)
            {
                buffer[index++] = (char)Read();
            }
            return av;
        }
        //
        // Summary:
        //     Reads a maximum of count characters from the current stream and writes the
        //     data to buffer, beginning at index.
        //
        // Parameters:
        //   count:
        //     The maximum number of characters to read.
        //
        //   buffer:
        //     When this method returns, this parameter contains the specified character
        //     array with the values between index and (index + count -1) replaced by the
        //     characters read from the current source.
        //
        //   index:
        //     The place in buffer at which to begin writing.
        //
        // Returns:
        //     The number of characters that have been read. The number will be less than
        //     or equal to count, depending on whether all input characters have been read.
        //
        // Exceptions:
        //   System.IO.IOException:
        //     An I/O error occurs.
        //
        //   System.ArgumentOutOfRangeException:
        //     index or count is negative.
        //
        //   System.ArgumentException:
        //     The buffer length minus index is less than count.
        //
        //   System.ArgumentNullException:
        //     buffer is null.
        //
        //   System.ObjectDisposedException:
        //     The System.IO.TextReader is closed.
        public override int ReadBlock(char[] buffer, int index, int count)
        {
            int av = available();
            while (count > av)
            {
                av = available();
            }

            if (count > av)
            {
                count = av;
            }
            else
            {
                av = count;
            }
            while (count-- > 0)
            {
                buffer[index++] = (char)Read();
            }
            return av;
        }
        //
        // Summary:
        //     Reads a line of characters from the current stream and returns the data as
        //     a string.
        //
        // Returns:
        //     The next line from the input stream, or null if all characters have been
        //     read.
        //
        // Exceptions:
        //   System.ArgumentOutOfRangeException:
        //     The number of characters in the next line is larger than System.Int32.MaxValue
        //
        //   System.IO.IOException:
        //     An I/O error occurs.
        //
        //   System.OutOfMemoryException:
        //     There is insufficient memory to allocate a buffer for the returned string.
        //
        //   System.ObjectDisposedException:
        //     The System.IO.TextReader is closed.
        public override string ReadLine()
        {
            int pos = myBuffer.IndexOf('\n');
            while (pos == -1)
            {
                pos = myBuffer.IndexOf('\n');
            }
            String ret = myBuffer.Substring(0, pos);
            Consume(pos + 1);
            return ret;
        }

        private void Consume(int p)
        {
            myBuffer = myBuffer.Substring(0);
        }
        //
        // Summary:
        //     Reads all characters from the current position to the end of the TextReader
        //     and returns them as one string.
        //
        // Returns:
        //     A string containing all characters from the current position to the end of
        //     the TextReader.
        //
        // Exceptions:
        //   System.ArgumentOutOfRangeException:
        //     The number of characters in the next line is larger than System.Int32.MaxValue
        //
        //   System.IO.IOException:
        //     An I/O error occurs.
        //
        //   System.OutOfMemoryException:
        //     There is insufficient memory to allocate a buffer for the returned string.
        //
        //   System.ObjectDisposedException:
        //     The System.IO.TextReader is closed.
        public override string ReadToEnd()
        {
            LockBuffer();
            string ret = myBuffer;
            UnlockBuffer();
            return ret;
        }

        private void LockBuffer()
        {
            //throw new Exception("The method or operation is not implemented.");
        }

        private void UnlockBuffer()
        {
            //throw new Exception("The method or operation is not implemented.");
        }
        //
        // Summary:
        //     Creates a thread-safe wrapper around the specified TextReader.
        //
        // Parameters:
        //   reader:
        //     The TextReader to synchronize.
        //
        // Returns:
        //     A thread-safe System.IO.TextReader.
        //
        // Exceptions:
        //   System.ArgumentNullException:
        //     reader is null.

        internal void AddToBuffer(string command)
        {
            myBuffer = myBuffer + command;
        }
    }

    public class LispEngine
    {


        private java.io.OutputStream getOutputStream()
        {
            return new WinformOutputStream(this);
        }

        TextReaderStringBuffer buffer = new TextReaderStringBuffer();
        private java.io.InputStream getInputStream()
        {
            //#using System.IO;
            java.io.InputStream isr = new WinformInputStream(buffer);
            // TextReader rt = TextReader.Synchronized(tr);
            return isr;
        }

        LispObject lastPackage = Lisp.NIL;
        Symbol varPackage = null;

        private Interpreter interpreter = null;//Interpreter.getInstance();
        Thread jlispThread = null;
        void StartJLISP()
        {
            interpreter = Interpreter.createJLispInstance(getInputStream(), getOutputStream(), ".", VersionString);
        }
        public Interpreter getInterpreter()
        {
            if (interpreter == null)
            {
                jlispThread = new Thread(new ThreadStart(StartJLISP));
                jlispThread.Start();
                while (interpreter == null)
                {
                    ironTextBoxControl.Update();
                }
            }
            if (varPackage == null)
            {
                varPackage = Symbol._PACKAGE_;
            }
            LispObject pkg = varPackage.symbolValue();
            if (lastPackage != pkg)
            {
                lastPackage = pkg;
                ironTextBoxControl.Prompt = ((Package)pkg).getName() + "> ";
                //ironTextBoxControl.ConsoleTextBox
            }
            return interpreter;

        }
        // 
        public LispEngine()
        {
        }
        //
        readonly static public String VersionString = "Winform REPL";
        internal void AddToPath(string p)
        {
            throw new Exception("The method or operation is not implemented.");
        }

        internal void Execute(string command)
        {
            LispObject result = Evaluate(command);
            WriteText(result.princToString() + "\r\n");
        }


        internal LispObject Evaluate(string command)
        {
            getInterpreter();
            LispObject cmd = Lisp.readObjectFromString(command);
            LispObject result = Lisp.eval(cmd);
            return result;
        }
        IPEStreamWrapper output;
        internal void SetStandardOutput(IPEStreamWrapper iPEStreamWrapper)
        {
            output = iPEStreamWrapper;
            //throw new Exception("The method or operation is not implemented.");
        }

        internal void ExecuteFile(string p)
        {
            getInterpreter();
            Lisp.eval(new Cons(Symbol.LOAD, new Cons(new SimpleString(p), Lisp.NIL)));
        }

        internal void WriteText(string p)
        {
            char[] chars = p.ToCharArray();
            for (int i = 0; i < chars.Length; i++)
            {
                output.WriteByte((byte)chars[i]);
            }
            if (ironTextBoxControl != null)
            {
                ironTextBoxControl.WriteText(p);
            }
        }

        IronTextBoxControl ironTextBoxControl;

        internal void ExecuteToConsole(string command, IronTextBoxControl ironTextBoxControl)
        {
            if (ironTextBoxControl != null) this.ironTextBoxControl = ironTextBoxControl;

            buffer.AddToBuffer(command);
            getInterpreter();
            return;
        }
    }
}
