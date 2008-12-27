using System;
using System.Collections.Generic;
using System.Text;
using System.Threading;
using System.IO;
using System.Reflection;
using org.armedbear.lisp;

namespace cogbot.ScriptEngines
{


    public class ABCLInterpreter : ScriptInterpreter
    {


        private java.io.OutputStream getOutputStream()
        {
            return new WinformOutputStream(this);
        }

        static private TextReaderStringBuffer buffer;
        private java.io.InputStream getInputStream()
        {
            //#using System.IO;
            java.io.InputStream isr = new WinformInputStream(buffer);
            // TextReader rt = TextReader.Synchronized(tr);
            return isr;
        }

        LispObject lastPackage = Symbol.NIL;
        Symbol varPackage = null;

        private Interpreter interpreter = Interpreter.getInstance();
        Thread jlispThread = null;
        java.lang.Thread jthread = null;
        static bool isReady = false;
        static java.lang.Runnable jrunnable;


        // 
        public ABCLInterpreter()
        {
            buffer = new TextReaderStringBuffer(this);
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
            WriteText(result.writeToString() + "\r\n");
        }


        internal LispObject Evaluate(string command)
        {
            getInterpreter();
            LispObject cmd = Lisp.readObjectFromString(command);
            LispObject result = Lisp.eval(cmd);
            return result;
        }

        internal void ExecuteFile(string p)
        {
            getInterpreter();
            Lisp.eval(new Cons(Symbol.LOAD, new Cons(new SimpleString(p), Symbol.NIL)));
        }

        internal void WriteText(string p)
        {
            Console.Write(p);
     //       for (int i = 0; i < chars.Length; i++)
       //     {
         //       output.WriteByte((byte)chars[i]);
           // }
           // if (ironTextBoxControl != null)
           // {
             //   ironTextBoxControl.WriteText(p);
           // }
        }

        internal void ExecuteToConsole(string command)
        {
            getInterpreter();
            if (buffer.IsNeedy())
            {
                buffer.AddToBuffer(command + "\n");
                buffer.needsChars = false;
            }
            else
            {
                buffer.SetBuffer("");
                econsole = command;
                ///new Thread(new ThreadStart(EConsole)).Start();
                LispObject lo = Evaluate(econsole);
                WriteText("==> " + lo.writeToString() + "\r\n");
            }
            getInterpreter();
            return;
        }
        string econsole;
        internal void EConsole()
        {
            LispObject lo = Evaluate(econsole);
            WriteText("==> " + lo.writeToString() + "\r\n");
        }
        internal void Yield()
        {
            System.Windows.Forms.Application.DoEvents();
        }

        bool ScriptInterpreter.LoadFile(string p)
        {
            if (!p.StartsWith("cl-"))
            {
                p = "cl-" + p;
            }
            if (!p.EndsWith(".lisp"))
            {
                p = p + ".lisp";
            }
            FileInfo fi = new FileInfo(p);
            if (fi.Exists)
            {
                ExecuteFile(p);
                return true;
            }
            return false;
        }

        object ScriptInterpreter.Read(string p, StringReader stringCodeReader)
        {
            try
            {
                getInterpreter();
                return Lisp.readObjectFromString(stringCodeReader.ReadToEnd());
            }
            catch (Exception e)
            {
                Console.WriteLine(e.ToString());
                return Lisp.EOF;
            }
        }

        bool ScriptInterpreter.Eof(object codeTree)
        {
            return codeTree == Lisp.EOF;
        }

        static public List<object> allExceptFor = new List<object>();

        void ScriptInterpreter.Intern(string p, object globalcogbotTextForm)
        {
            Intern(p, globalcogbotTextForm,allExceptFor);
        }
        public Symbol Intern(string p, object globalcogbotTextForm, List<object> exceptFor)
        {
            Package pkg = CurrentPackage();
            p = p.ToUpper();
            Symbol s = Lisp.intern(p, pkg);
            LispObject sv = s.getSymbolValue();
            if (sv is JavaObject)
            {
                if (sv.javaInstance() == globalcogbotTextForm) return s;
            }
            JavaObject jo = new JavaObject(globalcogbotTextForm);
            s.setSymbolValue(jo);

            if (exceptFor.Contains(globalcogbotTextForm)) return s;
            exceptFor.Add(globalcogbotTextForm);
            Type t = globalcogbotTextForm.GetType();
            String ns = t.Namespace;
            String ts = t.Name;
            if (ns.StartsWith("System")) return s;
            AddMembers(p, s, t, exceptFor,1);
            return s;
        }

        private void AddMembers(String p, Symbol s, Type t, List<object> exceptFor, int maxDepth)
        {
            Package pkg = CurrentPackage();
            String ns = t.Namespace;
            String ts = t.Name;
            if (ns.StartsWith("System")) return;
            if (ns.StartsWith("cli.System")) return;
            if (ns.StartsWith("java.lang.Object")) return;
            Console.WriteLine("; importing " + p + " as " + ns + "." + ts);
            java.lang.Class clazz = ikvm.runtime.Util.getFriendlyClassFromType(t);

            java.lang.reflect.Field[] fi = clazz.getFields();
            for (int i = 0; i < fi.Length; i++)
            {
                java.lang.reflect.Field f = fi[i];
                String fname = ("" + p + "." + f.getName()).ToUpper();
                Symbol old = pkg.findAccessibleSymbol(fname);
                if (old != null)
                {
                    fname = ("" + p + "%" + f.getName()).ToUpper() + "";
                    old = pkg.findAccessibleSymbol(fname);
                } 
                if (old != null)
                {
                    Console.WriteLine(";;; skip field " + fname + " for " + f);
                    continue;
                }
                Console.WriteLine(";;; field " + fname + " for " + f);
                Symbol sfm = IkvmSite.fieldToInstanceSymbol(fname, pkg, s, f);
                if (maxDepth > 0)
                    {
                        exceptFor.Add(f);
                        AddMembers(fname, sfm, ikvm.runtime.Util.getInstanceTypeFromClass(f.getType()), exceptFor, maxDepth - 1);
                    }
                    else
                    {
                        if (false && !exceptFor.Contains(f)) {
                            exceptFor.Add(f);
                            AddMembers(fname, sfm, ikvm.runtime.Util.getInstanceTypeFromClass(f.getType()), exceptFor, maxDepth - 1);
                        }
                    }
            }
            java.lang.reflect.Method[] mi = clazz.getDeclaredMethods();
            for (int i = 0; i < mi.Length; i++)
            {
                java.lang.reflect.Method m = mi[i];
                String fname = ("" + p + "." + m.getName()).ToUpper() + "";
                Symbol old = pkg.findAccessibleSymbol(fname);
                if (old != null)
                {
                    fname = ("" + p + "/" + m.getName()).ToUpper() + "";
                    old = pkg.findAccessibleSymbol(fname);
                }
                if (old != null)
                {
                    fname = ("" + p + "//" + m.getName()).ToUpper() + "";
                    old = pkg.findAccessibleSymbol(fname);
                }
                if (old != null)
                {
                    Console.WriteLine(";;; skip method " + fname + " for " + m);
                    continue;
                }
                Console.WriteLine(";;; method " + p + " as " + fname + " to " + m);
                LispObject sfm = IkvmSite.methodToInstanceSymbol(fname, pkg, s, m);
            }
        }


        private Package CurrentPackage()
        {
            lastPackage = Package.getCurrentPackage();
            return (Package)lastPackage;
        }

        object ScriptInterpreter.Eval(object p)
        {
            getInterpreter();
            Console.WriteLine("ABCL EVAL: " + ToStr(p));
            //if (p is LispObject) return Lisp.eval((LispObject)p);
            return p;
        }

        private string ToStr(object x)
        {
            if (x is LispObject)
            {
                return ((LispObject)x).writeToString();
            }
            return x.ToString();
        }

        ScriptInterpreter ScriptInterpreter.newInterpreter()
        {
            getInterpreter();
            return this; //throw new Exception("The method or operation is not implemented.");
        }

        string ScriptInterpreter.Str(object x)
        {
            return ToStr(x);
        }

        public class JRunnable : java.lang.Runnable
        {
            ABCLInterpreter engine;
            public JRunnable(ABCLInterpreter lengine)
            {
                engine = lengine;
            }
            public void run()
            {
                engine.StartJLISP();
            }
        }

        java.io.InputStream inStream;
        java.io.OutputStream outStream;

        void StartJLISP()
        {
            StartInstanceBlocked();
            isReady = true;
            // interpreter.initializeJLisp();
            interpreter.run();
        }

        private void StartInstanceBlocked()
        {
            if (interpreter == null)
            {
                /// outStream = getOutputStream();
                //  inStream = getInputStream();
                //                interpreter = Interpreter.createJLispInstance(inStream, outStream, ".", VersionString);
                interpreter = Interpreter.createDefaultInstance(new String[0]);//inStream, outStream, ".", VersionString);
            }
        }

        public Interpreter getInterpreter()
        {
            if (interpreter == null)
            {
                if (false)
                {
                    jlispThread = new Thread(new ThreadStart(StartJLISP));
                    jlispThread.Start();
                }
                else
                {
                    // StartInstanceBlocked();
                    // isReady = true;

                    jrunnable = new JRunnable(this);
                    jthread = new java.lang.Thread(jrunnable);
                    jthread.start();
                }
                while (!isReady)
                {
                    java.lang.Thread.sleep(1000);// Thread.Sleep(1000);// ironTextBoxControl.Update();
                    Yield();
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
                //            ironTextBoxControl.Prompt = ((Package)pkg).getName() + "> ";
                //          ironTextBoxControl.getIronTextBox();

                // ironTextBoxControl.getIronTextBox().Parent;
                //ironTextBoxControl.ConsoleTextBox
            }
            return interpreter;

        }
        
       }

    public class WinformInputStream : java.io.InputStream
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
        ABCLInterpreter csTextWriter;
        int lastChar;
        public WinformOutputStream(ABCLInterpreter c)
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
        ABCLInterpreter lispEngine;
        // Summary:
        //     Initializes a new instance of the System.IO.TextReader class.
        public TextReaderStringBuffer(ABCLInterpreter le)
        {
            lispEngine = le;
        }

        // Summary:
        //     Closes the System.IO.TextReader and releases any system resources associated
        //     with the TextReader.
        public override void Close()
        {
            needsChars = false;
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
        public virtual int Peek()
        {
            int av = myBuffer.Length;
            if (av < 1) return -1;
            return myBuffer.ToCharArray()[0];
        }

        System.String myBuffer = "";
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
        public virtual int Read()
        {
            needsChars = true;
            int av = myBuffer.Length;
            while (av < 1)
            {
                lispEngine.Yield();
                av = myBuffer.Length;
                //   Monitor.Wait(lispEngine);
            }
            char c = myBuffer.ToCharArray()[0];
            Consume(1);
            needsChars = false;
            return c;
        }

        public int BufferLength()
        {
            return myBuffer.Length;
        }

        public int available()
        {
            int av = myBuffer.Length;
            if (av < 1) return 1;
            return av;
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
        public virtual int Read(char[] buffer, int index, int count)
        {
            needsChars = true;
            int av = myBuffer.Length;
            if (av == 0) return 0;
            needsChars = false;
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
            needsChars = false;
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
        public virtual int ReadBlock(char[] buffer, int index, int count)
        {
            needsChars = true;
            int av = myBuffer.Length;
            while (av < count)
            {
                lispEngine.Yield();
                av = myBuffer.Length;
            }
            needsChars = false;

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
        public virtual string ReadLine()
        {
            needsChars = true;
            int pos = myBuffer.IndexOf('\n');
            while (pos == -1)
            {
                pos = myBuffer.IndexOf('\n');
            }
            String ret = myBuffer.Substring(0, pos);
            Consume(pos + 1);
            needsChars = false;
            return ret;
        }

        private void Consume(int p)
        {
            myBuffer = myBuffer.Substring(p);
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
        internal void SetBuffer(string command)
        {
            myBuffer = command;
        }
        public bool needsChars = false;
        internal bool IsNeedy()
        {
            return needsChars;
        }
    }

}
