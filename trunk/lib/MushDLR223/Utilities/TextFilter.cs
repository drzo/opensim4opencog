using System;
using System.Collections;
using System.Collections.Generic;
using System.IO;
using System.Reflection;
using MushDLR223.ScriptEngines;

namespace MushDLR223.Utilities
{
    public class TextFilter : ICollection<string>
    {
        private HashSet<string> AnyOf = new HashSet<string>() {"ERROR", "LOADER"};
        private HashSet<string> ExceptFor = new HashSet<string>() { };
        bool addMode = true;
        bool remMode = false;
        public TextFilter()
        {

        }
        private string lastOutput = "xoxoxoxoxoxoxoxoxoxoxoxoxdo";
        public bool writeDebugLine(OutputDelegate console, string message, params object[] args)
        {
            console = console ?? DLRConsole.SystemWriteLine;
            try
            {
                bool printIt;
                if (args != null && args.Length != 0)
                {
                    try
                    {
                        message = String.Format(message, args);
                    }
                    catch (Exception exception)
                    {
                        writeDebugLine(console, "BAD FORMAT " + exception.Message + " " + exception.StackTrace);
                        throw exception;
                    }
                }
               // lock (this)
                {
                    printIt = message.StartsWith("-");
                    if (printIt)
                    {
                        message = message.Substring(1);
                    }
                    else
                    {
                        printIt = ShouldPrint(message);
                    }

                }
                if (printIt)
                {                   
                    PrintMessage(console, message);
                }
                return printIt;
            }
            catch (Exception e)
            {
                DLRConsole.DebugWriteLine("" + e);
                console(message + " --> " + e);
                return false;
            }
            return true;
        }

        private void PrintMessage(OutputDelegate console, string message)
        {
            bool doHeader = !message.Contains("<!--") && message.Contains("!");

            if (doHeader)
                RealyPrintMessage(console, "---------------------------------------------------------------");
            message = message.Replace("\r\n", "<br/>");
            message = message.Replace("\n", "<br/>");
            message = message.Replace("(From '", "<br/>     (From '");            
            message = message.Replace("<br/>", "  " + Environment.NewLine);
            RealyPrintMessage(console, message);
            if (doHeader)
                RealyPrintMessage(console, "----------------------------------------------------------------");
        }

        private void RealyPrintMessage(OutputDelegate console, string message)
        {
            if (message.Contains("-----------------"))
            {
                if (lastOutput == message) return;
            }
            if (lastOutput == message) return;
            if (message.Length > 10 && lastOutput.Contains(message)) return;
            if (lastOutput.Length > 10 && message.Contains(lastOutput)) return;
            console("{0}", ClipString(message, Clip));
            lastOutput = message;
        }

        public int Clip { get; set; }

        public bool ShouldPrint(string message)
        {
            if (message == null) return false;
            string msgTest = message.ToUpper();
            bool printIt = false;
            lock (AnyOf)
                foreach (string s in AnyOf)
                {
                    if (s == "*") printIt = true;
                    else if (msgTest.Contains(s))
                    {
                        printIt = true;
                        break;
                    }
                }
            if (printIt)
            {
                lock (ExceptFor)
                    foreach (string s in ExceptFor)
                    {
                        if (s == "*")
                        {
                            printIt = false;
                            break;
                        }
                        if (msgTest.Contains(s))
                        {
                            printIt = false;
                            break;
                        }
                    }
            }
            return printIt;
        }

        public void UpateLogging(string sa, OutputDelegate od)
        {
            od = CheckOutput(od);
            sa = sa.Trim().ToLower();
            while (sa != null && sa.Trim().Length > 0)
            {
                sa = sa.Trim();
                if (sa.StartsWith("clear"))
                {
                    ExceptFor.Clear();
                    AnyOf.Clear();
                    od("Clearing all");
                    sa = sa.Substring(5);
                    continue;
                }
                if (sa.StartsWith("reset"))
                {
                    sa = sa.Substring(5);
                    if (addMode)
                    {
                        od("Clearing AnyOf");
                        AnyOf.Clear();
                    }
                    if (remMode)
                    {
                        od("Clearing ExceptFor");
                        ExceptFor.Clear();
                    }
                    continue;
                }
                if (sa.StartsWith("+"))
                {
                    addMode = true;
                    remMode = false;
                    sa = sa.Substring(1);
                    continue;
                }
                if (sa.StartsWith("-"))
                {
                    remMode = true;
                    addMode = false;
                    sa = sa.Substring(1);
                    continue;
                }
                int f = (sa + "\n").IndexOfAny(new char[] {'+', '-', '\n'});
                string w = sa.Substring(0, f).ToUpper().Trim();
                if (addMode)
                {
                    AnyOf.Add(w);
                    ExceptFor.Remove(w);
                }
                if (remMode)
                {
                    ExceptFor.Add(w);
                    AnyOf.Remove(w);
                }
                sa = sa.Substring(f);
            }
            od(ToString());
        }
        public override string ToString()
        {
            StringWriter sw = new StringWriter();
            OutputDelegate od = sw.WriteLine;
            lock (AnyOf) foreach (var of in AnyOf)
                {
                    od("+" + of);
                }
            lock (ExceptFor) foreach (var of in ExceptFor)
                {
                    od("-" + of);
                }
            return sw.ToString();
        }

        public static OutputDelegate CheckOutput(OutputDelegate od)
        {
            if (od != null) return od;
            TextWriter tw = DLRConsole.Out ?? Console.Out ?? Console.Error;
            if (tw != null)
            {
                return tw.WriteLine;
            }
            return DLRConsole.SystemWriteLine;
            od = (new StringWriter()).WriteLine;
            return od;
        }

        public static bool ListEdit(ICollection<string> collection, string ss, OutputDelegate console)
        {
            console = CheckOutput(console);
            if (ss == null || null == collection) return false;
            lock (collection)
            {
                if (ss.StartsWith("?"))
                {
                    ss = ss.Substring(1);
                    bool contains = collection.Contains(ss);
                    if (contains)
                    {
                        console("Found {0} in col={1}", ss, collection.Count);
                    }
                    else
                    {
                        console("Abscent {0} in col={1}", ss, collection.Count);
                    }
                    return contains;
                }
                if (ss.StartsWith("+"))
                {
                    ss = ss.Substring(1);
                    bool contains = collection.Contains(ss);
                    if (!contains)
                    {
                        collection.Add(ss);
                        console("Added {0} to col={1}", ss, collection.Count);
                    }
                    else
                    {
                        console("Found {0} in col={1}", ss, collection.Count);
                    }
                    return contains;
                }
                if (ss.StartsWith("-"))
                {
                    ss = ss.Substring(1);
                    bool changed = collection.Remove(ss);
                    if (changed)
                    {
                        console("Removed {0} from {1}", ss, collection.Count);
                    }
                    else
                    {
                        console("Did not find {0} in {1}", ss, collection.Count);
                    }
                    return changed;
                }
                ss = ss.ToLower();
                if (ss == "clear")
                {
                    bool contains = collection.Count != 0;
                    collection.Clear();
                    return contains && collection.Count == 0;
                }
                if (ss == "list" || ss == "")
                {
                    DLRConsole.SystemWriteLine("{0} Count={1}", collection, collection.Count);
                    foreach (var s in collection)
                    {
                        console(String.Format(" {0}", s));
                    }
                    return collection.Count != 0;
                }
            }
            return false;
        }

        #region Implementation of IEnumerable

        /// <summary>
        /// Returns an enumerator that iterates through the collection.
        /// </summary>
        /// <returns>
        /// A <see cref="T:System.Collections.Generic.IEnumerator`1"/> that can be used to iterate through the collection.
        /// </returns>
        /// <filterpriority>1</filterpriority>
        public IEnumerator<string> GetEnumerator()
        {
            return AnyOf.GetEnumerator();
        }

        /// <summary>
        /// Returns an enumerator that iterates through a collection.
        /// </summary>
        /// <returns>
        /// An <see cref="T:System.Collections.IEnumerator"/> object that can be used to iterate through the collection.
        /// </returns>
        /// <filterpriority>2</filterpriority>
        IEnumerator IEnumerable.GetEnumerator()
        {
            return GetEnumerator();
        }

        #endregion

        #region Implementation of ICollection<string>

        /// <summary>
        /// Adds an item to the <see cref="T:System.Collections.Generic.ICollection`1"/>.
        /// </summary>
        /// <param name="item">The object to add to the <see cref="T:System.Collections.Generic.ICollection`1"/>.
        ///                 </param><exception cref="T:System.NotSupportedException">The <see cref="T:System.Collections.Generic.ICollection`1"/> is read-only.
        ///                 </exception>
        public void Add(string item)
        {
            lock (this)
            {
                UpateLogging(item, DEVNULL);
            }
        }

        static public void DEVNULL(string s, object[] args)
        {
        }

        /// <summary>
        /// Removes all items from the <see cref="T:System.Collections.Generic.ICollection`1"/>.
        /// </summary>
        /// <exception cref="T:System.NotSupportedException">The <see cref="T:System.Collections.Generic.ICollection`1"/> is read-only. 
        ///                 </exception>
        public void Clear()
        {
            lock (this)
            {
                lock (ExceptFor) ExceptFor.Clear();
                lock (AnyOf) AnyOf.Clear();
                remMode = false;
                addMode = true;
            }
        }

        /// <summary>
        /// Determines whether the <see cref="T:System.Collections.Generic.ICollection`1"/> contains a specific value.
        /// </summary>
        /// <returns>
        /// true if <paramref name="item"/> is found in the <see cref="T:System.Collections.Generic.ICollection`1"/>; otherwise, false.
        /// </returns>
        /// <param name="item">The object to locate in the <see cref="T:System.Collections.Generic.ICollection`1"/>.
        ///                 </param>
        public bool Contains(string item)
        {
            return CanBe(item) && !CannotBe(item);
        }

        private bool CannotBe(string item)
        {
            return FoundIn(item, ExceptFor);
        }

        private bool FoundIn(string item, ICollection<string> collection)
        {
            lock (this)
            {
                lock (collection)
                    foreach (string s in collection)
                    {
                        if ((s == "*") || item.Contains(s))
                        {
                            return true;
                        }
                    }
            }
            return false;
        }


        private bool CanBe(string item)
        {
            return FoundIn(item, AnyOf);
        }

        /// <summary>
        /// Copies the elements of the <see cref="T:System.Collections.Generic.ICollection`1"/> to an <see cref="T:System.Array"/>, starting at a particular <see cref="T:System.Array"/> index.
        /// </summary>
        /// <param name="array">The one-dimensional <see cref="T:System.Array"/> that is the destination of the elements copied from <see cref="T:System.Collections.Generic.ICollection`1"/>. The <see cref="T:System.Array"/> must have zero-based indexing.
        ///                 </param><param name="arrayIndex">The zero-based index in <paramref name="array"/> at which copying begins.
        ///                 </param><exception cref="T:System.ArgumentNullException"><paramref name="array"/> is null.
        ///                 </exception><exception cref="T:System.ArgumentOutOfRangeException"><paramref name="arrayIndex"/> is less than 0.
        ///                 </exception><exception cref="T:System.ArgumentException"><paramref name="array"/> is multidimensional.
        ///                     -or-
        ///                 <paramref name="arrayIndex"/> is equal to or greater than the length of <paramref name="array"/>.
        ///                     -or-
        ///                     The number of elements in the source <see cref="T:System.Collections.Generic.ICollection`1"/> is greater than the available space from <paramref name="arrayIndex"/> to the end of the destination <paramref name="array"/>.
        ///                     -or-
        ///                     Type <paramref name="T"/> cannot be cast automatically to the type of the destination <paramref name="array"/>.
        ///                 </exception>
        public void CopyTo(string[] array, int arrayIndex)
        {
            AnyOf.CopyTo(array, arrayIndex);
        }

        /// <summary>
        /// Removes the first occurrence of a specific object from the <see cref="T:System.Collections.Generic.ICollection`1"/>.
        /// </summary>
        /// <returns>
        /// true if <paramref name="item"/> was successfully removed from the <see cref="T:System.Collections.Generic.ICollection`1"/>; otherwise, false. This method also returns false if <paramref name="item"/> is not found in the original <see cref="T:System.Collections.Generic.ICollection`1"/>.
        /// </returns>
        /// <param name="item">The object to remove from the <see cref="T:System.Collections.Generic.ICollection`1"/>.
        ///                 </param><exception cref="T:System.NotSupportedException">The <see cref="T:System.Collections.Generic.ICollection`1"/> is read-only.
        ///                 </exception>
        public bool Remove(string item)
        {
            bool bR = remMode;
            bool bA = addMode;


            remMode = true;
            addMode = false;

            Add(item.Replace("+", "&").Replace("-", "+").Replace("&", "-"));

            remMode = bR;
            addMode = bA;

            return true;

        }

        /// <summary>
        /// Gets the number of elements contained in the <see cref="T:System.Collections.Generic.ICollection`1"/>.
        /// </summary>
        /// <returns>
        /// The number of elements contained in the <see cref="T:System.Collections.Generic.ICollection`1"/>.
        /// </returns>
        public int Count
        {
            get { return AnyOf.Count + ExceptFor.Count; }
        }

        public static string ReadLineFromInput(OutputDelegate outputDelegate, string prompt)
        {
            TextWriter w = (DLRConsole.Out ?? (Console.Out ?? Console.Error) ?? new StringWriter());
            MethodInfo rm = null;
            object ro = null;       
            if (outputDelegate != null)
            {
                var mi = outputDelegate.Method;
                ro = outputDelegate.Target;
                if (mi != null)
                {
                    var i = mi.DeclaringType;
                    rm = i.GetMethod("ReadLine", new Type[0]);
                    if (rm == null)
                    {
                        i = mi.ReflectedType;
                        rm = i.GetMethod("ReadLine", new Type[0]);
                    }
                }
            }
            outputDelegate = outputDelegate ?? w.Write;
            outputDelegate(prompt);
            w.Flush();

            if (rm == null)
            {
                TextReader r = DLRConsole.In;
                if (r == null)
                {
                    outputDelegate("SYSTEM: No Input");
                    return null;
                }
                return r.ReadLine();
            }
                return "" + rm.Invoke(ro, new object[0]);
        }

        /// <summary>
        /// Gets a value indicating whether the <see cref="T:System.Collections.Generic.ICollection`1"/> is read-only.
        /// </summary>
        /// <returns>
        /// true if the <see cref="T:System.Collections.Generic.ICollection`1"/> is read-only; otherwise, false.
        /// </returns>
        public bool IsReadOnly
        {
            get { return false; }
        }

        #endregion


        public static string ClipString(string info, int len)
        {
            if (info==null) return null;
            if (len < 1) return info;
            int il = info.Length;
            if (info.Length<=len) return info;
            string lfch = (info.Contains("\0")
                               ? "\0"
                               : !info.Contains("\r") ? "\n" : info.Contains("\r\n") ? "\r\n" : "\r");
            int find = info.IndexOf(lfch);
            if (find == -1)
            {
                len = len - 1;
                len = len/2;
                return info.Substring(0, len) + " ..." + lfch + ".. " + info.Substring(il - len);
            }
            return ClipString(info.Substring(0, find - 1), len) + lfch + ClipString(info.Substring(find + 1), len);
        }

        public string SafeFormatShould(string str, params object[] args)
        {
            str = DLRConsole.SafeFormat(str, args);
            if (!ShouldPrint(str)) return null;
            return str;
        }
    }
}
