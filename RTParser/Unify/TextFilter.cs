using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using RTParser;

namespace RTParser
{
    public class TextFilter: ICollection<string>
    {
        private HashSet<string> AnyOf = new HashSet<string>() {"ERROR"};
        private HashSet<string> ExceptFor = new HashSet<string>() {};
        bool addMode = true;
        bool remMode = true;
        public TextFilter()
        {
            
        }
        private string lastOutput = "";
        public void writeDebugLine(RTPBot.OutputDelegate console, string message, params object[] args)
        {
            console = console ?? Console.WriteLine;
            lock (this)
                try
                {
                    if (args != null && args.Length != 0) message = String.Format(message, args);
                    if (lastOutput == message) return;
                    if (lastOutput.Contains(message))
                    {
                        return;
                    }
                    lastOutput = message;
                    string msgTest = message.ToUpper();
                    bool printIt = message.StartsWith("-");
                    if (printIt)
                    {
                        message = message.Substring(1);
                    }
                    else
                    {
                        bool foundOne = false;
                        lock (AnyOf)
                            foreach (string s in AnyOf)
                            {
                                if (s == "*") printIt = true;
                                else if (msgTest.Contains(s))
                                {
                                    foundOne = true;
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
                    }
                    if (printIt)
                    {
                        bool doHeader = message.Contains("!");

                        if (doHeader)
                            writeDebugLine(console, "---------------------------------------------------------------");
                        message = message.Replace("\r\n", "<br/>");
                        message = message.Replace("\n", "<br/>");
                        message = message.Replace("<br/>", " " + Environment.NewLine);
                        System.Console.WriteLine(message);
                        if (doHeader)
                            writeDebugLine(console, "----------------------------------------------------------------");

                    }
                }
                catch (Exception e)
                {
                    console(message + " --> " + e);
                }
        }

        public void UpateLogging(string sa, RTPBot.OutputDelegate od)
        {
            while (sa != null && sa.Trim().Length > 0)
            {
                sa = sa.Trim().ToLower();
                if (sa.StartsWith("clear-"))
                {
                    addMode = true;
                    remMode = false;
                    ExceptFor.Clear();
                    od("Clearing -");
                    sa = sa.Substring(6);
                    continue;
                }
                if (sa.StartsWith("clear+"))
                {
                    addMode = true;
                    remMode = false;
                    ExceptFor.Clear();
                    od("Clearing +");
                    sa = sa.Substring(6);
                    continue;
                }
                if ((sa + " ").StartsWith("clear "))
                {
                    addMode = false;
                    remMode = false;
                    AnyOf.Clear();
                    ExceptFor.Clear();
                    od("Clearing +/-");
                    sa = sa.Substring(6);
                    continue;
                }
                if (sa.StartsWith("+"))
                {
                    addMode = true;
                    sa = sa.Substring(1);
                    continue;
                }
                if (sa.StartsWith("-"))
                {
                    remMode = true;
                    sa = sa.Substring(1);
                    continue;
                }
                int f = (sa + "\n").IndexOfAny(new char[] {'+', '-', '\n'});
                string w = sa.Substring(0, f).ToUpper().Trim();
                if (addMode)
                {
                    AnyOf.Add(w);
                }
                else if (remMode)
                {
                    ExceptFor.Add(w);
                }
                sa = sa.Substring(f);
            }
            foreach (var of in AnyOf)
            {
                od("AnyOf: " + of);
            }
            foreach (var of in ExceptFor)
            {
                od("ExceptFor: " + of);
            }
        }

        public static bool ListEdit(ICollection<string> collection, string ss, RTPBot.OutputDelegate console)
        {
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
                    Console.WriteLine("{0} Count={1}", collection, collection.Count);
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
            ExceptFor.Remove(item);
            AnyOf.Add(item);
        }

        /// <summary>
        /// Removes all items from the <see cref="T:System.Collections.Generic.ICollection`1"/>.
        /// </summary>
        /// <exception cref="T:System.NotSupportedException">The <see cref="T:System.Collections.Generic.ICollection`1"/> is read-only. 
        ///                 </exception>
        public void Clear()
        {
            AnyOf.Clear();
            ExceptFor.Clear();
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
            AnyOf.Remove(item);
            return ExceptFor.Add(item);
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
    }
}
