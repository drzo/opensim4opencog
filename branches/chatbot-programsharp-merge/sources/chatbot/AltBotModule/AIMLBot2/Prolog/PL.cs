#if (!STANDARD)
#define debugging
#define arg1index
#define mswindows
#define newor
#define partialengine
#endif

#if (!VISUAL_STUDIO)
#undef mswindows
#endif

//#define debug
namespace RTParser.Prolog
{
    using System;
    using System.IO;
    using System.Text;
    using System.Xml;
    using System.Collections;
    using System.Collections.Specialized;
    using System.Globalization;
    using System.Threading;
    using System.Diagnostics;
    using System.Collections.Generic;
    //using TrieArrayList = System.Collections.ArrayList;// System.Collections.Generic.HashSet<System.IComparable>;

    /* _______________________________________________________________________________________________
      |                                                                                               |
      |  C#Prolog -- Copyright (C) 2007 John Pool -- j.pool@ision.nl                                  |
      |                                                                                               |
      |  This library is free software; you can redistribute it and/or modify it under the terms of   |
      |  the GNU General Public License as published by the Free Software Foundation; either version  |
      |  2 of the License, or any later versionC:\development\opensim4opencog\RTParser\Prolog\PL.grm.                                                      |
      |                                                                                               |
      |  This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;    |
      |  without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.    |
      |  See the GNU General Public License for details, or enter 'license.' at the command prompt.   |
      |_______________________________________________________________________________________________|
    */

    // Parser Generator version 4.0 -- Date/Time: 29-6-2007 8:24:32

    #region Exceptions
    internal class ParserException : CSPrologException
    {
        public ParserException(string msg) : base(msg) { }
    }

    internal class SyntaxException : CSPrologException
    {
        public SyntaxException(string msg) : base(msg) { }
    }
    #endregion Exceptions

    #region Buffer
    public class Buffer
    {
        protected char indentChar = '\u0020';
        internal int indentLength = 0;
        protected int indentDelta = 2;
        private Stack indentStack = new Stack();
        protected string name;
        protected bool quietMode = false;
        protected bool firstSymbolOnLine = true; // NOT SET IN READ BUFFERS
        protected int positionOnLine = 0;

        public virtual char this[int i]
        {
            get { return '\0'; }
        }


        public string Name
        {
            get { return name; }
        }


        public virtual string Substring(int n, int len)
        {
            return ""; // gets overridden
        }


        public virtual int Length
        {
            get { return 0; } // gets overridden
        }


        public virtual void UpdateCache(int p)
        {
        }


        public void Indent()
        {
            indentStack.Push(indentLength); // save current
            indentLength += indentDelta;
        }


        public void Undent()
        {
            indentLength = (indentStack.Count == 0) ? 0 : (int)indentStack.Pop();
        }


        public void Indent(int i)
        {
            indentStack.Push(indentLength); // save current
            indentLength = i;
        }


        public virtual void Write(string s)
        {
        }


        public virtual void Write(string s, params object[] pa)
        {
        }


        public virtual void WriteLine(string s, params object[] pa)
        {
        }


        public virtual void WriteChar(char c)
        {
        }


        public virtual void NewLine()
        {
        }


        public virtual void Clear()
        {
        }


        public virtual void SaveToFile(string fileName)
        {
        }


        public virtual void Close()
        {
        }


        public bool QuietMode
        {
            get { return quietMode; }
            set { quietMode = value; }
        }


        public virtual bool FirstSymbolOnLine
        {
            get { return firstSymbolOnLine; }
        }


        public virtual int PositionOnLine
        {
            get { return positionOnLine; }
        }
    }


    #region StringBuffer
    public class StringBuffer : Buffer
    {
    }


    #region StringReadBuffer
    public class StringReadBuffer : StringBuffer
    {
        string buffer;

        public StringReadBuffer(string s)
        {
            buffer = s;
            name = "input string";
        }


        public override char this[int i]
        {
            get { return buffer[i]; }
        }


        public override int Length
        {
            get { return buffer.Length; }
        }


        public override string Substring(int n, int len)
        {
            return buffer.Substring(n, len);
        }
    }
    #endregion StringReadBuffer


    #region StringWriteBuffer
    public class StringWriteBuffer : StringBuffer
    {
        private StringBuilder sb;

        public StringWriteBuffer(string s)
        {
            sb = new StringBuilder(s);
        }


        public StringWriteBuffer()
        {
            sb = new StringBuilder();
        }


        public override void Clear()
        {
            sb.Length = 0;
            firstSymbolOnLine = true;
            positionOnLine = 0;
        }


        public override void Write(string s, params object[] pa)
        {
            if (quietMode) return;
            if (s == "") return;

            if (firstSymbolOnLine)
            {
                if (indentLength > 0) sb.Append(new String(indentChar, indentLength));
                firstSymbolOnLine = false;
                positionOnLine = indentLength;
            }

            sb.AppendFormat(s, pa);
            positionOnLine += s.Length;
        }


        public override void Write(string s)
        {
            if (quietMode) return;
            if (s == "") return;

            if (firstSymbolOnLine)
            {
                if (indentLength > 0) sb.Append(new String(indentChar, indentLength));
                firstSymbolOnLine = false;
                positionOnLine = indentLength;
            }

            //Console.WriteLine ("Write1: {0}", s);
            sb.Append(s);
            positionOnLine += s.Length;
        }


        public override void WriteLine(string s, params object[] pa)
        {
            if (quietMode) return;

            if (firstSymbolOnLine && indentLength > 0) sb.Append(new String(indentChar, indentLength));
            if (pa.Length == 0) sb.Append(s); else sb.AppendFormat(s, pa);
            sb.Append(Environment.NewLine);
            firstSymbolOnLine = true;
            positionOnLine = 0;
        }


        public override void WriteChar(char c)
        {
            if (quietMode) return;

            sb.Append(c);

            positionOnLine++;
        }


        public override void NewLine()
        {
            if (quietMode) return;

            sb.Append(Environment.NewLine);
            firstSymbolOnLine = true;
            positionOnLine = 0;
        }


        public override char this[int i]
        {
            get { return sb[i]; }
        }


        public override string ToString()
        {
            return sb.ToString();
        }


        public override void SaveToFile(string fileName)
        {
            StreamWriter sw = new StreamWriter(fileName);
            sw.Write(sb.ToString());
            sw.Close();
        }


        public override int Length
        {
            get { return sb.Length; }
        }
    }
    #endregion StringWriteBuffer
    #endregion StringBuffer


    // FileBuffer

    #region FileBuffer
    public class FileBuffer : Buffer
    {
        protected FileStream fs;
        protected long strmLength
        {
            get { return fs.Length; }
        }
    }

    // FileReadBuffer

    #region FileReadBuffer
    public class FileReadBuffer : FileBuffer
    {
        private const int CACHESIZE = 256 * 1024;
        private byte[] cache = new byte[CACHESIZE];
        private int cacheOfs; // number of chars in fs before first char of cache
        private int cacheLen; // cache length (normally CACHESIZE, less at eof)
        private bool little_endian;
        private StringBuilder sb;

        public FileReadBuffer(string fileName)
        {
            name = fileName;
            try
            {
                fs = new FileStream(fileName, FileMode.Open, FileAccess.Read, FileShare.Read);
                sb = new StringBuilder();
                cacheOfs = 0;
                cacheLen = 0;
            }
            catch
            {
                throw new ParserException(String.Format("*** Could not open file '{0}' for reading", fileName));
            }
            if (strmLength >= 2) // try to work out type of file (primitive approach)
            {
                fs.Read(cache, 0, 2);
                little_endian = (cache[0] == '\xFF' && cache[1] == '\xFE');
                //if (little_endian) throw new Exception (String.Format ("*** File '{0}' is little_endian-encoded -- not supported yet", fileName));
                fs.Position = 0; // rewind
                /* It appears that editing and saving a little_endian-encoded file causes
                   the  \xFF\x\FE to disappear. This will cause an error. No solution yet.
                */
            }
        }

        ~FileReadBuffer()
        {
            if (fs != null) Close();
        }


        public override void Close()
        {
            fs.Close();
        }


        public override void UpdateCache(int p)
        {
            int i;
            cacheOfs = CACHESIZE * (p / CACHESIZE);
            if (cacheOfs > strmLength)
                throw new Exception(String.Format("*** Attempt to read beyond end of FileReadBuffer '{0}'", name));
            else
                fs.Position = cacheOfs;
            cacheLen = fs.Read(cache, 0, CACHESIZE); // cacheLen is actual number of bytes read
            if (cacheLen < CACHESIZE)
            {
                for (i = cacheLen; i < CACHESIZE; cache[i++] = 32) ;
                //cacheLen += 2;
            }
        }


        public override char this[int i]
        {
            get
            {
                if (little_endian) i = 2 * i + 2;
                if ((i < cacheOfs) || (i >= cacheOfs + cacheLen)) UpdateCache(i);
                return (char)cache[i % CACHESIZE];  // no test on cacheLen
            }
        }


        public override string Substring(int n, int len)
        {
            sb.Length = 0;
            for (int i = n; i < n + len; i++) sb.Append(this[i]);

            return sb.ToString();
        }


        public override int Length
        {
            get { return Convert.ToInt32(((little_endian) ? (strmLength / 2 - 1) : strmLength)); }
        }
    }
    #endregion FileReadBuffer


    // FileWriteBuffer

    #region FileWriteBuffer
    public class FileWriteBuffer : FileBuffer
    {
        private StreamWriter sw;
        private bool isTemp;

        public FileWriteBuffer(string fileName)
        {
            name = fileName;
            try
            {
                fs = new FileStream(fileName, FileMode.Create, FileAccess.Write);
                sw = new StreamWriter(fs);
                isTemp = false;
            }
            catch
            {
                throw new ParserException(String.Format("*** Could not create file '{0}'", fileName));
            }
        }


        public FileWriteBuffer()
        {
            try
            {
                name = Path.GetTempFileName();
                fs = new FileStream(name, FileMode.Create);
                sw = new StreamWriter(fs);
                isTemp = true;
            }
            catch
            {
                throw new Exception("*** FileWriteBuffer constructor could not create temporary file");
            }
        }


        ~FileWriteBuffer()
        {
            Close();
        }


        public override void SaveToFile(string fileName)
        {
            FileStream f = new FileStream(fileName, FileMode.Create);
            byte[] b = new byte[strmLength];
            fs.Read(b, 0, b.Length);
            f.Write(b, 0, b.Length);
            f.Close();
        }


        public override string Substring(int n, int len)
        {
            byte[] b = new byte[len];
            fs.Position = n;
            fs.Read(b, 0, len);
            return new ASCIIEncoding().GetString(b);
        }


        public override string ToString()
        {
            byte[] b = new byte[strmLength];
            fs.Read(b, 0, b.Length);
            ASCIIEncoding enc = new ASCIIEncoding();
            return enc.GetString(b);
        }


        public override void Close()
        {
            fs.Close();
            if (isTemp)
            {
                try
                {
                    File.Delete(name);
                }
                catch
                {
                    throw new Exception("*** FileWriteBuffer Close() could not delete temporary file");
                }
            }
        }


        public void Append(FileWriteBuffer f)  // f is assumed open for reading
        {
            byte[] b = new byte[f.strmLength];
            f.fs.Read(b, 0, b.Length);
            fs.Position = strmLength;
            fs.Write(b, 0, b.Length);
            firstSymbolOnLine = false;
        }


        public new int Length
        {
            get { return Convert.ToInt32(strmLength); }
            set { fs.SetLength(value); }
        }


        public override void Clear()
        {
            fs.SetLength(0);
            firstSymbolOnLine = true;
        }


        public override void Write(string s, params object[] pa)
        {
            if (quietMode) return;
            if (s == "") return;

            if (firstSymbolOnLine)
            {
                if (indentLength > 0) sw.Write(new String(indentChar, indentLength));
                firstSymbolOnLine = false;
            }
            sw.Write(s, pa);
        }


        public override void WriteLine(string s, params object[] pa)
        {
            if (quietMode) return;

            if (firstSymbolOnLine) sw.Write(new String(indentChar, indentLength));
            sw.WriteLine(s, pa);
            firstSymbolOnLine = true;
        }


        public override void WriteChar(char c)
        {
            if (quietMode) return;

            sw.Write(c);
        }


        public override void NewLine()
        {
            if (quietMode) return;

            sw.Write(Environment.NewLine);
            firstSymbolOnLine = true;
        }
    }
    #endregion FileWriteBuffer
    #endregion FileBuffer

    #region XmlWriteBuffer
    public class XmlWriteBuffer
    {
        protected XmlTextWriter tw;
        protected Stack tagStack; // extra check on matching end tag

        protected void SetInitialValues(bool initialPI)
        {
            tagStack = new Stack();
            tw.QuoteChar = '"';
            tw.Formatting = Formatting.Indented;
            if (initialPI) tw.WriteProcessingInstruction("xml", "version=\"1.0\" encoding=\"ISO-8859-1\"");
            tw.WriteComment(String.Format(" Structure created at {0} ", DateTime.Now.ToString()));
        }


        private void WriteAttributes(params string[] av)
        {
            if (av.Length % 2 == 1)
                throw new ParserException("*** WriteStartElement -- last attribute value is missing");

            for (int j = 0; j < av.Length; j += 2) tw.WriteAttributeString(av[j], av[j + 1]);
        }


        public void WriteStartElement(string tag, params string[] av)
        {
            tw.WriteStartElement(tag);
            WriteAttributes(av);
            tagStack.Push(tag);
        }


        public void WriteAttributeString(string name, string value, params string[] av)
        {
            tw.WriteAttributeString(name, value);
            WriteAttributes(av);

            for (int j = 0; j < av.Length; j += 2) tw.WriteAttributeString(av[j], av[j + 1]);
        }


        public void WriteEndElement(string tag)
        {
            if (tagStack.Count == 0)
                throw new ParserException(String.Format("*** Spurious closing tag \"{0}\"", tag));

            string s = (string)tagStack.Peek();

            if (tag != s)
                throw new ParserException(String.Format("*** Closing tag \"{0}\" does not match opening tag \"{1}\"", tag, s));

            tw.WriteEndElement();
            tagStack.Pop();
        }


        public void WriteProcessingInstruction(string name, string text)
        {
            tw.WriteProcessingInstruction(name, text);
        }


        public void WriteComment(string text)
        {
            tw.WriteComment(text);
        }


        public void WriteString(string text)
        {
            tw.WriteString(text);
        }


        public void WriteRaw(string text)
        {
            tw.WriteRaw(text);
        }


        public void WriteCData(string text)
        {
            tw.WriteCData(text);
        }


        public void WriteSimpleElement(string elementName, string textContent, params string[] av)
        {
            tw.WriteStartElement(elementName);
            WriteAttributes(av);
            tw.WriteString(textContent);
            tw.WriteEndElement();
        }


        public void WriteEmptyElement(string elementName, params string[] av)
        {
            tw.WriteStartElement(elementName);
            WriteAttributes(av);
            tw.WriteEndElement();
        }


        public void Close()
        {
            tw.Close();
        }
    }
    #endregion XmlWriteBuffer

    #region XmlFileWriter

    public class XmlFileWriter : XmlWriteBuffer
    {
        public XmlFileWriter(string fileName, bool initialPI)
        {
            tw = new XmlTextWriter(fileName, System.Text.Encoding.GetEncoding(1252));
            SetInitialValues(initialPI);
        }


        public XmlFileWriter(string fileName)
        {
            tw = new XmlTextWriter(fileName, System.Text.Encoding.GetEncoding(1252));
            SetInitialValues(true);
        }
    }
    #endregion XmlFileWriter

    #region XmlStringWriter
    public class XmlStringWriter : XmlWriteBuffer
    {
        public XmlStringWriter(bool initialPI)
        {
            tw = new XmlTextWriter(new MemoryStream(), System.Text.Encoding.GetEncoding(1252));
            SetInitialValues(initialPI);
        }


        public XmlStringWriter()
        {
            tw = new XmlTextWriter(new MemoryStream(), System.Text.Encoding.GetEncoding(1252));
            SetInitialValues(true);
        }


        public void SaveToFile(string fileName)
        {
            StreamWriter sw = new StreamWriter(fileName);
            sw.Write(this.ToString());
            sw.Close();
        }


        public override string ToString()
        {
            Stream ms = tw.BaseStream;

            if (ms == null) return null;

            tw.Flush();
            return new ASCIIEncoding().GetString(((MemoryStream)ms).ToArray());
        }
    }
    #endregion XmlStringWriter
    #endregion Buffer

    #region delegates
    public delegate void SymbolScanned(ref int terminal, int prevTerminal);
    public delegate void SymbolProcessed(int terminal, int prevTerminal);
    public delegate void ProcessComment(bool multiLine, bool firstOnLine);
    public delegate void BlankLine(int eoLineCount);
    public delegate string EvaluateString(string s);
    public delegate bool AcceptIdentifier();
    #endregion delegates

    #region XmlExpression
    public class XmlExpression
    {
        private ArrayList expr = new ArrayList();
        private enum ElemType { s, e, a, c };

        private struct Entry
        {
            public ElemType Type;
            public string Arg1;
            public string Arg2;

            public Entry(ElemType t, string a1, string a2) { Type = t; Arg1 = a1; Arg2 = a2; }
        }


        public XmlExpression(XmlExpression x)
        {
            if (x != null) this.expr = (ArrayList)x.expr.Clone();
        }


        public XmlExpression()
        {
        }


        public void Write(XmlStringWriter writer)
        {
            foreach (Entry e in expr)
            {
                switch (e.Type)
                {
                    case ElemType.s:
                        writer.WriteStartElement(e.Arg1);
                        break;
                    case ElemType.a:
                        writer.WriteAttributeString(e.Arg1, e.Arg2);
                        break;
                    case ElemType.c:
                        writer.WriteComment(e.Arg1);
                        break;
                    case ElemType.e:
                        writer.WriteEndElement(e.Arg1);
                        break;
                }
            }
        }


        public XmlExpression EncloseInTag(string s, params string[] attr)
        {
            int len = attr.Length;

            if (len % 2 != 0) throw new Exception("*** EncloseInTags -- odd number of argument for tag '" + s + "'");

            // even entries are names, odd entries are values
            for (int i = len - 1; i >= 0; i -= 2) expr.Insert(0, new Entry(ElemType.a, attr[i - 1], attr[i]));
            expr.Insert(0, new Entry(ElemType.s, s, null));
            expr.Add(new Entry(ElemType.e, s, null));

            return this;
        }


        public XmlExpression AddEmptyElement(string s, params string[] attr)
        {
            int len = attr.Length;

            if (len % 2 != 0) throw new Exception("AddEmptyElement argument error for tag '" + s + "'");

            expr.Add(new Entry(ElemType.s, s, null));
            for (int i = 0; i < len; i += 2) expr.Add(new Entry(ElemType.a, attr[i], attr[i + 1]));
            expr.Add(new Entry(ElemType.e, s, null));

            return this;
        }


        public XmlExpression AppendXmlExpression(XmlExpression e)
        {
            if (e == null) return this;

            foreach (object o in e.expr) this.expr.Add(o);

            return this;
        }


        public void Clear()
        {
            expr.Clear();
        }
    }
    #endregion XmlExpression

    #region TRIEARRYALIST
    public class TrieComparer
#if tcomp
        <T> : IComparer<T>, IComparer where T : IComparable
#else
        :IComparer
#endif
    {
        #region Implementation of IComparer

        /// <summary>
        /// Compares two objects and returns a value indicating whether one is less than, equal to, or greater than the other.
        /// </summary>
        /// <returns>
        /// Value 
        ///                     Condition 
        ///                     Less than zero 
        ///                 <paramref name="x"/> is less than <paramref name="y"/>. 
        ///                     Zero 
        ///                 <paramref name="x"/> equals <paramref name="y"/>. 
        ///                     Greater than zero 
        ///                 <paramref name="x"/> is greater than <paramref name="y"/>. 
        /// </returns>
        /// <param name="x">The first object to compare. 
        ///                 </param><param name="y">The second object to compare. 
        ///                 </param><exception cref="T:System.ArgumentException">Neither <paramref name="x"/> nor <paramref name="y"/> implements the <see cref="T:System.IComparable"/> interface.
        ///                     -or- 
        ///                 <paramref name="x"/> and <paramref name="y"/> are of different types and neither one can handle comparisons with the other. 
        ///                 </exception><filterpriority>2</filterpriority>
        public int Compare(object x, object y)
        {
            return ((IComparable)x).CompareTo(y);
        }
#if tcomp
        #region IComparer<T> Members

        int IComparer<T>.Compare(T x, T y)
        {
            return x.CompareTo(y);
        }
        public int Compare(T x, T y)
        {
            return x.CompareTo(y);
            ///return x.ToString().CompareTo(y.ToString());
        }

        #endregion
#endif

        #endregion
    }
    public class TrieArrayList : TrieArrayListImpl<IComparable>
    {
      
#if tcomp
        public static IComparer Comparitor = new TrieComparer<IComparable>();
#else
        public static IComparer Comparitor = new TrieComparer();
#endif
    }
    public class TrieArrayListImpl<T> //: ArrayList
        : IEnumerable
    {

        private ArrayList list;
        public TrieArrayListImpl()
        {
            list = new ArrayList();
        }
        public void Add(T node)
        {
            list.Add(node);
        }

        public void Insert(int i, T node)
        {
            list.Insert(i, node);
        }

        public int Count
        {
            get { return list.Count; }
        }

        public int BinarySearch(int value)
        {
#if mswindows
            return list.BinarySearch(value);
#else
            return list.BinarySearch(value, TrieArrayList.Comparitor);
#endif
        }
        public int BinarySearch(char value)
        {
#if mswindows
            return list.BinarySearch(value);
#else
            return list.BinarySearch(value, TrieArrayList.Comparitor);
#endif
        }

        #region Implementation of IEnumerable

        /// <summary>
        /// Returns an enumerator that iterates through a collection.
        /// </summary>
        /// <returns>
        /// An <see cref="T:System.Collections.IEnumerator"/> object that can be used to iterate through the collection.
        /// </returns>
        /// <filterpriority>2</filterpriority>
        IEnumerator IEnumerable.GetEnumerator()
        {
            return list.GetEnumerator();
        }

        #endregion

        public void RemoveAt(int i)
        {
            list.RemoveAt(i);
        }

        public void Remove(T descr)
        {
            list.Remove(descr);
        }

        public void TrimToSize()
        {
            list.TrimToSize();
        }

        public T this[int i]
        {
            get { return (T)list[i]; }
        }
    }

    #endregion

    #region Parser
    public class Parser
    {
        private const int UNDEF = -1;
        private object[] parser_arg;
        private static CultureInfo CI = CultureInfo.InvariantCulture;

        #region TermDescr
        private class TermDescr : IComparable
        {
            int iVal;
            object oVal;
            string image;
            int type;

            public int IVal { get { return iVal; } set { iVal = value; } }
            public object OVal { get { return oVal; } set { oVal = value; } }
            public string Image { get { return image; } set { image = value; } }
            public int Type { get { return type; } set { type = value; } }

            public TermDescr(int iv, object ov, string im)
            {
                iVal = iv;
                oVal = ov;
                image = im;
                type = 0;
            }

            public TermDescr(int iv, object ov, string im, int tp)
            {
                iVal = iv;
                oVal = ov;
                image = im;
                type = tp;
            }

            public int CompareTo(object o)
            {
                if (iVal < (int)o)
                    return -1;
                else if (iVal > (int)o)
                    return 1;
                else
                {
                    return 0;
                }
            }

            public override string ToString()
            {
                if (oVal == null)
                    return String.Format("{0} ({1})", iVal, image);
                else
                    return String.Format("{0}:{1} ({2}) [{3}]", iVal, oVal.ToString(), image, type);
            }
        }
        #endregion TermDescr

        #region TrieNode
        private class TrieNode : IComparable
        {
            char keyChar;
            TermDescr termRec;
            TrieArrayList subTrie;

            public static TrieNode Match; // Result of CompareTo == 0, undefined otherwise. For performance only.
            public char KeyChar { get { return keyChar; } set { keyChar = value; } }
            public TermDescr TermRec { get { return termRec; } set { termRec = value; } }
            public TrieArrayList SubTrie { get { return subTrie; } set { subTrie = value; } }

            public TrieNode(char k, TrieArrayList s, TermDescr t)
            {
                keyChar = k;
                subTrie = s;
                termRec = t;
            }


            public int CompareTo(object o)
            {
                if (keyChar < (char)o)
                    return -1;
                else if (keyChar > (char)o)
                    return 1;
                else
                {
                    Match = this;
                    return 0;
                }
            }


            public override string ToString()
            {
                StringBuilder sb = new StringBuilder();

                //ToString (this, sb, 0);        // tree representation
                ToString(this, "", sb, 0);    // flat representation

                return sb.ToString();
            }


            private void ToString(TrieNode node, StringBuilder sb, int indent)
            {
                if (indent == 0)
                    sb.Append("<root>" + Environment.NewLine);
                else
                {
                    sb.AppendFormat("{0}{1} -- ", new string(' ', indent), node.keyChar);

                    if (node.termRec == null)
                        sb.Append(Environment.NewLine);
                    else
                        sb.Append(String.Format(node.TermRec.ToString()));
                }
                if (node.subTrie != null)
                    foreach (TrieNode subTrie in node.subTrie) ToString(subTrie, sb, indent + 1);
            }


            public static void ToArrayList(TrieNode node, bool atRoot, ref TrieArrayList a)
            {
                if (!atRoot) // skip root
                    if (node.termRec != null) a.Add((TrieNode)node.termRec.OVal);
                if (node.subTrie != null)
                    foreach (TrieNode subTrie in node.subTrie) ToArrayList(subTrie, false, ref a);
            }


            private void ToString(TrieNode node, string prefix, StringBuilder sb, int indent)
            {
                if (indent != 0) // skip root
                    if (node.termRec != null) sb.AppendFormat("{0} {1}\r\n", prefix, node.termRec.ToString());
                if (node.subTrie != null)
                    foreach (TrieNode subTrie in node.subTrie) ToString(subTrie, prefix, sb, indent + 1);
            }
        }
        #endregion TrieNode


        #region Trie
        private enum DupMode { dupIgnore, dupAccept, dupError };
        private class Trie
        {
            public static readonly int UNDEF = -1;
            private TrieNode root = new TrieNode('\x0', null, null);
            private TrieArrayList indices = Trie.MakeArrayList();
            private Hashtable names = new Hashtable();
            private TrieArrayList curr;
            private DupMode dupMode = DupMode.dupError;
            private bool caseSensitive = false; // every term to lowercase
            private TrieArrayList currSub;
            public bool AtLeaf { get { return (currSub == null); } }

            public Trie()
            {
            }


            public Trie(DupMode dm)
            {
                dupMode = dm;
            }


            public Trie(bool cs)
            {
                caseSensitive = cs;
            }


            public Trie(DupMode dm, bool cs)
            {
                dupMode = dm;
                caseSensitive = cs;
            }


            public void Add(int iVal, string name, params string[] images)
            {
                Add(iVal, 0, name, images);
            }


            public void Add(int iVal, int type, string name, params string[] images)
            {
                names[iVal] = name;

                foreach (string key in images) Add(key, iVal, null, type);
            }


            public void Add(string key, int iVal, object oVal)
            {
                Add(key, iVal, oVal, 0);
            }

            public void Add(string key, int iVal, object oVal, int type)
            {
                try
                {
                    Add0(key, iVal, oVal, type);
                }
                catch (Exception e)
                {
                    Console.WriteLine("Trie.Add({0},{1},{2},{3})" + e, key, iVal, oVal, type);
                }
            }

            private void Add0(string key, int iVal, object oVal, int type)
            {
                if (key == null || key == "")
                    throw new Exception("*** Trie.Add: Attempt to insert a null- or empty key");

                if (!caseSensitive) key = key.ToLower();

                if (root.SubTrie == null) root.SubTrie = Trie.MakeArrayList();

                curr = root.SubTrie;
                int imax = key.Length - 1;
                TrieNode node;
                TrieNode next;

                int i = 0;
                while (i <= imax)
                {
                    int k = (curr.Count == 0) ? -1 : curr.BinarySearch(key[i]);

                    if (k >= 0) // found
                    {
                        node = TrieNode.Match;
                        if (i == imax) // at end of key
                        {
                            if (node.TermRec == null)
                                AddToIndices(node.TermRec = new TermDescr(iVal, oVal, key, type));
                            else if (dupMode == DupMode.dupAccept)
                            {
                                TermDescr trec = node.TermRec;
                                trec.IVal = iVal;
                                trec.OVal = oVal;
                                trec.Image = key;
                                trec.Type = type;
                            }
                            else if (dupMode == DupMode.dupError)
                                throw new Exception(String.Format("*** Attempt to insert duplicate key '{0}'", key));
                            return;
                        }
                        if (node.SubTrie == null) node.SubTrie = Trie.MakeArrayList();
                        curr = node.SubTrie;
                        i++;
                    }
                    else // char not found => append chain of TrieNodes for rest of key
                    {
                        node = new TrieNode(key[i], null, null);
                        curr.Insert(~k, node);
                        while (true)
                        {
                            if (i == imax) // at end of key
                            {
                                AddToIndices(node.TermRec = new TermDescr(iVal, oVal, key, type));

                                return;
                            }
                            node.SubTrie = Trie.MakeArrayList();
                            node.SubTrie.Add(next = new TrieNode(key[++i], null, null));
                            node = next;
                        }
                    }
                }
            }

            private static TrieArrayList MakeArrayList()
            {
                return new TrieArrayList();
            }


            private void AddToIndices(TermDescr td)
            {
                int k = indices.BinarySearch(td.IVal);
                indices.Insert((k < 0) ? ~k : k, td);
            }


            public bool Find(string key, out TermDescr trec)
            {
                if (key == null || key == "")
                    throw new Exception("*** Trie.Add: Attempt to search for a null- or empty key");

                int imax = key.Length - 1;
                int i = 0;

                trec = null;
                curr = root.SubTrie;

                while (curr != null)
                {
                    if (curr.BinarySearch(key[i]) < 0) { curr = null; return false; }
                    if (i++ == imax) return ((trec = TrieNode.Match.TermRec) != null);
                    curr = TrieNode.Match.SubTrie;
                }
                return false;
            }


            public int this[string key]
            {
                get
                {
                    TermDescr trec;

                    return (Find(key, out trec) ? trec.IVal : UNDEF);
                }
                set
                {
                    TermDescr trec;

                    if (Find(key, out trec))
                        trec.IVal = value;
                    else
                        throw new Exception("*** Trie indexer: key [" + key + "] not found");
                }
            }


            public void FindCharInSubtreeReset()
            {
                currSub = root.SubTrie;
            }


            public bool FindCharInSubtree(char c, out TermDescr termRec)
            {
                int k;

                k = (currSub == null) ? -1 : k = currSub.BinarySearch(c);

                if (k >= 0)
                {
                    termRec = TrieNode.Match.TermRec;
                    currSub = TrieNode.Match.SubTrie;
                    return true;
                }
                else
                {
                    termRec = null;
                    return false;
                }
            }


            public string Name(int i)
            {
                return (string)names[i];
            }


            public TrieArrayList TerminalsOf(int i)
            {
                int k = indices.BinarySearch(i);

                if (k < 0) return null;

                var result = Trie.MakeArrayList();
                int k0 = k;

                while (true)
                {
                    result.Add(indices[k++]);
                    if (k == indices.Count || ((TermDescr)indices[k]).CompareTo(i) != 0) break;
                }

                k = k0 - 1;

                while (k > 0 && ((TermDescr)indices[k]).CompareTo(i) == 0)
                    result.Add(indices[k--]);

                return result;
            }


            public int IndexOf(string key)
            {
                TermDescr td;

                return Find(key, out td) ? td.IVal : -1;
            }


            public string TerminalImageSet(TerminalSet ts)
            {
                StringBuilder result = new StringBuilder();
                bool isFirst = true;
                int[] ii;

                ts.ToIntArray(out ii);

                TrieArrayList a;

                foreach (int i in ii)
                {
                    a = TerminalsOf(i);
                    bool isImage = false;

                    if (a != null)
                        foreach (TermDescr td in a)
                        {
                            isImage = true;
                            if (isFirst) isFirst = false; else result.Append(", ");
                            result.Append(td.Image);
                        }

                    if (!isImage)
                    {
                        if (isFirst) isFirst = false; else result.Append(", ");
                        result.Append("<" + (string)names[i] + ">");
                    }
                }
                return result.ToString();
            }


            public string TerminalImage(int t)
            {
                return TerminalImageSet(new TerminalSet(new int[] { t }));
            }


            public bool Remove(string key)
            {
                bool result;
                bool dummy;
                if (!caseSensitive) key = key.ToLower();
                RemoveEx(root, key, 0, key.Length - 1, out result, out dummy);

                return result;
            }


            public void RemoveEx(TrieNode curr, string key, int i, int imax, out bool result, out bool mayDelete)
            {
                result = false;
                mayDelete = false;

                if (curr.SubTrie == null) return;

                int k = curr.SubTrie.BinarySearch(key[i]);
                if (k < 0) return;

                if (i == imax) // last char of key -- now we work our way back to the root
                {
                    if (TrieNode.Match.TermRec != null)
                    {
                        indices.Remove(TrieNode.Match.TermRec);
                        TrieNode.Match.TermRec = null;
                        mayDelete = (curr.SubTrie == null);
                        result = true;
                    }
                }
                else
                    RemoveEx(TrieNode.Match, key, i + 1, imax, out result, out mayDelete);

                if (!result || !mayDelete) return;

                if (mayDelete) curr.SubTrie.RemoveAt(k);

                mayDelete = (curr.SubTrie == null && curr.TermRec != null);
            }


            public void Remove(int i) // remove all entries with index i
            {
                names.Remove(i);

                TrieArrayList a = TerminalsOf(i);

                if (a != null) foreach (TermDescr td in a) Remove(td.Image);
            }


            public void TrimToSize()  // minimize memory overhead if no new elements will be added
            {
                TrimToSizeEx(root);
            }


            public void TrimToSizeEx(TrieNode n)
            {
                if (n.SubTrie != null)
                {
                    n.SubTrie.TrimToSize();
                    foreach (TrieNode t in n.SubTrie) TrimToSizeEx(t);
                }
            }


            internal TrieArrayList ToArrayList()
            {
                var a = Trie.MakeArrayList();

                TrieNode.ToArrayList(root, true, ref a);

                return a;
            }


            public override string ToString()
            {
                return root.ToString();
            }
        }
        #endregion Trie

        #region TerminalSet
        class TerminalSet
        {
            private bool[] x;

            public TerminalSet(params int[] ta)
            {
                x = new bool[Parser.terminalCount];
                for (int i = 0; i < Parser.terminalCount; i++) x[i] = false;
                foreach (int terminal in ta) x[terminal] = true;
            }


            public TerminalSet(bool[] y)
            {
                x = new bool[Parser.terminalCount];
                for (int i = 0; i < Parser.terminalCount; i++) x[i] = y[i];
            }


            public TerminalSet Union(params int[] ta)
            {
                TerminalSet union = new TerminalSet(x);              // create an identical TerminalSet
                foreach (int terminal in ta) union[terminal] = true; // ... and unify
                return union;
            }


            public bool this[int i]
            {
                set { x[i] = value; }
            }


            public bool Contains(int terminal)
            {
                return x[terminal];
            }


            public bool IsEmpty()
            {
                for (int i = 0; i < x.Length; i++) if (x[i]) return false;

                return true;
            }


            public void ToIntArray(out int[] a)
            {
                int count = 0;
                for (int i = 0; i < x.Length; i++) if (x[i]) count++;
                a = new int[count];
                count = 0;
                for (int i = 0; i < x.Length; i++) if (x[i]) a[count++] = i;
            }
        }
        #endregion TerminalSet

        #region Symbol
        private struct Symbol
        {
            private Parser parser;
            private bool processed;
            internal object OValue;
            internal int Type; // type
            public int Terminal;
            internal int Start;     // position in input stream
            internal int Final;     // first position after symbol
            internal int FinalPlus; // first position of next symbol
            internal int PrevFinal; // final position of previous symbol
            internal int LineNo;
            internal int LineStart; // position of first char of line in input stream
            internal int AbsSeqNo;  // sequence number of symbol // -- absolute value, invariant under MARK/REDO
            internal int RelSeqNo;  // sequence number of symbol in current line // -- relative value, invariant under MARK/REDO
            internal bool HasIdFormat;

            public Symbol(Parser p)
            {
                parser = p;
                processed = false;
                OValue = null;
                Type = 0;
                Terminal = Parser.Undefined;
                Start = UNDEF;
                Final = UNDEF;
                FinalPlus = UNDEF;
                PrevFinal = UNDEF;
                LineNo = UNDEF;
                LineStart = UNDEF;
                AbsSeqNo = UNDEF;
                RelSeqNo = UNDEF;
                HasIdFormat = false;
            }

            public new string ToString()
            {
                if (this.Start == this.Final)
                    return "";
                if (this.Terminal == Parser.Undefined && this.Start > this.Final)
                    return "<Undefined>";
                if (this.Terminal == Parser.EndOfLine)
                    return "<EndOfLine>";
                if (this.Terminal == Parser.EndOfInput)
                    return "<EndOfInput>";
                return parser.StreamInClip(this.Start, this.Final);
            }


            public int ColNo
            {
                get
                {
                    return (Start >= LineStart) ? Start - LineStart + 1 : UNDEF;
                }
            }


            public string EvalString()
            {
                if (this.Start >= this.Final)
                    return "";
                else if (parser.EvaluateStringHook != null && parser.formatMode && !parser.tokenize)
                    return parser.EvaluateStringHook(parser.StreamInClip(this.Start, this.Final));
                else
                    return ToUnquoted();
            }


            public string ToName()
            {
                return Parser.terminalTable.Name(this.Terminal);
            }


            public string ToStringOrName()
            {
                //return parser.StreamInClip (this.Start, this.Final);
                return Parser.terminalTable.Name(this.Terminal);
            }


            public string ToUnquoted()
            {
                string s = this.ToString();
                int len = s.Length;
                char c;

                if (len < 2) return s;

                if ((c = s[0]) == parser.strStartChar && s[len - 1] == parser.strStartChar ||
                    (c = s[0]) == parser.strStartCharAlt && s[len - 1] == parser.strStartCharAlt)
                    return s.Substring(1, len - 2).Replace("\u0027" + c, c.ToString());
                else
                    return s;
            }


            public int ToInt()
            {
                try
                {
                    return Convert.ToInt32(ToString());
                }
                catch
                {
                    throw new ParserException("*** Unable to convert \"" + this.ToString() + "\" to an integer value");
                }
            }


            public int ToShort()
            {
                try
                {
                    return Convert.ToInt16(ToString());
                }
                catch
                {
                    throw new ParserException("*** Unable to convert \"" + this.ToString() + "\" to a short value");
                }
            }


            public double ToDouble()
            {
                try
                {
                    return Double.Parse(ToString(), invCulture);
                }
                catch
                {
                    throw new ParserException("*** Unable to convert \"" + this.ToString() + "\" to a real (double) value");
                }
            }


            public decimal ToDecimal()
            {
                try
                {
                    return Decimal.Parse(ToString(), invCulture);
                }
                catch
                {
                    throw new ParserException("*** Unable to convert \"" + this.ToString() + "\" to a decimal value");
                }
            }


            public string Dump()
            {
                return String.Format("Start={0} Final={1} LineStart={2}", Start, Final, LineStart);
            }


            public string DebugInfo
            {
                get { return String.Format("'{0}' ({1})", this.ToString(), this.ToName()); }
            }


            public string Context
            {
                get
                {
                    StringBuilder sb = new StringBuilder(String.Format("{0}*** {1}: Error in line {2}", Environment.NewLine,
                                                                         parser.inStream.Name, this.LineNo));

                    if (this.Start >= this.Final) return sb.ToString() + Environment.NewLine;

                    if (this.Start >= this.LineStart)
                        sb.Append(" at position " + ColNo.ToString());

                    if (parser.inStream.Name != "")
                        sb.Append(Environment.NewLine + InputLine + Environment.NewLine);

                    return sb.ToString();
                }
            }


            public bool IsMemberOf(params int[] ts)
            {
                int lo, hi, mid;
                int n = ts.Length;

                if (n <= 8) // linear
                {
                    for (lo = 0; lo < n; lo++) if (Terminal <= ts[lo]) break;

                    return (lo >= n) ? false : ts[lo] == Terminal;
                }
                else // binary
                {
                    lo = -1;
                    hi = n;
                    while (hi != lo + 1)
                    {
                        mid = (lo + hi) >> 1;
                        if (Terminal < ts[mid]) hi = mid; else lo = mid;
                    }
                    return (lo < 0) ? false : ts[lo] == Terminal;
                }
            }


            public bool IsProcessed
            {
                get { return processed; }
            }


            public void SetProcessed(bool status)
            {
                if (status && parser.formatMode && parser.SymbolProcessedHook != null)
                    parser.SymbolProcessedHook(Terminal, parser.prevTerminal);

                processed = status;
            }


            public void SetProcessed()
            {
                SetProcessed(true);
            }


            public string Name()
            {
                return Parser.terminalTable.Name(Terminal);
            }


            public string TextPlus()
            {
                if (Terminal == Parser.Undefined && this.Start > this.FinalPlus)
                    return "<Undefined>";
                else
                    return parser.StreamInClip(this.Start, this.FinalPlus);
            }


            public string IntroText()
            {
                if (Terminal == Parser.Undefined && this.Start > this.FinalPlus)
                    return "<Undefined>";
                else
                    return parser.StreamInClip(this.PrevFinal, this.Start);
            }


            public string InputLine
            {
                get
                {
                    char c;
                    int i = this.LineStart;
                    // find end of line
                    while (i < parser.streamInLen)
                    {
                        parser.GetStreamInChar(i, out c);
                        if (c == '\n') break;
                        i++;
                    }
                    return parser.StreamInClip(LineStart, i);
                }
            }
        }
        #endregion


        public static readonly string VersionTimeStamp = "2007-06-29 08:24:32";
        private static CultureInfo invCulture = CultureInfo.InvariantCulture;

        #region Fields and properties

        internal const int Undefined = 0;
        internal const int EndOfInput = 1;
        internal const int SkipTerminal = 2;
        internal const int ANYSYM = 3;
        internal const int B__T = 4;
        internal const int Comma = 5;
        internal const int LeftParen = 6;
        internal const int RightParen = 7;
        internal const int Identifier = 8;
        internal const int IntLiteral = 9;
        internal const int ppDefine = 10;
        internal const int ppUndefine = 11;
        internal const int ppIf = 12;
        internal const int ppIfNot = 13;
        internal const int ppElse = 14;
        internal const int ppEndIf = 15;
        internal const int RealLiteral = 16;
        internal const int StringLiteral = 17;
        internal const int CharLiteral = 18;
        internal const int Operator = 19;
        internal const int Atom = 20;
        internal const int EndOfLine = 21;
        internal const int CommentStart = 22;
        internal const int CommentSingle = 23;
        internal const int Dot = 24;
        internal const int DoubleQuote = 25;
        internal const int Anonymous = 26;
        internal const int CutSym = 27;
        internal const int ImpliesSym = 28;
        internal const int PromptSym = 29;
        internal const int DCGArrowSym = 30;
        internal const int BuiltinAssignSym = 31;
        internal const int LSqBracket = 32;
        internal const int RSqBracket = 33;
        internal const int LCuBracket = 34;
        internal const int RCuBracket = 35;
        internal const int VBar = 36;
        internal const int OpSym = 37;
        internal const int BuiltinSym = 38;
        internal const int QuerySym = 39;
        internal const int ProgramSym = 40;
        internal const int ReadingSym = 41;
        internal const int EnsureLoaded = 42;
        internal const int Discontiguous = 43;
        internal const int AllDiscontiguous = 44;
        internal const int Dynamic = 45;
        internal const int Persistent = 46;
        internal const int Unpersistent = 47;
        internal const int Module = 48;
        internal const int UndefPredAction = 49;
        internal const int PrologString = 50;
        internal const int QMark = 51;

        internal const int terminalCount = 52;

        private static void FillTerminalTable()
        {
            terminalTable.Add(Undefined, "Undefined");
            terminalTable.Add(EndOfInput, "EndOfInput");
            terminalTable.Add(SkipTerminal, "SkipTerminal");
            terminalTable.Add(ANYSYM, "ANYSYM");
            terminalTable.Add(B__T, "B__T");
            terminalTable.Add(Comma, "Comma", ",");
            terminalTable.Add(LeftParen, "LeftParen", "(");
            terminalTable.Add(RightParen, "RightParen", ")");
            terminalTable.Add(Identifier, "Identifier");
            terminalTable.Add(IntLiteral, "IntLiteral");
            terminalTable.Add(ppDefine, "ppDefine", "#define");
            terminalTable.Add(ppUndefine, "ppUndefine", "#undefine");
            terminalTable.Add(ppIf, "ppIf", "#if");
            terminalTable.Add(ppIfNot, "ppIfNot", "#ifnot");
            terminalTable.Add(ppElse, "ppElse", "#else");
            terminalTable.Add(ppEndIf, "ppEndIf", "#endif");
            terminalTable.Add(RealLiteral, "RealLiteral");
            terminalTable.Add(StringLiteral, "StringLiteral");
            terminalTable.Add(CharLiteral, "CharLiteral");
            terminalTable.Add(Operator, "Operator");
            terminalTable.Add(Atom, "Atom");
            terminalTable.Add(EndOfLine, "EndOfLine");
            terminalTable.Add(CommentStart, "CommentStart", "/*");
            terminalTable.Add(CommentSingle, "CommentSingle", "%");
            terminalTable.Add(Dot, "Dot", ".");
            terminalTable.Add(DoubleQuote, "DoubleQuote", @"""");
            terminalTable.Add(Anonymous, "Anonymous", "_");
            terminalTable.Add(CutSym, "CutSym", "!");
            terminalTable.Add(ImpliesSym, "ImpliesSym", ":-");
            terminalTable.Add(PromptSym, "PromptSym", "?-");
            terminalTable.Add(DCGArrowSym, "DCGArrowSym", "-->");
            terminalTable.Add(BuiltinAssignSym, "BuiltinAssignSym", ":==");
            terminalTable.Add(LSqBracket, "LSqBracket", "[");
            terminalTable.Add(RSqBracket, "RSqBracket", "]");
            terminalTable.Add(LCuBracket, "LCuBracket", "{");
            terminalTable.Add(RCuBracket, "RCuBracket", "}");
            terminalTable.Add(VBar, "VBar", "|");
            terminalTable.Add(OpSym, "OpSym", "op");
            terminalTable.Add(BuiltinSym, "BuiltinSym", "&builtin");
            terminalTable.Add(QuerySym, "QuerySym", "&query");
            terminalTable.Add(ProgramSym, "ProgramSym", "&program");
            terminalTable.Add(ReadingSym, "ReadingSym", "&reading");
            terminalTable.Add(EnsureLoaded, "EnsureLoaded", "ensure_loaded");
            terminalTable.Add(Discontiguous, "Discontiguous", "discontiguous");
            terminalTable.Add(AllDiscontiguous, "AllDiscontiguous", "alldiscontiguous");
            terminalTable.Add(Dynamic, "Dynamic", "dynamic");
            terminalTable.Add(Persistent, "Persistent", "persistent");
            terminalTable.Add(Unpersistent, "Unpersistent", "unpersistent");
            terminalTable.Add(Module, "Module", "module");
            terminalTable.Add(UndefPredAction, "UndefPredAction", "undef_pred_action");
            terminalTable.Add(PrologString, "PrologString");
            terminalTable.Add(QMark, "QMark", "??");
        }

        private CharSet ignoreChars = new CharSet(
          9, 13, 32);

        private CharSet initIdChars = new CharSet(
          65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 95, 97, 98, 99,
          100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 138,
          154, 192, 193, 194, 195, 196, 197, 198, 199, 200, 201, 202, 203, 204, 205, 206, 207, 209, 210, 211, 212, 213, 214, 216,
          217, 218, 219, 220, 221, 224, 225, 226, 227, 228, 229, 230, 231, 232, 233, 234, 235, 236, 237, 238, 239, 241, 242, 243,
          244, 245, 246, 248, 249, 250, 251, 252, 253);

        private CharSet idChars = new CharSet(
          48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84,
          85, 86, 87, 88, 89, 90, 95, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115,
          116, 117, 118, 119, 120, 121, 122, 138, 154, 192, 193, 194, 195, 196, 197, 198, 199, 200, 201, 202, 203, 204, 205, 206,
          207, 209, 210, 211, 212, 213, 214, 216, 217, 218, 219, 220, 221, 224, 225, 226, 227, 228, 229, 230, 231, 232, 233, 234,
          235, 236, 237, 238, 239, 241, 242, 243, 244, 245, 246, 248, 249, 250, 251, 252, 253);

        private CharSet atomCharsEx = new CharSet(
          35, 36, 38, 42, 43, 45, 46, 47, 58, 60, 61, 62, 63, 64, 92, 94, 96, 126);

        private static Trie terminalTable = new Trie(true);
        private char ch;
        private char prevCh;
        private bool endText;
        public event SymbolScanned SymbolScannedHook;
        public event SymbolProcessed SymbolProcessedHook;
        public event ProcessComment ProcessCommentHook;
        public event BlankLine BlankLineHook;
        public event EvaluateString EvaluateStringHook;
        public event AcceptIdentifier AcceptIdentifierHook;
        private Symbol symbol;
        private int maxRuntime = 0; // maximum runtime in miliseconds; 0 means unlimited
        public int MaxRuntime { get { return maxRuntime; } set { maxRuntime = value; } }
        private int actRuntime;     // actual runtime for Parse () in miliseconds
        public int ActRuntime { get { return actRuntime; } }
        private Buffer inStream = null;
        private Buffer outStream = null; // standard output
        private string streamInPrefix;
        public string Prefix
        {
            get
            {
                return streamInPrefix;
            }
            set
            {
                streamInPrefix = value;
                streamInPreLen = streamInPrefix.Length;
            }
        }
        private int streamInPreLen; // length of streamInPrefix
        private char strStartChar;
        private char strStartCharAlt;
        private bool syntaxErrorStat;
        private string errorMessage;
        internal int streamInLen;
        private StreamPointer streamInPtr;
        private bool stringMode; // iff a string is being read (e.g. ignore comments)
        private bool terminalAsId;
        internal bool parseAnyText;
        private int prevTerminal;
        private int anyTextStart;
        private int anyTextFinal;
        private int anyTextFPlus;
        private int clipBegin;
        public int ClipBegin { get { return clipBegin; } }
        private int clipEnd;
        public int ClipEnd { get { return clipEnd; } }
        private bool seeEndOfLine;
        private bool SeeEndOfLine { set { seeEndOfLine = value; } get { return seeEndOfLine; } }
        private bool formatMode = false;
        public bool FormatMode { get { return formatMode; } set { formatMode = value; } }
        private bool tokenize;

        /*
            Preprocessor symbols are retained in nested parser calls, i.e. the symbols
            defined in the outer call are visible in the inner call. At the end of the
            inner parse, the state is reset to the one at the start of the inner parse.
        */
        private static Hashtable ppSymbols = new Hashtable();
        public static Hashtable PpSymbols { get { return ppSymbols; } }
        private Hashtable ppSymSnapShot; // for saving/restoring the initial state
        private Stack ppXeqStack = new Stack();
        private Stack ppElseOK = new Stack();
        private bool ppProcessSource;
        private bool ppDefineSymbol;
        private bool ppUndefineSymbol;
        private bool ppDoIfSymbol;
        private bool ppDoIfNotSymbol;
        private int eoLineCount;
        private int lineCount = 0;
        public int LineCount { get { return lineCount; } }
        public TrieArrayList TerminalList
        {
            get { return terminalTable.ToArrayList(); }
        }
        protected static string logStreamName = "ParseLog.txt"; // wijzigbaar maken

        public string LogFileName
        {
            set { logStreamName = value; }
            get { return logStreamName; }
        }

        private bool tracingUsed = false;
        public bool TracingUsed { get { return tracingUsed; } }

        private bool showErrTrace = true;
        public bool ShowErrTrace { get { return showErrTrace; } set { showErrTrace = value; } }


        public string SyntaxError
        {
            set
            {
                if (parseAnyText) return;
                errorMessage = symbol.Context + value + Environment.NewLine;
                syntaxErrorStat = true;
                throw new SyntaxException(errorMessage);
            }
            get
            {
                return errorMessage;
            }
        }


        public string ErrorMessage
        {
            set
            {
                if (parseAnyText) return;

                throw new SyntaxException(String.Format("{0}{1}{2}", symbol.Context, value, Environment.NewLine));
            }
            get
            {
                return errorMessage;
            }
        }


        public void Error(string msg)
        {
            if (parseAnyText) return;

            throw new Exception(String.Format("{0}{1}{2}", symbol.Context, msg, Environment.NewLine));
        }


        public void Error(string msg, params Object[] o)
        {
            Error(String.Format(msg, o));
        }


        public string StreamIn
        {
            get
            {
                return inStream.ToString();
            }
            set
            {
                inStream = new StringReadBuffer(value);
                streamInLen = inStream.Length;
                Parse();
            }
        }


        public string StreamInName
        {
            get
            {
                return inStream.Name;
            }
        }


        public bool StreamInChar(int i, out char c)
        {
            try
            {
                GetStreamInChar(i, out c);
                return true;
            }
            catch
            {
                c = '\0';
                return false;
            }
        }


        public Buffer StreamOut
        {
            get
            {
                return outStream;
            }
            set
            {
                if (!(value is FileWriteBuffer || value is StringWriteBuffer))
                    throw new ParserException("*** StreamOut buffer type must be of type FileWriteBuffer or StringWriteBuffer");

                outStream = value;
            }
        }


        public void LoadFromFile(string fileName)
        {
            try
            {
                inStream = new FileReadBuffer(fileName);
                streamInLen = inStream.Length;
            }
            catch
            {
                Prefix = "";
                throw new Exception("*** Unable to read file \"" + fileName + "\"");
            }

            Parse();
        }

        public string StreamInClip(int n, int m)
        {
            return GetStreamInString(n, m);
        }

        public string StreamInClip()
        {
            return GetStreamInString(clipBegin, clipEnd);
        }

        public bool AtEndOfInput
        {
            get { return (inStream == null); }
        }
        #endregion // Public fields and properties

        #region Private structs and classes

        #region CharSet
        private class CharSet
        {
            private bool[] b = new bool[255];

            public CharSet(params byte[] a)
            {
                int i;
                for (i = 0; i < 255; b[i++] = false) ;
                for (i = 0; i < a.Length; b[a[i++]] = true) ;
            }


            public bool Contains(char c)
            {
                return b[(byte)c];
            }


            public void Add(string s)
            {
                foreach (char c in s.ToCharArray())
                {
                    b[(byte)c] = true;
                }
            }


            public void Remove(string s)
            {
                foreach (char c in s.ToCharArray())
                {
                    b[(byte)c] = false;
                }
            }
        }
        #endregion

        #region StreamPointer
        private struct StreamPointer
        {
            public int Position;
            public int LineNo;
            public int LineStart;
            public bool EndLine;
            public bool FOnLine;
        }
        #endregion

        #region positionMarker
        private struct positionMarker
        {
            public StreamPointer Pointer;
            public int Terminal;
            public object OValue;
            public int Start;
            public int Final;
            public int FinalPlus;
            public int PrevFinal;
            public int LineNo;
            public int AbsSeqNo;
            public int RelSeqNo;
            public int LineStart;
            public bool HasIdFormat;
            public bool Processed;
            public bool IsSet; // initialized to false
        }

        private positionMarker[] markerArray = new positionMarker[4];
        #endregion


        #endregion // Private structs and classes

        #region Parser constructor / destructor
        public Parser(params object[] arg)
        {
            parser_arg = arg;
            symbol = new Symbol(this);
            strStartChar = '\u0027';
            strStartCharAlt = '\u0022';
            streamInPrefix = "";
            streamInPreLen = 0;
        }


        static Parser()
        {
            FillTerminalTable();
            terminalTable.TrimToSize();
        }


        ~Parser()
        {
        }
        #endregion

        #region Miscellaneous methods

        public void ClipStart()
        {
            clipBegin = symbol.Start;
        }


        public void ClipStart(int clipBegin)
        {
            clipBegin = symbol.Start;
        }


        public void ClipFinal()
        {
            clipEnd = symbol.Start;
        }


        public void ClipFinal(int clipBegin)
        {
            clipEnd = symbol.Start;
        }


        public void ClipFinalPlus() // includes current symbol
        {
            clipEnd = symbol.Final;
        }


        public void ClipFinalPlus(int clipEnd)
        {
            clipEnd = symbol.Final;
        }


        public void ClipFinalTrim()
        {
            clipEnd = symbol.PrevFinal;
        }


        public void ClipFinalTrim(int clipEnd)
        {
            clipEnd = symbol.PrevFinal;
        }
        #endregion Miscellaneous methods

        #region Lexical scanner
        internal void GetStreamInChar(int n, out char c)
        {
            if (n < streamInPreLen)
                c = streamInPrefix[n];
            else
            {
                n -= streamInPreLen;
                c = inStream[n];
            }
        }


        private string GetStreamInString(int n, int m)
        {
            string p;

            if (n < streamInPreLen) // start in Prefix
            {
                if (m < streamInPreLen) // end in Prefix
                {
                    return streamInPrefix.Substring(n, m - n);
                }
                else // end beyond Prefix
                {
                    p = streamInPrefix.Substring(n, streamInPreLen - n - 1); // part in Prefix
                    // Overlap. Number of chars taken from prefix is streamInPreLen - n,
                    // so decrease final position m accordingly. Set n to 0.
                    n = 0;
                    m -= streamInPreLen - n;
                }
            }
            else
            {
                n -= streamInPreLen;
                m -= streamInPreLen;
                p = "";
            }

            return p + inStream.Substring(n, m - n);
        }


        public int LineNo
        {
            get { return symbol.LineNo; }
        }


        public int ColNo
        {
            get { return symbol.ColNo; }
        }


        private void NextCh()
        {
            if (endText) return;

            if (streamInPtr.Position == streamInLen - 1)
            {
                endText = true;
                streamInPtr.EndLine = true;
                ch = '\0';
                streamInPtr.Position++;
            }
            else
            {
                if (streamInPtr.EndLine)
                {
                    streamInPtr.LineNo++;
                    streamInPtr.LineStart = streamInPtr.Position + 1;
                    symbol.RelSeqNo = 0;
                    streamInPtr.EndLine = false;
                    streamInPtr.FOnLine = true;
                }

                prevCh = ch;
                streamInPtr.Position++;
                GetStreamInChar(streamInPtr.Position, out ch);
                streamInPtr.EndLine = (ch == '\n');
            }
        }


        private void InitCh(StreamPointer c)
        {
            streamInPtr = c;
            GetStreamInChar(streamInPtr.Position, out ch);
            if (streamInPtr.Position <= 0)
                prevCh = ' ';
            else
                GetStreamInChar(streamInPtr.Position - 1, out prevCh);
            endText = (streamInPtr.Position > streamInLen - 1);
            if (endText) ch = '\x0';
        }


        private void SkipOverChars(string p)
        {
            if (p.Length == 0) return;

            do
            {
                if (ch == p[0])
                {
                    int i = 0;

                    do
                    {
                        NextCh();
                        if (++i == p.Length) return;
                    } while (ch == p[i]);
                }
                else
                    NextCh();
            } while (!endText);
        }


        private void DoComment(string p, bool multiLine, bool firstOnLine)
        {
            SkipOverChars(p);
            symbol.Final = streamInPtr.Position;

            // firstOnLine indicates whether a comment is the first 'symbol' on the line
            if (!parseAnyText && formatMode && ProcessCommentHook != null && !tokenize)
                ProcessCommentHook(multiLine, firstOnLine);
        }



        private void ScanIdOrTerminal()
        {
            TermDescr tRec;
            bool special = atomCharsEx.Contains(ch);
            bool firstLow = Char.IsLower(ch) || special;
            StreamPointer iPtr = streamInPtr;
            StreamPointer tPtr = iPtr;
            int fCnt; // count of calls to FindCharAndSubtree
            int tLen; // length of longest Terminal sofar
            int iLen; // length of longest Identifier sofar
            if (initIdChars.Contains(ch) || special) { iLen = 1; } else { iLen = 0; }
            tLen = 0;
            fCnt = 0;
            terminalTable.FindCharInSubtreeReset();
            while ((fCnt++ >= 0) && terminalTable.FindCharInSubtree(ch, out tRec))
            {
                if (tRec != null)
                {
                    symbol.Terminal = tRec.IVal;
                    symbol.OValue = tRec.OVal;
                    symbol.Type = tRec.Type;
                    tLen = fCnt;
                    tPtr = streamInPtr; // next char to be processed
                    if (terminalTable.AtLeaf) break; // terminal cannot be extended
                }
                NextCh();
                if (!special && iLen == fCnt && idChars.Contains(ch))
                {
                    iLen++;
                    iPtr = streamInPtr;
                }
            } // fCnt++ by last (i.e. failing) call
            if (iLen == fCnt && (!special || tLen == 0))
            {
                while (true)
                {
                    NextCh();
                    if (special ? atomCharsEx.Contains(ch) : idChars.Contains(ch))
                    {
                        iLen++;
                        iPtr = streamInPtr;
                    }
                    else
                        break;
                }
            }
            if (iLen > tLen)
            {
                if (firstLow)
                    symbol.Terminal = Atom;
                else
                    symbol.Terminal = Identifier;

                symbol.HasIdFormat = true;
                InitCh(iPtr);
            }
            else if (symbol.Terminal == Undefined)
                InitCh(iPtr);
            else // we have a terminal != Identifier
            {
                symbol.HasIdFormat = (iLen == tLen);
                InitCh(tPtr);
            }
            NextCh();
        }


        private void ScanNumber()
        {
            bool isReal;
            StreamPointer savPosition;

            do NextCh(); while (Char.IsDigit(ch));
            symbol.Terminal = IntLiteral;
            isReal = true; // assumed until proven conversily
            if (ch == '.') // fractional part?
            {
                savPosition = streamInPtr;
                NextCh();
                if (Char.IsDigit(ch))
                {
                    symbol.Terminal = RealLiteral;
                    do NextCh(); while (Char.IsDigit(ch));
                }
                else // not a digit after period
                {
                    InitCh(savPosition); // 'unread' dot
                    isReal = false;       // ... and remember this
                }
            }
            if (isReal) // integer or real, possibly with scale factor
            {
                savPosition = streamInPtr;
                if (ch == 'e' || ch == 'E' || ch == 'd' || ch == 'D')
                { // scale factor
                    NextCh();
                    if (ch == '+' || ch == '-') NextCh();
                    if (Char.IsDigit(ch))
                    {
                        do NextCh(); while (Char.IsDigit(ch));
                        symbol.Terminal = RealLiteral;
                    }
                    else if (!stringMode) // error in real syntax
                        InitCh(savPosition);
                }
            }
            symbol.Final = streamInPtr.Position;
        }


        private bool ScanFraction()
        {
            StreamPointer savPosition = streamInPtr; // Position of '.'

            do NextCh(); while (Char.IsDigit(ch));

            bool result = (streamInPtr.Position > savPosition.Position + 1); // a fraction

            if (result)
                symbol.Terminal = RealLiteral;
            else
                InitCh(savPosition);
            return result;
        }

        private void ScanString(char stringEnd)
        {
            do
            {
                NextCh();
                if (!streamInPtr.EndLine)
                {
                    if (ch == stringEnd)
                    {
                        NextCh();
                        if (streamInPtr.EndLine || ch != stringEnd) symbol.Terminal = StringLiteral;
                    }
                }
            } while (!(streamInPtr.EndLine) && symbol.Terminal != StringLiteral);
            symbol.Final = streamInPtr.Position;

            if (streamInPtr.EndLine && symbol.Terminal != StringLiteral && !tokenize)
                SyntaxError = "Unterminated string: " + symbol.ToString();
        }



        #region NextSymbol, GetSymbol
        private void NextSymbol()
        {
            NextSymbol("");
        }


        private void NextSymbol(string _Proc)
        {
            if (symbol.AbsSeqNo != 0 && streamInPtr.FOnLine) streamInPtr.FOnLine = false;
            symbol.PrevFinal = symbol.Final;
            if (symbol.Terminal == EndOfInput)
                SyntaxError = "*** Trying to read beyond end of input";
            prevTerminal = symbol.Terminal;
            symbol.HasIdFormat = false;
            symbol.OValue = null;
            bool Break = false;

            do
            {
                while (ignoreChars.Contains(ch)) NextCh();

                symbol.Start = streamInPtr.Position;
                symbol.LineNo = streamInPtr.LineNo;
                symbol.LineStart = streamInPtr.LineStart;
                symbol.Terminal = Undefined;

                if (endText)
                    symbol.Terminal = EndOfInput;
                else if (streamInPtr.EndLine)
                    symbol.Terminal = EndOfLine;
                else if (Char.IsDigit(ch))
                    ScanNumber();
                else if (ch == strStartChar || ch == strStartCharAlt)
                    ScanString(ch);
                else if (ch == '.')
                {
                    if (!ScanFraction()) ScanIdOrTerminal();
                }
                else
                    ScanIdOrTerminal();

                symbol.Final = streamInPtr.Position;
                symbol.FinalPlus = symbol.Final;

                if (SymbolScannedHook != null && !tokenize)
                    SymbolScannedHook(ref symbol.Terminal, prevTerminal);

                if (symbol.Terminal == EndOfLine)
                {
                    eoLineCount++;
                    NextCh();

                    // not sure whether the next line is always at the right place (cf. SetProcessed)
                    if (SymbolProcessedHook != null && formatMode && !tokenize)
                        SymbolProcessedHook(EndOfLine, prevTerminal);

                    Break = seeEndOfLine;
                }
                else
                {
                    if (eoLineCount > 1 && BlankLineHook != null && formatMode && !tokenize)
                        BlankLineHook(eoLineCount - 1);

                    eoLineCount = 0;

                    switch (symbol.Terminal)
                    {
                        case ppDefine:
                            CheckPpIllegalSymbol();
                            ppDefineSymbol = true;
                            break;
                        case ppUndefine:
                            CheckPpIllegalSymbol();
                            ppUndefineSymbol = true;
                            break;
                        case ppIf:
                            CheckPpIllegalSymbol();
                            ppDoIfSymbol = true;
                            ppElseOK.Push(true); // block is open
                            break;
                        case ppIfNot:
                            CheckPpIllegalSymbol();
                            ppDoIfNotSymbol = true;
                            ppElseOK.Push(true); // block is open
                            break;
                        case ppElse:
                            CheckPpIllegalSymbol();
                            if (!(bool)ppElseOK.Pop()) Error("Unexpected #else");
                            ppElseOK.Push(false); // no else allowed after an else
                            ppXeqStack.Pop(); // remove the current value of ppProcessSource (pushed by the if-branch)
                            // if the if-branch was executed, then this branch should not be
                            if (ppProcessSource) // ... it was executed
                                ppProcessSource = !ppProcessSource;
                            else // ... it was not. But execute this branch only if the outer scope value of ppProcessSource is true
                                if ((bool)ppXeqStack.Peek()) ppProcessSource = true;
                            ppXeqStack.Push(ppProcessSource); // push the new value for this scope
                            break;
                        case ppEndIf:
                            if (ppElseOK.Count == 0) Error("Unexpected #endif");
                            ppElseOK.Pop(); // go to outer scope
                            ppXeqStack.Pop();
                            ppProcessSource = (bool)ppXeqStack.Peek();
                            break;
                        case Identifier:
                        case Atom:
                            if (ppProcessSource && ppDefineSymbol)
                            {
                                ppSymbols[symbol.ToString().ToLower()] = true; // any non-null value will do
                                ppDefineSymbol = false;
                            }
                            else if (ppProcessSource && ppUndefineSymbol)
                            {
                                ppSymbols.Remove(symbol.ToString().ToLower());
                                ppUndefineSymbol = false;
                            }
                            else if (ppDoIfSymbol) // identifier following #if
                            {
                                // do not alter ppProcessSource here if the outer scope value of ppProcessSource is false
                                if (ppProcessSource && (bool)ppXeqStack.Peek()) // ... value is true
                                    if (ppSymbols[symbol.ToString().ToLower()] == null)
                                        ppProcessSource = false; // set to false if symbol is not defined

                                ppXeqStack.Push(ppProcessSource);
                                ppDoIfSymbol = false;
                            }
                            else if (ppDoIfNotSymbol) // identifier following #ifnot
                            {
                                // do not alter ppProcessSource here if the outer scope value of ppProcessSource is false
                                if (ppProcessSource && (bool)ppXeqStack.Peek()) // ... value is true
                                    if (ppSymbols[symbol.ToString().ToLower()] != null)
                                        ppProcessSource = false; // set to false if symbol is defined

                                ppXeqStack.Push(ppProcessSource);
                                ppDoIfNotSymbol = false;
                            }
                            else
                                Break = true; // 'regular' identifier
                            break;
                        case EndOfInput:
                            Break = true;
                            ppProcessSource = true; // force while-loop termination
                            break;
                        case CommentStart:
                            if (stringMode) Break = true;
                            DoComment("*/", true, streamInPtr.FOnLine);
                            if (tokenize) Break = true;
                            if (endText && !tokenize) ErrorMessage = "Unterminated comment starting at line " + symbol.LineNo.ToString();
                            break;
                        case CommentSingle:
                            if (stringMode) Break = true; else Break = false;
                            DoComment("\n", false, streamInPtr.FOnLine); // \r or \r\n does not work in a RichTextBox
                            if (tokenize) Break = true;
                            eoLineCount = 1;
                            if (seeEndOfLine)
                            {
                                symbol.Terminal = EndOfLine;
                                Break = true;
                            }
                            break;
                        default:
                            if (seeEndOfLine && symbol.Terminal != EndOfLine) streamInPtr.FOnLine = false;
                            Break = true;
                            break;
                    }
                    symbol.Final = streamInPtr.Position;
                }
            } while (!Break || !ppProcessSource);

            symbol.AbsSeqNo++;
            symbol.RelSeqNo++;
#if debug
      Console.WriteLine ("NextSymbol[{0,3:G}] line {1,3:G}: {2,-20:G} [{3}]",
                         symbol.AbsSeqNo, symbol.LineNo, "'" + symbol.ToString() + "'", symbol.ToName ());
#endif
        }

        private void CheckPpIllegalSymbol()
        {
            int prevTerminal = -1;
            string currSym = symbol.ToString();

            if (ppDefineSymbol)
                prevTerminal = ppDefine;
            else if (ppUndefineSymbol)
                prevTerminal = ppUndefine;
            else if (ppDoIfSymbol)
                prevTerminal = ppIf;
            else if (ppDoIfNotSymbol)
                prevTerminal = ppIfNot;

            if (prevTerminal != -1)
                Error(String.Format("Illegal symbol {0} after {1} (identifier expected)",
                                      currSym, terminalTable.TerminalImage(prevTerminal)));
        }

        private bool GetSymbol(TerminalSet followers, bool done, bool genXCPN)
        {
            string s;

            if (symbol.IsProcessed) NextSymbol();
            symbol.SetProcessed(done);
            if (parseAnyText || followers.IsEmpty()) return true;

            if (syntaxErrorStat) return false;
            if (symbol.Terminal == ANYSYM || followers.Contains(symbol.Terminal))
                return true;
            else
            {
                if (terminalAsId && symbol.Terminal != Identifier)
                {
                    int t = symbol.Terminal;
                    symbol.Terminal = Identifier;
                    if (symbol.HasIdFormat && followers.Contains(symbol.Terminal))
                    {
                        return true;
                    }
                    symbol.Terminal = t; // restore error value
                }

                switch (symbol.Terminal)
                {
                    case EndOfLine:
                        if (seeEndOfLine) s = "<EndOfLine>"; else goto default;
                        s = "<EndOfLine>";
                        break;
                    case EndOfInput:
                        s = "<EndOfInput>";
                        break;
                    default:
                        s = String.Format("\"{0}\"", symbol.ToString());
                        break;
                }
            }
            s = String.Format("*** Unexpected symbol: {0}{1}*** Expected one of: {2}", s,
                               Environment.NewLine, terminalTable.TerminalImageSet(followers));

            if (!PrologIO.WarnOrError(s))
                if (genXCPN)
                    SyntaxError = s;
                else
                    errorMessage = s;

            return true;
        }


        public StringDictionary IdentifierSet(string source)
        {
            string s = null;
            inStream = new StringReadBuffer(source);
            StringDictionary result = new StringDictionary();
            tokenize = true;
            InitParse();
            NextSymbol();

            while (symbol.Terminal != EndOfInput)
            {
                if (symbol.Terminal == Identifier && AcceptIdentifierHook != null && AcceptIdentifierHook())
                    result[(s = symbol.ToString()).ToUpper()] = s;

                NextSymbol();
            }

            return result;
        }


        private void EOL(TerminalSet followers, bool GenXCPN)
        {
            GetSymbol(new TerminalSet(EndOfLine), true, GenXCPN);
            while (symbol.Terminal == EndOfLine)
            {
                NextSymbol();
            }
            symbol.SetProcessed(false);
        }

        private void ANYTEXT(TerminalSet followers)
        {
            StreamPointer anyStart;
            StreamPointer follower;

            if (symbol.IsProcessed)
                anyTextStart = streamInPtr.Position;
            else
                anyTextStart = symbol.Start;

            if (followers.Contains(symbol.Terminal) && !symbol.IsProcessed)
            {
                anyTextFinal = anyTextStart; // empty, nullstring
                anyTextFPlus = anyTextStart;
                return;
            }
            parseAnyText = true;
            anyStart = streamInPtr;
            do
            {
                follower = streamInPtr;
                NextSymbol();
                symbol.FinalPlus = symbol.Start; // up to next symbol
                anyTextFPlus = symbol.Start;
            }
            while (symbol.Terminal != EndOfInput && !followers.Contains(symbol.Terminal));
            InitCh(follower);
            symbol.Start = anyTextStart;
            symbol.LineNo = anyStart.LineNo;
            symbol.LineStart = anyStart.LineStart;
            symbol.Final = streamInPtr.Position;
            symbol.Terminal = Undefined;
            symbol.SetProcessed(true);
            anyTextFinal = streamInPtr.Position;
            parseAnyText = false;
        }


        private void InitPositionMarkers(positionMarker[] ma)
        {
            for (int i = 0; i < 4; i++) ma[i].Start = UNDEF;
        }


        private void InputStreamMark(out positionMarker m)
        {
            m.Pointer = streamInPtr;
            m.Terminal = symbol.Terminal;
            m.OValue = symbol.OValue;
            m.Start = symbol.Start;
            m.Final = symbol.Final;
            m.FinalPlus = symbol.FinalPlus;
            m.PrevFinal = symbol.PrevFinal;
            m.LineNo = symbol.LineNo;
            m.AbsSeqNo = symbol.AbsSeqNo;
            m.RelSeqNo = symbol.RelSeqNo;
            m.LineStart = symbol.LineStart;
            m.HasIdFormat = symbol.HasIdFormat;
            m.Processed = symbol.IsProcessed;
            m.IsSet = true;
        }


        private void InputStreamRedo(positionMarker m, int n)
        {
            if (!m.IsSet)
                throw new Exception("REDO Error: positionMarker " + n.ToString() + " is not set");
            InitCh(m.Pointer);
            symbol.Terminal = m.Terminal;
            symbol.OValue = m.OValue;
            symbol.Start = m.Start;
            symbol.Final = m.Final;
            symbol.FinalPlus = m.FinalPlus;
            symbol.PrevFinal = m.PrevFinal;
            symbol.LineNo = m.LineNo;
            symbol.AbsSeqNo = m.AbsSeqNo;
            symbol.RelSeqNo = m.RelSeqNo;
            symbol.LineStart = m.LineStart;
            symbol.HasIdFormat = m.HasIdFormat;
            symbol.SetProcessed(m.Processed);
        }
        #endregion NextSymbol, GetSymbol

        #endregion

        #region Parse ()
        private void InitParse()
        {
            terminalAsId = false;
            stringMode = false;
            parseAnyText = false;
            anyTextStart = 0;
            anyTextFinal = 0;
            anyTextFPlus = 0;
            seeEndOfLine = false;
            eoLineCount = 1;
            ppSymSnapShot = (Hashtable)ppSymbols.Clone(); // save the entry state
            ppProcessSource = true;
            ppDefineSymbol = false;
            ppUndefineSymbol = false;
            ppDoIfSymbol = false;
            ppDoIfNotSymbol = false;
            ppXeqStack.Clear();
            ppXeqStack.Push(ppProcessSource);
            ppElseOK.Clear();
            symbol.SetProcessed(true);
            symbol.AbsSeqNo = 0;
            symbol.RelSeqNo = 0;
            symbol.Terminal = Undefined;
            streamInPtr.Position = UNDEF;
            streamInPtr.LineNo = 0;
            streamInPtr.LineStart = UNDEF;
            streamInPtr.EndLine = true;
            streamInPtr.FOnLine = true;
            errorMessage = null;
            syntaxErrorStat = false;
            endText = false;
            prevCh = ' ';
            streamInLen += streamInPreLen;
            inStream.UpdateCache(0);
            NextCh();

            SymbolScannedHook += new SymbolScanned(UserSymbolScanned);
        }

        // spawn a separate thread, in order to control maximum execution time
        private bool parseCompleted;
        private ManualResetEvent sema;

        private void Parse()
        {
            TimeSpan startProcTime = Process.GetCurrentProcess().TotalProcessorTime;

            if (maxRuntime == 0) ParseEx(); else StartParseThread();

            actRuntime = (Process.GetCurrentProcess().TotalProcessorTime - startProcTime).Milliseconds;
        }


        private void StartParseThread()
        {
            ThreadStart startParse = new ThreadStart(RunParse);
            Thread run = new Thread(startParse);
            run.Name = "Parse";
            run.IsBackground = true;
            sema = new ManualResetEvent(false);
            parseCompleted = false;
            run.Start(); // run will fall through to WaitOne
            sema.WaitOne(maxRuntime, false); // wait for timeOutMSecs (while the RunParse thread runs)

            if (!parseCompleted) // parseCompleted is set by RunParse()
            {
                run.Abort();

                Error("Parsing time exceed maximum value of {0:N} milliseconds", maxRuntime);
            }
        }


        private void RunParse()
        {
            try
            {
                ParseEx();
                parseCompleted = true;
            }
            catch (ThreadAbortException) // time-out
            {
                return;
            }
            catch // any other exception
            {
                parseCompleted = true;
                throw;
            }
            finally
            {
                sema.Set();
            }
        }


        private void ParseEx()
        {
            InitParse();
            tokenize = false;

            try
            {
                PrologCode(new TerminalSet(EndOfInput));
                lineCount = LineNo;
                if (symbol.IsProcessed) NextSymbol();
                if (symbol.Terminal != EndOfInput)
                    throw new Exception(String.Format("Spurious symbol {0} after end of input", symbol.ToString()));
                if (ppElseOK.Count > 0) Error("#endif expected");
            }
            catch (ParserException e)
            {
                throw new Exception(e.Message);
            }
            catch (SyntaxException)
            {
                throw new Exception(errorMessage);
            }
            catch (Exception e) // other errors
            {
                errorMessage = String.Format("*** Line {0}: {1}{2}", LineNo, e.Message,
                                              showErrTrace ? Environment.NewLine + e.StackTrace : null);
                throw;
            }
            finally
            {
                if (inStream != null) inStream.Close();
                ppSymbols = (Hashtable)ppSymSnapShot.Clone(); // restore the entry state
                inStream = null;
                Prefix = "";
                symbol.LineNo = UNDEF;
            }
        }
        #endregion

        public static void Show(string s, params object[] o)
        {
            Console.WriteLine(s, o);
        }

        #region User-supplied members
        private PredicateStorage ps;
        private TermNode queryNode = null;
        public TermNode QueryNode { get { return queryNode; } }

        public const string IMPLIES = ":-";
        public const string DCGIMPL = "-->";
        public const string ARROW = "->";
        public const string DOT = ".";
        public const string OP = "op";
        public const string ATOM = "atom";
        public const string CURL = "{}";
        public const string EQ = "=";
        public const string EOF = "end_of_file";
        public const string COMMA = ",";
        public const string SEMI = ";";

        private Term readTerm; // result of read (X)
        public Term ReadTerm { get { return readTerm; } }
        private bool inQueryMode;
        public bool InQueryMode { get { return inQueryMode; } }

        public enum ParseMode { Builtin, Program, Query, Reading }

        private ParseMode parseMode;

        protected void UserSymbolScanned(ref int terminal, int prevTerminal)
        {
            if (terminal == StringLiteral && symbol.ToString()[0] == '"') terminal = PrologString;
        }


        private void Initialize()
        {
            ps = (PredicateStorage)parser_arg[0];
            Add1000Operators();
            terminalTable.Remove("module");            // will be temporarily set after an initial ':-' only
            terminalTable.Remove("persistent");        // ...
            terminalTable.Remove("undef_pred_action"); // ...
        }


        private void Terminate()
        {
            inQueryMode = true;
        }


        #region Operator handling
        private static bool IsValidOpFormat(string a, bool genXcp) // checks an unquoted sequence
        {
            if (Char.IsLower(a[0]))
            {
                foreach (char c in a.ToCharArray())
                    if (!(Char.IsLetterOrDigit(c) || c == '_'))
                    {
                        if (genXcp)
                            PrologIO.Error(String.Format("Operator {0} contains illegal character {1}", a, c));
                        else
                            return false;
                    }
            }
            else
                foreach (char d in a.ToCharArray())
                    if (!(Globals.SpecialAtomChars.IndexOf(d) >= 0))
                    {
                        if (genXcp)
                            PrologIO.Error(String.Format("Operator {0} contains illegal character {1}", a, d));
                        else
                            return false;
                    }

            return true;
        }


        private static void AddPrologOperator(int prec, string type, string name, bool quoted)
        {
            if (prec < 0 || prec > 1200)
                PrologIO.Error(String.Format("Illegal precedence value {0} for operator {1}", prec, name));

            TermDescr td;

            if (terminalTable.Find(name, out td))
            {
                if (td.OVal is OperatorDescr)
                    ((OperatorDescr)td.OVal).Assign(prec, type, quoted);
                else
                {
                    td.IVal = Operator;
                    td.OVal = new OperatorDescr(prec, type, name, quoted);
                }
            }
            else // new operator
                terminalTable.Add(name, Operator, new OperatorDescr(prec, type, name, quoted));

            //IO.Message ("Operator defined: {0} ({1} {2})", name, prec, type);
        }


        private static void RemovePrologOperator(string type, string name)
        {
            TermDescr td;

            if (terminalTable.Find(name, out td))
            {
                if (td.OVal is OperatorDescr)
                    ((OperatorDescr)td.OVal).Assign(-1, type, true);
                else
                    PrologIO.Error(String.Format("Illegal attempt to remove operator {0}", name));

                PrologIO.Message("Operator deleted: {0} ({1} {2})", name, type);
            }
        }


        public static bool IsOperator(string key)
        {
            return (terminalTable[key] == Operator);
        }


        public static OperatorDescr GetOperatorDescr(string key)
        {
            TermDescr td;

            if (terminalTable.Find(key, out td))
                if (td.OVal is OperatorDescr) return (OperatorDescr)td.OVal;

            return null;
        }


        private static void Add1000Operators()
        {
            AddPrologOperator(1200, "xfx", IMPLIES, false);
            AddPrologOperator(1200, "fx", IMPLIES, false);
            AddPrologOperator(1200, "xfx", DCGIMPL, false);
            AddPrologOperator(1150, "xfy", ARROW, false);
            AddPrologOperator(1100, "xfy", SEMI, false);
            AddPrologOperator(1000, "xfy", COMMA, false);
            Set1000Operators(false);
        }

        private static bool is1000OperatorSetting;

        private static void Set1000Operators(bool mode)
        {
            if (mode) // parsed as Operator
            {
                terminalTable[IMPLIES] = Operator;
                terminalTable[DCGIMPL] = Operator;
            }
            else // parsed 'normally'
            {
                terminalTable[IMPLIES] = ImpliesSym;
                terminalTable[DCGIMPL] = DCGArrowSym;
                terminalTable[DOT] = Dot;
                terminalTable[OP] = OpSym;
            }
            is1000OperatorSetting = mode;
        }
        #endregion Operator handling
        #endregion User-supplied members

        #region PARSER PROCEDURES

        #region PrologCode
        private void PrologCode(TerminalSet _TS)
        {
            inQueryMode = false;
            try
            {
                SeeEndOfLine = false;
                GetSymbol(_TS.Union(LeftParen, Identifier, IntLiteral, RealLiteral, StringLiteral, Operator, Atom, Anonymous, CutSym,
                                      LSqBracket, LCuBracket, OpSym, BuiltinSym, ProgramSym, ReadingSym, PrologString, QMark), false,
                           true);
                if (symbol.IsMemberOf(LeftParen, Identifier, IntLiteral, RealLiteral, StringLiteral, Operator, Atom, Anonymous, CutSym,
                                       LSqBracket, LCuBracket, OpSym, BuiltinSym, ProgramSym, ReadingSym, PrologString, QMark))
                {
                    GetSymbol(new TerminalSet(LeftParen, Identifier, IntLiteral, RealLiteral, StringLiteral, Operator, Atom, Anonymous,
                                                CutSym, LSqBracket, LCuBracket, OpSym, BuiltinSym, ProgramSym, ReadingSym, PrologString,
                                                QMark), false, true);
                    if (symbol.Terminal == BuiltinSym)
                    {
                        symbol.SetProcessed();
                        parseMode = ParseMode.Builtin;
                        Initialize();
                        Predefineds(_TS);
                    }
                    else if (symbol.Terminal == ProgramSym)
                    {
                        symbol.SetProcessed();
                        parseMode = ParseMode.Program;
                        Initialize();
                        Program(_TS);
                    }
                    else if (symbol.Terminal == ReadingSym)
                    {
                        symbol.SetProcessed();
                        parseMode = ParseMode.Reading;
                        ClauseSequence(_TS);
                    }
                    else
                    {
                        Globals.EraseVariables();
                        parseMode = ParseMode.Query;
                        inQueryMode = true;
                        GetSymbol(new TerminalSet(LeftParen, Identifier, IntLiteral, RealLiteral, StringLiteral, Operator, Atom, Anonymous,
                                                    CutSym, LSqBracket, LCuBracket, OpSym, PrologString, QMark), false, true);
                        if (symbol.IsMemberOf(LeftParen, Identifier, IntLiteral, RealLiteral, StringLiteral, Operator, Atom, Anonymous,
                                               CutSym, LSqBracket, LCuBracket, PrologString, QMark))
                        {
                            terminalTable[DOT] = Dot;
                            Set1000Operators(true);
                            Query(new TerminalSet(Dot), out queryNode);
                        }
                        else
                        {
                            OperatorDefinition(new TerminalSet(Dot), true);
                            queryNode = null;
                        }
                        GetSymbol(new TerminalSet(Dot), true, true);
                    }
                }
            }
            finally
            {
                terminalTable[COMMA] = Operator;
                terminalTable[OP] = OpSym;
                terminalTable[DOT] = Dot;
                Terminate();
            }
        }
        #endregion

        #region Program
        private void Program(TerminalSet _TS)
        {
            do
            {
                ClauseNode(_TS.Union(LeftParen, Identifier, IntLiteral, RealLiteral, StringLiteral, Operator, Atom, Anonymous, CutSym,
                                       ImpliesSym, PromptSym, LSqBracket, LCuBracket, PrologString));
                GetSymbol(_TS.Union(LeftParen, Identifier, IntLiteral, RealLiteral, StringLiteral, Operator, Atom, Anonymous, CutSym,
                                      ImpliesSym, PromptSym, LSqBracket, LCuBracket, PrologString), false, true);
            } while (!(_TS.Contains(symbol.Terminal)));
        }
        #endregion

        #region ClauseSequence
        private void ClauseSequence(TerminalSet _TS)
        {
            Monitor.Enter(PrologEngine.TermMonitor);
            do
            {
                ClauseNode(_TS.Union(LeftParen, Identifier, IntLiteral, RealLiteral, StringLiteral, Operator, Atom, Anonymous, CutSym,
                                       ImpliesSym, PromptSym, LSqBracket, LCuBracket, PrologString));
                Monitor.Pulse(PrologEngine.TermMonitor);
                Monitor.Wait(PrologEngine.TermMonitor);
                GetSymbol(_TS.Union(LeftParen, Identifier, IntLiteral, RealLiteral, StringLiteral, Operator, Atom, Anonymous, CutSym,
                                      ImpliesSym, PromptSym, LSqBracket, LCuBracket, PrologString), false, true);
            } while (!(_TS.Contains(symbol.Terminal)));
            readTerm = new Term(EOF);
            Monitor.Pulse(PrologEngine.TermMonitor);
            Monitor.Exit(PrologEngine.TermMonitor);
        }
        #endregion

        #region ClauseNode
        private void ClauseNode(TerminalSet _TS)
        {
            Term head;
            TermNode body = null;
            ClauseNode c;
            if (parseMode != ParseMode.Reading) Globals.EraseVariables();
            GetSymbol(new TerminalSet(LeftParen, Identifier, IntLiteral, RealLiteral, StringLiteral, Operator, Atom, Anonymous,
                                        CutSym, ImpliesSym, PromptSym, LSqBracket, LCuBracket, PrologString), false, true);
            if (symbol.IsMemberOf(LeftParen, Identifier, IntLiteral, RealLiteral, StringLiteral, Operator, Atom, Anonymous, CutSym,
                                   LSqBracket, LCuBracket, PrologString))
            {
                PrologTerm(new TerminalSet(Dot, ImpliesSym, DCGArrowSym), out head);
                if (!head.IsGoal)
                    PrologIO.Error("Illegal predicate head: {0}", head.ToString());
                GetSymbol(new TerminalSet(Dot, ImpliesSym, DCGArrowSym), false, true);
                if (symbol.IsMemberOf(ImpliesSym, DCGArrowSym))
                {
                    GetSymbol(new TerminalSet(ImpliesSym, DCGArrowSym), false, true);
                    if (symbol.Terminal == ImpliesSym)
                    {
                        symbol.SetProcessed();
                        Query(new TerminalSet(Dot), out body);
                    }
                    else
                    {
                        symbol.SetProcessed();
                        Term t;
                        PrologTerm(new TerminalSet(Dot), out t);
                        body = t.ToDCG(ref head);
                    }
                }
                c = new ClauseNode(head, body);
                if (parseMode == ParseMode.Reading) readTerm = new Term(c).CleanUp(); else ps.AddClause(c);
            }
            else if (symbol.Terminal == PromptSym)
            {
                symbol.SetProcessed();
                bool m = inQueryMode;
                bool o = is1000OperatorSetting;
                int k = terminalTable[DOT];
                try
                {
                    parseMode = ParseMode.Query;
                    inQueryMode = true;
                    terminalTable[DOT] = Dot;
                    Set1000Operators(true);
                    Query(new TerminalSet(Dot), out queryNode);
                    PrologIO.Error("'?-' querymode in file not yet supported");
                }
                finally
                {
                    inQueryMode = m;
                    terminalTable[DOT] = k;
                    Set1000Operators(o);
                }
            }
            else
            {
                symbol.SetProcessed();
                terminalTable.Add(Persistent, "Persistent", "persistent");
                terminalTable.Add(Module, "Module", "module");
                terminalTable.Add(UndefPredAction, "UndefPredAction", "undef_pred_action");
                try
                {
                    GetSymbol(new TerminalSet(LSqBracket, OpSym, EnsureLoaded, Discontiguous, AllDiscontiguous, Dynamic, Persistent,
                                                Module, UndefPredAction), false, true);
                    if (symbol.Terminal == OpSym)
                    {
                        OperatorDefinition(new TerminalSet(Dot), true);
                    }
                    else if (symbol.Terminal == EnsureLoaded)
                    {
                        symbol.SetProcessed();
                        GetSymbol(new TerminalSet(LeftParen), true, true);
                        GetSymbol(new TerminalSet(StringLiteral, Atom), false, true);
                        if (symbol.Terminal == Atom)
                        {
                            symbol.SetProcessed();
                        }
                        else
                        {
                            symbol.SetProcessed();
                        }
                        string fileName = Utils.ExtendedFileName(symbol.ToString().ToLower(), ".pl");
                        if (Globals.ConsultedFiles[fileName] == null)
                        {
                            ps.Consult(fileName);
                            Globals.ConsultedFiles[fileName] = true;
                        }
                        GetSymbol(new TerminalSet(RightParen), true, true);
                    }
                    else if (symbol.Terminal == Discontiguous)
                    {
                        symbol.SetProcessed();
                        Term t;
                        PrologTerm(new TerminalSet(Dot), out t);
                        ps.SetDiscontiguous(t);
                    }
                    else if (symbol.Terminal == AllDiscontiguous)
                    {
                        symbol.SetProcessed();
                        ps.SetDiscontiguous(true);
                    }
                    else if (symbol.Terminal == UndefPredAction)
                    {
                        symbol.SetProcessed();
                        Term t;
                        PrologTerm(new TerminalSet(Dot), out t);
                        ps.SetUndefPredAction(t, true);
                    }
                    else if (symbol.Terminal == Dynamic)
                    {
                        symbol.SetProcessed();
                        Term t;
                        PrologTerm(new TerminalSet(Dot), out t);
                    }
                    else if (symbol.Terminal == Persistent)
                    {
                        PersistentDeclaration(new TerminalSet(Dot));
                    }
                    else if (symbol.Terminal == Module)
                    {
                        symbol.SetProcessed();
                        GetSymbol(new TerminalSet(LeftParen), true, true);
                        try
                        {
                            terminalTable[COMMA] = Comma;
                            GetSymbol(new TerminalSet(StringLiteral, Atom), false, true);
                            if (symbol.Terminal == Atom)
                            {
                                symbol.SetProcessed();
                            }
                            else
                            {
                                symbol.SetProcessed();
                            }
                            ps.SetModuleName(symbol.ToString());
                            GetSymbol(new TerminalSet(Comma), true, true);
                        }
                        finally
                        {
                            terminalTable[COMMA] = Operator;
                        }
                        Term t;
                        PrologTerm(new TerminalSet(RightParen), out t);
                        GetSymbol(new TerminalSet(RightParen), true, true);
                    }
                    else
                    {
                        symbol.SetProcessed();
                        int lines = 0;
                        int files = 0;
                        try
                        {
                            while (true)
                            {
                                GetSymbol(new TerminalSet(StringLiteral, Atom), false, true);
                                if (symbol.Terminal == Atom)
                                {
                                    symbol.SetProcessed();
                                }
                                else
                                {
                                    symbol.SetProcessed();
                                }
                                string fileName = Utils.FileNameFromSymbol(symbol.ToString(), ".pl");
                                terminalTable[COMMA] = Operator;
                                lines += ps.Consult(fileName);
                                files++;
                                terminalTable[COMMA] = Comma;
                                GetSymbol(new TerminalSet(Comma, RSqBracket), false, true);
                                if (symbol.Terminal == Comma)
                                {
                                    symbol.SetProcessed();
                                }
                                else
                                    break;
                            }
                            if (files > 1) PrologIO.Message("Grand total is {0} lines", lines);
                        }
                        finally
                        {
                            terminalTable[COMMA] = Operator;
                        }
                        GetSymbol(new TerminalSet(RSqBracket), true, true);
                    }
                }
                finally
                {
                    terminalTable.Remove("module");
                    terminalTable.Remove("persistent");
                    terminalTable.Remove("undef_pred_action");
                }
            }
            GetSymbol(new TerminalSet(Dot), true, true);
        }
        #endregion

        #region OperatorDefinition
        private void OperatorDefinition(TerminalSet _TS, bool user)
        {
            string name;
            string typeStr;
            bool quoted;
            try
            {
                terminalTable[COMMA] = Comma;
                GetSymbol(new TerminalSet(OpSym), true, true);
                GetSymbol(new TerminalSet(LeftParen), true, true);
                GetSymbol(new TerminalSet(IntLiteral), true, true);
                int prec = symbol.ToInt();
                GetSymbol(new TerminalSet(Comma), true, true);
                GetSymbol(new TerminalSet(Atom), true, true);
                typeStr = symbol.ToString();
                GetSymbol(new TerminalSet(Comma), true, true);
                GetSymbol(new TerminalSet() /*any symbol*/, false, true);
                if (symbol.Terminal == LSqBracket)
                {
                    symbol.SetProcessed();
                    while (true)
                    {
                        PotentialOpName(new TerminalSet(Comma, RSqBracket), out name, out quoted);
                        AddPrologOperator(prec, typeStr, name, quoted);
                        GetSymbol(new TerminalSet(Comma, RSqBracket), false, true);
                        if (symbol.Terminal == Comma)
                        {
                            symbol.SetProcessed();
                        }
                        else
                            break;
                    }
                    GetSymbol(new TerminalSet(RSqBracket), true, true);
                }
                else
                {
                    PotentialOpName(new TerminalSet(RightParen), out name, out quoted);
                    AddPrologOperator(prec, typeStr, name, quoted);
                }
                GetSymbol(new TerminalSet(RightParen), true, true);
            }
            finally
            {
                terminalTable[COMMA] = Operator;
            }
        }
        #endregion

        #region PersistentDeclaration
        private void PersistentDeclaration(TerminalSet _TS)
        {
            GetSymbol(new TerminalSet(Persistent), true, true);
            Term t;
            PrologTerm(_TS, out t);
            ps.SetPersistent(t);
        }
        #endregion

        #region UnpersistentDeclaration
        private void UnpersistentDeclaration(TerminalSet _TS)
        {
            GetSymbol(new TerminalSet(Unpersistent), true, true);
            Term t;
            PrologTerm(_TS, out t);
            ps.SetUnpersistent(t);
        }
        #endregion

        #region PotentialOpName
        private void PotentialOpName(TerminalSet _TS, out string name, out bool quoted)
        {
            GetSymbol(new TerminalSet() /*any symbol*/, false, true);
            if (symbol.Terminal == StringLiteral)
            {
                symbol.SetProcessed();
                name = Utils.MakeAtom(symbol.ToUnquoted());
                quoted = (name[0] == '\'');
            }
            else
            {
                ANYTEXT(_TS);
                IsValidOpFormat(name = symbol.ToString(), true);
                quoted = false;
            }
        }
        #endregion

        #region Predefineds
        private void Predefineds(TerminalSet _TS)
        {
            do
            {
                Predefined(_TS.Union(LeftParen, Identifier, IntLiteral, RealLiteral, StringLiteral, Operator, Atom, Anonymous, CutSym,
                                       ImpliesSym, LSqBracket, LCuBracket, PrologString));
                GetSymbol(_TS.Union(LeftParen, Identifier, IntLiteral, RealLiteral, StringLiteral, Operator, Atom, Anonymous, CutSym,
                                      ImpliesSym, LSqBracket, LCuBracket, PrologString), false, true);
            } while (!(_TS.Contains(symbol.Terminal)));
        }
        #endregion

        #region Predefined
        private void Predefined(TerminalSet _TS)
        {
            Term head;
            bool opt = true;
            TermNode body = null;
            Globals.EraseVariables();
            GetSymbol(new TerminalSet(LeftParen, Identifier, IntLiteral, RealLiteral, StringLiteral, Operator, Atom, Anonymous,
                                        CutSym, ImpliesSym, LSqBracket, LCuBracket, PrologString), false, true);
            if (symbol.Terminal == ImpliesSym)
            {
                symbol.SetProcessed();
                OperatorDefinition(new TerminalSet(Dot), false);
            }
            else
            {
                PrologTerm(new TerminalSet(Dot, ImpliesSym, DCGArrowSym, BuiltinAssignSym), out head);
                GetSymbol(new TerminalSet(Dot, ImpliesSym, DCGArrowSym, BuiltinAssignSym), false, true);
                if (symbol.IsMemberOf(ImpliesSym, DCGArrowSym, BuiltinAssignSym))
                {
                    GetSymbol(new TerminalSet(ImpliesSym, DCGArrowSym, BuiltinAssignSym), false, true);
                    if (symbol.Terminal == BuiltinAssignSym)
                    {
                        symbol.SetProcessed();
                        GetSymbol(new TerminalSet(Operator, Atom), false, true);
                        if (symbol.Terminal == Atom)
                        {
                            symbol.SetProcessed();
                        }
                        else
                        {
                            symbol.SetProcessed();
                        }
                        ps.AddPredefined(new ClauseNode(head, new TermNode(symbol.ToString())));
                        opt = false;
                    }
                    else if (symbol.Terminal == ImpliesSym)
                    {
                        symbol.SetProcessed();
                        Query(new TerminalSet(Dot), out body);
                        ps.AddPredefined(new ClauseNode(head, body));
                        opt = false;
                    }
                    else
                    {
                        symbol.SetProcessed();
                        Term term;
                        PrologTerm(new TerminalSet(Dot), out term);
                        body = term.ToDCG(ref head);
                        ps.AddPredefined(new ClauseNode(head, body));
                        opt = false;
                    }
                }
                if (opt) ps.AddPredefined(new ClauseNode(head, null));
            }
            GetSymbol(new TerminalSet(Dot), true, true);
        }
        #endregion

        #region Query
        private void Query(TerminalSet _TS, out TermNode body)
        {
            Term t = null;
            GetSymbol(new TerminalSet(LeftParen, Identifier, IntLiteral, RealLiteral, StringLiteral, Operator, Atom, Anonymous,
                                        CutSym, LSqBracket, LCuBracket, PrologString, QMark), false, true);
            if (symbol.IsMemberOf(LeftParen, Identifier, IntLiteral, RealLiteral, StringLiteral, Operator, Atom, Anonymous, CutSym,
                                   LSqBracket, LCuBracket, PrologString))
            {
                PrologTerm(_TS, out t);
                body = t.ToGoalList();
            }
            else
            {
                symbol.SetProcessed();
                body = Term.TREEROOT.ToGoalList();
                TreeConstruction(_TS);
            }
        }
        #endregion

        #region TreeConstruction
        private void TreeConstruction(TerminalSet _TS)
        {
            Term t;
            PrologTerm(_TS, out t);
            Console.WriteLine("Final tree: {0}", t.ToTree());
            Console.WriteLine("Final term: {0}", t.ToString());
        }
        #endregion

        #region PrologTerm
        private void PrologTerm(TerminalSet _TS, out Term t)
        {
            Term lastInfix = null;
            string s;
            ArrayList termInfixArr = new ArrayList();
            termInfixArr.Add(null);
            do
            {
                GetSymbol(new TerminalSet(LeftParen, Identifier, IntLiteral, RealLiteral, StringLiteral, Operator, Atom, Anonymous,
                                            CutSym, LSqBracket, LCuBracket, PrologString), false, true);
                if (symbol.Terminal == Operator)
                {
                    symbol.SetProcessed();
                    t = new Term((OperatorDescr)symbol.OValue);
                }
                else if (symbol.IsMemberOf(StringLiteral, Atom))
                {
                    GetSymbol(new TerminalSet(StringLiteral, Atom), false, true);
                    if (symbol.Terminal == Atom)
                    {
                        symbol.SetProcessed();
                        s = symbol.ToString();
                    }
                    else
                    {
                        symbol.SetProcessed();
                        s = Utils.UnquoteIfUnnecessary(symbol.ToString());
                    }
                    Term[] terms = null;
                    bool b = is1000OperatorSetting;
                    GetSymbol(_TS.Union(LeftParen, Identifier, IntLiteral, RealLiteral, StringLiteral, Operator, Atom, Anonymous,
                                          CutSym, LSqBracket, LCuBracket, PrologString), false, true);
                    if (symbol.Terminal == LeftParen)
                    {
                        symbol.SetProcessed();
                        Set1000Operators(true);
                        terminalTable[DOT] = Atom;
                        terminalTable[OP] = Atom;
                        PrologTerm(new TerminalSet(RightParen), out t);
                        terminalTable[DOT] = Dot;
                        terminalTable[OP] = OpSym;
                        terms = t.ArgumentsToTermArray();
                        Set1000Operators(b);
                        GetSymbol(new TerminalSet(RightParen), true, true);
                    }
                    if (terms == null) t = new Term(s); else t = new Term(s, terms);
                }
                else if (symbol.Terminal == Identifier)
                {
                    symbol.SetProcessed();
                    s = symbol.ToString();
                    t = Globals.GetVariable(s);
                    if (t == null)
                    {
                        t = new NamedVar(s);
                        Globals.SetVariable(t, s);
                    }
                }
                else if (symbol.Terminal == Anonymous)
                {
                    symbol.SetProcessed();
                    t = new Term();
                }
                else if (symbol.IsMemberOf(IntLiteral, RealLiteral))
                {
                    GetSymbol(new TerminalSet(IntLiteral, RealLiteral), false, true);
                    if (symbol.Terminal == IntLiteral)
                    {
                        symbol.SetProcessed();
                    }
                    else
                    {
                        symbol.SetProcessed();
                    }
                    t = new Term(symbol.ToString(), FType.number);
                }
                else if (symbol.Terminal == LSqBracket)
                {
                    List(_TS.Union(LeftParen, Identifier, IntLiteral, RealLiteral, StringLiteral, Operator, Atom, Anonymous, CutSym,
                                     LSqBracket, LCuBracket, PrologString), out t);
                }
                else if (symbol.Terminal == LCuBracket)
                {
                    DCGBracketList(_TS.Union(LeftParen, Identifier, IntLiteral, RealLiteral, StringLiteral, Operator, Atom, Anonymous,
                                               CutSym, LSqBracket, LCuBracket, PrologString), out t);
                }
                else if (symbol.Terminal == PrologString)
                {
                    symbol.SetProcessed();
                    t = new Term(symbol.ToUnquoted(), FType.text);
                }
                else if (symbol.Terminal == CutSym)
                {
                    symbol.SetProcessed();
                    t = new Cut(0);
                }
                else
                {
                    symbol.SetProcessed();
                    PrologTerm(new TerminalSet(RightParen), out t);
                    GetSymbol(new TerminalSet(RightParen), true, true);
                    t.IsPacked = true;
                }
                termInfixArr.Add(t);
                Term.AnalyzeInput(ref termInfixArr, ref lastInfix);
                GetSymbol(_TS.Union(LeftParen, Identifier, IntLiteral, RealLiteral, StringLiteral, Operator, Atom, Anonymous, CutSym,
                                      LSqBracket, LCuBracket, PrologString), false, true);
            } while (!(_TS.Contains(symbol.Terminal)));
            termInfixArr.Add(null);
            Term.AnalyzeInput(ref termInfixArr, ref lastInfix);
            t = Term.InfixToPrefix(termInfixArr);
        }
        #endregion

        #region List
        private void List(TerminalSet _TS, out Term term)
        {
            Term beforeBar;
            Term afterBar = null;
            ArrayList elements = null;
            terminalTable[DOT] = Atom;
            terminalTable[OP] = Atom;
            GetSymbol(new TerminalSet(LSqBracket), true, true);
            GetSymbol(new TerminalSet(LeftParen, Identifier, IntLiteral, RealLiteral, StringLiteral, Operator, Atom, Anonymous,
                                        CutSym, LSqBracket, RSqBracket, LCuBracket, PrologString), false, true);
            if (symbol.IsMemberOf(LeftParen, Identifier, IntLiteral, RealLiteral, StringLiteral, Operator, Atom, Anonymous, CutSym,
                                   LSqBracket, LCuBracket, PrologString))
            {
                PrologTerm(new TerminalSet(RSqBracket, VBar), out beforeBar);
                elements = beforeBar.ArgumentsToArrayList(false);
                GetSymbol(new TerminalSet(RSqBracket, VBar), false, true);
                if (symbol.Terminal == VBar)
                {
                    symbol.SetProcessed();
                    PrologTerm(new TerminalSet(RSqBracket), out afterBar);
                }
            }
            terminalTable[DOT] = Dot;
            terminalTable[OP] = OpSym;
            GetSymbol(new TerminalSet(RSqBracket), true, true);
            if (afterBar == null) term = Term.NULLLIST; else term = afterBar;
            if (elements != null)
                for (int i = elements.Count - 1; i >= 0; i--) term = new ListTerm((Term)elements[i], term);
        }
        #endregion

        #region DCGBracketList
        private void DCGBracketList(TerminalSet _TS, out Term term)
        {
            Term head;
            ArrayList elements = null;
            GetSymbol(new TerminalSet(LCuBracket), true, true);
            GetSymbol(new TerminalSet(LeftParen, Identifier, IntLiteral, RealLiteral, StringLiteral, Operator, Atom, Anonymous,
                                        CutSym, LSqBracket, LCuBracket, RCuBracket, PrologString), false, true);
            if (symbol.IsMemberOf(LeftParen, Identifier, IntLiteral, RealLiteral, StringLiteral, Operator, Atom, Anonymous, CutSym,
                                   LSqBracket, LCuBracket, PrologString))
            {
                PrologTerm(new TerminalSet(RCuBracket), out head);
                elements = head.ArgumentsToArrayList(true);
            }
            GetSymbol(new TerminalSet(RCuBracket), true, true);
            term = new DCGTerm();
            if (elements != null)
                for (int i = elements.Count - 1; i >= 0; i--) term = new DCGTerm((Term)elements[i], term);
        }
        #endregion


        #endregion
    } // end of Parser class

    #endregion
}
