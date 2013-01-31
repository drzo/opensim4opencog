using System;
using System.Collections.Generic;
using System.Collections;
using System.Linq;
using System.Text;
using System.Text.RegularExpressions;
using System.Xml;
using System.Xml.Serialization;
using System.Runtime.Serialization;
using System.IO;
using Lucene.Net.Analysis;

namespace AltAIMLbot
{

    [Serializable()]
    public class SerializableDictionary<TKey, TVal> : Dictionary<TKey, TVal>, IXmlSerializable, ISerializable
    {
        #region Constants
        private const string DictionaryNodeName = "Dictionary";
        private const string ItemNodeName = "Item";
        private const string KeyNodeName = "Key";
        private const string ValueNodeName = "Value";

        #endregion
        #region Constructors
        public SerializableDictionary()
        {
        }

        public SerializableDictionary(IDictionary<TKey, TVal> dictionary)
            : base(dictionary)
        {
        }

        public SerializableDictionary(IEqualityComparer<TKey> comparer)
            : base(comparer)
        {
        }

        public SerializableDictionary(int capacity)
            : base(capacity)
        {
        }

        public SerializableDictionary(IDictionary<TKey, TVal> dictionary, IEqualityComparer<TKey> comparer)
            : base(dictionary, comparer)
        {
        }

        public SerializableDictionary(int capacity, IEqualityComparer<TKey> comparer)
            : base(capacity, comparer)
        {
        }

        #endregion
        #region ISerializable Members

        protected SerializableDictionary(SerializationInfo info, StreamingContext context)
        {
            int itemCount = info.GetInt32("ItemCount");
            for (int i = 0; i < itemCount; i++)
            {
                KeyValuePair<TKey, TVal> kvp = (KeyValuePair<TKey, TVal>)info.GetValue(String.Format("Item{0}", i), typeof(KeyValuePair<TKey, TVal>));
                this.Add(kvp.Key, kvp.Value);
            }
        }

        void ISerializable.GetObjectData(SerializationInfo info, StreamingContext context)
        {
            info.AddValue("ItemCount", this.Count);
            int itemIdx = 0;
            foreach (KeyValuePair<TKey, TVal> kvp in this)
            {
                info.AddValue(String.Format("Item{0}", itemIdx), kvp, typeof(KeyValuePair<TKey, TVal>));
                itemIdx++;
            }
        }

        #endregion
        #region IXmlSerializable Members

        void IXmlSerializable.WriteXml(System.Xml.XmlWriter writer)
        {
            //writer.WriteStartElement(DictionaryNodeName);
            foreach (KeyValuePair<TKey, TVal> kvp in this)
            {
                writer.WriteStartElement(ItemNodeName);
                writer.WriteStartElement(KeyNodeName);
                KeySerializer.Serialize(writer, kvp.Key);
                writer.WriteEndElement();
                writer.WriteStartElement(ValueNodeName);
                ValueSerializer.Serialize(writer, kvp.Value);
                writer.WriteEndElement();
                writer.WriteEndElement();
            }
            //writer.WriteEndElement();
        }

        void IXmlSerializable.ReadXml(System.Xml.XmlReader reader)
        {
            if (reader.IsEmptyElement)
            {
                return;
            }

            // Move past container
            if (!reader.Read())
            {
                throw new XmlException("Error in Deserialization of Dictionary");
            }

            //reader.ReadStartElement(DictionaryNodeName);
            while (reader.NodeType != XmlNodeType.EndElement)
            {
                reader.ReadStartElement(ItemNodeName);
                reader.ReadStartElement(KeyNodeName);
                TKey key = (TKey)KeySerializer.Deserialize(reader);
                reader.ReadEndElement();
                reader.ReadStartElement(ValueNodeName);
                TVal value = (TVal)ValueSerializer.Deserialize(reader);
                reader.ReadEndElement();
                reader.ReadEndElement();
                this.Add(key, value);
                reader.MoveToContent();
            }
            //reader.ReadEndElement();

            reader.ReadEndElement(); // Read End Element to close Read of containing node
        }

        System.Xml.Schema.XmlSchema IXmlSerializable.GetSchema()
        {
            return null;
        }

        #endregion
        #region Private Properties
        XmlSerializer valueSerializer = null;
        XmlSerializer keySerializer = null;

        protected XmlSerializer ValueSerializer
        {
            get
            {
                if (valueSerializer == null)
                {
                    valueSerializer = new XmlSerializer(typeof(TVal));
                }
                return valueSerializer;
            }
        }

        private XmlSerializer KeySerializer
        {
            get
            {
                if (keySerializer == null)
                {
                    keySerializer = new XmlSerializer(typeof(TKey));
                }
                return keySerializer;
            }
        }
        #endregion
    }


    /// <summary>
    /// Implements a straightforward Baysian Classifier in Log space.
    /// will serialize to a file in XML format
    /// </summary>
    [Serializable]
    public class BayesClassifier
    {
        public SerializableDictionary<string, int> Count = new SerializableDictionary<string, int>();
        public SerializableDictionary<string, int> Vocab = new SerializableDictionary<string, int>();
        public SerializableDictionary<string, int> ClassCount = new SerializableDictionary<string, int>();

        public int Vcount = 0;
        public bool bigrams = false; // should we use bigrams
        public bool apriori = false; // should we use class probabilities 

        public void learnClass(string classtext, string id)
        {
            string[] s1 = classtext.Split(' ');
            foreach (string d in s1)
            {

                string key = d.Trim() + " " + id;
                key = key.Trim();
                try
                {
                    Count[key] = Count[key] + 1;
                }
                catch
                {
                    Count[key] = 1;
                }
                //Count[id]= Count[id]+1;
                try
                {
                    ClassCount[id]++;
                }
                catch
                {
                    ClassCount[id] = 1;
                }
                if (!Vocab.ContainsKey(d))
                {
                    Vocab[d] = 1;
                    Vcount++;
                }
            }
            // Bigrams ???
            if (bigrams)
            {
                int n = s1.Length;
                for (int i = 0; i < n; i++)
                {
                    string d = s1[i] + " " + s1[i + 1];
                    string key = d.Trim() + " " + id;
                    key = key.Trim();
                    try
                    {
                        Count[key] = Count[key] + 1;
                    }
                    catch
                    {
                        Count[key] = 1;
                    }
                    //Count[id]= Count[id]+1;
                    try
                    {
                        ClassCount[id]++;
                    }
                    catch
                    {
                        ClassCount[id] = 1;
                    }
                    if (!Vocab.ContainsKey(d))
                    {
                        Vocab[d] = 1;
                        Vcount++;
                    }
                }

            }
        }

        public string bestClassFor(string inseq)
        {
            Dictionary<string, double> classprobs = estimate(inseq);
            string guessClass = "unknown";
            double guessScore = double.NegativeInfinity;
            foreach (string c in classprobs.Keys)
            {
                if (classprobs[c] > guessScore)
                {
                    guessClass = c;
                    guessScore = classprobs[c];
                }
            }
            return guessClass;
        }

        public Dictionary<string, double> estimate(string intext)
        {
            Dictionary<string, double> classProbs = new Dictionary<string, double>();
            int ctot = 1; // Safety, should be zero
            foreach (string c in ClassCount.Keys)
            {
                ctot += ClassCount[c];
            }
            double smoother = Math.Log(1 / (double)ctot);

            foreach (string c in ClassCount.Keys)
            {
                if (apriori)
                {
                    classProbs[c] = Math.Log((double)ClassCount[c] / (double)ctot);
                }
                else
                {
                    classProbs[c] = 0;
                }
            }

            string[] words = intext.Split(' ');
            foreach (string d in words)
            {
                if (Vocab.ContainsKey(d))
                {
                    foreach (string c in ClassCount.Keys)
                    {
                        string key = d.Trim() + " " + c;
                        if (Count.ContainsKey(key))
                        {
                            double prob_d = ((1 + (double)Count[key]) / ((double)Vcount + (double)ClassCount[c]));
                            classProbs[c] = classProbs[c] + Math.Log(prob_d);
                        }
                        else
                        {
                            classProbs[c] = classProbs[c] + smoother;
                        }
                    }
                }
                else
                {
                    // unseen token oov
                }
            }

            if (bigrams)
            {
                int n = words.Length;
                for (int i = 0; i < n; i++)
                {
                    string d = words[i] + " " + words[i + 1];
                    if (Vocab.ContainsKey(d))
                    {
                        foreach (string c in ClassCount.Keys)
                        {
                            string key = d.Trim() + " " + c;
                            if (Count.ContainsKey(key))
                            {
                                double prob_d = (1 + Count[key]) / (Vcount + ClassCount[c]);
                                classProbs[c] = classProbs[c] + Math.Log(prob_d);
                            }
                            else
                            {
                                classProbs[c] = classProbs[c] + smoother;
                            }
                        }
                    }
                    else
                    {
                        // unseen token oov
                    }
                }

            }

            return classProbs;
        }

        public void loadClassifier(string filename)
        {
            System.IO.StreamReader R = null;
            try
            {
                System.Xml.Serialization.XmlSerializer S = new System.Xml.Serialization.XmlSerializer(typeof(BayesClassifier));
                R = new System.IO.StreamReader(filename);
                BayesClassifier F = (BayesClassifier)S.Deserialize(R);

                R.Close();
                this.Count = F.Count;
                this.Vocab = F.Vocab;
                this.ClassCount = F.ClassCount;
                this.Vcount = F.Vcount;
                this.bigrams = F.bigrams;
                this.apriori = F.apriori;
            }
            catch
            {
                if (R != null) R.Close();
            }

        }


        public void saveClassifier(string filename)
        {
            System.IO.StreamWriter W = null;
            try
            {
                W = new System.IO.StreamWriter(filename);
                System.Xml.Serialization.XmlSerializer S = new System.Xml.Serialization.XmlSerializer(typeof(BayesClassifier));
                S.Serialize(W, this);

                W.Close();
            }
            catch
            {
                if (W != null) W.Close();
            }
        }


    }

    static class LevenshteinDistance
    {
        /// <summary>
        /// Compute the distance between two strings.
        /// </summary>
        public static int Compute(string s, string t)
        {
            int n = s.Length;
            int m = t.Length;
            int[,] d = new int[n + 1, m + 1];

            // Step 1
            if (n == 0)
            {
                return m;
            }

            if (m == 0)
            {
                return n;
            }

            // Step 2
            for (int i = 0; i <= n; d[i, 0] = i++)
            {
            }

            for (int j = 0; j <= m; d[0, j] = j++)
            {
            }

            // Step 3
            for (int i = 1; i <= n; i++)
            {
                //Step 4
                for (int j = 1; j <= m; j++)
                {
                    // Step 5
                    int cost = (t[j - 1] == s[i - 1]) ? 0 : 1;

                    // Step 6
                    d[i, j] = Math.Min(
                        Math.Min(d[i - 1, j] + 1, d[i, j - 1] + 1),
                        d[i - 1, j - 1] + cost);
                }
            }
            // Step 7
            return d[n, m];
        }

        //http://siderite.blogspot.com/2007/01/super-fast-string-distance-algorithm.html
        // similar to Levenshtein but does a LCS (longest commmon substring) first
        // and approximates from there
        public static float Sift3Distance(string s1, string s2, int maxOffset)
        { 
            if (String.IsNullOrEmpty(s1))return String.IsNullOrEmpty(s2) ? 0 : s2.Length;
            if (String.IsNullOrEmpty(s2))return s1.Length;
            int c = 0;
            int offset1 = 0;
            int offset2 = 0;
            int lcs = 0;
            while ((c + offset1 < s1.Length) && (c + offset2 < s2.Length))
            {
                if (s1[c + offset1] == s2[c + offset2]) 
                    lcs++;
                else { 
                    offset1 = 0;
                    offset2 = 0;
                    for (int i = 0; i < maxOffset; i++)
                    { 
                        if ((c + i < s1.Length) && (s1[c + i] == s2[c]))
                        { 
                            offset1 = i; 
                            break; 
                        } 
                        if ((c + i < s2.Length) && (s1[c] == s2[c + i]))
                        {
                            offset2 = i;
                            break;
                        }
                    } 
                } 
                c++;
            }
            return (s1.Length + s2.Length) / 2 - lcs;
        }
        /// <SUMMARY>Computes the Levenshtein Edit Distance between two enumerables.</SUMMARY>
        /// <TYPEPARAM name="T">The type of the items in the enumerables.</TYPEPARAM>
        /// <PARAM name="x">The first enumerable.</PARAM>
        /// <PARAM name="y">The second enumerable.</PARAM>
        /// <RETURNS>The edit distance.</RETURNS>
        //http://blogs.msdn.com/b/toub/archive/2006/05/05/590814.aspx
        public static int EditDistance<T>(IEnumerable<T> x, IEnumerable<T> y)
            where T : IEquatable<T>
        {
            // Validate parameters
            if (x == null) throw new ArgumentNullException("x");
            if (y == null) throw new ArgumentNullException("y");

            // Convert the parameters into IList instances
            // in order to obtain indexing capabilities
            IList<T> first = x as IList<T> ?? new List<T>(x);
            IList<T> second = y as IList<T> ?? new List<T>(y);

            // Get the length of both.  If either is 0, return
            // the length of the other, since that number of insertions
            // would be required.
            int n = first.Count, m = second.Count;
            if (n == 0) return m;
            if (m == 0) return n;

            // Rather than maintain an entire matrix (which would require O(n*m) space),
            // just store the current row and the next row, each of which has a length m+1,
            // so just O(m) space. Initialize the current row.
            int curRow = 0, nextRow = 1;
            int[][] rows = new int[][] { new int[m + 1], new int[m + 1] };
            for (int j = 0; j <= m; ++j) rows[curRow][j] = j;

            // For each virtual row (since we only have physical storage for two)
            for (int i = 1; i <= n; ++i)
            {
                // Fill in the values in the row
                rows[nextRow][0] = i;
                for (int j = 1; j <= m; ++j)
                {
                    int dist1 = rows[curRow][j] + 1;
                    int dist2 = rows[nextRow][j - 1] + 1;
                    int dist3 = rows[curRow][j - 1] +
                        (first[i - 1].Equals(second[j - 1]) ? 0 : 1);

                    rows[nextRow][j] = Math.Min(dist1, Math.Min(dist2, dist3));
                }

                // Swap the current and next rows
                if (curRow == 0)
                {
                    curRow = 1;
                    nextRow = 0;
                }
                else
                {
                    curRow = 0;
                    nextRow = 1;
                }
            }

            // Return the computed edit distance
            return rows[curRow][m];
        }

        public static string hypotheizeSrai(string source, string target, out string newTarget)
        {


            string[] s = (source.Trim().ToUpper() + " ").Split(' ');
            string[] t = (target.Trim().ToUpper() + " ").Split(' ');
            int n = s.Length - 1;
            int m = t.Length - 1;
            int[,] d = new int[n + 1, m + 1];
            string[,] p1 = new string[n + 2, m + 2];
            string[,] p2 = new string[n + 2, m + 2];
            string[,] r = new string[n + 2, m + 2];
            // Step 2
            for (int i = 0; i <= n; d[i, 0] = i++)
            {
                p1[i, 0] = "";// s[i];
                p2[i, 0] = "";
            }

            for (int j = 0; j <= m; d[0, j] = j++)
            {
                p2[0, j] = "";// t[j];
                p1[0, j] = "*";
            }
            p1[0, 0] = "";
            p2[0, 0] = "";
            // Step 3
            for (int i = 1; i <= n; i++)
            {
                //Step 4
                for (int j = 1; j <= m; j++)
                {
                    // Step 5 : identical or an edit
                    int cost = (t[j - 1] == s[i - 1]) ? 0 : 1;

                    // Step 6
                    d[i, j] = Math.Min(
                        Math.Min(d[i - 1, j] + 1, d[i, j - 1] + 1),
                        d[i - 1, j - 1] + cost);
                    // Decode
                    if (d[i, j] == d[i - 1, j - 1] + cost)
                    {
                        // edit
                        p1[i, j] = p1[i - 1, j - 1] + " " + s[i - 1] + " ";

                        // if target is a "*" then fill in with the source
                        if (t[j - 1] == "*")
                        {
                            if (p1[i - 1, j - 1].EndsWith("*") || p1[i - 1, j - 1].EndsWith("* "))
                            {
                                p2[i, j] = p2[i - 1, j - 1] + " <star/> " + s[i - 1] + " ";
                            }
                            else
                            {
                                p2[i, j] = p2[i - 1, j - 1] + " " + s[i - 1] + " ";
                            }
                        }
                        else
                        {
                            // else use the orignal target value
                            p2[i, j] = p2[i - 1, j - 1] + " " + t[j - 1] + " ";
                        }
                        // if on an edge then assume we had to delete all the way to get there
                        if ((j == 1) && (i > 1)) { p1[i, j] = "* " + p1[i, j]; }

                        if (cost == 0)
                        {
                            //same
                            r[i, j] = r[i - 1, j - 1] + string.Format("|same {0} == {1} @ ({2},{3})", s[i - 1], t[j - 1], i, j);
                        }
                        else
                        {
                            // different
                            r[i, j] = r[i - 1, j - 1] + string.Format("|edit {0} -> {1} @ ({2},{3})", s[i - 1], t[j - 1], i, j);
                        }

                    }
                    else
                        if (d[i, j] == d[i - 1, j] + 1)
                        {
                            // deletion (from s to t)
                            p1[i, j] = p1[i - 1, j] + "* ";
                            p2[i, j] = p2[i - 1, j] + "";
                            r[i, j] = r[i - 1, j] + "|delete " + s[i - 1];
                        }
                        else
                            if (d[i, j] == d[i, j - 1] + 1)
                            {
                                // insertion (into t
                                p1[i, j] = p1[i, j - 1] + "";
                                p2[i, j] = p2[i, j - 1] + t[j - 1] + " ";
                                r[i, j] = r[i, j - 1] + "|insert " + t[j - 1];

                            }




                }
            }
            // Step 7
            int editDistance = d[n, m];
            string pattern = p1[n, m].ToUpper().Trim();
           // string srai = p2[n, m].Trim();
            while (pattern.Contains("  ")) { pattern = pattern.Replace("  ", " "); }
            while (pattern.Contains("* *")) { pattern = pattern.Replace("* *", "*"); }
            // string category = String.Format("<pattern>{0}<pattern> -> <srai>{1}</srai>", pattern, srai);
            newTarget = p2[n, m].Trim();
            return pattern;
        }

    }

    /*
    Copyright (c) 2008 Anthony Tong Lee

    Permission is hereby granted, free of charge, to any person
    obtaining a copy of this software and associated documentation
    files (the "Software"), to deal in the Software without
    restriction, including without limitation the rights to use,
    copy, modify, merge, publish, distribute, sublicense, and/or sell
    copies of the Software, and to permit persons to whom the
    Software is furnished to do so, subject to the following
    conditions:

    The above copyright notice and this permission notice shall be
    included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
    EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
    OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
    NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
    HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
    WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
    FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
    OTHER DEALINGS IN THE SOFTWARE.
    */
    //using System;
    //using System.Diagnostics.CodeAnalysis;
    //using System.Text;

    // Original Source
    // http://code.google.com/p/doublemetaphone/
    // http://doublemetaphone.googlecode.com/svn/tags/1/DoubleMetaphone.cs

    //namespace DoubleMetaphone
    //{
    /// <summary>
    /// DoubleMetaphone string extension
    /// </summary>
    /// <remarks>
    /// Original C++ implementation:
    ///		"Double Metaphone (c) 1998, 1999 by Lawrence Philips"
    ///		http://www.ddj.com/cpp/184401251?pgno=1
    /// </remarks>
    //[SuppressMessage("Microsoft.Naming", "CA1704:IdentifiersShouldBeSpelledCorrectly", MessageId = "Metaphone")]

    public static class DoubleMetaphoneStringExtension
    {
        //[SuppressMessage("Microsoft.Naming", "CA1704:IdentifiersShouldBeSpelledCorrectly", MessageId = "Metaphone")]
        //[SuppressMessage("Microsoft.Maintainability", "CA1505:AvoidUnmaintainableCode")]
        //[SuppressMessage("Microsoft.Maintainability", "CA1502:AvoidExcessiveComplexity")]
        //public static string GenerateDoubleMetaphone(this string self)
        public static string GenerateDoubleMetaphone(string self)
        {

            MetaphoneData metaphoneData = new MetaphoneData();
            int current = 0;

            if (self.Length < 1)
            {
                return self;
            }
            int last = self.Length - 1; //zero based index

            string workingString = self.ToUpperInvariant() + "     ";
            string prefix = "";
            if (workingString[0] != '<') prefix = workingString[0].ToString();

            bool isSlavoGermanic = (self.IndexOf('W') > -1) || (self.IndexOf('K') > -1) || (self.IndexOf("CZ", StringComparison.OrdinalIgnoreCase) > -1)
                || (self.IndexOf("WITZ", StringComparison.OrdinalIgnoreCase) > -1);

            //skip these when at start of word
            if (workingString.StartsWith("GN", StringComparison.OrdinalIgnoreCase) ||
                workingString.StartsWith("KN", StringComparison.OrdinalIgnoreCase) ||
                workingString.StartsWith("PN", StringComparison.OrdinalIgnoreCase) ||
                workingString.StartsWith("WR", StringComparison.OrdinalIgnoreCase) ||
                workingString.StartsWith("PS", StringComparison.OrdinalIgnoreCase))
            {
                current += 1;
            }

            //Initial 'X' is pronounced 'Z' e.g. 'Xavier'
            if (workingString[0] == 'X')
            {
                metaphoneData.Add("S"); //'Z' maps to 'S'
                current += 1;
            }

            while ((metaphoneData.PrimaryLength < 4) || (metaphoneData.SecondaryLength < 4))
            {
                if (current >= self.Length)
                {
                    break;
                }

                switch (workingString[current])
                {
                    case 'A':
                    case 'E':
                    case 'I':
                    case 'O':
                    case 'U':
                    case 'Y':
                        if (current == 0)
                        {
                            //all init vowels now map to 'A'
                            metaphoneData.Add("A");
                        }
                        current += 1;
                        break;

                    case 'B':
                        //"-mb", e.g", "dumb", already skipped over...
                        metaphoneData.Add("P");

                        if (workingString[current + 1] == 'B')
                        {
                            current += 2;
                        }
                        else
                        {
                            current += 1;
                        }
                        break;

                    case 'Ã':
                        metaphoneData.Add("S");
                        current += 1;
                        break;

                    case 'C':
                        //various germanic
                        if ((current > 1) && !IsVowel(workingString[current - 2]) && StringAt(workingString, (current - 1), "ACH")
                            && ((workingString[current + 2] != 'I') && ((workingString[current + 2] != 'E') || StringAt(workingString, (current - 2), "BACHER", "MACHER"))))
                        {
                            metaphoneData.Add("K");
                            current += 2;
                            break;
                        }

                        //special case 'caesar'
                        if ((current == 0) && StringAt(workingString, current, "CAESAR"))
                        {
                            metaphoneData.Add("S");
                            current += 2;
                            break;
                        }

                        //italian 'chianti'
                        if (StringAt(workingString, current, "CHIA"))
                        {
                            metaphoneData.Add("K");
                            current += 2;
                            break;
                        }

                        if (StringAt(workingString, current, "CH"))
                        {
                            //find 'michael'
                            if ((current > 0) && StringAt(workingString, current, "CHAE"))
                            {
                                metaphoneData.Add("K", "X");
                                current += 2;
                                break;
                            }

                            //greek roots e.g. 'chemistry', 'chorus'
                            if ((current == 0) && (StringAt(workingString, (current + 1), "HARAC", "HARIS") || StringAt(workingString, (current + 1), "HOR", "HYM", "HIA", "HEM"))
                                && !StringAt(workingString, 0, "CHORE"))
                            {
                                metaphoneData.Add("K");
                                current += 2;
                                break;
                            }

                            //germanic, greek, or otherwise 'ch' for 'kh' sound
                            if ((StringAt(workingString, 0, "VAN ", "VON ") || StringAt(workingString, 0, "SCH")) // 'architect but not 'arch', 'orchestra', 'orchid'
                                || StringAt(workingString, (current - 2), "ORCHES", "ARCHIT", "ORCHID") || StringAt(workingString, (current + 2), "T", "S")
                                    || ((StringAt(workingString, (current - 1), "A", "O", "U", "E") || (current == 0)) //e.g., 'wachtler', 'wechsler', but not 'tichner'
                                        && StringAt(workingString, (current + 2), "L", "R", "N", "M", "B", "H", "F", "V", "W", " ")))
                            {
                                metaphoneData.Add("K");
                            }
                            else
                            {
                                if (current > 0)
                                {
                                    if (StringAt(workingString, 0, "MC"))
                                    {
                                        //e.g., "McHugh"
                                        metaphoneData.Add("K");
                                    }
                                    else
                                    {
                                        metaphoneData.Add("X", "K");
                                    }
                                }
                                else
                                {
                                    metaphoneData.Add("X");
                                }
                            }
                            current += 2;
                            break;
                        }
                        //e.g, 'czerny'
                        if (StringAt(workingString, current, "CZ") && !StringAt(workingString, (current - 2), "WICZ"))
                        {
                            metaphoneData.Add("S", "X");
                            current += 2;
                            break;
                        }

                        //e.g., 'focaccia'
                        if (StringAt(workingString, (current + 1), "CIA"))
                        {
                            metaphoneData.Add("X");
                            current += 3;
                            break;
                        }

                        //double 'C', but not if e.g. 'McClellan'
                        if (StringAt(workingString, current, "CC") && !((current == 1) && (workingString[0] == 'M')))
                        {
                            //'bellocchio' but not 'bacchus'
                            if (StringAt(workingString, (current + 2), "I", "E", "H") && !StringAt(workingString, (current + 2), "HU"))
                            {
                                //'accident', 'accede' 'succeed'
                                if (((current == 1) && (workingString[current - 1] == 'A')) || StringAt(workingString, (current - 1), "UCCEE", "UCCES"))
                                {
                                    metaphoneData.Add("KS");
                                }
                                //'bacci', 'bertucci', other italian
                                else
                                {
                                    metaphoneData.Add("X");
                                }
                                current += 3;
                                break;
                            }
                            else
                            {
                                //Pierce's rule
                                metaphoneData.Add("K");
                                current += 2;
                                break;
                            }
                        }

                        if (StringAt(workingString, current, "CK", "CG", "CQ"))
                        {
                            metaphoneData.Add("K");
                            current += 2;
                            break;
                        }

                        if (StringAt(workingString, current, "CI", "CE", "CY"))
                        {
                            //italian vs. english
                            if (StringAt(workingString, current, "CIO", "CIE", "CIA"))
                            {
                                metaphoneData.Add("S", "X");
                            }
                            else
                            {
                                metaphoneData.Add("S");
                            }
                            current += 2;
                            break;
                        }

                        //else
                        metaphoneData.Add("K");

                        //name sent in 'mac caffrey', 'mac gregor
                        if (StringAt(workingString, (current + 1), " C", " Q", " G"))
                        {
                            current += 3;
                        }
                        else if (StringAt(workingString, (current + 1), "C", "K", "Q") && !StringAt(workingString, (current + 1), "CE", "CI"))
                        {
                            current += 2;
                        }
                        else
                        {
                            current += 1;
                        }
                        break;

                    case 'D':
                        if (StringAt(workingString, current, "DG"))
                        {
                            if (StringAt(workingString, (current + 2), "I", "E", "Y"))
                            {
                                //e.g. 'edge'
                                metaphoneData.Add("J");
                                current += 3;
                                break;
                            }
                            else
                            {
                                //e.g. 'edgar'
                                metaphoneData.Add("TK");
                                current += 2;
                                break;
                            }
                        }

                        if (StringAt(workingString, current, "DT", "DD"))
                        {
                            metaphoneData.Add("T");
                            current += 2;
                            break;
                        }

                        //else
                        metaphoneData.Add("T");
                        current += 1;
                        break;

                    case 'F':
                        if (workingString[current + 1] == 'F')
                        {
                            current += 2;
                        }
                        else
                        {
                            current += 1;
                        }
                        metaphoneData.Add("F");
                        break;

                    case 'G':
                        if (workingString[current + 1] == 'H')
                        {
                            if ((current > 0) && !IsVowel(workingString[current - 1]))
                            {
                                metaphoneData.Add("K");
                                current += 2;
                                break;
                            }

                            if (current < 3)
                            {
                                //'ghislane', ghiradelli
                                if (current == 0)
                                {
                                    if (workingString[current + 2] == 'I')
                                    {
                                        metaphoneData.Add("J");
                                    }
                                    else
                                    {
                                        metaphoneData.Add("K");
                                    }
                                    current += 2;
                                    break;
                                }
                            }
                            //Parker's rule (with some further refinements) - e.g., 'hugh'
                            if (((current > 1) && StringAt(workingString, (current - 2), "B", "H", "D")) //e.g., 'bough'
                                || ((current > 2) && StringAt(workingString, (current - 3), "B", "H", "D")) //e.g., 'broughton'
                                    || ((current > 3) && StringAt(workingString, (current - 4), "B", "H")))
                            {
                                current += 2;
                                break;
                            }
                            else
                            {
                                //e.g., 'laugh', 'McLaughlin', 'cough', 'gough', 'rough', 'tough'
                                if ((current > 2) && (workingString[current - 1] == 'U') && StringAt(workingString, (current - 3), "C", "G", "L", "R", "T"))
                                {
                                    metaphoneData.Add("F");
                                }
                                else if ((current > 0) && workingString[current - 1] != 'I')
                                {
                                    metaphoneData.Add("K");
                                }

                                current += 2;
                                break;
                            }
                        }

                        if (workingString[current + 1] == 'N')
                        {
                            if ((current == 1) && IsVowel(workingString[0]) && !isSlavoGermanic)
                            {
                                metaphoneData.Add("KN", "N");
                            }
                            else
                                //not e.g. 'cagney'
                                if (!StringAt(workingString, (current + 2), "EY") && (workingString[current + 1] != 'Y') && !isSlavoGermanic)
                                {
                                    metaphoneData.Add("N", "KN");
                                }
                                else
                                {
                                    metaphoneData.Add("KN");
                                }
                            current += 2;
                            break;
                        }

                        //'tagliaro'
                        if (StringAt(workingString, (current + 1), "LI") && !isSlavoGermanic)
                        {
                            metaphoneData.Add("KL", "L");
                            current += 2;
                            break;
                        }

                        //-ges-,-gep-,-gel-, -gie- at beginning
                        if ((current == 0)
                            && ((workingString[current + 1] == 'Y') || StringAt(workingString, (current + 1), "ES", "EP", "EB", "EL", "EY", "IB", "IL", "IN", "IE", "EI", "ER")))
                        {
                            metaphoneData.Add("K", "J");
                            current += 2;
                            break;
                        }

                        // -ger-,  -gy-
                        if ((StringAt(workingString, (current + 1), "ER") || (workingString[current + 1] == 'Y')) && !StringAt(workingString, 0, "DANGER", "RANGER", "MANGER")
                            && !StringAt(workingString, (current - 1), "E", "I") && !StringAt(workingString, (current - 1), "RGY", "OGY"))
                        {
                            metaphoneData.Add("K", "J");
                            current += 2;
                            break;
                        }

                        // italian e.g, 'biaggi'
                        if (StringAt(workingString, (current + 1), "E", "I", "Y") || StringAt(workingString, (current - 1), "AGGI", "OGGI"))
                        {
                            //obvious germanic
                            if ((StringAt(workingString, 0, "VAN ", "VON ") || StringAt(workingString, 0, "SCH")) || StringAt(workingString, (current + 1), "ET"))
                            {
                                metaphoneData.Add("K");
                            }
                            else
                                //always soft if french ending
                                if (StringAt(workingString, (current + 1), "IER "))
                                {
                                    metaphoneData.Add("J");
                                }
                                else
                                {
                                    metaphoneData.Add("J", "K");
                                }
                            current += 2;
                            break;
                        }

                        if (workingString[current + 1] == 'G')
                        {
                            current += 2;
                        }
                        else
                        {
                            current += 1;
                        }
                        metaphoneData.Add("K");
                        break;

                    case 'H':
                        //only keep if first & before vowel or btw. 2 vowels
                        if (((current == 0) || IsVowel(workingString[current - 1])) && IsVowel(workingString[current + 1]))
                        {
                            metaphoneData.Add("H");
                            current += 2;
                        }
                        else //also takes care of 'HH'
                        {
                            current += 1;
                        }
                        break;

                    case 'J':
                        //obvious spanish, 'jose', 'san jacinto'
                        if (StringAt(workingString, current, "JOSE") || StringAt(workingString, 0, "SAN "))
                        {
                            if (((current == 0) && (workingString[current + 4] == ' ')) || StringAt(workingString, 0, "SAN "))
                            {
                                metaphoneData.Add("H");
                            }
                            else
                            {
                                metaphoneData.Add("J", "H");
                            }
                            current += 1;
                            break;
                        }

                        if ((current == 0) && !StringAt(workingString, current, "JOSE"))
                        {
                            metaphoneData.Add("J", "A"); //Yankelovich/Jankelowicz
                        }
                        else
                            //spanish pron. of e.g. 'bajador'
                            if (IsVowel(workingString[current - 1]) && !isSlavoGermanic && ((workingString[current + 1] == 'A') || (workingString[current + 1] == 'O')))
                            {
                                metaphoneData.Add("J", "H");
                            }
                            else if (current == last)
                            {
                                metaphoneData.Add("J", " ");
                            }
                            else if (!StringAt(workingString, (current + 1), "L", "T", "K", "S", "N", "M", "B", "Z") && !StringAt(workingString, (current - 1), "S", "K", "L"))
                            {
                                metaphoneData.Add("J");
                            }

                        if (workingString[current + 1] == 'J') //it could happen!
                        {
                            current += 2;
                        }
                        else
                        {
                            current += 1;
                        }
                        break;

                    case 'K':
                        if (workingString[current + 1] == 'K')
                        {
                            current += 2;
                        }
                        else
                        {
                            current += 1;
                        }
                        metaphoneData.Add("K");
                        break;

                    case 'L':
                        if (workingString[current + 1] == 'L')
                        {
                            //spanish e.g. 'cabrillo', 'gallegos'
                            if (((current == (self.Length - 3)) && StringAt(workingString, (current - 1), "ILLO", "ILLA", "ALLE"))
                                || ((StringAt(workingString, (last - 1), "AS", "OS") || StringAt(workingString, last, "A", "O")) && StringAt(workingString, (current - 1), "ALLE")))
                            {
                                metaphoneData.Add("L", " ");
                                current += 2;
                                break;
                            }
                            current += 2;
                        }
                        else
                        {
                            current += 1;
                        }
                        metaphoneData.Add("L");
                        break;

                    case 'M':
                        if ((StringAt(workingString, (current - 1), "UMB") && (((current + 1) == last) || StringAt(workingString, (current + 2), "ER"))) //'dumb','thumb'
                            || (workingString[current + 1] == 'M'))
                        {
                            current += 2;
                        }
                        else
                        {
                            current += 1;
                        }
                        metaphoneData.Add("M");
                        break;

                    case 'N':
                        if (workingString[current + 1] == 'N')
                        {
                            current += 2;
                        }
                        else
                        {
                            current += 1;
                        }
                        metaphoneData.Add("N");
                        break;

                    case 'Ð':
                        current += 1;
                        metaphoneData.Add("N");
                        break;

                    case 'P':
                        if (workingString[current + 1] == 'H')
                        {
                            metaphoneData.Add("F");
                            current += 2;
                            break;
                        }

                        //also account for "campbell", "raspberry"
                        if (StringAt(workingString, (current + 1), "P", "B"))
                        {
                            current += 2;
                        }
                        else
                        {
                            current += 1;
                        }
                        metaphoneData.Add("P");
                        break;

                    case 'Q':
                        if (workingString[current + 1] == 'Q')
                        {
                            current += 2;
                        }
                        else
                        {
                            current += 1;
                        }
                        metaphoneData.Add("K");
                        break;

                    case 'R':
                        //french e.g. 'rogier', but exclude 'hochmeier'
                        if ((current == last) && !isSlavoGermanic && StringAt(workingString, (current - 2), "IE") && !StringAt(workingString, (current - 4), "ME", "MA"))
                        {
                            metaphoneData.Add("", "R");
                        }
                        else
                        {
                            metaphoneData.Add("R");
                        }

                        if (workingString[current + 1] == 'R')
                        {
                            current += 2;
                        }
                        else
                        {
                            current += 1;
                        }
                        break;

                    case 'S':
                        //special cases 'island', 'isle', 'carlisle', 'carlysle'
                        if (StringAt(workingString, (current - 1), "ISL", "YSL"))
                        {
                            current += 1;
                            break;
                        }

                        //special case 'sugar-'
                        if ((current == 0) && StringAt(workingString, current, "SUGAR"))
                        {
                            metaphoneData.Add("X", "S");
                            current += 1;
                            break;
                        }

                        if (StringAt(workingString, current, "SH"))
                        {
                            //germanic
                            if (StringAt(workingString, (current + 1), "HEIM", "HOEK", "HOLM", "HOLZ"))
                            {
                                metaphoneData.Add("S");
                            }
                            else
                            {
                                metaphoneData.Add("X");
                            }
                            current += 2;
                            break;
                        }

                        //italian & armenian
                        if (StringAt(workingString, current, "SIO", "SIA") || StringAt(workingString, current, "SIAN"))
                        {
                            if (!isSlavoGermanic)
                            {
                                metaphoneData.Add("S", "X");
                            }
                            else
                            {
                                metaphoneData.Add("S");
                            }
                            current += 3;
                            break;
                        }

                        //german & anglicisations, e.g. 'smith' match 'schmidt', 'snider' match 'schneider'
                        //also, -sz- in slavic language altho in hungarian it is pronounced 's'
                        if (((current == 0) && StringAt(workingString, (current + 1), "M", "N", "L", "W")) || StringAt(workingString, (current + 1), "Z"))
                        {
                            metaphoneData.Add("S", "X");
                            if (StringAt(workingString, (current + 1), "Z"))
                            {
                                current += 2;
                            }
                            else
                            {
                                current += 1;
                            }
                            break;
                        }

                        if (StringAt(workingString, current, "SC"))
                        {
                            //Schlesinger's rule
                            if (workingString[current + 2] == 'H')
                            {
                                //dutch origin, e.g. 'school', 'schooner'
                                if (StringAt(workingString, (current + 3), "OO", "ER", "EN", "UY", "ED", "EM"))
                                {
                                    //'schermerhorn', 'schenker'
                                    if (StringAt(workingString, (current + 3), "ER", "EN"))
                                    {
                                        metaphoneData.Add("X", "SK");
                                    }
                                    else
                                    {
                                        metaphoneData.Add("SK");
                                    }
                                    current += 3;
                                    break;
                                }
                                else
                                {
                                    if ((current == 0) && !IsVowel(workingString[3]) && (workingString[3] != 'W'))
                                    {
                                        metaphoneData.Add("X", "S");
                                    }
                                    else
                                    {
                                        metaphoneData.Add("X");
                                    }
                                    current += 3;
                                    break;
                                }
                            }

                            if (StringAt(workingString, (current + 2), "I", "E", "Y"))
                            {
                                metaphoneData.Add("S");
                                current += 3;
                                break;
                            }
                            //else
                            metaphoneData.Add("SK");
                            current += 3;
                            break;
                        }

                        //french e.g. 'resnais', 'artois'
                        if ((current == last) && StringAt(workingString, (current - 2), "AI", "OI"))
                        {
                            metaphoneData.Add("", "S");
                        }
                        else
                        {
                            metaphoneData.Add("S");
                        }

                        if (StringAt(workingString, (current + 1), "S", "Z"))
                        {
                            current += 2;
                        }
                        else
                        {
                            current += 1;
                        }
                        break;

                    case 'T':
                        if (StringAt(workingString, current, "TION"))
                        {
                            metaphoneData.Add("X");
                            current += 3;
                            break;
                        }

                        if (StringAt(workingString, current, "TIA", "TCH"))
                        {
                            metaphoneData.Add("X");
                            current += 3;
                            break;
                        }

                        if (StringAt(workingString, current, "TH") || StringAt(workingString, current, "TTH"))
                        {
                            //special case 'thomas', 'thames' or germanic
                            if (StringAt(workingString, (current + 2), "OM", "AM") || StringAt(workingString, 0, "VAN ", "VON ") || StringAt(workingString, 0, "SCH"))
                            {
                                metaphoneData.Add("T");
                            }
                            else
                            {
                                metaphoneData.Add("O", "T");
                            }
                            current += 2;
                            break;
                        }

                        if (StringAt(workingString, (current + 1), "T", "D"))
                        {
                            current += 2;
                        }
                        else
                        {
                            current += 1;
                        }
                        metaphoneData.Add("T");
                        break;

                    case 'V':
                        if (workingString[current + 1] == 'V')
                        {
                            current += 2;
                        }
                        else
                        {
                            current += 1;
                        }
                        metaphoneData.Add("F");
                        break;

                    case 'W':
                        //can also be in middle of word
                        if (StringAt(workingString, current, "WR"))
                        {
                            metaphoneData.Add("R");
                            current += 2;
                            break;
                        }

                        if ((current == 0) && (IsVowel(workingString[current + 1]) || StringAt(workingString, current, "WH")))
                        {
                            //Wasserman should match Vasserman
                            if (IsVowel(workingString[current + 1]))
                            {
                                metaphoneData.Add("A", "F");
                            }
                            else
                            {
                                //need Uomo to match Womo
                                metaphoneData.Add("A");
                            }
                        }

                        //Arnow should match Arnoff
                        if (((current == last) &&(current>0)&& IsVowel(workingString[current - 1])) || StringAt(workingString, (current - 1), "EWSKI", "EWSKY", "OWSKI", "OWSKY")
                            || StringAt(workingString, 0, "SCH"))
                        {
                            metaphoneData.Add("", "F");
                            current += 1;
                            break;
                        }

                        //polish e.g. 'filipowicz'
                        if (StringAt(workingString, current, "WICZ", "WITZ"))
                        {
                            metaphoneData.Add("TS", "FX");
                            current += 4;
                            break;
                        }

                        //else skip it
                        current += 1;
                        break;

                    case 'X':
                        //french e.g. breaux
                        if (!((current == last) && (StringAt(workingString, (current - 3), "IAU", "EAU") || StringAt(workingString, (current - 2), "AU", "OU"))))
                        {
                            metaphoneData.Add("KS");
                        }

                        if (StringAt(workingString, (current + 1), "C", "X"))
                        {
                            current += 2;
                        }
                        else
                        {
                            current += 1;
                        }
                        break;

                    case 'Z':
                        //chinese pinyin e.g. 'zhao'
                        if (workingString[current + 1] == 'H')
                        {
                            metaphoneData.Add("J");
                            current += 2;
                            break;
                        }
                        else if (StringAt(workingString, (current + 1), "ZO", "ZI", "ZA") || (isSlavoGermanic && ((current > 0) && workingString[current - 1] != 'T')))
                        {
                            metaphoneData.Add("S", "TS");
                        }
                        else
                        {
                            metaphoneData.Add("S");
                        }

                        if (workingString[current + 1] == 'Z')
                        {
                            current += 2;
                        }
                        else
                        {
                            current += 1;
                        }
                        break;

                    default:
                        current += 1;
                        break;
                }
            }

            return prefix + metaphoneData.ToString();
        }


        static bool IsVowel(char self)
        {
            return (self == 'A') || (self == 'E') || (self == 'I') || (self == 'O') || (self == 'U') || (self == 'Y');
        }


        static bool StartsWith(string self, StringComparison comparison, params string[] strings)
        {
            foreach (string str in strings)
            {
                if (self.StartsWith(str, comparison))
                {
                    return true;
                }
            }
            return false;
        }

        static bool StringAt(string self, int startIndex, params string[] strings)
        {
            if (startIndex < 0)
            {
                startIndex = 0;
            }
            foreach (string str in strings)
            {
                if (self.IndexOf(str, startIndex, StringComparison.OrdinalIgnoreCase) >= startIndex)
                {
                    return true;
                }
            }
            return false;
        }


        class MetaphoneData
        {
            readonly StringBuilder _primary = new StringBuilder(5);
            readonly StringBuilder _secondary = new StringBuilder(5);


            #region Properties

            internal bool Alternative { get; set; }
            internal int PrimaryLength
            {
                get
                {
                    return _primary.Length;
                }
            }

            internal int SecondaryLength
            {
                get
                {
                    return _secondary.Length;
                }
            }

            #endregion


            internal void Add(string main)
            {
                if (main != null)
                {
                    _primary.Append(main);
                    _secondary.Append(main);
                }
            }

            internal void Add(string main, string alternative)
            {
                if (main != null)
                {
                    _primary.Append(main);
                }

                if (alternative != null)
                {
                    Alternative = true;
                    if (alternative.Trim().Length > 0)
                    {
                        _secondary.Append(alternative);
                    }
                }
                else
                {
                    if (main != null && main.Trim().Length > 0)
                    {
                        _secondary.Append(main);
                    }
                }
            }

            public override string ToString()
            {
                string ret = (Alternative ? _secondary : _primary).ToString();
                //only give back 4 char metaph
                if (ret.Length > 4)
                {
                    ret = ret.Substring(0, 4);
                }

                return ret;
            }
        }
    }


    public class InvertedIndex
    {
        public int docs = 0;
        public SerializableDictionary<string, List<int>> index;
        public SerializableDictionary<int, string> Externindex;
        public AltPorterStemmer stemmer = new AltPorterStemmer();

        public InvertedIndex()
        {
            index = new SerializableDictionary<string, List<int>>();
            Externindex = new SerializableDictionary<int, string>();
        }
        public InvertedIndex(string path)
        {
            Load(path);

        }
        public void Load(string path)
        {
            System.IO.StreamReader R = null;
            if (File.Exists (path))
            {
                try
                {
                    System.Xml.Serialization.XmlSerializer S = new System.Xml.Serialization.XmlSerializer(typeof(InvertedIndex));
                    R = new System.IO.StreamReader(path);
                    InvertedIndex F = (InvertedIndex)S.Deserialize(R);

                    R.Close();
                    R=null;
                    this.index = F.index;
                    this.Externindex = F.Externindex;
                    this.docs = F.docs;
                }
                catch
                {
                }
                if (R != null) R.Close();
            }

        }
        public void Save(string path)
        {
            System.IO.StreamWriter W = null;
            if (Externindex.Count > 0)
                docs = Externindex.Count;
            try
            {
                W = new System.IO.StreamWriter(path);
                System.Xml.Serialization.XmlSerializer S = new System.Xml.Serialization.XmlSerializer(typeof(InvertedIndex));
                S.Serialize(W, this);

                W.Close();
            }
            catch
            {
                if (W != null) W.Close();
            }
        }

        public void Add(string key, int record_index)
        {
            if (key.Length == 0) return;
            if (index.ContainsKey(key))
            {
                index[key].Add(record_index);
            }
            else
            {
                index.Add(key, new List<int>() { record_index });
            }
        }
        public void Remove(string key)
        {
            index.Remove(key);
        }
        public void Remove(int record_index)
        {
            foreach (string key in index.Keys)
            {
                if (index[key].Contains(record_index))
                {
                    index[key].Remove(record_index); 
                } 
            }
        }

        public void AddDoc(string keys, int record_index)
        {
            Regex rgx = new Regex("[^a-zA-Z0-9 -]");
            keys = rgx.Replace(keys, " ");

            var terms = keys.Split(' ').ToList();
            foreach (string term in terms)
            {
                Add(term, record_index);
            }
            docs++;
        }
        public void RemoveDoc( int record_index)
        {
            Remove(record_index);
            if (docs > 0) docs--;
        }

        public void AddExternDoc(string keys, string docPath)
        {
            int record_index = docPath.GetHashCode();
            AddDoc(keys, record_index);
            if (Externindex.ContainsKey(record_index))
            {
                Externindex[record_index] = docPath;
            }
            else
            {
                Externindex.Add(record_index, docPath);
            }

        }

        public void RemoveExternDoc(string docPath)
        {
            int record_index = docPath.GetHashCode();
            Remove(record_index);
            if (docs > 0) docs--;
        }

        public void LoadGraphMap(List<string> graphMap)
        {
            Regex rgx = new Regex("[^a-zA-Z0-9 -]"); 

            foreach (string frag in graphMap)
            {
                XmlDocument xfrag = new XmlDocument();
                xfrag.LoadXml(frag);
                if (xfrag.DocumentElement.Name == "ser")
                {
                    string categoryPath = "";

                    categoryPath = xfrag.DocumentElement.Attributes["path"].Value;
                    if ((categoryPath == null) || (categoryPath.Length == 0)) continue;
                    categoryPath = categoryPath.Trim();
                    if ((categoryPath == null) || (categoryPath.Length == 0)) continue;
                    string categoryTemplate = "";
                    categoryTemplate = xfrag.InnerText;
                    string keys = " " +categoryPath + categoryTemplate+" ";
                    keys = keys.ToLower();
                    // our AIML stop list
                    keys = rgx.Replace(keys, " ");
                    keys = keys.Replace (" ser "," ");
                    keys = keys.Replace("pattern", " ");
                    keys = keys.Replace("state", " ");
                    keys = keys.Replace("category", " ");
                    keys = keys.Replace("template", " ");
                    keys = keys.Replace("srai", " ");
                    keys = keys.Replace("random", " ");
                    keys = keys.Replace(" li ", " ");
                    keys = keys.Replace(" path ", " ");
                    keys = keys.Replace(" name ", " ");
                    keys = keys.Replace(" enqueue ", " ");
                    keys = keys.Replace(" a ", " ");
                    keys = keys.Replace(" the ", " ");
                    keys = keys.Replace(" this ", " ");
                    keys = keys.Replace(" that ", " ");
                    keys = keys.Replace(" an ", " ");
                    keys = keys.Replace(" of ", " ");
                    keys = keys.Replace(" to ", " ");
                    keys = keys.Replace(" for ", " ");
                    keys = keys.Replace(" on ", " ");
                    keys = keys.Replace(" in ", " ");
                    keys = keys.Replace(" it ", " ");
                    keys = keys.Replace(" topic ", " ");
                    keys = keys.Replace(" and ", " ");
                    keys = keys.Replace(" from ", " ");
                    keys = keys.Replace(" with ", " ");
                    keys = keys.Replace(" eval ", " ");
                    keys = keys.Replace(" uppercase ", " ");
                    keys = keys.Replace(" get ", " ");
                    keys = keys.Replace(" set ", " ");
                    keys = keys.Replace(" say ", " ");
                    keys = keys.Replace(" think ", " ");
                    keys = keys.Replace(" am ", " ");
                    keys = keys.Replace(" blank ", " ");

                    while (keys.Contains("  ")) { keys = keys.Replace("  ", " "); }

                    keys = keys.Trim();

                    string [] ptokens = keys.Split (' ');
                    foreach (string x in ptokens)
                    {
                        string y = stemmer.Stem(x);
                        if (y != x) { keys += " " + y; }
                        string z=DoubleMetaphoneStringExtension.GenerateDoubleMetaphone(x);
                        keys += " " + z;
                    }
                    AddExternDoc(keys, categoryPath);
                }
            }
            if (Externindex.Count > 0)
                docs = Externindex.Count;
        }

        public List<int> Search(string query)
        {
            Regex rgx = new Regex("[^a-zA-Z0-9 -]");
            query = rgx.Replace(query, " ");
            var terms = parse(query);
            var result = performSearch(terms);
            return result;
        }

        public List<string> parse(string query)
        {
            var terms = query.Split(' ').ToList();
            if (terms.Count > 1) terms.Add(query);
            foreach (string x in terms)
            {
                string y = stemmer.Stem(x);
                if (y != x) { terms.Add(y); }
                string z = DoubleMetaphoneStringExtension.GenerateDoubleMetaphone(x);
                terms.Add(z);
            }

            return terms;
        }
        public double idf(string term)
        {
            double df = 1;
            if (index.ContainsKey(term))
            {
                df += index[term].Count;
            }
            double idfv = Math.Log(docs / df);
            return idfv;
        }

        public double tf(string term,int record_index)
        {
            double tfv=0.001;
            if (index.ContainsKey(term))
            {
                if (index[term].Contains(record_index ))
                {
                    tfv = 1;
                }
            }
            return tfv;
        }
       
        
        public List<int> performSearch(List<string> terms)
        {
            var result = new List<int>();
            var subResult = new List<int>();
            foreach (string term in terms)
            {
                var termResult = searchTerm(term);
                subResult.AddRange(termResult);
            }
            result = subResult.Distinct().ToList();
            return result;
        }

        public Dictionary<string, double> performTfIdfSearch(List<string> terms)
        {

            Dictionary<int, double> recResult = performTfIdfSearchRecs(terms);
            Dictionary<string, double> pathResults = new Dictionary<string, double>();
            foreach (int rec in recResult.Keys )
            {
                if (Externindex .ContainsKey(rec))
                {
                pathResults.Add(Externindex[rec],recResult[rec]);
                }
            }
            return pathResults;
        }

        public Dictionary<int,double> performTfIdfSearchRecs(List<string> terms)
        {
            var result = new Dictionary<int, double>();
            if (Externindex.Count > 0)
                docs = Externindex.Count;

            foreach (string term in terms)
            {
                double idfv = idf(term);
                var termResult = searchTerm(term);
                foreach (int recid in termResult)
                {
                    //double tfv = tf(term, recid);
                    //double tfidfv = tfv * idfv;
                    double tfidfv = idfv;
                    if (result.ContainsKey(recid))
                    {
                        result[recid] = result[recid] + tfidfv;
                    }
                    else
                    {
                        result[recid] = tfidfv;
                    }
                }
            }
            return result;
        }

        public List<int> searchTerm(string term)
        {
            var result = new List<int>();
            if (index.ContainsKey(term))
            {
                result = index[term];
            }
            return result;
        }

    }
}
