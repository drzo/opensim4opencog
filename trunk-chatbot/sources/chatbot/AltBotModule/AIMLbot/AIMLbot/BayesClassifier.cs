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

    public class InvertedIndex
    {
        public int docs = 0;
        public SerializableDictionary<string, List<int>> index;
        public SerializableDictionary<int, string> Externindex;

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
