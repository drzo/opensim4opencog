using System;
using System.Collections.Generic;
using System.Collections;
using System.Globalization;
using System.Text;
using System.IO;
using System.Xml;
using System.Xml.XPath;
using AltAIMLParser;
using LAIR.Collections.Generic;
using LAIR.ResourceAPIs.WordNet;
using Lucene.Net.Analysis;
using Lucene.Net.Analysis.Standard;
using Lucene.Net.Documents;
using Lucene.Net.Index;
using Lucene.Net.QueryParsers;
using Lucene.Net.Search;
using Lucene.Net.Store;
using MushDLR223.ScriptEngines;
using MushDLR223.Utilities;
using MushDLR223.Virtualization;
using RTParser.Utils;
using RTParser.Variables;

namespace RTParser.Database
{
    /// <summary>
    /// Delegate to the Wordnet data expander (walks up the Undisambiguated Hyponyms)
    /// </summary>
    /// <param name="inputString"></param>
    /// <param name="queryhook"></param>
    /// <returns></returns>
    public delegate string WordExpander(string inputString, bool queryhook);

    public class MyLuceneIndexer : IEnglishFactiodEngine, IDocSearch
    {
        private const string DOC_ID_FIELD_NAME = "ID_FIELD";
        private const string HYPO_FIELD_NAME = "HYPO_FIELD";

        private readonly string _fieldName;
        private readonly string _indexDir;
        private ulong _docid = 0;
        //private static System.IO.FileInfo _path;
        public WordNetEngine wordNetEngine;
        public AltBot TheBot;
        public bool userCached = false;
        public bool userCachedPending = false;

        /// <summary>
        ///   Assert Redundancy Checks (for loading multple factiods from files
        /// </summary>
        public bool AssertReducndancyChecks = true;
        private readonly Dictionary<string, ulong> allContentIdPairs = new Dictionary<string, ulong>();

        Lucene.Net.Store.Directory _directory;// = new RAMDirectory();
        readonly Analyzer _analyzer;// = new StandardAnalyzer();

        private readonly IEntityFilter EntityFilter;
        private Hashtable WNExpandCache = new Hashtable();

        public readonly TripleStoreFromEnglish TripleStoreProxy;

        public virtual object AskTextStringJustHere(string textstr, Request request)
        {
            string said;
            string tolang;
            if (!TextPatternUtils.SplitOff(textstr, "-", out tolang, out said))
            {
                tolang = "";
                said = textstr;
            }
            string res = tolang;
            var allResults = new List<ISearchResult>();
            foreach (ISearchResult re in Search(said, null))
            {
                allResults.Add(re);
                res += " " + re.ToString();
            }
            if (allResults.Count == 0) return tolang;
            return res.Trim();
        }

        /// <summary>
        /// @askall what is 1 plus 1?
        /// </summary>
        /// <param name="textstr"></param>
        /// <param name="request"></param>
        /// <returns></returns>
        public virtual object AskTextStringAll(string textstr, Request request)
        {
            string said;
            string tolang;
            if (!TextPatternUtils.SplitOff(textstr, "-", out tolang, out said))
            {
                tolang = "";
                said = textstr;
            }
            string res = tolang;
            var allResults = new List<ISearchResult>();
            foreach (IDocSearch searchSource in SearchSources)
            {
                
                try
                {
                    foreach (ISearchResult re in searchSource.Search(said, null))
                    {
                        allResults.Add(re);
                        res += " " + re.ToString();
                    }
                }
                catch (Exception e)
                {
                    writeToLog("ERROR: " + e);
                }
            }
            if (allResults.Count == 0) return tolang;
            return res.Trim();
        }

        /// <summary>
        /// 
        /// </summary>
        /// <param name="indexDir"></param>
        /// <param name="fieldName">usually "TEXT_MATTER"</param>
        public MyLuceneIndexer(string indexDir, string fieldName, AltBot myBot, WordNetEngine myWNEngine)
        {
            SearchSources = new List<IDocSearch>() { this };
            _indexDir = indexDir;
            _fieldName = fieldName;
            TheBot = myBot;
            wordNetEngine = myWNEngine;
            IEnglishFactiodEngine assertTo = this;
            SearchSources.Add(new TrueKnowledgeFactiodEngine(assertTo, TheBot));
            SearchSources.Add(new AskDotComFactiodEngine(assertTo, TheBot));
            SearchSources.Add(new WikiAnswersFactoidEngine(assertTo, TheBot));
            TheBot.AddExcuteHandler("asklucene", AskTextStringJustHere);
            TheBot.AddExcuteHandler("askall", AskTextStringAll);
            //WNUser2Cache();

            //_path = new System.IO.FileInfo(indexDir);
            //_directory = new RAMDirectory(); 
            _analyzer = new StandardAnalyzer();
            try
            {
                InitDatabase();
            }
            catch (Exception e)
            {
                writeToLog("ERROR {0}", e);
                IsDbPresent = false;
                if (_directory == null)
                {
                    // in ram because of disk error?!
                    _directory = new RAMDirectory();
                }
            }
            TripleStoreProxy = new TripleStoreFromEnglish(this, TheBot, WordNetExpand);
            EntityFilter = TripleStoreProxy.EntityFilter;
        }

        private void InitDatabase()
        {
            if (IndexReader.IndexExists(_indexDir))
            {
                try
                {
                    _directory = FSDirectory.GetDirectory(_indexDir, false);
                    IndexReader indexReader = IndexReader.Open(_indexDir);
                    _docid = (ulong)indexReader.NumDocs() + 1;
                    indexReader.Close();
                }
                catch (Exception e)
                {
                    throw e;
                }
            }
            else
            {
                _directory = FSDirectory.GetDirectory(_indexDir, true);
            }


        }

        public ulong IncDocId()
        {
            lock (dbLock) return ++_docid;
        }

        private void checkDbLock()
        {
            // latter on we'll lock-check
            if (false) writeToLog("ERROR unlocked db!");
        }

        private R EnsureLockedDatabase<R>(Func<R> call)
        {
            lock (dbLock)
            {
                try
                {
                    return (R)call();
                }
                catch (Exception e)
                {
                    writeToLog("ERROR {0}", e);
                    return default(R);
                }
            }
        }

        /// <summary>
        /// This method indexes the content that is sent across to it. Each piece of content (or "document")
        /// that is indexed has to have a unique identifier (so that the caller can take action based on the
        /// document id). Therefore, this method accepts key-value pairs in the form of a dictionary. The key
        /// is a ulong which uniquely identifies the string to be indexed. The string itself is the value
        /// within the dictionary for that key. Be aware that stop words (like the, this, at, etc.) are _not_
        /// indexed.
        /// </summary>
        /// <param name="txtIdPairToBeIndexed">A dictionary of key-value pairs that are sent by the caller
        /// to uniquely identify each string that is to be indexed.</param>
        /// <returns>The number of documents indexed.</returns>
        public int Index(Dictionary<ulong, string> txtIdPairToBeIndexed, WordExpander expandWithWordNet)
        {
            return EnsureLockedDatabase(() => Index0(txtIdPairToBeIndexed, expandWithWordNet));
        }
        internal int Index0(Dictionary<ulong, string> txtIdPairToBeIndexed, WordExpander expandWithWordNet)
        {
            checkDbLock();
            bool indexExists = IndexReader.IndexExists(_directory);
            bool createIndex = !indexExists;
            IndexWriter indexWriter = new IndexWriter(_directory, _analyzer, createIndex);
            indexWriter.SetUseCompoundFile(false);

            Dictionary<ulong, string>.KeyCollection keys = txtIdPairToBeIndexed.Keys;

            foreach (ulong id in keys)
            {
                string text = txtIdPairToBeIndexed[id];

                Document document = new Document();
                // Raw text
                Field bodyField = new Field(_fieldName, text, Field.Store.YES, Field.Index.TOKENIZED);
                document.Add(bodyField);
                // Undisambiguated Hyponyms of nouns in text
                expandWithWordNet = expandWithWordNet ?? NoWordNetExpander;
                string wn_hypo = expandWithWordNet(text, true);
                Field wnHypoField = new Field(HYPO_FIELD_NAME, wn_hypo, Field.Store.YES, Field.Index.TOKENIZED);
                document.Add(wnHypoField);
                // The doc ID
                Field idField = new Field(DOC_ID_FIELD_NAME, (id).ToString(), Field.Store.YES, Field.Index.NOT_ANALYZED);
                document.Add(idField);

                indexWriter.AddDocument(document);
            }

            int numIndexed = indexWriter.DocCount();
            indexWriter.Optimize();
            indexWriter.Close();

            return numIndexed;
        }

        public int InsertFactiod(string myText, XmlNode templateNode, WordExpander expandWithWordNet)
        {
            return EnsureLockedDatabase(() => Insert0(myText, expandWithWordNet));
        }

        private int Insert0(string myText, WordExpander expandWithWordNet)
        {
            checkDbLock();
            if (AssertReducndancyChecks)
            {
                lock (allContentIdPairs)
                {
                    ulong cid;
                    if (allContentIdPairs.TryGetValue(myText.ToLower(), out cid))
                        return 0;
                }
            }

            ulong myDocID = IncDocId();
            Dictionary<ulong, string> contentIdPairs = new Dictionary<ulong, string>();
            contentIdPairs.Add(myDocID, myText);

            // Indexing:
            int numIndexed = Index(contentIdPairs, expandWithWordNet);

            // the null check is when not tracking we dont fill it up
            if (AssertReducndancyChecks)
            {
                lock (allContentIdPairs) allContentIdPairs.Add(myText.ToLower(), myDocID);
            }

            return numIndexed;
        }
        /// <summary>
        /// This method searches for the search query, then deletes the top ranked and inserts.
        /// </summary>
        /// <param name="query">The search term as a string that the caller wants to search for within the
        /// index as referenced by this object.</param>
        /// <param name="myText">The new value to replace in the database.</param>

        public int UpdateFactoid(string searchQuery, string myText, XmlNode templateNode)
        {
            return EnsureLockedDatabase(() => Update0(searchQuery, myText, WordNetExpand));
        }
        internal int Update0(string searchQuery, string myText, WordExpander expandWithWordNet)
        {
            checkDbLock();
            // Searching:
            ulong[] ids;
            string[] results;
            float[] scores;

            int numHits;
            // find it
            writeToLog("LUCENE:Replacing best \"{0}\"...", searchQuery);
            //Search(query, out ids, out results, out scores);
            IndexSearcher indexSearcher = new IndexSearcher(_directory);
            try
            {
                QueryParser queryParser = new QueryParser(_fieldName, _analyzer);
                Query query = queryParser.Parse(searchQuery);
                Hits hits = indexSearcher.Search(query);
                numHits = hits.Length();

                // if we want to do something smarter later
                //ids = new ulong[numHits];
                //results = new string[numHits];
                //scores = new float[numHits];
                //for (int i = 0; i < numHits; ++i)
                //{
                //    float score = hits.Score(i);
                //    string text = hits.Doc(i).Get(_fieldName);
                //    string idAsText = hits.Doc(i).Get(MyLuceneIndexer.DOC_ID_FIELD_NAME);
                //    ids[i] = UInt64.Parse(idAsText);
                //    results[i] = text;
                //    scores[i] = score;
                //}
                if (numHits > 0)
                {
                    indexSearcher.GetIndexReader().DeleteDocument(0);

                }

            }
            finally
            {
                indexSearcher.Close();
            }

            return Insert0(myText, expandWithWordNet);
        }

        public long LoadDocuments(string filename, XmlNode templateNode)
        {
            return EnsureLockedDatabase(() => LoadFileByLines0(filename, WordNetExpand));
        }
        internal long LoadFileByLines0(string filename, WordExpander expandWithWordNet)
        {
            checkDbLock();
            long totals = 0;
            string line;
            long linecount = 0;

            if (HostSystem.FileExists(filename))
            {
                string absoluteFileName = HostSystem.GetAbsolutePath(filename);
                System.IO.TextReader tr = new StreamReader(absoluteFileName);
                Dictionary<ulong, string> contentIdPairs = new Dictionary<ulong, string>();

                while ((linecount < 80000) && ((line = tr.ReadLine()) != null))
                {
                    linecount++;
                    if (linecount % 1000 == 0)
                    {
                        // batch a 1000
                        writeToLog("Lucene learn {0}", linecount);
                        int numIndexedb = Index(contentIdPairs, expandWithWordNet);
                        writeToLog("Indexed {0} lines.", numIndexedb);
                        totals += linecount;

                        contentIdPairs = new Dictionary<ulong, string>();
                    }
                    line = line.Trim();
                    if (line.Length != 0 && line[0] != '#')
                    {
                        contentIdPairs.Add(IncDocId(), line);
                    }
                }
                tr.Close();
                // Indexing:
                int numIndexed = Index(contentIdPairs, expandWithWordNet);
                writeToLog("Indexed {0} lines.", numIndexed);


                writeToLog("Last Line Mlearn {0}", linecount);
            }
            else
            {
                writeToLog(" LoadFileByLines cannot find file '{0}'", filename);
            }
            return totals;
        }


        /// <summary>
        /// This method searches for the search query, then deletes those with a score equal to the top ranked.
        /// </summary>
        /// <param name="query">The search term as a string that the caller wants to search for within the
        /// index as referenced by this object.</param>         
        public int DeleteTopScoring(string searchQuery, XmlNode templateNode, bool mustContainExact)
        {
            // If must contain the exact words (the defualt is false)
            // bool mustContainExact = false;
            bool tf;
            if (StaticXMLUtils.TryParseBool(templateNode, "exact", out tf))
            {
                mustContainExact = tf;
            }
            return DeleteTopScoring(searchQuery, mustContainExact);
        }
        public int DeleteTopScoring(string searchQuery, bool mustContainExact)
        {
            return EnsureLockedDatabase(() => DeleteTopScoring0(searchQuery, mustContainExact));
        }
        internal int DeleteTopScoring0(string searchQuery, bool mustContainExact)
        {
            checkDbLock();
            if (!IsDbPresent) return 0;
            // Searching:
            ulong[] ids;
            string[] results;
            float[] scores;

            int numHits;
            // find it
            writeToLog("Replacing best \"{0}\"...", searchQuery);
            //Search(query, out ids, out results, out scores);
            IndexSearcher indexSearcher = new IndexSearcher(_directory);
            int deleted = 0;
            try
            {
                QueryParser queryParser = new QueryParser(_fieldName, _analyzer);
                Query query = queryParser.Parse(searchQuery);
                Hits hits = indexSearcher.Search(query);
                string searchQueryToLower = searchQuery.ToLower();
                numHits = hits.Length();

                // if we want to do something smarter later
                ids = new ulong[numHits];
                results = new string[numHits];
                scores = new float[numHits];
                for (int i = 0; i < numHits; ++i)
                {
                    float score = hits.Score(i);
                    string text = hits.Doc(i).Get(_fieldName);
                    string idAsText = hits.Doc(i).Get(MyLuceneIndexer.DOC_ID_FIELD_NAME);
                    ids[i] = UInt64.Parse(idAsText);
                    results[i] = text;
                    scores[i] = score;
                }
                if (numHits > 0)
                {
                    //IndexReader indexReader = indexSearcher.GetIndexReader();
                    IndexWriter indexWriter = new IndexWriter(_directory, _analyzer);
                    float topscore = scores[0];
                    for (int i = 0; i < numHits; i++)
                    {
                        if (scores[i] == topscore)
                        {
                            if (mustContainExact)
                            {
                                // checks word order basically?
                                if (!results[i].ToLower().Contains(searchQueryToLower))
                                {
                                    writeToLog("Cannot or wont delete " + searchQueryToLower);
                                    continue;
                                }
                            }
                            writeToLog("DEBUG9 deleting " + searchQueryToLower);
                            //indexSearcher.GetIndexReader().DeleteDocument(i);
                            //indexReader.DeleteDocuments(new Term( MyLuceneIndexer.DOC_ID_FIELD_NAME, ids[i].ToString () ) );
                            indexWriter.DeleteDocuments(new Term(MyLuceneIndexer.DOC_ID_FIELD_NAME, ids[i].ToString()));
                            deleted++;
                        }
                    }
                    //indexReader.Commit();
                    //indexReader.Close();
                    indexWriter.Commit();
                    indexWriter.Close();
                }

            }
            finally
            {
                indexSearcher.Close();
            }
            return deleted;

        }

        public ICollection<ISearchResult> Search(string searchTerm, WordExpander expandWithWordNet)
        {
            lock (dbLock)
            {
                try
                {
                    bool expandOnNoHits = expandWithWordNet != null;
                    Object[] ids;
                    string[] results;
                    float[] scores;
                    int found = Search(searchTerm, out ids, out results, out scores, expandWithWordNet, expandOnNoHits);
                    var res = new List<ISearchResult>();
                    for (int i = 0; i < ids.Length; i++)
                    {
                        res.Add(new OneSearchResult(ids[i], results[i], scores[i]));
                    }
                    return res;
                }
                catch (Exception e)
                {
                    writeToLog("ERROR Search {0}", e);
                    return new ISearchResult[0];
                }
            }
        }
        /// <summary>
        /// This method searches for the search term passed by the caller.
        /// </summary>
        /// <param name="searchTerm">The search term as a string that the caller wants to search for within the
        /// index as referenced by this object.</param>
        /// <param name="ids">An out parameter that is populated by this method for the caller with docments ids.</param>
        /// <param name="results">An out parameter that is populated by this method for the caller with docments text.</param>
        /// <param name="scores">An out parameter that is populated by this method for the caller with docments scores.</param>
        internal int Search(string searchTerm, out Object[] ids, out string[] results, out float[] scores, WordExpander expandWithWordNet, bool expandOnNoHits)
        {
            checkDbLock();
            if (!IsDbPresent)
            {
                ids = new Document[0];
                results = new string[0];
                scores = new float[0];
                return 0;
            }
            IndexSearcher indexSearcher = new IndexSearcher(_directory);
            try
            {

                QueryParser queryParser = new QueryParser(_fieldName, _analyzer);
                Query query = queryParser.Parse(searchTerm);
                Hits hits = indexSearcher.Search(query);
                int numHits = hits.Length();

                ids = new Document[numHits];
                results = new string[numHits];
                scores = new float[numHits];

                for (int i = 0; i < numHits; ++i)
                {
                    float score = hits.Score(i);
                    var hdoc = hits.Doc(i);
                    string text = hdoc.Get(_fieldName);
                    //string idAsText = hdoc.Get(MyLuceneIndexer.DOC_ID_FIELD_NAME);
                    ids[i] = hdoc;
                    results[i] = text;
                    scores[i] = score;
                }

                if (numHits == 0 && expandOnNoHits)
                {
                    // Try expansion
                    //QueryParser queryParser = new QueryParser(_fieldName, _analyzer);
                    MultiFieldQueryParser queryParserWN = new MultiFieldQueryParser(
                        new string[] { _fieldName, MyLuceneIndexer.HYPO_FIELD_NAME },
                        _analyzer);
                    string hypo_expand = expandWithWordNet(searchTerm, false);
                    Query queryWN = queryParserWN.Parse(hypo_expand);
                    Hits hitsWN = indexSearcher.Search(queryWN);
                    int numHitsWN = hitsWN.Length();

                    ids = new Document[numHitsWN];
                    results = new string[numHitsWN];
                    scores = new float[numHitsWN];
                    for (int i = 0; i < numHitsWN; ++i)
                    {
                        float score = hitsWN.Score(i);
                        string text = hitsWN.Doc(i).Get(_fieldName);
                        //string idAsText = hitsWN.Doc(i).Get(MyLuceneIndexer.DOC_ID_FIELD_NAME);
                        ids[i] = hitsWN.Doc(i);// UInt64.Parse(idAsText);
                        results[i] = text;
                        scores[i] = score;
                    }

                }

            }
            finally
            {
                indexSearcher.Close();
            }
            return ids.Length;

        }/*
        internal int Search01(string searchTerm, out Document[] ids, out string[] results, out float[] scores, WordExpander expandWithWordNet, bool expandOnNoHits)
        {
            checkDbLock();
            if (!IsDbPresent)
            {
                ids = new Document[0];
                results = new string[0];
                scores = new float[0];
                return 0;
            }
            IndexSearcher indexSearcher = new IndexSearcher(_directory);
            try
            {

                QueryParser queryParser = new QueryParser(_fieldName, _analyzer);
                Query query = queryParser.Parse(searchTerm);
                Hits hits = indexSearcher.Search(query);
                int numHits = hits.Length();

                // Try expansion
                //QueryParser queryParser = new QueryParser(_fieldName, _analyzer);
                MultiFieldQueryParser queryParserWN = new MultiFieldQueryParser(
                    new string[] { _fieldName, MyLuceneIndexer.HYPO_FIELD_NAME },
                    _analyzer);
                string hypo_expand = expandWithWordNet(searchTerm, false);
                Query queryWN = queryParserWN.Parse(hypo_expand);
                Hits hitsWN = indexSearcher.Search(queryWN);
                int numHitsWN = hitsWN.Length();

                if ((numHitsWN == 0) || ((numHits > 0) && (hits.Score(0) > hitsWN.Score(0))))
                {
                    ids = new Document[numHits];
                    results = new string[numHits];
                    scores = new float[numHits];

                    for (int i = 0; i < numHits; ++i)
                    {
                        float score = hits.Score(i);
                        string text = hits.Doc(i).Get(_fieldName);
                        //string idAsText = hits.Doc(i).Get(MyLuceneIndexer.DOC_ID_FIELD_NAME);
                        ids[i] = hits.Doc(i);// UInt64.Parse(idAsText);
                        results[i] = text;
                        scores[i] = score;
                    }
                }
                else
                //if (numHits == 0 && expandOnNoHits)
                {

                    ids = new Document[numHitsWN];
                    results = new string[numHitsWN];
                    scores = new float[numHitsWN];
                    for (int i = 0; i < numHitsWN; ++i)
                    {
                        float score = hitsWN.Score(i);
                        string text = hitsWN.Doc(i).Get(_fieldName);
                        //string idAsText = hitsWN.Doc(i).Get(MyLuceneIndexer.DOC_ID_FIELD_NAME);
                        ids[i] = hitsWN.Doc(i);// UInt64.Parse(idAsText);
                        results[i] = text;
                        scores[i] = score;
                    }

                }

            }
            finally
            {
                indexSearcher.Close();
            }
            return ids.Length;

        }
        */
        private bool _IsDbPresent;
        readonly private object dbLock = new object();
        private bool ExtremeDebug;
        private readonly ICollection<IDocSearch> SearchSources;

        public bool IsDbPresent
        {
            get
            {
                lock (dbLock)
                {
                    IsDbPresent = _IsDbPresent || IndexReader.IndexExists(_directory);
                    return _IsDbPresent;
                }
            }
            set
            {
                lock (dbLock)
                {
                    if (!_IsDbPresent && value)
                    {
                        writeToLog("Noticing IsDbPresent");
                    }
                    _IsDbPresent = value;
                }
            }
        }

        public void WNUser2Cache()
        {
            try
            {
                if (userCached == false)
                {
                    userCachedPending = true;
                    string personDef = WordNetExpand("person", true);
                    WNExpandCache.Add(TheBot.BotUserID, personDef.Trim());
                    WNExpandCache[TripleStoreProxy.Entify(TheBot.BotUserID)] = personDef.Trim();
                    userCached = true;
                    userCachedPending = false;
                }
            }
            catch (Exception e)
            {
            }
        }

        public string MayPush(string text, XmlNode templateNode)
        {
            string[] textToLowerSplit = text.ToLower().Split(new[] { ' ' }, StringSplitOptions.RemoveEmptyEntries);
            if (textToLowerSplit.Length < 3)
            {
                writeToLog("ExcludedShort! '{0}' Len='{1}' ", text, textToLowerSplit.Length);
            }
            foreach (var words in textToLowerSplit)
            {
                if (IsExcludedSubject(words))
                {
                    writeToLog("ExcludedSRV '{0}' Subject='{1}' ", text, words);
                    return null;
                }
                if (IsExcludedValue(words))
                {
                    writeToLog("ExcludedSRV '{0}' Value='{1}' ", text, words);
                    return null;
                }
            }
            return text;
        }

        public string MayAsk(string text, XmlNode templateNode)
        {
            string[] textToLowerSplit = text.ToLower().Split(new[] { ' ' }, StringSplitOptions.RemoveEmptyEntries);
            if (textToLowerSplit.Length < 2)
            {
                writeToLog("ExcludedShort! '{0}' Len='{1}' ", text, textToLowerSplit.Length);
            }
            foreach (var words in textToLowerSplit)
            {
                if (IsExcludedSubject(words))
                {
                    writeToLog("ExcludedSRV '{0}' Subject='{1}' ", text, words);
                    return null;
                }
                if (IsExcludedValue(words))
                {
                    writeToLog("ExcludedSRV '{0}' Value='{1}' ", text, words);
                    return null;
                }
            }
            return text;
        }

        public string FixPronouns(string myText, XmlNode templateNode)
        {
            var dict = TheBot.GetDictionary(templateNode.LocalName);
            return TripleStoreProxy.FixPronouns(myText, dict == null ? (Func<string, string>) null : dict.grabSetting);
        }

        public string FixPronouns(string english, Func<string ,string> whoAmI)
        {
            return TripleStoreProxy.FixPronouns(english, whoAmI);
        }

        /// <summary>
        ///  Inspects the database for 
        /// searchTerm1
        ///     max  = 1,
        ///     failprefix = "",
        ///     wordnet = true,
        ///     threshold = 0.0f
        /// </summary>
        /// <param name="searchTerm1"></param>
        /// <param name="dbgLog"></param>
        /// <param name="OnFalure"></param>
        /// <param name="templateNode"></param>
        /// <param name="threshold"> &lt;dbquery&gt; uses 0.0f by default</param>
        /// <param name="expandOnNoHits"></param>
        /// <returns></returns>
        private string callDbQuery(string searchTerm1, OutputDelegate dbgLog, Func<Unifiable> OnFalure,
            XmlNode templateNode, float threshold, bool expandOnNoHits, out float reliablity)
        {
            // if synonyms is overriden?? the defualt is true
            bool expandWithWordNet = true;

            return AskQuery(searchTerm1, dbgLog, OnFalure, templateNode, threshold, expandWithWordNet, expandOnNoHits, out reliablity);
        }

        public string AskQuery(string searchTerm1, OutputDelegate dbgLog, Func<Unifiable> OnFalure, XmlNode templateNode, float threshold, bool expandWithWordNet, bool expandOnNoHits, out float reliablity)
        {
            lock (dbLock)
                return callDbQuery0(searchTerm1, dbgLog, OnFalure, templateNode, threshold, expandWithWordNet,
                                    expandOnNoHits, out reliablity);

        }

        public string callDbQuery0(string searchTerm1, OutputDelegate dbgLog, Func<Unifiable> OnFalure, XmlNode templateNode, float threshold, bool expandWithWordNet, bool expandOnNoHits, out float reliablity)
        {
            checkDbLock();
            bool tf;
            if (StaticXMLUtils.TryParseBool(templateNode, "wordnet,synonyms", out tf))
            {
                expandWithWordNet = tf;
            }
            WordExpander wordNetExpander = expandWithWordNet ? (WordExpander)WordNetExpand : NoWordNetExpander;
            string userFilter = "";
            // Do we only want responses with the current user name in it ?
            // As in "what is my favorite color?" 
            string onlyUserStr = AltBot.GetAttribValue(templateNode, "onlyUser", "false").ToLower();
            if (onlyUserStr.Equals("true"))
            {
                userFilter = TripleStoreProxy.Entify(TheBot.BotUserID);
            }
            string res = callDbQueryStatic(SearchSources, searchTerm1, dbgLog, templateNode, threshold, out reliablity,
                                           userFilter, wordNetExpander);
            if (OnFalure != null && string.IsNullOrEmpty(res)) return OnFalure();
            return res;
        }

        public string callDbQueryStatic(ICollection<IDocSearch> searchables, string searchTerm1, OutputDelegate dbgLog,
            XmlNode templateNode, float threshold,
            out float reliablity, string userFilter, WordExpander wordNetExpander)
        {
            reliablity = 0.0f;
            try
            {
                // if dbgLog == null then use /dev/null logger
                dbgLog = dbgLog ?? TextFilter.DEVNULL;
                // Searching:

                int numHits;

                string maxReplyStr = AltBot.GetAttribValue(templateNode, "max", "1").ToLower();
                int maxReply = Int16.Parse(maxReplyStr);
                
                string thresholdStr = AltBot.GetAttribValue(templateNode, "threshold", null);
                if (!string.IsNullOrEmpty(thresholdStr))
                {
                    float parsed;
                    if (float.TryParse(thresholdStr, out parsed))
                    {
                        threshold = parsed;
                    }
                }

                // Do we only want responses with the current user name in it ?
                // As in "what is my favorite color?" 
                var results = new List<ISearchResult>();
                dbgLog("Searching for the term \"{0}\"...", searchTerm1);
                foreach (var ss in searchables)
                {
                    results.AddRange(Search(searchTerm1, wordNetExpander));
                }

                numHits = results.Count;
                dbgLog("Number of hits == {0}.", numHits);

                float topScore = 0;
                int goodHits = 0;
                for (int i = 0; i < numHits; ++i)
                {
                    dbgLog("{0}) Doc-id: {1}; Content: \"{2}\" with score {3}.", i + 1, results[i].ID, results[i].Text, results[i].Score);
                    if (results[i].Text.Contains(userFilter))
                    {
                        if (results[i].Score >= threshold) { goodHits++; }
                        if (results[i].Score > topScore) { topScore = results[i].Score; }
                    }
                }

                //if (numHits > 0) topScore = scores[0];
                // Console.WriteLine();




                if ((goodHits > 0) && (topScore >= threshold))
                {
                    // should be weighted but lets just use the highest scoring
                    string reply = "";
                    int numReturned = 0;
                    if (goodHits < maxReply) maxReply = goodHits;
                    for (int i = 0; ((i < numHits) && (numReturned < maxReply)); i++)
                    {
                        if (results[i].Text.Contains(userFilter) && (results[i].Score >= topScore))
                        {
                            reply = reply + " " + results[i].Text;
                            numReturned++;
                            reliablity = topScore;
                        }
                    }
                    Unifiable converseMemo = reply.Trim();
                    dbgLog(" reply = {0}", reply);
                    return converseMemo;
                }
                else
                {
                    return null;
                }

            }
            catch
            {
                return Unifiable.Empty;
            }
        }

        internal string NoWordNetExpander(string inputstring, bool queryhook)
        {
            var result = WordNetExpand0(inputstring, queryhook);
            if (ExtremeDebug) writeToLog("NoWordNetExpander('{0}','{1}') ==> '{2}'", inputstring, queryhook, result);
            return result;
        }

        internal string WordNetExpand(string inputstring, bool queryhook)
        {
            var result = WordNetExpand0(inputstring, queryhook);
            if (ExtremeDebug) writeToLog("WordNetExpand('{0}','{1}') ==> '{2}'", inputstring, queryhook, result);
            return result;
        }


        public string WordNetExpand0(string inputString, bool queryhook)
        {
            string[] words = inputString.Split(' ');
            string returnText = inputString + " ";

            if (userCachedPending == false) WNUser2Cache();
            int numWords = words.Length;
            // Ok, lets try WordNet
            //WordNetEngine ourWordNetEngine = this.user.bot.wordNetEngine;
            WordNetEngine.POS ourPOS = WordNetEngine.POS.Noun;
            List<WordNetEngine.SynSetRelation> vlist = new List<WordNetEngine.SynSetRelation>(); //[2];
            //vlist.Add(WordNetEngine.SynSetRelation.Hypernym);
            //vlist.Add(WordNetEngine.SynSetRelation.InstanceHypernym);
            vlist.Add(WordNetEngine.SynSetRelation.Hyponym);
            vlist.Add(WordNetEngine.SynSetRelation.InstanceHyponym);



            // retrive synsets
            Set<SynSet> synStartSet = null;
            try { synStartSet = wordNetEngine.GetSynSets("entity", ourPOS); }
            catch (Exception)
            {
                writeToLog("Invalid Start SynSet ID");
                return returnText;
            }

            for (int i = 0; i < numWords; i++)
            {
                string focusWord = words[i];
                string focusWordResults = "";
                if (WNExpandCache.Contains(focusWord))
                {
                    focusWordResults = (string)WNExpandCache[focusWord];
                }
                else
                {
                    Set<SynSet> synDestSet = null;
                    try { synDestSet = wordNetEngine.GetSynSets(focusWord, ourPOS); }
                    catch (Exception)
                    {
                        writeToLog("Invalid Dest SynSet ID");
                        continue;
                    }
                    int numlinks = 0;
                    if (synStartSet.Count > 0)
                    {
                        //WordNetEngine.SynSetRelation[] vlist = new WordNetEngine.SynSetRelation[2];
                        //vlist[0] = WordNetEngine.SynSetRelation.Hyponym;
                        //vlist[1] = WordNetEngine.SynSetRelation.InstanceHyponym;
                        foreach (SynSet synSrcSet in synStartSet)
                        {
                            foreach (SynSet synDstSet in synDestSet)
                            {
                                //synSets.Items.Add(synSet);
                                List<SynSet> linkageList = null;

                                linkageList = synSrcSet.GetShortestPathTo(synDstSet, vlist);
                                if ((linkageList != null) && (linkageList.Count > 0))
                                {
                                    foreach (SynSet s in linkageList)
                                    {
                                        StringBuilder desc = new StringBuilder();
                                        //desc.Append("{");
                                        bool prependComma = false;
                                        foreach (string word in s.Words)
                                        {
                                            desc.Append((prependComma ? ", " : "") + word);
                                            prependComma = true;
                                        }

                                        //desc.Append("}");

                                        //LinkBox.Items.Add(desc.ToString());
                                        focusWordResults = focusWordResults + " " + desc.ToString() + " ";
                                    }
                                    //LinkBox.Text = "true";
                                    numlinks++;
                                    //return;
                                }
                            }
                        }

                    }
                    WNExpandCache.Add(focusWord, focusWordResults.Trim()); //Add to Cache
                }
                returnText = returnText + " " + focusWordResults;
            }
            returnText = returnText.Trim();

            if (queryhook)
            {
                if (returnText.Contains("person")) { returnText = returnText + " who"; }
                if (returnText.Contains("imaginary_being")) { returnText = returnText + " who"; }
                if (returnText.Contains("causal_agent")) { returnText = returnText + " who"; }

                if (returnText.Contains("object")) { returnText = returnText + " what"; }
                if (returnText.Contains("location")) { returnText = returnText + " where"; }
                if (returnText.Contains("time_period")) { returnText = returnText + " when"; }
                if (returnText.Contains("amount")) { returnText = returnText + " how much how many"; }
                if (returnText.Contains("measure")) { returnText = returnText + "  how much how many"; }
                if (returnText.Contains("quantity")) { returnText = returnText + "  how much how many"; }

            }
            // filter out "stop concepts" which have a > 70% occurance and thus low info content
            returnText = returnText.Replace("entity", "");
            returnText = returnText.Replace("abstraction", "");
            returnText = returnText.Replace("abstract", "");
            returnText = returnText.Replace("unit", "");
            returnText = returnText.Replace("physical", "");
            returnText = returnText.Replace("yes", "");
            return returnText.Trim();
        }

        public int callDbPush(string myText, XmlNode expandWordnet)
        {
            // the defualt is true

            if (MayPush(myText, expandWordnet) == null)
            {
                return -1;
            }
            WordExpander expandWithWordNet = WordNetExpand;

            bool tf;
            if (StaticXMLUtils.TryParseBool(expandWordnet, "wordnet,synonyms", out tf))
            {
                expandWithWordNet = tf ? (WordExpander)WordNetExpand : NoWordNetExpander;
            }

            ulong myDocID = IncDocId();
            Dictionary<ulong, string> contentIdPairs = new Dictionary<ulong, string>();
            contentIdPairs.Add(myDocID, myText);

            // Indexing:
            int numIndexed = Index(contentIdPairs, expandWithWordNet);
            writeToLog("Indexed {0} docs.", numIndexed);
            return numIndexed;
        }

        public bool IsExcludedSubject(string words)
        {
            return EntityFilter.IsExcludedSubject(words);
        }

        public bool IsExcludedValue(string words)
        {
            return EntityFilter.IsExcludedValue(words);
        }

        private void writeToLog(string s, params object[] p)
        {
            //bool tempB = TheBot.IsLogging;
            //TheBot.IsLogging = true;
            //TheBot.writeToLog("LUCENE: " + s, p);
            //TheBot.IsLogging = tempB;
            s = TextPatternUtils.SafeFormat(s, p);

            if (s.ToUpper().Contains("EXCLUDE")) return;
            DLRConsole.DebugWriteLine("LUCENE: " + s);
        }
    }
}