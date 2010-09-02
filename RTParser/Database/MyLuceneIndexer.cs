using System;
using System.Collections.Generic;
using System.Text;
using System.IO;
using System.Text.RegularExpressions;
using System.Xml;
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

namespace RTParser.Database
{
    /// <summary>
    /// Delegate to the Wordnet data expander (walks up the Undisambiguated Hyponyms)
    /// </summary>
    /// <param name="inputString"></param>
    /// <param name="queryhook"></param>
    /// <returns></returns>
    public delegate string WordNetExpander(string inputString, bool queryhook);

    public class MyLuceneIndexer
    {
        private const string DOC_ID_FIELD_NAME = "ID_FIELD";
        private const string HYPO_FIELD_NAME = "HYPO_FIELD";

        private readonly string _fieldName;
        private readonly string _indexDir;
        private ulong _docid=0;
        //private static System.IO.FileInfo _path;
        public WordNetEngine wordNetEngine;
        public RTPBot TheBot;

        /// <summary>
        ///   Assert Redundancy Checks (for loading multple factiods from files
        /// </summary>
        public bool AssertReducndancyChecks;
        private readonly Dictionary<string, ulong> allContentIdPairs = new Dictionary<string, ulong>();

        Lucene.Net.Store.Directory _directory;// = new RAMDirectory();
        readonly Analyzer _analyzer;// = new StandardAnalyzer();

        private readonly HashSet<string> ExcludeRels = new HashSet<string>();


        private readonly HashSet<string> ExcludeVals = new HashSet<string>();

        private void AddDefaultExclusions()
                                                  {
            foreach (string str in
                new[]
                    {
                                                      "topic",
                                                      "he",
                                                      "she",
                                                      "name",
                                                      "id",
                                                      "username",
                                                      "userid",
                                                      "they",
                        "",
                                                      "it",
                        // because  "$user emotion is $value" should be "$bot feels $value emotion towards $user"
                        "emotion", 
                    })
                                                           {
                AddExcludedRelation(str);
            }
            foreach (string str in
                new[]
                    {
                        "",
                                                               "unknown_user",
                                                               "unknown.*",
                    })
            {
                AddRegex(ExcludeVals, str);
            }
        }

        /// <summary>
        /// 
        /// </summary>
        /// <param name="indexDir"></param>
        /// <param name="fieldName">usually "TEXT_MATTER"</param>
        public MyLuceneIndexer(string indexDir, string fieldName)
        {
            _indexDir = indexDir;
            _fieldName = fieldName;
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
            AddDefaultExclusions();
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
                catch(Exception e)
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
                    return (R) call();
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
        public int Index(Dictionary<ulong, string> txtIdPairToBeIndexed, WordNetExpander expandWithWordNet)
        {
            return EnsureLockedDatabase(() => Index0(txtIdPairToBeIndexed, expandWithWordNet));
        }
        internal int Index0(Dictionary<ulong, string> txtIdPairToBeIndexed, WordNetExpander expandWithWordNet)
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


        public int Insert(string myText, WordNetExpander expandWithWordNet)
        {
            return EnsureLockedDatabase(() => Insert0(myText, expandWithWordNet));
        }
        private int Insert0(string myText, WordNetExpander expandWithWordNet)
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

        public int Update(string searchQuery, string myText, XmlNode templateNode)
        {
            return EnsureLockedDatabase(() => Update0(searchQuery, myText, WordNetExpand));
        }
        internal int Update0(string searchQuery, string myText, WordNetExpander expandWithWordNet)
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


        public void LoadFileByLines(string filename, XmlNode templateNode)
        {
            EnsureLockedDatabase(() => LoadFileByLines0(filename, WordNetExpand));
        }
        internal long LoadFileByLines0(string filename, WordNetExpander expandWithWordNet)
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
        public int DeleteTopScoring(string searchQuery, XmlNode templateNode)
        {
            // If must contain the exact words (the defualt is false)
            bool mustContainExact = false;
            bool tf;
            if (Unifiable.TryParseBool(RTPBot.GetAttribValue(templateNode, "exact", "" + mustContainExact), out tf))
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
                            indexWriter.DeleteDocuments(new Term( MyLuceneIndexer.DOC_ID_FIELD_NAME, ids[i].ToString () ) );
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
        /// <summary>
        /// This method searches for the search term passed by the caller.
        /// </summary>
        /// <param name="searchTerm">The search term as a string that the caller wants to search for within the
        /// index as referenced by this object.</param>
        /// <param name="ids">An out parameter that is populated by this method for the caller with docments ids.</param>
        /// <param name="results">An out parameter that is populated by this method for the caller with docments text.</param>
        /// <param name="scores">An out parameter that is populated by this method for the caller with docments scores.</param>
        public int Search(string searchTerm, out ulong[] ids, out string[] results, out float[] scores, WordNetExpander expandWithWordNet)
        {
            lock (dbLock)
            {
                try
                {
                    return Search0(searchTerm, out ids, out results, out scores, expandWithWordNet);
                }
                catch (Exception e)
                {
                    writeToLog("ERROR Search {0}", e);
                    ids = new ulong[0];
                    results = new string[0];
                    scores = new float[0];
                    return 0;
                }
            }
        }
        internal int Search0(string searchTerm, out ulong[] ids, out string[] results, out float[] scores, WordNetExpander expandWithWordNet)
        {
            checkDbLock();
            if (!IsDbPresent)
            {
                ids = new ulong[0];
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

                if (numHits == 0)
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

                    ids = new ulong[numHitsWN];
                    results = new string[numHitsWN];
                    scores = new float[numHitsWN];
                    for (int i = 0; i < numHitsWN; ++i)
                    {
                        float score = hitsWN.Score(i);
                        string text = hitsWN.Doc(i).Get(_fieldName);
                        string idAsText = hitsWN.Doc(i).Get(MyLuceneIndexer.DOC_ID_FIELD_NAME);
                        ids[i] = UInt64.Parse(idAsText);
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

        private bool _IsDbPresent;
        readonly private object dbLock = new object();
        private bool ExtremeDebug;

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

        public int assertTriple(string subject, string relation, string value)
        {
            string factoidSRV = String.Format("{0} {1} is {2}", subject, relation, value);
            string prefix = string.Format("assertTriple {0}", factoidSRV);
            if (IsExcludedSRV(subject, relation, value, prefix, writeToLog)) return -1;
            writeToLog(prefix);
            return Insert(factoidSRV, WordNetExpand);
        }

        public int retractTriple(string subject, string relation, string value)
        {
            if (!IsDbPresent) return 0;
            string factoidSRV = String.Format("{0} {1} is {2}", subject, relation, value);
            string prefix = string.Format("retractTriple {0}", factoidSRV);
            if (IsExcludedSRV(subject, relation, value, prefix, writeToLog)) return -1;
            writeToLog(prefix);
            return DeleteTopScoring(factoidSRV, true);
        }

        public int retractAllTriple(string subject, string relation)
        {
            if (!IsDbPresent) return 0;
            string factoidSR = String.Format("{0} {1} is {2}", subject, relation, "");
            string prefix = string.Format("retractAllTriple {0}", factoidSR);
            if (IsExcludedSRV(subject, relation, subject, prefix, writeToLog)) return -1;
            writeToLog(prefix);
            return DeleteTopScoring(factoidSR, true);
        }

        public int updateTriple(string subject, string relation, string value)
        {
            string factoidSRV = String.Format("{0} {1} is {2}", subject, relation, value);
            string prefix = string.Format("updateTriple {0}", factoidSRV);
            if (IsExcludedSRV(subject, relation, value, prefix, writeToLog)) return -1;
            string factoidSR = String.Format("{0} {1} is", subject, relation);
            return EnsureLockedDatabase(() =>
                                            {
                                                int deleted = DeleteTopScoring0(factoidSR, true);
                                                return deleted + Insert0(factoidSRV, WordNetExpand);
                                            });
        }

        public String queryTriple(string subject, string relation, XmlNode templateNode)
        {
            string factoidSR = String.Format("{0} {1} is", subject, relation);
            string prefix = string.Format("queryTriple {0}", factoidSR);
            if (IsExcludedSRV(subject, relation, subject, prefix, writeToLog)) return String.Empty;

            string result = callDbQuery(factoidSR, writeToLog, (any) => null, templateNode);
            if (!string.IsNullOrEmpty(result) && result.ToLower().StartsWith(factoidSR.ToLower()))
            {
                writeToLog("Success! queryTriple {0}, {1} => {2}", subject, relation, result);
                return result.Substring(factoidSR.Length).Trim();
            }
            writeToLog("queryTriple {0}, {1} => '{2}' (returning String.Empty)", subject, relation, result);
            return String.Empty;
        }
         
        public int callDbPush(string myText, XmlNode expandWordnet)
        {
            // the defualt is true
            WordNetExpander expandWithWordNet = WordNetExpand;

            bool tf;
            if (Unifiable.TryParseBool(RTPBot.GetAttribValue(expandWordnet,"wordnet,synonyms", "" + expandWithWordNet), out tf))
        {
                expandWithWordNet = tf ? (WordNetExpander) WordNetExpand : NoWordNetExpander;
            }

            ulong myDocID = IncDocId();
            Dictionary<ulong, string> contentIdPairs = new Dictionary<ulong, string>();
            contentIdPairs.Add(myDocID, myText);

            // Indexing:
            int numIndexed = Index(contentIdPairs, expandWithWordNet);
            writeToLog("Indexed {0} docs.", numIndexed);
            return numIndexed;
        }

        public string callDbQuery(string searchTerm1, OutputDelegate dbgLog, Func<string, Unifiable> OnFalure, XmlNode templateNode)
        {
            // if synonyms is overriden?? the defualt is true
            bool expandWithWordNet = true;

            bool tf;
            if (Unifiable.TryParseBool(RTPBot.GetAttribValue(templateNode, "wordnet,synonyms", "" + expandWithWordNet),
                                       out tf))
            {
                expandWithWordNet = tf;
            }
            return callDbQuery(searchTerm1, dbgLog, OnFalure, templateNode, expandWithWordNet);
        }
        public string callDbQuery(string searchTerm1, OutputDelegate dbgLog, Func<string, Unifiable> OnFalure, XmlNode templateNode, bool expandWithWordNet)
        {
            return EnsureLockedDatabase(() => callDbQuery0(searchTerm1, dbgLog, OnFalure, templateNode, expandWithWordNet)); 
        }
        public string callDbQuery0(string searchTerm1, OutputDelegate dbgLog, Func<string, Unifiable> OnFalure, XmlNode templateNode, bool expandWithWordNet)
        {
            checkDbLock();
            WordNetExpander wordNetExpander = expandWithWordNet ? (WordNetExpander)WordNetExpand : NoWordNetExpander;
            try
            {
                // if dbgLog == null then use /dev/null logger
                dbgLog = dbgLog ?? TextFilter.DEVNULL;
                // Searching:
                ulong[] ids;
                string[] results;
                float[] scores;

                int numHits;

                string maxReplyStr = RTPBot.GetAttribValue(templateNode, "max", "1").ToLower();
                int maxReply = Int16.Parse(maxReplyStr);
                string failPrefix = RTPBot.GetAttribValue(templateNode, "failprefix", "").ToLower();
                string thresholdStr = RTPBot.GetAttribValue(templateNode, "threshold", "0").ToLower();
                float threshold = float.Parse(thresholdStr);

                dbgLog("Searching for the term \"{0}\"...", searchTerm1);
                Search0(searchTerm1, out ids, out results, out scores, wordNetExpander);
                numHits = ids.Length;
                dbgLog("Number of hits == {0}.", numHits);
                for (int i = 0; i < numHits; ++i)
                {
                    dbgLog("{0}) Doc-id: {1}; Content: \"{2}\" with score {3}.", i + 1, ids[i], results[i], scores[i]);
                }

                float topScore = 0;
                if (numHits > 0) topScore = scores[0];
                // Console.WriteLine();




                if ((numHits > 0) && (topScore >= threshold))
                {
                    // should be weighted but lets just use the highest scoring
                    string reply = "";
                    if (numHits < maxReply) maxReply = numHits;
                    for (int i = 0; i < maxReply; i++)
                    {
                        reply = reply + " " + results[i];
                    }
                    Unifiable converseMemo = reply.Trim();
                    dbgLog(" reply = {0}", reply);
                    
                    return converseMemo;
                }
                else
                {
                    return OnFalure(failPrefix);
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
            string [] words  = inputString.Split(' ');
            string returnText = inputString+" ";

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
                                    returnText = returnText + " " + desc.ToString()+ " ";
                                }
                                //LinkBox.Text = "true";
                                numlinks++;
                                //return;
                            }
                        }
                    }

                }
            }
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


        private bool IsExcludedSRV(string subject, string relation, string value, string prefix, OutputDelegate writeToLog)
        {
            bool ExcludedFactPattern = false;
            bool debug = (writeToLog != null && prefix != null);
            if (IsExcludedRelation(relation))
            {
                if (!debug) return true;
                writeToLog("ExcludedSRV: '{0}' Relation '{1}'", prefix, relation);
                if (!ExtremeDebug) return true;
                ExcludedFactPattern = true;
            }
            if (IsExcludedSubject(subject))
            {
                if (!debug) return true;
                writeToLog("ExcludedSRV: '{0}' Subject '{1}'", prefix, subject);
                if (!ExtremeDebug) return true;
                ExcludedFactPattern = true;
            }
            if (IsExcludedValue(value))
            {
                if (!debug) return true;
                writeToLog("ExcludedSRV: '{0}' Value '{1}'", prefix, value);
                ExcludedFactPattern = true;
            }
            return ExcludedFactPattern;
        }

        public bool IsExcludedSubject(string subject)
        {
            return IsExcludedValue(subject);
        }

        public bool IsExcludedRelation(string value)
        {
            return NullOrMatchInSet(ExcludeRels, value);
        }

        public void AddExcludedRelation(string value)
        {
            AddRegex(ExcludeRels, "^" + value + "$");
        }

        public void AddExcludedValue(string value)
        {
            AddRegex(ExcludeVals, "^" + value + "$");
        }

        private static void AddRegex(ICollection<string> rels, string value)
        {
            lock (rels)
            {
                value = value.ToLower();
                value = value.Replace("_", " ");                
                value = value.Replace("~", ".*");
                rels.Add(value);
            }
        }

        private static bool NullOrMatchInSet(ICollection<string> rels, string value)
        {
            if (string.IsNullOrEmpty(value))
            {
                if (rels == null) return true;
                lock (rels) return rels.Contains("") || rels.Contains("^$");                
            }
            if (rels == null) return false;
            lock (rels)
            {
                value = value.Replace("_", " ").ToLower();
                foreach (var rel in rels)
                {
                    if (Regex.IsMatch(value, rel)) return true;
                }
            }
            return false;
        }

        public bool IsExcludedValue(string value)
        {
            return NullOrMatchInSet(ExcludeVals, value);
        }

        private void writeToLog(string s, params object[] p)
        {
            //bool tempB = TheBot.IsLogging;
            //TheBot.IsLogging = true;
            //TheBot.writeToLog("LUCENE: " + s, p);
            //TheBot.IsLogging = tempB;
            DLRConsole.DebugWriteLine("LUCENE: " + s, p);
        }
    }



}