using System;
using System.Collections.Generic;
using System.Text;
using System.Data;
using System.IO;

namespace LAIR.ResourceAPIs.Wikipedia
{
    /// <summary>
    /// Class for reading/writing the FastMirror database. The FastMirror database is a transformation
    /// of the standard MediaWiki database schema to one that is more efficiently read and written.
    /// Page text is stored without markup and with sections, wiki-links, and term frequencies already 
    /// extracted and stored as fields in a table. The schemas:
    ///   CustomTables/page_index.sql
    ///   CustomTables/title_index.sql
    /// The first schema is for pages. The second table indexes words against the page titles that contain
    /// them - this provides quick keyword searching.
    /// </summary>
    public class FastMirror : WikiDB
    {
        private WikiDB _mainDB;            // connection to standard Wiki DB
        private bool _stopWriting;         // whether or not to stop writing
        private int _lastIDWritten;        // ID of last page written
        private string _lastTitleWritten;  // title of last page written

        #region properties
        /// <summary>
        /// Gets or sets whether or not to stop writing
        /// </summary>
        public bool StopWriting
        {
            get { return _stopWriting; }
            set { _stopWriting = value; }
        }

        /// <summary>
        /// Gets the last ID written
        /// </summary>
        public int LastIDWritten
        {
            get { return _lastIDWritten; }
        }

        /// <summary>
        /// Gets the last title written
        /// </summary>
        public string LastTitleWritten
        {
            get { return _lastTitleWritten; }
        }
        #endregion

        /// <summary>
        /// Get the main DB
        /// </summary>
        public WikiDB MainDB
        {
            get { return _mainDB; }
        }

        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="server">Server for mirror DB</param>
        /// <param name="database">Name of mirror DB</param>
        /// <param name="user">Authorized user</param>
        /// <param name="password">Authorized password</param>
        public FastMirror(string server, string database, string user, string password)
            : base(server, database, user, password)
        {
        }

        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="mainServer">Server for main Wikipedia DB</param>
        /// <param name="mainDatabase">Name of main Wikipedia DB</param>
        /// <param name="mainUser">Authorized user</param>
        /// <param name="mainPassword">Authorized password</param>
        /// <param name="mirrorServer">Server for mirror DB</param>
        /// <param name="mirrorDatabase">Name of mirror DB</param>
        /// <param name="mirrorUser">Authorized user</param>
        /// <param name="mirrorPassword">Authorized password</param>
        public FastMirror(string mainServer, string mainDatabase, string mainUser, string mainPassword,
                          string mirrorServer, string mirrorDatabase, string mirrorUser, string mirrorPassword)
            : this(mirrorServer, mirrorDatabase, mirrorUser, mirrorPassword)
        {
            _mainDB = new WikiDB(mainServer, mainDatabase, mainUser, mainPassword);
        }

        /// <summary>
        /// Connects to the mirror and main DBs
        /// </summary>
        public override void Connect()
        {
            base.Connect();

            if(_mainDB != null)
                _mainDB.Connect();
        }

        /// <summary>
        /// Disconnects from the mirror and main DBs
        /// </summary>
        public override void Disconnect()
        {
            base.Disconnect();

            if (_mainDB != null)
                _mainDB.Disconnect();
        }

        /// <summary>
        /// Looks up a page in the database (recovers sections)
        /// </summary>
        /// <param name="ns">Namespace to look page up in</param>
        /// <param name="title">Title of page to look up</param>
        /// <param name="followRedirection">Whether or not to look up destination of redirect pages</param>
        /// <returns>Page instance</returns>
        public override Page LookupPage(WikiDB.Namespace ns, string title, bool followRedirection)
        {
            return LookupPage(ns, title, followRedirection, true, true, true);
        }

        /// <summary>
        /// Looks up page in database
        /// </summary>
        /// <param name="ns">Namespace to look page up in</param>
        /// <param name="title">Title of page to look up</param>
        /// <param name="followRedirection">Whether or not to look up destination of redirect pages</param>
        /// <param name="recoverSections">Whether or not to recover the section structure of the page</param>
        /// <param name="readLinks">Read link information</param>
        /// <param name="readTFTable">Read term frequency table</param>
        /// <returns>Page instance</returns>
        public Page LookupPage(WikiDB.Namespace ns, string title, bool followRedirection, 
                               bool recoverSections, bool readLinks, bool readTFTable)
        {
            string url = GetURLFromPageTitle(title);

            // check page cache
            if (_pageCache.ContainsKey(url))
            {
                // see if page is dirty
                Page cached = (Page)_pageCache[url];
                bool dirty = (recoverSections && !cached.SectionsRecovered) ||
                             (readLinks && cached.WikiLinks.Count == 0) ||
                             (readTFTable && cached.TermFrequencies.Count == 0);
                if (!dirty)
                    return cached;
                else
                    _pageCache.Remove(url);
            }

            if (!CheckConnection(true))
                throw new Exception("Could not establish connection with Wikipedia database");

            Page p = null;

            url = MySQLEscape(url);
            int nsVal = NamespaceValue(ns);

            // get text and redirect page
            string selectCols = "page_id, page_text, redirects_to";
            if (recoverSections)
                selectCols += ", section_layout";

            string query = "SELECT " + selectCols + " FROM page " +
                           "WHERE page_namespace=" + nsVal + " AND page_title=\"" + url + "\"";
            IDataReader reader = SubmitQuery(query);

            if (reader.Read())
            {
                int id = int.Parse(reader["page_id"].ToString());
                string text = Encoding.UTF8.GetString((byte[])reader["page_text"]);
                string redirectsTo = reader["redirects_to"].ToString();

                // split into lines
                List<string> lines = new List<string>(text.Split(new char[] { '\n' }, StringSplitOptions.RemoveEmptyEntries));
                if (recoverSections)
                {
                    string sectionLayout = reader["section_layout"].ToString();
                    p = new Page(title, ns, id, lines, sectionLayout);
                }
                else
                {
                    p = new Page(title, ns, id);
                    p.Lines = lines;

                    // add a single section to the page
                    Section s = new Section("full page section", lines, null, p, 0, lines.Count - 1);
                    p.Sections.Add(s);
                }
                reader.Close();

                // check for page redirection
                Page redirectPage = null;
                if (redirectsTo != "" && followRedirection)
                    redirectPage = LookupPage(ns, redirectsTo, followRedirection, recoverSections, readLinks, readTFTable);

                p.RedirectsTo = redirectPage;
            }
            else
            {
                reader.Close();
                return null;
            }
            reader.Close();

            // get links
            if (readLinks)
            {
                query = "SELECT link_list FROM pagelinks " +
                        "WHERE page_namespace=" + nsVal + " AND page_title=\"" + url + "\"";
                reader = SubmitQuery(query);

                if (reader.Read())
                {
                    string linkList = Encoding.UTF8.GetString((byte[])reader["link_list"]);
                    string[] splitList = linkList.Split(new char[] { '\n' }, StringSplitOptions.RemoveEmptyEntries);

                    foreach (string link in splitList)
                    {
                        string[] entry = link.Split(' ');
                        if (entry.Length != 2)
                            throw new Exception("Invalid link entry");

                        string destPage = entry[0];
                        string sourceSectionID = recoverSections ? entry[1] : "1";
                        Section s = p.GetSection(sourceSectionID);
                        WikiLink wl = new WikiLink("[[" + destPage + "]]", destPage, "", destPage, s);
                        s.AddLink(wl);
                    }
                }
                reader.Close();
            }

            // get TFs
            if (readTFTable)
            {
                query = "SELECT freq_list FROM termfreqs WHERE page_namespace=" + nsVal + " AND page_title=\"" + url + "\"";
                reader = SubmitQuery(query);

                if (reader.Read())
                {
                    string freqList = Encoding.UTF8.GetString((byte[])reader["freq_list"]);
                    string[] splitList = freqList.Split(new char[] { '\n' }, StringSplitOptions.RemoveEmptyEntries);

                    // get freqs
                    for (int i = 0; i < splitList.Length; ++i)
                    {
                        string[] entry = splitList[i].Split(' ');
                        if (entry.Length != 2)
                            throw new Exception("Invalid frequency entry");

                        string word = entry[0];
                        float freq = float.Parse(entry[1]);

                        if (p.TermFrequencies.ContainsKey(word))
                            throw new Exception("Duplicate TF entry");

                        p.TermFrequencies[word] = freq;
                    }
                }
                reader.Close();
            }

            // add page to cache
            CachePage(p);

            return p;
        }

        /// <summary>
        /// Writes the titleindex table
        /// </summary>
        /// <param name="startID">Where to start writing</param>
        /// <param name="numPages">Total number of pages to write</param>
        /// <param name="blockSize">Number of pages to read at a time</param>
        /// <param name="ns">Namespace to read/write from/to</param> 
        /// <param name="includeRedirects">Whether or not to include redirect pages in the write indexing operation</param>
        /// <returns>Last title written</returns>
        public string WriteTitleIndex(int startID, int numPages, int blockSize, WikiDB.Namespace ns, bool includeRedirects)
        {
            if (_mainDB == null)
                throw new Exception("Main DB not connected.  Use the right constructor.");

            if (!_mainDB.Connected)
                throw new Exception("Not connected to main DB");

            if (!Connected)
                throw new Exception("Not connected to mirror DB");

            int nsVal = NamespaceValue(ns);
            int end = startID + numPages - 1;

            // check block size
            if (blockSize > numPages)
                blockSize = numPages;

            for (int i = startID; i <= end; i += blockSize)
            {
                List<string> titles;
                try
                {
                    // get titles
                    titles = _mainDB.GetTitleRange(ns, i, blockSize, includeRedirects);
                    WriteTitleIndex(ns, titles);
                }
                catch (Exception ex)
                {
                    throw new Exception("Failed at start ID " + i + ".  Error:  " + ex);
                }
            }

            return _lastTitleWritten;
        }

        /// <summary>
        /// Writes the titleindex table
        /// </summary>
        /// <param name="ns">Namespace to read/write from/to</param>        
        /// <param name="titles">List of titles to write</param>
        /// <returns>Last title written</returns>
        public string WriteTitleIndex(Namespace ns, List<string> titles)
        {
            int nsVal = NamespaceValue(ns);

            // process titles
            for (int j = 0; j < titles.Count; ++j)
            {
                string title = titles[j];
                if (title.Trim() == "")
                    continue;

                string[] words = title.Split(new char[] { '_', '\\', '/' }, StringSplitOptions.RemoveEmptyEntries);

                // process words in title
                for (int k = 0; k < words.Length; ++k)
                {
                    string word = words[k];
                    word = word.Trim().ToLower();
                    word = WikiMarkup.TrimPunctuation(word);

                    if (WikiMarkup.IsStopWord(word, true))
                        continue;

                    word = MySQLEscape(word);
                    title = MySQLEscape(title);

                    try
                    {
                        IDataReader reader = SubmitQuery("SELECT word FROM titleindex " +
                                                             "WHERE word=\"" + word + "\" AND page_namespace=" + nsVal + " AND page_title=\"" + title + "\"");

                        if (!reader.Read())
                        {
                            reader.Close();
                            ExecuteNonQuery("INSERT INTO titleindex (word, page_namespace, page_title) " +
                                            "VALUES (\"" + word + "\", " + nsVal + ", \"" + title + "\")");
                        }
                        reader.Close();
                        _lastTitleWritten = title;
                    }
                    catch (Exception ex)
                    {
                        throw new Exception("Failed at title " + title + ", error:  " + ex);
                    }
                }
            }

            return _lastTitleWritten;
        }

        /// <summary>
        /// Write page index table
        /// </summary>
        /// <param name="startID">Where to start writing</param>
        /// <param name="numPages">Total number of pages to write</param>
        /// <param name="blockSize">Number of pages to read at a time</param>
        /// <param name="ns">Namespace to read/write from/to</param> 
        /// <param name="includeRedirects">Whether or not to include redirect pages</param>
        /// <returns>Last title written</returns>
        public string WritePageIndex(int startID, int numPages, int blockSize, WikiDB.Namespace ns, bool includeRedirects)
        {
            if (_mainDB == null)
                throw new Exception("Main DB not connected.  Use the right constructor.");

            if (!_mainDB.CheckConnection(true))
                throw new Exception("Not connected to main DB");

            if (!CheckConnection(true))
                throw new Exception("Not connected to mirror DB");

            _stopWriting = false;

            int end = startID + numPages - 1;
            int i;
            
            // check block size
            if (blockSize > numPages)
                blockSize = numPages;

            for (i = startID; i <= end; i += blockSize)
            {
                if (_stopWriting)
                    break;

                // get titles
                List<string> titles = null;
                try
                {
                    titles = _mainDB.GetTitleRange(ns, i, blockSize, includeRedirects);
                    WritePageIndex(ns, titles);
                }
                catch (Exception ex)
                {
                    throw new Exception("Failed at start ID " + i + ".  Error:  " + ex);
                }
            }

            return _lastTitleWritten;
        }

        /// <summary>
        /// Write page index table
        /// </summary>
        /// <param name="ns">Namespace to read/write pages from/to</param>
        /// <param name="titles">List of page titles to write</param>
        /// <returns>Last title written</returns>
        public string WritePageIndex(Namespace ns, List<string> titles)
        {
            // process titles
            foreach (string title in titles)
            {
                if (_stopWriting)
                    break;

                try
                {
                    // check for page in mirror DB
                    if(PageExists(Namespace.Main, title))
                        continue;

                    // get page from main DB and write it to mirror
                    Page p = _mainDB.LookupPage(ns, title, false);

                    if (p == null)
                    {
                        StreamWriter logWriter = new StreamWriter("write_page_index_log.txt", true);
                        logWriter.WriteLine("Could not find page to write:  " + title);
                        logWriter.Close();
                    }
                    else
                    {
                        WritePage(p);
                        _lastTitleWritten = p.Title;
                        _lastIDWritten = p.ID;
                    }
                }
                catch (Exception ex)
                {
                    throw new Exception("Failed at title \"" + title + "\".  Error: " + ex);
                }
            }

            return _lastTitleWritten;
        }

        /// <summary>
        /// Writes a page to the mirror DB
        /// </summary>
        /// <param name="p">Page to write</param>
        public void WritePage(Page p)
        {
            if (!Connected)
                throw new Exception("Not connected to mirror DB");

            if (p == null)
                return;

            if (!PageExists(p.Namespace, p.Title))
            {
                // write page to database
                string piVals = p.PageInsertValues;
                string liVals = p.LinksInsertValues;
                string tfVals = p.TermFreqsValues;

                if (piVals != "")
                    ExecuteNonQuery("INSERT INTO page (page_namespace, page_title, page_text, section_layout, redirects_to) " + piVals);

                if (liVals != "")
                    ExecuteNonQuery("INSERT INTO pagelinks (page_namespace, page_title, link_list) " + liVals);

                if (tfVals != "")
                    ExecuteNonQuery("INSERT INTO termfreqs (page_namespace, page_title, freq_list) " + tfVals);
            }
            else
            {
                // update page
            }
        }

        /// <summary>
        /// Dumps a range of pages to a file
        /// </summary>
        /// <param name="startID">Where to start dumping</param>
        /// <param name="numPages">Total number of pages to dump</param>
        /// <param name="blockSize">Number of pages to read at a time</param>
        /// <param name="ns">Namespace to read/write from/to</param>        
        /// <param name="dumpDir">Directory to save dump files in</param>
        /// <param name="includeRedirects">Whether or not to dump redirect pages</param>
        /// <returns>Last title dumped</returns>
        public string DumpToLemur(int startID, int numPages, int blockSize, WikiDB.Namespace ns, string dumpDir, bool includeRedirects)
        {
            if (_mainDB == null)
                throw new Exception("Main DB not connected.  Use the right constructor.");

            if (!_mainDB.Connected)
                throw new Exception("Not connected to main DB");

            if (!Connected)
                throw new Exception("Not connected to mirror DB");

            if (dumpDir[dumpDir.Length - 1] != '\\')
                dumpDir += @"\";

            _stopWriting = false;
            string dumpFile = dumpDir + "dump_" + DateTime.Now.Ticks + ".xml";
            StreamWriter dumpWriter = new StreamWriter(dumpFile);
            dumpWriter.AutoFlush = true;

            // bytes in a MB
            int mb = 1024 * 1024;

            // file size limit in MB
            float mbLimit = 200;

            int end = startID + numPages - 1;
            int i;
            for (i = startID; i <= end; i += blockSize)
            {
                if (_stopWriting)
                    break;

                // check file size, start another one if needed
                FileInfo fi = new FileInfo(dumpFile);
                float sizeMB = fi.Length / (float)mb;
                if (sizeMB > mbLimit)
                {
                    dumpWriter.Close();
                    dumpFile = dumpDir + "dump_" + DateTime.Now.Ticks + ".xml";
                    dumpWriter = new StreamWriter(dumpFile);
                }

                // get titles
                List<string> titles = null;
                try
                {
                    titles = GetTitleRange(ns, i, blockSize, includeRedirects);
                }
                catch (Exception ex)
                {
                    dumpWriter.Close();
                    throw new Exception("Failed at start ID " + i + ".  Error:  " + ex);
                }

                // process titles
                foreach (string title in titles)
                {
                    if (_stopWriting)
                        break;

                    try
                    {
                        // get page from mirror and dump to file
                        Page p = LookupPage(ns, title, false, false, false, false);
                        if (p == null)
                        {
                            StreamWriter logWriter = new StreamWriter("lemur_dump_log.txt", true);
                            logWriter.WriteLine("Dump to Lemur:  could not find page to write:  " + title);
                            logWriter.Close();
                        }
                        else
                            dumpWriter.WriteLine(p.LemurDump);

                        _lastTitleWritten = p.Title;
                        _lastIDWritten = p.ID;
                    }
                    catch (Exception ex)
                    {
                        dumpWriter.Close();
                        throw new Exception("Failed at start ID " + i + ", title \"" + title + "\".  Error: " + ex);
                    }
                }
            }

            dumpWriter.Close();
            return _lastTitleWritten;
        }

        /// <summary>
        /// Gets a range of titles from the DB
        /// </summary>
        /// <param name="ns">Namespace to search</param>
        /// <param name="startPage">Page to start at</param>
        /// <param name="numPages">Number of titles</param>
        /// <param name="includeRedirects">Whether or not to include redirects</param>
        /// <returns>List of titles</returns>
        public override List<string> GetTitleRange(Namespace ns, int startPage, int numPages, bool includeRedirects)
        {
            if (startPage < 0 || numPages < 0)
                throw new Exception("Invalid page range");

            int endPage = startPage + numPages - 1;
            IDataReader reader = SubmitQuery("SELECT page_title, redirects_to FROM page WHERE page_namespace=" + NamespaceValue(ns) + " AND page_id >= " + startPage + " AND page_id <= " + endPage);

            // get titles
            List<string> titles = new List<string>();
            while (reader.Read())
            {
                string pageTitle = reader["page_title"].ToString();
                bool pageIsRedirect = reader["redirects_to"].ToString() != "";
                if (pageTitle.Trim() == "" || (pageIsRedirect && !includeRedirects))
                    continue;
                titles.Add(pageTitle);
            }
            reader.Close();

            return titles;
        }

        /// <summary>
        /// Get titles containing a word
        /// </summary>
        /// <param name="ns">Namespace to search</param>
        /// <param name="s">Word to look for</param>
        /// <returns>List of page titles (strings) containing word</returns>
        public override List<string> GetTitlesContaining(WikiDB.Namespace ns, string s)
        {
            if (!Connected)
                throw new Exception("Not connected to mirror DB");

            s = MySQLEscape(s);
            List<string> titles = new List<string>();
            if (s == "")
                return titles;

            int nsVal = NamespaceValue(ns);
            IDataReader reader = SubmitQuery("SELECT page_title FROM titleindex " +
                                                 "WHERE word=\"" + s + "\" AND page_namespace=" + nsVal);

            while (reader.Read())
                titles.Add(reader["page_title"].ToString());
            reader.Close();

            return titles;
        }

        /// <summary>
        /// Gets whether or not a page is a redirect page
        /// </summary>
        /// <param name="ns">Namespace to search</param>
        /// <param name="title">Title to search</param>
        /// <returns>True if page is a redirect page, False otherwise</returns>
        public override bool IsRedirect(WikiDB.Namespace ns, string title)
        {
            title = MySQLEscape(GetURLFromPageTitle(title));
            if (title == "")
                return false;

            int nsVal = NamespaceValue(ns);
            IDataReader reader = SubmitQuery("SELECT redirects_to FROM page " +
                                                 "WHERE page_namespace=" + nsVal + " AND page_title=\"" + title + "\"");
            if (reader.Read())
            {          
                string redirectsTo = reader["redirects_to"].ToString();
                reader.Close();
                return redirectsTo != "";
            }
            else
            {
                reader.Close();
                return false;
            }
        }
    }
}
