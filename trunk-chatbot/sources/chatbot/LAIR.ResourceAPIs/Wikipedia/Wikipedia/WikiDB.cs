using System;
using System.Data;
using System.Collections.Generic;
using System.Data.Common;
using System.Text;
using System.Net;
using System.Text.RegularExpressions;
using System.Security.Cryptography;
using System.IO;
using System.Xml;

namespace LAIR.ResourceAPIs.Wikipedia
{
    /// <summary>
    /// Provides access to the MySQL database containing Wikipedia
    /// </summary>
    public class WikiDB
    {
        #region static functions
        /// <summary>
        /// Page cache
        /// </summary>
        protected static Dictionary<string, Page> _pageCache;

        /// <summary>
        /// Max cache size (in pages)
        /// </summary>        
        protected static int _maxCacheSize;

        /// <summary>
        /// Static constructor
        /// </summary>
        static WikiDB()
        {
            // set up page cache
            _pageCache = new Dictionary<string, Page>();
            _maxCacheSize = 1000;
        }

        /// <summary>
        /// Gets the integer value associated with a namespace
        /// </summary>
        /// <param name="ns">Namespace to get integer value for</param>
        /// <returns>Integer value associated with a namespace</returns>
        public static int NamespaceValue(Namespace ns)
        {
            List<string> names = new List<string>(Enum.GetNames(typeof(Namespace)));
            int index = names.IndexOf(ns.ToString());
            return index;
        }

        /// <summary>
        /// Gets a page URL from its title
        /// </summary>
        /// <param name="title">Title of page</param>
        /// <returns>URL of page</returns>
        public static string GetURLFromPageTitle(string title)
        {
            if (title == null)
                return "";

            title = title.Trim();
            if (title == "")
                return "";

            return title[0].ToString().ToUpper() + (title.Length > 1 ? title.Substring(1).Replace(' ', '_') : "");
        }

        /// <summary>
        /// Escapes double-quotes and backslashes
        /// </summary>
        /// <param name="s"></param>
        /// <returns></returns>
        public static string MySQLEscape(string s)
        {
            return s.Replace("\"", "\"\"").Replace(@"\", @"\\");
        }
        #endregion

        /// <summary>
        /// Namespace enumeration from MediaWiki 1.8.2 (includes/Defines.php)
        /// </summary>
        public enum Namespace
        {
            /// <summary>
            /// Main pages
            /// </summary>
            Main,

            /// <summary>
            /// Talk pages
            /// </summary>
            Talk,

            /// <summary>
            /// User pages
            /// </summary>
            User,

            /// <summary>
            /// User talk pages
            /// </summary>
            UserTalk,

            /// <summary>
            /// Project pages
            /// </summary>
            Project,

            /// <summary>
            /// Project talk pages
            /// </summary>
            ProjectTalk,

            /// <summary>
            /// Image pages
            /// </summary>
            Image,

            /// <summary>
            /// Image talk pages
            /// </summary>
            ImageTalk,

            /// <summary>
            /// MediaWiki pages
            /// </summary>
            MediaWiki,

            /// <summary>
            /// MediaWiki talk pages
            /// </summary>
            MediaWikiTalk,

            /// <summary>
            /// Template pages
            /// </summary>
            Template,

            /// <summary>
            /// Template talk pages
            /// </summary>
            TemplateTalk,

            /// <summary>
            /// Help pages
            /// </summary>
            Help,

            /// <summary>
            /// Help talk pages
            /// </summary>
            HelpTalk,

            /// <summary>
            /// Category pages
            /// </summary>
            Category,

            /// <summary>
            /// Category talk pages
            /// </summary>
            CategoryTalk,

            /// <summary>
            /// Undefined namespace
            /// </summary>
            Undef
        };

        /// <summary>
        /// Connection to DB
        /// </summary>
        protected DbConnection _connection;   
        
        /// <summary>
        /// Name of DB
        /// </summary>
        protected string _DB;

        /// <summary>
        /// Name of server
        /// </summary>
        protected string _server;

        /// <summary>
        /// Authorized user
        /// </summary>
        protected string _user;
        
        /// <summary>
        /// Authorized password
        /// </summary>
        protected string _password;

        #region properties
        /// <summary>
        /// Page cache
        /// </summary>
        protected static Dictionary<string, Page> PageCache
        {
            get { return _pageCache; }
            set { _pageCache = value; }
        }

        /// <summary>
        /// Max cache size (in pages)
        /// </summary>
        public static int MaxCacheSize
        {
            get { return _maxCacheSize; }
            set
            {                
                _maxCacheSize = value;
                CheckCacheSize();
            }
        } 

        /// <summary>
        /// Gets or sets the name of the database
        /// </summary>
        public string DB
        {
            get { return _DB; }
            set { _DB = value; }
        }

        /// <summary>
        /// Gets or sets the name of the server
        /// </summary>
        public string Server
        {
            get { return _server; }
            set { _server = value; }
        }

        /// <summary>
        /// Gets or sets the name of the user authorized on the server
        /// </summary>
        public string User
        {
            get { return _user; }
            set { _user = value; }
        }

        /// <summary>
        /// Gets or sets the password for the user
        /// </summary>
        public string Password
        {
            get { return _password; }
            set { _password = value; }
        }

        /// <summary>
        /// Gets whether or not the connection is open
        /// </summary>
        public bool Connected
        {
            get { return _connection != null && _connection.State == ConnectionState.Open; }
        }
        #endregion

        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="server">Server hosting database</param>
        /// <param name="database">Name of database</param>
        /// <param name="user">Authorized user</param>
        /// <param name="password">Authorized password</param>
        public WikiDB(string server, string database, string user, string password)
        {
            _server = server;
            _DB = database;
            _user = user;
            _password = password;
        }
        
        /// <summary>
        /// Connect to the MySQL database containing the Wikipedia data
        /// </summary>
        public virtual void Connect()
        {
            if (Connected)
                return;

            // connect to DB
#if NET40
            _connection = new MySqlConnection("Data Source=" + _server + "; " +
                                              "Database=" + _DB + "; " +
                                              "User Id=" + _user + "; " +
                                              "Password=" + _password + "; " +
                                              "CharSet=utf8;");
#endif
            _connection.Open();

            if (_connection.State != ConnectionState.Open)
                throw new Exception("Could not connect to database \"" + _DB + "\"");

            ExecuteNonQuery("SET SQL_MODE='STRICT_ALL_TABLES'");
        }

        /// <summary>
        /// Disconnects from database
        /// </summary>
        public virtual void Disconnect()
        {
            if (_connection != null && _connection.State != ConnectionState.Closed)
            {
                _connection.Close();

                if (_connection.State != ConnectionState.Closed)
                    throw new Exception("Failed to disconnect from database");
            }
        }

        /// <summary>
        /// Submit a query to a database
        /// </summary>
        /// <param name="query">MySQL query to submit</param>
        /// <returns>Reader for results</returns>
        public IDataReader SubmitQuery(string query)
        {
            if (!CheckConnection(true))
                throw new Exception("Must call Connect before SubmitQuery");

#if NET40
            IDbCommand cmd = new MySqlCommand(command, (MySqlConnection) _connection);
            return cmd.ExecuteReader();
#else
            throw new NotImplementedException("Find a newer version of MySql.Data.dll");
#endif
        }

        /// <summary>
        /// Execute a non-query command on a database
        /// </summary>
        /// <param name="command">Command to submit</param>
        /// <returns>Number of rows affected</returns>
        public int ExecuteNonQuery(string command)
        {
            if (!CheckConnection(true))
                throw new Exception("Must call Connect before SubmitQuery");
#if NET40
            IDbCommand cmd = new MySqlCommand(command, (MySqlConnection) _connection);
            return cmd.ExecuteNonQuery();
#else
            throw new NotImplementedException("Find a newer version of MySql.Data.dll");
#endif
        }

        /// <summary>
        /// Look up a page in the database
        /// </summary>
        /// <param name="ns">Namespace to look up</param>
        /// <param name="title">Title of page to look up</param>
        /// <param name="followRedirection">Whether or not to follow redirection</param>
        /// <returns>Page instance</returns>
        public virtual Page LookupPage(Namespace ns, string title, bool followRedirection)
        {
            // check URL
            if (title == "")
                throw new Exception("Invalid page URL in LookupPage");

            // check connection
            if (!CheckConnection(true))
                throw new Exception("Could not establish connection to Wikipedia database");

            string url = MySQLEscape(GetURLFromPageTitle(title));
            int nsVal = NamespaceValue(ns);

            string query = "SELECT text.old_text, text.old_flags, page.page_id FROM text " +
                           "INNER JOIN revision ON text.old_id=revision.rev_id " +
                           "INNER JOIN page ON revision.rev_page=page.page_id " +
                           "WHERE page.page_namespace=" + nsVal + " " +
                           "AND page.page_title=\"" + url + "\"";

            IDataReader reader = SubmitQuery(query);

            Page p = null;
            if (reader.Read())
            {
                // make sure we're getting utf-8 encoding from the database
                string flags = Encoding.UTF8.GetString((byte[])reader["old_flags"]);
                if (flags != "utf-8")
                    throw new Exception("Non UTF-8 text encoding in page");

                // get page in UTF-8 encoding
                string text = Encoding.UTF8.GetString((byte[])reader["old_text"]);

                // get page ID
                int id = int.Parse(reader["page_id"].ToString());

                reader.Close();

                if (text == "")
                    return null;

                p = new Page(title, ns, id, text, this, followRedirection);
            }
            else
                reader.Close();

            return p;
        }

        /// <summary>
        /// Look up a list of linked pages for a particular page
        /// </summary>
        /// <param name="p">Page to look up linked page for</param>
        public void LookupLinkedPages(Page p)
        {
            List<WikiLink> links = p.WikiLinks;
            foreach (WikiLink wl in links)
                wl.DestPage = LookupPage(wl.DestPageNamespace, wl.DestPageURL, true);
        }

        /// <summary>
        /// Gets a range of pages
        /// </summary>
        /// <param name="ns">Namespace to get pages from</param>
        /// <param name="startPage">Start page</param>
        /// <param name="numPages">Number of pages to get</param>
        /// <param name="followRedirection">Whether or not to follow redirection</param>
        /// <returns>List of page objects</returns>
        public List<Page> GetPageRange(Namespace ns, int startPage, int numPages, bool followRedirection)
        {
            if (startPage < 0 || numPages < 0)
                throw new Exception("Invalid page range");

            int endPage = startPage + numPages - 1;
            IDataReader reader = SubmitQuery("SELECT * FROM page WHERE page_namespace=" + NamespaceValue(ns) + " AND page_id >= " + startPage + " AND page_id <= " + endPage);

            // get titles
            List<string> titles = new List<string>();
            while (reader.Read())
            {
                string pageTitle = reader["page_title"].ToString();
                if (pageTitle.Trim() == "")
                    continue;
                titles.Add(pageTitle);
            }
            reader.Close();

            // get pages
            List<Page> pages = new List<Page>();
            foreach(string title in titles)
            {
                Page p = LookupPage(ns, title, followRedirection);

                if (p != null)
                    pages.Add(p);
            }
            return pages;
        }

        /// <summary>
        /// Gets a range of titles from the database
        /// </summary>
        /// <param name="ns">Namespace to get titles from</param>
        /// <param name="startPage">Page to start at</param>
        /// <param name="numPages">Number of pages to get titles from</param>
        /// <param name="includeRedirects">Whether or not to include titles from redirect pages</param>
        /// <returns>List of titles</returns>
        public virtual List<string> GetTitleRange(Namespace ns, int startPage, int numPages, bool includeRedirects)
        {
            if (startPage < 0 || numPages < 0)
                throw new Exception("Invalid page range");

            int endPage = startPage + numPages - 1;
            IDataReader reader = SubmitQuery("SELECT * FROM page WHERE page_namespace=" + NamespaceValue(ns) + " AND page_id >= " + startPage + " AND page_id <= " + endPage);

            // get titles
            List<string> titles = new List<string>();
            while (reader.Read())
            {
                string pageTitle = reader["page_title"].ToString();
                int pageIsRedirect = int.Parse(reader["page_is_redirect"].ToString());
                if (pageTitle.Trim() == "" || (pageIsRedirect == 1 && !includeRedirects))
                    continue;
                titles.Add(pageTitle);
            }
            reader.Close();

            return titles;
        }

        /// <summary>
        /// Gets a list of page titles that contain a word. This function is _extremely_ inefficient and incomplete because 
        /// the default Wikipedia database schema does not provide specialized indexes for this task. 
        /// You are recommended to build a special cross-reference table, inherit this class, and override this method.
        /// </summary>
        /// <param name="ns">Namespace to get titles from</param>
        /// <param name="s">String to match within page titles (must match casing in database _exactly_)</param>
        /// <returns>List of page titles</returns>
        public virtual List<string> GetTitlesContaining(Namespace ns, string s)
        {
            s = MySQLEscape(s.ToLower().Trim());
            List<string> titles = new List<string>();
            if (s == "")
                return titles;

            int nsVal = NamespaceValue(ns);
            IDataReader reader = SubmitQuery("SELECT page_title FROM page " + 
                                                 "WHERE page_namespace=" + nsVal + " AND page_title LIKE \"%" + s + "\"%");
            while (reader.Read())
                titles.Add(reader["page_title"].ToString());

            return titles;
        }

        /// <summary>
        /// Gets whether or not a page exists in the DB
        /// </summary>
        /// <param name="ns">Namespace to check</param>
        /// <param name="title">Title to check</param>
        /// <returns>True if page exists, False otherwise</returns>
        public virtual bool PageExists(Namespace ns, string title)
        {
            title = MySQLEscape(GetURLFromPageTitle(title));
            if (title == "")
                return false;

            int nsVal = NamespaceValue(ns);
            IDataReader reader = SubmitQuery("SELECT page_id FROM page " +
                                                 "WHERE page_namespace=" + nsVal + " AND page_title=\"" + title + "\"");
            if (reader.Read())
            {
                reader.Close();
                return true;
            }
            else
            {
                reader.Close();
                return false;
            }
        }

        /// <summary>
        /// Gets whether or not a page is a redirect page
        /// </summary>
        /// <param name="ns">Namespace to search</param>
        /// <param name="title">Title to look for</param>
        /// <returns>True if the page is a redirect page, False otherwise</returns>
        public virtual bool IsRedirect(Namespace ns, string title)
        {
            title = MySQLEscape(GetURLFromPageTitle(title));
            if (title == "")
                return false;

            int nsVal = NamespaceValue(ns);
            IDataReader reader = SubmitQuery("SELECT page_is_redirect FROM page " +
                                                 "WHERE page_namespace=" + nsVal + " AND page_title=\"" + title + "\"");
            if (reader.Read())
            {
                int isRedirect = int.Parse(reader["page_is_redirect"].ToString());
                reader.Close();
                return isRedirect == 1;
            }
            else
            {
                reader.Close();
                return false;
            }
        }

        /// <summary>
        /// Checks the current connection and optionally reconnects
        /// </summary>
        /// <param name="autoReconnect">Whether or not to reconnect a bad connection</param>
        /// <returns>True if connection state is okay or reconnected, False otherwise</returns>
        public bool CheckConnection(bool autoReconnect)
        {
            bool badConnection = _connection == null || _connection.State == ConnectionState.Closed;
            if (!badConnection)
                return true;
            else if(!autoReconnect)
                return false;

            try { Connect(); }
            catch (Exception) { return false; }

            return true;
        }

        /// <summary>
        /// Adds a page to the page cache
        /// </summary>
        /// <param name="p">Page to add</param>
        protected void CachePage(Page p)
        {
            if (_pageCache.ContainsKey(p.URL))
                return;

            CheckCacheSize();

            // add page to cache
            _pageCache.Add(p.URL, p);
        }

        /// <summary>
        /// Checks the size of the page cache
        /// </summary>
        private static void CheckCacheSize()
        {
            // remove a page from the cache if it's full
            while (_pageCache.Count > _maxCacheSize && _pageCache.Count > 0)
            {
                Dictionary<string, Page>.Enumerator ide = _pageCache.GetEnumerator();
                ide.MoveNext();
                _pageCache.Remove(ide.Current.Key);
            }
        }

        /// <summary>
        /// Clears the page cache
        /// </summary>
        public void ClearPageCache()
        {
            _pageCache.Clear();
        }

        /// <summary>
        /// Checks whether two titles are coreferential (i.e., they refer to the same page or to pages that
        /// redirect to the same page.
        /// </summary>
        /// <param name="title1">First title</param>
        /// <param name="title2">Second title</param>
        /// <returns>True if titles are coreferential, False otherwise</returns>
        public bool AreCoreferentialTitles(string title1, string title2)
        {
            Page p1 = LookupPage(Namespace.Main, title1, true);
            Page p2 = LookupPage(Namespace.Main, title2, true);
            if (p1 == null || p2 == null)
                return false;

            if (p1 == p2)
                return true;

            if (p1.IsRedirect)
                p1 = p1.RedirectsTo;
            if (p2.IsRedirect)
                p2 = p2.RedirectsTo;

            return p1 == p2;
        }
    }
}
