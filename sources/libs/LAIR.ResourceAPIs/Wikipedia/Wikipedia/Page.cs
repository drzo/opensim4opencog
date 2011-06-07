using System;
using System.Collections.Generic;
using System.Text;
using System.Text.RegularExpressions;
using System.Xml;
using System.IO;
using System.Xml.Serialization;

namespace LAIR.ResourceAPIs.Wikipedia
{
    /// <summary>
    /// Represents a Wikipedia page
    /// </summary>
    public class Page : IComparable
    {
        private string _title;                                // title of page
        private int _ID;                                      // ID of page
        private List<Section> _sections;                      // sections in this page
        private List<string> _lines;                          // all lines on page
        private Page _redirectsTo;                            // page this page redirects to
        private Dictionary<string, double> _termFrequencies;  // table of term frequencies
        private WikiDB.Namespace _namespace;                  // namespace for this page
        private float _weight;                                // weight of this page
        private bool _sectionsRecovered;                      // whether or not page sections were recovered

        /// <summary>
        /// Gets whether or not the sections have been recovered for this page
        /// </summary>
        public bool SectionsRecovered
        {
            get { return _sectionsRecovered; }
        }

        /// <summary>
        /// Gets or sets the ID for this page
        /// </summary>
        public int ID
        {
            get { return _ID; }
            set { _ID = value; }
        }

        /// <summary>
        /// Gets or sets the title for this page
        /// </summary>
        public string Title
        {
            get { return _title; }
            set { _title = value; }
        }

        /// <summary>
        /// Gets the URL for this page
        /// </summary>
        public string URL
        {
            get { return WikiDB.GetURLFromPageTitle(_title); }
        }

        /// <summary>
        /// Gets or sets the sections in this page
        /// </summary>
        public List<Section> Sections
        {
            get { return _sections; }
            set { _sections = value; }
        }           

        /// <summary>
        /// Gets or sets the list of internally linked pages
        /// </summary>
        public List<WikiLink> WikiLinks
        {
            get
            {
                // get all links in all sections
                List<WikiLink> links = new List<WikiLink>();
                foreach (Section s in _sections)
                    links.AddRange(s.WikiLinks);

                return links;
            }
        }

        /// <summary>
        /// Gets or sets the collection of lines in this page
        /// </summary>
        public List<string> Lines
        {
            get { return _lines; }
            set { _lines = value; }
        }

        /// <summary>
        /// Gets whether or not this page is a redirect page
        /// </summary>
        public bool IsRedirect
        {
            get { return _redirectsTo != null; }
        }

        /// <summary>
        /// Gets or sets the redirect page for this page
        /// </summary>
        public Page RedirectsTo
        {
            get
            {
                if (_redirectsTo != null &&
                    _redirectsTo.RedirectsTo != null)
                    return _redirectsTo.RedirectsTo;

                return _redirectsTo;
            }
            set { _redirectsTo = value; }
        }

        /// <summary>
        /// Gets or sets the term frequency table for this page
        /// </summary>
        [XmlIgnore]
        public Dictionary<string, double> TermFrequencies
        {
            get { return _termFrequencies; }
            set { _termFrequencies = value; }
        }

        /// <summary>
        /// Gets or sets the namespace for this page
        /// </summary>
        public WikiDB.Namespace Namespace
        {
            get { return _namespace; }
            set { _namespace = value; }
        }

        /// <summary>
        /// Gets or sets the weight of this page
        /// </summary>
        public float Weight
        {
            get { return _weight; }
            set { _weight = value; }
        }

        /// <summary>
        /// Get Page table insert values
        /// </summary>
        public string PageInsertValues
        {
            get
            {                
                string values = "";
                string url = WikiDB.MySQLEscape(URL);
                if (_lines.Count > 0)
                {
                    values = "VALUES (" + WikiDB.NamespaceValue(_namespace) + ",\"" + url + "\", \"";

                    foreach (string line in _lines)
                        values += WikiDB.MySQLEscape(line) + "\n";

                    // get section layout (must be valid XML)
                    string secLayout = GetSectionLayout(false, true);
                    values += "\", \"" + WikiDB.MySQLEscape(secLayout) + "\", ";
                    
                    // get redirect page
                    string redirectsTo = "";
                    if (_lines.Count > 0 &&
                       _lines[0].ToString().ToLower().Contains("redirect") &&
                       WikiLinks.Count == 1)
                    {
                        redirectsTo = WikiDB.MySQLEscape(WikiLinks[0].DestPageURL);
                    }

                    values += "\"" + redirectsTo + "\")";
                }

                return values;
            }
        }

        /// <summary>
        /// Gets values for link table
        /// </summary>
        public string LinksInsertValues
        {
            get
            {
                string values = "";

                // get links
                string url = WikiDB.MySQLEscape(URL);
                List<WikiLink> links = WikiLinks;
                if (links.Count > 0)
                {
                    values = "VALUES (" + WikiDB.NamespaceValue(_namespace) + ", \"" + url + "\", \"";

                    foreach (WikiLink wl in links)
                        values += WikiDB.MySQLEscape(wl.DestPageURL) + " " + wl.SourceSection.ID + "\n";

                    values += "\")";
                }
                return values;
            }
        }
        
        /// <summary>
        /// Gets values for TF table
        /// </summary>
        public string TermFreqsValues
        {
            get
            {
                string values = "";

                // get TFs
                string url = WikiDB.MySQLEscape(URL);
                if (_termFrequencies.Count > 0)
                {
                    values = "VALUES (" + WikiDB.NamespaceValue(_namespace) + ", \"" + url + "\", \"";

                    Dictionary<string, double>.Enumerator terms = _termFrequencies.GetEnumerator();
                    while (terms.MoveNext())
                    {
                        string word = WikiDB.MySQLEscape(terms.Current.Key.ToString());
                        string freq = terms.Current.Value.ToString();

                        // sanity check
                        if(word.Contains(" "))
                            throw new Exception("Words shouldn't have spaces in them");

                        values += word + " " + freq + "\n";
                    }

                    values += "\")";
                }
                return values;
            }
        }

        /// <summary>
        /// Gets the section layout for this page
        /// </summary>
        /// <param name="includeSectionText">Whether or not to include section text</param>
        /// <param name="includeNonContentSections">Whether or not to include non-content sections</param>
        public string GetSectionLayout(bool includeSectionText, bool includeNonContentSections)
        {
            string sectionLayout = "<layout>";
            foreach (Section s in _sections)
                sectionLayout += s.GetLayout(includeSectionText, includeNonContentSections);

            return sectionLayout + "</layout>";
        }

        /// <summary>
        /// Gets dump to be indexed/searched with the Lemur IR engine
        /// </summary>
        public string LemurDump
        {
            get
            {
                string dumpString = "<DOC>\n";
                dumpString += "<DOCNO>" + URL + "</DOCNO>\n" + 
                              "<TEXT>\n";

                foreach (string line in _lines)
                    dumpString += line + "\n";
                dumpString += "</TEXT>\n" +
                              "</DOC>";

                return dumpString;
            }
        }

        /// <summary>
        /// Gets the plain text of this page
        /// </summary>
        public string Text
        {
            get
            {
                string text = "";
                foreach (Section s in _sections)
                    text += s.Text;
                return text;
            }
        }

        /// <summary>
        /// Constructor
        /// </summary>
        public Page()
        {
            _sections = new List<Section>();
            _termFrequencies = new Dictionary<string, double>();
            _weight = 0.0F;
        }

        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="title">Title of page</param>
        /// <param name="ns">Namespace of this page</param>
        /// <param name="ID">ID for this page</param>
        public Page(string title, WikiDB.Namespace ns, int ID)
            : this()
        {
            _title = title;
            _namespace = ns;
            _ID = ID;
        }

        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="title">Title of page</param>
        /// <param name="ns">Namespace of this page</param>
        /// <param name="ID">ID for this page</param>
        /// <param name="wikiText">Wiki text for page</param>
        /// <param name="database">Database containing this page</param>
        /// <param name="followRedirection">Whether or not to follow redirection</param>
        public Page(string title, WikiDB.Namespace ns, int ID, string wikiText, WikiDB database, bool followRedirection)
            : this(title, ns, ID)
        {
            // remove irrelevant markup
            wikiText = WikiMarkup.RemoveIrrelevantMarkup(wikiText);

            // split page up into lines
            _lines = Section.GetLines(wikiText);

            int firstSectionStart = Section.GetNextSectionStart(_lines, 0);
            List<string> headerLines = Section.ExtractLines(_lines, 0, firstSectionStart - 1);

            if (headerLines.Count > 0)
            {
                Header h = new Header(headerLines, this);
                _sections.Add(h);
            }

            // get sections
            _sections.AddRange(Section.ExtractSections(_lines, null, this));

            // check for redirect page
            string firstLine = "";
            if(_lines.Count > 0)
                firstLine = _lines[0];
            string redirect = "#redirect";
            if (firstLine.Length >= redirect.Length &&
                firstLine.Substring(0, redirect.Length).ToLower() == redirect &&
                WikiLinks.Count == 1 &&
                followRedirection)
            {
                // get redirect page
                string redirectURL = WikiLinks[0].DestPageURL;
                _redirectsTo = database.LookupPage(ns, redirectURL, followRedirection);
            }

            // process markup
            WikiMarkup.ProcessMarkup(_lines);

            // set line information for the page
            SetLineInfo();

            // get TF information
            foreach (Section s in _sections)
            {
                foreach (string line in s.Lines)
                {
                    string[] tokens = line.Split(' ');

                    foreach (string token in tokens)
                    {
                        // ignore case
                        string lowerToken = token.ToLower().Trim();
                        lowerToken = WikiMarkup.TrimPunctuation(lowerToken);
                        
                        if (lowerToken == "" ||
                            WikiMarkup.IsStopWord(lowerToken, false))
                            continue;

                        if (!_termFrequencies.ContainsKey(lowerToken))
                            _termFrequencies[lowerToken] = 1.0F;
                        else
                            _termFrequencies[lowerToken] = _termFrequencies[lowerToken] + 1;
                    }
                }
            }            
        }

        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="title">Title of page</param>
        /// <param name="ns">Namespace of this page</param>
        /// <param name="ID">ID for this page</param>
        /// <param name="lines">Lines contained in this page</param>
        /// <param name="sectionLayout">Layout of sections (see section class for documentation)</param>
        public Page(string title, WikiDB.Namespace ns, int ID, List<string> lines, string sectionLayout)
            : this(title, ns, ID)
        {
            _lines = lines;
            _sections = Section.ExtractSections(lines, null, this, sectionLayout);
            _sectionsRecovered = true;
        }

        /// <summary>
        /// Gets hash code for this page
        /// </summary>
        /// <returns>Hash code for this page</returns>
        public override int GetHashCode()
        {
            return _title.GetHashCode();
        }

        /// <summary>
        /// Equality check
        /// </summary>
        /// <param name="obj">Page to check</param>
        /// <returns>True if pages are equal, False otherwise</returns>
        public override bool Equals(object obj)
        {
            if (!(obj is Page))
                return false;

            Page p = (Page)obj;
            return URL == p.URL;
        }

        /// <summary>
        /// Equality operator
        /// </summary>
        /// <param name="p1"></param>
        /// <param name="p2"></param>
        /// <returns></returns>
        public static bool operator ==(Page p1, Page p2)
        {
            if (!(p1 is Page))
                return !(p2 is Page);

            return p1.Equals(p2);
        }

        /// <summary>
        /// Inequality operator
        /// </summary>
        /// <param name="p1"></param>
        /// <param name="p2"></param>
        /// <returns></returns>
        public static bool operator !=(Page p1, Page p2)
        {
            return !(p1 == p2);
        }

        /// <summary>
        /// Gets the similarity between this page and another page
        /// </summary>
        /// <param name="p">Page to compare this one to</param>
        /// <returns>Similarity in [0, 1]</returns>
        public float Similarity(Page p)
        {
            if (p == null)
                return 0.0F;

            Dictionary<string, double>.Enumerator t1Enum = _termFrequencies.GetEnumerator();
            Dictionary<string, double>.Enumerator t2Enum = p.TermFrequencies.GetEnumerator();

            // get dot product and magnitudes
            double dotProd = 0;
            double t1Mag = 0;
            double t2Mag = 0;
            while (t1Enum.MoveNext())
            {
                double t1Val = t1Enum.Current.Value;
                t1Mag += Math.Pow(t1Val, 2);

                if (p.TermFrequencies.ContainsKey(t1Enum.Current.Key))
                {
                    double t2Val = p.TermFrequencies[t1Enum.Current.Key];
                    dotProd += t1Val * t2Val;
                    t2Mag += Math.Pow(t2Val, 2);
                }
            }

            // add terms to t2 mag that didn't occur in t1
            while (t2Enum.MoveNext())
            {
                if (!_termFrequencies.ContainsKey(t2Enum.Current.Key))
                    t2Mag += Math.Pow(t2Enum.Current.Value, 2);
            }

            t1Mag = Math.Sqrt(t1Mag);
            t2Mag = Math.Sqrt(t2Mag);

            // get weight
            float weight;
            if (dotProd == 0 || t1Mag == 0 || t2Mag == 0)
                weight = 0;
            else
                weight = (float)(dotProd / (t1Mag * t2Mag));

            return weight;
        }

        /// <summary>
        /// Gets a section from this page
        /// </summary>
        /// <param name="sectionID">One-based identifier of the form 1.3.2</param>
        /// <returns>Section from this page</returns>
        public Section GetSection(string sectionID)
        {
            if (!Section.IDRE.Match(sectionID).Success)
                throw new Exception("Invalid section ID");

            string[] indexes = sectionID.Split('.');
            int first = int.Parse(indexes[0]) - 1;
            if (_sections.Count <= first)
                return null;

            int[] intIndexes = new int[indexes.Length - 1];
            for (int i = 1; i < indexes.Length; ++i)
                intIndexes[i - 1] = int.Parse(indexes[i]);

            return _sections[first].GetSection(intIndexes);
        }

        /// <summary>
        /// Compare this page to another object
        /// </summary>
        /// <param name="obj">Object to compare page to</param>
        /// <returns>-1 if this lt obj, 1 if this gt obj, 0 otherwise</returns>
        public int CompareTo(object obj)
        {
            if(!(obj is Page))
                return 1;

            return _weight.CompareTo(((Page)obj).Weight);
        }

        /// <summary>
        /// Removes markup from this page
        /// </summary>
        public void RemoveMarkup()
        {
            WikiMarkup.ProcessMarkup(_lines);
            foreach (Section s in _sections)
                s.RemoveMarkup();
        }

        /// <summary>
        /// Sets the line information for this page (all sections)
        /// </summary>
        public void SetLineInfo()
        {
            // set start and end position for sections
            Section prevSec = null;
            foreach (Section s in _sections)
            {
                s.SetLineInfo(prevSec != null ? prevSec.SectionEnd + 1 : 0);
                prevSec = s;
            }
        }        
    }
}
