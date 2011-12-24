using System;
using System.Collections.Generic;
using System.Text;
using System.Text.RegularExpressions;
using System.Collections;
using System.Xml.Serialization;
using LAIR.XML;

namespace LAIR.ResourceAPIs.Wikipedia
{
    /// <summary>
    /// Section class.  Represents a section on a Wikipedia page
    /// </summary>
    public class Section
    {
        #region protected members
        /// <summary>
        /// Name of this section
        /// </summary>
        protected string _name;

        /// <summary>
        /// Collection of lines that make up this section
        /// </summary>
        protected List<string> _lines;

        /// <summary>
        /// List of subsections within this section
        /// </summary>
        protected List<Section> _subSections;

        /// <summary>
        /// Parent section for this section
        /// </summary>
        protected Section _parentSection;

        /// <summary>
        /// Starting line number of this section within its parent section
        /// </summary>
        protected int _sectionStart;

        /// <summary>
        /// Ending line number of this section within its parent section
        /// </summary>
        protected int _sectionEnd;

        /// <summary>
        /// List of Wiki links in this section and all subsections
        /// </summary>
        protected List<WikiLink> _wikiLinks;

        /// <summary>
        /// Page that contains this section
        /// </summary>
        protected Page _containingPage;
        #endregion

        #region properties
        /// <summary>
        /// Gets or sets the name of this section
        /// </summary>
        public string Name
        {
            get { return _name; }
            set { _name = value; }
        }

        /// <summary>
        /// Gets or sets the collection of lines that make up this section
        /// </summary>
        public List<string> Lines
        {
            get { return _lines; }
            set { _lines = value; }
        }

        /// <summary>
        /// Gets the depth of this section within the section hierarchy
        /// </summary>
        public int Depth
        {
            get
            {
                if (_parentSection == null)
                    return 0;

                return _parentSection.Depth + 1;
            }
        }

        /// <summary>
        /// Gets or sets the list of subsections within this section
        /// </summary>
        public List<Section> SubSections
        {
            get { return _subSections; }
            set { _subSections = value; }
        }

        /// <summary>
        /// Gets or sets the parent section for this section
        /// </summary>
        public Section ParentSection
        {
            get { return _parentSection; }
            set { _parentSection = value; }
        }

        /// <summary>
        /// Gets or sets the starting line number of this section within its parent section
        /// </summary>
        public int SectionStart
        {
            get { return _sectionStart; }
            set { _sectionStart = value; }
        }

        /// <summary>
        /// Gets or sets the ending line number of this section within its parent section
        /// </summary>
        public int SectionEnd
        {
            get { return _sectionEnd; }
            set { _sectionEnd = value; }
        }

        /// <summary>
        /// Gets or sets the list of Wiki links in this section and all subsections
        /// </summary>
        public List<WikiLink> WikiLinks
        {
            get
            {
                // get links from subsections
                List<WikiLink> links = new List<WikiLink>();
                foreach (Section s in _subSections)
                    links.AddRange(s.WikiLinks);

                // add links from text prior to first subsection
                links.AddRange(_wikiLinks);

                return links;
            }
            set { _wikiLinks = value; }
        }

        /// <summary>
        /// Gets or sets the page that contains this section
        /// </summary>
        public Page ContainingPage
        {
            get{ return _containingPage;}
            set { _containingPage = value;}
        }

        /// <summary>
        /// Gets the ID for this section. E.g., 1.3.2 for the second subsection in the third subsection of the first section on a page.
        /// </summary>
        public string ID
        {
            get
            {
                if (_parentSection == null && _containingPage != null)
                    return (_containingPage.Sections.IndexOf(this) + 1).ToString();
                else if (_parentSection != null)
                    return _parentSection.ID + "." + (_parentSection.SubSections.IndexOf(this) + 1).ToString();
                else
                    throw new Exception("Cannot compute ID for section");
            }
        }

        /// <summary>
        /// Gets the text of this section
        /// </summary>
        public string Text
        {
            get
            {
                string text = "";
                foreach (string s in _lines)
                    text += s + "\n";
                text.Trim();
                return text;
            }
        }
        #endregion

        #region static members
        /// <summary>
        /// RE for section IDs
        /// </summary>
        public static Regex IDRE = new Regex(@"[0-9]+(\.[0-9]+)*");

        private static List<string> _nonContentSections;  // list of section titles that don't usually have content (Header, See Also, etc.)

        /// <summary>
        /// Static constructor
        /// </summary>
        static Section()
        {
            _nonContentSections = new List<string>(new string[] { "header", "see also", "references", "external links", "further reading",
                                                                  "notes", "footnotes", "bibliography"});
        }

        /// <summary>
        /// Gets or sets the list of non-content sections
        /// </summary>
        public static List<string> NonContentSections
        {
            get { return _nonContentSections; }
            set { _nonContentSections = value; }
        }

        /// <summary>
        /// Split the page up into lines
        /// </summary>
        /// <param name="wikiText">Text to split</param>
        /// <returns>List of lines</returns>
        public static List<string> GetLines(string wikiText)
        {
            string[] lines = wikiText.Trim(' ', '\r', '\n').Split(new string[] { "\r\n", "\n", Environment.NewLine }, StringSplitOptions.RemoveEmptyEntries);
            return new List<string>(lines);
        }

        /// <summary>
        /// Finds the end of a section.
        /// </summary>
        /// <param name="lines">Lines to search</param>
        /// <param name="start">Where to start search. This must be a valid section line (e.g., ==Section X==)</param>
        /// <returns>Number of last line in the section</returns>
        public static int GetSectionEnd(List<string> lines, int start)
        {
            Regex r = WikiMarkup.GetSubsectionRE();
            
            // get current depth
            Match m = r.Match(lines[start]);
            if (!m.Success)
                throw new Exception("Invalid starting line for section");

            int depth = m.Groups["SectionStart"].Length;

            // find end of section
            int subsectionEnd;
            for (subsectionEnd = start + 1; subsectionEnd < lines.Count; ++subsectionEnd)
            {
                string line = lines[subsectionEnd];
                m = r.Match(line);
                if (m.Success)
                {
                    int depthSub = m.Groups["SectionStart"].Length;
                    if (depthSub <= depth)
                        break;
                }
            }

            return subsectionEnd - 1;
        }

        /// <summary>
        /// Gets the line number within a list of lines of the next section start
        /// </summary>
        /// <param name="lines">List of lines to search</param>
        /// <param name="start">Where to start search</param>
        /// <returns>Line number of section start</returns>
        public static int GetNextSectionStart(List<string> lines, int start)
        {
            Regex sectionRE = WikiMarkup.GetSubsectionRE();

            int sectionStart = start;
            while (sectionStart < lines.Count &&
                   !sectionRE.Match(lines[sectionStart]).Success)
                ++sectionStart;

            return sectionStart;
        }

        /// <summary>
        /// Extract a section and its subsections (and their subsections, etc.) from a list of lines
        /// </summary>
        /// <param name="lines">Lines to extract from</param>
        /// <param name="parent">Parent section for top-level sections</param>
        /// <param name="containingPage">Page that contains these sections</param>
        /// <returns>List of Section instances</returns>
        public static List<Section> ExtractSections(List<string> lines, Section parent, Page containingPage)
        {
            List<Section> sections = new List<Section>();

            if (lines == null || lines.Count == 0)
                return sections;

            // get sections
            Regex subsectionRE = WikiMarkup.GetSubsectionRE();
            int startLine = GetNextSectionStart(lines, 0);
            while (startLine < lines.Count)
            {
                // check starting line
                Match m = subsectionRE.Match(lines[startLine]);
                if (!m.Success)
                    throw new Exception("Invalid starting line for section");

                // get the end of this section
                int endLine = GetSectionEnd(lines, startLine);

                // get lines for this section
                List<string> sectionLines = ExtractLines(lines, startLine, endLine);
                
                // create section, which recursively extracts subsections
                string secName = m.Groups["Name"].ToString();
                secName = WikiMarkup.ProcessMarkup(secName);
                Section s = new Section(secName, sectionLines, parent, containingPage);
                sections.Add(s);

                // get start of next section
                startLine = GetNextSectionStart(lines, endLine + 1);
            }

            return sections;
        }

        /// <summary>
        /// Extract a section and its subsections (and their subsections, etc.) from a list of lines
        /// </summary>
        /// <param name="lines">Lines to extract from</param>
        /// <param name="parent">Parent section for top-level sections</param>
        /// <param name="containingPage">Page that contains these sections</param>
        /// <param name="sectionLayout">Layout of sections, as produced by Section.Layout.</param>
        /// <returns>List of Section instances</returns>
        public static List<Section> ExtractSections(List<string> lines, Section parent, Page containingPage, string sectionLayout)
        {
            List<Section> sections = new List<Section>();

            XmlParser p = new XmlParser(sectionLayout);
            while (true)
            {
                string def = p.OuterXML("section");
                if (def == null)
                    break;

                XmlParser secParser = new XmlParser(def);
                int parentStart = parent != null ? parent.SectionStart : 0;

                int absStart = int.Parse(secParser.AttributeValue("section", "start"));
                int absEnd = int.Parse(secParser.AttributeValue("section", "end"));
                int numLines = absEnd - absStart + 1;

                int start = absStart - parentStart;
                int end = start + numLines - 1;

                List<string> secLines = Section.ExtractLines(lines, start, end);
                sections.Add(new Section(secLines, def, parent, containingPage));
            }

            return sections;
        }

        /// <summary>
        /// Extract a subset of lines from this section
        /// </summary>
        /// <param name="lines"></param>
        /// <param name="startLine"></param>
        /// <param name="endLine"></param>
        /// <returns></returns>
        public static List<string> ExtractLines(List<string> lines, int startLine, int endLine)
        {
            List<string> selected = new List<string>();
            for (int i = startLine; i < lines.Count && i <= endLine; ++i)
                selected.Add(lines[i]);

            return selected;
        }

        /// <summary>
        /// Equality operator
        /// </summary>
        /// <param name="s1"></param>
        /// <param name="s2"></param>
        /// <returns></returns>
        public static bool operator ==(Section s1, Section s2)
        {
            if (!(s1 is Section))
                return (!(s2 is Section));

            return s1.Equals(s2);
        }

        /// <summary>
        /// Inequality operator
        /// </summary>
        /// <param name="s1"></param>
        /// <param name="s2"></param>
        /// <returns></returns>
        public static bool operator !=(Section s1, Section s2)
        {
            return !(s1 == s2);
        }
        #endregion

        #region constructors
        /// <summary>
        /// Constructor
        /// </summary>
        public Section()
        {
            _subSections = new List<Section>();
            _wikiLinks = new List<WikiLink>();
        }
        
        /// <summary>
        /// Constructor for marked up section
        /// </summary>
        /// <param name="name">Name of section</param>
        /// <param name="wikiTextLines">List of lines for the page</param>
        /// <param name="parentSection">Parent section</param>
        /// <param name="containingPage">Page that contains this section</param>
        public Section(string name, List<string> wikiTextLines, Section parentSection, Page containingPage)
            : this()
        {
            if (wikiTextLines == null || wikiTextLines.Count == 0)
                throw new Exception("Cannot create Section with blank text");

            _lines = wikiTextLines;
            _parentSection = parentSection;
            _name = name;
            _containingPage = containingPage;

            // include the first line if it's not a section line (the case for headers)
            Regex sectionRE = WikiMarkup.GetSubsectionRE();
            int secLinesStart;
            if (sectionRE.Match(_lines[0]).Success)
                secLinesStart = 1;
            else
                secLinesStart = 0;

            List<string> subsectionLines = ExtractLines(_lines, secLinesStart, _lines.Count - secLinesStart);
            List<Section> subsections = ExtractSections(subsectionLines, this, _containingPage);
            _subSections.AddRange(subsections);

            // get links prior to first subsection
            int subStart = GetNextSectionStart(_lines, secLinesStart);
            List<string> preSubsectionLines = ExtractLines(_lines, 0, subStart - 1);
            _wikiLinks = WikiLink.GetWikiLinks(preSubsectionLines, this);

            // remove markup
            WikiMarkup.ProcessMarkup(_lines);
        }

        /// <summary>
        /// Constructor for marked up section
        /// </summary>
        /// <param name="name">Name of section</param>
        /// <param name="wikiText">Text for Wiki page</param>
        /// <param name="parentSection">Parent section</param>
        /// <param name="containingPage">Page that contains this section</param>
        public Section(string name, string wikiText, Section parentSection, Page containingPage)
            : this(name, GetLines(wikiText), parentSection, containingPage)
        {
        }

        /// <summary>
        /// Constructor for non-marked up section
        /// </summary>
        /// <param name="lines">Lines to create section from</param>
        /// <param name="sectionDef">Section definition XML</param>
        /// <param name="parentSection">Parent section</param>
        /// <param name="containingPage">Page that contains this section</param>
        public Section(List<string> lines, string sectionDef, Section parentSection, Page containingPage)
            : this()
        {
            XmlParser defParser = new XmlParser(sectionDef);

            _lines = lines;
            _parentSection = parentSection;
            _containingPage = containingPage;
            _name = defParser.AttributeValue("section", "name");
            _sectionStart = int.Parse(defParser.AttributeValue("section", "start"));
            _sectionEnd = int.Parse(defParser.AttributeValue("section", "end"));

            string subsectionDef = defParser.InnerXML("section");
            _subSections = ExtractSections(lines, this, containingPage, "<layout>" + subsectionDef + "</layout>");            
        }

        /// <summary>
        /// Constructor for non-marked up section
        /// </summary>
        /// <param name="name">Name of section</param>
        /// <param name="lines">Lines in section</param>
        /// <param name="parentSection">Parent section</param>
        /// <param name="containingPage">Containing page</param>
        /// <param name="start">Start of section within containing page</param>
        /// <param name="end">End of section within containing page</param>
        public Section(string name, List<string> lines, Section parentSection, Page containingPage, int start, int end)
            : this()
        {
            _name = name;
            _lines = lines;
            _parentSection = parentSection;
            _containingPage = containingPage;
            _sectionStart = start;
            _sectionEnd = end;
        }
        #endregion

        /// <summary>
        /// Sets the line information for this section and all subsections
        /// </summary>
        /// <param name="start"></param>
        public void SetLineInfo(int start)
        {
            _sectionStart = start;
            _sectionEnd = _sectionStart + _lines.Count - 1;

            foreach (Section s in _subSections)
            {
                int subStart = _sectionStart + FindStart(s);
                s.SetLineInfo(subStart);
            }
        }

        /// <summary>
        /// Finds index where a section starts within this section
        /// </summary>
        /// <param name="s">Section to find within this section</param>
        /// <returns>Starting line of `s' within this section</returns>
        public int FindStart(Section s)
        {
            for(int currStart = 0; currStart < _lines.Count; ++currStart)
            {
                int thisLine = currStart;
                int secLine = 0;
                for (; secLine < s.Lines.Count && thisLine < _lines.Count; ++thisLine, ++secLine)
                {
                    string l1 = (string)s.Lines[secLine];
                    string l2 = (string)_lines[thisLine];

                    if (l1 != l2)
                        break;
                }

                if (secLine == s.Lines.Count)
                    return currStart;
            }

            return -1;
        }

        /// <summary>
        /// Gets a section at a specific index
        /// </summary>
        /// <param name="indexes"></param>
        /// <returns></returns>
        public Section GetSection(int[] indexes)
        {
            if (indexes.Length == 0)
                return this;

            int[] newIndexes = new int[indexes.Length - 1];
            for (int i = 1; i < indexes.Length; ++i)
                newIndexes[i - 1] = indexes[i];

            return _subSections[indexes[0] - 1].GetSection(newIndexes);
        }

        /// <summary>
        /// Adds a link to this section
        /// </summary>
        /// <param name="wl"></param>
        public void AddLink(WikiLink wl)
        {
            _wikiLinks.Add(wl);
        }

        /// <summary>
        /// Removes markup from lines in this section and all subsections
        /// </summary>
        public void RemoveMarkup()
        {
            WikiMarkup.ProcessMarkup(_lines);
            foreach (Section s in _subSections)
                s.RemoveMarkup();
        }

        /// <summary>
        /// Gets the layout of the section
        /// </summary>
        /// <param name="includeText">Whether or not to include the text of the section in the layout</param>
        /// <param name="includeNonContentSections">Whether or not to include non-content sections in the layout</param>
        public string GetLayout(bool includeText, bool includeNonContentSections)
        {
            // ignore non-content sections
            if (!includeNonContentSections &&
                _nonContentSections.Contains(_name.ToLower()))
                return "";

            string nameAttrib = XmlParser.EscapeAttribute("name", _name);
            string layout = "<section " + nameAttrib + " id=\"" + ID + "\" start=\"" + _sectionStart + "\" end=\"" + _sectionEnd + "\">";

            // add text if needed
            if (includeText)
                layout += "<text><![CDATA[" + Text + "]]></text>";

            // get subsection layout
            foreach (Section s in _subSections)
                layout += s.GetLayout(includeText, includeNonContentSections);
            layout += "</section>";
            return layout;
        }

        #region overrides
        /// <summary>
        /// Gets whether or not two sections are equivalent
        /// </summary>
        /// <param name="obj"></param>
        /// <returns></returns>
        public override bool Equals(object obj)
        {
            if (!(obj is Section))
                return false;

            Section s = (Section)obj;
            return _containingPage == s.ContainingPage && _sectionStart == s.SectionStart && _sectionEnd == s.SectionEnd;
        }

        /// <summary>
        /// Gets hash code for this section
        /// </summary>
        /// <returns>Hash code for this section</returns>
        public override int GetHashCode()
        {
            return _name.GetHashCode();
        }
        #endregion
    }
}
