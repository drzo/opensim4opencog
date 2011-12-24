using System;
using System.Collections.Generic;
using System.Text;
using System.Collections;
using System.Text.RegularExpressions;
using System.Data.Odbc;

namespace LAIR.ResourceAPIs.Wikipedia
{
    /// <summary>
    /// Wiki link class
    /// </summary>
    public class WikiLink : IComparable
    {
        private string _rawLink;
        private string _destPageTitle;
        private string _destPageSection;
        private string _displayText;
        private Page _sourcePage;
        private Page _destPage;
        private WikiDB.Namespace _destPageNamespace;
        private float _weight;
        private Section _sourceSection;

        /// <summary>
        /// Gets or sets the raw link (with markup) for this Wiki link
        /// </summary>
        public string RawLink
        {
            get { return _rawLink; }
            set { _rawLink = value; }
        }

        /// <summary>
        /// Gets or sets the title of the page being linked to
        /// </summary>
        public string DestPageTitle
        {
            get { return _destPageTitle; }
            set { _destPageTitle = value; }
        }

        /// <summary>
        /// Gets or sets the section of the page being linked to
        /// </summary>
        public string DestPageSection
        {
            get { return _destPageSection; }
            set { _destPageSection = value; }
        }

        /// <summary>
        /// Gets the URL for destination page
        /// </summary>
        public string DestPageURL
        {
            get
            {
                return WikiDB.GetURLFromPageTitle(_destPageTitle);
            }
        }

        /// <summary>
        /// Gets or sets the display text for this Wiki link
        /// </summary>
        public string DisplayText
        {
            get { return _displayText; }
            set { _displayText = value; }
        }

        /// <summary>
        /// Gets or sets the source page
        /// </summary>
        public Page SourcePage
        {
            get
            {
                if (_sourcePage != null)
                    return _sourcePage.IsRedirect ? _sourcePage.RedirectsTo : _sourcePage;

                return null;
            }
            set
            {
                if (_sourcePage != null && _sourcePage.IsRedirect)
                    _sourcePage.RedirectsTo = value;
                else
                    _sourcePage = value;

                if (SourcePage != null && DestPage != null)
                    _weight = SourcePage.Similarity(DestPage);
            }
        }

        /// <summary>
        /// Gets or sets the source section for this link
        /// </summary>
        public Section SourceSection
        {
            get { return _sourceSection; }
            set { _sourceSection = value; }
        }

        /// <summary>
        /// Gets or sets the page this link points to
        /// </summary>
        public Page DestPage
        {
            get
            {
                if (_destPage != null)
                    return _destPage.IsRedirect ? _destPage.RedirectsTo : _destPage;

                return null;
            }
            set
            {
                if (_destPage != null && _destPage.IsRedirect)
                    _destPage.RedirectsTo = value;
                else
                    _destPage = value;

                if (SourcePage != null && DestPage != null)
                    _weight = SourcePage.Similarity(DestPage);
            }
        }

        /// <summary>
        /// Gets or sets the namespace this link points to
        /// </summary>
        public WikiDB.Namespace DestPageNamespace
        {
            get { return _destPageNamespace; }
            set { _destPageNamespace = value; }
        }

        /// <summary>
        /// Gets or sets the weight of this link
        /// </summary>
        public float Weight
        {
            get { return _weight; }
            set { _weight = value; }
        }

        /// <summary>
        /// Gets the redirect page for this link
        /// </summary>
        public Page RedirectPage
        {
            get { return _destPage.IsRedirect ? _destPage : null; }
        }

        /// <summary>
        /// Gets WikiLinks from a section
        /// </summary>
        /// <param name="lines"></param>
        /// <param name="source"></param>
        /// <returns>List of links for section</returns>
        public static List<WikiLink> GetWikiLinks(List<string> lines, Section source)
        {
            List<WikiLink> wikiLinks = new List<WikiLink>();

            Regex wikiLinkRE = WikiMarkup.GetWikiLinkRE();
            Regex subsectionRE = WikiMarkup.GetSubsectionRE();
            Regex emphasisRE = WikiMarkup.GetEmphasisRE();
            foreach (string line in lines)
            {
                MatchCollection mc = wikiLinkRE.Matches(line);
                foreach (Match linkMatch in mc)
                {
                    // create wiki link
                    string rawLink = linkMatch.Value;
                    string page = linkMatch.Groups["LinkDest"].ToString().Trim();
                    string section = linkMatch.Groups["Section"].ToString().Trim();
                    string displayText = linkMatch.Groups["DisplayText"].ToString().Trim();
                    WikiLink link = new WikiLink(rawLink, page, section, displayText, source);

                    wikiLinks.Add(link);
                }
            }

            wikiLinks.Sort();
            return wikiLinks;
        }

        /// <summary>
        /// Remove duplicate links from a list of links
        /// </summary>
        /// <param name="links"></param>
        public static void RemoveDuplicateLinks(List<WikiLink> links)
        {
            // remove duplicate links
            links.Sort();
            for (int i = 0; i < links.Count; ++i)
            {
                WikiLink link1 = links[i];
                for (int j = i + 1; j < links.Count; )
                {
                    WikiLink link2 = links[j];
                    if (link1 == link2)
                        links.RemoveAt(j);
                    else
                        break;
                }
            }
        }

        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="rawLink">Raw text for link</param>
        /// <param name="pageTitle">Title of page being linked to</param>
        /// <param name="pageSection">Section of page being linked to</param>
        /// <param name="displayText">Display text for link</param>
        /// <param name="sourceSection">Section of source page that contains this link</param>
        public WikiLink(string rawLink, string pageTitle, string pageSection, string displayText, Section sourceSection)
        {
            _rawLink = rawLink;
            _destPageTitle = pageTitle;
            _destPageSection = pageSection;
            _displayText = displayText != "" ? displayText : (pageTitle + (_destPageSection != "" ? "#" + _destPageSection : ""));
            _displayText = WikiMarkup.ProcessMarkup(_displayText);
            _destPageNamespace = WikiDB.Namespace.Main;
            _sourcePage = sourceSection.ContainingPage;
            _sourceSection = sourceSection;
            _weight = 0;
        }

        /// <summary>
        /// Look up this link
        /// </summary>
        /// <param name="database">Database to look up link destination in</param>
        /// <returns>True if destination page was found, False otherwise</returns>
        public bool LookupDestPage(WikiDB database)
        {
            DestPage = database.LookupPage(_destPageNamespace, DestPageURL, true);

            return DestPage != null;
        }

        /// <summary>
        /// Equality check
        /// </summary>
        /// <param name="obj">Object to compare to</param>
        /// <returns>True if obj equals this WikiLink, False otherwise</returns>
        public override bool Equals(object obj)
        {
            if (!(obj is WikiLink))
                return false;

            WikiLink wl = (WikiLink)obj;

            if (SourcePage != wl.SourcePage || 
                SourceSection.ID != wl.SourceSection.ID)
                return false;

            // if we've loaded destination pages, compare them
            if (DestPage != null && wl.DestPage != null)
                return DestPage == wl.DestPage;

            // otherwise, compare destination URLs
            return DestPageURL == wl.DestPageURL;
        }

        /// <summary>
        /// Gets hash code for this Wiki link
        /// </summary>
        /// <returns>Hash code for this Wiki link</returns>
        public override int GetHashCode()
        {
            return _rawLink.GetHashCode();
        }

        /// <summary>
        /// CompareTo function
        /// </summary>
        /// <param name="obj">Object to compare this WikiLink to</param>
        /// <returns>1 if this gt obj, -1 if this lt obj, and 0 o.w.</returns>
        public int CompareTo(object obj)
        {
            if (!(obj is WikiLink))
                return 1;

            WikiLink wl = (WikiLink)obj;

            int weightCmp = _weight.CompareTo(wl.Weight);
            int urlCmp = DestPageURL.CompareTo(wl.DestPageURL);

            return weightCmp != 0 ? weightCmp : urlCmp;
        }

        /// <summary>
        /// Equality operator
        /// </summary>
        /// <param name="wl1">Operand 1</param>
        /// <param name="wl2">Operand 2</param>
        /// <returns>True if links are equal, False otherwise</returns>
        public static bool operator ==(WikiLink wl1, WikiLink wl2)
        {
            return wl1.Equals(wl2);
        }

        /// <summary>
        /// Inequality operator
        /// </summary>
        /// <param name="wl1">Operant 1</param>
        /// <param name="wl2">Operand 2</param>
        /// <returns>True if links are equal, False otherwise</returns>
        public static bool operator !=(WikiLink wl1, WikiLink wl2)
        {
            return !(wl1 == wl2);
        }

        /// <summary>
        /// ToString override
        /// </summary>
        /// <returns>Display text of link</returns>
        public override string ToString()
        {
            string weight = "0.0000000";
            if(_weight > 0.0D)
                weight = Math.Round(_weight, 7).ToString().PadRight(9, '0');
            if (_weight == 1.0D)
                weight = "1.0000000";

            return weight + "  " + (DestPage != null ? DestPage.URL : DestPageURL) + " (" + _displayText + ")";
        }

        /// <summary>
        /// Reverse sorter for WikiLink class
        /// </summary>
        public class ReverseSorter : IComparer<WikiLink>
        {
            /// <summary>
            /// Compares two WikiLink objects
            /// </summary>
            /// <param name="x">First WikiLink</param>
            /// <param name="y">Second WikiLink</param>
            /// <returns></returns>
            public int Compare(WikiLink x, WikiLink y)
            {
                int normCmp = x.CompareTo(y);
                return normCmp == 1 ? -1 : (normCmp == -1 ? 1 : 0);
            }
        }
    }
}
