using System;
using System.Collections.Generic;
using System.Text;
using System.Text.RegularExpressions;
using System.Collections;
using System.IO;

using LAIR.ResourceAPIs.Wikipedia.Properties;
using LAIR.Collections.Generic;

namespace LAIR.ResourceAPIs.Wikipedia
{
    /// <summary>
    /// WikiMarkup class, based on documentation at http://en.wikipedia.org/wiki/Help:Editing_tips_and_tricks
    /// </summary>
    public static class WikiMarkup
    {
        private static List<string> _langCodes;
        private static Regex _subsectionRE;
        private static Regex _wikiLinkRE;
        private static Regex _emphasisRE;
        private static Regex _templateRE;
        private static Regex _htmlRE;
        private static Regex _imageRE;
        private static Regex _listRE;
        private static Regex _httpLinkRE;
        private static Regex _refRE;
        private static Regex _nonWord;
        private static Regex _htmlCommentRE;
        private static Regex _interLangLinkRE;
        private static Regex _categoryLinkRE;
        private static Regex _tableRE;
        private static Set<string> _stopWords;

        /// <summary>
        /// Static constructor
        /// </summary>
        static WikiMarkup()
        {
                           // ISO-639-1 language codes
            string langs = "aa ab ae af ak am an ar as av ay az ba be bg bh bi bm bn bo br bs ca ce ch co " +
                           "cr cs cu cv cy da de dv dz ee el en eo es et eu fa ff fi fj fo fr fy ga gd gl " +
                           "gn gu gv ha he hi ho hr ht hu hy hz ia id ie ig ii ik io is it iu ja jv ka kg " +
                           "ki kj kk kl km kn ko kr ks ku kv kw ky la lb lg li ln lo lt lu lv mg mh mi mk " +
                           "ml mn mo mr ms mt my na nb nd ne ng nl nn no nr nv ny oc oj om or os pa pi pl " +
                           "ps pt qu rm rn ro ru rw sa sc sd se sg sh si sk sl sm sn so sq sr ss st su sv " +
                           "sw ta te tg th ti tk tl tn to tr ts tt tw ty ug uk ur uz ve vi vo wa wo xh yi " +
                           "yo za zh zu" +

                           // some longer language codes from Wikipedia documentation
                           "ceb lmo simple nap bpy new scn ast ru-sib ksh nds pms vec als pam zh-yue zh-min-nan " +
                           "nrm nds-nl war frp nov sco fur lij pdc vls bat-smg diq map-bms lad csb nah fiu-vro " +
                           "zh-classical ang hsb bar jbo arc tpi wuu roa-rup udm tokipona pag tet rmy chr eml " +
                           "glk cbk-zam cdo tlh zea pap haw roa-tara mzn xal tum bxr pih mus bug chy lbe cho got ilo";

            _langCodes = new List<string>(langs.Split(new char[] { ' ' }, StringSplitOptions.RemoveEmptyEntries));

            _subsectionRE = GetSubsectionRE();
            _wikiLinkRE = GetWikiLinkRE();
            _emphasisRE = GetEmphasisRE();
            _templateRE = GetTemplateRE();
            _htmlRE = GetHTMLRE();
            _imageRE = GetImageRE();
            _listRE = GetListRE();
            _httpLinkRE = GetHTTPLinkRE();
            _refRE = GetReferenceRE();
            _nonWord = new Regex(@"\W");
            _htmlCommentRE = GetHTMLCommentRE();
            _interLangLinkRE = GetInterLangLinkRE();
            _categoryLinkRE = GetCategoryLinkRE();
            _tableRE = GetTableRE();
        }

        #region subsection
        /// <summary>
        /// Gets new instance of Subsection Regex. Subsections look like this:
        ///   == Subsection ==
        ///   === Subsubsection ===
        /// Group "Name" holds the section name
        /// Group "SectionStart" holds the section start (== and === from above)
        /// </summary>
        /// <returns>Regex matching subsection markup</returns>
        public static Regex GetSubsectionRE()
        {
            return new Regex(@"(?<SectionStart>^=+)(?<Name>.+)\k<SectionStart>\s*$");
        }

        /// <summary>
        /// Gets the replacement string for matched subsection markup
        /// </summary>
        /// <param name="m">Match instance</param>
        /// <returns>Replacement string for matched subsection markup</returns>
        public static string GetSubsectionReplacement(Match m)
        {
            return m.Groups["Name"].Value.Trim();
        }

        /// <summary>
        /// Match evaluator for subsection replacement
        /// </summary>
        public static readonly MatchEvaluator SubsectionReplacementME = new MatchEvaluator(GetSubsectionReplacement);
        #endregion

        #region Wiki links
        /// <summary>
        /// Gets new instance of Wiki link Regex. Wiki links are links to internal
        /// Wikipedia pages. They have a few different forms (see documentation linked to 
        /// in class description.
        /// Group "LinkDest" contains the destination page title
        /// Group "Section" contains the destination page section
        /// Group "DisplayText" contains the text to be displayed for the link
        /// </summary>
        /// <returns>Regex matching Wiki link markup</returns>
        public static Regex GetWikiLinkRE()
        {
            return new Regex(GetWikiLinkPattern());                                  
        }

        /// <summary>
        /// Gets the pattern string for wiki links
        /// </summary>
        /// <returns>Pattern string for wiki links</returns>
        private static string GetWikiLinkPattern()
        {
            string linkText = @"[^\[\]\|]+";

            string linkDest = @"(?<LinkDest>" + linkText + ")";
            string section = @"#(?<Section>" + linkText + ")";
            string displayText = @"\|(?<DisplayText>" + linkText + ")";
            string pipeTrick = @"((?<LinkDest>" + linkText + @")\|)";
            string start = @"(\[\[\s*";
            string end = @"\s*\]\])";

            string pattern = "(" +
                                  start + linkDest + end + "|" +
                                  start + linkDest + displayText + end + "|" +
                                  start + linkDest + section + end + "|" +
                                  start + linkDest + section + displayText + end + "|" +
                                  start + pipeTrick + end + ")";

            return pattern;
        }

        /// <summary>
        /// Gets the replacement text for a matched Wiki link
        /// </summary>
        /// <param name="m"></param>
        /// <returns></returns>
        public static string GetWikiLinkReplacement(Match m)
        {
            string displayText = m.Groups["DisplayText"].Value.Trim();
            string linkPage = m.Groups["LinkDest"].Value.Trim();

            return displayText != "" ? displayText : linkPage;
        }

        /// <summary>
        /// MatchEvaluator for Wiki links
        /// </summary>
        public static readonly MatchEvaluator WikiLinkReplacementME = new MatchEvaluator(GetWikiLinkReplacement);
        #endregion

        #region emphasis
        /// <summary>
        /// Gets new instance of emphasis Regex. 
        /// </summary>
        /// <returns></returns>
        public static Regex GetEmphasisRE()
        {
            string emphText = @"(?<EmphText>([^']('[^'])*)+)";
            string italics = "(''" + emphText + "'')";
            string bold = "('''" + emphText + "''')";
            string boldItalics = "('''''" + emphText + "''''')";

            // find longest matching string - BI, B, I
            string pattern = "(" + boldItalics + "|" + bold + "|" + italics + ")";

            return new Regex(pattern);            
        }

        /// <summary>
        /// Gets replacement string for emphasized markup
        /// </summary>
        /// <param name="m">Match for markup</param>
        /// <returns>Replacement string</returns>
        public static string GetEmphasisReplacement(Match m)
        {
            return m.Groups["EmphText"].Value.Trim();
        }

        /// <summary>
        /// MatchEvaluator for emphasis replacement
        /// </summary>
        public static readonly MatchEvaluator EmphasisReplacementME = new MatchEvaluator(GetEmphasisReplacement);
        #endregion

        #region HTTP links
        /// <summary>
        /// Gets a new HTTP link RE
        /// </summary>
        /// <returns>New HTTP link RE</returns>
        public static Regex GetHTTPLinkRE()
        {
            // from RFC 1738
            string alnum = "a-zA-Z0-9";
            string reserved = @";/?:@=&";
            string other = @"$\-_.+!*'(),";

            string url = @"(https?://[" + alnum + reserved + other + "]+)";
            string p1 = @"(\[\s*" + url + @"\s*\])";
            string p2 = @"(\[\s*" + url + @"\s(?<DisplayText>[^\]]+)\])";

            return new Regex(url + "|" + p1 + "|" + p2);
        }

        /// <summary>
        /// Gets replacement text for http links
        /// </summary>
        /// <param name="m">Match to get replacement text for</param>
        /// <returns>Replacement text for http links</returns>
        public static string GetHTTPLinkReplacement(Match m)
        {
            return m.Groups["DisplayText"].Value.Trim();
        }

        /// <summary>
        /// MatchEvaluator for HTTP links
        /// </summary>
        public static readonly MatchEvaluator HTTPLinkME = new MatchEvaluator(GetHTTPLinkReplacement);
        #endregion

        #region templates, html, images, lists, references, html comments, tables, interlanguage links (all null replacements)
        /// <summary>
        /// Gets a new instance of template Regex
        /// </summary>
        /// <returns>New instance of template Regex</returns>
        public static Regex GetTemplateRE()
        {
            string start = @"\{\{";
            string end = @"\}\}";
            string middle = "([^{}]|([{}][^{}]))*";
            string pattern = start + middle + end;

            return new Regex(pattern);            
        }

        /// <summary>
        /// Gets a RE for HTML comments
        /// </summary>
        /// <returns>RE for HTML comments</returns>
        public static Regex GetHTMLCommentRE()
        {
            return new Regex("<!--([^-]|(-[^-]))*-->");
        }

        /// <summary>
        /// Gets a new instance of the HTML regex
        /// </summary>
        /// <returns>New instance of the HTML regex</returns>
        public static Regex GetHTMLRE()
        {
            string elName = "[^<>!]+";
            string startTag = "(<" + elName + ">)";
            string endTag = "(</" + elName + ">)";
            string emptyTag = "(<" + elName + "/>)";

            string pattern = "(" + startTag + "|" + endTag + "|" + emptyTag + ")";

            return new Regex(pattern);
        }

        /// <summary>
        /// Gets new instance of image RE
        /// </summary>
        /// <returns>New instance of image RE</returns>
        public static Regex GetImageRE()
        {
            string noBrace = @"[^[\]]";
            string singleBrace = @"\[" + noBrace + @"*\]";
            string doubleBrace = @"\[\[" + noBrace + @"*\]\]";
            string nestedBraces = "(" + singleBrace + "|" + doubleBrace + ")";
            string pattern = @"\[\[\s*[Ii]mage\s*:(" + noBrace + "|" + nestedBraces + @")*\]\]";
            return new Regex(pattern);
        }

        /// <summary>
        /// Gets a new list RE
        /// </summary>
        /// <returns>List RE</returns>
        public static Regex GetListRE()
        {
            string newLine = @"((\r\n)|\n)";
            string startLine = "(^|" + newLine + ")";
            string bulletList = "(" + startLine + @"\s*\*+)";
            string numberList = "(" + startLine + @"\s*#+)";
            return new Regex(bulletList + "|" + numberList);
        }

        /// <summary>
        /// Gets a RE for references
        /// </summary>
        /// <returns></returns>
        public static Regex GetReferenceRE()
        {
            string refStart = @"<ref([^>/]|(/[^/\s>]))*>";
            string refEnd = "</ref>";
            string refText = "([^<]|(<[^/]))*";
            string emptyRef = "<ref[^>]*/>";
            string refPattern = "(" + refStart + refText + refEnd + ")|(" + emptyRef + ")";

            return new Regex(refPattern);
        }

        /// <summary>
        /// Gets a RE for interlanguage links
        /// </summary>
        /// <returns></returns>
        public static Regex GetInterLangLinkRE()
        {
            string pattern = @"\[\[:?(";
            foreach (string lc in _langCodes)
                pattern += "(" + lc + ")|";
            pattern += @"(simple)):[^\]]+\]\]";
            return new Regex(pattern);
        }

        /// <summary>
        /// Gets a RE for category links
        /// </summary>
        /// <returns></returns>
        public static Regex GetCategoryLinkRE()
        {
            return new Regex(@"\[\[\s*Category\s*:[^]]+\]\]");
        }

        /// <summary>
        /// Gets a RE for tables
        /// </summary>
        /// <returns></returns>
        public static Regex GetTableRE()
        {
            string start = @"((\{\|)";
            start += "|(<table>))";
            string end = @"((\|\})";
            end += "|(</table>))";
            return new Regex(start + @"([^|]|(\|[^}]))*" + end);            
        }
        #endregion

        /// <summary>
        /// Remove markup from Wiki text
        /// </summary>
        /// <param name="s">String to remove markup from</param>
        /// <returns>String without markup</returns>
        public static string ProcessMarkup(string s)
        {
            string original = s;

            s = _emphasisRE.Replace(s, EmphasisReplacementME);       // must be before anything
            s = _httpLinkRE.Replace(s, HTTPLinkME);
            s = _wikiLinkRE.Replace(s, WikiLinkReplacementME);
            s = _subsectionRE.Replace(s, SubsectionReplacementME);
            s = s.Trim();

            // recursively remove embedded markup
            if(s != original)
                return ProcessMarkup(s);

            return s;
        }

        /// <summary>
        /// Remove markup from a list of lines
        /// </summary>
        /// <param name="lines">Lines to remove markup from</param>
        public static void ProcessMarkup(List<string> lines)
        {
            for (int i = 0; i < lines.Count;)
            {
                string line = lines[i].Trim();
                if (line == "")
                {
                    lines.RemoveAt(i);
                    continue;
                }

                // remove markup from line
                line = ProcessMarkup(line);

                if (line == "")
                    lines.RemoveAt(i);
                else
                {
                    lines[i] = line;
                    ++i;
                }               
            }
        }

        /// <summary>
        /// Trims leading and trailing punctuation from a string
        /// </summary>
        /// <param name="s">String to trim</param>
        /// <returns>`s', without leading and trailing punctuation</returns>
        public static string TrimPunctuation(string s)
        {
            // remove leading and trailing punctuation
            bool modified = true;
            while (modified)
            {
                modified = false;
                if (s.Length > 1 && _nonWord.Match(s[0].ToString()).Success)
                {
                    s = s.Substring(1);
                    modified = true;
                }

                if (s.Length > 0 && _nonWord.Match(s[s.Length - 1].ToString()).Success)
                {
                    s = s.Substring(0, s.Length - 1);
                    modified = true;
                }
            }
            return s;
        }

        /// <summary>
        /// Removes irrelevant markup from Wiki text
        /// </summary>
        /// <param name="s">String to process</param>
        /// <returns>String, sans markup</returns>
        public static string RemoveIrrelevantMarkup(string s)
        {
            string original = s;

            s = _refRE.Replace(s, "");                       
            s = _templateRE.Replace(s, "");
            s = _listRE.Replace(s, "\n");
            s = _imageRE.Replace(s, "");
            s = _interLangLinkRE.Replace(s, "");             
            s = _categoryLinkRE.Replace(s, "");              
            s = _tableRE.Replace(s, "");
            s = _htmlCommentRE.Replace(s, "");
            s = _htmlRE.Replace(s, "");                

            if (original != s)
                return RemoveIrrelevantMarkup(s);

            Regex extraWS = new Regex("  +");
            return extraWS.Replace(s, " ");
        }


        /// <summary>
        /// Check if a word is a stop word (case-insensitive)
        /// </summary>
        /// <param name="word">Word to check</param>
        /// <returns>True if word is stop word, False otherwise</returns>
        public static bool IsStopWord(string word)
        {
            return IsStopWord(word, true);
        }

        /// <summary>
        /// Check if a word is a stop word
        /// </summary>
        /// <param name="word"></param>
        /// <param name="caseSensitive"></param>
        /// <returns></returns>
        public static bool IsStopWord(string word, bool caseSensitive)
        {
            if (_stopWords == null)
            {
                string[] stopWords = Resources.stopwords.Split(new char[] { '\n' }, StringSplitOptions.RemoveEmptyEntries);
                _stopWords = new Set<string>();

                // read stop words
                for (int i = 0; i < stopWords.Length; ++i)
                {
                    string s = stopWords[i].Trim();
                    _stopWords.Add(s);
                }
            }

            if (!caseSensitive)
                word = word.ToLower();

            word = TrimPunctuation(word);
            if (word == "")
                return true;

            return _stopWords.Contains(word);
        }
    }
}
