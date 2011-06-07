using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Text.RegularExpressions;

namespace LAIR.ResourceAPIs.PennBank.TreeBank
{
    /// <summary>
    /// Encapsulates a raw parse text string so that copies are not made during the recursive parsing process
    /// </summary>
    public class RawParseText
    {
        private readonly string _rawParseText;

        /// <summary>
        /// Gets a character at a location
        /// </summary>
        /// <param name="index"></param>
        /// <returns></returns>
        public char this[int index]
        {
            get { return _rawParseText[index]; }
        }
            
        /// <summary>
        /// Gets the length of the parse text
        /// </summary>
        public int Length
        {
            get { return _rawParseText.Length; }
        }

        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="rawParseText">Raw parse text to store</param>
        public RawParseText(string rawParseText)
        {
            _rawParseText = rawParseText;
        }

        /// <summary>
        /// Gets a substring of the parse text
        /// </summary>
        /// <param name="startIndex">Start of substring</param>
        /// <param name="length">Length of substring</param>
        public string Substring(int startIndex, int length)
        {
            return _rawParseText.Substring(startIndex, length);
        }

        /// <summary>
        /// Gets a match for a RE
        /// </summary>
        /// <param name="re">RE to match</param>
        /// <param name="start">Where to start match</param>
        /// <returns>Match</returns>
        public Match GetMatch(Regex re, int start)
        {
            return re.Match(_rawParseText, start);
        }
    }
}
