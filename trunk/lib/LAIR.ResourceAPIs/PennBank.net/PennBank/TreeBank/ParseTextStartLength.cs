using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace LAIR.ResourceAPIs.PennBank.TreeBank
{
    /// <summary>
    /// Stores parse tree byte start and length values.
    /// </summary>
    public class ParseTextStartLength
    {
        private int _start;
        private int _length;

        /// <summary>
        /// Gets the start byte of the sentence
        /// </summary>
        public int Start
        {
            get { return _start; }
        }

        /// <summary>
        /// Gets the byte length of the sentence
        /// </summary>
        public int Length
        {
            get { return _length; }
        }

        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="start">Start byte of sentence</param>
        /// <param name="length">Byte length of sentence</param>
        public ParseTextStartLength(int start, int length)
        {
            _start = start;
            _length = length;
        }
    }
}
