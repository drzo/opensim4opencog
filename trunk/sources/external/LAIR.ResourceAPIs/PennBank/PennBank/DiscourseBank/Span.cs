using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace LAIR.ResourceAPIs.PennBank.DiscourseBank
{
    /// <summary>
    /// Represents a span of text with start and end token numbers
    /// </summary>
    public class Span
    {
        private int _startToken;
        private int _endToken;

        /// <summary>
        /// Gets the start token
        /// </summary>
        public int StartToken
        {
            get { return _startToken; }
        }

        /// <summary>
        /// Gets the end token
        /// </summary>
        public int EndToken
        {
            get { return _endToken; }
        }

        /// <summary>
        /// Gets the length (in tokens)
        /// </summary>
        public int Length
        {
            get { return _endToken - _startToken + 1; }
        }

        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="startToken">Start token</param>
        /// <param name="endToken">End token</param>
        public Span(int startToken, int endToken)
        {
            _startToken = startToken;
            _endToken = endToken;

            if (_endToken < _startToken)
                throw new Exception("End token precedes start token");
        }

        /// <summary>
        /// Gets nicely formatted string for this span
        /// </summary>
        /// <returns>Nicely formatted string for this span</returns>
        public override string ToString()
        {
            return "[" + _startToken + "..." + _endToken + "]";
        }
    }
}
