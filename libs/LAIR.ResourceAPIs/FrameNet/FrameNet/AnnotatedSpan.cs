using System;
using System.Collections.Generic;
using System.Text;

namespace LAIR.ResourceAPIs.FrameNet
{
    /// <summary>
    /// Represents an annotated span of text
    /// </summary>
    public class AnnotatedSpan
    {
        private int _start;
        private string _value;

        /// <summary>
        /// Gets or sets the start position
        /// </summary>
        public int Start
        {
            get { return _start; }
        }

        /// <summary>
        /// Gets the end position
        /// </summary>
        public int End
        {
            get { return _start + _value.Length - 1; }
        }

        /// <summary>
        /// Gets the length
        /// </summary>
        public int Length
        {
            get { return _value.Length; }
        }

        /// <summary>
        /// Gets or sets the value
        /// </summary>
        public string Value
        {
            get { return _value; }
        }

        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="start">Start of span</param>
        /// <param name="value">Value of span</param>
        public AnnotatedSpan(int start, string value)
        {
            if (start < 0 || value == null)
                throw new Exception("Invalid span parameters");

            _start = start;
            _value = value;
        }

        /// <summary>
        /// Returns the value of the span
        /// </summary>
        /// <returns></returns>
        public override string ToString()
        {
            return _value;
        }
    }
}
