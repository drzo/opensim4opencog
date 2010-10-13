using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace LAIR.ResourceAPIs.FrameNet
{
    /// <summary>
    /// Represents a lexeme
    /// </summary>
    public class Lexeme
    {
        private string _value;
        private string _pos;
        private bool _breakBefore;
        private bool _head;

        /// <summary>
        /// Gets the POS
        /// </summary>
        public string POS
        {
            get { return _pos; }
        }

        /// <summary>
        /// Gets the value of this lexeme
        /// </summary>
        public string Value
        {
            get { return _value; }
        }

        /// <summary>
        /// Gets whether or not words may be inserted before this lexeme (for multi-word LUs)
        /// </summary>
        public bool BreakBefore
        {
            get { return _breakBefore; }
        }

        /// <summary>
        /// Gets whether or not this lexeme is the head of its lexical unit
        /// </summary>
        public bool Head
        {
            get { return _head; }
        }

        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="value">Value of lexeme</param>
        /// <param name="pos">Part of speech</param>
        /// <param name="breakBefore">Whether or not words may be inserted before this lexeme (for multi-word LUs)</param>
        /// <param name="head">Whether or not this lexeme is the head of its LU</param>
        public Lexeme(string value, string pos, bool breakBefore, bool head)
        {
            _value = value;
            _pos = pos;
            _breakBefore = breakBefore;
            _head = head;
        }

        /// <summary>
        /// Checks whether or not this lexeme equals another
        /// </summary>
        /// <param name="obj"></param>
        /// <returns></returns>
        public override bool Equals(object obj)
        {
            if (!(obj is Lexeme))
                return false;

            Lexeme lexeme = obj as Lexeme;

            return _value == lexeme.Value && _breakBefore == lexeme.BreakBefore && _head == lexeme.Head && _pos == lexeme.POS;
        }

        /// <summary>
        /// Gets hash code
        /// </summary>
        /// <returns></returns>
        public override int GetHashCode()
        {
            return _value.GetHashCode();
        }

        /// <summary>
        /// Returns value of lexeme
        /// </summary>
        /// <returns></returns>
        public override string ToString()
        {
            return _value;
        }
    }
}
