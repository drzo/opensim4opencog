using System;
using System.Collections.Generic;
using System.Text;
using System.IO;

using LAIR.XML;
using LAIR.Collections.Generic;

namespace LAIR.ResourceAPIs.FrameNet
{
    /// <summary>
    /// Represents a lexical unit
    /// </summary>
    public class LexicalUnit
    {
        private int _id;
        private string _definition;
        private string _name;
        private string _pos;
        private Set<Lexeme> _lexemes;

        /// <summary>
        /// Gets the lexemes on this lexical unit
        /// </summary>
        public Set<Lexeme> Lexemes
        {
            get { return _lexemes; }
        }

        /// <summary>
        /// Gets the ID
        /// </summary>
        public int ID
        {
            get { return _id; }
        }

        /// <summary>
        /// Gets the name
        /// </summary>
        public string Name
        {
            get { return _name; }
        }

        /// <summary>
        /// Gets the POS
        /// </summary>
        public string POS
        {
            get { return _pos; }
        }

        /// <summary>
        /// Gets the definition
        /// </summary>
        public string Definition
        {
            get { return _definition; }
        }

        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="id">ID for LU</param>
        /// <param name="name">Name of LU</param>
        /// <param name="pos">POS of LU</param>
        /// <param name="definition">Definition of LU</param>
        /// <param name="lexemes">Lexemes on this lexical unit</param>
        public LexicalUnit(int id, string name, string pos, string definition, Set<Lexeme> lexemes)
        {
            _id = id;
            _name = name;
            _pos = pos;
            _definition = definition;
            _lexemes = lexemes;
        }

        /// <summary>
        /// Gets a concatenation of all lexemes defined on this lexunit
        /// </summary>
        /// <returns></returns>
        public override string ToString()
        {
            StringBuilder text = new StringBuilder();
            bool prependSpace = false;
            foreach (Lexeme lexeme in _lexemes)
                text.Append((prependSpace ? " " : "") + lexeme);

            return text.ToString();
        }

        /// <summary>
        /// Gets hash code for this lexical unit
        /// </summary>
        /// <returns>Hash code for this lexical unit</returns>
        public override int GetHashCode()
        {
            return _id.GetHashCode();
        }

        /// <summary>
        /// Checks whether or not this lexical unit equals another
        /// </summary>
        /// <param name="obj"></param>
        /// <returns></returns>
        public override bool Equals(object obj)
        {
            if (!(obj is LexicalUnit))
                return false;

            LexicalUnit lu = obj as LexicalUnit;

            return _id == lu.ID;
        }
    }
}
