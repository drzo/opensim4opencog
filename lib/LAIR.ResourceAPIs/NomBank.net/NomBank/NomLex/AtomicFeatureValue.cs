using System;
using System.Collections.Generic;
using System.Text;

namespace LAIR.ResourceAPIs.NomBank.NomLex
{
    /// <summary>
    /// Represents an atomic NomLex feature value
    /// </summary>
    public class AtomicFeatureValue : FeatureValue
    {
        private string _atom;

        /// <summary>
        /// Gets or sets the atom
        /// </summary>
        public string Atom
        {
            get { return _atom; }
            set { _atom = value; }
        }

        /// <summary>
        /// Constructor
        /// </summary>
        public AtomicFeatureValue()
        {
        }
        
        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="atom">Atom for this value</param>
        public AtomicFeatureValue(string atom)
            : this()
        {
            _atom = atom;
        }

        /// <summary>
        /// ToString override
        /// </summary>
        /// <returns></returns>
        public override string ToString()
        {
            return _atom;
        }
    }
}
