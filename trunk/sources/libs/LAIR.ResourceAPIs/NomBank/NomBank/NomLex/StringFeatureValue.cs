using System;
using System.Collections.Generic;
using System.Text;

namespace LAIR.ResourceAPIs.NomBank.NomLex
{
    /// <summary>
    /// Represents a NomLex string feature value
    /// </summary>
    public class StringFeatureValue : FeatureValue
    {
        private string _string;

        /// <summary>
        /// Gets or sets the string
        /// </summary>
        public string String
        {
            get { return _string; }
            set { _string = value; }
        }

        /// <summary>
        /// Constructor
        /// </summary>
        public StringFeatureValue()
        {
        }

        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="stringValue">String value</param>
        public StringFeatureValue(string stringValue)
            : this()
        {
            _string = stringValue;
        }

        /// <summary>
        /// ToString override
        /// </summary>
        /// <returns></returns>
        public override string ToString()
        {
            return _string;
        }
    }
}
