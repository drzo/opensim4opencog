using System;
using System.Collections.Generic;
using System.Text;

namespace LAIR.ResourceAPIs.NomBank.NomLex
{
    /// <summary>
    /// Represents a list of NomLex feature values
    /// </summary>
    public class FeatureValueList : FeatureValue
    {
        private List<FeatureValue> _values;

        /// <summary>
        /// Constructor
        /// </summary>
        public FeatureValueList()
        {
            _values = new List<FeatureValue>();
        }

        /// <summary>
        /// Adds a value to this list
        /// </summary>
        /// <param name="value"></param>
        public void Add(FeatureValue value)
        {
            _values.Add(value);
        }

        /// <summary>
        /// ToString override
        /// </summary>
        /// <returns></returns>
        public override string ToString()
        {
            string text = "";
            bool first = true;
            foreach (FeatureValue val in _values)
            {
                text += (!first ? ", " : "") + val;
                first = false;
            }

            return text;
        }
    }
}
