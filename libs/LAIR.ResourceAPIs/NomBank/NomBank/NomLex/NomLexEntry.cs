using System;
using System.Collections.Generic;
using System.Text;

namespace LAIR.ResourceAPIs.NomBank.NomLex
{
    /// <summary>
    /// Stores NomLex information
    /// </summary>
    public class NomLexEntry : FeatureValue
    {
        private string _name;
        private Dictionary<string, FeatureValue> _features;

        /// <summary>
        /// Gets or sets the name
        /// </summary>
        public string Name
        {
            get { return _name; }
            set { _name = value; }
        }

        /// <summary>
        /// Gets or sets the features
        /// </summary>
        public Dictionary<string, FeatureValue> Features
        {
            get { return _features; }
            set { _features = value; }
        }

        /// <summary>
        /// Constructor
        /// </summary>
        public NomLexEntry()
        {
            _features = new Dictionary<string, FeatureValue>();
        }

        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="name">Name of entry</param>
        public NomLexEntry(string name)
            : this()
        {
            _name = name;
        }

        /// <summary>
        /// ToString override
        /// </summary>
        /// <returns></returns>
        public override string ToString()
        {
            string text = _name;

            bool first = true;
            foreach (string featureName in _features.Keys)
            {
                text += (first ? ":  " : ", ") + featureName + "=" + _features[featureName];
                first = false;
            }

            return text;
        }
    }
}
