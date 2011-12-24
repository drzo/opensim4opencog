using System;
using System.Collections.Generic;
using System.Text;
using LAIR.MachineLearning;
using LAIR.MachineLearning.Clustering;

namespace LAIR.ResourceAPIs.NomBank
{
    /// <summary>
    /// Represents a nominalization that can be classified
    /// </summary>
    public class Nominalization : ClassifiableEntity
    {
        private string _value;

        /// <summary>
        /// Gets or sets the value
        /// </summary>
        public string Value
        {
            get { return _value; }
            set { _value = value; }
        }

        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="value">Value of nominalization</param>
        public Nominalization(string value)
        {
            _value = value;
        }

        /// <summary>
        /// Returns the value of this nominalization
        /// </summary>
        /// <returns></returns>
        public override string ToString()
        {
            return _value;
        }
    }
}
