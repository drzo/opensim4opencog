using System;
using System.Collections.Generic;
using System.Text;

namespace LAIR.ResourceAPIs.NomBank
{
    /// <summary>
    /// Represents a role within NomBank
    /// </summary>
    public class Role
    {
        private string _description;
        private int _source;
        private readonly int _number;
        private readonly int _hashCode;

        /// <summary>
        /// Gets or sets the source argument position within the source role set
        /// </summary>
        public int Source
        {
            get { return _source; }
        }

        /// <summary>
        /// Gets or sets the description
        /// </summary>
        public string Description
        {
            get { return _description; }
        }

        /// <summary>
        /// Gets or sets the argument number
        /// </summary>
        public int Number
        {
            get { return _number; }
        }

        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="description">Role description</param>
        /// <param name="number">Role number</param>
        /// <param name="source">Source argument in PropBank</param>
        public Role(string description, int number, int source)
        {
            _description = description;
            _number = number;
            _source = source;
            _hashCode = _number.GetHashCode();
        }

        /// <summary>
        /// Provides nicely formatted string for this role
        /// </summary>
        /// <returns>String</returns>
        public override string ToString()
        {
            // start with argument number
            string argDesc = "Argument " + _number;

            // add verb source if present
            if (_source >= 0)
                argDesc += " (source:  " + _source + ")";

            // add description
            argDesc += ":  " + _description;

            return argDesc;
        }

        /// <summary>
        /// Checks whether or not to roles are equal
        /// </summary>
        /// <param name="obj">Role to compare this one to</param>
        /// <returns>True if roles are equal, false otherwise</returns>
        public override bool Equals(object obj)
        {
            if (!(obj is Role))
                return false;

            Role r = obj as Role;

            return _number == r.Number;
        }

        /// <summary>
        /// Gets hash code for this role
        /// </summary>
        /// <returns>Hash code</returns>
        public override int GetHashCode()
        {
            return _hashCode;
        }
    }
}
