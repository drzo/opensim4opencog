using System;
using System.Collections.Generic;
using System.Text;

namespace LAIR.ResourceAPIs.PennBank.PropBank
{
    /// <summary>
    /// Represents a PropBank role
    /// </summary>
    public class Role
    {
        private string _description;
        private int _number;

        /// <summary>
        /// Gets or sets the description
        /// </summary>
        public string Description
        {
            get { return _description; }
            set { _description = value; }
        }

        /// <summary>
        /// Gets or sets the argument number
        /// </summary>
        public int Number
        {
            get { return _number; }
            set { _number = value; }
        }

        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="description">Role description</param>
        /// <param name="number">Role number</param>
        public Role(string description, int number)
        {
            _description = description;
            _number = number;
        }

        /// <summary>
        /// Provides nicely formatted string for this role
        /// </summary>
        /// <returns>String</returns>
        public override string ToString()
        {
            return "Argument " + _number + ":  " + _description;
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
            return _number.GetHashCode();
        }
    }
}
