using System;
using System.Collections.Generic;
using System.Text;
using System.Collections;

using LAIR.Collections.Generic;

namespace LAIR.ResourceAPIs.PennBank.PropBank
{
    /// <summary>
    /// Represents a PropBank role set
    /// </summary>
    public class RoleSet : IEnumerable<Role>
    {
        private Dictionary<int, Role> _numberRole;
        private int _ID;
        private string _name;

        /// <summary>
        /// Gets the ID
        /// </summary>
        public int ID
        {
            get { return _ID; }
        }

        /// <summary>
        /// Gets the name
        /// </summary>
        public string Name
        {
            get { return _name; }
        }

        /// <summary>
        /// Gets the number of roles in this set
        /// </summary>
        public int Count
        {
            get { return _numberRole.Count; }
        }

        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="id">ID of role set</param>
        /// <param name="name">Name of role set</param>
        public RoleSet(int id, string name)
        {
            _ID = id;
            _name = name;

            _numberRole = new Dictionary<int, Role>();
        }

        /// <summary>
        /// Provides nicely formatted text to describe this role set
        /// </summary>
        /// <returns>String</returns>
        public override string ToString()
        {
            StringBuilder s = new StringBuilder("Name:  " + _name + ", ID:  " + _ID + Environment.NewLine);

            // get sorted list of role numbers
            List<int> numbers = new List<int>(_numberRole.Keys);
            numbers.Sort();

            // append each role
            foreach (int number in numbers)
            {
                Role r = Get(number);
                s.Append("  " + r.ToString() + Environment.NewLine);
            }

            return s.ToString();
        }

        /// <summary>
        /// Adds a role to this set
        /// </summary>
        /// <param name="role">Role to add to this set</param>
        public void Add(Role role)
        {
            _numberRole.Add(role.Number, role);
        }

        /// <summary>
        /// Gets a role by its number
        /// </summary>
        /// <param name="number">Number of role to get</param>
        public Role Get(int number)
        {
            return _numberRole[number];
        }

        /// <summary>
        /// Tries to get a role by its number
        /// </summary>
        /// <param name="number">Number of role to get</param>
        /// <param name="role">Corresponding role, or null if not present</param>
        /// <returns>True if role was found, false otherwise</returns>
        public bool TryGet(int number, out Role role)
        {
            return _numberRole.TryGetValue(number, out role);
        }

        /// <summary>
        /// Gets an enumerator over roles in this set
        /// </summary>
        /// <returns></returns>
        public IEnumerator<Role> GetEnumerator()
        {
            return _numberRole.Values.GetEnumerator();
        }

        /// <summary>
        /// Gets an enumerator over roles in this set
        /// </summary>
        /// <returns></returns>
        IEnumerator IEnumerable.GetEnumerator()
        {
            return GetEnumerator();
        }
    }
}
