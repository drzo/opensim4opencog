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
        #region static members
        private static bool _includeArgumentsInToString = true;

        /// <summary>
        /// Gets or sets whether or not to include the argument list when ToString is called (default:  true)
        /// </summary>
        public static bool IncludeArgumentsInToString
        {
            get { return RoleSet._includeArgumentsInToString; }
            set { RoleSet._includeArgumentsInToString = value; }
        }
        #endregion

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
            StringBuilder s = new StringBuilder("Name:  " + _name + ", ID:  " + _ID);

            if (!_includeArgumentsInToString)
                return s.ToString();

            // get sorted list of role numbers
            List<int> numbers = new List<int>(_numberRole.Keys);
            numbers.Sort();
            foreach (int number in numbers)
                s.Append(Environment.NewLine + "  " + Get(number));

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
        /// Gets whether or not this role set contains a role
        /// </summary>
        /// <param name="number">Argument number of role</param>
        /// <returns>True if role is present and false otherwise</returns>
        public bool Contains(int number)
        {
            return _numberRole.ContainsKey(number);
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
