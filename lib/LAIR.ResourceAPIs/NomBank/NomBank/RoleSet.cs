using System;
using System.Collections.Generic;
using System.Text;
using System.Collections;
using LAIR.Collections.Generic;
using System.Text.RegularExpressions;

namespace LAIR.ResourceAPIs.NomBank
{
    /// <summary>
    /// Represents a NomBank role set
    /// </summary>
    public class RoleSet : IEnumerable<Role>
    {
        private Dictionary<int, Role> _numberRole;
        private int _id;
        private string _name;
        private string _sourceVerb;
        private int _sourceRoleSet;

        /// <summary>
        /// Gets the source verb for this role set. NOTE:  NomBank also contains nominal predicates derived from other parts
        /// of speech, in which case the source of the predicate is something other than a verb. Currently, you cannot retrieve
        /// sources that are non-verb (i.e., this will return null iff the source exists and is a verb).
        /// </summary>
        public string SourceVerb
        {
            get { return _sourceVerb; }
        }

        /// <summary>
        /// Gets the source role set for this role set
        /// </summary>
        public int SourceRoleSet
        {
            get { return _sourceRoleSet; }
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
        /// <param name="sourceVerb">Source verb for this role set</param>
        /// <param name="sourceRoleSet">Source role set for this role set</param>
        public RoleSet(int id, string name, string sourceVerb, int sourceRoleSet)
        {
            _id = id;
            _name = name;
            _sourceVerb = sourceVerb;
            _sourceRoleSet = sourceRoleSet;

            _numberRole = new Dictionary<int, Role>();

            if (_sourceVerb == null && _sourceRoleSet != -1)
                throw new Exception("Source role set should be -1 if there is no source verb");

            if (_sourceVerb != null && _sourceRoleSet <= 0)
                throw new Exception("Source role set should be >= 1 if there is a source verb");
        }

        /// <summary>
        /// Provides nicely formatted text to describe this role set
        /// </summary>
        /// <returns>String</returns>
        public override string ToString()
        {
            StringBuilder s = new StringBuilder("ID:  " + _id + ", Name:  " + _name + (_sourceVerb != null ? ", Source:  " + _sourceVerb + "." + _sourceRoleSet : ""));

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
        /// Gets a role by its number
        /// </summary>
        /// <param name="number">Number of role to get</param>
        /// <returns>Corresponding role</returns>
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
