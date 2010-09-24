using System;
using System.Collections.Generic;
using System.Text;

namespace LAIR.ResourceAPIs.NomBank
{
    /// <summary>
    /// Represents a NomBank frame
    /// </summary>
    public class Frame
    {
        private string _noun;
        private Dictionary<int, RoleSet> _roleSets;

        /// <summary>
        /// Gets the noun. This is the lemma form, which is used in the noun.id role set notation
        /// </summary>
        public string Noun
        {
            get { return _noun; }
        }

        /// <summary>
        /// Gets the number of role sets in this frame
        /// </summary>
        public int RoleSetCount
        {
            get { return _roleSets.Count; }
        }

        /// <summary>
        /// Gets an enumerator over role sets in this frame
        /// </summary>
        public IEnumerable<RoleSet> RoleSets
        {
            get { return _roleSets.Values; }
        }

        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="noun">Noun for this frame</param>
        public Frame(string noun)
        {
            _noun = noun;

            _roleSets = new Dictionary<int, RoleSet>();
        }

        /// <summary>
        /// Gets nicely formatted string representation of this frame
        /// </summary>
        /// <returns>Frame description</returns>
        public override string ToString()
        {
            StringBuilder s = new StringBuilder(_noun + " role sets:" + Environment.NewLine + Environment.NewLine);

            foreach (RoleSet r in _roleSets.Values)
                s.Append(r.ToString() + Environment.NewLine);

            return s.ToString();
        }

        /// <summary>
        /// Adds a role set to this frame
        /// </summary>
        /// <param name="roleSet">Role set to add</param>
        public void AddRoleSet(RoleSet roleSet)
        {
            _roleSets.Add(roleSet.ID, roleSet);
        }

        /// <summary>
        /// Gets a role set by ID
        /// </summary>
        /// <param name="roleSetID">ID of role set to get</param>
        /// <returns>RoleSet</returns>
        public RoleSet GetRoleSet(int roleSetID)
        {
            return _roleSets[roleSetID];
        }

        /// <summary>
        /// Gets whether or not this frame contains a role set
        /// </summary>
        /// <param name="roleSetID">ID of role set to check for</param>
        /// <returns>True if role set is contained, false otherwise</returns>
        public bool Contains(int roleSetID)
        {
            return _roleSets.ContainsKey(roleSetID);
        }
    }
}
