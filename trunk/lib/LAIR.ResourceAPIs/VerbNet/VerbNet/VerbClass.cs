using System;
using System.Collections.Generic;
using System.Text;
using System.Linq;

using LAIR.Collections.Generic;
using LAIR.Extensions;

namespace LAIR.ResourceAPIs.VerbNet
{
    /// <summary>
    /// Represents a class of verbs in VerbNet
    /// </summary>
    public class VerbClass
    {
        #region static members
        private static bool _displayVerbsInToString = false;

        /// <summary>
        /// Gets or sets whether or not to display verbs in the return value for ToString (default:  false)
        /// </summary>
        public static bool DisplayVerbsInToString
        {
            get { return VerbClass._displayVerbsInToString; }
            set { VerbClass._displayVerbsInToString = value; }
        }
        #endregion

        private readonly string _id;
        private readonly int _hashCode;
        private Set<string> _verbs;
        private VerbClass _parent;
        private Set<VerbClass> _children;
        private Set<VerbNetEngine.ThematicRole> _thematicRoles;
        private Set<string> _examples;

        /// <summary>
        /// Gets the ID of this class
        /// </summary>
        public string ID
        {
            get { return _id; }
        }

        /// <summary>
        /// Gets the parent of this class
        /// </summary>
        public VerbClass Parent
        {
            get { return _parent; }
        }

        /// <summary>
        /// Gets whether or not this class is a root class
        /// </summary>
        public bool IsRoot
        {
            get { return _parent == null; }
        }

        /// <summary>
        /// Gets one-based depth of current class (root has depth of 1)
        /// </summary>
        public int Depth
        {
            get { return _id.Count(c => c == '.') + 1; }
        }

        /// <summary>
        /// Gets the shallowest ancestor of this class that has one or more verbs
        /// </summary>
        public VerbClass ShallowestAncestorWithVerbs
        {
            get
            {
                // the root never has verbs
                if (IsRoot)
                    return null;

                // get the parent's shallowest ancestor with verbs...return it if non-null
                VerbClass parentsShallowest = _parent.ShallowestAncestorWithVerbs;
                if (parentsShallowest != null)
                    return parentsShallowest;
                else
                    // otherwise, if the current class has verbs, it must be the shallowest, so return it
                    if (_verbs.Count > 0)
                        return this;
                    // no classes at or above this one have verbs
                    else
                        return null;
            }
        }

        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="id">ID of class in dotted notation</param>
        public VerbClass(string id)
        {
            _id = id;
            _hashCode = _id.GetHashCode();
            _verbs = new Set<string>();
            _children = new Set<VerbClass>();
            _thematicRoles = new Set<VerbNetEngine.ThematicRole>();
            _examples = new Set<string>(false);
        }

        /// <summary>
        /// Adds a verb to this class
        /// </summary>
        /// <param name="verb">Verb to add</param>
        public void AddVerb(string verb)
        {
            _verbs.Add(verb);
        }

        /// <summary>
        /// Adds a child class to this class
        /// </summary>
        /// <param name="child">Child class to add</param>
        public void AddChild(VerbClass child)
        {
            if (child.Parent != null)
                throw new Exception("Child class already has a parent");

            child._parent = this;

            _children.Add(child);
        }

        /// <summary>
        /// Gets child classes of this class
        /// </summary>
        /// <param name="recursive">Whether or not to retrieve classes recursively</param>
        /// <returns>Verb classes</returns>
        public Set<VerbClass> GetChildren(bool recursive)
        {
            Set<VerbClass> children = new Set<VerbClass>();
            if (recursive)
                GetChildrenRecursive(ref children);
            else
                children = _children.Copy();

            return children;
        }

        /// <summary>
        /// Gets child classes recursively without creating many new Set objects
        /// </summary>
        /// <param name="children">Current list of child classes</param>
        private void GetChildrenRecursive(ref Set<VerbClass> children)
        {
            // add each child class
            foreach (VerbClass child in _children)
            {
                children.Add(child);

                // add child's children
                child.GetChildrenRecursive(ref children);
            }
        }

        /// <summary>
        /// Gets verbs in this class
        /// </summary>
        /// <param name="recursive">Whether or not to recursively get verbs from all children</param>
        /// <returns>Set of verbs</returns>
        public Set<string> GetVerbs(bool recursive)
        {
            // start with verbs from the current class
            Set<string> verbs = _verbs.Copy();

            // add child class verbs recursively
            if (recursive)
            {
                verbs.ThrowExceptionOnDuplicateAdd = false;

                foreach (VerbClass child in GetChildren(true))
                    verbs.AddRange(child._verbs);
            }

            return verbs;
        }

        /// <summary>
        /// Adds a thematic role to this class
        /// </summary>
        /// <param name="role">Role to add</param>
        public void AddThematicRole(VerbNetEngine.ThematicRole role)
        {
            _thematicRoles.Add(role);
        }

        /// <summary>
        /// Checks whether this class has a thematic role
        /// </summary>
        /// <param name="role">Role to check for</param>
        /// <param name="includeInherited">Whether or not to include inherited roles</param>
        /// <returns>True if role is present and false otherwise</returns>
        public bool ContainsThematicRole(VerbNetEngine.ThematicRole role, bool includeInherited)
        {
            VerbClass currClass = this;
            while (currClass != null)
                // check current class
                if (currClass._thematicRoles.Contains(role))
                    return true;
                // check parent class if allowed
                else if (includeInherited)
                    currClass = currClass.Parent;
                else
                    return false;

            return false;
        }

        /// <summary>
        /// Gets thematic roles in this class
        /// </summary>
        /// <param name="includeInherited">Whether or not to include inherited roles</param>
        /// <returns>Thematic roles</returns>
        public Set<VerbNetEngine.ThematicRole> GetThematicRoles(bool includeInherited)
        {
            // start with roles in this class
            Set<VerbNetEngine.ThematicRole> roles = _thematicRoles.Copy();

            // optionally add inherited
            if (includeInherited)
            {
                roles.ThrowExceptionOnDuplicateAdd = false;

                VerbClass current = this;
                while ((current = current.Parent) != null)
                    roles.AddRange(current.GetThematicRoles(false));
            }

            return roles;
        }

        /// <summary>
        /// Adds an example to this class
        /// </summary>
        /// <param name="example">Example to add</param>
        public void AddExample(string example)
        {
            _examples.Add(example);
        }

        /// <summary>
        /// Gets examples from this class
        /// </summary>
        /// <param name="includeInherited">Whether or not to include inherited examples</param>
        /// <returns>Examples</returns>
        public Set<string> GetExamples(bool includeInherited)
        {
            // start with roles in this class
            Set<string> examples = _examples.Copy();

            // optionally add inherited
            if (includeInherited)
            {
                examples.ThrowExceptionOnDuplicateAdd = false;

                VerbClass current = this;
                while ((current = current.Parent) != null)
                    examples.AddRange(current.GetExamples(false));
            }

            return examples;
        }

        /// <summary>
        /// Gets the lowest common ancestor between this class and another
        /// </summary>
        /// <param name="verbClass">Other class</param>
        /// <returns>Lowest common ancestor</returns>
        public VerbClass GetLowestCommonAncestor(VerbClass verbClass)
        {
            // append period to both IDs to remove any ambiguity about classes...then get common substring
            string lcaID = GetCommonInitialSubstring((_id + "."), (verbClass.ID + "."));
            ;
            int lastPeriod = lcaID.LastIndexOf('.');
            lcaID = lcaID.Substring(0, lastPeriod);

            // find lca
            VerbClass lca = this;
            while (lca.ID != lcaID)
                lca = lca.Parent;

            return lca;
        }

        private string GetCommonInitialSubstring(string p, string p_2)
        {
            throw new NotImplementedException();
        }

        /// <summary>
        /// Gets the VerbClass path from this class to another class
        /// </summary>
        /// <param name="verbClass">Destination class</param>
        /// <returns>Sequence of verb classes representing the path</returns>
        public List<VerbClass> GetPathTo(VerbClass verbClass)
        {
            // get the lca
            VerbClass lca = GetLowestCommonAncestor(verbClass);

            // add classes from the current to the lca
            List<VerbClass> path = new List<VerbClass>();
            VerbClass current = this;
            while (current != lca)
            {
                path.Add(current);
                current = current.Parent;
            }

            // add lca
            path.Add(lca);

            // insert classes from the destination to the lca (make sure ordering it correct)
            int currInsert = path.Count;
            current = verbClass;
            while (current != lca)
            {
                path.Insert(currInsert, current);
                current = current.Parent;
            }

            return path;
        }

        /// <summary>
        /// Gets a simple measure of similarity between the current class and another class. The formula:  2 * depth(lca) / (depth(current) + depth(other)).
        /// This is similar to the Wu and Palmer (1994) metric for WordNet synset similarity.
        /// </summary>
        /// <param name="verbClass"></param>
        /// <returns></returns>
        public float GetHierarchicalSimilarityWith(VerbClass verbClass)
        {
            return (2 * GetLowestCommonAncestor(verbClass).Depth) / (float)(Depth + verbClass.Depth);
        }

        /// <summary>
        /// Gets whether or not this class contains a verb
        /// </summary>
        /// <param name="verb">Verb to check for</param>
        /// <returns>True if verb is present and false otherwise</returns>
        public bool Contains(string verb)
        {
            return _verbs.Contains(verb);
        }

        /// <summary>
        /// Checks whether this verb class equals another
        /// </summary>
        /// <param name="obj">Other verb class to compare with</param>
        /// <returns>True if classes are equal and false otherwise</returns>
        public override bool Equals(object obj)
        {
            if (!(obj is VerbClass))
                return false;

            return _id == (obj as VerbClass).ID;
        }

        /// <summary>
        /// Gets the hash code for this class
        /// </summary>
        /// <returns></returns>
        public override int GetHashCode()
        {
            return _hashCode;
        }

        /// <summary>
        /// Gets ID of class
        /// </summary>
        /// <returns>ID of class</returns>
        public override string ToString()
        {
            if (!_displayVerbsInToString)
                return _id;

            StringBuilder s = new StringBuilder(_id + ":  ");
            bool prependComma = false;
            foreach (string verb in _verbs)
            {
                s.Append((prependComma ? ", " : "") + verb);
                prependComma = true;
            }

            return s.ToString();            
        }        
    }
}
