using System;
using System.Collections.Generic;
using System.Text;
using System.IO;

using LAIR.XML;
using LAIR.Collections.Generic;
using LAIR.Extensions;

namespace LAIR.ResourceAPIs.VerbNet
{
    /// <summary>
    /// Provides access to VerbNet
    /// </summary>
    public class VerbNetEngine
    {
        #region static members
        /// <summary>
        /// Thematic roles defined in VerbNet
        /// </summary>
        public enum ThematicRole
        {
            Actor,
            Actor1,
            Actor2,
            Agent,
            Asset,
            Attribute,
            Beneficiary,
            Cause,
            Destination,
            Experiencer,
            Extent,
            Instrument,
            Location,
            Material,
            Oblique,
            Patient,
            Patient1,
            Patient2,
            Predicate,
            Product,
            Proposition,
            Recipient,
            Source,
            Stimulus,
            Theme,
            Theme1,
            Theme2,
            Time,
            Topic,
            Value
        }
        #endregion

        private VerbClass _rootVerbClass;
        private Dictionary<string, Set<VerbClass>> _verbVerbClasses;
        private Dictionary<string, VerbClass> _idVerbClass;

        /// <summary>
        /// Gets the root verb class of the VerbNet hierarchy
        /// </summary>
        public VerbClass RootVerbClass
        {
            get { return _rootVerbClass; }
        }

        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="verbNetDirectory">Path to the VerbNet directory</param>
        public VerbNetEngine(string verbNetDirectory)
        {
            // check directory
            if (!Directory.Exists(verbNetDirectory))
                throw new Exception("Invalid VerbNet directory");

            // extract all verb classes
            _idVerbClass = new Dictionary<string, VerbClass>();
            foreach (string classPath in Directory.GetFiles(verbNetDirectory, "*.xml"))
                ExtractClass(File.ReadAllText(classPath), false);

            // add root class
            _rootVerbClass = new VerbClass("0");
            _idVerbClass.Add(_rootVerbClass.ID, _rootVerbClass);

            // assemble hierarchy
            while (true)
            {
                // create empty "connector" classes for verbnet classes whose direct parent is not defined in verbnet
                Dictionary<string, VerbClass> newIdVerbClass = new Dictionary<string, VerbClass>();

                // check all non-root classes that don't have a parent
                foreach (string id in _idVerbClass.Keys)
                    if (id != _rootVerbClass.ID && _idVerbClass[id].Parent == null)
                    {
                        VerbClass currentClass = _idVerbClass[id];

                        // get id of parent by removing the final location
                        string parentID = id.Substring(0, id.LastIndexOf('.'));

                        // try to get parent class
                        VerbClass parentClass;
                        if (!_idVerbClass.TryGetValue(parentID, out parentClass))
                        {
                            // create new connector class, reusing existing connector class if we have one
                            VerbClass connectorClass;
                            if (!newIdVerbClass.TryGetValue(parentID, out connectorClass))
                            {
                                connectorClass = new VerbClass(parentID);
                                newIdVerbClass.Add(connectorClass.ID, connectorClass);
                            }

                            parentClass = connectorClass;
                        }

                        // add current class as sub-class of parent
                        parentClass.AddChild(currentClass);
                    }

                // add all newly created connector classes
                foreach (string id in newIdVerbClass.Keys)
                    _idVerbClass.Add(id, newIdVerbClass[id]);

                // if we didn't add any connector classes, each class (except the root) has a parent defined in _idVerbClass - hierarchy is complete
                if (newIdVerbClass.Count == 0)
                    break;
            }

            // map each verb to its classes
            _verbVerbClasses = new Dictionary<string, Set<VerbClass>>();
            foreach (VerbClass verbClass in _rootVerbClass.GetChildren(true))
                foreach (string verb in verbClass.GetVerbs(false))
                {
                    _verbVerbClasses.EnsureContainsKey(verb, typeof(Set<VerbClass>));
                    _verbVerbClasses[verb].Add(verbClass);
                }

            // make sure all verb classes except for the root has a parent (i.e., make sure we have a rooted tree)
            foreach (VerbClass vnClass in _idVerbClass.Values)
                if (vnClass.ID != "0" && vnClass.Parent == null)
                    throw new Exception("Invalid VerbNet tree structure!");
        }

        /// <summary>
        /// Extracts a VerbNet class from its XML definition
        /// </summary>
        /// <param name="classXML">XML for class</param>
        /// <param name="isSubClassXML">Whether or not the XML describes a sub-class</param>
        /// <returns>VerbClass</returns>
        private VerbClass ExtractClass(string classXML, bool isSubClassXML)
        {
            // extract class from given XML
            VerbClass vnClass = null;
            XmlParser classP = new XmlParser(classXML);
            string classTag = isSubClassXML ? "VNSUBCLASS" : "VNCLASS";
            string vnClassXML;
            while ((vnClassXML = classP.OuterXML(classTag)) != null)
            {
                XmlParser vnClassP = new XmlParser(vnClassXML);

                // get id, using only the dotted number portion and using 0 as the root of all classes
                string id = vnClassP.AttributeValue(classTag, "ID").Trim().Replace('-', '.');
                id = "0." + id.Substring(id.IndexOf('.') + 1);

                // create class
                vnClass = new VerbClass(id);

                // extract verbs
                XmlParser membersP = new XmlParser(vnClassP.OuterXML("MEMBERS"));
                string verb;
                while ((verb = membersP.AttributeValue("MEMBER", "name")) != null)
                {
                    vnClass.AddVerb(verb);
                    membersP.MoveToElementNode(false);
                }

                // extract thematic roles
                XmlParser rolesP = new XmlParser(vnClassP.OuterXML("THEMROLES"));
                string role;
                while ((role = rolesP.AttributeValue("THEMROLE", "type")) != null)
                {
                    vnClass.AddThematicRole((ThematicRole)Enum.Parse(typeof(ThematicRole), role));
                    rolesP.MoveToElementNode(false);
                }

                // extract examples
                XmlParser examplesP = new XmlParser(vnClassP.OuterXML("FRAMES"));
                string example;
                while ((example = examplesP.ElementText("EXAMPLE")) != null)
                    vnClass.AddExample(example);

                // extract subclasses
                XmlParser subClassesP = new XmlParser(vnClassP.OuterXML("SUBCLASSES"));
                string subClassXML;
                while ((subClassXML = subClassesP.OuterXML("VNSUBCLASS")) != null)
                    vnClass.AddChild(ExtractClass(subClassXML, true));

                // map id to verb class
                _idVerbClass.Add(vnClass.ID, vnClass);
            }

            if (vnClass == null)
                throw new Exception("Invalid class XML");

            return vnClass;
        }

        /// <summary>
        /// Gets whether or not VerbNet contains a verb
        /// </summary>
        /// <param name="verb">Verb to check</param>
        /// <returns>True if verb is present and false otherwise</returns>
        public bool ContainsVerb(string verb)
        {
            // verbnet uses the underscore for phrasal verbs
            verb = verb.Replace(' ', '_');

            return _verbVerbClasses.ContainsKey(verb);
        }

        /// <summary>
        /// Checks whether or not a VerbNet class exists
        /// </summary>
        /// <param name="classID">ID of class to check for in dotted notation</param>
        /// <returns>True if class is present and false otherwise</returns>
        public bool ContainsClass(string classID)
        {
            return _idVerbClass.ContainsKey(classID);
        }

        /// <summary>
        /// Gets classes for a verb
        /// </summary>
        /// <param name="verb">Verb to get classes for</param>
        /// <returns>Classes for verb</returns>
        public Set<VerbClass> GetClassesFor(string verb)
        {
            // verbnet uses the underscore for phrasal verbs
            verb = verb.Replace(' ', '_');

            Set<VerbClass> classes;
            if (!_verbVerbClasses.TryGetValue(verb, out classes))
                classes = new Set<VerbClass>();

            return classes;
        }

        /// <summary>
        /// Gets a class from the VerbNet hierarchy
        /// </summary>
        /// <param name="id">ID of class in 0.1.2.3 format</param>
        /// <returns>Verb class</returns>
        public VerbClass GetClass(string id)
        {
            return _idVerbClass[id];
        }

        /// <summary>
        /// Gets a class from the VerbNet hierarchy
        /// </summary>
        /// <param name="id">ID of class in 0.1.2.3 format</param>
        /// <param name="verbClass">Verb class if one was found</param>
        /// <returns>Verb class</returns>
        public bool TryGetClass(string id, out VerbClass verbClass)
        {
            return _idVerbClass.TryGetValue(id, out verbClass);
        }

        /// <summary>
        /// Gets classes for a set of verbs (union)
        /// </summary>
        /// <param name="verbs">Verbs to get classes for</param>
        /// <returns>Classes for verbs</returns>
        public Set<VerbClass> GetClassesFor(IEnumerable<string> verbs)
        {
            Set<VerbClass> classes = new Set<VerbClass>(false);
            foreach (string verb in verbs)
                classes.AddRange(GetClassesFor(verb));

            return classes;
        }
    }
}
