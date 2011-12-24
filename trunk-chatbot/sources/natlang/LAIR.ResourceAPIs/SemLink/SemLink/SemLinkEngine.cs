using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.IO;

using LAIR.XML;
using LAIR.Collections.Generic;
using LAIR.Extensions;

namespace LAIR.ResourceAPIs.SemLink
{
    /// <summary>
    /// Provides mapping information contained in the SemLink annotation project.
    /// </summary>
    public class SemLinkEngine
    {
        /// <summary>
        /// Ways of extending the SemLink mapping when VerbNet classes are involved. This is motivated by the following
        /// example:  the PropBank predicate spend.2 maps to the VerbNet class 66.1; however, SemLink maps VerbNet classes
        /// 66 and 66.1 to a FrameNet frame. Thus, if we're interested in mapping spend.2 to FrameNet via VerbNet class
        /// 66, we need to do some extra work. If we apply the SuperClass extension to the VerbNet-FrameNet mapping, the 
        /// result is that we will search super-classes of 66.1 for a FrameNet mapping, giving us the desired result that 
        /// PropBank predicate spend.2 maps to FrameNet frame Exhaust_resource (via 66) and Frugality (via 66.1).
        /// </summary>
        public enum VerbNetExtension
        {
            /// <summary>
            /// No extension
            /// </summary>
            None,

            /// <summary>
            /// Use super-classes of encountered VerbNet classes
            /// </summary>
            SuperClass,

            /// <summary>
            /// Use sub-classes of encountered VerbNet classes
            /// </summary>
            SubClass,

            /// <summary>
            /// Combination of sub- and super-class extensions
            /// </summary>
            SuperAndSubClass
        }

        private Dictionary<string, Set<string>> _propBankRoleVerbNetRoles;  // maps propbank roles to verbnet roles
        private Dictionary<string, Set<string>> _verbNetRolePropBankRoles;  // maps verbnet roles to propbank roles
        private Dictionary<string, Set<string>> _verbNetRoleFrameElements;  // maps verbnet roles to frame elements
        private Dictionary<string, Set<string>> _frameElementVerbNetRoles;  // maps frame elements to verbnet roles
        private string _dataDirectory;

        /// <summary>
        /// Gets all PropBank roles, in Verb.RoleSet.Role notation
        /// </summary>
        public IEnumerable<string> PropBankRoles
        {
            get { return _propBankRoleVerbNetRoles.Keys; }
        }

        /// <summary>
        /// Gets all frame elements, in Frame.FrameElement notation
        /// </summary>
        public IEnumerable<string> FrameElements
        {
            get { return _frameElementVerbNetRoles.Keys; }
        }

        /// <summary>
        /// Gets PropBank verbs defined in SemLink
        /// </summary>
        public IEnumerable<string> PropBankVerbs
        {
            get
            {
                Set<string> verbs = new Set<string>(false);
                foreach (string propBankRole in _propBankRoleVerbNetRoles.Keys)
                    verbs.Add(propBankRole.Substring(0, propBankRole.IndexOf(".")));

                return verbs;
            }
        }

        /// <summary>
        /// Gets FrameNet frames defined in SemLink
        /// </summary>
        public IEnumerable<string> FrameNetFrames
        {
            get
            {
                Set<string> frames = new Set<string>(false);
                foreach (string frameElement in _frameElementVerbNetRoles.Keys)
                    frames.Add(frameElement.Substring(0, frameElement.IndexOf(".")));

                return frames;
            }
        }

        /// <summary>
        /// Gets the path to the PropBank-VerbNet linking file
        /// </summary>
        public string PropBankVerbNetLinkingPath
        {
            get { return Path.Combine(Path.Combine(_dataDirectory, "vn-pb"), "type_map.xml"); }
        }

        /// <summary>
        /// Gets the path to the FrameNet-VerbNet linking file
        /// </summary>
        public string FrameNetVerbNetLinkingPath
        {
            get { return Path.Combine(Path.Combine(_dataDirectory, "vn-fn"), "VN-FN_roleMapping.xml"); }
        }

        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="dataDirectory">Path to the SemLink data directory</param>
        public SemLinkEngine(string dataDirectory)
        {
            _dataDirectory = dataDirectory;

            #region propbank-verbnet
            // check for file
            if (!File.Exists(PropBankVerbNetLinkingPath))
                throw new FileNotFoundException("Failed to find PropBank-VerbNet mapping file:  " + PropBankVerbNetLinkingPath);

            // read each predicate mapping
            _propBankRoleVerbNetRoles = new Dictionary<string, Set<string>>();
            XmlParser propBankVerbNetP = new XmlParser(File.ReadAllText(PropBankVerbNetLinkingPath));
            string predicateXML;
            while ((predicateXML = propBankVerbNetP.OuterXML("predicate")) != null)
            {
                XmlParser predicateP = new XmlParser(predicateXML);

                string pbVerb = predicateP.AttributeValue("predicate", "lemma").Trim();
                if (pbVerb == "")
                    throw new Exception("Blank PropBank verb");

                // get argument mappings
                string argMapXML;
                while ((argMapXML = predicateP.OuterXML("argmap")) != null)
                {
                    XmlParser argMapP = new XmlParser(argMapXML);

                    // get role set for current argument mapping
                    string pbRoleSetStr = argMapP.AttributeValue("argmap", "pb-roleset");
                    int pbRoleSet = int.Parse(pbRoleSetStr.Substring(pbRoleSetStr.IndexOf('.') + 1));
                    if (pbRoleSet <= 0)
                        throw new Exception("Invalid PropBank role set:  " + pbRoleSet);

                    // get verbnet class, using periods instead of dashes
                    string vnClass = argMapP.AttributeValue("argmap", "vn-class").Trim().Replace("-", ".");
                    if (vnClass == "")
                        throw new Exception("Blank VerbNet class");

                    // read argument mapping
                    string roleXML;
                    while ((roleXML = argMapP.OuterXML("role")) != null)
                    {
                        XmlParser roleP = new XmlParser(roleXML);

                        // get fully-specified propbank role
                        string pbArgStr = roleP.AttributeValue("role", "pb-arg");
                        if (pbArgStr == "M" || pbArgStr == "A")
                            continue;

                        int pbArg = int.Parse(pbArgStr);
                        string fullPbRole = pbVerb + "." + pbRoleSet + "." + pbArg;

                        // get fully-specified verbnet role
                        string vnRole = roleP.AttributeValue("role", "vn-theta").Trim();
                        if (vnRole == "")
                            throw new Exception("Blank VerbNet role");

                        string fullVnRole = vnClass + "." + vnRole;

                        // create entry
                        _propBankRoleVerbNetRoles.EnsureContainsKey(fullPbRole, typeof(Set<string>));
                        _propBankRoleVerbNetRoles[fullPbRole].Add(fullVnRole);
                    }
                }
            }

            // map verbnet to propbank
            _verbNetRolePropBankRoles = new Dictionary<string, Set<string>>();
            foreach (string propBankRole in _propBankRoleVerbNetRoles.Keys)
                foreach (string verbNetRole in _propBankRoleVerbNetRoles[propBankRole])
                {
                    _verbNetRolePropBankRoles.EnsureContainsKey(verbNetRole, typeof(Set<string>));
                    _verbNetRolePropBankRoles[verbNetRole].Add(propBankRole);
                }
            #endregion

            #region verbnet-framenet
            // check for file
            if (!File.Exists(FrameNetVerbNetLinkingPath))
                throw new FileNotFoundException("Failed to find FrameNet-VerbNet mapping file:  " + FrameNetVerbNetLinkingPath);

            // read each mapping
            _verbNetRoleFrameElements = new Dictionary<string, Set<string>>();
            XmlParser verbNetFrameNetP = new XmlParser(File.ReadAllText(FrameNetVerbNetLinkingPath));
            string vnClassXML;
            while ((vnClassXML = verbNetFrameNetP.OuterXML("vncls")) != null)
            {
                XmlParser vnClassP = new XmlParser(vnClassXML);

                // get verbnet class and framenet frame
                string vnClass = vnClassP.AttributeValue("vncls", "class").Trim().Replace("-", ".");
                string frame = vnClassP.AttributeValue("vncls", "fnframe").Trim().ToLower();

                // get each role mapping
                string roleXML;
                while ((roleXML = vnClassP.OuterXML("role")) != null)
                {
                    // get fe and vn role
                    XmlParser roleP = new XmlParser(roleXML);
                    string fe = frame + "." + roleP.AttributeValue("role", "fnrole").Trim().ToLower();
                    string vnRole = vnClass + "." + roleP.AttributeValue("role", "vnrole").Trim();

                    // add to list of FEs for vn role
                    _verbNetRoleFrameElements.EnsureContainsKey(vnRole, typeof(Set<string>), false);
                    _verbNetRoleFrameElements[vnRole].Add(fe);
                }
            }

            // map frame elements to verbnet roles
            _frameElementVerbNetRoles = new Dictionary<string, Set<string>>();
            foreach (string verbNetRole in _verbNetRoleFrameElements.Keys)
                foreach (string frameElement in _verbNetRoleFrameElements[verbNetRole])
                {
                    _frameElementVerbNetRoles.EnsureContainsKey(frameElement, typeof(Set<string>));
                    _frameElementVerbNetRoles[frameElement].Add(verbNetRole);
                }
            #endregion
        }

        #region get verbnet roles
        /// <summary>
        /// Gets VerbNet roles for a PropBank role
        /// </summary>
        /// <param name="propBankRole">PropBank role, in Verb.RoleSet.Role notation</param>
        /// <returns>VerbNet roles</returns>
        public Set<string> GetVerbNetRolesForPropBank(string propBankRole)
        {
            Set<string> vnRoles;
            if (!_propBankRoleVerbNetRoles.TryGetValue(propBankRole.Trim(), out vnRoles))
                vnRoles = new Set<string>();

            return vnRoles;
        }

        /// <summary>
        /// Gets VerbNet roles for a PropBank role
        /// </summary>
        /// <param name="verb">PropBank verb</param>
        /// <param name="roleSet">PropBank role set</param>
        /// <param name="role">PropBank role</param>
        /// <returns>VerbNet roles</returns>
        public Set<string> GetVerbNetRolesForPropBank(string verb, int roleSet, int role)
        {
            return GetVerbNetRolesForPropBank(verb.Trim() + "." + roleSet + "." + role);
        }

        /// <summary>
        /// Gets VerbNet roles for a FrameNet frame element
        /// </summary>
        /// <param name="frameElement">Frame element, in Frame.FrameElement notation</param>
        /// <returns>VerbNet roles</returns>
        public Set<string> GetVerbNetRolesForFrameNet(string frameElement)
        {
            Set<string> vnRoles;
            if (!_frameElementVerbNetRoles.TryGetValue(frameElement.Trim(), out vnRoles))
                vnRoles = new Set<string>();

            return vnRoles;
        }

        /// <summary>
        /// Gets VerbNet roles for a FrameNet frame element
        /// </summary>
        /// <param name="frame">FrameNet frame</param>
        /// <param name="frameElement">FrameNet frame element</param>
        /// <returns>VerbNet roles</returns>
        public Set<string> GetVerbNetRolesForFrameNet(string frame, string frameElement)
        {
            return GetVerbNetRolesForFrameNet(frame.Trim() + "." + frameElement.Trim());
        }

        /// <summary>
        /// Gets all VerbNet classes for a PropBank verb
        /// </summary>
        /// <param name="verb">Verb to get classes for</param>
        /// <returns>Verb class IDs</returns>
        public IEnumerable<string> GetVerbClassesForPropBankVerb(string verb)
        {
            Set<string> classes = new Set<string>(false);
            foreach (string propBankRole in _propBankRoleVerbNetRoles.Keys)
                if (propBankRole.StartsWith(verb + "."))
                    foreach (string verbNetRole in _propBankRoleVerbNetRoles[propBankRole])
                        classes.Add(verbNetRole.Substring(0, verbNetRole.LastIndexOf(".")));

            return classes;
        }

        /// <summary>
        /// Gets all VerbNet classes for a FrameNet frame
        /// </summary>
        /// <param name="frame">Frame to get classes for</param>
        /// <returns>Verb class IDs</returns>
        public IEnumerable<string> GetVerbClassesForFrameNetFrame(string frame)
        {
            Set<string> classes = new Set<string>(false);
            foreach (string frameElement in _frameElementVerbNetRoles.Keys)
                if (frameElement.StartsWith(frame + "."))
                    foreach (string verbNetRole in _frameElementVerbNetRoles[frameElement])
                        classes.Add(verbNetRole.Substring(0, verbNetRole.LastIndexOf(".")));

            return classes;
        }

        /// <summary>
        /// Adds a mapping from PropBank to VerbNet
        /// </summary>
        /// <param name="propBankRole">PropBank role</param>
        /// <param name="verbNetRole">VerbNet role to add to PropBank role</param>
        public void AddVerbNetRoleForPropBank(string propBankRole, string verbNetRole)
        {
            // map propbank to verbnet
            _propBankRoleVerbNetRoles.EnsureContainsKey(propBankRole, typeof(Set<string>));
            if (!_propBankRoleVerbNetRoles[propBankRole].Contains(verbNetRole))
                _propBankRoleVerbNetRoles[propBankRole].Add(verbNetRole);

            // map verbnet to propbank
            _verbNetRolePropBankRoles.EnsureContainsKey(verbNetRole, typeof(Set<string>));
            if (!_verbNetRolePropBankRoles[verbNetRole].Contains(propBankRole))
                _verbNetRolePropBankRoles[verbNetRole].Add(propBankRole);
        }

        /// <summary>
        /// Adds a mapping from FrameNet to VerbNet
        /// </summary>
        /// <param name="frameElement">Frame element</param>
        /// <param name="verbNetRole">VerbNet role to add to frame element</param>
        public void AddVerbNetRoleForFrameNet(string frameElement, string verbNetRole)
        {
            // map framenet to verbnet
            _frameElementVerbNetRoles.EnsureContainsKey(frameElement, typeof(Set<string>));
            if (!_frameElementVerbNetRoles[frameElement].Contains(verbNetRole))
                _frameElementVerbNetRoles[frameElement].Add(verbNetRole);

            // map verbnet to framenet
            _verbNetRoleFrameElements.EnsureContainsKey(verbNetRole, typeof(Set<string>));
            if (!_verbNetRoleFrameElements[verbNetRole].Contains(frameElement))
                _verbNetRoleFrameElements[verbNetRole].Add(frameElement);
        }

        /// <summary>
        /// Removes a mapping from PropBank to VerbNet
        /// </summary>
        /// <param name="propBankRole">PropBank role</param>
        /// <param name="verbNetRole">VerbNet role to remove from PropBank role</param>
        public void RemoveVerbNetRoleForPropBank(string propBankRole, string verbNetRole)
        {
            _propBankRoleVerbNetRoles[propBankRole].Remove(verbNetRole);
            _verbNetRolePropBankRoles[verbNetRole].Remove(propBankRole);
        }

        /// <summary>
        /// Removes a mapping from FrameNet to VerbNet
        /// </summary>
        /// <param name="frameElement">Frame element</param>
        /// <param name="verbNetRole">VerbNet role to remove from frame element</param>
        public void RemoveVerbNetRoleForFrameElement(string frameElement, string verbNetRole)
        {
            _frameElementVerbNetRoles[frameElement].Remove(verbNetRole);
            _verbNetRoleFrameElements[verbNetRole].Remove(frameElement);
        }

        /// <summary>
        /// Gets all VerbNet roles
        /// </summary>
        /// <param name="includePropBankVerbNet">Whether or not to include VerbNet roles from the PropBank-VerbNet mapping</param>
        /// <param name="includeFrameNetVerbNet">Whether or not to include VerbNet roles from the FrameNet-VerbNet mapping</param>
        public IEnumerable<string> GetVerbNetRoles(bool includePropBankVerbNet, bool includeFrameNetVerbNet)
        {
            Set<string> vnRoles = new Set<string>(false);

            if (includePropBankVerbNet)
                foreach (string vnRole in _verbNetRolePropBankRoles.Keys)
                    vnRoles.Add(vnRole);

            if (includeFrameNetVerbNet)
                foreach (string vnRole in _verbNetRoleFrameElements.Keys)
                    vnRoles.Add(vnRole);

            return vnRoles;
        }
        #endregion

        #region get propbank roles
        /// <summary>
        /// Gets PropBank roles for a VerbNet role
        /// </summary>
        /// <param name="verbNetClass">VerbNet class</param>
        /// <param name="verbNetRole">VerbNet role</param>
        /// <param name="verbNetExtension">Extension to apply</param>
        /// <returns>PropBank roles</returns>
        public Set<string> GetPropBankRolesForVerbNet(string verbNetClass, string verbNetRole, VerbNetExtension verbNetExtension)
        {
            verbNetClass = verbNetClass.Trim();
            verbNetRole = verbNetRole.Trim();

            // try for exact match
            Set<string> pbRoles;
            if (!_verbNetRolePropBankRoles.TryGetValue(verbNetClass + "." + verbNetRole, out pbRoles))
                pbRoles = new Set<string>();

            if (pbRoles.Count > 0)
                return pbRoles;

            pbRoles.ThrowExceptionOnDuplicateAdd = false;

            // add sub-class extension
            if (verbNetExtension == VerbNetExtension.SubClass || verbNetExtension == VerbNetExtension.SuperAndSubClass)
                // add propbank roles corresponding to verbnet roles that are in sub-classes of the given class
                foreach (string fullVerbNetRole in _verbNetRolePropBankRoles.Keys)
                    if (fullVerbNetRole.StartsWith(verbNetClass) && fullVerbNetRole.EndsWith(verbNetRole))
                        pbRoles.AddRange(_verbNetRolePropBankRoles[fullVerbNetRole]);

            // add super-class extension
            if (verbNetExtension == VerbNetExtension.SuperClass || verbNetExtension == VerbNetExtension.SuperAndSubClass)
                // add propbank roles corresponding to verbnet roles that are in super-classes of the given class
                foreach (string fullVerbNetRole in _verbNetRolePropBankRoles.Keys)
                {
                    int lastPeriodIndex = fullVerbNetRole.LastIndexOf('.');
                    if (verbNetClass.StartsWith(fullVerbNetRole.Substring(0, lastPeriodIndex)) && verbNetRole == fullVerbNetRole.Substring(lastPeriodIndex + 1))
                        pbRoles.AddRange(_verbNetRolePropBankRoles[fullVerbNetRole]);
                }

            return pbRoles;
        }

        /// <summary>
        /// Gets PropBank roles for a VerbNet role
        /// </summary>
        /// <param name="verbNetRole">VerbNet role, in Class.Role notation</param>
        /// <param name="verbNetExtension">Extension to apply</param>
        /// <returns>PropBank roles</returns>
        public Set<string> GetPropBankRolesForVerbNet(string verbNetRole, VerbNetExtension verbNetExtension)
        {
            int lastPeriodIndex = verbNetRole.LastIndexOf('.');
            return GetPropBankRolesForVerbNet(verbNetRole.Substring(0, lastPeriodIndex), verbNetRole.Substring(lastPeriodIndex + 1), verbNetExtension);
        }

        /// <summary>
        /// Gets PropBank roles for a FrameNet frame element
        /// </summary>
        /// <param name="frameElement">Frame element, in Frame.FrameElement notation</param>
        /// <param name="verbNetToPropBankExtension">Extension to use when mapping VerbNet roles to PropBank</param>
        /// <returns>PropBank roles</returns>
        public Set<string> GetPropBankRolesForFrameNet(string frameElement, VerbNetExtension verbNetToPropBankExtension)
        {
            Set<string> pbRoles = new Set<string>(false);
            foreach (string vnRole in GetVerbNetRolesForFrameNet(frameElement.Trim()))
                pbRoles.AddRange(GetPropBankRolesForVerbNet(vnRole, verbNetToPropBankExtension));

            return pbRoles;
        }

        /// <summary>
        /// Gets PropBank roles for a FrameNet frame element
        /// </summary>
        /// <param name="frame">FrameNet frame</param>
        /// <param name="frameElement">FrameNet frame element</param>
        /// <param name="verbNetToPropBankExtension">Extension to use when mapping VerbNet roles to PropBank</param>
        /// <returns>PropBank roles</returns>
        public Set<string> GetPropBankRolesForFrameNet(string frame, string frameElement, VerbNetExtension verbNetToPropBankExtension)
        {
            return GetPropBankRolesForFrameNet(frame.Trim() + "." + frameElement.Trim(), verbNetToPropBankExtension);
        }

        /// <summary>
        /// Checks whether this SemLink contains a PropBank verb
        /// </summary>
        /// <param name="verb">Verb to check for</param>
        /// <returns>True if verb is present and false otherwise</returns>
        public bool ContainsPropBankVerb(string verb)
        {
            foreach (string propBankRole in _propBankRoleVerbNetRoles.Keys)
                if (propBankRole.StartsWith(verb + "."))
                    return true;

            return false;
        }
        #endregion

        #region get frame elements
        /// <summary>
        /// Gets frame elements for a VerbNet role
        /// </summary>
        /// <param name="verbNetClass">VerbNet class to get frame elements for</param>
        /// <param name="verbNetRole">VerbNet role to get frame elements for</param>
        /// <param name="verbNetExtension">Extension to apply</param>
        /// <returns>Frame elements</returns>
        public Set<string> GetFrameElementsForVerbNet(string verbNetClass, string verbNetRole, VerbNetExtension verbNetExtension)
        {
            verbNetClass = verbNetClass.Trim();
            verbNetRole = verbNetRole.Trim();

            // try for exact match
            Set<string> fes;
            if (!_verbNetRoleFrameElements.TryGetValue(verbNetClass + "." + verbNetRole, out fes))
                fes = new Set<string>();

            if (fes.Count > 0)
                return fes;

            fes.ThrowExceptionOnDuplicateAdd = false;

            // add sub-class extension
            if (verbNetExtension == VerbNetExtension.SubClass || verbNetExtension == VerbNetExtension.SuperAndSubClass)
                // add frame elements corresponding to verbnet roles that are in sub-classes of the given class
                foreach (string fullVerbNetRole in _verbNetRoleFrameElements.Keys)
                    if (fullVerbNetRole.StartsWith(verbNetClass) && fullVerbNetRole.EndsWith(verbNetRole))
                        fes.AddRange(_verbNetRoleFrameElements[fullVerbNetRole]);

            // add super-class extension
            if (verbNetExtension == VerbNetExtension.SuperClass || verbNetExtension == VerbNetExtension.SuperAndSubClass)
                // add frame elements corresponding to verbnet roles that are in super-classes of the given class
                foreach (string fullVerbNetRole in _verbNetRoleFrameElements.Keys)
                {
                    int lastPeriodIndex = fullVerbNetRole.LastIndexOf('.');
                    if (verbNetClass.StartsWith(fullVerbNetRole.Substring(0, lastPeriodIndex)) && verbNetRole == fullVerbNetRole.Substring(lastPeriodIndex + 1))
                        fes.AddRange(_verbNetRoleFrameElements[fullVerbNetRole]);
                }

            return fes;
        }

        /// <summary>
        /// Gets frame elements for a VerbNet role
        /// </summary>
        /// <param name="verbNetRole">VerbNet role, in Class.Role notation</param>
        /// <param name="verbNetExtension">Extension to apply</param>
        /// <returns>Frame elements</returns>
        public Set<string> GetFrameElementsForVerbNet(string verbNetRole, VerbNetExtension verbNetExtension)
        {
            int lastPeriodIndex = verbNetRole.LastIndexOf('.');
            return GetFrameElementsForVerbNet(verbNetRole.Substring(0, lastPeriodIndex), verbNetRole.Substring(lastPeriodIndex + 1), verbNetExtension);
        }

        /// <summary>
        /// Gets frame elements for a PropBank role
        /// </summary>
        /// <param name="propBankRole">PropBank role, in Verb.RoleSet.Role notation</param>
        /// <param name="verbNetToFrameNetExtension">Extension to use when mapping VerbNet roles to FrameNet</param>
        /// <returns>Frame elements</returns>
        public Set<string> GetFrameElementsForPropBank(string propBankRole, VerbNetExtension verbNetToFrameNetExtension)
        {
            Set<string> fes = new Set<string>(false);
            foreach (string verbNetRole in GetVerbNetRolesForPropBank(propBankRole.Trim()))
                fes.AddRange(GetFrameElementsForVerbNet(verbNetRole, verbNetToFrameNetExtension));

            return fes;
        }

        /// <summary>
        /// Gets frame elements for a PropBank role
        /// </summary>
        /// <param name="verb">PropBank verb</param>
        /// <param name="roleSet">PropBank role set</param>
        /// <param name="role">PropBank role</param>
        /// <param name="verbNetToFrameNetExtension">Extension to use when mapping VerbNet roles to FrameNet</param>
        /// <returns>Frame elements</returns>
        public Set<string> GetFrameElementsForPropBank(string verb, int roleSet, int role, VerbNetExtension verbNetToFrameNetExtension)
        {
            return GetFrameElementsForPropBank(verb.Trim() + "." + roleSet + "." + role, verbNetToFrameNetExtension);
        }
        #endregion

        /// <summary>
        /// Saves the PropBank-to-VerbNet mapping to file
        /// </summary>
        /// <param name="path">Path to file</param>
        public void SavePropBankVerbNetLinking(string path)
        {
            // gather propbank-verbnet linking - organized by verb, role set, verbnet class, then tuples of pb-vn argument links
            Dictionary<string, Dictionary<int, Dictionary<string, List<Tuple<int, string>>>>> pbVnLinking = new Dictionary<string, Dictionary<int, Dictionary<string, List<Tuple<int, string>>>>>();
            foreach (string propBankRole in PropBankRoles)
            {
                string[] parts = propBankRole.Split('.');
                string verb = parts[0];
                int roleSet = int.Parse(parts[1]);
                int arg = int.Parse(parts[2]);

                pbVnLinking.EnsureContainsKey(verb, typeof(Dictionary<int, Dictionary<string, List<Tuple<int, string>>>>));
                pbVnLinking[verb].EnsureContainsKey(roleSet, typeof(Dictionary<string, List<Tuple<int, string>>>));

                foreach (string verbNetRole in GetVerbNetRolesForPropBank(propBankRole))
                {
                    string verbNetClass = verbNetRole.Substring(0, verbNetRole.LastIndexOf('.'));
                    string themeRole = verbNetRole.Substring(verbNetRole.LastIndexOf('.') + 1);
                    pbVnLinking[verb][roleSet].EnsureContainsKey(verbNetClass, typeof(List<Tuple<int, string>>));
                    pbVnLinking[verb][roleSet][verbNetClass].Add(new Tuple<int, string>(arg, themeRole));
                }
            }

            // write linking file
            StreamWriter file = new StreamWriter(path);
            file.WriteLine("<pbvn-typemap>");
            foreach (string predicate in pbVnLinking.Keys)
            {
                file.WriteLine("  <predicate lemma=\"" + predicate + "\">");
                foreach (int roleSet in pbVnLinking[predicate].Keys)
                    foreach (string vnClass in pbVnLinking[predicate][roleSet].Keys)
                    {
                        file.WriteLine("    <argmap pb-roleset=\"" + predicate + "." + roleSet + "\" vn-class=\"" + vnClass + "\">");
                        foreach (Tuple<int, string> map in pbVnLinking[predicate][roleSet][vnClass])
                            file.WriteLine("      <role pb-arg=\"" + map.Item1 + "\" vn-theta=\"" + map.Item2 + "\" />");

                        file.WriteLine("    </argmap>");
                    }

                file.WriteLine("  </predicate>");
            }

            file.WriteLine("</pbvn-typemap>");
            file.Close();
        }

        /// <summary>
        /// Saves the FrameNet-to-VerbNet mapping to file
        /// </summary>
        /// <param name="path">Path to file</param>
        public void SaveFrameNetVerbNetLinking(string path)
        {
            // gather framenet-verbnet linking - organized by frame, verbnet class, then tuples of fn-vn links
            Dictionary<string, Dictionary<string, List<Tuple<string, string>>>> fnVnLinking = new Dictionary<string, Dictionary<string, List<Tuple<string, string>>>>();
            foreach (string frameElement in FrameElements)
            {
                string[] parts = frameElement.Split('.');
                string frame = InitialCharactersToUpper(parts[0],1);
                string fe = InitialCharactersToUpper(parts[1],1);

                fnVnLinking.EnsureContainsKey(frame, typeof(Dictionary<string, List<Tuple<string, string>>>));

                // gather roles for frame element
                foreach (string verbNetRole in GetVerbNetRolesForFrameNet(frameElement))
                {
                    string verbNetClass = verbNetRole.Substring(0, verbNetRole.LastIndexOf('.'));
                    string themeRole = verbNetRole.Substring(verbNetRole.LastIndexOf('.') + 1);
                    fnVnLinking[frame].EnsureContainsKey(verbNetClass, typeof(List<Tuple<string, string>>));
                    fnVnLinking[frame][verbNetClass].Add(new Tuple<string, string>(fe, themeRole));
                }
            }

            // write linking file...sort everything to make version control more informative
            StreamWriter file = new StreamWriter(path);
            file.WriteLine("<verbnetRoles-framenetFEs_RoleMappingData>");
            foreach (string frame in new SortedSet<string>(fnVnLinking.Keys))
                foreach (string vnClass in new SortedSet<string>(fnVnLinking[frame].Keys))
                {
                    file.WriteLine("  <vncls class='" + vnClass + "' fnframe='" + frame + "'>" + Environment.NewLine +
                                   "    <roles>");

                    foreach (Tuple<string, string> map in new SortedSet<Tuple<string, string>>(fnVnLinking[frame][vnClass]))
                        file.WriteLine("      <role fnrole='" + map.Item1 + "' vnrole='" + map.Item2 + "'/>");

                    file.WriteLine("    </roles>" + Environment.NewLine +
                                   "  </vncls>");
                }

            file.WriteLine("</verbnetRoles-framenetFEs_RoleMappingData>");
            file.Close();
        }

        private string InitialCharactersToUpper(string p0, int len)
        {
            throw new NotImplementedException();
        }
    }

    public class SortedSet<T> : IEnumerable<T>
    {
        public SortedSet(object keys)
        {
            throw new NotImplementedException();
        }
        public SortedSet(Dictionary<string, List<Tuple<string, string>>>.KeyCollection keys)
        {
            throw new NotImplementedException();
        }

        public IEnumerator<T> GetEnumerator()
        {
            throw new NotImplementedException();
        }

        IEnumerator IEnumerable.GetEnumerator()
        {
            return GetEnumerator();
        }
    }

    public class Tuple<T, T1>
    {
        public Tuple(T i, T1 themeRole)
        {
            throw new NotImplementedException();
            
        }

        public string Item1
        {
            get { throw new NotImplementedException(); }
            set { throw new NotImplementedException(); }
        }

        public string Item2

        {
            get { throw new NotImplementedException(); }
            set { throw new NotImplementedException(); }
        }
    }
}