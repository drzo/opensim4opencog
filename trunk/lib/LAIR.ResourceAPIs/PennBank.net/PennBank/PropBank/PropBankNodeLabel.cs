using System;
using System.Collections.Generic;
using System.Text;

using LAIR.Collections.Generic;

namespace LAIR.ResourceAPIs.PennBank.PropBank
{
    /// <summary>
    /// Describes a PropBank node in terms of its type and feature
    /// </summary>
    public class PropBankNodeLabel
    {
        #region static members
        /// <summary>
        /// Node types
        /// </summary>
        public enum NodeType
        {
            /// <summary>
            /// Argument 0
            /// </summary>
            Arg0,

            /// <summary>
            /// Argument 1
            /// </summary>
            Arg1,

            /// <summary>
            /// Argument 2
            /// </summary>
            Arg2,

            /// <summary>
            /// Argument 3
            /// </summary>
            Arg3,

            /// <summary>
            /// Argument 4
            /// </summary>
            Arg4,

            /// <summary>
            /// Argument 5
            /// </summary>
            Arg5,

            /// <summary>
            /// Causative agent
            /// </summary>
            CausativeAgent,

            /// <summary>
            /// Predicate node
            /// </summary>
            Predicate,

            /// <summary>
            /// Modifier node
            /// </summary>
            Modifier
        }

        /// <summary>
        /// Node features
        /// </summary>
        public enum NodeFeature
        {
            /// <summary>
            /// No feature
            /// </summary>
            None,

            /// <summary>
            /// Extent
            /// </summary>
            Extent,

            /// <summary>
            /// Direction
            /// </summary>
            Direction,

            /// <summary>
            /// Location
            /// </summary>
            Location,

            /// <summary>
            /// Temporal
            /// </summary>
            Temporal,

            /// <summary>
            /// Reciprocal
            /// </summary>
            Reciprocal,

            /// <summary>
            /// Secondary predication
            /// </summary>
            SecondaryPredication,

            /// <summary>
            /// Negation
            /// </summary>
            Negation,

            /// <summary>
            /// Modal verb
            /// </summary>
            ModalVerb,

            /// <summary>
            /// Adverbial
            /// </summary>
            Adverbial,

            /// <summary>
            /// Manner
            /// </summary>
            Manner,

            /// <summary>
            /// Cause
            /// </summary>
            Cause,

            /// <summary>
            /// Purpose
            /// </summary>
            Purpose,

            /// <summary>
            /// Discourse connective
            /// </summary>
            DiscourseConnective,

            /// <summary>
            /// For prepositions
            /// </summary>
            Preposition
        }

        private static Dictionary<string, NodeType> _stringTypeMap;
        private static Dictionary<NodeType, string> _typeStringMap;
        private static Dictionary<string, NodeFeature> _stringFeatureMap;
        private static Dictionary<NodeFeature, string> _featureStringMap;
        private static Set<NodeType> _argumentTypes;

        /// <summary>
        /// Gets the argument types
        /// </summary>
        public static Set<NodeType> ArgumentTypes
        {
            get { return _argumentTypes; }
        }

        /// <summary>
        /// Static constructor
        /// </summary>
        static PropBankNodeLabel()
        {
            _stringTypeMap = new Dictionary<string, NodeType>();

            // argument nodes
            _stringTypeMap.Add("ARG0", NodeType.Arg0);
            _stringTypeMap.Add("ARG1", NodeType.Arg1);
            _stringTypeMap.Add("ARG2", NodeType.Arg2);
            _stringTypeMap.Add("ARG3", NodeType.Arg3);
            _stringTypeMap.Add("ARG4", NodeType.Arg4);
            _stringTypeMap.Add("ARG5", NodeType.Arg5);
            _stringTypeMap.Add("ARGM", NodeType.Modifier);
            _stringTypeMap.Add("ARGA", NodeType.CausativeAgent);
            _stringTypeMap.Add("rel", NodeType.Predicate);

            // reverse lookup
            _typeStringMap = new Dictionary<NodeType, string>();
            foreach (string s in _stringTypeMap.Keys)
                _typeStringMap.Add(_stringTypeMap[s], s);

            // argument features
            _stringFeatureMap = new Dictionary<string, NodeFeature>();
            _stringFeatureMap.Add("EXT", NodeFeature.Extent);
            _stringFeatureMap.Add("DIR", NodeFeature.Direction);
            _stringFeatureMap.Add("LOC", NodeFeature.Location);
            _stringFeatureMap.Add("TMP", NodeFeature.Temporal);
            _stringFeatureMap.Add("REC", NodeFeature.Reciprocal);
            _stringFeatureMap.Add("PRD", NodeFeature.SecondaryPredication);
            _stringFeatureMap.Add("NEG", NodeFeature.Negation);
            _stringFeatureMap.Add("MOD", NodeFeature.ModalVerb);
            _stringFeatureMap.Add("ADV", NodeFeature.Adverbial);
            _stringFeatureMap.Add("MNR", NodeFeature.Manner);
            _stringFeatureMap.Add("CAU", NodeFeature.Cause);
            _stringFeatureMap.Add("PNC", NodeFeature.Purpose);
            _stringFeatureMap.Add("DIS", NodeFeature.DiscourseConnective);

            /* PRP is not a feature defined in PropBank. Instead, PropBank uses the actual preposition as the 
             * feature. This is problematic when trying to parse the prop.txt file, so we created the PRP feature
             * to use instead. */
            _stringFeatureMap.Add("PRP", NodeFeature.Preposition);

            // reverse lookup
            _featureStringMap = new Dictionary<NodeFeature, string>();
            foreach (string s in _stringFeatureMap.Keys)
                _featureStringMap.Add(_stringFeatureMap[s], s);

            // get all argument types
            _argumentTypes = new Set<NodeType>(new NodeType[] { NodeType.Arg0, NodeType.Arg1, NodeType.Arg2, NodeType.Arg3, NodeType.Arg4, NodeType.Arg5 });
        }

        /// <summary>
        /// Gets the node type given a string
        /// </summary>
        /// <param name="label">Node label</param>
        /// <returns>NodeType</returns>
        public static NodeType GetNodeType(string label)
        {
            return _stringTypeMap[label];
        }

        /// <summary>
        /// Gets node type string from the node type
        /// </summary>
        /// <param name="type">Type of node</param>
        /// <returns>Node type string</returns>
        public static string GetNodeTypeString(NodeType type)
        {
            return _typeStringMap[type];
        }

        /// <summary>
        /// Gets the feature for a string
        /// </summary>
        /// <param name="feature">Feature name</param>
        /// <returns>NodeFeature</returns>
        public static NodeFeature GetNodeFeature(string feature)
        {
            return _stringFeatureMap[feature];
        }

        /// <summary>
        /// Tries to get the feature for a string
        /// </summary>
        /// <param name="feature">Feature name</param>
        /// <param name="nodeFeature">NodeFeature</param>
        /// <returns>True if feature was found, false otherwise</returns>
        public static bool TryGetNodeFeature(string feature, out NodeFeature nodeFeature)
        {
            return _stringFeatureMap.TryGetValue(feature, out nodeFeature);
        }

        /// <summary>
        /// Gets string for a feature
        /// </summary>
        /// <param name="feature">NodeFeature</param>
        /// <returns>String</returns>
        public static string GetNodeFeatureString(NodeFeature feature)
        {
            return _featureStringMap[feature];
        }

        /// <summary>
        /// Checks equality of two labels
        /// </summary>
        /// <param name="label1">First label</param>
        /// <param name="label2">Second label</param>
        /// <returns>True if labels are equal, false otherwise</returns>
        public static bool operator ==(PropBankNodeLabel label1, PropBankNodeLabel label2)
        {
            // check for null operand
            if (!(label1 is PropBankNodeLabel))
                return !(label2 is PropBankNodeLabel);

            return label1.Equals(label2);
        }

        /// <summary>
        /// Checks inequality of two labels
        /// </summary>
        /// <param name="label1">First label</param>
        /// <param name="label2">Second label</param>
        /// <returns>True if labels are not equal, false otherwise</returns>
        public static bool operator !=(PropBankNodeLabel label1, PropBankNodeLabel label2)
        {
            return !(label1 == label2);
        }
        #endregion

        private NodeType _type;
        private NodeFeature _feature;
        private float _confidence;

        /// <summary>
        /// Gets or sets the confidence for this label
        /// </summary>
        public float Confidence
        {
            get { return _confidence; }
        }

        /// <summary>
        /// Gets or sets the type
        /// </summary>
        public NodeType Type
        {
            get { return _type; }
        }

        /// <summary>
        /// Gets or sets the feature
        /// </summary>
        public NodeFeature Feature
        {
            get { return _feature; }
        }

        /// <summary>
        /// Gets whether or not this is an argument label
        /// </summary>
        public bool IsArgument
        {
            get { return _argumentTypes.Contains(_type); }
        }

        /// <summary>
        /// Gets whether or not this is a modifier label
        /// </summary>
        public bool IsModifier
        {
            get { return _type == NodeType.Modifier; }
        }

        /// <summary>
        /// Gets whether or not this is a predicate label
        /// </summary>
        public bool IsPredicate
        {
            get { return _type == NodeType.Predicate; }
        }

        /// <summary>
        /// Gets the argument index of this node. Only valid for argument nodes.
        /// </summary>
        public int ArgumentIndex
        {
            get
            {
                if (!IsArgument)
                    throw new Exception("ArgumentIndex only valid for argument nodes");

                return int.Parse(_type.ToString()[3].ToString());
            }
        }

        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="type">Type of node</param>
        /// <param name="confidence">Confidence of label</param>
        public PropBankNodeLabel(NodeType type, float confidence)
        {
            _type = type;
            _confidence = confidence;
            _feature = NodeFeature.None;
        }

        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="type">Type of node</param>
        /// <param name="feature">Feature for node</param>
        /// <param name="confidence">Confidence of label</param>
        public PropBankNodeLabel(NodeType type, NodeFeature feature, float confidence)
            : this(type, confidence)
        {
            _feature = feature;
        }

        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="label">Label string (e.g., "Arg0", "Modifier-Temporal", etc.)</param>
        /// <param name="confidence">Confidence of label</param>
        public PropBankNodeLabel(string label, float confidence)
        {
            _confidence = confidence;

            string[] labelParts = label.Split('-');

            if (labelParts.Length > 2)
                throw new Exception("Unexpected label parts");

            // first part is always type
            _type = (NodeType)Enum.Parse(typeof(NodeType), labelParts[0]);

            // check for second part (the feature)
            if (labelParts.Length == 2)
                _feature = (NodeFeature)Enum.Parse(typeof(NodeFeature), labelParts[1]);
            else
                _feature = NodeFeature.None;
        }

        /// <summary>
        /// Returns the type/feature in a nicely formatted string
        /// </summary>
        /// <returns>Label</returns>
        public override string ToString()
        {
            return ToString(true);
        }

        /// <summary>
        /// Returns the type/feature in a nicely formatted string
        /// </summary>
        /// <param name="includeArgumentFeature">Whether or not to include features for argument labels</param>
        /// <returns>Label</returns>
        public string ToString(bool includeArgumentFeature)
        {
            // always include type
            string descString = _type.ToString();

            // get whether or not we should add the feature
            bool addFeature = false;
            if (IsArgument)
            {
                if (includeArgumentFeature && _feature != NodeFeature.None)
                    addFeature = true;
            }
            else if (IsModifier)
                if (_feature != NodeFeature.None)
                    addFeature = true;
                else
                    throw new Exception("Modifiers must have features");

            if (addFeature)
                descString += "-" + _feature;

            return descString;
        }

        /// <summary>
        /// Gets whether or not this PropBankNodeLabel equals another object
        /// </summary>
        /// <param name="obj">Object to compare this one with</param>
        /// <returns>True if the other object is a PropBankNodeLabel and is equal to this one, false otherwise</returns>
        public override bool Equals(object obj)
        {
            if (!(obj is PropBankNodeLabel))
                return false;

            PropBankNodeLabel label = obj as PropBankNodeLabel;

            return _type == label.Type && _feature == label.Feature;
        }

        /// <summary>
        /// Gets hash code for this label
        /// </summary>
        /// <returns></returns>
        public override int GetHashCode()
        {
            return ToString().GetHashCode();
        }
    }
}