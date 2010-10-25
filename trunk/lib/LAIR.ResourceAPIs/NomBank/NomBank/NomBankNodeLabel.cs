using System;
using System.Collections.Generic;
using System.Text;
using LAIR.Collections.Generic;

namespace LAIR.ResourceAPIs.NomBank
{
    /// <summary>
    /// Describes a NomBank node in terms of its type, feature, and hyphen indexes
    /// </summary>
    public class NomBankNodeLabel
    {
        #region static members
        /// <summary>
        /// NodeType enumeration
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
            /// Argument 6
            /// </summary>
            Arg6,

            /// <summary>
            /// Argument 7
            /// </summary>
            Arg7,

            /// <summary>
            /// Argument 8
            /// </summary>
            Arg8,

            /// <summary>
            /// Argument 9
            /// </summary>
            Arg9,

            /// <summary>
            /// Predicate node
            /// </summary>
            Predicate,

            /// <summary>
            /// Modifier node
            /// </summary>
            Modifier,

            /// <summary>
            /// Support item
            /// </summary>
            Support
        }

        /// <summary>
        /// NodeFeature enumeration
        /// </summary>
        public enum NodeFeature
        {
            /// <summary>
            /// No node feature
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
            /// Secondary predication
            /// </summary>
            SecondaryPredication,

            /// <summary>
            /// Negation
            /// </summary>
            Negation,

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
            /// Outside reference
            /// </summary>
            OutsideReference,
        }

        /// <summary>
        /// Hyphenation indexes used
        /// </summary>
        public enum HyphenationIndex
        {
            /// <summary>
            /// First hyphenated part
            /// </summary>
            H0,

            /// <summary>
            /// Second hyphenated part
            /// </summary>
            H1,

            /// <summary>
            /// Third hyphenated part
            /// </summary>
            H2,

            /// <summary>
            /// Fourth hyphenated part
            /// </summary>
            H3,

            /// <summary>
            /// Fifth hyphenated part
            /// </summary>
            H4,

            /// <summary>
            /// Sixth hyphenated part
            /// </summary>
            H5
        };

        /// <summary>
        /// Gets the argument types
        /// </summary>
        public static Set<NodeType> ArgumentTypes
        {
            get { return _argumentTypes; }
        }

        /// <summary>
        /// Gets the feature types (excluding None)
        /// </summary>
        public static Set<NodeFeature> FeatureTypes
        {
            get { return _featureTypes; }
        }

        private static Dictionary<string, NodeType> _stringTypeMap;
        private static Dictionary<NodeType, string> _typeStringMap;
        private static Dictionary<string, NodeFeature> _stringFeatureMap;
        private static Dictionary<NodeFeature, string> _featureStringMap;
        private static Set<NodeType> _argumentTypes;
        private static Set<NodeFeature> _featureTypes;

        /// <summary>
        /// Static constructor
        /// </summary>
        static NomBankNodeLabel()
        {
            _stringTypeMap = new Dictionary<string, NodeType>();

            // argument nodes
            _stringTypeMap.Add("ARG0", NodeType.Arg0);
            _stringTypeMap.Add("ARG1", NodeType.Arg1);
            _stringTypeMap.Add("ARG2", NodeType.Arg2);
            _stringTypeMap.Add("ARG3", NodeType.Arg3);
            _stringTypeMap.Add("ARG4", NodeType.Arg4);
            _stringTypeMap.Add("ARG5", NodeType.Arg5);
            _stringTypeMap.Add("ARG6", NodeType.Arg6);
            _stringTypeMap.Add("ARG7", NodeType.Arg7);
            _stringTypeMap.Add("ARG8", NodeType.Arg8);
            _stringTypeMap.Add("ARG9", NodeType.Arg9);
            _stringTypeMap.Add("ARGM", NodeType.Modifier);
            _stringTypeMap.Add("Support", NodeType.Support);
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
            _stringFeatureMap.Add("PRD", NodeFeature.SecondaryPredication);
            _stringFeatureMap.Add("NEG", NodeFeature.Negation);
            _stringFeatureMap.Add("ADV", NodeFeature.Adverbial);
            _stringFeatureMap.Add("MNR", NodeFeature.Manner);
            _stringFeatureMap.Add("CAU", NodeFeature.Cause);
            _stringFeatureMap.Add("PNC", NodeFeature.Purpose);
            _stringFeatureMap.Add("DIS", NodeFeature.DiscourseConnective);
            _stringFeatureMap.Add("REF", NodeFeature.OutsideReference);

            // reverse lookup
            _featureStringMap = new Dictionary<NodeFeature, string>();
            foreach (string s in _stringFeatureMap.Keys)
                _featureStringMap.Add(_stringFeatureMap[s], s);

            // get all argument types
            _argumentTypes = new Set<NodeType>(new NodeType[]{
                    NodeType.Arg0, NodeType.Arg1, NodeType.Arg2, NodeType.Arg3, 
                    NodeType.Arg4, NodeType.Arg5, NodeType.Arg6, NodeType.Arg7,
                    NodeType.Arg8, NodeType.Arg9});

            // get all feature types
            _featureTypes = new Set<NodeFeature>();
            foreach (NodeFeature f in Enum.GetValues(typeof(NodeFeature)))
                if (f != NodeFeature.None)
                    _featureTypes.Add(f);
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
        public static bool operator == (NomBankNodeLabel label1, NomBankNodeLabel label2)
        {
            // check for null labels
            if (!(label1 is NomBankNodeLabel))
                return !(label2 is NomBankNodeLabel);

            return label1.Equals(label2);
        }

        /// <summary>
        /// Checks inequality of two labels
        /// </summary>
        /// <param name="label1">First label</param>
        /// <param name="label2">Second label</param>
        /// <returns>True if labels are not equal, false otherwise</returns>
        public static bool operator != (NomBankNodeLabel label1, NomBankNodeLabel label2)
        {
            return !(label1 == label2);
        }
        #endregion

        private readonly NodeType _type;
        private readonly NodeFeature _feature;
        private readonly int _hashCode;
        private List<HyphenationIndex> _hyphenIndexes;
        private float _confidence;

        /// <summary>
        /// Gets or sets the confidence for this label
        /// </summary>
        public float Confidence
        {
            get { return _confidence; }
            set { _confidence = value; }
        }

        /// <summary>
        /// Gets the list of hyphen indexes
        /// </summary>
        public List<HyphenationIndex> HyphenIndexes
        {
            get { return _hyphenIndexes; }
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
        /// Gets whether or not this is a support verb label
        /// </summary>
        public bool IsSupportVerb
        {
            get { return _type == NodeType.Support; }
        }

        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="type">Type of node</param>
        /// <param name="confidence">Confidence of label</param>
        public NomBankNodeLabel(NodeType type, float confidence)
        {
            _type = type;
            _confidence = confidence;
            _feature = NodeFeature.None;
            _hyphenIndexes = new List<HyphenationIndex>();
            _hashCode = type.ToString().GetHashCode();
        }

        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="type">Type of node</param>
        /// <param name="feature">Feature for node</param>
        /// <param name="confidence">Confidence of label</param>
        public NomBankNodeLabel(NodeType type, NodeFeature feature, float confidence)
            : this(type, confidence)
        {
            _feature = feature;
            _hashCode = (_type.ToString() + "-" + _feature.ToString()).GetHashCode();
        }

        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="label">Label string (e.g., "Arg0", "Modifier-Temporal", "Arg1-H0", etc.)</param>
        /// <param name="confidence">Confidence of label</param>
        public NomBankNodeLabel(string label, float confidence)
        {
            _confidence = confidence;
            _hyphenIndexes = new List<HyphenationIndex>();

            string[] labelParts = label.Split('-');

            // first part is always type
            _type = (NodeType)Enum.Parse(typeof(NodeType), labelParts[0]);

            // check for second part
            if (labelParts.Length > 1)
            {
                string secondPart = labelParts[1];
                
                // try to parse second part as a feature
                try { _feature = (NodeFeature)Enum.Parse(typeof(NodeFeature), secondPart); }
                catch (Exception)
                {
                    // if not a feature, the second part must be a hyphenation index
                    _hyphenIndexes.Add((HyphenationIndex)Enum.Parse(typeof(HyphenationIndex), secondPart));
                }                
            }

            // any remaining parts must be hyphenation indexes
            for (int i = 2; i < labelParts.Length; ++i)
                _hyphenIndexes.Add((HyphenationIndex)Enum.Parse(typeof(HyphenationIndex), labelParts[i]));
        }


        /// <summary>
        /// Adds a hyphen index to this label
        /// </summary>
        /// <param name="hyphenIndex">Hyphen index to add</param>
        public void AddHyphenIndex(HyphenationIndex hyphenIndex)
        {
            if (_hyphenIndexes.Contains(hyphenIndex))
                throw new Exception("Attempted to add duplicate hyphen index");

            _hyphenIndexes.Add(hyphenIndex);
        }        

        /// <summary>
        /// Gets whether or not this label contains a hyphen index
        /// </summary>
        /// <param name="hyphenIndex">Index to check for</param>
        /// <returns>True if this label contains the given index, false otherwise</returns>
        public bool ContainsHyphenIndex(HyphenationIndex hyphenIndex)
        {
            return _hyphenIndexes.Contains(hyphenIndex);
        }

        /// <summary>
        /// Returns the type/feature/indexes in a nicely formatted string
        /// </summary>
        /// <returns>Label</returns>
        public override string ToString()
        {
            return ToString(true, true);
        }

        /// <summary>
        /// Returns the type/feature/indexes in a nicely formatted string
        /// </summary>
        /// <param name="includeArgumentFeature">Whether or not to include features for argument labels</param>
        /// <param name="includeHyphenIndexes">Whether or not to include hyphen indexes</param>
        /// <returns>Label</returns>
        public string ToString(bool includeArgumentFeature, bool includeHyphenIndexes)
        {
            // always include type
            string labelString = _type.ToString();

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
                labelString += "-" + _feature;

            // add hyphenation indexes
            if (includeHyphenIndexes)
                foreach (HyphenationIndex hyphenIndex in _hyphenIndexes)
                    labelString += "-" + hyphenIndex;

            return labelString;
        }

        /// <summary>
        /// Gets whether or not this NomBankNodeLabel equals another object
        /// </summary>
        /// <param name="obj">Object to compare this one with</param>
        /// <returns>True if the other object is a NomBankNodeLabel and is equal to this one, false otherwise</returns>
        public override bool Equals(object obj)
        {
            if (!(obj is NomBankNodeLabel))
                return false;

            NomBankNodeLabel label = obj as NomBankNodeLabel;

            return _type == label.Type && _feature == label.Feature;
        }

        /// <summary>
        /// Gets hash code for this label
        /// </summary>
        /// <returns></returns>
        public override int GetHashCode()
        {
            return _hashCode;
        }

        /// <summary>
        /// Gets a deep copy of this label
        /// </summary>
        /// <returns>Deep copy of this label</returns>
        public NomBankNodeLabel Copy()
        {
            NomBankNodeLabel copied = new NomBankNodeLabel(_type, _feature, _confidence);
            foreach (HyphenationIndex hyphenIndex in _hyphenIndexes)
                copied.AddHyphenIndex(hyphenIndex);

            return copied;
        }
    }
}
