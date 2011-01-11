using System;
using System.Collections.Generic;
using System.Text;
using System.IO;
using System.Collections.ObjectModel;

namespace LAIR.ResourceAPIs.FrameNet
{
    /// <summary>
    /// Represents a FrameNet frame
    /// </summary>
    public class Frame
    {
        #region static members
        private static Dictionary<string, FrameRelation> _stringFrameRelations;

        /// <summary>
        /// Different relations that can hold between two frames
        /// </summary>
        public enum FrameRelation
        {
            Inheritance,
            Subframe,
            Using,
            SeeAlso,
            ReframingMapping,
            CoreSet,
            Excludes,
            Requires,
            InchoativeOf,
            CausativeOf,
            Precedes,
            PerspectiveOn
        };

        /// <summary>
        /// Directions for frame relations
        /// </summary>
        public enum FrameRelationDirection
        {
            /// <summary>
            /// Super frame
            /// </summary>
            Super,

            /// <summary>
            /// Sub frame
            /// </summary>
            Sub,

            /// <summary>
            /// Indicate both super- and sub-frames
            /// </summary>
            Both
        }

        /// <summary>
        /// Equality operator
        /// </summary>
        /// <param name="f1"></param>
        /// <param name="f2"></param>
        /// <returns></returns>
        public static bool operator ==(Frame f1, Frame f2)
        {
            // check for null f1
            if (!(f1 is Frame))
                return !(f2 is Frame);

            return f1.Equals(f2);
        }

        /// <summary>
        /// Inequality operator
        /// </summary>
        /// <param name="f1"></param>
        /// <param name="f2"></param>
        /// <returns></returns>
        public static bool operator !=(Frame f1, Frame f2)
        {
            return !(f1 == f2);
        }

        /// <summary>
        /// Constructor
        /// </summary>
        static Frame()
        {
            _stringFrameRelations = new Dictionary<string, FrameRelation>();
            _stringFrameRelations.Add("Inheritance", FrameRelation.Inheritance);
            _stringFrameRelations.Add("Subframe", FrameRelation.Subframe);
            _stringFrameRelations.Add("Using", FrameRelation.Using);
            _stringFrameRelations.Add("See_also", FrameRelation.SeeAlso);
            _stringFrameRelations.Add("ReFraming_Mapping", FrameRelation.ReframingMapping);
            _stringFrameRelations.Add("CoreSet", FrameRelation.CoreSet);
            _stringFrameRelations.Add("Excludes", FrameRelation.Excludes);
            _stringFrameRelations.Add("Requires", FrameRelation.Requires);
            _stringFrameRelations.Add("Inchoative_of", FrameRelation.InchoativeOf);
            _stringFrameRelations.Add("Causative_of", FrameRelation.CausativeOf);
            _stringFrameRelations.Add("Precedes", FrameRelation.Precedes);
            _stringFrameRelations.Add("Perspective_on", FrameRelation.PerspectiveOn);
        }

        /// <summary>
        /// Gets a frame relation from it's XML attribute name
        /// </summary>
        /// <param name="relation">Relation name</param>
        /// <returns>Frame relation</returns>
        public static FrameRelation GetFrameRelation(string relation)
        {
            return _stringFrameRelations[relation];
        }
        #endregion

        private int _id;
        private readonly string _name;
        private readonly int _hashCode;
        private string _definition;
        private FrameElementSet _frameElements;
        private LexicalUnitSet _lexicalUnits;
        private Dictionary<FrameRelation, FrameSet> _relationSubFrames;
        private Dictionary<FrameRelation, FrameSet> _relationSuperFrames;

        /// <summary>
        /// Gets frame elements in this frame
        /// </summary>
        public FrameElementSet FrameElements
        {
            get { return _frameElements; }
        }

        /// <summary>
        /// Gets lexical units in this frame
        /// </summary>
        public LexicalUnitSet LexicalUnits
        {
            get { return _lexicalUnits; }
        }

        /// <summary>
        /// Gets the name of the frame. Both the ID and Name are unique identifiers.
        /// </summary>
        public string Name
        {
            get { return _name; }
        }

        /// <summary>
        /// Gets the definition
        /// </summary>
        public string Definition
        {
            get { return _definition; }
        }

        /// <summary>
        /// Gets ID for frame. Both the ID and Name are unique identifiers.
        /// </summary>
        public int ID
        {
            get { return _id; }
        }

        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="name">Name of frame</param>
        /// <param name="definition">Definition of frame</param>
        /// <param name="id">ID of frame</param>
        public Frame(string name, string definition, int id)
        {
            _name = name;
            _hashCode = _name.GetHashCode();
            _definition = definition;
            _id = id;
            _frameElements = new FrameElementSet();
            _lexicalUnits = new LexicalUnitSet();

            // initialize empty sets of related frames
            _relationSubFrames = new Dictionary<FrameRelation, FrameSet>();
            _relationSuperFrames = new Dictionary<FrameRelation, FrameSet>();
            foreach (FrameRelation relation in Enum.GetValues(typeof(FrameRelation)))
            {
                // version 1.3 of framenet contains duplicate frame-frame relation mappings, so allow duplicate elements to be added to these sets
                _relationSubFrames.Add(relation, new FrameSet(false));
                _relationSuperFrames.Add(relation, new FrameSet(false));
            }
        }

        /// <summary>
        /// Gets a list of related frames
        /// </summary>
        /// <param name="relation">Type of relation to fetch</param>
        /// <param name="relationDirection">Relation direction</param>
        /// <param name="recursive">Whether or not to get related frames recursively</param>
        /// <returns>Set of frames</returns>
        public FrameSet GetRelatedFrames(FrameRelation relation, FrameRelationDirection relationDirection, bool recursive)
        {
            FrameSet frames = new FrameSet(true);
            GetRelatedFrames(relation, relationDirection, recursive, frames);

            return frames;
        }

        /// <summary>
        /// Gets a list of related frames
        /// </summary>
        /// <param name="relation">Type of relation to fetch</param>
        /// <param name="relationDirection">Relation direction</param>
        /// <param name="recursive">Whether or not to get related frames recursively</param>
        /// <param name="currentFrames">Current set of frames</param>
        /// <returns>Set of frames</returns>
        private void GetRelatedFrames(FrameRelation relation, FrameRelationDirection relationDirection, bool recursive, FrameSet currentFrames)
        {
            // add sub-frames
            if (relationDirection == FrameRelationDirection.Sub || relationDirection == FrameRelationDirection.Both)
                foreach (Frame subFrame in _relationSubFrames[relation])
                    if (!currentFrames.Contains(subFrame))
                    {
                        currentFrames.Add(subFrame);

                        // recursively add sub-frames
                        if (recursive)
                            subFrame.GetRelatedFrames(relation, relationDirection, recursive, currentFrames);
                    }

            // add super-frames
            if (relationDirection == FrameRelationDirection.Super || relationDirection == FrameRelationDirection.Both)
                foreach (Frame superFrame in _relationSuperFrames[relation])
                    if (!currentFrames.Contains(superFrame))
                    {
                        currentFrames.Add(superFrame);

                        // recursively add super-frames
                        if (recursive)
                            superFrame.GetRelatedFrames(relation, relationDirection, recursive, currentFrames);
                    }
        }

        /// <summary>
        /// Gets the name of this frame
        /// </summary>
        /// <returns>Name of this frame</returns>
        public override string ToString()
        {
            return _name;
        }

        /// <summary>
        /// Gets whether this frame equals another
        /// </summary>
        /// <param name="obj">Object for comparison</param>
        /// <returns>True if frames are equal, False otherwise</returns>
        public override bool Equals(object obj)
        {
            if (!(obj is Frame))
                return false;

            Frame frame = obj as Frame;

            return _name == frame.Name;
        }

        /// <summary>
        /// Gets hashcode for this frame
        /// </summary>
        /// <returns>Hashcode for this frame</returns>
        public override int GetHashCode()
        {
            return _hashCode;
        }

        /// <summary>
        /// Gets super-frames
        /// </summary>
        /// <param name="relation">Relation to query</param>
        /// <returns>Super-frames</returns>
        public FrameSet GetSuperFrames(FrameRelation relation)
        {
            return _relationSuperFrames[relation];
        }

        /// <summary>
        /// Gets sub-frames
        /// </summary>
        /// <param name="relation">Relation to query</param>
        /// <returns>Sub-frames</returns>
        public FrameSet GetSubFrames(FrameRelation relation)
        {
            return _relationSubFrames[relation];
        }
    }
}
