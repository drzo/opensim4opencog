using System;
using System.Collections.Generic;
using System.Text;
using LAIR.Collections.Generic;

namespace LAIR.ResourceAPIs.FrameNet
{
    /// <summary>
    /// Represents a frame element within FrameNet
    /// </summary>
    public class FrameElement
    {
        private readonly int _id;
        private readonly int _hashCode;
        private string _name;
        private string _definition;
        private Dictionary<Frame.FrameRelation, FrameElementSet> _relationSubFrameElements;
        private Dictionary<Frame.FrameRelation, FrameElementSet> _relationSuperFrameElements;
        private Frame _frame;
        private FrameElement _frameElementSearchBackPointer;
        private Frame.FrameRelation _frameRelationSearchBackPointer;

        /// <summary>
        /// Gets the frame that contains this frame element
        /// </summary>
        public Frame Frame
        {
            get { return _frame; }
        }

        /// <summary>
        /// After calling GetShortestPathTo, this will point back towards the frame element from which the search originated.
        /// </summary>
        internal FrameElement FrameElementSearchBackPointer
        {
            get { return _frameElementSearchBackPointer; }
        }

        /// <summary>
        /// After calling GetShortestPathTo, this will contain the relation between this frame element and the one returned 
        /// by FrameElementSearchBackPointer.
        /// </summary>
        internal Frame.FrameRelation FrameRelationSearchBackPointer
        {
            get { return _frameRelationSearchBackPointer; }
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
        /// Gets the definition
        /// </summary>
        public string Definition
        {
            get { return _definition; }
        }

        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="id">ID of FE</param>
        /// <param name="name">Name of FE</param>
        /// <param name="definition">Definition of FE</param>
        /// <param name="frame">Frame that contains this frame element</param>
        public FrameElement(int id, string name, string definition, Frame frame)
        {
            _id = id;
            _hashCode = _id.GetHashCode();
            _name = name;
            _definition = definition;
            _frame = frame;

            _relationSubFrameElements = new Dictionary<Frame.FrameRelation, FrameElementSet>();
            _relationSuperFrameElements = new Dictionary<Frame.FrameRelation, FrameElementSet>();

            // initialize empty lists of related frame elements
            foreach (Frame.FrameRelation relation in Enum.GetValues(typeof(Frame.FrameRelation)))
            {
                _relationSubFrameElements.Add(relation, new FrameElementSet());
                _relationSuperFrameElements.Add(relation, new FrameElementSet());
            }
        }

        /// <summary>
        /// Equals override
        /// </summary>
        /// <param name="obj">Object to compare this one to</param>
        /// <returns>True if object is a FE with the same ID, False otherwise</returns>
        public override bool Equals(object obj)
        {
            if (!(obj is FrameElement))
                return false;

            FrameElement fe = (FrameElement)obj;

            return _id == fe.ID;
        }

        /// <summary>
        /// Gets the name of the frame element in Frame.FrameElement notation
        /// </summary>
        /// <returns>Name of this FE</returns>
        public override string ToString()
        {
            return _frame + "." + _name;
        }

        /// <summary>
        /// GetHashCode override
        /// </summary>
        /// <returns>Hashcode for this FE</returns>
        public override int GetHashCode()
        {
            return _hashCode;
        }

        /// <summary>
        /// Adds a frame element to the sub-FE collection for a relation
        /// </summary>
        /// <param name="frameElement">Frame element to add</param>
        /// <param name="relation">Relation between current and given frame element</param>
        internal void AddSubFrameElement(FrameElement frameElement, Frame.FrameRelation relation)
        {
            _relationSubFrameElements[relation].Add(frameElement);
        }

        /// <summary>
        /// Adds a frame element to the super-FE collection for a relation
        /// </summary>
        /// <param name="frameElement">Frame element to add</param>
        /// <param name="relation">Relation between current and given frame element</param>
        internal void AddSuperFrameElement(FrameElement frameElement, Frame.FrameRelation relation)
        {
            _relationSuperFrameElements[relation].Add(frameElement);
        }

        /// <summary>
        /// Gets a list of related frame element
        /// </summary>
        /// <param name="relation">Type of relation to fetch</param>
        /// <param name="relationDirection">Relation direction</param>
        /// <param name="recursive">Whether or not to recursively get related frame elements</param>        
        /// <returns>List of FEs</returns>
        public FrameElementSet GetRelatedFrameElements(Frame.FrameRelation relation, Frame.FrameRelationDirection relationDirection, bool recursive)
        {
            FrameElementSet relatedFEs = new FrameElementSet();
            GetRelatedFrameElements(relation, relationDirection, recursive, relatedFEs);

            return relatedFEs;
        }

        /// <summary>
        /// Gets list of related frame elements
        /// </summary>
        /// <param name="relation">Type of relation to fetch</param>
        /// <param name="relationDirection">Relation direction</param>
        /// <param name="recursive">Whether or not to recursively get related frame elements</param>
        /// <param name="currentFEs">Current list of related FEs</param>
        private void GetRelatedFrameElements(Frame.FrameRelation relation, Frame.FrameRelationDirection relationDirection, bool recursive, FrameElementSet currentFEs)
        {
            // add sub-FEs
            if (relationDirection == Frame.FrameRelationDirection.Sub || relationDirection == Frame.FrameRelationDirection.Both)
                foreach (FrameElement subFE in _relationSubFrameElements[relation])
                    if (!currentFEs.Contains(subFE))
                    {
                        currentFEs.Add(subFE);

                        // recursively add sub-FEs
                        if (recursive)
                            subFE.GetRelatedFrameElements(relation, relationDirection, recursive, currentFEs);
                    }

            // add super-FEs
            if (relationDirection == Frame.FrameRelationDirection.Super || relationDirection == Frame.FrameRelationDirection.Both)
                foreach (FrameElement superFE in _relationSuperFrameElements[relation])
                    if (!currentFEs.Contains(superFE))
                    {
                        currentFEs.Add(superFE);

                        // recursively add super-FEs
                        if (recursive)
                            superFE.GetRelatedFrameElements(relation, relationDirection, recursive, currentFEs);
                    }
        }

        /// <summary>
        /// Gets the shortest network path from the current frame element to another frame element
        /// </summary>
        /// <param name="destinationFrameElement">Destination frame element</param>
        /// <param name="searchRelations">Relations to search</param>
        /// <param name="searchDirection">Relation direction to search</param>
        /// <param name="maxDepth">Maximum depth to search within the network (i.e., maximum distance destination frame element can be from the current one)</param>
        /// <param name="frameElementPath">Path from this frame element to the destination frame element, or null for no path</param>
        /// <param name="relationPath">Relation path between this frame element and the destination frame element, or null for no path</param>
        /// <returns>True if path exists, false otherwise</returns>
        public bool GetShortestPathTo(FrameElement destinationFrameElement,
                                      Set<Frame.FrameRelation> searchRelations,
                                      Frame.FrameRelationDirection searchDirection,
                                      int maxDepth,
                                      out List<FrameElement> frameElementPath,
                                      out List<Frame.FrameRelation> relationPath)
        {
            frameElementPath = null;
            relationPath = null;

            // breadth-first search originating at the current frame element
            Queue<FrameElement> searchQueue = new Queue<FrameElement>();
            _frameElementSearchBackPointer = null;                        // make sure to null out the source frame element back pointer
            searchQueue.Enqueue(this);

            Set<FrameElement> frameElementsEncountered = new Set<FrameElement>();  // keep track of frame elements we see so we don't enter any cycles
            frameElementsEncountered.Add(this);

            int currentDepth = 0;                // tracks current search depth
            int nodesAtCurrentDepth = 1;         // tracks nodes at current search depth
            int nodesAtCurrentDepthPlusOne = 0;  // tracks nodes at one beyond the current search depth

            while (searchQueue.Count > 0 && currentDepth <= maxDepth)
            {
                FrameElement currentFrameElement = searchQueue.Dequeue();

                // check for destination frame element
                if (currentFrameElement == destinationFrameElement)
                {
                    // create path by following backpointers
                    frameElementPath = new List<FrameElement>();
                    relationPath = new List<Frame.FrameRelation>();
                    while (destinationFrameElement != null)
                    {
                        frameElementPath.Add(destinationFrameElement);

                        // back up to previous frame element
                        FrameElement previousFrameElement = destinationFrameElement.FrameElementSearchBackPointer;

                        // if the previous frame element isn't null, record the relationship
                        if (previousFrameElement != null)
                            relationPath.Add(destinationFrameElement.FrameRelationSearchBackPointer);

                        destinationFrameElement = previousFrameElement;
                    }

                    // reverse paths to be from the current to the destination frame elements
                    frameElementPath.Reverse();
                    relationPath.Reverse();

                    if (frameElementPath[0] != this)
                        throw new Exception("Path should start at current frame element");

                    if (frameElementPath.Count != relationPath.Count + 1)
                        throw new Exception("Path length mismatch between frame elements and relations");

                    if (frameElementPath.Count - 1 > maxDepth)
                        throw new Exception("Exceeded maximum allowed search depth");

                    return true;
                }

                // queue up frame elements related to the current one by any of the given relations
                int nodesAdded = 0;
                foreach (Frame.FrameRelation searchRelation in searchRelations)
                {
                    // add sub-FEs
                    if (searchDirection == Frame.FrameRelationDirection.Sub || searchDirection == Frame.FrameRelationDirection.Both)
                        foreach (FrameElement subFE in currentFrameElement._relationSubFrameElements[searchRelation])
                            if (!frameElementsEncountered.Contains(subFE))
                            {
                                subFE._frameElementSearchBackPointer = currentFrameElement;
                                subFE._frameRelationSearchBackPointer = searchRelation;

                                searchQueue.Enqueue(subFE);
                                frameElementsEncountered.Add(subFE);

                                ++nodesAdded;
                            }

                    // add super-FEs
                    if (searchDirection == Frame.FrameRelationDirection.Super || searchDirection == Frame.FrameRelationDirection.Both)
                        foreach (FrameElement superFE in currentFrameElement._relationSuperFrameElements[searchRelation])
                            if (!frameElementsEncountered.Contains(superFE))
                            {
                                superFE._frameElementSearchBackPointer = currentFrameElement;
                                superFE._frameRelationSearchBackPointer = searchRelation;

                                searchQueue.Enqueue(superFE);
                                frameElementsEncountered.Add(superFE);

                                ++nodesAdded;
                            }
                }

                // all generated search nodes belong in the next depth level
                nodesAtCurrentDepthPlusOne += nodesAdded;

                // if there aren't any nodes left at the current depth level, move to next level out
                if (--nodesAtCurrentDepth == 0)
                {
                    nodesAtCurrentDepth = nodesAtCurrentDepthPlusOne;
                    nodesAtCurrentDepthPlusOne = 0;
                    currentDepth++;
                }
            }

            return false;
        }
    }
}
