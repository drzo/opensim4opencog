using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

using LAIR.Collections.Generic;
using LAIR.Extensions;

namespace LAIR.ResourceAPIs.FrameNet
{
    /// <summary>
    /// Represents a set of frame elements
    /// </summary>
    public class FrameElementSet
    {
        private Set<FrameElement> _frameElements;
        private Dictionary<int, FrameElement> _idFrameElement;
        private Dictionary<string, Set<FrameElement>> _nameFrameElements;

        /// <summary>
        /// Gets number of frame elements in set
        /// </summary>
        public int Count
        {
            get { return _frameElements.Count; }
        }

        /// <summary>
        /// Constructor
        /// </summary>
        public FrameElementSet()
        {
            _frameElements = new Set<FrameElement>();
            _idFrameElement = new Dictionary<int, FrameElement>();
            _nameFrameElements = new Dictionary<string, Set<FrameElement>>();
        }

        /// <summary>
        /// Adds a frame element to this set
        /// </summary>
        /// <param name="frameElement">Frame element to add</param>
        public void Add(FrameElement frameElement)
        {
            _frameElements.Add(frameElement);
            _idFrameElement.Add(frameElement.ID, frameElement);

            string lowerName = frameElement.Name.ToLower();
            _nameFrameElements.EnsureContainsKey(lowerName, typeof(Set<FrameElement>));
            _nameFrameElements[lowerName].Add(frameElement);
        }

        /// <summary>
        /// Removes a frae element from this set
        /// </summary>
        /// <param name="frameElement">Frame element to remove</param>
        public void Remove(FrameElement frameElement)
        {
            _frameElements.Remove(frameElement);
            _idFrameElement.Remove(frameElement.ID);
            _nameFrameElements[frameElement.Name.ToLower()].Remove(frameElement);
        }
        
        /// <summary>
        /// Gets a frame element in the set by its ID
        /// </summary>
        /// <param name="id">ID of FE to get</param>
        /// <returns>FE with specified ID</returns>
        public FrameElement Get(int id)
        {
            return _idFrameElement[id];
        }

        /// <summary>
        /// Gets a frame element in the set by its name
        /// </summary>
        /// <param name="name">Name of frame element to get</param>
        /// <returns>FE with specified name</returns>
        public FrameElement Get(string name)
        {
            name = name.ToLower();

            if (_nameFrameElements[name].Count != 1)
                throw new Exception("Multiple frame elements present!");

            return _nameFrameElements[name].First();
        }

        /// <summary>
        /// Checks whether or not this set contains a frame element
        /// </summary>
        /// <param name="name">Name of frame element to check for</param>
        /// <returns>True if frame element is present, false otherwise</returns>
        public bool Contains(string name)
        {
            return _nameFrameElements.ContainsKey(name.ToLower());
        }

        /// <summary>
        /// Checks for a frame element
        /// </summary>
        /// <param name="frameElement">Frame element to check for</param>
        /// <returns>True if frame element is present and false otherwise</returns>
        public bool Contains(FrameElement frameElement)
        {
            return _frameElements.Contains(frameElement);
        }

        /// <summary>
        /// Checks for a frame element
        /// </summary>
        /// <param name="id">ID of frame element to check for</param>
        /// <returns>True if frame element is present and false otherwise</returns>
        public bool Contains(int id)
        {
            return _idFrameElement.ContainsKey(id);
        }

        /// <summary>
        /// Gets enumerator over frame elements
        /// </summary>
        /// <returns></returns>
        public IEnumerator<FrameElement> GetEnumerator()
        {
            return _frameElements.GetEnumerator();
        }

        /// <summary>
        /// Gets number of frame elements in set
        /// </summary>
        /// <returns></returns>
        public override string ToString()
        {
            return _frameElements.ToString();
        }
    }
}
