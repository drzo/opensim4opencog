using System;
using System.Collections.Generic;
using System.Text;
using LAIR.Collections.Generic;

namespace LAIR.ResourceAPIs.FrameNet
{
    /// <summary>
    /// Represents a set of frames
    /// </summary>
    public class FrameSet
    {
        private Set<Frame> _frames;
        private Dictionary<string, Frame> _nameFrames;

        /// <summary>
        /// Gets number of frames in set
        /// </summary>
        public int Count
        {
            get { return _frames.Count; }
        }

        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="throwExceptionOnDuplicateAdd">Whether or not to throw exceptions when duplicate frames are added</param>
        public FrameSet(bool throwExceptionOnDuplicateAdd)
        {
            _frames = new Set<Frame>(throwExceptionOnDuplicateAdd);
            _nameFrames = new Dictionary<string, Frame>();
        }

        /// <summary>
        /// Adds a frame to the set
        /// </summary>
        /// <param name="frame">Frame to add</param>
        public void Add(Frame frame)
        {
            if (_frames.Add(frame))
                _nameFrames.Add(frame.Name, frame);
        }

        /// <summary>
        /// Checks for a frame
        /// </summary>
        /// <param name="frame">Frame to check for</param>
        /// <returns>True if frame is present and false otherwise</returns>
        public bool Contains(Frame frame)
        {
            return _frames.Contains(frame);
        }

        /// <summary>
        /// Gets a frame by its name
        /// </summary>
        /// <param name="name">Name of frame to get</param>
        /// <returns>Frame</returns>
        public Frame Get(string name)
        {
            return _nameFrames[name];
        }

        /// <summary>
        /// Gets enumerator over frames
        /// </summary>
        /// <returns></returns>
        public IEnumerator<Frame> GetEnumerator()
        {
            return _frames.GetEnumerator();
        }

        /// <summary>
        /// Gets array of frames
        /// </summary>
        /// <returns>Array of frames</returns>
        public Frame[] ToArray()
        {
            return _frames.ToArray();
        }

        /// <summary>
        /// Gets number of frames in set
        /// </summary>
        /// <returns></returns>
        public override string ToString()
        {
            return _frames.ToString();
        }
    }
}
