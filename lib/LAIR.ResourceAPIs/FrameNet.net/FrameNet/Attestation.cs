using System;
using System.Collections.Generic;
using System.Text;

namespace LAIR.ResourceAPIs.FrameNet
{
    /// <summary>
    /// Represents a single attestation for a frame
    /// </summary>
    public class Attestation
    {
        private string _sentence;
        private Dictionary<FrameElement, List<AnnotatedSpan>> _frameElementBindings;
        private List<AnnotatedSpan> _targets;

        /// <summary>
        /// Gets or sets the frame element bindings for this attestation
        /// </summary>
        public Dictionary<FrameElement, List<AnnotatedSpan>> FrameElementBindings
        {
            get { return _frameElementBindings; }
            set { _frameElementBindings = value; }
        }

        /// <summary>
        /// Gets or sets the targets for this attestation
        /// </summary>
        public List<AnnotatedSpan> Targets
        {
            get { return _targets; }
            set { _targets = value; }
        }

        /// <summary>
        /// Gets or sets the sentence
        /// </summary>
        public string Sentence
        {
            get { return _sentence; }
            set { _sentence = value; }
        }

        /// <summary>
        /// Constructor
        /// </summary>
        public Attestation()
        {
            _frameElementBindings = new Dictionary<FrameElement, List<AnnotatedSpan>>();
            _targets = new List<AnnotatedSpan>();
        }

        /// <summary>
        /// Gets nicely formatted string for current attestation
        /// </summary>
        /// <returns></returns>
        public override string ToString()
        {
            StringBuilder attestation = new StringBuilder();
            attestation.AppendLine(_sentence);

            // add FE bindings
            foreach (FrameElement fe in _frameElementBindings.Keys)
            {
                attestation.AppendLine("\t\"" + fe.Name + "\" bindings:  ");
                foreach (AnnotatedSpan aSpan in _frameElementBindings[fe])
                    attestation.AppendLine("\t\t" + aSpan);
            }

            // add target
            foreach (AnnotatedSpan aSpan in _targets)
                attestation.AppendLine("\tTarget:  " + aSpan);

            return attestation.ToString().Trim();
        }
    }
}
