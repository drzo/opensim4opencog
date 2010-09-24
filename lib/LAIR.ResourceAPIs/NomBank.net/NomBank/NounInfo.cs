using System;
using System.Collections.Generic;
using System.Text;
using System.IO;
using System.Text.RegularExpressions;

using LAIR.ResourceAPIs.PennBank.TreeBank;
using LAIR.Extensions;

namespace LAIR.ResourceAPIs.NomBank
{
    /// <summary>
    /// Represents a single line in the NomBank propositions file
    /// </summary>
    public class NounInfo
    {
        #region static members
        /// <summary>
        /// RE for prop entries
        /// </summary>
        private static Regex _propLineRE = new Regex(@"^(?<file_name>[^\s]+)\s" +
                                                     @"(?<sent_num>[^\s]+)\s" +
                                                     @"(?<leaf_num>[^\s]+)\s" +
                                                     @"(?<noun>[^\s]+)\s" +
                                                     @"(?<roleset>[^\s]+)\s" +
                                                     @"(?<labelLocations>.+)$");
        #endregion

        private string _noun;
        private string _file;
        private int _sentenceNumber;
        private int _leafNumber;
        private string _labeledNodeLocations;
        private int _roleSetID;
        private Frame _frame;

        /// <summary>
        /// Gets or sets the Frame
        /// </summary>
        public Frame Frame
        {
            get { return _frame; }
            set { _frame = value; }
        }

        /// <summary>
        /// Gets or sets the role set ID
        /// </summary>
        public int RoleSetId
        {
            get { return _roleSetID; }
            set { _roleSetID = value; }
        }

        /// <summary>
        /// Gets or sets the labeled node locations, as specified in the NomBank propositions file
        /// </summary>
        public string LabeledNodeLocations
        {
            get { return _labeledNodeLocations; }
            set { _labeledNodeLocations = value; }
        }

        /// <summary>
        /// Gets or sets the noun
        /// </summary>
        public string Noun
        {
            get { return _noun; }
            set { _noun = value; }
        }

        /// <summary>
        /// Gets or sets the MRG file that this entry refers to
        /// </summary>
        public string File
        {
            get { return _file; }
            set { _file = value; }
        }

        /// <summary>
        /// Gets or sets the TreeBank sentence number within the MRG file
        /// </summary>
        public int SentenceNumber
        {
            get { return _sentenceNumber; }
            set { _sentenceNumber = value; }
        }

        /// <summary>
        /// Gets or sets the predicating leaf number within the sentence
        /// </summary>
        public int LeafNumber
        {
            get { return _leafNumber; }
            set { _leafNumber = value; }
        }

        /// <summary>
        /// Constructor
        /// </summary>
        public NounInfo()
        {
            _roleSetID = -1;
        }

        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="propLine">Prop entry line</param>
        public NounInfo(string propLine)
            : this()
        {
            Match propLineMatch = _propLineRE.Match(propLine);
            if (!propLineMatch.Success)
                throw new Exception("Invalid proposition line");

            _file = propLineMatch.Groups["file_name"].Value.Replace('/', Path.DirectorySeparatorChar);  // use environment-specific path separators
            _sentenceNumber = int.Parse(propLineMatch.Groups["sent_num"].Value);
            _leafNumber = int.Parse(propLineMatch.Groups["leaf_num"].Value);
            _noun = propLineMatch.Groups["noun"].Value;
            _roleSetID = int.Parse(propLineMatch.Groups["roleset"].Value);
            _labeledNodeLocations = propLineMatch.Groups["labelLocations"].Value;
        }

        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="noun">Noun</param>
        /// <param name="file">Annotation file</param>
        /// <param name="sentenceNumber">Sentence wichin annotation file</param>
        /// <param name="leafNumber">Leaf within sentence</param>
        /// <param name="roleSetID">Role set ID</param>
        /// <param name="labeledNodeLocations">Labeled node locations</param>
        public NounInfo(string noun, string file, int sentenceNumber, int leafNumber,
                        int roleSetID, string labeledNodeLocations)
            : this()
        {
            _noun = noun;
            _file = file;
            _sentenceNumber = sentenceNumber;
            _leafNumber = leafNumber;
            _roleSetID = roleSetID;
            _labeledNodeLocations = labeledNodeLocations;
        }

        /// <summary>
        /// Gets the prop entry for this noun info
        /// </summary>
        /// <param name="treeBankPath">Path to TreeBank upon which this information is based. Pass null to use the entire file path
        /// contained in this information.</param>
        /// <returns>Prop entry line</returns>
        public string GetPropEntry(string treeBankPath)
        {
            // use path relative to underlying treebank, if one is given
            string propEntry = (treeBankPath != null ? treeBankPath.GetRelativePathTo(_file) : _file) + " " + _sentenceNumber + " " + _leafNumber + " " + _noun + " " + _roleSetID + " " + _labeledNodeLocations;

            return propEntry;
        }
    }
}
