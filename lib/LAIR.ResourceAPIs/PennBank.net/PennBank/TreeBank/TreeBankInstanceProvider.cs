using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using LAIR.MachineLearning;
using LAIR.Collections.Generic;

namespace LAIR.ResourceAPIs.PennBank.TreeBank
{
    /// <summary>
    /// Provides training instances (parse tree nodes) from the TreeBank corpus
    /// </summary>
    public abstract class TreeBankInstanceProvider : TrainingInstanceProvider
    {
        private TreeBankEngine _treeBankEngine;
        private List<string>.Enumerator _fileEnum;
        private Set<int> _sections;

        /// <summary>
        /// Gets the TreeBank engine used in this provider
        /// </summary>
        public TreeBankEngine TreeBankEngine
        {
            get { return _treeBankEngine; }
        }

        /// <summary>
        /// Gets sections this provider draws nodes from
        /// </summary>
        public Set<int> Sections
        {
            get { return _sections; }
        }

        /// <summary>
        /// Gets the current MRG file
        /// </summary>
        protected string CurrentMrgFile
        {
            get { return _fileEnum.Current; }
        }

        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="treeBankEngine">TreeBank engine to draw training nodes from</param>
        /// <param name="instanceFilter">Instance filter to apply to training instances</param>
        /// <param name="sections">TreeBank sections to draw instances from (null for all sections)</param>
        public TreeBankInstanceProvider(TreeBankEngine treeBankEngine, InstanceFilterDelegate instanceFilter, Set<int> sections)
            : base(instanceFilter)
        {
            _treeBankEngine = treeBankEngine;
            if (_treeBankEngine == null)
                throw new Exception("TreeBank engine cannot be null");

            _sections = sections;
            if (_sections != null && _sections.Count == 0)
                throw new Exception("It makes no sense to restrict TreeBank training sections to the empty set");
        }

        /// <summary>
        /// Starts the instance provider before the first MRG file
        /// </summary>
        public override void Start()
        {
            // set file enum to first valid .mrg file
            _fileEnum = TreeBankEngine.IndexedMrgFiles.GetEnumerator();
        }

        /// <summary>
        /// Moves a MRG file enumerator to the next valid MRG file based on the training section restriction (if any)
        /// </summary>
        /// <returns>True if valid MRG file was found, false otherwise</returns>
        protected bool MoveToNextValidMrgFile()
        {
            // try moving to next file
            if (!_fileEnum.MoveNext())
                return false;

            // impose section restriction if there is one
            if (_sections != null)
                while (!_sections.Contains(TreeBankEngine.GetSectionNumber(_fileEnum.Current)))
                    if (!_fileEnum.MoveNext())
                        return false;

            return true;
        }
    }
}
