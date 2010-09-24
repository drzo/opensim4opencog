using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.IO;
using System.Text.RegularExpressions;

using LAIR.ResourceAPIs.PennBank.TreeBank;

namespace LAIR.ResourceAPIs.PennBank.DiscourseBank
{
    /// <summary>
    /// Provides access to the Penn Discourse TreeBank data
    /// </summary>
    public class DiscourseBankEngine : TreeBankEngine
    {
        #region static members
        /// <summary>
        /// Matches a line of underscores
        /// </summary>
        private Regex _underscoreLineRE = new Regex("^_+$");

        /// <summary>
        /// Matches a header line
        /// </summary>
        private Regex _headerRE = new Regex("^____(?<header>[^_]+)____$");
        #endregion

        private string _dataDirectory;

        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="dataDirectory">Path to the PDTB data directory</param>
        /// <param name="mrgPath">Path to the Penn TreeBank MRG files</param>
        /// <param name="indexDirectory">TreeBank index directory</param>
        public DiscourseBankEngine(string dataDirectory, string mrgPath, string indexDirectory)
            : base(mrgPath, indexDirectory)
        {
            _dataDirectory = dataDirectory;

            if (!Directory.Exists(_dataDirectory))
                throw new DirectoryNotFoundException("PDTB data directory does not exist:  " + _dataDirectory);
        }

        /// <summary>
        /// Gets whether or not the PDTB contains annotations for a given MRG file
        /// </summary>
        /// <param name="mrgFile">MRG file to check</param>
        /// <returns>True if annotations are present, false otherwise</returns>
        public bool Contains(string mrgFile)
        {
            return GetDiscourseFilePath(mrgFile) != null;
        }

        /// <summary>
        /// Gets all relations within a MRG file
        /// </summary>
        /// <param name="mrgFile">MRG file to get relations within</param>
        /// <returns>List of PDTB relations</returns>
        public List<DiscourseBankRelation> GetRelations(string mrgFile)
        {
            // make sure the MRG file exists
            if (!File.Exists(mrgFile))
                throw new FileNotFoundException("Invalid MRG file:  " + mrgFile);

            List<DiscourseBankRelation> relations = new List<DiscourseBankRelation>();

            // scan PDTB file
            string discourseBankPath = GetDiscourseFilePath(mrgFile);
            if (!File.Exists(discourseBankPath))
                return relations;

            StreamReader dataFile = new StreamReader(discourseBankPath);
            string line;
            while ((line = dataFile.ReadLine()) != null)
            {
                // make sure the first line is an underscore line
                if (!_underscoreLineRE.Match(line).Success)
                    throw new Exception("Expected an underscore line");

                // get section lines
                Dictionary<string, List<string>> sectionLines = new Dictionary<string, List<string>>();
                string currentSection = null;
                string relationType = null;
                bool firstSection = true;
                while (!_underscoreLineRE.Match(line = dataFile.ReadLine()).Success)
                {
                    // check if we've switched sections
                    Match headerMatch = _headerRE.Match(line);
                    if (headerMatch.Success)
                    {
                        currentSection = headerMatch.Groups["header"].Value;
                        sectionLines.Add(currentSection, new List<string>());

                        // if this is the first section, record the relation type
                        if (firstSection)
                        {
                            relationType = currentSection;
                            firstSection = false;
                        }
                    }

                    // store section lines
                    sectionLines[currentSection].Add(line);
                }

                if (relationType == null)
                    throw new NullReferenceException("Invalid relation type");

                // get the semantic class for explicit, implicit, and altlex relations
                string semanticClass = null;
                if (relationType == "Explicit" || relationType == "Implicit" || relationType == "AltLex")
                {
                    // get column of semantic class depending on relation type
                    int column = -1;
                    if (relationType == "Explicit")
                        column = 1;
                    else if (relationType == "Implicit")
                        column = 1;
                    else if (relationType == "AltLex")
                        column = 0;
                    else
                        throw new Exception("Invalid relation type");

                    semanticClass = sectionLines[relationType].Last<string>().Split(',')[column].Trim();
                }

                // get Arg1 and Arg2 nodes
                List<TreeBankNode> arg1Nodes = new List<TreeBankNode>();
                foreach (List<int> gornAddress in GetGornAddresses(sectionLines["Arg1"][2]))
                    arg1Nodes.Add(GetParseTreeNode(mrgFile, gornAddress.ToArray()));

                List<TreeBankNode> arg2Nodes = new List<TreeBankNode>();
                foreach (List<int> gornAddress in GetGornAddresses(sectionLines["Arg2"][2]))
                    arg2Nodes.Add(GetParseTreeNode(mrgFile, gornAddress.ToArray()));

                // create relation
                DiscourseBankRelation relation = new DiscourseBankRelation(relationType, semanticClass, arg1Nodes, arg2Nodes);
                relations.Add(relation);
            }

            return relations;
        }

        /// <summary>
        /// Gets the path to the PDTB data for for a given MRG file
        /// </summary>
        /// <param name="mrgFile">MRG file to get PDTB data file path for</param>
        /// <returns>PDTB file path, or null if none exists</returns>
        public string GetDiscourseFilePath(string mrgFile)
        {
            // make sure we've only got the file name
            mrgFile = Path.GetFileName(mrgFile);

            string dataFileName = Path.GetFileNameWithoutExtension(mrgFile) + ".pdtb";
            string treeBankSection = GetSectionNumber(mrgFile).ToString().PadLeft(2, '0');
            string dataFileDirectory = Path.Combine(_dataDirectory, treeBankSection);
            string dataPath = Path.Combine(dataFileDirectory, dataFileName);

            if (File.Exists(dataPath))
                return dataPath;
            else
                return null;
        }

        /// <summary>
        /// Gets Gorn addresses from the string representation used in the PDTB
        /// </summary>
        /// <param name="gornAddressesString">Gorn addresses string</param>
        /// <returns>List of Gorn addresses</returns>
        private List<List<int>> GetGornAddresses(string gornAddressesString)
        {
            List<List<int>> gornAddresses = new List<List<int>>();

            // split on semicolons first
            string[] gornStrings = gornAddressesString.Split(';');

            // add each gorn address
            foreach (string gornString in gornStrings)
            {
                // split indexes on comma
                string[] gornStringIndexes = gornString.Split(',');
                List<int> gornAddress = new List<int>();
                foreach (string gornStringIndex in gornStringIndexes)
                    gornAddress.Add(int.Parse(gornStringIndex));

                gornAddresses.Add(gornAddress);
            }

            // make sure we got at least one Gorn address
            if (gornAddresses.Count == 0)
                throw new Exception("No Gorn addresses found");

            return gornAddresses;
        }
    }
}
