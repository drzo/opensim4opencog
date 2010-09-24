using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Text;
using System.IO;
using System.Text.RegularExpressions;
using LAIR.CommonPort;
using LAIR.ResourceAPIs.PennBank.TreeBank;
using LAIR.XML;
using LAIR.Collections.Generic;
using LAIR.Extensions;

namespace LAIR.ResourceAPIs.PennBank.PropBank
{
    /// <summary>
    /// Provides access to PropBank layer of information on top of the TreeBank layer.
    /// </summary>
    public class PropBankEngine : TreeBankEngine
    {
        #region static members
        /// <summary>
        /// Gets relation nodes given a root and a location series label
        /// </summary>
        /// <param name="root">Root node to get nodes from</param>
        /// <param name="locationLabel">Location series label</param>
        /// <param name="nodeCollection">Collection to add nodes to</param>
        public static void AddNodesToCollection(TreeBankNode root, string locationLabel, LabeledNodeCollection nodeCollection)
        {
            // split on * then on ,
            string[] corefLocations = locationLabel.Split('*');
            foreach (string corefLocation in corefLocations)
            {
                if (corefLocation.Contains(","))
                    nodeCollection.AddSplitNode(GetSplitNode(root, corefLocation));
                else
                    nodeCollection.AddSingleNode(root.GetNode(corefLocation));
            }
        }

        /// <summary>
        /// Gets set of split nodes from a root node
        /// </summary>
        /// <param name="root">Root node to get split nodes from</param>
        /// <param name="splitNodeLocations">Node locations that form the split node</param>
        /// <returns>Set of split nodes</returns>
        private static List<TreeBankNode> GetSplitNode(TreeBankNode root, string splitNodeLocations)
        {
            List<TreeBankNode> splitNode = new List<TreeBankNode>();

            foreach (string nodeLocation in splitNodeLocations.Split(','))
                splitNode.Add(root.GetNode(nodeLocation));

            if (splitNode.Count <= 1)
                throw new Exception("Invalid split node locations");

            return splitNode;
        }

        /// <summary>
        /// Builds the Frame index
        /// </summary>
        /// <param name="framesDirectory">Directory containing PropBank frame XML files</param>
        /// <returns>Frame index - a map from verb lemma to corresponding frame</returns>
        private static Dictionary<string, Frame> BuildFrameIndex(string framesDirectory)
        {
            if (!Directory.Exists(framesDirectory))
                throw new Exception("Invalid frame directory:  \"" + framesDirectory + "\"");

            // build index
            Dictionary<string, Frame> frameIndex = new Dictionary<string, Frame>();

            // get each frame file
            foreach (string frameFilePath in Directory.GetFiles(framesDirectory))
            {
                Frame currFrame = null;

                // read entire file and create XML parser
                StreamReader frameFile = new StreamReader(frameFilePath);
                string frameXML = frameFile.ReadToEnd();
                frameFile.Close();
                var frameP = new LAIR.CommonPort.CommonXmlParser(frameXML);

                // get role sets
                string roleSetXML;
                while ((roleSetXML = frameP.OuterXML("roleset")) != null)
                {
                    var roleSetP = new LAIR.CommonPort.CommonXmlParser(roleSetXML);

                    // get role set ID string in verb.id format
                    string roleSetIdStr = roleSetP.AttributeValue("roleset", "id").Trim();
                    int dotIndex = roleSetIdStr.IndexOf('.');

                    // get role set verb
                    string roleSetVerb = roleSetIdStr.Substring(0, dotIndex);

                    // if this is the first role set, create the frame
                    if (currFrame == null)
                        currFrame = new Frame(roleSetVerb);
                    // all role sets must use the same verb
                    else if (roleSetVerb != currFrame.Verb)
                        throw new Exception("Role set verb mismatch");

                    // get role set ID/name and create role set
                    int roleSetID = int.Parse(roleSetIdStr.Substring(dotIndex + 1));
                    string roleSetName = roleSetP.AttributeValue("roleset", "name");
                    RoleSet roleSet = new RoleSet(roleSetID, roleSetName);

                    // get roles
                    string roleXML;
                    while ((roleXML = roleSetP.OuterXML("role")) != null)
                    {
                        var roleP = new LAIR.CommonPort.CommonXmlParser(roleXML);

                        string description = roleP.AttributeValue("role", "descr");
                        string roleNumber = roleP.AttributeValue("role", "n").ToLower();

                        // skip modifier and agentive modifier
                        if (roleNumber == "m" || roleNumber == "a")
                            continue;

                        Role role = new Role(description, int.Parse(roleNumber));
                        roleSet.Add(role);
                    }

                    // add role set to frame
                    currFrame.AddRoleSet(roleSet);
                }

                frameIndex.Add(currFrame.Verb, currFrame);
            }

            return frameIndex;
        }
        #endregion

        private Dictionary<string, long> _verbInfoFilePosition;
        private Dictionary<string, Dictionary<int, long>> _mrgSentInfoFilePosition;
        private Dictionary<string, Set<string>> _verbMorphs;
        private Dictionary<string, string> _morphVerb;
        private Dictionary<string, Frame> _verbFrame;

        /// <summary>
        /// Gets the mapping from base verbs to their morphological variants
        /// </summary>
        public Dictionary<string, Set<string>> VerbMorphs
        {
            get { return _verbMorphs; }
        }

        /// <summary>
        /// Gets the mapping from morphological variants to base verbs
        /// </summary>
        public Dictionary<string, string> MorphVerb
        {
            get { return _morphVerb; }
        }

        /// <summary>
        /// Gets a list of all verbs in the database
        /// </summary>
        public ReadOnlyCollection<string> AllVerbs
        {
            get { return new ReadOnlyCollection<string>(new List<string>(_verbInfoFilePosition.Keys)); }
        }

        #region paths
        /// <summary>
        /// Gets the path to the morphological variant index
        /// </summary>
        private string MorphMapIndexPath
        {
            get { return Path.Combine(IndexDirectory, "morph_map"); }
        }

        /// <summary>
        /// Gets the path to the VerbInfo positions file, which indexes a verb to its list of VerbInfo objects within the index file
        /// </summary>
        private string VerbInfoFilePositionPath
        {
            get { return VerbInfoPath + "_positions"; }
        }

        /// <summary>
        /// Gets the path to the VerbInfo index
        /// </summary>
        private string VerbInfoPath
        {
            get { return Path.Combine(IndexDirectory, "verb_info"); }
        }

        /// <summary>
        /// Gets the path to the MRG file/sentence number VerbInfo index
        /// </summary>
        private string MrgSentenceInfoPath
        {
            get { return Path.Combine(IndexDirectory, "mrg_sentence_info"); }
        }

        /// <summary>
        /// Gets the path to the MRG file index, which maps a MRG file to its sentence number/VerbInfo index
        /// </summary>
        private string MrgSentenceInfoFilePositionsPath
        {
            get { return MrgSentenceInfoPath + "_positions"; }
        }
        #endregion

        #region constructors
        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="mrgPath">Path to TreeBank directory to search recursively for .mrg files.</param>
        /// <param name="propFilePath">Path to prop.txt file in the PropBank distribution</param>
        /// <param name="framesPath">Path to the "frames" subdirectory of the PropBank distribution</param>
        /// <param name="indexDirectory">Path to index directory, where all indexing information is stored</param>
        public PropBankEngine(string mrgPath,
                              string propFilePath,
                              string framesPath,
                              string indexDirectory)
            : base(mrgPath, indexDirectory)
        {
            Construct(propFilePath, framesPath);
        }

        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="mrgPath">Path to TreeBank directory to search recursively for .mrg files.</param>
        /// <param name="propBankPath">Path to PropBank directory. This directory must contain the "prop.txt" file as well
        /// as the "frames" directory.</param>
        /// <param name="indexDirectory">Path to index directory, where all indexing information is stored</param>
        public PropBankEngine(string mrgPath, string propBankPath, string indexDirectory)
            : this(mrgPath, Path.Combine(propBankPath, "prop.txt"), Path.Combine(propBankPath, "frames"), indexDirectory)
        {
        }

        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="mrgPath">Path to TreeBank directory to search recursively for .mrg files.</param>
        /// <param name="propFilePath">Path to prop.txt file in the PropBank distribution</param>
        /// <param name="framesPath">Path to the "frames" subdirectory of the PropBank distribution</param>
        /// <param name="indexDirectory">Path to index directory, where all indexing information is stored</param>
        /// <param name="mrgFileNameRE">Regular expression for MRG file name. See MrgFileNameRE property for details.</param>
        public PropBankEngine(string mrgPath,
                              string propFilePath,
                              string framesPath,
                              string indexDirectory,
                              Regex mrgFileNameRE)
            : base(mrgPath, indexDirectory, mrgFileNameRE)
        {
            Construct(propFilePath, framesPath);
        }

        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="mrgPath">Path to TreeBank directory to search recursively for .mrg files.</param>
        /// <param name="propBankPath">Path to PropBank directory. This directory must contain the "prop.txt" file as well
        /// as the "frames" directory.</param>
        /// <param name="indexDirectory">Path to index directory, where all indexing information is stored</param>
        /// <param name="mrgFileNameRE">Regular expression for MRG file name. See MrgFileNameRE property for details.</param>
        public PropBankEngine(string mrgPath, string propBankPath, string indexDirectory, Regex mrgFileNameRE)
            : this(mrgPath, Path.Combine(propBankPath, "prop.txt"), Path.Combine(propBankPath, "frames"), indexDirectory, mrgFileNameRE)
        {
        }

        /// <summary>
        /// Constructs engine
        /// </summary>
        /// <param name="propFilePath">Path to prop.txt file in the PropBank distribution</param>
        /// <param name="framesPath">Path to the "frames" subdirectory of the PropBank distribution</param>
        private void Construct(string propFilePath, string framesPath)
        {
            // build frame index
            _verbFrame = BuildFrameIndex(framesPath);

            // build verb info index
            LoadProps(propFilePath);

            // build morphology map
            BuildMorphMap();
        }
        #endregion

        #region loading
        /// <summary>
        /// Loads the propositions file
        /// </summary>
        /// <param name="propsPath">Path to prop.txt file</param>
        private void LoadProps(string propsPath)
        {
            if (!File.Exists(propsPath))
                throw new Exception("Invalid PropBank propositions file:  \"" + propsPath + "\"");

            // reuse existing index files if present
            if (File.Exists(VerbInfoPath) &&
                File.Exists(VerbInfoFilePositionPath) &&
                File.Exists(MrgSentenceInfoPath) &&
                File.Exists(MrgSentenceInfoFilePositionsPath))
            {
                // load verb info positions
                _verbInfoFilePosition = new Dictionary<string, long>();
                StreamReader positionsFile = new StreamReader(VerbInfoFilePositionPath);
                string line;
                while ((line = positionsFile.ReadLine()) != null)
                {
                    // format:  position verb
                    int spaceLoc = line.IndexOf(' ');
                    _verbInfoFilePosition.Add(line.Substring(spaceLoc + 1), long.Parse(line.Substring(0, spaceLoc)));
                }
                positionsFile.Close();

                // load mrg-sentence verb info positions
                _mrgSentInfoFilePosition = new Dictionary<string, Dictionary<int, long>>();
                positionsFile = new StreamReader(MrgSentenceInfoFilePositionsPath);
                while ((line = positionsFile.ReadLine()) != null)
                {
                    // each line lists a MRG file then a series of pipe-delimited sentence-position pairs...get MRG file first
                    int pipeLoc = line.IndexOf('|');
                    string mrgFile = line.Substring(0, pipeLoc);
                    _mrgSentInfoFilePosition.Add(mrgFile, new Dictionary<int, long>());

                    // get sentence-position pairs
                    while (pipeLoc != line.Length - 1)
                    {
                        // find next pipe and space
                        int nextPipe = line.IndexOf('|', pipeLoc + 1);
                        int spaceLoc = line.IndexOf(' ', pipeLoc + 1);

                        // get sentence and position
                        int sent = int.Parse(line.Substring(pipeLoc + 1, spaceLoc - pipeLoc - 1));
                        long position = long.Parse(line.Substring(spaceLoc + 1, nextPipe - spaceLoc - 1));

                        // add to index
                        _mrgSentInfoFilePosition[mrgFile].Add(sent, position);

                        pipeLoc = nextPipe;
                    }
                }
                positionsFile.Close();

                return;
            }

            // maps each verb to a list of VerbInfo objects, each of which stores an annotation instance
            Dictionary<string, List<VerbInfo>> verbInfo = new Dictionary<string, List<VerbInfo>>();

            // maps each mrg file and sentence number to a list of VerbInfo objects for that sentence
            Dictionary<string, Dictionary<int, List<VerbInfo>>> mrgSentInfo = new Dictionary<string, Dictionary<int, List<VerbInfo>>>();

            // read propositions file
            StreamReader propFile = new StreamReader(propsPath);
            string propLine;
            while ((propLine = propFile.ReadLine()) != null)
            {
                VerbInfo vi = new VerbInfo(propLine);

                // add to mapping from verbs to their information
                verbInfo.EnsureContainsKey(vi.Verb, typeof(List<VerbInfo>));
                verbInfo[vi.Verb].Add(vi);

                // add to mapping from file-sentence pairs to their information
                mrgSentInfo.EnsureContainsKey(vi.File, typeof(Dictionary<int, List<VerbInfo>>));
                mrgSentInfo[vi.File].EnsureContainsKey(vi.SentenceNumber, typeof(List<VerbInfo>));
                mrgSentInfo[vi.File][vi.SentenceNumber].Add(vi);
            }

            propFile.Close();

            // write verb index to disk and record file positions of verb info lists
            _verbInfoFilePosition = new Dictionary<string, long>();
            FileStream saveStream = new FileStream(VerbInfoPath, FileMode.Create);
            foreach (string verb in verbInfo.Keys)
            {
                // save position of VerbInfo list
                _verbInfoFilePosition.Add(verb, saveStream.Position);

                WriteVerbInfoList(verbInfo[verb], saveStream);
            }
            saveStream.Close();

            // save file positions
            StreamWriter verbInfoPositionFile = new StreamWriter(VerbInfoFilePositionPath);
            foreach (string verb in _verbInfoFilePosition.Keys)
                verbInfoPositionFile.WriteLine(_verbInfoFilePosition[verb] + " " + verb);
            verbInfoPositionFile.Close();

            // save mrg-sentence info
            _mrgSentInfoFilePosition = new Dictionary<string, Dictionary<int, long>>();
            saveStream = new FileStream(MrgSentenceInfoPath, FileMode.Create);
            foreach (string mrgFile in mrgSentInfo.Keys)
            {
                // add each sentence, recording position
                Dictionary<int, long> sentInfoPosition = new Dictionary<int, long>();
                foreach (int sentNum in mrgSentInfo[mrgFile].Keys)
                {
                    // add index of sentence to file position
                    sentInfoPosition.Add(sentNum, saveStream.Position);

                    // write VerbInfo list for sentence
                    WriteVerbInfoList(mrgSentInfo[mrgFile][sentNum], saveStream);
                }

                _mrgSentInfoFilePosition.Add(mrgFile, sentInfoPosition);
            }
            saveStream.Close();

            // save file positions for MRG file index
            StreamWriter mrgSentInfoPositionsFile = new StreamWriter(MrgSentenceInfoFilePositionsPath);
            foreach (string mrgFile in _mrgSentInfoFilePosition.Keys)
            {
                mrgSentInfoPositionsFile.Write(mrgFile);
                foreach (int sent in _mrgSentInfoFilePosition[mrgFile].Keys)
                    mrgSentInfoPositionsFile.Write("|" + sent + " " + _mrgSentInfoFilePosition[mrgFile][sent]);

                mrgSentInfoPositionsFile.WriteLine("|");
            }
            mrgSentInfoPositionsFile.Close();
        }

        /// <summary>
        /// Writes a list of VerbInfo to a stream
        /// </summary>
        /// <param name="list">List of VerbInfo to write</param>
        /// <param name="stream">Stream to write to</param>
        private void WriteVerbInfoList(List<VerbInfo> list, FileStream stream)
        {
            byte[] bytes;
            foreach (VerbInfo vi in list)
            {
                bytes = Encoding.UTF8.GetBytes("|" + vi.GetPropEntry(null));
                stream.Write(bytes, 0, bytes.Length);
            }

            bytes = Encoding.UTF8.GetBytes("|" + Environment.NewLine);
            stream.Write(bytes, 0, bytes.Length);
        }

        /// <summary>
        /// Builds the mapping from verbs to their morphological variants. Variants are identified by looking
        /// at the marked predicates throughout PropBank. For example, the verb "join" will have many different surface
        /// realizations in the TreeBank:  "joins", "joined", etc.
        /// </summary>
        private void BuildMorphMap()
        {
            if (File.Exists(MorphMapIndexPath))
            {
                // load verb morphs from saved file
                StreamReader file = new StreamReader(MorphMapIndexPath);
                _verbMorphs = new Dictionary<string, Set<string>>(int.Parse(file.ReadLine()));
                string line;
                while (file.TryReadLine(out line))
                    _verbMorphs.Add(line, new Set<string>(file.ReadLine().Split('|')));
            }
            else
            {
                // process all verbs
                _verbMorphs = new Dictionary<string, Set<string>>();
                foreach (string verb in AllVerbs)
                {
                    // get each predicate tree, checking for morphological variants of the verb
                    Set<string> variants = new Set<string>(false);
                    foreach (VerbInfo vi in GetVerbInfo(verb))
                    {
                        PropBankNode predicateTree = GetPropBankTree(vi);

                        // get variant
                        StringBuilder variantBuilder = new StringBuilder();
                        foreach (PropBankNode predNode in predicateTree.PredicateNodes)
                            variantBuilder.Append((variantBuilder.Length > 0 ? " " : "") + predNode.SurfaceText);

                        // normalize case
                        string variant = variantBuilder.ToString().ToLower();

                        // add if we found a variant
                        if (variant != verb)
                            variants.Add(variant);
                    }

                    // add variants if we found any
                    if (variants.Count > 0)
                        _verbMorphs.Add(verb, variants);
                }

                // save verb morphs collection
                _verbMorphs.Save(MorphMapIndexPath, x => x, x =>
                    {
                        StringBuilder morphBuilder = new StringBuilder();
                        foreach (string morph in x)
                        {
                            morph.Disallow('|');
                            morphBuilder.Append((morphBuilder.Length > 0 ? "|" : "") + morph);
                        }

                        return morphBuilder.ToString();
                    }, DictionaryExtensions.Sort.None, false, true, "", Environment.NewLine, true);
            }

            // map variants back to base verbs
            _morphVerb = new Dictionary<string, string>();
            foreach (string verb in _verbMorphs.Keys)
                foreach (string morph in _verbMorphs[verb])
                {
                    // make sure morphs have a single base verb
                    string currBase;
                    if (!_morphVerb.TryGetValue(morph, out currBase))
                        _morphVerb.Add(morph, verb);
                    else if (currBase != verb)
                        throw new Exception("Multiple base verbs");
                }
        }
        #endregion

        /// <summary>
        /// Looks up all information for a given verb
        /// </summary>
        /// <param name="verb">Verb to look up information for</param>
        /// <returns>List of VerbInfo objects</returns>
        public List<VerbInfo> GetVerbInfo(string verb)
        {
            return LoadVerbInfo(VerbInfoPath, _verbInfoFilePosition[verb]);
        }

        /// <summary>
        /// Looks up all information for a given verb, organized by sense.
        /// Key:  Sense of verb (role set ID)
        /// Value:  List of VerbInfo objects for senses of verb
        /// </summary>
        /// <param name="verb">Verb to look up information for</param>
        /// <returns>Verb information, organized by sense</returns>
        public Dictionary<int, List<VerbInfo>> GetVerbInfoBySense(string verb)
        {
            // get all verb info
            Dictionary<int, List<VerbInfo>> info = new Dictionary<int, List<VerbInfo>>();
            foreach (VerbInfo vi in GetVerbInfo(verb))
            {
                info.EnsureContainsKey(vi.RoleSetId, typeof(List<VerbInfo>));
                info[vi.RoleSetId].Add(vi);
            }

            return info;
        }

        /// <summary>
        /// Tries to get VerbInfo list for a sentence
        /// </summary>
        /// <param name="mrgFile">MRG file to get info from</param>
        /// <param name="sentenceNumber">Number of sentence within MRG file for which to get VerbInfo</param>
        /// <param name="verbInfo">List of VerbInfo</param>
        /// <returns>True if successful, false otherwise</returns>
        public bool TryGetVerbInfoForSentence(string mrgFile, int sentenceNumber, out List<VerbInfo> verbInfo)
        {
            verbInfo = null;

            // check query...only need to check the MRG file and sentence number
            EnsureValidQuery(mrgFile, sentenceNumber, 0);

            // truncate mrg path to relative version
            mrgFile = MrgPath.GetRelativePathTo(mrgFile);

            // try to get sentence/position index
            Dictionary<int, long> sentPosition;
            long listPosition;
            if (_mrgSentInfoFilePosition.TryGetValue(mrgFile, out sentPosition) &&
                sentPosition.TryGetValue(sentenceNumber, out listPosition))
                verbInfo = LoadVerbInfo(MrgSentenceInfoPath, listPosition);

            return verbInfo != null;
        }

        /// <summary>
        /// Tries to get VerbInfo for a leaf
        /// </summary>
        /// <param name="mrgFile">MRG file of leaf</param>
        /// <param name="sentenceNumber">Sentence number of leaf</param>
        /// <param name="leafNumber">Leaf number</param>
        /// <param name="verbInfo">Verb info found for leaf</param>
        /// <returns>True if verb info was found, false otherwise</returns>
        public bool TryGetVerbInfoForLeaf(string mrgFile, int sentenceNumber, int leafNumber, out List<VerbInfo> verbInfo)
        {
            // check query
            EnsureValidQuery(mrgFile, sentenceNumber, leafNumber);

            // first get verb info for sentence
            verbInfo = null;
            if (TryGetVerbInfoForSentence(mrgFile, sentenceNumber, out verbInfo))
            {
                // remove verb info that isn't for the specified leaf
                List<VerbInfo> toRemove = new List<VerbInfo>();
                foreach (VerbInfo vi in verbInfo)
                    if (vi.LeafNumber != leafNumber)
                        toRemove.Add(vi);

                foreach (VerbInfo vi in toRemove)
                    verbInfo.Remove(vi);

                // null out collection if no information remains
                if (verbInfo.Count == 0)
                    verbInfo = null;
            }

            return verbInfo != null;
        }

        /// <summary>
        /// Loads a VerbInfo list from a file starting at a specific position
        /// </summary>
        /// <param name="file">File to load list from</param>
        /// <param name="position">Position to start at</param>
        /// <returns>List of VerbInfo</returns>
        private List<VerbInfo> LoadVerbInfo(string file, long position)
        {
            // read line at specified position
            StreamReader loadFile = new StreamReader(file);
            loadFile.BaseStream.Position = position;
            string infoLine = loadFile.ReadLine();
            loadFile.Close();

            // parse line...pipe-delimited series of verb entries
            List<VerbInfo> viList = new List<VerbInfo>();
            int pipeLoc = infoLine.IndexOf('|');
            while (pipeLoc != infoLine.Length - 1)
            {
                // get entry line
                int nextPipeLoc = infoLine.IndexOf('|', pipeLoc + 1);
                string entry = infoLine.Substring(pipeLoc + 1, nextPipeLoc - pipeLoc - 1);

                // parse verb info object
                VerbInfo vi = new VerbInfo(entry);

                // attach frame if we can find one for the verb
                Frame frame;
                if (_verbFrame.TryGetValue(vi.Verb, out frame))
                    vi.VerbFrame = frame;
                // if we didn't get a frame for the verb, throw exception if we're using a gold-standard tagger (non-gold taggers might identify predicates that propbank doesn't know about)
                else if (vi.Tagger == "gold")
                    throw new Exception("Failed to get PropBank frame for verb:  " + vi.Verb);

                // set full file path
                vi.File = Path.Combine(MrgPath, vi.File);
                if (!File.Exists(vi.File))
                    throw new Exception("Invalid PropBank file:  " + vi.File);

                viList.Add(vi);

                // start at next pipe
                pipeLoc = nextPipeLoc;
            }

            return viList;
        }

        /// <summary>
        /// Gets a predicate tree for a PropBank propositions entry
        /// </summary>
        /// <param name="vi">VerbInfo specifying tree to look up</param>
        /// <returns>PropBankNode</returns>
        public PropBankNode GetPropBankTree(VerbInfo vi)
        {
            TreeBankNode parse = GetParseTree(vi.File, vi.SentenceNumber);
            PropBankNode predTree = new PropBankNode(parse);
            predTree.Information = vi;

            // label information is space-delimited
            string[] labels = vi.LabeledNodeLocations.Split(' ');
            foreach (string label in labels)
            {
                // label columns are dash-delimited
                string[] labelCols = label.Split('-');

                // get label type
                PropBankNodeLabel.NodeType labelType = PropBankNodeLabel.GetNodeType(labelCols[1]);

                // get label feature if any
                PropBankNodeLabel.NodeFeature labelFeature = PropBankNodeLabel.NodeFeature.None;
                if (labelCols.Length > 2)
                {
                    // sometimes the feature is the actual preposition, so this might fail
                    string featureStr = labelCols[2];
                    if (!PropBankNodeLabel.TryGetNodeFeature(featureStr, out labelFeature))
                    {
                        // use PRP as the feature, which we have added for this case
                        featureStr = "PRP";
                        labelFeature = PropBankNodeLabel.GetNodeFeature(featureStr);
                    }

                    if (labelCols.Length > 3)
                        throw new Exception("Missed feature");
                }

                // create new labeled node collection
                PropBankLabeledNodeCollection labeledNodes = new PropBankLabeledNodeCollection(new PropBankNodeLabel(labelType, labelFeature, 1));
                AddNodesToCollection(predTree, labelCols[0], labeledNodes);

                // add to root's list of nodes
                predTree.LabeledNodeCollections.Add(labeledNodes);
            }

            // make sure one of the predicate leaves has the leaf number from the propositions file entry
            bool foundMatch = false;
            foreach (PropBankNode predicateNode in predTree.PredicateNodes)
            {
                foreach (PropBankNode leaf in predicateNode.Leaves)
                    if (leaf.LeafNumber == vi.LeafNumber)
                    {
                        foundMatch = true;
                        break;
                    }

                if (foundMatch)
                    break;
            }

            if (!foundMatch)
                throw new Exception("Mismatch between VerbInfo predicate leaf number and actual predicate leaf number");

            return predTree;
        }

        /// <summary>
        /// Gets full bracketing of a sentence
        /// </summary>
        /// <param name="mrgFile">MRG file to get sentence from</param>
        /// <param name="sentNum">Sentence number</param>
        /// <returns>Full bracketing</returns>
        public string GetFullBracketing(string mrgFile, int sentNum)
        {
            // make sure we have predicates for the sentence...if we don't, simply return the surface text if needed
            List<VerbInfo> sentVerbInfo;
            if (!TryGetVerbInfoForSentence(mrgFile, sentNum, out sentVerbInfo))
                return GetParseTree(mrgFile, sentNum).SurfaceText;

            // lists of tokens and labels for each token across all predicates
            List<string> tokens = null;
            Dictionary<int, List<string>> tokenLabels = null;

            // read each predicate for the sentence
            foreach (VerbInfo vi in sentVerbInfo)
            {
                PropBankNode predTree = GetPropBankTree(vi);

                // init lists on first predicate
                if (tokens == null)
                {
                    tokens = new List<string>();
                    tokenLabels = new Dictionary<int, List<string>>();

                    foreach (TreeBankNode token in predTree.Tokens)
                    {
                        tokens.Add(token.SurfaceText);
                        tokenLabels.Add(token.TokenNumber, new List<string>());
                    }
                }
                // sanity check on number of tokens
                else if (tokenLabels.Count != predTree.Tokens.Length)
                    throw new Exception("Token count mismatch");

                // collect labels for tokens w.r.t. current predicate
                foreach (PropBankNode token in predTree.Tokens)
                {
                    // get labels from current token as well as all ancestors
                    List<PropBankNodeLabel> labels = new List<PropBankNodeLabel>();
                    foreach (PropBankNode node in token.Ancestors)
                        if (node.Label != null)
                            labels.Add(node.Label);

                    // build label for token
                    string tokenLabel = "";
                    foreach (PropBankNodeLabel label in labels)
                    {
                        string typeStr = label.Type.ToString();

                        if (label.IsArgument)
                            tokenLabel += typeStr[0].ToString() + typeStr[typeStr.Length - 1].ToString() + ",";
                        else if (label.IsModifier)
                            tokenLabel += typeStr[0] + "-" + label.Feature.ToString() + ",";
                        else if (label.IsPredicate)
                            tokenLabel += "Predicate,";
                    }

                    tokenLabels[token.TokenNumber].Add(tokenLabel.Trim(','));
                }
            }

            // build format string
            string format = "";
            for (int tokenNum = 0; tokenNum < tokens.Count; ++tokenNum)
            {
                // get maximum label length
                int maxLabelLen = tokens[tokenNum].Length;
                foreach (string label in tokenLabels[tokenNum])
                    if (label.Length > maxLabelLen)
                        maxLabelLen = label.Length;

                format += "{" + tokenNum + "," + maxLabelLen + "}|";
            }

            format = format.Trim('|');

            // start with surface text
            StringBuilder bracketedText = new StringBuilder(string.Format(format, tokens.ToArray()));

            // only add newline if there is another line
            int numPreds = sentVerbInfo.Count;
            if (numPreds > 0)
                bracketedText.Append(Environment.NewLine);

            // add each predicate line
            for (int lineNum = 0; lineNum < numPreds; ++lineNum)
            {
                List<string> lineLabels = new List<string>();
                foreach (int tokenNum in tokenLabels.Keys)
                    lineLabels.Add(tokenLabels[tokenNum][lineNum]);

                // append bracketed line
                bracketedText.Append(string.Format(format, lineLabels.ToArray()));

                // only add newline if there is another line
                if (lineNum < numPreds - 1)
                    bracketedText.Append(Environment.NewLine);
            }

            return bracketedText.ToString();
        }

        /// <summary>
        /// Gets whether or not PropBank contains a verb
        /// </summary>
        /// <param name="verb">Verb to check for</param>
        /// <returns>True if PropBank contains the verb, false otherwise</returns>
        public bool Contains(string verb)
        {
            return _verbInfoFilePosition.ContainsKey(verb);
        }

        /// <summary>
        /// Gets the frame for a verb
        /// </summary>
        /// <param name="verb">Verb to get frame for</param>
        /// <returns>Frame</returns>
        public Frame GetFrame(string verb)
        {
            return _verbFrame[verb];
        }

        /// <summary>
        /// Tries to get the frame for a verb
        /// </summary>
        /// <param name="verb">Verb to (try to) get frame for</param>
        /// <param name="frame">Frame (output)</param>
        /// <returns>True if frame was found, false otherwise</returns>
        public bool TryGetFrame(string verb, out Frame frame)
        {
            return _verbFrame.TryGetValue(verb, out frame);
        }
    }
}

namespace LAIR.CommonPort
{
}
