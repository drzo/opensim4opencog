using System;
using System.Collections.Generic;
using System.Text;
using System.IO;
using System.Text.RegularExpressions;
using System.Collections.ObjectModel;
using System.Linq;

using LAIR.ResourceAPIs.PennBank.TreeBank;
using LAIR.ResourceAPIs.NomBank.NomLex;
using LAIR.ResourceAPIs.PennBank.PropBank;
using LAIR.XML;
using LAIR.Collections.Generic;
using LAIR.Extensions;

namespace LAIR.ResourceAPIs.NomBank
{
    /// <summary>
    /// Provides access to NomBank layer of information on top of the TreeBank.
    /// </summary>
    public class NomBankEngine : TreeBankEngine
    {
        #region static members
        /// <summary>
        /// Full bracketing options
        /// </summary>
        public enum FullBracketingOptions
        {
            /// <summary>
            /// Bracket predicates
            /// </summary>
            BracketPredicates,

            /// <summary>
            /// Bracket arguments
            /// </summary>
            BracketArguments,

            /// <summary>
            /// Bracket modifiers
            /// </summary>
            BracketModifiers,

            /// <summary>
            /// Bracket support verbs
            /// </summary>
            BracketSupportVerbs,

            /// <summary>
            /// Indicate non-markable nominals with a '*'
            /// </summary>
            IndicateNonMarkables,

            /// <summary>
            /// Separate predicate lines for the sentence with a line of '-'
            /// </summary>
            HorizontalLineSeparators,

            /// <summary>
            /// Include original sentence text as first line
            /// </summary>
            IncludeOriginalText,

            /// <summary>
            /// Include all available information
            /// </summary>
            IncludeAllInformation
        }

        /// <summary>
        /// Saves a token index
        /// </summary>
        /// <param name="index">Index to save</param>
        /// <param name="path">Path to file in which to save index</param>
        private static void SaveTokenIndex(Dictionary<string, Dictionary<uint, Set<ushort>>> index, string path)
        {
            StreamWriter indexFile = new StreamWriter(path);

            foreach (string mrgFile in index.Keys)
            {
                indexFile.WriteLine("m " + mrgFile);
                foreach (uint sentNum in index[mrgFile].Keys)
                {
                    indexFile.WriteLine("s " + sentNum);
                    foreach (ushort tokenNum in index[mrgFile][sentNum])
                        indexFile.WriteLine(tokenNum);
                }
            }
            indexFile.Close();
        }

        /// <summary>
        /// Reads a token index
        /// </summary>
        /// <param name="path">Path to token index</param>
        /// <returns>Token index</returns>
        private static Dictionary<string, Dictionary<uint, Set<ushort>>> ReadTokenIndex(string path)
        {
            StreamReader indexFile = new StreamReader(path);

            Dictionary<string, Dictionary<uint, Set<ushort>>> index = new Dictionary<string, Dictionary<uint, Set<ushort>>>();
            Dictionary<uint, Set<ushort>> sentenceTokens = null;
            Set<ushort> tokens = null;

            string line;
            while ((line = indexFile.ReadLine()) != null)
            {
                char firstChar = line[0];
                if (firstChar == 'm')
                {
                    string mrgFile = line.Substring(2);
                    sentenceTokens = new Dictionary<uint, Set<ushort>>();
                    index.Add(mrgFile, sentenceTokens);
                }
                else if (firstChar == 's')
                {
                    uint sentence = uint.Parse(line.Substring(2));
                    tokens = new Set<ushort>();
                    sentenceTokens.Add(sentence, tokens);
                }
                else
                    tokens.Add(ushort.Parse(line));
            }
            indexFile.Close();

            return index;
        }

        /// <summary>
        /// Builds the Frame index
        /// </summary>
        /// <param name="framesDirectory">Directory containing NomBank frame XML files</param>
        /// <returns>Frame index - a map from noun lemma to corresponding frame</returns>
        private static Dictionary<string, Frame> BuildFrameIndex(string framesDirectory)
        {
            if (!Directory.Exists(framesDirectory))
                throw new Exception("Invalid frame directory:  \"" + framesDirectory + "\"");

            // matches the "source" attribute on each role set...gets verb and role set source
            Regex sourceRE = new Regex(@"^verb-(?<source_verb>[^.]+)\.(?<source_roleset>[0-9]+)$");

            // read each frame file
            Dictionary<string, Frame> frameIndex = new Dictionary<string, Frame>();
            foreach (string framePath in Directory.GetFiles(framesDirectory))
            {
                Frame currFrame = null;

                // read entire file and create XML parser
                StreamReader frameFile = new StreamReader(framePath);
                string frameXML = frameFile.ReadToEnd();
                frameFile.Close();
                XmlParser frameP = new XmlParser(frameXML);

                // get role sets
                string roleSetXML;
                while ((roleSetXML = frameP.OuterXML("roleset")) != null)
                {
                    XmlParser roleSetP = new XmlParser(roleSetXML);

                    // get role set ID string in noun.id format
                    string roleSetIdStr = roleSetP.AttributeValue("roleset", "id");
                    int dotIndex = roleSetIdStr.IndexOf('.');

                    // get role set noun
                    string roleSetNoun = roleSetIdStr.Substring(0, dotIndex);

                    // convert some special lemmas back to their normal form
                    if (roleSetNoun == "perc-sign")
                        roleSetNoun = "%";
                    else if (roleSetNoun == "oneslashonezeroth")
                        roleSetNoun = "1/10th";

                    // if this is the first role set, create the frame
                    if (currFrame == null)
                        currFrame = new Frame(roleSetNoun);
                    // all role sets must use the same noun
                    else if (roleSetNoun != currFrame.Noun)
                        throw new Exception("Role set noun mismatch");

                    // get role set ID/name
                    int roleSetID = int.Parse(roleSetIdStr.Substring(roleSetIdStr.LastIndexOf('.') + 1));
                    string roleSetName = roleSetP.AttributeValue("roleset", "name");

                    // get source verb and role set
                    string sourceVerb = null;
                    int sourceRoleSet = -1;
                    Match sourceMatch = sourceRE.Match(roleSetP.AttributeValue("roleset", "source").ToLower().Trim());
                    if (sourceMatch.Success)
                    {
                        sourceVerb = sourceMatch.Groups["source_verb"].Value;
                        sourceRoleSet = int.Parse(sourceMatch.Groups["source_roleset"].Value);
                    }

                    // create role set
                    RoleSet roleSet = new RoleSet(roleSetID, roleSetName, sourceVerb, sourceRoleSet);

                    // get roles
                    string roleXML;
                    while ((roleXML = roleSetP.OuterXML("role")) != null)
                    {
                        XmlParser roleP = new XmlParser(roleXML);

                        string description = roleP.AttributeValue("role", "descr");
                        string roleNumber = roleP.AttributeValue("role", "n").ToLower();

                        // skip modifier and agentive modifier
                        if (roleNumber == "m" || roleNumber == "a")
                            continue;

                        // get source argument in propbank
                        string sourceStr = roleP.AttributeValue("role", "source").ToLower();
                        int source = -1;
                        if (sourceStr != "" && sourceStr != "m" && sourceStr != "a")
                            source = int.Parse(sourceStr);

                        Role role = new Role(description, int.Parse(roleNumber), source);
                        roleSet.Add(role);
                    }

                    // add role set to frame
                    currFrame.AddRoleSet(roleSet);
                }

                frameIndex.Add(currFrame.Noun, currFrame);
            }

            return frameIndex;
        }
        #endregion

        private Dictionary<string, long> _nounInfoFilePosition;
        private Dictionary<string, Dictionary<int, long>> _mrgSentInfoFilePosition;
        private Dictionary<string, Set<string>> _nounMorphs;
        private Dictionary<string, Set<string>> _morphNouns;
        private Dictionary<string, Dictionary<uint, Set<ushort>>> _mrgSentMarkableTokens;
        private Dictionary<string, Dictionary<uint, Set<ushort>>> _mrgSentSupportVerbTokens;
        private Dictionary<string, Set<ushort>> _nounSections;
        private NomLexEngine _nomLexEngine;
        private Dictionary<string, Frame> _nounFrame;
        private Dictionary<string, Set<string>> _nounVerbs;
        private Dictionary<string, Set<string>> _verbNouns;

        /// <summary>
        /// Gets the NomLex engine used
        /// </summary>
        public NomLexEngine NomLexEngine
        {
            get { return _nomLexEngine; }
        }

        /// <summary>
        /// Gets the mapping from base nouns to their morphological variants
        /// </summary>
        public Dictionary<string, Set<string>> NounMorphs
        {
            get { return _nounMorphs; }
        }

        /// <summary>
        /// Gets the mapping from morphological variants to their base nouns
        /// </summary>
        public Dictionary<string, Set<string>> MorphNouns
        {
            get { return _morphNouns; }
        }

        /// <summary>
        /// Gets a list of all nouns in the lexicon
        /// </summary>
        public ReadOnlyCollection<string> AllNouns
        {
            get { return new ReadOnlyCollection<string>(new List<string>(_nounInfoFilePosition.Keys)); }
        }

        #region paths
        /// <summary>
        /// Gets the path to the NounInfo positions file, which stores the file positions of the NounInfo list for each noun
        /// </summary>
        private string NounInfoFilePositionPath
        {
            get { return NounInfoPath + "_positions"; }
        }

        /// <summary>
        /// Gets the path to the noun info file, which stores lists of NounInfo objects, one list for each noun
        /// </summary>
        private string NounInfoPath
        {
            get { return Path.Combine(IndexDirectory, "noun_info"); }
        }

        /// <summary>
        /// Gets the path to the MRG file/sentence number NounInfo index
        /// </summary>
        private string MrgSentenceInfoPath
        {
            get { return Path.Combine(IndexDirectory, "mrg_sentence_info"); }
        }

        /// <summary>
        /// Gets the path to the MRG file index, which maps a MRG file to its sentence number/NounInfo index
        /// </summary>
        private string MrgSentenceInfoFilePositionsPath
        {
            get { return MrgSentenceInfoPath + "_positions"; }
        }

        /// <summary>
        /// Gets the path to the markable tokens file
        /// </summary>
        private string MarkableTokensPath
        {
            get { return Path.Combine(IndexDirectory, "markable_tokens"); }
        }

        /// <summary>
        /// Gets the path to the support verb tokens file
        /// </summary>
        private string SupportVerbTokenPath
        {
            get { return Path.Combine(IndexDirectory, "support_verbs"); }
        }

        /// <summary>
        /// Gets the path to the noun section file, which maps nouns to the sections they appear in
        /// </summary>
        private string NounSectionPath
        {
            get { return Path.Combine(IndexDirectory, "noun_sections"); }
        }
        #endregion

        #region constructors
        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="mrgPath">Path to TreeBank directory to search recursively for .mrg files</param>
        /// <param name="propsFilePath">Path to NomBank propositions file</param>
        /// <param name="framesDirectory">Path to the NomBank frames directory</param>
        /// <param name="morphFilePath">Path to NomBank morphological dictionary file</param>
        /// <param name="nomLexFilePath">Path to NomLex file</param>
        /// <param name="indexDirectory">Path to the index directory, where all index information is stored</param>
        public NomBankEngine(string mrgPath,
                             string propsFilePath,
                             string framesDirectory,
                             string morphFilePath,
                             string nomLexFilePath,
                             string indexDirectory)
            : base(mrgPath, indexDirectory)
        {
            Construct(propsFilePath, framesDirectory, morphFilePath, nomLexFilePath, indexDirectory);
        }

        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="mrgPath">Path to TreeBank directory to search recursively for .mrg files</param>
        /// <param name="nomBankPath">Path to NomBank directory. This directory must contain the standard "nombank.1.0" propositions
        /// file, the "frames" directory, the "nombank-morph.dict.1.0" morphological dictionary, and the "NOMLEX-plus.1.0" NomLex
        /// file.</param>
        /// <param name="indexDirectory">Path to the index directory, where all index information is stored</param>
        public NomBankEngine(string mrgPath,
                             string nomBankPath,
                             string indexDirectory)
            : this(mrgPath, Path.Combine(nomBankPath, "nombank.1.0"), Path.Combine(nomBankPath, "frames"), Path.Combine(nomBankPath, "nombank-morph.dict.1.0"), Path.Combine(nomBankPath, "NOMLEX-plus.1.0"), indexDirectory)
        {
        }

        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="mrgPath">Path to TreeBank directory to search recursively for .mrg files</param>
        /// <param name="propsFilePath">Path to NomBank propositions file</param>
        /// <param name="framesDirectory">Path to the NomBank frames directory</param>
        /// <param name="morphFilePath">Path to NomBank morphological dictionary file</param>
        /// <param name="nomLexFilePath">Path to NomLex file</param>
        /// <param name="indexDirectory">Path to the index directory, where all index information is stored</param>
        /// <param name="mrgFileNameRE">Regular expression for MRG file name. See MrgFileNameRE property for details.</param>
        public NomBankEngine(string mrgPath,
                             string propsFilePath,
                             string framesDirectory,
                             string morphFilePath,
                             string nomLexFilePath,
                             string indexDirectory,
                             Regex mrgFileNameRE)
            : base(mrgPath, indexDirectory, mrgFileNameRE)  // must construct list this because we need MrgFileNameRE to be set correctly before body of constructor
        {
            Construct(propsFilePath, framesDirectory, morphFilePath, nomLexFilePath, indexDirectory);
        }

        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="mrgPath">Path to TreeBank directory to search recursively for .mrg files</param>
        /// <param name="nomBankPath">Path to NomBank directory. This directory must contain the standard "nombank.1.0" propositions
        /// file, the "frames" directory, the "nombank-morph.dict.1.0" morphological dictionary, and the "NOMLEX-plus.1.0" NomLex
        /// file.</param>
        /// <param name="indexDirectory">Path to the index directory, where all index information is stored</param>
        /// <param name="mrgFileNameRE">Regular expression for MRG file name. See MrgFileNameRE property for details.</param>
        public NomBankEngine(string mrgPath,
                             string nomBankPath,
                             string indexDirectory,
                             Regex mrgFileNameRE)
            : this(mrgPath, Path.Combine(nomBankPath, "nombank.1.0"), Path.Combine(nomBankPath, "frames"), Path.Combine(nomBankPath, "nombank-morph.dict.1.0"), Path.Combine(nomBankPath, "NOMLEX-plus.1.0"), indexDirectory, mrgFileNameRE)
        {
        }

        /// <summary>
        /// Constructs the engine
        /// </summary>
        /// <param name="propsFilePath">Path to NomBank propositions file</param>
        /// <param name="framesDirectory">Path to the NomBank frames directory</param>
        /// <param name="morphFilePath">Path to NomBank morphological dictionary file</param>
        /// <param name="nomLexFilePath">Path to NomLex file</param>
        /// <param name="indexDirectory">Path to the index directory, where all index information is stored</param>
        private void Construct(string propsFilePath, string framesDirectory, string morphFilePath, string nomLexFilePath, string indexDirectory)
        {
            // build frame index...we'll need frames when loading the props, so do this first
            _nounFrame = BuildFrameIndex(framesDirectory);

            // initialize NomLex...we'll also need this
            _nomLexEngine = new NomLexEngine(nomLexFilePath);

            // build noun index
            LoadProps(propsFilePath);

            // build morphological mapping index
            BuildMorphMap(morphFilePath);

            #region build verb-noun and noun-verb maps from frames
            int mapCount = 0;
            foreach (string noun in _nounFrame.Keys)
            {
                bool mapped = false;
                foreach (RoleSet roleSet in _nounFrame[noun].RoleSets)
                    if (roleSet.SourceVerb != null)
                    {
                        mapped = true;
                        break;
                    }

                if (mapped)
                    ++mapCount;
            }

            _nounVerbs = new Dictionary<string, Set<string>>(mapCount);
            _verbNouns = new Dictionary<string, Set<string>>(mapCount);
            foreach (string noun in _nounFrame.Keys)
                foreach (RoleSet roleSet in _nounFrame[noun].RoleSets)
                    if (roleSet.SourceVerb != null)
                    {
                        _nounVerbs.EnsureContainsKey(noun, typeof(Set<string>), false);
                        _nounVerbs[noun].Add(roleSet.SourceVerb);

                        _verbNouns.EnsureContainsKey(roleSet.SourceVerb, typeof(Set<string>), false);
                        _verbNouns[roleSet.SourceVerb].Add(noun);
                    }
            #endregion
        }
        #endregion

        #region loading
        /// <summary>
        /// Loads the propositions file
        /// </summary>
        /// <param name="propFilePath">Path to prop.txt file</param>
        private void LoadProps(string propFilePath)
        {
            if (!File.Exists(propFilePath))
                throw new Exception("Invalid NomBank proposition file:  " + propFilePath);

            // reuse existing index files if present
            if (File.Exists(NounInfoPath) &&
                File.Exists(NounInfoFilePositionPath) &&
                File.Exists(MrgSentenceInfoPath) &&
                File.Exists(MrgSentenceInfoFilePositionsPath) &&
                File.Exists(MarkableTokensPath) &&
                File.Exists(SupportVerbTokenPath) &&
                File.Exists(NounSectionPath))
            {
                // load noun info index
                _nounInfoFilePosition = new Dictionary<string, long>();
                StreamReader positionsFile = new StreamReader(NounInfoFilePositionPath);
                string line;
                while ((line = positionsFile.ReadLine()) != null)
                {
                    // format:  position noun
                    int spaceLoc = line.IndexOf(' ');
                    _nounInfoFilePosition.Add(line.Substring(spaceLoc + 1), long.Parse(line.Substring(0, spaceLoc)));
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
                        // find next pipe
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

                // load markable tokens
                _mrgSentMarkableTokens = ReadTokenIndex(MarkableTokensPath);

                // load support verb tokens
                _mrgSentSupportVerbTokens = ReadTokenIndex(SupportVerbTokenPath);

                // load noun section index
                StreamReader nounSectionFile = new StreamReader(NounSectionPath);
                _nounSections = new Dictionary<string, Set<ushort>>(int.Parse(nounSectionFile.ReadLine()));
                while (nounSectionFile.TryReadLine(out line))
                    _nounSections.Add(line, new Set<ushort>(nounSectionFile.ReadLine().Split().Select(sec => ushort.Parse(sec)).ToArray()));

                return;
            }

            // maps each noun to a list of NounInfo objects, each of which stores information for retrieving a predicate tree
            Dictionary<string, List<NounInfo>> nounInfo = new Dictionary<string, List<NounInfo>>();

            // maps each mrg file and sentence number to a list of NounInfo objects for that sentence
            Dictionary<string, Dictionary<int, List<NounInfo>>> mrgSentInfo = new Dictionary<string, Dictionary<int, List<NounInfo>>>();

            // maps nouns to the sections they occur in
            _nounSections = new Dictionary<string, Set<ushort>>();

            // read all lines in the propositions file
            StreamReader propFile = new StreamReader(propFilePath);
            string propLine;
            while ((propLine = propFile.ReadLine()) != null)
            {
                NounInfo ni = new NounInfo(propLine);

                // add to mapping from nouns to their information
                nounInfo.EnsureContainsKey(ni.Noun, typeof(List<NounInfo>));
                nounInfo[ni.Noun].Add(ni);

                // add to mapping from file-sentence pairs to their information
                mrgSentInfo.EnsureContainsKey(ni.File, typeof(Dictionary<int, List<NounInfo>>));
                mrgSentInfo[ni.File].EnsureContainsKey(ni.SentenceNumber, typeof(List<NounInfo>));
                mrgSentInfo[ni.File][ni.SentenceNumber].Add(ni);

                // add to section index
                _nounSections.EnsureContainsKey(ni.Noun, typeof(Set<ushort>), false);
                _nounSections[ni.Noun].Add((ushort)GetSectionNumber(ni.File));
            }

            propFile.Close();

            // write noun index to disk and record file positions of noun info lists
            _nounInfoFilePosition = new Dictionary<string, long>();
            FileStream saveStream = new FileStream(NounInfoPath, FileMode.Create);
            foreach (string noun in nounInfo.Keys)
            {
                // save position of NounInfo list
                _nounInfoFilePosition.Add(noun, saveStream.Position);

                WriteNounInfoList(nounInfo[noun], saveStream);
            }
            saveStream.Close();

            // save file positions
            StreamWriter nounInfoPositionFile = new StreamWriter(NounInfoFilePositionPath);
            foreach (string noun in _nounInfoFilePosition.Keys)
                nounInfoPositionFile.WriteLine(_nounInfoFilePosition[noun] + " " + noun);
            nounInfoPositionFile.Close();

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

                    // write NounInfo list for sentence
                    WriteNounInfoList(mrgSentInfo[mrgFile][sentNum], saveStream);
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

            // get predicate and support verb tokens in all MRG files
            _mrgSentMarkableTokens = new Dictionary<string, Dictionary<uint, Set<ushort>>>();
            _mrgSentSupportVerbTokens = new Dictionary<string, Dictionary<uint, Set<ushort>>>();
            foreach (string mrgFile in IndexedMrgFiles)
            {
                Dictionary<uint, Set<ushort>> sentenceMarkableTokens = new Dictionary<uint, Set<ushort>>();
                Dictionary<uint, Set<ushort>> sentenceSupportVerbTokens = new Dictionary<uint, Set<ushort>>();

                // scan each sentence in current MRG file
                foreach (int sentNum in GetSentenceNumbers(mrgFile))
                {
                    Set<ushort> markableTokens = new Set<ushort>(false);
                    Set<ushort> supportVerbTokens = new Set<ushort>(false);

                    List<NounInfo> niList;
                    if (TryGetNounInfoForSentence(mrgFile, sentNum, out niList))
                        foreach (NounInfo info in niList)
                            foreach (NomBankNode n in GetNomBankTree(info).AllNodes)
                            {
                                if (n.IsPredicate)
                                    if (!n.IsToken)
                                        throw new Exception("Non-token predicate found");
                                    else
                                        markableTokens.Add((ushort)n.TokenNumber);

                                if (n.IsSupportVerb)
                                    if (!n.IsToken)
                                        throw new Exception("Non-token support verb found");
                                    else
                                        supportVerbTokens.Add((ushort)n.TokenNumber);
                            }

                    // add to sentence lists if we found any in the current sentence
                    uint uSentNum = (uint)sentNum;

                    if (markableTokens.Count > 0)
                        sentenceMarkableTokens.Add(uSentNum, markableTokens);

                    if (supportVerbTokens.Count > 0)
                        sentenceSupportVerbTokens.Add(uSentNum, supportVerbTokens);
                }

                // add to MRG file lists if we found any in the current MRG file
                string relativeMrgFile = MrgPath.GetRelativePathTo(mrgFile);
                if (sentenceMarkableTokens.Count > 0)
                    _mrgSentMarkableTokens.Add(relativeMrgFile, sentenceMarkableTokens);

                if (sentenceSupportVerbTokens.Count > 0)
                    _mrgSentSupportVerbTokens.Add(relativeMrgFile, sentenceSupportVerbTokens);
            }

            // save token indexes
            SaveTokenIndex(_mrgSentMarkableTokens, MarkableTokensPath);
            SaveTokenIndex(_mrgSentSupportVerbTokens, SupportVerbTokenPath);

            // save noun section info
            _nounSections.Save(NounSectionPath, x => x, x =>
                {
                    StringBuilder line = new StringBuilder();
                    foreach (ushort sec in x)
                        line.Append((line.Length > 0 ? " " : "") + sec);

                    return line.ToString();
                }, DictionaryExtensions.Sort.None, false, true, "", Environment.NewLine, true);
        }

        /// <summary>
        /// Writes a list of NounInfo to a stream
        /// </summary>
        /// <param name="list">List of NounInfo to write</param>
        /// <param name="stream">Stream to write to</param>
        private void WriteNounInfoList(List<NounInfo> list, FileStream stream)
        {
            byte[] bytes;
            foreach (NounInfo ni in list)
            {
                bytes = Encoding.UTF8.GetBytes("|" + ni.GetPropEntry(null));
                stream.Write(bytes, 0, bytes.Length);
            }

            bytes = Encoding.UTF8.GetBytes("|" + Environment.NewLine);
            stream.Write(bytes, 0, bytes.Length);
        }
        #endregion

        /// <summary>
        /// Looks up all information for a given noun
        /// </summary>
        /// <param name="noun">Noun to look up information for</param>
        /// <returns>List of NounInfo objects</returns>
        public List<NounInfo> GetNounInfo(string noun)
        {
            return LoadNounInfo(NounInfoPath, _nounInfoFilePosition[noun]);
        }

        /// <summary>
        /// Looks up all information for a given noun, organized by sense.
        /// Key:  Sense of noun (role set ID)
        /// Value:  List of NounInfo objects for senses of noun
        /// </summary>
        /// <param name="noun">Noun to look up information for</param>
        /// <returns>Noun information, organized by sense</returns>
        public Dictionary<int, List<NounInfo>> GetNounInfoBySense(string noun)
        {
            Dictionary<int, List<NounInfo>> info = new Dictionary<int, List<NounInfo>>();

            // get all noun info
            foreach (NounInfo ni in GetNounInfo(noun))
            {
                info.EnsureContainsKey(ni.RoleSetId, typeof(List<NounInfo>));
                info[ni.RoleSetId].Add(ni);
            }

            return info;
        }

        /// <summary>
        /// Tries to get NounInfo for a sentence
        /// </summary>
        /// <param name="mrgFile">MRG file to look in</param>
        /// <param name="sentenceNumber">Sentence number to look at</param>
        /// <param name="nounInfo">List of NounInfo if any was found, null otherwise</param>
        /// <returns>True if NounInfo was found, false otherwise</returns>
        public bool TryGetNounInfoForSentence(string mrgFile, int sentenceNumber, out List<NounInfo> nounInfo)
        {
            nounInfo = null;

            // check query...only need to check MRG file and sentence number
            EnsureValidQuery(mrgFile, sentenceNumber, 0);

            // truncate mrg path to relative version
            mrgFile = MrgPath.GetRelativePathTo(mrgFile);

            // try to get the noun info list
            Dictionary<int, long> sentPosition;
            long listPosition;
            if (_mrgSentInfoFilePosition.TryGetValue(mrgFile, out sentPosition) &&
                sentPosition.TryGetValue(sentenceNumber, out listPosition))
                nounInfo = LoadNounInfo(MrgSentenceInfoPath, listPosition);

            return nounInfo != null;
        }

        /// <summary>
        /// Tries to get NounInfo for a leaf
        /// </summary>
        /// <param name="mrgFile">MRG file of leaf</param>
        /// <param name="sentenceNumber">Sentence number of leaf</param>
        /// <param name="leafNumber">Leaf number</param>
        /// <param name="nounInfo">Noun info found for leaf</param>
        /// <returns>True if noun info was found, false otherwise</returns>
        public bool TryGetNounInfoForLeaf(string mrgFile, int sentenceNumber, int leafNumber, out List<NounInfo> nounInfo)
        {
            // check query
            EnsureValidQuery(mrgFile, sentenceNumber, leafNumber);

            // first get noun info for sentence
            nounInfo = null;
            if (TryGetNounInfoForSentence(mrgFile, sentenceNumber, out nounInfo))
            {
                // remove noun info that isn't for the specified leaf
                List<NounInfo> toRemove = new List<NounInfo>();
                foreach (NounInfo ni in nounInfo)
                    if (ni.LeafNumber != leafNumber)
                        toRemove.Add(ni);

                foreach (NounInfo ni in toRemove)
                    nounInfo.Remove(ni);

                // null out collection if no information remains
                if (nounInfo.Count == 0)
                    nounInfo = null;
            }

            return nounInfo != null;
        }

        /// <summary>
        /// Loads a NounInfo list from a file starting at a specific position
        /// </summary>
        /// <param name="file">File to load list from</param>
        /// <param name="position">Position to start at</param>
        /// <returns>List of NounInfo</returns>
        private List<NounInfo> LoadNounInfo(string file, long position)
        {
            // read line at specified position
            StreamReader loadFile = new StreamReader(file);
            loadFile.BaseStream.Position = position;
            string infoLine = loadFile.ReadLine();
            loadFile.Close();

            // parse line...pipe-delimited series of noun entries
            List<NounInfo> niList = new List<NounInfo>();
            int pipeLoc = infoLine.IndexOf('|');
            while (pipeLoc != infoLine.Length - 1)
            {
                // get entry line
                int nextPipeLoc = infoLine.IndexOf('|', pipeLoc + 1);
                string entry = infoLine.Substring(pipeLoc + 1, nextPipeLoc - pipeLoc - 1);

                // parse noun info object and set frame
                NounInfo ni = new NounInfo(entry);
                ni.Frame = _nounFrame[ni.Noun];

                // set full file path
                ni.File = Path.Combine(MrgPath, ni.File);
                if (!File.Exists(ni.File))
                    throw new Exception("Invalid NomBank file:  " + ni.File);

                niList.Add(ni);

                // start at next pipe
                pipeLoc = nextPipeLoc;
            }

            return niList;
        }

        /// <summary>
        /// Gets a NomBank tree for an entry in the propositions file
        /// </summary>
        /// <param name="ni">NounInfo specifying tree to look up</param>
        /// <returns>NomBankNode</returns>
        public NomBankNode GetNomBankTree(NounInfo ni)
        {
            // look up the parse tree
            TreeBankNode tbRoot = GetParseTree(ni.File, ni.SentenceNumber);
            NomBankNode nbRoot = new NomBankNode(tbRoot);
            nbRoot.Information = ni;

            Regex hyphenIndexRE = new Regex("H[0-5]");

            // label information is space-delimited
            string[] labels = ni.LabeledNodeLocations.Split(' ');
            foreach (string label in labels)
            {
                // label columns are dash-delimited
                string[] labelCols = label.Split('-');

                // get label type
                NomBankNodeLabel.NodeType labelType = NomBankNodeLabel.GetNodeType(labelCols[1]);

                // get label feature and hyphen indexes if any
                NomBankNodeLabel.NodeFeature labelFeature = NomBankNodeLabel.NodeFeature.None;
                List<NomBankNodeLabel.HyphenationIndex> hyphenIndexes = new List<NomBankNodeLabel.HyphenationIndex>();
                for (int i = 2; i < labelCols.Length; ++i)
                {
                    string featureStr = labelCols[i];

                    // check for hyphen index
                    if (hyphenIndexRE.Match(featureStr).Success)
                    {
                        NomBankNodeLabel.HyphenationIndex hyphenIndex = (NomBankNodeLabel.HyphenationIndex)Enum.Parse(typeof(NomBankNodeLabel.HyphenationIndex), featureStr);
                        hyphenIndexes.Add(hyphenIndex);
                    }
                    // a feature is the only other option
                    else
                    {
                        // shouldn't have two features
                        if (labelFeature != NomBankNodeLabel.NodeFeature.None)
                            throw new Exception("Multiple node features");

                        labelFeature = NomBankNodeLabel.GetNodeFeature(featureStr);
                    }
                }

                // get type label for nodes
                NomBankNodeLabel collectionLabel = new NomBankNodeLabel(labelType, labelFeature, 1);
                foreach (NomBankNodeLabel.HyphenationIndex hyphenIndex in hyphenIndexes)
                    collectionLabel.AddHyphenIndex(hyphenIndex);

                // get referenced nodes
                NomBankLabeledNodeCollection labeledNodes = new NomBankLabeledNodeCollection(collectionLabel);
                PropBankEngine.AddNodesToCollection(nbRoot, labelCols[0], labeledNodes);

                // make sure all hyphen indexes were applied
                if (labeledNodes.Label.HyphenIndexes.Count != labeledNodes.AppliedIndexes.Count)
                    throw new Exception("Not all hyphen indexes applied");

                // add labeled nodes to root's collection
                nbRoot.LabeledNodeCollections.Add(labeledNodes);
            }

            // do a few things to the predicate node
            NomBankNode predicateNode = nbRoot.PredicateNode;

            // get NomLex entries for predicate node
            List<NomLexEntry> entries;
            if (_nomLexEngine.TryGetEntries(ni.Noun, out entries))
                predicateNode.NomLexEntries = entries;
            else
                predicateNode.NomLexEntries = new List<NomLexEntry>();

            // make sure one of the predicate leaves has the leaf number from the propositions file entry
            bool foundMatch = false;
            foreach (NomBankNode leaf in predicateNode.Leaves)
                if (leaf.LeafNumber == ni.LeafNumber)
                {
                    foundMatch = true;
                    break;
                }

            if (!foundMatch)
                throw new Exception("Mismatch between NounInfo predicate leaf number and actual predicate leaf number");

            return nbRoot;
        }

        /// <summary>
        /// Gets whether or not NomBank contains a noun
        /// </summary>
        /// <param name="noun">Noun to check for</param>
        /// <returns>True if NomBank contains the noun, false otherwise</returns>
        public bool Contains(string noun)
        {
            return _nounInfoFilePosition.ContainsKey(noun);
        }

        /// <summary>
        /// Gets whether or not NomBank contains a noun or a morphological variant of the noun
        /// </summary>
        /// <param name="noun">Noun to check</param>
        /// <returns>True if noun/variant is contained, false otherwise</returns>
        public bool ContainsMorphologicalVariantOf(string noun)
        {
            if (Contains(noun))
                return true;

            // check if token has entry in morphological dictionary
            Set<string> baseNouns;
            if (_morphNouns.TryGetValue(noun, out baseNouns))
                foreach (string baseNoun in baseNouns)
                    if (Contains(baseNoun))
                        return true;

            return false;
        }

        /// <summary>
        /// Tries to get the nouns for a given verb
        /// </summary>
        /// <param name="verb">Verb to get nouns for</param>
        /// <param name="nouns">Nouns corresponding to verb</param>
        /// <returns>True if nouns were found</returns>
        public bool TryGetNounsForVerb(string verb, out Set<string> nouns)
        {
            return _verbNouns.TryGetValue(verb, out nouns);
        }

        /// <summary>
        /// Tries to get the verbs for a given noun
        /// </summary>
        /// <param name="noun">Noun to get verbs for</param>
        /// <param name="verbs">Verbs corresponding to noun</param>
        /// <returns>True if verbs were found</returns>
        public bool TryGetVerbsForNoun(string noun, out Set<string> verbs)
        {
            return _nounVerbs.TryGetValue(noun, out verbs);
        }

        /// <summary>
        /// Gets whether or not NomBank contains a given noun in a range of sections
        /// </summary>
        /// <param name="noun">Noun to check</param>
        /// <param name="inSections">Sections to look in</param>
        /// <returns>True if noun in contained in one of the sections, false otherwise</returns>
        public bool ContainsMorphologicalVariantOf(string noun, params int[] inSections)
        {
            if (inSections == null)
                throw new Exception("Invalid section list");

            return ContainsMorphologicalVariantOf(noun, new List<int>(inSections));
        }

        /// <summary>
        /// Gets whether or not NomBank contains a given noun in a range of sections
        /// </summary>
        /// <param name="noun">Noun to check</param>
        /// <param name="inSections">Sections to look in</param>
        /// <returns>True if noun in contained in one of the sections, false otherwise</returns>
        public bool ContainsMorphologicalVariantOf(string noun, IEnumerable<int> inSections)
        {
            if (inSections == null)
                throw new Exception("Invalid section list");

            Set<ushort> sections;

            // try for direct containment
            if (_nounSections.TryGetValue(noun, out sections))
                foreach (ushort section in inSections)
                    if (sections.Contains(section))
                        return true;

            // check if token has entry in morphological dictionary
            Set<string> baseNouns;
            if (_morphNouns.TryGetValue(noun, out baseNouns))
                foreach (string baseNoun in baseNouns)
                    if (_nounSections.TryGetValue(baseNoun, out sections))
                        foreach (ushort section in inSections)
                            if (sections.Contains(section))
                                return true;
            return false;
        }

        /// <summary>
        /// Tries to get the base noun for a given noun. Succeeds only if base noun is unambiguous.
        /// </summary>
        /// <param name="noun">Noun to get base noun for</param>
        /// <param name="baseNoun">Base noun</param>
        /// <returns>True if base noun was found, false otherwise</returns>
        public bool TryGetBaseNoun(string noun, out string baseNoun)
        {
            baseNoun = null;

            if (Contains(noun))
                baseNoun = noun;
            else
            {
                // if the noun maps to a single base noun, return the base noun
                Set<string> baseNouns;
                if (_morphNouns.TryGetValue(noun, out baseNouns) && baseNouns.Count == 1)
                {
                    baseNoun = baseNouns.ElementAt(0);
                    if (!Contains(baseNoun))
                        baseNoun = null;
                }
            }

            return baseNoun != null;
        }

        /// <summary>
        /// Gets the Frame for a noun
        /// </summary>
        /// <param name="noun">Noun to get Frame for</param>
        /// <returns>Frame</returns>
        public Frame GetFrame(string noun)
        {
            return _nounFrame[noun];
        }

        /// <summary>
        /// Tries to get the frame for a noun
        /// </summary>
        /// <param name="noun">Noun to get frame for</param>
        /// <param name="frame">Frame for noun</param>
        /// <returns>True if frame was found, false otherwise</returns>
        public bool TryGetFrame(string noun, out Frame frame)
        {
            frame = null;

            string baseNoun;
            if (_nounFrame.ContainsKey(noun))
                frame = GetFrame(noun);
            else if (TryGetBaseNoun(noun, out baseNoun))
                frame = GetFrame(baseNoun);

            return frame != null;
        }

        /// <summary>
        /// Builds the mapping from nouns to their morphological variants as specified in nombank-morph.dict.1.0
        /// </summary>
        /// <param name="path">Path to the NomBank morphological variant dictionary</param>
        private void BuildMorphMap(string path)
        {
            if (!File.Exists(path))
                throw new Exception("Invalid NomBank morphological dictionary:  \"" + path + "\"");

            _nounMorphs = new Dictionary<string, Set<string>>();
            _morphNouns = new Dictionary<string, Set<string>>();

            // read dictionary
            StreamReader morphFile = new StreamReader(path);
            string line;
            while (morphFile.TryReadLine(out line))
            {
                string[] entry = line.Split(new char[] { ' ' }, StringSplitOptions.RemoveEmptyEntries);

                // first column is base noun
                string noun = entry[0];

                Set<string> nounMorphs;
                if (!_nounMorphs.TryGetValue(noun, out nounMorphs))
                {
                    nounMorphs = new Set<string>(false);
                    _nounMorphs.Add(noun, nounMorphs);
                }

                // variants start with second column
                for (int i = 1; i < entry.Length; ++i)
                {
                    string variant = entry[i];

                    // only use actual variants
                    if (variant == noun)
                        continue;

                    // add mapping from noun to variant
                    nounMorphs.Add(variant);

                    // add mapping from variant to noun
                    Set<string> morphNouns;
                    if (!_morphNouns.TryGetValue(variant, out morphNouns))
                    {
                        morphNouns = new Set<string>(false);
                        _morphNouns.Add(variant, morphNouns);
                    }

                    morphNouns.Add(noun);
                }
            }
        }

        /// <summary>
        /// Gets NomLex entries for noun and any of its morphological variants
        /// </summary>
        /// <param name="noun">Noun to get NomLex entries for</param>
        /// <returns>NomLex entries</returns>
        public List<NomLexEntry> GetNomLexEntries(string noun)
        {
            // try the plain noun first
            List<NomLexEntry> nomLexEntries;
            if (!_nomLexEngine.TryGetEntries(noun, out nomLexEntries))
            {
                nomLexEntries = new List<NomLexEntry>();

                // try all base nouns if we didn't find any entries for the given form
                Set<string> baseNouns;
                if (_morphNouns.TryGetValue(noun, out baseNouns))
                    foreach (string baseNoun in baseNouns)
                    {
                        List<NomLexEntry> entries;
                        if (_nomLexEngine.TryGetEntries(baseNoun, out entries))
                            nomLexEntries.AddRange(entries);
                    }
            }

            return nomLexEntries;
        }

        /// <summary>
        /// Removes nominalizations from a list that aren't contained in NomBank
        /// </summary>
        /// <param name="nominalizations">List of nominalizations to check</param>
        public void RemoveNominalizationsNotContained(Set<string> nominalizations)
        {
            if (nominalizations == null)
                throw new Exception("Invalid nominalization list");

            // get nominalizations to remove
            List<string> nomsToRemove = new List<string>();
            foreach (string nom in nominalizations)
                if (!Contains(nom))
                    nomsToRemove.Add(nom);

            foreach (string toRemove in nomsToRemove)
                nominalizations.Remove(toRemove);
        }

        /// <summary>
        /// Gets whether or not a token is markable (i.e., whether it is annotated in this NomBank)
        /// </summary>
        /// <param name="mrgFile">MRG file to check</param>
        /// <param name="sentence">Number of sentence to check</param>
        /// <param name="tokenNumber">Token number to check</param>
        /// <returns>True if token is markable, false otherwise</returns>
        public bool TokenIsMarkable(string mrgFile, int sentence, int tokenNumber)
        {
            // check query
            EnsureValidQuery(mrgFile, sentence, tokenNumber);

            // truncate mrg path to relative version
            mrgFile = MrgPath.GetRelativePathTo(mrgFile);

            // check if token is markable
            Dictionary<uint, Set<ushort>> sentMarkableTokens;
            Set<ushort> markableTokens;
            if (_mrgSentMarkableTokens.TryGetValue(mrgFile, out sentMarkableTokens) &&
                sentMarkableTokens.TryGetValue((uint)sentence, out markableTokens) &&
                markableTokens.Contains((ushort)tokenNumber))
                return true;

            return false;
        }

        /// <summary>
        /// Gets whether or not a token is a support verb
        /// </summary>
        /// <param name="mrgFile">MRG to file check</param>
        /// <param name="sentence">Number of sentence to check</param>
        /// <param name="tokenNumber">Token number to check</param>
        /// <returns>True if token is a support verb, false otherwise</returns>
        public bool TokenIsSupportVerb(string mrgFile, int sentence, int tokenNumber)
        {
            // check query
            EnsureValidQuery(mrgFile, sentence, tokenNumber);

            // truncate mrg path to relative version
            mrgFile = MrgPath.GetRelativePathTo(mrgFile);

            // check if token is support verb
            Dictionary<uint, Set<ushort>> sentSupportTokens;
            Set<ushort> supportTokens;
            if (_mrgSentSupportVerbTokens.TryGetValue(mrgFile, out sentSupportTokens) &&
                sentSupportTokens.TryGetValue((uint)sentence, out supportTokens) &&
                supportTokens.Contains((ushort)tokenNumber))
                return true;

            return false;
        }

        /// <summary>
        /// Gets full bracketing of a sentence
        /// </summary>
        /// <param name="mrgFile">MRG file to get sentence from</param>
        /// <param name="sentNum">Sentence number</param>
        /// <param name="options">Bracketing options</param>
        /// <returns>Full bracketing</returns>
        public string GetFullBracketing(string mrgFile, int sentNum, params FullBracketingOptions[] options)
        {
            if (options == null || options.Length == 0)
                throw new Exception("No bracketing options specified");

            // create option set for quick lookup
            Set<FullBracketingOptions> optionSet = new Set<FullBracketingOptions>(options);
            if (optionSet.Contains(FullBracketingOptions.IncludeAllInformation))
                optionSet = new Set<FullBracketingOptions>(new FullBracketingOptions[]{
                    FullBracketingOptions.BracketArguments, FullBracketingOptions.BracketModifiers,
                    FullBracketingOptions.BracketPredicates, FullBracketingOptions.BracketSupportVerbs,
                    FullBracketingOptions.IndicateNonMarkables, FullBracketingOptions.IncludeOriginalText});

            // make sure we have predicates for the sentence...if we don't, simply return the surface text if needed
            List<NounInfo> sentNounInfo;
            if (!TryGetNounInfoForSentence(mrgFile, sentNum, out sentNounInfo))
            {
                if (optionSet.Contains(FullBracketingOptions.IncludeOriginalText))
                    return GetParseTree(mrgFile, sentNum).SurfaceText;
                else
                    return "";
            }

            // lists of tokens and labels for each token across all predicates
            List<string> tokens = null;
            Dictionary<int, List<string>> tokenLabels = null;

            // read each predicate for the sentence
            foreach (NounInfo ni in sentNounInfo)
            {
                NomBankNode predTree = GetNomBankTree(ni);

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
                foreach (NomBankNode token in predTree.Tokens)
                {
                    // build label for token
                    string tokenLabel = "";

                    // get labels from current token as well as all ancestors
                    List<NomBankNodeLabel> labels = new List<NomBankNodeLabel>(token.Labels);
                    NomBankNode tokenRelative = token;
                    while ((tokenRelative = tokenRelative.Parent as NomBankNode) != null)
                        labels.AddRange(tokenRelative.Labels);

                    // add label for each label
                    foreach (NomBankNodeLabel label in labels)
                    {
                        string typeStr = label.Type.ToString();

                        if (optionSet.Contains(FullBracketingOptions.BracketArguments) && label.IsArgument)
                            tokenLabel += typeStr[0].ToString() + typeStr[typeStr.Length - 1].ToString() + ",";
                        else if (optionSet.Contains(FullBracketingOptions.BracketModifiers) && label.IsModifier)
                            tokenLabel += typeStr[0] + "-" + label.Feature.ToString() + ",";
                        else if (optionSet.Contains(FullBracketingOptions.BracketSupportVerbs) && label.IsSupportVerb)
                            tokenLabel += "Support,";
                        else if (optionSet.Contains(FullBracketingOptions.BracketPredicates) && label.IsPredicate)
                            tokenLabel += "Predicate,";
                    }

                    // add special label for non-markables
                    if (optionSet.Contains(FullBracketingOptions.IndicateNonMarkables))
                    {
                        bool markableCategory = token.Category == TreeBankEngine.SyntacticCategory.NounSingular || token.Category == TreeBankEngine.SyntacticCategory.NounPlural;
                        if (markableCategory &&
                            ContainsMorphologicalVariantOf(token.SurfaceText.ToLower()) &&
                            !TokenIsMarkable(mrgFile, sentNum, token.TokenNumber))
                            tokenLabel += "*";
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

            // init separator
            string separator = null;
            if (optionSet.Contains(FullBracketingOptions.HorizontalLineSeparators))
                separator = "".PadLeft(bracketedText.Length, '-');

            // only add newline if there is another line
            int numPreds = sentNounInfo.Count;
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

                // append separator
                if (optionSet.Contains(FullBracketingOptions.HorizontalLineSeparators))
                    bracketedText.Append(Environment.NewLine + separator);

                // only add newline if there is another line
                if (lineNum < numPreds - 1)
                    bracketedText.Append(Environment.NewLine);
            }

            return bracketedText.ToString();
        }
    }
}
