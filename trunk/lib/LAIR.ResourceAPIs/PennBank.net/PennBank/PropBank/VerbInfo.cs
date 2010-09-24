using System;
using System.Collections.Generic;
using System.Text;
using System.Text.RegularExpressions;
using System.IO;

using LAIR.ResourceAPIs.PennBank.TreeBank;
using LAIR.Extensions;

namespace LAIR.ResourceAPIs.PennBank.PropBank
{
    /// <summary>
    /// Captures information in a single line of the prop.txt file.
    /// </summary>
    public class VerbInfo
    {
        #region static members
        private static Dictionary<string, Form> _strFormMap;
        private static Dictionary<Form, string> _formStrMap;
        private static Dictionary<string, Tense> _strTenseMap;
        private static Dictionary<Tense, string> _tenseStrMap;
        private static Dictionary<string, Aspect> _strAspectMap;
        private static Dictionary<Aspect, string> _aspectStrMap;
        private static Dictionary<string, Person> _strPersonMap;
        private static Dictionary<Person, string> _personStrMap;
        private static Dictionary<string, Voice> _strVoiceMap;
        private static Dictionary<Voice, string> _voiceStrMap;

        /// <summary>
        /// Person enumeration
        /// </summary>
        public enum Person { None, Third };

        /// <summary>
        /// Tense enumeration
        /// </summary>
        public enum Tense { None, Future, Past, Present };

        /// <summary>
        /// Aspect enumeration
        /// </summary>
        public enum Aspect { None, Perfect, Progressive, PerfectProgressive };

        /// <summary>
        /// Voice enumeration
        /// </summary>
        public enum Voice { None, Active, Passive };

        /// <summary>
        /// Form enumeration
        /// </summary>
        public enum Form { None, Infinitive, Gerund, Participle, Finite };

        /// <summary>
        /// Static constructor
        /// </summary>
        static VerbInfo()
        {
            _strFormMap = new Dictionary<string, Form>();
            _strFormMap.Add("i", Form.Infinitive);
            _strFormMap.Add("g", Form.Gerund);
            _strFormMap.Add("p", Form.Participle);
            _strFormMap.Add("v", Form.Finite);
            _strFormMap.Add("-", Form.None);
            _formStrMap = new Dictionary<Form, string>();
            foreach (string s in _strFormMap.Keys)
                _formStrMap.Add(_strFormMap[s], s);

            _strTenseMap = new Dictionary<string, Tense>();
            _strTenseMap.Add("f", Tense.Future);
            _strTenseMap.Add("p", Tense.Past);
            _strTenseMap.Add("n", Tense.Present);
            _strTenseMap.Add("-", Tense.None);
            _tenseStrMap = new Dictionary<Tense, string>();
            foreach (string s in _strTenseMap.Keys)
                _tenseStrMap.Add(_strTenseMap[s], s);

            _strAspectMap = new Dictionary<string, Aspect>();
            _strAspectMap.Add("p", Aspect.Perfect);
            _strAspectMap.Add("o", Aspect.Progressive);
            _strAspectMap.Add("b", Aspect.PerfectProgressive);
            _strAspectMap.Add("-", Aspect.None);
            _aspectStrMap = new Dictionary<Aspect, string>();
            foreach (string s in _strAspectMap.Keys)
                _aspectStrMap.Add(_strAspectMap[s], s);

            _strPersonMap = new Dictionary<string, Person>();
            _strPersonMap.Add("3", Person.Third);
            _strPersonMap.Add("-", Person.None);
            _personStrMap = new Dictionary<Person, string>();
            foreach (string s in _strPersonMap.Keys)
                _personStrMap.Add(_strPersonMap[s], s);

            _strVoiceMap = new Dictionary<string, Voice>();
            _strVoiceMap.Add("a", Voice.Active);
            _strVoiceMap.Add("p", Voice.Passive);
            _strVoiceMap.Add("-", Voice.None);
            _voiceStrMap = new Dictionary<Voice, string>();
            foreach (string s in _strVoiceMap.Keys)
                _voiceStrMap.Add(_strVoiceMap[s], s);
        }

        /// <summary>
        /// RE for proposition entries
        /// </summary>
        private static Regex _propLineRE = new Regex(@"^(?<file_name>[^\s]+)\s" +
                                                     @"(?<sent_num>[^\s]+)\s" +
                                                     @"(?<leaf_num>[^\s]+)\s" +
                                                     @"(?<tagger>[^\s]+)\s" +
                                                     @"(?<role_set>[^\s]+)\s" +
                                                     @"(?<form>.)(?<tense>.)(?<aspect>.)(?<person>.)(?<voice>.)\s" +
                                                     @"(?<labelLocations>.+)$");
        #endregion

        private string _verb;
        private string _file;
        private int _sentenceNumber;
        private int _leafNumber;
        private string _labeledNodeLocations;
        private Person _vPerson;
        private Tense _vTense;
        private Aspect _vAspect;
        private Voice _vVoice;
        private Form _vForm;
        private int _roleSetID;
        private string _tagger;
        private Frame _verbFrame;

        /// <summary>
        /// Gets or sets the frame for this information
        /// </summary>
        public Frame VerbFrame
        {
            get { return _verbFrame; }
            set { _verbFrame = value; }
        }

        /// <summary>
        /// Tagger for this verb
        /// </summary>
        public string Tagger
        {
            get { return _tagger; }
            set { _tagger = value; }
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
        /// Gets or sets the person
        /// </summary>
        public Person VPerson
        {
            get { return _vPerson; }
            set { _vPerson = value; }
        }

        /// <summary>
        /// Gets or sets the tense
        /// </summary>
        public Tense VTense
        {
            get { return _vTense; }
            set { _vTense = value; }
        }

        /// <summary>
        /// Gets or sets the aspect
        /// </summary>
        public Aspect VAspect
        {
            get { return _vAspect; }
            set { _vAspect = value; }
        }

        /// <summary>
        /// Gets or sets the voice
        /// </summary>
        public Voice VVoice
        {
            get { return _vVoice; }
            set { _vVoice = value; }
        }

        /// <summary>
        /// Gets or sets the form
        /// </summary>
        public Form VForm
        {
            get { return _vForm; }
            set { _vForm = value; }
        }

        /// <summary>
        /// Gets or sets the labeled node locations
        /// </summary>
        public string LabeledNodeLocations
        {
            get { return _labeledNodeLocations; }
            set { _labeledNodeLocations = value; }
        }

        /// <summary>
        /// Gets or sets the verb
        /// </summary>
        public string Verb
        {
            get { return _verb; }
            set { _verb = value; }
        }

        /// <summary>
        /// Gets or sets the file
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
        public VerbInfo()
        {
            _roleSetID = -1;
        }

        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="propLine">Prop entry line</param>
        public VerbInfo(string propLine)
            : this()
        {
            Match propLineMatch = _propLineRE.Match(propLine);
            if (!propLineMatch.Success)
                throw new Exception("Failed to match proposition line");

            // verb role set example:  hit.01 - when automatically tagging, the predicate might end up with a period in it (probably an SRL error), so use last period
            string verbRoleSet = propLineMatch.Groups["role_set"].Value;
            int roleSetLastPeriodLoc = verbRoleSet.LastIndexOf('.');
            string roleSet = verbRoleSet.Substring(roleSetLastPeriodLoc + 1);

            // set verb and role set ID
            _verb = verbRoleSet.Substring(0, roleSetLastPeriodLoc);
            _roleSetID = roleSet != "XX" ? int.Parse(roleSet) : -1;

            // get other information
            _file = propLineMatch.Groups["file_name"].Value.Replace('/', Path.DirectorySeparatorChar);  // use environment-specific path separators
            _sentenceNumber = int.Parse(propLineMatch.Groups["sent_num"].Value);
            _leafNumber = int.Parse(propLineMatch.Groups["leaf_num"].Value);
            _tagger = propLineMatch.Groups["tagger"].Value;
            _vForm = _strFormMap[propLineMatch.Groups["form"].Value];
            _vTense = _strTenseMap[propLineMatch.Groups["tense"].Value];
            _vAspect = _strAspectMap[propLineMatch.Groups["aspect"].Value];
            _vPerson = _strPersonMap[propLineMatch.Groups["person"].Value];
            _vVoice = _strVoiceMap[propLineMatch.Groups["voice"].Value];
            _labeledNodeLocations = propLineMatch.Groups["labelLocations"].Value;
        }

        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="verb">Verb</param>
        /// <param name="file">Annotation file</param>
        /// <param name="sentenceNumber">Sentence wichin annotation file</param>
        /// <param name="leafNumber">Leaf within sentence</param>
        /// <param name="tagger">Tagger used</param>
        /// <param name="roleSetID">ID of role set for this instance</param>
        /// <param name="form">Form flag</param>
        /// <param name="tense">Tense flag</param>
        /// <param name="aspect">Aspect flag</param>
        /// <param name="person">Person flag</param>
        /// <param name="voice">Voice flag</param>
        /// <param name="labeledNodeLocations">Labeled node locations</param>
        public VerbInfo(string verb, string file, int sentenceNumber, int leafNumber,
                        string tagger, int roleSetID, string form, string tense,
                        string aspect, string person, string voice, string labeledNodeLocations)
            : this()
        {
            _verb = verb;
            _file = file;
            _sentenceNumber = sentenceNumber;
            _leafNumber = leafNumber;
            _tagger = tagger;
            _roleSetID = roleSetID;
            _vForm = form != null ? _strFormMap[form] : Form.None;
            _vTense = tense != null ? _strTenseMap[tense] : Tense.None;
            _vAspect = aspect != null ? _strAspectMap[aspect] : Aspect.None;
            _vPerson = person != null ? _strPersonMap[person] : Person.None;
            _vVoice = voice != null ? _strVoiceMap[voice] : Voice.None;
            _labeledNodeLocations = labeledNodeLocations;
        }

        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="verb">Verb</param>
        /// <param name="file">Annotation file</param>
        /// <param name="sentenceNumber">Sentence wichin annotation file</param>
        /// <param name="leafNumber">Leaf within sentence</param>
        /// <param name="tagger">Tagger used</param>
        /// <param name="roleSetID">ID of role set for this instance</param>
        /// <param name="form">Form flag</param>
        /// <param name="tense">Tense flag</param>
        /// <param name="aspect">Aspect flag</param>
        /// <param name="person">Person flag</param>
        /// <param name="voice">Voice flag</param>
        /// <param name="labeledNodeLocations">Labeled node locations</param>
        public VerbInfo(string verb, string file, int sentenceNumber, int leafNumber,
                        string tagger, int roleSetID, Form form, Tense tense,
                        Aspect aspect, Person person, Voice voice, string labeledNodeLocations)
        {
            _verb = verb;
            _file = file;
            _sentenceNumber = sentenceNumber;
            _leafNumber = leafNumber;
            _tagger = tagger;
            _roleSetID = roleSetID;
            _vForm = form;
            _vTense = tense;
            _vAspect = aspect;
            _vPerson = person;
            _vVoice = voice;
            _labeledNodeLocations = labeledNodeLocations;
        }

        /// <summary>
        /// Gets the prop entry for this verb info
        /// </summary>
        /// <param name="treeBankPath">Path to TreeBank upon which this information is based. Pass null to use the entire file path
        /// contained in this information.</param>
        /// <returns>Prop entry line</returns>
        public string GetPropEntry(string treeBankPath)
        {
            string propEntry = (treeBankPath != null ? treeBankPath.GetRelativePathTo(_file) : _file) + " " + _sentenceNumber + " " + _leafNumber + " " +
                               _tagger + " " + _verb + "." + (_roleSetID < 1 ? "XX" : _roleSetID.ToString()) + " " +
                               _formStrMap[_vForm] + _tenseStrMap[_vTense] + _aspectStrMap[_vAspect] +
                               _personStrMap[_vPerson] + _voiceStrMap[_vVoice] + " " + _labeledNodeLocations;

            return propEntry;
        }
    }
}
