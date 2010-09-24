using System;
using System.Collections.Generic;
using System.Text;
using System.IO;
using System.Collections.ObjectModel;

using LAIR.XML;
using LAIR.Collections.Generic;
using LAIR.Extensions;

namespace LAIR.ResourceAPIs.FrameNet
{
    /// <summary>
    /// Provides indexing and retrieval functionality for the FrameNet database
    /// </summary>
    public class FrameNetEngine
    {
        private Dictionary<string, Frame> _frameNameFrame;
        private Dictionary<int, FrameElement> _frameElementIdFrameElement;
        private LexicalUnitAnnotationEngine _lexicalUnitAnnotationEngine;
        private Dictionary<string, Set<int>> _lexemeLexicalUnitIDs;
        private Dictionary<int, Frame> _lexicalUnitIdFrame;
        private Dictionary<string, Set<int>> _lexicalUnitLexicalUnitIDs;
        private Dictionary<int, LexicalUnit> _lexicalUnitIdLexicalUnit;

        /// <summary>
        /// Gets all frames
        /// </summary>
        public IEnumerable<Frame> Frames
        {
            get { return _frameNameFrame.Values; }
        }

        /// <summary>
        /// Gets all frame names
        /// </summary>
        public IEnumerable<string> FrameNames
        {
            get { return _frameNameFrame.Keys; }
        }

        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="frameXmlDirectory">Path to frame XML subdirectory (typically "frXML") of the FrameNet distribution</param>
        /// <param name="lexicalUnitAnnotationDirectory">Path to lexical unit XML subdirectory (typically "luXML") of the FrameNet distribution</param>
        public FrameNetEngine(string frameXmlDirectory, string lexicalUnitAnnotationDirectory)
        {
            _lexicalUnitAnnotationEngine = new LexicalUnitAnnotationEngine(lexicalUnitAnnotationDirectory);

            BuildFrameIndex(frameXmlDirectory);
        }

        /// <summary>
        /// Builds the frame index
        /// </summary>
        /// <param name="frameXmlDirectory">Frame XML directory</param>
        private void BuildFrameIndex(string frameXmlDirectory)
        {
            if (!Directory.Exists(frameXmlDirectory))
                throw new DirectoryNotFoundException("Invalid frame XML path");

            // get paths to frame and frame relation files
            string framesXmlPath = Path.Combine(frameXmlDirectory, "frames.xml");
            string frameRelationsPath = Path.Combine(frameXmlDirectory, "frRelation.xml");

            if (!File.Exists(framesXmlPath))
                throw new FileNotFoundException("Invalid frame XML path:  " + framesXmlPath);

            if (!File.Exists(frameRelationsPath))
                throw new FileNotFoundException("Invalid frame relation XML path:  " + frameRelationsPath);

            _frameNameFrame = new Dictionary<string, Frame>();
            _frameElementIdFrameElement = new Dictionary<int, FrameElement>();
            _lexemeLexicalUnitIDs = new Dictionary<string, Set<int>>();
            _lexicalUnitIdFrame = new Dictionary<int, Frame>();
            _lexicalUnitLexicalUnitIDs = new Dictionary<string, Set<int>>();
            _lexicalUnitIdLexicalUnit = new Dictionary<int, LexicalUnit>();

            #region get frames
            StreamReader frameXmlFile = new StreamReader(framesXmlPath);
            string framesXML = frameXmlFile.ReadToEnd();
            frameXmlFile.Close();

            Set<int> uniqueFrameIDCheck = new Set<int>();
            XmlParser framesP = new XmlParser(framesXML);
            while (framesP.SkipToElement("frame"))
            {
                // create frame
                string frameXML = framesP.OuterXML("frame");
                XmlParser frameP = new XmlParser(frameXML);
                int frameID = int.Parse(frameP.AttributeValue("frame", "ID"));
                string frameName = frameP.AttributeValue("frame", "name").ToLower().Trim();  // use lowercase for all frame names
                string frameDefinition = frameP.ElementText("definition");
                Frame frame = new Frame(frameName, frameDefinition, frameID);

                // add to frame index index
                _frameNameFrame.Add(frame.Name, frame);
                uniqueFrameIDCheck.Add(frame.ID);

                // get frame elements
                string fesXML = frameP.OuterXML("fes");
                XmlParser fesP = new XmlParser(fesXML);
                string feXML;
                while ((feXML = fesP.OuterXML("fe")) != null)
                {
                    // get frame element
                    XmlParser feParser = new XmlParser(feXML);
                    int feID = int.Parse(feParser.AttributeValue("fe", "ID"));
                    string feName = feParser.AttributeValue("fe", "name").Trim().ToLower();
                    string feDef = feParser.ElementText("definition");
                    FrameElement fe = new FrameElement(feID, feName, feDef, frame);
                    frame.FrameElements.Add(fe);

                    // add to index
                    _frameElementIdFrameElement.Add(fe.ID, fe);
                }

                // get lexical units
                string lusXML = frameP.OuterXML("lexunits");
                XmlParser lusParser = new XmlParser(lusXML);
                string luXML;
                while ((luXML = lusParser.OuterXML("lexunit")) != null)
                {
                    XmlParser luParser = new XmlParser(luXML);
                    int luID = int.Parse(luParser.AttributeValue("lexunit", "ID"));
                    string luName = luParser.AttributeValue("lexunit", "name");
                    luName = luName.Substring(0, luName.IndexOf('.'));
                    string luPos = luParser.AttributeValue("lexunit", "pos");
                    string luDef = luParser.ElementText("definition");

                    // get lexemes for this lexunit...we may get duplicates...don't worry about them
                    Set<Lexeme> lexemes = new Set<Lexeme>(false);
                    string lexemesXML = luParser.OuterXML("lexemes");
                    XmlParser lexemesP = new XmlParser(lexemesXML);
                    string lexemeXML;
                    while ((lexemeXML = lexemesP.OuterXML("lexeme")) != null)
                    {
                        XmlParser lexemeP = new XmlParser(lexemeXML);
                        string pos = lexemeP.AttributeValue("lexeme", "pos");
                        bool breakBefore = bool.Parse(lexemeP.AttributeValue("lexeme", "breakBefore"));
                        bool head = bool.Parse(lexemeP.AttributeValue("lexeme", "headword"));
                        string value = lexemeP.ElementText("lexeme");

                        lexemes.Add(new Lexeme(value, pos, breakBefore, head));
                    }

                    // create lexical unit and add to frame
                    LexicalUnit lexicalUnit = new LexicalUnit(luID, luName, luPos, luDef, lexemes);

                    frame.LexicalUnits.Add(lexicalUnit);

                    // add map from full lexeme string to lexical unit id
                    string lexemeString = lexicalUnit.ToString();
                    _lexemeLexicalUnitIDs.EnsureContainsKey(lexemeString, typeof(Set<int>), false);
                    _lexemeLexicalUnitIDs[lexemeString].Add(luID);

                    // add map from lexical unit to frame
                    _lexicalUnitIdFrame.Add(lexicalUnit.ID, frame);

                    // add map from lexical unit to lexical unit id
                    _lexicalUnitLexicalUnitIDs.EnsureContainsKey(lexicalUnit.Name, typeof(Set<int>));
                    _lexicalUnitLexicalUnitIDs[lexicalUnit.Name].Add(lexicalUnit.ID);

                    // add map from lexical unit ID to lexical unit
                    _lexicalUnitIdLexicalUnit.Add(lexicalUnit.ID, lexicalUnit);
                }
            }
            #endregion

            #region get frame relations
            frameXmlFile = new StreamReader(frameRelationsPath);
            string allRelationsXML = frameXmlFile.ReadToEnd();
            frameXmlFile.Close();
            framesP = new XmlParser(allRelationsXML);

            // read relations
            string relationsXML;
            while ((relationsXML = framesP.OuterXML("frame-relation-type")) != null)
            {
                // get relation type
                XmlParser relationsP = new XmlParser(relationsXML);
                Frame.FrameRelation relation = Frame.GetFrameRelation(relationsP.AttributeValue("frame-relation-type", "name"));

                string relationXML;
                while ((relationXML = relationsP.OuterXML("frame-relation")) != null)
                {
                    XmlParser relationP = new XmlParser(relationXML);
                    string superFrameName = relationP.AttributeValue("frame-relation", "superFrameName").ToLower();
                    string subFrameName = relationP.AttributeValue("frame-relation", "subFrameName").ToLower();

                    Frame superFrame = _frameNameFrame[superFrameName];
                    Frame subFrame = _frameNameFrame[subFrameName];

                    superFrame.GetSubFrames(relation).Add(subFrame);
                    subFrame.GetSuperFrames(relation).Add(superFrame);

                    // add FE relations
                    while (relationP.SkipToElement("fe-relation"))
                    {
                        int superFeID = int.Parse(relationP.AttributeValue("fe-relation", "supId"));
                        int subFeID = int.Parse(relationP.AttributeValue("fe-relation", "subId"));

                        FrameElement superFE = superFrame.FrameElements.Get(superFeID);
                        FrameElement subFE = subFrame.FrameElements.Get(subFeID);

                        superFE.AddSubFrameElement(subFE, relation);
                        subFE.AddSuperFrameElement(superFE, relation);
                    }
                }
            }
            #endregion
        }

        /// <summary>
        /// Gets a frame by name
        /// </summary>
        /// <param name="name">Name of frame to get</param>
        /// <returns>Frame</returns>
        public Frame GetFrame(string name)
        {
            name = name.ToLower();

            return _frameNameFrame[name];
        }

        /// <summary>
        /// Gets a frame element
        /// </summary>
        /// <param name="frameName">Frame for which to get frame element</param>
        /// <param name="frameElementName">Name of frame element to get</param>
        /// <returns>Frame element</returns>
        public FrameElement GetFrameElement(string frameName, string frameElementName)
        {
            return GetFrame(frameName).FrameElements.Get(frameElementName);
        }

        /// <summary>
        /// Gets a frame element
        /// </summary>
        /// <param name="frameElement">Frame element, in Frame.FrameElement notation</param>
        /// <returns>Frame element</returns>
        public FrameElement GetFrameElement(string frameElement)
        {
            int dotLoc = frameElement.IndexOf('.');

            return GetFrame(frameElement.Substring(0, dotLoc)).FrameElements.Get(frameElement.Substring(dotLoc + 1));
        }

        /// <summary>
        /// Checks whether or not a frame exists in the database
        /// </summary>
        /// <param name="name">Name of frame to check for</param>
        /// <returns>True if frame exists, false otherwise</returns>
        public bool Contains(string name)
        {
            name = name.ToLower();

            return _frameNameFrame.ContainsKey(name);
        }

        /// <summary>
        /// Gets all attestations for a frame
        /// </summary>
        /// <param name="frame">Frame to get attestations for</param>
        /// <returns>List of attestations</returns>
        public List<Attestation> GetAttestationsForFrame(Frame frame)
        {
            return GetAttestationsForFrame(frame, null);
        }

        /// <summary>
        /// Gets all attestations for a frame
        /// </summary>
        /// <param name="frame">Frame to get attestations for</param>
        /// <param name="pos">Part of speech for attestation</param>
        /// <returns>List of attestations</returns>
        public List<Attestation> GetAttestationsForFrame(Frame frame, string pos)
        {
            List<Attestation> attestations = new List<Attestation>();
            foreach (LexicalUnit lu in frame.LexicalUnits)
                if (pos == null || lu.POS == pos)
                    attestations.AddRange(_lexicalUnitAnnotationEngine.GetAttestations(frame, lu.ID));

            return attestations;
        }

        /// <summary>
        /// Gets the set of frames evoked by a lexeme
        /// </summary>
        /// <param name="lexeme">Lexeme to get frames for</param>
        /// <returns>Set of frames</returns>
        public Set<Frame> GetFramesForLexeme(string lexeme)
        {
            Set<Frame> frames = new Set<Frame>(false);

            Set<int> luIDs;
            if (_lexemeLexicalUnitIDs.TryGetValue(lexeme, out luIDs))
                foreach (int luID in luIDs)
                    frames.Add(_lexicalUnitIdFrame[luID]);
            else
                throw new Exception("FrameNet does not contain lexeme");

            return frames;
        }

        /// <summary>
        /// Gets the set of frames evoked by a lexical unit (includes frames for all contained lexemes)
        /// </summary>
        /// <param name="lexicalUnit">Lexical unit to get frames for</param>
        /// <returns>Set of frames</returns>
        public Set<Frame> GetFramesForLexicalUnit(string lexicalUnit)
        {
            Set<Frame> frames = new Set<Frame>(false);

            Set<int> luIDs;
            if (_lexicalUnitLexicalUnitIDs.TryGetValue(lexicalUnit, out luIDs))
                foreach (int luID in luIDs)
                    frames.Add(_lexicalUnitIdFrame[luID]);
            else
                throw new Exception("FrameNet does not contain lexical unit");

            return frames;
        }

        /// <summary>
        /// Checks whether or not a lexeme is in FrameNet
        /// </summary>
        /// <param name="lexeme">Lexeme to check</param>
        /// <returns>True if lexeme is contained, false otherwise</returns>
        public bool ContainsLexeme(string lexeme)
        {
            return _lexemeLexicalUnitIDs.ContainsKey(lexeme);
        }

        /// <summary>
        /// Checks whether or not a lexical unit is in FrameNet
        /// </summary>
        /// <param name="lexicalUnit"></param>
        /// <returns></returns>
        public bool ContainsLexicalUnit(string lexicalUnit)
        {
            return _lexicalUnitLexicalUnitIDs.ContainsKey(lexicalUnit);
        }
    }
}
