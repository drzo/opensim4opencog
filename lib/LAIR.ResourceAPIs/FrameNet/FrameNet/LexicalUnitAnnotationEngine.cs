using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.IO;

using LAIR.XML;
using LAIR.Extensions;

namespace LAIR.ResourceAPIs.FrameNet
{
    /// <summary>
    /// Lexical unit annotation engine
    /// </summary>
    public class LexicalUnitAnnotationEngine
    {
        private string _annotationDirectory;
        private FrameNetEngine.Version _version;

        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="annotationDirectory">Lexical unit annotation directory</param>
        /// <param name="version">FrameNet version</param>
        public LexicalUnitAnnotationEngine(string annotationDirectory, FrameNetEngine.Version version)
        {
            if (!Directory.Exists(annotationDirectory))
                throw new DirectoryNotFoundException("Invalid lexical unit annotation directory");

            _annotationDirectory = annotationDirectory;
            _version = version;
        }

        /// <summary>
        /// Gets annotations for a lexical unit
        /// </summary>
        /// <param name="frame">Frame for which we're getting annotations</param>
        /// <param name="lexicalUnitID">ID of lexical unit for which to get annotations</param>
        /// <returns>Annotation information</returns>
        public List<Attestation> GetAttestations(Frame frame, int lexicalUnitID)
        {
            List<Attestation> attestations = new List<Attestation>();

            // return nothing if no file exists
            string attestationFilePath = Path.Combine(_annotationDirectory, "lu" + lexicalUnitID + ".xml");
            if (!File.Exists(attestationFilePath))
                return attestations;

            // constraints to skip to FE layer
            Dictionary<string, string> feAttributeConstraints = new Dictionary<string, string>();
            feAttributeConstraints.Add("name", "FE");

            // constraints to skip to Target layer
            Dictionary<string, string> targetAttributeConstraints = new Dictionary<string, string>();
            targetAttributeConstraints.Add("name", "Target");

            // get all attestations
            XmlParser attestationP = new XmlParser(File.ReadAllText(attestationFilePath));

            if (_version == FrameNetEngine.Version.FrameNet_1_3)
            {
                string annotationSetXML;
                while ((annotationSetXML = attestationP.OuterXML("annotationSet")) != null)
                {
                    Attestation annotation = new Attestation();

                    // parser for entire annotation set
                    XmlParser annotationSetP = new XmlParser(annotationSetXML);

                    // first get sentence...it is below the annotation layers
                    annotation.Sentence = new XmlParser(annotationSetP.OuterXML("sentence")).ElementText("text").Trim();

                    #region get fe bindings
                    // parser is forward-only, so rewind
                    annotationSetP.Reset();

                    // get FE bindings
                    if (!annotationSetP.SkipToElement("layer", feAttributeConstraints))
                        throw new Exception("Failed to find FE layer in annotation set");

                    string feBindingXML = annotationSetP.OuterXML("layer");
                    XmlParser feBindingP = new XmlParser(feBindingXML);

                    // read off FE binding labels
                    string labelXML;
                    while ((labelXML = feBindingP.OuterXML("label")) != null)
                    {
                        XmlParser labelP = new XmlParser(labelXML);

                        // skip null instantiations, which don't have start/end values
                        if (!labelP.GetAttributeNames("label").Contains("start"))
                            continue;

                        // get annotated span of text
                        int feStart = int.Parse(labelP.AttributeValue("label", "start"));
                        int feEnd = int.Parse(labelP.AttributeValue("label", "end"));
                        string feText = annotation.Sentence.Substring(feStart, feEnd - feStart + 1);
                        AnnotatedSpan span = new AnnotatedSpan(feStart, feText);

                        // add FE binding...we shouldn't have to check for the existence of a frame element, but errors abound!
                        string feName = labelP.AttributeValue("label", "name");
                        if (frame.FrameElements.Contains(feName))
                        {
                            FrameElement fe = frame.FrameElements.Get(feName);
                            annotation.FrameElementBindings.EnsureContainsKey(fe, typeof(List<AnnotatedSpan>));
                            annotation.FrameElementBindings[fe].Add(span);
                        }
                    }
                    #endregion

                    #region targets
                    // get target annotation...reset parser...sometimes the target comes before the FE layer
                    annotationSetP.Reset();

                    if (!annotationSetP.SkipToElement("layer", targetAttributeConstraints))
                        throw new Exception("Failed to find target layer in annotation set");

                    // read all targets
                    XmlParser targetP = new XmlParser(annotationSetP.OuterXML("layer"));
                    while ((labelXML = targetP.OuterXML("label")) != null)
                    {
                        XmlParser labelP = new XmlParser(labelXML);
                        int targetStart = int.Parse(labelP.AttributeValue("label", "start"));
                        int targetEnd = int.Parse(labelP.AttributeValue("label", "end"));
                        string targetText = annotation.Sentence.Substring(targetStart, targetEnd - targetStart + 1);

                        annotation.Targets.Add(new AnnotatedSpan(targetStart, targetText));
                    }
                    #endregion

                    attestations.Add(annotation);
                }
            }
            else if (_version == FrameNetEngine.Version.FrameNet_1_5)
            {
                string sentenceXML;
                while ((sentenceXML = attestationP.OuterXML("sentence")) != null)
                {
                    Attestation annotation = new Attestation();

                    XmlParser sentenceP = new XmlParser(sentenceXML);
                    annotation.Sentence = sentenceP.ElementText("text").Trim();

                    #region get fe bindings
                    if (!sentenceP.SkipToElement("layer", feAttributeConstraints))
                        throw new Exception("Failed to find FE layer in annotation set");

                    // read off FE binding labels
                    XmlParser feBindingP = new XmlParser(sentenceP.OuterXML("layer"));
                    string labelXML;
                    while ((labelXML = feBindingP.OuterXML("label")) != null)
                    {
                        XmlParser labelP = new XmlParser(labelXML);

                        // skip null instantiations, which don't have start/end values
                        if (!labelP.GetAttributeNames("label").Contains("start"))
                            continue;

                        // get annotated span of text
                        int feStart = int.Parse(labelP.AttributeValue("label", "start"));
                        int feEnd = int.Parse(labelP.AttributeValue("label", "end"));
                        string feText = annotation.Sentence.Substring(feStart, feEnd - feStart + 1);
                        AnnotatedSpan span = new AnnotatedSpan(feStart, feText);

                        // add FE binding...we shouldn't have to check for the existence of a frame element, but errors abound!
                        int feID = int.Parse(labelP.AttributeValue("label", "feID"));
                        if (frame.FrameElements.Contains(feID))
                        {
                            FrameElement fe = frame.FrameElements.Get(feID);
                            annotation.FrameElementBindings.EnsureContainsKey(fe, typeof(List<AnnotatedSpan>));
                            annotation.FrameElementBindings[fe].Add(span);
                        }
                    }
                    #endregion

                    #region targets
                    // get target annotation...reset parser...sometimes the target comes before the FE layer
                    sentenceP.Reset();

                    if (!sentenceP.SkipToElement("layer", targetAttributeConstraints))
                        throw new Exception("Failed to find target layer in annotation set");

                    // read all targets
                    XmlParser targetP = new XmlParser(sentenceP.OuterXML("layer"));
                    while ((labelXML = targetP.OuterXML("label")) != null)
                    {
                        XmlParser labelP = new XmlParser(labelXML);
                        int targetEnd = int.Parse(labelP.AttributeValue("label", "end"));
                        int targetStart = int.Parse(labelP.AttributeValue("label", "start"));

                        // bug in framenet:  bad sentence
                        if (targetStart >= annotation.Sentence.Length || targetEnd >= annotation.Sentence.Length)
                            continue;

                        string targetText = annotation.Sentence.Substring(targetStart, targetEnd - targetStart + 1);

                        annotation.Targets.Add(new AnnotatedSpan(targetStart, targetText));
                    }
                    #endregion

                    attestations.Add(annotation);
                }
            }
            else
                throw new Exception("Unrecognized FrameNet version:  " + _version);

            return attestations;
        }
    }
}
