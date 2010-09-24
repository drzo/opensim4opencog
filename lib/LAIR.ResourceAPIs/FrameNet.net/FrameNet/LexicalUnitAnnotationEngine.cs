using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.IO;
using LAIR.XML;

namespace LAIR.ResourceAPIs.FrameNet
{
    /// <summary>
    /// Lexical unit annotation engine
    /// </summary>
    public class LexicalUnitAnnotationEngine
    {
        private string _annotationDirectory;

        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="annotationDirectory">Lexical unit annotation directory</param>
        public LexicalUnitAnnotationEngine(string annotationDirectory)
        {
            if (!Directory.Exists(annotationDirectory))
                throw new Exception("Invalid lexical unit annotation directory");

            _annotationDirectory = annotationDirectory;
        }

        /// <summary>
        /// Gets annotations for a lexical unit
        /// </summary>
        /// <param name="frame">Frame for which we're getting annotations</param>
        /// <param name="lexicalUnitID">ID of lexical unit for which to get annotations</param>
        /// <returns>Annotation information</returns>
        public List<Attestation> GetAttestations(Frame frame, int lexicalUnitID)
        {
            string attestationFilePath = Path.Combine(_annotationDirectory, "lu" + lexicalUnitID + ".xml");
            if (!File.Exists(attestationFilePath))
                throw new Exception("Could not find file for lexical unit ID " + lexicalUnitID);

            // constraints to skip to FE layer
            Dictionary<string, string> feBindingXMLConstraints = new Dictionary<string, string>();
            feBindingXMLConstraints.Add("name", "FE");

            // constraints to skip to Target layer
            Dictionary<string, string> targetXMLConstraints = new Dictionary<string, string>();
            targetXMLConstraints.Add("name", "Target");

            // read attestation XML
            StreamReader attestationFile = new StreamReader(attestationFilePath);
            string attestationXML = attestationFile.ReadToEnd();
            attestationFile.Close();
            var attestationP = new LAIR.CommonPort.CommonXmlParser(attestationXML);

            // get all attestations
            List<Attestation> attestations = new List<Attestation>();
            string annotationSetXML;
            while ((annotationSetXML = attestationP.OuterXML("annotationSet")) != null)
            {
                Attestation a = new Attestation();

                // parser for entire annotation set
                var annotationSetP = new LAIR.CommonPort.CommonXmlParser(annotationSetXML);

                // first get sentence...it is below the annotation layers
                string setenceXML = annotationSetP.OuterXML("sentence");
                var sentenceP = new LAIR.CommonPort.CommonXmlParser(setenceXML);
                a.Sentence = sentenceP.ElementText("text");

                #region fe bindings
                // parser is forward-only, so create another from the top
                annotationSetP = new LAIR.CommonPort.CommonXmlParser(annotationSetXML);

                // get FE bindings
                if (!annotationSetP.SkipToElement("layer", feBindingXMLConstraints))
                    throw new Exception("Failed to find FE layer in annotation set");

                string feBindingXML = annotationSetP.OuterXML("layer");
                var feBindingP = new LAIR.CommonPort.CommonXmlParser(feBindingXML);

                // read off FE binding labels
                string labelXML;
                while ((labelXML = feBindingP.OuterXML("label")) != null)
                {
                    var labelP = new LAIR.CommonPort.CommonXmlParser(labelXML);
                    string feName = labelP.AttributeValue("label", "name");

                    // ignore non-expression FEs (INI, DNI, CNI)
                    int feStart, feEnd;
                    try
                    {
                        feStart = int.Parse(labelP.AttributeValue("label", "start"));
                        feEnd = int.Parse(labelP.AttributeValue("label", "end"));
                    }
                    catch (Exception) { continue; }

                    string value = a.Sentence.Substring(feStart, feEnd - feStart + 1);
                    AnnotatedSpan span = new AnnotatedSpan(feStart, value);

                    // add FE binding...we shouldn't have to check for the existence of a frame element, but errors abound!
                    if (frame.FrameElements.Contains(feName))
                    {
                        FrameElement fe = frame.FrameElements.Get(feName);
                        if (!a.FrameElementBindings.ContainsKey(fe))
                            a.FrameElementBindings[fe] = new List<AnnotatedSpan>();

                        a.FrameElementBindings[fe].Add(span);
                    }
                }
                #endregion

                #region targets
                // get target annotation...reset parser...sometimes the target comes before the FE layer
                annotationSetP = new LAIR.CommonPort.CommonXmlParser(annotationSetXML);

                if (!annotationSetP.SkipToElement("layer", targetXMLConstraints))
                    throw new Exception("Failed to find target layer in annotation set");

                string targetXML = annotationSetP.OuterXML("layer");
                XmlParser targetP = new XmlParser(targetXML);
                while ((labelXML = targetP.OuterXML("label")) != null)
                {
                    var labelP = new LAIR.CommonPort.CommonXmlParser(labelXML);
                    int targetStart = int.Parse(labelP.AttributeValue("label", "start"));
                    int targetEnd = int.Parse(labelP.AttributeValue("label", "end"));
                    string targetVal = a.Sentence.Substring(targetStart, targetEnd - targetStart + 1);

                    AnnotatedSpan span = new AnnotatedSpan(targetStart, targetVal);

                    a.Targets.Add(span);
                }
                #endregion

                attestations.Add(a);
            }

            return attestations;
        }
    }
}
