using System;
using System.Collections.Generic;
using System.Xml;
using System.IO;
using System.Text;

namespace RTParser.Utils
{
    /// <summary>
    /// A utility class for loading AIML files from disk into the graphmaster structure that 
    /// forms an AIML RProcessor's "brain"
    /// </summary>
    public class AIMLLoader
    {
        #region Attributes
        /// <summary>
        /// The RProcessor whose brain is being processed
        /// </summary>
        private RTParser.RTPBot RProcessor;

        /// <summary>
        /// Allow all chars in RawUserInput
        /// </summary>
        public bool RawUserInput = true;
        #endregion

        /// <summary>
        /// Ctor
        /// </summary>
        /// <param name="bot">The bot whose brain is being processed</param>
        public AIMLLoader(RTParser.RTPBot bot)
        {
            this.RProcessor = bot;
        }

        #region Methods

        /// <summary>
        /// Loads the AIML from files found in the RProcessor's AIMLpath into the RProcessor's brain
        /// </summary>
        public void loadAIML()
        {
            this.loadAIML(this.RProcessor.PathToAIML);
        }

        /// <summary>
        /// Loads the AIML from files found in the path
        /// </summary>
        /// <param name="path"></param>
        public void loadAIML(string path)
        {
            RProcessor.ReloadHooks.Add(() => loadAIML(path));
            if (Directory.Exists(path))
            {
                // process the AIML
                loadAIMLDir(path);
            } else if (File.Exists(path))
            {
                this.loadAIMLFile(path); 
            }
            else
            {
                this.loadAIMLURI(path);
            }
        }

        private void loadAIMLDir(string path)
        {
            this.RProcessor.writeToLog("Starting to process AIML files found in the directory " + path);

            string[] fileEntries = Directory.GetFiles(path, "*.aiml");
            if (fileEntries.Length > 0)
            {
                foreach (string filename in fileEntries)
                {
                    try
                    {
                        this.loadAIMLFile(filename);
                    } catch(Exception ee)
                    {
                        Console.WriteLine("" + ee);
                        RProcessor.writeToLog("Error in loadAIMLFile " + ee);
                    }
                }
                this.RProcessor.writeToLog("Finished processing the AIML files. " + Convert.ToString(this.RProcessor.Size) + " categories processed.");
            }
            else
            {
                this.RProcessor.writeToLog("Could not find any .aiml files in the specified directory (" + path + "). Please make sure that your aiml file end in a lowercase aiml extension, for example - myFile.aiml is valid but myFile.AIML is not.");
            }
        }

        private void loadAIMLURI(string path)
        {
            this.RProcessor.writeToLog("Processing AIML URI: " + path);
            XmlDocument doc = new XmlDocument();
            try
            {
                FileInfo fi = new FileInfo(path);
                if (fi.Exists)
                {
                    doc.Load(path);
                }
                else
                {
                    XmlTextReader reader = new XmlTextReader(path);
                    doc.Load(reader);
                }
            }
            catch (Exception e)
            {
                String s = "ERROR: XmlTextReader of AIML files (" + path + ")  threw " + e;
                throw new FileNotFoundException(s);
            }

            doc.Load(path);
            loadAIMLFromXML(doc, path);
        }

        /// <summary>
        /// Given the name of a file in the AIML path directory, attempts to load it into the 
        /// graphmaster
        /// </summary>
        /// <param name="filename">The name of the file to process</param>
        public void loadAIMLFile(string filename)
        {
            this.RProcessor.writeToLog("Processing AIML file: " + filename);
            
            // load the document
            XmlDocument doc = new XmlDocument();
            try
            {
                doc.Load(filename);
            } catch(Exception e)
            {
                if (Directory.Exists(filename))
                {
                    loadAIMLDir(filename);
                    return;
                }
                this.RProcessor.writeToLog("Error in AIML Stacktrace: " + filename + " " + e.StackTrace);
                this.RProcessor.writeToLog("Error in AIML file: " + filename + " Message " + e.Message);
                //return;
            }
            this.loadAIMLFromXML(doc, filename);
        }

        /// <summary>
        /// Given an XML document containing valid AIML, attempts to load it into the graphmaster
        /// </summary>
        /// <param name="doc">The XML document containing the AIML</param>
        /// <param name="filename">Where the XML document originated</param>
        public void loadAIMLFromXML(XmlDocument doc, string filename)
        {
            // Get a list of the nodes that are children of the <aiml> tag
            // these nodes should only be either <topic> or <category>
            // the <topic> nodes will contain more <category> nodes
            XmlNodeList rootChildren = doc.DocumentElement.ChildNodes;

            // process each of these child nodes
            foreach (XmlNode currentNode in rootChildren)
            {
                loadAIMLNode(currentNode, filename);
            }
        }

        private void loadAIMLNode(XmlNode currentNode, string filename)
        {
            if (currentNode.NodeType == XmlNodeType.Comment) return;
            if (currentNode.Name == "root")
            {
                // process each of these child "settings"? nodes
                foreach (XmlNode child in currentNode.ChildNodes)
                {
                    loadAIMLNode(child, filename);
                }
                return;
            }
            if (currentNode.Name == "item")
            {
                this.RProcessor.GlobalSettings.loadSettingNode(currentNode);
                return;
            }

            if (currentNode.Name == "topic")
            {
                this.processTopic(currentNode, filename);
            }
            else if (currentNode.Name == "category")
            {
                this.processCategory(currentNode, filename);
            }
            else
            {
                this.RProcessor.writeToLog("unused node in " + filename + ": " + currentNode.OuterXml);
            }
        }

        /// <summary>
        /// Given a "topic" node, processes all the categories for the topic and adds them to the 
        /// graphmaster "brain"
        /// </summary>
        /// <param name="node">the "topic" node</param>
        /// <param name="filename">the file from which this node is taken</param>
        private void processTopic(XmlNode node, string filename)
        {
            // find the name of the topic or set to default "*"
            Unifiable topicName="*";
            if((node.Attributes.Count==1)&(node.Attributes[0].Name=="name"))
            {
                topicName = node.Attributes["name"].Value;
            }

            // process all the category nodes
            foreach (XmlNode thisNode in node.ChildNodes)
            {
                if (thisNode.Name == "category")
                {
                    processCategory(thisNode, topicName, filename);
                }
            }
        }

        /// <summary>
        /// Adds a category to the graphmaster structure using the default topic ("*")
        /// </summary>
        /// <param name="node">the XML node containing the category</param>
        /// <param name="filename">the file from which this category was taken</param>
        private void processCategory(XmlNode node, string filename)
        {
            this.processCategory(node, Unifiable.STAR, filename);
        }

        /// <summary>
        /// Adds a category to the graphmaster structure using the given topic
        /// </summary>
        /// <param name="node">the XML node containing the category</param>
        /// <param name="topicName">the topic to be used</param>
        /// <param name="filename">the file from which this category was taken</param>
        private void processCategory(XmlNode node, Unifiable topicName, string filename)
        {
            Dictionary<string,List<XmlNode>> store = GetTopicStore();
            // reference and check the required nodes
            List<XmlNode> patterns = this.FindNodes("pattern", node);
            foreach (XmlNode pattern in patterns)
            {
                XmlNode template = this.FindNode("template", node);
                XmlNode guard = this.FindNode("guard", node);

                if (object.Equals(null, pattern))
                {
                    throw new XmlException("Missing pattern tag in a node found in " + filename);
                }
                if (object.Equals(null, template))
                {
                    throw new XmlException("Missing template tag in the node with pattern: " + pattern.InnerText + " found in " + filename);
                }

                Unifiable categoryPath = this.generatePath00(pattern, node, topicName, false);

                // o.k., add the processed AIML to the GraphMaster structure
                if (!categoryPath.IsEmpty)
                {
                    try
                    {
                        this.RProcessor.Graphmaster.addCategoryTag(categoryPath, template, guard, filename);
                        // keep count of the number of categories that have been processed
                        this.RProcessor.Size++;
                    }
                    catch
                    {
                        this.RProcessor.writeToLog("ERROR! Failed to load a new category into the graphmaster where the path = " + categoryPath + " and template = " + template.OuterXml + " produced by a category in the file: " + filename);
                    }
                }
                else
                {
                    this.RProcessor.writeToLog("WARNING! Attempted to load a new category with an empty pattern where the path = " + categoryPath + " and template = " + template.OuterXml + " produced by a category in the file: " + filename);
                }
            }
        }

        private Dictionary<string, List<XmlNode>> GetTopicStore()
        {
            return null;// throw new NotImplementedException();
        }

        /// <summary>
        /// Generates a path from a category XML node and topic name
        /// </summary>
        /// <param name="node">the category XML node</param>
        /// <param name="topicName">the topic</param>
        /// <param name="isUserInput">marks the path to be created as originating from user input - so
        /// normalize out the * and _ wildcards used by AIML</param>
        /// <returns>The appropriately processed path</returns>
        private Unifiable generatePath00(XmlNode pattern, XmlNode node, Unifiable topicName, bool isUserInput)
        {
            // get the nodes that we need
            XmlNode that = this.FindNode("that", node);

            Unifiable patternText;
            Unifiable thatText = Unifiable.STAR;
            if (object.Equals(null, pattern))
            {
                patternText = Unifiable.Empty;
            }
            else
            {
                patternText = Unifiable.Create(pattern);//.InnerXml;
            }
            if (!object.Equals(null, that))
            {
                thatText = that.InnerText;
            }

            return this.generatePath(patternText, thatText, topicName, isUserInput);
        }

        /// <summary>
        /// Given a name will try to find a node named "name" in the childnodes or return null
        /// </summary>
        /// <param name="name">The name of the node</param>
        /// <param name="node">The node whose children need searching</param>
        /// <returns>The node (or null)</returns>
        private XmlNode FindNode(string name, XmlNode node)
        {
            foreach(XmlNode child in node.ChildNodes)
            {
                if (child.Name == name)
                {
                    return child;
                }
            }
            return null;
        }
        private List<XmlNode> FindNodes(string name, XmlNode node)
        {
            List<XmlNode> nodes = new List<XmlNode>();
            foreach (XmlNode child in node.ChildNodes)
            {
                if (child.Name == name)
                {
                    nodes.Add(child);
                }
            }
            return nodes;
        }

        /// <summary>
        /// Generates a path from the passed arguments
        /// </summary>
        /// <param name="pattern">the pattern</param>
        /// <param name="that">the that</param>
        /// <param name="topicName">the topic</param>
        /// <param name="isUserInput">marks the path to be created as originating from user input - so
        /// normalize out the * and _ wildcards used by AIML</param>
        /// <returns>The appropriately processed path</returns>
        public Unifiable generatePath(Unifiable pattern, Unifiable that, Unifiable topicName, bool isUserInput)
        {
            // to hold the normalized path to be entered into the graphmaster
            Unifiable normalizedPath = Unifiable.CreateAppendable();
            string normalizedPattern;// = Unifiable.Empty;
            Unifiable normalizedThat;// = Unifiable.STAR;
            Unifiable normalizedTopic;// = Unifiable.STAR;

            if ((this.RProcessor.TrustAIML) & (!isUserInput || RawUserInput))
            {

                normalizedPattern = pattern.Trim();
                while (normalizedPattern.EndsWith("?") || normalizedPattern.EndsWith("."))
                {
                    normalizedPattern = normalizedPattern.Substring(0, normalizedPattern.Length - 1).Trim();
                }

                normalizedThat = that.Trim();
                normalizedTopic = topicName.Trim();
            }
            else
            {
                normalizedPattern = this.Normalize(pattern, isUserInput).Trim();
                normalizedThat = this.Normalize(that, isUserInput).Trim();
                normalizedTopic = this.Normalize(topicName, isUserInput).Trim();
            }

            // check sizes
            if (normalizedPattern.Length > 0)
            {
                if (normalizedThat.IsEmpty)
                {
                    normalizedThat = Unifiable.STAR;
                }
                if (normalizedTopic.IsEmpty)
                {
                    normalizedTopic = Unifiable.STAR;
                }

                // This check is in place to avoid huge "that" elements having to be processed by the 
                // graphmaster. 
                //if (normalizedThat.Length > this.RProcessor.MaxThatSize)
                //{
                //    normalizedThat = Unifiable.STAR;
                //}

                // o.k. build the path
                normalizedPath.Append(Unifiable.Create(normalizedPattern));
                normalizedPath.Append(Unifiable.ThatTag);
                normalizedPath.Append(normalizedThat);
                normalizedPath.Append(Unifiable.TopicTag);
                normalizedPath.Append(normalizedTopic);

                return normalizedPath;//.Frozen();
            }
            else
            {
                return Unifiable.Empty;
            }
        }

        /// <summary>
        /// Given an input, provide a normalized output
        /// </summary>
        /// <param name="input">The Unifiable to be normalized</param>
        /// <param name="isUserInput">True if the Unifiable being normalized is part of the user input path - 
        /// flags that we need to normalize out * and _ chars</param>
        /// <returns>The normalized Unifiable</returns>
        public Unifiable Normalize(string input, bool isUserInput)
        {
            input = input.Trim();
            while (input.EndsWith("?"))
            {
                input = input.Substring(0, input.Length - 1).Trim();
            }
            while (input.EndsWith("."))
            {
                input = input.Substring(0, input.Length - 1).Trim();
            }
            while (input.EndsWith("!"))
            {
                input = input.Substring(0, input.Length - 1).Trim();
            }
            if (isUserInput && false)
            {
                return input;
            }

            Unifiable result = Unifiable.CreateAppendable();

            // objects for normalization of the input
            Normalize.ApplySubstitutions substitutor = new RTParser.Normalize.ApplySubstitutions(this.RProcessor);
            Normalize.StripIllegalCharacters stripper = new RTParser.Normalize.StripIllegalCharacters(this.RProcessor);

            Unifiable substitutedInput = substitutor.Transform(input);
            // split the pattern into it's component words
            string[] substitutedWords = substitutedInput.AsString().Split(' ');

            // Normalize all words unless they're the AIML wildcards "*" and "_" during AIML loading
            foreach (Unifiable word in substitutedWords)
            {
                Unifiable normalizedWord;
                if (isUserInput)
                {
                    normalizedWord = stripper.Transform(word);
                }
                else
                {
                    if (word.IsWildCard())
                    {
                        normalizedWord = word;
                    }
                    else
                    {
                        normalizedWord = stripper.Transform(word);
                    }
                }
                result.Append(normalizedWord.Trim() + " ");
            }

            return result.ToString().Replace("  "," "); // make sure the whitespace is neat
        }
        #endregion
    }
}
