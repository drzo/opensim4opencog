using System;
using System.Collections.Generic;
using System.Xml;
using System.IO;
using System.Text;
using System.Threading;
using MushDLR223.Utilities;


namespace AltAIMLbot.Utils
{
    /// <summary>
    /// A utility class for loading AIML files from disk into the graphmaster structure that 
    /// forms an AIML bot's "brain"
    /// </summary>
    public class AIMLLoader
    {
        #region Attributes
        /// <summary>
        /// The bot whose brain is being processed
        /// </summary>
        private AltAIMLbot.AltBot bot;
        public string graphName="*";
        ExternDB extDB = null;
        public static bool SeekOutAndRepair = false;

        #endregion

        /// <summary>
        /// Ctor
        /// </summary>
        /// <param name="bot">The bot whose brain is being processed</param>
        public AIMLLoader(AltAIMLbot.AltBot bot)
        {
            this.bot = bot;
        }

        #region Methods

        /// <summary>
        /// Loads the AIML from files found in the bot's AIMLpath into the bot's brain
        /// </summary>
        public void loadAIML()
        {
            this.loadAIML(this.bot.PathToAIML);
        }

        /// <summary>
        /// Loads the AIML from files found in the path
        /// </summary>
        /// <param name="path"></param>
        public void loadAIML(string path)
        {
            if (Directory.Exists(path))
            {
                // process the AIML
                this.bot.writeToLog("Starting to process AIML files found in the directory " + path);

                string[] fileEntries = Directory.GetFiles(path, "*.aiml");
                if (fileEntries.Length > 0)
                {
                    foreach (string filename in fileEntries)
                    {
                        this.loadAIMLFile(filename);
                    }
                    this.bot.writeToLog("Finished processing the AIML files. " + Convert.ToString(this.bot.Size) + " categories processed.");
                }
                else
                {
                    throw new FileNotFoundException("Could not find any .aiml files in the specified directory (" + path + "). Please make sure that your aiml file end in a lowercase aiml extension, for example - myFile.aiml is valid but myFile.AIML is not.");
                }
            }
            else
            {
                if (false) throw new FileNotFoundException("The directory specified as the path to the AIML files (" + path + ") cannot be found by the AIMLLoader object. Please make sure the directory where you think the AIML files are to be found is the same as the directory specified in the settings file.");
                loadAIMLFile(path);
            }
        }

        /// <summary>
        /// Given the name of a file in the AIML path directory, attempts to load it into the 
        /// graphmaster
        /// </summary>
        /// <param name="filename">The name of the file to process</param>
        public void loadAIMLFile(string filename)
        {
            //lock (ExternDB.mylock)
            {

                this.bot.writeToLog("Processing AIML file: " + filename);
                if (this.bot.rapStoreDirectory != null)
                {
                    if (extDB != null)
                    {
                        extDB.Close();
                        extDB = null;
                    }
                    if (extDB == null)
                    {
                        extDB = new ExternDB(this.bot.rapStoreDirectory);
                        extDB.bot = this.bot;
                    }
                }
                XmlTextReader reader = new XmlTextReader(filename);
                XmlDocument doc = new XmlDocument();
                try
                {
                    // load the document
                    //doc.Load(filename);
                    doc.Load(reader);
                    this.loadAIMLFromXML(doc, filename);
                }

                catch (XmlException e)
                {
                    Console.WriteLine("================= XML ERROR ==========================");
                    Console.WriteLine(" FILENAME:" + filename);
                    if (reader != null)
                    {
                        Console.WriteLine("XmlReader Line, pos: (" + reader.LineNumber + "," + reader.LinePosition + ")");
                        Console.WriteLine("XmlReader Value, value: (" + reader.Value + ")");
                        Console.WriteLine("XmlReader Value, LocalName: (" + reader.LocalName + ")");
                        Console.WriteLine("XmlReader Value, ReadState: (" + reader.ReadState.ToString() + ")");
                    }
                    if ((doc != null) && (doc.ParentNode != null))
                    {
                        Console.WriteLine("doc, ParentNode: (" + doc.ParentNode.ToString() + ")");
                    }
                    Console.WriteLine(" XmlException Error:" + e.Message + " " + e.StackTrace);
                    Console.WriteLine(" Exception object Line, pos: (" + e.LineNumber + "," + e.LinePosition + ")");
                    Console.WriteLine(" XmlReader Line, source: (" + e.Source + ")");
                    Console.WriteLine(" XmlException Source:" + e.Source);
                    if ((e.InnerException != null) && (e.InnerException.Message != null))
                    {
                        Console.WriteLine(" XmlException InnerException.Message:" + e.InnerException.Message);

                    }
                    Console.WriteLine("================= XML ERROR ==========================");

                    Thread.Sleep(5000);
                }
                catch (Exception e)
                {
                    Console.WriteLine("General Error:" + e.Message + " " + e.StackTrace);
                    if (reader != null)
                    {
                        Console.WriteLine("XmlReader Line, pos: (" + reader.LineNumber + "," + reader.LinePosition + ")");
                        Console.WriteLine("XmlReader Value, value: (" + reader.Value + ")");
                        Console.WriteLine("XmlReader Value, LocalName: (" + reader.LocalName + ")");
                        Console.WriteLine("XmlReader Value, ReadState: (" + reader.ReadState.ToString() + ")");
                    }
                    if ((doc != null) && (doc.ParentNode != null))
                    {
                        Console.WriteLine("doc, ParentNode: (" + doc.ParentNode.ToString() + ")");
                    }
                    Console.WriteLine(" XmlReader Line, pos: (" + reader.LineNumber + "," + reader.LinePosition + ")");
                    Console.WriteLine(" XmlReader Line, source: (" + e.Source + ")");
                    Console.WriteLine(" XmlException Source:" + e.Source);
                    if ((e.InnerException != null) && (e.InnerException.Message != null))
                    {
                        Console.WriteLine(" XmlException InnerException.Message:" + e.InnerException.Message);

                    }
                    Thread.Sleep(5000);
                }
                //extDB.SaveIndex();
                if (extDB != null)
                {
                    extDB.Close();
                    extDB = null;
                }
            }
        }

        /// <summary>
        /// Given an XML document containing valid AIML, attempts to load it into the graphmaster
        /// </summary>
        /// <param name="doc">The XML document containing the AIML</param>
        /// <param name="filename">Where the XML document originated</param>
        public void loadAIMLFromXML(XmlDocument doc, string filename)
        {
            lock (ExternDB.mylock)
            {

                if (this.bot.rapStoreDirectory != null)
                {
                    if (extDB != null)
                    {
                        //     extDB.Close();
                        //     extDB = null;
                    }
                    if (extDB == null)
                    {
                        extDB = new ExternDB(this.bot.rapStoreDirectory);
                        extDB.bot = this.bot;
                    }
                }
                // Get a list of the nodes that are children of the <aiml> tag
                // these nodes should only be either <topic> or <category>
                // the <topic> nodes will contain more <category> nodes
                loadAIMLFromXML(doc.DocumentElement, filename);
            }
        }

        public void loadAIMLFromXML(XmlNode doc, string filename)
        {
            {
                XmlNodeList rootChildren = doc.ChildNodes;

                // find the name of the graph or set to default "*"
                string graphName = StaticXMLUtils.GetAttribValue(doc, "graph,name", "*");                              
                {
                    this.graphName = graphName;
                }
                if (this.bot.rapStoreDirectory != null)
                {
                    if ((filename.Contains("\\") || filename.Contains("/")) && (extDB.wasLoaded(filename)))
                    {
                        // We loaded that file
                        extDB.Close();
                        extDB = null;
                        return;
                    }
                    else
                    {
                        extDB._dbdir = this.bot.rapStoreDirectory;
                        if (this.bot.rapStoreSlices > 0) extDB.slices = this.bot.rapStoreSlices;
                        if (this.bot.rapStoreTrunkLevel > 0) extDB.trunkLevel = this.bot.rapStoreTrunkLevel;
                        extDB.OpenAll();
                    }
                }
                // process each of these child nodes
                foreach (XmlNode currentNode in rootChildren)
                {
                    if (currentNode.Name == "ser")
                    {
                        processSer(currentNode, filename);
                        continue;
                    }
                    if (currentNode.Name == "topic")
                    {
                        this.processTopic(currentNode, filename);
                    }
                    if (currentNode.Name == "state")
                    {
                        this.processState(currentNode, filename);
                    }
                    else if (currentNode.Name == "category")
                    {
                        this.processCategory(currentNode, filename);
                    }
                    if ((currentNode.Name == "behavior") || (currentNode.Name == "rbehavior"))
                    {
                        processImmediate(currentNode, filename);
                    }
                    if (currentNode.Name == "crontag")
                    {
                        processImmediate(currentNode, filename);
                    }
                    if (currentNode.Name == "scxml")
                    {
                        processImmediate(currentNode, filename);
                    }
                    if (currentNode.Name == "task")
                    {
                        processImmediate(currentNode, filename);
                    }
                    if (currentNode.Name == "subaiml")
                    {
                        processImmediate(currentNode, filename);
                    }
                }

                if (this.bot.rapStoreDirectory != null)
                {
                    extDB.rememberLoaded(filename);
                    extDB.Close();
                    extDB = null;
                }
            }
        }

        /// <summary>
        /// Processes certain normal "template" side commands at the top level
        /// </summary>
        /// <param name="node">the "topic" node</param>
        /// <param name="filename">the file from which this node is taken</param>

        private void processImmediate(XmlNode node, string filename)
        {
            try
            {
                this.bot.evalTemplateNode(node);
            }
            catch (Exception e)
            {
                this.bot.writeToLog("WARNING! Error "+e.Message +" Stack="+e.StackTrace + " and Code = " + node.OuterXml + " produced by a code in the file: " + filename);
 
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
            string topicName="*";
            string stateNamePre = "*";
            string stateNamePost = "*";
            if ((node.Attributes.Count == 1) && (node.Attributes[0].Name == "name"))
            {
                topicName = node.Attributes["name"].Value;
            }
            foreach (XmlAttribute Attrib in node.Attributes)
            {
                if (Attrib.Name == "name") { topicName = node.Attributes["name"].Value; }
                if (Attrib.Name == "state") { stateNamePre = node.Attributes["state"].Value; }
                if (Attrib.Name == "topic") { topicName = node.Attributes["topic"].Value; }
                if (Attrib.Name == "prestate") { stateNamePre = node.Attributes["prestate"].Value; }
                if (Attrib.Name == "poststate") { stateNamePost = node.Attributes["poststate"].Value; }
            }
            // process all the category nodes
            foreach (XmlNode thisNode in node.ChildNodes)
            {
                if (thisNode.Name == "category")
                {
                    processCategory(thisNode, topicName, stateNamePre, stateNamePost, filename);
                }
                if ((thisNode.Name == "behavior")||(thisNode.Name == "rbehavior"))
                {
                    processImmediate(thisNode, filename);
                }
                if (thisNode.Name == "crontag")
                {
                    processImmediate(thisNode, filename);
                }
                if (thisNode.Name == "scxml")
                {
                    processImmediate(thisNode, filename);
                }
                if (thisNode.Name == "task")
                {
                    processImmediate(thisNode, filename);
                }
                if (thisNode.Name == "subaiml")
                {
                    processImmediate(thisNode, filename);
                }
                if (thisNode.Name == "ser")
                {
                    processSer(thisNode, filename);
                }
            }
        }

        /// <summary>
        /// Given a "state" node, processes all the categories for the state and adds them to the 
        /// graphmaster "brain"
        /// </summary>
        /// <param name="node">the "topic" node</param>
        /// <param name="filename">the file from which this node is taken</param>
        private void processState(XmlNode node, string filename)
        {
            // find the name of the topic or set to default "*"
            string stateNamePre = "*";
            string stateNamePost = "*";
            string topicName = "*";
            if ((node.Attributes.Count == 1) && (node.Attributes[0].Name == "name"))
            {
                stateNamePre = node.Attributes["name"].Value;
            }
            foreach (XmlAttribute Attrib in node.Attributes)
            {
                if (Attrib.Name == "name") { stateNamePre = node.Attributes["name"].Value; }
                if (Attrib.Name == "state") { stateNamePre = node.Attributes["state"].Value; }
                if (Attrib.Name == "topic") { topicName = node.Attributes["topic"].Value; }
                if (Attrib.Name == "prestate") { stateNamePre = node.Attributes["prestate"].Value; }
                if (Attrib.Name == "poststate") { stateNamePost = node.Attributes["poststate"].Value; }
            }

            // process all the category nodes
            foreach (XmlNode thisNode in node.ChildNodes)
            {
                if (thisNode.Name == "category")
                {
                    processCategory(thisNode, topicName, stateNamePre,stateNamePost, filename);
                }
                if ((thisNode.Name == "behavior")||(thisNode.Name == "rbehavior"))
                {
                    processImmediate(thisNode, filename);
                }
                if (thisNode.Name == "crontag")
                {
                    processImmediate(thisNode, filename);
                }
                if (thisNode.Name == "scxml")
                {
                    processImmediate(thisNode, filename);
                }
                if (thisNode.Name == "task")
                {
                    processImmediate(thisNode, filename);
                }
                if (thisNode.Name == "subaiml")
                {
                    processImmediate(thisNode, filename);
                }
                if (thisNode.Name == "ser")
                {
                    processSer(thisNode, filename);
                }
            }
        }
        /// <summary>
        /// Adds a preprocessed path and content to the  graphmaster structure
        /// </summary>
        /// <param name="node">the XML node containing the category</param>
        /// <param name="filename">the file from which this category was taken</param>
        private void processSer(XmlNode node, string filename)
        {
            GraphMaster ourGraphMaster = this.bot.GetGraph(this.graphName) ?? this.bot.Graphmaster;

            XmlNode template = this.FindNode("template", node);
            string categoryPath = "";
            
            categoryPath = node.Attributes["path"].Value;
            if ((categoryPath ==null) || (categoryPath.Length ==0)) return;
            categoryPath = categoryPath.Trim();
            if ((categoryPath == null) || (categoryPath.Length == 0)) return;

            if (object.Equals(null, template))
            {
                this.bot.writeToLog("Missing template tag in the node with path: " + categoryPath + " found in " + filename);
                return;
            }

            // o.k., add the processed AIML to the GraphMaster structure
            if (categoryPath.Length > 0)
            {
                try
                {
                    // //this.bot.Graphmaster.addCategory(categoryPath, template.OuterXml, filename, 1, 1);
                    //ourGraphMaster.addCategory(categoryPath, template.OuterXml, filename, 1, 1);

                    if (this.bot.rapStoreDirectory != null)
                    {
                        Node.addCategoryDB("", categoryPath, template.OuterXml, filename, 1, 1, "", extDB);
                    }
                    else
                    {
                        ourGraphMaster.addCategory(categoryPath, template.OuterXml, filename, 1, 1);
                    }

                    
                    // keep count of the number of categories that have been processed
                    this.bot.Size++;
                }
                catch
                {
                    this.bot.writeToLog("ERROR! Failed to load a new category into the graphmaster where the path = " + categoryPath + " and template = " + template.OuterXml + " produced by a category in the file: " + filename);
                }
            }
            else
            {
                this.bot.writeToLog("WARNING! Attempted to load a new category with an empty pattern where the path = " + categoryPath + " and template = " + template.OuterXml + " produced by a category in the file: " + filename);
            }
        }

        /// <summary>
        /// Adds a category to the graphmaster structure using the default topic ("*")
        /// </summary>
        /// <param name="node">the XML node containing the category</param>
        /// <param name="filename">the file from which this category was taken</param>
        private void processCategory(XmlNode node, string filename)
        {
            this.processCategory(node, "*","*","*", filename);
        }

        /// <summary>
        /// Adds a category to the graphmaster structure using the given topic
        /// </summary>
        /// <param name="node">the XML node containing the category</param>
        /// <param name="topicName">the topic to be used</param>
        /// <param name="stateNamePre">the state in the front</param>
        /// <param name="stateNamePost">the state at the end (tie breaker)</param>
        /// <param name="filename">the file from which this category was taken</param>
        private void processCategory(XmlNode node, string topicName, string stateNamePre, string stateNamePost, string filename)
        {
            // reference and check the required nodes
            XmlNode pattern = this.FindNode("pattern", node);
            XmlNode template = this.FindNode("template", node);

            GraphMaster ourGraphMaster = this.bot.GetGraph(this.graphName) ?? this.bot.Graphmaster;

            if (object.Equals(null, pattern))
            {
                throw new XmlException("Missing pattern tag in a node found in " + filename);
            }
            if (object.Equals(null, template))
            {
                if (false) throw new XmlException("Missing template tag in the node with pattern: " + pattern.InnerText + " found in " + filename);
            }
            string templateXML = InnerTextOrXML(template) ?? "";

            string categoryPath = this.generatePath(node, topicName, stateNamePre, stateNamePost, false);
            // o.k., add the processed AIML to the GraphMaster structure
            if (categoryPath.Length > 0)
            {
                try
                {
                    //this.bot.Graphmaster.addCategory(categoryPath, template.OuterXml, filename, 1, 1);
                    if (this.bot.rapStoreDirectory != null)
                    {
                        Node.addCategoryDB("", categoryPath, templateXML, filename, 1, 1, "", extDB);
                    }
                    else
                    {
                        ourGraphMaster.addCategory(categoryPath, templateXML, filename, 1, 1);
                    }
                    // keep count of the number of categories that have been processed
                    this.bot.Size++;
                }
                catch(Exception e)
                {
                    this.bot.writeToLog("ERROR! Failed to load a new category into the graphmaster where the path = " + categoryPath + " and template = " + template.OuterXml + " produced by a category in the file: " + filename+ "because "+e.Message+ " "+e.StackTrace);

                }
            }
            else
            {
                this.bot.writeToLog("WARNING! Attempted to load a new category with an empty pattern where the path = " + categoryPath + " and template = " + template.OuterXml + " produced by a category in the file: " + filename);
            }
        }

        /// <summary>
        /// Generates a path from a category XML node and topic name
        /// </summary>
        /// <param name="node">the category XML node</param>
        /// <param name="topicName">the topic</param>
        /// <param name="stateNamePre">the state in the front</param>
        /// <param name="stateNamePost">the state at the end (tie breaker)</param>
        /// <param name="isUserInput">marks the path to be created as originating from user input - so
        /// normalize out the * and _ wildcards used by AIML</param>
        /// <returns>The appropriately processed path</returns>
        public string generatePath(XmlNode node, string topicName,string stateNamePre,string stateNamePost, bool isUserInput)
        {
            // get the nodes that we need
            XmlNode pattern = this.FindNode("pattern", node);
            XmlNode that = this.FindNode("that", node);

            string patternText = InnerTextOrXML(pattern) ?? "";
            string thatText = InnerTextOrXML(that) ?? "*";
            return this.generatePath(patternText, thatText, topicName,stateNamePre,stateNamePost, isUserInput);
        }

        private static string InnerTextOrXML(XmlNode pattern)
        {
            if (object.Equals(null, pattern))
            {
                return null;
            }
            var xo = pattern.InnerXml;
            if (xo.Contains("<")) return xo;
            return pattern.InnerText;
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

        /// <summary>
        /// Generates a path from the passed arguments
        /// </summary>
        /// <param name="pattern">the pattern</param>
        /// <param name="that">the that</param>
        /// <param name="topicName">the topic</param>
        /// <param name="stateNamePre">the state in the front</param>
        /// <param name="stateNamePost">the state at the end (tie breaker)</param>
        /// <param name="isUserInput">marks the path to be created as originating from user input - so
        /// normalize out the * and _ wildcards used by AIML</param>
        /// <returns>The appropriately processed path</returns>
        public string generatePath(string pattern, string that, string topicName,string stateNamePre,string stateNamePost, bool isUserInput)
        {
            // to hold the normalized path to be entered into the graphmaster
            StringBuilder normalizedPath = new StringBuilder();
            string normalizedPattern = string.Empty;
            string normalizedThat = "*";
            string normalizedTopic = "*";
            string normalizedStatePre = "*";
            string normalizedStatePost = "*";

            if ((this.bot.TrustAIML)&(!isUserInput))
            {
                normalizedPattern = pattern.Trim();
                normalizedThat = that.Trim();
                normalizedTopic = topicName.Trim();
                normalizedStatePre = stateNamePre.Trim();
                normalizedStatePost = stateNamePost.Trim();
            }
            else
            {
                normalizedPattern = this.Normalize(pattern, isUserInput).Trim();
                normalizedThat = this.Normalize(that, isUserInput).Trim();
                normalizedTopic = this.Normalize(topicName, isUserInput).Trim();
                normalizedStatePre = this.Normalize(stateNamePre, isUserInput).Trim();
                normalizedStatePost = this.Normalize(stateNamePost, isUserInput).Trim();
            }

            // check sizes
            if (normalizedPattern.Length > 0)
            {
                if (normalizedThat.Length == 0)
                {
                    normalizedThat = "*";
                }
                if (normalizedTopic.Length == 0)
                {
                    normalizedTopic = "*";
                }
                if (normalizedStatePre.Length == 0)
                {
                    normalizedStatePre = "*";
                }
                if (normalizedStatePost.Length == 0)
                {
                    normalizedStatePost = "*";
                }

                // This check is in place to avoid huge "that" elements having to be processed by the 
                // graphmaster. 
                if (normalizedThat.Length > this.bot.MaxThatSize)
                {
                    normalizedThat = "*";
                }

                //KHC: we could dump them all in an hash table then use a setting
                //     string to define the sequence
                // o.k. build the path :standard <pattern><that><state><topic>
                if (0 == 0)
                {
                    normalizedPath.Append("<state> ");
                    normalizedPath.Append(normalizedStatePre);
                    normalizedPath.Append(" <pattern> ");
                    normalizedPath.Append(normalizedPattern);
                    normalizedPath.Append(" <that> ");
                    normalizedPath.Append(normalizedThat);
                    normalizedPath.Append(" <state> ");
                    normalizedPath.Append(normalizedStatePost);
                    normalizedPath.Append(" <topic> ");
                    normalizedPath.Append(normalizedTopic);
                }
                else
                {
                    // An alternate form of path is <state><topic><that><pattern>
                    normalizedPath.Append("<state> ");
                    normalizedPath.Append(normalizedStatePre);
                    normalizedPath.Append(" <topic> ");
                    normalizedPath.Append(normalizedTopic);
                    normalizedPath.Append(" <pattern> ");
                    normalizedPath.Append(normalizedPattern);
                    normalizedPath.Append(" <that> ");
                    normalizedPath.Append(normalizedThat);
                    normalizedPath.Append(" <state> ");
                    normalizedPath.Append(normalizedStatePost);
                }
                return normalizedPath.ToString();
            }
            else
            {
                return string.Empty;
            }
        }

        /// <summary>
        /// Given an input, provide a normalized output
        /// </summary>
        /// <param name="input">The string to be normalized</param>
        /// <param name="isUserInput">True if the string being normalized is part of the user input path - 
        /// flags that we need to normalize out * and _ chars</param>
        /// <returns>The normalized string</returns>
        public string Normalize(string input, bool isUserInput)
        {
            StringBuilder result = new StringBuilder();

            // objects for normalization of the input
            Normalize.ApplySubstitutions substitutor = new AltAIMLbot.Normalize.ApplySubstitutions(this.bot);
            Normalize.StripIllegalCharacters stripper = new AltAIMLbot.Normalize.StripIllegalCharacters(this.bot);

            string substitutedInput = substitutor.Transform(input);
            // split the pattern into it's component words
            string[] substitutedWords = substitutedInput.Split(" \r\n\t".ToCharArray());

            // Normalize all words unless they're the AIML wildcards "*" and "_" during AIML loading
            foreach (string word in substitutedWords)
            {
                string normalizedWord;
                if (isUserInput)
                {
                    normalizedWord = stripper.Transform(word);
                }
                else
                {
                    if ((word == "*") || (word == "_"))
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
