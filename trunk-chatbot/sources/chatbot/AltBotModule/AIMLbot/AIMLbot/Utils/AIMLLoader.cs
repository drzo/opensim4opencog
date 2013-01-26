using System;
using System.Collections.Generic;
using System.Xml;
using System.IO;
using System.Text;
using System.Threading;
using MushDLR223.Utilities;
using RTParser;
using RTParser.Normalize;
using RTParser.Utils;


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
        private AltBot bot;
        private string graphName = "*";
        private string topicName = "*";
        private string stateNamePre = "*";
        private string stateNamePost = "*";
        private string currentThat = "*";
        ExternDB extDBCached = null;
        private static bool SeekOutAndRepair = false;

        #endregion

        /// <summary>
        /// Ctor
        /// </summary>
        /// <param name="bot">The bot whose brain is being processed</param>
        public AIMLLoader(AltBot bot)
        {
            this.bot = bot;
        }

        #region Methods


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
                    Console.WriteLine("WARN No aiml files fopu8nd in directory " + path);
                    return;
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
                var fn = filename;
                filename = filename.Replace("\\", "/");// new FileInfo(fn).FullName;
                this.bot.writeToLog("Processing AIML file(2): " + filename);
                if (!File.Exists(filename))
                {
                    Console.WriteLine("WARNING: '{0}' does not exist", filename);
                }
                if (this.bot.UseRapstoreDB)
                {
                    var extDB = bot.GetGraph(graphName);
                    if (extDB.wasloaded(filename)) return;
                    extDB.Close();
                    extDB.ensureEdb();
                }
                XmlTextReader reader = null;
                XmlDocumentLineInfo doc = null; 
                try
                {
                    reader = new XmlTextReader(filename);
                    if (reader != null)
                    {
                        doc = new XmlDocumentLineInfo(filename, true);
                        if (doc != null)
                        {
                            // load the document
                            //doc.Load(filename);
                            reader.MoveToContent();                // Skip over the XML declaration
                            doc.Load(reader);
                            this.loadAIMLFromXML(doc, filename);
                           // XmlNodeList rootChildren = doc.ChildNodes;
                           // foreach (XmlNode currentNode in rootChildren)
                           // {
                           //     loadAIMLFromXML(currentNode, filename);
                          //  }
                        }
                        else
                        {
                            Console.WriteLine("WARNING: XmlDocumentLineInfo({0}) == null !", filename);
                        }
                    }
                    else
                    {
                        Console.WriteLine("WARNING: XmlTextReader({0}) == null !" , filename);
                    }
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
                var extDB0 = bot.GetGraph(graphName);
                if (extDB0 != null)
                {
                    extDB0.Close();
                }
            }
        }

        /// <summary>
        /// Given an XML document containing valid AIML, attempts to load it into the graphmaster
        /// </summary>
        /// <param name="doc">The XML document containing the AIML</param>
        /// <param name="filename">Where the XML document originated</param>
        private void loadAIMLFromXMLDoc(XmlDocument doc, string filename)
        {
            if (doc.DocumentElement != null)
            {
                if (doc.DocumentElement.Attributes != null)
                {
                    if (doc.DocumentElement.Attributes.Count > 0)
                    {
                        doc.DocumentElement.Attributes.RemoveNamedItem("xmlns");
                    }
                }
            }
            //Console.WriteLine("Check: loadAIMLFromXMLDoc");
            lock (ExternDB.mylock)
            {

                if (this.bot.UseRapstoreDB)
                {
                    //var extDB = bot.GetGraph(graphName);
                    //extDB.Close();
                   // extDB.ensureEdb();
                }
                // Get a list of the nodes that are children of the <aiml> tag
                // these nodes should only be either <topic> or <category>
                // the <topic> nodes will contain more <category> nodes
                //Console.WriteLine("Check: loadAIMLFromXML enter(2)");
                //Console.WriteLine("Check: OUTER XML :{0}", doc.OuterXml);
                //Console.WriteLine("Check: doc.ChildNodes.Count :{0}", doc.ChildNodes.Count);
                //Console.WriteLine("Check: loadAIMLFromXML Code={0}", doc.DocumentElement.OuterXml);
                if (doc.DocumentElement != null)
                {
                    //Console.WriteLine("Check: Has DocumentElement: {0}", doc.DocumentElement.OuterXml);
                    loadAIMLFromXML(doc.DocumentElement, filename);
                }
            }
        }

        public void loadAIMLFromXML(XmlNode doc, string filename)
        {
            if (doc == null)
            {
                Console.WriteLine("Check: loadAIMLFromXML : (doc is NULL) filename={0}", filename);
                return;
            }
            if (doc is XmlDocument)
            {
                //Console.WriteLine("Check: loadAIMLFromXML : (doc is XmlDocument) filename={0}", filename);
                loadAIMLFromXMLDoc((XmlDocument)doc, filename);
                return;
            }

            if (doc is XmlDeclaration)
            {
                Console.WriteLine("Check: loadAIMLFromXML : (doc is XmlDeclaration (0)) filename={0}", filename);
                return;

            }
            //Console.WriteLine("Check: loadAIMLFromXML : (doc isNOT XmlDocument) filename={0}", filename);
            {

                if (this.bot.UseRapstore(graphName))
                {
                    var extDB0 = bot.GetGraph(graphName);

                    //if ((filename.Contains("\\") || filename.Contains("/")) && (!filename.Contains("servitorgraphmap")) && (extDB0.ensureEdb().wasLoaded(filename)))
                    //if ((extDB0.ensureEdb().wasLoaded(filename))
                    if ((extDB0.wasloaded(filename))
                         && (!filename.Contains("servitorgraphmap"))
                         && (filename.Contains(Path.DirectorySeparatorChar.ToString()))
                        )
                    {
                        // We loaded that file
                        extDB0.Close();
                        Console.WriteLine("Check: loadAIMLFromXML skipping {0}", filename);
                        return;
                    }
                    else
                    {
                        extDB0.ensureEdb();
                        //extDB._dbdir = this.bot.rapStoreDirectory;
                    }
                }
                Console.WriteLine("Check: loadAIMLFromXML processing {0}",filename);
                cleanXMLNS(doc);
                LoadBXML(doc, filename);
                if (this.bot.UseRapstore(graphName))
                {
                    var extDB0 = bot.GetGraph(graphName);
                    var extDB = extDB0.ensureEdb();
                    extDB.rememberLoaded(filename);
                    extDB0.Close();
                }
            }
        }

        private void LoadBXML(XmlNode doc, string filename)
        {
            bool didIt = processBXML(doc, filename);
            if (didIt) return;
            processAiml(doc, filename);
        }

        private void processAiml(XmlNode doc, string filename)
        {
            if (doc == null)
            {
                return;
            }
            XmlNodeList rootChildren = doc.ChildNodes;
            // find the name of the graph or set to default "*"
            string oldGraph = graphName;
            graphName = StaticXMLUtils.GetAttribValue(doc, "graph", this.graphName);
            // process each of these child nodes
            foreach (XmlNode currentNode in rootChildren)
            {
               if (!processBXML(currentNode, filename))
               {
                   processImmediate(currentNode, filename);
               }
            }
            graphName = oldGraph;
        }

        private bool processBXML(XmlNode thisNode, string filename)
        {
            if (thisNode == null) return false;
            if (StaticXMLUtils.IsBlank(thisNode)) return true;
            if (thisNode.Name == null) return false;
            string named = thisNode.Name.ToLower();
            if (named == "ser")
            {
                processSer(thisNode, filename);
                return true;
            }
            if (named == "aiml")
            {
                processAiml(thisNode, filename);
                return true;
            }
            if (named == "topic")
            {
                this.processTopic(thisNode, filename);
                return true;
            }
            if (named == "state")
            {
                this.processState(thisNode, filename);
                return true;
            }
            else if (named == "category")
            {
                this.processCategory(thisNode, filename);
                return true;
            }
            if ((named == "behavior") || (named == "rbehavior"))
            {
                processImmediate(thisNode, filename);
                return true;
            }
            if (named == "crontag")
            {
                processImmediate(thisNode, filename);
                return true;
            }
            if ((named == "scxml")||(named == "btxml"))
            {
                processImmediate(thisNode, filename);
                return true;
            }
            if (named == "task")
            {
                processImmediate(thisNode, filename);
                return true;
            }
            if (named == "subaiml")
            {
                processImmediate(thisNode, filename);
                return true;

            }


            return false;
        }

        private static void cleanXMLNS(XmlNode node)
        {
            if (!(node is XmlElement)) return;
            XmlAttributeCollection attribs = node.Attributes;
            if (attribs != null && attribs.Count > 0)
            {
                var remove = new List<string>();
                foreach (XmlAttribute attrib in attribs)
                {
                    if (attrib.Name.StartsWith("xmlns")) remove.Add(attrib.Name);
                }
                foreach (var r in remove)
                {
                    node.Attributes.RemoveNamedItem(r);
                }
            }
            var cns = node.ChildNodes;
            if (cns.Count > 0)
            {
                foreach (XmlNode xmlNode in cns)
                {
                    if (xmlNode is XmlElement)
                    {
                        if (xmlNode == node)
                        {
                            continue;
                        }
                        cleanXMLNS(xmlNode);
                    }
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
            if (node == null) return;

            try
            {
                object result = this.bot.evalTemplateNode(node);
                if (AltBot.tl_aimlResult != null)
                {
                    AltBot.tl_aimlResult.AddResult(node, filename, result);
                }
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
            withAttributes(node, ref topicName, () =>
                                                processChildren(node, filename));
        }

        private void withAttributes(XmlNode node, ref string defaultElement, Action action)
        {
            string preTopic = topicName;
            string preRef = defaultElement;
            string preStateNamePre = stateNamePre;
            string preStateNamePost = stateNamePost;
            string preThat = currentThat;
            string preGraph = graphName;
            foreach (XmlAttribute Attrib in node.Attributes)
            {
                if (Attrib.Name == "name") { defaultElement = node.Attributes["name"].Value; }
                if (Attrib.Name == "state") { stateNamePre = node.Attributes["state"].Value; }
                if (Attrib.Name == "topic") { topicName = node.Attributes["topic"].Value; }
                if (Attrib.Name == "graph") { graphName = node.Attributes["graph"].Value; }
                if (Attrib.Name == "that") { currentThat = node.Attributes["that"].Value; }
                if (Attrib.Name == "prestate") { stateNamePre = node.Attributes["prestate"].Value; }
                if (Attrib.Name == "poststate") { stateNamePost = node.Attributes["poststate"].Value; }
            }
            try
            {
                action();
            }
            finally
            {
                topicName = preTopic;
                stateNamePre = preStateNamePre;
                stateNamePost = preStateNamePost;
                graphName = preGraph;
                currentThat = preThat;
                defaultElement = preRef;
            }
        }

        private void processChildren(XmlNode node, string filename)
        {
            if (StaticXMLUtils.IsBlank(node)) return;
            // process all the category nodes
            foreach (XmlNode thisNode in node.ChildNodes)
            {
                if (!processBXML(thisNode, filename))
                {
                    processImmediate(thisNode, filename);
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
            withAttributes(node, ref stateNamePre, () =>
                                                processChildren(node, filename));
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

                    if (this.bot.UseRapstore(graphName))
                    {
                        var extDB = bot.GetGraph(graphName).ensureEdb();
                        Node.addCategoryDB("", categoryPath, template.OuterXml, filename, 1, 1, "", extDB);
                    }
                    else
                    {
                        ourGraphMaster.addCategory(categoryPath, template.OuterXml, filename, 1, 1);
                    }

                    
                    // keep count of the number of categories that have been processed
                    //this.bot.Size++;
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
            this.processCategory(node, topicName, stateNamePre, stateNamePost, filename);
        }


        private void CategoryCheck(XmlNode node, string filename)
        {
            if (object.Equals(null, FindNode("pattern", node)))
            {
                throw new XmlException("Missing pattern tag in a node found in " + filename + " from " + node.OuterXml);
            }
            if (object.Equals(null, this.FindNode("template", node)))
            {
                if (false) throw new XmlException("Missing template tag in the node with pattern: " + node.OuterXml + " found in " + filename);
            }
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
            if (node is IXmlLineInfo)
            {
                var li  = ((IXmlLineInfo) node).LineNumber;
                if (li != 0)
                {
                    filename += ":" + li;
                }
            }

            // reference and check the required nodes
            CategoryCheck(node, filename);

            var categoryPaths = this.generatePaths(node, topicName, stateNamePre, stateNamePost, false);
            // o.k., add the processed AIML to the GraphMaster structure
            int found = 0;
            foreach (KeyValuePair<string, string> pair in categoryPaths)
            {
                found++;
                var categoryPath = pair.Key;
                var templateXML = pair.Value;
                try
                {
                    //this.bot.Graphmaster.addCategory(categoryPath, template.OuterXml, filename, 1, 1);
                    if (this.bot.UseRapstore(graphName))
                    {
                        var extDB = bot.GetGraph(graphName).ensureEdb();
                        Node.addCategoryDB("", categoryPath, templateXML, filename, 1, 1, "", extDB);
                    }
                    else
                    {
                        GraphMaster ourGraphMaster = this.bot.GetGraph(this.graphName) ?? this.bot.Graphmaster;
                        ourGraphMaster.addCategory(categoryPath, templateXML, filename, 1, 1);
                    }
                    // keep count of the number of categories that have been processed
                    this.bot.SizeC++;
                }
                catch (Exception e)
                {
                    this.bot.writeToLog("ERROR! Failed to load a new category into the graphmaster where the path = " +
                                        categoryPath + " and template = " + templateXML +
                                        " produced by a category in the file: " + filename + "because " + e.Message +
                                        " " + e.StackTrace);

                }
            }
            if (found == 0)
            {
                this.bot.writeToLog(
                    "WARNING! Attempted to load a new category with an empty pattern: " +
                    node.OuterXml + " produced by a category in the file: " +
                    filename);
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
        private List<KeyValuePair<string, string>> generatePaths(XmlNode node, string topicName, string stateNamePre, string stateNamePost, bool isUserInput)
        {
            var lretval = new List<KeyValuePair<string, string>>();
            // get the nodes that we need
            var patterns = StaticXMLUtils.FindNodes("pattern", node);            
            var thats = StaticXMLUtils.FindNodes("that", node);
            var templates = StaticXMLUtils.FindNodes("template", node);
            if (thats.Count == 0) thats = new List<XmlNode>() {StaticAIMLUtils.XmlStar.Value};

            foreach (XmlNode template in templates)
            {
                string templateXML = InnerTextOrXML(template, false) ?? "";
                foreach (var that in thats)
                {
                    string thatText = InnerTextOrXML(that, true) ?? currentThat;
                    foreach (var pattern in patterns)
                    {
                        string patternText = InnerTextOrXML(pattern, true) ?? "";
                        string categoryPath = generatePath(patternText, thatText, topicName, stateNamePre, stateNamePost,
                                                           isUserInput);
                        lretval.Add(new KeyValuePair<string, string>(categoryPath, templateXML));
                    }
                }
            }
            return lretval;
        }

        private static string InnerTextOrXML(XmlNode pattern, bool isPatternMatcher)
        {
            if (object.Equals(null, pattern))
            {
                return null;
            }
            var xo = pattern.InnerXml;
            if (!xo.Contains("<") && !xo.Contains("&"))
            {
                xo = pattern.InnerText;
            }

            if (xo != xo.Trim() || xo.IndexOfAny("\n\r\t\b\"".ToCharArray()) != -1)
            {
                if (isPatternMatcher)
                {
                    xo = StaticXMLUtils.ReTrimAndspace(xo);
                }
            }
            return xo;
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
        private string Normalize(string input, bool isUserInput)
        {
            if (input == "*") return input;
            string r = Normalize0(input, isUserInput);
            if (input.Contains("_"))
            {
                if (r.Trim()!=input.Trim())
                {
                    r = Normalize0(input, isUserInput);
                }
            }
            return r;
        }
        private string Normalize0(string input, bool isUserInput)
        {
            StringBuilder result = new StringBuilder();

            // objects for normalization of the input
            ApplySubstitutions substitutor = new ApplySubstitutions(this.bot);
            StripIllegalCharacters stripper = new StripIllegalCharacters(this.bot);

            string substitutedInput = substitutor.Transform(input);
            // split the pattern into it's component words
            string[] substitutedWords = substitutedInput.Split(" \r\n\t".ToCharArray());

            // Normalize all words unless they're the AIML wildcards "*" and "_" during AIML loading
            foreach (string word in substitutedWords)
            {
                if (word == "") continue;
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

            return result.ToString().Replace("  "," ").Trim(); // make sure the whitespace is neat
        }
        #endregion
    }
}
