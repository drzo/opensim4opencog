using System;
using System.Collections.Generic;
using System.Net;
using System.Xml;
using System.IO;
using System.Text;
using System.Threading;
using AltAIMLParser;
using MushDLR223.ScriptEngines;
using MushDLR223.Utilities;
using MushDLR223.Virtualization;
using AltAIMLbot.Normalize;
using AltAIMLbot.Utils;
using LoaderOptions = AltAIMLbot.Utils.AIMLLoaderS;


namespace AltAIMLbot.Utils
{
    /// <summary>
    /// A utility class for loading AIML files from disk into the graphmaster structure that 
    /// forms an AIML bot's "brain"
    /// </summary>
    public partial class AIMLLoaderS
    {
        #region Attributes

        /// <summary>
        /// The bot whose brain is being processed
        /// </summary>
        public AltBot bot;


        private ExternDB extDBCached = null;
        //public bool recurse;
        private static bool SeekOutAndRepair = false;
        private bool forceReload = false;

        public EasyLogger Logger
        {
            get { return this.bot.Logger; }
        }

        public AIMLLoaderU ULoader;

        private LoaderOptions _loadOpts1;

        public LoaderOptions loadOpts
        {
            get
            {
                if (_loadOpts1 == null)
                {
                    _loadOpts1 = ULoader.LoaderRequest00.LoadOptions;
                }
                return _loadOpts1;
            }
            set { _loadOpts1 = value; }
        }

        #endregion

        /// <summary>
        /// Ctor
        /// </summary>
        /// <param name="bot">The bot whose brain is being processed</param>
        public AIMLLoaderS(AltBot bot)
        {
            this.bot = bot;
        }

        #region Methods


        public override string ToString()
        {
            return LogicalParticleFilter1.GlobalSharedSettings.StructToString(this);
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

                string[] fileEntries = HostSystem.GetFiles(path, "*.aiml");
                if (fileEntries.Length > 0)
                {
                    foreach (string filename in fileEntries)
                    {
                        this.loadAIMLFile(filename);
                    }
                    this.bot.writeToLog("Finished processing the AIML files. " + Convert.ToString(this.bot.Size) +
                                        " categories processed.");
                }
                else
                {
                    Console.WriteLine("WARN No aiml files fopu8nd in directory " + path);
                    return;
                    throw new FileNotFoundException("Could not find any .aiml files in the specified directory (" + path +
                                                    "). Please make sure that your aiml file end in a lowercase aiml extension, for example - myFile.aiml is valid but myFile.AIML is not.");
                }
            }
            else
            {
                if (false)
                    throw new FileNotFoundException("The directory specified as the path to the AIML files (" + path +
                                                    ") cannot be found by the AIMLLoader object. Please make sure the directory where you think the AIML files are to be found is the same as the directory specified in the settings file.");
                loadAIMLFile(path);
            }
        }

        /// <summary>
        /// Given the name of a file in the AIML path directory, attempts to load it into the 
        /// graphmaster
        /// </summary>
        /// <param name="filename">The name of the file to process</param>
        public long loadAIMLFile(string filename)
        {
            //lock (ExternDB.mylock)
            {
                var fn = filename;
                var graphName = loadOpts.graphName;
                filename = filename.Replace("\\", "/"); // new FileInfo(fn).FullName;
                this.bot.writeToLog("Processing AIML file(2): " + filename);
                if (this.bot.UseRapstoreDB)
                {
                    var extDB = bot.GetGraph(graphName);
                    if (extDB.wasloaded(filename)) return extDB.Size;
                    extDB.Close();
                    extDB.ensureEdb();
                }
                if (!File.Exists(filename))
                {
                    Console.WriteLine("WARNING: '{0}' does not exist as a 'file' calling stream loader", filename);
                    return loadAIMLURI(filename);
                }

                XmlTextReader reader = null;
                XmlDocumentLineInfo doc = null;
                try
                {
                    reader = XmlDocumentLineInfo.CreateXmlTextFileReader(filename);
                    doc = new XmlDocumentLineInfo(filename, true);
                    // Skip over the XML declaration
                    reader.MoveToContent();
                    doc.Load(reader);
                    return this.loadAIMLFromXML(doc, filename);
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
                    return 0;
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
                    return 0;
                }
                finally
                {
                    //extDB.SaveIndex();
                    var extDB0 = bot.GetGraph(graphName);
                    if (extDB0 != null)
                    {
                        extDB0.Close();
                    }
                }
            }
        }

        /// <summary>
        /// Given an XML document containing valid AIML, attempts to load it into the graphmaster
        /// </summary>
        /// <param name="doc">The XML document containing the AIML</param>
        /// <param name="filename">Where the XML document originated</param>
        private long loadAIMLFromXMLDoc(XmlDocument doc, string filename)
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
                    return loadAIMLFromXML(doc.DocumentElement, filename);
                }
            }
            return 0;
        }

        public long loadAIMLFromXML(XmlNode doc, string filename)
        {
            if (doc == null)
            {
                Console.WriteLine("Check: loadAIMLFromXML : (doc is NULL) filename={0}", filename);
                return 0;
            }
            if (doc is XmlDocument)
            {
                //Console.WriteLine("Check: loadAIMLFromXML : (doc is XmlDocument) filename={0}", filename);
                return loadAIMLFromXMLDoc((XmlDocument) doc, filename);
            }

            if (doc is XmlDeclaration)
            {
                Console.WriteLine("Check: loadAIMLFromXML : (doc is XmlDeclaration (0)) filename={0}", filename);
                return 0;

            }
            //Console.WriteLine("Check: loadAIMLFromXML : (doc isNOT XmlDocument) filename={0}", filename);
            {
                var graphName = loadOpts.graphName;
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
                        return extDB0.Size;
                    }
                    else
                    {
                        extDB0.ensureEdb();
                        //extDB._dbdir = this.bot.rapStoreDirectory;
                    }
                }
                Console.WriteLine("Check: loadAIMLFromXML processing {0}", filename);
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
            return 1;
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
            string oldGraph = loadOpts.graphName;
            var graphName = StaticXMLUtils.GetAttribValue(doc, "graph", loadOpts.graphName);
            if (graphName != "*")
            {
                Logger.Warn("Loading to non * graph: " + graphName);
            }
            // process each of these child nodes
            foreach (XmlNode currentNode in rootChildren)
            {
                loadOpts.graphName = graphName;
                if (!processBXML(currentNode, filename))
                {
                    processImmediate(currentNode, filename);
                }
            }
            loadOpts.graphName = oldGraph;
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
            if ((named == "scxml") || (named == "btxml"))
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
                object result = this.bot.evalTemplateNode(node, RequestKind.AIMLLoader | RequestKind.BotPropertyEval);
                if (AltBot.tl_aimlResult != null)
                {
                    AltBot.tl_aimlResult.AddResult(node, filename, result);
                }
            }
            catch (Exception e)
            {
                this.bot.writeToLog("WARNING! Error " + e.Message + " Stack=" + e.StackTrace + " and Code = " +
                                    node.OuterXml + " produced by a code in the file: " + filename);

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
            loadOpts.withAttributes(node, ref loadOpts.topicName, () =>
                                                processChildren(node, filename));
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
        /// <param name="node">the "state" node</param>
        /// <param name="filename">the file from which this node is taken</param>
        private void processState(XmlNode node, string filename)
        {
            loadOpts.withAttributes(node, ref loadOpts.stateNamePre, () =>
                                                   processChildren(node, filename));
        }

        /// <summary>
        /// Adds a preprocessed path and content to the  graphmaster structure
        /// </summary>
        /// <param name="node">the XML node containing the category</param>
        /// <param name="filename">the file from which this category was taken</param>
        private void processSer(XmlNode node, string filename)
        {
            GraphMaster ourGraphMaster = this.bot.GetGraph(loadOpts.graphName) ?? this.bot.Graphmaster;

            XmlNode template = this.FindNode("template", node);
            string categoryPath = "";

            categoryPath = node.Attributes["path"].Value;
            if ((categoryPath == null) || (categoryPath.Length == 0)) return;
            categoryPath = categoryPath.Trim();
            if ((categoryPath == null) || (categoryPath.Length == 0)) return;

            if (object.Equals(null, template))
            {
                this.bot.writeToLog("Missing template tag in the node with path: " + categoryPath + " found in " +
                                    filename);
                return;
            }

            // o.k., add the processed AIML to the GraphMaster structure
            if (categoryPath.Length > 0)
            {
                try
                {
                    // //this.bot.Graphmaster.addCategory(categoryPath, template.OuterXml, filename, 1, 1);
                    //ourGraphMaster.addCategory(categoryPath, template.OuterXml, filename, 1, 1);
                    var graphName = loadOpts.graphName;
                    if (this.bot.UseRapstore(graphName))
                    {
                        GraphMaster G = bot.GetGraph(graphName);
                        G.Size++;
                        var extDB = G.ensureEdb();
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
                    this.bot.writeToLog("ERROR! Failed to load a new category into the graphmaster where the path = " +
                                        categoryPath + " and template = " + template.OuterXml +
                                        " produced by a category in the file: " + filename);
                }
            }
            else
            {
                this.bot.writeToLog(
                    "WARNING! Attempted to load a new category with an empty pattern where the path = " + categoryPath +
                    " and template = " + template.OuterXml + " produced by a category in the file: " + filename);
            }
        }

        /// <summary>
        /// Adds a category to the graphmaster structure using the default topic ("*")
        /// </summary>
        /// <param name="node">the XML node containing the category</param>
        /// <param name="filename">the file from which this category was taken</param>
        private void processCategory(XmlNode node, string filename)
        {
            this.processCategory(node, loadOpts.topicName, loadOpts.stateNamePre, loadOpts.stateNamePost, filename);
        }


        private void CategoryCheck(XmlNode node, string filename)
        {
            if (object.Equals(null, FindNode("pattern", node)))
            {
                throw new XmlException("Missing pattern tag in a node found in " + filename + " from " + node.OuterXml);
            }
            if (object.Equals(null, this.FindNode("template", node)))
            {
                if (false)
                    throw new XmlException("Missing template tag in the node with pattern: " + node.OuterXml +
                                           " found in " + filename);
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
        private void processCategory(XmlNode node, string topicName, string stateNamePre, string stateNamePost,
                                     string filename)
        {
            if (node is IXmlLineInfo)
            {
                var li = ((IXmlLineInfo) node).LineNumber;
                if (li != 0)
                {
                    filename += ":" + li;
                }
            }

            // reference and check the required nodes
            CategoryCheck(node, filename);
            var graphName = node.AttributeValueOfDefault("graph", loadOpts.graphName);
            var categoryPaths = this.generatePaths(node, graphName, topicName, stateNamePre, stateNamePost, false);
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
                        GraphMaster ourGraphMaster = this.bot.GetGraph(loadOpts.graphName) ?? this.bot.Graphmaster;
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
        private List<KeyValuePair<string, string>> generatePaths(XmlNode node, string graphName, string topicName,
                                                                 string stateNamePre, string stateNamePost,
                                                                 bool isUserInput)
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
                    string thatText = InnerTextOrXML(that, true) ?? loadOpts.currentThat;
                    foreach (var pattern in patterns)
                    {
                        string patternText = InnerTextOrXML(pattern, true) ?? "";
                        string categoryPath = generatePath(graphName, patternText, thatText, topicName, stateNamePre,
                                                           stateNamePost,
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
            foreach (XmlNode child in node.ChildNodes)
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
        public string generatePath(string graphName, string pattern, string that, string topicName, string stateNamePre,
                                   string stateNamePost, bool isUserInput)
        {
            // to hold the normalized path to be entered into the graphmaster
            StringBuilder normalizedPath = new StringBuilder();
            string normalizedPattern = string.Empty;
            string normalizedThat = "*";
            string normalizedTopic = "*";
            string normalizedStatePre = "*";
            string normalizedStatePost = "*";
            string normalizedGraphName = "*";

            if ((this.bot.TrustAIML) & (!isUserInput))
            {
                normalizedPattern = pattern.Trim();
                normalizedThat = that.Trim();
                normalizedTopic = topicName.Trim();
                normalizedStatePre = stateNamePre.Trim();
                normalizedStatePost = stateNamePost.Trim();
                normalizedGraphName = graphName.Trim();
            }
            else
            {
                normalizedPattern = this.Normalize(pattern, isUserInput).Trim();
                normalizedThat = this.Normalize(that, isUserInput).Trim();
                normalizedTopic = this.Normalize(topicName, isUserInput).Trim();
                normalizedStatePre = this.Normalize(stateNamePre, isUserInput).Trim();
                normalizedStatePost = this.Normalize(stateNamePost, isUserInput).Trim();
                normalizedGraphName = this.Normalize(graphName, isUserInput).Trim();
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
                if (normalizedGraphName.Length == 0)
                {
                    normalizedGraphName = "*";
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
                    normalizedPath.Append(normalizedGraphName);
                    normalizedPath.Append(" ");
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
                if (r.Trim() != input.Trim())
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

            return result.ToString().Replace("  ", " ").Trim(); // make sure the whitespace is neat
        }

        #endregion



        private R LoaderOper<R>(Func<R> action, GraphMaster gm)
        {
            OutputDelegate prev = StaticAIMLUtils.userTraceRedir;
            try
            {
                StaticAIMLUtils.userTraceRedir = gm.writeToLog;
                try
                {
                    if (!loadOpts.NeedsLoaderLock) return action();
                    lock (StaticAIMLUtils.ErrorList)
                    {
                        lock (gm.LockerObject)
                        {
                            return action();
                        }
                    }
                }
                catch (ChatSignal e)
                {
                    throw;
                }
                catch (Exception e)
                {
                    writeToLog("ERROR: LoaderOper {0}", e);
                    if (StaticAIMLUtils.NoRuntimeErrors) return default(R);
                    throw;
                    //return default(R);
                }
            }
            finally
            {
                StaticAIMLUtils.userTraceRedir = prev;
            }
        }

        public void QuietLogger(string s, params object[] objects)
        {
            s = DLRConsole.SafeFormat("USERTRACE: " + s, objects);
            if (s.ToUpper().Contains("ERROR"))
            {
                writeToLog(s, objects);
            }
        }


        public long loadAIMLURI(string path)
        {
            long total = LoaderOper(() => loadAIMLURI0(path), loadOpts.CtxGraph);
            TotalCheck(path, total);
            return total;
        }

        private void TotalCheck(string path, long total)
        {
        }

        public long loadAIMLURI0(string path0)
        {
            AltBot RProcessor = loadOpts.RProcessor;
            string path = path0;
            loadOpts.Loading0 = path;
            string pathIn = path;
            path = ResolveToURI(pathIn);
            string fullPath = HostSystem.GetAbsolutePath(pathIn);
            if (!HostSystem.FileOrDirExists(path)) path = fullPath;
            long total = 0;
            try
            {
                if (HostSystem.DirExists(path))
                {
                    path = HostSystem.ToCanonicalDirectory(path);
                    Request request = loadOpts.TheRequest;
                    LoaderOptions savedOpt = request.LoadOptions;
                    try
                    {
                        // request.LoadOptions = loadOpts;
                        // request.Filename = path;
                        //loadOpts = request.LoadOptions;
                        //AltBot.loadConfigs(RProcessor, path, request);
                        total += loadAIMLDir(path);
                        TotalCheck(path, total);
                    }
                    finally
                    {
                        request.LoadOptions = savedOpt;
                    }
                    return total;
                }
                else if (HostSystem.FileExists(path))
                {
                    string hostSystemGetBaseDir = HostSystem.GetBaseDir(pathIn);
                    Request request = loadOpts.TheRequest;

                    string currentPath = HostSystem.ToRelativePath(pathIn, hostSystemGetBaseDir);
                    LoaderOptions savedOpt = request.LoadOptions;
                    string pop = null;
                    try
                    {
                        pop = RProcessor.PushSearchPath(hostSystemGetBaseDir);
                        request.LoadOptions = loadOpts;
                        //total += loadAIMLFile0(path, loadOpts, true);
                        total += loadAIMLFile(path);
                    }
                    finally
                    {
                        request.LoadOptions = savedOpt;
                        RProcessor.PopSearchPath(pop);
                    }
                    return total;
                }
                else if (!HostSystem.IsWildPath(path) && Uri.IsWellFormedUriString(path, UriKind.RelativeOrAbsolute))
                {
                    writeToLog("Processing AIML URI: " + path);
                    Uri uri = new Uri(path);
                    if (uri.IsFile)
                    {
                        total += loadAIMLURI0(uri.AbsolutePath);
                        TotalCheck(uri.AbsolutePath, total);
                        return total;
                    }
                    WebRequest req = WebRequest.Create(uri);
                    WebResponse resp = req.GetResponse();
                    Stream stream = resp.GetResponseStream();
                    Request request = loadOpts.TheRequest;
                    LoaderOptions savedOpt = request.LoadOptions;
                    try
                    {
                        loadOpts.Loading0 = uri.ToString();
                        total += loadAIMLStream(stream);
                        writeToLog("Completed AIML URI: " + path);
                        TotalCheck(uri.AbsolutePath, total);
                        return total;
                    }
                    finally
                    {
                        request.LoadOptions = savedOpt;
                    }
                }
                else
                {
                    string dir;
                    string filemask;
                    var pathnames = HostSystem.GetWildFiles(path, out dir, out filemask);
                    string pop = null;
                    try
                    {
                        Request request = loadOpts.TheRequest;
                        LoaderOptions savedOpt = request.LoadOptions;
                        pop = RProcessor.PushSearchPath(HostSystem.GetBaseDir(dir));
                        if (pathnames != null && pathnames.Length > 0)
                            foreach (string pathname in pathnames)
                            {
                                try
                                {
                                    request.LoadOptions = loadOpts;
                                    total += loadAIMLFile(pathname);
                                }
                                finally
                                {
                                    request.LoadOptions = savedOpt;
                                }
                            }
                        long saved0 = Unifiable.LowMemExpireUnifiableCaches();
                        return total;
                    }
                    finally
                    {
                        RProcessor.PopSearchPath(pop);
                    }
                }

                String nf = "ERROR: XmlTextReader of AIML files (" + path + ")";
                FileNotFoundException nfe = new FileNotFoundException(nf);
                RProcessor.writeToLog(nfe);
                writeToLog(nf);
                throw nfe;
            }
            catch (ChatSignal e)
            {
                throw;
            }
            catch (Exception e)
            {
                RProcessor.writeToLog(e);
                writeToLog("ERROR! " + e);
                throw e;
            }
        }

        /// <summary>
        /// Loads the AIML from files found in the RProcessor's AIMLpath into the RProcessor's brain
        /// </summary>
        //public void loadAIML(string path)
        //{
        //    LoaderOptions loadOpts = request.loader;
        //    request.Loader.loadAIMLFromURI(path);
        //}
        /// <summary>
        /// Loads the AIML from files found in the path
        /// </summary>
        /// <param name="path"></param>                
        public long loadAIMLDir(string path)
        {
            long total = 0;

            int before = loadOpts.CtxGraph.Size;
            long saved0 = Unifiable.LowMemExpireUnifiableCaches();
            writeToLog("Starting to process AIML files found in the directory (" + path + ")");
            try
            {
                total = loadAIMLDir0(path);
            }
            finally
            {
                long saved1 = Unifiable.LowMemExpireUnifiableCaches();
                long saved2 = Unifiable.LowMemExpireUnifiableCaches();
                writeToLog("Finished processing the AIML files. " + Convert.ToString(before - loadOpts.CtxGraph.Size) +
                           " categories processed. saved0-2 {0}-{1}-{2} ", saved0, saved1, saved2);
            }

            return total;
        }

        public long loadAIMLDir0(string path)
        {
            long total = 0;
            AltBot RProcessor = loadOpts.RProcessor;
            path = ResolveToURI(path);

            Request request = loadOpts.TheRequest;
            loadOpts = EnsureOptions(loadOpts, request, path);

            var fileEntries = HostSystem.GetFiles(path, "*.aiml");
            if (fileEntries.Length > 0)
            {
                foreach (string f in fileEntries)
                {
                    try
                    {
                        if (loadOpts.CtxGraph.IsFileLoaded(f))
                        {
                            //writeToLog("(skipping) " + path);
                            continue;
                        }

                        LoaderOptions savedOpt = request.LoadOptions;
                        try
                        {
                            request.LoadOptions = loadOpts;
                            request.Filename = f;
                            loadOpts = request.LoadOptions;
                            total += loadAIMLFile(f);
                        }
                        finally
                        {
                            request.LoadOptions = savedOpt;
                        }
                    }
                    catch (ChatSignal e)
                    {
                        throw;
                    }
                    catch (Exception ee)
                    {
                        loadOpts.CtxGraph.RemoveFileLoaded(path);
                        writeToLog("Error in loadAIMLFile " + ee);
                    }
                }
            }
            else
            {
                writeToLog("Could not find any .aiml files in the specified directory (" + path +
                           "). Please make sure that your aiml file end in a lowercase aiml extension, for example - myFile.aiml is valid but myFile.AIML is not.");
            }

            if (loadOpts.recurse)
            {
                foreach (string f in HostSystem.GetDirectories(path))
                {
                    total += loadAIMLDir(path + Path.DirectorySeparatorChar + f);
                }
            }
            return total;
        }

        private string ResolveToURI(string pathIn)
        {
            return ResolveToURI0(pathIn, loadOpts);
        }

        public static string ResolveToURI0(string pathIn, LoaderOptions loadOpts)
        {
            pathIn = HostSystem.ActualExistingPathIfExists(pathIn) ?? pathIn;

            string baseFile = GetBaseDirectory(loadOpts);
            var inPath = HostSystem.ToRelativePath(pathIn, baseFile);
            IEnumerable<string> combine;
            //combine = baseFile != null ? new[] { "./", baseFile, "aiml/" } : new[] { "./", "aiml/" };
            string prefix;
            combine = loadOpts.TheRequest.TargetBot.RuntimeDirectories;
            string pathFull = HostSystem.ResolveToURI(pathIn, combine, out prefix);
            string relPath = HostSystem.ToRelativePath(pathFull, prefix);
            string rpath = HostSystem.Combine(prefix, relPath);
            if (HostSystem.FileOrDirExists(rpath) && !HostSystem.IsWildPath(rpath))
            {
                rpath = HostSystem.ActualExistingPathIfExists(rpath);
                return rpath;
            }
            relPath = HostSystem.ActualExistingPathIfExists(relPath);
            if (!HostSystem.FileOrDirExists(relPath))
            {
                if (HostSystem.FileOrDirExists(pathIn) && !HostSystem.IsWildPath(pathIn))
                {
                    return pathIn;
                }
                if (!relPath.Contains("*")) AltBot.writeDebugLine("WARNING PATH NOT EXIST ERROR: " + relPath);
            }
            return relPath;
        }

        private static string GetBaseDirectory(LoaderOptions loadOpts)
        {
            string baseFile = loadOpts.CurrentlyLoadingFrom ?? loadOpts.CurrentFilename ?? LoaderOptions.MISSING_FILE;
            if (baseFile == null)
            {
                baseFile = HostSystem.ToCanonicalDirectory(".");
            }
            if (baseFile.EndsWith("/") || baseFile.EndsWith("\\")) return HostSystem.ToCanonicalDirectory(baseFile);
            if (HostSystem.FileExists(baseFile))
                return HostSystem.ToCanonicalDirectory(new FileInfo(baseFile).DirectoryName);
            if (HostSystem.DirExists(baseFile)) return HostSystem.ToCanonicalDirectory(baseFile);
            string bd = HostSystem.ToCanonicalDirectory(HostSystem.GetBaseDir(baseFile));
            return bd;
        }

        /// <summary>
        /// Given the name of a file in the AIML path directory, attempts to load it into the 
        /// graphmaster
        /// </summary>
        /// <param name="path">The name of the file to process</param>
        public long loadAIMLFileStream(string path)
        {
            long total = loadAIMLFile0(path);
            TotalCheck(path, total);
            return total;
        }

        public long loadAIMLFile0(string path)
        {
            long total = 0;
            path = ResolveToURI(path);
            AltBot RProcessor = loadOpts.RProcessor;
            //RProcessor.ReloadHooks.Add(() => loadAIMLFile0(path, loadOpts));
            Request request = loadOpts.TheRequest;
            loadOpts = EnsureOptions(loadOpts, request, path);
            //request.TargetBot.ReloadHooks.Add(() => request.Loader.loadAIMLFile0(path, loadOpts));

            if (!HostSystem.FileExists(path))
            {
                writeToLog("WARNING (Re)writing url: " + path);
                if (loadOpts.recurse)
                {
                    total += loadAIMLURI0(path);
                    TotalCheck(path, total);
                }
                return total;
            }
            GraphMaster master = loadOpts.CtxGraph;
            try
            {
                // load the document
                if (master.IsFileLoaded(path))
                {
                    if (!forceReload)
                    {
                        return total;
                    }
                    writeToLog("Already loaded! (but loading again) " + path + " from " + master);
                    master.RemoveFileLoaded(path);
                    //return;
                }
                else
                    writeToLog("Processing AIML file(1): " + path + " from " + master);
                if (!RProcessor.LoadFromStreamLoader)
                {
                    RProcessor.servitor.loadAIMLFromFile(path);
                }
                else
                {
                    master.AddFileLoaded(path);
                    AutoClosingStream tr = HostSystem.OpenRead(path);
                    try
                    {
                        string pfile = request.Filename;
                        try
                        {
                            request.Filename = path;
                            loadOpts = request.LoadOptions;
                            total += loadAIMLStream(tr);
                            TotalCheck(pfile, total);
                        }
                        finally
                        {
                            request.Filename = pfile;
                        }
                    }
                    finally
                    {
                        HostSystem.Close(tr);
                    }
                }
                writeToLog("Loaded AIMLFile: '{0}'", path + " from " + master);
                return total;
            }
            catch (ChatSignal e)
            {
                throw;
            }
            catch (Exception e)
            {
                master.RemoveFileLoaded(path);
                writeToLog("Error in AIML Stacktrace: " + path + "\n  " + e.Message + "\n" + e.StackTrace);
                writeToLog("Error in AIML file: " + path + " Message " + e.Message);
            }
            return total;
        }

        /// <summary>
        /// Given an XML document containing valid AIML, attempts to load it into the graphmaster
        /// </summary>
        /// <param name="doc">The XML document containing the AIML</param>
        /// <param name="loadOpts">Where the XML document originated</param>
        public long loadAIMLStream(Stream input0)
        {
            long total = 0;
            AltBot RProcessor = loadOpts.RProcessor;
            Request request = loadOpts.TheRequest;
            DateTime oneMinuteFromNow = AltBot.Now + TimeSpan.FromMinutes(1);
            if (request.TimesOutAt < oneMinuteFromNow)
            {
                request.TimesOutAt = oneMinuteFromNow + TimeSpan.FromMinutes(1);
                writeToLog("Bumping up timelimit for " + loadOpts);
            }
            string path = request.Filename;
            loadOpts = EnsureOptions(loadOpts, request, path);

            XmlReader xtr = XmlDocumentLineInfo.CreateXmlTextReader(input0);
            string namefile = "" + path;
            XmlDocumentLineInfo doc = new XmlDocumentLineInfo("" + namefile, false);
            while (!xtr.EOF && xtr.ReadState != ReadState.Closed)
            {
                //  IXmlLineInfo text = (IXmlLineInfo)xtr;
                try
                {
                    doc.SetNodesReadOnly = true;
                    doc.IsFile = true;
                    doc.Load(xtr);
                    if (doc.DocumentElement == null)
                    {
                        RProcessor.writeToLog("ERROR: No Document at " + namefile);
                        //        continue;
                    }
                    else
                    {
                        total += ULoader.loadAIMLNode(doc.DocumentElement, loadOpts, request);
                    }
                }
                catch (ChatSignal e)
                {
                    throw;
                }
                catch (Exception e2)
                {
                    String s = "ERROR: LoadAIMLStream in '" + loadOpts + "' " + doc.GetErrorMsg(xtr, e2);
                    writeToLog(s);
                    //System.Console.Flush();
                    // if (!xtr.Read())
                    {
                        throw;
                    }
                    // 
                }
                finally
                {
                    //   xtr.Close();
                }
            }
            return total;
        }

        /// <summary>
        /// Given an XML document containing valid AIML, attempts to load it into the graphmaster
        /// </summary>
        /// <param name="doc">The XML document containing the AIML</param>
        /// <param name="loadOpts">Where the XML document originated</param>
        public void loadAIMLStreamFallback(Stream input0)
        {
            AltBot RProcessor = loadOpts.RProcessor;
            Request request = loadOpts.TheRequest;
            string path = request.Filename;
            loadOpts = EnsureOptions(loadOpts, request, path);
            path = ResolveToURI(path);

            StreamReader strmreader = new StreamReader(input0);
            string ssss = strmreader.ReadToEnd().Replace("\0", "\n").Replace("t\"src=\"", "t\" src=\"");
            HostSystem.Close(input0);
            int offset = 0;
            if (!ssss.StartsWith("<?"))
            {
                if (!ssss.StartsWith("<aiml"))
                {
                    ssss = "<aiml>" + ssss + "</aiml>";
                    offset += 12;
                }
                string h = "<?xml version=\"1.0\" encoding=\"iso-8859-1\"?>";
                ssss = h + ssss;
                offset += h.Length;
            }


            //var xtr = XmlDocumentLineInfo.CreateXmlTextReader(ssr);
            string namefile = "" + path;
            //while (!xtr.EOF)
            {
                //  IXmlLineInfo text = (IXmlLineInfo)xtr;
                XmlDocumentLineInfo doc = new XmlDocumentLineInfo("" + namefile, false);
                try
                {
                    doc.LoadXml(ssss);
                    if (doc.DocumentElement == null)
                    {
                        RProcessor.writeToLog("ERROR: No Document at " + namefile);
                        //        continue;
                    }
                    ULoader.loadAIMLNode(doc.DocumentElement, loadOpts, null);
                }
                catch (ChatSignal e)
                {
                    throw;
                }
                catch (Exception e2)
                {
                    String s = "ERROR: LoadAIMLStream in '" + loadOpts + "' " + doc.GetErrorMsg(input0, e2);
                    writeToLog(s);
                    //System.Console.Flush();
                    // if (!xtr.Read())
                    {
                        throw e2;
                    }
                    // 
                }
                finally
                {
                    //   xtr.Close();
                }
            }
            return;
        }

        private LoaderOptions EnsureOptions(LoaderOptions loadOpts, Request request1, string path)
        {
            Request request = request1;
            if (request1 == null)
            {
                writeToLog("ERROR! Ensuring Request=" + request1);
                request1 = request;
            }
            string fn = loadOpts.CurrentFilename;
            if (fn == LoaderOptions.MISSING_FILE || Unifiable.IsNullOrEmpty(fn))
            {
                writeToLog("ERROR! Ensuring loadOpts.Filename='{0}' but path='{1}'", fn, path);
            }
            fn = fn.Replace("\\", "/").ToLower();
            path = path.Replace("\\", "/").ToLower();
            if (!fn.Contains(path) && !path.Contains(fn))
            {
                // writeToLog("WARNING! Ensuring loadOpts.Filename='{0}' but path='{1}'", fn, path);
            }
            if (!request1.LoadOptions.Equals(loadOpts))
            {
                /// writeToLog("ERROR! Ensuring loadOpts.Filename='{0}' but path='{1}'", fn, path);
            }
            return loadOpts;
        }

        private void writeToLog(string f, params object[] a)
        {
            System.Console.WriteLine(f, a);
        }

        public long loadAIMLNode(XmlNode templateNode)
        {
            LoadBXML(templateNode, CurrentlyLoadingFrom);            
            return 1;
        }

        public void loadAIMLString(string s)
        {
            throw new NotImplementedException();
        }

        public void DumpErrors(OutputDelegate debugWriteLine, bool clearAfter)
        {
          //  throw new NotImplementedException();
        }

        public string CurrentlyLoadingFrom { get; set; }
    }
}
