#region

using System;
using System.Collections;
using System.Collections.Generic;
using System.IO;
using System.Net;
using System.Text;
using System.Threading;
using System.Xml;
using AltAIMLbot.Normalize;
using AltAIMLbot.Variables;
using LogicalParticleFilter1;
using MushDLR223.ScriptEngines;
using MushDLR223.Utilities;
using MushDLR223.Virtualization;

#endregion

namespace AltAIMLbot.Utils
{
    /// <summary>
    ///   A utility class for loading AIML files from disk into the graphmaster structure that 
    ///   forms an AIML bot's "brain"
    /// </summary>           
    [StructToString(true)]
    public class AIMLLoader : XmlNodeEvaluatorImpl
    {
        public static bool SeekOutAndRepair = false;

        private static readonly Dictionary<string, RenderOptions> PatternSideRenderingCache =
            new Dictionary<string, RenderOptions>();

        /// <summary>
        ///   Deletes any content that contains these words in it
        ///   TODO make this done in a  text file
        /// </summary>
        private static readonly HashSet<string> GlobalFilteredWords = new HashSet<string>
                                                                          {
                                                                              /*
                                                                              "pandorabots",
                                                                              "com",
                                                                              "dr wallice",
                                                                              "wallice",
                                                                              "wallace",
                                                                              "chatbot",
                                                                              "chatterbot",
                                                                              //  "aiml",
                                                                              "alicebots.com",
                                                                              "alice",
                                                                              "england",
                                                                              "kent",
                                                                              "a spelling",
                                                                              "webcam",
                                                                              "george bush",
                                                                              "iraq",*/
                                                                              //"terror",
                                                                          };

        public HashSet<string> FilteredWords = new HashSet<string>(GlobalFilteredWords);

        /// <summary>
        ///   Allow all chars in RawUserInput
        /// </summary>
        public bool RawUserInput;

        protected Request request;

        /// <summary>
        ///   The bot whose brain is being processed
        /// </summary>
        public AltBot bot;


        private ExternDB extDBCached = null;
        //public bool recurse;
        public bool forceReload = false;

        /// <summary>
        ///   Ctor
        /// </summary>
        /// <param name="bot"> The bot whose brain is being processed </param>
        internal AIMLLoader(AltBot bot)
        {
            this.bot = bot;
        }

        public AIMLLoader(AltBot bot, Request request)
        {
            request.loader = this;
            this.bot = bot;
            this.request = request;
        }

        public EasyLogger Logger
        {
            get { return bot.Logger; }
        }

        public LoaderOptions loadOpts
        {
           get { return request; }
        }

        public override string ToString()
        {
            return this.StructToString();
        }

        /// <summary>
        ///   Loads the AIML from files found in the path
        /// </summary>
        /// <param name="path"> </param>
        public void loadAIML(string path)
        {
            if (Directory.Exists(path))
            {
                // process the AIML
                bot.writeToLog("Starting to process AIML files found in the directory " + path);

                string[] fileEntries = HostSystem.GetFiles(path, "*.aiml");
                if (fileEntries.Length > 0)
                {
                    foreach (string filename in fileEntries)
                    {
                        loadAIMLFile(filename);
                    }
                    bot.writeToLog("Finished processing the AIML files. " + Convert.ToString(bot.Size) +
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
        ///   Given the name of a file in the AIML path directory, attempts to load it into the 
        ///   graphmaster
        /// </summary>
        /// <param name="path"> The name of the file to process </param>
        public long loadAIMLFile(string path)
        {
            long total = loadAIMLFile0(path);
            TotalCheck(path, total);
            return total;
        }

        public long loadAIMLFile0(string path)
        {
            long total = 0;
            path = ResolveToURI(path);
            //RProcessor.ReloadHooks.Add(() => loadAIMLFile0(path, loadOpts, forceReload));
            EnsureOptions(path);
            //request.TargetBot.ReloadHooks.Add(() => request.Loader.loadAIMLFile0(path, loadOpts, forceReload));

            if (!HostSystem.FileExists(path))
            {
                writeToLog("WARNING (Re)writing url: " + path);
                if (loadOpts.Recurse)
                {
                    total += loadAIMLURI0(path);
                    TotalCheck(path, total);
                }
                return total;
            }
            GraphMaster master = loadOpts.Graph;
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
                master.AddFileLoaded(path);
                return loadAIMLFile0_v2(path);
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
        ///   Given the name of a file in the AIML path directory, attempts to load it into the 
        ///   graphmaster
        /// </summary>
        /// <param name="filename"> The name of the file to process </param>
        public long loadAIMLFile0_v2(string filename)
        {
            //lock (ExternDB.mylock)
            {
                string fn = filename;
                string graphName = loadOpts.graphName;
                filename = filename.Replace("\\", "/"); // new FileInfo(fn).FullName;
                bot.writeToLog("Processing AIML file(2): " + filename);
                if (bot.UseRapstoreDB)
                {
                    GraphMaster extDB = bot.GetGraph(graphName);
                    if (extDB.wasloaded(filename))
                    {                        
                        return extDB.Size;
                    }
                    extDB.Close();
                    extDB.ensureEdb();
                }
                if (!File.Exists(filename))
                {
                    Console.WriteLine("WARNING: '{0}' does not exist as a 'file' calling stream loader", filename);
                    return loadAIMLURI(filename);
                }
                string path = filename;
                long total = 0;
                AutoClosingStream tr = HostSystem.OpenRead(path);
                try
                {
                    string pfile = request.CurrentFilename;
                    try
                    {
                        request.CurrentFilename = path;
                        loadOpts.CurrentFilename = pfile ?? path;
                        total += loadAIMLStream(tr);
                        TotalCheck(pfile, total);
                        return total;
                    }
                    finally
                    {
                        request.CurrentFilename = pfile;
                    }
                }
                finally
                {
                    HostSystem.Close(tr);
                }
            }
        }


        public long loadAIMLFile0_v3(string path)
        {
            long total = 0;
            path = ResolveToURI(path);
            //RProcessor.ReloadHooks.Add(() => loadAIMLFile0(path, loadOpts));
            EnsureOptions(path);
            //request.TargetBot.ReloadHooks.Add(() => request.Loader.loadAIMLFile0(path, loadOpts));

            if (!HostSystem.FileExists(path))
            {
                writeToLog("WARNING (Re)writing url: " + path);
                if (loadOpts.Recurse)
                {
                    total += loadAIMLURI0(path);
                    TotalCheck(path, total);
                }
                return total;
            }
            GraphMaster master = loadOpts.Graph;
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
                if (!bot.LoadFromStreamLoader)
                {
                    bot.servitor.loadAIMLFromFile(path);
                }
                else
                {
                    master.AddFileLoaded(path);
                    AutoClosingStream tr = HostSystem.OpenRead(path);
                    try
                    {
                        string pfile = request.CurrentFilename;
                        try
                        {
                            request.CurrentFilename = path;
                            total += loadAIMLStream(tr);
                            TotalCheck(pfile, total);
                        }
                        finally
                        {
                            request.CurrentFilename = pfile;
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
        ///   Given an XML document containing valid AIML, attempts to load it into the graphmaster
        /// </summary>
        /// <param name="doc"> The XML document containing the AIML </param>
        /// <param name="filename"> Where the XML document originated </param>
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
                if (bot.UseRapstoreDB)
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
                string graphName = loadOpts.graphName;
                if (bot.UseRapstore(graphName))
                {
                    GraphMaster extDB0 = bot.GetGraph(graphName);

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
                LoadBtxml(doc, filename);
                if (bot.UseRapstore(graphName))
                {
                    GraphMaster extDB0 = bot.GetGraph(graphName);
                    ExternDB extDB = extDB0.ensureEdb();
                    extDB.rememberLoaded(filename);
                    extDB0.Close();
                }
            }
            return 1;
        }

        private void processAiml(XmlNode doc, string filename)
        {
            if (doc == null)
            {
                return;
            }
            // find the name of the graph or set to default "*"
            string fooName = "z123";
            loadOpts.WithAttributes(doc, ref fooName, () => processChildren(doc, filename));
            if (fooName != "z123")
            {
                Logger.Warn("ERROR Loading to name=" + fooName);
            }
        }

        private void LoadBtxml(XmlNode currentNode, string filename)
        {
            if (!processBXML(currentNode, filename))
            {
                processImmediate(currentNode, filename);
            }
        }

        private bool processBXML(XmlNode thisNode, string filename)
        {
            if (thisNode == null) return false;
            if (IsBlank(thisNode)) return true;
            if (thisNode.Name == null)
            {
                return false;
            }
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
                processTopic(thisNode, filename);
                return true;
            }
            if (named == "state")
            {
                processState(thisNode, filename);
                return true;
            }
            else if (named == "category")
            {
                processCategory(thisNode, filename);
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
                foreach (string r in remove)
                {
                    node.Attributes.RemoveNamedItem(r);
                }
            }
            XmlNodeList cns = node.ChildNodes;
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
        ///   Processes certain normal "template" side commands at the top level
        /// </summary>
        /// <param name="node"> the "topic" node </param>
        /// <param name="filename"> the file from which this node is taken </param>
        public object processImmediate(XmlNode node, string filename)
        {
            if (node == null) return null;

            try
            {
                object result = bot.BotBehaving.evalTemplateNode(node, RequestKind.AIMLLoader | RequestKind.BotPropertyEval);
                if (AltBot.tl_aimlResult != null)
                {
                    AltBot.tl_aimlResult.AddResult(node, filename, result);
                }
                return result;
            }
            catch (Exception e)
            {
                bot.writeToLog("WARNING! Error " + e.Message + " Stack=" + e.StackTrace + " and Code = " +
                               node.OuterXml + " produced by a code in the file: " + filename);
                return e;
            }
        }

        /// <summary>
        ///   Given a "topic" node, processes all the categories for the topic and adds them to the 
        ///   graphmaster "brain"
        /// </summary>
        /// <param name="node"> the "topic" node </param>
        /// <param name="filename"> the file from which this node is taken </param>
        internal void processTopic(XmlNode node, string filename)
        {
            loadOpts.WithAttributes(node, ref loadOpts.topicName, () =>
                                                                  processChildren(node, filename));
        }

        private void processChildren(XmlNode node, string filename)
        {
            if (IsBlank(node)) return;
            // process all the category nodes
            foreach (XmlNode thisNode in node.ChildNodes)
            {
                LoadBtxml(thisNode, filename);
            }
        }

        /// <summary>
        ///   Given a "state" node, processes all the categories for the state and adds them to the 
        ///   graphmaster "brain"
        /// </summary>
        /// <param name="node"> the "state" node </param>
        /// <param name="filename"> the file from which this node is taken </param>
        private void processState(XmlNode node, string filename)
        {
            loadOpts.WithAttributes(node, ref loadOpts.stateNamePre, () =>
                                                                     processChildren(node, filename));
        }

        /// <summary>
        ///   Adds a preprocessed path and content to the  graphmaster structure
        /// </summary>
        /// <param name="node"> the XML node containing the category </param>
        /// <param name="filename"> the file from which this category was taken </param>
        private void processSer(XmlNode node, string filename)
        {
            GraphMaster ourGraphMaster = bot.GetGraph(loadOpts.graphName) ?? bot.Graphmaster;

            XmlNode template = FindNode("template", node);
            string categoryPath = "";

            categoryPath = node.Attributes["path"].Value;
            if ((categoryPath == null) || (categoryPath.Length == 0)) return;
            categoryPath = categoryPath.Trim();
            if ((categoryPath == null) || (categoryPath.Length == 0)) return;

            if (Equals(null, template))
            {
                bot.writeToLog("Missing template tag in the node with path: " + categoryPath + " found in " +
                               filename);
                return;
            }

            // o.k., add the processed AIML to the GraphMaster structure
            if (categoryPath.Length > 0)
            {
                AddCateAndTemplate(filename, template, ourGraphMaster, categoryPath);
            }
            else
            {
                bot.writeToLog(
                    "WARNING! Attempted to load a new category with an empty pattern where the path = " + categoryPath +
                    " and template = " + template.OuterXml + " produced by a category in the file: " + filename);
            }
        }

        private void AddCateAndTemplate(string filename, XmlNode template, GraphMaster ourGraphMaster, string categoryPath)
        {
            try
            {
                // //this.bot.Graphmaster.addCategory(categoryPath, template.OuterXml, filename, 1, 1);
                //ourGraphMaster.addCategory(categoryPath, template.OuterXml, filename, 1, 1);
                string graphName = loadOpts.graphName;
                if (bot.UseRapstore(graphName))
                {
                    GraphMaster G = bot.GetGraph(graphName);
                    G.Size++;
                    ExternDB extDB = G.ensureEdb();
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
                bot.writeToLog("ERROR! Failed to load a new category into the graphmaster where the path = " +
                               categoryPath + " and template = " + template.OuterXml +
                               " produced by a category in the file: " + filename);
            }
        }

        /// <summary>
        ///   Adds a category to the graphmaster structure using the default topic ("*")
        /// </summary>
        /// <param name="node"> the XML node containing the category </param>
        /// <param name="filename"> the file from which this category was taken </param>
        public void processCategory(XmlNode node, string filename)
        {
            processCategory(node, node.ParentNode, loadOpts, loadOpts.AdditionalPreconditions);
        }


        private void CategoryCheck(XmlNode node, string filename)
        {
            if (Equals(null, FindNode("pattern", node)))
            {
                throw new XmlException("Missing pattern tag in a node found in " + filename + " from " + node.OuterXml);
            }
            if (Equals(null, FindNode("template", node)))
            {
                if (false)
                    throw new XmlException("Missing template tag in the node with pattern: " + node.OuterXml +
                                           " found in " + filename);
            }
        }

        /// <summary>
        ///   Adds a category to the graphmaster structure using the given topic
        /// </summary>
        /// <param name="node"> the XML node containing the category </param>
        /// <param name="topicName"> the topic to be used </param>
        /// <param name="stateNamePre"> the state in the front </param>
        /// <param name="stateNamePost"> the state at the end (tie breaker) </param>
        /// <param name="filename"> the file from which this category was taken </param>
        private void processCategory(XmlNode node, string graphName, string topicName, string that,
                                        string stateNamePre, string stateNamePost,
                                        string filename)
        {
            if (node is IXmlLineInfo)
            {
                int li = ((IXmlLineInfo) node).LineNumber;
                if (li != 0)
                {
                    filename += ":" + li;
                }
            }

            // reference and check the required nodes
            CategoryCheck(node, filename);
            graphName = node.AttributeValueOfDefault("graph", graphName);
            List<KeyValuePair<string, string>> categoryPaths = generatePaths(node, graphName, topicName, that,
                                                                             stateNamePre, stateNamePost, false);
            // o.k., add the processed AIML to the GraphMaster structure
            int found = 0;
            foreach (var pair in categoryPaths)
            {
                found++;
                string categoryPath = pair.Key;
                string templateXML = pair.Value;
                try
                {
                    //this.bot.Graphmaster.addCategory(categoryPath, template.OuterXml, filename, 1, 1);
                    if (bot.UseRapstore(graphName))
                    {
                        ExternDB extDB = bot.GetGraph(graphName).ensureEdb();
                        Node.addCategoryDB("", categoryPath, templateXML, filename, 1, 1, "", extDB);
                    }
                    else
                    {
                        GraphMaster ourGraphMaster = bot.GetGraph(loadOpts.graphName) ?? bot.Graphmaster;
                        ourGraphMaster.addCategory(categoryPath, templateXML, filename, 1, 1);
                    }
                    // keep count of the number of categories that have been processed
                    bot.SizeC++;
                }
                catch (Exception e)
                {
                    bot.writeToLog("ERROR! Failed to load a new category into the graphmaster where the path = " +
                                   categoryPath + " and template = " + templateXML +
                                   " produced by a category in the file: " + filename + "because " + e.Message +
                                   " " + e.StackTrace);
                }
            }
            if (found == 0)
            {
                bot.writeToLog(
                    "WARNING! Attempted to load a new category with an empty pattern: " +
                    node.OuterXml + " produced by a category in the file: " +
                    filename);
            }
        }

        /// <summary>
        ///   Generates a path from a category XML node and topic name
        /// </summary>
        /// <param name="node"> the category XML node </param>
        /// <param name="topicName"> the topic </param>
        /// <param name="stateNamePre"> the state in the front </param>
        /// <param name="stateNamePost"> the state at the end (tie breaker) </param>
        /// <param name="isUserInput"> marks the path to be created as originating from user input - so normalize out the * and _ wildcards used by AIML </param>
        /// <returns> The appropriately processed path </returns>
        private List<KeyValuePair<string, string>> generatePaths(XmlNode node, string graphName, string topicName,
                                                                 string thatIn,
                                                                 string stateNamePre, string stateNamePost,
                                                                 bool isUserInput)
        {
            if (!isUserInput && thatIn != "*")
            {
                bot.RaiseError("Stange entery to " + this);
            }
            var lretval = new List<KeyValuePair<string, string>>();
            // get the nodes that we need
            List<XmlNode> patterns = FindNodes("pattern", node);
            List<XmlNode> thats = FindNodes("that", node);
            List<XmlNode> templates = FindNodes("template", node);
            if (thats.Count == 0) thats = new List<XmlNode> {XmlStar.Value};

            foreach (XmlNode template in templates)
            {
                string templateXML = InnerTextOrXML(template, false) ?? "";
                foreach (XmlNode that in thats)
                {
                    string thatText = InnerTextOrXML(that, true) ?? loadOpts.currentThat;
                    foreach (XmlNode pattern in patterns)
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
            if (Equals(null, pattern))
            {
                return null;
            }
            string xo = pattern.InnerXml;
            if (!xo.Contains("<") && !xo.Contains("&"))
            {
                xo = pattern.InnerText;
            }

            if (xo != xo.Trim() || xo.IndexOfAny("\n\r\t\b\"".ToCharArray()) != -1)
            {
                if (isPatternMatcher)
                {
                    xo = ReTrimAndspace(xo);
                }
            }
            return xo;
        }

        /// <summary>
        ///   Given a name will try to find a node named "name" in the childnodes or return null
        /// </summary>
        /// <param name="name"> The name of the node </param>
        /// <param name="node"> The node whose children need searching </param>
        /// <returns> The node (or null) </returns>
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

        public string generatePath(string graphName, string pattern, string that, string topicName, string stateNamePre,
                           string stateNamePost, bool isUserInput)
        {
            return generateCPath(graphName, pattern, that, loadOpts.currentFlags, topicName, stateNamePre, stateNamePost,
                                 isUserInput, null, bot);
        }
        /// <summary>
        ///   Generates a path from the passed arguments
        /// </summary>
        /// <param name="pattern"> the pattern </param>
        /// <param name="that"> the that </param>
        /// <param name="topicName"> the topic </param>
        /// <param name="stateNamePre"> the state in the front </param>
        /// <param name="stateNamePost"> the state at the end (tie breaker) </param>
        /// <param name="isUserInput"> marks the path to be created as originating from user input - so normalize out the * and _ wildcards used by AIML </param>
        /// <returns> The appropriately processed path </returns>
        static public string generateCPath(string graphName, string pattern, string that, string flag, string topicName, string stateNamePre,
                                   string stateNamePost, bool isUserInput, Func<Unifiable, bool, Unifiable> innerFormater, AltBot bot)
        {
            // to hold the normalized path to be entered into the graphmaster
            var normalizedPath = new StringBuilder();
            string normalizedPattern = "*";
            string normalizedThat = "*";
            string normalizedTopic = "*";
            string normalizedStatePre = "*";
            string normalizedStatePost = "*";
            string normalizedGraphName = "*";

            if ((bot.TrustAIML) & (!isUserInput))
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
                var Normalize = new Func<string, bool, string>((a, b) => Normalize3(a, b, bot));
                normalizedPattern = Normalize(pattern, isUserInput).Trim();
                normalizedThat = Normalize(that, isUserInput).Trim();
                normalizedTopic = Normalize(topicName, isUserInput).Trim();
                normalizedStatePre = Normalize(stateNamePre, isUserInput).Trim();
                normalizedStatePost = Normalize(stateNamePost, isUserInput).Trim();
                normalizedGraphName = Normalize(graphName, isUserInput).Trim();
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
                if (normalizedThat.Length > bot.MaxThatSize)
                {
                    normalizedThat = "*";
                }

                //KHC: we could dump them all in an hash table then use a setting
                //     string to define the sequence
                // o.k. build the path :standard <pattern><that><state><topic>
                if (0 == 0)
                {
                    normalizedPath.Append("TAG-STATE ");
                    normalizedPath.Append(normalizedGraphName);
                    normalizedPath.Append(" ");
                    normalizedPath.Append(normalizedStatePre);
                    normalizedPath.Append(" TAG-INPUT ");
                    normalizedPath.Append(normalizedPattern);
                    normalizedPath.Append(" TAG-THAT ");
                    normalizedPath.Append(normalizedThat);
                    normalizedPath.Append(" TAG-STATE ");
                    normalizedPath.Append(normalizedStatePost);
                    normalizedPath.Append(" TAG-TOPIC ");
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
        ///   Given an input, provide a normalized output
        /// </summary>
        /// <param name="input"> The string to be normalized </param>
        /// <param name="isUserInput"> True if the string being normalized is part of the user input path - flags that we need to normalize out * and _ chars </param>
        /// <returns> The normalized string </returns>
        static private string Normalize3(string input, bool isUserInput, AltBot bot)
        {
            if (input == null)
            {
                return "NULL-ERROR";
            }
            if (input == "*") return input;
            string r = Normalize0(input, isUserInput, bot);
            if (input.Contains("_"))
            {
                if (r.Trim() != input.Trim())
                {
                    r = Normalize0(input, isUserInput, bot);
                }
            }
            return r;
        }

        static private string Normalize0(string input, bool isUserInput, AltBot bot)
        {
            if (input == null) return "NULL-ERROR";
            var result = new StringBuilder();

            // objects for normalization of the input
            var substitutor = new ApplySubstitutions(bot);
            var stripper = new StripIllegalCharacters(bot);

            string substitutedInput = substitutor.TransformU(input);
            // split the pattern into it's component words
            string[] substitutedWords = substitutedInput.Split(" \r\n\t".ToCharArray());

            // Normalize all words unless they're the AIML wildcards "*" and "_" during AIML loading
            foreach (string word in substitutedWords)
            {
                if (word == "") continue;
                string normalizedWord;
                if (isUserInput)
                {
                    normalizedWord = stripper.TransformU(word);
                }
                else
                {
                    if ((word == "*") || (word == "_"))
                    {
                        normalizedWord = word;
                    }
                    else
                    {
                        normalizedWord = stripper.TransformU(word);
                    }
                }
                result.Append(normalizedWord.Trim() + " ");
            }

            return result.ToString().Replace("  ", " ").Trim(); // make sure the whitespace is neat
        }


        private void TotalCheck(string path, long total)
        {
        }

        public static string GetBaseDirectory(LoaderOptions loadOpts)
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
        ///   Given the name of a file in the AIML path directory, attempts to load it into the 
        ///   graphmaster
        /// </summary>
        /// <param name="path"> The name of the file to process </param>
        public long loadAIMLFileStream(string path)
        {
            long total = loadAIMLFile0(path);
            TotalCheck(path, total);
            return total;
        }

        public long loadAIMLNode(XmlNode templateNode)
        {
            LoadBtxml(templateNode, loadOpts.CurrentlyLoadingFrom);
            return 1;
        }

        //public void loadAIML(string path)
        //{
        //    LoaderOptions loadOpts = request.loader;
        //    request.Loader.loadAIMLFromURI(path);
        //}
        /// <summary>
        ///   Ctor
        /// </summary>
        /// <param name="bot"> The bot whose brain is being processed </param>
        /// <summary>
        ///   Loads the AIML from files found in the RProcessor's AIMLpath into the RProcessor's brain
        /// </summary>
        /// <summary>
        ///   Loads the AIML from files found in the path
        /// </summary>
        /// <param name="path"> </param>
        public long loadAIMLDir(string path)
        {
            long total = 0;

            int before = loadOpts.Graph.Size;
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
                writeToLog("Finished processing the AIML files. " + Convert.ToString(before - loadOpts.Graph.Size) +
                           " categories processed. saved0-2 {0}-{1}-{2} ", saved0, saved1, saved2);
            }

            return total;
        }

        public long loadAIMLDir0(string path)
        {
            long total = 0;
            path = ResolveToURI(path);
            loadOpts.CurrentlyLoadingFrom = path;
            LoaderOptions savedOpt = request.LoadOptions;
            string[] fileEntries = HostSystem.GetFiles(path, "*.aiml");
            if (fileEntries.Length > 0)
            {
                foreach (string f in fileEntries)
                {
                    try
                    {
                        try
                        {
                            request.LoadOptions = savedOpt;
                            request.CurrentFilename = f;
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
                        loadOpts.Graph.RemoveFileLoaded(path);
                        writeToLog("Error in loadAIMLFile " + ee);
                    }
                }
            }
            else
            {
                writeToLog("Could not find any .aiml files in the specified directory (" + path +
                           "). Please make sure that your aiml file end in a lowercase aiml extension, for example - myFile.aiml is valid but myFile.AIML is not.");
            }

            if (loadOpts.Recurse)
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
            string path = ResolveToURI0(pathIn);
            if (path != pathIn)
            {
                if (DLRConsole.DebugLevel > 6) writeToLog("ResolveToURI '{0}'->'{1}'", pathIn, path);
            }
            return path;
        }

        private string ResolveToURI0(string pathIn)
        {
            pathIn = HostSystem.ActualExistingPathIfExists(pathIn) ?? pathIn;

            string baseFile = GetBaseDirectory(loadOpts);
            string inPath = HostSystem.ToRelativePath(pathIn, baseFile);
            IEnumerable<string> combine;
            //combine = baseFile != null ? new[] { "./", baseFile, "aiml/" } : new[] { "./", "aiml/" };
            string prefix;
            combine = TargetBot.RuntimeDirectories;
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

        protected AltBot TargetBot
        {
            get { return bot; }
        }

        private R LoaderOper<R>(Func<R> action, GraphMaster gm)
        {
            OutputDelegate prev = userTraceRedir;
            try
            {
                userTraceRedir = gm.writeToLog;
                try
                {
                    if (!loadOpts.NeedsLoaderLock) return action();
                    lock (ErrorList)
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
                    if (!ChatOptions.AllowRuntimeErrors) return default(R);
                    throw;
                    //return default(R);
                }
            }
            finally
            {
                userTraceRedir = prev;
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
            loadOpts.SuggestPath(path);
            long total = LoaderOper(() => loadAIMLURI0(path), loadOpts.Graph);
            TotalCheck(path, total);
            return total;
        }

        public long loadAIMLURI0(string path0)
        {
            string path = path0;
            loadOpts.CurrentlyLoadingFrom = path;
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
                    var savedOpt = request.LoadOptions;
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
                        request.LoadOptions.SetLoaderOptions(savedOpt);
                    }
                    return total;
                }
                else if (HostSystem.FileExists(path))
                {
                    string hostSystemGetBaseDir = HostSystem.GetBaseDir(pathIn);
                    string currentPath = HostSystem.ToRelativePath(pathIn, hostSystemGetBaseDir);
                    LoaderOptions savedOpt = request.LoadOptions;
                    string pop = null;
                    try
                    {
                        pop = bot.PushSearchPath(hostSystemGetBaseDir);
                        //total += loadAIMLFile0(path, loadOpts, true);
                        total += loadAIMLFile(currentPath);
                    }
                    finally
                    {
                        request.LoadOptions = savedOpt;
                        bot.PopSearchPath(pop);
                    }
                    return total;
                }
                else if (!HostSystem.IsWildPath(path) && Uri.IsWellFormedUriString(path, UriKind.RelativeOrAbsolute))
                {
                    writeToLog("Processing AIML URI: " + path);
                    var uri = new Uri(path);
                    if (uri.IsFile)
                    {
                        total += loadAIMLURI0(uri.AbsolutePath);
                        TotalCheck(uri.AbsolutePath, total);
                        return total;
                    }
                    WebRequest req = WebRequest.Create(uri);
                    WebResponse resp = req.GetResponse();
                    Stream stream = resp.GetResponseStream();
                    LoaderOptions savedOpt = request.LoadOptions;
                    try
                    {
                        loadOpts.CurrentFilename = uri.ToString();
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
                    string[] pathnames = HostSystem.GetWildFiles(path, out dir, out filemask);
                    string pop = null;
                    try
                    {
                        LoaderOptions savedOpt = request.LoadOptions;
                        pop = bot.PushSearchPath(HostSystem.GetBaseDir(dir));
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
                        bot.PopSearchPath(pop);
                    }
                }

                String nf = "ERROR: XmlTextReader of AIML files (" + path + ")";
                var nfe = new FileNotFoundException(nf);
                bot.writeToLog(nfe);
                writeToLog(nf);
                throw nfe;
            }
            catch (ChatSignal e)
            {
                throw;
            }
            catch (Exception e)
            {
                bot.writeToLog(e);
                writeToLog("ERROR! " + e);
                throw e;
            }
        }

 
        /// <summary>
        ///   Given an XML document containing valid AIML, attempts to load it into the graphmaster
        /// </summary>
        /// <param name="doc"> The XML document containing the AIML </param>
        /// <param name="loadOpts"> Where the XML document originated </param>
        public void loadAIMLString(string docString)
        {
            //            RProcessor0.ReloadHooks.Add(() => loadAIMLFile0(path, loadOpts));
            string path = request.CurrentFilename;
            EnsureOptions(path);
            try
            {
                byte[] byteArray = Encoding.ASCII.GetBytes(docString);
                var stream = new MemoryStream(byteArray);
                loadAIMLStream(stream);
            }
            catch (ChatSignal e)
            {
                throw;
            }
            catch (Exception e2)
            {
                String s = "ERROR loadAIMLString '" + docString + "' " + loadOpts;
                s = s + "\n" + e2.Message + "\n" + e2.StackTrace + "\n" + s;
                writeToLog(s);
                throw e2;
            }

            return;
        }

        /// <summary>
        ///   Given an XML document containing valid AIML, attempts to load it into the graphmaster
        /// </summary>
        /// <param name="doc"> The XML document containing the AIML </param>
        /// <param name="loadOpts"> Where the XML document originated </param>
        public long loadAIMLStream(Stream input0)
        {
            long total = 0;
            DateTime oneMinuteFromNow = AltBot.Now + TimeSpan.FromMinutes(1);
            if (request.TimesOutAt < oneMinuteFromNow)
            {
                request.TimesOutAt = oneMinuteFromNow + TimeSpan.FromMinutes(1);
                writeToLog("Bumping up timelimit for " + loadOpts);
            }
            string path = request.CurrentFilename;
            EnsureOptions(path);

            XmlReader xtr = XmlDocumentLineInfo.CreateXmlTextReader(input0);
            string namefile = "" + path;
            var doc = new XmlDocumentLineInfo("" + namefile, false);
            while (!xtr.EOF && xtr.ReadState != ReadState.Closed)
            {
                //  IXmlLineInfo text = (IXmlLineInfo)xtr;
                try
                {
                    doc.SetNodesReadOnly = true;
                    doc.IsFile = true;
                    return DocLoad(xtr, doc, loadOpts.CurrentFilename);

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
                    //extDB.SaveIndex();
                    GraphMaster extDB0 = this.bot.GetGraph(loadOpts.graphName);
                    if (extDB0 != null)
                    {
                        extDB0.Close();
                    }
                }
            }
            return total;
        }

        private long DocLoad(XmlReader reader, XmlDocumentLineInfo doc,  string filename)
        {
            IXmlLineInfo lireader = (IXmlLineInfo) reader;
            try
            {
                //doc = new XmlDocumentLineInfo(filename, true);
                // Skip over the XML declaration
                reader.MoveToContent();
                doc.Load(reader);
                if (doc.DocumentElement == null)
                {
                    bot.writeToLog("ERROR: No Document at " + filename);
                    //        continue;
                    return 0;
                }
                else
                {
                    return loadAIMLFromXML(doc, filename);
                    //return loadAIMLNode(doc.DocumentElement, loadOpts, Request);
                }
            }
            catch (XmlException e)
            {
                Console.WriteLine("================= XML ERROR ==========================");
                Console.WriteLine(" FILENAME:" + filename);
                if (reader != null)
                {
                    Console.WriteLine("XmlReader Line, pos: (" + lireader.LineNumber + "," + lireader.LinePosition + ")");
                    Console.WriteLine("XmlReader Value, value: (" + reader.Value + ")");
                    Console.WriteLine("XmlReader Value, LocalName: (" + reader.LocalName + ")");
                    Console.WriteLine("XmlReader Value, ReadState: (" + reader.ReadState.ToString() + ")");
                }
                if ((doc != null) && (doc.ParentNode != null))
                {
                    Console.WriteLine("doc, ParentNode: (" + doc.ParentNode + ")");
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
                    Console.WriteLine("XmlReader Line, pos: (" + lireader.LineNumber + "," + lireader.LinePosition + ")");
                    Console.WriteLine("XmlReader Value, value: (" + reader.Value + ")");
                    Console.WriteLine("XmlReader Value, LocalName: (" + reader.LocalName + ")");
                    Console.WriteLine("XmlReader Value, ReadState: (" + reader.ReadState.ToString() + ")");
                }
                if ((doc != null) && (doc.ParentNode != null))
                {
                    Console.WriteLine("doc, ParentNode: (" + doc.ParentNode + ")");
                }
                Console.WriteLine(" XmlReader Line, pos: (" + lireader.LineNumber + "," + lireader.LinePosition + ")");
                Console.WriteLine(" XmlReader Line, source: (" + e.Source + ")");
                Console.WriteLine(" XmlException Source:" + e.Source);
                if ((e.InnerException != null) && (e.InnerException.Message != null))
                {
                    Console.WriteLine(" XmlException InnerException.Message:" + e.InnerException.Message);
                }
                Thread.Sleep(5000);
                return 0;
            }

        }

        /// <summary>
        ///   Given an XML document containing valid AIML, attempts to load it into the graphmaster
        /// </summary>
        /// <param name="doc"> The XML document containing the AIML </param>
        /// <param name="loadOpts"> Where the XML document originated </param>
        public void loadAIMLStreamFallback(Stream input0)
        {
            string path = request.CurrentFilename;
            EnsureOptions(path);
            path = ResolveToURI(path);

            var strmreader = new StreamReader(input0);
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
                var doc = new XmlDocumentLineInfo("" + namefile, false);
                try
                {
                    doc.LoadXml(ssss);
                    if (doc.DocumentElement == null)
                    {
                        bot.writeToLog("ERROR: No Document at " + namefile);
                        //        continue;
                    }
                    loadAIMLNode(doc.DocumentElement, loadOpts, request);
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

        private void EnsureOptions(string path)
        {
            loadOpts.SuggestPath(path);
            if (request == null)
            {
                writeToLog("ERROR! Ensuring Request=" + request);
                request = request;
            }
            if (request != loadOpts)
            {
                writeToLog("ERROR! loadOpts wrong Request=" + request);
            }
            string fn = loadOpts.CurrentFilename;
            if (fn == LoaderOptions.MISSING_FILE || CommonStaticUtils.IsNullOrEmpty(fn))
            {
                writeToLog("ERROR! Ensuring loadOpts.Filename='{0}' but path='{1}'", fn, path);
            }
            fn = fn.Replace("\\", "/").ToLower();
            path = path.Replace("\\", "/").ToLower();
            if (!fn.Contains(path) && !path.Contains(fn))
            {
                writeToLog("WARNING! Ensuring loadOpts.Filename='{0}' but path='{1}'", fn, path);
            }
            if (!request.Equals(loadOpts))
            {
                writeToLog("ERROR! Ensuring loadOpts.Filename='{0}' but path='{1}'", fn, path);
            }
        }

        public long InsideLoaderContext(XmlNode currentNode, Request request, SubQuery query,
                                        LoaderOptions loadOpts, Func<long> doit)
        {
            long total = 0;
            query = query ?? request.CurrentQuery;
            //Result result = query.Result;
            AIMLLoader prev = bot.Loader;
            GraphMaster loadOptsPrevGraph = loadOpts.Graph;
            try
            {
                bot.Loader = this;
                // Get a list of the nodes that are children of the <aiml> tag
                // these nodes should only be either <topic> or <category>
                // the <topic> nodes will contain more <category> nodes
                //string currentNodeName = currentNode.Name.ToLower();

                var ts = EnterTag(request, currentNode, query);
                try
                {
                    loadOpts.Graph = request.Graph;
                    long done = doit();
                    total += done;
                    // TotalCheck(currentNode.OuterXml, total, null);
                }
                finally
                {
                    ts();
                    loadOpts.Graph = loadOptsPrevGraph;
                }
            }
            finally
            {
                bot.Loader = prev;
            }
            return total;
        }


        private long loadAIMLNodes(IEnumerable nodes, LoaderOptions loadOpts, Request request,
                                   List<ConversationCondition> additionalRules)
        {
            long total = 0;
            if (nodes != null)
            {
                loadOpts.AdditionalPreconditions = additionalRules;
                foreach (object o in nodes)
                {
                    var node = o as XmlNode;
                    total += loadAIMLNode(node, loadOpts, request);
                    //TotalCheck(TextAndSourceInfo(node), total);
                }
            }
            return total;
        }

        public long loadAIMLNode(XmlNode currentNode, LoaderOptions loadOpts, Request request)
        {
            List<ConversationCondition> additionalRules = loadOpts.AdditionalPreconditions;
            long total = LoaderOper(() => loadAIMLNode0(currentNode, loadOpts, request, additionalRules),
                                    loadOpts.Graph);
            //TotalCheck(TextAndSourceInfo(currentNode), total);
            return total;
        }

        public long loadAIMLNode0(XmlNode currentNode, LoaderOptions loadOpts, Request request,
                                  List<ConversationCondition> additionalRules)
        {
            if (request.bot.useServitor)
            {
                Console.WriteLine("Check: loadAIMLFromXML enter(3)");

                loadAIMLFromXML(currentNode, loadOpts.CurrentFilename);
                return 1;
            }

            if (currentNode == null)
            {
                writeToLog("ERROR: no currentNode in " + loadOpts);
                return 0;
            }
            long total = 0;
            AIMLLoader prev = bot.Loader;
            try
            {
                bot.Loader = this;
                // Get a list of the nodes that are children of the <aiml> tag
                // these nodes should only be either <topic> or <category>
                // the <topic> nodes will contain more <category> nodes
                string currentNodeName = currentNode.Name.ToLower();
                if (currentNodeName == "aiml")
                {
                    total += InsideLoaderContext(currentNode, request, request.CurrentQuery, loadOpts,
                                                 () =>
                                                 loadAIMLNodes(currentNode.ChildNodes, loadOpts, request,
                                                               additionalRules));
                    return total;
                }
                else if (currentNodeName == "topic")
                {
                    additionalRules = PushAddtionalRuleContext(additionalRules);
                    Unifiable topicName = StaticXMLUtils.GetAttribValue(currentNode, "name,topic", Unifiable.STAR);
                    var newConversationCondition = new ConversationCondition("topic", topicName,
                                                                             currentNode);
                    additionalRules.Add(newConversationCondition);
                    // "getNode("<pretest name=\"topic\">" + topicName + "</pretest>", currentNode))
                    List<CategoryInfo> vv = processTopic(currentNode, currentNode.ParentNode, loadOpts, additionalRules);
                    additionalRules.Remove(newConversationCondition);
                    total += vv.Count;
                    return total;
                }
                else if (currentNodeName == "category")
                {
                    List<CategoryInfo> vv = processCategory(currentNode, currentNode.ParentNode, loadOpts,
                                                            additionalRules);
                    total += vv.Count;
                    return total;
                }
                else if (currentNodeName == "that")
                {
                    additionalRules = PushAddtionalRuleContext(additionalRules);
                    string valueThat = GetAttribValue(currentNode, "value,that", currentNode.InnerXml);
                    var thatRule = new ConversationCondition("that", valueThat, currentNode);
                    additionalRules.Add(thatRule);
                    total +=
                        InsideLoaderContext(currentNode, request, request.CurrentQuery, loadOpts,
                                            () =>
                                            loadAIMLNodes(
                                                currentNode.ChildNodes,
                                                loadOpts,
                                                request,
                                                additionalRules));
                    additionalRules.Remove(thatRule);
                    return total;
                }
                else if (currentNodeName == "meta" || currentNodeName == "#comment")
                {
                    // skip fo now
                    return 0;
                }
                else if (currentNodeName == "#text")
                {
                    writeDebugLine("skipping #text: " + currentNode);
                    return 0;
                }
                else if (currentNodeName == "template")
                {
                    bot.ImmediateAiml(currentNode, request, this,
                                                      request.RequestType | RequestKind.TemplateExpander |
                                                      RequestKind.AIMLLoader);
                    total += 1;
                }
                ISettingsDictionary dict = IsSettingsTag(currentNodeName, request);
                if (dict != null)
                {
                    SettingsDictionaryReal.loadSettingNode(dict, currentNode, SettingsPolicy.Default,
                                                           request);
                }
                else
                {
                    if (false && !request.IsToplevelRequest && !request.SraiDepth.IsOverMax)
                    {
                        Request res = request.CreateSubRequest(ToTemplateXML(currentNode), null,
                                                               request.RequestType | RequestKind.TemplateExpander |
                                                               RequestKind.AIMLLoader);
                        res.IsToplevelRequest = true;
                        bot.ImmediateAiml(currentNode, res, this,
                                                          request.RequestType | RequestKind.TemplateExpander |
                                                          RequestKind.AIMLLoader);
                        total += 1;
                    }
                    else
                    {
                        if (request.NoImmediate) return 0;
                        bot.ImmediateAiml(currentNode, request, this,
                                                          request.RequestType | RequestKind.TemplateExpander |
                                                          RequestKind.AIMLLoader);
                        total += 1;
                    }
                }
            }
            finally
            {
                bot.Loader = prev;
            }
            return total;
        }

        private ISettingsDictionary IsSettingsTag(string tag, Request request)
        {
            switch (tag)
            {
                case "vars":
                case "root":
                case "predicates": //CML
                    return request.TargetSettings;
                case "properties":
                case "bots":
                    return request.TargetBot.GlobalSettings;
                case "substitutions":
                    return request.TargetBot.InputSubstitutions;
                default:
                    return null;
            }
        }

        private List<ConversationCondition> PushAddtionalRuleContext(List<ConversationCondition> nodes)
        {
            var newList = new List<ConversationCondition>();
            ;
            if (nodes != null) newList.AddRange(nodes);
            return newList;
        }

        public override IEnumerable<XmlNodeEval> GetEvaluators(XmlNode node)
        {
            return base.GetEvaluatorsFromReflection(node);
        }

        public virtual IEnumerable<XmlNode> Eval_Element_NodeType(XmlNode src, Request request,
                                                                  OutputDelegate outputdelegate)
        {
            //XmlNode node = 
            loadAIMLNode(src, request, request);
            //if (node == null) return NO_XmlNode;
            return new[] {src};
        }


        /// <summary>
        ///   Given a "topic" topicNode, processes all the categories for the topic and adds them to the 
        ///   graphmaster "brain"
        /// </summary>
        /// <param name="topicNode"> the "topic" node </param>
        /// <param name="path"> the file from which this topicNode is taken </param>
        public List<CategoryInfo> processTopic(XmlNode topicNode, XmlNode outerNode, LoaderOptions path,
                                               List<ConversationCondition> additionalRules)
        {
            // find the name of the topic or set to default "*"
            Unifiable topicName = GetAttribValue(topicNode, "name,topic", Unifiable.STAR);
            if (IsNullOrEmpty(topicName))
            {
                topicName = GetAttribValue(topicNode, "name,topic", Unifiable.STAR);
            }
            // process all the category nodes
            foreach (XmlNode cateNode in topicNode.ChildNodes)
            {
                if (cateNode.Name == "category")
                {
                    processCategoryWithTopic(cateNode, topicName, topicNode, path, additionalRules);
                    continue;
                }
                else if (cateNode.Name == "guard")
                {
                    additionalRules.Add(new ConversationCondition(cateNode));
                    continue;
                }
                AddErrorCategory("UNKNOWN NODETYPE ", cateNode);
                processCategoryWithTopic(cateNode, topicName, topicNode, path, additionalRules);
            }
            //additionalRules = prev;
            return path.CategoryInfos;
        }

        /// <summary>
        ///   Given a "topic" topicNode, processes all the categories for the topic and adds them to the 
        ///   graphmaster "brain"
        /// </summary>
        /// <param name="topicNode"> the "topic" node </param>
        /// <param name="path"> the file from which this topicNode is taken </param>
        public List<CategoryInfo> processOuterThat(XmlNode thatNode, XmlNode outerNode, LoaderOptions path,
                                                   List<ConversationCondition> additionalRules)
        {
            List<CategoryInfo> CIS = path.CategoryInfos;
            List<CategoryInfo> newCats = path.CategoryInfos = new List<CategoryInfo>();
            // find the name of the topic or set to default "*"
            Unifiable thatPattten = GetAttribValue(thatNode, "pattern,value,name", Unifiable.STAR);
            // process all the category nodes
            Unifiable newThatInfo = path.Graph.FindThat(thatNode, thatPattten);
            foreach (XmlNode cateNode in thatNode.ChildNodes)
            {
                // getting stacked up inside
                loadAIMLNode0(cateNode, path, request, additionalRules);
            }
            foreach (CategoryInfo ci0 in newCats)
            {
                ci0.AddPrecondition(newThatInfo);
            }

            CIS.AddRange(newCats);
            return path.CategoryInfos = CIS;
        }

        /// <summary>
        ///   Adds a category to the graphmaster structure using the default topic ("*")
        /// </summary>
        /// <param name="cateNode"> the XML node containing the category </param>
        /// <param name="path"> the file from which this category was taken </param>
        public List<CategoryInfo> processCategory(XmlNode cateNode, XmlNode outerNode, LoaderOptions path,
                                                  List<ConversationCondition> additionalRules)
        {
            return processCategoryWithTopic(cateNode, Unifiable.STAR, outerNode, path, additionalRules);
        }

        /// <summary>
        ///   Adds a category to the graphmaster structure using the given topic
        /// </summary>
        /// <param name="cateNode"> the XML node containing the category </param>
        /// <param name="topicName"> the topic to be used </param>
        /// <param name="loadOpts"> the file from which this category was taken </param>
        private List<CategoryInfo> processCategoryWithTopic(XmlNode cateNode, Unifiable topicName, XmlNode outerNode,
                                                            LoaderOptions loadOpts,
                                                            List<ConversationCondition> additionalRules)
        {
            List<CategoryInfo> CIs = loadOpts.CategoryInfos;
            LineInfoElementImpl.SetReadOnly(cateNode);
            // reference and check the required nodes
            List<XmlNode> patterns = FindNodes("pattern", cateNode);
            List<XmlNode> templates = FindNodes("template", cateNode);
            List<XmlNode> rulesNodes = FindNodes("rule", cateNode);
            List<XmlNode> topicNodesInCate = FindNodes("topic", cateNode);
            List<XmlNode> thatNodes = FindNodes("that", cateNode);
            // var thatNodes = new List<XmlNode>();
            //FindNodes("that", cateNode, thatNodes, 2);
            string errors = "";
            if (templates.Count == 0)
            {
                XmlNode TemplateOverwrite = TheTemplateOverwrite;
                if (TemplateOverwrite != null)
                {
                    templates = new List<XmlNode>();
                    templates.Add(TemplateOverwrite);
                }
                else
                {
                    errors += " TEMPLATE MISSING ";
                }
            }
            if (patterns.Count == 0)
            {
                errors += " PATTERN MISSING ";
            }
            if (!string.IsNullOrEmpty(errors))
            {
                AddErrorCategory(errors, cateNode);
                return CIs;
            }
            XmlNode that0 = null;

            if (thatNodes.Count > 0)
            {
                foreach (XmlNode that in new List<XmlNode>(thatNodes))
                {
                    string getAttribValue = GetAttribValue(that, "index", null);
                    if (getAttribValue == null || getAttribValue == "1" || getAttribValue == "1,1")
                    {
                        if (that0 != null) AddErrorCategory("more than one <that>", cateNode);
                        that0 = that;
                        thatNodes.Remove(that);
                    }
                }
            }
            foreach (XmlNode xmlNode in thatNodes)
            {
                additionalRules = additionalRules ?? PushAddtionalRuleContext(additionalRules);
                additionalRules.Add(new ConversationCondition(xmlNode));
            }
            foreach (XmlNode xmlNode in rulesNodes)
            {
                additionalRules = additionalRules ?? PushAddtionalRuleContext(additionalRules);
                additionalRules.Add(new ConversationCondition(xmlNode));
            }
            foreach (XmlNode pattern in patterns)
            {
                foreach (XmlNode template in templates)
                {
                    try
                    {
                        List<CategoryInfo> v = addCatNode(cateNode, pattern, that0, loadOpts, template, topicName,
                                                          outerNode,
                                                          additionalRules, 1.0);
                        if (v == null && !string.IsNullOrEmpty(errors))
                        {
                            AddErrorCategory(errors, cateNode);
                            writeToLog("WARN: MISSING CATE: " + cateNode + " " + LocationInfo(cateNode));
                            continue;
                        }
                        if (v != null) CIs.AddRange(v);
                    }
                    catch (ChatSignal e)
                    {
                        throw;
                    }
                    catch (Exception e2)
                    {
                        String s = "ERROR: processCategoryWithTopic '" + e2 + "' " + loadOpts;
                        writeToLog(s + " " + LocationInfo(cateNode));
                    }
                }
            }
            return CIs;
        }

        public Dictionary<XmlNode, StringBuilder> DumpErrors(OutputDelegate action, bool clr)
        {
            Dictionary<XmlNode, StringBuilder> ErrorList = StaticAIMLUtils.ErrorList;
            lock (ErrorList)
            {
                Dictionary<XmlNode, StringBuilder> el = ErrorList;
                int ct = el.Count;
                if (clr) StaticAIMLUtils.ErrorList = new Dictionary<XmlNode, StringBuilder>();
                if (action != null)
                {
                    foreach (var kv in el)
                    {
                        action("\n\nERRORS: " + kv.Value + "\n in" + kv.Key.OuterXml);
                    }
                    action("TOTAL ERRORS: " + ct);
                }
                return el;
            }
        }

        private void AddErrorCategory(string errors, XmlNode node)
        {
            if (string.IsNullOrEmpty(errors))
            {
                return;
                writeToLog("XMLERRORNODE: " + node + " " + LocationInfo(node));
            }
            writeToLog("XMLERROR: " + errors + " \n in " + node + " " + LocationInfo(node));
            Dictionary<XmlNode, StringBuilder> ErrorList = StaticAIMLUtils.ErrorList;
            lock (ErrorList)
            {
                StringBuilder stingBuilder;
                if (!ErrorList.TryGetValue(node, out stingBuilder))
                {
                    ErrorList[node] = new StringBuilder(errors);
                    return;
                }
                stingBuilder.AppendLine(errors);
            }
        }

        private List<CategoryInfo> addCatNode(XmlNode cateNode, XmlNode patternNode, XmlNode thatNodeOrNull,
                                              LoaderOptions loaderOpts,
                                              XmlNode templateNode,
                                              Unifiable topicName, XmlNode outerNode,
                                              List<ConversationCondition> additionalRules, double score)
        {
            additionalRules = additionalRules ?? PushAddtionalRuleContext(loaderOpts.AdditionalPreconditions);
            // additionalRules = new List<XmlNode>();
            //if (prev != null) additionalRules.AddRange(prev);

            XmlNode guardnode = FindNode("guard", cateNode, null);
            if (guardnode == null && outerNode != null && outerNode.Name != "aiml")
            {
                if (loaderOpts.SearchForGuard) guardnode = FindNode("guard", outerNode, null);
            }
            Unifiable guard = guardnode == null ? null : loaderOpts.Graph.GetGuardInfo(guardnode);
            string errors = "";
            XmlNode TemplateOverwrite = TheTemplateOverwrite;
            bool unusableCategory = false;
            if (ReferenceEquals(null, templateNode))
            {
                if (TemplateOverwrite != null)
                {
                    writeToLog("USING DELETION TEMPALTE " + cateNode);
                    templateNode = TemplateOverwrite;
                }
                else
                {
                    errors += " Missing pattern tag ";
                }
            }
            else
            {
                if (UnusableCategory(templateNode))
                {
                    unusableCategory = true;
                    score /= 10;
                }
            }
            if (ReferenceEquals(null, patternNode))
            {
                errors += " Missing pattern tag2 ";
            }

            XmlNode newPattern0;

            patternNode = GetPatternNode("that", cateNode, patternNode, ref thatNodeOrNull);

            string that = GeneratePatternNodeRules(additionalRules, thatNodeOrNull);
            if (that != "*")
            {
                score *= 1.5;
            }
            XmlNode flagXML = null;
            patternNode = GetPatternNode("flag", cateNode, patternNode, ref flagXML);
            Unifiable cond = VisibleChildsRenderingOrStar(flagXML);

            XmlNode topicTagText = null;
            patternNode = GetPatternNode("topic", cateNode, patternNode, ref topicTagText);

            string patternText = VisibleChildsRenderingOrStar(patternNode);
            if (!string.IsNullOrEmpty(errors))
            {
                AddErrorCategory(errors, cateNode);
                return null;
            }

            if (loaderOpts.DebugFiles && !ContansNoInfo(topicTagText.InnerXml))
            {
                string s = GetAttribValue(topicTagText, "name", Unifiable.STAR);
                if (topicName != s)
                {
                    errors = "TOPIC ERROR " + topicTagText.InnerXml + " topicName=" + topicName + " " + s;
                    AddErrorCategory(errors, cateNode);
                    return null;
                }
            }

            Func<Unifiable, bool, Unifiable> normalizerT =
                (inputText, isUserInput) => Trim(NormalizeU(inputText, isUserInput));
            Unifiable categoryPath =
                generateAddableCategoryPath(patternText, that, cond, topicName, loaderOpts.graphName, loaderOpts.stateNamePre,
                             loaderOpts.stateNamePost, loaderOpts.CurrentFilename,
                             false, normalizerT);//.ToUpper();
            Unifiable patternInfo = loaderOpts.Graph.FindPattern(patternNode, patternText);
            //PatternInfo.GetPattern(loaderOpts, patternNode, categoryPath);
            Unifiable topicInfo = loaderOpts.Graph.FindTopic(topicName);
            Unifiable thatInfo = loaderOpts.Graph.FindThat(thatNodeOrNull, that);
            FirstUse<XmlNode> templateNodeFindable = TheTemplateOverwrite;

            if (templateNode != null)
            {
                if (templateNode.HasChildNodes)
                {
                    if (templateNode.ChildNodes[0].NodeType != XmlNodeType.Comment || templateNode.ChildNodes.Count > 1)
                    {
                        templateNodeFindable = templateNode;
                    }
                }
            }
            templateNode = templateNodeFindable;

            // o.k., add the processed AIML to the GraphMaster structure
            if (!IsNullOrEmpty(categoryPath))
            {
                GraphMaster pathCtxGraph = loaderOpts.Graph;
                //lock (pathCtxGraph.LockerObject)
                {
                    try
                    {
                        /* ResponseInfo responseInfo = null;

                         if (tni != "")
                         {
                             responseInfo = loaderOpts.CtxGraph.FindResponse(templateNode, templateNode.InnerXml);
                         }*/
                        bool wouldBeRemoval;
                        if (unusableCategory) return null;

                        if (bot.useServitor)
                        {
                            AddCateAndTemplate(loaderOpts.CurrentFilename, templateNode, loaderOpts.Graph, categoryPath);
                            return null;
                        }
                        List<CategoryInfo> added = pathCtxGraph.addCategoryTag(categoryPath, patternInfo,
                                                                               cateNode, templateNode, guard, topicInfo,
                                                                               thatInfo,
                                                                               additionalRules,
                                                                               out wouldBeRemoval, loaderOpts);
                        if (added != null)
                        {
                            if (added.Count != 1)
                            {
                            }
                            foreach (CategoryInfo cate in added)
                            {
                                // half the rating
                                cate.Template.TemplateRating = score;
                            }
                        }
                        else
                        {
                        }
                        return added;
                    }
                    catch (ChatSignal e)
                    {
                        throw;
                    }
                    catch (Exception e)
                    {
                        AddErrorCategory(
                            "ERROR! " + e + " Failed to load a new category into the graphmaster where the path = " +
                            categoryPath + " and templateNode = " + templateNode.OuterXml +
                            " produced by a category in the file: " + loaderOpts, cateNode);
                        return null;
                    }
                }
            }
            else
            {
                AddErrorCategory(
                    "ERROR WARNING! Attempted to load a new category with an empty patternNode where the path = " +
                    categoryPath + " and templateNode = " + templateNode.OuterXml +
                    " produced by a category in the file: " + loaderOpts, cateNode);
                return null;
            }
        }

        private string VisibleChildsRenderingOrStar(XmlNode nodeI)
        {
            if (nodeI == null) return "*";
            return VisibleRendering(nodeI.ChildNodes, PatternSideRendering);
        }

        private bool UnusableCategory(XmlNode templateNode)
        {
            string tempStringS = templateNode.OuterXml.ToLower()
                .Replace("l>", " ")
                .Replace("<", " ").Replace(">", " ").Replace(".", " ").
                Replace("\"", " ").Replace("'", " ").Replace(",", " ");
            lock (FilteredWords)
            {
                foreach (string word in FilteredWords)
                {
                    if (tempStringS.Contains(" " + word + " "))
                    {
                        XmlNode skip = templateNode.ParentNode ?? templateNode;
                        //string skiping = "skipping: " + skip.OuterXml;
                        //writeDebugLine("DEBUG9: PROPRIETARY " + skiping);
                        return true;
                    }
                }
            }
            return false;
        }

        private XmlNode GetPatternNode(String tagName, XmlNode cateNode, XmlNode patternNode, ref XmlNode foundNode)
        {
            XmlNode newPattern = null;

            string patternText0;
            XmlNode foundNode0 = extractPrecondNode(patternNode, tagName, cateNode, out patternText0, out newPattern);
            if (foundNode0 != null) foundNode = foundNode0;
            if (newPattern != null)
            {
                string newPatternOuterXml = newPattern.OuterXml;
                patternNode = newPattern;
                if (newPatternOuterXml.Contains("<" + tagName))
                {
                    writeToLog("ERROR in extractThat: " + newPatternOuterXml + " " + LocationInfo(patternNode));
                }
                if (newPattern != patternNode)
                {
                    patternNode = newPattern;
                }
            }
            return patternNode;
        }

        private string GeneratePatternNodeRules(ICollection<ConversationCondition> additionalRules, XmlNode xmlNode)
        {
            if (xmlNode == null) return "*";
            bool isPatternSide;
            string indexValue;
            bool indexPosition1;
            string tagName = xmlNode.LocalName;
            string pattern = TryGetPrecondionThat(tagName, xmlNode, out isPatternSide, out indexValue,
                                                  out indexPosition1);
            if (isPatternSide)
            {
                if (!indexPosition1)
                {
                    // ReSharper disable ConditionIsAlwaysTrueOrFalse
                    var newConversationCondition
                        = new ConversationCondition(isPatternSide, tagName, pattern, indexValue, indexPosition1, xmlNode);
                    // ReSharper restore ConditionIsAlwaysTrueOrFalse
                    additionalRules.Add(newConversationCondition);
                }
            }

            if (isPatternSide || pattern != "*")
            {
                if (pattern == null || Trim(pattern).Length == 0)
                {
                    pattern = TryGetPrecondionThat(tagName, xmlNode, out isPatternSide, out indexValue,
                                                   out indexPosition1);
                }
                if (xmlNode != null && isPatternSide)
                {
                    if (!indexPosition1)
                    {
                        var newPatternMatchRule1 =
                            new ConversationCondition(isPatternSide, tagName, pattern, indexValue, indexPosition1,
                                                      xmlNode);
                        additionalRules.Add(newPatternMatchRule1);
                    }
                    if (!string.IsNullOrEmpty(pattern)) return pattern;
                    writeToLog("extract" + tagName + ": " + xmlNode.OuterXml + " " + LocationInfo(xmlNode));
                }
                else
                {
                    pattern = "*";
                }
            }
            if (!string.IsNullOrEmpty(pattern)) return pattern;
            return "*";
        }

        public static string TryGetPrecondionThat(string tagName, XmlNode extractedXML, out bool isRequired,
                                                  out string indexVal, out bool indexPosition1)
        {
            isRequired = extractedXML != null && extractedXML.LocalName == tagName;
            indexVal = GetAttribValue(extractedXML, "index", "1,1");
            indexPosition1 = indexVal == "1" || indexVal == "1,1" || indexVal == "1,*";
            if (!isRequired)
            {
                return null;
            }
            string patternTxt = VisibleRendering(extractedXML.ChildNodes, PatternSideRendering);
            if (patternTxt == "")
            {
                patternTxt = GetAttribValue(extractedXML, "value," + tagName + ",match,name", null);
            }
            isRequired = false;
            foreach (XmlNode childs in extractedXML.ChildNodes)
            {
                if (childs.NodeType != XmlNodeType.Comment)
                {
                    isRequired = true;
                    break;
                }
            }
            if (!isRequired)
            {
                if (patternTxt == null)
                {
                    //writeToLog("extractThat1: '" + patternTxt + "' from " + extractedXML.OuterXml);
                }
                else
                {
                    isRequired = true;
                }
            }
            return patternTxt;
        }


        public void writeToLog(string message, params object[] args)
        {
            string prefix = ToString();
            prefix = SafeFormat("LOADERTRACE: " + message + " while " + DLRConsole.NoFormatDirectives(prefix), args);

            try
            {
                bot.writeToLog(prefix);
            }
            catch (ChatSignal e)
            {
                throw;
            }
            catch
            {
            }
        }


        /// <summary>
        ///   Generates a path from a category XML cateNode and topic name
        /// </summary>
        /// <param name="cateNode"> the category XML node </param>
        /// <param name="topicName"> the topic </param>
        /// <param name="isUserInput"> marks the path to be created as originating from user input - so normalize out the * and _ wildcards used by AIML </param>
        /// <returns> The appropriately processed path </returns>
        private static XmlNode extractPrecondNode(XmlNode patternNode, String tagname, XmlNode cateNode,
                                                  out string patternText, out XmlNode newPattern)
        {
            // get the nodes that we need
            List<XmlNode> foundInPattern = FindNodes(tagname, patternNode);

            XmlNode foundHigherThanPattern = FindHigher(tagname, patternNode, null);
            XmlNode foundAnywhere = foundHigherThanPattern ?? FindNode(tagname, cateNode, null, 2);

            //Unifiable thatText = Unifiable.STAR;

            if (ReferenceEquals(null, patternNode))
            {
                patternNode = null;
                patternText = null;
                newPattern = patternNode;
                return foundAnywhere;
            }
            if (foundInPattern.Count == 0)
            {
                patternText = null; // patternNode.InnerXml;
                newPattern = null; // patternNode;
                return foundAnywhere;
            }
            {
                RenderOptions patternSideRendering = GetPatternSideRendering(tagname);
                string patternString =
                    VisibleRendering(patternNode.ChildNodes, patternSideRendering);
                int f = cateNode.OuterXml.IndexOf("<" + tagname);
                if (f == -1)
                {
                    patternText = InnerXmlText(patternNode);
                    newPattern = patternNode;
                    return null;
                }
                string thatString = null;
                if (foundAnywhere != null)
                {
                    thatString = foundAnywhere.OuterXml;
                }
                if (thatString != null)
                {
                    patternString = MatchKeyClean(patternString.Replace(thatString, ""));
                }
                XmlNode newLineInfoPattern = getNodeAndSetSibling(true,
                                                                  "<pattern>" + patternString +
                                                                  "</pattern>", false,
                                                                  false, patternNode);
                //TODO BEFORE COMMIT DMILES
                LineInfoElementImpl.SetParentFromNode(newLineInfoPattern, patternNode);
                LineInfoElementImpl.SetReadOnly(newLineInfoPattern);
                patternNode = newLineInfoPattern;
                patternText = Unifiable.Create(CommonStaticUtils.InnerXmlText(patternNode));
            }
            newPattern = patternNode;
            return foundAnywhere;
        }

        private static RenderOptions GetPatternSideRendering(string tagname)
        {
            lock (PatternSideRenderingCache)
            {
                RenderOptions patternSideRendering;
                if (!PatternSideRenderingCache.TryGetValue(tagname, out patternSideRendering))
                {
                    patternSideRendering = new RenderOptions(PatternSideRendering);
                    patternSideRendering.skip.Add(tagname);
                    PatternSideRenderingCache[tagname] = patternSideRendering;
                }
                return patternSideRendering;
            }
        }

        /// <summary>
        ///   Generates a path from the passed arguments
        /// </summary>
        /// <param name="pattern"> the pattern </param>
        /// <param name="that"> the that </param>
        /// <param name="topicName"> the topic </param>
        /// <param name="isUserInput"> marks the path to be created as originating from user input - so normalize out the * and _ wildcards used by AIML </param>
        /// <returns> The appropriately processed path </returns>
        public string generateAddableCategoryPath(Unifiable pattern, Unifiable that, Unifiable flag, Unifiable topicName,
            string graphName, string stateNamePre, string stateNamePost, string filename,
                                      bool isUserInput, Func<Unifiable, bool, Unifiable> innerFormater)
        {
            if (isUserInput)
            {
            }

            if (that.AsString().Contains("TAG-"))
            {
                throw new NullReferenceException("bad that: " + that);
            }
            if (pattern.AsString().Contains("TAG-"))
            {
                throw new NullReferenceException("bad pattern: " + pattern);
            }
            if (topicName.AsString().Contains("TAG-"))
            {
                throw new NullReferenceException("bad topicName: " + topicName);
            }

            Unifiable res =
                Unifiable.MakePath(generateCPath(graphName, pattern, that, flag, topicName, stateNamePre, stateNamePost,
                                                 isUserInput, null, bot));
            if (isUserInput)
            {
                var ress = (string) res;
                if (false && ress.Contains("*"))
                {
                    bot.RaiseError("Wrong generate path in code");
                    string problem = "generatePath failed: " + ress;
                    writeToLog("ERROR: " + problem);
                    throw new Exception(problem);
                }
            }
            return res;
        }

        private static string LastRepair(string normalizedPattern, bool isUserInput, bool UseRawUserInput)
        {
            if (!SeekOutAndRepair) return normalizedPattern;
            bool hasStars = normalizedPattern.Contains("*") || normalizedPattern.Contains("_");
            if (!hasStars) return normalizedPattern;
            bool hasBrackets = normalizedPattern.Contains("<") || normalizedPattern.Contains(">");
            if (hasBrackets)
            {
                normalizedPattern = normalizedPattern.Replace("> ", ">");
                normalizedPattern = normalizedPattern.Replace(" <", "<");
                normalizedPattern = normalizedPattern.Replace(" >", ">");
                normalizedPattern = normalizedPattern.Replace("< ", "<");
            }
            normalizedPattern = normalizedPattern.Replace("  ", " ");
            normalizedPattern = Trim(normalizedPattern);
            string normalizedPattern1 = normalizedPattern;
            normalizedPattern = normalizedPattern.Replace("**", " * * ");
            normalizedPattern = normalizedPattern.Replace("*_", " * _ ");
            normalizedPattern = normalizedPattern.Replace("_*", " _ * ");
            if (!UseRawUserInput || hasBrackets)
            {
                normalizedPattern = normalizedPattern.Replace("*", " * ");
                normalizedPattern = normalizedPattern.Replace("_", " _ ");
                {
                    normalizedPattern = normalizedPattern.Replace("> ", ">");
                    normalizedPattern = normalizedPattern.Replace(" <", "<");
                }
            }
            normalizedPattern = normalizedPattern.Replace("  ", " ");
            normalizedPattern = Trim(normalizedPattern);
            if (isUserInput)
            {
                if (normalizedPattern.Contains("*") || normalizedPattern.Contains("_"))
                {
                    return normalizedPattern;
                }
            }
            if (normalizedPattern != normalizedPattern1)
            {
                if (normalizedPattern1.Contains("<"))
                {
                    return normalizedPattern1;
                }
                normalizedPattern = normalizedPattern.Replace("  ", " ");
                writeDebugLine("WARN SHOULD REPAIR? '{0}' -> '{1}' ", normalizedPattern1,
                               normalizedPattern);
                return normalizedPattern1;
            }
            return normalizedPattern;
        }

        /// <summary>
        ///   Generates a path from the passed arguments
        /// </summary>
        /// <param name="pattern"> the pattern </param>
        /// <param name="that"> the that </param>
        /// <param name="topicName"> the topic </param>
        /// <param name="isUserInput"> marks the path to be created as originating from user input - so normalize out the * and _ wildcards used by AIML </param>
        /// <returns> The appropriately processed path </returns>
        private Unifiable generateCPath_OLD(Unifiable pattern, Unifiable that, Unifiable flag, Unifiable topicName,
                                        bool isUserInput, Func<Unifiable, bool, Unifiable> innerFormater)
        {
            // to hold the normalized path to be entered into the graphmaster
            Unifiable normalizedPath = Unifiable.CreateAppendable();
            string normalizedPattern; // = Unifiable.Empty;
            Unifiable normalizedThat; // = Unifiable.STAR;
            Unifiable normalizedTopic; // = Unifiable.STAR;
            bool UseRawUserInput = RawUserInput;
            string patString = " " + pattern.AsString() + " ";
            if (patString.Contains(" exec ") || patString.Contains(" aiml ")
                || patString.Contains(" lisp ") || patString.Contains(" tag ")
                || patString.Contains("<") || patString.Contains(">") || patString.Contains("\\") ||
                patString.Contains("/")
                || patString.Contains("\"") || patString.Contains("=") || patString.Contains("#$")
                || patString.Contains("~") || patString.Contains("*"))
            {
                UseRawUserInput = true;
                isUserInput = false;
            }

            bool thatContainedAnd = that.ToUpper().Contains(" AND ");
            if ((bot.TrustAIML) & (!isUserInput || UseRawUserInput))
            {
                normalizedPattern = Trim(pattern);
                // clip only one off
                if (isUserInput) normalizedPattern = CleanPunct(normalizedPattern);
                if (false)
                {
                    normalizedPattern = MatchKeyClean(normalizedPattern);
                    normalizedThat = MatchKeyClean(that);
                    normalizedTopic = MatchKeyClean(topicName);
                }
                normalizedThat = CleanPunct(that.Trim());
                normalizedTopic = topicName.Trim();
            }
            else
            {
                normalizedPattern = innerFormater(pattern, isUserInput);
                normalizedThat = innerFormater(that, isUserInput);
                normalizedTopic = innerFormater(topicName, isUserInput);
            }


            // check sizes
            if (normalizedPattern.Length > 0)
            {
                if (IsNullOrEmpty(normalizedThat))
                {
                    normalizedThat = Unifiable.STAR;
                }
                else
                {
                    if (!normalizedThat.IsWildCard)
                    {
                        // normalizedThat = "* " + normalizedThat;
                    }
                }
                if (IsNullOrEmpty(normalizedTopic))
                {
                    normalizedTopic = Unifiable.STAR;
                }

                if (ThatWideStar)
                {
                    if (!isUserInput)
                        normalizedThat = "* " + normalizedThat;
                    else
                        normalizedThat = "* " + normalizedThat;
                    //normalizedThat = "<br/> " + normalizedThat;
                }
                // This check is in place to avoid huge "that" elements having to be processed by the 
                // graphmaster. 
                //if (normalizedThat.Length > this.RProcessor.MaxThatSize)
                //{
                //    normalizedThat = Unifiable.STAR;
                //}
                Unifiable addTagEnd = isUserInput ? Unifiable.TagEndText : null;
                Unifiable addTagStart = isUserInput ? Unifiable.TagStartText : null;

                //useInexactMatching = true;

                if (useInexactMatching)
                {
                    Func<string, string> PadStarsOrNoWilds = isUserInput
                                                                 ? (Func<string, string>) NoWilds
                                                                 : PadStars;
                    normalizedPattern = PadStarsOrNoWilds(normalizedPattern);
                    normalizedThat = PadStarsOrNoWilds(normalizedThat);
                    normalizedTopic = PadStarsOrNoWilds(normalizedTopic);
                }
                else
                {
                    addTagEnd = addTagStart = null;
                }

                if (thatContainedAnd)
                {
                    if (!normalizedThat.ToUpper().Contains(" AND "))
                    {
                        writeToLog("ERROR in that: " + that + " -> " + normalizedThat);
                        normalizedThat = CleanPunct(that);
                    }
                }

                normalizedPattern = LastRepair(normalizedPattern, isUserInput, UseRawUserInput);
                normalizedThat = LastRepair(normalizedThat, isUserInput, UseRawUserInput);
                normalizedTopic = LastRepair(normalizedTopic, isUserInput, UseRawUserInput);

                // o.k. build the path
                normalizedPath.Append(Unifiable.TagInput);
                if (addTagStart != null) normalizedPath.Append(addTagStart);
                normalizedPath.Append(Unifiable.Create(normalizedPattern));
                if (addTagEnd != null) normalizedPath.Append(addTagEnd);
                if (bot.UseInlineThat)
                {
                    normalizedPath.Append(Unifiable.TagThat);
                    if (addTagStart != null) normalizedPath.Append(addTagStart);
                    normalizedPath.Append(normalizedThat);
                    if (addTagEnd != null) normalizedPath.Append(addTagEnd);
                }
                normalizedPath.Append(Unifiable.TagTopic);
                if (addTagStart != null) normalizedPath.Append(addTagStart);
                normalizedPath.Append(normalizedTopic);
                if (addTagEnd != null) normalizedPath.Append(addTagEnd);

                normalizedPath.Append(Unifiable.TagFlag);
                normalizedPath.Append(flag);

                return normalizedPath; //.Frozen();
            }
            else
            {
                return Unifiable.Empty;
            }
        }

        /// <summary>
        ///   Given an input, provide a normalized output
        /// </summary>
        /// <param name="input"> The Unifiable to be normalized </param>
        /// <param name="isUserInput"> True if the Unifiable being normalized is part of the user input path - flags that we need to normalize out * and _ chars </param>
        /// <returns> The normalized Unifiable </returns>
        public Unifiable NormalizeU(string input, bool isUserInput)
        {
            string input0 = input;
            if (CommonStaticUtils.IsNullOrEmpty(input)) return Unifiable.Empty;
            if (isUserInput) input = CleanWhitepaces(input);
            if (CommonStaticUtils.IsNullOrEmpty(input)) return Unifiable.Empty;
            input = input.TrimEnd("?!. \n\r\t".ToCharArray());
            if (CommonStaticUtils.IsNullOrEmpty(input))
            {
                //return input0;
                return Unifiable.Empty;
            }
            Unifiable result = Unifiable.CreateAppendable();
            input = input.Replace("*", " * ").Replace("  ", " ");
            input = Trim(input);
            // objects for normalization of the input
            var substitutor = new ApplySubstitutions(bot);
            var stripper = new StripIllegalCharacters(bot);

            Unifiable substitutedInput = Trim(substitutor.TransformU(" " + input + " "));
            bool cand = ToUpper(input).Contains(" AND ");
            bool cand2 = substitutedInput.ToUpper().Contains(" AND ");
            int nonAlpha = NonAlphaCount(input);
            int nonAlpha2 = NonAlphaCount(substitutedInput);
            if (cand != cand2 || nonAlpha != nonAlpha2)
            {
                substitutedInput = input;
            }
            else
            {
                substitutedInput = input;
            }

            // split the pattern into it's component words
            string[] substitutedWords = substitutedInput.AsString().Split(' ');

            // Normalize all words unless they're the AIML wildcards "*" and "_" during AIML loading
            foreach (Unifiable word in substitutedWords)
            {
                Unifiable normalizedWord;
                if (isUserInput)
                {
                    string wwword = word.AsString().Trim(",. \"?!".ToCharArray());
                    if (wwword.Length == 0) continue;
                    normalizedWord = stripper.TransformU(word);
                    if (normalizedWord != wwword)
                    {
                        normalizedWord = stripper.TransformU(word);
                        //  if (!wwword.Contains("'"))
                        //    writeToLog("Normalize stripper " + word + "->" + normalizedWord);
                    }
                }
                else
                {
                    if (word.IsWildCard)
                    {
                        normalizedWord = word;
                    }
                    else
                    {
                        normalizedWord = stripper.TransformU(word);
                    }
                }
                result.Append(normalizedWord);
            }

            return Unifiable.ToVMString(result).Replace("  ", " "); // make sure the whitespace is neat
        }
    }
}