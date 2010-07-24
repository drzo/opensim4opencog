using System;
using System.Collections;
using System.Collections.Generic;
using System.Net;
using System.Xml;
using System.IO;
using System.Text;
using MushDLR223.ScriptEngines;
using RTParser.AIMLTagHandlers;
using RTParser.Variables;
using UPath = RTParser.Unifiable;
using MushDLR223.Virtualization;

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
        private RTParser.RTPBot RProcessorOld
        {
            get
            {
                return LoaderRequest00.Proccessor;
            }
        }

        public Request LoaderRequest00;
        /// <summary>
        /// Allow all chars in RawUserInput
        /// </summary>
        public bool RawUserInput = false;
        #endregion

        /// <summary>
        /// Ctor
        /// </summary>
        /// <param name="bot">The bot whose brain is being processed</param>
        public AIMLLoader(RTParser.RTPBot bot, Request request)
        {
            this.LoaderRequest00 = request;
        }

        #region Methods

        /// <summary>
        /// Loads the AIML from files found in the RProcessor's AIMLpath into the RProcessor's brain
        /// </summary>
        //public void loadAIML(string path)
        //{
        //    LoaderOptions loadOpts = request.loader;
        //    request.Loader.loadAIMLFromURI(path, loadOpts);
        //}

        /// <summary>
        /// Loads the AIML from files found in the path
        /// </summary>
        /// <param name="path"></param>
        public void loadAIMLDir0(string path, LoaderOptions loadOpts)
        {
            RTPBot RProcessor = loadOpts.RProcessor;
            path = ResolveToURI(path, loadOpts);
            RProcessor.ReloadHooks.Add(() => loadAIMLDir0(path, loadOpts));

            Request request = loadOpts.TheRequest;
            loadOpts = EnsureOptions(loadOpts, request, path);

            writeToLog("Starting to process AIML files found in the directory " + path);

            string[] fileEntries = HostSystem.GetFiles(path, "*.aiml");
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

                        var savedOpt = request.LoadOptions;
                        try
                        {
                            request.LoadOptions = loadOpts;
                            request.Filename = f;
                            loadOpts = request.LoadOptions;
                            loadAIMLFile0(f, loadOpts, false);
                        }
                        finally
                        {
                            request.LoadOptions = savedOpt;
                        }
                    }
                    catch (Exception ee)
                    {
                        loadOpts.CtxGraph.RemoveFileLoaded(path);
                        writeToLog("Error in loadAIMLFile " + ee);
                    }
                }
                writeToLog("Finished processing the AIML files. " + Convert.ToString(loadOpts.CtxGraph.Size) +
                           " categories processed.");
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
                    loadAIMLDir0(path + Path.DirectorySeparatorChar + f, loadOpts);
                }
            }
        }

        static string ResolveToURI(string pathIn, LoaderOptions loadOpts)
        {
            string baseFile = GetBaseDirectory(loadOpts);
            var combine = baseFile != null ? new[] { ".", baseFile, "aiml" } : new[] { ".", "aiml" };
            string path = HostSystem.ResolveToURI(pathIn, combine);
            path = HostSystem.ToRelativePath(path);
            return path;
        }

        static string GetBaseDirectory(LoaderOptions loadOpts)
        {
            string baseFile = loadOpts.CurrentFilename ?? loadOpts.CurrentlyLoadingFrom ?? LoaderOptions.MISSING_FILE;
            if (baseFile == null) return ".";
            return HostSystem.GetBaseDir(baseFile);
        }

        public void loadAIMLURI(string path, LoaderOptions loadOpts)
        {

            RTPBot RProcessor = loadOpts.RProcessor;
            RProcessor.ReloadHooks.Add(() => loadAIMLURI(path, loadOpts));
            path = ResolveToURI(path, loadOpts);


            try
            {
                if (HostSystem.DirExists(path))
                {
                    Request request = loadOpts.TheRequest;
                    var savedOpt = request.LoadOptions;
                    try
                    {
                        request.LoadOptions = loadOpts;
                        request.Filename = path;
                        loadOpts = request.LoadOptions;
                        loadAIMLDir0(path, loadOpts);
                    }
                    finally
                    {
                        request.LoadOptions = savedOpt;
                    }
                    return;
                }
                else if (HostSystem.FileExists(path))
                {
                    Request request = loadOpts.TheRequest;
                    var savedOpt = request.LoadOptions;
                    try
                    {
                        request.LoadOptions = loadOpts;
                        loadAIMLFile0(path, loadOpts, true);
                    }
                    finally
                    {
                        request.LoadOptions = savedOpt;
                    }
                    return;
                }
                else if (Uri.IsWellFormedUriString(path, UriKind.RelativeOrAbsolute))
                {
                    writeToLog("Processing AIML URI: " + path);
                    var uri = new Uri(path);
                    if (uri.IsFile)
                    {
                        loadAIMLURI(uri.AbsolutePath, loadOpts);
                        return;
                    }
                    WebRequest req = WebRequest.Create(uri);
                    WebResponse resp = req.GetResponse();
                    Stream stream = resp.GetResponseStream();
                    Request request = loadOpts.TheRequest;
                    var savedOpt = request.LoadOptions;
                    try
                    {
                        loadOpts.Loading0 = uri.ToString();
                        loadAIMLStream(stream, loadOpts);
                        writeToLog("Completed AIML URI: " + path);
                        return;
                    }
                    finally
                    {
                        request.LoadOptions = savedOpt;
                    }
                }
            }
            catch (Exception e)
            {
                RProcessor.writeToLog(e);
                writeToLog("ERROR! " + e);
                throw e;
            }
            String nf = "ERROR: XmlTextReader of AIML files (" + path + ")";
            var nfe = new FileNotFoundException(nf);
            RProcessor.writeToLog(nfe);
            writeToLog(nf);
            throw nfe;
        }

        /// <summary>
        /// Given the name of a file in the AIML path directory, attempts to load it into the 
        /// graphmaster
        /// </summary>
        /// <param name="path">The name of the file to process</param>
        public void loadAIMLFile0(string path, LoaderOptions loadOpts, bool forceReload)
        {
            path = ResolveToURI(path, loadOpts);
            RTPBot RProcessor = loadOpts.RProcessor;
            //RProcessor.ReloadHooks.Add(() => loadAIMLFile0(path, loadOpts, forceReload));
            Request request = loadOpts.TheRequest;
            loadOpts = EnsureOptions(loadOpts, request, path);


            if (!HostSystem.FileExists(path))
            {
                writeToLog("Processing url: " + path);
                if (loadOpts.recurse) loadAIMLURI(path, loadOpts);
                return;
            }
            try
            {
                // load the document
                if (loadOpts.CtxGraph.IsFileLoaded(path))
                {
                    if (!forceReload) return;
                    writeToLog("Already loaded! (but loading again) " + path + " from " + loadOpts.CtxGraph);
                    //return;
                }
                else
                    writeToLog("Processing AIML file: " + path + " from " + loadOpts.CtxGraph);
                loadOpts.CtxGraph.AddFileLoaded(path);
                var tr = HostSystem.OpenRead(path);
                try
                {
                    string pfile = request.Filename;
                    try
                    {
                        request.Filename = path;
                        loadOpts.Loading0 = path;
                        this.loadAIMLStream(tr, loadOpts);
                    }
                    finally
                    {
                        request.Filename = pfile;
                    }
                }
                finally
                {
                    try
                    {
                        tr.Close();
                    }
                    catch (Exception)
                    {
                    }
                }

                writeToLog("Loaded AIMLFile: '{0}'", path + " from " + loadOpts.CtxGraph);
                return;
            }
            catch (Exception e)
            {
                loadOpts.CtxGraph.RemoveFileLoaded(path);
                writeToLog("Error in AIML Stacktrace: " + path + "\n  " + e.Message + "\n" + e.StackTrace);
                writeToLog("Error in AIML file: " + path + " Message " + e.Message);
            }
        }
        /// <summary>
        /// Given an XML document containing valid AIML, attempts to load it into the graphmaster
        /// </summary>
        /// <param name="doc">The XML document containing the AIML</param>
        /// <param name="loadOpts">Where the XML document originated</param>
        public void loadAIMLString(string docString, LoaderOptions loadOpts)
        {
            RTPBot RProcessor = loadOpts.RProcessor;
            Request request = loadOpts.TheRequest;
            //            RProcessor0.ReloadHooks.Add(() => loadAIMLFile0(path, loadOpts));
            string path = request.Filename;
            loadOpts = EnsureOptions(loadOpts, request, path);
            path = ResolveToURI(path, loadOpts);


            try
            {
                byte[] byteArray = Encoding.ASCII.GetBytes(docString);
                MemoryStream stream = new MemoryStream(byteArray);
                loadAIMLStream(stream, loadOpts);
            }
            catch (Exception e2)
            {
                String s = "which causes loadAIMLString '" + docString + "' " + loadOpts;
                s = s + "\n" + e2.Message + "\n" + e2.StackTrace + "\n" + s;
                writeToLog(s);
                throw e2;
            }

            return;
        }

        /// <summary>
        /// Given an XML document containing valid AIML, attempts to load it into the graphmaster
        /// </summary>
        /// <param name="doc">The XML document containing the AIML</param>
        /// <param name="loadOpts">Where the XML document originated</param>
        public void loadAIMLStream(Stream input, LoaderOptions loadOpts)
        {
            RTPBot RProcessor = loadOpts.RProcessor;
            Request request = loadOpts.TheRequest;
            string path = request.Filename;
            loadOpts = EnsureOptions(loadOpts, request, path);
            path = ResolveToURI(path, loadOpts);


            var xtr = XmlDocumentLineInfo.CreateXmlTextReader(input);
            string namefile = "" + path;
            while (!xtr.EOF)
            {
                try
                {
                    XmlDocumentLineInfo doc = new XmlDocumentLineInfo(input, namefile);
                    doc.Load(xtr);
                    if (doc.DocumentElement == null)
                    {
                        RProcessor.writeToLog("ERROR: No Document at " + namefile);
                        continue;
                    }
                    HostSystem.Close(input);
                    this.loadAIMLNode(doc.DocumentElement, loadOpts);
                }
                catch (Exception e2)
                {
                    String s = "which causes loadAIMLStream '" + input + "' " + loadOpts + " charpos=" + input.Position;
                    s = s + "\n" + e2.Message + "\n" + e2.StackTrace + "\n" + s;
                    writeToLog(s);
                    //System.Console.Flush();
                    if (!xtr.Read())
                    {
                        throw e2;
                    }
                    // 
                }

            }
            return;
        }

        private LoaderOptions EnsureOptions(LoaderOptions loadOpts, Request request1, string path)
        {
            Request request = LoaderRequest00;
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
            if (!fn.Contains(path) && !path.Contains(fn))
            {
                writeToLog("WARNING! Ensuring loadOpts.Filename='{0}' but path='{1}'", fn, path);
            }
            if (!request1.LoadOptions.Equals(loadOpts))
            {
               /// writeToLog("ERROR! Ensuring loadOpts.Filename='{0}' but path='{1}'", fn, path);
            }
            return loadOpts;
        }

        public void loadAIMLNode(XmlNode currentNode, LoaderOptions loadOpts)
        {
            RTPBot RProcessor = loadOpts.RProcessor;
            Request request = loadOpts.TheRequest;
            string path = request.Filename;
            loadOpts = EnsureOptions(loadOpts, request, path);
            path = ResolveToURI(path, loadOpts);

            var prev = RProcessor.Loader;

            try
            {
                RProcessor.Loader = this;
                if (currentNode.NodeType == XmlNodeType.Comment) return;
                // Get a list of the nodes that are children of the <aiml> tag
                // these nodes should only be either <topic> or <category>
                // the <topic> nodes will contain more <category> nodes
                string currentNodeName = currentNode.Name.ToLower();
                if (currentNodeName == "aiml")
                {
                    IntoGraph(request, currentNode, loadOpts);
                    return;
                }
                if (currentNodeName == "root")
                {
                    var prevDict = request.Settings;

                    // process each of these child "settings"? nodes
                    try
                    {
                        IntoGraph(request, currentNode, loadOpts);
                    }
                    finally
                    {
                        request.Settings = prevDict;
                    }
                }
                if (currentNodeName == "item")
                {
                    SettingsDictionary.loadSettingNode(request.Settings, currentNode, true, false);
                }

                if (currentNodeName == "topic")
                {
                    this.processTopic(currentNode, currentNode.ParentNode, loadOpts);
                }
                else if (currentNodeName == "category")
                {
                    this.processCategory(currentNode, currentNode.ParentNode, loadOpts);
                }
                else if (currentNodeName == "meta")
                {
                    writeToLog("UNUSED: " + currentNode.OuterXml);
                }
                else if (currentNodeName == "srai")
                {
                    EvalNode(currentNode, request, loadOpts);
                }
                else
                {
                    writeToLog("ImmediateAiml:: " + currentNode.OuterXml);
                    EvalNode(currentNode, request, loadOpts);
                }
            }
            finally
            {
                RProcessor.Loader = prev;
            }
        }

        private void IntoGraph(Request request, XmlNode currentNode, LoaderOptions loadOpts)
        {

            RTPBot RProcessor = loadOpts.RProcessor;
            string path = request.Filename;
            loadOpts = EnsureOptions(loadOpts, request, path);
            path = ResolveToURI(path, loadOpts);


            string graphname = RTPBot.GetAttribValue(currentNode, "graph", null);
            GraphMaster outerGraph = request.Graph;
            GraphMaster innerGraph = outerGraph;
            try
            {
                if (graphname != null)
                {
                    innerGraph = RProcessor.GetGraph(graphname, outerGraph);
                    if (innerGraph != null)
                    {
                        if (innerGraph != outerGraph)
                        {
                            request.Graph = innerGraph;
                            loadOpts.CtxGraph = innerGraph;
                            writeToLog("ENTERING: " + graphname + " as " + innerGraph + " from " + outerGraph);
                        }
                    }
                    else
                    {
                        innerGraph = outerGraph;
                    }
                }
                bool prev = innerGraph.IsBusy;
                innerGraph.IsBusy = true;
                // process each of these child nodes
                foreach (XmlNode child in currentNode.ChildNodes)
                {
                    loadAIMLNode(child, loadOpts);
                }
                innerGraph.IsBusy = prev;
                return;
            }
            finally
            {
                if (innerGraph != outerGraph)
                {
                    writeToLog("LEAVING: " + graphname + " as " + innerGraph + " back to " + outerGraph);
                    request.Graph = outerGraph;
                    loadOpts.CtxGraph = outerGraph;
                }
            }
            return;
        }

        private void EvalNode(XmlNode currentNode, Request request, LoaderOptions loadOpts)
        {
            RTPBot RProcessor = loadOpts.RProcessor;
            string path = request.Filename;
            loadOpts = EnsureOptions(loadOpts, request, path);
            path = ResolveToURI(path, loadOpts);
            try
            {
                RProcessor.ImmediateAiml(currentNode, request, this, null);
            }
            catch (Exception e)
            {
                RProcessor.writeToLog(e);
                writeToLog("ImmediateAiml: ERROR: " + e);
                XmlNode element = currentNode;
                string s = TextAndSourceInfo(element) + " " + LocationEscapedInfo(element);
                writeToLog("ERROR PARSING: " + s);
            }
        }

        /// <summary>
        /// Given a "topic" topicNode, processes all the categories for the topic and adds them to the 
        /// graphmaster "brain"
        /// </summary>
        /// <param name="topicNode">the "topic" node</param>
        /// <param name="path">the file from which this topicNode is taken</param>
        public void processTopic(XmlNode topicNode, XmlNode outerNode, LoaderOptions path)
        {
            // find the name of the topic or set to default "*"
            Unifiable topicName = RTPBot.GetAttribValue(topicNode, "name", Unifiable.STAR);
            // process all the category nodes
            foreach (XmlNode cateNode in topicNode.ChildNodes)
            {
                if (cateNode.Name == "category")
                {
                    processCategoryWithTopic(cateNode, topicName, topicNode, path);
                }
            }
        }

        /// <summary>
        /// Adds a category to the graphmaster structure using the default topic ("*")
        /// </summary>
        /// <param name="cateNode">the XML node containing the category</param>
        /// <param name="path">the file from which this category was taken</param>
        public void processCategory(XmlNode cateNode, XmlNode outerNode, LoaderOptions path)
        {
            this.processCategoryWithTopic(cateNode, Unifiable.STAR, outerNode, path);
        }

        /// <summary>
        /// Adds a category to the graphmaster structure using the given topic
        /// </summary>
        /// <param name="cateNode">the XML node containing the category</param>
        /// <param name="topicName">the topic to be used</param>
        /// <param name="path">the file from which this category was taken</param>
        private void processCategoryWithTopic(XmlNode cateNode, Unifiable topicName, XmlNode outerNode, LoaderOptions path)
        {
            // reference and check the required nodes
            List<XmlNode> patterns = FindNodes("pattern", cateNode);
            List<XmlNode> templates = FindNodes("template", cateNode);
            foreach (XmlNode pattern in patterns)
            {
                foreach (var template in templates)
                {
                    if (Equals(null, pattern))
                    {
                        throw new XmlException("Missing pattern tag in a cateNode found in " + path);
                    }
                    if (Equals(null, template))
                    {
                        throw new XmlException("Missing template tag in the cateNode with pattern: " + pattern.InnerText + " found in " + path);
                    }
                    addCatNode(cateNode, pattern, path, template, topicName, outerNode);
                }
            }
        }

        private void addCatNode(XmlNode cateNode, XmlNode patternNode, LoaderOptions path, XmlNode templateNode,
            Unifiable topicName, XmlNode outerNode)
        {
            XmlNode guardnode = FindNode("guard", cateNode, FindNode("guard", outerNode, null));
            GuardInfo guard = guardnode == null ? null : GuardInfo.GetGuardInfo(guardnode);


            XmlNode newPattern;
            Unifiable patternText;
            Unifiable that = extractThat(patternNode, "that", cateNode, out patternText, out newPattern).InnerXml;
            patternNode = newPattern;
            Unifiable cond = extractThat(patternNode, "flag", cateNode, out patternText, out newPattern).InnerXml;
            patternNode = newPattern;
            XmlNode topicTagText = extractThat(patternNode, "topic", cateNode, out patternText, out newPattern);
            patternNode = newPattern;
            if (path.DebugFiles && !ContansNoInfo(topicTagText.InnerXml))
            {
                var s = RTPBot.GetAttribValue(topicTagText, "name", Unifiable.STAR);
                if (topicName != s)
                {
                    throw new InvalidOperationException(cateNode.OuterXml);
                }
            }
            Unifiable categoryPath = generateCPath(patternText, that, cond, topicName, false);
            PatternInfo patternInfo = PatternInfo.GetPattern(path, patternNode, categoryPath);
            TopicInfo topicInfo = TopicInfo.FindTopic(path, topicName);
            ThatInfo thatInfo = ThatInfo.GetPattern(path, that);

            // o.k., add the processed AIML to the GraphMaster structure
            if (!categoryPath.IsEmpty)
            {
                try
                {
                    CategoryInfo categoryInfo = CategoryInfo.GetCategoryInfo(patternInfo, cateNode, path);
                    path.CtxGraph.addCategoryTag(categoryPath, patternInfo, categoryInfo,
                                                  outerNode, templateNode, guard, thatInfo);
                }
                catch (Exception e)
                {
                    string s = "ERROR! Failed to load a new category into the graphmaster where the path = " +
                               categoryPath + " and templateNode = " + templateNode.OuterXml +
                               " produced by a category in the file: " + path + "\n";
                    writeToLog(s + e + "\n" + s);
                }
            }
            else
            {
                writeToLog("WARNING! Attempted to load a new category with an empty patternNode where the path = " + categoryPath + " and templateNode = " + templateNode.OuterXml + " produced by a category in the file: " + path);
            }
        }

        public override string ToString()
        {
            return "[AIMLLoader req='" + LoaderRequest00 + "' bot='" + LoaderRequest00.Proccessor.BotID + "']";
            return base.ToString();
        }

        public void writeToLog(string message, params object[] args)
        {
            string prefix = ToString();
            LoaderRequest00.Proccessor.writeToLog("LOADERTRACE: " + message + " while " + prefix, args);
            try
            {
                message = message.ToUpper();
                Console.Out.Flush();
                if (message.Contains("ERROR") || message.Contains("WARN"))
                {
                    Console.Error.Flush();

                }
            }
            catch { }
        }

        private static bool ContansNoInfo(Unifiable cond)
        {
            return cond == null || cond == Unifiable.STAR || cond == Unifiable.Empty;
        }

        /// <summary>
        /// Generates a path from a category XML cateNode and topic name
        /// </summary>
        /// <param name="cateNode">the category XML node</param>
        /// <param name="topicName">the topic</param>
        /// <param name="isUserInput">marks the path to be created as originating from user input - so
        /// normalize out the * and _ wildcards used by AIML</param>
        /// <returns>The appropriately processed path</returns>
        static XmlNode extractThat(XmlNode patternNode, String tagname, XmlNode cateNode, out Unifiable patternText, out XmlNode newPattern)
        {
            // get the nodes that we need
            XmlNode that = FindNodeOrHigher(tagname, cateNode, null);

            //Unifiable thatText = Unifiable.STAR;

            if (Equals(null, patternNode))
            {
                patternText = Unifiable.Empty;
            }
            else
            {
                patternText = Unifiable.Create(Unifiable.InnerXmlText(patternNode));
            }

            string patternString = patternNode.InnerXml;
            int f = patternString.IndexOf("<" + tagname);
            if (f >= 0)
            {
                that = FindNode(tagname, patternNode, null);
                if (that == null)
                {
                    throw new NotImplementedException("generatePathExtractWhat: " + patternString);
                }
                string thatString = that.OuterXml;
                if (!patternString.Contains(thatString))
                {
                    throw new NotImplementedException("generatePathExtractWhat: " + patternString);
                }
                patternString = MatchKeyClean(patternString.Replace(thatString, ""));
                var newLineInfoPattern = AIMLTagHandler.getNode("<pattern>" + patternString + "</pattern>", patternNode);
                newLineInfoPattern.SetParentFromNode((LineInfoElement)patternNode);
                patternNode = newLineInfoPattern;
                patternText = Unifiable.Create(Unifiable.InnerXmlText(patternNode));
            }

            newPattern = patternNode;
            if (!Equals(null, that))
            {
                //thatText = that.InnerXml;
            }
            return that ?? PatternStar;// hatText;//this.generatePath(patternText, thatText, topicName, isUserInput);
        }

        protected static XmlNode PatternStar = AIMLTagHandler.getNode("<pattern name=\"*\">*</pattern>");
        public bool ThatWideStar = false;

        /// <summary>
        /// Given a name will try to find a node named "name" in the childnodes or return null
        /// </summary>
        /// <param name="name">The name of the node</param>
        /// <param name="node">The node whose children need searching</param>
        /// <returns>The node (or null)</returns>
        static public XmlNode FindNode(string name, XmlNode node, XmlNode ifMissing)
        {
            foreach (XmlNode child in node.ChildNodes)
            {
                if (child.Name == name)
                {
                    return child;
                }
            }
            return ifMissing;
        }
        static public XmlNode FindNodeOrHigher(string name, XmlNode node, XmlNode ifMissing)
        {
            if (node == null) return ifMissing;
            foreach (XmlNode child in node.ChildNodes)
            {
                if (child.Name == name)
                {
                    return child;
                }
            }
            return FindHigher(name, node.ParentNode, ifMissing);
        }
        static public XmlNode FindHigher(string name, XmlNode node, XmlNode ifMissing)
        {
            if (node == null) return ifMissing;
            if (node.Name == name)
            {
                return node;
            }
            return FindHigher(name, node.ParentNode, ifMissing);
        }
        static public List<XmlNode> FindNodes(string name, XmlNode node)
        {
            name = name.ToLower();
            List<XmlNode> nodes = new List<XmlNode>();
            foreach (XmlNode child in node.ChildNodes)
            {
                if (child.Name.ToLower() == name)
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
        public Unifiable generatePath(Unifiable pattern, Unifiable that, Unifiable flag, Unifiable topicName, bool isUserInput)
        {
            return Unifiable.MakePath(generateCPath(pattern, that, flag, topicName, isUserInput));
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
        public Unifiable generateCPath(Unifiable pattern, Unifiable that, Unifiable flag, Unifiable topicName, bool isUserInput)
        {
            var RProcessor = LoaderRequest00.Proccessor;

            // to hold the normalized path to be entered into the graphmaster
            Unifiable normalizedPath = Unifiable.CreateAppendable();
            string normalizedPattern;// = Unifiable.Empty;
            Unifiable normalizedThat;// = Unifiable.STAR;
            Unifiable normalizedTopic;// = Unifiable.STAR;
            bool UseRawUserInput = RawUserInput;
            string patString = " " + pattern.AsString() + " ";
            if (patString.Contains(" exec ") || patString.Contains(" aiml ")
                || patString.Contains(" lisp ") || patString.Contains(" tag ")
                || patString.Contains("<") || patString.Contains(">")
                || patString.Contains("\"") || patString.Contains("=") || patString.Contains("#$")
                || patString.Contains("~") || patString.Contains("*"))
            {
                UseRawUserInput = true;
            }
            if ((RProcessor.TrustAIML) & (!isUserInput || UseRawUserInput))
            {

                normalizedPattern = pattern.Trim();
                while (normalizedPattern.EndsWith("?") || normalizedPattern.EndsWith("."))
                {
                    normalizedPattern = normalizedPattern.Substring(0, normalizedPattern.Length - 1).Trim();
                }

                normalizedPattern = MatchKeyClean(normalizedPattern);
                normalizedThat = MatchKeyClean(that);
                normalizedTopic = MatchKeyClean(topicName);
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
                else
                {
                    if (!normalizedThat.IsWildCard())
                    {
                        // normalizedThat = "* " + normalizedThat;
                    }
                }
                if (normalizedTopic.IsEmpty)
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

                // o.k. build the path
                normalizedPath.Append(Unifiable.Create(normalizedPattern));
                if (RProcessor.UseInlineThat)
                {
                    normalizedPath.Append(Unifiable.ThatTag);
                    normalizedPath.Append(normalizedThat);
                }
                normalizedPath.Append(Unifiable.FlagTag);
                normalizedPath.Append(flag);
                normalizedPath.Append(Unifiable.TopicTag);
                normalizedPath.Append(normalizedTopic);

                return normalizedPath;//.Frozen();
            }
            else
            {
                return Unifiable.Empty;
            }
        }
        public static string MatchKeyClean(Unifiable unifiable)
        {
            return MatchKeyClean(unifiable.AsString());
        }

        public static string MatchKeyClean(string s)
        {
            s = CleanWhitepaces(s).Trim();
            s = s.Replace("  ", " ");
            if (s == "")
            {
                return "*";
            }
            return s;
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
            RTPBot RProcessor = LoaderRequest00.Proccessor;
            string input0 = input;
            if (Unifiable.IsNullOrEmpty(input)) return Unifiable.Empty;
            input = CleanWhitepaces(input);
            if (Unifiable.IsNullOrEmpty(input)) return Unifiable.Empty;
            while (input.EndsWith("?") || input.EndsWith(".") || input.EndsWith("!"))
            {
                input = input.Substring(0, input.Length - 1).Trim();
            }
            if (Unifiable.IsNullOrEmpty(input))
            {
                //return input0;
                return Unifiable.Empty;
            }
            if (isUserInput && false)
            {
                return input;
            }

            Unifiable result = Unifiable.CreateAppendable();

            // objects for normalization of the input
            Normalize.ApplySubstitutions substitutor = new RTParser.Normalize.ApplySubstitutions(RProcessor);
            Normalize.StripIllegalCharacters stripper = new RTParser.Normalize.StripIllegalCharacters(RProcessor);

            Unifiable substitutedInput = substitutor.Transform(" " + input + " ").Trim();
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
                result.Append(normalizedWord);
            }

            return Unifiable.ToVMString(result).Replace("  ", " "); // make sure the whitespace is neat
        }
        #endregion

        public static bool AimlSame(string xml1, string xml2)
        {
            if (xml1 == xml2) return true;
            if (xml1 == null) return String.IsNullOrEmpty(xml2);
            if (xml2 == null) return String.IsNullOrEmpty(xml1);
            xml1 = CleanWhitepacesLower(xml1);
            xml2 = CleanWhitepacesLower(xml2);
            if (xml1.Length != xml2.Length) return false;
            if (xml1 == xml2) return true;
            if (xml1.ToUpper() == xml2.ToUpper())
            {
                return true;
            }
            return false;
        }


        public static string CleanWhitepaces(string xml2)
        {
            if (xml2 == null) return xml2;
            const long maxCleanSize = 2 << 16;
            int inlen = xml2.Length;
            if (inlen > maxCleanSize)
            {
                return xml2;
            }

            String s = "";

            bool chgd = false;
            bool xmlFound = false;
            bool inwhite = true;
            bool pendingWhitespace = false;
            char lastChar = '\0';
            foreach (char c0 in xml2)
            {

                if (c0 <= 32)
                {
                    if (inwhite)
                    {
                        chgd = true;
                        continue;
                    }
                    inwhite = true;
                    pendingWhitespace = true;
                    continue;
                }
                switch (c0)
                {
                    case '/':
                        if (lastChar == 'r')
                        {
                            xmlFound = true;
                        }
                        inwhite = true;
                        if (pendingWhitespace)
                        {
                            chgd = true;
                            pendingWhitespace = false;
                        }
                        break;
                    case '>':
                    case '<':
                    case '\\':
                        inwhite = true;
                        if (pendingWhitespace)
                        {
                            chgd = true;
                            pendingWhitespace = false;
                        }
                        break;
                    default:
                        inwhite = false;
                        break;
                }
                if (pendingWhitespace)
                {
                    s += ' ';
                    pendingWhitespace = false;
                }
                s += c0;
                lastChar = c0;
            }
            if (pendingWhitespace) chgd = true;
            int len = s.Length;
            if (xmlFound)
            {
                s = s.Replace("<sr/>", "<srai><star index=\"1\"/></srai>");
                s = s.Replace("<star/>", "<star index=\"1\"/>");
                if (len != s.Length) chgd = true;
            }
            if (!chgd)
            {
                if (len != inlen)
                {
                    return s;
                }
                return xml2;
            }
            //s = s.Replace("<star index=\"1\"", "<star");

            return s;
        }

        public static string CleanWhitepacesLower(string xml2)
        {
            if (xml2 == null) return xml2;
            return CleanWhitepaces(xml2).ToLower().Replace(".", "").Replace("?", "").Replace("!", "");
        }

        public static bool ContainsAiml(Unifiable unifiable)
        {
            String s = unifiable.AsString();
            if (s.Contains(">") && s.Contains("<")) return true;
            if (s.Contains("&"))
            {
                return true;
            }
            return false;
        }

        public static string NodeInfo(XmlNode templateNode, Func<string, XmlNode, string> funct)
        {
            string s = null;
            XmlNode nxt = templateNode;
            s = funct("same", nxt);
            if (s != null) return s;
            nxt = templateNode.NextSibling;
            s = funct("next", nxt);
            if (s != null) return s;
            nxt = templateNode.PreviousSibling;
            s = funct("prev", nxt);
            if (s != null) return s;
            nxt = templateNode.ParentNode;
            s = funct("prnt", nxt);
            if (s != null) return s;
            return s;
        }

        public static string LocationEscapedInfo(XmlNode templateNode)
        {
            return "<!-- " + LocationInfo(templateNode) + " -->";
        }

        public static string LocationInfo(XmlNode templateNode)
        {
            string lines = NodeInfo(templateNode, LineNoInfo) ?? "(-1,-1)";
            string doc = NodeInfo(templateNode,
                                  (
                                      (strng, node) =>
                                      {
                                          if (node == null) return null;
                                          var od = node.OwnerDocument;
                                          if (od == null) return null;
                                          string st = od.ToString().Trim();
                                          if (st.Length == 0) return null;
                                          return st;
                                      }));
            if (doc == null)
            {
                doc = "nodoc";
            }
            return doc + ":" + lines;
        }

        private static string LineNoInfo(string where, XmlNode templateNode)
        {
            string s = null;
            LineInfoElement li = templateNode as LineInfoElement;
            if (li != null)
            {
                if (li.LinePosition != 0 && li.LinePosition != 0)
                {
                    return "(" + li.LinePosition + "," + li.LinePosition + ") ";
                }
                XmlNode Parent = li.ParentNode;
                if (Parent != null && Parent != li)
                {
                    s = LineNoInfo(where + ".prnt", Parent);
                }
            }
            return s;
        }

        public static string TextAndSourceInfo(XmlNode templateNode)
        {
            return TextInfo(templateNode) + " " + LocationEscapedInfo(templateNode);
        }

        public static string TextInfo(XmlNode templateNode)
        {
            XmlNode textNode = templateNode;//.ParentNode ?? templateNode;
            string s = CleanWhitepaces(textNode.OuterXml);
            if (String.IsNullOrEmpty(s))
            {
                var Parent = templateNode.ParentNode;
                if (Parent != null && Parent != templateNode)
                {
                    return TextInfo(Parent);
                }
                return textNode.OuterXml;
            }
            return s;
        }

        public static string ParentTextAndSourceInfo(XmlNode element)
        {
            return TextAndSourceInfo(element.ParentNode ?? element) + " " + LocationEscapedInfo(element);
        }

        public static void PrintResult(Result result, OutputDelegate console)
        {
            console("-----------------------------------------------------------------");
            console("Result: " + result.Graph + " Request: " + result.request);
            foreach (var s in result.InputSentences)
            {
                console("input: \"" + s + "\"");
            }
            PrintTemplates(result.UsedTemplates, console);
            foreach (var s in result.SubQueries)
            {
                console("\n" + s);
            }
            console("-");
            foreach (var s in result.OutputSentences)
            {
                console("outputsentence: " + s);
            }
            console("-----------------------------------------------------------------");
        }

        public static string GetTemplateSource(IEnumerable CI)
        {
            if (CI == null) return "";
            string hide = "";
            foreach (var ci in CI)
            {
                string c;
                if (ci is IAIMLInfo) c = ((IAIMLInfo)ci).ToFileString();
                else c = "" + ci;
                string ss = "" + CleanWhitepaces(c) + "\n";
                if (hide.Contains(ss)) continue;
                hide += ss;
            }
            return hide;
        }

        public static void PrintTemplates(IEnumerable CI, OutputDelegate console)
        {
            if (CI == null) return;
            string hide = "";
            foreach (var ci in CI)
            {
                string c;
                if (ci is IAIMLInfo) c = ((IAIMLInfo)ci).ToFileString();
                else c = "" + ci;
                string ss = "" + CleanWhitepaces(c) + "\n";
                if (hide.Contains(ss)) continue;
                if (hide.Length > 30000) hide = "";
                hide += ss;
                console(" {0}", c);
            }
            return;
        }

        public static string CleanWhitepaces(object info)
        {
            if (info is XmlNode)
            {
                XmlNode n = (XmlNode)info;
                if (n.Name == "template") info = n.ParentNode;
            }
            if (info is TemplateInfo)
            {
                info = ((TemplateInfo)info).CategoryInfo;
            }
            return CleanWhitepaces("" + info);
        }

        public static bool IsSilentTag(XmlNode node)
        {
            // if (true) return false;
            if (node.Name == "think") return true;
            if (node.NodeType == XmlNodeType.Text)
            {
                string innerText = node.InnerText;
                if (innerText.Trim().Length == 0)
                {
                    return true;
                }
                return false;
            }
            if (node.Name == "template")
            {
                foreach (XmlNode xmlNode in node.ChildNodes)
                {
                    if (!IsSilentTag(xmlNode)) return false;
                }
                if (node.ChildNodes.Count != 1)
                {
                    return true;
                }
                return true;
            }
            return false;
        }
    }

    public class XmlDocumentLineInfo : XmlDocument
    {
        public override void Load(Stream reader)
        {
            if (reader is IXmlLineInfo)
            {
                LineTracker = (IXmlLineInfo)reader;
            }
            base.Load(reader);
        }

        public override void Load(XmlReader reader)
        {
            if (reader is IXmlLineInfo)
            {
                LineTracker = (IXmlLineInfo)reader;
            }
            base.Load(reader);
        }
        public override void Load(TextReader reader)
        {
            base.Load(new XmlTextReader(reader));
        }

        public override string ToString()
        {
            return InfoString ?? base.ToString();
        }
        private Stream LineInfoReader;
        public IXmlLineInfo LineTracker;
        public string InfoString;
        private bool PresrveWhitespace = false;
        public XmlDocumentLineInfo(Stream lineInfoReader, string toString)
        {
            InfoString = toString;
            LineInfoReader = lineInfoReader;
        }
        public XmlDocumentLineInfo(string toString, bool presrveWhite)
        {
            PresrveWhitespace = true;
            InfoString = toString;
        }

        public XmlDocumentLineInfo(IXmlLineInfo lineInfoReader, string toString)
        {
            InfoString = toString;
            LineTracker = lineInfoReader;
        }

        /// <summary>
        /// Creates an <see cref="T:System.Xml.XmlNode"/> object based on the information in the <see cref="T:System.Xml.XmlReader"/>. The reader must be positioned on a node or attribute.
        /// </summary>
        /// <returns>
        /// The new XmlNode or null if no more nodes exist.
        /// </returns>
        /// <param name="reader">The XML source 
        ///                 </param><exception cref="T:System.NullReferenceException">The reader is positioned on a node type that does not translate to a valid DOM node (for example, EndElement or EndEntity). 
        ///                 </exception>
        public override XmlNode ReadNode(XmlReader reader)
        {
            if (reader is IXmlLineInfo)
            {
                LineTracker = (IXmlLineInfo)reader;
            }
            return base.ReadNode(reader);
        }

        public override void LoadXml(string xml)
        {
            base.Load(new XmlTextReader(new StringReader(xml)));
        }
        protected internal virtual void XmlText(string strData, System.Xml.XmlDocument doc)
        {

        }
        public override XmlText CreateTextNode(string text)
        {
            string clean = AIMLLoader.CleanWhitepaces(text);
            if (clean != text)
            {
                if (PresrveWhitespace) return new XmlTextLineInfo(text, this);
                return new XmlTextLineInfo(clean, this);
            }
            return new XmlTextLineInfo(text, this);
        }

        public override XmlAttribute CreateAttribute(string prefix, string localName, string namespaceURI)
        {
            return new XmlAttributeLineInfo(prefix, localName, namespaceURI, this);
        }
        public override XmlWhitespace CreateWhitespace(string text)
        {
            return base.CreateWhitespace(text);
        }
        public override XmlElement CreateElement(string prefix, string localname, string nsURI)
        {
            LineInfoElement elem = new LineInfoElement(prefix, localname, nsURI, this);
            try
            {
                //if (LineInfoReader != null && LineInfoReader.Position != -1)
                //  elem.SetPos(LineInfoReader.Position);
            }
            catch (Exception)
            {
            }
            if (LineTracker != null)
            {
                elem.SetLineInfo(LineTracker.LineNumber, LineTracker.LinePosition);
            }
            return elem;
        }

        public static XmlReader CreateXmlTextReader(Stream stream)
        {
            return XmlTextReader.Create(stream, DefaultSettings);
        }
        internal static XmlReader CreateXmlTextReader(StringReader tr)
        {
            XmlTextReader xmlTextReader = new XmlTextReader(tr);
            return XmlTextReader.Create(tr, DefaultSettings);
        }


        /***
         
         This document already has a 'DocumentElement' node.
          at System.Xml.XmlDocument.IsValidChildType(XmlNodeType type)
          at System.Xml.XmlDocument.AppendChildForLoad(XmlNode newChild, XmlDocument doc)
          at System.Xml.XmlLoader.LoadDocSequence(XmlDocument parentDoc)
          at System.Xml.XmlLoader.Load(XmlDocument doc, XmlReader reader, Boolean preserveWhitespace)
          at System.Xml.XmlDocument.Load(XmlReader reader)
                 
         */

        protected static XmlReaderSettings DefaultSettings
        {
            get
            {
                XmlReaderSettings settings = new XmlReaderSettings();
                settings.ConformanceLevel = ConformanceLevel.Fragment;
                settings.IgnoreWhitespace = true;
                settings.IgnoreComments = true;
                return settings;
            }
        }

    }

    public class XmlAttributeLineInfo : XmlAttribute, IXmlLineInfo
    {
        public XmlAttributeLineInfo(string prefix, string name, string uri, XmlDocumentLineInfo doc)
            : base(prefix, name, uri, doc)
        {

        }

        public override string ToString()
        {
            return AIMLLoader.TextAndSourceInfo(this);
        }
        public int lineNumber = 0;
        public int linePosition = 0;
        public long charPos = 0;
        public LineInfoElement lParent;
        public void SetLineInfo(int linenum, int linepos)
        {
            lineNumber = linenum;
            linePosition = linepos;
        }
        public int LineNumber
        {
            get
            {
                return lineNumber;
            }
        }
        public int LinePosition
        {
            get
            {
                return linePosition;
            }
        }
        public bool HasLineInfo()
        {
            return true;
        }

        public void SetPos(long position)
        {
            charPos = position;
        }


        internal void SetParentFromNode(XmlNode xmlNode)
        {

            var pn = xmlNode.ParentNode;
            if (pn is LineInfoElement)
            {
                lParent = (LineInfoElement)pn;
            }
            if (!(xmlNode is LineInfoElement))
            {
                xmlNode = lParent;
            }
            if (xmlNode is LineInfoElement)
            {
                LineInfoElement lie = (LineInfoElement)xmlNode;
                lineNumber = lie.LineNumber;
                linePosition = lie.linePosition;
                charPos = lie.charPos;
            }
        }

        public override XmlNode ParentNode
        {
            get
            {
                if (lParent == null) return base.ParentNode;
                return lParent;
            }
        }
    }

    //#define HOLD_PARENT
    public class XmlTextLineInfo : XmlText, IXmlLineInfo
    {
        public XmlTextLineInfo(string text, XmlDocumentLineInfo info)
            : base(text, info)
        {
        }
        public override string ToString()
        {
            return AIMLLoader.TextAndSourceInfo(this);
        }
        public int lineNumber = 0;
        public int linePosition = 0;
        public long charPos = 0;
        public LineInfoElement lParent;
        public void SetLineInfo(int linenum, int linepos)
        {
            lineNumber = linenum;
            linePosition = linepos;
        }
        public int LineNumber
        {
            get
            {
                return lineNumber;
            }
        }
        public int LinePosition
        {
            get
            {
                return linePosition;
            }
        }
        public bool HasLineInfo()
        {
            return true;
        }

        public void SetPos(long position)
        {
            charPos = position;
        }


        internal void SetParentFromNode(XmlNode xmlNode)
        {

            var pn = xmlNode.ParentNode;
            if (pn is LineInfoElement)
            {
                lParent = (LineInfoElement)pn;
            }
            if (!(xmlNode is LineInfoElement))
            {
                xmlNode = lParent;
            }
            if (xmlNode is LineInfoElement)
            {
                LineInfoElement lie = (LineInfoElement)xmlNode;
                lineNumber = lie.LineNumber;
                linePosition = lie.linePosition;
                charPos = lie.charPos;
            }
        }

        public override XmlNode ParentNode
        {
            get
            {
                if (lParent == null) return base.ParentNode;
                return lParent;
            }
        }
    }

    public class LineInfoElement : XmlElement, IXmlLineInfo
    {
        internal LineInfoElement(string prefix, string localname, string nsURI, XmlDocument doc)
            : base(prefix, localname.ToLower(), nsURI, doc)
        {
            //((XmlDocumentLineInfo)doc).IncrementElementCount();
        }
        public override string ToString()
        {
            return AIMLLoader.TextAndSourceInfo(this);
        }
        public int lineNumber = 0;
        public int linePosition = 0;
        public long charPos = 0;
        public LineInfoElement lParent;
        public void SetLineInfo(int linenum, int linepos)
        {
            lineNumber = linenum;
            linePosition = linepos;
        }
        public int LineNumber
        {
            get
            {
                return lineNumber;
            }
        }
        public int LinePosition
        {
            get
            {
                return linePosition;
            }
        }
        public bool HasLineInfo()
        {
            return true;
        }

        public void SetPos(long position)
        {
            charPos = position;
        }


        internal void SetParentFromNode(XmlNode xmlNode)
        {

            var pn = xmlNode.ParentNode;
            if (pn is LineInfoElement)
            {
                lParent = (LineInfoElement)pn;
            }
            if (!(xmlNode is LineInfoElement))
            {
                xmlNode = lParent;
            }
            if (xmlNode is LineInfoElement)
            {
                LineInfoElement lie = (LineInfoElement)xmlNode;
                lineNumber = lie.LineNumber;
                linePosition = lie.linePosition;
                charPos = lie.charPos;
            }
        }

        public override XmlNode ParentNode
        {
            get
            {
                if (lParent == null) return base.ParentNode;
                return lParent;
            }
        }
    } // End LineInfoElement class.
}
