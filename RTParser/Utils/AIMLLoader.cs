using System;
using System.Collections;
using System.Collections.Generic;
using System.Net;
using System.IO;
using System.Threading;
using System.Xml;
using System.Xml.Schema;
using System.Text;
using AIMLbot;
using MushDLR223.ScriptEngines;
using RTParser.Variables;
using UPath = RTParser.Unifiable;
using MushDLR223.Virtualization;

namespace RTParser.Utils
{
    /// <summary>
    /// A utility class for loading AIML files from disk into the graphmaster structure that 
    /// forms an AIML RProcessor's "brain"
    /// </summary>
    public class AIMLLoader : XmlNodeEvaluator
    {
        #region Attributes
        /// <summary>
        /// The RProcessor whose brain is being processed
        /// </summary>
        public RTParser.RTPBot RProcessorOld
        {
            get
            {
                return LoaderRequest00.TargetBot;
            }
        }


        private TestCaseRunner testCaseRunner;
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
            testCaseRunner = new TestCaseRunner(this);
            XmlNodeEvaluators.Add(testCaseRunner);
            //XmlNodeEvaluators.Add(this);
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
            if (!HostSystem.FileOrDirExists(path))
            {
                RTPBot.writeDebugLine("WARNING PATH NOT EXIST ERROR: " + path);
            }
            return path;
        }

        static string GetBaseDirectory(LoaderOptions loadOpts)
        {
            string baseFile = loadOpts.CurrentlyLoadingFrom ?? loadOpts.CurrentFilename ?? LoaderOptions.MISSING_FILE;
            if (baseFile == null) return ".";
            return HostSystem.GetBaseDir(baseFile);
        }

        public void loadAIMLURI(string path, LoaderOptions loadOpts)
        {

            RTPBot RProcessor = loadOpts.RProcessor;
            loadOpts.Loading0 = path;
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
                        RTPBot.loadConfigs(RProcessor, path, request);
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
                writeToLog("WARNING (Re)writing url: " + path);
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
                        loadOpts = request.LoadOptions;
                        this.loadAIMLStream(tr, loadOpts);
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
        public void loadAIMLStream(Stream input0, LoaderOptions loadOpts)
        {

            RTPBot RProcessor = loadOpts.RProcessor;
            Request request = loadOpts.TheRequest;
            string path = request.Filename;
            loadOpts = EnsureOptions(loadOpts, request, path);
            path = ResolveToURI(path, loadOpts);

            var xtr = XmlDocumentLineInfo.CreateXmlTextReader(input0);
            string namefile = "" + path;
            while (!xtr.EOF)
            {
                //  IXmlLineInfo text = (IXmlLineInfo)xtr;
                try
                {
                    XmlDocumentLineInfo doc = new XmlDocumentLineInfo("" + namefile, false);
                    doc.Load(xtr);
                    if (doc.DocumentElement == null)
                    {
                        RProcessor.writeToLog("ERROR: No Document at " + namefile);
                        //        continue;
                    }
                    this.loadAIMLNode(doc.DocumentElement, loadOpts, request);
                }
                catch (Exception e2)
                {
                    String s = "which causes loadAIMLStream '" + e2 + "' " + loadOpts; //" charpos=" + xtr1.LineNumber;
                    //s = s + "\n" + e2.Message + "\n" + e2.StackTrace + "\n"; //+ s;
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
        /// <summary>
        /// Given an XML document containing valid AIML, attempts to load it into the graphmaster
        /// </summary>
        /// <param name="doc">The XML document containing the AIML</param>
        /// <param name="loadOpts">Where the XML document originated</param>
        public void loadAIMLStreamFallback(Stream input0, LoaderOptions loadOpts)
        {

            RTPBot RProcessor = loadOpts.RProcessor;
            Request request = loadOpts.TheRequest;
            string path = request.Filename;
            loadOpts = EnsureOptions(loadOpts, request, path);
            path = ResolveToURI(path, loadOpts);

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
                try
                {
                    XmlDocumentLineInfo doc = new XmlDocumentLineInfo("" + namefile, false);
                    doc.LoadXml(ssss);
                    if (doc.DocumentElement == null)
                    {
                        RProcessor.writeToLog("ERROR: No Document at " + namefile);
                        //        continue;
                    }
                    this.loadAIMLNode(doc.DocumentElement, loadOpts, request);
                }
                catch (Exception e2)
                {
                    String s = "which causes loadAIMLStream '" + e2 + "' " + loadOpts; //" charpos=" + xtr1.LineNumber;
                    //s = s + "\n" + e2.Message + "\n" + e2.StackTrace + "\n"; //+ s;
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

        public void InsideLoaderContext(XmlNode currentNode, Request request, SubQuery query, ThreadStart doit)
        {
            query = query ?? request.CurrentQuery;
            //Result result = query.Result;
            RTPBot RProcessor = request.TargetBot;
            var prev = RProcessor.Loader;
            try
            {
                RProcessor.Loader = this;
                // Get a list of the nodes that are children of the <aiml> tag
                // these nodes should only be either <topic> or <category>
                // the <topic> nodes will contain more <category> nodes
                string currentNodeName = currentNode.Name.ToLower();

                ThreadStart ts = AIMLTagHandler.EnterTag(request, currentNode, query);
                try
                {
                    doit();
                }
                finally
                {
                    ts();
                }
            }
            finally
            {
                RProcessor.Loader = prev;
            }
        }


        public List<CategoryInfo> loadAIMLNodes(IEnumerable nodes, LoaderOptions loadOpts, Request request)
        {
            if (nodes!=null)
            {
                foreach (var node in nodes)
                {
                    loadAIMLNode((XmlNode) node, loadOpts, request);
                }
            }
            return loadOpts.CategoryInfos;
        }

        public List<CategoryInfo> loadAIMLNode(XmlNode currentNode, LoaderOptions loadOpts, Request request)
        {
            List<CategoryInfo> CIs = loadOpts.CategoryInfos;
            RTPBot RProcessor = loadOpts.RProcessor;
            var prev = RProcessor.Loader;
            try
            {
                RProcessor.Loader = this;
                // Get a list of the nodes that are children of the <aiml> tag
                // these nodes should only be either <topic> or <category>
                // the <topic> nodes will contain more <category> nodes
                string currentNodeName = currentNode.Name.ToLower();
                if (currentNodeName == "aiml")
                {
                    InsideLoaderContext(currentNode, request, request.CurrentQuery,
                                        () =>
                                        CIs.AddRange(loadAIMLNodes(currentNode.ChildNodes, loadOpts, request)));
                    return CIs;
                }
                if (currentNodeName == "topic")
                {
                    this.processTopic(currentNode, currentNode.ParentNode, loadOpts);
                    return CIs;
                }
                else if (currentNodeName == "category")
                {
                    this.processCategory(currentNode, currentNode.ParentNode, loadOpts);
                    return CIs;
                }
                else if (currentNodeName == "that")
                {
                    InsideLoaderContext(currentNode, request, request.CurrentQuery,
                                        () =>
                                        loadAIMLNodes(currentNode.ChildNodes, loadOpts, request));
                    return CIs;
                    return CIs;
                }
                else if (currentNodeName == "genlmt")
                {
                    string name = RTPBot.GetAttribValue(currentNode, "graph,name,mt,to", null);
                    string from = RTPBot.GetAttribValue(currentNode, "from", null);
                    if (name == null)
                    {
                        name = currentNode.InnerText.Trim();
                    }
                    GraphMaster FROM = request.TargetBot.GetGraph(from, loadOpts.CtxGraph);
                    GraphMaster TO = request.TargetBot.GetGraph(name, loadOpts.CtxGraph);
                    FROM.AddGenlMT(TO);
                    writeToLog("GENLMT: " + FROM + " => " + name + " => " + TO);
                    return CIs;
                }
                else if (currentNodeName == "meta")
                {
                    writeToLog("UNUSED: " + currentNode.OuterXml);
                }
                else if (currentNodeName == "#comment")
                {
                    writeToLog("UNUSED: " + currentNode.OuterXml);
                }
                else 
                {
                    EvalNode(currentNode, request, loadOpts);                   
                }
            }
            finally
            {
                RProcessor.Loader = prev;
            }
            return CIs;
        }

        private void EvalNode(XmlNode currentNode, Request request, LoaderOptions loadOpts)
        {
            string currentNodeName = currentNode.Name.ToLower();
            if (currentNodeName == "root")
            {
                // process each of these child "settings"? nodes
                var prevDict = request.TargetSettings;
                try
                {
                    SettingsDictionary.loadSettingNode(request.TargetSettings, currentNode, true, false, request);
                }
                finally
                {
                    request.TargetSettings = prevDict;
                }
                return;
            }
            if (currentNodeName == "substitutions")
            {
                var prevDict = request.TargetSettings;
                // process each of these child "settings"? nodes
                try
                {
                    request.TargetSettings = request.TargetBot.InputSubstitutions;
                    SettingsDictionary.loadSettingNode(request.TargetSettings, currentNode, true, false, request);
                }
                finally
                {
                    request.TargetSettings = prevDict;
                }
                return;
            }
            if (currentNodeName == "item")
            {
                SettingsDictionary.loadSettingNode(request.TargetSettings, currentNode, true, false, request);
                return;
            }
            if (currentNodeName == "bot")
            {
                SettingsDictionary.loadSettingNode(request.TargetBot.Settings, currentNode, true, false, request);
                return;
            }
            writeToLog("ImmediateAiml:: " + currentNode.OuterXml);
            /*
               <TestCase name="connect">
                    <Input>CONNECT</Input>
                    <ExpectedAnswer>Connected to test case AIML set.</ExpectedAnswer>
               </TestCase>
            */

            if (currentNode.NodeType == XmlNodeType.Comment) return;

            OutputDelegate del = Console.WriteLine;
            HashSet<XmlNode> nodes = new HashSet<XmlNode>();
            bool evaledNode = false;
            foreach (XmlNodeEval funct in GetEvaluators(currentNode))
            {
                evaledNode = true;
                var newNode = funct(currentNode, request, del);
                if (newNode != null)
                {
                    evaledNode = true;
                    foreach (var node in newNode)
                    {
                        nodes.Add(node);
                    }
                }
            }
            if (evaledNode)
            {
                del("evaledNode=" + evaledNode);
                del("nodes.Count=" + nodes.Count);
                int nc = 1;
                foreach (XmlNode n in nodes)
                {
                    del("node {0}:{1}", nc, n);
                    nc++;
                }
                return;
            }
            RTPBot RProcessor = loadOpts.RProcessor;
            string path = request.Filename;
            loadOpts = EnsureOptions(loadOpts, request, path);
            path = ResolveToURI(path, loadOpts);
            try
            {
                ImmediateAiml(currentNode, request, this, null);
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

        private AIMLbot.Result ImmediateAiml(XmlNode node, Request request, AIMLLoader loader, object o)
        {
            RTPBot RProcessor = request.TargetBot;
            return RProcessor.ImmediateAiml(node, request, this, null);
        }

        public virtual IEnumerable<XmlNode> Eval_Element_NodeType(XmlNode src, Request request, OutputDelegate outputdelegate)
        {
            outputdelegate("" + ImmediateAiml(src, request, this, null) + "");
            return new XmlNode[] {src};
        }

        public override IEnumerable<XmlNodeEval> GetEvaluators(XmlNode node)
        {

            List<XmlNodeEval> nodes = new List<XmlNodeEval>();
            foreach (XmlNodeEvaluator xmlNodeEvaluator in XmlNodeEvaluators)
            {
                if (xmlNodeEvaluator == this)
                {
                    nodes.AddRange(base.GetEvaluators(node));
                    continue;
                }
                IEnumerable<XmlNodeEval> nodeE = xmlNodeEvaluator.GetEvaluators(node);
                nodes.AddRange(nodeE);
            }
            return nodes;
        }

        /// <summary>
        /// Given a "topic" topicNode, processes all the categories for the topic and adds them to the 
        /// graphmaster "brain"
        /// </summary>
        /// <param name="topicNode">the "topic" node</param>
        /// <param name="path">the file from which this topicNode is taken</param>
        public List<CategoryInfo> processTopic(XmlNode topicNode, XmlNode outerNode, LoaderOptions path)
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
            return path.CategoryInfos;
        }

        /// <summary>
        /// Given a "topic" topicNode, processes all the categories for the topic and adds them to the 
        /// graphmaster "brain"
        /// </summary>
        /// <param name="topicNode">the "topic" node</param>
        /// <param name="path">the file from which this topicNode is taken</param>
        public List<CategoryInfo> processOuterThat(XmlNode thatNode, XmlNode outerNode, LoaderOptions path)
        {
            List<CategoryInfo> CIS = path.CategoryInfos;
            List<CategoryInfo> newCats = path.CategoryInfos = new List<CategoryInfo>();
            // find the name of the topic or set to default "*"
            Unifiable thatPattten = RTPBot.GetAttribValue(thatNode, "pattern,value,name", Unifiable.STAR);
            // process all the category nodes
            ThatInfo newThatInfo = new ThatInfo(thatNode, thatPattten);
            foreach (XmlNode cateNode in thatNode.ChildNodes)
            {
                // getting stacked up inside
                loadAIMLNode(cateNode, path, path.TheRequest);
            }
            foreach (var ci0 in newCats)
            {
                ci0.AddPrecondition(newThatInfo);
            }

            CIS.AddRange(newCats);
            return path.CategoryInfos = CIS;
        }
        /// <summary>
        /// Adds a category to the graphmaster structure using the default topic ("*")
        /// </summary>
        /// <param name="cateNode">the XML node containing the category</param>
        /// <param name="path">the file from which this category was taken</param>
        public List<CategoryInfo> processCategory(XmlNode cateNode, XmlNode outerNode, LoaderOptions path)
        {
            return processCategoryWithTopic(cateNode, Unifiable.STAR, outerNode, path);
        }

        /// <summary>
        /// Adds a category to the graphmaster structure using the given topic
        /// </summary>
        /// <param name="cateNode">the XML node containing the category</param>
        /// <param name="topicName">the topic to be used</param>
        /// <param name="path">the file from which this category was taken</param>
        private List<CategoryInfo> processCategoryWithTopic(XmlNode cateNode, Unifiable topicName, XmlNode outerNode, LoaderOptions path)
        {
            List<CategoryInfo> CIs = path.CategoryInfos;
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
                    CIs.Add(addCatNode(cateNode, pattern, path, template, topicName, outerNode));                   
                }
            }
            return CIs;
        }

        private CategoryInfo addCatNode(XmlNode cateNode, XmlNode patternNode, LoaderOptions path, XmlNode templateNode,
            Unifiable topicName, XmlNode outerNode)
        {
            XmlNode guardnode = FindNode("guard", cateNode, null);
            if (guardnode == null && outerNode != null && outerNode.Name != "aiml")
            {
                guardnode = FindNode("guard", outerNode, null);
            }
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
                    categoryInfo.SetCategoryTag(categoryPath, patternInfo, categoryInfo,
                                                  outerNode, templateNode, guard, thatInfo);
                    GraphMaster pathCtxGraph = path.CtxGraph;
                    pathCtxGraph.addCategoryTag(categoryPath, patternInfo, categoryInfo,
                                                outerNode, templateNode, guard, thatInfo);
                    return categoryInfo;
                }
                catch (Exception e)
                {
                    string s = "ERROR! Failed to load a new category into the graphmaster where the path = " +
                               categoryPath + " and templateNode = " + templateNode.OuterXml +
                               " produced by a category in the file: " + path + "\n";
                    writeToLog(s + e + "\n" + s);
                    return null;
                }
            }
            else
            {
                writeToLog("ERROR WARNING! Attempted to load a new category with an empty patternNode where the path = " + categoryPath + " and templateNode = " + templateNode.OuterXml + " produced by a category in the file: " + path);
                return null;
            }
        }

        public override string ToString()
        {
            return "[AIMLLoader req='" + LoaderRequest00 + "' bot='" + LoaderRequest00.TargetBot.BotID + "']";
            return base.ToString();
        }

        public void writeToLog(string message, params object[] args)
        {
            string prefix = ToString();
            LoaderRequest00.writeToLog("LOADERTRACE: " + message + " while " + prefix, args);
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
                //TODO BEFORE COMMIT DMILES
                newLineInfoPattern.ReadOnly = true;
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

        protected static XmlNode PatternStar
        {
            get
            {
                var ps = AIMLTagHandler.getNode("<pattern name=\"*\">*</pattern>");
                ps.ReadOnly = true;
                return ps;
            }
        }
        public bool ThatWideStar = false;
        public static bool useInexactMatching = false;
        private List<XmlNodeEvaluator> XmlNodeEvaluators = new List<XmlNodeEvaluator>();

        /// <summary>
        /// Given a name will try to find a node named "name" in the childnodes or return null
        /// </summary>
        /// <param name="name">The name of the node</param>
        /// <param name="node">The node whose children need searching</param>
        /// <returns>The node (or null)</returns>
        static public XmlNode FindNode(string name, XmlNode node, XmlNode ifMissing)
        {
            name = name.ToLower();
            foreach (var n in name.Split(",".ToCharArray(), StringSplitOptions.RemoveEmptyEntries))
            {
                foreach (XmlNode child in node.ChildNodes)
                {
                    if (child.Name.ToLower() == n)
                    {
                        return child;
                    }
                }
            }
            return ifMissing;
        }
        static public XmlNode FindNodeOrHigher(string name, XmlNode node, XmlNode ifMissing)
        {
            if (node == null) return ifMissing;
            foreach (var n in name.Split(",".ToCharArray(), StringSplitOptions.RemoveEmptyEntries))
            {
                foreach (XmlNode child in node.ChildNodes)
                {
                    if (child.Name.ToLower() == n)
                    {
                        return child;
                    }
                }
            }
            return FindHigher(name, node.ParentNode, ifMissing);
        }
        static public XmlNode FindHigher(string name, XmlNode node, XmlNode ifMissing)
        {
            if (node == null) return ifMissing;
            foreach (var n in name.Split(",".ToCharArray(), StringSplitOptions.RemoveEmptyEntries))
            {
                if (node.Name.ToLower() == n)
                {
                    return node;
                }
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
            var RProcessor = LoaderRequest00.TargetBot;

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
                // clip only one off
                if (isUserInput) if (normalizedPattern.EndsWith("?") || normalizedPattern.EndsWith(".") || normalizedPattern.EndsWith("!"))
                {
                    normalizedPattern = normalizedPattern.Substring(0, normalizedPattern.Length - 1).Trim();
                }
                if (false)
                {
                    normalizedPattern = MatchKeyClean(normalizedPattern);
                    normalizedThat = MatchKeyClean(that);
                    normalizedTopic = MatchKeyClean(topicName);
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
                Unifiable addTagEnd = isUserInput ? Unifiable.TagEndText : null;
                Unifiable addTagStart = isUserInput ? Unifiable.TagStartText : null;

                //useInexactMatching = true;
                if (useInexactMatching)
                {
                    if (!isUserInput)
                    {
                        normalizedPattern = PadStars(normalizedPattern);
                        normalizedThat = PadStars(normalizedThat);
                        normalizedTopic = PadStars(normalizedTopic);
                    }
                    else
                    {
                        normalizedPattern = NoWilds(normalizedPattern);
                        normalizedThat = NoWilds(normalizedThat);
                        normalizedTopic = NoWilds(normalizedTopic);
                    }
                }
                else
                {
                    addTagEnd = addTagStart = null;
                }

                // o.k. build the path
                normalizedPath.Append(Unifiable.InputTag);
                if (addTagStart != null) normalizedPath.Append(addTagStart);
                normalizedPath.Append(Unifiable.Create(normalizedPattern));
                if (addTagEnd != null) normalizedPath.Append(addTagEnd);
                if (RProcessor.UseInlineThat)
                {
                    normalizedPath.Append(Unifiable.ThatTag);
                    if (addTagStart != null) normalizedPath.Append(addTagStart);
                    normalizedPath.Append(normalizedThat);
                    if (addTagEnd != null) normalizedPath.Append(addTagEnd);
                }
                normalizedPath.Append(Unifiable.FlagTag);
                normalizedPath.Append(flag);
                normalizedPath.Append(Unifiable.TopicTag);
                if (addTagStart != null) normalizedPath.Append(addTagStart);
                normalizedPath.Append(normalizedTopic);
                if (addTagEnd != null) normalizedPath.Append(addTagEnd);

                return normalizedPath; //.Frozen();
            }
            else
            {
                return Unifiable.Empty;
            }
        }

        private string NoWilds(string pattern)
        {
            pattern = pattern.Trim();
            int pl = pattern.Length;
            if (pl < 4) return pattern;
            while (pattern.Contains("*"))
            {
                pattern = pattern.Replace("*", " ").Trim();
            }
            return pattern;
        }

        private string PadStars(string pattern)
        {
            pattern = pattern.Trim();
            int pl = pattern.Length;
            if (pl == 0) return "~*";
            if (pl == 1) return pattern;
            if (pl == 2) return pattern;
            if (char.IsLetterOrDigit(pattern[pl - 1])) pattern = pattern + " ~*";
            if (char.IsLetterOrDigit(pattern[0])) pattern = "~* " + pattern;
            return pattern;
        }

        public static string MatchKeyClean(Unifiable unifiable)
        {
            return MatchKeyClean(unifiable.AsString());
        }

        public static string MatchKeyClean(string s)
        {
            s = CleanWhitepaces(s);
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
            var RProcessor = LoaderRequest00.TargetBot;
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
            return CleanWhitepaces(xml2, null, Unused, Unused);
        }

        private static bool Unused(char arg1, char arg2)
        {
            throw new NotImplementedException();
            return false;
        }


        public static string CleanWildcards(string text)
        {
            if (text == null) return text;
            if (text.Contains("\""))
            {
                return CleanWhitepaces(text);
            }
            string clean = text;
            clean = AIMLLoader.CleanWhitepaces(
                text, "*_",
                new Func<char, char, bool>((c0, c1) =>
                                               {
                                                   if (char.IsLetterOrDigit(c1) || char.IsControl(c1)) return true;
                                                   if ("\"'".Contains("" + c1)) return false;
                                                   return false;
                                               }),
                new Func<char, char, bool>((c0, c1) =>
                                               {
                                                   if (char.IsLetterOrDigit(c0) || char.IsControl(c0)) return true;
                                                   if ("\"'".Contains("" + c0)) return false;
                                                   return false;
                                               }));
            return clean;
        }

        public static string CleanWhitepaces(string xml2, string padchars,
            Func<char, char, bool> ifBefore, Func<char, char, bool> ifAfter)
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
                        if (padchars != null)
                        {

                            bool before = padchars.Contains("" + lastChar);
                            if (before && ifBefore(lastChar, c0))
                            {
                                if (!inwhite) pendingWhitespace = true;
                            }

                            bool after = padchars.Contains("" + c0);
                            if (after && ifAfter(lastChar, c0))
                            {
                                if (!inwhite) pendingWhitespace = true;
                            }

                        }
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
                s = s.Replace("star/>", "star index=\"1\"/>");
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
                if (li.LineNumber != 0 && li.LinePosition != 0)
                {
                    return "(" + li.LineNumber + "," + li.LinePosition + ") ";
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
            PrintTemplates(result.UsedTemplates, console, true);
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

        public static void PrintTemplates(IEnumerable CI, OutputDelegate console, bool fileinfo)
        {
            if (CI == null) return;
            string hide = "";
            foreach (var ci in CI)
            {
                string c;
                var cate = ci as IAIMLInfo;
                if (cate != null) c = cate.ToFileString();
                else c = "" + ci;
                string ss = CleanWhitepaces(c);
                if (hide.Contains(ss)) continue;
                if (hide.Length > 30000) hide = "";
                hide += ss;
                if (cate != null)
                {
                    c = cate.SourceInfo();
                    if (!c.Contains("(0,0)"))
                    {
                        if (ss.Length > 50) ss = ss + "\n";
                        ss = ss + "   <!--   " + c + "  -->";
                    }
                }
                console(" {0}", ss);
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

        public static LineInfoElement CopyNode(XmlNode node, bool copyParent)
        {
            if (copyParent)
            {
                XmlNode parentNode = node.ParentNode;
                if (parentNode != null)
                {
                    LineInfoElement xmlNode0 = node as LineInfoElement;
                    int idx = xmlNode0.IndexInBaseParent;
                    if (idx >= 0)
                    {
                        var parentCopy = (LineInfoElement)parentNode.CloneNode(true);
                        parentCopy.ReadOnly = xmlNode0.ReadOnly;
                        xmlNode0 = (LineInfoElement)parentCopy.ChildNodes[idx];
                        xmlNode0.ReadOnly = !copyParent;
                        return xmlNode0;
                    }
                }
            }

            XmlNode oc = node.CloneNode(true);

            LineInfoElement xmlNode = oc as LineInfoElement;
            if (xmlNode == null)
            {
                xmlNode = AIMLTagHandler.getNode(node.OuterXml, node);
                xmlNode.ReadOnly = false;
            }
            else
            {
                xmlNode.ReadOnly = false;
            }
            xmlNode.ReadOnly = false;
            return xmlNode;
        }

        public static LineInfoElement CopyNode(string newName, XmlNode node, bool copyParent)
        {
            var od = node.OwnerDocument;
            LineInfoElement newnode = (LineInfoElement)node.OwnerDocument.CreateNode(node.NodeType, newName, node.NamespaceURI);
            newnode.SetParentFromNode(node);
            newnode.lParent = ToLineInfoElement(node.ParentNode);
            var ats = node.Attributes;
            if (ats != null) foreach (XmlAttribute a in ats)
                {
                    XmlAttribute na = od.CreateAttribute(a.Prefix, a.LocalName, a.NamespaceURI);
                    na.Value = a.Value;
                    newnode.Attributes.Append(na);
                }
            foreach (XmlNode a in node.ChildNodes)
            {
                newnode.AppendChild(a.CloneNode(true));
            }
            return (LineInfoElement)newnode;
        }

        public static LineInfoElement ToLineInfoElement(XmlNode pattern)
        {
            if (pattern == null) return null;
            if (pattern is LineInfoElement)
            {
                return (LineInfoElement)pattern;
            }
            return CopyNode(pattern, true);
        }
    }

    internal class TestCaseRunner : XmlNodeEvaluator
    {
        private AIMLLoader Loader;

        public TestCaseRunner(AIMLLoader loader)
            : base("Eval", "_")
        {
            Loader = loader;
        }

        /// <summary>
        ///     <TestCase name="connect">
        //        <Input>CONNECT</Input>
        ///       <ExpectedAnswer>Connected to test case AIML set.</ExpectedAnswer>
        ///    </TestCase>
        /// </summary>
        /// <param name="src"></param>
        /// <param name="request"></param>
        /// <param name="outputdelegate"></param>
        /// <returns></returns>
        public IEnumerable<XmlNode> EvalTestCase(XmlNode src, Request request, OutputDelegate outputdelegate)
        {
            request = request ?? Loader.LoaderRequest00;
            User user = request.user;
            var robot = request.TargetBot ?? Loader.RProcessorOld;

            string tcname =FindNodeOrAttrib(src, "name", null);
            string tcdesc = FindNodeOrAttrib(src, "Description", null);
            string input =  FindNodeOrAttrib(src, "Input", null);
            if (input == null)
            {
                outputdelegate("ERROR cannot find 'Input' in '" + src.OuterXml + "'");
            }
            string userID = FindNodeOrAttrib(src, "UserId,UserName", user.UserID);

            string expectedAnswer = FindNodeOrAttrib(src, "ExpectedAnswer", null);
            if (expectedAnswer == null)
            {
                outputdelegate("ERROR cannot find 'ExpectedAnswer' in '" + src.OuterXml + "'");
            }
            outputdelegate("{0}: {1} ", tcname, tcdesc);
            outputdelegate("{0}: {1} ", userID, input);
            string resp = robot.ChatString(input, userID);
            outputdelegate("{0}: {1} ", robot, resp);
            bool m = Matches(resp, expectedAnswer, FindNodeOrAttrib(src, "MatchType,Match", null));
            outputdelegate("PASSED={0}", m);
            return new[] {src};
        }

        static bool Matches(string resp, string answer, string s)
        {
            return resp == answer;
        }


        static string FindNodeOrAttrib(XmlNode myNode, string names, string defaultNotFound)
        {
            string value = RTPBot.GetAttribValue(myNode, names, defaultNotFound);
            if (value == defaultNotFound)
            {
                XmlNode holder = AIMLLoader.FindNode(names, myNode, null);
                if (holder != null)
                {
                    value = holder.InnerText;
                    return value;
                }
            }
            return defaultNotFound;
        }


        public override string ToString()
        {
            return GetType().Name;
        }
    }
}
