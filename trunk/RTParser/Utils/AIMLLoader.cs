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
using MushDLR223.Utilities;
using RTParser.AIMLTagHandlers;
using RTParser.Variables;
using UPath = RTParser.Unifiable;
using MushDLR223.Virtualization;
using LineInfoElement = RTParser.Utils.LineInfoElementImpl;

namespace RTParser.Utils
{
    /// <summary>
    /// A utility class for loading AIML files from disk into the graphmaster structure that 
    /// forms an AIML RProcessor's "brain"
    /// </summary>
    public class AIMLLoader : XmlNodeEvaluatorImpl
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
        public int loadAIMLDir0(string path, LoaderOptions loadOpts)
        {
            int total = 0;
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
                            total += loadAIMLFile0(f, loadOpts, false);
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
                    total += loadAIMLDir0(path + Path.DirectorySeparatorChar + f, loadOpts);
                }
            }
            return total;
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

        private R LoaderOper<R>(Func<R> action, GraphMaster gm)
        {
            OutputDelegate prev = userTraceRedir;
            try
            {
                userTraceRedir = gm.writeToLog;
                try
                {
                    lock (ErrorList)
                    {
                        lock (gm.LockerObject)
                        {
                            return action();
                        }
                    }
                }
                catch (Exception e)
                {
                    writeToLog("ERROR: LoaderOper {0}", e);
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
            s = string.Format("USERTRACE: " + s, objects);
            if (s.ToUpper().Contains("ERROR"))
            {
                writeToLog(s, objects);
            }
        }

        public int loadAIMLURI(string path, LoaderOptions loadOpts)
        {
            int total = LoaderOper(() => loadAIMLURI0(path, loadOpts), loadOpts.CtxGraph);
            return total;
        }

        public int loadAIMLURI0(string path, LoaderOptions loadOpts)
        {

            RTPBot RProcessor = loadOpts.RProcessor;
            loadOpts.Loading0 = path;
            RProcessor.ReloadHooks.Add(() => loadAIMLURI0(path, loadOpts));
            path = ResolveToURI(path, loadOpts);
            int total = 0;
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
                        total += loadAIMLDir0(path, loadOpts);
                    }
                    finally
                    {
                        request.LoadOptions = savedOpt;
                    }
                    return total;
                }
                else if (HostSystem.FileExists(path))
                {
                    Request request = loadOpts.TheRequest;
                    var savedOpt = request.LoadOptions;
                    try
                    {
                        request.LoadOptions = loadOpts;
                        total += loadAIMLFile0(path, loadOpts, true);
                    }
                    finally
                    {
                        request.LoadOptions = savedOpt;
                    }
                    return total;
                }
                else if (Uri.IsWellFormedUriString(path, UriKind.RelativeOrAbsolute))
                {
                    writeToLog("Processing AIML URI: " + path);
                    var uri = new Uri(path);
                    if (uri.IsFile)
                    {
                        total += loadAIMLURI0(uri.AbsolutePath, loadOpts);
                        return total;
                    }
                    WebRequest req = WebRequest.Create(uri);
                    WebResponse resp = req.GetResponse();
                    Stream stream = resp.GetResponseStream();
                    Request request = loadOpts.TheRequest;
                    var savedOpt = request.LoadOptions;
                    try
                    {
                        loadOpts.Loading0 = uri.ToString();
                        total += loadAIMLStream(stream, loadOpts);
                        writeToLog("Completed AIML URI: " + path);
                        return total;
                    }
                    finally
                    {
                        request.LoadOptions = savedOpt;
                    }
                }
                else
                {
                    string[] pathnames = HostSystem.GetFiles(path);
                    if (pathnames != null && pathnames.Length > 0)
                    {
                        foreach (string pathname in pathnames)
                        {
                            Request request = loadOpts.TheRequest;
                            var savedOpt = request.LoadOptions;
                            try
                            {
                                request.LoadOptions = loadOpts;
                                total += loadAIMLFile0(pathname, loadOpts, false);
                            }
                            finally
                            {
                                request.LoadOptions = savedOpt;
                            }

                        }
                        return total;
                    }
                }
                String nf = "ERROR: XmlTextReader of AIML files (" + path + ")";
                var nfe = new FileNotFoundException(nf);
                RProcessor.writeToLog(nfe);
                writeToLog(nf);
                throw nfe;
            }
            catch (Exception e)
            {
                RProcessor.writeToLog(e);
                writeToLog("ERROR! " + e);
                throw e;
            }
        }

        /// <summary>
        /// Given the name of a file in the AIML path directory, attempts to load it into the 
        /// graphmaster
        /// </summary>
        /// <param name="path">The name of the file to process</param>
        public int loadAIMLFile0(string path, LoaderOptions loadOpts, bool forceReload)
        {
            int total = 0;
            path = ResolveToURI(path, loadOpts);
            RTPBot RProcessor = loadOpts.RProcessor;
            //RProcessor.ReloadHooks.Add(() => loadAIMLFile0(path, loadOpts, forceReload));
            Request request = loadOpts.TheRequest;
            loadOpts = EnsureOptions(loadOpts, request, path);
            //request.TargetBot.ReloadHooks.Add(() => request.Loader.loadAIMLFile0(path, loadOpts, forceReload));

            if (!HostSystem.FileExists(path))
            {
                writeToLog("WARNING (Re)writing url: " + path);
                if (loadOpts.recurse)
                {
                    total += loadAIMLURI0(path, loadOpts);
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
                    //return;
                }
                else
                    writeToLog("Processing AIML file: " + path + " from " + master);
                master.AddFileLoaded(path);
                var tr = HostSystem.OpenRead(path);
                try
                {
                    string pfile = request.Filename;
                    try
                    {
                        request.Filename = path;
                        loadOpts = request.LoadOptions;
                        total += this.loadAIMLStream(tr, loadOpts);
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

                writeToLog("Loaded AIMLFile: '{0}'", path + " from " + master);
                return total;
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
                String s = "ERROR loadAIMLString '" + docString + "' " + loadOpts;
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
        public int loadAIMLStream(Stream input0, LoaderOptions loadOpts)
        {
            int total = 0;
            RTPBot RProcessor = loadOpts.RProcessor;
            Request request = loadOpts.TheRequest;
            string path = request.Filename;
            loadOpts = EnsureOptions(loadOpts, request, path);

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
                    total += this.loadAIMLNode(doc.DocumentElement, loadOpts, request);
                }
                catch (Exception e2)
                {
                    String s = "ERROR: LoadAIMLStream '" + e2 + "' in " + loadOpts; //" charpos=" + xtr1.LineNumber;
                    //s = s + "\n" + e2.Message + "\n" + e2.StackTrace + "\n"; //+ s;
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
            fn = fn.Replace("\\", "/");
            path = path.Replace("\\", "/");
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

        public int InsideLoaderContext(XmlNode currentNode, Request request, SubQuery query, Func<int> doit)
        {
            int total = 0;
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
                    total += doit();
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
            return total;
        }


        private int loadAIMLNodes(IEnumerable nodes, LoaderOptions loadOpts, Request request, List<XmlNode> additionalRules)
        {
            int total = 0;
            if (nodes!=null)
            {

                foreach (var node in nodes)
                {
                    total += loadAIMLNode((XmlNode)node, loadOpts, request);
                }
            }
            return total;
        }

        public int loadAIMLNode(XmlNode currentNode, LoaderOptions loadOpts, Request request)
        {
            List<XmlNode> additionalRules = loadOpts.AdditionalPreconditions;
            return LoaderOper(() => loadAIMLNode0((XmlNode) currentNode, loadOpts, request, additionalRules),
                              loadOpts.CtxGraph);
        }

        public int loadAIMLNode0(XmlNode currentNode, LoaderOptions loadOpts, Request request, List<XmlNode> additionalRules)
        {
            int total = 0;
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
                    total += InsideLoaderContext(currentNode, request, request.CurrentQuery,() =>loadAIMLNodes(currentNode.ChildNodes, loadOpts, request,additionalRules));
                }
                else if (currentNodeName == "topic")
                {
                    additionalRules = additionalRules ?? new List<XmlNode>();
                    additionalRules.Add(currentNode);
                    var vv = this.processTopic(currentNode, currentNode.ParentNode, loadOpts, additionalRules);
                    additionalRules.Remove(currentNode);
                    total += vv.Count;
                }
                else if (currentNodeName == "category")
                {
                    var vv = this.processCategory(currentNode, currentNode.ParentNode, loadOpts, additionalRules);
                    total += vv.Count;
                }
                else if (currentNodeName == "that")
                {
                    additionalRules.Add(currentNode);
                    total +=
                        InsideLoaderContext(currentNode, request, request.CurrentQuery,
                                            () =>
                                            loadAIMLNodes(
                                                currentNode.ChildNodes,
                                                loadOpts,
                                                request,
                                                additionalRules));
                    additionalRules.Remove(currentNode);
                }
                else if (currentNodeName == "meta" || currentNodeName == "#comment")
                {
                    // skip fo now
                }
                else
                {
                    loadOpts.RProcessor.ImmediateAiml(currentNode, request, this, null);
                    total += 1;
                }
            }
            finally
            {
                RProcessor.Loader = prev;
            }
            return total;
        }

        public override IEnumerable<XmlNodeEval> GetEvaluators(XmlNode node)
        {
            return base.GetEvaluatorsFromReflection(node);
        }

        public virtual IEnumerable<XmlNode> Eval_Element_NodeType(XmlNode src, Request request, OutputDelegate outputdelegate)
        {
            //XmlNode node = 
            loadAIMLNode(src, request.LoadOptions, request);
            //if (node == null) return NO_XmlNode;
            return new XmlNode[] {src};
        }


        /// <summary>
        /// Given a "topic" topicNode, processes all the categories for the topic and adds them to the 
        /// graphmaster "brain"
        /// </summary>
        /// <param name="topicNode">the "topic" node</param>
        /// <param name="path">the file from which this topicNode is taken</param>
        public List<CategoryInfo> processTopic(XmlNode topicNode, XmlNode outerNode, LoaderOptions path, List<XmlNode> additionalRules)
        {
            // find the name of the topic or set to default "*"
            var prev = additionalRules;
            if (prev != null)
            {
                additionalRules = new List<XmlNode>();
                additionalRules.AddRange(prev);
            }

            Unifiable topicName = RTPBot.GetAttribValue(topicNode, "name,topic", Unifiable.STAR);
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
                    additionalRules.Add(cateNode);
                    continue;
                }
                AddErrorCategory("UNKNOWN NODETYPE ", cateNode);
                processCategoryWithTopic(cateNode, topicName, topicNode, path, additionalRules);
            }
            //additionalRules = prev;
            return path.CategoryInfos;
        }

        /// <summary>
        /// Given a "topic" topicNode, processes all the categories for the topic and adds them to the 
        /// graphmaster "brain"
        /// </summary>
        /// <param name="topicNode">the "topic" node</param>
        /// <param name="path">the file from which this topicNode is taken</param>
        public List<CategoryInfo> processOuterThat(XmlNode thatNode, XmlNode outerNode, LoaderOptions path, List<XmlNode> additionalRules)
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
                loadAIMLNode0(cateNode, path, path.TheRequest, additionalRules);
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
        public List<CategoryInfo> processCategory(XmlNode cateNode, XmlNode outerNode, LoaderOptions path, List<XmlNode> additionalRules)
        {
            return processCategoryWithTopic(cateNode, Unifiable.STAR, outerNode, path, additionalRules);
        }

        /// <summary>
        /// Adds a category to the graphmaster structure using the given topic
        /// </summary>
        /// <param name="cateNode">the XML node containing the category</param>
        /// <param name="topicName">the topic to be used</param>
        /// <param name="loadOpts">the file from which this category was taken</param>
        private List<CategoryInfo> processCategoryWithTopic(XmlNode cateNode, Unifiable topicName, XmlNode outerNode, LoaderOptions loadOpts, List<XmlNode> additionalRules)
        {
            List<CategoryInfo> CIs = loadOpts.CategoryInfos;
            // reference and check the required nodes
            List<XmlNode> patterns = FindNodes("pattern", cateNode);
            List<XmlNode> templates = FindNodes("template", cateNode);
            string errors = "";
            if (templates.Count == 0)
            {
                XmlNode TemplateOverwrite = TheTemplateOverwrite;
                if (TemplateOverwrite!=null)
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
            foreach (XmlNode pattern in patterns)
            {
                foreach (XmlNode template in templates)
                {
                    try
                    {
                        var v = addCatNode(cateNode, pattern, loadOpts, template, topicName, outerNode, additionalRules);
                        if (v == null)
                        {
                            AddErrorCategory(errors, cateNode);
                            writeToLog("WARN: MISSING CATE: " + cateNode);
                            continue;
                        }
                        CIs.Add(v);
                    }
                    catch (Exception e2)
                    {
                        String s = "ERROR: processCategoryWithTopic '" + e2 + "' " + loadOpts;
                        writeToLog(s);
                    }
                }

            }
            return CIs;
        }

        static public readonly XmlNode  TheTemplateOverwrite = AIMLTagHandler.getNode("<template></template>");
        static Dictionary<XmlNode, StringBuilder> ErrorList = new Dictionary<XmlNode, StringBuilder>();
        public Dictionary<XmlNode, StringBuilder> DumpErrors(OutputDelegate action, bool clr)
        {
            lock (ErrorList)
            {
                var el = ErrorList;
                int ct = el.Count;
                if (clr) ErrorList = new Dictionary<XmlNode, StringBuilder>();
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
            writeToLog("XMLERROR: " + errors + " \n in " + node);
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

        private CategoryInfo addCatNode(XmlNode cateNode, XmlNode patternNode, LoaderOptions loaderOpts, XmlNode templateNode,
            Unifiable topicName, XmlNode outerNode, List<XmlNode> additionalRules)
        {

            var prev = additionalRules ?? loaderOpts.AdditionalPreconditions;
            additionalRules = new List<XmlNode>();
            if (prev != null) additionalRules.AddRange(prev);

            XmlNode guardnode = FindNode("guard", cateNode, null);
            if (guardnode == null && outerNode != null && outerNode.Name != "aiml")
            {
                guardnode = FindNode("guard", outerNode, null);
            }
            GuardInfo guard = guardnode == null ? null : GuardInfo.GetGuardInfo(guardnode);
            string errors = "";
            XmlNode TemplateOverwrite = TheTemplateOverwrite;
            if (ReferenceEquals(null, templateNode))
            {
                if (TemplateOverwrite != null)
                {
                    writeToLog("USING DELETION TEMPALTE " + cateNode);
                    templateNode = TemplateOverwrite;
                } else
                {
                    errors += " Missing pattern tag ";
                }
            }
            if (ReferenceEquals(null, patternNode))
            {
                errors += " Missing pattern tag ";
            }

            XmlNode newPattern;
            Unifiable patternText;
            Func<XmlNode,string> Render = RTPBot.RenderInner;
            XmlNode extractThat1 = extractThat(patternNode, "that", cateNode, out patternText, out newPattern);
            string that;
            string ssss = RTPBot.GetAttribValue(extractThat1, "index", null);
            XmlNode extra = extractThat1;
            if (ssss == null || ssss == "1" || ssss == "1,1")
            {
                that = Render(extractThat1);
            }
            else
            {
                that = "*";
                additionalRules.Add(extractThat1);
            }

            patternNode = newPattern;
            Unifiable cond = Render(extractThat(patternNode, "flag", cateNode, out patternText, out newPattern));
            patternNode = newPattern;
            XmlNode topicTagText = extractThat(patternNode, "topic", cateNode, out patternText, out newPattern);
            patternNode = newPattern;

            if (!string.IsNullOrEmpty(errors))
            {
                AddErrorCategory(errors, cateNode);
                return null;
            }

            if (loaderOpts.DebugFiles && !ContansNoInfo(topicTagText.InnerXml))
            {
                var s = RTPBot.GetAttribValue(topicTagText, "name", Unifiable.STAR);
                if (topicName != s)
                {
                    errors = "TOPIC ERROR " + topicTagText.InnerXml + " topicName=" + topicName + " " + s;
                    AddErrorCategory(errors, cateNode);
                    return null;
                }
            }

            Unifiable categoryPath = generateCPath(patternText, that, cond, topicName, false);
            PatternInfo patternInfo = PatternInfo.GetPattern(loaderOpts, patternNode, categoryPath);
            TopicInfo topicInfo = TopicInfo.FindTopic(loaderOpts, topicName);
            ThatInfo thatInfo = ThatInfo.GetPattern(loaderOpts, that);

            // o.k., add the processed AIML to the GraphMaster structure
            if (!categoryPath.IsEmpty)
            {
                GraphMaster pathCtxGraph = loaderOpts.CtxGraph;
                lock (pathCtxGraph.LockerObject)
                {
                    try
                    {
                        CategoryInfo categoryInfo = CategoryInfo.GetCategoryInfo(patternInfo, cateNode, loaderOpts);
                        categoryInfo.SetCategoryTag(categoryPath, patternInfo, categoryInfo,
                                                    outerNode, templateNode, guard, thatInfo);

                        pathCtxGraph.addCategoryTag(categoryPath, patternInfo, categoryInfo,
                                                    outerNode, templateNode, guard, thatInfo ,additionalRules);
                        foreach (var node in additionalRules)
                        {
                            categoryInfo.AddPrecondition(node);
                        }
                        return categoryInfo;
                    }
                    catch (Exception e)
                    {
                        AddErrorCategory("ERROR! Failed to load a new category into the graphmaster where the path = " +
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

        public override string ToString()
        {
            return "[AIMLLoader req='" + LoaderRequest00 + "' bot='" + LoaderRequest00.TargetBot.BotID + "']";
            return base.ToString();
        }

        public void writeToLog(string message, params object[] args)
        {
            string prefix = ToString();
            prefix = DLRConsole.SafeFormat("LOADERTRACE: " + message + " while " + prefix, args);
            
            try
            {
                this.LoaderRequest00.writeToLog(prefix);

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
                LineInfoElementImpl.SetParentFromNode(newLineInfoPattern, patternNode);
                LineInfoElementImpl.SetReadOnly(newLineInfoPattern);
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
                LineInfoElementImpl.SetReadOnly(ps);

                return ps;
            }
        }
        public bool ThatWideStar = false;
        public static bool useInexactMatching = false;
        private OutputDelegate userTraceRedir;

        /// <summary>
        /// Given a name will try to find a node named "name" in the childnodes or return null
        /// </summary>
        /// <param name="name">The name of the node</param>
        /// <param name="node">The node whose children need searching</param>
        /// <returns>The node (or null)</returns>
        static public XmlNode FindNode(string name, XmlNode node, XmlNode ifMissing)
        {
            name = name.ToLower();
            foreach (var n in NamesStrings(name))
            {
                foreach (XmlNode child in node.ChildNodes)
                {
                    if (AIMLLoader.NameMatches(child, n))
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
            foreach (var n in NamesStrings(name))
            {
                foreach (XmlNode child in node.ChildNodes)
                {
                    if (AIMLLoader.NameMatches(child, n))
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
            foreach (var n in NamesStrings(name))
            {
                if (AIMLLoader.NameMatches(node, n))
                {
                    return node;
                }
            }
            return FindHigher(name, node.ParentNode, ifMissing);
        }

        private static string[] NamesStrings(string name)
        {
            return name.Split(",".ToCharArray(), StringSplitOptions.RemoveEmptyEntries);
        }

        private static bool NameMatches(XmlNode node, string s)
        {
            return node.Name.ToLower() == s || node.LocalName.ToLower() == s;
        }

        static public List<XmlNode> FindNodes(string name, XmlNode node)
        {
            name = name.ToLower();
            if (name.Contains(","))
                throw new NotImplementedException("Commans in FindNodes " + name + " in " + node.OuterXml);
            List<XmlNode> nodes = new List<XmlNode>();
            foreach (XmlNode child in node.ChildNodes)
            {
                if (AIMLLoader.NameMatches(child, name))
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
        private Unifiable generateCPath(Unifiable pattern, Unifiable that, Unifiable flag, Unifiable topicName, bool isUserInput)
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
                || patString.Contains("<") || patString.Contains(">") || patString.Contains("\\") || patString.Contains("/")
                || patString.Contains("\"") || patString.Contains("=") || patString.Contains("#$")
                || patString.Contains("~") || patString.Contains("*"))
            {
                UseRawUserInput = true;
                isUserInput = false;
            }

            bool thatContainedAnd = that.ToUpper().Contains(" AND ");
            if ((RProcessor.TrustAIML) & (!isUserInput || UseRawUserInput))
            {

                normalizedPattern = pattern.Trim();
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

                if (thatContainedAnd)
                {
                    if (!normalizedThat.ToUpper().Contains(" AND "))
                    {
                        writeToLog("ERROR in that: " + that + " -> " + normalizedThat);
                        normalizedThat = CleanPunct(that);
                    }
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
                normalizedPath.Append(Unifiable.TopicTag);
                if (addTagStart != null) normalizedPath.Append(addTagStart);
                normalizedPath.Append(normalizedTopic);
                if (addTagEnd != null) normalizedPath.Append(addTagEnd);

                normalizedPath.Append(Unifiable.FlagTag);
                normalizedPath.Append(flag);

                return normalizedPath; //.Frozen();
            }
            else
            {
                return Unifiable.Empty;
            }
        }

        public static string CleanPunct(string normalizedPattern)
        {
            if (normalizedPattern.EndsWith("?") || normalizedPattern.EndsWith(".") || normalizedPattern.EndsWith("!"))
            {
                normalizedPattern = normalizedPattern.Substring(0, normalizedPattern.Length - 1).Trim();
            }
            return normalizedPattern;
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
            if (isUserInput) input = CleanWhitepaces(input);
            if (Unifiable.IsNullOrEmpty(input)) return Unifiable.Empty;
            input = input.TrimEnd("?!. \n\r\t".ToCharArray());
            if (Unifiable.IsNullOrEmpty(input))
            {
                //return input0;
                return Unifiable.Empty;
            }
            Unifiable result = Unifiable.CreateAppendable();

            // objects for normalization of the input
            var RProcessor = LoaderRequest00.TargetBot;
            Normalize.ApplySubstitutions substitutor = new RTParser.Normalize.ApplySubstitutions(RProcessor);
            Normalize.StripIllegalCharacters stripper = new RTParser.Normalize.StripIllegalCharacters(RProcessor);

            Unifiable substitutedInput = substitutor.Transform(" " + input + " ").Trim();
            bool cand = input.ToUpper().Contains(" AND ");
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
                    string wwword = word.AsString().Trim(",. \"?".ToCharArray());
                    if (wwword.Length==0) continue;
                    normalizedWord = stripper.Transform(word);
                    if (normalizedWord != wwword)
                    {
                        writeToLog("Normalize stripper " + word + "->" + normalizedWord);
                    }
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

        private int NonAlphaCount(string input)
        {
            input = CleanWhitepaces(input);
            int na = 0;
            foreach (var s in input)
            {
                if (char.IsLetterOrDigit(s)) continue;
                na++;
            }
            return na;
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
                text, "*",
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
            const long maxCleanSize = 2 << 14;
            int inlen = xml2.Length;
            if (inlen > maxCleanSize)
            {
                return xml2;
            }

            bool padWildCards = true;

            padWildCards = xml2.IndexOfAny("\\:/".ToCharArray(), 0) == -1;

            if (!padWildCards) padchars = null;

            var s = new StringBuilder(inlen);

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
                    s.Append(' ');
                    pendingWhitespace = false;
                }
                s.Append(c0);
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
                    return s.ToString();
                }
                return xml2;
            }
            //s = s.Replace("<star index=\"1\"", "<star");

            return s.ToString();
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

        public static void PrintResult(Result result, OutputDelegate console, PrintOptions printOptions)
        {
            console("-----------------------------------------------------------------");
            console("Result: " + result.Graph + " Request: " + result.request);
            foreach (var s in result.InputSentences)
            {
                console("input: \"" + s + "\"");
            }
            PrintTemplates(result.UsedTemplates, console, printOptions);
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

        public static string GetTemplateSource(IEnumerable CI, PrintOptions printOptions)
        {
            if (CI == null) return "";
            var fs = new StringWriter();
            GraphMaster.PrintToWriter(CI, printOptions, fs, null);
            return fs.ToString();
        }

        public static void PrintTemplates(IEnumerable CI, OutputDelegate console, PrintOptions printOptions)
        {
            GraphMaster.PrintToWriter(CI, printOptions, new OutputDelegateWriter(console), null);
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

            LineInfoElement xmlNode = (LineInfoElement) (oc as IXmlLineInfo);
            if (xmlNode == null)
            {
                xmlNode = (LineInfoElement) AIMLTagHandler.getNode(node.OuterXml, node);
                LineInfoElementImpl.unsetReadonly(xmlNode);
            }
            else
            {
                LineInfoElementImpl.unsetReadonly(xmlNode);
            }
            LineInfoElementImpl.unsetReadonly(xmlNode);
            return xmlNode;
        }

        public static LineInfoElement CopyNode(string newName, XmlNode node, bool copyParent)
        {
            var od = node.OwnerDocument;
            LineInfoElement newnode = (LineInfoElement)node.OwnerDocument.CreateNode(node.NodeType, newName, node.NamespaceURI);
            newnode.ReadOnly = false;
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
            newnode.ReadOnly = node.IsReadOnly;
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

        public static bool IsHtmlTag(string name)
        {
            return " html head body font pre p div br dd td th tr table frame frameset &ltl &gt; input option select  " // etc
                .Contains(" " + name.ToLower()+ " ");
        }
    }
}
