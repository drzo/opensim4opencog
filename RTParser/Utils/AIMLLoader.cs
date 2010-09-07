using System;
using System.Collections;
using System.Collections.Generic;
using System.IO;
using System.Net;
using System.Text;
using System.Threading;
using System.Xml;
using MushDLR223.ScriptEngines;
using MushDLR223.Utilities;
using MushDLR223.Virtualization;
using RTParser.Normalize;
using UPath = RTParser.Unifiable;
using LineInfoElement = MushDLR223.Utilities.LineInfoElementImpl;

namespace RTParser.Utils
{
    /// <summary>
    /// A utility class for loading AIML files from disk into the graphmaster structure that 
    /// forms an AIML RProcessor's "brain"
    /// </summary>
    public class AIMLLoader : XmlNodeEvaluatorImpl
    {
        #region Attributes

        public Request LoaderRequest00;

        /// <summary>
        /// Allow all chars in RawUserInput
        /// </summary>
        public bool RawUserInput;

        /// <summary>
        /// The RProcessor whose brain is being processed
        /// </summary>
        public RTPBot RProcessorOld
        {
            get { return LoaderRequest00.TargetBot; }
        }

        #endregion

        public AIMLLoader(RTPBot bot)
        {
        }

        /// <summary>
        /// Ctor
        /// </summary>
        /// <param name="bot">The bot whose brain is being processed</param>
        public AIMLLoader(RTPBot bot, Request request)
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

        private string ResolveToURI(string pathIn, LoaderOptions loadOpts)
        {
            string path = ResolveToURI0(pathIn, loadOpts);
            if (path!=pathIn)
            {
                writeToLog("ResolveToURI '{0}'->'{1}'", pathIn, path);
            }
            return path;
        }
        private static string ResolveToURI0(string pathIn, LoaderOptions loadOpts)
        {
            string baseFile = GetBaseDirectory(loadOpts);
            var inPath = HostSystem.ToRelativePath(pathIn, baseFile);
            IEnumerable<string> combine;
            //combine = baseFile != null ? new[] { "./", baseFile, "aiml/" } : new[] { "./", "aiml/" };
            string prefix;
            combine = loadOpts.TheRequest.TargetBot.RuntimeDirectories;
            string pathFull = HostSystem.ResolveToURI(pathIn, combine, out prefix);
            string relPath = HostSystem.ToRelativePath(pathFull, prefix);
            string rpath = HostSystem.Combine(prefix, relPath);
            if (HostSystem.FileOrDirExists(rpath))
            {
                return rpath;
            }
            if (!HostSystem.FileOrDirExists(relPath))
            {
                RTPBot.writeDebugLine("WARNING PATH NOT EXIST ERROR: " + relPath);
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
            if (File.Exists(baseFile)) return HostSystem.ToCanonicalDirectory(new FileInfo(baseFile).DirectoryName);
            if (Directory.Exists(baseFile)) return HostSystem.ToCanonicalDirectory(baseFile);
            string bd = HostSystem.ToCanonicalDirectory(HostSystem.GetBaseDir(baseFile));
            return bd;
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
                    if (NoRuntimeErrors) return default(R);
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

        public int loadAIMLURI0(string path0, LoaderOptions loadOpts)
        {
            RTPBot RProcessor = loadOpts.RProcessor;
            string path = path0;
            loadOpts.Loading0 = path;
            string pathIn = path;            
            RProcessor.ReloadHooks.Add(() => loadAIMLURI0(pathIn, loadOpts));
            path = ResolveToURI(pathIn, loadOpts);
            string fullPath = HostSystem.GetAbsolutePath(pathIn);
            if (!HostSystem.FileOrDirExists(path)) path = fullPath;
            int total = 0;
            try
            {
                if (HostSystem.DirExists(path))
                {
                    path = HostSystem.ToCanonicalDirectory(path);
                    Request request = loadOpts.TheRequest;
                    LoaderOptions savedOpt = request.LoadOptions;
                    try
                    {
                        request.LoadOptions = loadOpts;
                        request.Filename = path;
                        loadOpts = request.LoadOptions;
                        //RTPBot.loadConfigs(RProcessor, path, request);
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
                        total += loadAIMLFile0(currentPath, loadOpts, true);
                    }
                    finally
                    {
                        request.LoadOptions = savedOpt;
                        RProcessor.PopSearchPath(pop);
                    }
                    return total;
                }
                else if (Uri.IsWellFormedUriString(path, UriKind.RelativeOrAbsolute))
                {
                    writeToLog("Processing AIML URI: " + path);
                    Uri uri = new Uri(path);
                    if (uri.IsFile)
                    {
                        total += loadAIMLURI0(uri.AbsolutePath, loadOpts);
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
                                    total += loadAIMLFile0(pathname, loadOpts, false);
                                }
                                finally
                                {
                                    request.LoadOptions = savedOpt;
                                }
                            }
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
                AutoClosingStream tr = HostSystem.OpenRead(path);
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
                var byteArray = Encoding.ASCII.GetBytes(docString);
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

            XmlReader xtr = XmlDocumentLineInfo.CreateXmlTextReader(input0);
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
            fn = fn.Replace("\\", "/").ToLower();
            path = path.Replace("\\", "/").ToLower();
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
            AIMLLoader prev = RProcessor.Loader;
            try
            {
                RProcessor.Loader = this;
                // Get a list of the nodes that are children of the <aiml> tag
                // these nodes should only be either <topic> or <category>
                // the <topic> nodes will contain more <category> nodes
                string currentNodeName = currentNode.Name.ToLower();

                ThreadStart ts = EnterTag(request, currentNode, query);
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


        private int loadAIMLNodes(IEnumerable nodes, LoaderOptions loadOpts, Request request,
                                  List<XmlNode> additionalRules)
        {
            int total = 0;
            if (nodes != null)
            {
                foreach (object node in nodes)
                {
                    total += loadAIMLNode((XmlNode) node, loadOpts, request);
                }
            }
            return total;
        }

        public int loadAIMLNode(XmlNode currentNode, LoaderOptions loadOpts, Request request)
        {
            var additionalRules = loadOpts.AdditionalPreconditions;
            return LoaderOper(() => loadAIMLNode0(currentNode, loadOpts, request, additionalRules),
                              loadOpts.CtxGraph);
        }

        public int loadAIMLNode0(XmlNode currentNode, LoaderOptions loadOpts, Request request,
                                 List<XmlNode> additionalRules)
        {
            int total = 0;
            RTPBot RProcessor = loadOpts.RProcessor;
            AIMLLoader prev = RProcessor.Loader;
            try
            {
                RProcessor.Loader = this;
                // Get a list of the nodes that are children of the <aiml> tag
                // these nodes should only be either <topic> or <category>
                // the <topic> nodes will contain more <category> nodes
                string currentNodeName = currentNode.Name.ToLower();
                if (currentNodeName == "aiml")
                {
                    total += InsideLoaderContext(currentNode, request, request.CurrentQuery,
                                                 () =>
                                                 loadAIMLNodes(currentNode.ChildNodes, loadOpts, request,
                                                               additionalRules));
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

        public virtual IEnumerable<XmlNode> Eval_Element_NodeType(XmlNode src, Request request,
                                                                  OutputDelegate outputdelegate)
        {
            //XmlNode node = 
            loadAIMLNode(src, request.LoadOptions, request);
            //if (node == null) return NO_XmlNode;
            return new[] {src};
        }


        /// <summary>
        /// Given a "topic" topicNode, processes all the categories for the topic and adds them to the 
        /// graphmaster "brain"
        /// </summary>
        /// <param name="topicNode">the "topic" node</param>
        /// <param name="path">the file from which this topicNode is taken</param>
        public List<CategoryInfo> processTopic(XmlNode topicNode, XmlNode outerNode, LoaderOptions path,
                                               List<XmlNode> additionalRules)
        {
            // find the name of the topic or set to default "*"
            var prev = additionalRules;
            if (prev != null)
            {
                additionalRules = new List<XmlNode>();
                additionalRules.AddRange(prev);
            }

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
        public List<CategoryInfo> processOuterThat(XmlNode thatNode, XmlNode outerNode, LoaderOptions path,
                                                   List<XmlNode> additionalRules)
        {
            var CIS = path.CategoryInfos;
            var newCats = path.CategoryInfos = new List<CategoryInfo>();
            // find the name of the topic or set to default "*"
            Unifiable thatPattten = GetAttribValue(thatNode, "pattern,value,name", Unifiable.STAR);
            // process all the category nodes
            ThatInfo newThatInfo = new ThatInfo(thatNode, thatPattten);
            foreach (XmlNode cateNode in thatNode.ChildNodes)
            {
                // getting stacked up inside
                loadAIMLNode0(cateNode, path, path.TheRequest, additionalRules);
            }
            foreach (CategoryInfo ci0 in newCats)
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
        public List<CategoryInfo> processCategory(XmlNode cateNode, XmlNode outerNode, LoaderOptions path,
                                                  List<XmlNode> additionalRules)
        {
            return processCategoryWithTopic(cateNode, Unifiable.STAR, outerNode, path, additionalRules);
        }

        /// <summary>
        /// Adds a category to the graphmaster structure using the given topic
        /// </summary>
        /// <param name="cateNode">the XML node containing the category</param>
        /// <param name="topicName">the topic to be used</param>
        /// <param name="loadOpts">the file from which this category was taken</param>
        private List<CategoryInfo> processCategoryWithTopic(XmlNode cateNode, Unifiable topicName, XmlNode outerNode,
                                                            LoaderOptions loadOpts, List<XmlNode> additionalRules)
        {
            var CIs = loadOpts.CategoryInfos;
            // reference and check the required nodes
            var patterns = FindNodes("pattern", cateNode);
            var templates = FindNodes("template", cateNode);
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
            foreach (XmlNode pattern in patterns)
            {
                foreach (XmlNode template in templates)
                {
                    try
                    {
                        CategoryInfo v = addCatNode(cateNode, pattern, loadOpts, template, topicName, outerNode,
                                                    additionalRules);
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

        public Dictionary<XmlNode, StringBuilder> DumpErrors(OutputDelegate action, bool clr)
        {
            lock (ErrorList)
            {
                var el = ErrorList;
                int ct = el.Count;
                if (clr) ErrorList = new Dictionary<XmlNode, StringBuilder>();
                if (action != null)
                {
                    foreach (KeyValuePair<XmlNode, StringBuilder> kv in el)
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

        private CategoryInfo addCatNode(XmlNode cateNode, XmlNode patternNode, LoaderOptions loaderOpts,
                                        XmlNode templateNode,
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
                }
                else
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
            Func<XmlNode, string> Render = nodeI => VisibleRendering(nodeI, PatternSideRendering);
            XmlNode extractThat1 = extractThat(patternNode, "that", cateNode, out patternText, out newPattern);
            string that;
            string ssss = GetAttribValue(extractThat1, "index", null);
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
                string s = GetAttribValue(topicTagText, "name", Unifiable.STAR);
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
                                                    outerNode, templateNode, guard, thatInfo, additionalRules);
                        foreach (XmlNode node in additionalRules)
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
            catch
            {
            }
        }


        /// <summary>
        /// Generates a path from a category XML cateNode and topic name
        /// </summary>
        /// <param name="cateNode">the category XML node</param>
        /// <param name="topicName">the topic</param>
        /// <param name="isUserInput">marks the path to be created as originating from user input - so
        /// normalize out the * and _ wildcards used by AIML</param>
        /// <returns>The appropriately processed path</returns>
        private static XmlNode extractThat(XmlNode patternNode, String tagname, XmlNode cateNode,
                                           out Unifiable patternText, out XmlNode newPattern)
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
                XmlNode newLineInfoPattern = getNode("<pattern>" + patternString + "</pattern>", patternNode);
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
            return that ?? PatternStar; // hatText;//this.generatePath(patternText, thatText, topicName, isUserInput);
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
        public Unifiable generatePath(Unifiable pattern, Unifiable that, Unifiable flag, Unifiable topicName,
                                      bool isUserInput)
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
        private Unifiable generateCPath(Unifiable pattern, Unifiable that, Unifiable flag, Unifiable topicName,
                                        bool isUserInput)
        {
            RTPBot RProcessor = LoaderRequest00.TargetBot;

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
            RTPBot RProcessor = LoaderRequest00.TargetBot;
            ApplySubstitutions substitutor = new ApplySubstitutions(RProcessor);
            StripIllegalCharacters stripper = new StripIllegalCharacters(RProcessor);

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
            var substitutedWords = substitutedInput.AsString().Split(' ');

            // Normalize all words unless they're the AIML wildcards "*" and "_" during AIML loading
            foreach (Unifiable word in substitutedWords)
            {
                Unifiable normalizedWord;
                if (isUserInput)
                {
                    string wwword = word.AsString().Trim(",. \"?!".ToCharArray());
                    if (wwword.Length == 0) continue;
                    normalizedWord = stripper.Transform(word);
                    if (normalizedWord != wwword)
                    {
                        if (!wwword.Contains("'"))
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

        #endregion
    }
}