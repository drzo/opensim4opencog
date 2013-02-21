#if false
using System;
using System.Collections;
using System.Collections.Generic;
using System.IO;
using System.Net;
using System.Text;
using System.Threading;
using System.Xml;
using AltAIMLbot.Normalize;
using AltAIMLbot.Utils;
using AltAIMLParser;
using AltAIMLbot.Variables;
using MushDLR223.ScriptEngines;
using MushDLR223.Utilities;
using MushDLR223.Virtualization;
using AltAIMLbot.AIMLTagHandlers;
using UPath = AltAIMLbot.Unifiable;
using LineInfoElement = MushDLR223.Utilities.LineInfoElementImpl;
//using CategoryInfo = RTParser.Utils.TemplateInfo;
using PatternInfo = AltAIMLbot.Unifiable;
using ThatInfo = AltAIMLbot.Unifiable;
using TopicInfo = AltAIMLbot.Unifiable;
using GuardInfo = AltAIMLbot.Unifiable;
using ResponseInfo = AltAIMLbot.Unifiable;

namespace AltAIMLbot.Utils
{
    /// <summary>
    /// A utility class for loading AIML files from disk into the graphmaster structure that 
    /// forms an AIML RProcessor's "brain"
    /// </summary>
    public partial class AIMLLoader : XmlNodeEvaluatorImpl
    {
        #region Attributes

        public Request LoaderRequest00;

        /// <summary>
        /// Allow all chars in RawUserInput
        /// </summary>
        public bool RawUserInput;

        /// <summary>
        /// Deletes any content that contains these words in it
        /// TODO make this done in a  text file
        /// </summary>
        private static HashSet<string> GlobalFilteredWords = new HashSet<string>()
                                                                 {
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
                                                                     "iraq",
                                                                     "terror",
                                                                 };

        public HashSet<string> FilteredWords = new HashSet<string>(GlobalFilteredWords);

        /// <summary>
        /// The RProcessor whose brain is being processed
        /// </summary>
        public AltBot RProcessorOld
        {
            get { return LoaderRequest00.TargetBot; }
        }

        #endregion

        public AIMLLoader SLoader = null;
        /*
        public AIMLLoaderU(AltBot bot)
        {
            SLoader = new AIMLLoader(bot);
        }
        */
        /// <summary>
        /// Ctor
        /// </summary>
        /// <param name="bot">The bot whose brain is being processed</param>
        /*public AIMLLoaderU_unused(AltBot bot, Request request)
        {
            this.LoaderRequest00 = request;
            SLoader = new AIMLLoader(bot, request);
            SLoader.loadOpts = request.LoadOptions;
            //XmlNodeEvaluators.Add(this);
        }
        */
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
        public long loadAIMLDir(string path, LoaderOptions loadOpts)
        {
            this.SLoader.bot.RaiseError("Wrong path in code");
            long total = 0;

            int before = loadOpts.CtxGraph.Size;
            long saved0 = Unifiable.LowMemExpireUnifiableCaches();
            writeToLog("Starting to process AIML files found in the directory (" + path + ")");
            try
            {
                total = loadAIMLDir0(path, loadOpts);
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
        public long loadAIMLDir0(string path, LoaderOptions loadOpts)
        {
            long total = 0;
            AltBot RProcessor = loadOpts.RProcessor;
            path = ResolveToURI(path, loadOpts);

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
                            total += loadAIMLFile(f, loadOpts, false);
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
                    total += loadAIMLDir(path + Path.DirectorySeparatorChar + f, loadOpts);
                }
            }
            return total;
        }

        private string ResolveToURI(string pathIn, LoaderOptions loadOpts)
        {
            this.SLoader.bot.RaiseError("Wrong path in code");
            string path = ResolveToURI0(pathIn, loadOpts);
            if (path!=pathIn)
            {
                writeToLog("ResolveToURI '{0}'->'{1}'", pathIn, path);
            }
            return path;
        }
        private static string ResolveToURI0(string pathIn, LoaderOptions loadOpts)
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

        private R LoaderOper<R>(Func<R> action, GraphMaster gm, LoaderOptions loadOpts)
        {
            this.SLoader.bot.RaiseError("Wrong path in code");
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
            s = SafeFormat("USERTRACE: " + s, objects);
            if (ToUpper(s).Contains("ERROR"))
            {
                writeToLog(s, objects);
            }
        }

        public long loadAIMLURI(string path, LoaderOptions loadOpts)
        {
            long total = LoaderOper(() => loadAIMLURI0(path, loadOpts), loadOpts.CtxGraph, loadOpts);
            TotalCheck(path, total, loadOpts);
            return total;
        }

        private void TotalCheck(string path, long total, LoaderOptions loadOpts)
        {
        }

        public long loadAIMLURI0(string path0, LoaderOptions loadOpts)
        {
            AltBot RProcessor = loadOpts.RProcessor;
            string path = path0;
            loadOpts.Loading0 = path;
            string pathIn = path;
            path = ResolveToURI(pathIn, loadOpts);
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
                        total += loadAIMLDir(path, loadOpts);
                        TotalCheck(path, total, loadOpts);
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
                        total += loadAIMLFile(currentPath, loadOpts, true);
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
                        total += loadAIMLURI0(uri.AbsolutePath, loadOpts);
                        TotalCheck(uri.AbsolutePath, total, loadOpts);
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
                        TotalCheck(uri.AbsolutePath, total, loadOpts);
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
                                    total += loadAIMLFile(pathname, loadOpts, false);
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
        /// Given the name of a file in the AIML path directory, attempts to load it into the 
        /// graphmaster
        /// </summary>
        /// <param name="path">The name of the file to process</param>
        public long loadAIMLFile(string path, LoaderOptions loadOpts, bool forceReload)
        {
            long total = loadAIMLFile0(path, loadOpts, forceReload);
            TotalCheck(path, total, loadOpts);
            return total;
        }
        public long loadAIMLFile0(string path, LoaderOptions loadOpts, bool forceReload)
        {
            long total = 0;
            path = ResolveToURI(path, loadOpts);
            AltBot RProcessor = loadOpts.RProcessor;
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
                    TotalCheck(path, total, loadOpts);
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
                master.AddFileLoaded(path);
                if (RProcessor.useServitor)
                {
                    RProcessor.servitor.loadAIMLFromFile(path);
                }
                else
                {
                    if (!RProcessor.useNonServitor) return 1;
                    AutoClosingStream tr = HostSystem.OpenRead(path);
                    try
                    {
                        string pfile = request.Filename;
                        try
                        {
                            request.Filename = path;
                            loadOpts = request.LoadOptions;
                            total += this.loadAIMLStream(tr, loadOpts);
                            TotalCheck(pfile, total, loadOpts);
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
        public void loadAIMLString(string docString, LoaderOptions loadOpts)
        {
            AltBot RProcessor = loadOpts.RProcessor;
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
        /// Given an XML document containing valid AIML, attempts to load it into the graphmaster
        /// </summary>
        /// <param name="doc">The XML document containing the AIML</param>
        /// <param name="loadOpts">Where the XML document originated</param>
        public long loadAIMLStream(Stream input0, LoaderOptions loadOpts)
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
                        total += this.loadAIMLNode(doc.DocumentElement, loadOpts, request);
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
        public void loadAIMLStreamFallback(Stream input0, LoaderOptions loadOpts)
        {
            AltBot RProcessor = loadOpts.RProcessor;
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
                XmlDocumentLineInfo doc = new XmlDocumentLineInfo("" + namefile, false);
                try
                {
                    doc.LoadXml(ssss);
                    if (doc.DocumentElement == null)
                    {
                        RProcessor.writeToLog("ERROR: No Document at " + namefile);
                        //        continue;
                    }
                    this.loadAIMLNode(doc.DocumentElement, loadOpts, request);
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
                // writeToLog("WARNING! Ensuring loadOpts.Filename='{0}' but path='{1}'", fn, path);
            }
            if (!request1.LoadOptions.Equals(loadOpts))
            {
                /// writeToLog("ERROR! Ensuring loadOpts.Filename='{0}' but path='{1}'", fn, path);
            }
            return loadOpts;
        }

        public long InsideLoaderContext(XmlNode currentNode, Request request, SubQuery query,
            LoaderOptions loadOpts, Func<long> doit)
        {
            long total = 0;
            query = query ?? request.CurrentQuery;
            //Result result = query.Result;
            AltBot RProcessor = request.TargetBot;
            var prev = RProcessor.Loader;
            GraphMaster loadOptsPrevGraph = loadOpts.CtxGraph;
            try
            {
                RProcessor.Loader = this.SLoader;
                // Get a list of the nodes that are children of the <aiml> tag
                // these nodes should only be either <topic> or <category>
                // the <topic> nodes will contain more <category> nodes
                //string currentNodeName = currentNode.Name.ToLower();

                ThreadStart ts = StaticAIMLUtils.EnterTag(request, currentNode, query);                 
                try
                {
                    loadOpts.CtxGraph = request.Graph;
                    long done = doit();
                    total += done;
                   // TotalCheck(currentNode.OuterXml, total, null);
                }
                finally
                {
                    ts();
                    loadOpts.CtxGraph = loadOptsPrevGraph;
                }
            }
            finally
            {
                RProcessor.Loader = prev;
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
                    XmlNode node = o as XmlNode;
                    total += loadAIMLNode((XmlNode) node, loadOpts, request);
                    //TotalCheck(TextAndSourceInfo(node), total, loadOpts);
                }
            }
            return total;
        }

        public long loadAIMLNode(XmlNode currentNode, LoaderOptions loadOpts, Request request)
        {
            var additionalRules = loadOpts.AdditionalPreconditions;
            long total = LoaderOper(() => loadAIMLNode0(currentNode, loadOpts, request, additionalRules),
                                    loadOpts.CtxGraph, loadOpts);
            //TotalCheck(TextAndSourceInfo(currentNode), total, loadOpts);
            return total;
        }

        public long loadAIMLNode0(XmlNode currentNode, LoaderOptions loadOpts, Request request,
                                 List<ConversationCondition> additionalRules)
        {
            if (request.bot.useServitor)
            {
                Console.WriteLine("Check: loadAIMLFromXML enter(3)");

                SLoader.loadAIMLFromXML(currentNode, loadOpts.CurrentFilename);
                return 1;
            }

            if (currentNode == null)
            {
                writeToLog("ERROR: no currentNode in " + loadOpts);
                return 0;
            }
            long total = 0;
            AltBot RProcessor = loadOpts.RProcessor;
            var prev = RProcessor.Loader;
            try
            {
                RProcessor.Loader = this.SLoader;
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
                    Unifiable topicName = GetAttribValue(currentNode, "name,topic", Unifiable.STAR);
                    ConversationCondition newConversationCondition = new ConversationCondition("topic", topicName,
                                                                                               currentNode);
                    additionalRules.Add(newConversationCondition);
                    // "getNode("<pretest name=\"topic\">" + topicName + "</pretest>", currentNode))
                    var vv = this.processTopic(currentNode, currentNode.ParentNode, loadOpts, additionalRules);
                    additionalRules.Remove(newConversationCondition);
                    total += vv.Count;
                    return total;
                }
                else if (currentNodeName == "category")
                {
                    var vv = this.processCategory(currentNode, currentNode.ParentNode, loadOpts, additionalRules);
                    total += vv.Count;
                    return total;
                }
                else if (currentNodeName == "that")
                {
                    additionalRules = PushAddtionalRuleContext(additionalRules);
                    string valueThat = GetAttribValue(currentNode, "value,that", currentNode.InnerXml);
                    ConversationCondition thatRule = new ConversationCondition("that", valueThat, currentNode);
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
                    loadOpts.RProcessor.ImmediateAiml(currentNode, request, this.SLoader,
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
                        Request res = request.CreateSubRequest(StaticAIMLUtils.ToTemplateXML(currentNode), null,
                                                               request.RequestType | RequestKind.TemplateExpander |
                                                               RequestKind.AIMLLoader);
                        res.IsToplevelRequest = true;
                        loadOpts.RProcessor.ImmediateAiml(currentNode, res, this.SLoader,
                                                               request.RequestType | RequestKind.TemplateExpander |
                                                               RequestKind.AIMLLoader);
                        total += 1;
                    }
                    else
                    {
                        if (request.NoImmediate) return 0;
                        loadOpts.RProcessor.ImmediateAiml(currentNode, request, this.SLoader,
                                                               request.RequestType | RequestKind.TemplateExpander |
                                                               RequestKind.AIMLLoader);
                        total += 1;
                    }
                }
            }
            finally
            {
                RProcessor.Loader = prev;
            }
            return total;
        }
        ISettingsDictionary IsSettingsTag(string tag, Request request)
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
            List<ConversationCondition> newList = new List<ConversationCondition>(); ;
            if (nodes!=null) newList.AddRange(nodes);
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
        /// Given a "topic" topicNode, processes all the categories for the topic and adds them to the 
        /// graphmaster "brain"
        /// </summary>
        /// <param name="topicNode">the "topic" node</param>
        /// <param name="path">the file from which this topicNode is taken</param>
        public List<CategoryInfo> processOuterThat(XmlNode thatNode, XmlNode outerNode, LoaderOptions path,
                                                   List<ConversationCondition> additionalRules)
        {
            var CIS = path.CategoryInfos;
            var newCats = path.CategoryInfos = new List<CategoryInfo>();
            // find the name of the topic or set to default "*"
            Unifiable thatPattten = GetAttribValue(thatNode, "pattern,value,name", Unifiable.STAR);
            // process all the category nodes
            Unifiable newThatInfo = path.CtxGraph.FindThat(thatNode, thatPattten);
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
                                                  List<ConversationCondition> additionalRules)
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
                                                            LoaderOptions loadOpts, List<ConversationCondition> additionalRules)
        {
            var CIs = loadOpts.CategoryInfos;
            LineInfoElementImpl.SetReadOnly(cateNode);
            // reference and check the required nodes
            var patterns = FindNodes("pattern", cateNode);
            var templates = FindNodes("template", cateNode);
            var rulesNodes = FindNodes("rule", cateNode);
            var topicNodesInCate = FindNodes("topic", cateNode);
            var thatNodes = FindNodes("that", cateNode);
           // var thatNodes = new List<XmlNode>();
            //FindNodes("that", cateNode, thatNodes, 2);
            string errors = "";
            if (templates.Count == 0)
            {
                XmlNode TemplateOverwrite = StaticAIMLUtils.TheTemplateOverwrite;
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
                foreach (var that in new List<XmlNode>(thatNodes))
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
            foreach (var xmlNode in thatNodes)
            {
                additionalRules = additionalRules ?? PushAddtionalRuleContext(additionalRules);
                additionalRules.Add(new ConversationCondition(xmlNode));
            }
            foreach (var xmlNode in rulesNodes)
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
                        var v = addCatNode(cateNode, pattern, that0, loadOpts, template, topicName,
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
            var ErrorList = StaticAIMLUtils.ErrorList;
            lock (ErrorList)
            {
                var el = ErrorList;
                int ct = el.Count;
                if (clr) StaticAIMLUtils.ErrorList = new Dictionary<XmlNode, StringBuilder>();
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
            if (string.IsNullOrEmpty(errors))
            {
                return;
                writeToLog("XMLERRORNODE: " + node + " " + LocationInfo(node));
            }
            writeToLog("XMLERROR: " + errors + " \n in " + node + " " + LocationInfo(node));
            var ErrorList = StaticAIMLUtils.ErrorList;
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

        private List<CategoryInfo> addCatNode(XmlNode cateNode, XmlNode patternNode, XmlNode thatNodeOrNull, LoaderOptions loaderOpts,
                                        XmlNode templateNode,
                                        Unifiable topicName, XmlNode outerNode, List<ConversationCondition> additionalRules, double score)
        {
            additionalRules = additionalRules ?? PushAddtionalRuleContext(loaderOpts.AdditionalPreconditions);
            // additionalRules = new List<XmlNode>();
            //if (prev != null) additionalRules.AddRange(prev);

            XmlNode guardnode = FindNode("guard", cateNode, null);
            if (guardnode == null && outerNode != null && outerNode.Name != "aiml")
            {
                if (loaderOpts.SearchForGuard) guardnode = FindNode("guard", outerNode, null);
            }
            Unifiable guard = guardnode == null ? null : loaderOpts.CtxGraph.GetGuardInfo(guardnode);
            string errors = "";
            XmlNode TemplateOverwrite = StaticAIMLUtils.TheTemplateOverwrite;
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

            var patternText = VisibleChildsRenderingOrStar(patternNode);
            if (!string.IsNullOrEmpty(errors))
            {
                AddErrorCategory(errors, cateNode);
                return null;
            }

            if (loaderOpts.DebugFiles && !StaticAIMLUtils.ContansNoInfo(topicTagText.InnerXml))
            {
                string s = GetAttribValue(topicTagText, "name", Unifiable.STAR);
                if (topicName != s)
                {
                    errors = "TOPIC ERROR " + topicTagText.InnerXml + " topicName=" + topicName + " " + s;
                    AddErrorCategory(errors, cateNode);
                    return null;
                }
            }

            Func<Unifiable, bool, Unifiable> normalizerT = (inputText, isUserInput) => Trim(NormalizeU(inputText, isUserInput));
            Unifiable categoryPath = generatePath(patternText, that, cond, topicName, false, normalizerT).ToUpper();
            Unifiable patternInfo = loaderOpts.CtxGraph.FindPattern(patternNode, patternText);//PatternInfo.GetPattern(loaderOpts, patternNode, categoryPath);
            Unifiable topicInfo = loaderOpts.CtxGraph.FindTopic(topicName);
            Unifiable thatInfo = loaderOpts.CtxGraph.FindThat(thatNodeOrNull, that);
            var templateNodeFindable = StaticAIMLUtils.TheTemplateOverwrite;

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
                GraphMaster pathCtxGraph = loaderOpts.CtxGraph;
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
                        var added = pathCtxGraph.addCategoryTag(categoryPath, patternInfo,
                                                           cateNode, templateNode, guard, topicInfo, thatInfo,
                                                           additionalRules,
                                                           out wouldBeRemoval, loaderOpts);
                        if (added != null)
                        {
                            if (added.Count != 1)
                            {

                            }
                            foreach (var cate in added)
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
                        AddErrorCategory("ERROR! " + e + " Failed to load a new category into the graphmaster where the path = " +
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
            return VisibleRendering(nodeI.ChildNodes, StaticAIMLUtils.PatternSideRendering);
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
                        var skip = templateNode.ParentNode ?? templateNode;
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
            var foundNode0 = extractPrecondNode(patternNode, tagName, cateNode, out patternText0, out newPattern);
            if (foundNode0 != null) foundNode = foundNode0;
            if (newPattern != null)
            {
                var newPatternOuterXml = newPattern.OuterXml;
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
            string pattern = TryGetPrecondionThat(tagName, xmlNode, out isPatternSide, out indexValue, out indexPosition1);
            if (isPatternSide)
            {
                if (!indexPosition1)
                {
                    // ReSharper disable ConditionIsAlwaysTrueOrFalse
                    ConversationCondition newConversationCondition
                        = new ConversationCondition(isPatternSide, tagName, pattern, indexValue, indexPosition1, xmlNode);
                    // ReSharper restore ConditionIsAlwaysTrueOrFalse
                    additionalRules.Add(newConversationCondition);
                }
            }

            if (isPatternSide || pattern != "*")
            {
                if (pattern == null || Trim(pattern).Length == 0)
                {
                    pattern = TryGetPrecondionThat(tagName, xmlNode, out isPatternSide, out indexValue, out indexPosition1);
                }
                if (xmlNode != null && isPatternSide)
                {
                    if (!indexPosition1)
                    {
                        ConversationCondition newPatternMatchRule1 =
                            new ConversationCondition(isPatternSide, tagName, pattern, indexValue, indexPosition1, xmlNode);
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

        public static string TryGetPrecondionThat(string tagName, XmlNode extractedXML, out bool isRequired, out string indexVal, out bool indexPosition1)
        {
            isRequired = extractedXML != null && extractedXML.LocalName == tagName;
            indexVal = GetAttribValue(extractedXML, "index", "1,1");
            indexPosition1 = indexVal == "1" || indexVal == "1,1" || indexVal == "1,*";
            if (!isRequired)
            {
                return null;
            }
            string patternTxt = VisibleRendering(extractedXML.ChildNodes, StaticAIMLUtils.PatternSideRendering);
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
                this.LoaderRequest00.TargetBot.writeToLog(prefix);
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
        /// Generates a path from a category XML cateNode and topic name
        /// </summary>
        /// <param name="cateNode">the category XML node</param>
        /// <param name="topicName">the topic</param>
        /// <param name="isUserInput">marks the path to be created as originating from user input - so
        /// normalize out the * and _ wildcards used by AIML</param>
        /// <returns>The appropriately processed path</returns>
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
                patternText = null;// patternNode.InnerXml;
                newPattern = null;// patternNode;
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
                    patternString = StaticAIMLUtils.MatchKeyClean(patternString.Replace(thatString, ""));
                }
                XmlNode newLineInfoPattern = getNodeAndSetSibling(true, "<pattern>" + patternString + "</pattern>", false,
                                                                 false, patternNode);
                //TODO BEFORE COMMIT DMILES
                LineInfoElementImpl.SetParentFromNode(newLineInfoPattern, patternNode);
                LineInfoElementImpl.SetReadOnly(newLineInfoPattern);
                patternNode = newLineInfoPattern;
                patternText = Unifiable.Create(Unifiable.InnerXmlText(patternNode));
            }
            newPattern = patternNode;
            return foundAnywhere;
        }

        static readonly Dictionary<string,RenderOptions> PatternSideRenderingCache = new Dictionary<string, RenderOptions>();
        private static RenderOptions GetPatternSideRendering(string tagname)
        {
            lock(PatternSideRenderingCache)
            {
                RenderOptions patternSideRendering;
                if (!PatternSideRenderingCache.TryGetValue(tagname, out patternSideRendering))
                {
                    patternSideRendering = new RenderOptions(StaticAIMLUtils.PatternSideRendering);
                    patternSideRendering.skip.Add(tagname);
                    PatternSideRenderingCache[tagname] = patternSideRendering;
                }
                return patternSideRendering;
            }
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
                                      bool isUserInput, Func<Unifiable, bool, Unifiable> innerFormater)
        {
            this.RProcessorOld.RaiseError("Wrong generate path in code");
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

            Unifiable res = Unifiable.MakePath(generateCPath(pattern, that, flag, topicName, isUserInput, innerFormater));
            if (isUserInput)
            {
                var ress = (string)res;
                if (false && ress.Contains("*"))
                {
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
            var normalizedPattern1 = normalizedPattern;
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
                writeDebugLine("WARN SHOULD REPAIR? '{0}' -> '{1}' ", normalizedPattern1, normalizedPattern);
                return normalizedPattern1;
            }
            return normalizedPattern;
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
                                        bool isUserInput, Func<Unifiable, bool, Unifiable> innerFormater)
        {
            AltBot RProcessor = LoaderRequest00.TargetBot;

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
                normalizedPattern = Trim(pattern);
                // clip only one off
                if (isUserInput) normalizedPattern = StaticAIMLUtils.CleanPunct(normalizedPattern);
                if (false)
                {
                    normalizedPattern = StaticAIMLUtils.MatchKeyClean(normalizedPattern);
                    normalizedThat = StaticAIMLUtils.MatchKeyClean(that);
                    normalizedTopic = StaticAIMLUtils.MatchKeyClean(topicName);
                }
                normalizedThat = StaticAIMLUtils.CleanPunct(that.Trim());
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

                if (StaticAIMLUtils.ThatWideStar)
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

                if (StaticAIMLUtils.useInexactMatching)
                {
                    Func<string, string> PadStarsOrNoWilds = isUserInput
                                                                 ? (Func<string, string>) StaticAIMLUtils.NoWilds
                                                                 : StaticAIMLUtils.PadStars;
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
                        normalizedThat = StaticAIMLUtils.CleanPunct(that);
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
                if (RProcessor.UseInlineThat)
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
        /// Given an input, provide a normalized output
        /// </summary>
        /// <param name="input">The Unifiable to be normalized</param>
        /// <param name="isUserInput">True if the Unifiable being normalized is part of the user input path - 
        /// flags that we need to normalize out * and _ chars</param>
        /// <returns>The normalized Unifiable</returns>
        public Unifiable NormalizeU(string input, bool isUserInput)
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
            input = input.Replace("*", " * ").Replace("  ", " ");
            input = Trim(input);
            // objects for normalization of the input
            AltBot RProcessor = LoaderRequest00.TargetBot;
            ApplySubstitutions substitutor = new ApplySubstitutions(RProcessor);
            StripIllegalCharacters stripper = new StripIllegalCharacters(RProcessor);

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
            var substitutedWords = substitutedInput.AsString().Split(' ');

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

        #endregion
    }
}

#endif