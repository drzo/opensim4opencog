using System;
using System.Collections.Generic;
using System.Net;
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
        public AIMLLoader(RTParser.RTPBot bot, Request request)
        {
            this.RProcessor = bot;
        }

        #region Methods

        /// <summary>
        /// Loads the AIML from files found in the RProcessor's AIMLpath into the RProcessor's brain
        /// </summary>
        public void loadAIML(Request request)
        {
            this.loadAIML(this.RProcessor.PathToAIML, LoaderOptions.GetDefault(request), request);
        }

        /// <summary>
        /// Loads the AIML from files found in the path
        /// </summary>
        /// <param name="path"></param>
        public void loadAIML(string path, LoaderOptions options, Request request)
        {
            RProcessor.ReloadHooks.Add(() => loadAIML(path, options, request));
            if (Directory.Exists(path))
            {
                // process the AIML
                loadAIMLDir(path, options, request);
            }
            else if (File.Exists(path))
            {
                this.loadAIMLFile(path, options, request);
            }
            else
            {
                this.loadAIMLURI(path, options, request);
            }
            Console.WriteLine("*** Loaded AIMLFiles From Location: '{0}' ***", path);

        }

        private void loadAIMLDir(string path, LoaderOptions options, Request request)
        {
            this.RProcessor.writeToLog("Starting to process AIML files found in the directory " + path);

            string[] fileEntries = Directory.GetFiles(path, "*.aiml");
            if (fileEntries.Length > 0)
            {
                foreach (string filename in fileEntries)
                {
                    try
                    {
                        this.loadAIMLFile(filename, options, request);
                    }
                    catch (Exception ee)
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

            if (options.recurse)
            {
                foreach (string filename in Directory.GetDirectories(path))
                {
                    loadAIMLDir(path + Path.DirectorySeparatorChar + filename, options, request);
                }
            }

        }

        private void loadAIMLURI(string path, LoaderOptions loadOpts, Request request)
        {
            try
            {
                this.RProcessor.writeToLog("Processing AIML URI: " + path);
                if (Directory.Exists(path))
                {
                    loadAIMLDir(path, loadOpts, request);
                }
                else if (File.Exists(path))
                {
                    loadAIMLFile(path, loadOpts, request);
                }
                else if (Uri.IsWellFormedUriString(path, UriKind.RelativeOrAbsolute))
                {
                    var uri = new Uri(path);
                    if (uri.IsFile)
                    {
                        loadAIMLURI(uri.AbsolutePath, loadOpts, request);
                        return;
                    }
                    WebRequest req = WebRequest.Create(uri);
                    WebResponse resp = req.GetResponse();
                    Stream stream = resp.GetResponseStream();
                    loadAIMLStream(stream, LoaderOptions.FromFilename(path, request), request);
                }
            }
            catch (Exception e)
            {
                String s = "ERROR: XmlTextReader of AIML files (" + path + ")  threw " + e;
                throw new FileNotFoundException(s);
            }
            String nf = "ERROR: XmlTextReader of AIML files (" + path + ")";
            throw new FileNotFoundException(nf);
        }

        /// <summary>
        /// Given the name of a file in the AIML path directory, attempts to load it into the 
        /// graphmaster
        /// </summary>
        /// <param name="filename">The name of the file to process</param>
        public void loadAIMLFile(string filename, LoaderOptions opt, Request request)
        {
            this.RProcessor.writeToLog("Processing AIML file: " + filename);
            if (Directory.Exists(filename))
            {
                if (opt.recurse) loadAIMLDir(filename, opt, request);
                return;
            }
            try
            {
                // load the document
                var tr = File.OpenRead(filename);
                try
                {
                    opt.SetFilename(filename);
                    this.loadAIMLStream(tr, opt, request);
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

                Console.WriteLine("Loaded AIMLFile: '{0}'", filename);
                return;
            }
            catch (Exception e)
            {
                this.RProcessor.writeToLog("Error in AIML Stacktrace: " + filename + "\n  " + e.Message + "\n" + e.StackTrace);
                this.RProcessor.writeToLog("Error in AIML file: " + filename + " Message " + e.Message);
            }
        }
        /// <summary>
        /// Given an XML document containing valid AIML, attempts to load it into the graphmaster
        /// </summary>
        /// <param name="doc">The XML document containing the AIML</param>
        /// <param name="filename">Where the XML document originated</param>
        public void loadAIMLString(string input, LoaderOptions filename, Request request)
        {
            var tr = new StringReader(input);
            XmlTextReader xtr = new XmlTextReader(tr);
            while (!xtr.EOF)
            {
                try
                {
                    XmlDocumentLineInfo doc = new XmlDocumentLineInfo(tr);
                    doc.Load(xtr);
                    if (doc.DocumentElement == null) continue;
                    this.loadAIMLNode(doc.DocumentElement, filename, request);
                }
                catch (Exception e2)
                {
                    String s = "which causes loadAIMLString '" + input + "' " + filename + " charpos " + tr;
                    s = s + "\n" + e2.Message + "\n" + e2.StackTrace + "\n" + s;
                    System.Console.WriteLine(s);
                    throw e2;
                }

            }
            return;
        }

        /// <summary>
        /// Given an XML document containing valid AIML, attempts to load it into the graphmaster
        /// </summary>
        /// <param name="doc">The XML document containing the AIML</param>
        /// <param name="filename">Where the XML document originated</param>
        public void loadAIMLStream(Stream input, LoaderOptions filename, Request request)
        {

            var xtr = new XmlTextReader(input);
            while (!xtr.EOF)
            {
                try
                {
                    XmlDocumentLineInfo doc = new XmlDocumentLineInfo(input);
                    doc.Load(xtr);
                    if (doc.DocumentElement == null) continue;
                    this.loadAIMLNode(doc.DocumentElement, filename, request);
                }
                catch (Exception e2)
                {
                    String s = "which causes loadAIMLStream '" + input + "' " + filename + " charpos=" + input.Position;
                    s = s + "\n" + e2.Message + "\n" + e2.StackTrace + "\n" + s;
                    System.Console.WriteLine(s);
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

        public void loadAIMLNode(XmlNode currentNode, LoaderOptions filename, Request request)
        {
            var prev = RProcessor.Loader;
            try
            {
                RProcessor.Loader = this;
                if (currentNode.NodeType == XmlNodeType.Comment) return;
                // Get a list of the nodes that are children of the <aiml> tag
                // these nodes should only be either <topic> or <category>
                // the <topic> nodes will contain more <category> nodes
                if (currentNode.Name == "aiml")
                {
                    // process each of these child nodes
                    foreach (XmlNode child in currentNode.ChildNodes)
                    {
                        loadAIMLNode(child, filename, request);
                    }
                    return;
                }
                if (currentNode.Name == "root")
                {
                    // process each of these child "settings"? nodes
                    foreach (XmlNode child in currentNode.ChildNodes)
                    {
                        loadAIMLNode(child, filename, request);
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
                    this.processTopic(currentNode, currentNode.ParentNode, filename);
                }
                else if (currentNode.Name == "category")
                {
                    this.processCategory(currentNode, currentNode.ParentNode, filename);
                }
                else
                {
                    try
                    {
                        RProcessor.ImmediateAiml(currentNode, request, this);
                    }
                    catch (Exception e)
                    {
                        RProcessor.writeToLog("ImmediateAiml: " + e);
                    }
                }
            }
            finally
            {
                RProcessor.Loader = prev;
            }
        }

        /// <summary>
        /// Given a "topic" topicNode, processes all the categories for the topic and adds them to the 
        /// graphmaster "brain"
        /// </summary>
        /// <param name="topicNode">the "topic" node</param>
        /// <param name="filename">the file from which this topicNode is taken</param>
        public void processTopic(XmlNode topicNode, XmlNode outerNode, LoaderOptions filename)
        {
            // find the name of the topic or set to default "*"
            Unifiable topicName = RTPBot.GetAttribValue(topicNode, "name", Unifiable.STAR);
            // process all the category nodes
            foreach (XmlNode cateNode in topicNode.ChildNodes)
            {
                if (cateNode.Name == "category")
                {
                    processCategoryWithTopic(cateNode, topicName, topicNode, filename);
                }
            }
        }

        /// <summary>
        /// Adds a category to the graphmaster structure using the default topic ("*")
        /// </summary>
        /// <param name="cateNode">the XML node containing the category</param>
        /// <param name="filename">the file from which this category was taken</param>
        public void processCategory(XmlNode cateNode, XmlNode outerNode, LoaderOptions filename)
        {
            this.processCategoryWithTopic(cateNode, Unifiable.STAR, outerNode, filename);
        }

        /// <summary>
        /// Adds a category to the graphmaster structure using the given topic
        /// </summary>
        /// <param name="cateNode">the XML node containing the category</param>
        /// <param name="topicName">the topic to be used</param>
        /// <param name="filename">the file from which this category was taken</param>
        private void processCategoryWithTopic(XmlNode cateNode, Unifiable topicName, XmlNode outerNode,  LoaderOptions filename)
        {
            // reference and check the required nodes
            List<XmlNode> patterns = FindNodes("pattern", cateNode);
            List<XmlNode> templates = FindNodes("template", cateNode);
            foreach (XmlNode pattern in patterns)
            {
                foreach (var template in templates)
                {
                    if (object.Equals(null, pattern))
                    {
                        throw new XmlException("Missing pattern tag in a cateNode found in " + filename);
                    }
                    if (object.Equals(null, template))
                    {
                        throw new XmlException("Missing template tag in the cateNode with pattern: " + pattern.InnerText + " found in " + filename);
                    }
                    addCatNode(cateNode, pattern, filename, template, topicName, outerNode);
                }
            }            
        }

        private void addCatNode(XmlNode cateNode, XmlNode patternNode, LoaderOptions filename, XmlNode templateNode, 
            Unifiable topicName, XmlNode outerNode)
        {
            XmlNode guardnode = FindNode("guard", cateNode, FindNode("guard", outerNode, null));
            GuardInfo guard = guardnode == null ? null : new GuardInfo(guardnode);


            Unifiable categoryPath = this.generatePathExtractWhat(patternNode, cateNode, topicName, false);
            PatternInfo patternInfo = PatternInfo.GetPattern(filename, patternNode, categoryPath);

            // o.k., add the processed AIML to the GraphMaster structure
            if (!categoryPath.IsEmpty)
            {
                try
                {
                    this.RProcessor.GraphMaster.addCategoryTag(categoryPath, patternInfo,
                                                               CategoryInfo.GetCategoryInfo(patternInfo, cateNode, filename),
                                                               outerNode,templateNode, guard);
                }
                catch (Exception e)
                {
                    string s = "ERROR! Failed to load a new category into the graphmaster where the path = " +
                               categoryPath + " and templateNode = " + templateNode.OuterXml +
                               " produced by a category in the file: " + filename + "\n";
                    this.RProcessor.writeToLog(s + e + "\n" + s);
                }
            }
            else
            {
                this.RProcessor.writeToLog("WARNING! Attempted to load a new category with an empty patternNode where the path = " + categoryPath + " and templateNode = " + templateNode.OuterXml + " produced by a category in the file: " + filename);
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
        private Unifiable generatePathExtractWhat(XmlNode patternNode, XmlNode cateNode, Unifiable topicName, bool isUserInput)
        {
            // get the nodes that we need
            XmlNode that = FindNode("that", cateNode, null);

            Unifiable patternText;
            Unifiable thatText = Unifiable.STAR;
            if (object.Equals(null, patternNode))
            {
                patternText = Unifiable.Empty;
            }
            else
            {
                patternText = Unifiable.Create(patternNode);//.InnerXml;
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
        static public List<XmlNode> FindNodes(string name, XmlNode node)
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

            return result.ToString().Replace("  ", " "); // make sure the whitespace is neat
        }
        #endregion
    }

    class XmlDocumentLineInfo : XmlDocument
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

        private Stream LineInfoReader;
        public IXmlLineInfo LineTracker;
        private StringReader stringReader;
        public XmlDocumentLineInfo(Stream lineInfoReader)
        {
            LineInfoReader = lineInfoReader;
        }
        public XmlDocumentLineInfo(StringReader lineInfoReader)
        {
            stringReader = lineInfoReader;
        }
        public XmlDocumentLineInfo(IXmlLineInfo lineInfoReader)
        {
            LineTracker = lineInfoReader;
        }
        public override XmlElement CreateElement(string prefix, string localname, string nsURI)
        {
            LineInfoElement elem = new LineInfoElement(prefix, localname, nsURI, this);
            if (LineInfoReader != null)
                elem.SetPos(LineInfoReader.Position);
            if (LineTracker != null)
            {
                elem.SetLineInfo(LineTracker.LineNumber, LineTracker.LinePosition);
            }
            return elem;
        }
    }

    class LineInfoElement : XmlElement, IXmlLineInfo
    {
        public int lineNumber = 0;
        public int linePosition = 0;
        public long charPos = 0;
        internal LineInfoElement(string prefix, string localname, string nsURI, XmlDocument doc)
            : base(prefix, localname, nsURI, doc)
        {
            //((XmlDocumentLineInfo)doc).IncrementElementCount();
        }
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
    } // End LineInfoElement class.
}
