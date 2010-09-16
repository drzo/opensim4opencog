using System;
using System.Collections;
using System.Collections.Generic;
using System.Xml;
using MushDLR223.ScriptEngines;
using MushDLR223.Utilities;

namespace RTParser.Utils
{
    public class QueryList : QuerySettings
    {
        public bool IsComplete(Request request)
        {
            var toplevel = this;
            return toplevel.IsMaxedOut ||
                   request.IsComplete(toplevel.CurrentResult);
        }

        private List<SubQuery> Bindings;
        public bool NoMoreResults;
        public List<Node> PatternsUsed;
        private List<TemplateInfo> Templates;
        public RequestImpl TheRequest;
        public Unifiable InputPath;
        public string WhyComplete
        {
            get
            {
                return TheRequest.WhyNoSearch(CurrentResult) + (IsMaxedOut ? " TopLevel.IsMaxedOut" : "");
            }
        }

        public Result CurrentResult
        {
            get { return TheRequest.CurrentResult; }
        }

        public ICollection<GraphMaster> DisallowedGraphs
        {
            get { return TheRequest.ParentMostRequest.DisallowedGraphs; }
        }

         
        public QueryList(Unifiable inputPath, Request request)
            : base(request)
        {
            InputPath = inputPath;
            TheRequest = (RequestImpl) request;
        }

        #region Overrides of RequestSettingsImpl

        public override string GraphName
        {
            get { return ((QuerySettingsReadOnly) TheRequest).GraphName; }
            set { TheRequest.GraphName = value; }
        }

        #endregion

        public int TemplateCount
        {
            get { return Templates == null ? 0 : Templates.Count; }
        }

        public int PatternCount
        {
            get { return PatternsUsed == null ? 0 : PatternsUsed.Count; }
        }

        public bool IsMaxedOut
        {
            get
            {
                if (TemplateCount >= MaxTemplates)
                {
                    return true;
                }
                if (PatternCount >= MaxPatterns)
                {
                    return true;
                }
                return false;
            }
        }

        public override string ToString()
        {
            lock (this)
            {
                string whyComplete = WhyComplete;
                String s = TheRequest + " " + Environment.NewLine;
                var noddes = new List<Node>();
                s += ToString("  Q: ", Bindings);
                s += ToString("  P: ", PatternsUsed);
                s += ToString("  T: ", Templates);
                return s + Environment.NewLine + (whyComplete != null ? " WhyComplete=" + whyComplete : "");
            }
        }


        public void AddPattern(Node node)
        {
            lock (this)
            {
                if (PatternsUsed == null)
                {
                    PatternsUsed = new List<Node>();
                    PatternsUsed.Add(node);
                }
                else
                {
                    if (!PatternsUsed.Contains(node))
                    {
                        PatternsUsed.Add(node);
                        writeToLog("PatternsCount=" + PatternsUsed.Count);
                        if (PatternsUsed.Count == 3)
                        {
                            foreach (Node list in PatternsUsed)
                            {
                                writeToLog("Pattern=" + list);
                            }
                        }
                    }
                }
                CheckConsistent();
            }
        }

        internal static void writeToLog(string message, params object[] args)
        {
            RTPBot.writeDebugLine("QUERYTRACE: " + message, args);
        }

        private void CheckConsistent()
        {
            return;
            if (PatternCount != 0)
            {
                if (PatternCount != Bindings.Count)
                {
                    throw new NotImplementedException();
                }
            }
        }

        public void AddTemplate(TemplateInfo info)
        {
            if (Templates == null) Templates = new List<TemplateInfo>();
            Templates.Add(info);
        }

        public bool ContainsPattern(Node node)
        {
            lock (this)
            {
                bool b = PatternsUsed != null && PatternsUsed.Contains(node);
                return b;
            }
        }

        public void AddBindingSet(SubQuery query)
        {
            lock (this)
            {
                if (Bindings == null) Bindings = new List<SubQuery>();
                Bindings.Add(query);
                AddPattern(query.Pattern);
            }
        }

        public IEnumerable<SubQuery> GetBindings()
        {
            return Bindings;
        }

        private static string ToString(string pre, ICollection c)
        {
            if (c == null || c.Count == 0) return Environment.NewLine;
            string s = "";
            foreach (object node in c)
            {
                s += pre + CleanWhitepacesObject(node) + Environment.NewLine;
            }
            s += Environment.NewLine;
            return s;
        }

        public static string CleanWhitepacesObject(object info)
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

        public bool ContainsPattern(KeyValuePair<Unifiable, Node> pair)
        {
            return PatternsUsed != null && ContainsPattern(pair.Value);
        }

        public bool CanUseNode(Node node)
        {
            return true;
        }
        public List<SubQuery> PreprocessSubQueries(Request request, ICollection<SubQuery> resultSubQueries, bool isTraced, ref bool printedSQs, OutputDelegate writeToLog)
        {
            // var usingResultSubQueries = new List<SubQuery>();
            //todo pick and chose the queries
            // if (result.SubQueries.Count != 1)
            if (isTraced)
            {
                string s = "AIMLTRACE: SubQueries.Count = " + resultSubQueries.Count;
                foreach (SubQuery path in resultSubQueries)
                {
                    s += Environment.NewLine;
                    s += "  " + Unifiable.ToVMString(path.FullPath);
                }
                printedSQs = true;
                writeToLog(s);
                DLRConsole.SystemFlush();
            }
            return (List<SubQuery>) resultSubQueries;
        }
    }
}