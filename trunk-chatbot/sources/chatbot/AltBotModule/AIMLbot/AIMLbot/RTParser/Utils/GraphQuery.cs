using System;
using System.Collections;
using System.Collections.Generic;
using System.Xml;
using AIMLbot;
using AltAIMLbot;
using AltAIMLbot.Utils;
using AltAIMLParser;
using MushDLR223.ScriptEngines;
using MushDLR223.Utilities;
using MasterRequest = AltAIMLbot.Utils.Request;


namespace AltAIMLbot.Utils
{
    public class GraphQuery : QuerySettings
    {
        public bool IsComplete(Request request)
        {
            var toplevel = this;
            return toplevel.IsMaxedOut ||
                   request.IsComplete(toplevel.CurrentResult);
        }

        public MasterRequest TheRequest;
        private List<TemplateInfo> Templates;
        private List<SubQuery> Bindings;
        public List<Node> PatternsUsed;
        public Unifiable InputPath;
        public MatchState matchState;
        public GraphMaster graphMaster;

        private bool _NoMoreResults_;
        public bool NoMoreResults
        {
            get
            {
                return _NoMoreResults_;
            }
            set
            {
                TheRequest.SuspendSearchLimits = false;
                _NoMoreResults_ = value;
            }
        }

        public string WhyToplevelComplete
        {
            get
            {
                if (TheRequest.SuspendSearchLimits) return null;
                string s = null;
                if (TemplateCount >= MaxTemplates)
                {
                    s = "TLMaxTemplatesT";
                }
                if (PatternCount >= MaxPatterns)
                {
                    s = (s ?? "") + " TLMaxPatternsT";
                }
                return s;
            }
        }

        public string WhyComplete
        {
            get
            {
                string ss = TheRequest.WhyNoSearch(CurrentResult);
                if (ss == null)
                {
                    if (!IsMaxedOut) return null;
                    return " TopLevel.IsMaxedOut";
                }
                string localMax = WhyToplevelComplete;
                if (!string.IsNullOrEmpty(ss)) return ss + " " + localMax;
                return null;
            }
        }

        public Result CurrentResult
        {
            get { return TheRequest.CurrentResult; }
        }


        public bool IsAllowedGraph(GraphMaster graph)
        {
            if (!TheRequest.IsAllowedGraph(graph)) return false;
            return true;
        }


        public GraphQuery(Unifiable inputPath, Request request, GraphMaster gMaster, MatchState mstate)
            : base(request)
        {
            InputPath = inputPath;
            TheRequest = (MasterRequest)request;
            graphMaster = gMaster;
            matchState = mstate;
        }


        #region Overrides of RequestSettingsImpl

        public override string StartGraphName
        {
            get { return ((QuerySettingsReadOnly)TheRequest).StartGraphName; }
            set { TheRequest.StartGraphName = value; }
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
                if (TheRequest.SuspendSearchLimits) return false;
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
        public bool IsMaxedOutOrOverBudget
        {
            get
            {
                if (TheRequest.SuspendSearchLimits) return false;
                return IsMaxedOut || TheRequest.IsTimedOutOrOverBudget;
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
                        //writeToLog("PatternsCount=" + PatternsUsed.Count);
                        // ReSharper disable ConditionIsAlwaysTrueOrFalse
                        if (false && PatternsUsed.Count == 3)
                            // ReSharper restore ConditionIsAlwaysTrueOrFalse
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
            AltBot.writeDebugLine("QUERYTRACE: " + message, args);
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
            if (Bindings == null) return new List<SubQuery>();
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
            return StaticAIMLUtils.CleanWhitepaces("" + info);
        }

        public bool ContainsPattern(KeyValuePair<Unifiable, Node> pair)
        {
            return PatternsUsed != null && ContainsPattern(pair.Value);
        }

        public bool CanUseNode(Node node)
        {
            return true;
        }
        public ICollection<SubQuery> PreprocessSubQueries(Request request, ICollection<SubQuery> resultSubQueries, bool isTraced, ref bool printedSQs, OutputDelegate writeToLog)
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
            return resultSubQueries;
        }
    }
}