using System;
using System.Collections;
using System.Collections.Generic;
using System.Xml;
using AltAIMLbot.Utils;
using AltAIMLParser;
using GraphQuery = AltAIMLbot.Utils.QueryList;
using MushDLR223.Utilities;

namespace AltAIMLbot.Utils
{
    public partial class QueryList : QuerySettings
    {

        public override string ToString()
        {
            lock (this)
            {
                String s = TheRequest.ToString() + " " + Environment.NewLine;
                var noddes = new List<Node>();
                s += ToString("  Q: ", Bindings);
                s += ToString("  P: ", PatternsUsed);
                s += ToString("  T: ", Templates);
                return s + Environment.NewLine;
            }
        }

        public QueryList(Request request)
            : base(request)
        {
            TheRequest = request;
        }

        #region Overrides of RequestSettingsImpl

        public override string StartGraphName
        {
            get { return TheRequest.StartGraphName; }
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

        private List<TemplateInfo> Templates;
        public List<Node> PatternsUsed;
        private List<SubQuery> Bindings;
        public Request TheRequest;

        public bool NoMoreResults;

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
                            foreach (var list in PatternsUsed)
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
            return Bindings;
        }

        private string ToString(string pre, ICollection c)
        {
            if (c == null || c.Count == 0) return Environment.NewLine;
            string s = "";
            foreach (object node in c)
            {
                s += pre + (string)StaticAIMLUtils.CleanWhitepaces(""+node) + Environment.NewLine;
            }
            s += Environment.NewLine;
            return s;
        }

        public bool ContainsPattern(KeyValuePair<Unifiable, Node> pair)
        {
            return PatternsUsed != null && ContainsPattern(pair.Value);
        }

        public bool CanUseNode(Node node)
        {
            return true;
        }
    }
}