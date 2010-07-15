using System;
using System.Collections;
using System.Collections.Generic;
using System.Xml;

namespace RTParser.Utils
{
    public class QueryList : RequestSettingsImpl
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
        {
            TheRequest = request;
        }
        #region Overrides of RequestSettingsImpl

        public override GraphMaster Graph
        {
            get { return TheRequest.Graph; }
            set { TheRequest.Graph = value; }
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
        private List<Node> PatternsUsed;
        private List<SubQuery> Bindings;
        public Request TheRequest;
        public Node Bubble;
        public bool IsNewType = true;
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


        private void AddPattern(Node node)
        {
            if (PatternsUsed == null)
            {
                PatternsUsed = new List<Node>();
            }
            PatternsUsed.Add(node);
            if (PatternsUsed.Count > 1)
            {
                writeToLog("PatternsCount=" + PatternsUsed.Count);
            }
            CheckConsistent();
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
            bool b = PatternsUsed != null && PatternsUsed.Contains(node);
            if (b)
            {
                writeToLog("Node=" + node);
            }
            return b;
        }

        public void AddBindingSet(SubQuery query)
        {
            if (Bindings == null) Bindings = new List<SubQuery>();
            Bindings.Add(query);
            AddPattern(query.Pattern);
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
                s += pre + AIMLLoader.CleanWhitepaces(node) + Environment.NewLine;
            }
            s += Environment.NewLine;
            return s;
        }

        public bool ContainsPattern(KeyValuePair<Unifiable, Node> pair)
        {
            return PatternsUsed != null && ContainsPattern(pair.Value);
        }
    }
}