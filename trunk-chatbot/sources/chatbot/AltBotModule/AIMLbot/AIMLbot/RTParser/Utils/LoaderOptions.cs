using System;
using System.Collections.Generic;
using System.Xml;
using AIMLbot;
using AltAIMLParser;
using MasterRequest = AltAIMLbot.Utils.Request;


//using CategoryInfo = RTParser.Utils.TemplateInfo;

namespace AltAIMLbot.Utils
{
    public class LoaderOptions
    {
        //public LoaderOptions prevoious;
        public static readonly string MISSING_FILE = "loadopts_MISSING_FILE";
        public readonly MasterRequest TheRequest;
        private string _curently_loading;
        public string _currently_loadingfrom;
        private GraphMaster _specified_Graph;
        public List<ConversationCondition> AdditionalPreconditions;
        public List<CategoryInfo> CategoryInfos;
        public bool DebugFiles;
        public bool recurse;
        public AltBot RProcessor;
        public bool NeedsLoaderLock;
        private bool _searchForGuard = false;
        internal string graphName = "*";
        internal string topicName = "*";
        internal string stateNamePre = "*";
        internal string stateNamePost = "*";
        internal string currentThat = "*";
        public void withAttributes(XmlNode node, ref string defaultElement, Action action)
        {
            string preTopic = topicName;
            string preRef = defaultElement;
            string preStateNamePre = stateNamePre;
            string preStateNamePost = stateNamePost;
            string preThat = currentThat;
            string preGraph = graphName;
            foreach (XmlAttribute Attrib in node.Attributes)
            {
                if (Attrib.Name == "name")
                {
                    defaultElement = node.Attributes["name"].Value;
                }
                if (Attrib.Name == "state")
                {
                    stateNamePre = node.Attributes["state"].Value;
                }
                if (Attrib.Name == "topic")
                {
                    topicName = node.Attributes["topic"].Value;
                }
                if (Attrib.Name == "graph")
                {
                    graphName = node.Attributes["graph"].Value;
                }
                if (Attrib.Name == "that")
                {
                    currentThat = node.Attributes["that"].Value;
                }
                if (Attrib.Name == "prestate")
                {
                    stateNamePre = node.Attributes["prestate"].Value;
                }
                if (Attrib.Name == "poststate")
                {
                    stateNamePost = node.Attributes["poststate"].Value;
                }
            }
            try
            {
                action();
            }
            finally
            {
                topicName = preTopic;
                stateNamePre = preStateNamePre;
                stateNamePost = preStateNamePost;
                graphName = preGraph;
                currentThat = preThat;
                defaultElement = preRef;
            }
        }

        public bool RemovePreviousTemplatesFromNodes
        {
            get { return CtxGraph.RemovePreviousTemplatesFromNodes; }
        }
        public bool DistinguishSilenetTags
        {
            get { return CtxGraph.DistinguishSilenetTags; }
        }


        public LoaderOptions(Request impl, GraphMaster master)
        {
            TheRequest = (MasterRequest) impl ;
            _curently_loading = impl.Filename;
            DebugFiles = false;
            recurse = false;
            _currently_loadingfrom = impl.LoadingFrom;
            _specified_Graph = master;
            RProcessor = impl.TargetBot;
            CategoryInfos = new List<CategoryInfo>();
        }

        public GraphMaster CtxGraph
        {
            get
            {
                if (_specified_Graph != null)
                {
                    return _specified_Graph;
                }
                return TheRequest.Graph;
            }
            set
            {
                _specified_Graph = value;
                TheRequest.sGraph = value;
            }
        }

        public string CurrentFilename
        {
            get { return _curently_loading ?? TheRequest.Filename; }
        }


        public string Loading0
        {
            set
            {
                //PrevFilename = _filename;
                TheRequest.Filename = value;
                _curently_loading = value;
            }
        }

        public string LoadingFrom0
        {
            set
            {
                if (TheRequest.Filename == null)
                {
                    TheRequest.Filename = value;
                }
                _currently_loadingfrom = value;
            }
        }

        public string CurrentlyLoadingFrom
        {
            get { return _currently_loadingfrom ?? TheRequest.LoadingFrom; }
        }


        public LoaderOptions Value
        {
            get { return this; }
        }

        public ICollection<CrossAppDomainDelegate> ReloadHooks
        {
            get { return RProcessor.ReloadHooks; }
        }

        public bool SearchForGuard
        {
            get {
                return _searchForGuard;
            }
            set {
                _searchForGuard = value;
            }
        }


        /*public static LoaderOptions GetDefault(Request r)
        {
            LoaderOptions ops = new LoaderOptions();
            ops.TheRequest = r;
            return ops;
        }*/
        //private LoaderOptions(Request request)
        //{
        //  TheRequest = request;
        //}

        public override string ToString()
        {
            return CtxGraph + "<-" + CurrentFilename + "<-" + CurrentlyLoadingFrom;
        }

        public bool Equals(LoaderOptions other)
        {
            bool b = Equals(other.CurrentFilename, CurrentFilename)
                     && other.recurse.Equals(recurse)
                     && Equals(other.CtxGraph, CtxGraph);
            if (b) return b;
            return false;
        }


        public override bool Equals(object obj)
        {
            if (ReferenceEquals(null, obj)) return false;
            if (obj.GetType() != typeof (LoaderOptions)) return false;
            return Equals((LoaderOptions) obj);
        }

        public static bool operator ==(LoaderOptions thiz, LoaderOptions other)
        {
            if (ReferenceEquals(null, thiz)) return ReferenceEquals(null, other);
            if (ReferenceEquals(null, other)) return false;
            return thiz.Equals(other);
        }

        public static bool operator !=(LoaderOptions thiz, LoaderOptions other)
        {
            return !(thiz == other);
        }


        public override int GetHashCode()
        {
            unchecked
            {
                int result = (_curently_loading != null ? _curently_loading.GetHashCode() : 0);
                result = (result*397) ^ recurse.GetHashCode();
                result = (result*397) ^ (_specified_Graph != null ? _specified_Graph.GetHashCode() : 0);
                return result;
            }
        }
    }
}