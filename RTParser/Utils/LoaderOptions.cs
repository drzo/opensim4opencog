using System;
using System.Collections.Generic;

namespace RTParser.Utils
{
    public class LoaderOptions
    {
        //public LoaderOptions prevoious;
        public readonly Request TheRequest;
        private string _curently_loading;
        public bool DebugFiles;
        public bool recurse;
        public string _currently_loadingfrom;
        private GraphMaster _specified_Graph;
        public GraphMaster CtxGraph
        {
            get
            {
               // if (_specified_Graph != null) return _specified_Graph;
                return TheRequest.Graph;
            }
            set
            {
                TheRequest.Graph = value;
                _specified_Graph = value;
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
            get
            {
                return _currently_loadingfrom ?? TheRequest.LoadingFrom;
            }
        }


        public static readonly string MISSING_FILE = "loadopts_MISSING_FILE";
        public RTPBot RProcessor;
        public List<CategoryInfo> CategoryInfos;

        public LoaderOptions Value
        {
            get
            {
                return this;
            }
        }


        public LoaderOptions(RequestImpl impl, GraphMaster master)
        {
            TheRequest = impl;
            _curently_loading = impl.Filename;
            DebugFiles = false;
            recurse = false;
            _currently_loadingfrom = impl.LoadingFrom;
            _specified_Graph = master;
            RProcessor = impl.TargetBot; 
            CategoryInfos = new List<CategoryInfo>();
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
            if (obj.GetType() != typeof(LoaderOptions)) return false;
            return Equals((LoaderOptions)obj);
        }

        public static bool operator ==(LoaderOptions thiz, LoaderOptions other)
        {
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
                result = (result * 397) ^ recurse.GetHashCode();
                result = (result * 397) ^ (_specified_Graph != null ? _specified_Graph.GetHashCode() : 0);
                return result;
            }
        }
    }
}