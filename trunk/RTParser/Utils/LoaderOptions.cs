namespace RTParser.Utils
{
    public struct LoaderOptions
    {
        public Request TheRequest
        {
            set
            {
                Graph = value.Graph;
            }
        }
        private string _filename;
        public bool DebugFiles;
        public bool recurse;
        public string PrevFilename;
        private GraphMaster TheRequestGraph;

        public GraphMaster Graph
        {
            get { return TheRequestGraph; }
            set { TheRequestGraph = value; }
        }

        public string Filename
        {
            get { return _filename ?? PrevFilename ?? "loadopts_no_file"; }
            set
            {
                PrevFilename = Filename;
                _filename = value;
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
            return Filename + " " + Graph;
        }
    }
}