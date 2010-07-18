namespace RTParser.Utils
{
    public class LoaderOptions
    {
        public Request TheRequest;
        private string _filename;
        public bool DebugFiles;
        public bool recurse;
        public string PrevFilename;

        public GraphMaster Graph
        {
            get { return TheRequest.Graph; }
            set { TheRequest.Graph = value; }
        }

        public string Filename
        {
            get { return _filename; }
            set
            {
                PrevFilename = Filename;
                _filename = value;
            }
        }

        public static LoaderOptions GetDefault(Request r)
        {
            LoaderOptions ops = new LoaderOptions(r);
            return ops;
        }
        private LoaderOptions(Request request)
        {
            TheRequest = request;
        }

        public override string ToString()
        {
            return _filename ?? "LoaderOptions - no filename";
        }
    }
}