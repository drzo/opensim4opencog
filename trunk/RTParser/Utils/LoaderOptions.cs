namespace RTParser.Utils
{
    public class LoaderOptions
    {
        public bool recurse;

        public GraphMaster Graph;

        public static LoaderOptions GetDefault(Request r)
        {
            LoaderOptions ops = new LoaderOptions(r.Graph);
            ops.request = r;
            return ops;
        }
        //public static LoaderOptions GetDefault(GraphMaster g)
        //{
        //    return new LoaderOptions(g);
        //}
        //public static LoaderOptions GetDefault(RTPBot bot)
        //{
        //    return GetDefault(bot.GraphMaster);
        //}

        public Request request;
        public string Filename;
        public int LineOffset = 0;
        public bool DebugFiles;

        public LoaderOptions(GraphMaster g)
        {
            Graph = g;

        }

        public override string ToString()
        {
            return Filename ?? "LoaderOptions - no filename";
        }

        public static LoaderOptions FromFilename(string filename, Request request)
        {
            LoaderOptions lo = LoaderOptions.GetDefault(request);
            lo.Filename = filename;
            return lo;
        }

        public void SetFilename(string filename)
        {
            Filename = filename;
            LineOffset = 0;
        }
    }
}