using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;


namespace AltAIMLbot.Utils
{
    [Serializable]
    public class GraphMaster
    {
        public Node root = new AltAIMLbot.Utils.Node();
        public string evaluate(string path, SubQuery query, Request request, MatchState state, StringBuilder builder)
        {
            return root.evaluate(path, query, request, state, builder);
        }

        public double getPathScore(string path)
        {
            return root.getPathScore(path);
        }

        public void collectFullPaths(string inpath, List<string> collector)
        {
            root.collectFullPaths(inpath, collector);
        }

        public void searchFullPaths(string targetPath,string inpath, List<string> collector)
        {
            root.searchFullPaths(targetPath, inpath, collector);        
        }

        public void addCategory(string path, string template, string filename, double score, double scale)
        {
            root.addCategory(path, template, filename, score, scale);
        }

        public void DisableFilename(string filename)
        {
            root.WithFilename(filename, false, false);
        }
        
        public void EnableFilename(string filename)
        {
            root.WithFilename(filename, false, true);
        }

        public void UnloadFilename(string filename)
        {
            root.WithFilename(filename, true, false);
        }
    }
}