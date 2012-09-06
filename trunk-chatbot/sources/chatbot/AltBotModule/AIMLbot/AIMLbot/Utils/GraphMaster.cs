using System;
using System.Collections;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading;
using System.Windows.Forms;


namespace AltAIMLbot.Utils
{
    [Serializable]
    public class GraphMaster
    {
        public static string[] StarTypes = new[] {"state", "topic", "pattern", "that", };
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

        public static void PrintToWriter(IEnumerable items, PrintOptions printOptions, TextWriter fs, IList written, TimeSpan sleepBetween)
        {
            //string hide = "";
            if (items == null) return;
            foreach (var cio in items)
            {
                if (sleepBetween > TimeSpan.Zero) Thread.Sleep(sleepBetween);
                fs.WriteLine(""+cio);

                /*
                IAIMLInfo ci = cio as IAIMLInfo;
                if (ci == null)
                {
                    fs.WriteLine("" + cio);
                    continue;
                }
                string graphName = ci.Graph.graphName;
                if (printOptions.DontPrint(ci)) continue;
                string c = ci.ToFileString(printOptions);
                string cws = TextPatternUtils.CleanWhitepaces(c);
                if (printOptions.DontPrint(cws)) continue;

                if (printOptions.RemoveDuplicates)
                {
                    printOptions.Writting(c);
                    if (cws != c) printOptions.Writting(cws);
                }
                if (written != null) written.Add(ci);
                string ss = c.TrimEnd();
                if (printOptions.CleanWhitepaces)
                {
                    // ss = cws;
                }
                fs.Write(ss);
                if (printOptions.IncludeLineInfoExternal || printOptions.IncludeGraphName)
                {
                    if (!printOptions.CategoryPerLine) if (ss.Length > 50) fs.WriteLine();
                    fs.Write("   <!-- ");
                    if (printOptions.IncludeGraphName)
                    {
                        fs.Write(graphName);
                    }
                    if (printOptions.IncludeLineInfoExternal)
                    {
                        c = ci.SourceInfo();
                        if (!c.Contains("(0,0)"))
                        {
                            fs.Write(" " + c);
                        }
                    }
                    fs.WriteLine("-->");
                }
                 */
                Application.DoEvents();
            }
        }
    }
}