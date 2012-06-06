using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Text.RegularExpressions;

namespace AltAIMLbot
{
    class NodeData
    {
        public string token;
        private int linksOutCount;
        private double pageRank;
        private List<int> linksIn;

        public NodeData()
        {
            linksIn = new List<int>();
        }

        public int LinksOutCount
        {
            get
            {
                return linksOutCount;
            }
            set
            {
                linksOutCount = value;
            }
        }

        public double PageRank
        {
            get
            {
                return pageRank;
            }
            set
            {
                pageRank = value;
            }
        }

        public List<int> LinksIn
        {
            get
            {
                return linksIn;
            }
        }
    }

    public class TokenRanker
    {
        Dictionary<int, NodeData> tokenData = new Dictionary<int, NodeData>();
        List<int> nodesToRank = new List<int>();
        List<string> definingSentences = new List<string>();
        Regex SentRegex = new Regex(@"(\S.+?[.!?,\)])(?=\s+|$)");
        public string stoplist = " a an the this that and of for in to at by with so it on but as uh or";
        public double changeLim = 0.000001;
        public int maxIterations = 50;
        public double lamda = 0.15;

        public void TokenRanker()
        {
        }

        public double scoreText(string text)
        {
            double sum = 0;
            string[] tokens = text.Split(' ');
            foreach (string w in tokens)
            {
                int id = w.GetHashCode();
                double v = 0;
                if (tokenData.ContainsKey(id)) v=tokenData[id].PageRank;
                sum += v;
            }
            return sum;
        }

        public string summaryByRank(int lengthLim)
        {
            Dictionary<string, double> summaryOrder = new Dictionary<string, double>();
            foreach (string sent in definingSentences)
            {
                double v = scoreText(sent);
                summaryOrder.Add(sent, v);
            }
            summaryOrder = summaryOrder.OrderByDescending(x => x.Value).ToDictionary(x => x.Key, x => x.Value);
            
            string summ = "";
            foreach (string s in summaryOrder.Keys)
            {
                summ += s + "\n";
                if (summ.Length >= lengthLim) break;
            }
            return summ;
        }

        public string summaryByOriginalSequence(int lengthLim)
        {
            Dictionary<string, double> summaryOrder = new Dictionary<string, double>();
            foreach (string sent in definingSentences)
            {
                double v = scoreText(sent);
                summaryOrder.Add(sent, v);
            }
            summaryOrder = summaryOrder.OrderByDescending(x => x.Value).ToDictionary(x => x.Key, x => x.Value);

            // remember those that are in the "in-set"
            List<string> keepList = new List<string>();
            string summ = "";
            foreach (string s in summaryOrder.Keys)
            {
                keepList.Add(s);
                summ += s + "\n";
                if (summ.Length >= lengthLim) break;
            }
            // build the actual in-sequence summary
            summ = "";
            foreach (string s in definingSentences)
            {
                if (keepList.Contains(s))
                {
                    summ += s + "\n";
                    if (summ.Length >= lengthLim) break;
                }
            }

            return summ;
        }

        public double scoreToken(string token)
        {
            double v = 0;
            int id = token.GetHashCode();
            if (tokenData.ContainsKey(id)) v = tokenData[id].PageRank;
            return v;
        }

        public Dictionary<string, double> tokenRanks()
        {
            Dictionary<string, double> tkrs = new Dictionary<string, double>();
            foreach (int k in tokenData.Keys)
            {
                tkrs.Add(tokenData[k].token, tokenData[k].PageRank);
            }
            return tkrs;
        }

        public string createSummaryByRank(string text, int lengthLim)
        {
            defineRank(text);
            return summaryByRank(lengthLim);
        }

        public string createByOriginalSequence(string text, int lengthLim)
        {
            defineRank(text);
            return summaryByOriginalSequence(lengthLim);
        }

        public void defineRank(string text)
        {
            //Expects sentences
            // Will create a ranking after skipping stopwords

          List<string> sl = stoplist.ToLower().Split(' ').ToList<string>();
            

        // Create all the word tokens first
        foreach (Match match in SentRegex.Matches(text))
        {
            int i = match.Index;
            string s = match.Value;
            definingSentences.Add(s);

            string[] tokens = s.Split(' ');
            foreach (string t in tokens)
            {
                string word = t.ToLower();
                if (sl.Contains (word)) continue;
                int id = word.GetHashCode();
                if (!tokenData.ContainsKey(id))
                {
                    NodeData D = new NodeData ();
                    D.token = word;
                    tokenData.Add(id, D);
                }

            }
        }

        // Remove stoplist words
        foreach (string s in sl)
        {
            int id = s.GetHashCode();
            if (tokenData.ContainsKey(id))
            {
                tokenData.Remove(id);
            }
        }

        double startPageRank = 1.0 / tokenData.Count;

        /* Book -> aproaches 1 as lamda aproaches 1 */
        double PR = lamda / tokenData.Count;
        double Q = 1 - lamda;

        /* Wikipedia -> aproaches 1 as lamda aproaches 0 */
        //double PR = (1.0 - lamda) / siteNodes.Count;
        //double Q = lamda;

        /* Wikipedia -> aproaches N as lamda aproaches 0 */
        //double PR = (1.0 - lamda);
        //double Q = lamda

       //Add all the links between words in a sentence
        foreach (Match match in SentRegex.Matches(text))
        {
            int i = match.Index;
            string s = match.Value;
            string[] tokens = s.Split(' ');

            for (int j = 0; j < tokens.Length-1; j++)
            {
                string word1 = tokens[j].ToLower();
                if (!sl.Contains(word1))
                {
                    // Find next non-stop-list word
                    bool ifound = false;
                    for (int k = j + 1; k < tokens.Length && !ifound; k++)
                    {
                        string word2 = tokens[k].ToLower();
                        if (!sl.Contains(word2))
                        {
                            int id1 = word1.GetHashCode();
                            int id2 = word2.GetHashCode();
                            tokenData[id2].LinksIn.Add(id1);
                            tokenData[id1].LinksOutCount++;
                            ifound = true;
                        }
                    }
                }
            }
        }

        // Complete initialization
        foreach (int k in tokenData.Keys)
        {
            nodesToRank.Add(k);
            tokenData[k].PageRank = startPageRank;
        }

        // Page rank algorithm
        double MaxChange = 1;
        int iterations = 0;
        while ((MaxChange > changeLim)&&(iterations <maxIterations ))
        {
            //Console.WriteLine(MaxChange);
            MaxChange = 0;

            foreach (int id in nodesToRank)
            {
                double newPageRank = 0.0;

                // Add all the ranks of pages linking to the site
                foreach (int pageid in tokenData[id].LinksIn)
                {
                    newPageRank += (tokenData[pageid].PageRank / tokenData[pageid].LinksOutCount);
                }

                newPageRank = newPageRank * Q + PR; // Calculate new page rank

                double difference = Math.Abs(newPageRank - tokenData[id].PageRank);
                if (difference > MaxChange)
                {
                    MaxChange = difference;
                }

                tokenData[id].PageRank = newPageRank; //set the new rank to this document
            }
            iterations++;
        }
      }

    }
}
