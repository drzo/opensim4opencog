using System;
using System.Collections.Generic;
using System.Collections;
using System.Linq;
using System.Text;
using System.Text.RegularExpressions;

namespace MiniSatCS
{
    public class IndexedSymbolTable
    {
        public Dictionary<string, Int32> SymbolTable = new Dictionary<string, Int32>();
        public Dictionary<Int32,string> IndexTable = new Dictionary<Int32,string >();
        Int32 symbolCount = 0;
        Int32 clauseCount = 0;

        public IndexedSymbolTable()
        {
        }
        public void Clear()
        {
            symbolCount = 0;
            clauseCount = 0;
            SymbolTable.Clear();
        }

        public string varName(int var)
        {
            return IndexTable[var];
        }
        public string AIMAToDIMACS(string Clauses)
        {
            // Convert "((A OR -B) OR C)" into "1 -2 3"

            string outputline = "";
            string [] lines = Clauses.Split('\n');
            foreach (string line in lines)
            {
                string[] tokens = Regex.Split(line, @"\W+");
                foreach (string token in tokens)
                {
                    if (token == "") continue;
                    if ((!token.ToLower().Equals("not")) && (!token.ToLower().Equals("or")))
                    {
                        if( !SymbolTable.ContainsKey(token) )
                        {
                            SymbolTable.Add(token, ++symbolCount);
                            IndexTable[symbolCount]=token;
                        }
                    }
                }
                int tokcount = 0;
                foreach (string token in tokens)
                {
                    if (token == "") { continue; }

                    if (token.ToLower().Equals("not"))
                    {
                        outputline += "-";
                    }
                    else
                    {
                        if (token.ToLower().Equals("or"))
                        {
                            outputline += " ";
                        }
                        else
                        {
                          outputline += SymbolTable[token].ToString();
                          tokcount++;
                        }
                    }
                }
                if (tokcount > 0)
                {
                    outputline += " 0\n";
                    clauseCount++;
                 }
            }

            return String.Format("p cnf {0} {1}\n{2}",symbolCount,clauseCount ,outputline);
        }

        public string DIMACSToAIMA(string CNF)
        {
            string outputline = "";
            string[] variables = CNF.Split(' ');
            Int32 tokenCount = 0;
            foreach (string var in variables)
            {
                if (var == "") continue;
                Int32 varv = Int32.Parse(var);
                Int32 varp = Math.Abs(varv);
                string symbol = "";
                tokenCount++;
                if (varv < 0)
                {
                  symbol = "(NOT "+ IndexTable[varp]+")";
                }
                else
                {
                    symbol = IndexTable[varp];
                }
                if (tokenCount > 1)
                {
                    outputline = String.Format("( {0} AND {1} \n )", outputline, symbol); 
                }
                else
                {
                    outputline = symbol;
                }
            }
            return outputline;
        }
    }
}
