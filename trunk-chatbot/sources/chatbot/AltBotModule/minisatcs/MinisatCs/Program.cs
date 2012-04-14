using System;
using System.IO;
using System.Text;
using System.Diagnostics;
using System.Collections.Generic;
using Aima.Core.Logic.Propositional.Algorithms;
using Aima.Core.Logic.Propositional.Parsing;
using Aima.Core.Logic.Propositional.Parsing.AST;
using Aima.Core.Logic.Propositional.Visitors;

using MiniSatCS;

public class MiniSatCSSolver
{
    public  IndexedSymbolTable translator = new IndexedSymbolTable();
    public string solverReport = "";
    public bool wasSAT = false;

     string ReadWord(TextReader s)
    {
        StringBuilder sb = new StringBuilder();
        for (; ; )
        {
            int ch = s.Read();
            if (ch == -1) break;
            char c = (char)ch;
            if (char.IsWhiteSpace(c))
            {
                if (sb.Length > 0) break;
            }
            else
            {
                if (c == 'p' || c == 'c')
                {
                    do
                    {
                        ch = s.Read();
                    } while (ch != -1 && (char)ch != '\n');
                    if (sb.Length > 0) break;
                }
                else
                    sb.Append(c);
            }
        }

        if (sb.Length == 0) return null;
        else return sb.ToString();
    }
     static string ReadWordStatic(TextReader s)
     {
         StringBuilder sb = new StringBuilder();
         for (; ; )
         {
             int ch = s.Read();
             if (ch == -1) break;
             char c = (char)ch;
             if (char.IsWhiteSpace(c))
             {
                 if (sb.Length > 0) break;
             }
             else
             {
                 if (c == 'p' || c == 'c')
                 {
                     do
                     {
                         ch = s.Read();
                     } while (ch != -1 && (char)ch != '\n');
                     if (sb.Length > 0) break;
                 }
                 else
                     sb.Append(c);
             }
         }

         if (sb.Length == 0) return null;
         else return sb.ToString();
     }


    public  string solveIt(string AIMAproblem,out Model proposal)
    {
        string solution="";
        string DIMACSsolution = "";
        translator.Clear();

        proposal = new Model();
        string DIMACSproblem = translator.AIMAToDIMACS(AIMAproblem);

        StringReader sr = new StringReader(DIMACSproblem);
            
        vec<Solver.Lit> lits = new vec<Solver.Lit>();

        Solver S = new Solver();
        Solver.solverReport = "";

        for (; ; )
        {
            lits.clear();
            string w;
            while ((w = ReadWordStatic(sr)) != null)
            {
                if (w == "%") break;
                int parsed_lit = int.Parse(w);
                if (parsed_lit == 0)
                    break;
                int var = Math.Abs(parsed_lit) - 1;
                while (var >= S.nVars()) S.newVar();
                lits.push((parsed_lit > 0) ? new Solver.Lit(var) : ~new Solver.Lit(var));
            }
            if (w == null) break;
            S.addClause(lits);
        }
        S.verbosity = 1;
        S.solve();
        Solver.reportf(S.okay() ? "SATISFIABLE\n" : "UNSATISFIABLE\n");
        S.printStats();
        /*
            if (S.okay())
            {
                for (int i = 0; i < S.nVars(); i++)
                    if ((S.model[i] != Solver.lbool.Undef0) && (S.model[i] != Solver.lbool .Undef1))
                        Solver.reportf("{0}{1}\n", (S.model[i] == Solver.lbool.True) ? " " : "-", i + 1);
            }
        */
        wasSAT = S.okay();

        if (S.okay())
        {
            for (int i = 0; i < S.nVars(); i++)
                if ((S.model[i] != Solver.lbool.Undef0) && (S.model[i] != Solver.lbool.Undef1))
                    DIMACSsolution += String.Format("{0}{1} ", (S.model[i] == Solver.lbool.True) ? " " : "-", i + 1);
        }
        if (S.okay())
        {
            for (int i = 0; i < S.nVars(); i++)
                if ((S.model[i] != Solver.lbool.Undef0) && (S.model[i] != Solver.lbool.Undef1))
                {
                    //DIMACSsolution += String.Format("{0}{1} ", (S.model[i] == Solver.lbool.True) ? " " : "-", i + 1);
                    if (S.model[i] == Solver.lbool.True)
                    {
                        proposal = proposal.Extend(translator.varName(i+1), true);
                        Console.WriteLine("+{0}",translator.varName(i + 1));
                    }
                    else
                    {
                        proposal = proposal.Extend(translator.varName(i + 1), false);
                        //Console.WriteLine("-{0}", translator.varName(i + 1));
                    }
                }
        }
        solution = translator.DIMACSToAIMA(DIMACSsolution);
        solverReport = Solver.solverReport;
        return solution;
}

    public static void Main(string[] args)
    {
        //string test = "((A OR B) OR (NOT C))\n((NOT A) OR B) OR C)\n(B OR C)\n";
        //Model myModel = new Model();
        //string ans = solveIt(test,myModel);

        if (args.Length < 1)
        {
            Console.Error.WriteLine("usage: minisat [-s|-u] <file1.cnf> ...");
            return;
        }

        bool expect = false;
        bool expect_res = false;


        int pos = 0;
        if (args[pos] == "-s")
        {
            expect = true;
            expect_res = true;
            pos++;
        }
        if (args[pos] == "-u")
        {
            expect = true;
            expect_res = false;
            pos++;
        }

        for (; pos < args.Length; pos++)
        {
            StreamReader sr = File.OpenText(args[pos]);
            vec<Solver.Lit> lits = new vec<Solver.Lit>();

            Solver S = new Solver();

            for (; ; )
            {
                lits.clear();
                string w;
                while ((w = ReadWordStatic(sr)) != null)
                {
                    if (w == "%") break;
                    int parsed_lit = int.Parse(w);
                    if (parsed_lit == 0)
                        break;
                    int var = Math.Abs(parsed_lit) - 1;
                    while (var >= S.nVars()) S.newVar();
                    lits.push((parsed_lit > 0) ? new Solver.Lit(var) : ~new Solver.Lit(var));
                }
                if (w == null) break;
                S.addClause(lits);
            }

            if (expect)
            {
                S.verbosity = 0;
                S.solve();
                if (S.okay() == expect_res)
                    Solver.reportf(".");
                else
                    Solver.reportf("\nproblem: {0}\n", args[pos]);
            }
            else
            {
                S.verbosity = 1;
                S.solve();
                Solver.reportf(S.okay() ? "SATISFIABLE\n" : "UNSATISFIABLE\n");
                S.printStats();
            }



#if false
			if (S.okay()) {
				for (int i = 0; i < S.nVars(); i++)
					if (S.model[i] != l_Undef)
						Solver.reportf("{0}{1}\n", (S.model[i]==l_True)?" ":"-", i+1);
			}
#endif
        }
    }
} // class MiniSat

