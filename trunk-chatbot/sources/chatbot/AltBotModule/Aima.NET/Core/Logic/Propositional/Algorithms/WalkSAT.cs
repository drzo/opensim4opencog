using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Iesi.Collections.Generic;

namespace Aima.Core.Logic.Propositional.Algorithms
{
    using System.Collections;

    using Aima.Core.Logic.Propositional.Parsing;
    using Aima.Core.Logic.Propositional.Parsing.AST;
    using Aima.Core.Logic.Propositional.Visitors;
    using Aima.Core.Util;
    [Serializable]
    public class WalkSAT
    {
        private Model myModel;

        private Random random = new Random();

        public int trials = 0;

        public String lastReport = "";
        public IList<Sentence> lastClauses;
        public IList<Int64> lastClausesFlip;

        public Model FindModelFor(String logicalSentence, int numberOfFlips,
                double probabilityOfRandomWalk)
        {
            this.myModel = new Model();
            var s = (Sentence)new PEParser().Parse(logicalSentence);
            var transformer = new CNFTransformer();
            var clauseGatherer = new CNFClauseGatherer();
            var sc = new SymbolCollector();

            var symbols = sc.GetSymbolsIn(s).ToList();
            var r = new Random();
            foreach (var sym in symbols)
            {
                this.myModel = this.myModel.Extend(sym, Util.RandomBoolean());
            }

            IList<Sentence> clauses = clauseGatherer.GetClausesFrom(transformer.Transform(s)).ToList();
            //IList<Sentence> ClauseSet = new Converter<Sentence>().ListToSet(clauses);
            lastClauses = clauses;
            lastClausesFlip = new List<Int64>(clauses.Count+1);
            for (int i = 0; i < clauses.Count; i++) lastClausesFlip.Add(0);

            for (int i = 0; i < numberOfFlips; i++)
            {
                List<int> unsatList = new List<int>();
                int numSat = this.GetNumberOfClausesSatisfiedIn(new Converter<Sentence>().ListToSet(clauses), myModel, unsatList);
                if (numSat == clauses.Count)
                {
                    trials = i;
                    return myModel;
                }
                int probe = FindAnRandomUnsatisfiedClause(clauses, myModel,unsatList);
                //Sentence clause = clauses[random.Next(clauses.Count)];
                Sentence clause = clauses[probe];
                lastClausesFlip[probe]++;

                IList<Symbol> symbolsInClause = sc.GetSymbolsIn(clause).ToList();
                
                if (random.NextDouble() >= probabilityOfRandomWalk)
                {
                    Symbol randomSymbol = symbolsInClause[random.Next(symbolsInClause.Count)];
                    myModel = myModel.Flip(randomSymbol);
                }
                else
                {
                    Symbol symbolToFlip = this.GetSymbolWhoseFlipMaximisesSatisfiedClauses(
                            new Converter<Sentence>().ListToSet(clauses),
                            symbolsInClause, myModel);
                    myModel = myModel.Flip(symbolToFlip);
                }

            }
            trials = numberOfFlips;
            return null;
        }

        public Model FindModelFor(String logicalSentence, int numberOfFlips,
        double probabilityOfRandomWalk, Model initialModel)
        {
            this.myModel = new Model();
            var s = (Sentence)new PEParser().Parse(logicalSentence);
            var transformer = new CNFTransformer();
            var clauseGatherer = new CNFClauseGatherer();
            var sc = new SymbolCollector();

            var symbols = sc.GetSymbolsIn(s).ToList();
            var r = new Random();
            foreach (var sym in symbols)
            {
                if ((initialModel != null) && (r.NextDouble() < 0.96) && (!sym.ToString().Contains("act")) )
                {
                    try
                    {
                        bool? status = initialModel.GetStatus(sym);
                        if ((status == true)||(status == false))
                        {
                            this.myModel = this.myModel.Extend(sym, status);
                        }
                        else
                        {
                            this.myModel = this.myModel.Extend(sym, Util.RandomBoolean());
                        }
                    }
                    catch
                    {
                        this.myModel = this.myModel.Extend(sym, Util.RandomBoolean());
                    }
                }
                else
                {
                    this.myModel = this.myModel.Extend(sym, Util.RandomBoolean());

                }
            }

            IList<Sentence> clauses = clauseGatherer.GetClausesFrom(transformer.Transform(s)).ToList();
            lastClauses = clauses;
            lastClausesFlip = new List<Int64>(clauses.Count + 1);

            for (int i = 0; i < clauses.Count; i++) lastClausesFlip.Add(0);

            //IList<Sentence> ClauseSet = new Converter<Sentence>().ListToSet(clauses);

            for (int i = 0; i < numberOfFlips; i++)
            {
                List<int> unsatList = new List<int>();
                int numSat = this.GetNumberOfClausesSatisfiedIn(new Converter<Sentence>().ListToSet(clauses), myModel, unsatList);
                if (numSat == clauses.Count)
                {
                    trials = i;
                    return myModel;
                }
                int probe = FindAnRandomUnsatisfiedClause(clauses, myModel, unsatList);
                //Sentence clause = clauses[random.Next(clauses.Count)];
                Sentence clause = clauses[probe];
                lastClausesFlip[probe]++;

                IList<Symbol> symbolsInClause = sc.GetSymbolsIn(clause).ToList();

                if (random.NextDouble() >= probabilityOfRandomWalk)
                {
                    Symbol randomSymbol = symbolsInClause[random.Next(symbolsInClause.Count)];
                    myModel = myModel.Flip(randomSymbol);
                }
                else
                {
                    Symbol symbolToFlip = this.GetSymbolWhoseFlipMaximisesSatisfiedClauses(
                            new Converter<Sentence>().ListToSet(clauses),
                            symbolsInClause, myModel);
                    myModel = myModel.Flip(symbolToFlip);
                }

            }
            trials = numberOfFlips;
            return null;
        }
        private Symbol GetSymbolWhoseFlipMaximisesSatisfiedClauses(
                ISet<Sentence> clauses, IList<Symbol> symbols, Model model)
        {
            if (symbols.Count > 0)
            {
                Symbol retVal = symbols[0];
                int maxClausesSatisfied = 0;
                List<int> localUnsatList = new List<int>();
                foreach (Symbol sym in
                    symbols.Where(sym => this.GetNumberOfClausesSatisfiedIn(clauses, model.Flip(sym),localUnsatList) > maxClausesSatisfied))
                {
                    retVal = sym;
                    maxClausesSatisfied = this.GetNumberOfClausesSatisfiedIn(
                        clauses, model.Flip(sym), localUnsatList);
                }
                return retVal;
            }
            return null;
        }

        private int FindAnRandomUnsatisfiedClause(IList<Sentence> clauses, Model model, List<int> unsatList)
        {
            int probe = 0;
            int pl = 0;
            if (unsatList.Count == 0)
            {
                return random.Next(clauses.Count);
            }
            for (int i = 0; i < clauses.Count * 10; i++) 
            {
                pl = random.Next(unsatList.Count);
                probe = unsatList[pl];
                Sentence clause = clauses[probe];
                if (model.IsFalse(clause))
                {
                    return probe;
                }

            }
            return probe;
        }

        private int GetNumberOfClausesSatisfiedIn(ISet<Sentence> clauses, Model model, List<int> unsatList)
        {
            int retVal = 0;
            int probe =0;
            foreach(var s in clauses)
            {
                if (model.IsTrue(s))
                {
                    retVal += 1;
                }
                else
                {
                    unsatList.Add(probe);
                }
                probe++;
            }
            return retVal;
        }

        public string ExamineClauseStatistics(double threshold )
        {
            // Possibly useful in finding problem clauses
            if ((lastClauses == null) || (lastClausesFlip == null))
            {
                lastReport = "No Stats.";
                return lastReport;
            }
            double average = lastClausesFlip.Average();
            double max = lastClausesFlip.Max();
            double sum = lastClausesFlip.Sum();
            lastReport = "";
            lastReport += String.Format("Trials = {0}\n", trials);
            lastReport += String.Format("Average = {0}\n", average);
            lastReport += String.Format("Max = {0}\n", max);
            lastReport += String.Format("Sum = {0}\n", sum);
            lastReport += "Most Flipped\n";

            // Sort by the most flipped
            List <int> ranking= new List<int>(lastClauses.Count);
            for (int i = 0; i < lastClauses.Count; i++) {ranking.Add(i);}
            ranking.Sort(delegate(int p1,int p2){ return lastClausesFlip[p2].CompareTo(lastClausesFlip[p1]);} );

            for (int ix = 0; ix < lastClauses.Count; ix++)
            {
                int i = ranking[ix];
                long flips = lastClausesFlip[i];
                double p = flips / sum;
                if ((flips == max) || (p > threshold))
                {
                    lastReport += String.Format("{0}:{1} {2} {3}\n", i, myModel.decodeImplication(lastClauses[i]), flips, p);
                }
            }
            lastReport += "\n\nFlipped Implications\n";
            // Interested in true implications
            for (int ix = 0; ix < lastClauses.Count; ix++)
            {
                int i = ranking[ix];
                Sentence clause = lastClauses[i];
                long flips = lastClausesFlip[i];
                double p = flips / sum;
                if (flips > 0)
                {
                    lastReport += String.Format("{0}:{1}  :{2}\n", i, myModel.decodeImplication(lastClauses[i]), p);
                }
            }

            lastReport += "\n\nActive Implications\n";
            // Interested in true implications
            for (int ix = 0; ix < lastClauses.Count; ix++)
            {
                int i = ranking[ix];
                Sentence clause = lastClauses[i];
                long flips = lastClausesFlip[i];
                double p = flips / sum;
                if (myModel.IsActiveImplication(clause))
                {
                    lastReport += String.Format("{0}:{1}  :{2}\n", i, myModel.decodeImplication ( lastClauses[i]),p);
                }
            }

            lastReport += "\n\nFALSE Clauses\n";
            // Interested in true implications
            for (int ix = 0; ix < lastClauses.Count; ix++)
            {
                int i = ranking[ix];
                Sentence clause = lastClauses[i];
                long flips = lastClausesFlip[i];
                double p = flips / sum;
                if (myModel.IsFalse(clause))
                {
                    lastReport += String.Format("{0}:{1}  :{2}\n", i, myModel.decodeImplication(lastClauses[i]), p);
                }
            }



            return lastReport;
        }
    }
}
