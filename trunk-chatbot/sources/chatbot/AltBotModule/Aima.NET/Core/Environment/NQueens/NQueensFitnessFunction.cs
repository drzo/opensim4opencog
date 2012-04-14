using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Iesi.Collections.Generic;

namespace Aima.Core.Environment.NQueens
{
    using System.Diagnostics;

    using Aima.Core.Search.Framework;
    using Aima.Core.Search.Local;
    using Aima.Core.Util.Datastructure;

    /// <summary>
    /// A class whose purpose is to evaluate the fitness of NQueen individuals
    /// and to provide utility methods for translating between an NQueensBoard
    /// representation and the String representation used by the GeneticAlgorithm.
    /// </summary>
    public class NQueensFitnessFunction : IFitnessFunction, IGoalTest 
    {

        private readonly NQueensGoalTest goalTest = new NQueensGoalTest();

        public double GetValue(string individual) 
        {
            double fitness = 0;

            var board = this.GetBoardForIndividual(individual);
            var boardSize = board.Size;

            // Calculate the number of non-attacking pairs of queens (refer to AIMA
            // page 117).
            var qPositions = board.GetQueenPositions();
            for (var fromX = 0; fromX < (boardSize - 1); fromX++)
            {
                for (var toX = fromX + 1; toX < boardSize; toX++)
                {
                    var fromY = qPositions[fromX].YCoordinate;
                    var nonAttackingPair = true;

                    // Check right beside
                    var toY = fromY;
                    if (board.QueenExistsAt(new XYLocation(toX, toY)))
                    {
                        nonAttackingPair = false;
                    }

                    // Check right and above
                    toY = fromY - (toX - fromX);
                    if (toY >= 0)
                    {
                        if (board.QueenExistsAt(new XYLocation(toX, toY)))
                        {
                            nonAttackingPair = false;
                        }
                    }

                    // Check right and below
                    toY = fromY + (toX - fromX);
                    if (toY < boardSize)
                    {
                        if (board.QueenExistsAt(new XYLocation(toX, toY)))
                        {
                            nonAttackingPair = false;
                        }
                    }

                    if (nonAttackingPair)
                    {
                        fitness += 1.0;
                    }
                }
            }

            return fitness;
        }

        public bool IsGoalState(object state)
        {
            return this.goalTest.IsGoalState(this.GetBoardForIndividual((string)state));
        }

        public NQueensBoard GetBoardForIndividual(string individual)
        {
            var boardSize = individual.Length;
            var board = new NQueensBoard(boardSize);
            for (var i = 0; i < boardSize; i++)
            {
                var pos = Int32.Parse(individual.Substring(i, 1));
                if (pos > boardSize)
                {
                    pos = -1;
                }

                board.AddQueenAt(new XYLocation(i, pos));
            }

            return board;
        }

        public string GenerateRandomIndividual(int boardSize)
        {
            StringBuilder ind = new StringBuilder();

            Debug.Assert(boardSize >= 1 && boardSize <= 9, "invalid board size");

            for (var i = 0; i < boardSize; i++)
            {
                ind.Append(new Random().Next(boardSize));
            }

            return ind.ToString();
        }

        public ISet<char> GetFiniteAlphabetForBoardOfSize(int size)
        {
            ISet<char> fab = new HashedSet<char>();

            Debug.Assert(size >= 1 && size <= 9, "invalid board size");

            for (var i = 0; i < size; i++)
            {
                fab.Add((char)i);
            }

            return fab;
        }
    }
}
