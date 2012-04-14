using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Environment.CellWorld
{
    using Aima.Core.Probability;
    using Aima.Core.Probability.Decision;
    using Aima.Core.Util.Datastructure;

    public class CellWorld : IMDPSource<CellWorldPosition, String> {
        public static readonly String LEFT = "left";

        public static readonly String RIGHT = "right";

        public static readonly String UP = "up";

        public static readonly String DOWN = "down";

        public static readonly String NO_OP = "no_op";

        IList<Cell> blockedCells, allCells;

        private int numberOfRows;

        private int numberOfColumns;

        private IList<Cell> terminalStates;

        private Cell initialState;

        public CellWorld(int numberOfRows, int numberOfColumns, double initialReward) 
        {
            allCells = new List<Cell>();
            blockedCells = new List<Cell>();

            terminalStates = new List<Cell>();

            this.numberOfRows = numberOfRows;
            this.numberOfColumns = numberOfColumns;

            for (var row = 1; row <= numberOfRows; row++) {
                for (var col = 1; col <= numberOfColumns; col++) {
                    allCells.Add(new Cell(row, col, initialReward));
                }
            }

            //TODO: does this need to be hardcoded?
            initialState = GetCellAt(1, 4);
        }

        public void MarkBlocked(int i, int j) {
            blockedCells.Add(GetCellAt(i, j));

        }

        private bool IsBlocked(int i, int j) 
        {
            if ((i < 1) || (i > numberOfRows) || (j < 1) || (j > numberOfColumns)) 
            {
                return true;
            }
            return blockedCells.Any(cell => cell.X == i && cell.Y == j);
        }

        private Cell GetCellAt(int i, int j)
        {
            var cell = allCells.Where(c => c.X == i && c.Y == j).First();
            if (cell != null)
            {
                return cell;
            }
            throw new ArgumentOutOfRangeException("No Cell found at " + i + " , " + j);
        }

        public CellWorldPosition MoveProbabilisticallyFrom(int i, int j,
                string direction, IRandomizer r) 
        {
            var c = GetCellAt(i, j);
            if (terminalStates.Contains(c)) 
            {
                return c.Position();
            }
            return MoveFrom(i, j, DetermineDirectionOfActualMovement(direction, r));

        }

        private CellWorldPosition MoveFrom(int i, int j, String direction) {
            //TODO: could enum be used here rather than string constants?
            if (direction.Equals(LEFT)) {
                return this.MoveLeftFrom(i, j);
            }
            if (direction.Equals(RIGHT)) {
                return this.MoveRightFrom(i, j);
            }
            if (direction.Equals(UP)) {
                return this.MoveUpFrom(i, j);
            }
            if (direction.Equals(DOWN)) {
                return this.MoveDownFrom(i, j);
            }
            throw new ArgumentOutOfRangeException("Unable to move " + direction + " from " + i
                    + " , " + j);
        }

        private CellWorldPosition MoveFrom(CellWorldPosition startingPosition,
                String direction) {
            return this.MoveFrom(startingPosition.X, startingPosition.Y,
                    direction);
        }

        private String DetermineDirectionOfActualMovement(
                String commandedDirection, double prob) 
        {
            if (prob < 0.8) {
                return commandedDirection;
            } else if ((prob > 0.8) && (prob < 0.9)) {
                if ((commandedDirection.Equals(LEFT))
                        || (commandedDirection.Equals(RIGHT))) {
                    return UP;
                }
                if ((commandedDirection.Equals(UP))
                        || (commandedDirection.Equals(DOWN))) {
                    return LEFT;
                }
            } else { // 0.9 < prob < 1.0
                if ((commandedDirection.Equals(LEFT))
                        || (commandedDirection.Equals(RIGHT))) {
                    return DOWN;
                }
                if ((commandedDirection.Equals(UP))
                        || (commandedDirection.Equals(DOWN))) {
                    return RIGHT;
                }
            }
            throw new ArgumentException(
                    "Unable to determine direction when command =  "
                            + commandedDirection + " and probability = " + prob);

        }

        private String DetermineDirectionOfActualMovement(
                String commandedDirection, IRandomizer r) {
            return DetermineDirectionOfActualMovement(commandedDirection, r
                    .NextDouble());

        }

        private CellWorldPosition MoveLeftFrom(int i, int j) {
            if (IsBlocked(i, j - 1)) {
                return new CellWorldPosition(i, j);
            }
            return new CellWorldPosition(i, j - 1);
        }

        private CellWorldPosition MoveRightFrom(int i, int j) {
            if (IsBlocked(i, j + 1)) {
                return new CellWorldPosition(i, j);
            }
            return new CellWorldPosition(i, j + 1);
        }

        private CellWorldPosition MoveUpFrom(int i, int j) {
            if (IsBlocked(i + 1, j)) {
                return new CellWorldPosition(i, j);
            }
            return new CellWorldPosition(i + 1, j);
        }

        private CellWorldPosition MoveDownFrom(int i, int j) {
            if (IsBlocked(i - 1, j)) {
                return new CellWorldPosition(i, j);
            }
            return new CellWorldPosition(i - 1, j);
        }

        public void SetReward(int i, int j, double reward) {
            var c = GetCellAt(i, j);
            c.Reward = reward;
        }

        public IList<Cell> UnblockedCells() 
        {
            return this.allCells.Where(c => !(this.blockedCells.Contains(c))).ToList();
        }

        public bool IsBlocked(Pair<int, int> p) 
        {
            return IsBlocked(p.GetFirst(), p.GetSecond());
        }

        // what is the probability of starting from position p1 taking action a and
        // reaaching position p2
        // method is public ONLY for testing do not use in client code.
        public double GetTransitionProbability(CellWorldPosition startingPosition,
                string actionDesired, CellWorldPosition endingPosition) {

            var firstRightAngledAction = this.DetermineDirectionOfActualMovement(
                    actionDesired, 0.85);
            var secondRightAngledAction = this.DetermineDirectionOfActualMovement(
                    actionDesired, 0.95);

            var actionsToPositions = new Dictionary<string, CellWorldPosition>();
            actionsToPositions[actionDesired] = this.MoveFrom(startingPosition,
                    actionDesired);
            actionsToPositions[firstRightAngledAction] = this.MoveFrom(
                    startingPosition, firstRightAngledAction);
            actionsToPositions[secondRightAngledAction] = this.MoveFrom(
                    startingPosition, secondRightAngledAction);

            var positionsToProbability = new Dictionary<CellWorldPosition, double>();
            foreach (var p in actionsToPositions.Values) {
                positionsToProbability[p] = 0.0;
            }

            foreach (var action in actionsToPositions.Keys) 
            {
                var position = actionsToPositions[action];
                var value = positionsToProbability[position];
                if (action.Equals(actionDesired)) {
                    positionsToProbability[position] = value + 0.8;
                } else { // right angled steps
                    positionsToProbability[position] = value + 0.1;
                }

            }

            if (positionsToProbability.Keys.Contains(endingPosition)) {
                return positionsToProbability[endingPosition];
            } else {
                return 0.0;
            }

        }

        public MDPTransitionModel<CellWorldPosition, String> GetTransitionModel() {
            IList<CellWorldPosition> terminalPositions = new List<CellWorldPosition>();
            foreach (Cell tc in terminalStates) {
                terminalPositions.Add(tc.Position());
            }
            var mtm = new MDPTransitionModel<CellWorldPosition, String>(
                    terminalPositions);

            IList<String> actions = new String[] { UP, DOWN, LEFT,
                    RIGHT };

            foreach (var startingPosition in this.GetNonFinalStates()) 
            {
                foreach (var actionDesired in actions) 
                {
                    foreach (var target in this.UnblockedCells()) 
                    { // too much work? should
                        // just cycle through
                        // neighbouring cells
                        // instead of all cells.
                        //TODO: Optimize to just go through neighbouring cells
                        var endingPosition = target.Position();
                        var transitionProbability = this.GetTransitionProbability(
                                startingPosition, actionDesired, endingPosition);
                        if (transitionProbability != 0.0) {

                            mtm.SetTransitionProbability(startingPosition,
                                    actionDesired, endingPosition,
                                    transitionProbability);
                        }
                    }
                }
            }
            return mtm;
        }

        public MDPRewardFunction<CellWorldPosition> GetRewardFunction() {

            MDPRewardFunction<CellWorldPosition> result = new MDPRewardFunction<CellWorldPosition>();
            foreach (Cell c in this.UnblockedCells()) {
                var pos = c.Position();
                var reward = c.Reward;
                result.SetReward(pos, reward);
            }

            return result;
        }

        public IList<CellWorldPosition> UnblockedPositions() 
        {
            return this.UnblockedCells().Select(c => c.Position()).ToList();
        }

        public MDP<CellWorldPosition, String> AsMdp() {

            return new MDP<CellWorldPosition, String>(this);
        }

        public IList<CellWorldPosition> GetNonFinalStates() 
        {
            // TODO: why (2,4) and (3,4) cells are hardcoded here?
            var nonFinalPositions = this.UnblockedPositions();
            nonFinalPositions.Remove(this.GetCellAt(2, 4).Position());
            nonFinalPositions.Remove(this.GetCellAt(3, 4).Position());
            return nonFinalPositions;
        }

        public IList<CellWorldPosition> GetFinalStates() 
        {
            // TODO: why (2,4) and (3,4) cells are hardcoded here?
            IList<CellWorldPosition> finalPositions = new List<CellWorldPosition>();
            finalPositions.Add(this.GetCellAt(2, 4).Position());
            finalPositions.Add(this.GetCellAt(3, 4).Position());
            return finalPositions;
        }

        public void SetTerminalState(int i, int j) 
        {
            this.SetTerminalState(new CellWorldPosition(i, j));

        }

        public void SetTerminalState(CellWorldPosition position) 
        {
            terminalStates.Add(GetCellAt(position.X, position.Y));

        }

        public CellWorldPosition GetInitialState() {
            return initialState.Position();
        }

        public MDPPerception<CellWorldPosition> Execute(CellWorldPosition position, string action, IRandomizer r)
        {
            var pos = this.MoveProbabilisticallyFrom(position.X, position.Y, action, r);
            double reward = this.GetCellAt(pos.X, pos.Y).Reward;
            return new MDPPerception<CellWorldPosition>(pos, reward);
        }

        public IList<String> GetAllActions() {

            return new String[] { LEFT, RIGHT, UP, DOWN };
        }
    }

}
