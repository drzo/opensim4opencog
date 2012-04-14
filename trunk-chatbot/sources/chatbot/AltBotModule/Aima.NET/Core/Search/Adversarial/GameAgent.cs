using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Search.Adversarial
{
    using Aima.Core.Agent.Impl;

    public class GameAgent : AbstractAgent 
    {
        private Game game;

        public GameAgent(Game g) {
            this.game = g;
        }

        public void MakeMiniMaxMove() {
            game.MakeMiniMaxMove();
        }

        public void MakeAlphaBetaMove() {
            game.MakeAlphaBetaMove();
        }

    }
}
