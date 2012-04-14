using System;

namespace Aima.Core.Agent.Impl
{
    public abstract class AbstractAgent : IAgent 
    {
        protected IAgentProgram program;

        private bool alive = true;

        public AbstractAgent()
        {
        }

        public AbstractAgent(IAgentProgram aProgram) {
            program = aProgram;
        }

        public bool Alive
        {
            get
            {
                return this.alive;
            }
            set
            {
                this.alive = value;
            }
        }

        public virtual IAction Execute(IPercept p)
        {
            if (this.program != null)
            {
                return this.program.Execute(p);
            }
            return NoOpAction.NoOp;
        }
    }
}
