using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Environment.Vacuum
{
    using Aima.Core.Agent;
    using Aima.Core.Agent.Impl;

    /// <summary>
    /// Artificial Intelligence A Modern Approach (3rd Edition): Figure 2.8, page 48.
    /// <code>
    /// function REFLEX-VACUUM-AGENT([location, status]) returns an action
    ///   if status = Dirty then return Suck
    ///   else if location = A then return Right
    ///   else if location = B then return Left
    /// </code>
    /// Figure 2.8 The agent program for a simple reflex agent in the two-state vacuum environment.
    /// This program implements the action function tabulated in Figure 2.3.
    /// </summary>
    public class ReflexVacuumAgent : AbstractAgent 
    {
        public ReflexVacuumAgent(): base(new ReflexVacuumAgentProgram())
        {
        }

        private class ReflexVacuumAgentProgram : IAgentProgram
        {
            // function REFLEX-VACUUM-AGENT([location, status]) returns an
            // action
            public IAction Execute(IPercept percept)
            {
                VacuumEnvPercept vep = (VacuumEnvPercept)percept;

                // if status = Dirty then return Suck
                if (VacuumEnvironment.LocationState.Dirty == vep
                        .GetLocationState())
                {
                    return VacuumEnvironment.ActionSuck;
                    // else if location = A then return Right
                }
                else if (VacuumEnvironment.LocationA == vep
                      .GetAgentLocation())
                {
                    return VacuumEnvironment.ActionMoveRight;
                }
                else if (VacuumEnvironment.LocationB == vep
                      .GetAgentLocation())
                {
                    // else if location = B then return Left
                    return VacuumEnvironment.ActionMoveLeft;
                }

                // Note: This should not be returned if the
                // environment is correct
                return NoOpAction.NoOp;
            }
        }
    }
}
