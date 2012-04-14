using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Environment.Vacuum
{
    using Aima.Core.Agent;
    using Aima.Core.Agent.Impl;
    using Aima.Core.Agent.Impl.AProg;

    /// <summary>
    /// Artificial Intelligence A Modern Approach (3rd Edition): Figure 2.3, page 36.
    /// Figure 2.3 Partial tabulation of a simple agent function for the vacuum-cleaner world
    /// shown in Figure 2.2.
    /// </summary>
    public class TableDrivenVacuumAgent : AbstractAgent 
    {
        public TableDrivenVacuumAgent()
            : base(new TableDrivenAgentProgram(GetPerceptSequenceActions()))
        {
        }

        private static Dictionary<IList<IPercept>, IAction> GetPerceptSequenceActions() 
        {
            var perceptSequenceActions = new Dictionary<IList<IPercept>, IAction>();

            // NOTE: While this particular table could be setup simply
            // using a few loops, the intent is to show how quickly a table
            // based approach grows and becomes unusable.
            IList<IPercept> ps;
            //
            // Level 1: 4 states
            ps = CreatePerceptSequence(new VacuumEnvPercept(
                    VacuumEnvironment.LocationA,
                    VacuumEnvironment.LocationState.Clean));
            perceptSequenceActions[ps] = VacuumEnvironment.ActionMoveRight;
            ps = CreatePerceptSequence(new VacuumEnvPercept(
                    VacuumEnvironment.LocationA,
                    VacuumEnvironment.LocationState.Dirty));
            perceptSequenceActions[ps] = VacuumEnvironment.ActionSuck;
            ps = CreatePerceptSequence(new VacuumEnvPercept(
                    VacuumEnvironment.LocationB,
                    VacuumEnvironment.LocationState.Clean));
            perceptSequenceActions[ps] = VacuumEnvironment.ActionMoveLeft;
            ps = CreatePerceptSequence(new VacuumEnvPercept(
                    VacuumEnvironment.LocationB,
                    VacuumEnvironment.LocationState.Dirty));
            perceptSequenceActions[ps] = VacuumEnvironment.ActionSuck;

            //
            // Level 2: 4x4 states
            // 1
            ps = CreatePerceptSequence(new VacuumEnvPercept(
                    VacuumEnvironment.LocationA,
                    VacuumEnvironment.LocationState.Clean), new VacuumEnvPercept(
                    VacuumEnvironment.LocationA,
                    VacuumEnvironment.LocationState.Clean));
            perceptSequenceActions[ps] = VacuumEnvironment.ActionMoveRight;
            ps = CreatePerceptSequence(new VacuumEnvPercept(
                    VacuumEnvironment.LocationA,
                    VacuumEnvironment.LocationState.Clean), new VacuumEnvPercept(
                    VacuumEnvironment.LocationA,
                    VacuumEnvironment.LocationState.Dirty));
            perceptSequenceActions[ps] = VacuumEnvironment.ActionSuck;
            ps = CreatePerceptSequence(new VacuumEnvPercept(
                    VacuumEnvironment.LocationA,
                    VacuumEnvironment.LocationState.Clean), new VacuumEnvPercept(
                    VacuumEnvironment.LocationB,
                    VacuumEnvironment.LocationState.Clean));
            perceptSequenceActions[ps] = VacuumEnvironment.ActionMoveLeft;
            ps = CreatePerceptSequence(new VacuumEnvPercept(
                    VacuumEnvironment.LocationA,
                    VacuumEnvironment.LocationState.Clean), new VacuumEnvPercept(
                    VacuumEnvironment.LocationB,
                    VacuumEnvironment.LocationState.Dirty));
            perceptSequenceActions[ps] = VacuumEnvironment.ActionSuck;
            // 2
            ps = CreatePerceptSequence(new VacuumEnvPercept(
                    VacuumEnvironment.LocationA,
                    VacuumEnvironment.LocationState.Dirty), new VacuumEnvPercept(
                    VacuumEnvironment.LocationA,
                    VacuumEnvironment.LocationState.Clean));
            perceptSequenceActions[ps] = VacuumEnvironment.ActionMoveRight;
            ps = CreatePerceptSequence(new VacuumEnvPercept(
                    VacuumEnvironment.LocationA,
                    VacuumEnvironment.LocationState.Dirty), new VacuumEnvPercept(
                    VacuumEnvironment.LocationA,
                    VacuumEnvironment.LocationState.Dirty));
            perceptSequenceActions[ps] = VacuumEnvironment.ActionSuck;
            ps = CreatePerceptSequence(new VacuumEnvPercept(
                    VacuumEnvironment.LocationA,
                    VacuumEnvironment.LocationState.Dirty), new VacuumEnvPercept(
                    VacuumEnvironment.LocationB,
                    VacuumEnvironment.LocationState.Clean));
            perceptSequenceActions[ps] = VacuumEnvironment.ActionMoveLeft;
            ps = CreatePerceptSequence(new VacuumEnvPercept(
                    VacuumEnvironment.LocationA,
                    VacuumEnvironment.LocationState.Dirty), new VacuumEnvPercept(
                    VacuumEnvironment.LocationB,
                    VacuumEnvironment.LocationState.Dirty));
            perceptSequenceActions[ps] = VacuumEnvironment.ActionSuck;
            // 3
            ps = CreatePerceptSequence(new VacuumEnvPercept(
                    VacuumEnvironment.LocationB,
                    VacuumEnvironment.LocationState.Clean), new VacuumEnvPercept(
                    VacuumEnvironment.LocationA,
                    VacuumEnvironment.LocationState.Clean));
            perceptSequenceActions[ps] = VacuumEnvironment.ActionMoveRight;
            ps = CreatePerceptSequence(new VacuumEnvPercept(
                    VacuumEnvironment.LocationB,
                    VacuumEnvironment.LocationState.Clean), new VacuumEnvPercept(
                    VacuumEnvironment.LocationA,
                    VacuumEnvironment.LocationState.Dirty));
            perceptSequenceActions[ps] = VacuumEnvironment.ActionSuck;
            ps = CreatePerceptSequence(new VacuumEnvPercept(
                    VacuumEnvironment.LocationB,
                    VacuumEnvironment.LocationState.Clean), new VacuumEnvPercept(
                    VacuumEnvironment.LocationB,
                    VacuumEnvironment.LocationState.Clean));
            perceptSequenceActions[ps] = VacuumEnvironment.ActionMoveLeft;
            ps = CreatePerceptSequence(new VacuumEnvPercept(
                    VacuumEnvironment.LocationB,
                    VacuumEnvironment.LocationState.Clean), new VacuumEnvPercept(
                    VacuumEnvironment.LocationB,
                    VacuumEnvironment.LocationState.Dirty));
            perceptSequenceActions[ps] = VacuumEnvironment.ActionSuck;
            // 4
            ps = CreatePerceptSequence(new VacuumEnvPercept(
                    VacuumEnvironment.LocationB,
                    VacuumEnvironment.LocationState.Dirty), new VacuumEnvPercept(
                    VacuumEnvironment.LocationA,
                    VacuumEnvironment.LocationState.Clean));
            perceptSequenceActions[ps] = VacuumEnvironment.ActionMoveRight;
            ps = CreatePerceptSequence(new VacuumEnvPercept(
                    VacuumEnvironment.LocationB,
                    VacuumEnvironment.LocationState.Dirty), new VacuumEnvPercept(
                    VacuumEnvironment.LocationA,
                    VacuumEnvironment.LocationState.Dirty));
            perceptSequenceActions[ps] = VacuumEnvironment.ActionSuck;
            ps = CreatePerceptSequence(new VacuumEnvPercept(
                    VacuumEnvironment.LocationB,
                    VacuumEnvironment.LocationState.Dirty), new VacuumEnvPercept(
                    VacuumEnvironment.LocationB,
                    VacuumEnvironment.LocationState.Clean));
            perceptSequenceActions[ps] = VacuumEnvironment.ActionMoveLeft;
            ps = CreatePerceptSequence(new VacuumEnvPercept(
                    VacuumEnvironment.LocationB,
                    VacuumEnvironment.LocationState.Dirty), new VacuumEnvPercept(
                    VacuumEnvironment.LocationB,
                    VacuumEnvironment.LocationState.Dirty));
            perceptSequenceActions[ps] = VacuumEnvironment.ActionSuck;

            //
            // Level 3: 4x4x4 states
            // 1-1
            ps = CreatePerceptSequence(new VacuumEnvPercept(
                    VacuumEnvironment.LocationA,
                    VacuumEnvironment.LocationState.Clean), new VacuumEnvPercept(
                    VacuumEnvironment.LocationA,
                    VacuumEnvironment.LocationState.Clean), new VacuumEnvPercept(
                    VacuumEnvironment.LocationA,
                    VacuumEnvironment.LocationState.Clean));
            perceptSequenceActions[ps] = VacuumEnvironment.ActionMoveRight;
            ps = CreatePerceptSequence(new VacuumEnvPercept(
                    VacuumEnvironment.LocationA,
                    VacuumEnvironment.LocationState.Clean), new VacuumEnvPercept(
                    VacuumEnvironment.LocationA,
                    VacuumEnvironment.LocationState.Clean), new VacuumEnvPercept(
                    VacuumEnvironment.LocationA,
                    VacuumEnvironment.LocationState.Dirty));
            perceptSequenceActions[ps] = VacuumEnvironment.ActionSuck;
            ps = CreatePerceptSequence(new VacuumEnvPercept(
                    VacuumEnvironment.LocationA,
                    VacuumEnvironment.LocationState.Clean), new VacuumEnvPercept(
                    VacuumEnvironment.LocationA,
                    VacuumEnvironment.LocationState.Clean), new VacuumEnvPercept(
                    VacuumEnvironment.LocationB,
                    VacuumEnvironment.LocationState.Clean));
            perceptSequenceActions[ps] = VacuumEnvironment.ActionMoveLeft;
            ps = CreatePerceptSequence(new VacuumEnvPercept(
                    VacuumEnvironment.LocationA,
                    VacuumEnvironment.LocationState.Clean), new VacuumEnvPercept(
                    VacuumEnvironment.LocationA,
                    VacuumEnvironment.LocationState.Clean), new VacuumEnvPercept(
                    VacuumEnvironment.LocationB,
                    VacuumEnvironment.LocationState.Dirty));
            perceptSequenceActions[ps] = VacuumEnvironment.ActionSuck;
            // 1-2
            ps = CreatePerceptSequence(new VacuumEnvPercept(
                    VacuumEnvironment.LocationA,
                    VacuumEnvironment.LocationState.Clean), new VacuumEnvPercept(
                    VacuumEnvironment.LocationA,
                    VacuumEnvironment.LocationState.Dirty), new VacuumEnvPercept(
                    VacuumEnvironment.LocationA,
                    VacuumEnvironment.LocationState.Clean));
            perceptSequenceActions[ps] = VacuumEnvironment.ActionMoveRight;
            ps = CreatePerceptSequence(new VacuumEnvPercept(
                    VacuumEnvironment.LocationA,
                    VacuumEnvironment.LocationState.Clean), new VacuumEnvPercept(
                    VacuumEnvironment.LocationA,
                    VacuumEnvironment.LocationState.Dirty), new VacuumEnvPercept(
                    VacuumEnvironment.LocationA,
                    VacuumEnvironment.LocationState.Dirty));
            perceptSequenceActions[ps] = VacuumEnvironment.ActionSuck;
            ps = CreatePerceptSequence(new VacuumEnvPercept(
                    VacuumEnvironment.LocationA,
                    VacuumEnvironment.LocationState.Clean), new VacuumEnvPercept(
                    VacuumEnvironment.LocationA,
                    VacuumEnvironment.LocationState.Dirty), new VacuumEnvPercept(
                    VacuumEnvironment.LocationB,
                    VacuumEnvironment.LocationState.Clean));
            perceptSequenceActions[ps] = VacuumEnvironment.ActionMoveLeft;
            ps = CreatePerceptSequence(new VacuumEnvPercept(
                    VacuumEnvironment.LocationA,
                    VacuumEnvironment.LocationState.Clean), new VacuumEnvPercept(
                    VacuumEnvironment.LocationA,
                    VacuumEnvironment.LocationState.Dirty), new VacuumEnvPercept(
                    VacuumEnvironment.LocationB,
                    VacuumEnvironment.LocationState.Dirty));
            perceptSequenceActions[ps] = VacuumEnvironment.ActionSuck;
            // 1-3
            ps = CreatePerceptSequence(new VacuumEnvPercept(
                    VacuumEnvironment.LocationA,
                    VacuumEnvironment.LocationState.Clean), new VacuumEnvPercept(
                    VacuumEnvironment.LocationB,
                    VacuumEnvironment.LocationState.Clean), new VacuumEnvPercept(
                    VacuumEnvironment.LocationA,
                    VacuumEnvironment.LocationState.Clean));
            perceptSequenceActions[ps] = VacuumEnvironment.ActionMoveRight;
            ps = CreatePerceptSequence(new VacuumEnvPercept(
                    VacuumEnvironment.LocationA,
                    VacuumEnvironment.LocationState.Clean), new VacuumEnvPercept(
                    VacuumEnvironment.LocationB,
                    VacuumEnvironment.LocationState.Clean), new VacuumEnvPercept(
                    VacuumEnvironment.LocationA,
                    VacuumEnvironment.LocationState.Dirty));
            perceptSequenceActions[ps] = VacuumEnvironment.ActionSuck;
            ps = CreatePerceptSequence(new VacuumEnvPercept(
                    VacuumEnvironment.LocationA,
                    VacuumEnvironment.LocationState.Clean), new VacuumEnvPercept(
                    VacuumEnvironment.LocationB,
                    VacuumEnvironment.LocationState.Clean), new VacuumEnvPercept(
                    VacuumEnvironment.LocationB,
                    VacuumEnvironment.LocationState.Clean));
            perceptSequenceActions[ps] = VacuumEnvironment.ActionMoveLeft;
            ps = CreatePerceptSequence(new VacuumEnvPercept(
                    VacuumEnvironment.LocationA,
                    VacuumEnvironment.LocationState.Clean), new VacuumEnvPercept(
                    VacuumEnvironment.LocationB,
                    VacuumEnvironment.LocationState.Clean), new VacuumEnvPercept(
                    VacuumEnvironment.LocationB,
                    VacuumEnvironment.LocationState.Dirty));
            perceptSequenceActions[ps] = VacuumEnvironment.ActionSuck;
            // 1-4
            ps = CreatePerceptSequence(new VacuumEnvPercept(
                    VacuumEnvironment.LocationA,
                    VacuumEnvironment.LocationState.Clean), new VacuumEnvPercept(
                    VacuumEnvironment.LocationB,
                    VacuumEnvironment.LocationState.Dirty), new VacuumEnvPercept(
                    VacuumEnvironment.LocationA,
                    VacuumEnvironment.LocationState.Clean));
            perceptSequenceActions[ps] = VacuumEnvironment.ActionMoveRight;
            ps = CreatePerceptSequence(new VacuumEnvPercept(
                    VacuumEnvironment.LocationA,
                    VacuumEnvironment.LocationState.Clean), new VacuumEnvPercept(
                    VacuumEnvironment.LocationB,
                    VacuumEnvironment.LocationState.Dirty), new VacuumEnvPercept(
                    VacuumEnvironment.LocationA,
                    VacuumEnvironment.LocationState.Dirty));
            perceptSequenceActions[ps] = VacuumEnvironment.ActionSuck;
            ps = CreatePerceptSequence(new VacuumEnvPercept(
                    VacuumEnvironment.LocationA,
                    VacuumEnvironment.LocationState.Clean), new VacuumEnvPercept(
                    VacuumEnvironment.LocationB,
                    VacuumEnvironment.LocationState.Dirty), new VacuumEnvPercept(
                    VacuumEnvironment.LocationB,
                    VacuumEnvironment.LocationState.Clean));
            perceptSequenceActions[ps] = VacuumEnvironment.ActionMoveLeft;
            ps = CreatePerceptSequence(new VacuumEnvPercept(
                    VacuumEnvironment.LocationA,
                    VacuumEnvironment.LocationState.Clean), new VacuumEnvPercept(
                    VacuumEnvironment.LocationB,
                    VacuumEnvironment.LocationState.Dirty), new VacuumEnvPercept(
                    VacuumEnvironment.LocationB,
                    VacuumEnvironment.LocationState.Dirty));
            perceptSequenceActions[ps] = VacuumEnvironment.ActionSuck;
            // 2-1
            ps = CreatePerceptSequence(new VacuumEnvPercept(
                    VacuumEnvironment.LocationA,
                    VacuumEnvironment.LocationState.Dirty), new VacuumEnvPercept(
                    VacuumEnvironment.LocationA,
                    VacuumEnvironment.LocationState.Clean), new VacuumEnvPercept(
                    VacuumEnvironment.LocationA,
                    VacuumEnvironment.LocationState.Clean));
            perceptSequenceActions[ps] = VacuumEnvironment.ActionMoveRight;
            ps = CreatePerceptSequence(new VacuumEnvPercept(
                    VacuumEnvironment.LocationA,
                    VacuumEnvironment.LocationState.Dirty), new VacuumEnvPercept(
                    VacuumEnvironment.LocationA,
                    VacuumEnvironment.LocationState.Clean), new VacuumEnvPercept(
                    VacuumEnvironment.LocationA,
                    VacuumEnvironment.LocationState.Dirty));
            perceptSequenceActions[ps] = VacuumEnvironment.ActionSuck;
            ps = CreatePerceptSequence(new VacuumEnvPercept(
                    VacuumEnvironment.LocationA,
                    VacuumEnvironment.LocationState.Dirty), new VacuumEnvPercept(
                    VacuumEnvironment.LocationA,
                    VacuumEnvironment.LocationState.Clean), new VacuumEnvPercept(
                    VacuumEnvironment.LocationB,
                    VacuumEnvironment.LocationState.Clean));
            perceptSequenceActions[ps] = VacuumEnvironment.ActionMoveLeft;
            ps = CreatePerceptSequence(new VacuumEnvPercept(
                    VacuumEnvironment.LocationA,
                    VacuumEnvironment.LocationState.Dirty), new VacuumEnvPercept(
                    VacuumEnvironment.LocationA,
                    VacuumEnvironment.LocationState.Clean), new VacuumEnvPercept(
                    VacuumEnvironment.LocationB,
                    VacuumEnvironment.LocationState.Dirty));
            perceptSequenceActions[ps] = VacuumEnvironment.ActionSuck;
            // 2-2
            ps = CreatePerceptSequence(new VacuumEnvPercept(
                    VacuumEnvironment.LocationA,
                    VacuumEnvironment.LocationState.Dirty), new VacuumEnvPercept(
                    VacuumEnvironment.LocationA,
                    VacuumEnvironment.LocationState.Dirty), new VacuumEnvPercept(
                    VacuumEnvironment.LocationA,
                    VacuumEnvironment.LocationState.Clean));
            perceptSequenceActions[ps] = VacuumEnvironment.ActionMoveRight;
            ps = CreatePerceptSequence(new VacuumEnvPercept(
                    VacuumEnvironment.LocationA,
                    VacuumEnvironment.LocationState.Dirty), new VacuumEnvPercept(
                    VacuumEnvironment.LocationA,
                    VacuumEnvironment.LocationState.Dirty), new VacuumEnvPercept(
                    VacuumEnvironment.LocationA,
                    VacuumEnvironment.LocationState.Dirty));
            perceptSequenceActions[ps] = VacuumEnvironment.ActionSuck;
            ps = CreatePerceptSequence(new VacuumEnvPercept(
                    VacuumEnvironment.LocationA,
                    VacuumEnvironment.LocationState.Dirty), new VacuumEnvPercept(
                    VacuumEnvironment.LocationA,
                    VacuumEnvironment.LocationState.Dirty), new VacuumEnvPercept(
                    VacuumEnvironment.LocationB,
                    VacuumEnvironment.LocationState.Clean));
            perceptSequenceActions[ps] = VacuumEnvironment.ActionMoveLeft;
            ps = CreatePerceptSequence(new VacuumEnvPercept(
                    VacuumEnvironment.LocationA,
                    VacuumEnvironment.LocationState.Dirty), new VacuumEnvPercept(
                    VacuumEnvironment.LocationA,
                    VacuumEnvironment.LocationState.Dirty), new VacuumEnvPercept(
                    VacuumEnvironment.LocationB,
                    VacuumEnvironment.LocationState.Dirty));
            perceptSequenceActions[ps] = VacuumEnvironment.ActionSuck;
            // 2-3
            ps = CreatePerceptSequence(new VacuumEnvPercept(
                    VacuumEnvironment.LocationA,
                    VacuumEnvironment.LocationState.Dirty), new VacuumEnvPercept(
                    VacuumEnvironment.LocationB,
                    VacuumEnvironment.LocationState.Clean), new VacuumEnvPercept(
                    VacuumEnvironment.LocationA,
                    VacuumEnvironment.LocationState.Clean));
            perceptSequenceActions[ps] = VacuumEnvironment.ActionMoveRight;
            ps = CreatePerceptSequence(new VacuumEnvPercept(
                    VacuumEnvironment.LocationA,
                    VacuumEnvironment.LocationState.Dirty), new VacuumEnvPercept(
                    VacuumEnvironment.LocationB,
                    VacuumEnvironment.LocationState.Clean), new VacuumEnvPercept(
                    VacuumEnvironment.LocationA,
                    VacuumEnvironment.LocationState.Dirty));
            perceptSequenceActions[ps] = VacuumEnvironment.ActionSuck;
            ps = CreatePerceptSequence(new VacuumEnvPercept(
                    VacuumEnvironment.LocationA,
                    VacuumEnvironment.LocationState.Dirty), new VacuumEnvPercept(
                    VacuumEnvironment.LocationB,
                    VacuumEnvironment.LocationState.Clean), new VacuumEnvPercept(
                    VacuumEnvironment.LocationB,
                    VacuumEnvironment.LocationState.Clean));
            perceptSequenceActions[ps] = VacuumEnvironment.ActionMoveLeft;
            ps = CreatePerceptSequence(new VacuumEnvPercept(
                    VacuumEnvironment.LocationA,
                    VacuumEnvironment.LocationState.Dirty), new VacuumEnvPercept(
                    VacuumEnvironment.LocationB,
                    VacuumEnvironment.LocationState.Clean), new VacuumEnvPercept(
                    VacuumEnvironment.LocationB,
                    VacuumEnvironment.LocationState.Dirty));
            perceptSequenceActions[ps] = VacuumEnvironment.ActionSuck;
            // 2-4
            ps = CreatePerceptSequence(new VacuumEnvPercept(
                    VacuumEnvironment.LocationA,
                    VacuumEnvironment.LocationState.Dirty), new VacuumEnvPercept(
                    VacuumEnvironment.LocationB,
                    VacuumEnvironment.LocationState.Dirty), new VacuumEnvPercept(
                    VacuumEnvironment.LocationA,
                    VacuumEnvironment.LocationState.Clean));
            perceptSequenceActions[ps] = VacuumEnvironment.ActionMoveRight;
            ps = CreatePerceptSequence(new VacuumEnvPercept(
                    VacuumEnvironment.LocationA,
                    VacuumEnvironment.LocationState.Dirty), new VacuumEnvPercept(
                    VacuumEnvironment.LocationB,
                    VacuumEnvironment.LocationState.Dirty), new VacuumEnvPercept(
                    VacuumEnvironment.LocationA,
                    VacuumEnvironment.LocationState.Dirty));
            perceptSequenceActions[ps] = VacuumEnvironment.ActionSuck;
            ps = CreatePerceptSequence(new VacuumEnvPercept(
                    VacuumEnvironment.LocationA,
                    VacuumEnvironment.LocationState.Dirty), new VacuumEnvPercept(
                    VacuumEnvironment.LocationB,
                    VacuumEnvironment.LocationState.Dirty), new VacuumEnvPercept(
                    VacuumEnvironment.LocationB,
                    VacuumEnvironment.LocationState.Clean));
            perceptSequenceActions[ps] = VacuumEnvironment.ActionMoveLeft;
            ps = CreatePerceptSequence(new VacuumEnvPercept(
                    VacuumEnvironment.LocationA,
                    VacuumEnvironment.LocationState.Dirty), new VacuumEnvPercept(
                    VacuumEnvironment.LocationB,
                    VacuumEnvironment.LocationState.Dirty), new VacuumEnvPercept(
                    VacuumEnvironment.LocationB,
                    VacuumEnvironment.LocationState.Dirty));
            perceptSequenceActions[ps] = VacuumEnvironment.ActionSuck;
            // 3-1
            ps = CreatePerceptSequence(new VacuumEnvPercept(
                    VacuumEnvironment.LocationB,
                    VacuumEnvironment.LocationState.Clean), new VacuumEnvPercept(
                    VacuumEnvironment.LocationA,
                    VacuumEnvironment.LocationState.Clean), new VacuumEnvPercept(
                    VacuumEnvironment.LocationA,
                    VacuumEnvironment.LocationState.Clean));
            perceptSequenceActions[ps] = VacuumEnvironment.ActionMoveRight;
            ps = CreatePerceptSequence(new VacuumEnvPercept(
                    VacuumEnvironment.LocationB,
                    VacuumEnvironment.LocationState.Clean), new VacuumEnvPercept(
                    VacuumEnvironment.LocationA,
                    VacuumEnvironment.LocationState.Clean), new VacuumEnvPercept(
                    VacuumEnvironment.LocationA,
                    VacuumEnvironment.LocationState.Dirty));
            perceptSequenceActions[ps] = VacuumEnvironment.ActionSuck;
            ps = CreatePerceptSequence(new VacuumEnvPercept(
                    VacuumEnvironment.LocationB,
                    VacuumEnvironment.LocationState.Clean), new VacuumEnvPercept(
                    VacuumEnvironment.LocationA,
                    VacuumEnvironment.LocationState.Clean), new VacuumEnvPercept(
                    VacuumEnvironment.LocationB,
                    VacuumEnvironment.LocationState.Clean));
            perceptSequenceActions[ps] = VacuumEnvironment.ActionMoveLeft;
            ps = CreatePerceptSequence(new VacuumEnvPercept(
                    VacuumEnvironment.LocationB,
                    VacuumEnvironment.LocationState.Clean), new VacuumEnvPercept(
                    VacuumEnvironment.LocationA,
                    VacuumEnvironment.LocationState.Clean), new VacuumEnvPercept(
                    VacuumEnvironment.LocationB,
                    VacuumEnvironment.LocationState.Dirty));
            perceptSequenceActions[ps] = VacuumEnvironment.ActionSuck;
            // 3-2
            ps = CreatePerceptSequence(new VacuumEnvPercept(
                    VacuumEnvironment.LocationB,
                    VacuumEnvironment.LocationState.Clean), new VacuumEnvPercept(
                    VacuumEnvironment.LocationA,
                    VacuumEnvironment.LocationState.Dirty), new VacuumEnvPercept(
                    VacuumEnvironment.LocationA,
                    VacuumEnvironment.LocationState.Clean));
            perceptSequenceActions[ps] = VacuumEnvironment.ActionMoveRight;
            ps = CreatePerceptSequence(new VacuumEnvPercept(
                    VacuumEnvironment.LocationB,
                    VacuumEnvironment.LocationState.Clean), new VacuumEnvPercept(
                    VacuumEnvironment.LocationA,
                    VacuumEnvironment.LocationState.Dirty), new VacuumEnvPercept(
                    VacuumEnvironment.LocationA,
                    VacuumEnvironment.LocationState.Dirty));
            perceptSequenceActions[ps] = VacuumEnvironment.ActionSuck;
            ps = CreatePerceptSequence(new VacuumEnvPercept(
                    VacuumEnvironment.LocationB,
                    VacuumEnvironment.LocationState.Clean), new VacuumEnvPercept(
                    VacuumEnvironment.LocationA,
                    VacuumEnvironment.LocationState.Dirty), new VacuumEnvPercept(
                    VacuumEnvironment.LocationB,
                    VacuumEnvironment.LocationState.Clean));
            perceptSequenceActions[ps] = VacuumEnvironment.ActionMoveLeft;
            ps = CreatePerceptSequence(new VacuumEnvPercept(
                    VacuumEnvironment.LocationB,
                    VacuumEnvironment.LocationState.Clean), new VacuumEnvPercept(
                    VacuumEnvironment.LocationA,
                    VacuumEnvironment.LocationState.Dirty), new VacuumEnvPercept(
                    VacuumEnvironment.LocationB,
                    VacuumEnvironment.LocationState.Dirty));
            perceptSequenceActions[ps] = VacuumEnvironment.ActionSuck;
            // 3-3
            ps = CreatePerceptSequence(new VacuumEnvPercept(
                    VacuumEnvironment.LocationB,
                    VacuumEnvironment.LocationState.Clean), new VacuumEnvPercept(
                    VacuumEnvironment.LocationB,
                    VacuumEnvironment.LocationState.Clean), new VacuumEnvPercept(
                    VacuumEnvironment.LocationA,
                    VacuumEnvironment.LocationState.Clean));
            perceptSequenceActions[ps] = VacuumEnvironment.ActionMoveRight;
            ps = CreatePerceptSequence(new VacuumEnvPercept(
                    VacuumEnvironment.LocationB,
                    VacuumEnvironment.LocationState.Clean), new VacuumEnvPercept(
                    VacuumEnvironment.LocationB,
                    VacuumEnvironment.LocationState.Clean), new VacuumEnvPercept(
                    VacuumEnvironment.LocationA,
                    VacuumEnvironment.LocationState.Dirty));
            perceptSequenceActions[ps] = VacuumEnvironment.ActionSuck;
            ps = CreatePerceptSequence(new VacuumEnvPercept(
                    VacuumEnvironment.LocationB,
                    VacuumEnvironment.LocationState.Clean), new VacuumEnvPercept(
                    VacuumEnvironment.LocationB,
                    VacuumEnvironment.LocationState.Clean), new VacuumEnvPercept(
                    VacuumEnvironment.LocationB,
                    VacuumEnvironment.LocationState.Clean));
            perceptSequenceActions[ps] = VacuumEnvironment.ActionMoveLeft;
            ps = CreatePerceptSequence(new VacuumEnvPercept(
                    VacuumEnvironment.LocationB,
                    VacuumEnvironment.LocationState.Clean), new VacuumEnvPercept(
                    VacuumEnvironment.LocationB,
                    VacuumEnvironment.LocationState.Clean), new VacuumEnvPercept(
                    VacuumEnvironment.LocationB,
                    VacuumEnvironment.LocationState.Dirty));
            perceptSequenceActions[ps] = VacuumEnvironment.ActionSuck;
            // 3-4
            ps = CreatePerceptSequence(new VacuumEnvPercept(
                    VacuumEnvironment.LocationB,
                    VacuumEnvironment.LocationState.Clean), new VacuumEnvPercept(
                    VacuumEnvironment.LocationB,
                    VacuumEnvironment.LocationState.Dirty), new VacuumEnvPercept(
                    VacuumEnvironment.LocationA,
                    VacuumEnvironment.LocationState.Clean));
            perceptSequenceActions[ps] = VacuumEnvironment.ActionMoveRight;
            ps = CreatePerceptSequence(new VacuumEnvPercept(
                    VacuumEnvironment.LocationB,
                    VacuumEnvironment.LocationState.Clean), new VacuumEnvPercept(
                    VacuumEnvironment.LocationB,
                    VacuumEnvironment.LocationState.Dirty), new VacuumEnvPercept(
                    VacuumEnvironment.LocationA,
                    VacuumEnvironment.LocationState.Dirty));
            perceptSequenceActions[ps] = VacuumEnvironment.ActionSuck;
            ps = CreatePerceptSequence(new VacuumEnvPercept(
                    VacuumEnvironment.LocationB,
                    VacuumEnvironment.LocationState.Clean), new VacuumEnvPercept(
                    VacuumEnvironment.LocationB,
                    VacuumEnvironment.LocationState.Dirty), new VacuumEnvPercept(
                    VacuumEnvironment.LocationB,
                    VacuumEnvironment.LocationState.Clean));
            perceptSequenceActions[ps] = VacuumEnvironment.ActionMoveLeft;
            ps = CreatePerceptSequence(new VacuumEnvPercept(
                    VacuumEnvironment.LocationB,
                    VacuumEnvironment.LocationState.Clean), new VacuumEnvPercept(
                    VacuumEnvironment.LocationB,
                    VacuumEnvironment.LocationState.Dirty), new VacuumEnvPercept(
                    VacuumEnvironment.LocationB,
                    VacuumEnvironment.LocationState.Dirty));
            perceptSequenceActions[ps] = VacuumEnvironment.ActionSuck;
            // 4-1
            ps = CreatePerceptSequence(new VacuumEnvPercept(
                    VacuumEnvironment.LocationB,
                    VacuumEnvironment.LocationState.Dirty), new VacuumEnvPercept(
                    VacuumEnvironment.LocationA,
                    VacuumEnvironment.LocationState.Clean), new VacuumEnvPercept(
                    VacuumEnvironment.LocationA,
                    VacuumEnvironment.LocationState.Clean));
            perceptSequenceActions[ps] = VacuumEnvironment.ActionMoveRight;
            ps = CreatePerceptSequence(new VacuumEnvPercept(
                    VacuumEnvironment.LocationB,
                    VacuumEnvironment.LocationState.Dirty), new VacuumEnvPercept(
                    VacuumEnvironment.LocationA,
                    VacuumEnvironment.LocationState.Clean), new VacuumEnvPercept(
                    VacuumEnvironment.LocationA,
                    VacuumEnvironment.LocationState.Dirty));
            perceptSequenceActions[ps] = VacuumEnvironment.ActionSuck;
            ps = CreatePerceptSequence(new VacuumEnvPercept(
                    VacuumEnvironment.LocationB,
                    VacuumEnvironment.LocationState.Dirty), new VacuumEnvPercept(
                    VacuumEnvironment.LocationA,
                    VacuumEnvironment.LocationState.Clean), new VacuumEnvPercept(
                    VacuumEnvironment.LocationB,
                    VacuumEnvironment.LocationState.Clean));
            perceptSequenceActions[ps] = VacuumEnvironment.ActionMoveLeft;
            ps = CreatePerceptSequence(new VacuumEnvPercept(
                    VacuumEnvironment.LocationB,
                    VacuumEnvironment.LocationState.Dirty), new VacuumEnvPercept(
                    VacuumEnvironment.LocationA,
                    VacuumEnvironment.LocationState.Clean), new VacuumEnvPercept(
                    VacuumEnvironment.LocationB,
                    VacuumEnvironment.LocationState.Dirty));
            perceptSequenceActions[ps] = VacuumEnvironment.ActionSuck;
            // 4-2
            ps = CreatePerceptSequence(new VacuumEnvPercept(
                    VacuumEnvironment.LocationB,
                    VacuumEnvironment.LocationState.Dirty), new VacuumEnvPercept(
                    VacuumEnvironment.LocationA,
                    VacuumEnvironment.LocationState.Dirty), new VacuumEnvPercept(
                    VacuumEnvironment.LocationA,
                    VacuumEnvironment.LocationState.Clean));
            perceptSequenceActions[ps] = VacuumEnvironment.ActionMoveRight;
            ps = CreatePerceptSequence(new VacuumEnvPercept(
                    VacuumEnvironment.LocationB,
                    VacuumEnvironment.LocationState.Dirty), new VacuumEnvPercept(
                    VacuumEnvironment.LocationA,
                    VacuumEnvironment.LocationState.Dirty), new VacuumEnvPercept(
                    VacuumEnvironment.LocationA,
                    VacuumEnvironment.LocationState.Dirty));
            perceptSequenceActions[ps] = VacuumEnvironment.ActionSuck;
            ps = CreatePerceptSequence(new VacuumEnvPercept(
                    VacuumEnvironment.LocationB,
                    VacuumEnvironment.LocationState.Dirty), new VacuumEnvPercept(
                    VacuumEnvironment.LocationA,
                    VacuumEnvironment.LocationState.Dirty), new VacuumEnvPercept(
                    VacuumEnvironment.LocationB,
                    VacuumEnvironment.LocationState.Clean));
            perceptSequenceActions[ps] = VacuumEnvironment.ActionMoveLeft;
            ps = CreatePerceptSequence(new VacuumEnvPercept(
                    VacuumEnvironment.LocationB,
                    VacuumEnvironment.LocationState.Dirty), new VacuumEnvPercept(
                    VacuumEnvironment.LocationA,
                    VacuumEnvironment.LocationState.Dirty), new VacuumEnvPercept(
                    VacuumEnvironment.LocationB,
                    VacuumEnvironment.LocationState.Dirty));
            perceptSequenceActions[ps] = VacuumEnvironment.ActionSuck;
            // 4-3
            ps = CreatePerceptSequence(new VacuumEnvPercept(
                    VacuumEnvironment.LocationB,
                    VacuumEnvironment.LocationState.Dirty), new VacuumEnvPercept(
                    VacuumEnvironment.LocationB,
                    VacuumEnvironment.LocationState.Clean), new VacuumEnvPercept(
                    VacuumEnvironment.LocationA,
                    VacuumEnvironment.LocationState.Clean));
            perceptSequenceActions[ps] = VacuumEnvironment.ActionMoveRight;
            ps = CreatePerceptSequence(new VacuumEnvPercept(
                    VacuumEnvironment.LocationB,
                    VacuumEnvironment.LocationState.Dirty), new VacuumEnvPercept(
                    VacuumEnvironment.LocationB,
                    VacuumEnvironment.LocationState.Clean), new VacuumEnvPercept(
                    VacuumEnvironment.LocationA,
                    VacuumEnvironment.LocationState.Dirty));
            perceptSequenceActions[ps] = VacuumEnvironment.ActionSuck;
            ps = CreatePerceptSequence(new VacuumEnvPercept(
                    VacuumEnvironment.LocationB,
                    VacuumEnvironment.LocationState.Dirty), new VacuumEnvPercept(
                    VacuumEnvironment.LocationB,
                    VacuumEnvironment.LocationState.Clean), new VacuumEnvPercept(
                    VacuumEnvironment.LocationB,
                    VacuumEnvironment.LocationState.Clean));
            perceptSequenceActions[ps] = VacuumEnvironment.ActionMoveLeft;
            ps = CreatePerceptSequence(new VacuumEnvPercept(
                    VacuumEnvironment.LocationB,
                    VacuumEnvironment.LocationState.Dirty), new VacuumEnvPercept(
                    VacuumEnvironment.LocationB,
                    VacuumEnvironment.LocationState.Clean), new VacuumEnvPercept(
                    VacuumEnvironment.LocationB,
                    VacuumEnvironment.LocationState.Dirty));
            perceptSequenceActions[ps] = VacuumEnvironment.ActionSuck;
            // 4-4
            ps = CreatePerceptSequence(new VacuumEnvPercept(
                    VacuumEnvironment.LocationB,
                    VacuumEnvironment.LocationState.Dirty), new VacuumEnvPercept(
                    VacuumEnvironment.LocationB,
                    VacuumEnvironment.LocationState.Dirty), new VacuumEnvPercept(
                    VacuumEnvironment.LocationA,
                    VacuumEnvironment.LocationState.Clean));
            perceptSequenceActions[ps] = VacuumEnvironment.ActionMoveRight;
            ps =
                CreatePerceptSequence(
                    new VacuumEnvPercept(VacuumEnvironment.LocationB, VacuumEnvironment.LocationState.Dirty),
                    new VacuumEnvPercept(VacuumEnvironment.LocationB, VacuumEnvironment.LocationState.Dirty),
                    new VacuumEnvPercept(VacuumEnvironment.LocationA, VacuumEnvironment.LocationState.Dirty));
            perceptSequenceActions[ps] = VacuumEnvironment.ActionSuck;
            ps =
                CreatePerceptSequence(
                    new VacuumEnvPercept(VacuumEnvironment.LocationB, VacuumEnvironment.LocationState.Dirty),
                    new VacuumEnvPercept(VacuumEnvironment.LocationB, VacuumEnvironment.LocationState.Dirty),
                    new VacuumEnvPercept(VacuumEnvironment.LocationB, VacuumEnvironment.LocationState.Clean));
            perceptSequenceActions[ps] = VacuumEnvironment.ActionMoveLeft;
            ps =
                CreatePerceptSequence(
                    new VacuumEnvPercept(VacuumEnvironment.LocationB, VacuumEnvironment.LocationState.Dirty),
                    new VacuumEnvPercept(VacuumEnvironment.LocationB, VacuumEnvironment.LocationState.Dirty),
                    new VacuumEnvPercept(VacuumEnvironment.LocationB, VacuumEnvironment.LocationState.Dirty));
            perceptSequenceActions[ps] = VacuumEnvironment.ActionSuck;

            //
            // Level 4: 4x4x4x4 states
            // ...

            return perceptSequenceActions;
        }

        private static IList<IPercept> CreatePerceptSequence(params IPercept[] percepts) 
        {
            return percepts.ToList();
        }
    }

}
