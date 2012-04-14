using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Probability.Reasoning
{
    public class HMMFactory
    {

        public static HiddenMarkovModel CreateRobotHMM()
        {

            // Example adopted from Sebastian Thrun's "Probabilistic Robotics"
            // Chapter 2
            // A robot faces a door. The state of the door (open or closed)
            // constitutes the "hidden state"
            // The robot's sensor detects a "closed" or "open" state.
            // (Perception may be out of synch with reality because sensors are
            // probabilistic).
            // The robot can either "do nothing" or "push" the door. These are the
            // possible actions.

            IList<string> states = new[] { HmmConstants.DoorOpen, HmmConstants.DoorClosed };
            IList<string> actions = new [] { HmmConstants.DoNothing, HmmConstants.PushDoor };
            IList<string> perceptions = new [] {HmmConstants.SeeDoorOpen, HmmConstants.SeeDoorClosed };

            RandomVariable prior = new RandomVariable(states);
            TransitionModel tm = new TransitionModel(states, actions);
            // tm.SetTransitionProbability(start_state, action, end_state,
            // probability)
            // given a start state and an action the probability of the end state is
            // probability
            tm.SetTransitionProbability(HmmConstants.DoorOpen,
                    HmmConstants.DoNothing, HmmConstants.DoorOpen, 1.0);
            tm.SetTransitionProbability(HmmConstants.DoorOpen,
                    HmmConstants.DoNothing, HmmConstants.DoorOpen, 1.0);
            tm.SetTransitionProbability(HmmConstants.DoorOpen,
                    HmmConstants.DoNothing, HmmConstants.DoorClosed, 0.0);
            tm.SetTransitionProbability(HmmConstants.DoorClosed,
                    HmmConstants.DoNothing, HmmConstants.DoorClosed, 1.0);
            tm.SetTransitionProbability(HmmConstants.DoorClosed,
                    HmmConstants.DoNothing, HmmConstants.DoorOpen, 0.0);

            tm.SetTransitionProbability(HmmConstants.DoorOpen,
                    HmmConstants.PushDoor, HmmConstants.DoorOpen, 1.0);
            tm.SetTransitionProbability(HmmConstants.DoorOpen,
                    HmmConstants.PushDoor, HmmConstants.DoorClosed, 0.0);
            tm.SetTransitionProbability(HmmConstants.DoorClosed,
                    HmmConstants.PushDoor, HmmConstants.DoorClosed, 0.2);
            tm.SetTransitionProbability(HmmConstants.DoorClosed,
                    HmmConstants.PushDoor, HmmConstants.DoorOpen, 0.8);

            SensorModel sm = new SensorModel(states, perceptions);
            // sm.setSensingProbaility(state,perception,p); given a state the
            // probability of a perception is p
            sm.SetSensingProbability(HmmConstants.DoorOpen,
                    HmmConstants.SeeDoorClosed, 0.4);
            sm.SetSensingProbability(HmmConstants.DoorOpen,
                    HmmConstants.SeeDoorOpen, 0.6);
            sm.SetSensingProbability(HmmConstants.DoorClosed,
                    HmmConstants.SeeDoorOpen, 0.2);
            sm.SetSensingProbability(HmmConstants.DoorClosed,
                    HmmConstants.SeeDoorClosed, 0.8);

            HiddenMarkovModel hmm = new HiddenMarkovModel(prior, tm, sm);
            return hmm;
        }

        public static HiddenMarkovModel createRainmanHMM()
        {
            IList<string> states = new [] {HmmConstants.Raining, HmmConstants.NotRaining };
            // no actions because the observer has no way of changing the hidden
            // state and i spassive
            IList<string> perceptions = new [] {HmmConstants.SeeUmbrella, HmmConstants.SeeNoUmbrella };

            RandomVariable prior = new RandomVariable(states);

            TransitionModel tm = new TransitionModel(states);
            // tm.setTransitionModelValue(start_state, action, end_state,
            // probability);
            // given a start state and an action the probability of the end state is
            // probability
            tm.SetTransitionProbability(HmmConstants.Raining, HmmConstants.Raining,
                    0.7);
            tm.SetTransitionProbability(HmmConstants.Raining,
                    HmmConstants.NotRaining, 0.3);
            tm.SetTransitionProbability(HmmConstants.NotRaining,
                    HmmConstants.Raining, 0.3);
            tm.SetTransitionProbability(HmmConstants.NotRaining,
                    HmmConstants.NotRaining, 0.7);

            SensorModel sm = new SensorModel(states, perceptions);
            // sm.setSensingProbaility(state,perception,p); given a state the
            // probability of a perception is p
            sm.SetSensingProbability(HmmConstants.Raining,
                    HmmConstants.SeeUmbrella, 0.9);
            sm.SetSensingProbability(HmmConstants.Raining,
                    HmmConstants.SeeNoUmbrella, 0.1);
            sm.SetSensingProbability(HmmConstants.NotRaining,
                    HmmConstants.SeeUmbrella, 0.2);
            sm.SetSensingProbability(HmmConstants.NotRaining,
                    HmmConstants.SeeNoUmbrella, 0.8);

            HiddenMarkovModel hmm = new HiddenMarkovModel(prior, tm, sm);

            // hmm.setSensorModelValue(state,perception,p); given a state the
            // probability of a perception is p

            return hmm;
        }
    }
}
