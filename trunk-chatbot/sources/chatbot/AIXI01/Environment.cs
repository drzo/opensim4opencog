using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Collections;

namespace MCAIXI0
{
    using reward_t = System.Double;
    using action_t = System.UInt64;
    using hash_t = System.UInt64;
    using symbol_t = System.Boolean;
    using visit_t = System.UInt64;
    using precept_t = System.UInt64;
    using age_t = System.UInt64;


    class Environment
    {
     // visible to inherited classes
    public action_t m_last_action = 0;  // the last action performed by the agent
    public UInt64 m_observation; // the current observation
    public UInt64 m_reward;      // the current reward

    // receives the agent's action and calculates the new environment percept
    public virtual void performAction(action_t action)
    {
        m_last_action = action;
       // return; // TODO: implement in inherited class
    }
	// returns true if the environment cannot interact with the agent anymore
    public virtual bool isFinished() { return false; } // TODO: implement in inherited class (if necessary)

    public virtual void getPercept(BitArray symlist)
    {
    }

    public virtual UInt64 getObservation() { return m_observation; }

    public virtual UInt64 getReward() { return m_reward; }


    }

    class CoinFlip:Environment
    {
        // Constructor: set up the initial environment percept
	    // TODO: implement in inherited class
        double p; // Probability of observing 1 (heads)

        public    CoinFlip(Hashtable options) 
        {
            // Determine the probability of the coin landing on heads
            p = 1.0;
            if (options.ContainsKey("coin-flip-p")) 
            {
                 p=double.Parse((string)options["coin-flip-p"]);
            }
            //assert(0.0 <= p);
            //assert(p <= 1.0);

            // Set up the initial observation
            Random r = new Random ();

            m_observation =(UInt64) ( r.NextDouble() < p ? 1 : 0);
            m_reward = 0;
        }


    // Observes 1 (heads) with probability p and 0 (tails) with probability 1 - p.
    // Observations are independent of the agent's actions. Gives a reward of 1 if
    // the agent correctly predicts the next observation and 0 otherwise.
    public override void performAction(UInt64 action) 
        {
            // Set up the initial observation
            Random r = new Random();
            m_last_action = action;
            m_observation = (UInt64)(r.NextDouble() < p ? 1 : 0);
	        m_reward =(UInt64 )( action == m_observation ? 1 : 0);
        }

    }
}
