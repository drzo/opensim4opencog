using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Collections;
using AIMLbot;
using AltAIMLParser;
using LogicalParticleFilter1;
using AltAIMLbot;

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
    class MtLink : Environment
    {
        public string mtName="aixiMt";
        AltBot envBot = null;
        Dictionary<string, UInt64> observeDictionary = new Dictionary<string, UInt64>();
        Dictionary<UInt64, string> actDictionary = new Dictionary<UInt64, string>();
        Dictionary<string, UInt64> rewardDictionary = new Dictionary<string, UInt64>();
        UInt64 max_observe = 0;
        UInt64 max_act = 0;
        UInt64 max_reward = 0;

        // Defintitions in MTs
        // aixiActions(SYMBOL,CODE) = action CODE mapps to performAct(SYMBOL) in ouput
        // aixiObservable(SYMBOL,BIT) = aixiObserve(SYMBOL) sets observe[BIT]=1 
        // aixiRewards(SYMBOL,VALUE) = seeing aixiFeedback(SYMBOL) adds value to reward
        //
        public MtLink(AltBot myBot, string mt)
        {
            mtName = mt;
            envBot = myBot;

            List<Dictionary<string, string>> bingingsList = new List<Dictionary<string, string>>();
            
            envBot.servitor.prologEngine.askQuery("aixiObservable(SYMBOL,BIT)", mtName, out bingingsList);
            foreach (Dictionary<string, string> bindings in bingingsList)
            {
                foreach (string key in bindings.Keys)
                {
                    string symbol = bindings["SYMBOL"].Trim();
                    UInt64 bit = UInt64.Parse(bindings["BIT"].Trim());
                    if (bit > max_observe) max_observe = bit;
                    observeDictionary[symbol] = bit;
                }
            }

            envBot.servitor.prologEngine.askQuery("aixiActions(SYMBOL,CODE)", mtName, out bingingsList);
            foreach (Dictionary<string, string> bindings in bingingsList)
            {
                foreach (string key in bindings.Keys)
                {
                    string symbol = bindings["SYMBOL"].Trim();
                    UInt64 bit = UInt64.Parse(bindings["CODE"].Trim());
                    if (bit > max_act) max_act = bit;
                    actDictionary[bit] = symbol;
                }
            }

            envBot.servitor.prologEngine.askQuery("aixiRewards(SYMBOL,VALUE)", mtName, out bingingsList);
            foreach (Dictionary<string, string> bindings in bingingsList)
            {
                foreach (string key in bindings.Keys)
                {
                    string symbol = bindings["SYMBOL"].Trim();
                    UInt64 value = UInt64.Parse(bindings["VALUE"].Trim());
                    if (value > max_reward) max_reward = value;
                    rewardDictionary[symbol] = value;
                }
            }

        }

        public UInt64 decodeReward()
        {
            UInt64 reward = 0;
            List<Dictionary<string, string>> bingingsList = new List<Dictionary<string, string>>();
            envBot.servitor.prologEngine.askQuery("aixiFeedback(SYMBOL)", mtName, out bingingsList);
            foreach (Dictionary<string, string> bindings in bingingsList)
            {
                foreach (string key in bindings.Keys)
                {
                    string symbol = bindings["SYMBOL"].Trim();
                    if (rewardDictionary.ContainsKey(symbol))
                    {
                        reward += (UInt64) rewardDictionary[symbol];
                    }
                }
            }
            return reward;
        }

        public UInt64 encodeObservation()
        {
            UInt64 observation = 0;
            List<Dictionary<string, string>> bingingsList = new List<Dictionary<string, string>>();
            envBot.servitor.prologEngine.askQuery("aixiObserve(SYMBOL)", mtName, out bingingsList);
            foreach (Dictionary<string, string> bindings in bingingsList)
            {
                foreach (string key in bindings.Keys)
                {
                    string symbol = bindings["SYMBOL"].Trim();
                    if (observeDictionary.ContainsKey(symbol))
                    {
                        UInt64 bit = observeDictionary[symbol];
                        UInt64 mask = (UInt64) 1 << (int)bit;
                        observation = observation | mask;
                    }
                }
            }
            return observation;
        }

        public void decodeAction(UInt64 action)
        {
            if (actDictionary.ContainsKey((UInt64)action))
            {
                string actSymbol = actDictionary[(UInt64)action];
                string assert = String.Format("performAct({0}).", actSymbol);
                envBot.servitor.prologEngine.insertKB(assert, "aixiOutputMt");
            }
        }

        public override void performAction(UInt64 action)
        {
            // Set up the initial observation
            Random r = new Random();
            m_last_action = action;
            decodeAction(action);
            m_observation = (UInt64)encodeObservation();
            m_reward = (UInt64)decodeReward();

           // m_observation = (UInt64)(r.NextDouble() < p ? 1 : 0);
           // m_reward = (UInt64)(action == m_observation ? 1 : 0);
        }

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
