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

    class MainLoops
    {
        public Hashtable options = new Hashtable ();
        public Environment env;
       // public ListBox logbox = null;

        public MainLoops()
        {
            options["ct-depth"] = "3";
            options["agent-horizon"] = "8";
            options["exploration"] = "0";     // do not explore
            options["explore-decay"] = "0.95"; // exploration rate does not decay
            options["terminate-age"] = "1000";
            options["cycle-length-ms"] = "200";
           // options["controller"] = "mc";
            options["controller"] = "mcts";
            //options["controller"] = "random";        
        }

       public void selectEnvironment()
       {
           string environment_name = (string)options["environment"];

 	        if (environment_name == "coin-flip") {
		        env = new CoinFlip(options);
		        options["agent-actions"] = "2";
		        options["observation-bits"] = "1";
		        options["reward-bits"] = "1";
	        }
	        else if (environment_name == "1d-maze") {
		        // TODO: instantiate "env" (if appropriate)
	        }
	        else if (environment_name == "cheese-maze") {
		        // TODO: instantiate "env" (if appropriate)
	        }
	        else if (environment_name == "tiger") {
		        // TODO: instantiate "env" (if appropriate)
	        }
	        else if (environment_name == "extended-tiger") {
		        // TODO: instantiate "env" (if appropriate)
	        }
	        else if (environment_name == "4x4-grid") {
		        // TODO: instantiate "env" (if appropriate)
	        }
	        else if (environment_name == "tictactoe") {
		        // TODO: instantiate "env" (if appropriate)
	        }
	        else if (environment_name == "biased-rock-paper-scissor") {
		        // TODO: instantiate "env" (if appropriate)
	        }
	        else if (environment_name == "kuhn-poker") {
		        // TODO: instantiate "env" (if appropriate)
	        }
	        else if (environment_name == "pacman") {
		        // TODO: instantiate "env" (if appropriate)
	        }
	        else {
		        //std::cerr << "ERROR: unknown environment '" << environment_name << "'" << std::endl;
		        return;
	        }

	        // Set up the agent
	        Agent ai = new Agent (options);

	        // Run the main agent/environment interaction loop
	        mainLoop(ai, env, options);
       }

// The main agent/environment interaction loop
void mainLoop(Agent ai, Environment env, Hashtable options) {

    //ai.logbox = logbox;
	// Determine exploration options
	bool explore = options.ContainsKey("exploration");
	double explore_rate =0;
    double explore_decay=0;
	if (explore) {
		explore_rate = Double.Parse((string)options["exploration"]);
		explore_decay = Double.Parse((string)options["explore-decay"]);

		//assert(0.0 <= explore_rate && explore_rate <= 1.0);
		//assert(0.0 <= explore_decay && explore_decay <= 1.0);
	}


	// Determine termination age
	bool terminate_check = options.ContainsKey("terminate-age");
	age_t terminate_age=0;
	if (terminate_check) {

        terminate_age = UInt64.Parse((string) options["terminate-age"]);
		//assert(0 <= terminate_age);
	}

	// Agent/environment interaction loop
	for ( int cycle = 1; !env.isFinished(); cycle++)
    {

		// check for agent termination
		if (terminate_check && ai.age() > terminate_age)
        {
			//log << "info: terminating agent" << std::endl;
            //logbox.Items.Add("info: terminating agent ");
			break;
		}

		// Get a percept from the environment
		UInt64 observation = env.getObservation();
		UInt64 reward = env.getReward();

		// Update agent's environment model with the new percept
		ai.modelUpdate(observation, reward); // TODO: implement in agent.cpp

		// Determine best exploitive action, or explore
		action_t action;
		bool explored = false;
        Random rand01 = new Random();

		if (explore && rand01.NextDouble () < explore_rate) {
			explored = true;
			action = ai.genRandomAction();
		}
		else {
            Search s = new Search();
			action = s.search(ai); // TODO: implement in search.cpp
		}

		// Send an action to the environment
		env.performAction(action); // TODO: implement for each environment

		// Update agent's environment model with the chosen action
		ai.modelUpdate(action); // TODO: implement in agent.cpp

        		// Log this turn
        //logbox.Items.Add("----------------------");
        //logbox.Items.Add("cycle: " + cycle);
        // logbox.Items.Add("action: " + action );
        //logbox.Items.Add("m_observation: " + env.m_observation);
        //logbox.Items.Add("m_reward: " +env.m_reward );
        //logbox.Items.Add("explored: " + (explored ? "yes" : "no") );
        //logbox.Items.Add("explore rate: "  + explore_rate );
        //logbox.Items.Add("total reward: "  + ai.reward());
        //logbox.Items.Add("average reward: "  + ai.averageReward());
        //Application.DoEvents();

		// Update exploration rate
		if (explore) explore_rate *= explore_decay;

	}


        }
    }
}
