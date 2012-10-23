using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace CAMeRAVUEmotion
{
    // From the work on Silicon Coppelia
    // http://www.few.vu.nl/~mpr210/
    // http://www.few.vu.nl/~mpr210/DissertationMAPontier.pdf
    // http://camera-vu.nl/matthijs/IAT-2009_Coppelia.pdf
    //Hoorn, J.F., Pontier, M.A., & Siddiqui, G.F., (2011).
    //Coppélius’ Concoction: Similarity and Complementarity
    //Among Three Affect-related Agent Models. Cognitive
    //Systems Research Journal, in press.

    //include this in any class file that needs to access classes from the CAMeRAVUEmotion library
    //Make sure you also add the library (the DLL, which I have copied to this project's main folder, along with the XML Documentation) in the "References" of your project (which can be found in the "Solution Explorer" window, View -> Solution Explorer)
    //Simply right click -> Add Reference -> Browse, select the DLL
    class Program
    {
        //class variables for use across multiple functions
        static HumanAgent a2;
        static Agent a1;

        static int STATE_LOST_THE_GAME;
        static AgentAction OK;

        static void Main(string[] args)
        {
            //Create new agents
            //I've made these variables accessible throughout the class, so we can access them at later times.
            a1 = new Agent(0, 0, 1, -1, 0, 0, 0, 0);
            a2 = new HumanAgent();

            //Register the RequestingInput function (see below) as a function that needs to be called when Agent a2 (which is the HumanAgent) needs to respond to input
            a2.input += new HumanAgent.InputRequest(RequestingInput);

            //Register the GlobalActionReceived function (see below) as a function that needs to be called whenever any action is performed by any agent.
            Global.actionBroadcast += new Global.ActionListener(GlobalActionReceived);

            //Register both agents with the model, so they will be updated
            //You'll most likely always need to do this for all agents
            Model.RegisterAgent(a1);
            Model.RegisterAgent(a2);

            //Register the agents with each other, so they know they exist
            //Agents will not target other agents with actions unless they know they exist
            a1.AddAgent(a2.AgentID);
            a2.AddAgent(a1.AgentID);

            //Create actions with a name, positivity, negativity, (optional) and actionGroup (useful for conversation topics)
            //Actiongroups currently have no functionality attached to them, but simply store this information in the actions
            AgentAction Welles = new AgentAction("Welles", 0, 0);
            AgentAction Nietes = new AgentAction("Nietes", 0, 0);
            OK = new AgentAction("OK", 0, 0);

            AgentAction HowAreYouDoing = new AgentAction("HowAreYouDoing", 1, -1);
            AgentAction Terrible = new AgentAction("Terrible", -1, 1);
            AgentAction Great = new AgentAction("Great", 1, -1);
            AgentAction HowCome = new AgentAction("HowCome", 1, -1);
            AgentAction ThatsGreat = new AgentAction("ThatsGreat", 1, -1);

            //Determine possible response actions for each action
            HowAreYouDoing.AddResponse(Terrible.GlobalIndex);
            HowAreYouDoing.AddResponse(Great.GlobalIndex);

            Terrible.AddResponse(HowCome.GlobalIndex);
            Great.AddResponse(ThatsGreat.GlobalIndex);

            Welles.AddResponse(Nietes.GlobalIndex);
            Welles.AddResponse(OK.GlobalIndex);

            Nietes.AddResponse(Welles.GlobalIndex);
            Nietes.AddResponse(OK.GlobalIndex);

            //Create states
            //I've made this a class-member variable, because I want to set this state at a later time (in the GlobalActionReceived function)
            STATE_LOST_THE_GAME = Global.AddState(false);

            //Setup the agent variables, in this case based on states and actions
            //This agent does NOT want STATE_LOST_THE_GAME to become true
            a1.AddAmbition(STATE_LOST_THE_GAME, -1);
            //This agent believes STATE_LOST_THE_GAME will become true if the action OK is performed, ergo he will avoid that action
            a1.SetActionStateBelief(OK.GlobalIndex, STATE_LOST_THE_GAME, 1);

            //Kickstart the model by having the AI perform the "Welles" action
            //Welles/Nietes example
            a1.ManualPerform(Welles, a2);
            //Conversation example
            //a1.ManualPerform(HowAreYouDoing, a2);

            //Start running the model
            //This will run the Model in a separate thread, until it is stopped
            //Stopping the model will in essense pause the simulation, so you can do this whenever necessary
            //The Model will pause when a HumanAgent needs to respond to input
            Model.Start();
        }

        /// <summary>
        /// This function is called when the HumanAgent a1's input member is fired (which happens when it receives an action from another agent)
        /// </summary>
        /// <returns></returns>
        static int RequestingInput()
        {
            //Some display so users know what's going on

            Console.WriteLine("Select Response");

            //display possible responses
            for (int i = 0; i < a2.PossibleResponses.Count; ++i)
            {
                Console.WriteLine("" + i + ": " + Global.GetActionByID(a2.PossibleResponses[i]).Name);
            }

            //Request input from users
            int num = -1;
            bool failedParse = false;
            do
            {
                failedParse = false;
                string input = Console.In.ReadLine();
                if (!int.TryParse(input, out num))
                {
                    failedParse = true;
                }
            } while (!failedParse && num < 0 || num >= a2.PossibleResponses.Count);

            int responseID = -1;

            //Check if the input of the user is valid
            if (num >= 0 && num < a2.PossibleResponses.Count)
                responseID = a2.PossibleResponses[num];

            //Return the selected response
            //-1 is an invalid ActionID and will constitute "no action"
            return responseID;
        }

        /// <summary>
        /// This function is called when any agent performs any action.
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="action"></param>
        /// <param name="target"></param>
        static void GlobalActionReceived(int sender, int action, int target)
        {
            Console.WriteLine("Caught Action: Agent " + sender + " performed action " + Global.GetActionByID(action).Name + " on Agent " + target);

            //if the OK action is performed, update STATE_LOST_THE_GAME to true
            if (action == OK.GlobalIndex)
            {
                Console.WriteLine("Setting state STATE_LOST_THE_GAME to true");
                Global.SetState(STATE_LOST_THE_GAME, true);
            }
        }
    }
}
