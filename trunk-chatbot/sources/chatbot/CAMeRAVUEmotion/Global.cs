using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace CAMeRAVUEmotion
{
    // This code was taken from Silicon Coppelia,
    // developed within the Center for Advanced Media Research Amsterdam 
    // at the VU University Amsterdam (CAMeRA@VU) 
    // and written by Matthijs Aart Pontier and Ghazanfar Farooq Siddiqui. 
    // More information and publications can be found here:
    // http://camera-vu.nl/matthijs/
    // http://www.linkedin.com/profile/view?id=19933074 
    // http://www.few.vu.nl/~mpr210/
    // http://www.few.vu.nl/~mpr210/DissertationMAPontier.pdf
    // http://camera-vu.nl/matthijs/IAT-2009_Coppelia.pdf
    //Hoorn, J.F., Pontier, M.A., & Siddiqui, G.F., (2011).
    //Coppélius’ Concoction: Similarity and Complementarity
    //Among Three Affect-related Agent Models. Cognitive
    //Systems Research Journal, in press.

    /// <summary>
    /// This class provides access to global properties such as States and Actions.
    /// </summary>
    public static class Global
    {
        //this listener defines a type which agents use to perceive each other's actions
        /// <summary>
        /// This delegate is implemented in the actionBroadcast member.
        /// 
        /// += the actionBroadcast member with functons that adhere to the "void ...(int, int, int)" format
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="action"></param>
        /// <param name="target"></param>
        public delegate void ActionListener(int sender, int action, int target);
        /// <summary>
        /// This listener is fired when an action is performed. This is to be used to listen for actions globally, without requiring the user to be on the receiving end.
        /// Useful for updating states.
        /// </summary>
        public static ActionListener actionBroadcast;

        #region variables

        //states, what type should this be? should there be a separate state class?
        internal static Dictionary<int, bool> STATES = new Dictionary<int, bool>();	//0 = Make good impression / Get Date	//1 = Be honest 	//2=Sub2	//3=Sub3	//4=Sub4	//5=Sub5	//6=Sub6	//7=Sub7	//8=Sub8

        static int _lastState = 0;
        static int _lastAction = 0;

        //moralprinciples, what type should this be? should there be a separate moralprinciple class?
        internal static Dictionary<int, bool> MORALPRINCIPLES = new Dictionary<int, bool>();	//0 = Autonomy //1 = Non-Maleficence //2 = Beneficence	//3 = Justice

        static int _lastMoralPrinciple = 0;

        internal static Dictionary<int, List<AgentAction>> ACTIONS = new Dictionary<int, List<AgentAction>>();

        #endregion variables

        #region functions

        /// <summary>
        /// This function allows end-users to get AgentAction instances via their ID's.
        /// These ID's are stored, for instance, in the Agent.PossibleResponses list which is used when a HumanAgent must select a response.
        /// </summary>
        /// <param name="globalID"></param>
        /// <returns></returns>
        public static AgentAction GetActionByID(int globalID)
        {
            foreach (int key in ACTIONS.Keys)
            {
                List<AgentAction> temp = ACTIONS[key];

                foreach (AgentAction a in temp)
                {
                    if (a.GlobalIndex == globalID)
                        return a;
                }
            }

            return null;
        }

        public static AgentAction GetActionByName(string name)
        {
            foreach (int key in ACTIONS.Keys)
            {
                List<AgentAction> temp = ACTIONS[key];

                foreach (AgentAction a in temp)
                {
                    if (a.Name == name)
                        return a;
                }
            }

            return null;
        }

        internal static int NextActionID()
        {
            return _lastAction++;
        }

        /// <summary>
        /// This function can be called to register a new state.
        /// </summary>
        /// <returns>A value that can be stored to indicate a specific state. Store these in a variable, and use that variable to access the state.</returns>
        public static int AddState(bool initialValue)
        {
            int newState = _lastState;
            STATES[_lastState++] = initialValue;
            return newState;
        }
        public static int AddState()
        {
            return AddState(true);
        }
        /// <summary>
        /// Gets the value of a state.
        /// </summary>
        /// <param name="stateIndex">The index, returned by AddState at an earlier time, that indicates a state.</param>
        /// <returns></returns>
        public static bool GetState(int stateIndex)
        {
            if (STATES.Keys.Contains(stateIndex))
                return STATES[stateIndex];
            else return false;
        }
        /// <summary>
        /// Gets the value of a state.
        /// </summary>
        /// <param name="stateIndex">The index, returned by AddState at an earlier time, that indicates a state.</param>
        /// <param name="value"></param>
        public static void SetState(int stateIndex, bool value)
        {
            if (STATES.Keys.Contains(stateIndex))
                STATES[stateIndex] = value;
        }

        /// <summary>
        /// The current number of states in the simulation.
        /// </summary>
        public static int StateCount
        {
            get
            {
                //Console.WriteLine("# States is: " + STATES.Count);
                return STATES.Count;
            }
        }


        /// <summary>
        /// This function can be called to register a new moral principle.
        /// </summary>
        /// <returns>A value that can be stored to indicate a specific moral principle. Store these in a variable, and use that variable to access the moral principle.</returns>
        public static int AddMoralPrinciple(bool initialValue)
        {
            int newMoralPrinciple = _lastMoralPrinciple;
            MORALPRINCIPLES[_lastMoralPrinciple++] = initialValue;
            Console.WriteLine("Moral Principle added: " + newMoralPrinciple);
            return newMoralPrinciple;
        }
        public static int AddMoralPrinciple()
        {
            return AddMoralPrinciple(true);
        }

        /// <summary>
        /// Gets the value of a moral principle.
        /// </summary>
        /// <param name="stateIndex">The index, returned by AddMoralPrinciple at an earlier time, that indicates a moral principle.</param>
        /// <returns></returns>
        public static bool GetMoralPrinciple(int moralPrincipleIndex)
        {
            if (MORALPRINCIPLES.Keys.Contains(moralPrincipleIndex))
                return MORALPRINCIPLES[moralPrincipleIndex];
            else return false;
        }
        /// <summary>
        /// Sets the value of a moral principle.
        /// </summary>
        /// <param name="moralPrincipleIndex">The index, returned by AddMoralPrinciple at an earlier time, that indicates a moral principle.</param>
        /// <param name="value"></param>
        public static void SetMoralPrinciple(int moralPrincipleIndex, bool value)
        {
            if (MORALPRINCIPLES.Keys.Contains(moralPrincipleIndex))
                MORALPRINCIPLES[moralPrincipleIndex] = value;

        }

         /// <summary>
        /// The current number of moral principles in the simulation.
        /// </summary>
        public static int MoralPrincipleCount
        {
            get
            {
                //Console.WriteLine("# Moral Principles is: " + MORALPRINCIPLES.Count);
                return MORALPRINCIPLES.Count;
            }
        }


        /// <summary>
        /// A function that can be used to sort descending with the List class's Sort function.
        /// </summary>
        /// <param name="x"></param>
        /// <param name="y"></param>
        /// <returns></returns>
        public static int SortDescending(float x, float y)
        {
            if (x == y)
                return 0;
            if (x > y)
                return -1;
            if (x < y)
                return 1;

            //this can't possibly ever be executed
            return 0;
        }
        /// <summary>
        /// A function that can be used to sort ascending with the List class's Sort function.
        /// </summary>
        /// <param name="x"></param>
        /// <param name="y"></param>
        /// <returns></returns>
        public static int SortAscending(float x, float y)
        {
            if (x == y)
                return 0;
            if (x > y)
                return 1;
            if (x < y)
                return -1;

            //this can't possibly ever be executed
            return 0;
        }

        /// <summary>
        /// This function is called by Agents when they perform an action, and will trigged the actionBroadcast to fire.
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="action"></param>
        /// <param name="target"></param>
        internal static void Broadcast(int sender, int action, int target)
        {
            if (actionBroadcast != null)
                actionBroadcast(sender, action, target);
        }

        #endregion functions
    }
}