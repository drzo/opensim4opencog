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

    /// <summary>
    /// The class that is to be used for users, as opposed to AI.
    /// </summary>
    public class HumanAgent : Agent
    {
        /// <summary>
        /// This delegate is implemented in the input member.
        /// 
        /// += the input member with functons that adhere to the "int ...()" format
        /// </summary>
        /// <returns></returns>
        public delegate int InputRequest();
        /// <summary>
        /// InputRequest input is called when the HumanAgent has been on the receiving end of an AgentAction
        /// </summary>
        public InputRequest input;

        protected override internal void PerceiveAction(Agent sender, AgentAction performed)
        {
            base.PerceiveAction(sender, performed);

            Model.waitingForInput = true;
            Model.inputAgent = ID;
        }

        protected internal override void Perform()
        {
            if (queuedAction != -1 && Model.inputAgent == ID)
            {
                Model.waitingForInput = false;
                Model.inputAgent = -1;
            }

            base.Perform();
        }
    }
}
