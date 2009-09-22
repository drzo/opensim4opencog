using System;
using System.Collections.Generic;
using OpenMetaverse;

namespace cogbot.TheOpenSims
{
    class SimGroup : BotMentalAspect
    {
        public Group Group
        {
            get
            {
                return group;
            }
            set
            {
                group = value;
            }
        }

        public UUID ID;
        private Group group;
            
        public SimGroup(UUID g)
        {
            ID = g;
        }
        public FirstOrderTerm GetTerm()
        {
            throw new NotImplementedException();
        }
    }
}
