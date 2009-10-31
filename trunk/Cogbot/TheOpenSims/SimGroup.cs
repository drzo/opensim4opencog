using System;
using System.Collections.Generic;
using cogbot.Listeners;
using OpenMetaverse;

namespace cogbot.TheOpenSims
{
    public class SimGroup : BotMentalAspect
    {
        public ICollection<NamedParam> GetInfoMap()
        {
            return WorldObjects.GetMemberValues("", this);
        }

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

        public override string ToString()
        {
            if (!string.IsNullOrEmpty(group.Name)) return group.Name;
            return "Group-" + ID;
        }
        public UUID ID;
        private Group group;
        public GroupAccountSummary Summary;

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
