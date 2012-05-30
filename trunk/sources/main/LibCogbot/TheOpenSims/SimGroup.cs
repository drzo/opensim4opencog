#undef COGBOT_LIBOMV
using System;
using System.Collections.Generic;
using cogbot.Listeners;
using MushDLR223.ScriptEngines;
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
#if COGBOT_LIBOMV
            if (group == null) return "Group-" + _id;
#endif
            if (!string.IsNullOrEmpty(group.Name)) return group.Name;
            return "Group-" + ID;
        }
        private UUID _id = UUID.Zero;
        public UUID ID
        {
            get
            {
                return _id;
            }
            set
            {
                _id = value;
            }
        }
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
