using System;
using System.Collections.Generic;
using System.Text;
using OpenMetaverse; //using libsecondlife;

namespace cogbot.Listeners
{
#pragma warning disable 0168
    public class Avatars : Listener
    {
        protected Dictionary<string, Avatar> avatarCache;
        private Vector3 compPos;
        private int searchStep;
        private List<string> numberedAvatars;

        public Avatars(TextForm parent)
            : base(parent)
        {
            searchStep = 1;
            avatarCache = new Dictionary<string,Avatar>();
            numberedAvatars = new List<string>();

            client.Objects.OnNewAvatar += new ObjectManager.NewAvatarCallback(Objects_OnNewAvatar);
        }

        void Objects_OnNewAvatar(Simulator simulator, Avatar avatar, ulong regionHandle, ushort timeDilation)
        {
            try
            {
                lock (avatarCache)
                {
                    if (avatar != null)
                    {
                        if (!avatarCache.ContainsKey(avatar.Name))
                        {
                            avatarCache[avatar.Name] = avatar;
                        }
                    }
                }
            describeAvatarToAI(avatar);
            }
            catch(Exception e)
                {
                    parent.output("err :" + e.StackTrace);
                }
        }

        public int numAvatars()
        {
            return avatarCache.Count;
        }

        public int comp(Avatar a1, Avatar a2)
        {
            return (int)(Vector3.Distance(a1.Position, compPos) - Vector3.Distance(a2.Position, compPos));
        }

        public List<Avatar> getAvatarsNear(Vector3 pos, int num)
        {
            compPos = pos;
            List<Avatar> avatarList = new List<Avatar>();
            foreach (Avatar avatar in avatarCache.Values)
                if (avatar.Name != client.Self.Name)
                    avatarList.Add(avatar);

            if (avatarList.Count > num)
            {
                avatarList.Sort(new Comparison<Avatar>(comp));

                for (; searchStep * num > avatarList.Count; --searchStep) ;

                List<Avatar> ret = new List<Avatar>();
                for (int i = 0; i < num && i < avatarList.Count; i += searchStep)
                    ret.Add(avatarList[i]);
                searchStep = (searchStep + 1) % 4 + 1;
                updateNumberedAvatars(ret);
                return ret;
            }
            else
            {
                updateNumberedAvatars(avatarList);
                return avatarList;
            }
        }

        private void updateNumberedAvatars(List<Avatar> avatars)
        {
            numberedAvatars.Clear();
            for (int i = 0; i < avatars.Count; ++i)
                numberedAvatars.Add(avatars[i].Name);
        }

        public bool tryGetAvatarById(UUID id, out Avatar avatar)
        {
            avatar = null;
            foreach (Avatar av in avatarCache.Values)
            {
                if (av.ID == id)
                {
                    avatar = av;
                    return true;
                }
            }
            return false;
        }

        public bool tryGetAvatar(string name, out Avatar avatar)
        {
            avatar = null;

            string[] toks = name.Split(null);
            if (toks.Length == 2 && toks[0] == "person")
            {
                try
                {
                    int i = Convert.ToInt32(toks[1]);
                    if (i > 0 && i <= numberedAvatars.Count)
                    {
                        avatar = avatarCache[numberedAvatars[i - 1]];
                        return true;
                    }
                }
                catch (FormatException e)
                {
                }
            }

            if (avatarCache.ContainsKey(name))
            {
                avatar = avatarCache[name];
                return true;
            }

            foreach (string avatarName in avatarCache.Keys)
            {
                if (avatarName.Length >= name.Length && avatarName.Substring(0, name.Length) == name)
                {
                    avatar = avatarCache[avatarName];
                    return true;
                }
            }

            return false;
        }

        public string getAvatarName(Avatar avatar)
        {
            string name = avatar.Name;
            for (int i = 0; i < numberedAvatars.Count; ++i)
                if (numberedAvatars[i] == name)
                    name = (i + 1) + ": " + name;
            return name;
        }

        public void describeAvatar(Avatar avatar)
        {
            string verb;
            if (avatar.SittingOn == 0)
                verb = "standing";
            else
                verb = "sitting";
            parent.output(avatar.Name + " is " + verb + " in " + avatar.CurrentSim.Name + ".");
            parent.output(avatar.Name + " is " + Vector3.Distance(client.Self.SimPosition, avatar.Position).ToString() + " distant.");
            if (avatar.ProfileProperties.BornOn != null)
                parent.output("Born on: " + avatar.ProfileProperties.BornOn);
            if (avatar.ProfileProperties.AboutText != null)
                parent.output("About their second life: " + avatar.ProfileProperties.AboutText);
            if (avatar.ProfileProperties.FirstLifeText != null)
                parent.output("About their first life: " + avatar.ProfileProperties.FirstLifeText);
            if (avatar.ProfileInterests.LanguagesText != null)
                parent.output("Languages spoken: " + avatar.ProfileInterests.LanguagesText);
            if (avatar.ProfileInterests.SkillsText != null)
                parent.output("Skills: " + avatar.ProfileInterests.SkillsText);
            if (avatar.ProfileInterests.WantToText != null)
                parent.output("Wants to: " + avatar.ProfileInterests.WantToText);
        }

        public void describeAvatarToAI(Avatar avatar)
        {
            string verb;
            if (avatar.SittingOn == 0)
                verb = "standing";
            else
                verb = "sitting";
            //parent.output(avatar.Name + " is " + verb + " in " + avatar.CurrentSim.Name + ".");
            //parent.output(avatar.Name + " is " + Vector3.Distance(client.Self.SimPosition, avatar.Position).ToString() + " distant.");

            parent.enqueueLispTask("(on-avatar-dist (@\"" + avatar.Name + "\") " + Vector3.Distance(client.Self.SimPosition, avatar.Position).ToString() + " )");
            parent.enqueueLispTask("(on-avatar-pos (@\"" + avatar.Name + "\") (@\"" + avatar.Position.ToString() + "\") )");
            parent.enqueueLispTask("(on-avatar-posture (@\"" + avatar.Name + "\") (@\"" + verb + "\") )");
            
            /*
            if (avatar.ProfileProperties.BornOn != null)
                parent.output("Born on: " + avatar.ProfileProperties.BornOn);
            if (avatar.ProfileProperties.AboutText != null)
                parent.output("About their second life: " + avatar.ProfileProperties.AboutText);
            if (avatar.ProfileProperties.FirstLifeText != null)
                parent.output("About their first life: " + avatar.ProfileProperties.FirstLifeText);
            if (avatar.ProfileInterests.LanguagesText != null)
                parent.output("Languages spoken: " + avatar.ProfileInterests.LanguagesText);
            if (avatar.ProfileInterests.SkillsText != null)
                parent.output("Skills: " + avatar.ProfileInterests.SkillsText);
            if (avatar.ProfileInterests.WantToText != null)
                parent.output("Wants to: " + avatar.ProfileInterests.WantToText);
            */
        
        }

    }
#pragma warning restore 0168
}
