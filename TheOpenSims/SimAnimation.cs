using System;
using System.Collections.Generic;
using System.Text;
using OpenMetaverse;
using System.Threading;
using System.Reflection;
using cogbot.Listeners;

namespace cogbot.TheOpenSims
{


    public class AnimThread
    {
        AgentManager ClientSelf;
        UUID anim;
        bool repeat = true;
        Thread animLoop;
        public AnimThread(AgentManager c, UUID amin0)
        {
            ClientSelf = c;//.Self;
            if (cogbot.TheOpenSims.SimAnimation.GetAnimationName(amin0).StartsWith("S"))
            {
                repeat = false;
            }
            anim = amin0;
        }

        public override string ToString()
        {
            return "AnimLoop " + anim + " of " + ClientSelf;
        }

        public void Start()
        {
            animLoop = new Thread(new ThreadStart(LoopAnim));
            animLoop.Name = "Thread for " + this.ToString();
            animLoop.Start();
        }
        void LoopAnim()
        {
            try
            {
                ClientSelf.AnimationStart(anim, true);
                while (repeat)
                {
                    // some anims will only last a short time so we have to 
                    // remind the server we still want to be ussing it 
                    // like Laugh .. lasts for about .9 seconds
                    //12000 is a estimate avage
                    Thread.Sleep(3200);
                    ClientSelf.AnimationStop(anim, true);
                    ClientSelf.AnimationStart(anim, true);
                }
            }
            catch (Exception) { } // for the Abort 
        }
        public void Stop()
        {
            repeat = false;
            if (animLoop != null)
            {
                try
                {
                    if (animLoop.IsAlive) animLoop.Abort();
                }
                catch (Exception) { }
                animLoop = null;
            }
            ClientSelf.AnimationStop(anim, true);
        }
    }

    public class SimAnimation
    {
        static Dictionary<UUID, string> animationName = new Dictionary<UUID, string>();
        static Dictionary<string, UUID> nameAnimation = new Dictionary<string, UUID>();

        public UUID AnimationID;
        public string Name;

        public SimAnimation(UUID anim, String name)
        {
            AnimationID = anim;
            Name = name;
        }
        static void FillAnimationNames()
        {
            lock (animationName)
            {
                if (animationName.Count > 0) return;


                foreach (FieldInfo fi in typeof(Animations).GetFields())
                {
                    UUID uid = (UUID)fi.GetValue(null);
                    string uids = uid.ToString();
                    animationName[uid] = fi.Name;
                    WorldObjects.RegisterUUID(uid, fi.Name);
                    nameAnimation[fi.Name] = uid;
                }
            }
        }
        public static ICollection<string> GetAnimationList()
        {
            FillAnimationNames();
            return nameAnimation.Keys;
        }
        public static String GetAnimationName(UUID uuid)
        {
            FillAnimationNames();
            String name;
            if (animationName.TryGetValue(uuid, out name))
            {
                return name;
            }
            return uuid.ToString();
        }


        public static UUID GetAnimationUUID(string a)
        {
            a = a.ToLower();
            FillAnimationNames();
            UUID partial = default(UUID);
            foreach (String name in nameAnimation.Keys)
            {
                String sname = name.ToLower();
                if (sname.Equals(a))
                {
                    return nameAnimation[name];
                }
                if (sname.Contains(a))
                {
                    partial = nameAnimation[name];
                }
            }
            return partial;

        }

    }
}
