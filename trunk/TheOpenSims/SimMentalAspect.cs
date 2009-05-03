using System;
using System.Collections.Generic;
using System.Text;
using System.Reflection;
using cogbot.Listeners;
using OpenMetaverse;
using System.IO;
using DotLisp;
using System.Collections;

namespace cogbot.TheOpenSims
{
    // Mental Aspects
    public interface BotMentalAspect
    {
        //public BotMentalAspect(string s)
        //{
        //    AspectName = s;
        //}
        //public string AspectName;
        //public UUID SLRef; // the overhead bubble
        //public override string ToString()
        //{
        //    return GetType().Name + "::" + AspectName;
        //}
    }

    // See SimTypeSystem.cs for information on SimObjectType:BotMentalAspect

    // These needs are 0 - 100.0F     100.0 = satiafied (on the positive end i.g. less thirsty)
    public class BotNeeds
    {


        static public BotNeeds ZERO
        {
            get { return new BotNeeds(0.0f); }
        }
        static FieldInfo[] needFields = typeof(BotNeeds).GetFields();
        public IEnumerable<Object> GetNeeds()
        {
            return needFields;
        }
        public float GetNeed(object fi)
        {
            return GetValue(fi, this);
        }
        public void SetNeed(object fi, float v)
        {
            SetValue(fi, this, v);
        }
        public float GetValue(Object fi, BotNeeds newNeeds)
        {
            return (float)((FieldInfo)fi).GetValue(newNeeds);
        }

        public void SetValue(Object fi, BotNeeds needsBefore, object p)
        {
            if (!(p is Single))
            {
                p = float.Parse(p.ToString());
            }
            ((FieldInfo)fi).SetValue(needsBefore, p);
        }
        /*
         
        
        =======================================
        5.  Bot Needs
        =======================================

        ---------------------------------------
        5.1  The Eight Bot Needs
        ---------------------------------------

        ---------------------------------------
        5.1.1  Hunger
        ---------------------------------------
             The sims need to eat to survive.  The better the cook is and the better 
        the food equipment is, the more satisfaction they get from food.  If you are 
        a great cook and have good cooking equipment you won't have to eat so many 
        times in a day.  Who ever the best cook in your family is should cook as many 
        meals as possible.  I would suggest having a 2nd cook.

        ---------------------------------------
        5.1.2  Fun
        ---------------------------------------
             Just like humans your sims need to have fun.  Many objects including 
        televisions, radios, pool tables, computers, and many other objects make your 
        sims life fun.  More expensive objects like the big screen T.V. and the 
        §6,500 computer have extremely high fun ratings.  When your are building your 
        first home, buy a 27 inch T.V.

        ---------------------------------------
        5.1.3  Room
        ---------------------------------------
             The room rating is how well lighted and decorated a room is.  A few 
        windows to let light in, are needed for the daytime.  At night a few lights 
        are also needed.  Rooms that are decorated also help increase the room 
        rating.  Furniture, pictures, and other objects help to increase the room 
        rating.

        ---------------------------------------
        5.1.4  Social
        ---------------------------------------
             Social comes from how much you talk to other sims.  Look below under 7.0 
        to learn more about friends.  A good place to gain social is eating, watching 
        T.V., being in the spa, and other objects where you can accomplish more then 
        one thing at a time.

        ---------------------------------------
        5.1.5  Energy
        ---------------------------------------
             Bots need to be awake to do things so they need to have lots of energy.  
        There are only two ways to increase energy, sleeping and drinking 
        coffee/espresso.  When buying a bed try to buy the most expensive one you can 
        because your sim wont have to spend so many hours sleeping.  Espresso doesn't 
        really help your energy much and coffee helps increase your energy less then 
        espresso.

        ---------------------------------------
        5.1.6  Hygiene
        ---------------------------------------
             Your sims need to smell good at work and home.  Take a shower or wash 
        your hands to increase you hygiene.  One shower a day should be enough.  If 
        you use the toilet wash your hands afterwards, instead of before hand.

        ---------------------------------------
        5.1.7  Bladder
        ---------------------------------------
             You need to use the bathroom every so often.  When this bar goes red it 
        means you need to go immediately.  Use the toilet to empty your bladder, 
        obviously. 

        ---------------------------------------
        5.1.8  Comfort
        ---------------------------------------
             Your sims need to be comfortable.  You can gain comfort by sitting in a 
        chair or sleeping in bed.  You can also gain comfort in the bath tub.

        ---------------------------------------
        5.2 Health
        ---------------------------------------
             Your sims can become sick.  If you have the guinea pig and don't clean 
        the cage or feed it often enough you could become sick.  If you become sick 
        sell the Guinea Pig Cage immediately.  Once you have sold the Guinea Pig, 
        have the sick sim stay in bed and get lots of sleep.  Also make sure the 
        needs are all green.  Your sim should also get lots of coffee / espresso and 
        stay away from the other sims.  It is ok if your sim misses a day of work.
       
         */
        public float Energy = 0.0F; //energy
        public float Hunger = 0.0F;  //hunger 
        public float Bladder = 0.0F; //NeedToilet-ToNot
        public float Hygiene = 0.0F; //FilthyTo Clean
        public float Room = 0.0F; //ClostraphobicToSpaceious
        public float Social = 0.0F; //LonelyToSocialized
        public float Fun = 0.0F; //BoredToFun
        public float Comfort = 0.0F; //pain vs fitness //Uncomfortable-Comfort

        // extras
        public float ThirstyToFull = 0.0F; //thirst
        public float GenerallySadToHappy = 0.0F;
        public float Health = 0.0F;

        // personality
        //public float Neat, Outgoing, Active, Playful, Nice;
        //skills
        //public float Mechanical, Charisma, Body, Creativity, Logic, Cleaning, Cooking;


        public float Total()
        {
            float f = 0.0F;
            foreach (Object fi in GetNeeds())
            {
                f += GetNeed(fi);
            }
            return f;
        }

        public override string ToString()
        {
            string str = "BotNeeds:";
            foreach (Object fi in GetNeeds())
            {
                str += "\r\n\t" + Name(fi) + "=" + GetNeed(fi);
            }
            return str;
        }

        private string Name(object fi)
        {
            if (fi is FieldInfo)
                return ((FieldInfo)fi).Name;
            return fi.ToString();
        }

        public BotNeeds(float inital)
        {
            SetAll(inital);
        }

        public void AddFrom(BotNeeds needsDiff)
        {
            foreach (Object fi in GetNeeds())
            {
                SetNeed(fi, GetNeed(fi) + needsDiff.GetNeed(fi));
            }
        }

        public void SetFrom(BotNeeds newNeeds)
        {
            foreach (Object fi in GetNeeds())
            {
                SetNeed(fi, newNeeds.GetNeed(fi));
            }
        }

        public void SetAll(float newValue)
        {
            foreach (Object fi in GetNeeds())
            {
                SetNeed(fi, newValue);
            }
        }

        public BotNeeds Copy()
        {
            BotNeeds needsBefore = new BotNeeds(0.0F);
            needsBefore.SetFrom(this);
            return needsBefore;
        }

        public BotNeeds Magnify(float mag)
        {
            BotNeeds needsBefore = new BotNeeds(0.0F);
            foreach (Object fi in GetNeeds())
            {
                needsBefore.SetNeed(fi, GetNeed(fi) * mag);
            }
            return needsBefore;
        }


        public void SetRange(float p, float m)
        {
            foreach (Object fi in GetNeeds())
            {
                float f = GetNeed(fi);
                if (f < p)
                {
                    f = p;
                }
                if (f > m)
                {
                    f = m;
                }
                SetNeed(fi, f);
            }
        }

        public float TotalSideEffect(BotNeeds bn) {
            bn = bn.Copy();           
            bn.AddFrom(this);
            bn.SetRange(0f, 100f);
            return bn.Total();
        }

        public BotNeeds Minus(BotNeeds needsBefore)
        {
            BotNeeds copy = Copy();
            foreach (Object need in copy.GetNeeds())
            {
                copy.SetNeed(need, copy.GetNeed(need) - needsBefore.GetNeed(need));
            }
            return copy;
        }
        public string ShowNonZeroNeeds()
        {
            String str = "";
            foreach (Object fi in GetNeeds())
            {
                float f = GetNeed(fi);
                if (f != 0.0F) str += " " + Name(fi) + "=" + ((f < 0.0F) ? "" + f : "+" + f);

            }
            return str;
        }
    }

    public class ListAsSet<T> : List<T>
    {
        public void Clear()
        {
            lock (this)
                base.Clear();
        }
        // synchronization
        public bool Remove(T item)
        {
            lock (this)
                return base.Remove(item);
        }

        // synchronization
        public void ForEach(Action<T> act)
        {
            foreach (T item in CopyOf())
            {
                act(item);
            }
        }

        // synchronization
        public T Find(Predicate<T> act)
        {
            foreach (T item in CopyOf())
            {
                if (act(item)) return item;
            }
            return default(T);
        }

        public bool AddTo(T item)
        {
            lock (this)
            {
                if (true)
                {
                    {
                        IEnumerator enumer = base.GetEnumerator();
                        while (enumer.MoveNext())
                        {
                            if (item.Equals((T)enumer.Current)) return false;
                        }
                    }
                }
                else
                {
                    if (base.Contains(item)) return false;
                }
                base.Add(item);
                return true;
            }
        }

        // return a copy
        public Enumerator GetEnumerator()
        {
            return CopyOf().GetEnumerator();
        }

        public class BaseEnumerable : IEnumerable<T>
        {
            readonly IEnumerator<T> be;
            public BaseEnumerable(IEnumerator<T> r)
            {
                be = r;
            }


            #region IEnumerable<T> Members

            IEnumerator<T> IEnumerable<T>.GetEnumerator()
            {
                return be;
            }

            #endregion

            #region IEnumerable Members

            IEnumerator IEnumerable.GetEnumerator()
            {
                return be;
            }

            #endregion
        }
        // return the fast underlying
        public IEnumerable<T> GetBaseEnumerable()
        {
            return new BaseEnumerable(base.GetEnumerator());
        }
        // synchronization
        public List<T> CopyOf()
        {
            List<T> list = new List<T>();
            lock (this)
            {
                IEnumerator enumer = base.GetEnumerator();
                while (enumer.MoveNext())
                {
                    list.Add((T)enumer.Current);
                }
            }
            return list;
        }

        public new void Add(T item)
        {
            AddTo(item);
        }
        public override string ToString()
        {
            List<T> copy = CopyOf();
            switch (copy.Count)
            {
                case 0: return "[]";
                case 1: return "[" + copy[0] + "]";
                default:
                    {
                        String s = "";
                        foreach (T t in copy)
                        {
                            s += "," + t;
                        }
                        return "[" + s.Substring(1) + "]";
                    }
            }
        }
    }

}
