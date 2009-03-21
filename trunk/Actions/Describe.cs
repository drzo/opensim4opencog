using System;
using System.Collections.Generic;
using System.Text;
using OpenMetaverse;
using cogbot.TheOpenSims; //using libsecondlife;

namespace cogbot.Actions
{
    class Describe : Action
    {
        public Describe(BotClient Client)
            : base(Client)
        {
        }

        public override string makeHelpString()
        {
            string str = "Describe ";
            string[] names = new string[Client.describers.Count];
            Client.describers.Keys.CopyTo(names, 0);
            for (int i = 0; i < Client.describers.Count - 1; ++i)
                str += names[i] + ", ";
            str += "or " + names[Client.describers.Count - 1] + ".";
            return str;
        }

        public override string makeUsageString()
        {
            string str = "\"describe\": describes everything around you \r\n you can also type ";
            string[] names = new string[Client.describers.Count];
            Client.describers.Keys.CopyTo(names, 0);
            for (int i = 0; i < Client.describers.Count - 1; ++i)
                str += "\"describe " + names[i] + "\", ";
            str += "or \"describe " + names[Client.describers.Count - 1] + "\" to describe them respectively.";

            return str;
        }

        public override void acceptInput(string verb, Parser args)
        {
            //   base.acceptInput(verb, args);

            string subject = args.objectPhrase;
            if (subject.Length == 0)
            {
                Client.describeAll();
                Client.describeSituation();
                return;
            }
            Client.describeNext = false;

            if (subject == "inventory")
            {
                //Client.ListObjectsFolder();
                Client.PrintInventoryAll();
                return;
            }
            float range;
            if (float.TryParse(subject,out range)) {
                SimAvatar simAva = Client.WorldSystem.TheSimAvatar;
                if (simAva != null)
                {
                    List<SimObject> objs = simAva.GetNearByObjects(range, false);
                    if (objs.Count > 0)
                    {
                        foreach (SimObject o in objs)
                        {
                            WriteLine(Client.WorldSystem.describePrim(o.Prim));
                        }
                        return;
                    }
                }
            }
            else
            {
                if (Client.describers.ContainsKey(subject))
                    Client.describers[subject].Invoke(true);
                else
                {
                    Avatar avatar;
                    if (Client.WorldSystem.tryGetAvatar(subject, out avatar))
                        Client.WorldSystem.describeAvatar(avatar);
                    else
                    {
                        Primitive prim;
                        if (Client.WorldSystem.tryGetPrim(args.str, out prim))
                            WriteLine(Client.WorldSystem.describePrim(prim));
                        else
                        {

                            WriteLine("I don't know about " + subject + ".");
                        }
                    }
                }
            }

        }
    }
}
