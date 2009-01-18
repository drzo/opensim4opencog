using System;
using System.Collections.Generic;
using System.Text;
using OpenMetaverse; //using libsecondlife;

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
                        if (Client.WorldSystem.tryGetPrim(subject, out prim))
                            Client.WorldSystem.describePrim(prim);
                        else
                        {
                            if (subject == "inventory")
                            {
                                //Client.ListObjectsFolder();
                                Client.PrintInventoryAll();
                            }
                            else
                            WriteLine("I don't know about " + subject + ".");
                        }
                    }
                }
            }

            Client.describeNext = false;
        }
    }
}
