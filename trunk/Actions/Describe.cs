using System;
using System.Collections.Generic;
using System.Text;
using OpenMetaverse; //using libsecondlife;

namespace cogbot.Actions
{
    class Describe : Action
    {
        public Describe(TextForm parent)
            : base(parent)
        {
        }

        public override string makeHelpString()
        {
            string str = "Describe ";
            string[] names = new string[parent.describers.Count];
            parent.describers.Keys.CopyTo(names, 0);
            for (int i = 0; i < parent.describers.Count - 1; ++i)
                str += names[i] + ", ";
            str += "or " + names[parent.describers.Count - 1] + ".";
            return str;
        }

        public override string makeUsageString()
        {
            string str = "\"describe\": describes everything around you \r\n you can also type ";
            string[] names = new string[parent.describers.Count];
            parent.describers.Keys.CopyTo(names, 0);
            for (int i = 0; i < parent.describers.Count - 1; ++i)
                str += "\"describe " + names[i] + "\", ";
            str += "or \"describe " + names[parent.describers.Count - 1] + "\" to describe them respectively.";

            return str;
        }

        public override void acceptInput(string verb, Parser args)
        {
         //   base.acceptInput(verb, args);

            string subject = args.objectPhrase;
            if (subject.Length == 0)
            {
                parent.describeAll();
                parent.describeSituation();
            }
            else
            {
                if (parent.describers.ContainsKey(subject))
                    parent.describers[subject].Invoke(true);
                else
                {
                    Avatar avatar;
                    Listeners.Avatars avatars = (Listeners.Avatars)parent.listeners["avatars"];
                    if (avatars.tryGetAvatar(subject, out avatar))
                        avatars.describeAvatar(avatar);
                    else
                    {
                        Primitive prim;
                        Listeners.Objects objects = (Listeners.Objects)parent.listeners["objects"];
                        if (objects.tryGetPrim(subject, out prim))
                            objects.describePrim(prim);
                        else
                        {
                            if (subject == "inventory")
                            {
                                //parent.ListObjectsFolder();
                                parent.PrintInventoryAll();
                            }
                            else
                            parent.output("I don't know about " + subject + ".");
                        }
                    }
                }
            }

            parent.describeNext = false;
        }
    }
}
