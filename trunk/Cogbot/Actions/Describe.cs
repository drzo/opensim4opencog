using System;
using System.Collections.Generic;
using System.Text;
using OpenMetaverse;
using cogbot.TheOpenSims; //using libsecondlife;

namespace cogbot.Actions
{
    class Describe : Command
    {
        public Describe(BotClient Client)
            : base(Client)
        {
            Name = "Describe";
            Parameters = new []{ new NamedParam(typeof(SimObject), typeof(UUID))};
        }

        public override string makeHelpString()
        {
            BotClient Client = TheBotClient;
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

            BotClient Client = TheBotClient;
            string str = "\"describe\": describes everything around you \r\n you can also type ";
            string[] names = new string[Client.describers.Count];
            Client.describers.Keys.CopyTo(names, 0);
            for (int i = 0; i < Client.describers.Count - 1; ++i)
                str += "\"describe " + names[i] + "\", ";
            str += "or \"describe " + names[Client.describers.Count - 1] + "\" to describe them respectively.";

            return str;
        }

        public override CmdResult acceptInput(string verb, Parser args, OutputDelegate WriteLine)
        {
            acceptInput0(verb, args, WriteLine);
            return Success(verb + " complete");
        }

        void acceptInput0(string verb, Parser args, OutputDelegate WriteLine)
        {
            //   base.acceptInput(verb, args);

            BotClient Client = TheBotClient;
            string subject = args.objectPhrase;
            if (subject.Length == 0)
            {
                Client.describeAll(false, WriteLine);
              //  Client.describeSituation(WriteLine);
                return;
            }
            Client.describeNext = false;

            if (subject == "inventory")
            {
                //Client.ListObjectsFolder();
                TheBotClient.PrintInventoryAll();
                return;
            }
            float range;
            if (float.TryParse(subject,out range)) {
                SimAvatar simAva = WorldSystem.TheSimAvatar;
                if (simAva != null)
                {
                    List<SimObject> objs = ((SimObjectImpl)simAva).GetNearByObjects((double)range, false);
                    if (objs.Count > 0)
                    {
                        foreach (SimObject o in objs)
                        {
                            WriteLine(WorldSystem.describePrim(o.Prim, false));
                        }
                        return;
                    }
                }
            }
            else
            {
                if (TheBotClient.describers.ContainsKey(subject))
                    TheBotClient.describers[subject].Invoke(true, WriteLine);
                else
                {
                    {
                        Primitive prim;
                        if (WorldSystem.tryGetPrim(args.str, out prim))
                        {
                            if (prim is Avatar)
                                WriteLine(WorldSystem.describeAvatar((Avatar) prim));
                            else
                                WriteLine(WorldSystem.describePrim(prim, true));
                        }
                        else
                        {

                            int found = 0;
                            foreach (var o in WorldSystem.GetAllSimObjects(args.str))
                            {
                                found++;
                                if (o is SimAvatar)
                                    WriteLine(WorldSystem.describeAvatar((Avatar)o.Prim));
                                else
                                    WriteLine(WorldSystem.describePrim(o.Prim, false));
                                if (found > 30) break;
                            }
                            if (found == 0) WriteLine("I don't know about " + subject + ".");
                        }
                    }
                }
            }

        }
    }
}
