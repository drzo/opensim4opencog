using System;
using System.Collections.Generic;
using System.Text;
using MushDLR223.Utilities;
using OpenMetaverse;
using cogbot.TheOpenSims; //using libsecondlife;

using MushDLR223.ScriptEngines;

namespace cogbot.Actions.Land
{
    class Describe : Command, BotPersonalCommand
    {
        public Describe(BotClient Client)
            : base(Client)
        {
            Name = "Describe";
            Category = CommandCategory.Objects;
            Description = "Describe an object.";
            Details = @"<p>evinfo &lt;primspec&gt;</p><p>example: evinfo tacosofgod  <i>tacosofgod is a nearby plywood cube</i></p>
<pre>
[09:18] tacosofgod Box 70b5e8ab-3308-4bc6-bbf8-4f313cd7d518 (localID 2036105563)(ch0)(PrimFlagsFalse InventoryEmpty, ObjectOwnerModify)[](!IsPassable)
 1.53m annies haven II/84.77/138.42/21.56
 GroupLeader: tacosofgod Box 70b5e8ab-3308-4bc6-bbf8-4f313cd7d518 (localID 2036105563)(ch0)(PrimFlagsFalse InventoryEmpty, ObjectOwnerModify)[](!IsPassable)
 Light: 
 TextureEntry:
  Default texture: 89556747-24cb-43ed-920b-47caed15465f
[09:18] PS.Count==1
[09:18] Success: describe complete
[09:18] Success: describe complete
</pre>";
            Parameters = CreateParams("object", typeof(PrimSpec),
                "The object to describe, as specified in <a href='wiki/BotCommands#PrimSpec'>Prim Spec</a>");
            ResultMap = CreateParams(
                 "message", typeof(string), "if success was false, the reason why",
                 "success", typeof(bool), "true if we got the description");
        }

        public override string Description
        {
            get
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
            set
            {
                base.Description = value;
            }
        }

        public override string Details
        {
            get
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
            if (!Client.IsLoggedInAndReady)
            {
                WriteLine("Not yet logged in");
                return;
            }
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
                    Success("looked at range " + range + " and saw " + objs.Count);
                }
                Success("We have no body yet");
            }
            else
            {
                if (TheBotClient.describers.ContainsKey(subject))
                    TheBotClient.describers[subject].Invoke(true, WriteLine);
                else
                {
                    {
                        int found = 0;
                        int argsUsed;
                        List<SimObject> PS = WorldSystem.GetPrimitives(args.tokens, out argsUsed);
                        bool detailed = true;
                        if (PS.Count > 1) detailed = false;
                        if (!IsEmpty(PS)) 
                        foreach (var prim in PS)
                        {
                            found++;
                            if (!prim.HasPrim)
                            {
                                WriteLine("" + prim);
                                continue;
                            }
                            if (prim is SimAvatar)
                                WriteLine(WorldSystem.describeAvatar((Avatar) prim.Prim));
                            else
                                WriteLine(WorldSystem.describePrim(prim.Prim, detailed));
                            //if (found > 30) break;
                        }
                        else
                        {
                            foreach (var o in WorldSystem.GetAllSimObjects(args.str))
                            {
                                found++;
                                if (o is SimAvatar)
                                    WriteLine(WorldSystem.describeAvatar((Avatar)o.Prim));
                                else
                                    WriteLine(WorldSystem.describePrim(o.Prim, false));
                                if (found > 30) break;
                            }
                        }
                        if (found == 0) WriteLine("I don't know about " + subject + ".");
                        WriteLine("PS.Count==" + PS.Count);
                    }
                }
            }

        }
    }
}
