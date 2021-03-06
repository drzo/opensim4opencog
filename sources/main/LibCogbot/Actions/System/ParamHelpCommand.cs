using System;
using System.Collections;
using System.Collections.Generic;
using System.Text;
using Cogbot.World;
using OpenMetaverse;
using MushDLR223.ScriptEngines;

namespace Cogbot.Actions.System
{
    internal class ParamHelp : Command, SystemApplicationCommand
    {
        public ParamHelp(BotClient Client)
            : base(Client)
        {
        }

        public override void MakeInfo()
        {
            Description = "Explains predicate and features that are availble for PrimSpecs";
            AddVersion(CreateParams(Optional("commandMask", typeof (string), "term to search for")), Description);
            ResultMap = CreateParams(
                "message", typeof (string), "if term not found, will be <i>I don't know about ...</i>",
                "success", typeof (bool), "true if it found a match");
            Category = CommandCategory.BotClient;
            Name = "paramhelp";
        }

        public override CmdResult ExecuteRequest(CmdRequest args)
        {
            string str;
            if (args.TryGetValue("commandMask", out str))
            {
                SortedList<string, CommandInfo> all = null;
                if (TheBotClient != null)
                {
                    all = TheBotClient.AllCommands();
                }
                else
                {
                    all = ClientManager.SingleInstance.AllCommands();
                }
                foreach (var cmdinfo in all)
                {
                    if (cmdinfo.Value.Matches(str))
                    {
                        WriteLine(cmdinfo.Value.ToPrologString());
                    }
                }
            }
            WriteLine("GroupNames:");
            foreach (var name  in  WorldSystem.GroupNames)
            {
                WriteLine("$" + name);
            }
            WriteLine("Filters:");
            Type typesearch = typeof (SimObjectImpl);
            IEnumerable<FilterMember> filters = WorldSystem.GetFilters(typesearch);
            foreach (FilterMember fmemb in filters)
            {
                if (!fmemb.IsCollectionType) continue;
                WriteLine(typeString(fmemb.ReturnType) + " < = "
                          + fmemb.ReflectionMember.Name + (fmemb.IsOf ? "Of" : "")
                          + typeString(fmemb.CastArgTo));
            }
            foreach (FilterMember fmemb in filters)
            {
                if (fmemb.IsCollectionType) continue;
                WriteLine("[!]"
                          + fmemb.ReflectionMember.Name + (fmemb.IsOf ? "Of" : "")
                          + (fmemb.PreCompare ? "<>" : "")
                          + typeString(fmemb.CastArgTo));
            }
            return Success("Help complete");
        }

        private string typeString(Type type)
        {
            if (type == null) return " ";
            if (typeof (IConvertible).IsAssignableFrom(type)) return " " + type.Name;
            if (typeof (IEnumerable).IsAssignableFrom(type)) return " List";
            return " " + type.Name;
        }
    }
}