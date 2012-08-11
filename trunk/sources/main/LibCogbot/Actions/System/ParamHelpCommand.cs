using System;
using System.Collections;
using System.Collections.Generic;
using System.Text;
using Cogbot.World;
using OpenMetaverse;

using MushDLR223.ScriptEngines;

namespace Cogbot.Actions.System
{
    class ParamHelp : Command, SystemApplicationCommand
    {
        public ParamHelp(BotClient Client)
            : base(Client)
        {
            Description = "Explains predicate and features that are availble for PrimSpecs";
            AddVersion(CreateParams("term", typeof (string), "term to search for"), Description);
            ResultMap = CreateParams(
                 "message", typeof(string), "if term not found, will be <i>I don't know about ...</i>",
                 "success", typeof(bool), "true if it found a match");
            Category = CommandCategory.BotClient;
            Name = "paramhelp";
        }

        public override CmdResult acceptInput(string verb, Parser args, OutputDelegate WriteLine)
        {
            WriteLine("GroupNames:");
            foreach(var name  in  WorldSystem.GroupNames)
            {
                WriteLine("$" + name);                
            }
            WriteLine("Filters:");
            Type typesearch = typeof(SimObject);
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
                          + (fmemb.PreCompare ? "[<|>|=]" : "")
                          + typeString(fmemb.CastArgTo));
            }
            return Success("Help complete");
        }

        private string typeString(Type type)
        {
            if (type == null) return " ";
            if (typeof(IConvertible).IsAssignableFrom(type)) return " " + type.Name;
            if (typeof(IEnumerable).IsAssignableFrom(type)) return " List";
            return " " + type.Name;
        }
    }
}
