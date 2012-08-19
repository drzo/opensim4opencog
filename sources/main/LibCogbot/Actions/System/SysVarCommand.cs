using System;
using System.Collections;
using System.Collections.Generic;
using System.IO;
using System.Text;
using System.Xml;
using Cogbot;
using Cogbot.World;
using Cogbot.Utilities;
using MushDLR223.Utilities;
using OpenMetaverse;
using PathSystem3D.Navigation;

using MushDLR223.ScriptEngines;

namespace Cogbot.Actions.System
{
    public class SysVarCommand : Cogbot.Actions.Command, BotSystemCommand
    {
        public SysVarCommand(BotClient client): base(client)
        {
            Name = "sysvar";
            Description = "Manipulates system variables." +
                          " These are global settings that affect Cogbot." +
                          "Many SysVars contain the word 'Maintain' (eg MaintainSounds). Generally this means" +
                          "Cogbot won't make a special request from the server to get information about this sort of thing" +
                          "and will provide information about it only if available" +
                          "For booleans anything but no or false (case insensitive) is true.";
            AddVersion(CreateParams(Optional("save", typeof(string), "filename.xml to save to"),
                                    Optional("load", typeof(string), "filename.xml to load from")),
                       "List all Sysvars and their settings");
            AddVersion(CreateParams(
                           "key", typeof (string), "substring to match sysvar names",
                           Optional("value", typeof(object), "value to set")),
                       "Show sysvars matching key if value is supplied it tried to set those values");

            Details = AddExample("sysvar CanUseSit True", "allow the bot to sit on things") +
                      AddExample("sysvar CanUseSit no", "don't allow the bot to sit on things") +
                      AddExample("sysvar Maintain false",
                              "set every sysvar that contains Maintain in it's name to false") +
                      AddExample("sysvar MaintainEffectsDistance 8.0",
                              "set the maximum distance to notice effects to 8.0");


            Category = CommandCategory.BotClient;
        }

        public string SysVarHtml()
        {
            StringBuilder sb = new StringBuilder();
            sb.AppendLine("<table><tr><th>Variable Name</th><th>current value</th><th>Description</th></tr>");
            foreach (var sv in GetSysVars())
            {
                IKeyValuePair<string,object> svv = sv;
                sb.AppendLine(string.Format("<tr name=\"{0}\" id='{0}'><td>{0}</td><td>{1}</td><td>{2}</td></tr>", Htmlize.NoEnts(svv.Key), Htmlize.NoEnts("" + svv.Value), Htmlize.NoEnts(svv.Comments)));
            }
            sb.AppendLine("</table>");
            return sb.ToString();
        }

        public int LoadSysVarHtml(Stream content)
        {
            var sysvars = GetSysVars();
            var doc = new XmlDocumentLineInfo().ReadNode(XmlReader.Create(content));
            if (doc == null) return -1;
            if (doc.Name == "html") doc = doc.FirstChild;
            if (doc == null) return -1;
            if (doc.Name == "head") doc = doc.NextSibling;
            if (doc == null) return -1;
            if (doc.Name == "body") doc = doc.FirstChild;
            if (doc == null) return -1;
            if (doc.Name == "table") doc = doc.FirstChild;
            if (doc == null) return -1;
            int lineCount = 0;
            while (doc != null && doc.Name == "tr")
            {
                var fc = doc.FirstChild;
                if (fc.Name == "td")
                {
                    string svname = fc.InnerText;
                    var sc = fc.NextSibling;
                    string svalue = sc.InnerText;
                    bool found = false;
                    foreach (var s in ScriptManager.FindMatchingSysvars(sysvars, svname, true, true))
                    {
                        found = true;
                        lineCount++;
                        s.Value = svalue;
                    }
                    if (!found) Failure("Cant set: " + svname + "  " + svalue);
                }
                doc = doc.NextSibling;
            }
            return lineCount;
        }

        private IList<IKeyValuePair<string, object>> GetSysVars()
        {
            return LockInfo.CopyOf(ScriptManager.GetSysVars(TheBotClient));
        }


        public override CmdResult ExecuteRequest(CmdRequest args)
        {
            string filename;
            if (args.TryGetValue("save", out filename))
            {
                File.WriteAllText(filename,SysVarHtml());
                return Success("Wrote " + filename);
            }
            if (args.TryGetValue("load", out filename))
            {
                LoadSysVarHtml(File.OpenRead(filename));
                return Success("Loaded " + filename);
            }
            if (args.ContainsFlag("htmldoc"))
            {
                return Success(SysVarHtml());
            }

            bool exactMatch = args.ContainsFlag("--exact");
            bool caseSensitive = args.ContainsFlag("--case");

            int used = 0;
            var sysvars = GetSysVars();
            string find;
            if (!args.TryGetValueOr("key", 0, out find))
            {
                // display all
                foreach (var sv in sysvars)
                {
                    var svv = sv;
                    WriteLine(string.Format("{0}={1} //{2}", (svv.Key), svv.Value, svv.Comments));
                }
                return Success("count=" + sysvars.Count);
            }
            if (!caseSensitive) find = find.ToLower();
            var setThese = ScriptManager.FindMatchingSysvars(sysvars, find, exactMatch, caseSensitive);
            int found = 0;
            foreach (var svv in setThese)
            {
                {
                    found++;
                    WriteLine(svv.DebugInfo);
                }
            }
            string value;
            if (!args.TryGetValueOr("value", 1,  out value))
            {
                return Success("Found sysvars: " + found);             
            }
            int changed = 0;
            foreach(var one in setThese)
            {
                try
                {
                    one.Value = value;
                    AddSuccess("Set sysvar: " + one.Key + " to " + one.Value);
                    changed++;
                } catch(Exception e)
                {
                    
                }
            }
            return Success("set vars = " + changed);
        }
    }
}