using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Xml;
using MushDLR223.ScriptEngines;
using MushDLR223.Utilities;
using RTParser;
using RTParser.Utils;
using RTParser.Variables;

namespace RTParser.Database
{
    public class NamedValuesFromSettings: StaticAIMLUtils
    {
        static public bool UseLuceneForGet = true;
        static public bool UseLuceneForSet = true;
        static public Unifiable GetSettingForType(string type, SubQuery query, ISettingsDictionary dict, string name, out string realName, string gName, Unifiable defaultVal, out bool succeed, XmlNode node)
        {
            Request request = query.Request;
            OutputDelegate writeToLog = query.Result.WriteLine;
            RTPBot TargetBot = request.TargetBot;
            ISettingsDictionary udict = FindDict(type, query) ?? dict;
            // try to use a global blackboard predicate
            RTParser.User gUser = TargetBot.ExemplarUser;

            defaultVal = RTPBot.GetAttribValue(node, "default,defaultValue", defaultVal);
            gName = RTPBot.GetAttribValue(node, "global_name", gName);

            succeed = false;
            Unifiable resultGet = SettingsDictionary.grabSettingDefaultDict(udict, name, out realName);

            if (ReferenceEquals(resultGet, null))
            {
                resultGet = Unifiable.NULL;
            }
            // if ((!String.IsNullOrEmpty(result)) && (!result.IsWildCard())) return result; // we have a local one

            // try to use a global blackboard predicate
            Unifiable gResult = SettingsDictionary.grabSettingDefaultDict(gUser.Predicates, gName, out realName);

            if ((Unifiable.IsUnknown(resultGet)) && (!Unifiable.IsUnknown(gResult)))
            {
                // result=nothing, gResult=something => return gResult
                writeToLog("SETTINGS OVERRIDE " + gResult);
                return gResult;
            }
            string sresultGet = resultGet.ToValue(query);

            // if Unknown or empty
            if (UseLuceneForGet && Unifiable.IsUnknown(sresultGet))
            {
                Unifiable userName = udict.grabSetting("id");
                if (Unifiable.IsNullOrEmpty(userName))
                {
                    writeToLog("ERROR IsNullOrEmpty id in " + udict.NameSpace);
                }
                string resultLucene = query.Request.TargetBot.LuceneIndexer.queryTriple(userName, name, node);
                if (!string.IsNullOrEmpty(resultLucene))
                {
                    return resultLucene;
                }
            }


            if (sresultGet != null && sresultGet.ToUpper() == "UNKNOWN")
            {
                return sresultGet + " " + name;
            }
            if (!String.IsNullOrEmpty(sresultGet))
            {
                if (!Unifiable.IsNullOrEmpty(gResult))
                {
                    // result=*, gResult=something => return gResult
                    if (resultGet.IsWildCard()) return gResult;
                    succeed = true;
                    // result=something, gResult=something => return result
                    return resultGet;
                }
                else
                {
                    // result=something, gResult=nothing => return result
                    return resultGet;
                }
            }
            // default => return defaultVal
            return ReturnSetSetting(udict, name, defaultVal);
            //return defaultVal;
        }

        public static ISettingsDictionary FindDict(string named, SubQuery query)
        {
            return query.GetDictionary(named);
        }

        static public Unifiable SetSettingForType(string type, SubQuery query, ISettingsDictionary dict, string name, string gName, Unifiable value, string setReturn, XmlNode templateNode)
        {
            string _sreturn = setReturn;
            setReturn = StaticXMLUtil.GetAttribValue(templateNode, "set-return", () => _sreturn, query);

            Request request = query.Request;
            RTPBot TargetBot = request.TargetBot;
            ISettingsDictionary udict = FindDict(type, query) ?? dict;
            // try to use a global blackboard predicate
            RTParser.User gUser = TargetBot.ExemplarUser;

            string realName;
            Unifiable resultGet = SettingsDictionary.grabSettingDefaultDict(udict, name, out realName);

            bool shouldSet = ShouldSet(templateNode, dict, realName, value, resultGet);

            User user = query.CurrentUser;
            MyLuceneIndexer userbotLuceneIndexer = user.bot.LuceneIndexer;
            string userName = user.UserID;
            if (!shouldSet)
            {
                writeToLog("!shouldSet ERROR " + dict + " name=" + realName + " value=" + value + " old=" + resultGet);
                bool shouldSet2 = ShouldSet(templateNode, dict, realName, value, resultGet);
                return ReturnSetSetting(udict, name, setReturn);
            }
            if (value.IsEmpty)
            {
                if (UseLuceneForSet && userbotLuceneIndexer != null) userbotLuceneIndexer.retractAllTriple(userName, name);
                if (!String.IsNullOrEmpty(gName)) gUser.Predicates.removeSetting(gName);
                udict.removeSetting(name);
            }
            else
            {
                if (UseLuceneForSet && userbotLuceneIndexer != null) userbotLuceneIndexer.updateTriple(userName, name, value);
                if (!String.IsNullOrEmpty(gName)) gUser.Predicates.addSetting(gName, value);
                udict.addSetting(name, value);

            }
            return ReturnSetSetting(udict, name, setReturn);
        }

        public static void writeToLog(string message, params object[] args)
        {
            try
            {
                message = DLRConsole.SafeFormat("NAMEVALUES: " + message, args);
                DLRConsole.DebugWriteLine(message);
            }
                // ReSharper disable EmptyGeneralCatchClause
            catch
                // ReSharper restore EmptyGeneralCatchClause
            {
            }
        }

        private static bool ShouldSet(XmlNode templateNode, ISettingsDictionary dictionary, string name, Unifiable newValue, Unifiable oldValue)
        {
            if (templateNode == null) return true;

            bool onlyIfUnknown;
            if (StaticXMLUtil.TryParseBool(templateNode, "ifUnknown", out onlyIfUnknown))
            {
                if (onlyIfUnknown) return Unifiable.IsUnknown(oldValue);
            }

            bool overwriteExisting;
            if (StaticXMLUtil.TryParseBool(templateNode, "overwriteExisting", out overwriteExisting))
            {
                if (!overwriteExisting) return Unifiable.IsNullOrEmpty(oldValue);
                //if (overwriteExisting)                   
                return true;
            }

            string oldMatch = StaticXMLUtil.GetAttribValue(templateNode, "existing", null);
            bool shouldSet = true;

            if (oldMatch != null)
            {
                if (!IsPredMatch(oldMatch, oldValue, null))
                {
                    shouldSet = false;
                }
            }
            string newMatch = StaticXMLUtil.GetAttribValue(templateNode, "matches", null);

            if (newMatch != null)
            {
                if (!IsPredMatch(newMatch, newValue, null))
                {
                    shouldSet = false;
                }
            }
            return shouldSet;
        }

        public static Unifiable ReturnSetSetting(ISettingsDictionary dict, string name, string setReturn)
        {
            string defRet;
            string realName;
            if (setReturn == null)
            {
                setReturn = SettingsDictionary.ToSettingsDictionary(dict).GetSetReturn(name, out realName);
            }
            if (setReturn == null)
            {
                defRet = "value";
            }
            else defRet = setReturn.ToLower();
            if (defRet == "name") return name;
            if (defRet == "value")
            {
                Unifiable resultGet = SettingsDictionary.grabSettingDefaultDict(dict, name, out realName);
                return resultGet;
            }
            return setReturn;
        }
    }
}
