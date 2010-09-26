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
        static public bool UseLuceneForGet = false;
        static public bool UseLuceneForSet = false;
        static public Unifiable GetSettingForType(string subject, SubQuery query, ISettingsDictionary dict, string name, out string realName, string gName, Unifiable defaultVal, out bool succeed, XmlNode node)
        {
            Request request = query.Request;
            OutputDelegate writeToLog = request.writeToLog ?? DEVNULL;
            RTPBot TargetBot = request.TargetBot;
            ISettingsDictionary udict;
            string dictName = AIMLTagHandler.GetNameOfDict(query, subject ?? dict.NameSpace, node, out udict);
            // try to use a global blackboard predicate
            RTParser.User gUser = TargetBot.ExemplarUser;

            defaultVal = RTPBot.GetAttribValue(node, "default,defaultValue", defaultVal);
            gName = RTPBot.GetAttribValue(node, "global_name", gName);

            string realName0;
            Unifiable resultGet = SettingsDictionary.grabSettingDefaultDict(udict, name, out realName0);

            if (ReferenceEquals(resultGet, null))
            {
                realName = null;
                resultGet = Unifiable.NULL;
            }
            // if ((!String.IsNullOrEmpty(result)) && (!result.IsWildCard())) return result; // we have a local one

            String realNameG;
            // try to use a global blackboard predicate
            Unifiable gResult = SettingsDictionary.grabSettingDefaultDict(gUser.Predicates, gName, out realNameG);

            if ((Unifiable.IsUnknown(resultGet)) && (!Unifiable.IsUnknown(gResult)))
            {
                // result=nothing, gResult=something => return gResult
                writeToLog("SETTINGS OVERRIDE " + gResult);
                succeed = true;
                realName = realNameG;
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
                ITripleStore userbotLuceneIndexer = (ITripleStore)query.Request.TargetBot.TripleStore;
                string resultLucene = userbotLuceneIndexer.queryTriple(userName, name, node);
                if (!string.IsNullOrEmpty(resultLucene))
                {
                    succeed = true;
                    realName = name;
                    return resultLucene;
                }
            }


            if (sresultGet != null && sresultGet.ToUpper() == "UNKNOWN")
            {
                succeed = false;
                realName = null;
                return sresultGet + " " + name;
            }
            if (!String.IsNullOrEmpty(sresultGet))
            {
                succeed = true;
                realName = realName0;
                query.GetDictValue++;
                if (!IsNullOrEmpty(gResult))
                {
                    if (resultGet.IsWildCard())
                    {
                        realName = realNameG;
                        // result=*, gResult=something => return gResult
                        return gResult;
                    }
                    // result=something, gResult=something => return result
                    return resultGet;
                }
                else
                {
                    // result=something, gResult=nothing => return result
                    return resultGet;
                }
            }
            if (defaultVal==null)
            {
                succeed = false;
                realName = null;
                return defaultVal;
            }
            // default => return defaultVal
            succeed = true;
            realName = realName0;
            return ReturnSetSetting(udict, name, defaultVal);
            //return defaultVal;
        }

        static public Unifiable SetSettingForType(string subject, SubQuery query, ISettingsDictionary dict, string name, string gName, Unifiable value, string setReturn, XmlNode templateNode)
        {
            string _sreturn = setReturn;
            setReturn = StaticXMLUtils.GetAttribValue<string>(templateNode, "set-return", () => _sreturn, query.ReduceStarAttribute<string>);

            Request request = query.Request;
            RTPBot TargetBot = request.TargetBot;
            // try to use a global blackboard predicate
            RTParser.User gUser = TargetBot.ExemplarUser;

            string realName;
            Unifiable resultGet = SettingsDictionary.grabSettingDefaultDict(dict, name, out realName);

            bool shouldSet = ShouldSet(templateNode, dict, realName, value, resultGet);

            User user = query.CurrentUser;
            ITripleStore userbotLuceneIndexer = (ITripleStore)user.bot.TripleStore;
            string userName = user.UserID;
            if (!shouldSet)
            {
                writeToLog("!shouldSet ERROR " + dict + " name=" + realName + " value=" + value + " old=" + resultGet);
                bool shouldSet2 = ShouldSet(templateNode, dict, realName, value, resultGet);
                return ReturnSetSetting(dict, name, setReturn);
            }
            if (IsNull(value))
            {
                if (UseLuceneForSet && userbotLuceneIndexer != null) userbotLuceneIndexer.retractAllTriple(userName, name);
                if (!String.IsNullOrEmpty(gName)) gUser.Predicates.removeSetting(gName);
                dict.removeSetting(name);
            }
            else
            {
                if (UseLuceneForSet && userbotLuceneIndexer != null) userbotLuceneIndexer.updateTriple(userName, name, value);
                if (!String.IsNullOrEmpty(gName)) gUser.Predicates.addSetting(gName, value);
                query.SetDictValue++;
                dict.addSetting(name, value);

            }
            var retVal = ReturnSetSetting(dict, name, setReturn);
            if (!IsNullOrEmpty(retVal)) return retVal;
            if (!IsNull(retVal))
            {
                return retVal;                
            }
            return retVal;
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
            if (StaticXMLUtils.TryParseBool(templateNode, "ifUnknown", out onlyIfUnknown))
            {
                if (onlyIfUnknown) return Unifiable.IsUnknown(oldValue);
            }

            bool overwriteExisting;
            if (StaticXMLUtils.TryParseBool(templateNode, "overwriteExisting", out overwriteExisting))
            {
                if (!overwriteExisting) return Unifiable.IsNullOrEmpty(oldValue);
                //if (overwriteExisting)                   
                return true;
            }

            string oldMatch = StaticXMLUtils.GetAttribValue(templateNode, "existing", null);
            bool shouldSet = true;

            if (oldMatch != null)
            {
                if (!IsPredMatch(oldMatch, oldValue, null))
                {
                    shouldSet = false;
                }
            }
            string newMatch = StaticXMLUtils.GetAttribValue(templateNode, "matches", null);

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
            if (string.IsNullOrEmpty(setReturn))
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
