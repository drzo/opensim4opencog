using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Xml;
using AltAIMLbot;
using AltAIMLbot.Utils;
using AltAIMLParser;
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
            AltBot TargetBot = request.TargetBot;
            ISettingsDictionary udict;
            string dictName = AIMLTagHandlerU.GetNameOfDict(query, subject ?? dict.NameSpace, node, out udict);
            // try to use a global blackboard predicate
            User gUser = TargetBot.ExemplarUser;

            defaultVal = AltBot.GetAttribValue(node, "default,defaultValue", defaultVal);
            gName = AltBot.GetAttribValue(node, "global_name", gName);

            string realName0;


            var vv = ScriptManager.GetGroup(query.TargetBot.ObjectRequester, dictName, name);
            {
                if (vv != null)
                {
                    if (vv.Count == 0)
                    {
                        succeed = true;
                        realName = name;
                        return "";
                    }
                    succeed = true;
                    realName = name;
                    foreach (var e in vv)
                    {
                        return Unifiable.Create(e);
                    }
                }
            }
            Unifiable resultGet = SettingsDictionaryReal.grabSettingDefaultDict(udict, name, out realName0);

            if (ReferenceEquals(resultGet, null))
            {
                realName = null;
                resultGet = Unifiable.NULL;
            }
            // if ((!String.IsNullOrEmpty(result)) && (!result.IsWildCard())) return result; // we have a local one

            String realNameG;
            // try to use a global blackboard predicate
            Unifiable gResult = SettingsDictionaryReal.grabSettingDefaultDict(gUser.Predicates, gName, out realNameG);

            if ((Unifiable.IsUnknown(resultGet)) && (!Unifiable.IsUnknown(gResult)))
            {
                // result=nothing, gResult=something => return gResult
                writeToLog("SETTINGS OVERRIDE " + gResult);
                succeed = true;
                realName = realNameG;
               // return gResult;
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


            if (sresultGet != null)
            {
                if (sresultGet.ToUpper() == "UNKNOWN")
                {
                    succeed = false;
                    realName = null;
                    return sresultGet + " " + name;
                }
                else if (Unifiable.IsEMPTY(resultGet))
                {
                    succeed = true;
                    realName = name;
                    return resultGet;
                }
                else if (Unifiable.IsUnknown(resultGet))
                {
                    succeed = false;
                    realName = name;
                    return resultGet;
                }

            }
            if (!String.IsNullOrEmpty(sresultGet))
            {
                succeed = true;
                realName = realName0;
                query.GetDictValue++;
                if (!IsNullOrEmpty(gResult))
                {
                    if (resultGet.IsWildCard)
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

        static public Unifiable SetSettingForType(string subject, SubQuery query, ISettingsDictionary dict, string name, string gName, object value, string setReturn, XmlNode templateNode)
        {
            string _sreturn = setReturn;
            setReturn = StaticXMLUtils.GetAttribValue<string>(templateNode, "set-return", () => _sreturn, query.ReduceStarAttribute<string>);

            Request request = query.Request;
            AltBot TargetBot = request.TargetBot;
            // try to use a global blackboard predicate
            User gUser = TargetBot.ExemplarUser;

            string realName;
            Unifiable resultGet = SettingsDictionaryReal.grabSettingDefaultDict(dict, name, out realName);
            bool shouldSet = ShouldSet(templateNode, dict, realName, value, resultGet, query);

            User user = query.CurrentUser;
            ITripleStore userbotLuceneIndexer = (ITripleStore)user.rbot.TripleStore;
            string userName = user.UserID;
            if (!shouldSet)
            {
                writeToLog("!shouldSet ERROR {0} name={1} value={2} old={3}", dict, realName, value, resultGet);
                bool shouldSet2 = ShouldSet(templateNode, dict, realName, value, resultGet, query);
                return ReturnSetSetting(dict, name, setReturn);
            }
            if (IsIncomplete(value))
            {
                if (UseLuceneForSet && userbotLuceneIndexer != null) userbotLuceneIndexer.retractAllTriple(userName, name);
                SettingsDictionaryReal.removeSettingWithUndoCommit(query, dict, name);
                if (!IsNullOrEmpty(gName)) SettingsDictionaryReal.removeSettingWithUndoCommit(query, gUser, gName);
            }
            else
            {
                if (UseLuceneForSet && userbotLuceneIndexer != null) userbotLuceneIndexer.updateTriple(userName, name, value);
                if (!String.IsNullOrEmpty(gName))
                {
                    SettingsDictionaryReal.addSettingWithUndoCommit(query, gUser.Predicates, gUser.addSetting, gName, value);
                }
                query.SetDictValue++;
                SettingsDictionaryReal.addSettingWithUndoCommit(query, dict, dict.addSetting, name, value);
            }
            var retVal = ReturnSetSetting(dict, name, setReturn);
            if (!IsIncomplete(retVal) || !IsNullOrEmpty(retVal))
            {
                string comment = null;
                //if (query.LastTagHandler!=null) comment = query.LastTagHandler.Succeed(" setting " + name);
                return retVal;// +comment;
            }
            return retVal;
        }

        public static void writeToLog(string message, params object[] args)
        {
            try
            {
                message = SafeFormat("NAMEVALUES: " + message, args);
                DLRConsole.DebugWriteLine(message);
            }
                // ReSharper disable EmptyGeneralCatchClause
            catch
                // ReSharper restore EmptyGeneralCatchClause
            {
            }
        }

        private static bool ShouldSet(XmlNode templateNode, ISettingsDictionary dictionary, string name, object newValue, Unifiable oldValue, SubQuery query)
        {
            if (templateNode == null) return true;
            bool canSet = query.UseDictionaryForSet(dictionary);
;
            bool onlyIfUnknown;
            if (StaticXMLUtils.TryParseBool(templateNode, "ifUnknown", out onlyIfUnknown))
            {
                if (onlyIfUnknown) return (Unifiable.IsUnknown(oldValue) || IsIncomplete(oldValue)) && canSet;
            }

            bool overwriteExisting;
            if (StaticXMLUtils.TryParseBool(templateNode, "overwriteExisting", out overwriteExisting))
            {
                if (!overwriteExisting) return (Unifiable.IsNullOrEmpty(oldValue) || IsIncomplete(oldValue)) && canSet;
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
            var newValueU = Unifiable.Create(newValue);
            string newMatch = StaticXMLUtils.GetAttribValue(templateNode, "matches", null);

            if (newMatch != null)
            {
                if (!IsPredMatch(newMatch, newValueU, null))
                {
                    shouldSet = false;
                }
            }
            string wontvalue = StaticXMLUtils.GetAttribValue(templateNode, "wontvalue", null);

            if (wontvalue != null)
            {
                if (IsPredMatch(wontvalue, newValueU, null))
                {
                    shouldSet = false;
                }
            }
            return shouldSet && canSet;
        }

        public static Unifiable ReturnSetSetting(ISettingsDictionary dict, string name, string setReturn)
        {
            string defRet;
            string realName;
            if (setReturn == null)
            {
                setReturn = SettingsDictionaryReal.ToSettingsDictionary(dict).GetSetReturn(name, out realName);
            }
            if (string.IsNullOrEmpty(setReturn))
            {
                defRet = "value";
            }
            else defRet = setReturn.ToLower();
            if (defRet == "name") return name;
            if (defRet == "value")
            {
                Unifiable resultGet = SettingsDictionaryReal.grabSettingDefaultDict(dict, name, out realName);
                return resultGet;
            }
            return setReturn;
        }
    }
}
