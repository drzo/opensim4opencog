using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using RTParser;
using RTParser.Utils;
using RTParser.Variables;

namespace RTParser.Database
{
    public class NamedValuesFromSettings
    {
        static public Unifiable GetSettingForType(string type, SubQuery query, ISettingsDictionary dict, string name, string gName, Unifiable defaultVal, out bool succeed)
        {
            Request request = query.Request;
            RTPBot TargetBot = request.TargetBot;
            ISettingsDictionary udict = FindDict(type, query) ?? dict;
            // try to use a global blackboard predicate
            RTParser.User gUser = TargetBot.ExemplarUser;

            succeed = false;
            string realName;
            Unifiable resultGet = SettingsDictionary.grabSettingDefualt(udict, name, out realName);

            if (ReferenceEquals(resultGet, null))
            {
                resultGet = Unifiable.NULL;
            }
            // if ((!String.IsNullOrEmpty(result)) && (!result.IsWildCard())) return result; // we have a local one

            // try to use a global blackboard predicate
            Unifiable gResult = SettingsDictionary.grabSettingDefualt(gUser.Predicates, gName, out realName);

            if ((Unifiable.IsUnknown(resultGet)) && (!Unifiable.IsUnknown(gResult)))
            {
                // result=nothing, gResult=something => return gResult
                request.writeToLog("SETTINGS OVERRIDE " + gResult);
                return gResult;
            }
            string sresultGet = resultGet.ToValue(query);
            if (sresultGet != null && sresultGet.ToUpper() == "UNKNOWN")
            {
                return sresultGet + " " + name;
            }
            if (!String.IsNullOrEmpty(sresultGet))
            {
                if (!String.IsNullOrEmpty(gResult))
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

        static public Unifiable SetSettingForType(string type, SubQuery query, ISettingsDictionary dict, string name, string gName, Unifiable value, string setReturn)
        {
            Request request = query.Request;
            RTPBot TargetBot = request.TargetBot;
            ISettingsDictionary udict = FindDict(type, query) ?? dict;
            // try to use a global blackboard predicate
            RTParser.User gUser = TargetBot.ExemplarUser;

            string realName;
            Unifiable resultGet = SettingsDictionary.grabSettingDefualt(udict, name, out realName);

            if (value.IsEmpty)
            {
                if (!String.IsNullOrEmpty(gName)) gUser.Predicates.removeSetting(gName);
                udict.removeSetting(name);
            }
            else
            {
                if (!String.IsNullOrEmpty(gName)) gUser.Predicates.addSetting(gName, value);
                udict.addSetting(name, value);

            }
            return ReturnSetSetting(query.CurrentUser, udict, name, value, setReturn);
        }

        public static Unifiable ReturnSetSetting(User user, ISettingsDictionary dict, string name,Unifiable value, string setReturn)
        {
            string defRet;
            string realName;
            if (setReturn == null)
            {
                setReturn = SettingsDictionary.grabSetName(dict, name, out realName);
            }
            user.bot.LuceneIndexer.assertTriple(user.UserName, name, value);
            if (setReturn == null)
            {
                defRet = "value";
            }
            else defRet = setReturn.ToLower();
            if (defRet == "name") return name;
            if (defRet == "value")
            {
                Unifiable resultGet = SettingsDictionary.grabSettingDefualt(dict, name, out realName);
                return resultGet;
            }
            return setReturn;
        }
    }
}
