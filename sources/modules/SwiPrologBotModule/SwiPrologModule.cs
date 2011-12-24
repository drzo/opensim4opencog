using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using cogbot;
using cogbot.Listeners;
using cogbot.ScriptEngines;
using MushDLR223.ScriptEngines;
#if RTPARSER_INCLUDED
using RTParser.Utils;
using RTParser.Variables;
#endif 
namespace PrologScriptEngine
{
    ///<summary>
    ///</summary>
    public class SwiPrologModule : WorldObjectsModule, ICollectionProvider        
#if RTPARSER_INCLUDED
        , ISettingsDictionary
#endif
    {
        #region ISettingsDictionary Members
        public bool IsTraced { get; set; }
        public IEnumerable<string> SettingNames(int depth)
        {
            //get 
            {
                return new string[0]; /*throw new NotImplementedException(); */
            }
        }

        public PrologScriptInterpreter PLScriptInterpreter;
        ///<summary>
        ///</summary>
        ///<param name="parent"></param>
        public SwiPrologModule(BotClient parent)
            : base(parent)
        {
            PLScriptInterpreter = new PrologScriptInterpreter(parent);   
        }

#if RTPARSER_INCLUDED
        /// <summary>
        /// Adds a bespoke setting to the Settings class (accessed via the grabSettings(string name)
        /// method.
        /// </summary>
        /// <param name="name">The name of the new setting</param>
        /// <param name="value">The value associated with this setting</param>
        public bool addSetting(string name, RTParser.Unifiable value)
        {
            //throw new NotImplementedException();
           // Intern()
            return false;
        }

#endif
        /// <summary>
        /// Removes the named setting from this class
        /// </summary>
        /// <param name="name">The name of the setting to remove</param>
        public bool removeSetting(string name)
        {
            //throw new NotImplementedException();
            return false;
        }
#if RTPARSER_INCLUDED
        public bool updateSetting(string name, RTParser.Unifiable value)
        {
            //throw new NotImplementedException();
            return false;
        }

        public RTParser.Unifiable grabSetting(string name)
        {
            //throw new NotImplementedException();
            return null;
        }
#endif
        public bool containsLocalCalled(string name)
        {
            //throw new NotImplementedException();
            return false;
        }

        ///<summary>
        ///</summary>
        ///<param name="name"></param>
        ///<returns></returns>
        ///<exception cref="NotImplementedException"></exception>
        public bool containsSettingCalled(string name)
        {
           // throw new NotImplementedException();
            return false;
        }

        ///<summary>
        ///</summary>
        ///<exception cref="NotImplementedException"></exception>
        public string NameSpace
        {
            get { return "plugin." + GetModuleName(); }
        }

        #endregion

        #region Overrides of Listener

        /// <summary>
        ///  Name registered in the BotClient.registrationTypes collection
        /// </summary>
        /// <returns></returns>
        public override string GetModuleName()
        {
            return GetType().Name;
        }

        ///<summary>
        ///</summary>
        ///<exception cref="NotImplementedException"></exception>
        public override void StartupListener()
        {
            ScriptManager.AddInterpreter(PLScriptInterpreter);
        }

        /// <summary>
        /// Performs application-defined tasks associated with freeing, releasing, or resetting unmanaged resources.
        /// </summary>
        /// <filterpriority>2</filterpriority>
        public override void Dispose()
        {
            ScriptManager.RemoveInterpreter(PLScriptInterpreter);
        }

        #endregion

        #region Implementation of ICollectionProvider

        ///<summary>
        ///</summary>
        ///<param name="name"></param>
        ///<returns></returns>
        ///<exception cref="NotImplementedException"></exception>
        public ICollection GetGroup(string name)
        {

            return null;
          //  throw new NotImplementedException();
        }

        #endregion
    }
}
