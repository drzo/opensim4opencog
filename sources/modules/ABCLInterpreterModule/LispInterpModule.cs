using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using ABCLScriptEngine.UI;
using Cogbot;
using MushDLR223.ScriptEngines;
#if RTPARSER_INCLUDED
using RTParser.Utils;
using RTParser.Variables;
#endif
using MushDLR223.Utilities;
using SomeInterp = ABCLScriptEngine.ABCLInterpreter;
using ABCLScriptEngine.UI;
namespace ABCLScriptEngine
{
    ///<summary>
    ///</summary>
    public class LispInterpModule : WorldObjectsModule, ICollectionProvider        
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

        public SomeInterp modInterp;
        ///<summary>
        ///</summary>
        ///<param name="parent"></param>
        public LispInterpModule(BotClient parent)
            : base(parent)
        {
            modInterp = new SomeInterp(parent);
            ScriptManager.AddInterpreter(modInterp);
            parent.InvokeGUI((() =>
            {
                try
                {
                    parent.AddTab("ABCL Lisp", "ABCL", new IronTextBoxControl(), OnClose);
                }
                catch (Exception e)
                {
                    DLRConsole.DebugWriteLine("" + e);
                }
            }
             ));
        }

        private void OnClose(object sender, EventArgs e)
        {
            throw new NotImplementedException();
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

        public IEnumerable<string> SettingNames(ICollectionRequester requester, int depth)
        {
            throw new NotImplementedException();
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
            ScriptManager.AddInterpreter(modInterp);
        }

        /// <summary>
        /// Performs application-defined tasks associated with freeing, releasing, or resetting unmanaged resources.
        /// </summary>
        /// <filterpriority>2</filterpriority>
        public override void Dispose()
        {
            ScriptManager.RemoveInterpreter(modInterp);
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

        #region Implementation of ICollectionProviderSettable

        public void SetValue(ICollectionRequester requester, string name, object value)
        {
            throw new NotImplementedException();
        }

        public bool AcceptsNewKeys
        {
            get { throw new NotImplementedException(); }
        }

        public ICollection GetGroup(ICollectionRequester requester, string name)
        {
            throw new NotImplementedException();
        }

        #endregion
    }
}
