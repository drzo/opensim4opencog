/*  $Id$
*  
*  Project: Swicli.Library - Two Way Interface for .NET and MONO to SWI-Prolog
*  Author:        Douglas R. Miles
*  E-mail:        logicmoo@gmail.com
*  WWW:           http://www.logicmoo.com
*  Copyright (C):  2010-2012 LogicMOO Developement
*
*  This library is free software; you can redistribute it and/or
*  modify it under the terms of the GNU Lesser General Public
*  License as published by the Free Software Foundation; either
*  version 2.1 of the License, or (at your option) any later version.
*
*  This library is distributed in the hope that it will be useful,
*  but WITHOUT ANY WARRANTY; without even the implied warranty of
*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
*  Lesser General Public License for more details.
*
*  You should have received a copy of the GNU Lesser General Public
*  License along with this library; if not, write to the Free Software
*  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
*
*********************************************************/
using System;
using System.Reflection;
using System.Threading;
using SbsSW.SwiPlCs;

namespace Swicli.Library
{
    public partial class PrologClient
    {
        /// <summary>
        /// Create a Event Handler with a ResetEvent
        /// </summary>
        /// <param name="clazzOrInstance"></param>
        /// <param name="memberSpec"></param>
        /// <param name="blockOn"></param>
        /// <returns></returns>
        [PrologVisible]
        public static bool cliNewEventWaiter(PlTerm clazzOrInstance, PlTerm memberSpec, PlTerm blockOn)
        {
            object getInstance = GetInstance(clazzOrInstance);
            Type c = GetTypeFromInstance(getInstance, clazzOrInstance);
            Type[] paramz = null;
            EventInfo fi = findEventInfo(memberSpec, c, ref paramz);
            if (fi == null)
            {
                return Error("Cant find event {0} on {1}", memberSpec, (object) c ?? clazzOrInstance);
            }
            return blockOn.FromObject(new WaitUntilDelegate(fi, getInstance));
        }

        /// <summary>
        /// Block until ResetEvent occures and run the prolog code.. if it fails.. wait again for the reset event
        ///  if over maxTime return time_limit_exceeded
        /// </summary>
        /// <param name="blockOn"></param>
        /// <param name="maxTime"></param>
        /// <param name="testVarsCode"></param>
        /// <param name="exitCode"></param>
        /// <returns></returns>
        [PrologVisible]
        public static bool cliBlockUntilEvent(PlTerm blockOn, PlTerm maxTime, PlTerm testVarsCode, PlTerm exitCode)
        {
            var wud = GetInstance(blockOn) as WaitUntilDelegate;
            if (wud == null)
            {
                return Error("Not an instance of WaitUntilDelegate: " + blockOn);
            }
            var timeSpan = TimeSpan.FromDays(3650);
            if (maxTime.IsInteger)
            {
                timeSpan = TimeSpan.FromMilliseconds(maxTime.intValue());
            }
            else if (!maxTime.IsVar)
            {
                timeSpan = (TimeSpan) CastTerm(maxTime, typeof (TimeSpan));
            }

            DateTime expireyTime = DateTime.Now.Add(timeSpan);
            while (DateTime.Now < expireyTime)
            {
                var results = wud.WaitOne(timeSpan);
                if (results == null)
                {
                    return exitCode.UnifyAtom("time_limit_exceeded");
                }
                PlTerm copyTo = PlTerm.PlVar();
                PlTermV newPlTermV = new PlTermV(testVarsCode, copyTo);
                PlCall("system", "copy_term", newPlTermV);
                PlTerm ctestVars = copyTo.Arg(0);
                PlTerm ctestCode = copyTo.Arg(1);
                PlTerm[] terms = ToTermArray(ctestVars);
                int idx = terms.Length - 1;
                int resdex = results.Length - 1;                
                while (idx >= 0 && resdex >= 0)
                {
                    terms[idx--].FromObject(results[resdex--]);
                }
                if (PlCall("user", "call", new PlTermV(ctestCode)))
                    return UnifyToProlog(PlCall(null, "=", newPlTermV), exitCode) != 0;
            }
            return exitCode.UnifyAtom("time_limit_exceeded");
        }
    }

    public class WaitUntilDelegate : PrologGenericDelegate, IDisposable
    {
        public ManualResetEvent mre = new ManualResetEvent(false);
        public object[] Result;
        public EventInfo Event;
        public object Instance;

        public WaitUntilDelegate(EventInfo info, object instance)
        {
            Event = info;
            Instance = instance;
            SetInstanceOfDelegateType(info.EventHandlerType);
            Event.AddEventHandler(instance, Delegate);
        }

        public override object CallProlog(params object[] paramz)
        {
            return CallPrologFast(paramz);
        }
        public override void CallPrologV(params object[] paramz)
        {
            CallPrologFast(paramz);
        }

        public override object CallPrologFast(object[] paramz)
        {
            Result = paramz;
            mre.Set();
            return null;
        }

        public object[] WaitOne(int ts)
        {
            if (!mre.WaitOne(ts)) return null;
            mre.Reset();
            return Result;
        }

        public object[] WaitOne(TimeSpan ts)
        {
            if (!mre.WaitOne(ts)) return null;
            mre.Reset();
            return Result;
        }

        #region Implementation of IDisposable

        /// <summary>
        /// Performs application-defined tasks associated with freeing, releasing, or resetting unmanaged resources.
        /// </summary>
        /// <filterpriority>2</filterpriority>
        public void Dispose()
        {
            Event.RemoveEventHandler(Instance, Delegate);
            mre.Close();
        }

        #endregion
    }
}
