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
        [PrologVisible]
        public static bool cliUntilEventHandler(PlTerm clazzOrInstance, PlTerm memberSpec, PlTerm blockOn)
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
        [PrologVisible]
        public static bool cliBlockUntilEvent(PlTerm blockOn, PlTerm testVars, PlTerm testCode)
        {
            var getInstance = GetInstance(blockOn) as WaitUntilDelegate;
            if (getInstance == null)
            {
                return Error("Not an instanceo of WaitUntilDelegate: " + blockOn);
            }
            while (true)
            {
                var results = getInstance.WaitOne();
                PlTermV copyTo = new PlTermV(2);
                PlCall(null, "copy_term", new PlTermV(PlC("c", testVars, testCode), PlC("c", copyTo)));
                PlTerm ctestVars = copyTo[0];
                PlTerm ctestCode = copyTo[1];
                PlTerm[] terms = ToTermArray(ctestVars);
                int idx = terms.Length - 1;
                for (int i = results.Length - 1; i >= 0; i--)
                {
                    terms[idx--].FromObject(results[i]);
                }
                if (PlCall(null, testCode.Name, new PlTermV(ctestCode.Arg(0), ctestCode.Arity)))
                    return PlCall(null, "=", new PlTermV(PlC("c", testVars, testCode), PlC("c", copyTo)));
            }
        }
    }

    public class WaitUntilDelegate : PrologGenericDelegate, IDisposable
    {
        readonly ManualResetEvent mre = new ManualResetEvent(false);
        private object[] Result;
        private EventInfo Event;
        private object Instance;

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
        
        public object[] WaitOne()
        {
            mre.WaitOne();
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
        }

        #endregion
    }
}
