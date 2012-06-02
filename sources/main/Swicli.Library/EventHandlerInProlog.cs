/*********************************************************
* 
*  Project: Swicli.Library - Two Way Interface to .NET and MONO 
*  Author:        Douglas R. Miles
*  Copyright (C): 2008, Logicmoo - http://www.kqml.org
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

namespace Swicli.Library
{
    public struct EventHandlerInPrologKey : PrologKey
    {
        public String Module { get; set; }
        public String Name { get; set; }
        public int Arity { get; set; }
        public EventInfo Event;
        public Object Origin;
        public override string ToString()
        {
            return (Module ?? "user") + ":" + Name + "/" + Arity + " " + Event;
        }
    }

    public class EventHandlerInProlog : PrologGenericDelegate
    {
        public EventHandlerInPrologKey Key;

        public EventHandlerInProlog(EventHandlerInPrologKey key)
        {
            Key = key;
            var keyEvent = key.Event;
            var eht = keyEvent.EventHandlerType;
            SetInstanceOfDelegateType(eht);
            if (PrologArity != key.Arity)
            {
                throw new ArgumentException("Arity of needed info " + PrologArity + " does not match " + key.Arity + " for " + this);
            }
            SyncLock = Delegate;
        }

        public override string ToString()
        {
            return "EventHandlerInProlog: " + Key;
        }

        //#pragma unsafe
        public override object CallPrologFast(object[] paramz)
        {
            //lock (oneEvtHandlerAtATime)
            {
                try
                {
                    PrologEvents++;
                    return PrologClient.CallProlog(this, Key.Module ?? "user", Key.Name, PrologArity, Key.Origin, paramz,
                                                   ReturnType, true);
                }
                catch (AccessViolationException e)
                {
                    PrologClient.Warn("CallProlog: {0} ex: {1}", this, e);
                    return null;
                }
                catch (Exception e)
                {
                    PrologClient.Warn("CallProlog: {0} ex: {1}", this, e);

                    return null;
                }
            }
        }

        //static readonly Object oneEvtHandlerAtATime = new object();

        public static ulong PrologEvents;
    }
}
