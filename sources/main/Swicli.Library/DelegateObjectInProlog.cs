/*********************************************************
* 
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

namespace Swicli.Library
{

    public struct DelegateObjectInPrologKey
    {
        public String Module;
        public String Name;
        public int Arity;
        public Type DelegateType;
        //public PlTerm Origin;
        public override string ToString()
        {
            return (Module ?? "user") + ":" + Name + "/" + Arity + " " + DelegateType;
        }
    }

    public class DelegateObjectInProlog : PrologGenericDelegate
    {
        public static bool UseCallN = false;

        DelegateObjectInPrologKey Key;

        public override string ToString()
        {
            return "DelegateObjectInProlog: " + Key;
        }

        //public PlTerm Origin;

        public DelegateObjectInProlog(DelegateObjectInPrologKey key)
        {
            Key = key;
            Type eht = key.DelegateType;
            SetInstanceOfDelegateType(eht);
            SyncLock = Delegate;
        }

        //#pragma unsafe
        public override object CallPrologFast(object[] paramz)
        {
            //lock (oneEvtHandlerAtATime)
            {
                try
                {
                    object arg1 =
                        //Key.Origin; //makes sense for UseCallN
                        this;
                    PrologEvents++;
                    if (UseCallN)
                    {
                        return PrologClient.CallProlog(this, Key.Module, "call", PrologArity, arg1, paramz, ReturnType,
                                                       false);
                    }
                    return PrologClient.CallProlog(this, Key.Module ?? "user", Key.Name, PrologArity, arg1, paramz,
                                                   ReturnType, false);
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
