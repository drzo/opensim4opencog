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
using System.Collections.Generic;
using System.Reflection;

using CycFort = SbsSW.SwiPlCs.PlTerm;

namespace Swicli.Library
{
    public class CycTypeInfo
    {
        readonly public CycFort cycFort;
        readonly Type CType;
        public static Dictionary<Type, CycTypeInfo> typeFort = new Dictionary<Type, CycTypeInfo>();

        public bool IsEnum
        {
            get { return CType.IsEnum; }
        }

        public bool IsBitFlags
        {
            get; set;
        }

        public CycTypeInfo(CycFort fort, Type type)
        {
            cycFort = fort;
            CType = type;
            typeFort[type] = this;
            if (type.IsEnum)
            {
                SetupEnum();
            }
            else if (type.IsInterface)
            {
                SetupType("Interface");
            }
            else if (type.IsClass)
            {
                SetupType("Class");
            }
        }

        private void SetupEnum()
        {
            
          /*  SimCyclifier simCyclifier = SimCyclifier.Master;
            var docMembers = SimCyclifier.GetXmlDocMembers(CType);
            simCyclifier.assertIsa(cycFort, C("Collection"));
            simCyclifier.assertIsa(cycFort, C("SimEnumCollection"));
            String ele = SimCyclifier.GetDocString(docMembers, CType);
            simCyclifier.assertVocabGaf(CycAccess.comment, cycFort, "The sim enum for " + CType);
            if (!String.IsNullOrEmpty(ele))
            {
                simCyclifier.assertVocabGaf(CycAccess.comment, cycFort, ele);
            }*/
            if (CType.IsEnum)
            {
                IsBitFlags = IsFlagType(CType);
                if (IsBitFlags)
                {
                 //   simCyclifier.assertIsa(cycFort, C("SimEnumBitFlagsCollection"));
                }
                foreach (FieldInfo fort in CType.GetFields(BindingFlags.Public | BindingFlags.Static))
                {
//                    Enum ev = (Enum) fort.GetValue(null);
  //                  var tc= ev.GetTypeCode();
                    string v = string.Format("{0}-{1}", CType.Name, fort.Name);
                    CycFort cv = C(v);
                   /* simCyclifier.assertIsa(cv, C("Collection"));
                    simCyclifier.assertVocabGafNow(C("genls"), cv, cycFort);
                    simCyclifier.assertVocabGaf(CycAccess.comment, cv, "The sim enum value for: " + fort);
                    MemberInfo mi = fort;
                    simCyclifier.DocQueue.Enqueue(() =>
                                                      {
                                                          ele = SimCyclifier.GetDocString(docMembers, mi);
                                                          if (!String.IsNullOrEmpty(ele))
                                                          {
                                                              simCyclifier.assertVocabGaf(CycAccess.comment, cv, ele);
                                                          }
                                                      });*/
                }
            }
        }

        public static bool IsFlagType(Type type)
        {
            object[] attributes = type.GetCustomAttributes(typeof (FlagsAttribute), true);
            if (attributes != null && attributes.Length > 0)
            {
                return true;
            }
            /* if (type == typeof(OpenMetaverse.PrimFlags))
             {
                 return true;   
             }*/
            return false;
        }

        private void SetupType(String s)
        {
           // SimCyclifier simCyclifier = SimCyclifier.Master;
            //var docMembers = SimCyclifier.GetXmlDocMembers(CType);
            //simCyclifier.assertIsa(cycFort, C("Collection"));
            //simCyclifier.assertIsa(cycFort, C("Sim" + s + "Collection"));
            //String ele = SimCyclifier.GetDocString(docMembers, CType);
            //simCyclifier.assertVocabGaf(CycAccess.comment, cycFort, "The sim " + s + " for " + CType);
            //if (!String.IsNullOrEmpty(ele))
            //{
              //  simCyclifier.assertVocabGaf(CycAccess.comment, cycFort, ele);
            //}
        }

        private CycFort C(string collection)
        {
            return PrologClient.C(collection);
        }
    }
}