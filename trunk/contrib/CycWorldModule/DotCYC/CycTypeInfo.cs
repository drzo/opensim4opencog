using System;
using System.Reflection;
using org.opencyc.api;
using org.opencyc.cycobject;

namespace CycWorldModule.DotCYC
{
    public class CycTypeInfo
    {
        readonly public CycFort cycFort;
        readonly Type CType;

        public CycTypeInfo(CycFort fort, Type type)
        {
            cycFort = fort;
            CType = type;
            SimCyclifier.typeFort[type] = this;
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
            SimCyclifier simCyclifier = SimCyclifier.Master;
            var docMembers = SimCyclifier.GetXmlDocMembers(CType);
            simCyclifier.assertIsa(cycFort, C("Collection"));
            simCyclifier.assertIsa(cycFort, C("SimEnumCollection"));
            String ele = SimCyclifier.GetDocString(docMembers, CType);
            simCyclifier.assertGaf(CycAccess.comment, cycFort, "The sim enum for " + CType);
            if (!String.IsNullOrEmpty(ele))
            {
                simCyclifier.assertGaf(CycAccess.comment, cycFort, ele);
            }
            if (CType.IsEnum)
            {
                foreach (FieldInfo fort in CType.GetFields(BindingFlags.Public | BindingFlags.Static))
                {
                    //if (fort.GetValue(null))        
                    string v = string.Format("{0}-{1}", CType.Name, fort.Name);
                    CycFort cv = C(v);
                    simCyclifier.assertIsa(cv, C("Collection"));
                    simCyclifier.assertGafNow(C("genls"), cv, cycFort);
                    simCyclifier.assertGaf(CycAccess.comment, cv, "The sim enum value for: " + fort);
                    ele = SimCyclifier.GetDocString(docMembers, fort);
                    if (!String.IsNullOrEmpty(ele))
                    {
                        simCyclifier.assertGaf(CycAccess.comment, cv, ele);
                        continue;
                    }
                }
            }
        }

        private void SetupType(String s)
        {
            SimCyclifier simCyclifier = SimCyclifier.Master;
            var docMembers = SimCyclifier.GetXmlDocMembers(CType);
            simCyclifier.assertIsa(cycFort, C("Collection"));
            simCyclifier.assertIsa(cycFort, C("Sim" + s + "Collection"));
            String ele = SimCyclifier.GetDocString(docMembers, CType);
            simCyclifier.assertGaf(CycAccess.comment, cycFort, "The sim " + s + " for " + CType);
            if (!String.IsNullOrEmpty(ele))
            {
                simCyclifier.assertGaf(CycAccess.comment, cycFort, ele);
            }
        }

        private CycFort C(string collection)
        {
            return SimCyclifier.Master.C(collection);
        }
    }
}