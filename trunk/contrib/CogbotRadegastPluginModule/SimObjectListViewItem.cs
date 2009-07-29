using System;
using System.Collections;
using System.Windows.Forms;
using cogbot.TheOpenSims;
using OpenMetaverse;
using PathSystem3D.Navigation;

namespace CogbotRagegastPluginModule
{
    internal class SimObjectListViewItem : ListViewItem
    {
        private readonly SimObject _simObject;
        public SimObjectListViewItem(SimObject o)
        {
            _simObject = o;
            this.Tag = o;//.ID;
            this.Text = o.ToString();
        }

        public override string ToString()
        {
            return _simObject.ToString();   
        }

        public SimObject TheSimObject
        {
            get { return _simObject; }
        }
        public override bool Equals(object obj)
        {
            return base.Equals(obj);
        }
    }

    public class SimObjectSorterClass : IComparer
    {

        public SimPosition Origin;

        public SimObjectSorterClass()
        {

        }

        //this routine should return -1 if xy and 0 if x==y.
        // for our sample we'll just use string comparison
        public int Compare(object x, object y)
        {
            SimObject item1 = ((SimObjectListViewItem)x).TheSimObject;
            SimObject item2 = ((SimObjectListViewItem)y).TheSimObject;
            if (Origin==null || !Origin.IsRegionAttached())
            {
                return item1.ToString().CompareTo(item2.ToString());                
            }
           // SimAvatarImpl av = (SimAvatarImpl)Origin;

            double distance1 = item1.Distance(Origin);
            double distance2 = item2.Distance(Origin);
            if (distance1==distance2)
            {
                return item1.ToString().CompareTo(item2.ToString());
            }
            return distance1.CompareTo(distance2);
        }
    }
}