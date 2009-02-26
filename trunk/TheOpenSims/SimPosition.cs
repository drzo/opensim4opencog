using System;
using System.Collections.Generic;
using System.Text;
using OpenMetaverse;
using cogbot.TheOpenSims.Navigation;
using System.Threading;

namespace cogbot.TheOpenSims
{
    public interface SimPosition
    {
        Vector3 GetSimPosition();
        Vector3 GetUsePosition();
        float GetSizeDistance();
        SimWaypoint GetWaypoint();
        bool IsRegionAttached();
    }


}


