using System;
using System.Collections.Generic;
using System.Text;
using OpenMetaverse;

namespace cogbot.TheOpenSims
{
    public interface SimPosition
    {
        Vector3 GetSimPosition();
        Vector3 GetUsePosition();
        float GetSizeDistance();


    }
}
