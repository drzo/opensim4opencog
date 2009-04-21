using System;
using OpenMetaverse;
using PathSystem3D.Navigation;


namespace PathSystem3D.Navigation
{
    public interface SimPosition
    {
        bool IsPassable { get; set; }
        string DistanceVectorString(SimPosition RootObject);
        Vector3 GetSimPosition();
        float GetSizeDistance();
        bool IsRegionAttached();
        Quaternion GetSimRotation();
        Vector3d GetWorldPosition();
        SimPathStore GetPathStore();
    }
}
