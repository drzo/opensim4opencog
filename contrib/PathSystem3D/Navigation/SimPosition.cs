using System;
using OpenMetaverse;
using PathSystem3D.Navigation;


namespace PathSystem3D.Navigation
{
    public interface SimPosition
    {
        bool IsPassable { get; set; }
        string DistanceVectorString(SimPosition RootObject);
        Vector3 SimPosition { get; }
        float GetSizeDistance();
        bool IsRegionAttached { get; }
        Quaternion SimRotation { get; }
        Vector3d GlobalPosition { get; }
        bool TryGetSimPosition(out Vector3 pos);
        SimPosition UsePosition { get; }
        SimPathStore PathStore { get; }
    }
}
