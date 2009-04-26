using System;
using System.Collections.Generic;
using THIRDPARTY.PrimMesher;
namespace PathSystem3D.Mesher
{
    public interface IPrimMesh
    {
        List<Coord> coords { get; set; }
        List<Face> faces { get; set; }
        List<Coord> normals { get; set; }
        List<ViewerFace> viewerFaces { get; set; }
        void AddRot(Quat q);
        void Scale(float x, float y, float z);       
        void DumpRaw(string path, string name, string title);
        // IPrimMesh Copy();
    }
}
