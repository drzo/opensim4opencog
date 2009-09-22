#!/bin/sh

mono ./bin/Prebuild.exe  prebuild.xml /target nant
cp lib/Radegast/assemblies/*.dll bin/
cp lib/Radegast/*.dll bin/
cp lib/Radegast/*.pdb bin/
cp lib/Radegast/*.config bin/
cp lib/Radegast/*.so bin/
cp lib/Radegast/*.dylib bin/
rm -f lib/LookingGlass-svn/bin/Prebuild.exe
rm -f lib/LookingGlass-svn/bin/OpenMeta*
rm -f lib/LookingGlass-svn/bin/HttpServer.dll
rm -f lib/LookingGlass-svn/bin/PrimMesher.dll
rm -f lib/LookingGlass-svn/bin/Radegast.exe
cp lib/LookingGlass-svn/bin/*.* bin/

# Stomp on BuildFiles
cp NullBuild.txt lib/radegastspeech-svn/RadSpeechWin/RadSpeechWin.dll.build

#delay building LookingGlass on Linux
cp NullBuild.txt ./lib/LookingGlass-svn/src/LookingGlass.Renderer/LookingGlass.Renderer.dll.build
cp NullBuild.txt ./lib/LookingGlass-svn/src/LookingGlass.World/LookingGlass.World.dll.build
cp NullBuild.txt ./lib/LookingGlass-svn/src/LookingGlass.Radegast/LookingGlass.Radegast.dll.build
cp NullBuild.txt ./lib/LookingGlass-svn/src/LookingGlass.View/LookingGlass.View.dll.build
cp NullBuild.txt ./lib/LookingGlass-svn/src/LookingGlass.Comm/LookingGlass.Comm.dll.build
cp NullBuild.txt ./lib/LookingGlass-svn/src/LookingGlass.Comm.LLLP/LookingGlass.Comm.LLLP.dll.build
cp NullBuild.txt ./lib/LookingGlass-svn/src/LookingGlass.Framework/LookingGlass.Framework.dll.build
cp NullBuild.txt ./lib/LookingGlass-svn/src/LookingGlass.Rest/LookingGlass.Rest.dll.build
cp NullBuild.txt ./lib/LookingGlass-svn/src/LookingGlass.World.LL/LookingGlass.World.LL.dll.build
cp NullBuild.txt ./lib/LookingGlass-svn/src/LookingGlass/LookingGlass.exe.build
cp NullBuild.txt ./lib/LookingGlass-svn/src/LookingGlass.Renderer.Ogre/LookingGlass.Renderer.Ogr.dll.build
cp NullBuild.txt ./lib/LookingGlass-svn/src/LookingGlass.Renderer.Ogre/LookingGlass.Renderer.Ogre.dll.build


echo copied assembies

