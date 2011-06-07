#!/bin/sh

echo START: assemblies perms
chmod 755 bin/*.so bin/*.exe bin/*.dll bin/*.config bin/*.xml
echo END: assemblies perms

echo START: copy assembies

cp sources/external/Radegast/assemblies/*.dll bin/
cp sources/external/Radegast/*.dll bin/
cp sources/external/Radegast/*.pdb bin/
cp sources/external/Radegast/*.config bin/
rm -f sources/external/Radegast/libop*
cp sources/external/Radegast/*.so bin/
cp sources/external/Radegast/*.dylib bin/
svn revert bin/LAIR*.dll
cp sources/external/LAIR.ResourceAPIs/PennBank/TreeBankGrapher/bin/Release/*.dll bin/
svn revert bin/LAIR.MachineLearning.dll
svn revert bin/LAIR.Collect*.dll
svn revert bin/*.dll bin/*.exe bin/*.so

#cp sources/external/Radegast/*.ico /tmp
#cp -a sources/external/Radegast/Resources/ /tmp/


rm -f sources/external/LookingGlass-svn/bin/Prebuild.exe
rm -f sources/external/LookingGlass-svn/bin/OpenMeta*
rm -f sources/external/LookingGlass-svn/bin/HttpServer.dll
rm -f sources/external/LookingGlass-svn/bin/PrimMesher.dll
rm -f sources/external/LookingGlass-svn/bin/Radegast.exe
rm -f sources/external/LookingGlass-svn/bin/libo*

cp sources/external/LookingGlass-svn/bin/*.* bin/

echo DONE: copy assembies
mv ./bin/Mono.Security.dll ./bin/Mono.Security.dll.WindowsOnly

echo START: Generating NANT build files
mono ./bin/Prebuild.exe  prebuild.xml /target nant

# Stomp on BuildFiles
cp NullBuild.txt sources/external/Radegast.Plugin.Speech/RadSpeechWin/RadSpeechWin.dll.build

#delay building LookingGlass on Linux
find ./sources/external/LookingGlass-svn/src -iname "*.build" -exec cp NullBuild.txt '{}' \;

cp NullBuild.txt ./sources/external/LookingGlass-svn/src/LookingGlass.Renderer/LookingGlass.Renderer.dll.build
cp NullBuild.txt ./sources/external/LookingGlass-svn/src/LookingGlass.World/LookingGlass.World.dll.build
cp NullBuild.txt ./sources/external/LookingGlass-svn/src/LookingGlass.Radegast/LookingGlass.Radegast.dll.build
cp NullBuild.txt ./sources/external/LookingGlass-svn/src/LookingGlass.View/LookingGlass.View.dll.build
cp NullBuild.txt ./sources/external/LookingGlass-svn/src/LookingGlass.Comm/LookingGlass.Comm.dll.build
cp NullBuild.txt ./sources/external/LookingGlass-svn/src/LookingGlass.Comm.LLLP/LookingGlass.Comm.LLLP.dll.build
cp NullBuild.txt ./sources/external/LookingGlass-svn/src/LookingGlass.Framework/LookingGlass.Framework.dll.build
cp NullBuild.txt ./sources/external/LookingGlass-svn/src/LookingGlass.Rest/LookingGlass.Rest.dll.build
cp NullBuild.txt ./sources/external/LookingGlass-svn/src/LookingGlass.World.LL/LookingGlass.World.LL.dll.build
cp NullBuild.txt ./sources/external/LookingGlass-svn/src/LookingGlass/LookingGlass.exe.build
cp NullBuild.txt ./sources/external/LookingGlass-svn/src/LookingGlass.Renderer.Ogre/LookingGlass.Renderer.Ogre.dll.build
cp NullBuild.txt ./sources/external/LookingGlass-svn/src/LookingGlass.World.OS/LookingGlass.World.OS.dll.build
cp NullBuild.txt ./sources/external/LookingGlass-svn/src/LookingGlass.World.Services/LookingGlass.World.Services.dll.build
cp NullBuild.txt ./sources/external/xglore/xglore.exe.build
cp NullBuild.txt ./sources/external/Lucene/src/Lucene.Net/Lucene.Net.dll.build
cp NullBuild.txt ./sources/external/LAIR.ResourceAPIs/PennBank/PennBank/LAIR.ResourceAPIs.PennBank.dll.build
cp NullBuild.txt ./sources/external/LAIR.ResourceAPIs/NomBank/NomBank/LAIR.ResourceAPIs.NomBank.dll.build
cp NullBuild.txt ./sources/external/LAIR.ResourceAPIs/PennBank/TreeBankGrapher/TreeBankGrapher.exe.build
cp NullBuild.txt ./sources/external/LAIR.ResourceAPIs/Wikipedia/Wikipedia/LAIR.ResourceAPIs.Wikipedia.dll.build
cp NullBuild.txt ./sources/external/LAIR.ResourceAPIs/Wikipedia/MirrorTables/MirrorTables.exe.build
cp NullBuild.txt ./bin/CogbotLib.dll.build
cp NullBuild.txt ./sources/external/LAIR.ResourceAPIs/Cogbot.LAIR/Cogbot.LAIR.exe.build

echo DONE: Generating NANT build files
rm -rf bin/LookingGlass*.*
rm -rf bin/RadSpeechWin*.*

echo To just build CSProloge.exe
echo "./runprebuild.sh ;  find -iname \"*.build\" -not -name Cogbot.build -not -name CSProlog.*.build -exec cp NullBuild.txt '{}' \; ; nant "

echo otherwise type: nant
