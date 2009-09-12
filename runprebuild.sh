#!/bin/sh

mono ./bin/Prebuild.exe  prebuild.xml /target nant
cp lib/Radegast/assemblies/*.dll bin/
cp lib/Radegast/*.dll bin/
cp lib/Radegast/*.pdb bin/
cp lib/Radegast/*.config bin/
cp lib/Radegast/*.so bin/
cp lib/Radegast/*.dylib bin/
echo copied assembies

