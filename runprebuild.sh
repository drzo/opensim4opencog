#!/bin/sh

mono ./bin/Prebuild.exe  prebuild.xml /target nant
cp lib/Radegast/assemblies/*.dll bin/
echo copied assembies

