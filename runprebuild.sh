#!/bin/sh

echo START: assemblies perms
chmod 755 bin/*.so bin/*.exe bin/*.dll bin/*.config bin/*.xml 2>/dev/null
echo END: assemblies perms

echo START: copy assemblies

# Check if directories exist before copying
if [ -d "sources/external/Radegast/assemblies" ]; then
	cp sources/external/Radegast/assemblies/*.dll bin/ 2>/dev/null || echo "No DLLs found in sources/external/Radegast/assemblies"
else
	echo "Directory sources/external/Radegast/assemblies does not exist"
fi

if [ -d "sources/external/Radegast" ]; then
	cp sources/external/Radegast/*.dll bin/ 2>/dev/null || echo "No DLLs found in sources/external/Radegast"
	cp sources/external/Radegast/*.pdb bin/ 2>/dev/null || echo "No PDBs found in sources/external/Radegast"
	cp sources/external/Radegast/*.config bin/ 2>/dev/null || echo "No config files found in sources/external/Radegast"
	rm -f sources/external/Radegast/libop* 2>/dev/null
	cp sources/external/Radegast/*.so bin/ 2>/dev/null || echo "No SO files found in sources/external/Radegast"
	cp sources/external/Radegast/*.dylib bin/ 2>/dev/null || echo "No DYLIB files found in sources/external/Radegast"
else
	echo "Directory sources/external/Radegast does not exist"
fi

svn revert bin/LAIR*.dll 2>/dev/null || echo "SVN revert failed for bin/LAIR*.dll"

echo DONE: copy assemblies

echo START: Generating NANT build files

# Check if mono is installed
if command -v mono >/dev/null 2>&1; then
	# Use local NAnt installation
	if [ -f "./nant.exe" ]; then
		mono ./nant.exe || echo "Failed to run NAnt"
	else
		echo "NAnt.exe not found in the current directory"
	fi
else
	echo "mono is not installed"
fi

echo DONE: Generating NANT build files

# Ensure directories exist before copying NullBuild.txt
if [ -d "./sources/external/LAIR.ResourceAPIs/Wikipedia/MirrorTables" ]; then
	cp NullBuild.txt ./sources/external/LAIR.ResourceAPIs/Wikipedia/MirrorTables/MirrorTables.exe.build
else
	echo "Directory ./sources/external/LAIR.ResourceAPIs/Wikipedia/MirrorTables does not exist"
fi

if [ -d "./bin" ]; then
	cp NullBuild.txt ./bin/CogbotLib.dll.build
else
	echo "Directory ./bin does not exist"
fi

if [ -d "./sources/external/LAIR.ResourceAPIs/Cogbot.LAIR" ]; then
	cp NullBuild.txt ./sources/external/LAIR.ResourceAPIs/Cogbot.LAIR/Cogbot.LAIR.exe.build
else
	echo "Directory ./sources/external/LAIR.ResourceAPIs/Cogbot.LAIR does not exist"
fi

echo DONE: Generating NANT build files

# Remove specific files
rm -rf bin/RadSpeechWin*.*

echo To just build CSProloge.exe
echo './runprebuild.sh ;  find -iname "*.build" -not -name Cogbot.build -not -name CSProlog.*.build -exec cp NullBuild.txt '\''{}'\'' \; ; nant'

echo otherwise type: nant
