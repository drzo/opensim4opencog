#!/bin/sh

echo START: assemblies perms
chmod 755 bin/*.so bin/*.exe bin/*.dll bin/*.config bin/*.xml
echo END: assemblies perms

echo START: copy assemblies

# Check if directories exist before copying
if [ -d "sources/external/Radegast/assemblies" ]; then
	cp sources/external/Radegast/assemblies/*.dll bin/
else
	echo "Directory sources/external/Radegast/assemblies does not exist"
fi

if [ -d "sources/external/Radegast" ]; then
	cp sources/external/Radegast/*.dll bin/
	cp sources/external/Radegast/*.pdb bin/
	cp sources/external/Radegast/*.config bin/
	rm -f sources/external/Radegast/libop*
	cp sources/external/Radegast/*.so bin/
	cp sources/external/Radegast/*.dylib bin/
else
	echo "Directory sources/external/Radegast does not exist"
fi

svn revert bin/LAIR*.dll

echo DONE: copy assemblies

echo START: Generating NANT build files

# Check if mono is installed
if command -v mono >/dev/null 2>&1; then
	# Replace 'path/to/your/actual_assembly.exe' with the actual command you need to run
	mono bin/your_actual_assembly.exe
else
	echo "mono is not installed"
fi

# Add more commands here as needed

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
echo "./runprebuild.sh ;  find -iname \"*.build\" -not -name Cogbot.build -not -name CSProlog.*.build -exec cp NullBuild.txt '{}' \; ; ./nant.exe "

echo otherwise type: ./nant.exe
