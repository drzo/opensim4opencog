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
    cp sources/external/Radegast/*.so bin/
    cp sources/external/Radegast/*.dylib bin/
else
    echo "Directory sources/external/Radegast does not exist"
fi

# Check if svn is installed
if command -v svn >/dev/null 2>&1; then
    svn revert bin/LAIR*.dll
    svn revert bin/LAIR.MachineLearning.dll
    svn revert bin/LAIR.Collect*.dll
    svn revert bin/*.dll bin/*.exe bin/*.so
else
    echo "svn is not installed"
fi

if [ -d "sources/external/LAIR.ResourceAPIs/PennBank/TreeBankGrapher/bin/Release" ]; then
    cp sources/external/LAIR.ResourceAPIs/PennBank/TreeBankGrapher/bin/Release/*.dll bin/
else
    echo "Directory sources/external/LAIR.ResourceAPIs/PennBank/TreeBankGrapher/bin/Release does not exist"
fi

#cp sources/external/Radegast/*.ico /tmp
#cp -a sources/external/Radegast/Resources/ /tmp/

#rm -f sources/external/LookingGlass-svn/bin/Prebuild.exe
#rm -f sources/external/LookingGlass-svn/bin/OpenMeta*
#rm -f sources/external/LookingGlass-svn/bin/HttpServer.dll

echo DONE: copy assemblies

echo START: Generating NANT build files

# Check if mono is installed
if command -v mono >/dev/null 2>&1; then
    # Replace 'some_command_here' with the actual command you need to run
    mono actual_command_here
else
    echo "mono is not installed"
fi

# Add more commands here as needed

echo DONE: Generating NANT build files