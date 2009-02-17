#!/bin/sh

mv ./Properties/Resources.resx ./Properties/Resources.resx.noNant
mv ./DotCYC/CycConnectionForm.resx ./DotCYC/CycConnectionForm.resx.noNant
mv ./TextForm.resx ./TextForm.resx.noNant
mv ./LoginForm.resx ./LoginForm.resx.noNant
mkdir -p RESX
mv TheOpenSims/Navigation/Debug/*.resx RESX/

mono ./bin/Prebuild.exe  prebuild.xml /target nant

mv RESX/* TheOpenSims/Navigation/Debug/

mv ./Properties/Resources.resx.noNant ./Properties/Resources.resx
mv ./DotCYC/CycConnectionForm.resx.noNant ./DotCYC/CycConnectionForm.resx
mv ./TextForm.resx.noNant ./TextForm.resx
mv ./LoginForm.resx.noNant ./LoginForm.resx

