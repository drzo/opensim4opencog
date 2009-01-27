#!/bin/sh

mv ./Properties/Resources.resx ./Properties/Resources.resx.noNant
mv ./DotCYC/CycConnectionForm.resx ./DotCYC/CycConnectionForm.resx.noNant
mv ./TextForm.resx ./TextForm.resx.noNant
mv ./LoginForm.resx ./LoginForm.resx.noNant

mono ./bin/Prebuild.exe  prebuild.xml /target nant


mv ./Properties/Resources.resx.noNant ./Properties/Resources.resx
mv ./DotCYC/CycConnectionForm.resx.noNant ./DotCYC/CycConnectionForm.resx
mv ./TextForm.resx.noNant ./TextForm.resx
mv ./LoginForm.resx.noNant ./LoginForm.resx

