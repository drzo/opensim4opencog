
echo killing lucene database
del lucenedb\*.*
echo killing users databases
rd /s aiml\users\ 

del *.bak /s

setlocal
cd %CD%
if NOT EXIST ABuildStartup.exe jar x Built.zip
cmd /c ABuildStartup.exe --httpd --aiml BinaBot Daxeline
endlocal
