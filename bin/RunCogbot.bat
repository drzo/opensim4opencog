
echo killing lucene database
echo del lucenedb\*.*
echo killing users databases
echo rd /s aiml\users\ 

del *.bak /s

setlocal
cd %CD%
if NOT EXIST ABuildStartup.exe jar x Built.zip
cmd /c Cogbot.exe --httpd --aiml BinaBot Daxeline
endlocal
