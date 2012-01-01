

svn co https://opensim4opencog.googlecode.com/svn/data-chatbot/alicebot/aiml bin/aiml/
svn co https://opensim4opencog.googlecode.com/svn/data-chatbot/alicebot/config bin/config/
svn co https://opensim4opencog.googlecode.com/svn/data-chatbot/wordnet30 bin/wordnet30/
svn co https://opensim4opencog.googlecode.com/svn/trunk-chatbot/sources/chatbot sources/chatbot/
svn co https://opensim4opencog.googlecode.com/svn/trunk-chatbot/bin-chatbot bin-chatbot/
svn co https://opensim4opencog.googlecode.com/svn/trunk-logicmoo/prolog/programk bin/prolog/programk
svn co https://opensim4opencog.googlecode.com/svn/trunk-logicmoo/prolog/startrek bin/prolog/startrek
svn co https://opensim4opencog.googlecode.com/svn/trunk-logicmoo/prolog/hyhtn bin/prolog/hyhtn
xcopy /y /c bin-chatbot\*.* bin\
runprebuild.bat

