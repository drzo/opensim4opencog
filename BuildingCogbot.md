## How to build Cogbot on a 64 bit Windows (with Chatbot components) ##

  1. Install .NET 2.0 and 3.5 (.NET 2.0 might be optional.. i dont know) (2.0 came with visual studio 2005 and 3.5 comes with visual studio 2008)
  1. install swi-prolog development version 64 and 32 bit (use at least 6.1.9)
  1. if they offer to install in c:\Program files**\swipl\ choose c:\Program files**\pl\ (we are developers so we'll just do this)
  1. make sure c:\program files\pl\lib\libswipl.dll.a is there
  1. make sure c:\program files (x86)\pl\lib\libswipl.dll.a is there
  1. install a cmdline svn client
  1. checkout trunk from svn into a folder like c:\development\opensim4opencog\current\
  1. Open a command window and cd to your root cogbot directory.
  1. Run include-chatbot.bat.
  1. Run runprebuild.bat.
  1. In VisualStudio 2008 or 2010 (full versions) Open Cogbot\_VS9\_ChatBot.sln
  1. allow visual studio to upgrade your project if needbe
  1. right click on swicli project in SwiProlog folder, choose properties
  1. under Common Properties, you will see one or two missing references.
  1. Remove them, then add Swicli.Library and SWICLITestDLL by clicking 'Add New Reference...' button and on Projects tab select these.
  1. repeat for swicli32
  1. rebuild the Solution
  1. rebuild swicli.library
  1. rebuild swicli
  1. set startup project to Cogbot.exe



![http://cogbot.logicmoo.com/swicli32refs.png](http://cogbot.logicmoo.com/swicli32refs.png)


![http://cogbot.logicmoo.com/swiclirefs.png](http://cogbot.logicmoo.com/swiclirefs.png)


![http://cogbot.logicmoo.com/swiclifixed.png](http://cogbot.logicmoo.com/swiclifixed.png)

As long as nothing has been added to the project, it is generally sufficient to just update the project and build