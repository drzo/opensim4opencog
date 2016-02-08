# Instructions for Installing Cogbot using the binary installer. #

Note - as of this writing, we're still writing the installer.
These instructions may not work for a bit.

# Prerequisites #

Cogbot requires
Before installing Cogbot from binaries, ensure the following:

  1. You have a proper version of Windows. Cogbot supports TODO
  1. You must have an administrator account.  Try opening the directory C:\Program Files (x86)\   If you can see folders in that location, you are using an administrator account.
  1. Install [Microsoft .NET Framework 3.5](http://www.microsoft.com/download/en/details.aspx?id=21) if not already installed on your machine
  1. Decide whether to install 32 bit or 64 bit versions of Cogbot and swi-prolog. We strongly recommend the 64 bit versions, unless you have a specific reason to use the 32 bit version. You can install 32 bit swipl and 64 bit Cogbot or vice versa if you need to.
  1. Most users will want [swi-prolog](http://swi-prolog.org). Download and install the 64 bit version unless you specifically need the 32 bit version. If you don't intend to install swi-prolog, see below.

# Installation #

  1. Download our [Cogbot Installer Zip File](http://TODO.org) and unzip into a directory (not where you plan to install cogbot).
  1. Navigate to that directory and double click on ClickToInstall
  1. If all goes well, swi-prolog will launch and open a web page in your browser. The remainder of the install will happen in the browser.
  1. Select the components you want to install. Accept the installation plan and the licenses.
  1. rovide the needed configuration information

# Installing without swi-prolog #

If you don't intend to use swi-prolog, you can download our [Cogbot installer exe](http://TODO.org) zip file, unzip into a directory and, and run ClickToInstallWithoutSWIPL. The above zip file is smaller, otherwise they're identical.

# Running Cogbot #

Assuming you installed the Radegast component, simply double click the cogbot icon or use the start menu. Radegast should launch. If you checked 'autologin' in the install, your bot will automatically log in to the grid you selected.

# Your first cogbot command #

type
/say hello world

into the chat bar

type

/help

to see a list of commands

If you installed swipl integration you can control the bot from prolog. Try querying

?- botClientCmd(shout("hi there!")).



