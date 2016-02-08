# Server and Build Dependencies #

This is a list of all dependencies of the installer

# Common #

These are bundles common to more than one component

# Core #

  * The server must serve http://cogbot.logicmoo.com/install/cogbot/cogbot-core.zip, which is expanded into the install root

The cogbot-core.zip file must contain

  * bin
  * bin/Cogbot.exe
  * bin/cogbot.ico

botconfig.xml must include code to read

bin\\startupLisp.lisp

# Prolog integration #

  * the server must serve http://cogbot.logicmoo.com/install/cogbot/swicli.zip which is expanded into prolog home.

# Building to http://cogbot.logicmoo.com/install/cogbot/ #
Douglas from windows after building on his system

```

C:\development\opensim4opencog\current>ant
Buildfile: build.xml

cogbot-prolog:
      [jar] Building jar: C:\development\opensim4opencog\current\cogbot-prolog.zip

cogbot-aiml:

cogbot-documents:

cogbot-core-patches:
      [jar] Building jar: C:\development\opensim4opencog\current\cogbot-core-patches.zip

cogbot-installer:

all:

BUILD SUCCESSFUL
Total time: 7 seconds

```

Next he updates our server from Samba

```


root@titan current]# pwd
/mnt/enki/development/opensim4opencog/current


[root@titan current]# rsync --inplace -vc *.zip root@pathwayslms.com:/var/www/cogbot/install/cogbot/
root@pathwayslms.com's password:
cogbot-core-patches.zip
cogbot-prolog.zip

sent 3751279 bytes  received 22256 bytes  82934.84 bytes/sec
total size is 121918864  speedup is 32.31

[root@titan current]#
```