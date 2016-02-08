AIMLBotModulde

# Introduction #

AIML Bot module alows basic system loading of AIML files for the ends of controlling cogbot using a controlled natural language interface such as: "follow me" "stay there"  "sit on the blue chair".

It might even be enough if the AIML author is ambitious enough to facilitate writing a shop keeping virtual world bot.

# Installation #

is done via the botcmd:
```
load AIMLBotModule noserv
```

This can be done in botconfig.xml like
```
(thisClient.ExecuteCommand "load AIMLBotModule noserv")
```

# Configuration #

When cogbot loads the AIMLBotModule it loads some aiml files from 2 directories

```
$cogbin/aiml/shared_aiml/*.*
```
and
```
$cogbin/aiml/firstname_lastname/*.*
```

Our intial bot is examplebot\_resident

if firstname\_lastname is not present then it loads from instead
```
$cogbin/aiml/default_bot/*.*
```

One of these two directories ($cogbin/aiml/firstname\_lastname/**.** or $cogbin/aiml/default\_bot/**.**) become the personal directory of the bot.


If both the shared\_aiml dir and personal dir was empty, then no aiml files are loaded.   This is probably is ideal.


# Use #

At any time a botcmd of

```
aiml @load pathto/aimlfile.aiml
```
To load AIML files


# Bot Vars #

```
[6:43:16 PM] Anne Ogborn: it's the mechanism I'd use to have Yuppie 'know' in her aiml that she's a woman
[6:43:34 PM] Anne Ogborn: through some aiml get?
[6:43:41 PM] Douglas R. Miles: yes exactly 
when aimlbot is loaded it has a ton of settings like $favfood $hometown all these things can be used in botcmds
[6:44:05 PM] Douglas R. Miles: but the variable is stored in the cogbot memory and not the aiml
[6:44:26 PM] Douglas R. Miles: aiml comes to cogbot memory for these vars
[6:44:54 PM] Douglas R. Miles: why aiml isnt nesisary to load to store these things
[6:45:10 PM] Anne Ogborn: ah
[6:45:21 PM | Edited 6:45:32 PM] Douglas R. Miles: when aiml is loaded it keeps it's settings in cogbot
[6:45:30 PM] Anne Ogborn: I see
[6:46:03 PM] Anne Ogborn: so, for example, lets track $favfood
[6:46:14 PM] Anne Ogborn: where's that initially set?
[6:47:07 PM] Douglas R. Miles: from the aiml config/botvars.xml
[6:47:36 PM] Douglas R. Miles: whereas $master is set from setmaster
[6:48:13 PM] Douglas R. Miles: some varaibles are code calls like $self
[6:48:28 PM] Anne Ogborn: yes, ok
[6:48:48 PM] Anne Ogborn: I don't have a botvars.xml file
[6:48:57 PM | Edited 6:49:06 PM] Douglas R. Miles: i get getter calls a interface that retuns a uncalled function
[6:49:24 PM] Douglas R. Miles: right since you are not runing chatbot sources
[6:49:28 PM] Anne Ogborn: ah, ok
[6:49:39 PM | Edited 6:49:45 PM] Douglas R. Miles: if you are, then it gets copied to your bin directory
[6:49:51 PM] Anne Ogborn: ok, so sounds like this all gets a ref to wiki page for AIML
[6:49:53 PM] Anne Ogborn: 8cD
[6:50:13 PM] Anne Ogborn: lovely!
[6:50:41 PM] Anne Ogborn: brb, potty then do that one
[6:51:20 PM] Douglas R. Miles: http://code.google.com/p/opensim4opencog/source/browse/#svn%2Fdata-chatbot%2Falicebot%2Fconfig
[6:51:44 PM] Douglas R. Miles: oops i meant thbis link: http://code.google.com/p/opensim4opencog/source/browse/data-chatbot/alicebot/config/bot.xml


[7:02:21 PM] Douglas R. Miles: every bot loads that default...
[7:02:45 PM] Douglas R. Miles: then they load aiml/dimebagfurry_resident/*.xml
```