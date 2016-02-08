# Introduction #

The CogbotEvent class (formerly SimObjectEvent)

Events in general are:

  1. Simulator Packet  "parent id changed"
  1. LibOMV Event  "on sit changed"
  1. Cogbot Salient Event "Joe sat down"
  1. MethodInvoke "Please Make Beep"
  1. GUI Event "Use maximized App"
  1. Command Sent  "@joe standup"
  1. Command Result Received "you are not joe!"
  1. Module Raised Event "user is has typed (sending commands)"

```
i am tempted to make any commands that the user tpyes int eh cosole of cogobt an AVRO message
[2:39:48 PM] Douglas R. Miles says: then the command result a AVRO message back
[2:40:06 PM] kinodax says: that woudl be a valid use, so anyone could choose to subscribe to it
[2:40:10 PM] Douglas R. Miles says: OnMeanCollisioon in Sim is an AVRO message
[2:40:29 PM] kinodax says: yeah
[2:40:47 PM] Douglas R. Miles says: i type "jump" i am emitting an AVRO message
[2:41:05 PM] Douglas R. Miles says: the JumpCommand listens on AVRO  for its mission in life
[2:41:25 PM] Douglas R. Miles says: and then since someone sent the "jump" message it calls the Jump code it love to call so much
[2:42:21 PM] Douglas R. Miles says: just like the "jump" was registered.. so could be a "OnMeanCollision" listener
[2:42:35 PM] kinodax says: yeah, you could do a cogbot 2  using avro
[2:42:57 PM] Douglas R. Miles says: "OnChat" can even be registered
[2:43:20 PM] Douglas R. Miles says: "AltAimlBotREspondToThisText"
[2:45:15 PM] Douglas R. Miles says: "OnChat" handler sees its not the speaker -> raises "AltAimlBotREspondToThisText" ->  "AltAimlBotREspondToThisText"  handler calls the AltAimlBot who send out  "AltAimlBotREspondToThisText-Reply" -> who is bneing listened  for by the intial "OnChat" handler
```