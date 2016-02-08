#Cogbot command line


```
Cogbot.exe [opts]
```

### Common app options ###

  * --noexcpt Ignore exceptions
  * --console Always attach console even when running with Radegast
  * --cd `<`Dir`>` Change the working directory
  * --lcd Change the working directory of the to main class's assembly

### Execution options ###

  * --aiml          Runs just the AIML interpreter
  * --swipl         Runs just the SWIPL interpreter
  * --main      `<`StringNameOfClassWithMain`>`  (like --main "Cogbot.ConsoleApp" )

### Cogbot options ###
  * --nogui      Don't use Radegast (command console operation)
  * --norepl     No console read-eval of commands (daemon operation)
  * --radegast    Run as if started from Radegast.exe (implies --norepl)
```
```
  * --noconfig    Do not load the default botconfig.xml
  * --botconfig `<`FileName`>`  Use FileName instead of botconfig.xml
```
```
  * --tcpIPAddress `<`ip addr`>`  defaults to 0.0.0.0 - needed if you have multiple network adapters and wish to bind to only one
  * --httpd `<`port`>`  Start bot HTTPD on Port
  * --tcpPort  `<`portnum`>` for the tcp (telnet) port
  * --tcpPortOffset `<`int`>` amount to increase the telnet port when retrying (if cant bind to first port #). This allows multiple bots to bind to different ports (and avoid other services).
```
```
  * --firstname `<`name`>`  Username first name
  * --lastname `<`name`>` Username last name
  * --password `<`password`>` User password
  * --simURL   `<`loginuri`>`   login URI for grid
  * --autologin           Login the bot at start

```
Cogbot32.exe [opts]
```
like above in forced 32 bit mode (users might need to do this if they have plugins using 32bit C++ code)

```
Radegast.exe
```
to run Radegast Singleton UI + All Cogbot Plugin Features