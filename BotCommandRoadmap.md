# BotCommand at user interface #

Bot commands wrap complex operations into callable forms.

Most command may takes strings for arguments. Sometimes, these strings are expected to have a default interpretation.

Like **setmaster** expects an _avatar_ name.
```
 (setmaster "Anne Ogborn")
```

**say** uses a free form _message_ string.
```
 (say "hello world")
```

**im** _avatar_ and _message_.
```
 (im "Anne Ogborn" "Hi there!")
```


**moveprim** _primspec_ and _vector3_.
```
 (moveprim "yellow cube" (v3 34 128 22))
```


**sysvar** is variadic taking zero, one or two arguments

_form0_ list all the sysvars
```
 (sysvar)
```
_form1_ list sysvars matching string
```
 (sysvar UseMoveSpeed)
```
_form2_ set the sysvars matching string with the number 2
```
 (sysvar UseMoveSpeed 2)
```


Some cases you may override the interpretation of strings
**locate** _primspec_
```
 (locate "yellow cube")
```
with an avatar name
```
 (locate (av "Anne Ogborn"))
```


# The Implementation #

What was once...
```
        public override CmdResult Execute(string[] args, UUID fromAgentID, OutputDelegate WriteLine)
        {

            UUID targetID;
            int argsUsed;
            if (!UUIDTryParse(args,0, out targetID, out argsUsed))
				return ShowUsage();
             .... do something with targetID ....

        }

```

> Would become

```

        public CmdResult ExecuteRequest(CmdRequest args) {
 
             UUID targetID;
             if (!args.TryGetValue("agent", out targetID)) 
                              return ShowUsage();
             .... do something with targetID ....
        }

```
or
```

        public CmdResult ExecuteRequest(CmdRequest args) {
 
             UUID targetID = args.GetValue<UUID>("agent");
             .... do something with targetID ....
        }

```

This is because of the constructor of the command containing

```
            Parameters = NamedParam.CreateParams("agent", typeof(UUID), "agent you are going to " + Name);

```

That tells us the first argument of the command is named "agent" and the first parameter is expected to be an agent UUID

# The Reason #

It is much easier to maintain CmdRequest coming for various sources such as parser output, prolog, lisp and especially http/restful services


# What this takes #

Converting 188 commands.. should take me a couple hours.. its almost all find repleace