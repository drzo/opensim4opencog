
```


[11:30] <ski> but i means stuff like : if one thing happens, that may mean some other info isn't applicable, and so shouldn't even be passed any value for (if it can be helped)
[11:30] <ski> e.g. if there's a parse error, or unknown referent, then not much can be done
[11:31] <dmiles_afk> yeah like "pickup wood1 wood2 wood3 wood4"   -> objects_not_found([wood3,wood4])
[11:31] <ski> this might suggest defining a datatype by a "BNF", which can express exactly the variants which we want to allow
[11:32] <ski> this might be clunky, though, if you expect this format to change later
[11:32] <dmiles_afk> well it is sane to hjave a real documentn describing the command line parser
[11:33] <ski> but it seems there's a few general error replies, and then maybe some standard replies which can be reused across certain commands/queries
[11:33] * fliiipy (~fliiipy@host86-146-31-205.range86-146.btcentralplus.com) Quit
[11:33] <ski> then maybe some specific replies which is peculiar to one or a couple of commands
[11:35] <dmiles_afk> yeah i bet it will work that way
[11:36] <dmiles_afk> "look 0-3m"  "find orange"   for instance returns mutiple object refs
[11:36] <dmiles_afk> returns a 0-N list of objects .. just like "who"
[11:37] <ski> the natural Prology way would be to have multiple solutions
[11:38] <ski> but for CL/Clojure/Java/C# interface, probably that's not so nice
[11:38] <ski> (i suppose one could have something like an iterator in the latter)
[11:39] <dmiles_afk> prolog could  iterate_object_list(Result,Obj):- memberchk(object_list(Objs),Result),member(Obj,Objs)
[11:41] <dmiles_afk> well i am definately sold now on the Map object
[11:41] * Fly-Man- has left IRC
[11:41] <ski> hehe
[11:41] <dmiles_afk> CL/Clojure/Java/C#/Prolog  all can heave their own code for walking it
[11:42] <ski> yea
[11:42] <dmiles_afk> oh and lest i forget:  HTTPD which gets backj an XML document

```