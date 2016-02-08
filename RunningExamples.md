Example running from SWI-Prolog

```
C:\Program Files\pl\bin>swipl.exe
% c:/users/administrator/appdata/roaming/swi-prolog/pl.ini compiled 0.00 sec, 2 clauses
Welcome to SWI-Prolog (Multi-threaded, 64 bits, Version 6.0.2)
Copyright (c) 1990-2011 University of Amsterdam, VU Amsterdam
SWI-Prolog comes with ABSOLUTELY NO WARRANTY. This is free software,
and you are welcome to redistribute it under certain conditions.
Please visit http://www.swi-prolog.org for details.

For help, use ?- help(Topic). or ?- apropos(Word).

1 ?- use_module(library(swicli)).
SetupProlog
RC file missing from C:/Program Files/pl/bin
RegisterPLCSForeigns
done RegisterPLCSForeigns
swipl_win.install suceeded
% library(swicli) compiled into swicli 0.81 sec, 1,444 clauses
true.

2 ?- cd('C:/development/opensim4opencog/bin/BasicFlatCogbotMore/BasicFlatCogbotLargeBins').
true.

3 ?- pwd.
c:/development/opensim4opencog/bin/basicflatcogbotmore/basicflatcogbotlargebins
true.

4 ?- ['prolog/cogbot.pl'].
logicmoo_module_aiml:asserta(library_directory('c:/development/opensim4opencog/bin/basicflatcogbotmore/basicflatcogbotlarg
%     library(system) compiled into swi_system_utilities 0.00 sec, 8 clauses
%    ../hyhtn/htncode.pl compiled 0.03 sec, 445 clauses
%    createInstanceFile.pl compiled 0.06 sec, 1,018 clauses
%   runtest compiled 0.09 sec, 1,469 clauses
%  library(startrek/mudreader.pl) compiled 0.11 sec, 1,487 clauses

```