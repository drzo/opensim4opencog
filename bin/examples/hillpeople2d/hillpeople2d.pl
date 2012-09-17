:- use_module(library(swicli)).

exe_loc('C:/development/cogbot/bin/examples/hillpeople2d/bin/x64/Debug/hillpeople2d.exe').


start :-
	exe_loc(Exe),
	cli_load_assembly(Exe).
