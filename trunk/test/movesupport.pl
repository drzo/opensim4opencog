:- module(movesupport, [
                       move_bots/0,
                       start_moving_bot/1,
                       get_test_waypoints/3,
                       run_test/0
		      ]).

%--------------------------------------------------------
%
%  simplemove.pl
%
%    Testing of multi bot logon and movement
%
%    This requires a set of the 'hillpeople' bots
%
%---------------------------------------------------------------------

:-set_prolog_flag(double_quotes,string).

dbgfmt(F,A):-'format'(F,A).


:-use_module('../test/acctsupport').

%
% Change this line to cd to cogbot/bin directory on your system
%  the exists_file is so it doesn't do it again when reconsulted
:- exists_file('simplemove.pl') -> cd('../bin') ; true.

%% add to search paths
assertIfNewRC(Gaf):-catch(call(Gaf),_,fail),!.
assertIfNewRC(Gaf):-asserta(Gaf).

%
%  These lines need adjusted if you've moved this file
%
:- assertIfNewRC(user:file_search_path(library, '.')).
:- assertIfNewRC(user:file_search_path(library, '../test')).
:- assertIfNewRC(user:file_search_path(cogbot, './prolog/simulator')).
:- assertIfNewRC(user:file_search_path(cogbot, './prolog')).
:- assertIfNewRC(user:file_search_path(library, './prolog')).
% :- assertIfNewRC(user:file_search_path(hillpeople,
% './examples/hillpeople')).

:- use_module(cogbot(cogrobot)).

:-dynamic
	botID/2,
        last_name/1,
        get_test_waypoints/3,
	bot_ran/1.

:-multifile
        get_test_waypoints/3.

:-module_transparent
        get_test_waypoints/3.

:- discontiguous
	bot_waypoints/3.


move_bots :-
	test_bot(Name),
	start_moving_bot(Name),
	fail.
move_bots.

start_moving_bot(Name):- 
    once((thread_property(TName,_),Name=TName);
    thread_create(
	    do_move_test(Name),
	    _, [alias(Name)])).

run_test :- logon_bots,sleep(5),move_bots.


%%	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                  move the bots around
%%	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
do_move_test(Name) :-
	botID(Name, ID),
        retractall(stop_test(Name)),
	set_current_bot(ID),
        get_test_waypoints(Name,WPS),
        do_test_waypoints(Name,ID,WPS).


%%	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                    utilities
%%	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%

:-dynamic(stop_test/1).

do_test_waypoints(Name,ID,loop(List)):-!,repeat,do_test_waypoints(Name,ID,List),stop_test(Name).
do_test_waypoints(Name,ID,loop(N,List)):-!,foreach(between(1,N,_), do_test_waypoints(Name,ID,List)).
do_test_waypoints(Name,ID,List):-is_list(List),!,foreach(member(X,List),do_test_waypoints(Name,ID,X)).
do_test_waypoints(Name,_ID,stop_test):-assert(stop_test(Name)),!.
do_test_waypoints(_Name,_ID,call(H)):-!,ignore(call(H)).
do_test_waypoints(_Name,ID,botcmd(H)):-!,ignore(wb_botcmd(ID,H)).
do_test_waypoints(_Name,_ID,H):-compound(H),!,ignore(call(H)).
do_test_waypoints(Name,ID,H):-ignore(botmove(Name,ID,H)).

botmove(Name,ID,H):-
        set_current_bot(ID),
	botcmd(moveto(H, 1), MoveStat),
        say_ref('Move', MoveStat),
	botcmd(waitpos(20, H , 1), WaitStat),
        say_ref('Wait', WaitStat),
	say_format('~w went to ~w', [Name,H]).


say_format(Format, Args) :-
    'format'(string(Contents), Format, Args),
    'format'(string(QContents), '"~s"', [Contents]),
    botcmd(say(QContents)).

say_ref(Prompt, Ref) :-
    once(cli_to_str(Ref, S) ; S = "Darn cli_to_str failed"),
    say_format('~w = ~s', [Prompt, S]).

   
%%	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                    A default test
%%	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%

