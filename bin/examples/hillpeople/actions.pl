:- module(actions , [
		     say_format/2,
		     say_ref/2,
		     has_inventory/0,
		     remove_all/0,
		     wear_list/1
		    ]).

:-at_initialization(set_prolog_flag(double_quotes,string)).
:-set_prolog_flag(double_quotes,string).

:- use_module(cogbot(cogrobot)).
:- use_module(hillpeople(hillpeople)).
:- use_module(hillpeople(navigation)).

say_format(Format, Args) :-
    'format'(string(Contents), Format, Args),
%    'format'(string(QContents), '"~s"', [Contents]),
    botcmd(say(Contents)).

% DEBUG TODO
%  Douglas - what's up here? why are we quoting, and why is it failing?
say_format(Format, Args) :-
	'format'(Format, Args),nl.
say_format(Format, Args) :-
	write('Format='),write(Format),nl,
	write('Args='),write(Args),nl.
say_format(_, _).

say_ref(Prompt, Ref) :-
    once(cli_to_str(Ref, S) ; S = "Darn cli_to_str failed"),
    say_format('~w = ~s', [Prompt, S]).


has_inventory :-
	bot_has_inventory(["hillpeople"]),!.

remove_all :- bot_unwearall.

wear_list(Items) :-
	bot_replaceoutfit(["hillpeople"], Items).


	/*
	remove_all,
	wear_starter_outfit,

has_inventory.

get_inventory

init_setup(Name) :-
	botcmd(gohome),
	remove_all_clothing,
	dress_as_tribal(Name).

remove_all_clothing :-
	 botget([thesimavatar,children],X),cli_get(X,count,Y).
*/



