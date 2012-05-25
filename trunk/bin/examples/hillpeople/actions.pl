:- module(actions , [
		     say_format/2,
		     say_ref/2
		    ]).

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
	format(Format, Args),nl.
say_format(Format, Args) :-
	write('Format='),write(Format),nl,
	write('Args='),write(Args),nl.
say_format(_, _).

say_ref(Prompt, Ref) :-
    once(cli_to_str(Ref, S) ; S = "Darn cli_to_str failed"),
    say_format('~w = ~s', [Prompt, S]).


