:- module(actions , [
		     say_format/2,
		     say_ref/2
		    ]).

:- use_module(cogbot(cogrobot)).
:- use_module(hillpeople(hillpeople)).
:- use_module(hillpeople(navigation)).

say_format(Format, Args) :-
    format(string(Contents), Format, Args),
    format(string(QContents), '"~w"', [Contents]),
    botcmd(say(QContents)).

say_ref(Prompt, Ref) :-
    (	cli_to_str(Ref, S) ; S = "Darn cli_to_str failed"),
    format(string(Out), '"~w: ~w"', [Prompt, S]),
    botcmd(say(Out)).



