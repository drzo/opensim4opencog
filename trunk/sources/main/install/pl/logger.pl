:- module(logger , [logger/3]).


%
%  logger(+Topic, +Format, +Args) is det
%  as for debug/3
logger(Topic, Format, Args) :-
	debug(Topic, Format, Args).


