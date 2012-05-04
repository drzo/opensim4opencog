:- module(logger , [logger/3]).
%
%   Installer for Cogbot
%   Copyright (c) 2012, Anne Ogborn
%   This code governed by the Cogbot New BSD License
%   which should have come with this code.
%


%
%  logger(+Topic, +Format, +Args) is det
%  as for debug/3
logger(Topic, Format, Args) :-
	debug(Topic, Format, Args).


