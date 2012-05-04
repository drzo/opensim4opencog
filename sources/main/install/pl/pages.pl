:- module(pages, [
		  reset_installer/0,
		  set_architecture/2,
		  reset_install_request/1,
		  architecture/1
		 ]).

%
%   Installer for Cogbot
%   Copyright (c) 2012, Anne Ogborn
%   This code governed by the Cogbot New BSD License
%   which should have come with this code.
%
%   This is only the architecture stuff
%

:-dynamic  architecture/1.

architecture(64).

reset_installer :-
	format(user_error, 'reset_installer is probably not complete~n',[]),
	retractall(architecture(_)),
	assert(architecture(64)).

set_architecture(Architecture, _Request) :-
	retractall(architecture(_)),
	atom_number(Architecture, ArchN),
	assert(architecture(ArchN)).

% TODO change this to halt.
reset_install_request(_Request) :- reset_installer.
