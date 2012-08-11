%
%   Installer for Cogbot
%   Copyright (c) 2012, Anne Ogborn
%   This code governed by the Cogbot New BSD License
%   which should have come with this code.
%
%
%   bundle file for cogbot support for swi-prolog
%   This is the integration between swipl and cogbot
%

bundle_for(prolog, prolog_group).

bundle(prolog_group, group, 'Prolog Integration Group', []).

deps(prolog_group, cogbot_license).
deps(prolog_group, find_prolog_install).
deps(prolog_group, dotnet32, 32).
deps(prolog_group, dotnet64, 64).
deps(prolog_group, swicli).

bundle(swicli, files,
       'Swicli Files',
       [from(temp('swicli.zip')),
	url(logicmoo('swicli.zip')),
	to(prolog(.))]).

bundle(find_prolog_install,
       config,
       'Locate The Prolog Install',
       Args) :-
	find_prolog_question(X),
	Args = [inc(X)].

:- use_module(pages, [architecture/1]).

find_prolog_question(X) :-
	    architecture(32),
	    current_prolog_flag(home, Home),
	    prolog_to_os_filename(Home, OSHome),
	    X = [
	    label(for=prolog_location,
		  [
		   'Location of your ', em('32 bit'),
		   'swi-Prolog install home (something like C:\\Program Files (x86)\\pl):'
		  ]),
	    input([
		size=80,
		name=prolog_location,
		id=prolog_location,
		type=text,
		value=OSHome
		  ],[])
	     ].

find_prolog_question(X) :-
	    architecture(64),
	    current_prolog_flag(home, Home),
	    prolog_to_os_filename(Home, OSHome),
	  X = [
	    label(for=prolog_location,
	        [
		 'Location of your ', b(em('64 bit')),
		 ' swi-Prolog install (something like C:\\Program Files\\pl):'
		]),
	    input([
		size=80,
		name=prolog_location,
		id=prolog_location,
		type=text,
		value=OSHome
		  ],[])
	     ].






