%
%   Installer for Cogbot
%   Copyright (c) 2012, Anne Ogborn
%   This code governed by the Cogbot New BSD License
%   which should have come with this code.
%
%
%   bundle file that does an svn checkout
%

bundle_for(sources, sources_group).

bundle(sources_group, group, 'Sources Group', []).

deps(sources_group, sources_checkout).
deps(sources_group, cogbot_license).

bundle(sources_checkout, goal,
       'Checkout sources from SVN',
       [goal(checkout_sources)]).

source_repository(
'svn checkout http://opensim4opencog.googlecode.com/svn/trunk/ opensim4opencog-read-only').

checkout_sources :-
	source_repository(CO),
	shell(CO, Status),
	(   Status =:= 0
	->
	    true
	;
	    format('svn checkout failed with status ~w~n', [Status]),
	    format('ensure svn client on path and checkout with ~w~n', [CO])
	).



