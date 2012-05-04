:- module(bundle, [
		   bundle_install_plan/1,
		   bundle_section//1,
		   bundle/4
		  ]).
%
%   Installer for Cogbot
%   Copyright (c) 2012, Anne Ogborn
%   This code governed by the Cogbot New BSD License
%   which should have come with this code.
%
%
%    A 'bundle' is an atomic installer action.
%    This module includes code to handle the bundle descriptions on
%    the showplan page.
%
%    This module also includes the actual planner that generates an
%    ordered list of bundle names.
%
%    Individual bundles are in the directory bundles.
%    They need to be use_module'd here.
%
%%	%%%%%%%%%%%%%%%%%%%%%%%%
%
%
%  bundle support
%
%  The user selects a set of 'components' to be installed
%  Each component in turn has a list of bundles it requires.
%  So, if two components both need foo bundle, we only install foo
%  bundle once.
%
%  Each bundle has a type, which is used to control how it's displayed
%  on the install_list page, and sometimes to determine order
%
%  group   -  this bundle has no actions, it's only used to depend on
%             other bundles
%
%  license - add a license file to the list to be displayed for
%            acceptance. Every license bundle automatically has
%	     a dependency on licensepage, and all license bundles are
%	     before licensepage
%
%  licensepage  - display the licenses
%
%  config  - add a section to the config page that asks some questions
%            (e.g. an install file location).
%
%  configpage - This works like license/licensepage
%
%  files   -  this bundle downloads a file into the temp directory
%            if it doesn't exist,
%           then copies (and possibly unzips) files and possibly
%	     registers dlls. There is a config bundle that gets the
%	     install and temp directories that all files use
%
%  page	   - This displays a web page
%
%  desktop  - build the desktop icon and start menu entry, depending
%             on the config setting
%

:- use_module(library(http/html_write)).
:- use_module(library(clpfd)).
:- use_module(library(uri)).

:- use_module(component).
:- use_module(pages, [architecture/1]).

:- discontiguous bundle_for/2, bundle/4, deps/2, deps/3, before/2.

:-include('bundles/common.pl').
:-include('bundles/all.pl').
:-include('bundles/core.pl').
:-include('bundles/prolog.pl').
% If you add bundles, you have to add them above this line


%
% given a bundle name expands to the prolog-html
% representation of that bundle on the showplan page


%
%  bundles of these types aren't displayed
%
bundle_section(Bundle) -->
	{
	   bundle(Bundle, Type, _, _),
	   memberchk(Type, [config, group, license, debug])
	}, [].

bundle_section(Bundle) -->
	{
	   bundle(Bundle, licensepage, _, _)
	},
	html([
                li('Display the License Page')
	     ]).

bundle_section(Bundle) -->
	{
	   bundle(Bundle, files, Name, _)
	},
	html([
	        li('Install ~w files.'-[Name])
	     ]).

%
%  fallback handler
%
bundle_section(Bundle) -->
	{
	    bundle(Bundle, _, Name, _)
	},
	html([
	       li([Name])
	     ]).


%%	%%%%%%%%%%%%%%%%%%%%%%  The actual planner  %%%%%%%%%%%%%%%%%%%
%
% return an install-order list of bundles to install
%
bundle_install_plan(Plan)  :-
	det_setof(Name, component_will_install(Name) , Components),
	required_bundles(Components, [], [], AllBundles),
	length(AllBundles, N),
	add_free_vars(AllBundles, N, AllBundlePairs, AllVars),
	add_order_constraints(AllBundlePairs),
	all_distinct(AllVars),
	labeling([], AllVars),
	keysort(AllBundlePairs, SortedPairs),
	maplist(names_only, SortedPairs, Plan).

%  required_bundles(+Components, +UncheckedBundles, +Bundles,
%  -AllBundles) collect all bundles directly needed by the component
%  list
%
required_bundles([] , [], Bundles, Bundles).
required_bundles([HComponent|TComponents], [], SoFar, All) :-
	det_setof(Bundle, bundle_for(HComponent, Bundle), UncheckedSet),
	required_bundles(TComponents, UncheckedSet, SoFar, All).

required_bundles(Component, [HBundle|TBundles], SoFar, Bundles) :-
	memberchk(HBundle, SoFar), !, % efficiency
	required_bundles(Component, TBundles, SoFar, Bundles).

required_bundles(Component, [HBundle|TBundles], SoFar, Bundles) :-
	% efficiency- not a member, we got here we have to add it's deps
	det_setof(ABundle, deps(HBundle, ABundle), StuffToCheck),
	ord_union([HBundle], SoFar, NewSoFar),
	ord_union(StuffToCheck , TBundles, NewToCheck),
	required_bundles(Component, NewToCheck, NewSoFar, Bundles).

add_free_vars([], _, [], []).
add_free_vars([HBundleName|TBundleNames],
	      N,
	      [FreeVar-HBundleName|TPairs],
	      [FreeVar|TVars]) :-
	      FreeVar in 1..N,
	      add_free_vars(TBundleNames, N, TPairs, TVars).

add_order_constraints(AllPairs) :-
	bagof(X, add_order_constraints(AllPairs, X), _).

add_order_constraints(AllPairs, AName-BName) :-
	before(AName,BName),
	member(AVar-AName, AllPairs),
	member(BVar-BName, AllPairs),
	AVar #< BVar.

names_only(_-Name, Name).

%
%  a non failing version of setof
%  for situations where	[] is indeed a solution
%
det_setof(V, G, L) :-
	setof(V, G, L),!.
det_setof(_, _, []).



