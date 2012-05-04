:- module(component, [
      component/1,
      component_name/2,
      component_description/2,
      component_icon/2,
      component_icon_pos/2,
      select_status_html//1,
      select_status/2,
      component_will_install/1
		     ]).

%
%   Installer for Cogbot
%   Copyright (c) 2012, Anne Ogborn
%   This code governed by the Cogbot New BSD License
%   which should have come with this code.
%
% This is the model for the component selection page.
% Display code is in componentspage
%
% Towards the bottom are the component definitions
% If you add a component the definition goes down there
%

:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).

:- discontiguous component/1,
	component_name/2,
	component_description/2,
	component_depends_on/2,
	component_icon/2,
	component_icon_pos/2.

:- style_check(-atom).
:- dynamic selected/1.

:- http_handler(root(c) , select_component_request,
		[id(c), prefix]).

select_component_request(Request) :-
	memberchk(path(Path), Request),
	atom_chars(Path, Chars),
	append([/,c,/], ComponentChars, Chars),
	atom_chars(Component, ComponentChars),
	toggle_select(Component),
	http_redirect(moved_temporary, location_by_id(components), Request).

%
%  Default component selections
%
selected(core).
selected(prolog).
selected(radegast).

% unifies if  C is needed by C2 which will install
is_needed_by(C, C2) :-
	component(C),
	component(C2),
	component_depends_on(C2, C),
	selected(C2).

is_needed_by(C, C2) :-
	component(C),
	component(C2),
	component_depends_on(X, C),
	is_needed_by(X, C2).

component_will_install(X) :-
	component(X),
	select_status(X, selected).
component_will_install(X) :-
	component(X),
	select_status(X, needed).

%
%  select_status(?Name, ?Status)
%  unify if Status is the CSS classname status of
%  component Name
%
select_status(X, needed) :-
	is_needed_by(X, _).
select_status(X, selected) :-
	\+ is_needed_by(X, _),
	selected(X).
select_status(_, notselected).

%
% DCG that expands to the English string
% status of this component
%

select_status_html(X) -->
	{
	   select_status(X, needed),
	   is_needed_by(X, Y),
	   component_name(Y, YName)
	},
	html([
	       p([
		   div(class=checkholder,
		       img([class=check, src='/f/check.png'], [])),
	       p('Locked because it is required by ~w'-[YName])])
	    ]).

select_status_html(X) -->
	{
	     select_status(X, selected)
	},
	html([
	     p([
		 div(class=checkholder,
		     img([class=check, src='/f/check.png'], [])),
		p('This component will be installed. Click to not install.')])
	     ]).

select_status_html(_) -->
	html(p('Click to install this component')).

toggle_select(Component) :-
	selected(Component),
	retractall(selected(Component)),!.

toggle_select(Component) :-
	\+ selected(Component),
	assert(selected(Component)),!.


%%%%%%%%%%%%%%%%%%%%%%%%%  Component Definitions %%%%%%%%%%%%%%%%%%%%%%%
%
%	To add a component add it here, then add it's bundles in
%	bundles

%
%  core
%
component(core).
component_name(core, 'Cogbot Core').
component_icon_pos(core, '0 0').
component_description(core,
[p('The basics of Cogbot. Interact with the bot in the virtual world via botcmd language, or via a telnet or http interface with the headless cogbot process.'),
 p('Almost all users will require this component.'),
 \select_status_html(core)]).

%
%   prolog
%
component(prolog).
component_name(prolog, 'SWI-Prolog For Cogbot').
component_icon_pos(prolog, '404px 44px').
component_description(prolog,
[p([], ['Support for ', a([href='http:swi-prolog.org'], 'swi-prolog'),
	'. This is the connection between swi-Prolog and Cogbot. Install swi-Prolog first if needed.']),
 p([], ['Supports programming the bot in Prolog.']),
 p([], ['Most users will want this component, as Prolog is the primary language for programming Cogbot.']),
 \select_status_html(prolog)]).
component_depends_on(prolog, core).

%
%  radegast
%
component(radegast).
component_name(radegast, 'Radegast Metaverse Client').
component_icon_pos(radegast, '0px 0px').
component_description(radegast,
[p([], 'This special version is integrated with Cogbot.'),
 p([], ['Most bots require some amount of manual control for setup and debugging.']),
 p([], 'Radegast is a text based viewer for manually controlling the bot. Almost all desktop installs should include Radegast.'),
 \select_status_html(radegast)]).
component_depends_on(radegast, core).

%
%  aiml
%
component(aiml).
component_name(aiml, 'AIMLbot AIML Interpreter').
component_icon_pos(aiml, '400px 0px').
component_description(aiml,
[p([], ['An environmentally aware interpreter for ',
	a([href='http://www.alicebot.org/TR/2005/WD-aiml/'], 'AIML.')]),
 p([], ['Patterns can match against conditions in the world, and AIML templates are able to issue botcmd commands.']),
 p([], ['AIMLbot can query Cyc to infer facts. So, for example, people named Sue are usually women, so the bot may respond differently to Sue and George. A separate component makes Cyc world aware.']),
 p([], ['AIMLbot can use WordNet (a separate component) to match patterns against synonyms.']),
 p([], 'AIMLbot includes Lucene, a triple store database. Using Lucene, AIMLbot can persist information without ontologizing it. For example:'),
 ul([], [
     li([], 'User: "Joe likes sports movies"'),
     li([], '...(later)...'),
     li([], 'User: "What does Joe like?"'),
     li([], 'Bot: "sports movies"')]),
 p([], 'Users who want their bots to listen and speak will want this component.'),
 \select_status_html(aiml)
]).

component_depends_on(aiml, core).
% oh though .. aimlbot need a cyc config url even when cyc client not
% selected  - aimlbot can query ext. server

%
%  aiml_personality
%
component(aiml_personality).
component_name(aiml_personality, 'AIML Personality Files').
component_icon_pos(aiml_personality, '460px 8px').
component_description(aiml_personality,
[p('AIML files for an Alice based starter personality.'),
 p('This personality simulates a current day western culture adult.'),
 p('AIML users will want this component unless they have another personality.'),
 \select_status_html(aiml_personality)]).
component_depends_on(aiml_personality, aiml).

%
%  wordnet
%
component(wordnet).
component_name(wordnet, 'WordNet Lexical Database Support').
component_description(wordnet,
[p(['Improves AIML pattern matching by matching synonyms. So an AIML pattern that contains the word ',
   em([], 'sofa'), ' would match ', em([], 'divan'),
   ' in an utterance.']),
 p('AIMLbot users will find this component nifty.'),
 \select_status_html(wordnet)]).
component_depends_on(wordnet, aiml).
% TODO include the wordnet license

%
%   opencyc
%
component(opencyc).
component_name(opencyc, 'OpenCyc Client').
component_description(opencyc,
[p([], 'Integrated Cyc client which automatically pushes information about the virtual world to the external Cyc database.'),
 p([], 'Users who wish to use an external Cyc server should install this component.'),
 \select_status_html(opencyc)]).
component_depends_on(opencyc, core).

%
%   irc
%
component(irc).
component_name(irc, 'Internet Relay Chat Relay').
component_description(irc,
[p([], 'Relays chat between an IRC channel and the bot. Relay allows control of the bot via irc.'),
 p([], 'This is useful for long term monitoring and control of production bots. Users who will be operating an unattended bot should consider using this tool.'),
 \select_status_html(irc)]).
component_depends_on(irc, core).

%
%   lisp
%
component(lisp).
component_name(lisp, 'Common Lisp Interface').
component_icon_pos(lisp, '250px 35px').
component_description(lisp,
[p('Version of ABCL Common Lisp adapted to control the bot and know about the bot\'s environment.'),
 p('Lisp users will want to install this component.'),
 \select_status_html(lisp)
 ]).
component_depends_on(lisp, core).

%
%    sims
%
component(sims).
component_name(sims, 'The Sims').
component_icon_pos(sims, '120px 35px').
component_description(sims,
[p([
     'The Sims module loops through objects looking for affordances offered by the type system',
     'and ',
     &(quot),
     'uses',
     &(quot),
     'the one that best meets it',
     &(apos),
     's needs.']),
 p('Users who want to use affordances and types based AI should install this component.'),
 p('So should those who just want to see a really cool demo of Cogbot.'),
 \select_status_html(sims)]).
component_depends_on(sims, core).

%
%   examples
%
component(examples).
component_name(examples, 'Examples').
component_description(examples,
[p('Example files. Examples require various support services depending on the specific example.'),
 p('Users new to Cogbot will appreciate the examples.'),
 \select_status_html(examples)]).

%
%   docs
%
component(docs).
component_name(docs, 'Documentation').
component_description(docs,
[p('User Documentation Bundle.'),
 p('Some day this will be a robot lead series of courses on Cogbot and AI generally,'),
 p(['but for the moment it',
    &(apos),
    's just as likely to be a badly formatted Word doc.']),
 \select_status_html(docs)]).

%
%  objects
%
component(objects).
component_name(objects, 'Cogbot Virtual Objects').
component_description(objects,
[p('Virtual objects for use with Cogbot.'),
 ul([
     li('Tools Cogbot uses for the sim iterator'),
     li('the test suite tools'),
     li('and a cool Cogbot avatar.')]),
 \select_status_html(docs)]).

%
%  all
%
component(all).
component_name(all, 'Everything').
component_description(all,
[p('Get every Cogbot component with one click.'),
 p('This will be a large download, and may require extensive configuration.'),
 \select_status_html(all)]).
component_depends_on(all, X) :-
	X \= all.


%%%%%%%%%%%%%%%%%%%%%%%%  Defaults

component_icon(Name, FileName) :-
	atomic_list_concat(['/f/', Name, '.png'], FileName).
component_icon_pos(_, '0px 0px').

