:- module(showplan, [
		    ]).
%
% Display the planned install actions and get approval
%

:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(layout).
:- use_module(bundle).

:- http_handler(root(showplan) ,
		show_plan_page_handler,
		[id(showplan)]).

show_plan_page_handler(_Request) :-
	reply_html_page(
	    cogbot_web_style,
	    html([title('These Actions Will Be Taken')]),
	    show_plan_page_content).

show_plan_page_content -->
	{
	   % Plan is install-order list of bundle names to install
	   bundle_install_plan(Plan),
	%   maplist(bundle_debug, Plan, DebugHTML),
	   maplist(bundle_html, Plan, PlanHTML)
        },
	html([
	    h2('These Actions Will Be Taken'),
	    div([id=bundlebox],
		[ol([id=plan_list], PlanHTML)
		]),
	    /*
	    div([id=debugbox],
		[h3('Debug Info'),
		ol([], DebugHTML)]),  */
	    \action_bar(showplan:showplan_buttons)
	     ]).

bundle_html(X, \bundle_section(X)).

bundle_debug(X, \debug_bundle_section(X)).

debug_bundle_section(X) -->
	html([li(X)]).

showplan_buttons -->
	html([
	    \wizard_button(reset, 'Cancel The Installation'),
	    \wizard_button_default(license, 'Take These Actions')
	     ]).


% TODO there's a bug, it's installing both 32 and 64 bit dotnet
%
%
