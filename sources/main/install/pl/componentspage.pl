:- module(componentspage, [
			   components_page_content//0
			  ]).

:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(logger).
:- use_module(component).
:- use_module(layout).

:- http_handler(root(components) , components_page_handler, [id(components)]).

components_page_handler(Request) :-
	(   memberchk(search(Query), Request) ; Query = []),
	(   memberchk(arch=Arch, Query) ->
	    atom_number(Arch, ArchN),
	    retractall(pages:architecture(_)),
	    assert(pages:architecture(ArchN))
	;   true
	),
	reply_html_page(
	    cogbot_web_style,
	    html([title('Select Components To Install')]),
	    components_page_content).

components_page_content -->
	{
	    bagof(Name, component(Name), NameList),
	    maplist(component_html , NameList, ComponentHtml)
	},
	html([
	        h2('Select Components To Install'),
	        div([id=componentbox], ComponentHtml),
	        \action_bar(componentspage:components_page_buttons)
            ]).

component_html(X, \component_entry(X)).

component_entry(Name) -->
	{
	    component_name(Name, HumanName),
	    component_description(Name, Description),
	    component_icon(Name, Icon),
	    component_icon_pos(Name, IconPos),
	    select_status(Name, Status)
	},
	html([
	    div([
		class='component '+Status,
		cname=Name,
		style='background-image: url(~w); background-position: ~w'-[Icon, IconPos]], [
		    h3([], HumanName),
		    p([], Description)
		])
	     ]).


components_page_action_bar -->
	action_bar(componentspage:components_page_buttons).

components_page_buttons -->
	html([
	    \wizard_button(reset, 'Cancel Install'),
	    \wizard_button_default(location_by_id(showplan), 'Show The Installation Plan')
	     ]).





