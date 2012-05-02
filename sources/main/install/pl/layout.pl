:- module(layout, [
	action_bar//1,
	wizard_button//2,
	wizard_button_default//2,
	wizard_button_default_submit//1
]).

:- use_module(library(http/html_write)).

%% <module> General installer page layout.

action_bar(Actions) -->
	html([
	  div(id=buttonbar, \Actions)
        ]).

wizard_button(Action, Label) -->
	html([
	   div([id=next, class='wizard button nondefault'],
	       [a([href=Action, alt=Label],
		  Label)])
	     ]).

wizard_button_default(Action, Label) -->
	html([
	   div([id=next, class='wizard button default'],
	       [a([href=Action, alt=Label],
		  Label)])
	     ]).

wizard_button_default_submit(Label) -->
	html([
	   div([id=next,
		onClick='javascript:$(this).closest("form").submit();
',
		class='wizard button default submit'],
	       [Label])
	     ]).

