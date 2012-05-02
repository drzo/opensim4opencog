:- module(startpage , []).
%
%  The start page that just greets the user,
%  discovers what prolog they're running,
%  and offers them a choice of cogbot's to install
%
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(layout).

:- http_handler(root(start) , start_page_handler, [id(start)]).

start_page_handler(_Request) :-
	reply_html_page(
	    cogbot_web_style,
	    html([title('Welcome To Cogbot')]),
	    start_page_content).

start_page_content -->
	{
	    current_prolog_flag(address_bits, 64)
	},
	 html([
	     h2('Welcome To Cogbot'),
p(['If you are seeing this page you have installed ',
    b(em('64 bit')),
    ' swi-prolog correctly.']),
p('This wizard will guide you through the rest of the install.'),
p('Most users will want the 64 bit version of Cogbot. Only install 32 bit Cogbot if you have a specific reason.'),
p(['Cogbot also requires ',
   a(href='http://www.microsoft.com/download/en/details.aspx?id=21',
     'Microsoft .NET Framework 3.5'), ' (most users will already have this installed']),
	  \start_action_bar
       ]).

start_page_content -->
	{
	    current_prolog_flag(address_bits, 32)
	},
	html([
	    h2('Welcome to Cogbot'),
p(['You are using the ', b(em('32 bit version')),
   ' of swi-prolog to run this installer.',
   'Cogbot runs best with 64 bit swi-prolog. If you would rather use',
   ' the suggested 64 bit version, cancel this installer, ',
   'uninstall 32 bit swi-Prolog, and install the 64 bit version.']),
p(['Cogbot itself also comes in 32 and 64 bit versions.',
   'Only install Cogbot 32 if you have a definite reason to do so.']),
	  \start_action_bar
       ]).

start_action_bar -->
	action_bar(startpage:start_page_buttons).

start_page_buttons -->
	html([
	    \wizard_button(reset, 'Cancel Install'),
	    \wizard_button(location_by_id(components)+[arch=32], 'Install Cogbot 32 (experimental)'),
	    \wizard_button_default(location_by_id(components)+[arch=64], 'Install Cogbot 64')]).

