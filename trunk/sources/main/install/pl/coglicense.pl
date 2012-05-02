:-module(coglicense, [
		  ]).

:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).

:- use_module(logger).
:- use_module(bundle).
:- use_module(execute, [plan_license_entries/2]).
:- use_module(layout).

:- http_handler(root(license) ,
		license_page_handler,
		[id(license)]).

license_page_handler(_Request) :-
	reply_html_page(
	    cogbot_web_style,
	    html([title('Cogbot Component Licenses')]),
	    coglicense:license_page_content).

license_page_content -->
	{
	    bundle_install_plan(Plan),
	    plan_license_entries(Plan, LicenseHTML)
	},
	html([
	    h2('Cogbot Component Licenses'),
	    div(class=license_container, [
		\LicenseHTML,
		p('This installer includes and uses info-zip.'),
		\license_entry('Info-ZIP license', '/f/infoziplicense.html')
		]),
		\action_bar(coglicense:license_buttons)
	     ]).

/*
p('Copyright (c) 2012, Logicmoo'),
p('All rights reserved.'),
p('Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:'),
p('Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.'),
p('Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.'),
p('THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.')
       ]).
*/

license_entry(Title, URL) -->
	html([
	    div(class=license_entry,
		[
		    h4(Title),
		    div(class=license_frame,
			iframe(
			      [src=URL, width=600, height=400],
			      [p(['Oops can\'t find license file ' , URL])]))
		])
	     ]).

license_buttons -->
	html([
	    \wizard_button(reset, 'I decline the offered licenses'),
	    \wizard_button_default(config, 'I accept the offered licenses')
	     ]).
