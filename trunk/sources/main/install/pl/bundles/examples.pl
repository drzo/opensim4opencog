%
%   Installer for Cogbot
%   Copyright (c) 2012, Anne Ogborn
%   This code governed by the Cogbot New BSD License
%   which should have come with this code.
%
%
%   bundle file that installs the examples
%

bundle_for(examples, example_group).

bundle(example_group, group, 'Examples Group', []).

deps(example_group, example_files).

before(corepatch, example_files).

bundle(example_files, files,
       'Examples',
       [from(temp('cogbot-examples.zip')),
	url(logicmoo('cogbot-examples.zip')),
	to(program(.))]).


