%
%   Installer for Cogbot
%   Copyright (c) 2012, Anne Ogborn
%   This code governed by the Cogbot New BSD License
%   which should have come with this code.
%
%
%   bundle file that installs wordnet
%

bundle_for(wordnet, wordnet_group).

bundle(wordnet_group, group, 'Wordnet Group', []).

deps(wordnet_group, wordnet_files).
deps(wordnet_group, wordnet_license).

before(corepatch, wordnet_files).

bundle(wordnet_files, files,
       'Wordnet Files',
       [from(temp('cogbot-wordnet.zip')),
	url(logicmoo('cogbot-wordnet.zip')),
	to(program(.))]).

bundle(wordnet_license,
       license,
       'Wordnet License',
       [url('/f/wordnetlicense.html')]).



