%
%   Installer for Cogbot
%   Copyright (c) 2012, Anne Ogborn
%   This code governed by the Cogbot New BSD License
%   which should have come with this code.
%
%
%   bundle file that installs aiml personality file
%

bundle_for(aiml_personality, aiml_personality_group).

bundle(aiml_personality_group, group, 'Aiml Personality Group', []).

deps(aiml_personality_group, chomsky_files).

before(corepatch, chomsky_files).

bundle(chomsky_files, files,
       'AIML Personality',
       [from(temp('cogbot-aiml.zip')),
	url(logicmoo('cogbot-aiml.zip')),
	to(program(.))]).




