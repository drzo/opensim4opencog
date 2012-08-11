%
%   Installer for Cogbot
%   Copyright (c) 2012, Anne Ogborn
%   This code governed by the Cogbot New BSD License
%   which should have come with this code.
%
%
%   bundle file that installs the objects
%

bundle_for(objects, objects_group).

bundle(objects_group, group, 'Objects Group', []).

deps(objects_group, objects_files).

before(corepatch, objects_files).

bundle(objects_files, files,
       'Objects',
       [from(temp('cogbot-objects.zip')),
	url(logicmoo('cogbot-objects.zip')),
	to(program(.))]).


