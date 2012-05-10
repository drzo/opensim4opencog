% ===================================================================
% File 'cogbot_user.pl'
% Purpose:  User API atop Cogbot swipl support
% Maintainer: Anne Ogborn
% Contact: $Author: aogborn $@uh.edu ;
% Version: 'cogbot_user.pl' 1.0.0
% Revision:  $Revision: 0.1 $
% Revised At:   $Date: 2012/05/08 21:57:28 $
% ===================================================================
%
%  This is something Annie started -
%  It's unclear if we just need a bunch of utilities and examples
%  of how to do common things, or actually need an isolation layer
%
:- module(cogbot_user ,
	  [
	  cog_avatars/2
	  ]).

:- use_module(cogbot(cogrobot)).


%
% cog_avatars(?X, +Dist) is nondet
%
% iterates over all the truly signed on
% avatars within Dist
cog_avatars(AV, Dist) :-
	botClient(Bot),
	simAvatar(AV),
	AV \= Bot,
	simAvDistance(Bot, AV, D),
	D =< Dist.









