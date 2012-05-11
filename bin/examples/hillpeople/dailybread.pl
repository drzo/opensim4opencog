:- module(dailybread, [
		       yr/0
		      ]).

%%	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%       Simulation of a hunter-gatherer-swidden agriculture culture.
%
%       Every day each tribe member engages in one of these actions:
%
%       rest - do nothing. this is all infants and elderly do
%       play - all children play, adults can too
%       fish - two people can fish in the stream
%       plant - plant corn
%       harvest - harvest corn
%	cook - One adult must always be assigned to cook, or the only
%	       food that can be consumed is berries
%       gatherberry - Gather berries
%
%
%
%       Every member is one of the following
%
%	infant	     0-2 year old
%       child_f      2- pubescent female, only plays
%       child_m      2- pubescent male, only plays
%
%       adult_f      non pregnant female of working age
%       adult_m	     non pregnant male of working age
%       pregnant     pregnant female
%       nursing      mother of 0-2 year old
%       elderly      person too aged or disabled to work or reproduce
%
%
%

yr :-
	format(user_error, 'someday this will be dailybread~n', []).


