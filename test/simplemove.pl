%--------------------------------------------------------
%
%  simplemove.pl
%
%    Testing of multi bot logon and movement
%
%    This requires a set of the 'hillpeople' bots
%
%---------------------------------------------------------------------

:-set_prolog_flag(double_quotes,string).

:-use_module('../test/movesupport').
:-use_module('../test/acctsupport').

%--------------------------------------------------------
%
% define the waypoints of the test
%
%--------------------------------------------------------

get_test_waypoints(Name,loop([Name1,Name2,Name1])):-atom_concat(Name,1,Name1),atom_concat(Name,2,Name2).

%--------------------------------------------------------
%
% run the test
%
%--------------------------------------------------------
:-set_num_bots(2).
:-set_tribe('Hillpeople').

:-logon_bots.

:-run_test.
