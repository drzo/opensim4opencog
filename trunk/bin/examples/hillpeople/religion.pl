:- module(religion, [
		     god_likes/2
		    ]).
%%	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%            religion.pl
%
%
%
:- discontiguous
	god/1,
	long_name/2.

%
%  god_likes(God, Activity_name)
%
%  Unifies if this activity is pleasing to this God
%
god_likes(_, _).

god('Gopupu').
long_name('Gopupu', 'The God who can be named, but not in polite company').
god('Yoko Ono').
long_name('Yoko Ono', 'The beloved of John').
god('Latuppi').
god('Raivo').
long_name('Raivo', 'The Estonian One').
god('Snert').
god('Thalamus').
god('Prlg').
god('Prlg', 'The Horned God of Clauses').
god('Erlang').
god('Hickey').
god('Ringo').
god('Haskell').
god('Oppenheimer').
god('Medula').
long_name('Medula', 'Medula Oblongata').
god('Frontal Cortex').
god('Flying Spaghetti Monster').
god('Kastyke').  % courtesy of Erik Granström's "The Fifth Conflux" tetralogy
long_name('Kastyke', 'Beloved by Ski').
god('Knuth').
god('Annie').
god('Doug').
god('Kino').
long_name('Kino', 'The Creator').
god('Mammon').
god('Squat').
prayer('Squat', 'Squat, Squat, give us a parking spot! And we\'ll send you nine nuns in the mail').



/*
<ski>   "It is written that the goddess Kastyke loves those who loves themselves, and advantages those who advantages their own luck. It is also written that each one reaches the glory he manages to defend."
[22:08] <ski>   "Our goddess doesn't condemn anyone, she merely lets the nonindustrious wither and whirl away like leaves in the sighing autumn wind, being torn into shreads by the thorns of the world to deplorably drown in the salt sea of Tiamat on which the world is floating. But she rewards the enterprising and takes them to her side in success."
[22:08] <ski>   "Thus our city has to the glory of the goddess loved itself and through her grace been showered with the most sacred sign of her blessing -- the gold and the silver which exuberantly ferments in our coffers."
[22:08] <ski>   Erik Granström -- from "Sulphur Winter"
[22:08] <ski> that's an approximate translation
*/



