:- reconsult('classes.pl').
:- reconsult('cell.pl').

game:-
	new(W, window('Game')),
	new(Gameboard, gameboard('board')),
	new(Counter, counter(126)),
	new(Ph, phrase(W, point(50,500))),
	numlist(0, 125, NL),
	mapBoard(126, [], LL),
	maplist(build_list(150,100), NL, LP),
	new(ChCell, chain),
	maplist(create_cell(W, Counter, Ph, ChCell), LP, LL),
	send(W, size, size(1024, 600)),
	write('test'), nl,
	send(W, done_message, and(message(ChCell, for_all, message(@arg1, free)),
				  message(ChCell, free),
				  message(Counter, free),
				  message(Gameboard, free),
				  message(Ph, free),
				  message(W, destroy))),

	send(W, open).


mapBoard(0, LL, LL) :- !.

mapBoard(N, L1, LL) :-

	C is 100 - N,
	(   \+member(C, L1) ->
	    N1 is N-1, mapBoard(N1, [C|L1], LL)
	;   mapBoard(N, L1, LL)).

create_cell(W, Counter,Phrase, ChCell, Point,  Code) :-
	%char_code(Number, Code),
	Number is 100 - Code,
	new(H, cell(W, Counter, Phrase, Number, Point)),
	send(H, my_draw),
	send(ChCell, append, H).

build_list(X0,Y0, N, point(X,Y)) :-
	C is N mod  14,
	L is N // 14,
	C0 is C mod 2,
	X is C * 75 + X0,
	Y is L * round(50 * sqrt(3)) + C0 * round(25 * sqrt(3)) + Y0.


