game:-
	new(W, window('Game')),
	new(Counter, counter(126)),
	new(Ph, phrase(W, point(50,500))),
	numlist(0, 125, NL),
	mapBoard(126, [], LL),
	maplist(build_list(150,100), NL, LP),
	new(ChCell, chain),
	maplist(create_cell(W, Counter, Ph, ChCell), LP, LL),
	send(W, size, size(1024, 600)),
	send(W, done_message, and(message(ChCell, for_all, message(@arg1, free)),
				  message(ChCell, free),
				  message(Counter, free),
				  message(Ph, free),
				  message(W, destroy))),
	
	send(W, open).

	
mapBoard(0, LL, LL) :- !.

mapBoard(N, L1, LL) :-
	C is 126 - N,
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
	
:- pce_begin_class(phrase, string, "spelled string").
variable(str, string, both, "displayed string").
variable(window, object, both, "the display" ).
variable(pt, point, both, "where to display strings").
variable(lbl1, label, both, "label to display the letters").
variable(lbl2, label, both, "label to display the last letter").

initialise(P, Window : object, Point : point) :->
	send(P, slot, window, Window),
	send(P, slot, str, new(_, string(''))),
	send(P, slot, pt, Point),
	new(Lbl1, label),
	send(Lbl1, font,  @times_bold_24),
	send(P, slot, lbl1, Lbl1),
	new(Lbl2, label),
	send(Lbl2, font,  @times_bold_24),
	send(P, slot, lbl2, Lbl2).

unlink(P) :->
	get(P, slot, lbl1, Lbl1),
	send(Lbl1, free),
	get(P, slot, lbl2, Lbl2),
	send(Lbl2, free),
	send(P, send_super, unlink).

% display the list of the letters
% and the last letter on the screen
new_letter(P, Letter) :->
	get(P, slot, str, Str),
	send(Str, append, Letter),
	send(P, slot, str, Str),
	new(S1, string('Chosen : %s', Str)),
	get(P, slot, lbl1, Lbl1),
	send(Lbl1, selection, S1),
	get(P, slot, window, W),
	get(P, slot, pt, Pt),
	send(W, display,  Lbl1, Pt),
	new(S2, string('The user choose letter %c.', Letter)),
	get(P, slot, lbl2, Lbl2),
	send(Lbl2, selection, S2),
	get(Pt, x, X),
	get(Pt, y, Y),
	Y1 is Y + 30,
	send(W, display, Lbl2, point(X, Y1)).

:- pce_end_class(phrase).
	
:- pce_begin_class(counter, object, "count the unclicked cells").
	variable(nb, number, both, "number of unclicked cells").

	initialise(P, N : number) :->
		send(P, slot, nb, N).

	decrement(P) :->
		get(P, slot, nb, N),
		send(N, minus, 1),
		send(P, slot, nb, N),
		(   send(N, equal, 0) ->
			send(@display, inform, 'The game is over !')
		;   true).
:- pce_end_class(counter).

:- pce_begin_class(cell, path, "The honneycomb cell").
variable(p, path, both, "the cell itself" ).
variable(window, object, both, "the display" ).
variable(letter, name, both, "Upcase letter displayed in the cell" ).
variable(center, point, both, "coordinates of the center of the cell").
variable(color, colour, both, "colour of the cell").
variable(count, counter, both, "counter of unclicked cells").
variable(status, object, both, "clicked/unclicked").
variable(phr, phrase, both, "to display the new letter").
variable(northeast, name, both, "position").
variable(north, name, both, "position").
variable(northwest, name, both, "position").
variable(southeast, name, both, "position").
variable(south, name, both, "position").
variable(southwest, name, both, "position").


initialise(P, Window : object, Counter : counter,
	   Phrase: phrase, Number, Center:point) :->
	send_super(P, initialise),
	send(P, slot, letter, Number),
	send(P, slot, center, Center),
	send(P, slot, window, Window),
	send(P, slot, count, Counter),
	send(P, slot, status, unoccupied),
	send(P, slot, phr, Phrase),
	new(Pa, path),
        (
	   get(Center, x, X0),
	   get(Center, y, Y0),
	   X is X0 - 25, Y is Y0 -  round(25 * sqrt(3)),
   	   send(Pa, append, point(X, Y)),
	   X1 is X + 50,
  	   send(Pa, append, point(X1, Y)),
	   X2 is X1 + 25,
	   send(Pa, append, point(X2, Y0)),
 	   Y3 is  Y0 + round(25 * sqrt(3)),
	   send(Pa, append, point(X1, Y3)),
	   send(Pa, append, point(X, Y3)),
	   X4 is X - 25,
	   send(Pa, append, point(X4, Y0)),
	   send(Pa, closed, @on)
	),
	send(P, p, Pa),
	send(P, slot, color, colour(@default, 65535, 65535, 0)),
	((Number < 15, send(P, move, 'computer', 4));
	(Number > 112, send(P, move, 'player', 4));
	1 == 1),
	send(P, setNeighbors, Number),
	% create the link between the mouse and the cell
	send(Pa, recogniser,
	     click_gesture(left, '', single, message(P, click))).

setNeighbors(P, Number):->
	write('for number: '), write(Number), nl,
	(N is Number - 14, N > 0 ; N = 0),
	%write('N: '), write(N), nl,
	(S is Number + 14, S <126 ;S = 0),
	%write('S: '), write(S), nl,
	
	Mod is mod(Number, 2),
	(Mod == 0,
		((NE is Number - 1, NE > 0 ; NE = 0),
		(NW is Number + 1,NW > 0 ; NW = 0),
		(SE is Number + 13, SE <126 ;SE = 0),
		(SW is Number + 15), SW <126 ;SW = 0)
	);
	(Mod == 1,
		((NE is Number - 15, NE > 0 ; NE = 0),
		(NW is Number - 13,NW > 0 ; NW = 0),
		(SE is Number - 1, SE <126 ;SE = 0),
		(SW is Number + 1), SW <126 ;SW = 0)
	),
	(
	(member(Number, [1,15,29,43,57,71,85,99,113]), NW = 0, SW = 0);
	(member(Number, [14,28,24,56,70,84,98,112,126]), NE = 0, SE = 0);
	true),
	
	send(P, slot, northeast, NE),
	send(P, slot, north, N),
	send(P, slot, northwest, NW),
	send(P, slot, southeast, SE),
	send(P, slot, south, S),
	send(P, slot, southwest, SW)
	).
	
	
move(P, Player, Units) :->
	send(P, slot, status, Player),
	(((Player == 'player'),
		send(P, slot, color, colour(@default, 65535, 0, 65535)));
		%send(P, slot, letter, Units));
	((Player == 'computer'),
		send(P, slot, color, colour(@default, 0, 65535, 65535))
		%send(P, slot, letter, Units)
	)).
	
unlink(P) :->
	get(P, slot, p, Pa),
	send(Pa, free),
	send(P, send_super, unlink).


% message processed when the cell is clicked
% or when the letter is pressed on the keyboard
click(P) :->
	% test if the cell has already been clicked
	% succeed when the the status is 'unclicked'
	get(P, slot, status, unoccupied), 
	% change the status
	send(P, slot, status, clicked),
	% change the color
	send(P, slot, color, colour(@default, 65535, 0, 65535)),
	send(P, slot, letter, 4),
	send(P, my_draw),
	get(P, slot, letter, Letter),
	% inform the object "phrase" that a new letter is clicked
	get(P, slot, phr, Phrase),
	send(Phrase, new_letter, Letter),
	% inform the object "counter" that a new letter is clicked
	get(P, count, Counter),
	send(Counter, decrement).

my_draw(P) :->
	% display the path and fill it with the current colour
	get(P, slot, window, W),
	get(P, slot, p, Pa),
        send(W, display, Pa),
        get(P, slot, color, Col),
	send(Pa, fill_pattern, Col),

	% display the letter centered
	get(P, slot, letter, C),
   	new(Str, string(C)),
	new(Tx, text(Str?value)),
	send(Tx, font, font(times, bold, 24)),

	% compute the size of the message to center it
	get(P, slot, center, point(X0,Y0)),
	get(font(times, bold, 24), width(Str), M),
	XT is X0 - M/2,
	get(font(times, bold, 24), height, H),
	YT is Y0 - H/2,
	send(W, display, Tx, point(XT, YT)).

:- pce_end_class(cell).
