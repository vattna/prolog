:- pce_begin_class(cell, path, "The honneycomb cell").
variable(p, path, both, "the cell itself" ).
variable(id, name, both, "the id of the cell").
variable(window, object, both, "the display" ).
variable(letter, name, both, "Upcase letter displayed in the cell" ).
variable(center, point, both, "coordinates of the center of the cell").
variable(defaultColor, colour, both, 'default color of the cell').
variable(color, colour, both, "colour of the cell").
variable(count, counter, both, "counter of unclicked cells").
variable(status, object, both, "unoccupied/player/computer").
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
	send(P, slot, id, Number),
	send(P, slot, letter, ' '),
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
	send(P, slot, defaultColor, colour(@default, 65535, 65535, 0)),
	((Number < 15, send(P, move, 'computer', 4));
	(Number > 112, send(P, move, 'player', 4));
	1 == 1),
	send(P, setNeighbors, Number),
	% create the link between the mouse and the cell
	send(Pa, recogniser,
	     click_gesture(left, '', single, message(P, click))).

setNeighbors(P, Number):->
	(N is Number - 14, N > 0 ; N = 0),
	%write('N: '), write(N), nl,
	(S is Number + 14, S <126 ;S = 0),
	%write('S: '), write(S), nl,
	Mod is mod(Number, 2),
	((Mod == 0,
		((NE is Number - 1, NE > 0 ; NE = 0),
		(NW is Number + 1,NW > 0 ; NW = 0),
		(SE is Number + 13, SE <126 ;SE = 0),
		(SW is Number + 15, SW <126 ;SW = 0))
	);
	(Mod == 1,
		((NE is Number - 15, NE > 0 ; NE = 0),
		(NW is Number - 13,NW > 0 ; NW = 0),
		(SE is Number - 1, SE <126 ;SE = 0),
		(SW is Number + 1, SW <126 ;SW = 0))
	)),
	(
	(member(Number, [1,15,29,43,57,71,85,99,113]), NW = 0, SW = 0);
	(member(Number, [14,28,24,56,70,84,98,112,126]), NE = 0, SE = 0);
	true),
	send(P, slot, northeast, NE),
	send(P, slot, north, N),
	send(P, slot, northwest, NW),
	send(P, slot, southeast, SE),
	send(P, slot, south, S),
	send(P, slot, southwest, SW).


move(P, Player, Units) :->
	send(P, slot, status, Player),
	(((Player == 'player'),
		send(P, slot, color, colour(@default, 65535, 0, 65535)),
		send(P, slot, letter, Units));
	((Player == 'computer'),
		send(P, slot, color, colour(@default, 0, 65535, 65535)),
		send(P, slot, letter, Units)
	));
	send(P, slot, color, colour(@default, 65535, 65535, 0)),
	send(P, slot, letter, ' ')
	.

unlink(P) :->
	get(P, slot, p, Pa),
	send(Pa, free),
	send(P, send_super, unlink).


% message processed when the cell is clicked
% or when the letter is pressed on the keyboard
click(P) :->
	((get(P, slot, status, player),
		send(P, selectUnit));
	(get(P, slot, status, computer),
		send(P, attack));
	(get(P, slot, status, unoccupied),
		send(P, selectFree))),
	send(P, my_draw).
	
	% test if the cell has already been clicked
	% succeed when the the status is 'unclicked'
	%get(P, slot, status, unoccupied),
	% change the status
	%send(P, slot, status, clicked),
	% change the color
	%send(P, slot, color, colour(@default, 65535, 0, 65535)),
	%send(P, slot, letter, 4),
	%send(P, my_draw),
	%get(P, slot, letter, Letter),
	% inform the object "phrase" that a new letter is clicked
	%get(P, slot, phr, Phrase),
	%send(Phrase, new_letter, Letter),
	% inform the object "counter" that a new letter is clicked
	%get(P, count, Counter),
	%send(Counter, decrement).
reset(P):-
	%get(P, slot, defaultColor, Color),
	send(P, slot, color, Color),
	send(P, my_draw).
	
selectUnit(P) :->
	cellPred('selected', Old), Old \= null, send(Old, reset),
	cellPred('selected', P),
	%send(Old, slot, color, colour(@default, 65535, 65535, 0)),
	color('selectedUnit', Color),
	send(P, slot, color,  Color).
	
attack(P) :->
	
	write('selected enemy'), nl.

selectFree(P) :->
	write('selected free space'), nl.

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