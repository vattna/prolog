:- pce_begin_class(gameboard, object, "gameboard object").
variable(selected, object, both, "the selected cell" ).
variable(selectedStatus, name, both, 'true/false if a cell is selected').

variable(emptyColor, object, both, 'color of an empty cell').
variable(playerColor, object, both, 'color of an player occupied cell').
variable(enemyColor, object, both, 'color of an computer occupied cell').
variable(selectedColor, object, both, 'color of an selected cell').

initialise(P, String):-
	write('try to init '), write(String), nl,
	send(P, slot, emptyColor, colour(@default, 65535, 65535, 0)),
	send(P, slot, playerColor, colour(@default, 65535, 0, 65535)),
	send(P, slot, enemyColor, colour(@default, 0, 65535, 65535)),
	send(P, slot, selectedColor, colour(@default, 30000, 0, 30000)),
	send(P, slot, selectedStatus, 'false').

setSelected(P, Cell: object, Old):->
	write('try to selected A'), nl,
	(get(P, slot, selectedStatus, 'true'), get(P, slot, selected, Old));
	send(P, slot, selected, Cell), send(P,slot,selctedStatus, 'true'),
	write('selected A Hexagon'), nl.


:- pce_end_class(gameboard).

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
