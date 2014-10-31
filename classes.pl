:- pce_begin_class(gameboard, string, "spelled string").
variable(selected, object, both, "the selected cell" ).

initialise :->
		send(P, slot, nb, N).

setSelected(Cell: object):->
	send(P, slot, selected, Cell).




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