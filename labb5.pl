:- reconsult('recipes.pl').

listAll([]):-
	write('End of recipes'), nl.

listAll([[Name, Cat| _] |Rest]):-
	write(Name), tab, write(Cat), nl,
	listAll(Rest).

listAll:-
	write('try listall'), nl,
	listing(recipe).

search(Name, Ing, []):-
	write("no recipe found by name of "), write(Name), nl.

search(Name, Ing, [[Name, Cat, IngList ]|Tail]):-
	Ing = IngList.

search(Name, Ing, [[X, Cat, IngList ]|Tail]):-
	Name \= X,
	search(Name, Ing, Tail).


printIng([]):-
print('end of ingredients'),nl.

printIng([Ing|Rest]):-
	swritef(S, '%15L%w', Ing), write(S),nl,
	printIng(Rest).

select(Choice):-
	write('Select option:'),nl,
	write('[1]: Browse recipes'),nl,
	write('[2]: Add recipe'),nl,
	write('[3]: list recipes'),nl,
	write('[e]: Exit'),nl,
	read(C),
	((C == 1 ; C == 2; C==3 ; C == e ; C == 'E') -> Choice = C);
		(write('error processing input ['), write(C), write('] try again'), nl,	select(A), Choice = A).

enterIngredients(Ingredients):-
	write('enter amount [q] to cancel'), nl, read(Amount),
	(
	((not(Amount == 'q') ; not(Amount ==  'Q'));
		write('enter name of ingredient [q] to cancel'), nl, read(Ing),
		((not(Ing  == 'q') ; (Ing == 'Q'));
			write(Amount), write('	'), write(Ing), nl,
			enterIngredients([Ingredients|[Amount, Ing]])))
	);
	write('last Ing:'), nl,
	write(Amount), write('	'), write(Ing), nl.	

addRecipe(List, [List| Recipe]):-
	write('Enter name of recipe:'), nl, read(N),
	write('Enter category of recipe:'), nl, read(C),
	enterIngredients(Ingredients),
	Recipe = [Name, Cat| Ingredients].

	/* switch method */
run(C):-
	write('In run: '), write(C), nl,
	(C == 1,
		(write('Enter search or leave [1] to browse all: '),nl, read(Browse)),
			(
				(Browse == 1, list(Recipes);
				(search(Browse, Ing, Recipes),
				printIng(Ing))
			),
		program
		)
	);
	(C == 2,
		addRecipe(Recipes, NewRecipes), storeRecipe(NewRecipes) ,program
	);
	(C == 3,
		listAll, program
	);
	((C == e ; C == 'E'),
		storeRecipes,
		write('goodbye'), nl
	);
	write('[OTHER CHOICES] not implemented'),nl, program.

load(Stream, []):-
	at_end_of_stream(Stream).
load(Stream,[X|L]):-
    \+  at_end_of_stream(Stream),
    read(Stream,X),
    load(Stream,L).

	% method for loading recipes
loadRecipes(Recipes):-
	
    absolute_file_name(library(LibraryFile),
	[ file_type(prolog),
	access(read)
	],
	'backup.pl').
	/*open('newRecipes.pl', read, Str),
	loadRecipes(Str, Recipes),
	close(Str),
	write(Recipes).*/

store([]):-
	told.
store([Recipe| Rest]):-
	retractall(recipe),
	assert(recipe(Recipe)),
	listing(recipe(Recipe)),
	store(Rest).

	% method for storing recipes

storeRecipe([Name, Cat, Ing]):-
	assert(recepie(Name, Cat, Ing)).

storeRecipes:-
	listing(recipe),
	tell('test.pl'),
	listing(recipe),
	told.
	%open('newRecipes.pl', write, Stream),
    %store(Store, Stream).

	% main program
program:-
	%loadRecipes(Recipes),
	select(Choice),
	run(Choice).
